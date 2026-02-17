// ---------------------------------------------------------------------------
// Racket Strategy — inference bridge for symmetric operations
// ---------------------------------------------------------------------------
//
// Design:
//   The Racket strategy uses fact pack metadata (symmetric_partner properties)
//   and metasignatures to infer the type signatures of operations that are
//   only defined as stubs.
//
//   Example: Given only the metasignature of `+` and the fact that
//   `symmetric_partner(op_add) = op_subtract`, the strategy infers that
//   `-` has signature `(Number, Number) → Number` with category `arithmetic`
//   and effects `none`. Invariants do NOT transfer — they are op-specific.
//
//   On successful inference, the inferred op gets promoted to a full op
//   entry with a metasignature in the registry.
//
// Architecture:
//   - `build_racket_registry()` — loads racket_ops.yaml
//   - `load_keyword_map()` — builds keyword → op_name mapping from fact pack
//   - `infer_symmetric_op()` — core inference: partner metasig → inferred metasig
//   - `promote_inferred_ops()` — runs inference for all stubs, promotes results
//   - `resolve_keyword()` — NL keyword → canonical op name

use std::collections::HashMap;
use std::path::Path;

use crate::fact_pack::{FactPack, FactPackIndex};
use crate::registry::{
    MetaParam, MetaSignature, OperationRegistry,
    load_ops_pack_str,
};

// ---------------------------------------------------------------------------
// Error type
// ---------------------------------------------------------------------------

/// Errors from the Racket inference engine.
#[derive(Debug, Clone)]
pub enum InferenceError {
    /// The op has no symmetric partner declared in the fact pack
    NoSymmetricPartner(String),
    /// The symmetric partner has no metasignature to infer from
    PartnerHasNoMeta { op: String, partner: String },
    /// The op name could not be resolved from the fact pack
    UnknownEntity(String),
    /// Fact pack loading failed
    FactPackError(String),
}

impl std::fmt::Display for InferenceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InferenceError::NoSymmetricPartner(op) =>
                write!(f, "op '{}' has no symmetric partner in the fact pack", op),
            InferenceError::PartnerHasNoMeta { op, partner } =>
                write!(f, "symmetric partner '{}' of '{}' has no metasignature", partner, op),
            InferenceError::UnknownEntity(e) =>
                write!(f, "unknown entity '{}' in fact pack", e),
            InferenceError::FactPackError(msg) =>
                write!(f, "fact pack error: {}", msg),
        }
    }
}

impl std::error::Error for InferenceError {}

// ---------------------------------------------------------------------------
// Inference kind — how was the signature derived?
// ---------------------------------------------------------------------------

/// How an operation's signature was inferred.
#[derive(Debug, Clone, PartialEq)]
pub enum InferenceKind {
    /// Inferred from the inverse/symmetric partner (e.g., subtract from add)
    OpSymmetric,
    /// Inferred from a type-symmetric peer in the same class+category (e.g., multiply from add)
    TypeSymmetric { class: String },
}

// ---------------------------------------------------------------------------
// Inferred operation result
// ---------------------------------------------------------------------------

/// The result of inferring an operation's signature from its symmetric partner.
#[derive(Debug, Clone)]
pub struct InferredOp {
    /// The op name (e.g., "subtract")
    pub op_name: String,
    /// The entity id in the fact pack (e.g., "op_subtract")
    pub entity_id: String,
    /// The Racket symbol (e.g., "-")
    pub racket_symbol: String,
    /// The inferred metasignature (invariants stripped)
    pub meta: MetaSignature,
    /// The partner op this was inferred from
    pub inferred_from: String,
    /// Whether invariants were intentionally dropped
    pub invariants_dropped: bool,
    /// How the inference was performed
    pub inference_kind: InferenceKind,
}

// ---------------------------------------------------------------------------
// Embedded data
// ---------------------------------------------------------------------------

const RACKET_OPS_YAML: &str = include_str!("../data/racket_ops.yaml");

// ---------------------------------------------------------------------------
// Registry builder
// ---------------------------------------------------------------------------

/// Build an OperationRegistry populated with Racket ops.
///
/// Tries disk first, falls back to embedded YAML.
pub fn build_racket_registry() -> OperationRegistry {
    if let Ok(reg) = crate::registry::load_ops_pack("data/racket_ops.yaml") {
        return reg;
    }
    load_ops_pack_str(RACKET_OPS_YAML)
        .expect("embedded racket_ops.yaml should always parse")
}

// ---------------------------------------------------------------------------
// Fact pack helpers
// ---------------------------------------------------------------------------

/// Load the Racket fact pack and build an index.
pub fn load_racket_facts() -> Result<FactPackIndex, InferenceError> {
    let path = Path::new("data/racket_facts.yaml");
    crate::fact_pack::load_fact_pack(path)
        .map_err(|e| InferenceError::FactPackError(e.to_string()))
}

/// Load the Racket fact pack from embedded YAML (for tests / no-disk scenarios).
pub fn load_racket_facts_from_str(yaml: &str) -> Result<FactPackIndex, InferenceError> {
    let pack: FactPack = serde_yaml::from_str(yaml)
        .map_err(|e| InferenceError::FactPackError(e.to_string()))?;
    Ok(FactPackIndex::build(pack))
}

// ---------------------------------------------------------------------------
// Keyword map — natural language → op name
// ---------------------------------------------------------------------------

/// Build a keyword → op_name mapping from the fact pack.
///
/// Reads properties on the `keywords` axis where key starts with `keyword_`.
/// The property value is the keyword, the entity's `op_name` property gives
/// the canonical op name.
///
/// Example: entity=op_add, keyword_add="add", op_name="add" → "add" → "add"
///          entity=op_add, keyword_plus="plus", op_name="add" → "plus" → "add"
pub fn load_keyword_map(facts: &FactPackIndex) -> HashMap<String, String> {
    let mut map = HashMap::new();

    // First, build entity_id → op_name mapping
    let mut entity_to_op: HashMap<String, String> = HashMap::new();
    for prop in &facts.pack.properties {
        if prop.key == "op_name" {
            entity_to_op.insert(prop.entity.clone(), prop.value.clone());
        }
    }

    // Then, collect keyword → op_name
    for prop in &facts.pack.properties {
        if prop.axis == "keywords" && prop.key.starts_with("keyword_") {
            if let Some(op_name) = entity_to_op.get(&prop.entity) {
                map.insert(prop.value.clone(), op_name.clone());
            }
        }
    }

    // Also map Racket symbols → op_name
    for prop in &facts.pack.properties {
        if prop.key == "racket_symbol" {
            if let Some(op_name) = entity_to_op.get(&prop.entity) {
                map.insert(prop.value.clone(), op_name.clone());
            }
        }
    }

    map
}

/// Resolve a natural language keyword to a canonical op name.
///
/// Looks up in the keyword map built from the fact pack.
pub fn resolve_keyword(keyword: &str, keyword_map: &HashMap<String, String>) -> Option<String> {
    keyword_map.get(keyword).cloned()
}

// ---------------------------------------------------------------------------
// Symmetric partner lookup
// ---------------------------------------------------------------------------

/// Find the symmetric partner entity for a given entity id.
///
/// Reads the `symmetric_partner` property on the `symmetry` axis.
fn find_symmetric_partner(entity_id: &str, facts: &FactPackIndex) -> Option<String> {
    facts.pack.properties.iter()
        .find(|p| p.entity == entity_id && p.key == "symmetric_partner" && p.axis == "symmetry")
        .map(|p| p.value.clone())
}

/// Find the op_name for an entity id.
fn entity_to_op_name(entity_id: &str, facts: &FactPackIndex) -> Option<String> {
    facts.pack.properties.iter()
        .find(|p| p.entity == entity_id && p.key == "op_name")
        .map(|p| p.value.clone())
}

/// Find the entity id for an op_name.
fn op_name_to_entity(op_name: &str, facts: &FactPackIndex) -> Option<String> {
    facts.pack.properties.iter()
        .find(|p| p.key == "op_name" && p.value == op_name)
        .map(|p| p.entity.clone())
}

/// Find the Racket symbol for an entity id.
fn entity_to_racket_symbol(entity_id: &str, facts: &FactPackIndex) -> Option<String> {
    facts.pack.properties.iter()
        .find(|p| p.entity == entity_id && p.key == "racket_symbol")
        .map(|p| p.value.clone())
}

// ---------------------------------------------------------------------------
// Core inference
// ---------------------------------------------------------------------------

/// Infer the metasignature of an operation from its symmetric partner.
///
/// Given an op name (e.g., "subtract"), this:
/// 1. Finds the entity in the fact pack (op_subtract)
/// 2. Finds its symmetric partner (op_add)
/// 3. Looks up the partner's metasignature in the registry
/// 4. Copies the type structure (params, return_type, category, effects)
/// 5. Drops invariants (they are op-specific, not symmetric)
///
/// Returns an `InferredOp` with the inferred metasignature.
pub fn infer_symmetric_op(
    op_name: &str,
    registry: &OperationRegistry,
    facts: &FactPackIndex,
) -> Result<InferredOp, InferenceError> {
    // 1. Find entity for this op
    let entity_id = op_name_to_entity(op_name, facts)
        .ok_or_else(|| InferenceError::UnknownEntity(op_name.to_string()))?;

    // 2. Find symmetric partner entity
    let partner_entity = find_symmetric_partner(&entity_id, facts)
        .ok_or_else(|| InferenceError::NoSymmetricPartner(op_name.to_string()))?;

    // 3. Find partner's op name
    let partner_op = entity_to_op_name(&partner_entity, facts)
        .ok_or_else(|| InferenceError::UnknownEntity(partner_entity.clone()))?;

    // 4. Look up partner's metasignature in the registry
    let partner_entry = registry.get_poly(&partner_op)
        .ok_or_else(|| InferenceError::PartnerHasNoMeta {
            op: op_name.to_string(),
            partner: partner_op.clone(),
        })?;

    let partner_meta = partner_entry.meta.as_ref()
        .ok_or_else(|| InferenceError::PartnerHasNoMeta {
            op: op_name.to_string(),
            partner: partner_op.clone(),
        })?;

    // 5. Build inferred metasignature — copy structure, drop invariants
    let had_invariants = !partner_meta.invariants.is_empty();
    let inferred_meta = MetaSignature {
        params: partner_meta.params.iter().map(|p| MetaParam {
            name: p.name.clone(),
            type_name: p.type_name.clone(),
            variadic: p.variadic,
        }).collect(),
        return_type: partner_meta.return_type.clone(),
        invariants: vec![], // Invariants do NOT transfer
        category: partner_meta.category.clone(),
        effects: partner_meta.effects.clone(),
    };

    // 6. Find Racket symbol for this op
    let racket_symbol = entity_to_racket_symbol(&entity_id, facts)
        .unwrap_or_else(|| op_name.to_string());

    Ok(InferredOp {
        op_name: op_name.to_string(),
        entity_id,
        racket_symbol,
        meta: inferred_meta,
        inferred_from: partner_op,
        invariants_dropped: had_invariants,
        inference_kind: InferenceKind::OpSymmetric,
    })
}

// ---------------------------------------------------------------------------
// Type-symmetric inference — generalize across same-shape ops
// ---------------------------------------------------------------------------

/// Find the type_symmetry_class for an entity.
fn entity_type_symmetry_class(entity_id: &str, facts: &FactPackIndex) -> Option<String> {
    facts.pack.properties.iter()
        .find(|p| p.entity == entity_id && p.key == "type_symmetry_class" && p.axis == "type_symmetry")
        .map(|p| p.value.clone())
}

/// Find the category_name for an entity.
fn entity_category(entity_id: &str, facts: &FactPackIndex) -> Option<String> {
    facts.pack.properties.iter()
        .find(|p| p.entity == entity_id && p.key == "category_name" && p.axis == "category")
        .map(|p| p.value.clone())
}

/// Find a type-symmetric peer: another entity in the same class AND same
/// category that has a metasignature in the registry.
///
/// Returns the peer's op_name (not entity id).
pub fn find_type_symmetric_peer(
    op_name: &str,
    registry: &OperationRegistry,
    facts: &FactPackIndex,
) -> Option<String> {
    let entity_id = op_name_to_entity(op_name, facts)?;
    let my_class = entity_type_symmetry_class(&entity_id, facts)?;
    let my_category = entity_category(&entity_id, facts)?;

    // Find all entities in the same class + category that have a metasig
    for prop in &facts.pack.properties {
        if prop.key == "type_symmetry_class"
            && prop.axis == "type_symmetry"
            && prop.value == my_class
            && prop.entity != entity_id
        {
            // Check same category
            if entity_category(&prop.entity, facts).as_deref() != Some(&my_category) {
                continue;
            }
            // Check if this entity's op has a metasig in the registry
            if let Some(peer_op) = entity_to_op_name(&prop.entity, facts) {
                if let Some(entry) = registry.get_poly(&peer_op) {
                    if entry.meta.is_some() {
                        return Some(peer_op);
                    }
                }
            }
        }
    }
    None
}

/// Infer the metasignature of an operation from a type-symmetric peer.
///
/// Like `infer_symmetric_op`, but uses type_symmetry_class + category
/// instead of symmetric_partner. The peer is any op in the same class
/// and category that already has a metasignature.
pub fn infer_type_symmetric_op(
    op_name: &str,
    registry: &OperationRegistry,
    facts: &FactPackIndex,
) -> Result<InferredOp, InferenceError> {
    let entity_id = op_name_to_entity(op_name, facts)
        .ok_or_else(|| InferenceError::UnknownEntity(op_name.to_string()))?;

    let class = entity_type_symmetry_class(&entity_id, facts)
        .ok_or_else(|| InferenceError::NoSymmetricPartner(op_name.to_string()))?;

    let peer_op = find_type_symmetric_peer(op_name, registry, facts)
        .ok_or_else(|| InferenceError::PartnerHasNoMeta {
            op: op_name.to_string(),
            partner: format!("(no peer with meta in class '{}')", class),
        })?;

    let peer_meta = registry.get_poly(&peer_op)
        .and_then(|e| e.meta.as_ref())
        .ok_or_else(|| InferenceError::PartnerHasNoMeta {
            op: op_name.to_string(),
            partner: peer_op.clone(),
        })?;

    let had_invariants = !peer_meta.invariants.is_empty();
    let inferred_meta = MetaSignature {
        params: peer_meta.params.iter().map(|p| MetaParam {
            name: p.name.clone(),
            type_name: p.type_name.clone(),
            variadic: p.variadic,
        }).collect(),
        return_type: peer_meta.return_type.clone(),
        invariants: vec![], // Invariants do NOT transfer
        category: peer_meta.category.clone(),
        effects: peer_meta.effects.clone(),
    };

    let racket_symbol = entity_to_racket_symbol(&entity_id, facts)
        .unwrap_or_else(|| op_name.to_string());

    Ok(InferredOp {
        op_name: op_name.to_string(),
        entity_id,
        racket_symbol,
        meta: inferred_meta,
        inferred_from: peer_op,
        invariants_dropped: had_invariants,
        inference_kind: InferenceKind::TypeSymmetric { class },
    })
}

// ---------------------------------------------------------------------------
// Promotion — upgrade stubs to full ops
// ---------------------------------------------------------------------------

/// Run three-phase inference for all ops that lack metasignatures.
///
/// Phase 1: Op-symmetric inference (subtract from add via symmetric_partner)
/// Phase 2: Type-symmetric inference for remaining stubs (multiply from add
///          via same type_symmetry_class + category)
/// Phase 3: Op-symmetric replay — picks up pairs unblocked by phase 2
///          (divide from multiply, which now has a meta from phase 2)
///
/// Returns a list of successfully inferred ops.
pub fn promote_inferred_ops(
    registry: &mut OperationRegistry,
    facts: &FactPackIndex,
) -> Vec<InferredOp> {
    let mut inferred = Vec::new();

    // Helper: collect current stubs (ops with no meta)
    let collect_stubs = |reg: &OperationRegistry| -> Vec<String> {
        reg.poly_op_names()
        .into_iter()
        .filter(|name| reg.get_poly(name)
                .map(|op| op.meta.is_none())
                .unwrap_or(false))
        .map(|s| s.to_string())
        .collect()
    };

    // Helper: promote an inferred op into the registry
    let promote = |inf: &InferredOp, reg: &mut OperationRegistry, snapshot: &OperationRegistry| {
        if let Some(existing) = snapshot.get_poly(&inf.op_name) {
            reg.register_poly_with_meta(
                &inf.op_name,
                existing.signature.clone(),
                existing.properties.clone(),
                &existing.description,
                Some(inf.meta.clone()),
            );
        }
    };

    // --- Phase 1: Op-symmetric inference ---
    let snapshot = build_racket_registry();
    let stubs = collect_stubs(registry);
    for stub_name in &stubs {
        match infer_symmetric_op(stub_name, &snapshot, facts) {
            Ok(inf) => {
                promote(&inf, registry, &snapshot);
                inferred.push(inf);
            }
            Err(_) => {}
        }
    }

    // --- Phase 2: Type-symmetric inference for remaining stubs ---
    // Use the registry (which now has phase 1 promotions) for peer lookup
    // Only try type-symmetric for stubs that don't have a symmetric partner
    // with a meta (those will be handled by phase 3 op-symmetric replay).
    let remaining_stubs = collect_stubs(registry);
    for stub_name in &remaining_stubs {
        // If this stub has a symmetric partner that already has meta,
        // skip it — phase 3 will handle it via op-symmetric inference.
        if infer_symmetric_op(stub_name, registry, facts).is_ok() {
            continue;
        }
        match infer_type_symmetric_op(stub_name, registry, facts) {
            Ok(inf) => {
                promote(&inf, registry, &snapshot);
                inferred.push(inf);
            }
            Err(_) => {}
        }
    }

    // --- Phase 3: Op-symmetric replay ---
    // Re-check stubs that might now be unblocked (e.g., divide from multiply)
    let still_stubs = collect_stubs(registry);
    for stub_name in &still_stubs {
        // Skip if already inferred in an earlier phase
        if inferred.iter().any(|i| i.op_name == *stub_name) {
            continue;
        }
        match infer_symmetric_op(stub_name, registry, facts) {
            Ok(inf) => {
                promote(&inf, registry, &snapshot);
                inferred.push(inf);
            }
            Err(_) => {}
        }
    }

    inferred
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    const RACKET_FACTS_YAML: &str = include_str!("../data/racket_facts.yaml");

    fn setup() -> (OperationRegistry, FactPackIndex) {
        let reg = build_racket_registry();
        let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();
        (reg, facts)
    }

    // --- Registry loading ---

    #[test]
    fn test_build_racket_registry() {
        let reg = build_racket_registry();
        assert!(reg.get_poly("add").is_some());
        assert!(reg.get_poly("subtract").is_some());
        assert!(reg.get_poly("cons").is_some());
        assert!(reg.get_poly("display").is_some());
        assert!(reg.get_poly("racket_map").is_some());
    }

    // --- Keyword map ---

    #[test]
    fn test_keyword_map_verbs() {
        let (_, facts) = setup();
        let map = load_keyword_map(&facts);

        assert_eq!(map.get("add"), Some(&"add".to_string()));
        assert_eq!(map.get("subtract"), Some(&"subtract".to_string()));
        assert_eq!(map.get("multiply"), Some(&"multiply".to_string()));
        assert_eq!(map.get("divide"), Some(&"divide".to_string()));
    }

    #[test]
    fn test_keyword_map_synonyms() {
        let (_, facts) = setup();
        let map = load_keyword_map(&facts);

        assert_eq!(map.get("plus"), Some(&"add".to_string()));
        assert_eq!(map.get("sum"), Some(&"add".to_string()));
        assert_eq!(map.get("minus"), Some(&"subtract".to_string()));
        assert_eq!(map.get("times"), Some(&"multiply".to_string()));
        assert_eq!(map.get("quotient"), Some(&"divide".to_string()));
    }

    #[test]
    fn test_keyword_map_symbols() {
        let (_, facts) = setup();
        let map = load_keyword_map(&facts);

        assert_eq!(map.get("+"), Some(&"add".to_string()));
        assert_eq!(map.get("-"), Some(&"subtract".to_string()));
        assert_eq!(map.get("*"), Some(&"multiply".to_string()));
        assert_eq!(map.get("/"), Some(&"divide".to_string()));
    }

    #[test]
    fn test_resolve_keyword() {
        let (_, facts) = setup();
        let map = load_keyword_map(&facts);

        assert_eq!(resolve_keyword("add", &map), Some("add".to_string()));
        assert_eq!(resolve_keyword("plus", &map), Some("add".to_string()));
        assert_eq!(resolve_keyword("minus", &map), Some("subtract".to_string()));
        assert_eq!(resolve_keyword("nonexistent", &map), None);
    }

    // --- Symmetric partner lookup ---

    #[test]
    fn test_find_symmetric_partner() {
        let (_, facts) = setup();

        assert_eq!(find_symmetric_partner("op_add", &facts), Some("op_subtract".to_string()));
        assert_eq!(find_symmetric_partner("op_subtract", &facts), Some("op_add".to_string()));
        assert_eq!(find_symmetric_partner("op_multiply", &facts), Some("op_divide".to_string()));
        assert_eq!(find_symmetric_partner("op_divide", &facts), Some("op_multiply".to_string()));
    }

    // --- Core inference ---

    #[test]
    fn test_infer_subtract_from_add() {
        let (reg, facts) = setup();
        let inferred = infer_symmetric_op("subtract", &reg, &facts).unwrap();

        assert_eq!(inferred.op_name, "subtract");
        assert_eq!(inferred.racket_symbol, "-");
        assert_eq!(inferred.inferred_from, "add");

        // Type structure transferred
        assert_eq!(inferred.meta.params.len(), 2);
        assert_eq!(inferred.meta.params[0].name, "x");
        assert_eq!(inferred.meta.params[0].type_name, "Number");
        assert_eq!(inferred.meta.params[1].name, "y");
        assert_eq!(inferred.meta.params[1].type_name, "Number");
        assert_eq!(inferred.meta.return_type, "Number");

        // Category and effects transferred
        assert_eq!(inferred.meta.category.as_deref(), Some("arithmetic"));
        assert_eq!(inferred.meta.effects.as_deref(), Some("none"));

        // Invariants NOT transferred
        assert!(inferred.meta.invariants.is_empty());
        assert!(inferred.invariants_dropped);
    }

    #[test]
    fn test_infer_divide_from_multiply() {
        let (reg, facts) = setup();

        // multiply doesn't have a meta in the ops pack (it's a stub too)
        // So inferring divide should fail with PartnerHasNoMeta
        let result = infer_symmetric_op("divide", &reg, &facts);
        assert!(result.is_err());
        match result.unwrap_err() {
            InferenceError::PartnerHasNoMeta { op, partner } => {
                assert_eq!(op, "divide");
                assert_eq!(partner, "multiply");
            }
            other => panic!("expected PartnerHasNoMeta, got: {:?}", other),
        }
    }

    #[test]
    fn test_infer_add_from_subtract() {
        let (reg, facts) = setup();

        // subtract doesn't have a meta (it's a stub), so inferring add
        // from subtract should fail
        // But wait — add DOES have a meta. The symmetric partner of add is subtract.
        // So this tests: can we infer add from subtract? No, because subtract has no meta.
        // But add already has a meta, so this is a no-op scenario.
        // Let's test that inferring add works (its partner subtract has no meta,
        // but add already has meta so it wouldn't be a stub).
        let result = infer_symmetric_op("add", &reg, &facts);
        // add's partner is subtract, which has no meta
        assert!(result.is_err());
    }

    #[test]
    fn test_infer_no_symmetric_partner() {
        let (reg, facts) = setup();

        // abs has no symmetric partner
        let result = infer_symmetric_op("abs", &reg, &facts);
        assert!(result.is_err());
        match result.unwrap_err() {
            InferenceError::NoSymmetricPartner(_) |
            InferenceError::UnknownEntity(_) => {} // either is acceptable
            other => panic!("expected NoSymmetricPartner or UnknownEntity, got: {:?}", other),
        }
    }

    #[test]
    fn test_infer_unknown_op() {
        let (reg, facts) = setup();
        let result = infer_symmetric_op("nonexistent", &reg, &facts);
        assert!(result.is_err());
        match result.unwrap_err() {
            InferenceError::UnknownEntity(name) => assert_eq!(name, "nonexistent"),
            other => panic!("expected UnknownEntity, got: {:?}", other),
        }
    }

    #[test]
    fn test_invariants_not_transferred() {
        let (reg, facts) = setup();
        let inferred = infer_symmetric_op("subtract", &reg, &facts).unwrap();

        // add has invariant "y>0 => x+y>x" — this must NOT appear on subtract
        assert!(inferred.meta.invariants.is_empty(),
            "invariants should not transfer to symmetric partner");
        assert!(inferred.invariants_dropped,
            "should flag that invariants were dropped");
    }

    // --- Promotion ---

    #[test]
    fn test_promote_inferred_ops() {
        let (mut reg, facts) = setup();

        // Before promotion, subtract has no meta
        assert!(reg.get_poly("subtract").unwrap().meta.is_none());

        let inferred = promote_inferred_ops(&mut reg, &facts);

        // subtract should have been inferred from add
        let sub_inferred: Vec<_> = inferred.iter()
            .filter(|i| i.op_name == "subtract")
            .collect();
        assert_eq!(sub_inferred.len(), 1);
        assert_eq!(sub_inferred[0].inferred_from, "add");

        // After promotion, the registry should have a new entry with meta
        // (promote_inferred_ops re-registers, so there may be duplicates;
        // the last one wins on lookup)
        let all_ops = reg.poly_op_names();
        assert!(all_ops.contains(&"subtract"));
    }

    #[test]
    fn test_promote_preserves_existing_meta() {
        let (mut reg, facts) = setup();

        // add already has meta — promotion should not touch it
        let add_meta_before = reg.get_poly("add").unwrap().meta.clone();
        promote_inferred_ops(&mut reg, &facts);
        let add_meta_after = reg.get_poly("add").unwrap().meta.clone();

        // add's meta should be unchanged (it's not a stub)
        assert!(add_meta_before.is_some());
        assert!(add_meta_after.is_some());
        assert_eq!(
            add_meta_before.unwrap().invariants,
            add_meta_after.unwrap().invariants
        );
    }

    // --- Entity helpers ---

    #[test]
    fn test_entity_to_op_name() {
        let (_, facts) = setup();
        assert_eq!(entity_to_op_name("op_add", &facts), Some("add".to_string()));
        assert_eq!(entity_to_op_name("op_subtract", &facts), Some("subtract".to_string()));
        assert_eq!(entity_to_op_name("nonexistent", &facts), None);
    }

    #[test]
    fn test_op_name_to_entity() {
        let (_, facts) = setup();
        assert_eq!(op_name_to_entity("add", &facts), Some("op_add".to_string()));
        assert_eq!(op_name_to_entity("subtract", &facts), Some("op_subtract".to_string()));
        assert_eq!(op_name_to_entity("nonexistent", &facts), None);
    }

    #[test]
    fn test_entity_to_racket_symbol() {
        let (_, facts) = setup();
        assert_eq!(entity_to_racket_symbol("op_add", &facts), Some("+".to_string()));
        assert_eq!(entity_to_racket_symbol("op_subtract", &facts), Some("-".to_string()));
        assert_eq!(entity_to_racket_symbol("op_multiply", &facts), Some("*".to_string()));
        assert_eq!(entity_to_racket_symbol("op_divide", &facts), Some("/".to_string()));
    }

    // --- Type-symmetric inference ---

    #[test]
    fn test_entity_type_symmetry_class() {
        let (_, facts) = setup();
        assert_eq!(entity_type_symmetry_class("op_add", &facts), Some("binop".to_string()));
        assert_eq!(entity_type_symmetry_class("op_multiply", &facts), Some("binop".to_string()));
        assert_eq!(entity_type_symmetry_class("nonexistent", &facts), None);
    }

    #[test]
    fn test_entity_category() {
        let (_, facts) = setup();
        assert_eq!(entity_category("op_add", &facts), Some("arithmetic".to_string()));
        assert_eq!(entity_category("op_multiply", &facts), Some("arithmetic".to_string()));
        assert_eq!(entity_category("nonexistent", &facts), None);
    }

    #[test]
    fn test_find_type_symmetric_peer_multiply_finds_add() {
        let (reg, facts) = setup();
        // multiply is a stub, add has meta — should find add
        let peer = find_type_symmetric_peer("multiply", &reg, &facts);
        assert_eq!(peer, Some("add".to_string()));
    }

    #[test]
    fn test_find_type_symmetric_peer_divide_finds_add() {
        let (reg, facts) = setup();
        // divide is a stub, add has meta — should find add (same class+category)
        let peer = find_type_symmetric_peer("divide", &reg, &facts);
        assert_eq!(peer, Some("add".to_string()));
    }

    #[test]
    fn test_find_type_symmetric_peer_abs_returns_none() {
        let (reg, facts) = setup();
        // abs has no type_symmetry_class in the fact pack
        let peer = find_type_symmetric_peer("abs", &reg, &facts);
        assert_eq!(peer, None);
    }

    #[test]
    fn test_find_type_symmetric_peer_nonexistent_returns_none() {
        let (reg, facts) = setup();
        let peer = find_type_symmetric_peer("nonexistent", &reg, &facts);
        assert_eq!(peer, None);
    }

    #[test]
    fn test_infer_type_symmetric_multiply_from_add() {
        let (reg, facts) = setup();
        let inferred = infer_type_symmetric_op("multiply", &reg, &facts).unwrap();

        assert_eq!(inferred.op_name, "multiply");
        assert_eq!(inferred.racket_symbol, "*");
        assert_eq!(inferred.inferred_from, "add");
        assert_eq!(inferred.meta.return_type, "Number");
        assert_eq!(inferred.meta.params.len(), 2);
        assert_eq!(inferred.meta.params[0].name, "x");
        assert_eq!(inferred.meta.params[0].type_name, "Number");
        assert_eq!(inferred.meta.category.as_deref(), Some("arithmetic"));
        assert_eq!(inferred.meta.effects.as_deref(), Some("none"));

        // Invariants dropped
        assert!(inferred.meta.invariants.is_empty());
        assert!(inferred.invariants_dropped);

        // Inference kind
        assert_eq!(inferred.inference_kind, InferenceKind::TypeSymmetric {
            class: "binop".to_string(),
        });
    }

    #[test]
    fn test_infer_type_symmetric_abs_fails() {
        let (reg, facts) = setup();
        let result = infer_type_symmetric_op("abs", &reg, &facts);
        assert!(result.is_err(), "abs has no type_symmetry_class");
    }

    #[test]
    fn test_three_phase_promotion_all_four_ops() {
        let (mut reg, facts) = setup();

        // Before: only add has meta
        assert!(reg.get_poly("add").unwrap().meta.is_some());
        assert!(reg.get_poly("subtract").unwrap().meta.is_none());
        assert!(reg.get_poly("multiply").unwrap().meta.is_none());
        assert!(reg.get_poly("divide").unwrap().meta.is_none());

        let inferred = promote_inferred_ops(&mut reg, &facts);

        // After: all four should have meta
        assert!(reg.get_poly("add").unwrap().meta.is_some(), "add should still have meta");
        assert!(reg.get_poly("subtract").unwrap().meta.is_some(), "subtract should have meta");
        assert!(reg.get_poly("multiply").unwrap().meta.is_some(), "multiply should have meta");
        assert!(reg.get_poly("divide").unwrap().meta.is_some(), "divide should have meta");

        // Check inference paths
        let sub = inferred.iter().find(|i| i.op_name == "subtract").unwrap();
        assert_eq!(sub.inference_kind, InferenceKind::OpSymmetric);
        assert_eq!(sub.inferred_from, "add");

        let mul = inferred.iter().find(|i| i.op_name == "multiply").unwrap();
        assert_eq!(mul.inference_kind, InferenceKind::TypeSymmetric {
            class: "binop".to_string(),
        });
        assert_eq!(mul.inferred_from, "add");

        let div = inferred.iter().find(|i| i.op_name == "divide").unwrap();
        assert_eq!(div.inference_kind, InferenceKind::OpSymmetric);
        assert_eq!(div.inferred_from, "multiply");
    }

    #[test]
    fn test_modulo_remains_stub_after_promotion() {
        let (mut reg, facts) = setup();
        promote_inferred_ops(&mut reg, &facts);
        // modulo has no symmetric partner and no type_symmetry_class
        assert!(reg.get_poly("modulo").unwrap().meta.is_none(),
            "modulo should remain a stub — no inference path");
    }
}
