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
    MetaParam, MetaSignature, OperationRegistry, PolyOpSignature, AlgebraicProperties,
    load_ops_pack_str,
};
use crate::type_expr::TypeExpr;

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
        type_params: partner_meta.type_params.clone(),
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
        type_params: peer_meta.type_params.clone(),
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

/// Collect all type variable names from a set of TypeExprs.
///
/// Walks the input and output types, collecting any `Var` nodes into a
/// deduplicated, sorted list.  This lets `meta_to_poly_signature` infer
/// type_params automatically when the MetaSignature doesn't declare them.
fn collect_type_vars(inputs: &[TypeExpr], output: &TypeExpr) -> Vec<String> {
    let mut vars = std::collections::BTreeSet::new();
    fn walk(expr: &TypeExpr, vars: &mut std::collections::BTreeSet<String>) {
        match expr {
            TypeExpr::Var(v) => { vars.insert(v.clone()); }
            TypeExpr::Constructor(_, args) => {
                for arg in args { walk(arg, vars); }
            }
            TypeExpr::Primitive(_) => {}
        }
    }
    for input in inputs { walk(input, &mut vars); }
    walk(output, &mut vars);
    vars.into_iter().collect()
}

/// Build a PolyOpSignature from a MetaSignature.
///
/// Converts the named, typed params into TypeExpr inputs and the return type
/// into a TypeExpr output.  Parses type strings like "List(a)" into proper
/// Constructor/Var TypeExprs (not flat Primitives).  Type parameters are
/// taken from the MetaSignature's `type_params` field; if that field is
/// empty, they are collected automatically from any Var nodes produced by
/// the parser (lowercase identifiers like `a`, `b`).
fn meta_to_poly_signature(meta: &MetaSignature) -> PolyOpSignature {
    let parse_or_prim = |s: &str| -> TypeExpr {
        TypeExpr::parse(s).unwrap_or_else(|_| TypeExpr::prim(s))
    };

    let inputs: Vec<TypeExpr> = meta.params.iter()
        .map(|p| parse_or_prim(&p.type_name))
        .collect();
    let output = parse_or_prim(&meta.return_type);

    let type_params = if !meta.type_params.is_empty() {
        meta.type_params.clone()
    } else {
        collect_type_vars(&inputs, &output)
    };

    PolyOpSignature::new(type_params, inputs, output)
}

/// Discover ops from the fact pack that are not yet in the registry.
///
/// Scans for entities with both `op_name` and `racket_symbol` properties.
/// For each entity whose op_name is not already registered, creates a
/// minimal registry entry (no metasig — inference fills that in later).
///
/// Returns the list of discovered op names.
fn discover_ops(
    registry: &mut OperationRegistry,
    facts: &FactPackIndex,
) -> Vec<String> {
    let mut discovered = Vec::new();

    // Collect op_name and racket_symbol for each entity
    let mut entity_op_name: HashMap<String, String> = HashMap::new();
    let mut entity_symbol: HashMap<String, String> = HashMap::new();
    let mut entity_desc: HashMap<String, String> = HashMap::new();

    for prop in &facts.pack.properties {
        if prop.key == "op_name" {
            entity_op_name.insert(prop.entity.clone(), prop.value.clone());
        }
        if prop.key == "racket_symbol" {
            entity_symbol.insert(prop.entity.clone(), prop.value.clone());
        }
    }
    for entity in &facts.pack.entities {
        entity_desc.insert(entity.id.clone(), entity.description.clone());
    }

    // Register any entity that has both op_name and racket_symbol but isn't in the registry
    for (entity_id, op_name) in &entity_op_name {
        if registry.get_poly(op_name).is_some() {
            continue; // Already registered (from ops pack)
        }
        let symbol = match entity_symbol.get(entity_id) {
            Some(s) => s.clone(),
            None => continue, // No symbol — can't emit code for this op
        };
        let desc = entity_desc.get(entity_id)
            .cloned()
            .unwrap_or_default();

        // Register with a placeholder signature — inference will provide the real one.
        // Use an empty mono signature; promote() will rebuild from the inferred meta.
        let placeholder_sig = PolyOpSignature::mono(vec![], TypeExpr::prim("Any"));
        registry.register_poly_with_meta(
            op_name,
            placeholder_sig,
            AlgebraicProperties::default(),
            desc,
            None, // No meta yet — inference provides it
        );
        registry.set_racket_symbol(op_name, symbol);
        discovered.push(op_name.clone());
    }

    discovered
}

/// Run four-phase inference for all ops that lack metasignatures.
///
/// Phase 0: Discovery — scan the fact pack for entities not in the registry
///          and register them as stubs (e.g., subtract, multiply, divide)
/// Phase 1: Op-symmetric inference (subtract ← add via symmetric_partner)
/// Phase 2: Type-symmetric inference for remaining stubs (multiply ← add
///          via same type_symmetry_class + category)
/// Phase 3: Op-symmetric replay — picks up pairs unblocked by phase 2
///          (divide ← multiply, which now has a meta from phase 2)
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
    let promote = |inf: &InferredOp, reg: &mut OperationRegistry| {
        // Build the signature from the inferred meta (works for both
        // ops-pack entries and discovered ops with placeholder signatures)
        let sig = meta_to_poly_signature(&inf.meta);

        // Try to preserve description from existing entry, fall back to empty
        let desc = reg.get_poly(&inf.op_name)
            .map(|e| e.description.clone())
            .unwrap_or_default();

        let props = reg.get_poly(&inf.op_name)
            .map(|e| e.properties.clone())
            .unwrap_or_default();

        reg.register_poly_with_meta(
            &inf.op_name,
            sig,
            props,
            desc,
            Some(inf.meta.clone()),
        );
        // Write the inferred Racket symbol into the registry
        reg.set_racket_symbol(&inf.op_name, inf.racket_symbol.clone());
    };

    // --- Phase 0: Discovery — register ops from fact pack not in registry ---
    discover_ops(registry, facts);

    // --- Phase 1: Op-symmetric inference ---
    let phase1: Vec<InferredOp> = {
        let stubs = collect_stubs(registry);
        stubs.iter()
            .filter_map(|name| infer_symmetric_op(name, registry, facts).ok())
            .collect()
    };
    for inf in phase1 {
        promote(&inf, registry);
        inferred.push(inf);
    }

    // --- Phase 2: Type-symmetric inference for remaining stubs ---
    // Process one at a time so each promotion is visible to the next stub.
    {
        let remaining = collect_stubs(registry);
        for stub_name in &remaining {
            // Skip if op-symmetric would work now (partner has meta from phase 1 or earlier in phase 2)
            if infer_symmetric_op(stub_name, registry, facts).is_ok() {
                continue;
            }
            if let Ok(inf) = infer_type_symmetric_op(stub_name, registry, facts) {
                promote(&inf, registry);
                inferred.push(inf);
            }
        }
    }

    // --- Phase 3: Op-symmetric replay ---
    let phase3: Vec<InferredOp> = {
        let still_stubs = collect_stubs(registry);
        let already: Vec<&str> = inferred.iter().map(|i| i.op_name.as_str()).collect();
        still_stubs.iter()
            .filter(|name| !already.contains(&name.as_str()))
            .filter_map(|name| infer_symmetric_op(name, registry, facts).ok())
            .collect()
    };
    for inf in phase3 {
        promote(&inf, registry);
        inferred.push(inf);
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
        // subtract, multiply, divide are discovered from fact pack, not in ops pack
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

        // Before promotion, subtract is not in the registry (discovered from fact pack)
        assert!(reg.get_poly("subtract").is_none());

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
        assert!(reg.get_poly("subtract").is_none(), "subtract should not be in ops pack");
        assert!(reg.get_poly("multiply").is_none(), "multiply should not be in ops pack");
        assert!(reg.get_poly("divide").is_none(), "divide should not be in ops pack");

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
        // multiply may come via type-symmetric from add or op-symmetric from divide
        // depending on iteration order. Both produce the same signature.
        assert!(mul.inferred_from == "add" || mul.inferred_from == "divide",
            "multiply should be inferred from add or divide, got: {}", mul.inferred_from);

        let div = inferred.iter().find(|i| i.op_name == "divide").unwrap();
        // divide may come via op-symmetric from multiply or type-symmetric from add
        // depending on iteration order. Both produce the same signature.
        assert!(div.inferred_from == "multiply" || div.inferred_from == "add",
            "divide should be inferred from multiply or add, got: {}", div.inferred_from);
    }

    #[test]
    fn test_modulo_remains_stub_after_promotion() {
        let (mut reg, facts) = setup();
        promote_inferred_ops(&mut reg, &facts);
        // modulo has no symmetric partner and no type_symmetry_class
        assert!(reg.get_poly("modulo").unwrap().meta.is_none(),
            "modulo should remain a stub — no inference path");
    }

    // -----------------------------------------------------------------------
    // Polymorphic meta_to_poly_signature tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_meta_to_poly_signature_monomorphic() {
        // Number params should produce Primitive types, no type_params
        let meta = MetaSignature {
            params: vec![
                MetaParam { name: "x".into(), type_name: "Number".into(), variadic: false },
                MetaParam { name: "y".into(), type_name: "Number".into(), variadic: false },
            ],
            type_params: vec![],
            return_type: "Number".into(),
            invariants: vec![],
            category: Some("arithmetic".into()),
            effects: Some("none".into()),
        };
        let sig = meta_to_poly_signature(&meta);
        assert!(sig.type_params.is_empty(), "monomorphic meta should have no type_params");
        assert_eq!(sig.inputs.len(), 2);
        assert_eq!(sig.inputs[0], TypeExpr::prim("Number"));
        assert_eq!(sig.output, TypeExpr::prim("Number"));
    }

    #[test]
    fn test_meta_to_poly_signature_polymorphic_list() {
        // List(a) params should produce Constructor + Var, with type_params=["a"]
        let meta = MetaSignature {
            params: vec![
                MetaParam { name: "x".into(), type_name: "a".into(), variadic: false },
                MetaParam { name: "lst".into(), type_name: "List(a)".into(), variadic: false },
            ],
            type_params: vec!["a".into()],
            return_type: "List(a)".into(),
            invariants: vec![],
            category: Some("list".into()),
            effects: Some("none".into()),
        };
        let sig = meta_to_poly_signature(&meta);
        assert_eq!(sig.type_params, vec!["a".to_string()]);
        assert_eq!(sig.inputs.len(), 2);
        assert_eq!(sig.inputs[0], TypeExpr::var("a"));
        assert_eq!(sig.inputs[1], TypeExpr::cons("List", vec![TypeExpr::var("a")]));
        assert_eq!(sig.output, TypeExpr::cons("List", vec![TypeExpr::var("a")]));
    }

    #[test]
    fn test_meta_to_poly_signature_nested_type() {
        // List(List(a)) should parse correctly and deduplicate type_params
        let meta = MetaSignature {
            params: vec![
                MetaParam { name: "lst".into(), type_name: "List(List(a))".into(), variadic: false },
            ],
            type_params: vec![], // auto-collect
            return_type: "List(a)".into(),
            invariants: vec![],
            category: Some("list".into()),
            effects: None,
        };
        let sig = meta_to_poly_signature(&meta);
        assert_eq!(sig.type_params, vec!["a".to_string()], "should auto-collect 'a' from nested types");
        let expected_input = TypeExpr::cons("List", vec![TypeExpr::cons("List", vec![TypeExpr::var("a")])]);
        assert_eq!(sig.inputs[0], expected_input);
        assert_eq!(sig.output, TypeExpr::cons("List", vec![TypeExpr::var("a")]));
    }
    // -----------------------------------------------------------------------
    // List op discovery + polymorphic inference tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_list_remove_discovered_and_inferred() {
        let (mut reg, facts) = setup();
        // remove is NOT in the ops pack — it must be discovered from the fact pack
        assert!(reg.get_poly("remove").is_none(),
            "remove should not be in ops pack");

        let inferred = promote_inferred_ops(&mut reg, &facts);

        // remove should now be in the registry with a meta
        let entry = reg.get_poly("remove").expect("remove should be registered after inference");
        assert!(entry.meta.is_some(), "remove should have a meta after inference");

        let meta = entry.meta.as_ref().unwrap();
        assert_eq!(meta.params.len(), 2, "remove takes 2 params (x, lst)");
        assert_eq!(meta.params[0].type_name, "a");
        assert_eq!(meta.params[1].type_name, "List(a)");
        assert_eq!(meta.return_type, "List(a)");
        assert_eq!(meta.category.as_deref(), Some("list"));
        assert_eq!(meta.type_params, vec!["a".to_string()]);

        // Verify the PolyOpSignature is polymorphic
        assert_eq!(entry.signature.type_params, vec!["a".to_string()]);
        assert_eq!(entry.signature.inputs[0], TypeExpr::var("a"));
        assert_eq!(entry.signature.inputs[1], TypeExpr::cons("List", vec![TypeExpr::var("a")]));
        assert_eq!(entry.signature.output, TypeExpr::cons("List", vec![TypeExpr::var("a")]));

        // Verify inference path
        let inf = inferred.iter().find(|i| i.op_name == "remove").unwrap();
        assert!(matches!(inf.inference_kind, InferenceKind::TypeSymmetric { ref class } if class == "list_elem_to_list"),
            "remove should be inferred via type-symmetric from cons");
        assert_eq!(inf.inferred_from, "cons");
        assert_eq!(inf.racket_symbol, "remove");
    }

    #[test]
    fn test_list_reverse_discovered_and_inferred() {
        let (mut reg, facts) = setup();
        assert!(reg.get_poly("list_reverse").is_none(),
            "list_reverse should not be in ops pack");

        let inferred = promote_inferred_ops(&mut reg, &facts);

        let entry = reg.get_poly("list_reverse").expect("list_reverse should be registered after inference");
        assert!(entry.meta.is_some(), "list_reverse should have a meta after inference");

        let meta = entry.meta.as_ref().unwrap();
        assert_eq!(meta.params.len(), 1, "list_reverse takes 1 param (lst)");
        assert_eq!(meta.params[0].type_name, "List(a)");
        assert_eq!(meta.return_type, "List(a)");
        assert_eq!(meta.category.as_deref(), Some("list"));
        assert_eq!(meta.type_params, vec!["a".to_string()]);

        // Verify the PolyOpSignature is polymorphic
        assert_eq!(entry.signature.type_params, vec!["a".to_string()]);
        assert_eq!(entry.signature.inputs[0], TypeExpr::cons("List", vec![TypeExpr::var("a")]));
        assert_eq!(entry.signature.output, TypeExpr::cons("List", vec![TypeExpr::var("a")]));

        // Verify inference path
        let inf = inferred.iter().find(|i| i.op_name == "list_reverse").unwrap();
        assert!(matches!(inf.inference_kind, InferenceKind::TypeSymmetric { ref class } if class == "list_to_list"),
            "list_reverse should be inferred via type-symmetric from cdr");
        assert_eq!(inf.inferred_from, "cdr");
        assert_eq!(inf.racket_symbol, "reverse");
    }

    #[test]
    fn test_list_ref_not_affected_by_inference() {
        let (mut reg, facts) = setup();
        // list_ref is in the ops pack with a unique shape — inference should not touch it
        let before = reg.get_poly("list_ref").map(|e| e.meta.is_some());
        promote_inferred_ops(&mut reg, &facts);
        let after = reg.get_poly("list_ref").map(|e| e.meta.is_some());
        assert_eq!(before, after, "list_ref should not be modified by inference");
    }

    // -----------------------------------------------------------------------
    // Comparison op discovery + inference tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_greater_than_discovered_and_inferred() {
        let (mut reg, facts) = setup();
        // greater_than is NOT in the ops pack — discovered from fact pack
        assert!(reg.get_poly("greater_than").is_none(),
            "greater_than should not be in ops pack");

        let inferred = promote_inferred_ops(&mut reg, &facts);

        let entry = reg.get_poly("greater_than")
            .expect("greater_than should be registered after inference");
        assert!(entry.meta.is_some(), "greater_than should have a meta after inference");

        let meta = entry.meta.as_ref().unwrap();
        assert_eq!(meta.params.len(), 2, "greater_than takes 2 params");
        assert_eq!(meta.params[0].type_name, "Number");
        assert_eq!(meta.params[1].type_name, "Number");
        assert_eq!(meta.return_type, "Boolean");
        assert_eq!(meta.category.as_deref(), Some("comparison"));

        // Verify racket symbol
        assert_eq!(entry.racket_symbol.as_deref(), Some(">"));

        // Verify inference path — op-symmetric from less_than
        let inf = inferred.iter().find(|i| i.op_name == "greater_than").unwrap();
        assert_eq!(inf.inference_kind, InferenceKind::OpSymmetric);
        assert_eq!(inf.inferred_from, "less_than");
    }

    #[test]
    fn test_less_than_or_equal_discovered_and_inferred() {
        let (mut reg, facts) = setup();
        assert!(reg.get_poly("less_than_or_equal").is_none(),
            "less_than_or_equal should not be in ops pack");

        let inferred = promote_inferred_ops(&mut reg, &facts);

        let entry = reg.get_poly("less_than_or_equal")
            .expect("less_than_or_equal should be registered after inference");
        assert!(entry.meta.is_some());

        let meta = entry.meta.as_ref().unwrap();
        assert_eq!(meta.params.len(), 2);
        assert_eq!(meta.params[0].type_name, "Number");
        assert_eq!(meta.params[1].type_name, "Number");
        assert_eq!(meta.return_type, "Boolean");
        assert_eq!(meta.category.as_deref(), Some("comparison"));

        assert_eq!(entry.racket_symbol.as_deref(), Some("<="));

        // Inferred via type-symmetric from less_than (class: comparison_binop)
        let inf = inferred.iter().find(|i| i.op_name == "less_than_or_equal").unwrap();
        assert!(matches!(inf.inference_kind, InferenceKind::TypeSymmetric { ref class } if class == "comparison_binop"),
            "lte should be inferred via type-symmetric");
    }

    #[test]
    fn test_greater_than_or_equal_discovered_and_inferred() {
        let (mut reg, facts) = setup();
        assert!(reg.get_poly("greater_than_or_equal").is_none());

        let _inferred = promote_inferred_ops(&mut reg, &facts);

        let entry = reg.get_poly("greater_than_or_equal")
            .expect("greater_than_or_equal should be registered after inference");
        assert!(entry.meta.is_some());

        let meta = entry.meta.as_ref().unwrap();
        assert_eq!(meta.return_type, "Boolean");
        assert_eq!(meta.category.as_deref(), Some("comparison"));
        assert_eq!(entry.racket_symbol.as_deref(), Some(">="));
    }

    #[test]
    fn test_equal_not_affected_by_comparison_inference() {
        let (mut reg, facts) = setup();
        // equal is in the ops pack with (Any, Any) → Boolean — NOT in comparison_binop class
        let before_meta = reg.get_poly("equal").map(|e| e.meta.is_some());
        promote_inferred_ops(&mut reg, &facts);
        let after_meta = reg.get_poly("equal").map(|e| e.meta.is_some());
        assert_eq!(before_meta, after_meta, "equal should not be modified by inference");
    }

    // -----------------------------------------------------------------------
    // String op discovery + inference tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_string_downcase_discovered_and_inferred() {
        let (mut reg, facts) = setup();
        assert!(reg.get_poly("string_downcase").is_none(),
            "string_downcase should not be in ops pack");

        let inferred = promote_inferred_ops(&mut reg, &facts);

        let entry = reg.get_poly("string_downcase")
            .expect("string_downcase should be registered after inference");
        assert!(entry.meta.is_some(), "string_downcase should have a meta after inference");

        let meta = entry.meta.as_ref().unwrap();
        assert_eq!(meta.params.len(), 1, "string_downcase takes 1 param");
        assert_eq!(meta.params[0].type_name, "String");
        assert_eq!(meta.return_type, "String");
        assert_eq!(meta.category.as_deref(), Some("string"));

        assert_eq!(entry.racket_symbol.as_deref(), Some("string-downcase"));

        // Inferred via type-symmetric from string_upcase (class: string_to_string)
        let inf = inferred.iter().find(|i| i.op_name == "string_downcase").unwrap();
        assert!(matches!(inf.inference_kind, InferenceKind::TypeSymmetric { ref class } if class == "string_to_string"),
            "string_downcase should be inferred via type-symmetric");
        assert_eq!(inf.inferred_from, "string_upcase");
    }

    #[test]
    fn test_string_length_not_affected_by_string_inference() {
        let (mut reg, facts) = setup();
        // string_length is (String) → Number — different return type, not in string_to_string
        let before_meta = reg.get_poly("string_length").map(|e| e.meta.is_some());
        promote_inferred_ops(&mut reg, &facts);
        let after_meta = reg.get_poly("string_length").map(|e| e.meta.is_some());
        assert_eq!(before_meta, after_meta, "string_length should not be modified by inference");
    }

}
