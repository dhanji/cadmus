use std::collections::{HashMap, HashSet};

use crate::generic_planner::PlanNode;
use crate::registry::OperationRegistry;
#[cfg(test)]
use crate::registry::TypeId;

// ---------------------------------------------------------------------------
// Plan canonicalization — normalize plans using algebraic properties
// ---------------------------------------------------------------------------

/// Canonicalize a plan tree using algebraic properties from the registry.
/// Applies in order:
/// 1. Flatten associative nodes
/// 2. Sort commutative operands
/// 3. Remove identity elements
/// 4. Collapse absorbing elements
/// 5. Deduplicate idempotent operands
/// 6. Apply rewrite rules until fixpoint
pub fn canonicalize(node: &PlanNode, registry: &OperationRegistry) -> PlanNode {
    let mut current = node.clone();
    let mut prev_hash = String::new();

    // Iterate until fixpoint (plan stops changing)
    for _ in 0..100 {
        current = canonicalize_once(&current, registry);
        current = apply_rewrites(&current, registry);
        let new_hash = plan_fingerprint(&current);
        if new_hash == prev_hash {
            break;
        }
        prev_hash = new_hash;
    }

    current
}

fn canonicalize_once(node: &PlanNode, registry: &OperationRegistry) -> PlanNode {
    match node {
        PlanNode::Leaf { .. } => node.clone(),
        PlanNode::Op { op_name, output_type, children } => {
            // First, recursively canonicalize children
            let mut new_children: Vec<PlanNode> = children
                .iter()
                .map(|c| canonicalize_once(c, registry))
                .collect();

            let props = registry.properties(op_name);

            if let Some(props) = props {
                // 1. Flatten associative: if child is same op, pull its children up
                if props.associative {
                    new_children = flatten_associative(op_name, &new_children);
                }

                // 2. Sort commutative: sort children by stable fingerprint
                if props.commutative {
                    new_children.sort_by(|a, b| {
                        plan_fingerprint(a).cmp(&plan_fingerprint(b))
                    });
                }

                // 3. Remove identity elements
                if let Some(ref identity) = props.identity {
                    new_children = remove_identity(&new_children, identity);
                    // If all children were identity, return identity leaf
                    if new_children.is_empty() {
                        return PlanNode::Leaf {
                            key: identity.clone(),
                            output_type: output_type.clone(),
                        };
                    }
                    // If only one child remains, unwrap
                    if new_children.len() == 1 {
                        return new_children.into_iter().next().unwrap();
                    }
                }

                // 4. Collapse absorbing elements
                if let Some(ref absorbing) = props.absorbing {
                    if has_absorbing(&new_children, absorbing) {
                        return PlanNode::Leaf {
                            key: absorbing.clone(),
                            output_type: output_type.clone(),
                        };
                    }
                }

                // 5. Deduplicate idempotent operands
                if props.idempotent {
                    new_children = dedup_idempotent(&new_children);
                    if new_children.len() == 1 {
                        return new_children.into_iter().next().unwrap();
                    }
                }
            }

            PlanNode::Op {
                op_name: op_name.clone(),
                output_type: output_type.clone(),
                children: new_children,
            }
        }
    }
}

/// Flatten nested applications of the same associative operation.
/// add(add(1, 2), 3) → add(1, 2, 3)
fn flatten_associative(op_name: &str, children: &[PlanNode]) -> Vec<PlanNode> {
    let mut result = Vec::new();
    for child in children {
        match child {
            PlanNode::Op {
                op_name: child_op,
                children: grandchildren,
                ..
            } if child_op == op_name => {
                // Pull grandchildren up
                result.extend(grandchildren.iter().cloned());
            }
            _ => result.push(child.clone()),
        }
    }
    result
}

/// Remove children that are identity elements.
fn remove_identity(children: &[PlanNode], identity: &str) -> Vec<PlanNode> {
    children
        .iter()
        .filter(|c| !is_literal_with_key(c, identity))
        .cloned()
        .collect()
}

/// Check if any child is an absorbing element.
fn has_absorbing(children: &[PlanNode], absorbing: &str) -> bool {
    children.iter().any(|c| is_literal_with_key(c, absorbing))
}

/// Deduplicate children with identical fingerprints (for idempotent ops).
fn dedup_idempotent(children: &[PlanNode]) -> Vec<PlanNode> {
    let mut seen = HashSet::new();
    let mut result = Vec::new();
    for child in children {
        let fp = plan_fingerprint(child);
        if seen.insert(fp) {
            result.push(child.clone());
        }
    }
    result
}

fn is_literal_with_key(node: &PlanNode, key: &str) -> bool {
    matches!(node, PlanNode::Leaf { key: k, .. } if k == key)
}

// ---------------------------------------------------------------------------
// Rewrite rules — apply until fixpoint
// ---------------------------------------------------------------------------

fn apply_rewrites(node: &PlanNode, registry: &OperationRegistry) -> PlanNode {
    match node {
        PlanNode::Leaf { .. } => node.clone(),
        PlanNode::Op { op_name, output_type, children } => {
            // Recursively apply to children first
            let new_children: Vec<PlanNode> = children
                .iter()
                .map(|c| apply_rewrites(c, registry))
                .collect();

            // Check if any rewrite rule matches this node
            let rules = registry.all_rewrite_rules();
            for rule in &rules {
                if rule.op_name != *op_name {
                    continue;
                }
                // Check if input types match
                if rule.match_inputs.len() != new_children.len() {
                    continue;
                }
                let types_match = rule.match_inputs.iter().zip(new_children.iter()).all(
                    |(expected_type, child)| *child.output_type() == *expected_type,
                );
                if types_match {
                    // Apply the rewrite: look up the replacement op
                    if let Some(replacement) = registry.get(&rule.replacement_op) {
                        return PlanNode::Op {
                            op_name: rule.replacement_op.clone(),
                            output_type: replacement.signature.output.clone(),
                            children: new_children,
                        };
                    }
                }
            }

            PlanNode::Op {
                op_name: op_name.clone(),
                output_type: output_type.clone(),
                children: new_children,
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Plan fingerprinting — stable string representation for comparison
// ---------------------------------------------------------------------------

/// Produce a stable string fingerprint of a plan tree.
/// Two plans with the same fingerprint are structurally identical.
pub fn plan_fingerprint(node: &PlanNode) -> String {
    match node {
        PlanNode::Leaf { key, output_type } => {
            format!("L({},{})", key, output_type)
        }
        PlanNode::Op { op_name, output_type, children } => {
            let child_fps: Vec<String> = children.iter().map(plan_fingerprint).collect();
            format!("O({},{},{})", op_name, output_type, child_fps.join(";"))
        }
    }
}

// ---------------------------------------------------------------------------
// Plan deduplication — collapse equivalent plans
// ---------------------------------------------------------------------------

/// Given a set of candidate plans, deduplicate by canonical fingerprint.
/// Returns only unique plans (after canonicalization).
pub fn dedup_plans(plans: Vec<PlanNode>, registry: &OperationRegistry) -> Vec<PlanNode> {
    let mut seen = HashSet::new();
    let mut unique = Vec::new();

    for plan in plans {
        let canonical = canonicalize(&plan, registry);
        let fp = plan_fingerprint(&canonical);
        if seen.insert(fp) {
            unique.push(canonical);
        }
    }

    unique
}

// ---------------------------------------------------------------------------
// Inference rules — derive new facts from existing relations
// ---------------------------------------------------------------------------

/// A relation between two entities (e.g., "A > B").
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Fact {
    pub relation: String,
    pub left: String,
    pub right: String,
}

impl Fact {
    pub fn new(relation: impl Into<String>, left: impl Into<String>, right: impl Into<String>) -> Self {
        Self {
            relation: relation.into(),
            left: left.into(),
            right: right.into(),
        }
    }
}

impl std::fmt::Display for Fact {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({}, {})", self.relation, self.left, self.right)
    }
}

/// An inference rule that can derive new facts from existing ones.
#[derive(Debug, Clone)]
pub struct InferenceRule {
    /// Human-readable name
    pub name: String,
    /// The relation this rule applies to
    pub relation: String,
    /// The type of inference
    pub kind: InferenceKind,
}

/// Types of inference supported.
#[derive(Debug, Clone)]
pub enum InferenceKind {
    /// If R(A,B) and R(B,C) then R(A,C)
    Transitive,
    /// If R(A,B) then R(B,A)
    Symmetric,
    /// R(A,A) for all A
    Reflexive,
}

/// Result of running inference: derived facts and any detected conflicts.
#[derive(Debug, Clone)]
pub struct InferenceResult {
    /// All facts (original + derived)
    pub facts: Vec<Fact>,
    /// Facts that were derived (not in the original set)
    pub derived: Vec<Fact>,
    /// Conflicts detected (e.g., A>B and B>A for a strict order)
    pub conflicts: Vec<String>,
}

/// Run inference rules on a set of facts.
/// Returns all derived facts and any conflicts detected.
pub fn infer(facts: &[Fact], rules: &[InferenceRule]) -> InferenceResult {
    let mut all_facts: HashSet<Fact> = facts.iter().cloned().collect();
    let mut derived: Vec<Fact> = Vec::new();
    let mut conflicts: Vec<String> = Vec::new();

    // Group facts by relation
    let mut by_relation: HashMap<String, Vec<&Fact>> = HashMap::new();
    for fact in facts {
        by_relation
            .entry(fact.relation.clone())
            .or_default()
            .push(fact);
    }

    // Apply rules iteratively until fixpoint
    let mut changed = true;
    let mut iterations = 0;
    while changed && iterations < 100 {
        changed = false;
        iterations += 1;

        for rule in rules {
            let relation_facts: Vec<Fact> = all_facts
                .iter()
                .filter(|f| f.relation == rule.relation)
                .cloned()
                .collect();

            match &rule.kind {
                InferenceKind::Transitive => {
                    // For each pair (A,B) and (B,C), derive (A,C)
                    for f1 in &relation_facts {
                        for f2 in &relation_facts {
                            if f1.right == f2.left && f1.left != f2.right {
                                let new_fact = Fact::new(
                                    &rule.relation,
                                    &f1.left,
                                    &f2.right,
                                );
                                if all_facts.insert(new_fact.clone()) {
                                    derived.push(new_fact);
                                    changed = true;
                                }
                            }
                        }
                    }
                }
                InferenceKind::Symmetric => {
                    for f in &relation_facts {
                        let new_fact = Fact::new(&rule.relation, &f.right, &f.left);
                        if all_facts.insert(new_fact.clone()) {
                            derived.push(new_fact);
                            changed = true;
                        }
                    }
                }
                InferenceKind::Reflexive => {
                    // Collect all entities mentioned in this relation
                    let mut entities = HashSet::new();
                    for f in &relation_facts {
                        entities.insert(f.left.clone());
                        entities.insert(f.right.clone());
                    }
                    for entity in &entities {
                        let new_fact = Fact::new(&rule.relation, entity, entity);
                        if all_facts.insert(new_fact.clone()) {
                            derived.push(new_fact);
                            changed = true;
                        }
                    }
                }
            }
        }
    }

    // Detect contradictions: for strict orders, check if both A>B and B>A exist
    let strict_relations: HashSet<&str> = rules
        .iter()
        .filter(|r| matches!(r.kind, InferenceKind::Transitive))
        .map(|r| r.relation.as_str())
        .collect();

    for fact in &all_facts {
        if strict_relations.contains(fact.relation.as_str()) {
            let inverse = Fact::new(&fact.relation, &fact.right, &fact.left);
            if fact.left != fact.right && all_facts.contains(&inverse) {
                let msg = format!(
                    "Contradiction: {}({}, {}) and {}({}, {})",
                    fact.relation, fact.left, fact.right,
                    fact.relation, fact.right, fact.left,
                );
                if !conflicts.contains(&msg) {
                    // Avoid duplicate conflict messages
                    let inverse_msg = format!(
                        "Contradiction: {}({}, {}) and {}({}, {})",
                        fact.relation, fact.right, fact.left,
                        fact.relation, fact.left, fact.right,
                    );
                    if !conflicts.contains(&inverse_msg) {
                        conflicts.push(msg);
                    }
                }
            }
        }
    }

    InferenceResult {
        facts: all_facts.into_iter().collect(),
        derived,
        conflicts,
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::registry::{AlgebraicProperties, OpSignature, OperationRegistry, RewriteRule};

    fn noop_exec() -> crate::registry::ExecFn {
        Box::new(|_| Ok("noop".into()))
    }

    // --- Canonicalization tests ---

    #[test]
    fn test_commutative_sort() {
        let mut reg = OperationRegistry::new();
        reg.register(
            "add",
            OpSignature::new(
                vec![TypeId::new("Num"), TypeId::new("Num")],
                TypeId::new("Num"),
            ),
            AlgebraicProperties::commutative(),
            noop_exec(),
        );

        // add(3, 2) — children in "wrong" order
        let plan_a = PlanNode::Op {
            op_name: "add".into(),
            output_type: TypeId::new("Num"),
            children: vec![
                PlanNode::Leaf { key: "3".into(), output_type: TypeId::new("Num") },
                PlanNode::Leaf { key: "2".into(), output_type: TypeId::new("Num") },
            ],
        };

        // add(2, 3) — children in "right" order
        let plan_b = PlanNode::Op {
            op_name: "add".into(),
            output_type: TypeId::new("Num"),
            children: vec![
                PlanNode::Leaf { key: "2".into(), output_type: TypeId::new("Num") },
                PlanNode::Leaf { key: "3".into(), output_type: TypeId::new("Num") },
            ],
        };

        let canon_a = canonicalize(&plan_a, &reg);
        let canon_b = canonicalize(&plan_b, &reg);

        assert_eq!(
            plan_fingerprint(&canon_a),
            plan_fingerprint(&canon_b),
            "add(2,3) and add(3,2) should canonicalize to the same plan"
        );
    }

    #[test]
    fn test_non_commutative_preserves_order() {
        let mut reg = OperationRegistry::new();
        reg.register(
            "sub",
            OpSignature::new(
                vec![TypeId::new("Num"), TypeId::new("Num")],
                TypeId::new("Num"),
            ),
            AlgebraicProperties::none(), // NOT commutative
            noop_exec(),
        );

        let plan = PlanNode::Op {
            op_name: "sub".into(),
            output_type: TypeId::new("Num"),
            children: vec![
                PlanNode::Leaf { key: "5".into(), output_type: TypeId::new("Num") },
                PlanNode::Leaf { key: "3".into(), output_type: TypeId::new("Num") },
            ],
        };

        let canonical = canonicalize(&plan, &reg);

        // Order should be preserved: sub(5, 3) not sub(3, 5)
        match &canonical {
            PlanNode::Op { children, .. } => {
                match (&children[0], &children[1]) {
                    (PlanNode::Leaf { key: k0, .. }, PlanNode::Leaf { key: k1, .. }) => {
                        assert_eq!(k0, "5");
                        assert_eq!(k1, "3");
                    }
                    _ => panic!("expected two Leaf children"),
                }
            }
            _ => panic!("expected Op node"),
        }
    }

    #[test]
    fn test_associative_flatten() {
        let mut reg = OperationRegistry::new();
        reg.register(
            "add",
            OpSignature::new(
                vec![TypeId::new("Num"), TypeId::new("Num")],
                TypeId::new("Num"),
            ),
            AlgebraicProperties::associative(),
            noop_exec(),
        );

        // add(add(1, 2), 3)
        let plan = PlanNode::Op {
            op_name: "add".into(),
            output_type: TypeId::new("Num"),
            children: vec![
                PlanNode::Op {
                    op_name: "add".into(),
                    output_type: TypeId::new("Num"),
                    children: vec![
                        PlanNode::Leaf { key: "1".into(), output_type: TypeId::new("Num") },
                        PlanNode::Leaf { key: "2".into(), output_type: TypeId::new("Num") },
                    ],
                },
                PlanNode::Leaf { key: "3".into(), output_type: TypeId::new("Num") },
            ],
        };

        let canonical = canonicalize(&plan, &reg);

        // Should flatten to add(1, 2, 3)
        match &canonical {
            PlanNode::Op { op_name, children, .. } => {
                assert_eq!(op_name, "add");
                assert_eq!(children.len(), 3, "should flatten to 3 children");
                let keys: Vec<&str> = children.iter().map(|c| match c {
                    PlanNode::Leaf { key, .. } => key.as_str(),
                    _ => panic!("expected Leaf"),
                }).collect();
                assert_eq!(keys, vec!["1", "2", "3"]);
            }
            _ => panic!("expected Op node"),
        }
    }

    #[test]
    fn test_identity_removal() {
        let mut reg = OperationRegistry::new();
        reg.register(
            "add",
            OpSignature::new(
                vec![TypeId::new("Num"), TypeId::new("Num")],
                TypeId::new("Num"),
            ),
            AlgebraicProperties {
                commutative: false,
                associative: false,
                identity: Some("0".to_string()),
                absorbing: None,
                idempotent: false,
            },
            noop_exec(),
        );

        // add(x, 0) → x
        let plan = PlanNode::Op {
            op_name: "add".into(),
            output_type: TypeId::new("Num"),
            children: vec![
                PlanNode::Leaf { key: "x".into(), output_type: TypeId::new("Num") },
                PlanNode::Leaf { key: "0".into(), output_type: TypeId::new("Num") },
            ],
        };

        let canonical = canonicalize(&plan, &reg);

        // Should reduce to just Leaf("x")
        match &canonical {
            PlanNode::Leaf { key, .. } => assert_eq!(key, "x"),
            other => panic!("expected Leaf(x), got: {:?}", other),
        }
    }

    #[test]
    fn test_absorbing_element() {
        let mut reg = OperationRegistry::new();
        reg.register(
            "mul",
            OpSignature::new(
                vec![TypeId::new("Num"), TypeId::new("Num")],
                TypeId::new("Num"),
            ),
            AlgebraicProperties {
                commutative: false,
                associative: false,
                identity: None,
                absorbing: Some("0".to_string()),
                idempotent: false,
            },
            noop_exec(),
        );

        // mul(x, 0) → 0
        let plan = PlanNode::Op {
            op_name: "mul".into(),
            output_type: TypeId::new("Num"),
            children: vec![
                PlanNode::Leaf { key: "x".into(), output_type: TypeId::new("Num") },
                PlanNode::Leaf { key: "0".into(), output_type: TypeId::new("Num") },
            ],
        };

        let canonical = canonicalize(&plan, &reg);

        match &canonical {
            PlanNode::Leaf { key, .. } => assert_eq!(key, "0"),
            other => panic!("expected Leaf(0), got: {:?}", other),
        }
    }

    #[test]
    fn test_idempotent_dedup() {
        let mut reg = OperationRegistry::new();
        reg.register(
            "union",
            OpSignature::new(
                vec![TypeId::new("Set"), TypeId::new("Set")],
                TypeId::new("Set"),
            ),
            AlgebraicProperties {
                commutative: false,
                associative: true,
                identity: None,
                absorbing: None,
                idempotent: true,
            },
            noop_exec(),
        );

        // union(a, a) → a
        let plan = PlanNode::Op {
            op_name: "union".into(),
            output_type: TypeId::new("Set"),
            children: vec![
                PlanNode::Leaf { key: "a".into(), output_type: TypeId::new("Set") },
                PlanNode::Leaf { key: "a".into(), output_type: TypeId::new("Set") },
            ],
        };

        let canonical = canonicalize(&plan, &reg);

        match &canonical {
            PlanNode::Leaf { key, .. } => assert_eq!(key, "a"),
            other => panic!("expected Leaf(a), got: {:?}", other),
        }
    }

    #[test]
    fn test_rewrite_rule_no_match() {
        let mut reg = OperationRegistry::new();
        reg.register(
            "add",
            OpSignature::new(
                vec![TypeId::new("Num"), TypeId::new("Num")],
                TypeId::new("Num"),
            ),
            AlgebraicProperties::none(),
            noop_exec(),
        );
        reg.register_with_rewrites(
            "special",
            OpSignature::new(
                vec![TypeId::new("Str"), TypeId::new("Str")],
                TypeId::new("Str"),
            ),
            AlgebraicProperties::none(),
            vec![RewriteRule {
                name: "special_rewrite".into(),
                op_name: "special".into(),
                match_inputs: vec![TypeId::new("Str"), TypeId::new("Num")],
                replacement_op: "add".into(),
                replacement_inputs: vec![TypeId::new("Num"), TypeId::new("Num")],
            }],
            noop_exec(),
        );

        // add(1, 2) — the rewrite is for "special", not "add"
        let plan = PlanNode::Op {
            op_name: "add".into(),
            output_type: TypeId::new("Num"),
            children: vec![
                PlanNode::Leaf { key: "1".into(), output_type: TypeId::new("Num") },
                PlanNode::Leaf { key: "2".into(), output_type: TypeId::new("Num") },
            ],
        };

        let canonical = canonicalize(&plan, &reg);

        // Should be unchanged
        match &canonical {
            PlanNode::Op { op_name, children, .. } => {
                assert_eq!(op_name, "add");
                assert_eq!(children.len(), 2);
            }
            _ => panic!("expected unchanged Op"),
        }
    }

    #[test]
    fn test_plan_deduplication() {
        let mut reg = OperationRegistry::new();
        reg.register(
            "add",
            OpSignature::new(
                vec![TypeId::new("Num"), TypeId::new("Num")],
                TypeId::new("Num"),
            ),
            AlgebraicProperties::commutative(),
            noop_exec(),
        );

        let plan_a = PlanNode::Op {
            op_name: "add".into(),
            output_type: TypeId::new("Num"),
            children: vec![
                PlanNode::Leaf { key: "3".into(), output_type: TypeId::new("Num") },
                PlanNode::Leaf { key: "2".into(), output_type: TypeId::new("Num") },
            ],
        };

        let plan_b = PlanNode::Op {
            op_name: "add".into(),
            output_type: TypeId::new("Num"),
            children: vec![
                PlanNode::Leaf { key: "2".into(), output_type: TypeId::new("Num") },
                PlanNode::Leaf { key: "3".into(), output_type: TypeId::new("Num") },
            ],
        };

        let unique = dedup_plans(vec![plan_a, plan_b], &reg);
        assert_eq!(unique.len(), 1, "commutative variants should dedup to 1");
    }

    #[test]
    fn test_deep_associative_flatten() {
        let mut reg = OperationRegistry::new();
        reg.register(
            "add",
            OpSignature::new(
                vec![TypeId::new("Num"), TypeId::new("Num")],
                TypeId::new("Num"),
            ),
            AlgebraicProperties::associative(),
            noop_exec(),
        );

        // add(add(add(1, 2), 3), 4) — deeply nested
        let plan = PlanNode::Op {
            op_name: "add".into(),
            output_type: TypeId::new("Num"),
            children: vec![
                PlanNode::Op {
                    op_name: "add".into(),
                    output_type: TypeId::new("Num"),
                    children: vec![
                        PlanNode::Op {
                            op_name: "add".into(),
                            output_type: TypeId::new("Num"),
                            children: vec![
                                PlanNode::Leaf { key: "1".into(), output_type: TypeId::new("Num") },
                                PlanNode::Leaf { key: "2".into(), output_type: TypeId::new("Num") },
                            ],
                        },
                        PlanNode::Leaf { key: "3".into(), output_type: TypeId::new("Num") },
                    ],
                },
                PlanNode::Leaf { key: "4".into(), output_type: TypeId::new("Num") },
            ],
        };

        let canonical = canonicalize(&plan, &reg);

        match &canonical {
            PlanNode::Op { children, .. } => {
                assert_eq!(children.len(), 4, "should flatten to 4 children");
            }
            _ => panic!("expected Op"),
        }
    }

    #[test]
    fn test_comm_assoc_combined() {
        let mut reg = OperationRegistry::new();
        reg.register(
            "add",
            OpSignature::new(
                vec![TypeId::new("Num"), TypeId::new("Num")],
                TypeId::new("Num"),
            ),
            AlgebraicProperties::comm_assoc(),
            noop_exec(),
        );

        // add(3, add(1, 2)) and add(add(2, 1), 3) should canonicalize the same
        let plan_a = PlanNode::Op {
            op_name: "add".into(),
            output_type: TypeId::new("Num"),
            children: vec![
                PlanNode::Leaf { key: "3".into(), output_type: TypeId::new("Num") },
                PlanNode::Op {
                    op_name: "add".into(),
                    output_type: TypeId::new("Num"),
                    children: vec![
                        PlanNode::Leaf { key: "1".into(), output_type: TypeId::new("Num") },
                        PlanNode::Leaf { key: "2".into(), output_type: TypeId::new("Num") },
                    ],
                },
            ],
        };

        let plan_b = PlanNode::Op {
            op_name: "add".into(),
            output_type: TypeId::new("Num"),
            children: vec![
                PlanNode::Op {
                    op_name: "add".into(),
                    output_type: TypeId::new("Num"),
                    children: vec![
                        PlanNode::Leaf { key: "2".into(), output_type: TypeId::new("Num") },
                        PlanNode::Leaf { key: "1".into(), output_type: TypeId::new("Num") },
                    ],
                },
                PlanNode::Leaf { key: "3".into(), output_type: TypeId::new("Num") },
            ],
        };

        let canon_a = canonicalize(&plan_a, &reg);
        let canon_b = canonicalize(&plan_b, &reg);

        assert_eq!(
            plan_fingerprint(&canon_a),
            plan_fingerprint(&canon_b),
            "comm+assoc should make these equivalent"
        );
    }

    // --- Inference tests ---

    #[test]
    fn test_transitive_inference() {
        let facts = vec![
            Fact::new("greater_than", "A", "B"),
            Fact::new("greater_than", "B", "C"),
        ];
        let rules = vec![InferenceRule {
            name: "gt_transitive".into(),
            relation: "greater_than".into(),
            kind: InferenceKind::Transitive,
        }];

        let result = infer(&facts, &rules);

        let has_a_gt_c = result.facts.iter().any(|f| {
            f.relation == "greater_than" && f.left == "A" && f.right == "C"
        });
        assert!(has_a_gt_c, "should derive A > C from A > B and B > C");
        assert!(!result.derived.is_empty());
    }

    #[test]
    fn test_non_transitive_no_inference() {
        let facts = vec![
            Fact::new("likes", "A", "B"),
            Fact::new("likes", "B", "C"),
        ];
        // No rules declared for "likes"
        let rules = vec![InferenceRule {
            name: "gt_transitive".into(),
            relation: "greater_than".into(), // different relation
            kind: InferenceKind::Transitive,
        }];

        let result = infer(&facts, &rules);
        assert!(result.derived.is_empty(), "no inference should be made for unrelated relation");
    }

    #[test]
    fn test_transitive_chain_3_steps() {
        let facts = vec![
            Fact::new("gt", "A", "B"),
            Fact::new("gt", "B", "C"),
            Fact::new("gt", "C", "D"),
        ];
        let rules = vec![InferenceRule {
            name: "gt_trans".into(),
            relation: "gt".into(),
            kind: InferenceKind::Transitive,
        }];

        let result = infer(&facts, &rules);

        let has_a_gt_d = result.facts.iter().any(|f| {
            f.relation == "gt" && f.left == "A" && f.right == "D"
        });
        assert!(has_a_gt_d, "should derive A > D from chain A > B > C > D");

        // Should also have A>C and B>D
        let has_a_gt_c = result.facts.iter().any(|f| f.left == "A" && f.right == "C");
        let has_b_gt_d = result.facts.iter().any(|f| f.left == "B" && f.right == "D");
        assert!(has_a_gt_c);
        assert!(has_b_gt_d);
    }

    #[test]
    fn test_contradiction_detection() {
        let facts = vec![
            Fact::new("gt", "A", "B"),
            Fact::new("gt", "B", "A"),
        ];
        let rules = vec![InferenceRule {
            name: "gt_trans".into(),
            relation: "gt".into(),
            kind: InferenceKind::Transitive,
        }];

        let result = infer(&facts, &rules);
        assert!(!result.conflicts.is_empty(), "should detect contradiction A>B and B>A");
    }

    #[test]
    fn test_symmetric_inference() {
        let facts = vec![
            Fact::new("sibling", "A", "B"),
        ];
        let rules = vec![InferenceRule {
            name: "sibling_sym".into(),
            relation: "sibling".into(),
            kind: InferenceKind::Symmetric,
        }];

        let result = infer(&facts, &rules);
        let has_b_a = result.facts.iter().any(|f| f.left == "B" && f.right == "A");
        assert!(has_b_a, "symmetric: sibling(A,B) → sibling(B,A)");
    }

    #[test]
    fn test_reflexive_inference() {
        let facts = vec![
            Fact::new("eq", "A", "B"),
        ];
        let rules = vec![InferenceRule {
            name: "eq_refl".into(),
            relation: "eq".into(),
            kind: InferenceKind::Reflexive,
        }];

        let result = infer(&facts, &rules);
        let has_a_a = result.facts.iter().any(|f| f.left == "A" && f.right == "A");
        let has_b_b = result.facts.iter().any(|f| f.left == "B" && f.right == "B");
        assert!(has_a_a, "reflexive: eq(A,A)");
        assert!(has_b_b, "reflexive: eq(B,B)");
    }

    #[test]
    fn test_leaf_canonicalize_unchanged() {
        let reg = OperationRegistry::new();
        let leaf = PlanNode::Leaf {
            key: "x".into(),
            output_type: TypeId::new("Num"),
        };
        let canonical = canonicalize(&leaf, &reg);
        assert_eq!(plan_fingerprint(&leaf), plan_fingerprint(&canonical));
    }

    #[test]
    fn test_all_identity_children() {
        let mut reg = OperationRegistry::new();
        reg.register(
            "add",
            OpSignature::new(
                vec![TypeId::new("Num"), TypeId::new("Num")],
                TypeId::new("Num"),
            ),
            AlgebraicProperties {
                identity: Some("0".to_string()),
                ..Default::default()
            },
            noop_exec(),
        );

        // add(0, 0) → 0 (identity leaf)
        let plan = PlanNode::Op {
            op_name: "add".into(),
            output_type: TypeId::new("Num"),
            children: vec![
                PlanNode::Leaf { key: "0".into(), output_type: TypeId::new("Num") },
                PlanNode::Leaf { key: "0".into(), output_type: TypeId::new("Num") },
            ],
        };

        let canonical = canonicalize(&plan, &reg);
        match &canonical {
            PlanNode::Leaf { key, .. } => assert_eq!(key, "0"),
            other => panic!("expected identity Leaf, got: {:?}", other),
        }
    }
}
