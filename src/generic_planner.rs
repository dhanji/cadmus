use std::collections::{HashMap, HashSet};
use std::fmt;

use crate::registry::{Literal, OperationRegistry, TypeId};

// ---------------------------------------------------------------------------
// Generic Goal — what the planner must produce
// ---------------------------------------------------------------------------

/// A goal for the generic planner: a required output type plus structural
/// constraints that the plan must satisfy.
#[derive(Debug, Clone)]
pub struct GenericGoal {
    /// The output type the plan must ultimately produce
    pub required_output: TypeId,
    /// Available typed literals (leaf inputs the planner can use)
    pub available: Vec<Literal>,
    /// Constraint: these op names MUST appear in the plan
    pub must_include_ops: Vec<String>,
    /// Constraint: these literal keys MUST be consumed by the plan
    pub must_use_inputs: Vec<String>,
    /// Constraint: op A must execute before op B (pairs of op names)
    pub ordering_constraints: Vec<(String, String)>,
    /// Maximum search depth (prevents infinite recursion)
    pub max_depth: usize,
}

impl GenericGoal {
    /// Create a simple goal: produce the given type from the given literals.
    pub fn simple(output: impl Into<TypeId>, available: Vec<Literal>) -> Self {
        Self {
            required_output: output.into(),
            available,
            must_include_ops: Vec::new(),
            must_use_inputs: Vec::new(),
            ordering_constraints: Vec::new(),
            max_depth: 20,
        }
    }

    /// Add a must-include-op constraint.
    pub fn with_must_include(mut self, op_name: impl Into<String>) -> Self {
        self.must_include_ops.push(op_name.into());
        self
    }

    /// Add a must-use-input constraint.
    pub fn with_must_use(mut self, literal_key: impl Into<String>) -> Self {
        self.must_use_inputs.push(literal_key.into());
        self
    }

    /// Set max search depth.
    pub fn with_max_depth(mut self, depth: usize) -> Self {
        self.max_depth = depth;
        self
    }
}

// ---------------------------------------------------------------------------
// PlanTree — the output of planning
// ---------------------------------------------------------------------------

/// A node in the plan tree (DAG). Either an operation application or a leaf literal.
#[derive(Debug, Clone)]
pub enum PlanNode {
    /// Apply an operation to child nodes
    Op {
        /// Name of the operation from the registry
        op_name: String,
        /// The output type this node produces
        output_type: TypeId,
        /// Child nodes providing the inputs (one per input in the op signature)
        children: Vec<PlanNode>,
    },
    /// A leaf: a literal value available as input
    Leaf {
        /// The literal's key
        key: String,
        /// The type of this literal
        output_type: TypeId,
    },
}

impl PlanNode {
    /// The output type this node produces.
    pub fn output_type(&self) -> &TypeId {
        match self {
            PlanNode::Op { output_type, .. } => output_type,
            PlanNode::Leaf { output_type, .. } => output_type,
        }
    }

    /// Collect all operation names used in this plan tree.
    pub fn op_names(&self) -> HashSet<String> {
        let mut names = HashSet::new();
        self.collect_op_names(&mut names);
        names
    }

    fn collect_op_names(&self, names: &mut HashSet<String>) {
        match self {
            PlanNode::Op { op_name, children, .. } => {
                names.insert(op_name.clone());
                for child in children {
                    child.collect_op_names(names);
                }
            }
            PlanNode::Leaf { .. } => {}
        }
    }

    /// Collect all leaf literal keys used in this plan tree.
    pub fn leaf_keys(&self) -> HashSet<String> {
        let mut keys = HashSet::new();
        self.collect_leaf_keys(&mut keys);
        keys
    }

    fn collect_leaf_keys(&self, keys: &mut HashSet<String>) {
        match self {
            PlanNode::Op { children, .. } => {
                for child in children {
                    child.collect_leaf_keys(keys);
                }
            }
            PlanNode::Leaf { key, .. } => {
                keys.insert(key.clone());
            }
        }
    }

    /// Count total nodes in the tree.
    pub fn node_count(&self) -> usize {
        match self {
            PlanNode::Op { children, .. } => {
                1 + children.iter().map(|c| c.node_count()).sum::<usize>()
            }
            PlanNode::Leaf { .. } => 1,
        }
    }

    /// Count operation nodes only (not leaves).
    pub fn op_count(&self) -> usize {
        match self {
            PlanNode::Op { children, .. } => {
                1 + children.iter().map(|c| c.op_count()).sum::<usize>()
            }
            PlanNode::Leaf { .. } => 0,
        }
    }
}

impl fmt::Display for PlanNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_indent(f, 0)
    }
}

impl PlanNode {
    fn fmt_indent(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        let pad = "  ".repeat(indent);
        match self {
            PlanNode::Op { op_name, output_type, children } => {
                writeln!(f, "{}Op({}) → {}", pad, op_name, output_type)?;
                for child in children {
                    child.fmt_indent(f, indent + 1)?;
                }
                Ok(())
            }
            PlanNode::Leaf { key, output_type } => {
                writeln!(f, "{}Leaf({}: {})", pad, key, output_type)
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Plan errors
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub enum PlanError {
    /// No operation in the registry produces the required type
    NoProducer { type_id: TypeId },
    /// Cycle detected during planning (type appears in its own dependency chain)
    CycleDetected { type_id: TypeId, chain: Vec<String> },
    /// Max depth exceeded
    DepthExceeded { depth: usize },
    /// Must-include constraint not satisfiable
    MustIncludeUnsatisfied { op_name: String },
    /// Must-use-input constraint not satisfiable
    MustUseUnsatisfied { literal_key: String },
    /// No valid plan found after exhaustive search
    NoPlanFound { reason: String },
}

impl fmt::Display for PlanError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NoProducer { type_id } => {
                write!(f, "no operation produces type '{}'", type_id)
            }
            Self::CycleDetected { type_id, chain } => {
                write!(f, "cycle detected: type '{}' appears in chain [{}]",
                    type_id, chain.join(" → "))
            }
            Self::DepthExceeded { depth } => {
                write!(f, "max planning depth {} exceeded", depth)
            }
            Self::MustIncludeUnsatisfied { op_name } => {
                write!(f, "must-include constraint unsatisfied: op '{}'", op_name)
            }
            Self::MustUseUnsatisfied { literal_key } => {
                write!(f, "must-use constraint unsatisfied: literal '{}'", literal_key)
            }
            Self::NoPlanFound { reason } => {
                write!(f, "no valid plan found: {}", reason)
            }
        }
    }
}

impl std::error::Error for PlanError {}

// ---------------------------------------------------------------------------
// Planner — type-directed backtracking search
// ---------------------------------------------------------------------------

/// Plan a tree of operations that produces the required output type.
///
/// Algorithm:
/// 1. If the required type is available as a literal, return a Leaf node.
/// 2. Otherwise, find all ops that produce the required type.
/// 3. For each candidate op, recursively plan its input types.
/// 4. Backtrack if a branch fails.
/// 5. Validate constraints (must-include, must-use) on the final plan.
pub fn plan(
    goal: &GenericGoal,
    registry: &OperationRegistry,
) -> Result<PlanNode, PlanError> {
    // Build a lookup: type → available literals of that type
    let mut available_by_type: HashMap<TypeId, Vec<&Literal>> = HashMap::new();
    for lit in &goal.available {
        available_by_type
            .entry(lit.type_id.clone())
            .or_default()
            .push(lit);
    }

    // Track which literals have been used (for must-use validation)
    let mut used_literals: HashSet<String> = HashSet::new();
    let mut visiting: HashSet<TypeId> = HashSet::new();

    let plan = plan_recursive(
        &goal.required_output,
        registry,
        &available_by_type,
        &mut used_literals,
        &mut visiting,
        0,
        goal.max_depth,
    )?;

    // Validate must-include constraints
    let plan_ops = plan.op_names();
    for required_op in &goal.must_include_ops {
        if !plan_ops.contains(required_op) {
            // Try to find a plan that includes this op by forcing it
            // For now, report the error
            return Err(PlanError::MustIncludeUnsatisfied {
                op_name: required_op.clone(),
            });
        }
    }

    // Validate must-use constraints
    let plan_leaves = plan.leaf_keys();
    for required_key in &goal.must_use_inputs {
        if !plan_leaves.contains(required_key) {
            return Err(PlanError::MustUseUnsatisfied {
                literal_key: required_key.clone(),
            });
        }
    }

    Ok(plan)
}

fn plan_recursive(
    required_type: &TypeId,
    registry: &OperationRegistry,
    available: &HashMap<TypeId, Vec<&Literal>>,
    used_literals: &mut HashSet<String>,
    visiting: &mut HashSet<TypeId>,
    depth: usize,
    max_depth: usize,
) -> Result<PlanNode, PlanError> {
    // Depth check
    if depth > max_depth {
        return Err(PlanError::DepthExceeded { depth });
    }

    // Cycle check
    if visiting.contains(required_type) {
        return Err(PlanError::CycleDetected {
            type_id: required_type.clone(),
            chain: visiting.iter().map(|t| t.to_string()).collect(),
        });
    }

    // 1. Check if a literal of this type is available
    if let Some(literals) = available.get(required_type) {
        // Pick the first unused literal, or the first one if all are used
        let lit = literals
            .iter()
            .find(|l| !used_literals.contains(&l.key))
            .or_else(|| literals.first());

        if let Some(lit) = lit {
            used_literals.insert(lit.key.clone());
            return Ok(PlanNode::Leaf {
                key: lit.key.clone(),
                output_type: required_type.clone(),
            });
        }
    }

    // 2. Find ops that produce this type
    let candidates = registry.ops_for_output(required_type);
    if candidates.is_empty() {
        return Err(PlanError::NoProducer {
            type_id: required_type.clone(),
        });
    }

    // 3. Try each candidate op
    visiting.insert(required_type.clone());
    let mut last_error = None;

    for op in &candidates {
        // Try to plan all inputs for this op
        let mut children = Vec::new();
        let mut success = true;
        let mut branch_used = used_literals.clone();

        for input_type in &op.signature.inputs {
            match plan_recursive(
                input_type,
                registry,
                available,
                &mut branch_used,
                visiting,
                depth + 1,
                max_depth,
            ) {
                Ok(child) => children.push(child),
                Err(e) => {
                    last_error = Some(e);
                    success = false;
                    break;
                }
            }
        }

        if success {
            // Commit the used literals from this branch
            *used_literals = branch_used;
            visiting.remove(required_type);
            return Ok(PlanNode::Op {
                op_name: op.name.clone(),
                output_type: required_type.clone(),
                children,
            });
        }
    }

    visiting.remove(required_type);

    // All candidates failed
    Err(last_error.unwrap_or(PlanError::NoPlanFound {
        reason: format!("all ops producing '{}' failed", required_type),
    }))
}

/// Plan multiple output types from the same goal context.
/// Returns a plan for each requested type.
pub fn plan_multi(
    outputs: &[TypeId],
    available: Vec<Literal>,
    registry: &OperationRegistry,
    max_depth: usize,
) -> Result<Vec<PlanNode>, PlanError> {
    let mut plans = Vec::new();
    for output in outputs {
        let goal = GenericGoal {
            required_output: output.clone(),
            available: available.clone(),
            must_include_ops: Vec::new(),
            must_use_inputs: Vec::new(),
            ordering_constraints: Vec::new(),
            max_depth,
        };
        plans.push(plan(&goal, registry)?);
    }
    Ok(plans)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::registry::{AlgebraicProperties, OpSignature, OperationRegistry};

    fn noop_exec() -> crate::registry::ExecFn {
        Box::new(|_| Ok("noop".into()))
    }

    fn make_chain_registry() -> OperationRegistry {
        // Chain: retrieve→Evidence, summarize(Evidence)→Claim, compare(Claim,Claim)→Contrast
        let mut reg = OperationRegistry::new();
        reg.register(
            "retrieve",
            OpSignature::leaf(TypeId::new("Evidence")),
            AlgebraicProperties::none(),
            noop_exec(),
        );
        reg.register(
            "summarize",
            OpSignature::new(vec![TypeId::new("Evidence")], TypeId::new("Claim")),
            AlgebraicProperties::none(),
            noop_exec(),
        );
        reg.register(
            "compare",
            OpSignature::new(
                vec![TypeId::new("Claim"), TypeId::new("Claim")],
                TypeId::new("Contrast"),
            ),
            AlgebraicProperties::none(),
            noop_exec(),
        );
        reg
    }

    #[test]
    fn test_plan_chain_a_b_c() {
        let reg = make_chain_registry();
        let goal = GenericGoal::simple("Contrast", vec![]);
        let plan = plan(&goal, &reg).unwrap();

        // Should be: compare(summarize(retrieve()), summarize(retrieve()))
        assert_eq!(plan.op_count(), 5); // compare + 2*summarize + 2*retrieve
        assert_eq!(*plan.output_type(), TypeId::new("Contrast"));

        match &plan {
            PlanNode::Op { op_name, children, .. } => {
                assert_eq!(op_name, "compare");
                assert_eq!(children.len(), 2);
            }
            _ => panic!("expected Op node at root"),
        }
    }

    #[test]
    fn test_plan_with_available_literal() {
        let reg = make_chain_registry();
        let goal = GenericGoal::simple(
            "Claim",
            vec![Literal::new("Evidence", "ev1", "some evidence")],
        );
        let plan = plan(&goal, &reg).unwrap();

        // Should be: summarize(Leaf(ev1))
        assert_eq!(plan.op_count(), 1);
        match &plan {
            PlanNode::Op { op_name, children, .. } => {
                assert_eq!(op_name, "summarize");
                assert_eq!(children.len(), 1);
                match &children[0] {
                    PlanNode::Leaf { key, .. } => assert_eq!(key, "ev1"),
                    _ => panic!("expected Leaf child"),
                }
            }
            _ => panic!("expected Op node"),
        }
    }

    #[test]
    fn test_plan_literal_directly_satisfies_goal() {
        let reg = make_chain_registry();
        let goal = GenericGoal::simple(
            "Evidence",
            vec![Literal::new("Evidence", "ev_direct", "direct evidence")],
        );
        let plan = plan(&goal, &reg).unwrap();

        // Should be just a Leaf — the literal directly satisfies the goal
        assert_eq!(plan.op_count(), 0);
        match &plan {
            PlanNode::Leaf { key, .. } => assert_eq!(key, "ev_direct"),
            _ => panic!("expected Leaf node for direct literal match"),
        }
    }

    #[test]
    fn test_plan_no_producer_error() {
        let reg = OperationRegistry::new(); // empty registry
        let goal = GenericGoal::simple("Anything", vec![]);
        let result = plan(&goal, &reg);

        assert!(result.is_err());
        match result.unwrap_err() {
            PlanError::NoProducer { type_id } => {
                assert_eq!(type_id, TypeId::new("Anything"));
            }
            other => panic!("expected NoProducer, got: {:?}", other),
        }
    }

    #[test]
    fn test_plan_cycle_detection() {
        // Create a cycle: A needs B, B needs A
        let mut reg = OperationRegistry::new();
        reg.register(
            "a_from_b",
            OpSignature::new(vec![TypeId::new("B")], TypeId::new("A")),
            AlgebraicProperties::none(),
            noop_exec(),
        );
        reg.register(
            "b_from_a",
            OpSignature::new(vec![TypeId::new("A")], TypeId::new("B")),
            AlgebraicProperties::none(),
            noop_exec(),
        );

        let goal = GenericGoal::simple("A", vec![]).with_max_depth(10);
        let result = plan(&goal, &reg);

        assert!(result.is_err());
        match result.unwrap_err() {
            PlanError::CycleDetected { type_id, .. } => {
                // The cycle should be detected on either A or B
                assert!(
                    type_id == TypeId::new("A") || type_id == TypeId::new("B"),
                    "cycle should involve A or B, got: {}",
                    type_id
                );
            }
            PlanError::DepthExceeded { .. } => {
                // Also acceptable — depth limit catches the cycle
            }
            other => panic!("expected CycleDetected or DepthExceeded, got: {:?}", other),
        }
    }

    #[test]
    fn test_plan_must_include_constraint() {
        // Two ops produce Claim: summarize and infer
        let mut reg = OperationRegistry::new();
        reg.register(
            "summarize",
            OpSignature::new(vec![TypeId::new("Evidence")], TypeId::new("Claim")),
            AlgebraicProperties::none(),
            noop_exec(),
        );
        reg.register(
            "infer",
            OpSignature::new(vec![TypeId::new("Evidence")], TypeId::new("Claim")),
            AlgebraicProperties::none(),
            noop_exec(),
        );
        reg.register(
            "retrieve",
            OpSignature::leaf(TypeId::new("Evidence")),
            AlgebraicProperties::none(),
            noop_exec(),
        );

        // Without constraint, planner picks first available (summarize)
        let goal = GenericGoal::simple("Claim", vec![]);
        let plan_result = plan(&goal, &reg).unwrap();
        let ops = plan_result.op_names();

        // With must-include "infer" — if the plan doesn't include it, error
        let goal_constrained = GenericGoal::simple("Claim", vec![])
            .with_must_include("infer");
        let result = plan(&goal_constrained, &reg);

        // The planner picks the first candidate. If it picked "summarize",
        // the must-include check will fail. If it picked "infer", it passes.
        if ops.contains("infer") {
            // Planner happened to pick infer first — constrained should also work
            assert!(result.is_ok());
        } else {
            // Planner picked summarize — must-include "infer" fails
            match result {
                Err(PlanError::MustIncludeUnsatisfied { op_name }) => {
                    assert_eq!(op_name, "infer");
                }
                other => panic!("expected MustIncludeUnsatisfied, got: {:?}", other),
            }
        }
    }

    #[test]
    fn test_plan_must_use_input_constraint() {
        let mut reg = OperationRegistry::new();
        reg.register(
            "summarize",
            OpSignature::new(vec![TypeId::new("Evidence")], TypeId::new("Claim")),
            AlgebraicProperties::none(),
            noop_exec(),
        );

        let goal = GenericGoal::simple(
            "Claim",
            vec![Literal::new("Evidence", "ev1", "evidence 1")],
        )
        .with_must_use("ev1");

        let result = plan(&goal, &reg).unwrap();
        assert!(result.leaf_keys().contains("ev1"));

        // Must-use a literal that doesn't exist
        let goal_bad = GenericGoal::simple(
            "Claim",
            vec![Literal::new("Evidence", "ev1", "evidence 1")],
        )
        .with_must_use("ev_nonexistent");

        let result_bad = plan(&goal_bad, &reg);
        assert!(result_bad.is_err());
        match result_bad.unwrap_err() {
            PlanError::MustUseUnsatisfied { literal_key } => {
                assert_eq!(literal_key, "ev_nonexistent");
            }
            other => panic!("expected MustUseUnsatisfied, got: {:?}", other),
        }
    }

    #[test]
    fn test_plan_depth_exceeded() {
        // Deep chain that exceeds depth limit
        let mut reg = OperationRegistry::new();
        reg.register(
            "deep",
            OpSignature::new(vec![TypeId::new("A")], TypeId::new("A")),
            AlgebraicProperties::none(),
            noop_exec(),
        );

        let goal = GenericGoal::simple("A", vec![]).with_max_depth(3);
        let result = plan(&goal, &reg);

        assert!(result.is_err());
        // Should hit either cycle or depth
        match result.unwrap_err() {
            PlanError::CycleDetected { .. } | PlanError::DepthExceeded { .. } => {}
            other => panic!("expected CycleDetected or DepthExceeded, got: {:?}", other),
        }
    }

    #[test]
    fn test_plan_multi_outputs() {
        let reg = make_chain_registry();
        let plans = plan_multi(
            &[TypeId::new("Evidence"), TypeId::new("Claim")],
            vec![],
            &reg,
            20,
        )
        .unwrap();

        assert_eq!(plans.len(), 2);
        assert_eq!(*plans[0].output_type(), TypeId::new("Evidence"));
        assert_eq!(*plans[1].output_type(), TypeId::new("Claim"));
    }

    #[test]
    fn test_plan_node_display() {
        let reg = make_chain_registry();
        let goal = GenericGoal::simple(
            "Claim",
            vec![Literal::new("Evidence", "ev1", "data")],
        );
        let plan = plan(&goal, &reg).unwrap();
        let display = format!("{}", plan);
        assert!(display.contains("summarize"));
        assert!(display.contains("ev1"));
    }

    #[test]
    fn test_plan_uses_different_literals_for_each_input() {
        let mut reg = OperationRegistry::new();
        reg.register(
            "combine",
            OpSignature::new(
                vec![TypeId::new("X"), TypeId::new("X")],
                TypeId::new("Y"),
            ),
            AlgebraicProperties::none(),
            noop_exec(),
        );

        let goal = GenericGoal::simple(
            "Y",
            vec![
                Literal::new("X", "x1", "first"),
                Literal::new("X", "x2", "second"),
            ],
        );
        let plan = plan(&goal, &reg).unwrap();
        let keys = plan.leaf_keys();
        assert!(keys.contains("x1"), "should use x1");
        assert!(keys.contains("x2"), "should use x2");
    }
}
