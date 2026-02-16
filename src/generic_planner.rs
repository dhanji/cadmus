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

// ===========================================================================
// TypeExpr-based planner — unification, map insertion, fold insertion
// ===========================================================================

use crate::type_expr::{TypeExpr, unify};

// ---------------------------------------------------------------------------
// ExprGoal — goal using TypeExpr
// ---------------------------------------------------------------------------

/// A goal for the expr-based planner: produce a value of the given TypeExpr.
#[derive(Debug, Clone)]
pub struct ExprGoal {
    /// The output type the plan must produce
    pub target: TypeExpr,
    /// Available typed literals (leaf inputs)
    pub available: Vec<ExprLiteral>,
    /// Maximum search depth
    pub max_depth: usize,
}

/// A typed literal for the expr planner.
#[derive(Debug, Clone)]
pub struct ExprLiteral {
    /// Unique key
    pub key: String,
    /// The type of this literal as a TypeExpr
    pub type_expr: TypeExpr,
    /// Human-readable description
    pub description: String,
}

impl ExprLiteral {
    pub fn new(key: impl Into<String>, type_expr: TypeExpr, desc: impl Into<String>) -> Self {
        Self { key: key.into(), type_expr, description: desc.into() }
    }
}

impl ExprGoal {
    pub fn new(target: TypeExpr, available: Vec<ExprLiteral>) -> Self {
        Self { target, available, max_depth: 20 }
    }

    pub fn with_max_depth(mut self, depth: usize) -> Self {
        self.max_depth = depth;
        self
    }
}

// ---------------------------------------------------------------------------
// ExprPlanNode — plan tree with Map and Fold
// ---------------------------------------------------------------------------

/// A node in the expr-based plan tree.
#[derive(Debug, Clone)]
pub enum ExprPlanNode {
    /// Apply a polymorphic operation
    Op {
        op_name: String,
        output_type: TypeExpr,
        children: Vec<ExprPlanNode>,
    },
    /// A leaf literal
    Leaf {
        key: String,
        output_type: TypeExpr,
    },
    /// Map: apply an operation element-wise over a sequence
    /// Seq(A) → Seq(B) via op A→B
    Map {
        op_name: String,
        elem_output: TypeExpr,
        child: Box<ExprPlanNode>,
    },
    /// Fold: reduce a sequence using an associative binary operation
    /// Seq(B) → B via associative op B+B→B
    Fold {
        op_name: String,
        output_type: TypeExpr,
        child: Box<ExprPlanNode>,
    },
}

impl ExprPlanNode {
    pub fn output_type(&self) -> &TypeExpr {
        match self {
            ExprPlanNode::Op { output_type, .. } => output_type,
            ExprPlanNode::Leaf { output_type, .. } => output_type,
            ExprPlanNode::Map { elem_output, .. } => elem_output,
            ExprPlanNode::Fold { output_type, .. } => output_type,
        }
    }

    /// Collect all operation names in the plan.
    pub fn op_names(&self) -> HashSet<String> {
        let mut names = HashSet::new();
        self.collect_ops(&mut names);
        names
    }

    fn collect_ops(&self, names: &mut HashSet<String>) {
        match self {
            ExprPlanNode::Op { op_name, children, .. } => {
                names.insert(op_name.clone());
                for c in children { c.collect_ops(names); }
            }
            ExprPlanNode::Leaf { .. } => {}
            ExprPlanNode::Map { op_name, child, .. } => {
                names.insert(format!("map({})", op_name));
                names.insert(op_name.clone());
                child.collect_ops(names);
            }
            ExprPlanNode::Fold { op_name, child, .. } => {
                names.insert(format!("fold({})", op_name));
                names.insert(op_name.clone());
                child.collect_ops(names);
            }
        }
    }

    /// Collect all leaf keys.
    pub fn leaf_keys(&self) -> HashSet<String> {
        let mut keys = HashSet::new();
        self.collect_leaves(&mut keys);
        keys
    }

    fn collect_leaves(&self, keys: &mut HashSet<String>) {
        match self {
            ExprPlanNode::Op { children, .. } => {
                for c in children { c.collect_leaves(keys); }
            }
            ExprPlanNode::Leaf { key, .. } => { keys.insert(key.clone()); }
            ExprPlanNode::Map { child, .. } | ExprPlanNode::Fold { child, .. } => {
                child.collect_leaves(keys);
            }
        }
    }
}

impl fmt::Display for ExprPlanNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_indent_expr(f, 0)
    }
}

impl ExprPlanNode {
    fn fmt_indent_expr(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        let pad = "  ".repeat(indent);
        match self {
            ExprPlanNode::Op { op_name, output_type, children } => {
                writeln!(f, "{}Op({}) → {}", pad, op_name, output_type)?;
                for c in children { c.fmt_indent_expr(f, indent + 1)?; }
                Ok(())
            }
            ExprPlanNode::Leaf { key, output_type } => {
                writeln!(f, "{}Leaf({}: {})", pad, key, output_type)
            }
            ExprPlanNode::Map { op_name, elem_output, child } => {
                writeln!(f, "{}Map({}) → Seq({})", pad, op_name, elem_output)?;
                child.fmt_indent_expr(f, indent + 1)
            }
            ExprPlanNode::Fold { op_name, output_type, child } => {
                writeln!(f, "{}Fold({}) → {}", pad, op_name, output_type)?;
                child.fmt_indent_expr(f, indent + 1)
            }
        }
    }
}

// ---------------------------------------------------------------------------
// plan_expr — the unification-based planner
// ---------------------------------------------------------------------------

/// Plan using TypeExpr goals and polymorphic operations.
///
/// Algorithm:
/// 1. If a literal matches the target type (via unification), return Leaf.
/// 2. Find poly ops whose output unifies with the target.
/// 3. For each candidate, recursively plan its concrete inputs.
/// 4. If target is Seq(B) and we have Seq(A) + op A→B, insert Map.
/// 5. If target is B and we have Seq(B) + associative B+B→B, insert Fold.
pub fn plan_expr(
    goal: &ExprGoal,
    registry: &OperationRegistry,
) -> Result<ExprPlanNode, PlanError> {
    let mut used: HashSet<String> = HashSet::new();
    let mut visiting: Vec<String> = Vec::new();
    plan_expr_recursive(
        &goal.target,
        &goal.available,
        registry,
        &mut used,
        &mut visiting,
        0,
        goal.max_depth,
    )
}

fn plan_expr_recursive(
    target: &TypeExpr,
    available: &[ExprLiteral],
    registry: &OperationRegistry,
    used: &mut HashSet<String>,
    visiting: &mut Vec<String>,
    depth: usize,
    max_depth: usize,
) -> Result<ExprPlanNode, PlanError> {
    if depth > max_depth {
        return Err(PlanError::DepthExceeded { depth });
    }

    let target_str = target.to_string();

    // Cycle check
    if visiting.contains(&target_str) {
        return Err(PlanError::CycleDetected {
            type_id: TypeId::new(&target_str),
            chain: visiting.clone(),
        });
    }

    // 1. Check literals
    for lit in available {
        if !used.contains(&lit.key) {
            if unify(&lit.type_expr, target).is_ok() {
                used.insert(lit.key.clone());
                return Ok(ExprPlanNode::Leaf {
                    key: lit.key.clone(),
                    output_type: target.clone(),
                });
            }
        }
    }

    visiting.push(target_str.clone());

    // 2. Try poly ops whose output unifies with target
    let matches = registry.ops_for_output_expr(target);
    let mut last_error: Option<PlanError> = None;

    for m in &matches {
        let mut branch_used = used.clone();
        let mut success = true;
        let mut children = Vec::new();

        for input in &m.concrete_inputs {
            match plan_expr_recursive(input, available, registry, &mut branch_used, visiting, depth + 1, max_depth) {
                Ok(child) => children.push(child),
                Err(e) => { last_error = Some(e); success = false; break; }
            }
        }

        if success {
            *used = branch_used;
            visiting.pop();
            return Ok(ExprPlanNode::Op {
                op_name: m.op.name.clone(),
                output_type: m.concrete_output.clone(),
                children,
            });
        }
    }

    // 3. Map insertion: target is Seq(B), we have Seq(A), and op A→B exists
    if let TypeExpr::Constructor(seq_name, seq_args) = target {
        if seq_name == "Seq" && seq_args.len() == 1 {
            let target_elem = &seq_args[0];

            // Look for available Seq(?) literals or plannable Seq(?) values
            for lit in available {
                if used.contains(&lit.key) { continue; }
                if let TypeExpr::Constructor(lit_seq, lit_args) = &lit.type_expr {
                    if lit_seq == "Seq" && lit_args.len() == 1 {
                        let source_elem = &lit_args[0];
                        // Find an op that takes source_elem and produces target_elem
                        let elem_matches = registry.ops_for_output_expr(target_elem);
                        for em in &elem_matches {
                            if em.concrete_inputs.len() == 1 {
                                if unify(&em.concrete_inputs[0], source_elem).is_ok() {
                                    used.insert(lit.key.clone());
                                    visiting.pop();
                                    return Ok(ExprPlanNode::Map {
                                        op_name: em.op.name.clone(),
                                        elem_output: target_elem.clone(),
                                        child: Box::new(ExprPlanNode::Leaf {
                                            key: lit.key.clone(),
                                            output_type: lit.type_expr.clone(),
                                        }),
                                    });
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // 4. Fold insertion: target is B, we have Seq(B), and associative B+B→B exists
    for lit in available {
        if used.contains(&lit.key) { continue; }
        if let TypeExpr::Constructor(seq_name, seq_args) = &lit.type_expr {
            if seq_name == "Seq" && seq_args.len() == 1 {
                let elem_type = &seq_args[0];
                if unify(elem_type, target).is_ok() {
                    // Find an associative op B+B→B
                    let fold_matches = registry.ops_for_output_expr(target);
                    for fm in &fold_matches {
                        if fm.concrete_inputs.len() == 2 {
                            if unify(&fm.concrete_inputs[0], target).is_ok()
                                && unify(&fm.concrete_inputs[1], target).is_ok()
                            {
                                if let Some(poly_op) = registry.get_poly(&fm.op.name) {
                                    if poly_op.properties.associative {
                                        used.insert(lit.key.clone());
                                        visiting.pop();
                                        return Ok(ExprPlanNode::Fold {
                                            op_name: fm.op.name.clone(),
                                            output_type: target.clone(),
                                            child: Box::new(ExprPlanNode::Leaf {
                                                key: lit.key.clone(),
                                                output_type: lit.type_expr.clone(),
                                            }),
                                        });
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    visiting.pop();
    Err(last_error.unwrap_or(PlanError::NoPlanFound {
        reason: format!("no plan for type '{}'", target),
    }))
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

    // -----------------------------------------------------------------------
    // ExprPlanNode tests — unification-based planner
    // -----------------------------------------------------------------------

    use crate::type_expr::TypeExpr;
    use crate::registry::PolyOpSignature;

    fn make_fs_registry() -> OperationRegistry {
        let mut reg = OperationRegistry::new();

        // list_dir<a>: Dir(a) → Seq(Entry(Name, a))
        reg.register_poly(
            "list_dir",
            PolyOpSignature::new(
                vec!["a".into()],
                vec![TypeExpr::dir(TypeExpr::var("a"))],
                TypeExpr::seq(TypeExpr::entry(TypeExpr::prim("Name"), TypeExpr::var("a"))),
            ),
            AlgebraicProperties::none(),
            "ls",
        );

        // extract_archive<a, fmt>: File(Archive(a, fmt)) → Seq(Entry(Name, a))
        reg.register_poly(
            "extract_archive",
            PolyOpSignature::new(
                vec!["a".into(), "fmt".into()],
                vec![TypeExpr::file(TypeExpr::archive(TypeExpr::var("a"), TypeExpr::var("fmt")))],
                TypeExpr::seq(TypeExpr::entry(TypeExpr::prim("Name"), TypeExpr::var("a"))),
            ),
            AlgebraicProperties::none(),
            "unzip",
        );

        // pack_archive<a, fmt>: Seq(Entry(Name, a)), fmt → File(Archive(a, fmt))
        reg.register_poly(
            "pack_archive",
            PolyOpSignature::new(
                vec!["a".into(), "fmt".into()],
                vec![
                    TypeExpr::seq(TypeExpr::entry(TypeExpr::prim("Name"), TypeExpr::var("a"))),
                    TypeExpr::var("fmt"),
                ],
                TypeExpr::file(TypeExpr::archive(TypeExpr::var("a"), TypeExpr::var("fmt"))),
            ),
            AlgebraicProperties::none(),
            "zip",
        );

        // concat_seq<a>: Seq(a), Seq(a) → Seq(a)  [associative]
        reg.register_poly(
            "concat_seq",
            PolyOpSignature::new(
                vec!["a".into()],
                vec![TypeExpr::seq(TypeExpr::var("a")), TypeExpr::seq(TypeExpr::var("a"))],
                TypeExpr::seq(TypeExpr::var("a")),
            ),
            AlgebraicProperties::associative(),
            "concatenate sequences",
        );

        // read_file: File(a) → a
        reg.register_poly(
            "read_file",
            PolyOpSignature::new(
                vec!["a".into()],
                vec![TypeExpr::file(TypeExpr::var("a"))],
                TypeExpr::var("a"),
            ),
            AlgebraicProperties::none(),
            "cat",
        );

        reg
    }

    #[test]
    fn test_plan_expr_literal_match() {
        let reg = make_fs_registry();
        let goal = ExprGoal::new(
            TypeExpr::prim("Path"),
            vec![ExprLiteral::new("p1", TypeExpr::prim("Path"), "/tmp")],
        );
        let plan = plan_expr(&goal, &reg).unwrap();
        match &plan {
            ExprPlanNode::Leaf { key, .. } => assert_eq!(key, "p1"),
            other => panic!("expected Leaf, got: {}", other),
        }
    }

    #[test]
    fn test_plan_expr_single_op() {
        let reg = make_fs_registry();
        // Goal: Seq(Entry(Name, Bytes)) from Dir(Bytes)
        let goal = ExprGoal::new(
            TypeExpr::seq(TypeExpr::entry(TypeExpr::prim("Name"), TypeExpr::prim("Bytes"))),
            vec![ExprLiteral::new("d1", TypeExpr::dir(TypeExpr::prim("Bytes")), "/tmp")],
        );
        let plan = plan_expr(&goal, &reg).unwrap();
        let ops = plan.op_names();
        assert!(ops.contains("list_dir"), "plan should use list_dir, got: {}", plan);
    }

    #[test]
    fn test_plan_expr_chain() {
        let reg = make_fs_registry();
        // Goal: Seq(Entry(Name, File(Image))) from File(Archive(File(Image), Cbz))
        let goal = ExprGoal::new(
            TypeExpr::seq(TypeExpr::entry(
                TypeExpr::prim("Name"),
                TypeExpr::file(TypeExpr::prim("Image")),
            )),
            vec![ExprLiteral::new(
                "cbz1",
                TypeExpr::file(TypeExpr::archive(TypeExpr::file(TypeExpr::prim("Image")), TypeExpr::prim("Cbz"))),
                "comic.cbz",
            )],
        );
        let plan = plan_expr(&goal, &reg).unwrap();
        let ops = plan.op_names();
        assert!(ops.contains("extract_archive"), "plan should use extract_archive, got: {}", plan);
    }

    #[test]
    fn test_plan_expr_no_producer() {
        let reg = OperationRegistry::new();
        let goal = ExprGoal::new(TypeExpr::prim("Nonexistent"), vec![]);
        let result = plan_expr(&goal, &reg);
        assert!(result.is_err());
    }

    #[test]
    fn test_plan_expr_cycle_terminates() {
        // Register ops that could cycle: A→B and B→A
        let mut reg = OperationRegistry::new();
        reg.register_poly(
            "a_to_b",
            PolyOpSignature::mono(vec![TypeExpr::prim("A")], TypeExpr::prim("B")),
            AlgebraicProperties::none(),
            "a→b",
        );
        reg.register_poly(
            "b_to_a",
            PolyOpSignature::mono(vec![TypeExpr::prim("B")], TypeExpr::prim("A")),
            AlgebraicProperties::none(),
            "b→a",
        );

        let goal = ExprGoal::new(TypeExpr::prim("A"), vec![]).with_max_depth(10);
        let result = plan_expr(&goal, &reg);
        assert!(result.is_err(), "should fail due to cycle or depth");
    }

    #[test]
    fn test_plan_expr_map_insertion() {
        // We have Seq(A) and op A→B, goal is Seq(B)
        let mut reg = OperationRegistry::new();
        reg.register_poly(
            "transform",
            PolyOpSignature::mono(
                vec![TypeExpr::prim("A")],
                TypeExpr::prim("B"),
            ),
            AlgebraicProperties::none(),
            "transform A to B",
        );

        let goal = ExprGoal::new(
            TypeExpr::seq(TypeExpr::prim("B")),
            vec![ExprLiteral::new("seq_a", TypeExpr::seq(TypeExpr::prim("A")), "sequence of As")],
        );
        let plan = plan_expr(&goal, &reg).unwrap();
        let ops = plan.op_names();
        assert!(ops.contains("transform"), "plan should use transform: {}", plan);
        assert!(ops.contains("map(transform)"), "plan should have map(transform): {}", plan);
        match &plan {
            ExprPlanNode::Map { op_name, .. } => assert_eq!(op_name, "transform"),
            other => panic!("expected Map node, got: {}", other),
        }
    }

    #[test]
    fn test_plan_expr_fold_insertion() {
        // We have Seq(B) and associative B+B→B, goal is B
        let mut reg = OperationRegistry::new();
        reg.register_poly(
            "concat_seq",
            PolyOpSignature::new(
                vec!["a".into()],
                vec![TypeExpr::seq(TypeExpr::var("a")), TypeExpr::seq(TypeExpr::var("a"))],
                TypeExpr::seq(TypeExpr::var("a")),
            ),
            AlgebraicProperties::associative(),
            "concat",
        );

        let goal = ExprGoal::new(
            TypeExpr::seq(TypeExpr::prim("X")),
            vec![ExprLiteral::new(
                "nested",
                TypeExpr::seq(TypeExpr::seq(TypeExpr::prim("X"))),
                "sequence of sequences",
            )],
        );
        let plan = plan_expr(&goal, &reg).unwrap();
        let ops = plan.op_names();
        assert!(ops.contains("concat_seq"), "plan should use concat_seq: {}", plan);
        assert!(ops.contains("fold(concat_seq)"), "plan should have fold(concat_seq): {}", plan);
        match &plan {
            ExprPlanNode::Fold { op_name, .. } => assert_eq!(op_name, "concat_seq"),
            other => panic!("expected Fold node, got: {}", other),
        }
    }
}
