use std::fmt;

use crate::fs_types::build_fs_registry;
use crate::generic_planner::{plan_expr, ExprGoal, ExprLiteral, ExprPlanNode, PlanError};
use crate::registry::OperationRegistry;
use crate::type_expr::TypeExpr;

// ---------------------------------------------------------------------------
// FilesystemStrategy — dry-run planning for filesystem operations
// ---------------------------------------------------------------------------

/// A filesystem strategy that plans operations using the polymorphic type
/// system and produces a dry-run trace instead of executing real commands.
///
/// The trace shows each planned step with:
/// - Step number and operation name
/// - Input types and output type
/// - What shell command/action would be executed
pub struct FilesystemStrategy {
    registry: OperationRegistry,
}

impl FilesystemStrategy {
    /// Create a new FilesystemStrategy with the standard filesystem vocabulary.
    pub fn new() -> Self {
        Self {
            registry: build_fs_registry(),
        }
    }

    /// Get a reference to the underlying registry.
    pub fn registry(&self) -> &OperationRegistry {
        &self.registry
    }

    /// Plan a goal and return the plan tree.
    pub fn plan(
        &self,
        target: TypeExpr,
        available: Vec<ExprLiteral>,
    ) -> Result<ExprPlanNode, PlanError> {
        let goal = ExprGoal::new(target, available);
        plan_expr(&goal, &self.registry)
    }

    /// Plan a goal and return a dry-run trace as a formatted string.
    ///
    /// Each step shows the operation name, input/output types, and what
    /// shell command would be executed.
    pub fn dry_run(
        &self,
        target: TypeExpr,
        available: Vec<ExprLiteral>,
    ) -> Result<DryRunTrace, PlanError> {
        let plan = self.plan(target.clone(), available)?;
        let mut steps = Vec::new();
        let mut step_num = 0;
        collect_trace_steps(&plan, &self.registry, &mut steps, &mut step_num);
        Ok(DryRunTrace {
            goal: target,
            steps,
            plan,
        })
    }
}

impl Default for FilesystemStrategy {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// DryRunTrace — the output of a dry-run plan
// ---------------------------------------------------------------------------

/// A dry-run trace: the planned sequence of operations that would be
/// executed, without actually running any commands.
#[derive(Debug)]
pub struct DryRunTrace {
    /// The goal type that was planned for
    pub goal: TypeExpr,
    /// The ordered steps in the plan
    pub steps: Vec<TraceStep>,
    /// The full plan tree
    pub plan: ExprPlanNode,
}

/// A single step in the dry-run trace.
#[derive(Debug, Clone)]
pub struct TraceStep {
    /// Step number (1-based, in dependency order)
    pub step: usize,
    /// Operation name
    pub op_name: String,
    /// What kind of step (Op, Map, Fold, Leaf)
    pub kind: StepKind,
    /// Input type descriptions
    pub inputs: Vec<String>,
    /// Output type
    pub output: String,
    /// What command/action would be executed
    pub command_hint: String,
}

/// The kind of trace step.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StepKind {
    /// A direct operation application
    Op,
    /// A leaf literal (input data)
    Leaf,
    /// A map over a sequence
    Map,
    /// A fold over a sequence
    Fold,
}

impl fmt::Display for DryRunTrace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "=== Dry-Run Trace ===")?;
        writeln!(f, "Goal: {}", self.goal)?;
        writeln!(f, "Steps: {}", self.steps.len())?;
        writeln!(f, "---")?;
        for step in &self.steps {
            write!(f, "{}", step)?;
        }
        writeln!(f, "=== End Trace ===")
    }
}

impl fmt::Display for TraceStep {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let kind_str = match self.kind {
            StepKind::Op => "OP",
            StepKind::Leaf => "INPUT",
            StepKind::Map => "MAP",
            StepKind::Fold => "FOLD",
        };
        writeln!(f, "[{}] {} ({})", self.step, self.op_name, kind_str)?;
        if !self.inputs.is_empty() {
            writeln!(f, "     inputs:  {}", self.inputs.join(", "))?;
        }
        writeln!(f, "     output:  {}", self.output)?;
        writeln!(f, "     command: {}", self.command_hint)
    }
}

// ---------------------------------------------------------------------------
// Trace collection — walk the plan tree in dependency order
// ---------------------------------------------------------------------------

fn collect_trace_steps(
    node: &ExprPlanNode,
    registry: &OperationRegistry,
    steps: &mut Vec<TraceStep>,
    step_num: &mut usize,
) {
    match node {
        ExprPlanNode::Leaf { key, output_type } => {
            *step_num += 1;
            steps.push(TraceStep {
                step: *step_num,
                op_name: key.clone(),
                kind: StepKind::Leaf,
                inputs: vec![],
                output: output_type.to_string(),
                command_hint: format!("# input literal: {}", key),
            });
        }
        ExprPlanNode::Op { op_name, output_type, children } => {
            // Process children first (dependency order)
            for child in children {
                collect_trace_steps(child, registry, steps, step_num);
            }
            *step_num += 1;
            let input_types: Vec<String> = children
                .iter()
                .map(|c| c.output_type().to_string())
                .collect();
            let hint = get_command_hint(op_name, registry);
            steps.push(TraceStep {
                step: *step_num,
                op_name: op_name.clone(),
                kind: StepKind::Op,
                inputs: input_types,
                output: output_type.to_string(),
                command_hint: hint,
            });
        }
        ExprPlanNode::Map { op_name, elem_output, child } => {
            // Process child first
            collect_trace_steps(child, registry, steps, step_num);
            *step_num += 1;
            let hint = get_command_hint(op_name, registry);
            steps.push(TraceStep {
                step: *step_num,
                op_name: format!("map({})", op_name),
                kind: StepKind::Map,
                inputs: vec![child.output_type().to_string()],
                output: format!("Seq({})", elem_output),
                command_hint: format!("for each element: {}", hint),
            });
        }
        ExprPlanNode::Fold { op_name, output_type, child } => {
            // Process child first
            collect_trace_steps(child, registry, steps, step_num);
            *step_num += 1;
            let hint = get_command_hint(op_name, registry);
            steps.push(TraceStep {
                step: *step_num,
                op_name: format!("fold({})", op_name),
                kind: StepKind::Fold,
                inputs: vec![child.output_type().to_string()],
                output: output_type.to_string(),
                command_hint: format!("reduce with: {}", hint),
            });
        }
    }
}

fn get_command_hint(op_name: &str, registry: &OperationRegistry) -> String {
    registry
        .get_poly(op_name)
        .map(|op| op.description.clone())
        .unwrap_or_else(|| format!("<{}>", op_name))
}

// ---------------------------------------------------------------------------
// Convenience function
// ---------------------------------------------------------------------------

/// Plan a filesystem goal and return the dry-run trace as a String.
///
/// This is the main entry point for filesystem planning.
pub fn run_fs_goal(
    target: TypeExpr,
    available: Vec<ExprLiteral>,
) -> Result<String, PlanError> {
    let strategy = FilesystemStrategy::new();
    let trace = strategy.dry_run(target, available)?;
    Ok(trace.to_string())
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_strategy_plans_list_dir() {
        let strategy = FilesystemStrategy::new();
        let trace = strategy.dry_run(
            TypeExpr::seq(TypeExpr::entry(
                TypeExpr::prim("Name"),
                TypeExpr::prim("Bytes"),
            )),
            vec![ExprLiteral::new(
                "mydir",
                TypeExpr::dir(TypeExpr::prim("Bytes")),
                "/tmp/mydir",
            )],
        ).unwrap();

        assert!(!trace.steps.is_empty());
        let op_steps: Vec<&TraceStep> = trace.steps.iter()
            .filter(|s| s.kind == StepKind::Op)
            .collect();
        assert!(!op_steps.is_empty());
        assert_eq!(op_steps[0].op_name, "list_dir");
        assert!(op_steps[0].command_hint.contains("ls"), 
            "hint should mention ls: {}", op_steps[0].command_hint);

        // Verify trace display
        let display = trace.to_string();
        assert!(display.contains("list_dir"), "trace should mention list_dir");
        assert!(display.contains("ls"), "trace should mention ls command");
    }

    #[test]
    fn test_strategy_unreachable_goal() {
        let strategy = FilesystemStrategy::new();
        let result = strategy.dry_run(
            TypeExpr::prim("CompletelyUnknownType"),
            vec![],
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_strategy_extract_archive_trace() {
        let strategy = FilesystemStrategy::new();
        let trace = strategy.dry_run(
            TypeExpr::seq(TypeExpr::entry(
                TypeExpr::prim("Name"),
                TypeExpr::file(TypeExpr::prim("Image")),
            )),
            vec![ExprLiteral::new(
                "comic",
                TypeExpr::file(TypeExpr::archive(
                    TypeExpr::file(TypeExpr::prim("Image")),
                    TypeExpr::prim("Cbz"),
                )),
                "comic.cbz",
            )],
        ).unwrap();

        let op_names: Vec<&str> = trace.steps.iter()
            .filter(|s| s.kind == StepKind::Op)
            .map(|s| s.op_name.as_str())
            .collect();
        assert!(op_names.contains(&"extract_archive"), 
            "should use extract_archive, got: {:?}", op_names);
    }

    #[test]
    fn test_strategy_multi_step_dependency_order() {
        let strategy = FilesystemStrategy::new();
        let trace = strategy.dry_run(
            TypeExpr::seq(TypeExpr::entry(
                TypeExpr::prim("Name"),
                TypeExpr::file(TypeExpr::prim("Image")),
            )),
            vec![ExprLiteral::new(
                "archive1",
                TypeExpr::file(TypeExpr::archive(
                    TypeExpr::file(TypeExpr::prim("Image")),
                    TypeExpr::prim("Zip"),
                )),
                "photos.zip",
            )],
        ).unwrap();

        // Steps should be in dependency order: leaf first, then op
        assert!(trace.steps.len() >= 2, "should have at least 2 steps");
        assert_eq!(trace.steps[0].kind, StepKind::Leaf, "first step should be leaf input");
        let last = trace.steps.last().unwrap();
        assert_eq!(last.kind, StepKind::Op, "last step should be the operation");
    }

    #[test]
    fn test_strategy_empty_literals() {
        let strategy = FilesystemStrategy::new();
        // With no literals, planning should fail (no inputs to work with)
        let result = strategy.dry_run(
            TypeExpr::seq(TypeExpr::entry(
                TypeExpr::prim("Name"),
                TypeExpr::prim("Bytes"),
            )),
            vec![],
        );
        // This should fail — no Dir literal available to list
        assert!(result.is_err());
    }

    #[test]
    fn test_run_fs_goal_convenience() {
        let trace_str = run_fs_goal(
            TypeExpr::seq(TypeExpr::entry(
                TypeExpr::prim("Name"),
                TypeExpr::prim("Bytes"),
            )),
            vec![ExprLiteral::new(
                "d1",
                TypeExpr::dir(TypeExpr::prim("Bytes")),
                "/home/user",
            )],
        ).unwrap();

        assert!(trace_str.contains("Dry-Run Trace"));
        assert!(trace_str.contains("list_dir"));
        assert!(trace_str.contains("End Trace"));
    }

    #[test]
    fn test_trace_display_format() {
        let strategy = FilesystemStrategy::new();
        let trace = strategy.dry_run(
            TypeExpr::seq(TypeExpr::entry(
                TypeExpr::prim("Name"),
                TypeExpr::prim("Bytes"),
            )),
            vec![ExprLiteral::new(
                "d1",
                TypeExpr::dir(TypeExpr::prim("Bytes")),
                "/tmp",
            )],
        ).unwrap();

        let display = trace.to_string();
        // Should have structured format
        assert!(display.contains("[1]"), "should have step numbers");
        assert!(display.contains("[2]"), "should have step 2");
        assert!(display.contains("INPUT"), "should show INPUT for leaf");
        assert!(display.contains("OP"), "should show OP for operation");
        assert!(display.contains("output:"), "should show output type");
        assert!(display.contains("command:"), "should show command hint");
    }
}
