/// Unwrap Entry(K, V) → Some((K, V)), anything else → None.
fn unwrap_entry(ty: &TypeExpr) -> Option<(&TypeExpr, &TypeExpr)> {
    match ty {
        TypeExpr::Constructor(name, args) if name == "Entry" && args.len() == 2 => {
            Some((&args[0], &args[1]))
        }
        _ => None,
    }
}
use std::collections::HashMap;
use std::fmt;
use std::path::Path;

use serde::Deserialize;

use crate::fs_strategy::{DryRunTrace, StepKind, TraceStep};
use crate::fs_types::build_fs_registry;
use crate::generic_planner::ExprPlanNode;
use crate::registry::OperationRegistry;
use crate::type_expr::{TypeExpr, unify};

// ---------------------------------------------------------------------------
// Workflow YAML schema
// ---------------------------------------------------------------------------
//
// A workflow is a linear pipeline of filesystem operations:
//
//   workflow: "Find PDFs containing keyword"
//   inputs:
//     path: "~/Documents"
//     keyword: "contract"
//   steps:
//     - walk_tree
//     - filter:
//         extension: ".pdf"
//     - read_file: each
//     - search_content:
//         pattern: $keyword
//         mode: case-insensitive
//     - sort_by: name
//
// Rules:
//   - Bare string → op with no args
//   - op: scalar → op with a mode/flag
//   - op: map → op with named params
//   - $name → expand from inputs
//   - No $ → literal value or flag

// ---------------------------------------------------------------------------
// Raw YAML types (what serde parses)
// ---------------------------------------------------------------------------

/// Top-level workflow definition as parsed from YAML.
#[derive(Debug, Clone, Deserialize)]
pub struct WorkflowDef {
    /// Human-readable name for this workflow
    pub workflow: String,
    /// Named inputs (path, keyword, etc.)
    pub inputs: HashMap<String, String>,
    /// Ordered pipeline steps
    pub steps: Vec<RawStep>,
}

/// A single step in the workflow pipeline, as parsed from YAML.
///
/// YAML forms:
///   - `walk_tree`           → bare string, no params
///   - `read_file: each`     → op with scalar mode
///   - `filter: { ext: .pdf }` → op with named params
///   - `sort_by: name`       → op with scalar flag
#[derive(Debug, Clone)]
pub struct RawStep {
    /// The operation name (the YAML key)
    pub op: String,
    /// The step's arguments
    pub args: StepArgs,
}

/// Arguments to a workflow step.
#[derive(Debug, Clone, PartialEq)]
pub enum StepArgs {
    /// No arguments (bare string step)
    None,
    /// A single scalar value (mode or flag)
    Scalar(String),
    /// Named parameters (key-value pairs)
    Map(HashMap<String, String>),
}

impl StepArgs {
    /// Check if this step has the "each" mode.
    pub fn is_each(&self) -> bool {
        matches!(self, StepArgs::Scalar(s) if s == "each")
    }

    /// Get a named parameter value, with $var expansion.
    pub fn get_param(&self, key: &str, inputs: &HashMap<String, String>) -> Option<String> {
        match self {
            StepArgs::Map(map) => {
                map.get(key).map(|v| expand_var(v, inputs))
            }
            _ => None,
        }
    }

    /// Get the scalar value (if any), with $var expansion.
    pub fn scalar(&self, inputs: &HashMap<String, String>) -> Option<String> {
        match self {
            StepArgs::Scalar(s) => Some(expand_var(s, inputs)),
            _ => None,
        }
    }
}

/// Expand `$name` references to input values. Non-$ strings pass through.
fn expand_var(value: &str, inputs: &HashMap<String, String>) -> String {
    if let Some(var_name) = value.strip_prefix('$') {
        inputs.get(var_name).cloned().unwrap_or_else(|| value.to_string())
    } else {
        value.to_string()
    }
}

// ---------------------------------------------------------------------------
// Custom Deserialize for RawStep
// ---------------------------------------------------------------------------
//
// Each step in the YAML list is either:
//   - A bare string: "walk_tree"
//   - A single-key mapping: { read_file: each } or { filter: { extension: ".pdf" } }

impl<'de> Deserialize<'de> for RawStep {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::{self, MapAccess, Visitor};

        struct StepVisitor;

        impl<'de> Visitor<'de> for StepVisitor {
            type Value = RawStep;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_str("a string (bare op) or a single-key mapping (op: args)")
            }

            // Bare string: "walk_tree"
            fn visit_str<E: de::Error>(self, v: &str) -> Result<RawStep, E> {
                Ok(RawStep {
                    op: v.to_string(),
                    args: StepArgs::None,
                })
            }

            // Single-key mapping: { op: value }
            fn visit_map<M: MapAccess<'de>>(self, mut map: M) -> Result<RawStep, M::Error> {
                let (op, value): (String, serde_yaml::Value) = map
                    .next_entry()?
                    .ok_or_else(|| de::Error::custom("empty step mapping"))?;

                let args = match value {
                    serde_yaml::Value::Null => StepArgs::None,
                    serde_yaml::Value::Bool(b) => StepArgs::Scalar(b.to_string()),
                    serde_yaml::Value::Number(n) => StepArgs::Scalar(n.to_string()),
                    serde_yaml::Value::String(s) => StepArgs::Scalar(s),
                    serde_yaml::Value::Mapping(m) => {
                        let mut params = HashMap::new();
                        for (k, v) in m {
                            let key = match k {
                                serde_yaml::Value::String(s) => s,
                                other => other.as_str().unwrap_or("").to_string(),
                            };
                            let val = match v {
                                serde_yaml::Value::String(s) => s,
                                serde_yaml::Value::Number(n) => n.to_string(),
                                serde_yaml::Value::Bool(b) => b.to_string(),
                                other => format!("{:?}", other),
                            };
                            params.insert(key, val);
                        }
                        StepArgs::Map(params)
                    }
                    serde_yaml::Value::Sequence(_) => {
                        return Err(de::Error::custom(
                            "step value cannot be a sequence",
                        ));
                    }
                    serde_yaml::Value::Tagged(_) => {
                        return Err(de::Error::custom(
                            "step value cannot be a tagged value",
                        ));
                    }
                };

                Ok(RawStep { op, args })
            }
        }

        deserializer.deserialize_any(StepVisitor)
    }
}

// ---------------------------------------------------------------------------
// Workflow errors
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub enum WorkflowError {
    /// YAML parse error
    Parse(String),
    /// Unknown operation name
    UnknownOp { step: usize, op: String },
    /// Type mismatch between steps
    TypeMismatch {
        step: usize,
        op: String,
        expected: String,
        got: String,
    },
    /// Unknown $var reference
    UnknownVar { step: usize, var_name: String },
    /// Empty steps list
    EmptySteps,
    /// No inputs defined
    NoInputs,
    /// Input has no recognizable type
    UnknownInputType { name: String },
    /// Planning failed for a step
    PlanFailed { step: usize, op: String, reason: String },
}

impl fmt::Display for WorkflowError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parse(msg) => write!(f, "workflow parse error: {}", msg),
            Self::UnknownOp { step, op } => {
                write!(f, "step {}: unknown operation '{}'", step + 1, op)
            }
            Self::TypeMismatch { step, op, expected, got } => {
                write!(
                    f,
                    "step {}: type mismatch for '{}': expected {}, got {}",
                    step + 1, op, expected, got
                )
            }
            Self::UnknownVar { step, var_name } => {
                write!(f, "step {}: unknown variable '${}'", step + 1, var_name)
            }
            Self::EmptySteps => write!(f, "workflow has no steps"),
            Self::NoInputs => write!(f, "workflow has no inputs"),
            Self::UnknownInputType { name } => {
                write!(f, "cannot infer type for input '{}'", name)
            }
            Self::PlanFailed { step, op, reason } => {
                write!(f, "step {}: planning failed for '{}': {}", step + 1, op, reason)
            }
        }
    }
}

impl std::error::Error for WorkflowError {}

// ---------------------------------------------------------------------------
// Loading
// ---------------------------------------------------------------------------

/// Load a workflow definition from a YAML file.
pub fn load_workflow(path: &Path) -> Result<WorkflowDef, WorkflowError> {
    let content = std::fs::read_to_string(path)
        .map_err(|e| WorkflowError::Parse(format!("cannot read {}: {}", path.display(), e)))?;
    parse_workflow(&content)
}

/// Parse a workflow definition from a YAML string.
pub fn parse_workflow(yaml: &str) -> Result<WorkflowDef, WorkflowError> {
    let def: WorkflowDef =
        serde_yaml::from_str(yaml).map_err(|e| WorkflowError::Parse(e.to_string()))?;
    validate_workflow(&def)?;
    Ok(def)
}

/// Validate a parsed workflow for basic structural issues.
fn validate_workflow(def: &WorkflowDef) -> Result<(), WorkflowError> {
    if def.steps.is_empty() {
        return Err(WorkflowError::EmptySteps);
    }

    // Check for unknown $var references
    for (i, step) in def.steps.iter().enumerate() {
        if let StepArgs::Map(params) = &step.args {
            for (_, v) in params {
                if let Some(var_name) = v.strip_prefix('$') {
                    if !def.inputs.contains_key(var_name) {
                        return Err(WorkflowError::UnknownVar {
                            step: i,
                            var_name: var_name.to_string(),
                        });
                    }
                }
            }
        }
        if let StepArgs::Scalar(s) = &step.args {
            if let Some(var_name) = s.strip_prefix('$') {
                if !def.inputs.contains_key(var_name) {
                    return Err(WorkflowError::UnknownVar {
                        step: i,
                        var_name: var_name.to_string(),
                    });
                }
            }
        }
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Workflow compiler — resolve types step by step
// ---------------------------------------------------------------------------

/// A compiled workflow step with resolved types.
#[derive(Debug, Clone)]
pub struct CompiledStep {
    /// Step index (0-based)
    pub index: usize,
    /// Operation name
    pub op: String,
    /// Whether this is a map-each step
    pub is_each: bool,
    /// The resolved input type for this step
    pub input_type: TypeExpr,
    /// The resolved output type for this step
    pub output_type: TypeExpr,
    /// Resolved parameters (after $var expansion)
    pub params: HashMap<String, String>,
}

/// A fully compiled workflow ready for execution.
#[derive(Debug)]
pub struct CompiledWorkflow {
    /// Workflow name
    pub name: String,
    /// The initial input type (from the workflow's inputs)
    pub input_type: TypeExpr,
    /// The initial input literal description
    pub input_description: String,
    /// Compiled steps in order
    pub steps: Vec<CompiledStep>,
    /// The final output type
    pub output_type: TypeExpr,
}

/// Compile a workflow definition against the filesystem registry.
///
/// This resolves the type of each step by threading the output of the
/// previous step as the input to the next. The first step's input comes
/// from the workflow's `inputs`.
pub fn compile_workflow(
    def: &WorkflowDef,
    registry: &OperationRegistry,
) -> Result<CompiledWorkflow, WorkflowError> {
    if def.steps.is_empty() {
        return Err(WorkflowError::EmptySteps);
    }

    // Infer the initial input type from the workflow's inputs.
    // We try each input and pick the one whose inferred type best matches
    // the first step's expected input. Fallback: first directory/file input.
    if def.inputs.is_empty() {
        return Err(WorkflowError::NoInputs);
    }

    let first_op = registry.get_poly(&def.steps[0].op);
    let mut best_input: Option<(String, String, TypeExpr)> = None;

    for (name, value) in &def.inputs {
        let ty = infer_input_type(name, value)?;
        if let Some(op) = first_op {
            let (fresh, _) = op.signature.freshen("init");
            for inp in &fresh.inputs {
                if unify(inp, &ty).is_ok() {
                    best_input = Some((name.clone(), value.clone(), ty.clone()));
                    break;
                }
            }
        }
        if best_input.is_none() {
            // Fallback: pick the first input (deterministic via sorted keys)
            if best_input.is_none() {
                best_input = Some((name.clone(), value.clone(), ty));
            }
        }
    }

    let (_input_name, input_desc, input_type) = best_input.unwrap();

    let mut current_type = input_type.clone();
    let mut compiled_steps = Vec::new();

    for (i, step) in def.steps.iter().enumerate() {
        // Look up the operation in the registry
        let poly_op = registry.get_poly(&step.op).ok_or_else(|| {
            WorkflowError::UnknownOp {
                step: i,
                op: step.op.clone(),
            }
        })?;

        let is_each = step.args.is_each();

        // Resolve params
        let mut params = HashMap::new();
        if let StepArgs::Map(map) = &step.args {
            for (k, v) in map {
                params.insert(k.clone(), expand_var(v, &def.inputs));
            }
        } else if let StepArgs::Scalar(s) = &step.args {
            if s != "each" {
                params.insert("mode".to_string(), expand_var(s, &def.inputs));
            }
        }

        // Determine input/output types for this step.
        //
        // If `each` mode: the op applies element-wise inside a Seq.
        //   current_type must be Seq(X), op takes X, produces Y, result is Seq(Y).
        //
        // Otherwise: the op's first input must unify with current_type.
        //   The op may have additional inputs (Pattern, etc.) which we treat
        //   as implicit (provided by params or the environment).

        let (step_input, step_output) = if is_each {
            // current_type must be Seq(something)
            let inner = unwrap_seq(&current_type).ok_or_else(|| {
                WorkflowError::TypeMismatch {
                    step: i,
                    op: step.op.clone(),
                    expected: "Seq(...)".to_string(),
                    got: current_type.to_string(),
                }
            })?;

            let sig = &poly_op.signature;
            if sig.inputs.is_empty() {
                return Err(WorkflowError::TypeMismatch {
                    step: i,
                    op: step.op.clone(),
                    expected: "op with at least 1 input".to_string(),
                    got: "leaf op (no inputs)".to_string(),
                });
            }

            // Freshen and unify
            let (fresh, _) = poly_op.signature.freshen(&format!("wf{}", i));

            // Try direct unification first: op input unifies with inner
            if let Ok(sub) = unify(&fresh.inputs[0], inner) {
                let elem_output = fresh.output.apply_subst(&sub);
                let seq_output = TypeExpr::seq(elem_output);
                (current_type.clone(), seq_output)
            }
            // If inner is Entry(Name, X), try unifying op input with X
            // and produce Seq(Entry(Name, Y))
            else if let Some((key_type, val_type)) = unwrap_entry(inner) {
                let (fresh2, _) = poly_op.signature.freshen(&format!("wfe{}", i));
                match unify(&fresh2.inputs[0], val_type) {
                    Ok(sub) => {
                        let val_output = fresh2.output.apply_subst(&sub);
                        let entry_output = TypeExpr::entry(key_type.clone(), val_output);
                        let seq_output = TypeExpr::seq(entry_output);
                        (current_type.clone(), seq_output)
                    }
                    Err(_) => {
                        return Err(WorkflowError::TypeMismatch {
                            step: i,
                            op: step.op.clone(),
                            expected: format!("element value type matching {}", sig.inputs[0]),
                            got: val_type.to_string(),
                        });
                    }
                }
            } else {
                return Err(WorkflowError::TypeMismatch {
                    step: i,
                    op: step.op.clone(),
                    expected: format!("element type matching {}", sig.inputs[0]),
                    got: inner.to_string(),
                });
            }
        } else {
            // Normal mode: try to unify current_type with any of the op's inputs.
            // The matched input is the "data" input; others are implicit (from params).
            let sig = &poly_op.signature;
            if sig.inputs.is_empty() {
                // Leaf op — it produces output from nothing
                (current_type.clone(), sig.output.clone())
            } else {
                let (fresh, _) = poly_op.signature.freshen(&format!("wf{}", i));
                // Try each input position
                let mut matched = false;
                let mut step_output = fresh.output.clone();
                for input in &fresh.inputs {
                    if let Ok(sub) = unify(input, &current_type) {
                        step_output = fresh.output.apply_subst(&sub);
                        matched = true;
                        break;
                    }
                }
                if matched {
                    (current_type.clone(), step_output)
                } else {
                    return Err(WorkflowError::TypeMismatch {
                        step: i,
                        op: step.op.clone(),
                        expected: format!("one of {:?}", sig.inputs.iter().map(|i| i.to_string()).collect::<Vec<_>>()),
                        got: current_type.to_string(),
                    });
                }
            }
        };

        compiled_steps.push(CompiledStep {
            index: i,
            op: step.op.clone(),
            is_each,
            input_type: step_input,
            output_type: step_output.clone(),
            params,
        });

        current_type = step_output;
    }

    Ok(CompiledWorkflow {
        name: def.workflow.clone(),
        input_type: input_type,
        input_description: input_desc,
        steps: compiled_steps,
        output_type: current_type,
    })
}

/// Unwrap Seq(X) → Some(X), anything else → None.
fn unwrap_seq(ty: &TypeExpr) -> Option<&TypeExpr> {
    match ty {
        TypeExpr::Constructor(name, args) if name == "Seq" && args.len() == 1 => {
            Some(&args[0])
        }
        _ => None,
    }
}

/// Infer the TypeExpr for a workflow input based on its name and value.
///
/// Heuristics:
///   - name contains "path" or "dir" or value ends with "/" → Dir(Bytes)
///   - name contains "file" or value has a known extension → File(content)
///   - name contains "pattern" or "keyword" → Pattern
///   - otherwise → Bytes (generic)
fn infer_input_type(name: &str, value: &str) -> Result<TypeExpr, WorkflowError> {
    let name_lower = name.to_lowercase();
    let value_lower = value.to_lowercase();

    // Archive files
    if value_lower.ends_with(".cbz") {
        return Ok(TypeExpr::file(TypeExpr::archive(
            TypeExpr::file(TypeExpr::prim("Image")),
            TypeExpr::prim("Cbz"),
        )));
    }
    if value_lower.ends_with(".zip") {
        return Ok(TypeExpr::file(TypeExpr::archive(
            TypeExpr::prim("Bytes"),
            TypeExpr::prim("Zip"),
        )));
    }
    if value_lower.ends_with(".tar.gz") || value_lower.ends_with(".tgz") {
        return Ok(TypeExpr::file(TypeExpr::archive(
            TypeExpr::prim("Bytes"),
            TypeExpr::prim("TarGz"),
        )));
    }
    if value_lower.ends_with(".tar") {
        return Ok(TypeExpr::file(TypeExpr::archive(
            TypeExpr::prim("Bytes"),
            TypeExpr::prim("Tar"),
        )));
    }

    // Text files
    if value_lower.ends_with(".txt") || value_lower.ends_with(".md")
        || value_lower.ends_with(".rs") || value_lower.ends_with(".py")
        || value_lower.ends_with(".js") || value_lower.ends_with(".ts")
    {
        return Ok(TypeExpr::file(TypeExpr::prim("Text")));
    }

    // PDF files
    if value_lower.ends_with(".pdf") {
        return Ok(TypeExpr::file(TypeExpr::prim("PDF")));
    }

    // Directory (by name hint or trailing slash)
    if name_lower.contains("dir") || name_lower == "path"
        || value.ends_with('/') || value.starts_with("~/")
        || value.starts_with("/")
    {
        return Ok(TypeExpr::dir(TypeExpr::prim("Bytes")));
    }

    // Pattern/keyword
    if name_lower.contains("pattern") || name_lower.contains("keyword")
        || name_lower.contains("query") || name_lower.contains("search")
    {
        return Ok(TypeExpr::prim("Pattern"));
    }

    // Size
    if name_lower.contains("size") {
        return Ok(TypeExpr::prim("Size"));
    }

    // Generic fallback
    Ok(TypeExpr::prim("Bytes"))
}

// ---------------------------------------------------------------------------
// Workflow executor — produce DryRunTrace from compiled workflow
// ---------------------------------------------------------------------------

/// Execute a compiled workflow and produce a combined DryRunTrace.
///
/// Each compiled step becomes one or more trace steps showing what
/// operations would be performed.
pub fn execute_workflow(
    compiled: &CompiledWorkflow,
    registry: &OperationRegistry,
) -> Result<DryRunTrace, WorkflowError> {
    let mut trace_steps = Vec::new();
    let mut step_num = 0;

    // Step 0: the input literal
    step_num += 1;
    trace_steps.push(TraceStep {
        step: step_num,
        op_name: compiled.input_description.clone(),
        kind: StepKind::Leaf,
        inputs: vec![],
        output: compiled.input_type.to_string(),
        command_hint: format!("# input: {}", compiled.input_description),
    });

    // Each compiled step becomes a trace step
    for cs in &compiled.steps {
        step_num += 1;

        let hint = registry
            .get_poly(&cs.op)
            .map(|op| {
                let mut desc = op.description.clone();
                if !cs.params.is_empty() {
                    let param_str: Vec<String> = cs.params.iter()
                        .map(|(k, v)| format!("{}={}", k, v))
                        .collect();
                    desc = format!("{} [{}]", desc, param_str.join(", "));
                }
                desc
            })
            .unwrap_or_else(|| format!("<{}>", cs.op));

        let kind = if cs.is_each { StepKind::Map } else { StepKind::Op };

        let op_display = if cs.is_each {
            format!("map({})", cs.op)
        } else {
            cs.op.clone()
        };

        trace_steps.push(TraceStep {
            step: step_num,
            op_name: op_display,
            kind,
            inputs: vec![cs.input_type.to_string()],
            output: cs.output_type.to_string(),
            command_hint: hint,
        });
    }

    // Build the plan tree for display purposes
    // (a linear chain: each step wraps the previous)
    let plan = build_plan_chain(compiled);

    Ok(DryRunTrace {
        goal: compiled.output_type.clone(),
        steps: trace_steps,
        plan,
    })
}

/// Build a linear ExprPlanNode chain from the compiled workflow.
fn build_plan_chain(compiled: &CompiledWorkflow) -> ExprPlanNode {
    let mut current = ExprPlanNode::Leaf {
        key: compiled.input_description.clone(),
        output_type: compiled.input_type.clone(),
    };

    for cs in &compiled.steps {
        if cs.is_each {
            current = ExprPlanNode::Map {
                op_name: cs.op.clone(),
                elem_output: cs.output_type.clone(),
                child: Box::new(current),
            };
        } else {
            current = ExprPlanNode::Op {
                op_name: cs.op.clone(),
                output_type: cs.output_type.clone(),
                children: vec![current],
            };
        }
    }

    current
}

// ---------------------------------------------------------------------------
// Convenience: load + compile + execute in one call
// ---------------------------------------------------------------------------

/// Load a workflow YAML file, compile it, and produce a dry-run trace.
pub fn run_workflow(path: &Path) -> Result<DryRunTrace, WorkflowError> {
    let def = load_workflow(path)?;
    let registry = build_fs_registry();
    let compiled = compile_workflow(&def, &registry)?;
    execute_workflow(&compiled, &registry)
}

/// Parse a workflow YAML string, compile it, and produce a dry-run trace.
pub fn run_workflow_str(yaml: &str) -> Result<DryRunTrace, WorkflowError> {
    let def = parse_workflow(yaml)?;
    let registry = build_fs_registry();
    let compiled = compile_workflow(&def, &registry)?;
    execute_workflow(&compiled, &registry)
}

// ---------------------------------------------------------------------------
// Display for CompiledWorkflow
// ---------------------------------------------------------------------------

impl fmt::Display for CompiledWorkflow {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Workflow: {}", self.name)?;
        writeln!(f, "Input: {} ({})", self.input_description, self.input_type)?;
        writeln!(f, "Steps:")?;
        for cs in &self.steps {
            let each_marker = if cs.is_each { " [each]" } else { "" };
            writeln!(
                f,
                "  {}. {}{}: {} → {}",
                cs.index + 1,
                cs.op,
                each_marker,
                cs.input_type,
                cs.output_type,
            )?;
            for (k, v) in &cs.params {
                writeln!(f, "     {} = {}", k, v)?;
            }
        }
        writeln!(f, "Output: {}", self.output_type)
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // -- Parsing tests --

    #[test]
    fn test_parse_bare_string_step() {
        let yaml = r#"
workflow: "test"
inputs:
  path: "/tmp"
steps:
  - walk_tree
"#;
        let def = parse_workflow(yaml).unwrap();
        assert_eq!(def.steps.len(), 1);
        assert_eq!(def.steps[0].op, "walk_tree");
        assert_eq!(def.steps[0].args, StepArgs::None);
    }

    #[test]
    fn test_parse_scalar_step() {
        let yaml = r#"
workflow: "test"
inputs:
  path: "/tmp"
steps:
  - read_file: each
"#;
        let def = parse_workflow(yaml).unwrap();
        assert_eq!(def.steps[0].op, "read_file");
        assert_eq!(def.steps[0].args, StepArgs::Scalar("each".into()));
        assert!(def.steps[0].args.is_each());
    }

    #[test]
    fn test_parse_map_step() {
        let yaml = r#"
workflow: "test"
inputs:
  path: "/tmp"
steps:
  - filter:
      extension: ".pdf"
"#;
        let def = parse_workflow(yaml).unwrap();
        assert_eq!(def.steps[0].op, "filter");
        match &def.steps[0].args {
            StepArgs::Map(m) => {
                assert_eq!(m.get("extension").unwrap(), ".pdf");
            }
            other => panic!("expected Map, got: {:?}", other),
        }
    }

    #[test]
    fn test_parse_var_expansion() {
        let yaml = r#"
workflow: "test"
inputs:
  path: "/tmp"
  keyword: "hello"
steps:
  - search_content:
      pattern: $keyword
"#;
        let def = parse_workflow(yaml).unwrap();
        match &def.steps[0].args {
            StepArgs::Map(m) => {
                assert_eq!(m.get("pattern").unwrap(), "$keyword");
                // Expansion happens at compile time, not parse time
                let expanded = def.steps[0].args.get_param("pattern", &def.inputs);
                assert_eq!(expanded, Some("hello".to_string()));
            }
            other => panic!("expected Map, got: {:?}", other),
        }
    }

    #[test]
    fn test_parse_unknown_var_rejected() {
        let yaml = r#"
workflow: "test"
inputs:
  path: "/tmp"
steps:
  - search_content:
      pattern: $nonexistent
"#;
        let result = parse_workflow(yaml);
        assert!(result.is_err());
        match result.unwrap_err() {
            WorkflowError::UnknownVar { step, var_name } => {
                assert_eq!(step, 0);
                assert_eq!(var_name, "nonexistent");
            }
            other => panic!("expected UnknownVar, got: {}", other),
        }
    }

    #[test]
    fn test_parse_empty_steps_rejected() {
        let yaml = r#"
workflow: "test"
inputs:
  path: "/tmp"
steps: []
"#;
        let result = parse_workflow(yaml);
        assert!(result.is_err());
        match result.unwrap_err() {
            WorkflowError::EmptySteps => {}
            other => panic!("expected EmptySteps, got: {}", other),
        }
    }

    #[test]
    fn test_parse_multi_step_workflow() {
        let yaml = r#"
workflow: "Find PDFs containing keyword"
inputs:
  path: "~/Documents"
  keyword: "contract"
steps:
  - walk_tree
  - filter:
      extension: ".pdf"
  - read_file: each
  - search_content:
      pattern: $keyword
      mode: case-insensitive
  - sort_by: name
"#;
        let def = parse_workflow(yaml).unwrap();
        assert_eq!(def.workflow, "Find PDFs containing keyword");
        assert_eq!(def.inputs.len(), 2);
        assert_eq!(def.steps.len(), 5);

        assert_eq!(def.steps[0].op, "walk_tree");
        assert_eq!(def.steps[0].args, StepArgs::None);

        assert_eq!(def.steps[1].op, "filter");
        assert!(matches!(&def.steps[1].args, StepArgs::Map(_)));

        assert_eq!(def.steps[2].op, "read_file");
        assert!(def.steps[2].args.is_each());

        assert_eq!(def.steps[3].op, "search_content");
        assert!(matches!(&def.steps[3].args, StepArgs::Map(_)));

        assert_eq!(def.steps[4].op, "sort_by");
        assert_eq!(def.steps[4].args, StepArgs::Scalar("name".into()));
    }

    #[test]
    fn test_parse_missing_inputs_is_error() {
        let yaml = r#"
workflow: "test"
steps:
  - walk_tree
"#;
        let result = parse_workflow(yaml);
        assert!(result.is_err(), "missing inputs should fail: {:?}", result);
    }

    // -- Input type inference tests --

    #[test]
    fn test_infer_directory_type() {
        let ty = infer_input_type("path", "~/Documents").unwrap();
        assert_eq!(ty, TypeExpr::dir(TypeExpr::prim("Bytes")));
    }

    #[test]
    fn test_infer_cbz_type() {
        let ty = infer_input_type("archive", "comic.cbz").unwrap();
        assert_eq!(
            ty,
            TypeExpr::file(TypeExpr::archive(
                TypeExpr::file(TypeExpr::prim("Image")),
                TypeExpr::prim("Cbz"),
            ))
        );
    }

    #[test]
    fn test_infer_zip_type() {
        let ty = infer_input_type("file", "data.zip").unwrap();
        assert_eq!(
            ty,
            TypeExpr::file(TypeExpr::archive(
                TypeExpr::prim("Bytes"),
                TypeExpr::prim("Zip"),
            ))
        );
    }

    #[test]
    fn test_infer_keyword_type() {
        let ty = infer_input_type("keyword", "contract").unwrap();
        assert_eq!(ty, TypeExpr::prim("Pattern"));
    }

    // -- Compiler tests --

    #[test]
    fn test_compile_single_step() {
        let yaml = r#"
workflow: "Extract CBZ"
inputs:
  path: "comic.cbz"
steps:
  - extract_archive
"#;
        let def = parse_workflow(yaml).unwrap();
        let registry = build_fs_registry();
        let compiled = compile_workflow(&def, &registry).unwrap();

        assert_eq!(compiled.steps.len(), 1);
        assert_eq!(compiled.steps[0].op, "extract_archive");
        assert!(!compiled.steps[0].is_each);
        // Output should be Seq(Entry(Name, File(Image)))
        let out = &compiled.output_type;
        assert!(out.to_string().contains("Seq"), "output should be Seq: {}", out);
    }

    #[test]
    fn test_compile_unknown_op_rejected() {
        let yaml = r#"
workflow: "test"
inputs:
  path: "/tmp"
steps:
  - nonexistent_op
"#;
        let def = parse_workflow(yaml).unwrap();
        let registry = build_fs_registry();
        let result = compile_workflow(&def, &registry);
        assert!(result.is_err());
        match result.unwrap_err() {
            WorkflowError::UnknownOp { step, op } => {
                assert_eq!(step, 0);
                assert_eq!(op, "nonexistent_op");
            }
            other => panic!("expected UnknownOp, got: {}", other),
        }
    }

    #[test]
    fn test_compile_walk_then_filter() {
        let yaml = r#"
workflow: "List and filter"
inputs:
  path: "~/Documents"
steps:
  - list_dir
  - filter:
      extension: ".pdf"
"#;
        let def = parse_workflow(yaml).unwrap();
        let registry = build_fs_registry();
        let compiled = compile_workflow(&def, &registry).unwrap();

        assert_eq!(compiled.steps.len(), 2);
        assert_eq!(compiled.steps[0].op, "list_dir");
        assert_eq!(compiled.steps[1].op, "filter");
        // filter's output should still be a Seq (filter preserves type)
        let out = &compiled.output_type;
        assert!(out.to_string().contains("Seq"), "output should be Seq: {}", out);
    }

    #[test]
    fn test_compile_each_mode() {
        let yaml = r#"
workflow: "Extract and read"
inputs:
  archive: "photos.cbz"
steps:
  - extract_archive
  - read_file: each
"#;
        let def = parse_workflow(yaml).unwrap();
        let registry = build_fs_registry();
        let compiled = compile_workflow(&def, &registry).unwrap();

        assert_eq!(compiled.steps.len(), 2);
        assert!(compiled.steps[1].is_each);
        // extract_archive: File(Archive(File(Image), Cbz)) → Seq(Entry(Name, File(Image)))
        // read_file each: applies File(a)→a to each entry value → Seq(Entry(Name, Image))
        let out = &compiled.output_type;
        assert!(out.to_string().contains("Seq"), "output should be Seq: {}", out);
    }

    #[test]
    fn test_compile_type_mismatch_detected() {
        // stat takes Path, but walk_tree produces Tree(Entry(Name, Bytes))
        // stat after walk_tree should fail
        let yaml = r#"
workflow: "bad"
inputs:
  path: "~/docs"
steps:
  - walk_tree
  - stat
"#;
        let def = parse_workflow(yaml).unwrap();
        let registry = build_fs_registry();
        let result = compile_workflow(&def, &registry);
        assert!(result.is_err(), "stat after walk_tree should be a type mismatch");
        match result.unwrap_err() {
            WorkflowError::TypeMismatch { step, op, .. } => {
                assert_eq!(step, 1);
                assert_eq!(op, "stat");
            }
            other => panic!("expected TypeMismatch, got: {}", other),
        }
    }

    // -- Executor tests --

    #[test]
    fn test_execute_single_step() {
        let yaml = r#"
workflow: "Extract CBZ"
inputs:
  path: "comic.cbz"
steps:
  - extract_archive
"#;
        let def = parse_workflow(yaml).unwrap();
        let registry = build_fs_registry();
        let compiled = compile_workflow(&def, &registry).unwrap();
        let trace = execute_workflow(&compiled, &registry).unwrap();

        assert_eq!(trace.steps.len(), 2); // input + extract_archive
        assert_eq!(trace.steps[0].kind, StepKind::Leaf);
        assert_eq!(trace.steps[1].kind, StepKind::Op);
        assert_eq!(trace.steps[1].op_name, "extract_archive");
    }

    #[test]
    fn test_execute_multi_step() {
        let yaml = r#"
workflow: "List and sort"
inputs:
  path: "~/Documents"
steps:
  - list_dir
  - sort_by: name
"#;
        let def = parse_workflow(yaml).unwrap();
        let registry = build_fs_registry();
        let compiled = compile_workflow(&def, &registry).unwrap();
        let trace = execute_workflow(&compiled, &registry).unwrap();

        assert_eq!(trace.steps.len(), 3); // input + list_dir + sort_by
        assert_eq!(trace.steps[0].kind, StepKind::Leaf);
        assert_eq!(trace.steps[1].op_name, "list_dir");
        assert_eq!(trace.steps[2].op_name, "sort_by");
    }

    #[test]
    fn test_execute_each_shows_map() {
        // Use extract_archive to get Seq(Entry(Name, File(Image)))
        // then read_file: each to get Seq(Entry(Name, Image))
        // Actually read_file takes File(a) and Entry value is File(Image)
        // so read_file: each on Seq(Entry(Name, File(Image))) should work
        let yaml = r#"
workflow: "Extract and read"
inputs:
  archive: "photos.cbz"
steps:
  - extract_archive
  - read_file: each
"#;
        let def = parse_workflow(yaml).unwrap();
        let registry = build_fs_registry();
        let compiled = compile_workflow(&def, &registry).unwrap();
        let trace = execute_workflow(&compiled, &registry).unwrap();

        // Find the MAP step
        let map_steps: Vec<&TraceStep> = trace.steps.iter()
            .filter(|s| s.kind == StepKind::Map)
            .collect();
        assert!(!map_steps.is_empty(), "should have a MAP step");
        assert!(map_steps[0].op_name.contains("read_file"));
    }

    // -- End-to-end tests --

    #[test]
    fn test_run_workflow_str_cbz() {
        let yaml = r#"
workflow: "Extract CBZ"
inputs:
  path: "comic.cbz"
steps:
  - extract_archive
"#;
        let trace = run_workflow_str(yaml).unwrap();
        let display = trace.to_string();
        assert!(display.contains("extract_archive"), "trace: {}", display);
        assert!(display.contains("Dry-Run Trace"), "trace: {}", display);
    }

    #[test]
    fn test_run_workflow_str_walk_sort() {
        let yaml = r#"
workflow: "List and sort"
inputs:
  path: "~/docs"
steps:
  - list_dir
  - sort_by: name
"#;
        let trace = run_workflow_str(yaml).unwrap();
        let display = trace.to_string();
        assert!(display.contains("list_dir"), "trace: {}", display);
        assert!(display.contains("sort_by"), "trace: {}", display);
    }

    #[test]
    fn test_compiled_workflow_display() {
        let yaml = r#"
workflow: "List and sort"
inputs:
  path: "~/docs"
steps:
  - list_dir
  - sort_by: name
"#;
        let def = parse_workflow(yaml).unwrap();
        let registry = build_fs_registry();
        let compiled = compile_workflow(&def, &registry).unwrap();
        let display = format!("{}", compiled);
        assert!(display.contains("List and sort"));
        assert!(display.contains("list_dir"));
        assert!(display.contains("sort_by"));
    }
}
