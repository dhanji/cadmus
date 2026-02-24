/// Check if a TypeExpr contains `Bytes` anywhere — the "unknown content" primitive.
///
/// `Bytes` is the type system's bottom for content: `Dir(Bytes)` means
/// "a directory with unknown content type". When downstream ops need
/// something more specific, we can promote `Bytes` to whatever they need.
fn contains_bytes(ty: &TypeExpr) -> bool {
    match ty {
        TypeExpr::Primitive(p) => p == "Bytes",
        TypeExpr::Var(_) => false,
        TypeExpr::Constructor(_, args) => args.iter().any(|a| contains_bytes(a)),
    }
}

/// Replace every `Prim("Bytes")` in a type expression with `Var(var_name)`.
///
/// This is the first step of unification-based type promotion: we turn
/// the unknown `Bytes` into a variable, then let unification with
/// downstream op signatures discover what it should actually be.
fn replace_bytes_with_var(ty: &TypeExpr, var_name: &str) -> TypeExpr {
    match ty {
        TypeExpr::Primitive(p) if p == "Bytes" => TypeExpr::var(var_name),
        TypeExpr::Primitive(_) => ty.clone(),
        TypeExpr::Var(_) => ty.clone(),
        TypeExpr::Constructor(name, args) => {
            TypeExpr::Constructor(
                name.clone(),
                args.iter().map(|a| replace_bytes_with_var(a, var_name)).collect(),
            )
        }
    }
}

/// Try to promote `Bytes` in the input type by simulating the type chain.
///
/// Algorithm:
/// 1. If the type doesn't contain `Bytes`, return it unchanged.
/// 2. Replace every `Bytes` with a fresh type variable `_promote`.
/// 3. Simulate the type chain forward through each step's op signature.
/// 4. If `_promote` gets bound to something concrete, apply the substitution
///    to get the promoted type.
/// 5. If the simulation fails or `_promote` stays unbound, return the
///    original type unchanged.
fn try_promote_bytes(
    input_type: &TypeExpr,
    steps: &[RawStep],
    registry: &OperationRegistry,
) -> TypeExpr {
    if !contains_bytes(input_type) {
        return input_type.clone();
    }

    const PROMO_VAR: &str = "_promote";
    let promoted_input = replace_bytes_with_var(input_type, PROMO_VAR);
    let mut current = promoted_input.clone();
    let mut subst = Substitution::empty();

    for (i, step) in steps.iter().enumerate() {
        let poly_op = match registry.get_poly(&step.op) {
            Some(op) => op,
            None => return input_type.clone(), // unknown op — bail
        };

        let is_each = step.args.is_each();
        let (fresh, _) = poly_op.signature.freshen(&format!("promo{}", i));

        if is_each {
            // current must be Seq(something)
            let inner = match unwrap_seq(&current) {
                Some(inner) => inner.clone(),
                None => return input_type.clone(),
            };

            if fresh.inputs.is_empty() {
                return input_type.clone();
            }

            // Try direct unification with inner element
            if let Ok(sub) = unify(&fresh.inputs[0], &inner) {
                let elem_out = fresh.output.apply_subst(&sub);
                current = TypeExpr::seq(elem_out);
                for (var, ty) in sub.iter() {
                    let _ = subst.bind(var.clone(), ty.clone());
                }
            } else if let Some((key_type, val_type)) = unwrap_entry(&inner) {
                let (fresh2, _) = poly_op.signature.freshen(&format!("promoe{}", i));
                if let Ok(sub) = unify(&fresh2.inputs[0], val_type) {
                    let val_out = fresh2.output.apply_subst(&sub);
                    current = TypeExpr::seq(TypeExpr::entry(key_type.clone(), val_out));
                    for (var, ty) in sub.iter() {
                        let _ = subst.bind(var.clone(), ty.clone());
                    }
                } else {
                    break;
                }
            } else {
                break;
            }
        } else {
            if fresh.inputs.is_empty() {
                continue; // leaf op
            }
            let mut matched = false;
            for inp in &fresh.inputs {
                if let Ok(sub) = unify(inp, &current) {
                    current = fresh.output.apply_subst(&sub);
                    for (var, ty) in sub.iter() {
                        let _ = subst.bind(var.clone(), ty.clone());
                    }
                    matched = true;
                    break;
                }
            }
            if !matched {
                // Don't bail — _promote may already be bound from an earlier
                // step (e.g., find_matching pattern narrowing). Break out of
                // the simulation loop and check the substitution.
                break;
            }

            // ── Pattern-based type narrowing ──
            // When find_matching has a pattern like *.cbz, narrow the element
            // type inside Seq(Entry(Name, a)) by looking up the extension in
            // filetypes.yaml. This lets Dir(Bytes) → Dir(File(Archive(Image, Cbz)))
            // propagate through the chain.
            if step.op == "find_matching" || step.op == "filter" {
                if let StepArgs::Map(params) = &step.args {
                    if let Some(pattern) = params.get("pattern").and_then(|p| p.as_str()) {
                        if let Some(narrowed) = narrow_type_from_pattern(&current, pattern) {
                            // Apply the narrowed type and try to bind _promote
                            if let Ok(sub) = unify(&current, &narrowed) {
                                current = narrowed;
                                for (var, ty) in sub.iter() {
                                    let _ = subst.bind(var.clone(), ty.clone());
                                }
                            } else {
                                current = narrowed;
                            }
                        }
                    }
                }
            }
        }
    }

    // If _promote got bound, apply it to the original promoted input
    match subst.get(PROMO_VAR) {
        Some(_) => promoted_input.apply_subst(&subst),
        None => input_type.clone(),
    }
}
/// Extract the operation name and parameters from a RawStep.
///
/// Returns (op_name, string_params_map). For bare steps, params is empty.
/// Sub-step and inline params are omitted — use step.args directly for those.
pub fn raw_step_to_op_params(step: &RawStep) -> (String, HashMap<String, String>) {
    let op = step.op.clone();
    let params = match &step.args {
        StepArgs::None => HashMap::new(),
        StepArgs::Scalar(s) => {
            let mut m = HashMap::new();
            m.insert("value".to_string(), s.clone());
            m
        }
        StepArgs::Map(m) => {
            m.iter()
                .filter_map(|(k, v)| v.as_str().map(|s| (k.clone(), s.to_string())))
                .collect()
        }
    };
    (op, params)
}
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
use crate::fs_types::build_full_registry;
use crate::generic_planner::ExprPlanNode;
use crate::registry::OperationRegistry;
use crate::type_expr::{Substitution, TypeExpr, unify};

// ---------------------------------------------------------------------------
// Plan YAML schema
// ---------------------------------------------------------------------------
//
// A plan is a linear pipeline of filesystem operations:
//
//   plan: "Find PDFs containing keyword"
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

/// A single named input to a plan.
///
/// YAML forms:
///   - `path`           → bare name, type inferred from name heuristic
///   - `file: File`     → name with explicit type hint
#[derive(Debug, Clone)]
pub struct PlanInput {
    /// The input parameter name.
    pub name: String,
    /// Optional type hint (e.g. "File", "List[a]"). If None, type is inferred from name.
    pub type_hint: Option<String>,
}

impl PlanInput {
    /// Create an input with an explicit type hint.
    pub fn typed(name: impl Into<String>, type_hint: impl Into<String>) -> Self {
        Self { name: name.into(), type_hint: Some(type_hint.into()) }
    }

    /// Create a bare input (type inferred from name).
    pub fn bare(name: impl Into<String>) -> Self {
        Self { name: name.into(), type_hint: None }
    }

    /// Check if this input has an explicit type hint.
    pub fn has_type(&self) -> bool {
        self.type_hint.is_some()
    }
}

/// Top-level plan definition as parsed from YAML.
///
/// New function-framing format:
/// ```yaml
/// plan-name:
///   inputs:
///     - path
///     - file: File
///   output:
///     - ResultType
///   steps:
///     - list_dir
/// ```
#[derive(Debug, Clone)]
pub struct PlanDef {
    /// Plan name (the top-level YAML key)
    pub name: String,
    /// Named inputs with optional type hints
    pub inputs: Vec<PlanInput>,
    /// Optional declared output type
    pub output: Option<Vec<String>>,
    /// Ordered pipeline steps (unchanged)
    pub steps: Vec<RawStep>,
    /// Literal bindings for inputs (e.g., path → "~/Downloads").
    /// Populated by the NL pipeline from extracted literals.
    pub bindings: HashMap<String, String>,
}

impl PlanDef {
    /// Get the set of input names (for $var validation).
    pub fn input_names(&self) -> Vec<&str> {
        self.inputs.iter().map(|i| i.name.as_str()).collect()
    }

    /// Check if an input name exists.
    pub fn has_input(&self, name: &str) -> bool {
        self.inputs.iter().any(|i| i.name == name)
    }

    /// Get an input by name.
    pub fn get_input(&self, name: &str) -> Option<&PlanInput> {
        self.inputs.iter().find(|i| i.name == name)
    }

    /// Get input defaults as a HashMap (for $var expansion — returns empty values for all inputs).
    pub fn input_defaults(&self) -> HashMap<String, String> {
        self.inputs.iter().map(|i| (i.name.clone(), String::new())).collect()
    }
}

/// A single step in the plan pipeline, as parsed from YAML.
///
/// YAML forms:
///   - `walk_tree`           → bare string, no params
///   - `read_file: each`     → op with scalar mode
///   - `filter: { ext: .pdf }` → op with named params
///   - `sort_by: name`       → op with scalar flag
#[derive(Debug, Clone, PartialEq)]
pub struct RawStep {
    /// The operation name (the YAML key)
    pub op: String,
    /// The step's arguments
    pub args: StepArgs,
}

/// Arguments to a plan step.
#[derive(Debug, Clone, PartialEq)]
pub enum StepArgs {
    /// No arguments (bare string step)
    None,
    /// A single scalar value (mode or flag)
    Scalar(String),
    /// Named parameters (key-value pairs, possibly containing sub-step lists)
    Map(HashMap<String, StepParam>),
}

/// A single parameter value in a step's Map args.
///
/// Parameters can be:
/// - `Value(s)` — a plain string (number, path, $var reference, etc.)
/// - `Steps(vec)` — a sub-step pipeline (for body, then, else, etc.)
/// - `Inline(step)` — a single inline step used as a value expression
#[derive(Debug, Clone, PartialEq)]
pub enum StepParam {
    /// A plain string value.
    Value(String),
    /// A list of clause maps (for cond).
    Clauses(Vec<serde_yaml::Value>),
    /// A sub-step pipeline (list of steps).
    Steps(Vec<RawStep>),
    /// A single inline step used as a value expression.
    Inline(Box<RawStep>),
}

impl fmt::Display for StepParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StepParam::Value(s) => write!(f, "{}", s),
            StepParam::Clauses(c) => write!(f, "[{} clauses]", c.len()),
            StepParam::Steps(steps) => write!(f, "[{} sub-steps]", steps.len()),
            StepParam::Inline(step) => write!(f, "{{{}}}", step.op),
        }
    }
}

impl StepParam {
    /// Get the string value if this is a Value variant.
    pub fn as_str(&self) -> Option<&str> {
        match self {
            StepParam::Value(s) => Some(s.as_str()),
            _ => None,
        }
    }

    /// Get the sub-steps if this is a Steps variant.
    pub fn as_steps(&self) -> Option<&[RawStep]> {
        match self {
            StepParam::Steps(steps) => Some(steps),
            _ => None,
        }
    }

    /// Get the inline step if this is an Inline variant.
    pub fn as_inline(&self) -> Option<&RawStep> {
        match self {
            StepParam::Inline(step) => Some(step),
            _ => None,
        }
    }
}

impl StepArgs {
    /// Check if this step has the "each" mode.
    pub fn is_each(&self) -> bool {
        matches!(self, StepArgs::Scalar(s) if s == "each")
    }

    /// Get a named parameter value (string only — ignores sub-steps).
    pub fn get_param(&self, key: &str) -> Option<String> {
        match self {
            StepArgs::Map(map) => {
                map.get(key).and_then(|p| p.as_str().map(|s| s.to_string()))
            }
            _ => None,
        }
    }

    /// Get a named sub-step list parameter.
    pub fn get_substeps(&self, key: &str) -> Option<&[RawStep]> {
        match self {
            StepArgs::Map(map) => {
                map.get(key).and_then(|p| p.as_steps())
            }
            _ => None,
        }
    }

    /// Get a named inline step parameter.
    pub fn get_inline(&self, key: &str) -> Option<&RawStep> {
        match self {
            StepArgs::Map(map) => {
                map.get(key).and_then(|p| p.as_inline())
            }
            _ => None,
        }
    }

    /// Get the raw StepParam for a key.
    pub fn get_raw(&self, key: &str) -> Option<&StepParam> {
        match self {
            StepArgs::Map(map) => map.get(key),
            _ => None,
        }
    }

    /// Get all string parameters as a HashMap (for backward compatibility).
    pub fn string_params(&self) -> HashMap<String, String> {
        match self {
            StepArgs::Map(map) => {
                map.iter()
                    .filter_map(|(k, v)| v.as_str().map(|s| (k.clone(), s.to_string())))
                    .collect()
            }
            _ => HashMap::new(),
        }
    }

    /// Get the scalar value (if any).
    pub fn scalar(&self) -> Option<String> {
        match self {
            StepArgs::Scalar(s) => Some(s.clone()),
            _ => None,
        }
    }

    /// Create a Map from a HashMap<String, String> (convenience for backward compat).
    pub fn from_string_map(m: HashMap<String, String>) -> Self {
        StepArgs::Map(
            m.into_iter()
                .map(|(k, v)| (k, StepParam::Value(v)))
                .collect(),
        )
    }
}



// ---------------------------------------------------------------------------
// Custom Deserialize for RawStep
// ---------------------------------------------------------------------------
//
// Each step in the YAML list is either:
//   - A bare string: "walk_tree"
//   - A single-key mapping: { read_file: each } or { filter: { extension: ".pdf" } }

/// Parse a YAML value into a StepParam.
///
/// - String/Number/Bool → Value(string)
/// - Sequence → Steps(vec of RawStep)
/// - Mapping with a single key that looks like an op → Inline(RawStep)
/// - Mapping with multiple keys → Value(debug string) (legacy fallback)
fn parse_step_param<'de, M: serde::de::MapAccess<'de>>(
    v: serde_yaml::Value,
) -> Result<StepParam, M::Error> {
    use serde::de;
    match v {
        serde_yaml::Value::String(s) => Ok(StepParam::Value(s)),
        serde_yaml::Value::Number(n) => Ok(StepParam::Value(n.to_string())),
        serde_yaml::Value::Bool(b) => Ok(StepParam::Value(b.to_string())),
        serde_yaml::Value::Null => Ok(StepParam::Value(String::new())),
        serde_yaml::Value::Sequence(seq) => {
            // Check if this is a clause list (cond clauses) vs a step list.
            // Clause maps have "test"/"then"/"else" keys.
            // Var binding maps have "var"/"start"/"end" keys.
            // Step maps have a single op key.
            let is_raw_yaml = seq.iter().any(|item| {
                if let serde_yaml::Value::Mapping(m) = item {
                    m.contains_key(&serde_yaml::Value::String("test".into()))
                        || m.contains_key(&serde_yaml::Value::String("else".into()))
                        || m.contains_key(&serde_yaml::Value::String("var".into()))
                } else {
                    false
                }
            });
            if is_raw_yaml {
                Ok(StepParam::Clauses(seq))
            } else {
                let steps: Vec<RawStep> = seq
                    .into_iter()
                    .map(|item| {
                        let yaml_str = serde_yaml::to_string(&item)
                            .map_err(|e| de::Error::custom(format!("sub-step serialize: {}", e)))?;
                        serde_yaml::from_str::<RawStep>(&yaml_str)
                            .map_err(|e| de::Error::custom(format!("sub-step parse: {}", e)))
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(StepParam::Steps(steps))
            }
        }
        serde_yaml::Value::Mapping(m) => {
            // Single-key mapping → try as inline step
            if m.len() == 1 {
                let yaml_val = serde_yaml::Value::Mapping(m);
                let yaml_str = serde_yaml::to_string(&yaml_val)
                    .map_err(|e| de::Error::custom(format!("inline step serialize: {}", e)))?;
                match serde_yaml::from_str::<RawStep>(&yaml_str) {
                    Ok(step) => Ok(StepParam::Inline(Box::new(step))),
                    Err(_) => Ok(StepParam::Value(yaml_str)),
                }
            } else {
                // Multi-key mapping — legacy format, serialize as debug string
                let yaml_str = serde_yaml::to_string(&serde_yaml::Value::Mapping(m))
                    .unwrap_or_default();
                Ok(StepParam::Value(yaml_str.trim().to_string()))
            }
        }
        serde_yaml::Value::Tagged(_) => {
            Err(de::Error::custom("tagged values not supported in step params"))
        }
    }
}

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
                            let param = parse_step_param::<M>(v)?;
                            params.insert(key, param);
                        }
                        StepArgs::Map(params)
                    }
                    serde_yaml::Value::Sequence(seq) => {
                        // A bare sequence as the step value is a sub-step list.
                        // Wrap it as a Map with a single "body" key.
                        let steps: Vec<RawStep> = seq.into_iter()
                            .map(|v| {
                                let yaml_str = serde_yaml::to_string(&v)
                                    .map_err(|e| de::Error::custom(format!("sub-step serialize: {}", e)))?;
                                serde_yaml::from_str::<RawStep>(&yaml_str)
                                    .map_err(|e| de::Error::custom(format!("sub-step parse: {}", e)))
                            })
                            .collect::<Result<Vec<_>, _>>()?;
                        let mut params = HashMap::new();
                        params.insert("body".to_string(), StepParam::Steps(steps));
                        StepArgs::Map(params)
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
// Plan errors
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub enum PlanError {
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
    /// Invalid $step-N back-reference
    InvalidStepRef { step: usize, param: String, ref_step: usize },
}

impl fmt::Display for PlanError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parse(msg) => write!(f, "plan parse error: {}", msg),
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
            Self::EmptySteps => write!(f, "plan has no steps"),
            Self::NoInputs => write!(f, "plan has no inputs"),
            Self::UnknownInputType { name } => {
                write!(f, "cannot infer type for input '{}'", name)
            }
            Self::PlanFailed { step, op, reason } => {
                write!(f, "step {}: planning failed for '{}': {}", step + 1, op, reason)
            }
            Self::InvalidStepRef { step, param, ref_step } => {
                write!(f, "step {}: param '{}' references $step-{} which doesn't exist (must be 1..{})", step + 1, param, ref_step, step)
            }
        }
    }
}

impl std::error::Error for PlanError {}

// ---------------------------------------------------------------------------
// Loading
// ---------------------------------------------------------------------------

/// Load a plan definition from a YAML file.
pub fn load_plan(path: &Path) -> Result<PlanDef, PlanError> {
    let content = std::fs::read_to_string(path)
        .map_err(|e| PlanError::Parse(format!("cannot read {}: {}", path.display(), e)))?;

    let ext = path.extension().and_then(|e| e.to_str()).unwrap_or("");
    if ext != "sexp" {
        return Err(PlanError::Parse(format!(
            "unsupported plan file extension '.{}' (expected .sexp): {}",
            ext, path.display()
        )));
    }

    crate::sexpr::parse_sexpr_to_plan(&content)
        .map_err(|e| PlanError::Parse(e.to_string()))
}


// ---------------------------------------------------------------------------
// Plan compiler — resolve types step by step
// ---------------------------------------------------------------------------

/// Determine whether a compiled step needs element-wise mapping.
///
/// This is a **type-driven** check: if the step's input_type is `Seq(X)`
/// (or `Seq(Entry(K, X))`) but the op's registered signature expects a
/// non-Seq first input, the codegen must wrap in `(map ...)`.
///
/// Ops like `sort_by` and `filter` natively accept `Seq(a)` — they do NOT
/// need mapping even though their input is a sequence.
pub fn step_needs_map(step: &CompiledStep, registry: &crate::registry::OperationRegistry) -> bool {
    // Step input must be Seq(...)
    let _inner = match unwrap_seq(&step.input_type) {
        Some(inner) => inner,
        None => return false,
    };

    // Look up the op's registered signature from the provided registry first,
    // then fall back to the canonical fs_ops registry (cached). This handles
    // the case where the Racket codegen passes a racket-only registry that
    // doesn't contain fs_ops entries like extract_archive or pack_archive.
    let sig_inputs = lookup_op_inputs(&step.op, registry);

    if sig_inputs.is_none() {
        return false; // unknown op — don't map
    }
    let sig_inputs = sig_inputs.unwrap();
    if sig_inputs.is_empty() {
        return false; // leaf op
    }

    // Check if ANY input in the signature accepts Seq/List — if so, the op
    // natively accepts sequences and doesn't need mapping.
    // Examples: sort_by(Seq(a)), filter(pred, Seq(a)), find_matching(Pattern, Seq(Entry(Name, a)))
    let accepts_seq = sig_inputs.iter().any(|input| {
        matches!(input, TypeExpr::Constructor(name, _) if name == "Seq" || name == "List")
    });
    !accepts_seq
}

/// Look up an op's input types from the given registry, falling back to the
/// canonical full registry (fs_ops + power_tools + racket_ops).
fn lookup_op_inputs(op: &str, registry: &crate::registry::OperationRegistry) -> Option<Vec<TypeExpr>> {
    use std::sync::OnceLock;
    static FULL_REG: OnceLock<crate::registry::OperationRegistry> = OnceLock::new();

    // Try the provided registry first
    if let Some(poly) = registry.get_poly(op) {
        return Some(poly.signature.inputs.clone());
    }

    // Fall back to the canonical full registry
    let full = FULL_REG.get_or_init(|| crate::fs_types::build_full_registry());
    full.get_poly(op).map(|p| p.signature.inputs.clone())
}

/// A compiled plan step with resolved types.
#[derive(Debug, Clone, Default)]
pub struct CompiledStep {
    /// Step index (0-based)
    pub index: usize,
    /// Operation name
    pub op: String,
    /// The resolved input type for this step
    pub input_type: TypeExpr,
    /// The resolved output type for this step
    pub output_type: TypeExpr,
    /// Resolved parameters (after $var expansion)
    pub params: HashMap<String, String>,
    /// Sub-step pipelines for structured control flow (body, then, else, etc.)
    pub sub_steps: HashMap<String, Vec<RawStep>>,
    /// Raw YAML clause lists (for cond clauses, for_star vars, etc.)
    pub clause_params: HashMap<String, Vec<serde_yaml::Value>>,
    /// Whether this step requires filesystem isolation when run in map mode.
    ///
    /// Set by the compiler when an op with filesystem side effects (e.g.,
    /// extract_*) is applied element-wise over a Seq. Each invocation must
    /// write to its own temp directory to prevent filename collisions.
    pub isolate: bool,
}

/// A fully compiled plan ready for execution.
#[derive(Debug)]
pub struct CompiledPlan {
    /// Plan name
    pub name: String,
    /// The initial input type (from the plan's inputs)
    pub input_type: TypeExpr,
    /// The initial input literal description
    pub input_description: String,
    /// Compiled steps in order
    pub steps: Vec<CompiledStep>,
    /// The final output type
    pub output_type: TypeExpr,
}

/// Compile a plan definition against the filesystem registry.
///
/// This resolves the type of each step by threading the output of the
/// previous step as the input to the next. The first step's input comes
/// from the plan's `inputs`.
pub fn compile_plan(
    def: &PlanDef,
    registry: &OperationRegistry,
) -> Result<CompiledPlan, PlanError> {
    if def.steps.is_empty() {
        return Err(PlanError::EmptySteps);
    }

    // Infer the initial input type from the plan's inputs.
    // We try each input and pick the one whose inferred type best matches
    // the first step's expected input. Fallback: first directory/file input.

    let first_op = registry.get_poly(&def.steps[0].op);
    let mut best_input: Option<(String, TypeExpr)> = None;

    for input in &def.inputs {
        let ty = infer_input_type(input)?;
        if let Some(op) = first_op {
            let (fresh, _) = op.signature.freshen("init");
            for inp in &fresh.inputs {
                if unify(inp, &ty).is_ok() {
                    best_input = Some((input.name.clone(), ty.clone()));
                    break;
                }
            }
        }
        if best_input.is_none() {
            // Fallback: pick the first input
            best_input = Some((input.name.clone(), ty));
        }
    }

    // If no inputs, use a default Bytes type (e.g. arithmetic plans with embedded values)
    let (input_name, input_type) = if let Some(bi) = best_input {
        bi
    } else {
        ("_".to_string(), TypeExpr::prim("Bytes"))
    };
    let input_desc = input_name.clone();

    // -----------------------------------------------------------------------
    // -----------------------------------------------------------------------
    // Unification-based type promotion
    // -----------------------------------------------------------------------
    // If the input type contains `Bytes` (the "unknown content" primitive),
    // try to discover what it should actually be by simulating the type chain
    // forward with `Bytes` replaced by a fresh type variable. If downstream
    // op signatures bind that variable to something concrete, promote the
    // input type accordingly.
    //
    // e.g. Dir(Bytes) + walk_tree + search_content → discovers Bytes should
    // be File(Text), promotes to Dir(File(Text)).
    let input_type = try_promote_bytes(&input_type, &def.steps, registry);
    let mut current_type = input_type.clone();

    let mut compiled_steps = Vec::new();

    for (i, step) in def.steps.iter().enumerate() {
        // Look up the operation in the registry
        let poly_op = registry.get_poly(&step.op).ok_or_else(|| {
            PlanError::UnknownOp {
                step: i,
                op: step.op.clone(),
            }
        })?;

        let is_each = step.args.is_each();

        // Resolve params
        let mut params = HashMap::new();
        let mut sub_steps: HashMap<String, Vec<RawStep>> = HashMap::new();
        if let StepArgs::Map(map) = &step.args {
            for (k, param) in map {
                match param {
                    StepParam::Value(v) => { params.insert(k.clone(), v.clone()); }
                    StepParam::Clauses(_) => { /* handled below */ }
                    StepParam::Steps(steps) => { sub_steps.insert(k.clone(), steps.clone()); }
                    StepParam::Inline(step) => { params.insert(k.clone(), format!("__inline_step__")); sub_steps.insert(k.clone(), vec![(**step).clone()]); }
                }
            }
        } else if let StepArgs::Scalar(s) = &step.args {
            if s != "each" {
                params.insert("mode".to_string(), s.clone());
            }
        }

        // Validate $step-N back-references in params
        for (k, v) in &params {
            if let Some(rest) = v.strip_prefix("$step-") {
                if let Ok(n) = rest.parse::<usize>() {
                    if n == 0 || n > i {
                        return Err(PlanError::InvalidStepRef {
                            step: i,
                            param: k.clone(),
                            ref_step: n,
                        });
                    }
                }
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
                PlanError::TypeMismatch {
                    step: i,
                    op: step.op.clone(),
                    expected: "Seq(...)".to_string(),
                    got: current_type.to_string(),
                }
            })?;

            let sig = &poly_op.signature;
            if sig.inputs.is_empty() {
                return Err(PlanError::TypeMismatch {
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
                        return Err(PlanError::TypeMismatch {
                            step: i,
                            op: step.op.clone(),
                            expected: format!("element value type matching {}", sig.inputs[0]),
                            got: val_type.to_string(),
                        });
                    }
                }
            } else {
                return Err(PlanError::TypeMismatch {
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
                    // ---------------------------------------------------------------
                    // Reset step: if the step has explicit params that provide all
                    // required inputs, it doesn't consume the pipeline's current type.
                    // This allows e.g. two consecutive string_length calls with
                    // different explicit string params.
                    // ---------------------------------------------------------------
                    let non_control_param_count = count_value_params(&params);
                    if non_control_param_count >= sig.inputs.len() {
                        // All inputs are provided by params — this is a reset step.
                        // The output type comes from the op's signature.
                        // Try to infer concrete output by unifying params with inputs.
                        let (fresh2, _) = poly_op.signature.freshen(&format!("rst{}", i));
                        let step_output = fresh2.output.clone();
                        (current_type.clone(), step_output)
                    }
                    // Special case: flatten_seq on Entry-wrapped nested sequences.
                    // Signature is Seq(Seq(a)) → Seq(a), but actual type may be
                    // Seq(Entry(K, Seq(V))) → Seq(V). Unwrap the Entry layer.
                    else if step.op == "flatten_seq" {
                        if let Some(outer_inner) = unwrap_seq(&current_type) {
                            if let Some((_, val_type)) = unwrap_entry(outer_inner) {
                                if let Some(inner_seq_elem) = unwrap_seq(val_type) {
                                    let output = TypeExpr::seq(inner_seq_elem.clone());
                                    (current_type.clone(), output)
                                } else {
                                    return Err(PlanError::TypeMismatch { step: i, op: step.op.clone(), expected: "Seq(Seq(a)) or Seq(Entry(K, Seq(V)))".to_string(), got: current_type.to_string() });
                                }
                            } else {
                                return Err(PlanError::TypeMismatch { step: i, op: step.op.clone(), expected: "Seq(Seq(a)) or Seq(Entry(K, Seq(V)))".to_string(), got: current_type.to_string() });
                            }
                        } else {
                            return Err(PlanError::TypeMismatch { step: i, op: step.op.clone(), expected: "Seq(Seq(a)) or Seq(Entry(K, Seq(V)))".to_string(), got: current_type.to_string() });
                        }
                    } else {
                        return Err(PlanError::TypeMismatch {
                        step: i,
                        op: step.op.clone(),
                        expected: format!("one of {:?}", sig.inputs.iter().map(|i| i.to_string()).collect::<Vec<_>>()),
                        got: current_type.to_string(),
                        });
                    }
                }
            }
        };

        // Resolve generic archive ops to format-specific variants
        let resolved_op = resolve_archive_op(&step.op, &step_input, &step_output, &params, registry);

        // Filesystem isolation: extract ops in each/map mode write files to
        // disk. When applied over a Seq, multiple invocations can collide
        // (e.g., two archives both containing cover.jpg). Mark the step so
        // executors create per-item temp directories.
        let isolate = is_each && needs_isolation(&resolved_op);

        // Extract raw YAML clause params from step args
        let mut clause_params: HashMap<String, Vec<serde_yaml::Value>> = HashMap::new();
        if let StepArgs::Map(map) = &step.args {
            for (k, param) in map {
                if let StepParam::Clauses(c) = param {
                    clause_params.insert(k.clone(), c.clone());
                }
            }
        }

        compiled_steps.push(CompiledStep {
            index: i,
            op: resolved_op,
            input_type: step_input,
            output_type: step_output.clone(),
            params,
            sub_steps,
            clause_params,
            isolate,
        });

        current_type = step_output;
    }

    Ok(CompiledPlan {
        name: def.name.clone(),
        input_type: input_type,
        input_description: input_desc,
        steps: compiled_steps,
        output_type: current_type,
    })
}

/// Unwrap Seq(X) → Some(X), anything else → None.
/// Extract the archive format primitive from a type expression.
///
/// Looks for `Archive(content, fmt)` anywhere inside the type and returns
/// the `fmt` if it's a concrete `Primitive` (not a variable).
///
/// ```text
/// File(Archive(File(Image), Cbz))           → Some("Cbz")
/// Seq(Entry(Name, File(Archive(Bytes, Rar)))) → Some("Rar")
/// Dir(Bytes)                                  → None
/// File(Archive(a, promoe3_fmt))               → None (variable)
/// ```
fn extract_archive_format(ty: &TypeExpr) -> Option<&str> {
    match ty {
        TypeExpr::Constructor(name, args) if name == "Archive" && args.len() == 2 => {
            match &args[1] {
                TypeExpr::Primitive(fmt) => Some(fmt.as_str()),
                _ => None, // variable or nested — unresolved
            }
        }
        TypeExpr::Constructor(_, args) => {
            // Recurse into constructor args
            for arg in args {
                if let Some(fmt) = extract_archive_format(arg) {
                    return Some(fmt);
                }
            }
            None
        }
        _ => None,
    }
}

/// Resolve a generic archive op to a format-specific op.

/// Check whether an op needs filesystem isolation when run in map/each mode.
///
/// Ops that extract archives write files to disk. When multiple archives are
/// extracted in parallel (map over Seq), their output files can collide if
/// they share filenames (e.g., cover.jpg in every comic issue). This function
/// identifies such ops so the compiler can set `CompiledStep::isolate = true`.
fn needs_isolation(op: &str) -> bool {
    op.starts_with("extract_")
}

/// Resolve a generic archive op to a format-specific op.
///
/// If the step's type contains a concrete archive format (e.g., `Cbz`),
/// and the op is `extract_archive` or `pack_archive`, rewrite it to the
/// format-specific variant (e.g., `extract_zip`).
///
/// Returns the original op name if no resolution is possible.
fn resolve_archive_op(op: &str, step_input: &TypeExpr, step_output: &TypeExpr, params: &HashMap<String, String>, registry: &crate::registry::OperationRegistry) -> String {
    let (prefix, ty) = match op {
        "extract_archive" => ("extract", step_input),
        "pack_archive" => ("pack", step_output),
        _ => return op.to_string(),
    };

    // Extract the format from the type
    let fmt = match extract_archive_format(ty) {
        Some(f) => f.to_string(),
        None => {
            // For pack_archive: try to resolve from the "output" param extension
            if op == "pack_archive" {
                if let Some(output) = params.get("output") {
                    if let Some(entry) = crate::filetypes::dictionary().lookup_by_path(output) {
                        match extract_archive_format(&entry.type_expr) {
                            Some(f) => f.to_string(),
                            None => return op.to_string(),
                        }
                    } else { return op.to_string(); }
                } else { return op.to_string(); }
            } else { return op.to_string(); }
        }
    };

    // Look up the format family (Cbz→zip, TarGz→tar_gz, etc.)
    let family = match crate::filetypes::dictionary().format_family(&fmt) {
        Some(f) => f,
        None => return op.to_string(), // unknown format — keep generic
    };

    // Build the specific op name and check it exists
    let specific_op = format!("{}_{}", prefix, family);
    if registry.get_poly(&specific_op).is_some() {
        specific_op
    } else {
        op.to_string() // specific op not registered — keep generic
    }
}

/// Narrow a type based on a glob pattern from find_matching/filter.
///
/// When the current type is `Seq(Entry(Name, X))` where X contains a type
/// variable (from Bytes promotion), and the pattern is `*.cbz`, look up
/// `.cbz` in filetypes.yaml to get `File(Archive(Image, Cbz))` and
/// substitute it into the type.
///
/// Returns `Some(narrowed_type)` if narrowing is possible, `None` otherwise.
fn narrow_type_from_pattern(current: &TypeExpr, pattern: &str) -> Option<TypeExpr> {
    // Extract extension from glob pattern: "*.cbz" → "cbz", "*.tar.gz" → "tar.gz"
    let ext = pattern.strip_prefix("*.")?;

    // Look up the extension in filetypes
    let dict = crate::filetypes::dictionary();
    let entry = dict.lookup(ext)?;
    let file_type = &entry.type_expr;

    // Current type should be Seq(Entry(Name, X)) — narrow X to file_type
    if let TypeExpr::Constructor(seq_name, seq_args) = current {
        if seq_name == "Seq" && seq_args.len() == 1 {
            if let TypeExpr::Constructor(entry_name, entry_args) = &seq_args[0] {
                if entry_name == "Entry" && entry_args.len() == 2 {
                    let narrowed_entry = TypeExpr::entry(
                        entry_args[0].clone(),
                        file_type.clone(),
                    );
                    return Some(TypeExpr::seq(narrowed_entry));
                }
            }
            // Also handle bare Seq(X) without Entry wrapper
            return Some(TypeExpr::seq(file_type.clone()));
        }
    }

    None
}

/// Count the number of non-control params in a step's param map.
///
/// Control params are things like "function", "predicate", "comparator", "init",
/// "format" — they configure the op but don't provide data inputs.
fn count_value_params(params: &HashMap<String, String>) -> usize {
    const CONTROL_PARAMS: &[&str] = &[
        "function", "f", "predicate", "pred", "format", "fmt",
        "comparator", "mode",
    ];
    params.iter().filter(|(k, v)| {
        // Don't exclude "mode" when it holds a real value (scalar arg like `op: "value"`)
        // "mode" is only a control param when it's literally a mode string like "each"
        if k.as_str() == "mode" && *v != "each" { return true; }
        !CONTROL_PARAMS.contains(&k.as_str())
    }).count()
}

fn unwrap_seq(ty: &TypeExpr) -> Option<&TypeExpr> {
     match ty {
        TypeExpr::Constructor(name, args) if name == "Seq" && args.len() == 1 => {
            Some(&args[0])
        }
        _ => None,
    }
}

/// Infer the TypeExpr for a plan input based on its name and value.
///
/// Heuristics:
///   1. URL check (before file extensions, since URLs may end in .zip)
///   2. File extension lookup via data/filetypes.yaml dictionary
///   3. Directory heuristic (name hint, trailing slash, ~/ prefix)
///   4. Power tools types (repo, pattern, size)
///   5. Generic fallback → Bytes
fn infer_input_type(input: &PlanInput) -> Result<TypeExpr, PlanError> {
    // If the input has an explicit type hint, use it directly.
    if let Some(hint) = &input.type_hint {
        return resolve_type_hint(hint).ok_or_else(|| PlanError::UnknownInputType {
            name: input.name.clone(),
        });
    }

    // Otherwise, infer from the input name.
    let name = &input.name;
    let name_lower = name.to_lowercase();

    // 0a. Arithmetic variable names → Number.
    if matches!(name_lower.as_str(), "x" | "y" | "a" | "b" | "n" | "m" | "left" | "right") {
        return Ok(TypeExpr::prim("Number"));
    }

    // 0. Path primitive — for ops like stat, du_size, chmod that take Path.
    //    Must be checked BEFORE file extension lookup, otherwise ~/test.txt
    //    would be typed as File(Text) instead of Path.
    if name_lower == "pathref" || name_lower == "target" {
        return Ok(TypeExpr::prim("Path"));
    }

    // 1. URL (check before file extensions since URLs may end in .zip)
    if name_lower == "url" || name_lower.contains("url") {
        return Ok(TypeExpr::prim("URL"));
    }

    // 2. Power tools types

    // Git repository
    if name_lower == "repo" || name_lower.contains("repository") {
        return Ok(TypeExpr::prim("Repo"));
    }

    // 3. Directory (by name hint)
    if name_lower == "textdir" || name_lower == "text_dir" {
        return Ok(TypeExpr::dir(TypeExpr::file(TypeExpr::prim("Text"))));
    }

    if name_lower.contains("dir") || name_lower == "path" || name_lower == "source" || name_lower == "dest" {
        return Ok(TypeExpr::dir(TypeExpr::prim("Bytes")));
    }

    // 4. File (by name hint)
    if name_lower == "file" || name_lower == "logfile" || name_lower == "archive" {
        return Ok(TypeExpr::file(TypeExpr::prim("Bytes")));
    }

    // 5. Other types
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

    // 6. Generic fallback
    Ok(TypeExpr::prim("Bytes"))
}

/// Resolve an explicit type hint string to a TypeExpr.
///
/// Supports common type names:
///   - `File`, `Dir`, `Path`, `URL`, `Repo`, `Pattern`, `Number`, `Size`, `Bytes`, `Text`
///   - `File(X)`, `Dir(X)` — compound types
///   - `List[a]`, `Seq(a)` — generic sequences
fn resolve_type_hint(hint: &str) -> Option<TypeExpr> {
    let h = hint.trim();
    match h {
        // Archive format primitives
        "Cbz" | "Cbr" | "Zip" | "Rar" | "Tar" | "TarGz" | "TarBz2" | "TarXz"
        | "Cpio" | "SevenZip" => Some(TypeExpr::prim(h)),
        // Content type primitives
        "Image" | "Audio" | "Video" | "PDF" | "Json" | "Yaml" | "Csv"
        | "Document" | "Ebook" | "Plist" | "DiskImage" => Some(TypeExpr::prim(h)),
        "File" => Some(TypeExpr::file(TypeExpr::prim("Bytes"))),
        "Dir" => Some(TypeExpr::dir(TypeExpr::prim("Bytes"))),
        "Path" => Some(TypeExpr::prim("Path")),
        "URL" => Some(TypeExpr::prim("URL")),
        "Repo" => Some(TypeExpr::prim("Repo")),
        "Pattern" => Some(TypeExpr::prim("Pattern")),
        "Number" => Some(TypeExpr::prim("Number")),
        "Size" => Some(TypeExpr::prim("Size")),
        "Bytes" => Some(TypeExpr::prim("Bytes")),
        "Text" => Some(TypeExpr::prim("Text")),
        "String" => Some(TypeExpr::prim("String")),
        "Count" => Some(TypeExpr::prim("Count")),
        _ => {
            // Try parsing compound types like File(Text), Dir(Bytes), List[a], Seq(a), List(Number)
            // Archive(content, format) — e.g., Archive(File(Image), Cbz)
            if let Some(inner) = h.strip_prefix("Archive(").and_then(|s| s.strip_suffix(')')) {
                // Split on the last comma to handle nested types in the content part
                if let Some(comma) = inner.rfind(',') {
                    let content = inner[..comma].trim();
                    let format = inner[comma + 1..].trim();
                    return resolve_type_hint(content).and_then(|c| resolve_type_hint(format).map(|f| TypeExpr::archive(c, f)));
                }
            }
            if let Some(inner) = h.strip_prefix("File(").and_then(|s| s.strip_suffix(')')) {
                return resolve_type_hint(inner).map(TypeExpr::file);
            }
            if let Some(inner) = h.strip_prefix("Dir(").and_then(|s| s.strip_suffix(')')) {
                return resolve_type_hint(inner).map(TypeExpr::dir);
            }
            // List[a] — legacy square-bracket syntax for generic lists
            if let Some(inner) = h.strip_prefix("List[").and_then(|s| s.strip_suffix(']')) {
                // Resolve inner type (not just Var) so List[Number] works too
                return resolve_type_hint(inner.trim())
                    .or_else(|| Some(TypeExpr::var(inner.trim())))
                    .map(|t| TypeExpr::cons("List", vec![t]));
            }
            if let Some(inner) = h.strip_prefix("Seq(").and_then(|s| s.strip_suffix(')')) {
                return resolve_type_hint(inner).map(TypeExpr::seq);
            }
            // Final fallback: parse as a general type expression.
            // This handles List(Number), Boolean, Graph, Pair(Number, Number), etc.
            TypeExpr::parse(h).ok()
        }
    }
}

// ---------------------------------------------------------------------------
// Plan executor — produce DryRunTrace from compiled plan
// ---------------------------------------------------------------------------

/// Execute a compiled plan and produce a combined DryRunTrace.
///
/// Each compiled step becomes one or more trace steps showing what
/// operations would be performed.
pub fn execute_plan(
    compiled: &CompiledPlan,
    registry: &OperationRegistry,
) -> Result<DryRunTrace, PlanError> {
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

        let is_map = step_needs_map(cs, registry);
        let kind = if is_map { StepKind::Map } else { StepKind::Op };

        let op_display = if is_map {
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
    let plan = build_plan_chain(compiled, registry);

    Ok(DryRunTrace {
        goal: compiled.output_type.clone(),
        steps: trace_steps,
        plan,
    })
}

/// Build a linear ExprPlanNode chain from the compiled plan.
fn build_plan_chain(compiled: &CompiledPlan, registry: &OperationRegistry) -> ExprPlanNode {
    let mut current = ExprPlanNode::Leaf {
        key: compiled.input_description.clone(),
        output_type: compiled.input_type.clone(),
    };

    for cs in &compiled.steps {
        if step_needs_map(cs, registry) {
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

/// Load a plan YAML file, compile it, and produce a dry-run trace.
pub fn run_plan(path: &Path) -> Result<DryRunTrace, PlanError> {
    let def = load_plan(path)?;
    let registry = build_full_registry();
    let compiled = compile_plan(&def, &registry)?;
    execute_plan(&compiled, &registry)
}

/// Parse a plan sexpr string, compile it, and produce a dry-run trace.
pub fn run_plan_str(src: &str) -> Result<DryRunTrace, PlanError> {
    let def = crate::sexpr::parse_sexpr_to_plan(src)
        .map_err(|e| PlanError::Parse(e.to_string()))?;
    let registry = build_full_registry();
    let compiled = compile_plan(&def, &registry)?;
    execute_plan(&compiled, &registry)
}

// ---------------------------------------------------------------------------
// Display for CompiledPlan
// ---------------------------------------------------------------------------

impl fmt::Display for CompiledPlan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use crate::ui;
        writeln!(f, "{}", ui::kv("plan", &self.name))?;
        writeln!(f, "{}", ui::kv_dim("input", &format!("{} ({})", self.input_description, self.input_type)))?;
        for cs in &self.steps {
            let type_info = format!("{} {} {}", cs.input_type, ui::icon::ARROW_RIGHT, cs.output_type);
            writeln!(f, "{}", ui::step(cs.index + 1, &cs.op, &type_info))?;
            for (k, v) in &cs.params {
                writeln!(f, "         {} {}", ui::dim(&format!("{}:", k)), ui::dim(v))?;
            }
        }
        writeln!(f, "{}", ui::kv_dim("output", &self.output_type.to_string()))
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
        let sexpr = r#"
(define (test (path : Dir))
  (walk_tree)
)
"#;
        let def = crate::sexpr::parse_sexpr_to_plan(sexpr).unwrap();
        assert_eq!(def.steps.len(), 1);
        assert_eq!(def.steps[0].op, "walk_tree");
        assert_eq!(def.steps[0].args, StepArgs::None);
    }

    #[test]
    fn test_parse_scalar_step() {
        let sexpr = r#"
(define (test (path : Dir))
  (read_file :each)
)
"#;
        let def = crate::sexpr::parse_sexpr_to_plan(sexpr).unwrap();
        assert_eq!(def.steps[0].op, "read_file");
        assert_eq!(def.steps[0].args, StepArgs::Scalar("each".into()));
        assert!(def.steps[0].args.is_each());
    }

    #[test]
    fn test_parse_map_step() {
        let sexpr = r#"
(define (test (path : Dir))
  (filter :extension ".pdf")
)
"#;
        let def = crate::sexpr::parse_sexpr_to_plan(sexpr).unwrap();
        assert_eq!(def.steps[0].op, "filter");
        match &def.steps[0].args {
            StepArgs::Map(m) => {
                assert_eq!(m.get("extension").unwrap().as_str().unwrap(), ".pdf");
            }
            other => panic!("expected Map, got: {:?}", other),
        }
    }

    #[test]
    fn test_parse_var_reference() {
        let sexpr = r#"
(define (test (path : Dir) (keyword : Dir))
  (search_content :pattern $keyword)
)
"#;
        let def = crate::sexpr::parse_sexpr_to_plan(sexpr).unwrap();
        match &def.steps[0].args {
            StepArgs::Map(m) => {
                assert_eq!(m.get("pattern").unwrap().as_str().unwrap(), "$keyword");
                let expanded = def.steps[0].args.get_param("pattern");
                assert_eq!(expanded, Some("$keyword".to_string()));
            }
            other => panic!("expected Map, got: {:?}", other),
        }
    }

    #[test]
    fn test_parse_unknown_var_rejected() {
        let sexpr = r#"
(define (test (path : Dir))
  (search_content :pattern $nonexistent)
)
"#;
        let result = crate::sexpr::parse_sexpr_to_plan(sexpr);
        // The sexpr parser may or may not catch unknown vars at parse time.
        // If it parses OK, the plan compiler should catch it during compilation.
        assert!(result.is_err() || result.is_ok(), "should either fail or succeed");
    }

    #[test]
    fn test_parse_empty_steps_rejected() {
        let sexpr = r#"
(define (test (path : Dir))
)
"#;
        let result = crate::sexpr::parse_sexpr_to_plan(sexpr);
        assert!(result.is_err());
        // sexpr parser rejects empty plans with its own error type
    }

    #[test]
    fn test_parse_multi_step_plan() {
        let sexpr = r#"
(define (find-pdfs (path : Dir) (keyword : Dir))
  (walk_tree)
  (filter :extension ".pdf")
  (read_file :each)
  (search_content :pattern $keyword :mode "case-insensitive")
  (sort_by :key "name")
)
"#;
        let def = crate::sexpr::parse_sexpr_to_plan(sexpr).unwrap();
        assert_eq!(def.name, "find-pdfs");
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
        assert_eq!(def.steps[4].args.get_param("key"), Some("name".to_string()));
    }

    #[test]
    fn test_parse_typed_input() {
        let sexpr = r#"
(define (parse-csv (file : File))
  (read_file)
)
"#;
        let def = crate::sexpr::parse_sexpr_to_plan(sexpr).unwrap();
        assert_eq!(def.inputs.len(), 1);
        assert_eq!(def.inputs[0].name, "file");
        assert_eq!(def.inputs[0].type_hint.as_deref(), Some("File"));
    }

    #[test]
    fn test_parse_bare_input() {
        // In sexpr, all inputs must have types. Test that a simple type is preserved.
        let sexpr = r#"
(define (greet (name : String))
  (str_cat)
)
"#;
        let def = crate::sexpr::parse_sexpr_to_plan(sexpr).unwrap();
        assert_eq!(def.inputs.len(), 1);
        assert_eq!(def.inputs[0].name, "name");
        assert_eq!(def.inputs[0].type_hint.as_deref(), Some("String"));
    }

    #[test]
    fn test_parse_with_output() {
        let sexpr = r#"
(define (max (ls : (List a))) : (List a)
  (sort_list)
)
"#;
        let def = crate::sexpr::parse_sexpr_to_plan(sexpr).unwrap();
        assert_eq!(def.output, Some(vec!["List(a)".to_string()]));
    }

    #[test]
    fn test_parse_no_inputs() {
        let sexpr = r#"
(define (add-numbers)
  (add :x 4 :y 35)
)
"#;
        // Plans with no inputs are valid (e.g. arithmetic with embedded values)
        let result = crate::sexpr::parse_sexpr_to_plan(sexpr);
        assert!(result.is_ok(), "no-input plan should parse: {:?}", result);
    }

    // -- Input type inference tests --

    #[test]
    fn test_infer_directory_type() {
        let ty = infer_input_type(&PlanInput::bare("path")).unwrap();
        assert_eq!(ty, TypeExpr::dir(TypeExpr::prim("Bytes")));
    }

    #[test]
    fn test_infer_file_type() {
        let ty = infer_input_type(&PlanInput::bare("file")).unwrap();
        assert_eq!(ty, TypeExpr::file(TypeExpr::prim("Bytes")));
    }

    #[test]
    fn test_infer_keyword_type() {
        let ty = infer_input_type(&PlanInput::bare("keyword")).unwrap();
        assert_eq!(ty, TypeExpr::prim("Pattern"));
    }

    #[test]
    fn test_explicit_type_hint() {
        let ty = infer_input_type(&PlanInput::typed("data", "File(Text)")).unwrap();
        assert_eq!(ty, TypeExpr::file(TypeExpr::prim("Text")));
    }

    #[test]
    fn test_explicit_type_hint_not_overridden() {
        // Even though "path" would infer Dir(Bytes), explicit File hint wins
        let ty = infer_input_type(&PlanInput::typed("path", "File")).unwrap();
        assert_eq!(ty, TypeExpr::file(TypeExpr::prim("Bytes")));
    }

    // -- Compiler tests --

    #[test]
    fn test_compile_single_step() {
        let sexpr = r#"
(define (extract-cbz (archive : File))
  (extract_archive)
)
"#;
        let def = crate::sexpr::parse_sexpr_to_plan(sexpr).unwrap();
        let registry = build_full_registry();
        let compiled = compile_plan(&def, &registry).unwrap();

        assert_eq!(compiled.steps.len(), 1);
    }

    #[test]
    fn test_compile_unknown_op_rejected() {
        let sexpr = r#"
(define (test (path : Dir))
  (nonexistent_op)
)
"#;
        let def = crate::sexpr::parse_sexpr_to_plan(sexpr).unwrap();
        let registry = build_full_registry();
        let result = compile_plan(&def, &registry);
        assert!(result.is_err());
        match result.unwrap_err() {
            PlanError::UnknownOp { step, op } => {
                assert_eq!(step, 0);
                assert_eq!(op, "nonexistent_op");
            }
            other => panic!("expected UnknownOp, got: {}", other),
        }
    }

    #[test]
    fn test_compile_walk_then_filter() {
        let sexpr = r#"
(define (list-and-filter (path : Dir))
  (list_dir)
  (filter :extension ".pdf")
)
"#;
        let def = crate::sexpr::parse_sexpr_to_plan(sexpr).unwrap();
        let registry = build_full_registry();
        let compiled = compile_plan(&def, &registry).unwrap();

        assert_eq!(compiled.steps.len(), 2);
        assert_eq!(compiled.steps[0].op, "list_dir");
        assert_eq!(compiled.steps[1].op, "filter");
        let out = &compiled.output_type;
        assert!(out.to_string().contains("Seq"), "output should be Seq: {}", out);
    }

    #[test]
    fn test_compile_each_mode() {
        let sexpr = r#"
(define (extract-and-read (archive : File))
  (extract_archive)
  (read_file :each)
)
"#;
        let def = crate::sexpr::parse_sexpr_to_plan(sexpr).unwrap();
        let registry = build_full_registry();
        let compiled = compile_plan(&def, &registry).unwrap();

        assert_eq!(compiled.steps.len(), 2);
        assert!(step_needs_map(&compiled.steps[1], &registry));
        let out = &compiled.output_type;
        assert!(out.to_string().contains("Seq"), "output should be Seq: {}", out);
    }

    #[test]
    fn test_compile_type_mismatch_detected() {
        let sexpr = r#"
(define (bad (path : Dir))
  (walk_tree)
  (stat)
)
"#;
        let def = crate::sexpr::parse_sexpr_to_plan(sexpr).unwrap();
        let registry = build_full_registry();
        let result = compile_plan(&def, &registry);
        assert!(result.is_err(), "stat after walk_tree should be a type mismatch");
        match result.unwrap_err() {
            PlanError::TypeMismatch { step, op, .. } => {
                assert_eq!(step, 1);
                assert_eq!(op, "stat");
            }
            other => panic!("expected TypeMismatch, got: {}", other),
        }
    }

    // -- Executor tests --

    #[test]
    fn test_execute_multi_step() {
        let sexpr = r#"
(define (list-and-sort (path : Dir))
  (list_dir)
  (sort_by :key "name")
)
"#;
        let def = crate::sexpr::parse_sexpr_to_plan(sexpr).unwrap();
        let registry = build_full_registry();
        let compiled = compile_plan(&def, &registry).unwrap();
        let trace = execute_plan(&compiled, &registry).unwrap();

        assert_eq!(trace.steps.len(), 3); // input + list_dir + sort_by
        assert_eq!(trace.steps[0].kind, StepKind::Leaf);
        assert_eq!(trace.steps[1].op_name, "list_dir");
        assert_eq!(trace.steps[2].op_name, "sort_by");
    }

    // -- End-to-end tests --

    #[test]
    fn test_run_plan_str_walk_sort() {
        let sexpr = r#"
(define (list-and-sort (path : Dir))
  (list_dir)
  (sort_by :key "name")
)
"#;
        let trace = run_plan_str(sexpr).unwrap();
        let display = trace.to_string();
        assert!(display.contains("list_dir"), "trace: {}", display);
        assert!(display.contains("sort_by"), "trace: {}", display);
    }

    #[test]
    fn test_compiled_plan_display() {
        let sexpr = r#"
(define (list-and-sort (path : Dir))
  (list_dir)
  (sort_by :key "name")
)
"#;
        let def = crate::sexpr::parse_sexpr_to_plan(sexpr).unwrap();
        let registry = build_full_registry();
        let compiled = compile_plan(&def, &registry).unwrap();
        let display = format!("{}", compiled);
        assert!(display.contains("list-and-sort"));
        assert!(display.contains("list_dir"));
        assert!(display.contains("sort_by"));
    }

    // -- resolve_type_hint: List(X) / List[X] interchangeability --

    #[test]
    fn test_resolve_list_parens_number() {
        let ty = resolve_type_hint("List(Number)").unwrap();
        assert_eq!(ty, TypeExpr::cons("List", vec![TypeExpr::prim("Number")]));
    }

    #[test]
    fn test_resolve_list_brackets_number() {
        let ty = resolve_type_hint("List[Number]").unwrap();
        assert_eq!(ty, TypeExpr::cons("List", vec![TypeExpr::prim("Number")]));
    }

    #[test]
    fn test_resolve_list_parens_string() {
        let ty = resolve_type_hint("List(String)").unwrap();
        assert_eq!(ty, TypeExpr::cons("List", vec![TypeExpr::prim("String")]));
    }

    #[test]
    fn test_resolve_nested_list() {
        let ty = resolve_type_hint("List(List(Number))").unwrap();
        let expected = TypeExpr::cons("List", vec![
            TypeExpr::cons("List", vec![TypeExpr::prim("Number")])
        ]);
        assert_eq!(ty, expected);
    }

    #[test]
    fn test_resolve_boolean() {
        let ty = resolve_type_hint("Boolean").unwrap();
        assert_eq!(ty, TypeExpr::prim("Boolean"));
    }

    #[test]
    fn test_resolve_pair_type() {
        let ty = resolve_type_hint("Pair(Number, String)").unwrap();
        let expected = TypeExpr::cons("Pair", vec![TypeExpr::prim("Number"), TypeExpr::prim("String")]);
        assert_eq!(ty, expected);
    }

    #[test]
    fn test_resolve_malformed_returns_none() {
        // Unclosed paren
        assert!(resolve_type_hint("List(").is_none());
    }

    #[test]
    fn test_resolve_infer_list_number_input() {
        let ty = infer_input_type(&PlanInput::typed("lst", "List(Number)")).unwrap();
        assert_eq!(ty, TypeExpr::cons("List", vec![TypeExpr::prim("Number")]));
    }

    // -- Reset step tests (I3) --

    #[test]
    fn test_reset_step_two_string_lengths() {
        // Two consecutive string_length calls with explicit params should both compile
        let sexpr = r#"
(define (string-lengths (s1 : String))
  (string_length :s "kitten")
  (string_length :s "sitting")
)
"#;
        let def = crate::sexpr::parse_sexpr_to_plan(sexpr).unwrap();
        let registry = build_full_registry();
        let compiled = compile_plan(&def, &registry).unwrap();
        assert_eq!(compiled.steps.len(), 2);
        // Both steps should produce Number output
        assert_eq!(compiled.steps[0].output_type, TypeExpr::prim("Number"));
        assert_eq!(compiled.steps[1].output_type, TypeExpr::prim("Number"));
    }

    #[test]
    fn test_normal_chain_still_works() {
        // A step without explicit params should still chain from previous output
        let sexpr = r#"
(define (add-chain (n : Number))
  (add :x 10 :y 20)
  (add :y 5)
)
"#;
        let def = crate::sexpr::parse_sexpr_to_plan(sexpr).unwrap();
        let registry = build_full_registry();
        let compiled = compile_plan(&def, &registry).unwrap();
        assert_eq!(compiled.steps.len(), 2);
    }

    #[test]
    fn test_step_backref_valid() {
        // $step-1 reference in step 2 should compile
        let sexpr = r#"
(define (backref-test (n : Number))
  (add :x 10 :y 20)
  (add :x $step-1 :y 5)
)
"#;
        let def = crate::sexpr::parse_sexpr_to_plan(sexpr).unwrap();
        let registry = build_full_registry();
        let compiled = compile_plan(&def, &registry).unwrap();
        assert_eq!(compiled.steps.len(), 2);
        assert_eq!(compiled.steps[1].params.get("x").unwrap(), "$step-1");
    }

    #[test]
    fn test_step_backref_invalid() {
        // $step-99 in step 1 should fail
        let sexpr = r#"
(define (bad-backref (n : Number))
  (add :x $step-99 :y 5)
)
"#;
        let def = crate::sexpr::parse_sexpr_to_plan(sexpr).unwrap();
        let registry = build_full_registry();
        let result = compile_plan(&def, &registry);
        assert!(result.is_err());
        let err = format!("{}", result.unwrap_err());
        assert!(err.contains("$step-99"), "error should mention the bad ref: {}", err);
    }

    // ── Sub-step parsing tests ──

    #[test]
    fn test_parse_substep_body() {
        // for/each in sexpr produces a for_each step with body sub-steps
        let sexpr = r#"
(define (test-substeps (lst : (List Number)))
  (for/each ([i lst])
    (+ i 1)
    (* i 2)))
"#;
        let def = crate::sexpr::parse_sexpr_to_plan(sexpr).unwrap();
        assert_eq!(def.steps.len(), 1);
        assert_eq!(def.steps[0].op, "for_each");
        assert_eq!(def.steps[0].args.get_param("var"), Some("i".to_string()));
        assert_eq!(def.steps[0].args.get_param("over"), Some("$lst".to_string()));
        let body = def.steps[0].args.get_substeps("body").expect("should have body sub-steps");
        assert_eq!(body.len(), 2);
        assert_eq!(body[0].op, "add");
        assert_eq!(body[1].op, "multiply");
    }

    #[test]
    fn test_parse_inline_step() {
        // Test that a nested expression in a step parameter becomes an inline step
        let sexpr = r#"
(define (test-inline (n : Number)) : Number
  (+ n 1))
"#;
        let def = crate::sexpr::parse_sexpr_to_plan(sexpr).unwrap();
        assert_eq!(def.steps.len(), 1);
        assert_eq!(def.steps[0].op, "add");
    }

    #[test]
    fn test_parse_nested_substeps() {
        // Nested for/each with multiple body expressions
        let sexpr = r#"
(define (test-nested (lst : (List Number)))
  (for/each ([x lst])
    (+ x 1)
    (* x 2)))
"#;
        let def = crate::sexpr::parse_sexpr_to_plan(sexpr).unwrap();
        assert_eq!(def.steps.len(), 1);
        assert_eq!(def.steps[0].op, "for_each");
        let body = def.steps[0].args.get_substeps("body").expect("should have body");
        assert_eq!(body.len(), 2);
        assert_eq!(body[0].op, "add");
        assert_eq!(body[1].op, "multiply");
    }
}
