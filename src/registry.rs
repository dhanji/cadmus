use std::collections::HashMap;
use std::fmt;
use std::path::Path;
use crate::type_expr::{TypeExpr, Substitution, unify};
use serde::Deserialize;

// ---------------------------------------------------------------------------
// TypeId — strategy-defined type vocabulary
// ---------------------------------------------------------------------------

/// A type identifier in the reasoning engine. Strategies define their own
/// type vocabulary (e.g., "Claim", "Evidence", "AST", "Refactoring").
/// String-based so each strategy can introduce domain-specific types
/// without modifying the core engine.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeId(pub String);

impl TypeId {
    pub fn new(s: impl Into<String>) -> Self {
        Self(s.into())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for TypeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<&str> for TypeId {
    fn from(s: &str) -> Self {
        Self(s.to_string())
    }
}

impl From<String> for TypeId {
    fn from(s: String) -> Self {
        Self(s)
    }
}

// ---------------------------------------------------------------------------
// Typed literals — concrete values available as plan inputs
// ---------------------------------------------------------------------------

/// A concrete typed value available as a leaf input to the planner.
/// Strategies populate these from their domain data (e.g., fact pack
/// claims become Literal { type_id: "Claim", key: "putin_claim_legitimacy", ... }).
#[derive(Debug, Clone)]
pub struct Literal {
    /// The type of this literal
    pub type_id: TypeId,
    /// A unique key identifying this literal (for slot binding)
    pub key: String,
    /// The actual content/value
    pub value: String,
    /// Optional metadata (e.g., entity, axis)
    pub metadata: HashMap<String, String>,
}

impl Literal {
    pub fn new(type_id: impl Into<TypeId>, key: impl Into<String>, value: impl Into<String>) -> Self {
        Self {
            type_id: type_id.into(),
            key: key.into(),
            value: value.into(),
            metadata: HashMap::new(),
        }
    }

    pub fn with_meta(mut self, k: impl Into<String>, v: impl Into<String>) -> Self {
        self.metadata.insert(k.into(), v.into());
        self
    }
}

// ---------------------------------------------------------------------------
// Algebraic properties — declared per operation
// ---------------------------------------------------------------------------

/// Algebraic properties that an operation may declare. The algebra layer
/// uses these to canonicalize, prune, and deduplicate plans.
#[derive(Debug, Clone, Default)]
pub struct AlgebraicProperties {
    /// op(a, b) = op(b, a) — operand order doesn't matter
    pub commutative: bool,
    /// op(op(a, b), c) = op(a, op(b, c)) — can flatten nested applications
    pub associative: bool,
    /// An element e such that op(x, e) = x — can be dropped
    pub identity: Option<String>,
    /// An element z such that op(x, z) = z — collapses entire expression
    pub absorbing: Option<String>,
    /// op(x, x) = x — duplicate operands collapse
    pub idempotent: bool,
}

impl AlgebraicProperties {
    pub fn none() -> Self {
        Self::default()
    }

    pub fn commutative() -> Self {
        Self { commutative: true, ..Default::default() }
    }

    pub fn associative() -> Self {
        Self { associative: true, ..Default::default() }
    }

    pub fn comm_assoc() -> Self {
        Self { commutative: true, associative: true, ..Default::default() }
    }
}

// ---------------------------------------------------------------------------
// Rewrite rules — small transformations declared per operation
// ---------------------------------------------------------------------------

/// A rewrite rule that can transform a plan node. The algebra layer
/// applies these until fixpoint during canonicalization.
#[derive(Debug, Clone)]
pub struct RewriteRule {
    /// Human-readable name for debugging
    pub name: String,
    /// The operation this rule applies to
    pub op_name: String,
    /// Pattern: input type signature that must match
    pub match_inputs: Vec<TypeId>,
    /// Replacement: the operation + inputs to substitute
    pub replacement_op: String,
    /// Replacement input types (may reorder/drop/add)
    pub replacement_inputs: Vec<TypeId>,
}

// ---------------------------------------------------------------------------
// Operation signature — typed inputs → typed output
// ---------------------------------------------------------------------------

/// The type signature of an operation: what types it consumes and produces.
#[derive(Debug, Clone)]
pub struct OpSignature {
    /// Input types required (ordered)
    pub inputs: Vec<TypeId>,
    /// Output type produced
    pub output: TypeId,
}

impl OpSignature {
    pub fn new(inputs: Vec<TypeId>, output: TypeId) -> Self {
        Self { inputs, output }
    }

    /// A leaf operation that produces a type from nothing (e.g., retrieve from data source)
    pub fn leaf(output: TypeId) -> Self {
        Self { inputs: vec![], output }
    }
}

// ---------------------------------------------------------------------------
// Execution binding — how an operation actually runs
// ---------------------------------------------------------------------------

/// The execution context passed to operation bindings.
/// Contains the resolved input values and metadata for the current step.
#[derive(Debug, Clone)]
pub struct ExecContext {
    /// The resolved input values (one per input type in the signature)
    pub inputs: Vec<Literal>,
    /// Additional context from the strategy (e.g., axis name, entity)
    pub params: HashMap<String, String>,
}

/// Type alias for the execution function.
/// Takes an ExecContext and returns a result string (the produced value).
pub type ExecFn = Box<dyn Fn(&ExecContext) -> Result<String, String> + Send + Sync>;

// ---------------------------------------------------------------------------
// OpEntry — a registered operation
// ---------------------------------------------------------------------------

/// A fully registered operation: name, typed signature, algebraic properties,
/// rewrite rules, and execution binding.
pub struct OpEntry {
    /// Unique name for this operation
    pub name: String,
    /// Type signature (inputs → output)
    pub signature: OpSignature,
    /// Algebraic properties for canonicalization
    pub properties: AlgebraicProperties,
    /// Rewrite rules specific to this operation
    pub rewrite_rules: Vec<RewriteRule>,
    /// Execution binding
    pub exec: ExecFn,
}

impl fmt::Debug for OpEntry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("OpEntry")
            .field("name", &self.name)
            .field("signature", &self.signature)
            .field("properties", &self.properties)
            .field("rewrite_rules", &self.rewrite_rules)
            .field("exec", &"<fn>")
            .finish()
    }
}

// ---------------------------------------------------------------------------
// OperationRegistry — the central registry of typed operations
// ---------------------------------------------------------------------------

/// The operation registry: strategies register their domain-specific operations
/// here, and the generic planner queries it to find ops that produce required types.
pub struct OperationRegistry {
    /// All registered operations, keyed by name
    ops: HashMap<String, OpEntry>,
    /// Polymorphic operations (TypeExpr-based)
    poly_ops: Vec<PolyOpEntry>,
    /// Index: output TypeId → list of op names that produce it
    by_output: HashMap<TypeId, Vec<String>>,
}

impl fmt::Debug for OperationRegistry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("OperationRegistry")
            .field("ops", &self.ops.keys().collect::<Vec<_>>())
            .field("by_output", &self.by_output)
            .field("poly_ops", &self.poly_ops.iter().map(|p| &p.name).collect::<Vec<_>>())
            .finish()
    }
}

impl OperationRegistry {
    pub fn new() -> Self {
        Self {
            ops: HashMap::new(),
            by_output: HashMap::new(),
            poly_ops: Vec::new(),
        }
    }

    /// Register an operation with its signature, properties, and execution binding.
    pub fn register(
        &mut self,
        name: impl Into<String>,
        signature: OpSignature,
        properties: AlgebraicProperties,
        exec: ExecFn,
    ) -> &mut Self {
        let name = name.into();
        let output = signature.output.clone();
        self.ops.insert(
            name.clone(),
            OpEntry {
                name: name.clone(),
                signature,
                properties,
                rewrite_rules: Vec::new(),
                exec,
            },
        );
        self.by_output
            .entry(output)
            .or_default()
            .push(name);
        self
    }

    /// Register an operation with rewrite rules.
    pub fn register_with_rewrites(
        &mut self,
        name: impl Into<String>,
        signature: OpSignature,
        properties: AlgebraicProperties,
        rewrite_rules: Vec<RewriteRule>,
        exec: ExecFn,
    ) -> &mut Self {
        let name = name.into();
        let output = signature.output.clone();
        self.ops.insert(
            name.clone(),
            OpEntry {
                name: name.clone(),
                signature,
                properties,
                rewrite_rules,
                exec,
            },
        );
        self.by_output
            .entry(output)
            .or_default()
            .push(name);
        self
    }

    /// Look up all operations that produce the given output type.
    pub fn ops_for_output(&self, output: &TypeId) -> Vec<&OpEntry> {
        self.by_output
            .get(output)
            .map(|names| {
                names
                    .iter()
                    .filter_map(|n| self.ops.get(n))
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Get an operation by name.
    pub fn get(&self, name: &str) -> Option<&OpEntry> {
        self.ops.get(name)
    }

    /// Get all registered operation names.
    pub fn op_names(&self) -> Vec<&str> {
        self.ops.keys().map(|s| s.as_str()).collect()
    }

    /// Get all registered output types.
    pub fn output_types(&self) -> Vec<&TypeId> {
        self.by_output.keys().collect()
    }

    /// Check if any operation produces the given type.
    pub fn has_producer(&self, output: &TypeId) -> bool {
        self.by_output
            .get(output)
            .map(|v| !v.is_empty())
            .unwrap_or(false)
    }

    /// Get algebraic properties for an operation by name.
    pub fn properties(&self, name: &str) -> Option<&AlgebraicProperties> {
        self.ops.get(name).map(|e| &e.properties)
    }

    /// Collect all rewrite rules from all registered operations.
    pub fn all_rewrite_rules(&self) -> Vec<&RewriteRule> {
        self.ops
            .values()
            .flat_map(|e| e.rewrite_rules.iter())
            .collect()
    }
}

impl Default for OperationRegistry {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// PolyOpSignature — polymorphic operation signatures using TypeExpr
// ---------------------------------------------------------------------------

/// A polymorphic operation signature using compositional types.
///
/// Unlike `OpSignature` which uses flat `TypeId` strings, `PolyOpSignature`
/// uses `TypeExpr` for inputs and output, allowing type variables that get
/// bound during unification-based lookup.
///
/// Example: `extract_archive<a, fmt>: File(Archive(a, fmt)) → Seq(Entry(Name, a))`
#[derive(Debug, Clone)]
pub struct PolyOpSignature {
    /// Type parameter names (e.g., ["a", "fmt"])
    pub type_params: Vec<String>,
    /// Input types (may contain type variables)
    pub inputs: Vec<TypeExpr>,
    /// Output type (may contain type variables)
    pub output: TypeExpr,
}

impl PolyOpSignature {
    pub fn new(type_params: Vec<String>, inputs: Vec<TypeExpr>, output: TypeExpr) -> Self {
        Self { type_params, inputs, output }
    }

    /// A monomorphic poly signature (no type params).
    pub fn mono(inputs: Vec<TypeExpr>, output: TypeExpr) -> Self {
        Self { type_params: vec![], inputs, output }
    }

    /// A leaf poly signature (no inputs).
    pub fn leaf(output: TypeExpr) -> Self {
        Self { type_params: vec![], inputs: vec![], output }
    }

    /// Freshen type variables with a unique prefix to avoid capture.
    /// Returns a new signature with renamed variables and the renaming map.
    pub fn freshen(&self, prefix: &str) -> (PolyOpSignature, HashMap<String, String>) {
        let mut rename_map = HashMap::new();
        for param in &self.type_params {
            rename_map.insert(param.clone(), format!("{}_{}", prefix, param));
        }

        let freshen_expr = |expr: &TypeExpr| -> TypeExpr {
            let mut subst = Substitution::empty();
            for (old, new) in &rename_map {
                subst.bind(old.clone(), TypeExpr::Var(new.clone())).ok();
            }
            expr.apply_subst(&subst)
        };

        let new_sig = PolyOpSignature {
            type_params: self.type_params.iter().map(|p| rename_map[p].clone()).collect(),
            inputs: self.inputs.iter().map(|i| freshen_expr(i)).collect(),
            output: freshen_expr(&self.output),
        };

        (new_sig, rename_map)
    }
}

impl fmt::Display for PolyOpSignature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.type_params.is_empty() {
            write!(f, "<{}>", self.type_params.join(", "))?;
        }
        write!(f, "(")?;
        for (i, input) in self.inputs.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", input)?;
        }
        write!(f, ") → {}", self.output)
    }
}

// ---------------------------------------------------------------------------
// PolyOpEntry — a registered polymorphic operation
// ---------------------------------------------------------------------------

/// A fully registered polymorphic operation.
pub struct PolyOpEntry {
    /// Unique name for this operation
    pub name: String,
    /// Polymorphic type signature
    pub signature: PolyOpSignature,
    /// Algebraic properties for canonicalization
    pub properties: AlgebraicProperties,
    /// Human-readable description of what command/action this op represents
    pub description: String,
}

impl fmt::Debug for PolyOpEntry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("PolyOpEntry")
            .field("name", &self.name)
            .field("signature", &self.signature)
            .field("properties", &self.properties)
            .field("description", &self.description)
            .finish()
    }
}

// ---------------------------------------------------------------------------
// PolyOpMatch — result of a unification-based lookup
// ---------------------------------------------------------------------------

/// A match from `ops_for_output_expr()`: a poly op whose output unified
/// with the requested type, plus the resulting substitution.
#[derive(Debug)]
pub struct PolyOpMatch<'a> {
    /// The matched operation
    pub op: &'a PolyOpEntry,
    /// The substitution that makes the op's output equal to the requested type
    pub substitution: Substitution,
    /// The concrete input types after applying the substitution
    pub concrete_inputs: Vec<TypeExpr>,
    /// The concrete output type after applying the substitution
    pub concrete_output: TypeExpr,
}

// ---------------------------------------------------------------------------
// OperationRegistry — poly extensions
// ---------------------------------------------------------------------------

impl OperationRegistry {
    /// Register a polymorphic operation.
    pub fn register_poly(
        &mut self,
        name: impl Into<String>,
        signature: PolyOpSignature,
        properties: AlgebraicProperties,
        description: impl Into<String>,
    ) -> &mut Self {
        let name = name.into();
        self.poly_ops.push(PolyOpEntry {
            name,
            signature,
            properties,
            description: description.into(),
        });
        self
    }

    /// Look up all polymorphic operations whose output type unifies with the
    /// given type expression. Returns matches with their substitutions.
    pub fn ops_for_output_expr(&self, target: &TypeExpr) -> Vec<PolyOpMatch<'_>> {
        let mut matches = Vec::new();
        for (i, op) in self.poly_ops.iter().enumerate() {
            // Freshen type variables to avoid capture between different lookups
            let (fresh_sig, _rename) = op.signature.freshen(&format!("f{}", i));

            // Try to unify the op's output with the target
            if let Ok(subst) = unify(&fresh_sig.output, target) {
                let concrete_inputs: Vec<TypeExpr> = fresh_sig.inputs
                    .iter()
                    .map(|inp| inp.apply_subst(&subst))
                    .collect();
                let concrete_output = fresh_sig.output.apply_subst(&subst);

                matches.push(PolyOpMatch {
                    op,
                    substitution: subst,
                    concrete_inputs,
                    concrete_output,
                });
            }
        }
        matches
    }

    /// Get a polymorphic operation by name.
    pub fn get_poly(&self, name: &str) -> Option<&PolyOpEntry> {
        self.poly_ops.iter().find(|op| op.name == name)
    }

    /// Get all registered polymorphic operation names.
    pub fn poly_op_names(&self) -> Vec<&str> {
        self.poly_ops.iter().map(|op| op.name.as_str()).collect()
    }
}

// ---------------------------------------------------------------------------
// YAML Ops Pack — load domain operations from YAML files
// ---------------------------------------------------------------------------

/// Error type for ops pack loading.
#[derive(Debug)]
pub enum OpsPackError {
    /// I/O error reading the file
    Io(std::io::Error),
    /// YAML parsing error
    Yaml(serde_yaml::Error),
    /// Type signature parse error for a specific op
    TypeParse {
        op_name: String,
        field: String,
        source: String,
        message: String,
    },
}

impl fmt::Display for OpsPackError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OpsPackError::Io(e) => write!(f, "ops pack I/O error: {}", e),
            OpsPackError::Yaml(e) => write!(f, "ops pack YAML error: {}", e),
            OpsPackError::TypeParse { op_name, field, source, message } => {
                write!(f, "ops pack type error in '{}' field '{}': {} (source: \"{}\")", op_name, field, message, source)
            }
        }
    }
}

impl std::error::Error for OpsPackError {}

impl From<std::io::Error> for OpsPackError {
    fn from(e: std::io::Error) -> Self {
        OpsPackError::Io(e)
    }
}

impl From<serde_yaml::Error> for OpsPackError {
    fn from(e: serde_yaml::Error) -> Self {
        OpsPackError::Yaml(e)
    }
}

/// YAML schema for an ops pack file.
#[derive(Debug, Deserialize)]
pub struct OpsPack {
    /// Human-readable name for this ops pack
    #[serde(default)]
    pub name: String,
    /// Description of the domain
    #[serde(default)]
    pub description: String,
    /// The operation definitions
    #[serde(default)]
    pub ops: Vec<OpDef>,
}

/// YAML schema for a single operation definition.
#[derive(Debug, Deserialize)]
pub struct OpDef {
    /// Operation name (e.g., "list_dir")
    pub name: String,
    /// Type parameter names (e.g., ["a", "fmt"]). Empty for monomorphic ops.
    #[serde(default)]
    pub type_params: Vec<String>,
    /// Input type expressions as strings (e.g., ["Dir(a)"])
    #[serde(default)]
    pub inputs: Vec<String>,
    /// Output type expression as string (e.g., "Seq(Entry(Name, a))")
    pub output: String,
    /// Algebraic properties
    #[serde(default)]
    pub properties: OpDefProperties,
    /// Human-readable description / command hint for dry-run
    #[serde(default)]
    pub description: String,
}

/// YAML schema for algebraic properties.
#[derive(Debug, Default, Deserialize)]
pub struct OpDefProperties {
    #[serde(default)]
    pub commutative: bool,
    #[serde(default)]
    pub associative: bool,
    #[serde(default)]
    pub identity: Option<String>,
    #[serde(default)]
    pub absorbing: Option<String>,
    #[serde(default)]
    pub idempotent: bool,
}

impl From<OpDefProperties> for AlgebraicProperties {
    fn from(p: OpDefProperties) -> Self {
        AlgebraicProperties {
            commutative: p.commutative,
            associative: p.associative,
            identity: p.identity,
            absorbing: p.absorbing,
            idempotent: p.idempotent,
        }
    }
}

/// Load an ops pack from a YAML string, returning a populated OperationRegistry.
pub fn load_ops_pack_str(yaml: &str) -> Result<OperationRegistry, OpsPackError> {
    let pack: OpsPack = serde_yaml::from_str(yaml)?;
    let mut reg = OperationRegistry::new();

    for op_def in pack.ops {
        // Parse input type expressions
        let inputs: Vec<TypeExpr> = op_def.inputs.iter().enumerate().map(|(i, s)| {
            TypeExpr::parse(s).map_err(|e| OpsPackError::TypeParse {
                op_name: op_def.name.clone(),
                field: format!("inputs[{}]", i),
                source: s.clone(),
                message: e.to_string(),
            })
        }).collect::<Result<Vec<_>, _>>()?;

        // Parse output type expression
        let output = TypeExpr::parse(&op_def.output).map_err(|e| OpsPackError::TypeParse {
            op_name: op_def.name.clone(),
            field: "output".to_string(),
            source: op_def.output.clone(),
            message: e.to_string(),
        })?;

        let sig = PolyOpSignature::new(op_def.type_params, inputs, output);
        let props: AlgebraicProperties = op_def.properties.into();

        reg.register_poly(op_def.name, sig, props, op_def.description);
    }

    Ok(reg)
}

/// Load an ops pack from a YAML file path, returning a populated OperationRegistry.
pub fn load_ops_pack(path: impl AsRef<Path>) -> Result<OperationRegistry, OpsPackError> {
    let content = std::fs::read_to_string(path.as_ref())?;
    load_ops_pack_str(&content)
}

/// Load an ops pack from a YAML string into an existing registry.
pub fn load_ops_pack_str_into(yaml: &str, reg: &mut OperationRegistry) -> Result<(), OpsPackError> {
    let loaded = load_ops_pack_str(yaml)?;
    // Move all poly ops from loaded into reg
    for name in loaded.poly_op_names() {
        if let Some(op) = loaded.get_poly(name) {
            reg.register_poly(&op.name, op.signature.clone(), op.properties.clone(), &op.description);
        }
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn make_noop_exec() -> ExecFn {
        Box::new(|_ctx| Ok("noop".to_string()))
    }

    #[test]
    fn test_register_and_lookup_by_output() {
        let mut reg = OperationRegistry::new();
        reg.register(
            "summarize",
            OpSignature::new(
                vec![TypeId::new("Evidence")],
                TypeId::new("Claim"),
            ),
            AlgebraicProperties::none(),
            make_noop_exec(),
        );

        let ops = reg.ops_for_output(&TypeId::new("Claim"));
        assert_eq!(ops.len(), 1);
        assert_eq!(ops[0].name, "summarize");
        assert_eq!(ops[0].signature.inputs.len(), 1);
        assert_eq!(ops[0].signature.inputs[0], TypeId::new("Evidence"));
        assert_eq!(ops[0].signature.output, TypeId::new("Claim"));
    }

    #[test]
    fn test_lookup_unregistered_type_returns_empty() {
        let reg = OperationRegistry::new();
        let ops = reg.ops_for_output(&TypeId::new("NonExistent"));
        assert!(ops.is_empty());
    }

    #[test]
    fn test_leaf_op_with_empty_inputs() {
        let mut reg = OperationRegistry::new();
        reg.register(
            "retrieve",
            OpSignature::leaf(TypeId::new("Evidence")),
            AlgebraicProperties::none(),
            make_noop_exec(),
        );

        let ops = reg.ops_for_output(&TypeId::new("Evidence"));
        assert_eq!(ops.len(), 1);
        assert!(ops[0].signature.inputs.is_empty(), "leaf op should have no inputs");
    }

    #[test]
    fn test_multiple_ops_same_output_type() {
        let mut reg = OperationRegistry::new();
        reg.register(
            "summarize_v1",
            OpSignature::new(vec![TypeId::new("Evidence")], TypeId::new("Claim")),
            AlgebraicProperties::none(),
            make_noop_exec(),
        );
        reg.register(
            "summarize_v2",
            OpSignature::new(
                vec![TypeId::new("Evidence"), TypeId::new("Context")],
                TypeId::new("Claim"),
            ),
            AlgebraicProperties::none(),
            make_noop_exec(),
        );

        let ops = reg.ops_for_output(&TypeId::new("Claim"));
        assert_eq!(ops.len(), 2);
        let names: Vec<&str> = ops.iter().map(|o| o.name.as_str()).collect();
        assert!(names.contains(&"summarize_v1"));
        assert!(names.contains(&"summarize_v2"));
    }

    #[test]
    fn test_algebraic_properties_queryable() {
        let mut reg = OperationRegistry::new();
        reg.register(
            "add",
            OpSignature::new(
                vec![TypeId::new("Num"), TypeId::new("Num")],
                TypeId::new("Num"),
            ),
            AlgebraicProperties {
                commutative: true,
                associative: true,
                identity: Some("0".to_string()),
                absorbing: None,
                idempotent: false,
            },
            make_noop_exec(),
        );

        let props = reg.properties("add").unwrap();
        assert!(props.commutative);
        assert!(props.associative);
        assert_eq!(props.identity, Some("0".to_string()));
        assert!(props.absorbing.is_none());
        assert!(!props.idempotent);
    }

    #[test]
    fn test_has_producer() {
        let mut reg = OperationRegistry::new();
        reg.register(
            "op1",
            OpSignature::leaf(TypeId::new("A")),
            AlgebraicProperties::none(),
            make_noop_exec(),
        );

        assert!(reg.has_producer(&TypeId::new("A")));
        assert!(!reg.has_producer(&TypeId::new("B")));
    }

    #[test]
    fn test_get_by_name() {
        let mut reg = OperationRegistry::new();
        reg.register(
            "my_op",
            OpSignature::leaf(TypeId::new("X")),
            AlgebraicProperties::commutative(),
            make_noop_exec(),
        );

        assert!(reg.get("my_op").is_some());
        assert_eq!(reg.get("my_op").unwrap().name, "my_op");
        assert!(reg.get("nonexistent").is_none());
    }

    #[test]
    fn test_rewrite_rules_collected() {
        let mut reg = OperationRegistry::new();
        reg.register_with_rewrites(
            "add",
            OpSignature::new(
                vec![TypeId::new("Num"), TypeId::new("Num")],
                TypeId::new("Num"),
            ),
            AlgebraicProperties::comm_assoc(),
            vec![RewriteRule {
                name: "add_zero".to_string(),
                op_name: "add".to_string(),
                match_inputs: vec![TypeId::new("Num"), TypeId::new("Zero")],
                replacement_op: "identity".to_string(),
                replacement_inputs: vec![TypeId::new("Num")],
            }],
            make_noop_exec(),
        );

        let rules = reg.all_rewrite_rules();
        assert_eq!(rules.len(), 1);
        assert_eq!(rules[0].name, "add_zero");
    }

    #[test]
    fn test_op_names_and_output_types() {
        let mut reg = OperationRegistry::new();
        reg.register(
            "op_a",
            OpSignature::leaf(TypeId::new("TypeA")),
            AlgebraicProperties::none(),
            make_noop_exec(),
        );
        reg.register(
            "op_b",
            OpSignature::new(vec![TypeId::new("TypeA")], TypeId::new("TypeB")),
            AlgebraicProperties::none(),
            make_noop_exec(),
        );

        let names = reg.op_names();
        assert_eq!(names.len(), 2);

        let outputs = reg.output_types();
        assert_eq!(outputs.len(), 2);
    }

    #[test]
    fn test_literal_construction() {
        let lit = Literal::new("Claim", "putin_claim_leg", "Putin claims legitimacy through elections")
            .with_meta("entity", "putin")
            .with_meta("axis", "legitimacy");

        assert_eq!(lit.type_id, TypeId::new("Claim"));
        assert_eq!(lit.key, "putin_claim_leg");
        assert_eq!(lit.metadata.get("entity").unwrap(), "putin");
        assert_eq!(lit.metadata.get("axis").unwrap(), "legitimacy");
    }

    #[test]
    fn test_exec_binding_runs() {
        let mut reg = OperationRegistry::new();
        reg.register(
            "echo",
            OpSignature::leaf(TypeId::new("Text")),
            AlgebraicProperties::none(),
            Box::new(|ctx| {
                let msg = ctx.params.get("message").cloned().unwrap_or_default();
                Ok(format!("echo: {}", msg))
            }),
        );

        let op = reg.get("echo").unwrap();
        let ctx = ExecContext {
            inputs: vec![],
            params: {
                let mut m = HashMap::new();
                m.insert("message".to_string(), "hello".to_string());
                m
            },
        };
        let result = (op.exec)(&ctx).unwrap();
        assert_eq!(result, "echo: hello");
    }

    // --- Poly op tests ---

    #[test]
    fn test_register_poly_and_lookup() {
        use crate::type_expr::TypeExpr;

        let mut reg = OperationRegistry::new();
        // extract_archive<a, fmt>: File(Archive(a, fmt)) → Seq(Entry(Name, a))
        reg.register_poly(
            "extract_archive",
            PolyOpSignature::new(
                vec!["a".into(), "fmt".into()],
                vec![TypeExpr::file(TypeExpr::archive(TypeExpr::var("a"), TypeExpr::var("fmt")))],
                TypeExpr::seq(TypeExpr::entry(TypeExpr::prim("Name"), TypeExpr::var("a"))),
            ),
            AlgebraicProperties::none(),
            "unzip/extract archive contents",
        );

        // Lookup: Seq(Entry(Name, File(Image))) should match with {a → File(Image)}
        let target = TypeExpr::seq(TypeExpr::entry(
            TypeExpr::prim("Name"),
            TypeExpr::file(TypeExpr::prim("Image")),
        ));
        let matches = reg.ops_for_output_expr(&target);
        assert_eq!(matches.len(), 1);
        assert_eq!(matches[0].op.name, "extract_archive");

        // Check concrete input after substitution
        assert_eq!(matches[0].concrete_inputs.len(), 1);
        // The input should have 'a' bound to File(Image), fmt stays as a var
        let input = &matches[0].concrete_inputs[0];
        // Check it's File(Archive(File(Image), <something>))
        match input {
            TypeExpr::Constructor(name, args) if name == "File" => {
                match &args[0] {
                    TypeExpr::Constructor(name2, args2) if name2 == "Archive" => {
                        assert_eq!(args2[0], TypeExpr::file(TypeExpr::prim("Image")));
                        // fmt is unbound — could be any var
                    }
                    other => panic!("expected Archive constructor, got: {}", other),
                }
            }
            other => panic!("expected File constructor, got: {}", other),
        }
    }

    #[test]
    fn test_poly_lookup_no_match() {
        use crate::type_expr::TypeExpr;

        let mut reg = OperationRegistry::new();
        reg.register_poly(
            "list_dir",
            PolyOpSignature::new(
                vec!["a".into()],
                vec![TypeExpr::dir(TypeExpr::var("a"))],
                TypeExpr::seq(TypeExpr::entry(TypeExpr::prim("Name"), TypeExpr::var("a"))),
            ),
            AlgebraicProperties::none(),
            "ls — list directory contents",
        );

        // Lookup for a completely unrelated type
        let target = TypeExpr::prim("Metadata");
        let matches = reg.ops_for_output_expr(&target);
        assert!(matches.is_empty());
    }

    #[test]
    fn test_existing_typeid_apis_unchanged_with_poly() {
        use crate::type_expr::TypeExpr;

        let mut reg = OperationRegistry::new();
        // Register a monomorphic op
        reg.register(
            "summarize",
            OpSignature::new(vec![TypeId::new("Evidence")], TypeId::new("Claim")),
            AlgebraicProperties::none(),
            make_noop_exec(),
        );
        // Register a poly op
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

        // Monomorphic lookup still works
        let ops = reg.ops_for_output(&TypeId::new("Claim"));
        assert_eq!(ops.len(), 1);
        assert_eq!(ops[0].name, "summarize");

        // Poly lookup works independently
        let target = TypeExpr::seq(TypeExpr::entry(TypeExpr::prim("Name"), TypeExpr::prim("Bytes")));
        let poly_matches = reg.ops_for_output_expr(&target);
        assert_eq!(poly_matches.len(), 1);
        assert_eq!(poly_matches[0].op.name, "list_dir");
    }

    #[test]
    fn test_two_poly_ops_same_output_shape() {
        use crate::type_expr::TypeExpr;

        let mut reg = OperationRegistry::new();
        // Both produce Seq(Entry(Name, a))
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
        reg.register_poly(
            "find_matching",
            PolyOpSignature::new(
                vec!["a".into()],
                vec![TypeExpr::prim("Pattern"), TypeExpr::seq(TypeExpr::var("a"))],
                TypeExpr::seq(TypeExpr::entry(TypeExpr::prim("Name"), TypeExpr::var("a"))),
            ),
            AlgebraicProperties::none(),
            "find/grep — filter entries matching pattern",
        );

        let target = TypeExpr::seq(TypeExpr::entry(TypeExpr::prim("Name"), TypeExpr::prim("Bytes")));
        let matches = reg.ops_for_output_expr(&target);
        assert_eq!(matches.len(), 2);
        let names: Vec<&str> = matches.iter().map(|m| m.op.name.as_str()).collect();
        assert!(names.contains(&"list_dir"));
        assert!(names.contains(&"find_matching"));
    }

    // --- YAML ops pack loader tests ---

    #[test]
    fn test_load_ops_pack_basic() {
        let yaml = r#"
name: test_pack
description: A test ops pack
ops:
  - name: list_dir
    type_params: [a]
    inputs: ["Dir(a)"]
    output: "Seq(Entry(Name, a))"
    description: "ls — list directory contents"
"#;
        let reg = load_ops_pack_str(yaml).unwrap();
        let names = reg.poly_op_names();
        assert_eq!(names.len(), 1);
        assert_eq!(names[0], "list_dir");

        let op = reg.get_poly("list_dir").unwrap();
        assert_eq!(op.signature.type_params, vec!["a".to_string()]);
        assert_eq!(op.signature.inputs.len(), 1);
        assert_eq!(op.signature.inputs[0], TypeExpr::dir(TypeExpr::var("a")));
        assert_eq!(op.signature.output, TypeExpr::seq(TypeExpr::entry(TypeExpr::prim("Name"), TypeExpr::var("a"))));
        assert_eq!(op.description, "ls — list directory contents");
    }

    #[test]
    fn test_load_ops_pack_with_properties() {
        let yaml = r#"
ops:
  - name: unique
    type_params: [a]
    inputs: ["Seq(a)"]
    output: "Seq(a)"
    properties:
      idempotent: true
    description: "sort -u — deduplicate"
  - name: concat_seq
    type_params: [a]
    inputs: ["Seq(a)", "Seq(a)"]
    output: "Seq(a)"
    properties:
      associative: true
      identity: "empty_seq"
    description: "cat — concatenate sequences"
"#;
        let reg = load_ops_pack_str(yaml).unwrap();
        assert_eq!(reg.poly_op_names().len(), 2);

        let unique = reg.get_poly("unique").unwrap();
        assert!(unique.properties.idempotent);
        assert!(!unique.properties.commutative);

        let concat = reg.get_poly("concat_seq").unwrap();
        assert!(concat.properties.associative);
        assert_eq!(concat.properties.identity, Some("empty_seq".to_string()));
    }

    #[test]
    fn test_load_ops_pack_monomorphic() {
        let yaml = r#"
ops:
  - name: stat
    inputs: ["Path"]
    output: "Metadata"
    description: "stat — get file metadata"
"#;
        let reg = load_ops_pack_str(yaml).unwrap();
        let op = reg.get_poly("stat").unwrap();
        assert!(op.signature.type_params.is_empty());
        assert_eq!(op.signature.inputs, vec![TypeExpr::prim("Path")]);
        assert_eq!(op.signature.output, TypeExpr::prim("Metadata"));
    }

    #[test]
    fn test_load_ops_pack_empty() {
        let yaml = r#"
name: empty
ops: []
"#;
        let reg = load_ops_pack_str(yaml).unwrap();
        assert!(reg.poly_op_names().is_empty());
    }

    #[test]
    fn test_load_ops_pack_no_ops_field() {
        let yaml = r#"
name: minimal
description: no ops defined
"#;
        let reg = load_ops_pack_str(yaml).unwrap();
        assert!(reg.poly_op_names().is_empty());
    }

    #[test]
    fn test_load_ops_pack_malformed_type() {
        let yaml = r#"
ops:
  - name: bad_op
    inputs: ["Dir("]
    output: "Seq(a)"
"#;
        let err = load_ops_pack_str(yaml).unwrap_err();
        match err {
            OpsPackError::TypeParse { op_name, field, .. } => {
                assert_eq!(op_name, "bad_op");
                assert_eq!(field, "inputs[0]");
            }
            other => panic!("expected TypeParse error, got: {}", other),
        }
    }

    #[test]
    fn test_load_ops_pack_malformed_output() {
        let yaml = r#"
ops:
  - name: bad_out
    inputs: ["Path"]
    output: ""
"#;
        let err = load_ops_pack_str(yaml).unwrap_err();
        match err {
            OpsPackError::TypeParse { op_name, field, .. } => {
                assert_eq!(op_name, "bad_out");
                assert_eq!(field, "output");
            }
            other => panic!("expected TypeParse error, got: {}", other),
        }
    }

    #[test]
    fn test_load_ops_pack_lookup_works() {
        let yaml = r#"
ops:
  - name: read_file
    type_params: [a]
    inputs: ["File(a)"]
    output: "a"
    description: "cat — read file contents"
"#;
        let reg = load_ops_pack_str(yaml).unwrap();
        // Look up: what produces Bytes?
        let target = TypeExpr::prim("Bytes");
        let matches = reg.ops_for_output_expr(&target);
        assert_eq!(matches.len(), 1);
        assert_eq!(matches[0].op.name, "read_file");
        // Input should be File(Bytes)
        assert_eq!(matches[0].concrete_inputs[0], TypeExpr::file(TypeExpr::prim("Bytes")));
    }
}
