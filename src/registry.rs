use std::collections::HashMap;
use std::fmt;

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
    /// Index: output TypeId → list of op names that produce it
    by_output: HashMap<TypeId, Vec<String>>,
}

impl fmt::Debug for OperationRegistry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("OperationRegistry")
            .field("ops", &self.ops.keys().collect::<Vec<_>>())
            .field("by_output", &self.by_output)
            .finish()
    }
}

impl OperationRegistry {
    pub fn new() -> Self {
        Self {
            ops: HashMap::new(),
            by_output: HashMap::new(),
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
}
