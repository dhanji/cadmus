# Playbook: Adding Ops, Fact Packs, and Workflows

Step-by-step recipes for extending the reasoning engine with new domains.

---

## 1. Adding an Ops Pack (Pure YAML — No Rust)

This is the most common extension. You define typed operations in YAML and
the engine loads them at runtime.

### Step 1: Create the YAML file

Create `data/<domain>_ops.yaml`:

```yaml
name: my_domain
description: "Operations for my domain"

ops:
  - name: fetch_record
    type_params: [a]
    inputs: ["Table(a)"]
    output: "Seq(Entry(Name, a))"
    properties:
      idempotent: true
    description: "SELECT * — fetch all records from a table"

  - name: join_tables
    type_params: [a, b]
    inputs: ["Seq(Entry(Name, a))", "Seq(Entry(Name, b))"]
    output: "Seq(Entry(Name, Pair(a, b)))"
    description: "JOIN — combine two record sets"
```

### Step 2: Understand the YAML schema

Every op has these fields:

| Field | Required | Type | Notes |
|-------|----------|------|-------|
| `name` | yes | string | Unique op name. Snake_case. |
| `type_params` | no | list of strings | Polymorphic variables: `[a]`, `[a, b]`. Omit for monomorphic ops. |
| `inputs` | no | list of strings | Type expressions. Each parsed via `TypeExpr::parse()`. Empty = leaf op. |
| `output` | yes | string | Output type expression. |
| `properties` | no | map | Algebraic properties (see below). |
| `description` | no | string | Human-readable hint shown in dry-run traces. |

### Step 3: Write type expressions

Type expressions follow this grammar:

```
Uppercase(...)  → Constructor (if has parens) or Primitive (if no parens)
lowercase       → Type variable (bound during unification)
```

Examples:

| Expression | Meaning |
|------------|---------|
| `Bytes` | Primitive type |
| `a` | Type variable |
| `File(a)` | Constructor with one type arg |
| `Entry(Name, a)` | Constructor with two args |
| `Seq(Entry(Name, File(Bytes)))` | Nested constructors |
| `Option(a)` | Optional value |

**You can invent new primitives and constructors freely.** They're just
strings. No Rust changes needed. If you write `Table(a)` or `Query`, the
parser accepts them immediately.

### Step 4: Declare algebraic properties

```yaml
properties:
  idempotent: true      # op(op(x)) = op(x)
  commutative: true     # op(a, b) = op(b, a)
  associative: true     # op(op(a,b), c) = op(a, op(b,c))
  identity: "empty"     # op(x, empty) = x
  absorbing: "null"     # op(x, null) = null
```

All fields default to `false`/`null`. Only declare what's true.

The planner uses these:
- `associative` enables **fold insertion** (reduce `Seq(B) → B`)
- `idempotent` enables deduplication during canonicalization
- `commutative` enables canonical operand ordering

### Step 5: Load the pack

**Option A: Embedded in a strategy (compile-time)**

```rust
const MY_OPS: &str = include_str!("../data/my_domain_ops.yaml");

fn build_registry() -> OperationRegistry {
    load_ops_pack_str(MY_OPS).expect("my_domain ops should parse")
}
```

**Option B: From disk (runtime, editable without recompilation)**

```rust
fn build_registry() -> OperationRegistry {
    load_ops_pack("data/my_domain_ops.yaml").expect("load my_domain ops")
}
```

**Option C: Merge into an existing registry**

```rust
let mut reg = OperationRegistry::new();
// ... register other ops ...
load_ops_pack_str_into(MY_OPS_YAML, &mut reg)?;
```

See `src/fs_types.rs` for the disk-with-embedded-fallback pattern.

### Step 6: Verify

Write a test that loads the pack and checks op count + signatures:

```rust
#[test]
fn test_my_domain_ops_load() {
    let reg = load_ops_pack_str(include_str!("../data/my_domain_ops.yaml")).unwrap();
    assert!(reg.get_poly("fetch_record").is_some());
    assert!(reg.get_poly("join_tables").is_some());
    assert_eq!(reg.poly_op_names().len(), 2);
}
```

---

## 2. Adding Ops That Need Execution Bindings (Rust Required)

If your ops need to actually *run* (not just plan), you need Rust exec
bindings. This is the **dual registration** pattern.

### When you need this

- The comparison strategy (`src/strategy.rs`) — ops like `compare` have
  closures that produce text output
- The coding strategy (`src/coding_strategy.rs`) — ops like `detect_smells`
  have closures that analyze code

The filesystem strategy does NOT need this — it only produces dry-run traces.

### How it works

Register the monomorphic version with an exec binding in Rust, then load
the polymorphic version from YAML:

```rust
// 1. Monomorphic registration with exec binding
reg.register(
    "my_op",
    OpSignature::new(vec![TypeId::new("Input")], TypeId::new("Output")),
    AlgebraicProperties::none(),
    Box::new(|ctx: &ExecContext| {
        let input = &ctx.inputs[0].value;
        Ok(format!("processed: {}", input))
    }),
);

// 2. Polymorphic registration from YAML (for type-directed planning)
load_ops_pack_str_into(MY_OPS_YAML, &mut reg)?;
```

The monomorphic path is used by `execute_node()` in the strategy. The
polymorphic path is used by the planner for type-directed search.

### Reference implementations

- `src/strategy.rs` lines 60–210: `ComparisonStrategy::build_registry()`
- `src/coding_strategy.rs` lines 65–200: `CodingStrategy::build_registry()`

---

## 3. Adding a Fact Pack

Fact packs store domain knowledge: entities, axes of comparison, claims,
evidence, and measurable properties.

### Step 1: Create the YAML file

Create `data/<domain>_facts.yaml`:

```yaml
entities:
  - id: postgres
    name: "PostgreSQL"
    description: "Open-source relational database"
  - id: mysql
    name: "MySQL"
    description: "Popular open-source relational database"

axes:
  - id: performance
    name: "Performance"
    description: "Query execution speed and throughput"
    polarity: capability
    sub_axes:
      - id: oltp
        name: "OLTP Workloads"
      - id: olap
        name: "OLAP Workloads"

  - id: features
    name: "Feature Set"
    description: "SQL features and extensions"
    polarity: capability
    sub_axes: []

claims:
  - id: pg_perf_oltp
    entity: postgres
    axis: performance
    sub_axis: oltp
    text: "PostgreSQL handles high-concurrency OLTP well with MVCC"

  - id: mysql_perf_oltp
    entity: mysql
    axis: performance
    sub_axis: oltp
    text: "MySQL InnoDB provides strong OLTP performance with row-level locking"

evidence:
  - id: ev_pg_mvcc
    supports: pg_perf_oltp
    text: "PostgreSQL's MVCC implementation avoids read locks entirely"
    source: "PostgreSQL documentation"

properties:
  - entity: postgres
    axis: performance
    key: max_connections_default
    value: "100"
    ordinal: 100
  - entity: mysql
    axis: performance
    key: max_connections_default
    value: "151"
    ordinal: 151

relations:
  - kind: hierarchy
    id: rel_perf_hierarchy
    axis: performance
    parent: performance
    children: [oltp, olap]

  - kind: ordinal
    id: rel_max_conn
    axis: performance
    property_key: max_connections_default
    direction: higher_is_more
    note: "Higher default allows more concurrent connections"

uncertainties:
  - axis: performance
    text: "Benchmark results vary significantly by workload and hardware"
```

### Step 2: Understand the schema

| Section | Required | Purpose |
|---------|----------|---------|
| `entities` | yes | The things being compared (2+) |
| `axes` | yes | Dimensions of comparison |
| `claims` | yes | Per-entity, per-axis assertions |
| `evidence` | yes | Supporting evidence for claims |
| `properties` | no | Measurable values with ordinals (enables cross-entity comparison) |
| `relations` | no | Structural metadata: axis hierarchies, ordinal declarations |
| `uncertainties` | no | Per-axis caveats |

Key rules:
- Claims are **per-entity** — never reference another entity
- Cross-entity comparison is derived by the **theory layer**, not stated in data
- Properties with `ordinal` values enable automatic ordinal comparison
- Every evidence entry must `supports` a claim `id`

### Step 3: Load and index

```rust
use reasoning_engine::fact_pack::load_fact_pack;

let idx = load_fact_pack(Path::new("data/db_facts.yaml"))?;

// Query the index
let pg_claims = idx.claims_by_axis_entity.get(&("performance".into(), "postgres".into()));
let evidence = idx.evidence_by_claim.get("pg_perf_oltp");
```

### Step 4: Use with a strategy

To use a fact pack with the comparison strategy, pass its path as the
`fact_pack_path` in a `Goal`:

```rust
let goal = Goal {
    description: "Compare PostgreSQL and MySQL".into(),
    entities: vec!["postgres".into(), "mysql".into()],
    fact_pack_path: "data/db_facts.yaml".into(),
};
let output = pipeline::run(&goal)?;
```

---

## 4. Adding a Workflow

Workflows are linear pipelines of filesystem operations defined in YAML.

### Step 1: Create the YAML file

Create `data/workflows/<name>.yaml`:

```yaml
workflow: "Organize downloads by type"
inputs:
  path: "~/Downloads"
  pattern: "*.pdf"
steps:
  - walk_tree
  - filter:
      extension: ".pdf"
  - sort_by: name
```

### Step 2: Step forms

| YAML form | Meaning | Example |
|-----------|---------|---------|
| Bare string | Op with no args | `walk_tree` |
| `op: each` | Map mode (apply per element) | `read_file: each` |
| `op: scalar` | Op with a flag/mode | `sort_by: name` |
| `op: { k: v }` | Op with named params | `filter: { extension: ".pdf" }` |

Variable expansion: `$name` references a key from `inputs`.

### Step 3: Run it

```bash
cargo run -- --workflow data/workflows/my_workflow.yaml
```

Or programmatically:

```rust
let trace = workflow::load_and_run(Path::new("data/workflows/my_workflow.yaml"))?;
println!("{}", trace);
```

### Step 4: Test it

```rust
#[test]
fn test_my_workflow() {
    let yaml = include_str!("../data/workflows/my_workflow.yaml");
    let def: WorkflowDef = serde_yaml::from_str(yaml).unwrap();
    let compiled = compile_workflow(&def, &build_fs_registry()).unwrap();
    let trace = execute_workflow(&compiled).unwrap();
    assert!(trace.to_string().contains("walk_tree"));
}
```

---

## 5. Complete Worked Example: Database Domain

Here's a full end-to-end example adding a "database" domain with ops pack,
fact pack, and verification — no Rust changes needed for the ops.

### 5a. Create `data/db_ops.yaml`

```yaml
name: database
description: "Relational database operations"

ops:
  - name: query_table
    type_params: [a]
    inputs: ["Table(a)"]
    output: "Seq(Row(a))"
    properties:
      idempotent: true
    description: "SELECT * — scan all rows"

  - name: filter_rows
    type_params: [a]
    inputs: ["Seq(Row(a))", "Predicate"]
    output: "Seq(Row(a))"
    properties:
      idempotent: true
    description: "WHERE — filter rows by predicate"

  - name: project_columns
    type_params: [a, b]
    inputs: ["Seq(Row(a))", "ColumnList"]
    output: "Seq(Row(b))"
    description: "SELECT cols — project specific columns"

  - name: aggregate
    type_params: [a]
    inputs: ["Seq(Row(a))", "AggFunc"]
    output: "Scalar"
    description: "COUNT/SUM/AVG — aggregate rows"

  - name: join
    type_params: [a, b]
    inputs: ["Seq(Row(a))", "Seq(Row(b))", "JoinKey"]
    output: "Seq(Row(Pair(a, b)))"
    description: "JOIN — combine two tables on key"

  - name: sort_rows
    type_params: [a]
    inputs: ["Seq(Row(a))", "SortKey"]
    output: "Seq(Row(a))"
    properties:
      idempotent: true
    description: "ORDER BY — sort result set"

  - name: limit_rows
    type_params: [a]
    inputs: ["Seq(Row(a))", "Count"]
    output: "Seq(Row(a))"
    description: "LIMIT — take first N rows"

  - name: insert_row
    type_params: [a]
    inputs: ["Table(a)", "Row(a)"]
    output: "Unit"
    description: "INSERT — add a row"

  - name: delete_rows
    type_params: [a]
    inputs: ["Table(a)", "Predicate"]
    output: "Count"
    description: "DELETE — remove matching rows, return count"

  - name: create_index
    type_params: [a]
    inputs: ["Table(a)", "ColumnList"]
    output: "Index(a)"
    properties:
      idempotent: true
    description: "CREATE INDEX — build index on columns"
```

New types introduced (no Rust changes):
- `Table(a)`, `Row(a)`, `Index(a)` — constructors
- `Predicate`, `ColumnList`, `AggFunc`, `SortKey`, `JoinKey`, `Scalar` — primitives

### 5b. Verify it loads

```rust
#[test]
fn test_db_ops_load() {
    let yaml = include_str!("../data/db_ops.yaml");
    let reg = load_ops_pack_str(yaml).unwrap();
    assert_eq!(reg.poly_op_names().len(), 10);

    // Check a specific signature
    let query = reg.get_poly("query_table").unwrap();
    assert_eq!(query.signature.type_params, vec!["a"]);
    assert_eq!(query.signature.inputs.len(), 1);
    assert!(query.properties.idempotent);

    // Check unification-based lookup
    let target = TypeExpr::parse("Seq(Row(UserData))").unwrap();
    let matches = reg.ops_for_output_expr(&target);
    let names: Vec<&str> = matches.iter().map(|m| m.op.name.as_str()).collect();
    assert!(names.contains(&"query_table"));
}
```

### 5c. Create `data/db_facts.yaml` (optional)

```yaml
entities:
  - id: postgres
    name: "PostgreSQL"
    description: "Advanced open-source relational database"
  - id: sqlite
    name: "SQLite"
    description: "Embedded relational database"

axes:
  - id: concurrency
    name: "Concurrency"
    description: "Multi-user access patterns"
    polarity: capability
    sub_axes: []

claims:
  - id: pg_concurrent
    entity: postgres
    axis: concurrency
    text: "PostgreSQL supports full MVCC with concurrent readers and writers"
  - id: sqlite_concurrent
    entity: sqlite
    axis: concurrency
    text: "SQLite uses file-level locking; one writer at a time"

evidence:
  - id: ev_pg_mvcc
    supports: pg_concurrent
    text: "PostgreSQL MVCC allows reads to never block writes"
    source: "PostgreSQL docs"
  - id: ev_sqlite_wal
    supports: sqlite_concurrent
    text: "WAL mode allows concurrent reads but still single writer"
    source: "SQLite docs"

properties:
  - entity: postgres
    axis: concurrency
    key: max_writers
    value: "unlimited"
    ordinal: 1000
  - entity: sqlite
    axis: concurrency
    key: max_writers
    value: "1"
    ordinal: 1

relations:
  - kind: ordinal
    id: rel_writers
    axis: concurrency
    property_key: max_writers
    direction: higher_is_more

uncertainties:
  - axis: concurrency
    text: "Concurrency behavior depends heavily on workload and configuration"
```

---

## 6. Common Mistakes

### Malformed type expressions

```yaml
# ❌ Wrong — unclosed paren
inputs: ["Dir("]

# ❌ Wrong — empty string
output: ""

# ❌ Wrong — spaces in identifier
inputs: ["File (Bytes)"]
#         ^^^^^ space before paren is OK (parser trims), but
#               "File " is not a valid identifier

# ✅ Correct
inputs: ["Dir(a)"]
output: "Seq(Entry(Name, a))"
```

The loader reports the exact op name and field:
```
ops pack type error in 'bad_op' field 'inputs[0]': parse error at position 4: ...
```

### Forgetting to load the pack

Defining a YAML file doesn't register anything. You must call one of:
- `load_ops_pack_str(yaml)` — returns new registry
- `load_ops_pack(path)` — reads file, returns new registry
- `load_ops_pack_str_into(yaml, &mut reg)` — merges into existing

### Type variable naming

```yaml
# ❌ Wrong — uppercase = primitive, not variable
type_params: [A]
inputs: ["File(A)"]
# This parses A as Primitive("A"), not Var("A")

# ✅ Correct — lowercase = variable
type_params: [a]
inputs: ["File(a)"]
```

### Missing output field

```yaml
# ❌ Wrong — output is required
ops:
  - name: my_op
    inputs: ["Path"]
    # no output field → serde error
```

### Monomorphic ops with type_params

```yaml
# ❌ Confusing — type_params declared but not used in signature
- name: stat
  type_params: [a]
  inputs: ["Path"]
  output: "Metadata"
  # 'a' is declared but never appears in inputs or output

# ✅ Correct — omit type_params for monomorphic ops
- name: stat
  inputs: ["Path"]
  output: "Metadata"
```

### Properties that don't match semantics

```yaml
# ❌ Wrong — delete is NOT idempotent (second delete fails)
- name: delete
  properties:
    idempotent: true

# ✅ Correct
- name: delete
  properties:
    idempotent: false  # or just omit (defaults to false)
```

The planner trusts your property declarations. Wrong properties lead to
incorrect plan optimizations.

---

## Quick Reference

### Ops pack YAML schema

```yaml
name: string          # optional
description: string   # optional
ops:
  - name: string      # required, unique
    type_params: [a]  # optional, lowercase vars
    inputs: ["..."]   # optional, list of TypeExpr strings
    output: "..."     # required, TypeExpr string
    properties:       # optional
      idempotent: bool
      commutative: bool
      associative: bool
      identity: string
      absorbing: string
    description: string  # optional, shown in traces
```

### Fact pack YAML schema

```yaml
entities:
  - { id, name, description }
axes:
  - { id, name, description, polarity?, sub_axes?: [{ id, name }] }
claims:
  - { id, entity, axis, sub_axis?, text }
evidence:
  - { id, supports, text, source? }
properties:
  - { entity, axis, key, value, ordinal?, note? }
relations:
  - { kind: hierarchy, id, axis, parent, children: [...] }
  - { kind: ordinal, id, axis, property_key, direction, note? }
uncertainties:
  - { axis, text }
```

### Workflow YAML schema

```yaml
workflow: string
inputs:
  key: value
steps:
  - op_name                    # bare
  - op_name: each              # map mode
  - op_name: flag              # scalar arg
  - op_name: { k: v, ... }    # named params
  - op_name:
      param: $input_ref        # variable expansion
```

### Loader functions

```rust
// New registry from YAML string
let reg = load_ops_pack_str(yaml)?;

// New registry from file
let reg = load_ops_pack("data/my_ops.yaml")?;

// Merge into existing registry
load_ops_pack_str_into(yaml, &mut reg)?;

// Load fact pack
let idx = load_fact_pack(Path::new("data/my_facts.yaml"))?;
```
