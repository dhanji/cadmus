# Playbook: Adding Ops, Fact Packs, and Workflows

Step-by-step recipes for extending Cadmus with new domains.

---

## 0. Before You Start: Audit Existing Packs

**Do this first, every time.** Before writing any YAML, audit what already
exists to avoid duplication and identify composition opportunities.

### Step 1: List all existing ops

```bash
# Show all op names across all packs
for f in data/*_ops.yaml; do
  echo "=== $f ==="
  grep "^  - name:" "$f" | sed 's/.*name: //'
done
```

Current packs and their coverage:

| Pack | Ops | Domain |
|------|-----|--------|
| `data/packs/ops/fs_ops.yaml` | 49 | Filesystem: ls, cat, mv, find, grep, sort, zip, tar, chmod, curl, rsync, macOS tools |
| `data/packs/ops/power_tools_ops.yaml` | 64 | Dev tools: git, tmux, screen, jq, yq, awk, sed, ps, ssh, gzip, base64 |
| `comparison_ops.yaml` | 6 | Reasoning: retrieve_evidence, compare_claims, summarize |
| `coding_ops.yaml` | 6 | Code analysis: parse_source, detect_smells, generate_tests |

### Step 2: Check for overlaps

```bash
# Find op name collisions between two packs
comm -12 \
  <(grep "^  - name:" data/packs/ops/fs_ops.yaml | sed 's/.*name: //' | sort) \
  <(grep "^  - name:" data/packs/ops/power_tools_ops.yaml | sed 's/.*name: //' | sort)
```

If there are collisions, decide:
- **Rename** one of them (e.g. `replace` in fs_ops vs `sed_replace` in power_tools)
- **Move** the op to the pack where it fits best
- **Keep both** if they have genuinely different type signatures

### Step 3: Identify gaps

Ask yourself:
- What tools/operations does my domain need that don't exist yet?
- Which existing ops can I reuse? (e.g. `filter`, `sort_by` from fs_ops)
- Do I need new type primitives, or can I reuse existing ones?

### Step 4: Plan entity groupings (for fact packs)

For meaningful comparison, group entities by category:
- **Two-way**: tmux vs screen, jq vs yq, awk vs sed
- **Three-way**: ripgrep vs grep vs ag
- **Solo**: git (no direct peer, but rich axis coverage)

Each group should share axes where comparison makes sense.

### Step 5: Check composability needs

Will your fact pack need to be merged with existing packs? If so:
- Use **different entity ids** from existing packs (or accept first-wins dedup)
- Shared axis ids will have their sub_axes **unioned** automatically
- Claims, evidence, and properties are always concatenated

See [Section 3a: Fact Pack Composition](#3a-fact-pack-composition) for details.

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
use cadmus::fact_pack::load_fact_pack;

let idx = load_fact_pack(Path::new("data/db_facts.yaml"))?;

// Query the index
let pg_claims = idx.claims_by_axis_entity.get(&("performance".into(), "postgres".into()));
let evidence = idx.evidence_by_claim.get("pg_perf_oltp");
```

### Step 4: Use with a strategy

To use a fact pack with the comparison strategy, pass its path in a `Goal`:

```rust
let goal = Goal {
    description: "Compare PostgreSQL and MySQL".into(),
    entities: vec!["postgres".into(), "mysql".into()],
    fact_pack_paths: vec!["data/db_facts.yaml".into()],
};
let output = pipeline::run(&goal)?;
```

---

## 3a. Fact Pack Composition

Fact packs compose — you can merge multiple packs into a single index for
cross-domain reasoning.

### When to compose

- **"How do I set up tmux to manage my macOS files?"** — needs facts from
  both `power_tools.yaml` (tmux) and `macos_fs.yaml` (filesystem)
- **"Compare git's ecosystem to PostgreSQL's"** — needs facts from
  `power_tools.yaml` and `db_facts.yaml`

### How merging works

```rust
use cadmus::fact_pack::{load_fact_packs, FactPack};

// Option A: Load and merge from file paths
let idx = load_fact_packs(&["data/packs/facts/power_tools.yaml", "data/db_facts.yaml"])?;

// Option B: Merge FactPack structs directly
let merged = pack_a.merge(pack_b);
let idx = FactPackIndex::build(merged);

// Option C: Merge many packs
let merged = FactPack::merge_all(vec![pack_a, pack_b, pack_c]);
```

### Deduplication rules

Packs are merged left-to-right. When ids collide:

| Section | Rule | Example |
|---------|------|---------|
| **Entities** | First wins (later duplicates skipped) | If both define `git`, first pack's description kept |
| **Axes** | First wins, but sub_axes are **unioned** | If both define `performance`, sub_axes from both appear |
| **Relations** | First wins (dedup by id) | Duplicate `rel_perf_hierarchy` kept from first pack |
| **Claims** | Concatenated (no dedup) | All claims from all packs appear |
| **Evidence** | Concatenated (no dedup) | All evidence from all packs appear |
| **Properties** | Concatenated (no dedup) | All properties from all packs appear |
| **Uncertainties** | Concatenated (no dedup) | All uncertainties from all packs appear |

### Using composed packs with a Goal

```rust
let goal = Goal {
    description: "Compare tools across domains".into(),
    entities: vec!["git".into(), "postgres".into()],
    fact_pack_paths: vec![
        "data/packs/facts/power_tools.yaml".into(),
        "data/db_facts.yaml".into(),
    ],
};
let output = pipeline::run(&goal)?;
```

### When to split vs merge

| Scenario | Recommendation |
|----------|---------------|
| Entities are in the same domain | **One pack** (e.g. tmux + screen in `power_tools.yaml`) |
| Entities are in different domains | **Separate packs**, compose at query time |
| Shared axes (e.g. `performance`) | Fine — sub_axes are unioned on merge |
| Shared entity ids across packs | Avoid if possible; first pack wins on conflict |

---

## 4. Adding a Workflow

Workflows are linear pipelines of operations defined in YAML.

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

### Step 3: Type inference for inputs

The workflow compiler infers types from input names and values:

| Input pattern | Inferred type | Example |
|---------------|---------------|---------|
| `.cbz` extension | `File(Archive(File(Image), Cbz))` | `archive: "comics.cbz"` |
| `.zip` extension | `File(Archive(Bytes, Zip))` | `archive: "data.zip"` |
| `.json` extension | `File(Json)` | `config: "settings.json"` |
| `.yaml`/`.yml` | `File(Yaml)` | `config: "values.yaml"` |
| `.csv` extension | `File(Csv)` | `data: "report.csv"` |
| `.log`/`.txt`/`.rs`/etc | `File(Text)` | `logfile: "app.log"` |
| `http://`/`https://` | `URL` | `url: "https://example.com"` |
| Name contains `dir`/`path`, or `/` prefix | `Dir(Bytes)` | `path: "~/Downloads"` |
| Name is `repo` or `.git` suffix | `Repo` | `repo: "my-project.git"` |
| Name contains `pattern`/`keyword` | `Pattern` | `pattern: "*.pdf"` |

### Step 4: Run it

```bash
cargo run -- --workflow data/workflows/my_workflow.yaml
```

Or programmatically:

```rust
let trace = workflow::load_and_run(Path::new("data/workflows/my_workflow.yaml"))?;
println!("{}", trace);
```

### Step 5: Test it

```rust
#[test]
fn test_my_workflow() {
    let yaml = include_str!("../data/workflows/my_workflow.yaml");
    let def: WorkflowDef = serde_yaml::from_str(yaml).unwrap();
    let compiled = compile_workflow(&def, &build_full_registry()).unwrap();
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

## 6. Complete Worked Example: Power Tools Pack

This is a real example from the codebase — a comprehensive pack covering
developer power tools (git, tmux, jq, awk, etc.).

### 6a. Audit phase (Section 0)

Before writing any YAML, we audited existing packs:

**Already covered by `data/packs/ops/fs_ops.yaml`:**
- File I/O (ls, cat, mv, find, cp, rm, mkdir)
- Basic text processing (sed s///, head, tail, sort, grep, diff)
- Archives (zip, tar, extract, pack)
- macOS tools (mdfind, xattr, open, pbcopy)
- Network basics (curl/download, rsync/sync)

**Gaps identified:**
- Version control (git) — nothing VCS-related
- Terminal multiplexers (tmux, screen)
- Structured data (jq, yq, csv tools)
- Advanced text (awk, cut, tr, paste, tee)
- Process management (ps, kill, df, du)
- Networking (ssh, scp, ping, dig)
- Compression (gzip, xz, base64)

**Overlap decisions:**
- `sed` → fs_ops has `replace` (simple s///). Power tools adds `sed_script` (multi-command)
- `grep` → fs_ops has `search_content` and `filter`. Power tools doesn't duplicate
- `sort` → fs_ops has `sort_by`. Power tools doesn't duplicate

### 6b. Ops pack (`data/packs/ops/power_tools_ops.yaml`)

64 ops across 7 categories. Key design decisions:
- **Git ops** use domain types: `Repo`, `Commit`, `Branch`, `StagingArea`, `Diff`
- **Polymorphic ops** where appropriate: `tee_split(a, Path) → a`, `gzip_compress(File(a)) → File(Gzip(a))`
- **Leaf ops** (no inputs): `ps_list`, `df_usage`, `uname_info`, `uptime_info`
- **3-input ops**: `git_merge(Repo, Branch, MergeStrategy)`, `git_push(Repo, Remote, Branch)`

### 6c. Fact pack (`data/packs/facts/power_tools.yaml`)

10 entities, 5 axes, 80 claims. Entity groupings:
- **tmux vs screen** — terminal multiplexers
- **ripgrep vs grep vs ag** — three-way text search comparison
- **jq vs yq** — structured data query
- **awk vs sed** — stream processing
- **git** — solo entity with rich axis coverage

### 6d. Composition

The power tools fact pack composes with other packs:

```rust
let goal = Goal {
    description: "Compare tools across domains".into(),
    entities: vec!["git".into(), "postgres".into()],
    fact_pack_paths: vec![
        "data/packs/facts/power_tools.yaml".into(),
        "data/db_facts.yaml".into(),
    ],
};
```

### 6e. Registry wiring

Power tools ops are merged into the workflow registry via `build_full_registry()`:

```rust
// src/fs_types.rs
pub fn build_full_registry() -> OperationRegistry {
    let mut reg = build_fs_registry();  // fs_ops only
    load_ops_pack_str_into(POWER_TOOLS_OPS_YAML, &mut reg);
    reg
}
```

The fs_strategy planner uses `build_fs_registry()` (fs-only) to keep the
search space manageable. The workflow system uses `build_full_registry()`
for the complete op set.

---

## 7. Common Mistakes

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

### Fact pack entity contamination

```yaml
# ❌ Wrong — claim references another entity
claims:
  - id: pg_faster_than_mysql
    entity: postgres
    axis: performance
    text: "PostgreSQL is faster than MySQL for OLAP"
    #      ^^^^^^^^^^^^^^^^^^^^^^^^^ references mysql!

# ✅ Correct — claim is self-contained
claims:
  - id: pg_olap_perf
    entity: postgres
    axis: performance
    text: "PostgreSQL excels at OLAP workloads with parallel query execution"
```

The theory layer derives comparisons. Your claims should never mention
other entities.

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

// Load single fact pack
let idx = load_fact_pack(Path::new("data/my_facts.yaml"))?;

// Load and merge multiple fact packs
let idx = load_fact_packs(&["data/pack_a.yaml", "data/pack_b.yaml"])?;

// Merge FactPack structs directly
let merged = pack_a.merge(pack_b);
let idx = FactPackIndex::build(merged);
```

---

## 8. Adding File Types (Pure YAML — No Rust)

The file type dictionary lives in `data/filetypes.yaml` and is loaded at
runtime. Adding a new file type requires **zero Rust changes** — just edit
the YAML file.

### What the dictionary does

The file type dictionary is the **single source of truth** for:
- **NL path detection** — `slots.rs` and `dialogue.rs` use it to recognize
  file extensions in user input (e.g. "copy photo.heic" → detects `.heic`)
- **Workflow type inference** — `workflow.rs` uses it to infer `TypeExpr`
  from file extensions (e.g. `.json` → `File(Json)`, `.cbz` → `File(Archive(File(Image), Cbz))`)
- **Tool hints** — `describe_file_type()` returns ops and external tools
  for any extension

### Step 1: Add an entry to `data/filetypes.yaml`

```yaml
  - ext: webp
    category: image
    description: "WebP image (modern web format)"
    type_expr: "File(Image)"
    relevant_ops: [open_file, open_with, copy, checksum, stat, reveal]
    tools: [imagemagick, cwebp, dwebp]
```

### Step 2: Understand the schema

| Field | Required | Type | Notes |
|-------|----------|------|-------|
| `ext` | yes | string | Extension without dot. Lowercase. Compound exts OK: `tar.gz` |
| `category` | yes | string | One of: `image`, `video`, `audio`, `document`, `archive`, `source_code`, `data`, `config`, `markup`, `database`, `font`, `executable`, `disk_image`, `ebook` |
| `description` | yes | string | Human-readable description |
| `type_expr` | yes | string | TypeExpr string (parsed via `TypeExpr::parse`). See Section 1 Step 3 for grammar. |
| `relevant_ops` | no | list | Op names from `data/packs/ops/fs_ops.yaml` / `data/packs/ops/power_tools_ops.yaml` |
| `tools` | no | list | External CLI tools (not engine ops) |

### Step 3: Choose the right `type_expr`

| File kind | type_expr | Example extensions |
|-----------|-----------|-------------------|
| Text/code | `File(Text)` | `.rs`, `.py`, `.md`, `.sh` |
| JSON | `File(Json)` | `.json`, `.ndjson` |
| YAML | `File(Yaml)` | `.yaml`, `.yml` |
| CSV | `File(Csv)` | `.csv`, `.tsv` |
| PDF | `File(PDF)` | `.pdf` |
| Image | `File(Image)` | `.png`, `.jpg`, `.heic` |
| Video | `File(Video)` | `.mp4`, `.mkv`, `.webm` |
| Audio | `File(Audio)` | `.mp3`, `.flac`, `.wav` |
| Document | `File(Document)` | `.docx`, `.pptx`, `.odt` |
| Ebook | `File(Ebook)` | `.epub`, `.mobi` |
| Disk image | `File(DiskImage)` | `.dmg`, `.iso` |
| Plist | `File(Plist)` | `.plist` |
| ZIP archive | `File(Archive(Bytes, Zip))` | `.zip` |
| Gzipped tar | `File(Archive(Bytes, TarGz))` | `.tar.gz`, `.tgz` |
| Comic book | `File(Archive(File(Image), Cbz))` | `.cbz` |
| Gzip file | `File(Gzip(Bytes))` | `.gz` |
| XZ file | `File(Xz(Bytes))` | `.xz` |
| Binary/opaque | `File(Bytes)` | `.exe`, `.wasm`, `.db` |

**You can invent new type primitives freely.** If you write `File(Spreadsheet)`,
the parser accepts it immediately. No Rust changes needed.

### Step 4: Map relevant ops

Check which ops from `data/packs/ops/fs_ops.yaml` and `data/packs/ops/power_tools_ops.yaml`
make sense for your file type:

```bash
# List all available ops
for f in data/*_ops.yaml; do
  grep "^  - name:" "$f" | sed 's/.*name: //'
done | sort
```

Common op mappings:

| Category | Typical ops |
|----------|-------------|
| Any file | `copy`, `stat`, `checksum`, `rename`, `move_entry`, `delete` |
| Text/code | `read_file`, `search_content`, `replace`, `head`, `tail`, `diff`, `count`, `filter`, `sort_by`, `unique`, `git_add`, `git_diff`, `git_blame` |
| Structured data | `jq_query`, `jq_transform` (JSON), `yq_query`, `yq_convert` (YAML), `csv_cut`, `csv_join`, `csv_sort` (CSV) |
| Archives | `extract_archive`, `pack_archive`, `gzip_decompress`, `xz_compress` |
| macOS | `open_file`, `open_with`, `reveal`, `spotlight_search` |
| Config | `read_plist`, `write_plist` (for `.plist`) |
| Large files | `du_size` |

### Step 5: Verify

```bash
# Validate YAML parses and all ops exist
python3 -c "
import yaml
all_ops = set()
for f in ['data/packs/ops/fs_ops.yaml', 'data/packs/ops/power_tools_ops.yaml']:
    with open(f) as fh:
        for op in yaml.safe_load(fh)['ops']:
            all_ops.add(op['name'])
with open('data/filetypes.yaml') as f:
    data = yaml.safe_load(f)
bad = [(e['ext'], op) for e in data['filetypes'] for op in e.get('relevant_ops', []) if op not in all_ops]
print(f'{len(data[\"filetypes\"])} entries, {len(bad)} invalid ops')
for ext, op in bad: print(f'  .{ext}: unknown op {op}')
"
```

Then run the tests:

```bash
cargo test
```

All existing tests should pass. The dictionary is loaded once via `OnceLock`
and cached for the process lifetime.

### Step 6: Compound extensions

For compound extensions like `.tar.gz`, `.tar.bz2`, `.tar.xz`:

```yaml
  - ext: tar.gz
    category: archive
    description: "Gzipped TAR archive"
    type_expr: "File(Archive(Bytes, TarGz))"
    relevant_ops: [extract_archive, pack_archive, gzip_decompress, copy]
    tools: [tar, gzip, 7z]
```

The loader automatically detects compound extensions (those containing `.`)
and tries them **longest-first** before simple extensions. So `backup.tar.gz`
matches `tar.gz`, not `gz`.

### Reference

- **YAML file**: `data/filetypes.yaml` (197 entries, 14 categories)
- **Rust loader**: `src/filetypes.rs` (thin serde loader, `OnceLock` singleton)
- **Consumers**: `src/workflow.rs` (type inference), `src/nl/slots.rs` (path detection), `src/nl/dialogue.rs` (path detection)
- **Query API**: `filetypes::dictionary()` → `lookup()`, `lookup_by_path()`, `is_known_extension()`, `has_known_extension()`, `extensions_for_category()`, `describe_file_type()`, `all_extensions()`

---

## 9. Extending the NL Layer (Pure YAML — No Rust)

The Natural Language UX layer's word lists, dictionaries, and synonym tables
are all externalized to YAML files in `data/nl/`. You can extend the NL
layer's vocabulary without touching any Rust code.

### Adding a Synonym

To teach the NL layer a new phrase → op mapping, edit `data/nl/nl_vocab.yaml`:

```yaml
synonyms:
  # ... existing entries ...
  - phrase: [compress, everything]
    op: pack_archive
```

**Rules:**
- Multi-word phrases are matched greedily (longest match first)
- The `op` must be a valid op name from the ops YAML packs
- Phrases are lowercase tokens (the normalizer lowercases input)
- The loader sorts by phrase length automatically

### Adding a Contraction

```yaml
contractions:
  "won't": "will not"
  "y'all": "you all"    # ← add new ones here
```

Contractions are applied during tokenization (before synonym matching).
Longer contractions are applied first automatically.

### Adding Approval/Rejection Words

```yaml
approvals:
  single:
    - roger    # ← add single-word approvals
  multi:
    - sounds perfect    # ← add multi-word approval phrases

rejections:
  single:
    - nix    # ← add single-word rejections
  multi:
    - that is terrible    # ← add multi-word rejection phrases
```

### Adding Stopwords

Stopwords are filtered out during slot extraction (they don't become keywords):

```yaml
stopwords:
  - the
  - a
  # ... add more here
```

### Adding Dictionary Words (Typo Correction)

Edit `data/nl/nl_dictionary.yaml` to add words to the SymSpell dictionary:

```yaml
# Add a new category or extend an existing one
my_domain_words:
  kubernetes: 60
  terraform: 40
  ansible: 40
```

**Frequency guidelines:**
- **80-100**: Core op words, must-correct targets
- **40-60**: Domain vocabulary, moderate priority
- **20**: Common English, low priority (prevents false corrections)

### Adding a New Op (Automatic NL Recognition)

When you add a new op to any ops YAML pack (`data/packs/ops/fs_ops.yaml`,
`data/packs/ops/power_tools_ops.yaml`, etc.), it is **automatically** recognized by
the NL layer:

1. `is_canonical_op()` derives its set from the registry at load time
2. `get_op_explanation()` reads the `description` field from the ops YAML
3. `generate_workflow_name()` derives display names from the `description` field

No separate registration step needed. Just add the op to the YAML pack.

### Op Descriptions (Display Names)

Op descriptions live in the ops YAML packs themselves (not in a separate file):

```yaml
# In data/packs/ops/fs_ops.yaml:
- name: walk_tree
  description: "find — recursively walk directory tree (flattened)"
```

The NL layer uses these descriptions for:
- `get_op_explanation()` — answering "what does walk_tree mean?"
- `generate_workflow_name()` — generating workflow display names

### Reference

- **Vocab YAML**: `data/nl/nl_vocab.yaml` (synonyms, contractions, ordinals, approvals, rejections, stopwords)
- **Dictionary YAML**: `data/nl/nl_dictionary.yaml` (~2473 frequency-weighted words)
- **Rust loader**: `src/nl/vocab.rs` (thin serde loader, `OnceLock` singleton)
- **Op descriptions**: `data/packs/ops/fs_ops.yaml`, `data/packs/ops/power_tools_ops.yaml` (description field on each op)
- **Consumers**: `src/nl/normalize.rs` (synonyms, contractions, ordinals), `src/nl/intent.rs` (approvals, rejections), `src/nl/slots.rs` (stopwords), `src/nl/typo.rs` (dictionary), `src/nl/mod.rs` (op explanations), `src/nl/dialogue.rs` (display names)

---

## 10. Shell-Callable Racket Forms (Fact-Pack-Driven)

Shell-callable forms wrap macOS CLI tools (`ls`, `grep`, `find`, etc.) as
first-class Racket operations. Unlike manually-authored ops, they are
**discovered by the inference engine** from a single fact pack.

### Architecture

```
data/packs/facts/macos_cli_facts.yaml     ← Layer 1: CLI tool descriptions
        │
        ▼
data/packs/ops/racket_ops.yaml           ← Layer 2: 6 anchor ops (category: shell)
data/packs/facts/racket_facts.yaml         ← Layer 2: 12 shell entities
        │
        ▼
src/racket_strategy.rs         ← Inference phases 3 & 4
  Phase 3: type-symmetric discovery (6 non-anchor tools)
  Phase 4: submode discovery (45 variadic flag forms)
        │
        ▼
src/racket_executor.rs         ← Layer 3: Racket code generation
  shell-exec, shell-lines, shell-quote helpers
```

### Adding a New CLI Tool

**Step 1: Add the entity to `data/packs/facts/macos_cli_facts.yaml`**

```yaml
entities:
  - id: cli_mv
    name: "mv"
    description: "Move or rename files and directories"
```

**Step 2: Add claims for the entity**

Every CLI entity needs claims on these axes:

```yaml
claims:
  # 1. Category (always "shell")
  - entity: cli_mv
    axis: shell_category
    value: "shell"

  # 2. Type signature
  - entity: cli_mv
    axis: shell_type_signature
    value: "String, String -> List(String)"

  # 3. Output format class (pick one of 6)
  - entity: cli_mv
    axis: shell_output_format
    value: "text_lines"

  # 4. Type symmetry class (must match an anchor's class)
  - entity: cli_mv
    axis: shell_type_symmetry
    value: "shell_text_lines"
```

**Step 3: Add properties**

```yaml
properties:
  # Required: op_name, racket_symbol, base_command, category_name, type_symmetry_class
  - entity: cli_mv
    key: op_name
    value: "shell_mv"
  - entity: cli_mv
    key: racket_symbol
    value: "shell-mv"
  - entity: cli_mv
    key: base_command
    value: "mv"
  - entity: cli_mv
    key: category_name
    value: "shell"
  - entity: cli_mv
    key: type_symmetry_class
    value: "shell_text_lines"

  # Optional: submodes (flag variants)
  - entity: cli_mv
    key: submode_interactive
    value: "-i"
  - entity: cli_mv
    key: submode_verbose
    value: "-v"
  - entity: cli_mv
    key: submode_no_clobber
    value: "-n"
```

**Step 4: Add the entity to `data/packs/facts/racket_facts.yaml`** (if it's a new anchor)

If the tool introduces a **new output-format class**, it needs an anchor op
in `data/packs/ops/racket_ops.yaml` and an entity in `data/packs/facts/racket_facts.yaml`. If it
shares a class with an existing anchor (e.g., `shell_text_lines` like `ls`),
it will be **automatically discovered** via type-symmetric inference — no
anchor needed.

**Step 5: Done.** The inference engine discovers the tool and its submodes.
Run `cargo test` to verify.

### Output Format Classes

| Class                  | Anchor   | Return Type     | Tools                    |
|------------------------|----------|-----------------|--------------------------|
| `shell_text_lines`     | `shell_ls` | `List(String)` | ls, cat, head, tail, sort |
| `shell_tabular`        | `shell_ps` | `List(String)` | ps, df                   |
| `shell_tree`           | `shell_find` | `List(String)` | find                   |
| `shell_filtered_lines` | `shell_grep` | `List(String)` | grep                  |
| `shell_single_value`   | `shell_du` | `List(String)`  | du, wc                  |
| `shell_byte_stream`    | `shell_curl` | `List(String)` | curl                   |

All classes return `List(String)` (raw lines). Higher-level decomposition
uses existing Racket primitives (`racket_map`, `racket_filter`, etc.).

### Submodes

Submodes are **variadic flag forms** of a base op, discovered automatically
from `submode_*` properties in the CLI fact pack:

```
shell_ls + submode_long     → shell_ls_long     (flags: "-l")
shell_ls + submode_all      → shell_ls_all      (flags: "-a")
shell_grep + submode_recursive → shell_grep_recursive (flags: "-r")
```

Submodes inherit the parent's metasig and add flags to invariants. They are
**not** separate first-class ops — they are inferred at registry build time.

Current submode count: **45** across 12 tools.

### Generated Racket Code

Shell ops generate Racket code using three helper functions:

```racket
;; shell-exec: run command, return exit-code + stdout + stderr
(define (shell-exec cmd)
  (define-values (p out in err)
    (subprocess #f #f #f "/bin/sh" "-c" cmd))
  (define stdout (port->string out))
  (define stderr (port->string err))
  (subprocess-wait p)
  (values (subprocess-status p) stdout stderr))

;; shell-lines: run command, return stdout lines as list
(define (shell-lines cmd)
  (define-values (code stdout stderr) (shell-exec cmd))
  (if (= code 0)
      (filter (lambda (s) (not (string=? s "")))
              (string-split stdout "\n"))
      (error 'shell-lines (~a "command failed: " cmd "\n" stderr))))

;; shell-quote: escape a string for safe shell interpolation
(define (shell-quote s)
  (string-append "'" (string-replace s "'" "'\\''") "'"))
```

Example generated call for `shell_ls_long`:

```racket
(shell-lines (string-append "ls" " " "-l" " " (shell-quote path)))
```

### Inference Pipeline

The full inference pipeline runs 5 phases:

| Phase | Name              | What It Does                                    | Ops Added |
|-------|-------------------|-------------------------------------------------|-----------|
| 0     | Direct ops        | Load ops from YAML packs                        | 58        |
| 1     | Fact-pack claims  | Derive ops from fact pack claims                | 18        |
| 2     | Compositional     | Compose ops from existing ops                   | 12        |
| 3     | Type-symmetric    | Discover ops sharing type symmetry class         | 6 (shell) |
| 4     | Shell submodes    | Discover flag-variant ops from CLI fact pack     | 45        |

After all phases: **246 total ops** in the registry (including fs_ops and
power_tools_ops).

### Relationship to Existing Ops

Shell-callable forms **coexist** with `data/packs/ops/fs_ops.yaml` and `data/packs/ops/power_tools_ops.yaml`.
There are zero name collisions (`shell_ls` vs `list_dir`). See
[SUBSUMPTION.md](SUBSUMPTION.md) for the full mapping and migration roadmap.

**10 of 49 fs_ops** and **4 of 64 power_tools_ops** are subsumed today.
The remaining ops require additional CLI tool entities in the fact pack.

### Security: Shell Injection Prevention

All user-supplied arguments are wrapped with `shell-quote`, which uses
POSIX single-quote escaping (`'...'` with embedded quotes escaped as
`'\''`). This prevents shell injection attacks:

```racket
;; User input: "foo; rm -rf /"
;; Quoted:     "'foo; rm -rf /'"  ← treated as literal string by shell
```

### Reference

- **CLI fact pack**: `data/packs/facts/macos_cli_facts.yaml` (12 entities, 45 submodes)
- **Shell anchor ops**: `data/packs/ops/racket_ops.yaml` (6 ops with `category: shell`)
- **Shell entities**: `data/packs/facts/racket_facts.yaml` (12 entities with shell properties)
- **Submode discovery**: `src/racket_strategy.rs` (`discover_shell_submodes()`)
- **Racket generation**: `src/racket_executor.rs` (`shell_preamble()`, `generate_shell_call()`)
- **Tests**: `tests/shell_callable_tests.rs` (41 tests)
- **Subsumption plan**: `SUBSUMPTION.md`
