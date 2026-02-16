# Architecture

A pure-Rust reasoning engine that plans typed operations using unification,
algebraic properties, and strategy-pattern domain specialization. No LLM —
all derivation is mechanical.

## Core Idea

Operations have typed signatures (`inputs → output`). The planner works
backwards from a goal type, finding operations whose output unifies with
the goal, then recursively planning their inputs. Domain knowledge lives
in YAML data packs, not Rust code.

```
YAML ops pack  ──→  OperationRegistry  ──→  Planner  ──→  Plan tree
YAML fact pack ──→  FactPackIndex      ──→  (future)      ──→  DryRunTrace
```

## Type System

**`src/type_expr.rs`** (~930 lines) — the compositional type grammar.

```rust
enum TypeExpr {
    Primitive(String),              // Bytes, Path, Name, Text, Metadata, ...
    Constructor(String, Vec<TypeExpr>), // File(a), Seq(Entry(Name, a)), ...
    Var(String),                    // a, b, fmt — bound during unification
}
```

The grammar is **open** — new primitives and constructors are just strings.
No enum variants to add. Current vocabulary:

| Kind | Examples |
|------|----------|
| Primitives | `Path`, `Bytes`, `Name`, `Text`, `Line`, `Pattern`, `Metadata`, `Size`, `Timestamp`, `Permissions`, `FileType`, `Owner`, `Count`, `Hash`, `Diff`, `Unit`, `URL`, `App`, `Plist`, `XattrValue` |
| Constructors | `File(a)`, `Dir(a)`, `Seq(a)`, `Entry(k, v)`, `Archive(a, fmt)`, `Tree(a)`, `Match(p, v)`, `Option(a)`, `Symlink(a)` |

Key types and functions:

| Name | What |
|------|------|
| `TypeExpr::parse(s)` | Parse `"Seq(Entry(Name, File(Bytes)))"` → AST |
| `TypeExpr::prim/cons/var/seq/file/dir/entry/archive/tree/option` | Convenience constructors |
| `Substitution` | Map from variable names to bound types |
| `unify(left, right) → Result<Substitution, UnifyError>` | Structural unification with occurs check |

## Operation Registry

**`src/registry.rs`** (~1200 lines) — the central registry of typed operations.

Two parallel systems coexist:

### Monomorphic path (`TypeId`)

Used by the original comparison and coding strategies. Flat string types,
no unification.

```rust
struct TypeId(String);                    // "Claim", "Evidence", "AST"
struct OpSignature { inputs: Vec<TypeId>, output: TypeId }
struct OpEntry { name, signature, properties, rewrite_rules, exec: ExecFn }
```

- `register(name, signature, properties, exec_fn)` — register with execution binding
- `ops_for_output(type_id)` — lookup by output type (HashMap index)
- `get(name)` — lookup by name

### Polymorphic path (`TypeExpr`)

Used by filesystem and YAML-loaded ops. Supports type variables and
unification-based lookup.

```rust
struct PolyOpSignature { type_params, inputs: Vec<TypeExpr>, output: TypeExpr }
struct PolyOpEntry { name, signature, properties, description }
```

- `register_poly(name, signature, properties, description)` — register (no exec fn)
- `ops_for_output_expr(target)` — find all ops whose output unifies with target; returns `Vec<PolyOpMatch>` with substitutions and concrete input types
- `get_poly(name)` — lookup by name

### Dual registration

Strategies that need both exec bindings (for the old planner) and poly
signatures (for type-directed planning) register ops twice:

```rust
// In strategy.rs / coding_strategy.rs:
reg.register("compare", sig, props, exec_fn);        // monomorphic + exec
load_ops_pack_str_into(YAML, &mut reg)?;              // polymorphic from YAML
```

### Algebraic properties

Every operation declares properties used by the algebra layer:

```rust
struct AlgebraicProperties {
    commutative: bool,   // op(a,b) = op(b,a)
    associative: bool,   // op(op(a,b),c) = op(a,op(b,c))
    identity: Option<String>,   // op(x, e) = x
    absorbing: Option<String>,  // op(x, z) = z
    idempotent: bool,    // op(x, x) = x
}
```

## YAML Ops Packs

**`src/registry.rs`** (loader section) + **`data/*.yaml`**

Operations are defined as YAML data, loaded at runtime. Zero Rust
recompilation to add ops.

```yaml
# data/fs_ops.yaml
ops:
  - name: list_dir
    type_params: [a]
    inputs: ["Dir(a)"]
    output: "Seq(Entry(Name, a))"
    properties:
      idempotent: true
    description: "ls — list directory contents"
```

Serde structs: `OpsPack`, `OpDef`, `OpDefProperties`.

Loader functions:

| Function | What |
|----------|------|
| `load_ops_pack_str(yaml)` | Parse YAML → new `OperationRegistry` |
| `load_ops_pack(path)` | Read file + parse |
| `load_ops_pack_str_into(yaml, &mut reg)` | Parse and merge into existing registry |

Error type: `OpsPackError` — `Io`, `Yaml`, or `TypeParse { op_name, field, source, message }`.

Current packs:

| File | Ops | Domain |
|------|-----|--------|
| `data/fs_ops.yaml` | 49 | Filesystem (POSIX + macOS) |
| `data/comparison_ops.yaml` | 6 | Comparative reasoning |
| `data/coding_ops.yaml` | 6 | Code analysis |

## Fact Packs

**`src/fact_pack.rs`** (~290 lines) + **`data/macos_fs.yaml`**, **`data/putin_stalin.yaml`**

Domain knowledge as structured YAML. Currently loaded and indexed but
**not consulted during planning** (Phase 6 deferred).

```rust
struct FactPack { entities, axes, claims, evidence, properties, relations, uncertainties }
struct FactPackIndex { pack, claims_by_axis_entity, evidence_by_claim, ... }
```

- `load_fact_pack(path) → FactPackIndex` — load and build indexes
- Axes have optional `sub_axes` and `polarity`
- Claims link entity + axis + text
- Evidence links to a claim via `supports`
- Properties are per-entity, per-axis key-value pairs with optional ordinals
- Relations are tagged enums: `Hierarchy` or `Ordinal`

## Planners

**`src/generic_planner.rs`** (~1280 lines) — two planners in one file.

### Monomorphic planner (`plan`)

Type-directed backtracking search using `TypeId`:

```
plan(goal, registry) → PlanNode
```

- `GenericGoal` — target type + available literals + constraints (must-include ops, must-use inputs, ordering)
- `PlanNode` — tree of `Op { name, output, children }` and `Leaf { key, type }`
- Cycle detection, depth limiting, constraint validation

### Polymorphic planner (`plan_expr`)

Unification-based search using `TypeExpr`:

```
plan_expr(goal, registry) → ExprPlanNode
```

- `ExprGoal` — target TypeExpr + available `ExprLiteral`s
- `ExprPlanNode` — tree with four node types:
  - `Op` — direct operation application
  - `Leaf` — input literal
  - `Map` — element-wise: `Seq(A) → Seq(B)` via op `A → B`
  - `Fold` — reduction: `Seq(B) → B` via associative op `B + B → B`
- Automatic map/fold insertion when direct ops don't match

## Strategies

**`src/strategy.rs`** (~890 lines) — the `ReasonerStrategy` trait + `ComparisonStrategy`.

```rust
trait ReasonerStrategy {
    type Output;
    fn build_registry(&self) -> OperationRegistry;
    fn available_literals(&self) -> Vec<Literal>;
    fn goals(&self) -> Vec<GenericGoal>;
    fn execute_node(&self, node, children, registry) -> Result<Literal>;
    fn assemble(&self, results: Vec<Literal>) -> Result<Self::Output>;
}
```

The trait is the extension point. Each strategy:
1. Builds a registry with domain ops
2. Extracts literals from domain data (fact pack)
3. Defines goals (output types to produce)
4. Provides execution logic for each op
5. Assembles final output from plan results

### ComparisonStrategy (`src/strategy.rs`)

Compares entities across axes using fact pack data. Ops: `retrieve_claim`,
`retrieve_evidence`, `compare`, `find_similarity`, `summarize`, `assess_uncertainty`.
Uses the monomorphic planner. Loads `comparison_ops.yaml` for poly signatures.

### CodingStrategy (`src/coding_strategy.rs`, ~780 lines)

Analyzes source code. Ops: `parse_source`, `analyze_types`, `detect_smells`,
`plan_refactoring`, `generate_tests`, `compose`. Uses the monomorphic planner.
Loads `coding_ops.yaml` for poly signatures.

### FilesystemStrategy (`src/fs_strategy.rs`, ~400 lines)

Plans filesystem operations and produces a dry-run trace. Uses the
**polymorphic planner** exclusively. No exec bindings — ops are YAML-only.

```rust
struct FilesystemStrategy { registry: OperationRegistry }
fn dry_run(target, available) → DryRunTrace
```

`DryRunTrace` walks the plan tree in dependency order, producing `TraceStep`s
with step number, op name, input/output types, and command hint (from the
op's `description` field).

## Algebra

**`src/algebra.rs`** (~1030 lines) — plan canonicalization.

```
canonicalize(plan, registry) → PlanNode
```

Applies until fixpoint:
1. Flatten associative nodes
2. Sort commutative operands (canonical order)
3. Remove identity elements
4. Collapse absorbing elements
5. Deduplicate idempotent operands
6. Apply rewrite rules

Also: `InferenceRule` for deriving new facts, `infer()` for forward chaining.

## Theory

**`src/theory.rs`** (~1230 lines) — cross-entity reasoning.

Derives `OrdinalComparison`s and `Divergence`s by comparing per-entity
property values from the fact pack. Neither entity's data references the
other — the theory layer performs the cross-entity reasoning. Produces
`TheoryContext` consumed by the comparison strategy.

## Workflow DSL

**`src/workflow.rs`** (~1230 lines) — YAML workflow definitions.

```yaml
workflow: "Find PDFs"
inputs:
  path: "~/Documents"
steps:
  - walk_tree
  - filter:
      extension: ".pdf"
  - sort_by: name
```

Pipeline: parse → compile → execute.

| Phase | Function | What |
|-------|----------|------|
| Parse | `WorkflowDef` (serde) | YAML → `RawStep` list |
| Compile | `compile_workflow()` | Type-check pipeline, infer input types, resolve `$var` references |
| Execute | `execute_workflow()` | Run through `FilesystemStrategy::dry_run()` |

Step forms:
- Bare string: `walk_tree` → op with no args
- Scalar: `read_file: each` → map mode, `sort_by: name` → flag
- Map: `filter: { extension: ".pdf" }` → named params
- `$name` → expand from workflow inputs

## Module Map

| Module | Lines | Role |
|--------|-------|------|
| `type_expr` | 930 | Type grammar, parsing, unification |
| `registry` | 1200 | Operation registry, YAML ops loader |
| `generic_planner` | 1280 | Both planners (monomorphic + polymorphic) |
| `workflow` | 1230 | Workflow YAML DSL |
| `theory` | 1230 | Cross-entity theory derivation |
| `algebra` | 1030 | Plan canonicalization, rewrite rules |
| `strategy` | 890 | `ReasonerStrategy` trait + `ComparisonStrategy` |
| `coding_strategy` | 780 | `CodingStrategy` |
| `fs_strategy` | 400 | `FilesystemStrategy` + dry-run trace |
| `fact_pack` | 290 | Fact pack YAML schema + indexed loader |
| `fs_types` | 190 | `build_fs_registry()` — loads `fs_ops.yaml` |
| `types` | 310 | Core domain types (`Goal`, `Obligation`, `ReasoningOutput`) |
| `planner` | 350 | Legacy obligation-based planner (pre-strategy) |
| `pipeline` | 135 | Entry point — delegates to strategy |
| `main` | 360 | CLI: `--workflow` mode + strategy demos |
| `lib` | 14 | Module declarations |

## Data Files

```
data/
  fs_ops.yaml              49 filesystem ops (type signatures + properties)
  comparison_ops.yaml       6 comparison ops
  coding_ops.yaml           6 coding ops
  macos_fs.yaml            Fact pack: macOS tool knowledge (~40 claims)
  putin_stalin.yaml        Fact pack: political comparison domain
  tiramisu_cheesecake.yaml Fact pack: dessert comparison domain
  workflows/               9 workflow YAML examples
```

## Dependencies

- `serde`, `serde_yaml`, `serde_json` — serialization
- `thiserror` — error types
- No async, no LLM, no network calls
