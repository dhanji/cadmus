# Architecture

> **Purpose**: Detailed technical architecture of Cadmus for developers and maintainers.  
> **Audience**: Engineers reading the codebase, AI agents extending it.  
> **Last updated**: 2025-02-23  
> **Source of truth**: `src/` directory at commit `256e859`

---

## Overview

Cadmus is a type-directed reasoning engine that compiles operation pipelines into executable Racket programs. The core idea: operations have typed signatures (`inputs → output`), and the engine works backwards from a goal type to find operation chains that produce it. Domain knowledge lives in YAML data packs, not Rust code.

```
YAML ops pack  ──→  OperationRegistry  ──→  Planner  ──→  Plan tree
YAML fact pack ──→  FactPackIndex      ──→  Theory   ──→  Inferences
User input     ──→  NL Pipeline        ──→  PlanDef  ──→  CompiledPlan ──→ Racket script
```

## Type System (`src/type_expr.rs`, 978 lines)

The compositional type grammar is the foundation of everything:

```rust
enum TypeExpr {
    Primitive(String),                  // Bytes, Path, Name, Text, Number, ...
    Constructor(String, Vec<TypeExpr>), // File(a), Seq(Entry(Name, a)), ...
    Var(String),                        // a, b, fmt — bound during unification
}
```

The grammar is **open** — new primitives and constructors are just strings. No enum variants to add.

### Current Type Vocabulary

| Kind | Examples |
|------|----------|
| **Primitives** | `Bytes`, `Path`, `Name`, `Text`, `Line`, `Pattern`, `Metadata`, `Size`, `Timestamp`, `Permissions`, `FileType`, `Owner`, `Count`, `Hash`, `Diff`, `Unit`, `URL`, `App`, `Number`, `Boolean`, `String`, `Symbol` |
| **Constructors** | `File(a)`, `Dir(a)`, `Seq(a)`, `Entry(k, v)`, `Archive(a, fmt)`, `Tree(a)`, `Match(p, v)`, `Option(a)`, `Symlink(a)`, `List(a)`, `Pair(a, b)`, `Set(a)` |

### Key Functions

| Function | Signature | Purpose |
|----------|-----------|---------|
| `TypeExpr::parse(s)` | `&str → Result<TypeExpr>` | Parse `"Seq(Entry(Name, File(Bytes)))"` → AST |
| `unify(left, right)` | `(&TypeExpr, &TypeExpr) → Result<Substitution, UnifyError>` | Structural unification with occurs check |
| `Substitution::apply(ty)` | `(&TypeExpr) → TypeExpr` | Apply variable bindings to a type |
| `Substitution::compose(other)` | `(Substitution) → Substitution` | Compose two substitutions |

Convenience constructors: `TypeExpr::prim`, `cons`, `var`, `seq`, `file`, `dir`, `entry`, `archive`, `tree`, `option`.

### Bytes Promotion

`Bytes` is the type system's bottom for content — `Dir(Bytes)` means "directory with unknown content type." The plan compiler (`src/plan.rs`) promotes `Bytes` to concrete types when downstream operations need them:

1. Replace `Bytes` with a fresh type variable `_promote`
2. Simulate the type chain forward through each step's op signature
3. If `_promote` gets bound to something concrete (e.g., `File(Text)`), apply the substitution
4. If unbound, keep the original `Bytes`

This is implemented in `try_promote_bytes()` at `src/plan.rs:37`.

## Operation Registry (`src/registry.rs`, 1431 lines)

The central registry of typed operations. Two parallel systems coexist:

### Monomorphic Path (`TypeId`)

Used by the comparison and coding strategies. Flat string types, no unification.

```rust
struct TypeId(String);                    // "Claim", "Evidence", "AST"
struct OpSignature { inputs: Vec<TypeId>, output: TypeId }
struct OpEntry { name, signature, properties, rewrite_rules, exec: ExecFn }
```

- `register(name, signature, properties, exec_fn)` — register with execution binding
- `ops_for_output(type_id)` — lookup by output type (HashMap index)
- `get(name)` — lookup by name

### Polymorphic Path (`TypeExpr`)

Used by filesystem, Racket, and YAML-loaded ops. Supports type variables and unification-based lookup.

```rust
struct PolyOpSignature { type_params, inputs: Vec<TypeExpr>, output: TypeExpr }
struct PolyOpEntry { name, signature, properties, description, racket_symbol }
```

- `register_poly(name, signature, properties, description)` — register (no exec fn)
- `register_poly_with_meta(name, signature, properties, description, meta)` — register with metasignature
- `ops_for_output_expr(target)` — find all ops whose output unifies with target; returns `Vec<PolyOpMatch>` with substitutions and concrete input types
- `get_poly(name)` — lookup by name (uses `rfind` — last registration wins after promotion)

### MetaSignature

Operations can carry a `MetaSignature` with rich metadata used by the inference engine:

```rust
struct MetaSignature {
    params: Vec<MetaParam>,     // typed parameters
    return_type: String,        // return type expression
    invariants: Vec<String>,    // algebraic invariants
    category: String,           // domain category (e.g., "arithmetic", "shell")
    effects: Vec<String>,       // side effects (e.g., "io")
    type_params: Vec<String>,   // polymorphic type variables
}
```

### Algebraic Properties

Every operation declares properties used by the algebra layer:

```rust
struct AlgebraicProperties {
    commutative: bool,          // op(a,b) = op(b,a)
    associative: bool,          // op(op(a,b),c) = op(a,op(b,c))
    identity: Option<String>,   // op(x, e) = x
    absorbing: Option<String>,  // op(x, z) = z
    idempotent: bool,           // op(x, x) = x
}
```

### YAML Ops Pack Loader

Operations are defined as YAML data, loaded at runtime:

```yaml
ops:
  - name: list_dir
    type_params: [a]
    inputs: ["Dir(a)"]
    output: "Seq(Entry(Name, a))"
    properties:
      idempotent: true
    description: "ls — list directory contents"
    racket_symbol: "ls"         # optional: Racket identifier for codegen
```

Loader functions:

| Function | Purpose |
|----------|---------|
| `load_ops_pack_str(yaml)` | Parse YAML → new `OperationRegistry` |
| `load_ops_pack(path)` | Read file + parse |
| `load_ops_pack_str_into(yaml, &mut reg)` | Parse and merge into existing registry |

## Planners (`src/generic_planner.rs`, 1277 lines)

Two planners in one file, both using type-directed backtracking search.

### Monomorphic Planner (`plan`)

```
plan(goal: GenericGoal, registry: &OperationRegistry) → Result<PlanNode, PlanError>
```

- `GenericGoal` — target `TypeId` + available `Literal`s + constraints (must-include ops, must-use inputs, ordering)
- `PlanNode` — tree of `Op { name, output, children }` and `Leaf { key, type }`
- Cycle detection, depth limiting, constraint validation

### Polymorphic Planner (`plan_expr`)

```
plan_expr(goal: ExprGoal, registry: &OperationRegistry) → Result<ExprPlanNode, PlanError>
```

- `ExprGoal` — target `TypeExpr` + available `ExprLiteral`s
- `ExprPlanNode` — tree with four node types:
  - **Op** — direct operation application
  - **Leaf** — input literal
  - **Map** — element-wise: `Seq(A) → Seq(B)` via op `A → B`
  - **Fold** — reduction: `Seq(B) → B` via associative op `B + B → B`
- Automatic map/fold insertion when direct ops don't match

## Plan Compiler (`src/plan.rs`, 2000 lines)

The plan compiler is the heart of the pipeline. It takes a `PlanDef` (parsed from YAML) and produces a `CompiledPlan` with fully resolved types.

### Pipeline

```
YAML file → parse_plan() → PlanDef
                              ↓
                        compile_plan(def, registry) → CompiledPlan
                              ↓
                        execute_plan(compiled, registry) → DryRunTrace
```

### PlanDef Structure

```rust
struct PlanDef {
    name: String,                           // plan name (snake_case)
    inputs: Vec<PlanInput>,                 // typed input parameters
    steps: Vec<RawStep>,                    // operation pipeline
    bindings: HashMap<String, String>,      // literal input bindings
    output_type: Option<String>,            // declared output type
}
```

### Compilation Steps

1. **Parse** — YAML → `PlanDef` with `RawStep` list
2. **Resolve input type** — from explicit type hints or name-based inference
3. **Bytes promotion** — replace `Bytes` with concrete types via unification simulation
4. **Step compilation** — for each step:
   - Look up op in registry
   - Unify step input type with op's input signature
   - Resolve `$var` references from plan inputs
   - Determine if step needs map mode (element-wise processing)
   - Resolve archive format from type information
   - Compute output type
5. **Produce `CompiledPlan`** with typed step chain

### Step Forms

| YAML Syntax | Meaning |
|-------------|---------|
| `walk_tree` | Bare op, no args |
| `read_file: each` | Map mode — apply to each element |
| `sort_by: name` | Scalar parameter |
| `filter: { extension: ".pdf" }` | Named parameters |

### Type Inference for Inputs

`infer_input_type()` maps input names to types:

| Input Name | Inferred Type |
|------------|---------------|
| `path`, `dir`, `folder` | `Dir(Bytes)` |
| `file` | `File(Bytes)` |
| `url` | `URL` |
| `pattern`, `regex` | `Pattern` |
| `repo` | `Repo` |
| `n`, `x`, `y`, `count` | `Number` |
| Names with known file extensions | `File(<type>)` |

## Racket Executor (`src/racket_executor.rs`, 1910 lines)

Converts a `CompiledPlan` into a runnable Racket script.

### Architecture

```
CompiledPlan
    ↓
generate_racket_script(compiled, registry, bindings)
    ↓
    ├── Shell preamble (shell-exec, shell-lines, shell-quote helpers)
    ├── For each step: op_to_racket(op, params, prev_binding, ...)
    │     ├── Special ops (sort_list, format_string, racket_map, ...)
    │     ├── Shell ops (shell_ls, shell_find, shell_grep, ...)
    │     ├── Dual-behavior ops (sort_by, head, tail, count, unique)
    │     ├── Subsumed ops (walk_tree→shell_find, list_dir→shell_ls, ...)
    │     ├── Racket-native ops (filter→filter, sort_by→sort, ...)
    │     ├── Residual fs ops (pack_archive, extract_archive, ...)
    │     └── Data-driven ops (lookup racket_symbol + arity from registry)
    └── Script assembly (single expr or let* bindings)
```

### Dispatch Order in `op_to_racket()`

1. **Special ops** — 11 ops with complex parameter handling (sort_list, format_string, printf, racket_map, racket_filter, racket_foldl, racket_foldr, racket_for_each, racket_apply, andmap, ormap)
2. **Shell ops** — ops with `shell_` prefix, use `shell-exec`/`shell-lines` helpers
3. **Dual-behavior ops** — switch between shell and Racket-native based on pipeline position
4. **Subsumed ops** — fs_ops mapped to shell equivalents via `type_lowering.rs`
5. **Racket-native ops** — intermediate logic ops mapped to Racket primitives
6. **Residual fs ops** — ops not yet in the shell subsumption map
7. **Data-driven ops** — generic dispatch using `racket_symbol` and arity from registry

### Seq→String Bridge

When a shell-bridge step returns `List(String)` and the next step expects a single `String`, the executor inserts bridge code:

- **Residual exec ops** (pack_archive): `(string-join (map shell-quote prev) " ")`
- **Residual lines ops** (stat, diff): `(append-map (lambda (_f) (shell-lines ...)) prev)`
- **Subsumed binary ops** (search_content/grep): `(append-map (lambda (_f) (shell-lines (grep pattern _f))) prev)`

The `is_seq_output()` function (`src/racket_executor.rs:137`) determines if a step produces a sequence by checking the shell op's metasignature return type.

### Script Structure

**Single-step plans**: bare expression
```racket
#!/usr/bin/env racket
#lang racket
(displayln (shell-exec "ls" (shell-quote "/tmp")))
```

**Multi-step plans**: `let*` bindings
```racket
#!/usr/bin/env racket
#lang racket
(define (shell-exec cmd . args) ...)
(let* ([step-1 (shell-lines "find" (shell-quote path) "-type" "f")]
       [step-2 (filter (lambda (x) (regexp-match? #rx"\\.pdf$" x)) step-1)])
  (for-each displayln step-2))
```

## Type Lowering (`src/type_lowering.rs`, 608 lines)

Bridges the rich fs_ops type system and the flat shell-callable type system. Two tiers:

### Tier 1: Shell Bridges (World I/O)

Maps world-touching fs_ops to shell-callable equivalents:

| fs_op | shell_op | Arity | Note |
|-------|----------|-------|------|
| `walk_tree` | `shell_find` | 1 | `find <path> -type f` |
| `list_dir` | `shell_ls` | 1 | `ls <path>` |
| `read_file` | `shell_cat` | 1 | `cat <path>` |
| `search_content` | `shell_grep` | 2 | `grep <pattern> <path>` |
| `download` | `shell_curl` | 1 | `curl <url>` |
| `get_size` | `shell_du` | 1 | `du -sh <path>` |

Plus format-specific archive ops: `extract_zip`→`shell_unzip`, `pack_zip`→`shell_zip`, etc.

### Tier 2: Racket-Native (Intermediate Logic)

Maps in-memory transformation ops to Racket primitives:

| fs_op | Racket form | Note |
|-------|-------------|------|
| `filter` | `(filter pred lst)` | Racket-native only |
| `sort_by` | `(sort lst cmp)` | Dual-behavior |
| `count` | `(length lst)` | Dual-behavior |
| `unique` | `(remove-duplicates lst)` | Dual-behavior |
| `head` | `(take lst n)` | Dual-behavior |
| `tail` | `(drop lst n)` | Dual-behavior |

**Dual-behavior ops** switch between shell subprocess and Racket-native based on pipeline position.

### Key Functions

| Function | Purpose |
|----------|---------|
| `lookup_subsumption(fs_op)` | Find shell-callable equivalent |
| `lookup_racket_native(fs_op)` | Find Racket-native equivalent |
| `lookup_dual_behavior(fs_op)` | Find dual-behavior mapping |

## Inference Engine (`src/racket_strategy.rs`, 1438 lines)

The inference engine discovers operations from fact pack data through multi-phase reasoning.

### Five-Phase Pipeline

```
Phase 0: Discovery
  Scan fact pack for entities with op_name + racket_symbol not in registry
  Register placeholder ops
    ↓
Phase 1: Op-Symmetric
  For each stub op with symmetric_partner property:
    Copy partner's metasignature (invariants NOT transferred)
    Example: subtract ← add (symmetric_partner = "add")
    ↓
Phase 2: Type-Symmetric
  For each stub op in a type_symmetry_class:
    Find an anchor in the same class with a metasignature
    Copy the anchor's type signature
    Example: multiply ← add (both in class "binop")
    ↓
Phase 3: Op-Symmetric Replay
  Re-run Phase 1 to catch ops whose partners were discovered in Phase 2
    Example: divide ← multiply (multiply was discovered in Phase 2)
    ↓
Phase 4: Shell Submodes
  Read submode_* properties from CLI fact pack
  Create variadic form ops for each tool's flag variants
    Example: shell_ls_la, shell_find_name, shell_grep_r
```

### Current Discoveries

- **5 anchors**: add (arithmetic), cons (list), cdr (list), less_than (comparison), string_upcase (string)
- **9 discovered ops**: subtract, multiply, divide, remove, list_reverse, greater_than, less_than_or_equal, greater_than_or_equal, string_downcase

### Key Functions

| Function | Purpose |
|----------|---------|
| `discover_ops(registry, facts)` | Phase 0: scan fact pack for new ops |
| `promote_inferred_ops(registry, facts)` | Phases 1-3: symmetric inference |
| `discover_shell_submodes(registry, facts, cli_facts)` | Phase 4: CLI submodes |
| `infer_symmetric_op(registry, facts, op_name)` | Single op-symmetric inference |
| `infer_type_symmetric_op(registry, facts, op_name)` | Single type-symmetric inference |
| `meta_to_poly_signature(meta)` | Convert MetaSignature to PolyOpSignature |

## Strategies

### ReasonerStrategy Trait (`src/strategy.rs`)

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

### ComparisonStrategy (`src/strategy.rs`, ~500 lines)

Compares entities across axes using fact pack data. Ops: `retrieve_claim`, `retrieve_evidence`, `compare`, `find_similarity`, `summarize`, `assess_uncertainty`. Uses the monomorphic planner. Produces `ReasoningOutput` with per-axis results.

### CodingStrategy (`src/coding_strategy.rs`, 779 lines)

Analyzes source code. Ops: `parse_source`, `analyze_types`, `detect_smells`, `plan_refactoring`, `generate_tests`, `compose`. Uses the monomorphic planner. **Known issue**: `assemble()` only returns root node result; intermediate results lost.

### FilesystemStrategy (`src/fs_strategy.rs`, 404 lines)

Plans filesystem operations using the polymorphic planner. Produces `DryRunTrace` — no actual execution, just a plan trace with command hints.

## Theory Layer (`src/theory.rs`, 1220 lines)

Cross-entity reasoning from fact pack data. Derives:

- **OrdinalComparison** — comparing ordinal property values across entities
- **Divergence** — qualitatively different property values on the same axis
- **DerivedUncertainty** — four variants:
  - `EvidenceGap` — claims without supporting evidence
  - `OrdinalBoundary` — adjacent ordinal values (gap=0 or gap=1)
  - `CrossAxisTension` — opposing polarity on related axes
  - `PropertyClaimMismatch` — property values inconsistent with claims

Produces `TheoryContext` consumed by the comparison strategy.

## Algebra Layer (`src/algebra.rs`, 1026 lines)

Plan canonicalization via algebraic rewriting:

1. Flatten associative nodes
2. Sort commutative operands (canonical order)
3. Remove identity elements
4. Collapse absorbing elements
5. Deduplicate idempotent operands
6. Apply rewrite rules to fixpoint

Also provides `infer()` for forward chaining with `InferenceRule` (Transitive, Symmetric, Reflexive).

## Calling Frame (`src/calling_frame.rs`, 429 lines)

Orchestrates the full compile → codegen → execute pipeline:

```rust
trait CallingFrame {
    fn resolve_input(&self, name: &str) -> String;
    fn has_binding(&self, name: &str) -> bool;
    fn bindings(&self) -> &HashMap<String, String>;
    fn codegen(&self, plan: &PlanDef) -> Result<String, InvokeError>;
    fn invoke(&self, plan: &PlanDef) -> Result<Execution, InvokeError>;
    fn run_script(&self, script: &str) -> Result<Execution, InvokeError>;
}
```

`DefaultFrame` is the only implementation. It resolves inputs from literal bindings on the `PlanDef`, writes Racket scripts to temp files, and executes via `racket <tempfile>`.

### Execution Result

```rust
struct Execution {
    script: String,     // the generated Racket script
    stdout: String,     // process stdout
    stderr: String,     // process stderr
    success: bool,      // exit code == 0
    exit_code: Option<i32>,
}
```

## Registry Builder (`src/fs_types.rs`, 298 lines)

Two registry builders:

- `build_fs_registry()` — filesystem ops only (from embedded `fs.ops.yaml`)
- `build_full_registry()` — all ops: fs + power_tools + racket + inference + shell submodes

`build_full_registry()` is the standard path. It:
1. Loads embedded fs.ops.yaml (compatibility aliases)
2. Merges power_tools.ops.yaml
3. Merges racket.ops.yaml (disk-first, embedded fallback)
4. Runs `promote_inferred_ops()` (Phases 0-3)
5. Runs `discover_shell_submodes()` (Phase 4)

Result: ~246 ops in the registry.

## File Type Dictionary (`src/filetypes.rs`, 598 lines)

Loaded from `data/filetypes.yaml` via `OnceLock` singleton. 197 entries across 14 categories. Provides:

- `lookup(ext)` — get file type info by extension
- `lookup_by_path(path)` — extract extension and look up
- `is_known_extension(ext)` — check if extension is known
- `extensions_for_category(cat)` — all extensions in a category
- `format_family(format)` — archive format family (e.g., Cbz → zip)

Used by the plan compiler, NL layer, and type inference to eliminate hardcoded extension lists.

## Fact Pack System (`src/fact_pack.rs`, 780 lines)

### Compact Properties Format

```yaml
compact_properties:
  entity_name:
    axis_name:
      property_key: "simple_value"
      property_key_2:
        value: "extended_value"
        ordinal: 2
        note: "Explanation"
```

Properties are expanded into flat `Vec<Property>` at load time. All downstream code sees the same `Property { entity, axis, key, value, ordinal?, note? }` structs.

### FactPack Structure

```rust
struct FactPack { entities, axes, claims, evidence, properties, relations, uncertainties }
struct FactPackIndex { pack, claims_by_axis_entity, evidence_by_claim, ... }
```

### Merge Support

`FactPack::merge()` combines two packs with dedup on entities, axes, and relations. `FactPack::merge_all()` merges a vector. Used for multi-fact-pack goals.

## NL Pipeline (`src/nl/`, 9435 lines)

See [NL Pipeline documentation](nl-pipeline.md) for detailed coverage.

### Summary

A deterministic, low-latency adapter that converts natural language into structured `PlanDef` YAML. The pipeline:

1. **Normalize** — case fold, punctuation strip, contraction expansion, synonym mapping
2. **Typo correct** — SymSpell with ~2473-word domain dictionary
3. **Re-normalize** — apply synonyms to corrected tokens
4. **Intent dispatch** — approve/reject/explain/edit via keyword match
5. **Earley parse** — grammar-based command parsing (additive — falls back on failure)
6. **Intent IR** — parse tree → structured intent
7. **Intent compile** — IntentIR → PlanDef
8. **Fallback** — old intent/slots pipeline for unrecognized input

## Dependencies

| Crate | Version | Purpose |
|-------|---------|---------|
| `serde` | 1 | Serialization (with `derive` feature) |
| `serde_yaml` | 0.9 | YAML parsing |
| `serde_json` | 1 | JSON serialization |
| `thiserror` | 2 | Error type derivation |
| `rustyline` | 15 | Readline-style line editing |

No async runtime, no LLM, no network calls (except via generated Racket scripts).
