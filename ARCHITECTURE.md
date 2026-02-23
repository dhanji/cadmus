# Architecture

A pure-Rust engine (Cadmus) that plans typed operations using unification,
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
# data/fs.ops.yaml
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
| `data/fs.ops.yaml` | 49 | Filesystem (POSIX + macOS) |
| `data/comparison.ops.yaml` | 6 | Comparative reasoning |
| `data/coding.ops.yaml` | 6 | Code analysis |

## Fact Packs

**`src/fact_pack.rs`** (~350 lines) + **`data/packs/facts/`**

Domain knowledge as structured YAML. Currently loaded and indexed but
**not consulted during planning** (Phase 6 deferred).

```yaml
# Compact property format — entity → axis → key → value
compact_properties:
  putin:
    coercion:
      repression_scale:          # extended value (ordinal + note)
        value: targeted
        ordinal: 2
        note: "Selective targeting..."
    legitimacy:
      legitimacy_basis: electoral_managed   # simple string value
```

Properties are expanded into flat `Vec<Property>` at load time. All
downstream code (FactPackIndex, theory, strategies) sees the same
`Property { entity, axis, key, value, ordinal?, note? }` structs.

```rust
struct FactPack { entities, axes, claims, evidence, properties, relations, uncertainties }
struct FactPackIndex { pack, claims_by_axis_entity, evidence_by_claim, properties_by_axis_key_entity, ... }
```

- `load_fact_pack(path) → FactPackIndex` — load and build indexes
- Axes have optional `sub_axes` and `polarity`
- Claims link entity + axis + text
- Evidence links to a claim via `supports`
- Properties are per-entity, per-axis key-value pairs with optional ordinals and notes
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
Uses the monomorphic planner. Loads `comparison.ops.yaml` for poly signatures.

### CodingStrategy (`src/coding_strategy.rs`, ~780 lines)

Analyzes source code. Ops: `parse_source`, `analyze_types`, `detect_smells`,
`plan_refactoring`, `generate_tests`, `compose`. Uses the monomorphic planner.
Loads `coding.ops.yaml` for poly signatures.

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

## Plan DSL

**`src/plan.rs`** (~1230 lines) — YAML plan definitions.

```yaml
find-pdfs:
  inputs:
    - path
    - pattern
  steps:
    - walk_tree
    - filter:
        extension: ".pdf"
    - sort_by: name
```

Pipeline: parse → compile → execute.

| Phase | Function | What |
|-------|----------|------|
| Parse | `PlanDef` (serde) | YAML → `RawStep` list |
| Compile | `compile_plan()` | Type-check pipeline, infer input types, resolve `$var` references |
| Execute | `execute_plan()` | Run through `FilesystemStrategy::dry_run()` |

Step forms:
- Bare string: `walk_tree` → op with no args
- Scalar: `read_file: each` → map mode, `sort_by: name` → flag
- Map: `filter: { extension: ".pdf" }` → named params
- `$name` → expand from plan inputs

## Natural Language UX Layer

**`src/nl/`** (~9000 lines across 12 modules) — a deterministic, low-latency
adapter that converts chatty user input into structured plan YAML.

### Pipeline

```
User input
  │
  ▼
normalize → typo_correct → re-normalize (synonym mapping)
  │
  ▼
intent::parse_intent()  ← keyword/pattern match
  │
  ├─ Approve/Reject/Explain/Edit → handled directly
  │
  ├─ CreatePlan / NeedsClarification
  │     │
  │     ▼
  │   Earley parser (grammar.rs + earley.rs + lexicon.rs)
  │     │
  │     ▼
  │   Intent IR (intent_ir.rs) — parse tree → structured intent
  │     │
  │     ▼
  │   Intent Compiler (intent_compiler.rs) — IntentIR → PlanDef
  │     │
  │     ├─ Success → PlanCreated (YAML)
  │     └─ Failure → fall back to old intent/slots pipeline
  │
  └─ Fallback: old pipeline (intent.rs + slots.rs + dialogue.rs)
```

The Earley parser is **additive** — when it can't parse the input, the
system falls back to the original intent/slots pipeline transparently.

### Earley Parser (`earley.rs`, `grammar.rs`, `lexicon.rs`)

A from-scratch Earley parser (~530 lines) with predict/scan/complete and
parse forest via back-pointers. The grammar has 30+ rules for
`Command → Verb Object Modifiers` patterns.

- **Lexicon** (`data/nl/nl_lexicon.yaml`): YAML-driven word classification
  (verbs, nouns, path_nouns, orderings, prepositions, determiners, fillers)
- **Grammar** (`grammar.rs`): Builds the Earley grammar from lexicon categories
- **Parser** (`earley.rs`): Produces ranked parse trees (highest score first)

### Intent IR (`intent_ir.rs`)

Structured intent representation produced from Earley parse trees:

```rust
struct IntentIR {
    output: String,              // e.g. "Collection<File>"
    inputs: Vec<IRInput>,        // named inputs with selectors
    steps: Vec<IRStep>,          // abstract processing steps
    constraints: Vec<String>,    // result constraints
    acceptance: Vec<String>,     // acceptance criteria
    score: f64,                  // parse confidence
}

struct IRStep {
    action: String,              // "select", "order", "compress", etc.
    input_refs: Vec<String>,     // input entity IDs
    output_ref: String,          // output entity ID
    params: HashMap<String, String>,  // action parameters
}
```

`parse_trees_to_intents()` converts ranked parse trees into a primary
IntentIR plus alternatives. Multiple candidate parses are preserved for
future disambiguation.

### Intent Compiler (`intent_compiler.rs`)

Maps abstract actions to concrete PlanDef steps:

| Abstract Action | Plan Steps |
|----------------|------------|
| `select` | `walk_tree` + `find_matching` |
| `compress` | `walk_tree` + `pack_archive` (dir) or `gzip_compress` (file) |
| `decompress` | `extract_archive` |
| `order` | `sort_by` |
| `search_text` | `walk_tree` + `search_content` |
| `list` | `list_dir` |

Concept labels (e.g. `comic_issue_archive`) are resolved to file patterns
(`*.cbz`, `*.cbr`) via `noun_patterns` in the vocabulary.

### Dialogue State (`dialogue.rs`)

Multi-turn conversation management:

```rust
struct DialogueState {
    current_plan: Option<PlanDef>,
    alternative_intents: Vec<IntentIR>,  // from Earley parser
    focus: FocusStack,                    // anaphora resolution
    turn_count: usize,
    last_intent: Option<Intent>,
}
```

Supports create → edit → approve/reject cycles. Edits use pattern matching
(not the Earley parser). Approve/reject/explain use keyword matching.

### NL Module Map

| Module | Lines | Role |
|--------|-------|------|
| `mod.rs` | 811 | Entry point (`process_input`), Earley integration |
| `dialogue.rs` | 1620 | `DialogueState`, `build_plan`, `apply_edit`, `plan_to_yaml` |
| `intent.rs` | 1082 | Old intent recognition (keyword/pattern match) |
| `slots.rs` | 926 | Slot extraction (targets, anchors, modifiers) |
| `earley.rs` | 788 | Earley parser engine |
| `intent_ir.rs` | 671 | Intent IR schema + parse tree conversion |
| `normalize.rs` | 671 | Tokenization, case folding, synonym mapping |
| `typo.rs` | 612 | SymSpell-based typo correction |
| `intent_compiler.rs` | 593 | IntentIR → PlanDef compilation |
| `lexicon.rs` | 519 | YAML lexicon loader + token classifier |
| `grammar.rs` | 360 | Earley grammar builder |
| `vocab.rs` | 340 | Vocabulary YAML loader |

## Module Map

| Module | Lines | Role |
|--------|-------|------|
| `type_expr` | 930 | Type grammar, parsing, unification |
| `registry` | 1200 | Operation registry, YAML ops loader |
| `generic_planner` | 1280 | Both planners (monomorphic + polymorphic) |
| `plan` | 1230 | Plan YAML DSL |
| `theory` | 1230 | Cross-entity theory derivation |
| `algebra` | 1030 | Plan canonicalization, rewrite rules |
| `strategy` | 890 | `ReasonerStrategy` trait + `ComparisonStrategy` |
| `coding_strategy` | 780 | `CodingStrategy` |
| `fs_strategy` | 400 | `FilesystemStrategy` + dry-run trace |
| `fact_pack` | 290 | Fact pack YAML schema + indexed loader |
| `nl/` | 9000 | Natural language UX layer (12 modules, see NL section above) |
| `fs_types` | 190 | `build_fs_registry()` — loads `fs.ops.yaml` |
| `types` | 310 | Core domain types (`Goal`, `Obligation`, `ReasoningOutput`) |
| `planner` | 350 | Legacy obligation-based planner (pre-strategy) |
| `pipeline` | 135 | Entry point — delegates to strategy |
| `main` | 360 | CLI: `--plan` mode + strategy demos |
| `lib` | 14 | Module declarations |

## Data Files

```
data/
  fs.ops.yaml              49 filesystem ops (type signatures + properties)
  comparison.ops.yaml       6 comparison ops
  coding.ops.yaml           6 coding ops
  nl/nl_lexicon.yaml       Earley parser lexicon (verbs, nouns, orderings, etc.)
  nl/nl_vocab.yaml         NL synonyms, contractions, approvals, rejections
  macos_fs.facts.yaml            Fact pack: macOS tool knowledge (~40 claims)
  putin_stalin.facts.yaml        Fact pack: political comparison domain
  tiramisu_cheesecake.yaml Fact pack: dessert comparison domain
  plans/               9 plan YAML examples
```

## Dependencies

- `serde`, `serde_yaml`, `serde_json` — serialization
- `thiserror` — error types
- No async, no LLM, no network calls

## Plan DSL: Structured Sub-Steps

The plan DSL supports **nested sub-step lists** as first-class step parameters,
enabling Turing-complete computation without embedded Racket code. Control flow
constructs (`map`, `for_each`, `fold`, `cond`, `for_range`) take sub-step
pipelines as their bodies, not string expressions.

### Design Principles

1. **Every operation is a first-class step** — no string expressions, no
   embedded Racket in any parameter.
2. **Sub-step pipelines** use `$body-N` references (1-indexed) to refer to
   results of earlier steps within the same body.
3. **Outer scope** is accessible via `$var` (plan inputs) and `$step-N`
   (top-level pipeline steps).
4. **Loop variables** are accessible via `$var_name` within their body scope.
5. **The last step** in a sub-pipeline is its return value.

### New Step Types

#### `map` — Transform Each Element

Applies a sub-step pipeline to each element of a sequence. Returns a new
sequence of the same length.

```yaml
- map:
    var: "c"                    # loop variable name
    over: "$step-1"            # sequence to iterate (or inline step)
    body:                       # sub-step pipeline
      - char_to_integer: "$c"
      - add: { x: "$body-1", y: "1" }
```

Generates: `(map (lambda (c) (let* ([body-1 (char->integer c)] [body-2 (+ body-1 1)]) body-2)) step-1)`

#### `for_each` — Side Effects Over Sequence

Like `map` but for side effects (mutation). Returns void.

```yaml
- for_each:
    var: "x"
    over: "$coins"
    body:
      - for_range:
          var: "a"
          start: "$x"
          end: { add: { x: "$amount", y: "1" } }
          body:
            - vector_ref: { v: "$dp", i: { subtract: { x: "$a", y: "$x" } } }
            - add: { x: "$body-1", y: "1" }
            - vector_ref: { v: "$dp", i: "$a" }
            - min: { x: "$body-3", y: "$body-2" }
            - vector_set: { v: "$dp", i: "$a", val: "$body-4" }
```

Generates: `(for-each (lambda (x) ...) coins)`

#### `fold` — Accumulate Over Sequence

Reduces a sequence to a single value using an accumulator.

```yaml
- fold:
    acc: "result"               # accumulator variable name
    init: "1"                   # initial accumulator value
    var: "i"                    # element variable name
    over: "$step-1"            # sequence to fold over
    body:                       # sub-step pipeline (last step = new acc)
      - subtract: { x: "$n", y: "$i" }
      - add: { x: "$body-1", y: "1" }
      - multiply: { x: "$result", y: "$body-2" }
```

Generates: `(for/fold ([result 1]) ([i (in-list step-1)]) (let* (...) body-3))`

#### `cond` — Switch/Case

Multi-branch conditional. Each clause has a `test` (a step or inline op)
and a `then` (a value, `$`-reference, or sub-step pipeline). The last
clause can be `else`.

```yaml
- cond:
    clauses:
      - test: { char_alphabetic: "$c" }
        then:
          - cond:
              clauses:
                - test: { char_upper_case: "$c" }
                  then: "65"
                - else: "97"
          - char_to_integer: "$c"
          - subtract: { x: "$body-2", y: "$body-1" }
          - add: { x: "$body-3", y: "$k" }
          - modulo: { x: "$body-4", y: "26" }
          - add: { x: "$body-1", y: "$body-5" }
          - integer_to_char: "$body-6"
      - else: "$c"
```

Generates: `(cond [(char-alphabetic? c) (let* (...) body-7)] [else c])`

#### `for_range` — Iterate Over Integer Range

Iterates a sub-step pipeline over an integer range. Used for side effects
(with `vector_set`) or as a sequence generator.

```yaml
- for_range:
    var: "i"
    start: "1"
    end: { add: { x: "$n", y: "1" } }
    body:
      - vector_ref: { v: "$dp", i: "$i" }
      - add: { x: "$body-1", y: "1" }
      - vector_set: { v: "$dp", i: "$i", val: "$body-2" }
```

Generates: `(for ([i (in-range 1 (+ n 1))]) (let* (...) body-3))`

When used as a value (not for side effects), use `for_list` variant:

```yaml
- for_list:
    var: "i"
    start: "0"
    end: "$n"
    body:
      - bitwise_xor: { x: "$i", y: { arithmetic_shift: { x: "$i", y: "-1" } } }
```

Generates: `(for/list ([i (in-range 0 n)]) (bitwise-xor i (arithmetic-shift i -1)))`

#### Inline Steps

Any parameter that expects a value can be an **inline step** (a single-key
map that names an op). This avoids needing a separate step + `$body-N`
reference for simple expressions:

```yaml
# Instead of:
- add: { x: "$n", y: "1" }
- range: { start: "1", end: "$step-1" }

# Inline:
- range:
    start: "1"
    end: { add: { x: "$n", y: "1" } }
```

Inline steps are syntactic sugar — the codegen inlines them as nested
Racket expressions without intermediate `let*` bindings.

### Scope Rules

| Reference | Resolves to |
|-----------|-------------|
| `$var_name` | Plan input, loop variable, or accumulator in scope |
| `$step-N` | Result of top-level pipeline step N |
| `$body-N` | Result of sub-step N within current body |

Shadowing: loop variables shadow plan inputs of the same name within
their body scope.

### Examples

#### Factorial (SIMPLE — for_product over range)

```yaml
factorial:
  inputs:
    - n: "Number"
  bindings:
    n: "10"
  steps:
    - range:
        start: "1"
        end: { add: { x: "$n", y: "1" } }
    - for_product:
        var: "i"
        over: "$step-1"
```

#### Euler Totient (FOR_SUM with cond)

```yaml
euler_totient:
  inputs:
    - n: "Number"
  bindings:
    n: "12"
  steps:
    - range:
        start: "1"
        end: "$n"
    - for_sum:
        var: "i"
        over: "$step-1"
        body:
          - gcd: { x: "$i", y: "$n" }
          - cond:
              clauses:
                - test: { equal: { x: "$body-1", y: "1" } }
                  then: "1"
                - else: "0"
```

#### Caesar Cipher (map with nested cond)

```yaml
caesar_cipher:
  inputs:
    - s: "String"
    - k: "Number"
  bindings:
    s: "\"HELLO\""
    k: "3"
  steps:
    - string_to_list: "$s"
    - map:
        var: "c"
        over: "$step-1"
        body:
          - cond:
              clauses:
                - test: { char_alphabetic: "$c" }
                  then:
                    - cond:
                        clauses:
                          - test: { char_upper_case: "$c" }
                            then: "65"
                          - else: "97"
                    - char_to_integer: "$c"
                    - subtract: { x: "$body-2", y: "$body-1" }
                    - add: { x: "$body-3", y: "$k" }
                    - modulo: { x: "$body-4", y: "26" }
                    - add: { x: "$body-1", y: "$body-5" }
                    - integer_to_char: "$body-6"
                - else: "$c"
    - list_to_string: "$step-2"
```

#### Counting Sort (for_each with mutation)

```yaml
counting_sort:
  inputs:
    - lst: "List(Number)"
  bindings:
    lst: "(list 4 2 2 8 3 3 1)"
  steps:
    - apply_max: "$lst"
    - add: { x: "$step-1", y: "1" }
    - make_vector: { size: "$step-2", init: "0" }
    - for_each:
        var: "x"
        over: "$lst"
        body:
          - vector_ref: { v: "$step-3", i: "$x" }
          - add: { x: "$body-1", y: "1" }
          - vector_set: { v: "$step-3", i: "$x", val: "$body-2" }
    - for_list_star:
        vars:
          - { var: "i", start: "0", end: "$step-2" }
          - { var: "_", start: "0", end: { vector_ref: { v: "$step-3", i: "$i" } } }
        body:
          - identity: "$i"
```

#### 0/1 Knapsack (nested for_range with mutation)

```yaml
zero_one_knapsack:
  inputs:
    - capacity: "Number"
  bindings:
    capacity: "7"
  steps:
    - vector_new: "1 3 4 5"
    - vector_new: "1 4 5 7"
    - vector_length: "$step-1"
    - add: { x: "$capacity", y: "1" }
    - make_vector: { size: "$step-4", init: "0" }
    - for_range:
        var: "i"
        start: "0"
        end: "$step-3"
        body:
          - for_range_down:
              var: "w"
              start: "$capacity"
              end: { subtract: { x: { vector_ref: { v: "$step-1", i: "$i" } }, y: "1" } }
              body:
                - vector_ref: { v: "$step-5", i: "$w" }
                - subtract: { x: "$w", y: { vector_ref: { v: "$step-1", i: "$i" } } }
                - vector_ref: { v: "$step-5", i: "$body-2" }
                - add: { x: "$body-3", y: { vector_ref: { v: "$step-2", i: "$i" } } }
                - max: { x: "$body-1", y: "$body-4" }
                - vector_set: { v: "$step-5", i: "$w", val: "$body-5" }
    - vector_ref: { v: "$step-5", i: "$capacity" }
```

#### Bellman-Ford (for_each over list with mutation)

```yaml
bellman_ford:
  inputs:
    - n: "Number"
  bindings:
    n: "5"
  steps:
    - list_new: "(list (list 0 1 4) (list 0 2 1) (list 1 3 1) (list 2 1 -2) (list 2 3 5) (list 3 4 3))"
    - make_vector: { size: "$n", init: "+inf.0" }
    - vector_set: { v: "$step-2", i: "0", val: "0" }
    - for_range:
        var: "_"
        start: "0"
        end: { subtract: { x: "$n", y: "1" } }
        body:
          - for_each:
              var: "e"
              over: "$step-1"
              body:
                - first_elem: "$e"
                - second_elem: "$e"
                - third_elem: "$e"
                - vector_ref: { v: "$step-2", i: "$body-1" }
                - add: { x: "$body-4", y: "$body-3" }
                - vector_ref: { v: "$step-2", i: "$body-2" }
                - cond:
                    clauses:
                      - test: { less_than: { x: "$body-5", y: "$body-6" } }
                        then:
                          - vector_set: { v: "$step-2", i: "$body-2", val: "$body-5" }
                      - else: "0"
    - vector_to_list: "$step-2"
```

### Deferred: Recursive Plans (55 plans)

Plans requiring general recursion (iterate/named-let, recursive lambda)
retain their current string-expression form. These will be addressed in
a future phase with a `recur` step type for named recursive functions.
