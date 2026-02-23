# Plan DSL Reference

> **Purpose**: Complete reference for the Cadmus Plan YAML DSL.  
> **Audience**: Developers writing plan definitions or extending the plan compiler.  
> **Last updated**: 2025-02-23  
> **Source of truth**: `src/plan.rs`, `data/plans/`

---

## Overview

A Plan is a declarative YAML pipeline that chains typed operations. The plan compiler type-checks the chain using unification, resolves parameters, and produces a `CompiledPlan` that the Racket executor can turn into an executable script.

```
YAML file → parse_plan() → PlanDef → compile_plan() → CompiledPlan → generate_racket_script()
```

## Syntax

### Basic Structure

The top-level YAML key is the plan name (kebab-case). It contains `inputs`, optional `bindings`, optional `output`, and `steps`:

```yaml
find-pdfs:
  inputs:
    - path
    - keyword
  steps:
    - list_dir
    - find_matching:
        pattern: "*.pdf"
    - sort_by: name
```

### Inputs

Inputs are typed parameters. Three forms:

```yaml
inputs:
  - path                    # bare name — type inferred from name
  - keyword: "Pattern"      # typed — explicit type annotation
  - n: "Number"             # typed — numeric input
```

**Type inference from names** (`infer_input_type()` in `src/plan.rs`):

| Input Name | Inferred Type |
|------------|---------------|
| `path`, `dir`, `folder`, `directory` | `Dir(Bytes)` |
| `file` | `File(Bytes)` |
| `url` | `URL` |
| `pattern`, `regex`, `query` | `Pattern` |
| `repo` | `Repo` |
| `n`, `x`, `y`, `count`, `a`, `b`, `m`, `left`, `right` | `Number` |
| `lst`, `elements`, `items` | `List(Number)` |
| Names with known file extensions (e.g., `input.csv`) | `File(<type>)` |
| Names starting with `http://`, `https://`, `ftp://` | `URL` |

**Explicit type annotations** are parsed by `parse_type_annotation()`:

| Annotation | Resolved Type |
|------------|---------------|
| `Dir` | `Dir(Bytes)` |
| `File` | `File(Bytes)` |
| `Number` | `Number` |
| `Pattern` | `Pattern` |
| `File(Archive(File(Image), Cbz))` | Parsed as-is via `TypeExpr::parse()` |
| `List(a)` | Parsed as-is |

### Bindings

Literal values for inputs, used at execution time:

```yaml
find-pdfs:
  inputs:
    - path
  bindings:
    path: "~/Documents"
  steps:
    - list_dir
```

Bindings are resolved by the `CallingFrame` at execution time. The NL pipeline populates bindings from extracted path literals.

### Output

Optional declared output type:

```yaml
find-pdfs:
  inputs:
    - path
  output: "Seq(Entry(Name, File(PDF)))"
  steps:
    - list_dir
    - find_matching:
        pattern: "*.pdf"
```

**FACT**: The output declaration is stored but not currently used for type-checking. The compiler infers the output type from the step chain.

### Steps

Steps are the operation pipeline. Four syntactic forms:

| Form | YAML | Meaning |
|------|------|---------|
| Bare op | `- walk_tree` | No arguments |
| Each mode | `- read_file: each` | Map mode — apply to each element |
| Scalar param | `- sort_by: name` | Single parameter |
| Named params | `- filter: { extension: ".pdf" }` | Key-value parameters |

### Parameter References

Steps can reference plan inputs with `$var` syntax:

```yaml
git-log-search:
  inputs:
    - repo
    - pattern
  steps:
    - git_log
    - filter:
        pattern: "$pattern"
```

`$var` references are expanded during compilation. Unknown `$var` names produce `PlanError::UnknownVar`.

### Step Back-References

Steps can reference the output of previous steps with `$step-N` syntax:

```yaml
steps:
  - walk_tree
  - filter:
      predicate: "$step-0"    # reference step 0's output
```

**FACT**: `$step-N` references are validated during compilation. Invalid indices produce `PlanError::InvalidStepRef`.

## Compilation Pipeline

### 1. Parse (`parse_plan`)

Reads YAML, extracts the plan name from the top-level key, and produces a `PlanDef`:

```rust
struct PlanDef {
    name: String,
    inputs: Vec<PlanInput>,
    output: Option<Vec<String>>,
    steps: Vec<RawStep>,
    bindings: HashMap<String, String>,
}
```

### 2. Resolve Input Type

For each input, resolve its type:
- If the input has a `type_hint`, parse it via `parse_type_annotation()`
- Otherwise, infer from the input name via `infer_input_type()`
- If neither works, produce `PlanError::UnknownInputType`

### 3. Bytes Promotion

If the input type contains `Bytes` (the "unknown content" primitive), the compiler attempts to promote it:

1. Replace every `Bytes` with a fresh type variable `_promote`
2. Simulate the type chain forward through each step's op signature
3. If `_promote` gets bound to something concrete (e.g., `File(Text)`), apply the substitution
4. If unbound, keep the original `Bytes`

**Example**: `Dir(Bytes)` with steps `[walk_tree, search_content]` promotes to `Dir(File(Text))` because `search_content` expects `Seq(Entry(Name, File(Text)))`.

### 4. Step Compilation

For each step:

1. **Look up op** in registry → `PlanError::UnknownOp` if not found
2. **Unify** step input type with op's input signature
3. **Expand `$var` references** from plan inputs
4. **Determine map mode** — `step_needs_map()` checks if the step's input is a `Seq(...)` but the op expects a single element
5. **Resolve archive format** — `resolve_archive_op()` rewrites generic `extract_archive`/`pack_archive` to format-specific ops (e.g., `extract_zip`) based on type information
6. **Set isolation flag** — `needs_isolation()` marks extract ops that need temp directories in map mode
7. **Compute output type** — apply unification substitution to op's output type

### 5. Produce CompiledPlan

```rust
struct CompiledPlan {
    name: String,
    input_type: TypeExpr,
    input_description: String,
    steps: Vec<CompiledStep>,
    output_type: TypeExpr,
}

struct CompiledStep {
    index: usize,
    op: String,
    input_type: TypeExpr,
    output_type: TypeExpr,
    params: HashMap<String, String>,
    isolate: bool,
}
```

## Map Mode (Each)

When a step's input is `Seq(A)` but the op expects `A`, the compiler detects this and the executor wraps the step in a map:

```yaml
steps:
  - walk_tree          # Dir(Bytes) → Seq(Entry(Name, Bytes))
  - read_file: each    # each Entry → read its content
```

The `step_needs_map()` function (`src/plan.rs`) compares the step's input type against the op's signature to determine if mapping is needed. The `each` keyword in YAML is a hint, but the compiler also infers map mode automatically.

In Racket codegen, map mode wraps the step in `(map (lambda (_line) ...) prev)`.

## Archive Format Resolution

Generic archive ops (`extract_archive`, `pack_archive`) are rewritten to format-specific ops based on type information:

1. `extract_archive_format()` extracts the format from the type (e.g., `File(Archive(Bytes, Cbz))` → `"Cbz"`)
2. `filetypes::dictionary().format_family()` maps format to family (e.g., `Cbz` → `zip`)
3. The op is rewritten (e.g., `extract_archive` → `extract_zip`)

Format-specific ops: `extract_zip`, `extract_tar`, `extract_tar_gz`, `extract_tar_bz2`, `extract_tar_xz`, `extract_rar`, `pack_zip`, `pack_tar`, `pack_tar_gz`.

## Isolation

Steps marked with `isolate: true` get filesystem isolation in map mode. Currently only `extract_*` ops trigger isolation. In Racket codegen, isolated steps create per-archive temp directories via `(make-temporary-directory)`.

## Error Types

```rust
enum PlanError {
    Parse(String),                              // YAML parse error
    UnknownOp { step, op },                     // Op not in registry
    TypeMismatch { step, op, expected, got },    // Unification failure
    UnknownVar { step, var_name },               // Unknown $var reference
    EmptySteps,                                  // No steps defined
    NoInputs,                                    // No inputs defined
    UnknownInputType { name },                   // Can't infer input type
    PlanFailed { step, op, reason },             // Planning failure
    InvalidStepRef { step, param, ref_step },    // Bad $step-N reference
}
```

## Algorithm Plans

The `data/plans/algorithms/` directory contains 108 algorithm templates across 14 categories:

| Category | Plans | Examples |
|----------|-------|---------|
| arithmetic | 11 | fibonacci, factorial, gcd |
| number-theory | 9 | prime_check, sieve_of_eratosthenes |
| encoding | 4 | base64_encode, caesar_cipher |
| sorting | 11 | bubble_sort, merge_sort, quicksort |
| searching | 12 | binary_search, bfs, dfs |
| graph | 10 | dijkstra, topological_sort |
| dynamic-programming | 10 | knapsack, longest_common_subsequence |
| string | 10 | palindrome, anagram_check |
| set | 6 | union, intersection |
| combinatorics | 8 | permutations, combinations |
| geometry | 6 | distance, convex_hull |
| matrix | 6 | matrix_multiply, determinant |
| statistics | 4 | mean, standard_deviation |
| bitwise | 4 | count_bits, power_of_two |

**FACT**: As of the latest commit, 44/108 compile and 41/108 execute successfully. The main bottleneck is `infer_input_type()` not recognizing all input names (e.g., `lst`, `graph`, `elements`, `coins`, `edges`, `points`).

## Examples

### Simple Filesystem Plan

```yaml
find-pdfs:
  inputs:
    - path
  steps:
    - list_dir
    - find_matching:
        pattern: "*.pdf"
    - sort_by: name
```

### Plan with Explicit Types and Bindings

```yaml
extract-cbz:
  inputs:
    - path: "File(Archive(File(Image), Cbz))"
  bindings:
    path: "~/Comics/issue1.cbz"
  steps:
    - extract_archive
```

### Multi-Step with Each Mode

```yaml
repack-comics:
  inputs:
    - path
  steps:
    - list_dir
    - find_matching:
        pattern: "*.cbz"
    - sort_by: name
    - extract_archive: each
    - pack_archive:
        output: "combined.cbz"
```

### Algorithm Plan with Iteration

```yaml
fibonacci:
  inputs:
    - n: "Number"
  bindings:
    n: "10"
  steps:
    - iterate:
        name: "loop"
        bindings: "a=0 b=1 i=0"
        body: "(if (= i n) a (loop b (+ a b) (+ i 1)))"
```

### Plan with Variable References

```yaml
git-log-search:
  inputs:
    - repo
    - pattern
  steps:
    - git_log
    - filter:
        pattern: "$pattern"
    - sort_by: date
```
