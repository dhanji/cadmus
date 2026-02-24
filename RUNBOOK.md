# Cadmus Continuous Improvement Runbook

> **Purpose**: Machine-executable guide for an AI agent to add a new domain to Cadmus.
> Read this document, pick a domain, follow the phases, run the validation gates.
> No human intervention required.

---

## Prerequisites

- Rust toolchain (`cargo build` succeeds)
- Racket installed (`racket --version` works)
- All existing tests pass: `cargo test` (~1500 tests, 0 failures)

---

## Phase 0: Domain Selection

### 0.1 Read the domain registry

```bash
cat data/domains.yaml
```

The `suggested_next` section lists candidate domains. Pick one that:
- Does NOT overlap with any existing `categories` entry
- Has clear Racket-expressible semantics (pure functions, no I/O required)
- Can produce 8–12 plans with ≥70% being multi-step (composed from primitives)

### 0.2 Verify no overlap

Check the existing ops packs for name collisions:

```bash
rg '  - name:' data/packs/ops/*.yaml | rg '<your_op_name>'
```

Every op name must be globally unique across ALL ops packs. The registry silently
shadows on collision (last registration wins via `rfind`).

### 0.3 Create the category directory

```bash
mkdir -p data/plans/algorithms/<your-domain>/
```

Use kebab-case for the directory name (e.g., `text-processing`, `set-theory`).

---

## Phase 1: Ops Pack Creation

Create `data/packs/ops/<your_domain>.ops.yaml`. This is the source of truth for
all operations in your domain.

### 1.1 Schema

```yaml
name: <your_domain>
description: "<One-line description of the domain>"

ops:
  - name: <op_name>                    # snake_case, globally unique
    inputs: ['<Type1>', '<Type2>']      # TypeExpr strings
    input_names: ['<param1>', '<param2>']  # Racket parameter names
    output: "<OutputType>"             # TypeExpr string
    racket_symbol: "<op_name>"         # Usually same as name
    description: "<NL description>"    # Used by explain command
    racket_body: |                     # Single Racket expression
      <racket-s-expression>
```

### 1.2 Rules

- **Every op MUST have a `racket_body`**. Without it, the executor cannot generate
  code and the plan becomes a tautology.
- **`racket_body` must be a single expression**. Use `let*` for multiple bindings,
  `begin` for side effects, `cond` for branching.
- **Types**: `Number`, `String`, `Boolean`, `List(Number)`, `List(String)`,
  `List(List(Number))`, `List(Any)`, `Vector(Number)`, etc.
  Use `TypeExpr::parse()` syntax — constructors are `Name(args)`.
- **Input names** become Racket parameters. They must be valid Racket identifiers.
- **Test the Racket body** in isolation before adding it:
  ```bash
  echo '#lang racket\n(define (my_op x y) <body>)\n(displayln (my_op 5 3))' | racket -
  ```

### 1.3 Example

```yaml
name: text_processing
description: "Text processing operations: CSV, regex, string transforms"

ops:
  - name: csv_parse_row
    inputs: ['String']
    input_names: ['row']
    output: "List(String)"
    racket_symbol: "csv_parse_row"
    description: "Parse a CSV row into a list of fields"
    racket_body: |
      (let loop ([s (string->list row)] [field '()] [fields '()])
        (cond [(null? s) (reverse (cons (list->string (reverse field)) fields))]
              [(char=? (car s) #\,) (loop (cdr s) '() (cons (list->string (reverse field)) fields))]
              [else (loop (cdr s) (cons (car s) field) fields)]))
```

### 1.4 Wire into the registry

Edit `src/fs_types.rs` to load your new ops pack:

1. Add an `include_str!` constant:
   ```rust
   const TEXT_PROCESSING_OPS_YAML: &str = include_str!("../data/packs/ops/text_processing.ops.yaml");
   ```

2. Add a `load_ops_pack_str_into` call in `build_full_registry()`:
   ```rust
   let _ = load_ops_pack_str_into(
       &std::fs::read_to_string("data/packs/ops/text_processing.ops.yaml")
           .unwrap_or_else(|_| TEXT_PROCESSING_OPS_YAML.to_string()),
       &mut reg,
   );
   ```

3. Verify it loads:
   ```bash
   cargo test test_build_fs_registry_includes_power_tools -- --nocapture
   ```

---

## Phase 2: Plan Authoring

Plans live in `data/plans/algorithms/<your-domain>/`. Each plan is a `.yaml` or
`.sexp` file that defines inputs, bindings (test data), steps, and expected output.

### 2.1 Plan types

There are two kinds of plans:

**Single-step (atomic)**: Calls one op from your ops pack. The op's `racket_body`
does all the work. These are the simplest but count toward the <30% quota.

```yaml
# CSV row parsing: split a comma-separated row into fields
# Example: csv_parse_row("a,b,c") = (a b c)
# expected: (a b c)

csv_parse_row:
  inputs:
    - row: "String"
  bindings:
    row: '"a,b,c"'
  steps:
    - csv_parse_row
```

**Multi-step (composed)**: Chains multiple ops using `$step-N` back-references.
These are the interesting ones — they compose primitives into algorithms.
**At least 70% of your plans must be multi-step.**

```yaml
# CSV column sum: parse CSV rows and sum a numeric column
# Example: sum column 1 of "1,10\n2,20\n3,30" = 60
# expected: 60

csv_column_sum:
  inputs:
    - data: "String"
    - col: "Number"
  bindings:
    data: '"1,10\n2,20\n3,30"'
    col: "1"
  steps:
    - string_split:
        s: "$data"
        delim: '"\n"'
    - for_sum:
        var: "row"
        over: "$step-1"
        body:
          - csv_parse_row:
              row: "$row"
          - list_ref:
              lst: "$body-1"
              i: "$col"
          - string_to_number:
              s: "$body-2"
```

### 2.2 Format rules

- **Line 1**: `# <NL description>` — this is what the autoregression test feeds
  to `process_input()`. Make it natural and descriptive.
- **Line 2** (optional): `# Example: <human-readable example>`
- **`# expected: <value>`**: The exact string that Racket should print. The
  execution test compares `stdout.trim()` against this.
- **Plan name** = file name (without extension) = top-level YAML key. Use snake_case.
- **Bindings** must be valid Racket literals:
  - Numbers: `"42"`, `"3.14"`
  - Strings: `'"hello"'` (YAML string containing Racket string with quotes)
  - Lists: `"(list 1 2 3)"`, `"(list \"a\" \"b\")"` 
  - Booleans: `"#t"`, `"#f"`
- **`$step-N`** references are 1-indexed. `$step-1` = output of first step.
- **`$body-N`** references are for loop bodies (inside `for_sum`, `for_fold`, etc.).

### 2.3 Multi-step composition patterns

Use these existing ops as building blocks:

| Op | Signature | Use for |
|----|-----------|---------|
| `list_new` | `→ List(a)` | Create literal lists (variadic, space-separated) |
| `range` | `start, end → List(Number)` | Integer ranges |
| `for_sum` | `var, over, body → Number` | Sum over a list |
| `for_fold` | `var, over, init, body → a` | General fold/reduce |
| `for_list` | `var, over, body → List(a)` | Map over a list |
| `for_each` | `var, over, body → Void` | Side effects over a list |
| `iterate` | `bindings, test, body → a` | Named-let loops |
| `conditional` | `test, then, else → a` | If-then-else |
| `add`, `subtract`, `multiply`, `divide` | `x, y → Number` | Arithmetic |
| `modulo`, `quotient`, `remainder` | `x, y → Number` | Integer division |
| `equal`, `less_than`, `greater_than` | `x, y → Boolean` | Comparison |
| `string_length`, `string_ref`, `substring` | `String → ...` | String ops |
| `string_append`, `string_upcase`, `string_downcase` | `String → String` | String transforms |
| `list_ref`, `list_length`, `cons`, `car`, `cdr` | `List → ...` | List ops |
| `vector_new`, `vector_ref`, `vector_set` | `Vector → ...` | Mutable vectors |
| `make_vector` | `size, init → Vector` | Create vectors |
| `string_to_number`, `number_to_string` | conversion | Type conversion |
| `string_split` | `s, delim → List(String)` | Split strings |
| `string_contains`, `string_replace` | `String → ...` | String search/replace |
| `abs`, `min`, `max` | `Number → Number` | Math |

### 2.4 The 70% rule

Count your plans. At least 70% must use `$step-N` or `$body-N` references
(i.e., they compose multiple operations). Single-step plans that just call
one op from your pack are allowed but limited to 30%.

**Why**: Single-step plans are tautologies from the composition perspective —
they prove the op works but not that the plan system can compose it.

---

## Phase 3: NL Wiring

The NL pipeline must be able to match each plan's description to the correct plan.
Three things need wiring:

### 3.1 SymSpell dictionary (`data/nl/nl_dictionary.yaml`)

Add domain-specific words that might be false-corrected by the typo corrector.
The SymSpell dictionary runs BEFORE synonym mapping, so missing words cause
cascading failures.

```yaml
# In the appropriate section (or create a new one):
  # text-processing domain
  csv: 100
  regex: 100
  delimiter: 50
  tokenize: 50
  parse: 100      # may already exist — check first
```

**How to check**: Run the typo corrector on your description words:
```bash
cargo test test_nl_autoregression_report -- --nocapture 2>&1 | grep CLRFY
```

If a plan shows as CLARIFY, its description words are likely being false-corrected.

### 3.2 Lexicon entries (`data/nl/nl_lexicon.yaml`)

Add verb entries for your ops so the Earley parser can recognize them:

```yaml
# In the verbs section, under the appropriate domain:
  - word: csv_parse_row
    action: csv_parse_row
  - word: csv
    action: csv_parse_row
```

### 3.3 Phrase skeletons (`data/nl/nl_lexicon.yaml`)

For multi-word plan names, add phrase group entries that map content-word
skeletons to canonical tokens:

```yaml
# In phrase_groups section:
  - skeleton: [csv, column, sum]
    canonical: csv_column_sum
  - skeleton: [csv, parse]
    canonical: csv_parse_row
```

The phrase tokenizer strips stopwords and matches the remaining content words
against these skeletons. Order matters — longer skeletons match first.

### 3.4 Synonym entries (`data/nl/nl_vocab.yaml`)

If your domain has common synonyms, add them:

```yaml
# In synonyms section:
  parse: [parse, parsing, parsed]
  tokenize: [tokenize, tokenizing, tokenization, tokenise]
```

---

## Phase 4: Validation

Run the domain autoregression report to see the full funnel:

```bash
cargo test --test domain_autoregression -- --nocapture 2>&1 | tail -80
```

This produces a per-plan report showing:

| Stage | What it checks |
|-------|---------------|
| **NL** | Does `process_input(description)` produce the right plan? |
| **Compile** | Does `compile_plan()` succeed? |
| **Codegen** | Does `generate_racket_script()` succeed? |
| **Execute** | Does Racket run without error? |
| **Correct** | Does output match `expected:`? |

### 4.1 Gate: Execution correctness

```bash
cargo test --test algorithm_plans_tests test_<your_category>
```

All plans must execute and produce the correct `expected:` output.
If a plan fails:
- Check the Racket body in isolation
- Check bindings are valid Racket literals
- Check `$step-N` references are correct (1-indexed)
- Run `cadmus --plan <path> --run` to see the actual output

### 4.2 Gate: NL autoregression

```bash
cargo test --test nl_autoregression -- --nocapture
```

Must pass at 100% (all existing + new plans). If a new plan fails:

| Failure type | Fix |
|-------------|-----|
| CLARIFY | Add dictionary words (§3.1), lexicon entries (§3.2), or phrase skeletons (§3.3) |
| WRONG | Adjust the plan's NL description (line 1) to be more distinctive |
| ERROR | Check plan YAML syntax — the autoregression test parses the generated plan |

### 4.3 Gate: No regressions

```bash
cargo test
```

The full test suite must pass. Watch for:
- Existing plans that now match YOUR plan instead of theirs (name collision)
- SymSpell dictionary additions that false-correct existing words
- Lexicon entries that create ambiguity with existing verbs

---

## Phase 5: Integration

### 5.1 Add category test

Edit `tests/algorithm_plans_tests.rs`:

1. Add a `category_test!` macro invocation:
   ```rust
   category_test!(test_text_processing, "data/plans/algorithms/text-processing");
   ```

2. Add the directory to `ALL_CATEGORIES`:
   ```rust
   static ALL_CATEGORIES: &[&str] = &[
       // ... existing categories ...
       "data/plans/algorithms/text-processing",
   ];
   ```

### 5.2 Update domain registry

Edit `data/domains.yaml`:

```yaml
# Add to categories:
  - name: text-processing
    ops_pack: data/packs/ops/text_processing.ops.yaml
    plan_dir: data/plans/algorithms/text-processing
    plan_count: 10
    status: complete
```

Remove the domain from `suggested_next`.

### 5.3 Final verification

```bash
cargo test                                          # Full suite
cargo test --test nl_autoregression -- --nocapture  # NL report
cargo test --test domain_autoregression -- --nocapture  # Funnel report
```

### 5.4 Commit

```bash
git add data/packs/ops/<domain>.ops.yaml \
        data/plans/algorithms/<domain>/ \
        data/nl/nl_lexicon.yaml \
        data/nl/nl_dictionary.yaml \
        data/domains.yaml \
        src/fs_types.rs \
        tests/algorithm_plans_tests.rs
git commit -m "Add <domain> domain: N ops, M plans"
```

---

## Appendix A: Checklist

Before committing, verify:

- [ ] Ops pack loads: `cargo test test_all_ops_registered`
- [ ] All plans compile: `cargo test test_all_plans_compile`
- [ ] All plans execute correctly: `cargo test --test algorithm_plans_tests test_<category>`
- [ ] NL autoregression 100%: `cargo test --test nl_autoregression`
- [ ] No regressions: `cargo test`
- [ ] ≥70% of plans are multi-step
- [ ] Every op has `racket_body`
- [ ] Every plan has `# expected:` comment
- [ ] Every plan has NL description on line 1
- [ ] Domain registry updated
- [ ] No op name collisions with existing packs

## Appendix B: Common Failure Patterns

### "UnknownOp: my_op" during compilation

The ops pack isn't loaded. Check:
1. `include_str!` constant in `src/fs_types.rs`
2. `load_ops_pack_str_into` call in `build_full_registry()`
3. Op name matches exactly (snake_case)

### NL returns CLARIFY for a plan description

The NL pipeline can't parse the description. Common causes:
1. Domain-specific word gets false-corrected by SymSpell → add to dictionary
2. Multi-word plan name has no phrase skeleton → add to lexicon
3. Description is too vague → make it more specific

### Plan executes but wrong output

1. Check bindings are valid Racket literals
2. Run `cadmus --plan <path> --run` to see actual output
3. Test the `racket_body` in isolation
4. Check `$step-N` indexing (1-based, not 0-based)

### Existing plan now matches wrong plan (regression)

Your new plan's name tokens overlap with an existing plan. Solutions:
1. Rename your plan to be more specific
2. Adjust the NL description to use different words
3. Add a phrase skeleton that disambiguates

## Appendix C: Multi-Step Plan Patterns

### Pattern 1: Transform pipeline

```yaml
steps:
  - parse_input        # Step 1: parse raw input
  - transform          # Step 2: transform parsed data
  - format_output      # Step 3: format result
```

### Pattern 2: Accumulate with fold

```yaml
steps:
  - list_new: "1 2 3 4 5"
  - for_fold:
      var: "x"
      over: "$step-1"
      init: "0"
      body:
        - add:
            x: "$acc"
            y: "$x"
```

### Pattern 3: Build and query

```yaml
steps:
  - build_structure:       # Step 1: build data structure
      data: "$input"
  - query_structure:       # Step 2: query it
      structure: "$step-1"
      key: "$query"
```

### Pattern 4: Iterate with named-let

```yaml
steps:
  - iterate:
      bindings: "(i 0) (acc 1)"
      test: "(< i $n)"
      body:
        - multiply:
            x: "$acc"
            y:
              add:
                x: "$i"
                y: "1"
        - add:
            x: "$i"
            y: "1"
      update: "($body-2) ($body-1)"
```

### Pattern 5: Vector mutation loop

```yaml
steps:
  - make_vector:
      size: "$n"
      init: "0"
  - for_range:
      var: "i"
      start: "0"
      end: "$n"
      body:
        - compute_value:
            i: "$i"
        - vector_set:
            v: "$step-1"
            i: "$i"
            val: "$body-1"
  - vector_ref:
      v: "$step-1"
      i: "$target"
```
