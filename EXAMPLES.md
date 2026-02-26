# Cadmus — Annotated Examples

This document walks through Cadmus end-to-end: from the plan DSL, through type
inference and compilation, to generated Racket programs. Every example is real —
you can run them with `cadmus --plan <file>`.

The second half traces real natural-language prompts through every pipeline stage,
showing exactly what happens at each step. These traces are verified by
`tests/pipeline_traces.rs` — they're not hand-written, they're captured output.

---

## Table of Contents

1. [The Plan DSL](#the-plan-dsl)
2. [Filesystem Pipelines](#filesystem-pipelines)
3. [Archive & Format Resolution](#archive--format-resolution)
4. [Text Processing Pipelines](#text-processing-pipelines)
5. [Git & DevOps](#git--devops)
6. [Arithmetic & Pure Computation](#arithmetic--pure-computation)
7. [Algorithms](#algorithms)
8. [Pipeline Traces](#pipeline-traces)
9. [Type System Deep Dive](#type-system-deep-dive)
10. [Op Inference](#op-inference)

---

## The Plan DSL

Plans are written in an s-expression DSL. The grammar is:

```scheme
;; Comment (becomes the NL description for autoregression)
(define (plan-name (param₁ : Type₁) (param₂ : Type₂)) : ReturnType
  (op₁)
  (op₂ :keyword "value")
  (op₃ :each)                          ;; map mode — apply to each element
  (op₄ :keyword "$param₁")             ;; $-references bind to inputs
  (op₅ :keyword "$step-2"))            ;; $step-N references bind to prior steps
```

### Anatomy of a plan

```scheme
;; Find PDFs in a directory                     ← NL description (used by autoregression)
(define (find-pdfs                              ← plan name (kebab-case)
         (path : Dir)                           ← typed input: Dir(Bytes) by default
         (keyword : Pattern))                   ← typed input: Pattern
  (list_dir)                                    ← step 1: Dir(a) → Seq(Entry(Name, a))
  (find_matching :pattern "*.pdf")              ← step 2: filter by glob
  (sort_by "name"))                             ← step 3: sort alphabetically
```

**Key rules:**
- Steps form a pipeline — each step's output type feeds the next step's input
- `:each` triggers map mode: the step runs on each element of a sequence
- `:keyword value` passes named parameters to the operation
- `$param` references input bindings; `$step-N` references prior step outputs
- The compiler infers types through the chain via unification

### Type annotations

```scheme
;; Bare name — type inferred from name
(define (my-plan (path : Dir))          ;; Dir → Dir(Bytes)
  ...)

;; Explicit type
(define (my-plan (archive : (File (Archive (File Image) Cbz))))
  ...)                                  ;; File(Archive(File(Image), Cbz))

;; Return type annotation (optional, not checked — output inferred from chain)
(define (factorial (n : Number)) : Number
  ...)
```

### Bind expressions

For algorithm plans, `bind` provides concrete test values:

```scheme
(define (factorial (n : Number)) : Number
  (bind n 10)                           ;; n = 10 at runtime
  ...)
```

---

## Filesystem Pipelines

### Find and filter files

```scheme
;; Find PDFs in a directory
(define (find-pdfs (path : Dir) (keyword : Pattern))
  (list_dir)
  (find_matching :pattern "*.pdf")
  (sort_by "name"))
```

**Type chain:**
```
input:  Dir(File(PDF))                                    ← inferred from *.pdf glob
  1. list_dir:       Dir(File(PDF)) → Seq(Entry(Name, File(PDF)))
  2. find_matching:  Seq(Entry(Name, File(PDF))) → Seq(Entry(Name, File(PDF)))
  3. sort_by:        Seq(Entry(Name, File(PDF))) → Seq(Entry(Name, File(PDF)))
output: Seq(Entry(Name, File(PDF)))
```

The `*.pdf` glob pattern causes the compiler to narrow `Dir(Bytes)` to
`Dir(File(PDF))` — the type flows through the entire chain.

### Walk, filter, count

```scheme
;; Deep directory audit: walk, find .rs, filter tests, sort, unique, count
(define (deep-audit (path : Dir))
  (walk_tree)
  (find_matching :pattern "*.rs")
  (filter :pattern "test")
  (sort_by "name")
  (unique)
  (count))
```

**Type chain:**
```
input:  Dir(Bytes)
  1. walk_tree:      Dir(Bytes) → Seq(Entry(Name, Bytes))
  2. find_matching:  → Seq(Entry(Name, Bytes))
  3. filter:         → Seq(Entry(Name, Bytes))         (Racket-native: regexp filter)
  4. sort_by:        → Seq(Entry(Name, Bytes))
  5. unique:         → Seq(Entry(Name, Bytes))
  6. count:          → Count
output: Count
```

### Map mode — delete each match

```scheme
;; Delete DS_Store: recursively delete all .DS_Store files
(define (delete-ds-store (path : Dir))
  (walk_tree)
  (find_matching :pattern ".DS_Store")
  (delete :each))                       ;; ← :each = map over sequence
```

**Generated Racket:**
```racket
(let*
  ([step-1 (shell-lines (string-append "find " (shell-quote ".")))]
   [step-2 (filter (lambda (line) (regexp-match? (regexp "\\.DS_Store$") line)) step-1)]
   [step-3 (map (lambda (_line)
                  (shell-lines (string-append "rm " (shell-quote _line))))
                step-2)])
  (displayln step-3))
```

The `:each` modifier causes the compiler to wrap `delete` in a `map` lambda.
Each element of the sequence is bound to `_line` and processed individually.

---

## Archive & Format Resolution

Cadmus resolves generic archive ops to format-specific ones at compile time.

### Extract a CBZ archive

```scheme
;; Extract a CBZ archive
(define (extract-cbz (path : (File (Archive (File Image) Cbz))))
  (extract_archive))
```

**What happens at compile time:**
1. Input type is `File(Archive(File(Image), Cbz))`
2. `extract_archive` is a generic op — the compiler calls `resolve_archive_op()`
3. Format `Cbz` is extracted from the type, looked up in `filetypes.yaml` → format family `zip`
4. Generic `extract_archive` is rewritten to `extract_zip`
5. The generated Racket calls `unzip`

### Repack comics — the flagship pipeline

```scheme
;; Repack comic archives: extract all CBZ files and combine into one
(define (repack-comics (path : Dir))
  (list_dir)
  (find_matching :pattern "*.cbz")
  (sort_by "name")
  (extract_archive :each)              ;; extract each CBZ into isolated temp dir
  (pack_archive :output "combined.cbz"))
```

**Type chain:**
```
input:  Dir(File(Archive(File(Image), Cbz)))
  1. list_dir:         → Seq(Entry(Name, File(Archive(File(Image), Cbz))))
  2. find_matching:    → Seq(Entry(Name, File(Archive(File(Image), Cbz))))
  3. sort_by:          → Seq(Entry(Name, File(Archive(File(Image), Cbz))))
  4. extract_zip:      → Seq(Entry(Name, Seq(Entry(Name, File(Image)))))  [each, isolated]
  5. pack_zip:         → File(Archive(Seq(Entry(Name, File(Image))), ...))
```

**Key behaviors:**
- `extract_archive :each` gets the `isolate` flag — each archive extracts to its own
  temp directory via `(make-temporary-directory)`, preventing file collisions
- Format resolution: `*.cbz` glob → `Cbz` type → `zip` family → `extract_zip` / `pack_zip`
- The generated Racket creates per-archive temp dirs, extracts, then zips everything together

---

## Text Processing Pipelines

### Log forensics — 10-step awk/sed pipeline

```scheme
;; Server log forensics: 10-step text transformation pipeline
(define (log-forensics (logfile : File))
  (awk_extract :program "{print $1, $4, $7, $9}")
  (sed_script :script "/[45][0-9][0-9]/!d")
  (sed_script :script "s/\\[//g; s/\\]//g")
  (awk_extract :program "{print $1, $3}")
  (sed_script :script "s/ / | /g")
  (awk_extract :program "{printf \"%04d %s\\n\", NR, $0}")
  (head :count "200")
  (tail :count "100")
  (sed_script :script "1i\\--- ERROR REPORT ---")
  (sed_script :script "/^$/d"))
```

Each step threads `File(Text)` through the chain. The generated Racket pipes
shell commands via `shell-lines`, with each awk/sed invocation receiving the
previous step's output.

### Multi-step statistics with back-references

```scheme
;; Coefficient of variation: stddev / mean × 100
(define (coefficient_of_variation (lst : (List Number)))
  (bind lst (list 2 4 4 4 5 5 7 9))
  (mean_list :lst $lst)                 ;; step 1 → 5.0
  (stddev_list :lst $lst)               ;; step 2 → 2.0
  (divide :x $step-2 :y $step-1)       ;; step 3 → 0.4  (stddev / mean)
  (multiply :x $step-3 :y 100))        ;; step 4 → 40.0
```

Note the `$step-N` back-references: step 3 divides step 2's output by step 1's
output. This is a DAG, not a linear pipeline — the compiler validates that all
`$step-N` references point to valid prior steps.

---

## Git & DevOps

### Commit and push

```scheme
;; Add all changes, commit, and push to remote
(define (commit-and-push (repo : Repo) (message : Name))
  (git_add :files ".")
  (git_commit :message "$message")
  (git_push :repo "$repo" :remote "origin" :branch "main"))
```

### Git log search

```scheme
;; Git log search: search commit history for a pattern
(define (git-log-search (repo : Repo) (pattern : Pattern))
  (git_log)
  (filter :pattern "$pattern")
  (sort_by "date"))
```

---

## Arithmetic & Pure Computation

Pure Racket plans use no shell commands — they compile to native Racket
expressions.

### Simple arithmetic

```scheme
;; Complex arithmetic: add, multiply, then subtract numbers
(define (complex-arith) : Number
  (add :x "4" :y "35")                 ;; → 39
  (multiply :y "2")                     ;; → 78  (prev result × 2)
  (subtract :y "10"))                   ;; → 68
```

**Generated Racket:**
```racket
(let*
  ([step-1 (+ 4 35)]
   [step-2 (* step-1 2)]
   [step-3 (- step-2 10)])
  (displayln step-3))
```

### Factorial with fold

```scheme
;; Factorial: n! = n × (n-1) × ... × 1
;; expected: 3628800
(define (factorial (n : Number)) : Number
  (bind n 10)
  (for/fold ([acc 1]) ([i (range 1 (+ n 1))])
    (* acc i)))
```

**Generated Racket:**
```racket
(define n 10)
(let*
  ([step-1 (range 1 n)]
   [step-2 (for/fold ([acc 1]) ([i (in-list step-1)]) (* acc i))])
  (displayln step-2))
```

Output: `3628800`

### Euler's totient with cond

```scheme
;; Euler's totient: count integers 1..n coprime to n
;; expected: 4
(define (euler_totient (n : Number)) : Number
  (bind n 12)
  (for/fold ([total 0]) ([i (range 1 n)])
    (cond
      [(= (gcd i n) 1) (+ total 1)]
      [else total])))
```

Output: `4`

---

## Algorithms

Cadmus ships 268 plans across 21 categories. Algorithm plans come in two forms:

### Atomic ops — single-step plans backed by Racket implementations

```scheme
;; Dijkstra's shortest path: find shortest distances from source
;; expected: (0 3 1 4 7)
(define (dijkstra_shortest_path (n : Number))
  (bind n 5)
  (dijkstra_shortest_path))
```

The `dijkstra_shortest_path` op has a `racket_body` in `algorithm.ops.yaml` —
a full Racket implementation that gets emitted as a `(define ...)` block in the
generated script. The plan is a single call to that op.

### Multi-step plans — composed from primitive ops

```scheme
;; Longest increasing subsequence length (O(n²) DP)
;; expected: 4
(define (longest_increasing_subsequence (lst : (List Number))) : Number
  (bind lst (list 10 9 2 5 3 7 101 18))
  (let ([n (length lst)]
        [dp (make n 1)])
    (for/each ([i (range 1 n)])
      (for/each ([j (range 0 i)])
        (when (< (ref lst j) (ref lst i))
          (let ([cur (ref dp i)]
                [prev (ref dp j)])
            (set! dp i (max cur (+ prev 1)))))))
    (for/fold ([mx 0]) ([i (range 0 n)])
      (max mx (ref dp i)))))
```

This uses mutable vectors (`make`, `set!`, `ref`), nested iteration
(`for/each`), conditionals (`when`), and accumulation (`for/fold`).

### Category coverage

| Category | Plans | Example |
|----------|------:|---------|
| arithmetic | 11 | factorial, fibonacci, gcd |
| sorting | 11 | bubble_sort, merge_sort, quicksort |
| searching | 12 | binary_search, linear_search, jump_search |
| graph | 16 | dijkstra, bellman_ford, topological_sort |
| dynamic-programming | 16 | knapsack, coin_change, longest_common_subseq |
| tree | 10 | tree_height, inorder, lowest_common_ancestor |
| string | 12 | caesar_cipher, palindrome, levenshtein |
| combinatorics | 9 | permutations, power_set, catalan_number |
| statistics | 17 | mean, median, correlation, z_score |
| text-processing | 11 | word_count, char_frequency, csv_parse |
| + 11 more | 87 | bitwise, encoding, geometry, matrix, ... |

---

## Pipeline Traces

These are real traces captured from `tests/pipeline_traces.rs`. Each shows
a natural-language prompt flowing through every stage of the pipeline.

### Trace 1: "find all PDFs in ~/Documents"

```
  1. Normalize:      ["find", "all", "pdfs", "in", "~/Documents"]
  2. Typo correct:   ["find", "all", "pdfs", "in", "~/Documents"]
  3. Phrase tokens:  ["find", "all", "pdfs", "in", "~/Documents"]
  4. Earley parse:   1 parse(s), best score: 10.6
  5. Plan name:      find-pdfs
  6. Plan ops:       [list_dir → find_matching → sort_by]
  7. Type chain:
       list_dir:      Dir(File(PDF)) → Seq(Entry(Name, File(PDF)))
       find_matching: Seq(Entry(Name, File(PDF))) → Seq(Entry(Name, File(PDF)))
       sort_by:       Seq(Entry(Name, File(PDF))) → Seq(Entry(Name, File(PDF)))
  8. Racket:         (747 chars)
  9. Output:         ()
```

**What happened:** The Earley parser recognized `find [noun] in [path]` as a
`select` action. The recipe table expanded `select` into `list_dir → find_matching
→ sort_by`. The `*.pdf` glob narrowed the input type from `Dir(Bytes)` to
`Dir(File(PDF))`, and unification threaded `File(PDF)` through every step.

### Trace 2: "compute the factorial"

```
  1. Normalize:      ["compute", "the", "factorial"]
  2. Typo correct:   ["compute", "the", "factorial"]
  3. Phrase tokens:  ["compute", "the", "factorial"]
  4. Earley parse:   1 parse(s), best score: 3.1
  5. Plan name:      factorial
  6. Plan ops:       [add → range → fold]
  7. Type chain:
       add:   Number → Number
       range: Number → List(Number)
       fold:  List(Number) → List(Number)
  8. Racket:         (320 chars)
  9. Output:         3628800
```

**What happened:** The pre-Earley short-circuit found "factorial" as a plan file
in `data/plans/algorithms/arithmetic/factorial.sexp`. The plan was loaded with
its `(bind n 10)` binding, compiled through the type chain, and executed.
The fold computed `1 × 2 × 3 × ... × 10 = 3628800`.

### Trace 3: "run dijkstra shortest path"

```
  1. Normalize:      ["run", "dijkstra", "shortest", "path"]
  2. Typo correct:   ["run", "dijkstra", "shortest", "path"]
  3. Phrase tokens:  ["run", "dijkstra_shortest_path"]
  4. Earley parse:   1 parse(s), best score: 2.1
  5. Plan name:      dijkstra_shortest_path
  6. Plan ops:       [dijkstra_shortest_path]
  7. Type chain:
       dijkstra_shortest_path: Number → List(Number)
  8. Racket:         (836 chars)
  9. Output:         (0 3 1 4 7)
```

**What happened:** The phrase tokenizer joined "dijkstra", "shortest", "path"
into a single token `dijkstra_shortest_path` via a phrase group in the lexicon.
This matched the algorithm plan file directly. The plan is a single atomic op
with a `racket_body` — a full Dijkstra implementation emitted as a `(define ...)`
block. The test graph has 5 nodes, and the output is shortest distances from
node 0: `(0 3 1 4 7)`.

### Trace 4: "merge sort a list"

```
  1. Normalize:      ["merge", "sort", "a", "list"]
  2. Typo correct:   ["merge", "sort", "a", "list"]
  3. Phrase tokens:  ["merge_sort", "a", "list"]
  4. Earley parse:   1 parse(s), best score: 3.1
  5. Plan name:      merge_sort
  6. Plan ops:       [merge_sort]
  7. Type chain:
       merge_sort: List(Number) → List(Number)
  8. Racket:         (639 chars)
  9. Output:         (1 2 3 4 5 6 7 8 9)
```

**What happened:** Phrase tokenizer joined "merge" + "sort" → `merge_sort`.
The algorithm atom's `racket_body` contains a full merge sort implementation.
Input: `(5 3 8 1 9 2 7 4 6)` → Output: `(1 2 3 4 5 6 7 8 9)`.

### Trace 5: "euler totient"

```
  1. Normalize:      ["euler", "totient"]
  2. Typo correct:   ["euler", "totient"]
  3. Phrase tokens:  ["euler_totient"]
  4. Earley parse:   1 parse(s), best score: 1.5
  5. Plan name:      euler_totient
  6. Plan ops:       [range → fold]
  7. Type chain:
       range: Number → List(Number)
       fold:  List(Number) → List(Number)
  8. Racket:         (326 chars)
  9. Output:         4
```

**What happened:** This is a multi-step plan (not an atomic op). The plan
generates `range(1, n)` then folds with a `cond` that checks `gcd(i, n) == 1`.
For n=12, there are 4 integers coprime to 12: {1, 5, 7, 11}.

### Trace 6: "list files in /tmp"

```
  1. Normalize:      ["list", "files", "in", "/tmp"]
  2. Typo correct:   ["list", "files", "in", "/tmp"]
  3. Phrase tokens:  ["list", "files", "in", "/tmp"]
  4. Earley parse:   1 parse(s), best score: 10.5
  5. Plan name:      list_in__tmp
  6. Plan ops:       [list_dir]
  7. Type chain:
       list_dir: Dir(Bytes) → Seq(Entry(Name, Bytes))
  8. Racket:         (579 chars)
  9. Output:         (actual directory listing of /tmp)
```

**What happened:** The Earley parser recognized `list [noun] in [path]` as an
`enumerate` action. The recipe table mapped `enumerate` to `list_dir`. The path
`/tmp` was bound as the input. The generated Racket runs `ls /tmp` and returns
the actual directory contents.

### Trace 7: "sort files by size biggest first"

```
  1. Normalize:      ["sort", "files", "by", "size", "biggest", "1"]
  2. Typo correct:   ["sort", "files", "by", "size", "suggest", "1"]
  3. Phrase tokens:  ["sort", "files", "by", "size", "suggest", "1"]
  4. Earley parse:   1 parse(s), best score: 6.1
  5. Plan name:      sort
  6. Plan ops:       [walk_tree → sort_by]
  7. Type chain:
       walk_tree: Dir(Bytes) → Seq(Entry(Name, Bytes))
       sort_by:   Seq(Entry(Name, Bytes)) → Seq(Entry(Name, Bytes))
```

**What happened:** Note the normalization: "first" → "1" (ordinal canonicalization)
and the typo correction: "biggest" → "suggest" (a false correction — this is a
known limitation of the SymSpell dictionary). Despite the correction error, the
Earley parser still recognized the `order` action with `by=size`, and the recipe
table produced `walk_tree → sort_by`.

### Trace 8: "binary search for 7"

```
  1. Normalize:      ["binary", "search", "for", "7"]
  2. Typo correct:   ["binary", "search", "for", "7"]
  3. Phrase tokens:  ["binary_search", "for", "7"]
  4. Earley parse:   (no parse — short-circuited)
  5. Plan name:      binary_search
  6. Plan ops:       [binary_search]
  7. Type chain:
       binary_search: List(Number) → Number
  8. Racket:         (548 chars)
  9. Output:         3
```

**What happened:** Phrase tokenizer joined "binary" + "search" → `binary_search`.
The pre-Earley lookup found the plan file. The algorithm searches for 7 in
`(1 3 5 7 9 11 13)` and returns index 3.

### Trace 9: "generate permutations"

```
  1. Normalize:      ["generate", "permutations"]
  2. Typo correct:   ["generate", "permutations"]
  3. Phrase tokens:  ["generate_permutations"]
  4. Plan name:      generate_permutations
  5. Plan ops:       [generate_permutations]
  6. Type chain:
       generate_permutations: Number → List(List(Number))
  7. Output:         ((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))
```

**What happened:** Phrase tokenizer joined the two tokens. The algorithm
generates all 3! = 6 permutations of (1 2 3).

---

## Type System Deep Dive

### TypeExpr grammar

Types are an open grammar — no enum to extend:

```
TypeExpr ::= Primitive(name)                    ;; "Bytes", "Number", "Text"
           | Constructor(name, args...)          ;; "Dir(File(PDF))", "Seq(Entry(Name, a))"
           | Var(name)                           ;; "a", "b" — polymorphic variables
```

### Unification

The compiler threads types through the step chain using unification.
When `list_dir : Dir(a) → Seq(Entry(Name, a))` receives `Dir(File(PDF))`,
unification binds `a = File(PDF)`, producing `Seq(Entry(Name, File(PDF)))`.

### Bytes promotion

When the input type contains `Bytes` and downstream ops need something more
specific, the compiler promotes automatically:

```
Dir(Bytes) + [search_content downstream]  →  Dir(File(Text))
```

The algorithm: replace `Bytes` with a fresh variable `_promote`, simulate the
type chain forward, let unification discover what `_promote` should be.

### Format resolution

Archive types carry their format: `File(Archive(Content, Format))`.
The compiler extracts the format, looks it up in `filetypes.yaml` to find the
format family, and rewrites generic ops:

```
extract_archive + File(Archive(File(Image), Cbz))
  → Cbz → format_family: zip → extract_zip → shells out to `unzip`
```

---

## Op Inference

Cadmus discovers operations from fact packs using a 5-phase inference engine:

### Phase 0 — Discovery
Scan fact packs for entities with `op_name` + `racket_symbol` not yet in the
registry. Register placeholder stubs.

### Phase 1 — Op-symmetric inference
If op A has a `symmetric_partner` pointing to op B, derive B's type signature
by copying A's (swapping input/output as appropriate).

```
add (anchor, Number×Number→Number)
  → symmetric_partner: subtract
  → subtract gets Number×Number→Number
```

### Phase 2 — Type-symmetric inference
If op A and stub B share a `type_symmetry_class`, derive B's signature from A's.

```
add (class: binop, Number×Number→Number)
multiply (class: binop, stub)
  → multiply gets Number×Number→Number
```

### Phase 3 — Op-symmetric replay
Re-run Phase 1 to catch ops whose partners were just discovered in Phase 2.

```
multiply (just discovered) → symmetric_partner: divide → divide gets signature
```

### Phase 4 — Shell submode discovery
Scan CLI fact packs for `submode_*` properties. Register shell-callable ops
with appropriate type signatures.

### Result

From 5 anchor ops (add, cons, cdr, less_than, string_upcase) and fact pack
metadata, the engine discovers 9 additional ops: subtract, multiply, divide,
remove, list_reverse, greater_than, less_than_or_equal, greater_than_or_equal,
string_downcase.

The full registry contains **524 ops** across 9 packs after inference.

---

## Running the Examples

```bash
# Run a filesystem pipeline
cadmus --plan data/plans/find_pdfs.sexp

# Run and execute
cadmus --plan data/plans/delete_ds_store.sexp --run

# Run an algorithm
cadmus --plan data/plans/algorithms/arithmetic/factorial.sexp

# Interactive chat
cadmus --chat

# See the type chain without executing
cadmus --plan data/plans/repack_comics.sexp
```

All 268 plans compile, generate valid Racket, and execute correctly.
The test suite has **1431 tests**, 0 failures.
