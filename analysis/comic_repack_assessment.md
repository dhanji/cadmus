# Comic Book Repack: Cadmus Capability Assessment (Racket Path)

## Task Statement

> "In dir X I have a series of comic book files stored in cbr/cbz format.
> Repack them into a flattened archive preserving the order of images in each
> original archive as well as the total order of issues (archives)."

## Task Decomposition

| # | Sub-task | Op | Mode |
|---|----------|----|------|
| 1 | List archives in dir X | `list_dir` | normal |
| 2 | Filter to .cbz/.cbr files | `find_matching` | normal |
| 3 | Sort by issue order | `sort_by` | normal |
| 4 | Extract images from each archive | `extract_archive` | each |
| 5 | Repack into single flat archive | `pack_archive` | normal |

## Live Test

```yaml
workflow: "Repack comic books"
inputs:
  path: "/comics"
steps:
  - list_dir
  - find_matching:
      pattern: "*.cbz"
  - sort_by: name
  - extract_archive: each
  - pack_archive:
      output: "combined.cbz"
```

**Tested with `cadmus --workflow ... --racket`.** The workflow compiles and
type-checks. The generated Racket script has significant bugs.

### Generated Racket (actual output)

```racket
(let*
  (
    [step-1 (shell-lines (string-append "ls " (shell-quote "/comics")))]
    [step-2 (filter (lambda (line) (regexp-match? (regexp "\\.cbz$") line)) step-1)]
    [step-3 (sort step-2 string<?)]
    [step-4 (map (lambda (_line)
              (append-map (lambda (_f) (shell-lines (string-append "tar " (shell-quote _f)))) step-3))
              step-3)]
    [step-5 (shell-lines (string-append "tar " (shell-quote step-4)))]
  )
  (displayln step-5))
```

## Sub-task Verdicts

### ✓ Sub-task 1: List archives — CORRECT

**Codegen path:** `list_dir` → subsumed → `shell_ls` → `extract_shell_meta` → `("ls", None)`

```racket
[step-1 (shell-lines (string-append "ls " (shell-quote "/comics")))]
```

Returns `List(String)` — a list of filenames. Correct.

### ✓ Sub-task 2: Filter to .cbz — CORRECT

**Codegen path:** `find_matching` → `lookup_racket_native` → `FilterPredicate`

```racket
[step-2 (filter (lambda (line) (regexp-match? (regexp "\\.cbz$") line)) step-1)]
```

Racket-native filter with regex. Correct.

### ✓ Sub-task 3: Sort by name — CORRECT

**Codegen path:** `sort_by` → dual-behavior → has `prev_binding` → Racket-native `SortComparator`

```racket
[step-3 (sort step-2 string<?)]
```

Lexicographic sort. Correct for zero-padded filenames. Minor gap: no
`sort -V` equivalent for natural/version ordering (would need a custom
Racket comparator).

### ✗ Sub-task 4: Extract each archive — THREE BUGS

**Codegen path:** `extract_archive` → subsumed → `shell_tar` (arity 1)

The executor takes two passes through this op:

1. **`op_to_racket()`** sees `extract_archive` is subsumed to `shell_tar`.
   `prev_is_seq` is true (step-3 is a list), so `subsumed_op_to_racket()`
   generates the seq→string bridge:
   ```racket
   (append-map (lambda (_f) (shell-lines (string-append "tar " (shell-quote _f)))) step-3)
   ```

2. **`generate_racket_script()`** sees `is_each` is true, so it wraps the
   expression in a map:
   ```racket
   (map (lambda (_line) <expr-from-step-1>) step-3)
   ```

**Bug 1: Double wrapping.** The `each` mode in `generate_racket_script`
wraps in `(map ...)`, but `subsumed_op_to_racket` already generated an
`(append-map ...)` over the same list. The result iterates `step-3` twice —
once in the outer `map` and once in the inner `append-map` — producing a
nested `List(List(String))` instead of a flat `List(String)`.

The `each` mode wrapper and the seq→string bridge are solving the same
problem (iterating over a list) but don't know about each other.

**Bug 2: Wrong command.** Both `extract_archive` and `pack_archive` are
subsumed to the same `shell_tar` op. The codegen produces `"tar "` with no
flags. Extracting needs `tar -xf`, creating needs `tar -cf`. The
subsumption entry has no way to distinguish the two operations.

For CBZ/CBR files specifically, `tar` is the wrong tool entirely — CBZ is a
ZIP archive, CBR is a RAR archive. The correct commands are:
- CBZ: `unzip -o <file> -d <dir>`
- CBR: `unrar x <file> <dir>`

**Bug 3: `_line` variable unused.** The `each` mode binds `_line` as the
lambda parameter, but the inner expression references `step-3` (the whole
list) instead of `_line`. The `op_to_racket` call receives `prev_binding =
Some("step-3")` (the previous step), not `"_line"` (the each-mode
variable).

### ✗ Sub-task 5: Repack — WRONG

```racket
[step-5 (shell-lines (string-append "tar " (shell-quote step-4)))]
```

**Bug 4: `step-4` is a list, not a string.** `(shell-quote step-4)` will
fail at runtime because `shell-quote` expects a string, not a list.

**Bug 5: Wrong command (same as Bug 2).** `pack_archive` also maps to bare
`"tar "` with no flags. Creating a CBZ needs `zip`.

**Bug 6: No output path.** The `output: combined.cbz` parameter is in
`step.params` but the subsumed codegen path doesn't use it — it just passes
the previous binding as the operand.

## Root Cause Analysis

The bugs cluster around three design gaps:

### Gap 1: `each` mode vs seq→string bridge conflict

The `each` mode in `generate_racket_script()` (line 640) wraps in
`(map (lambda (_line) ...) prev)`. But `subsumed_op_to_racket()` (line 227)
also generates `(append-map ...)` when `prev_is_seq` is true. These two
mechanisms don't coordinate.

**Fix:** When `is_each` is true, `op_to_racket` should receive `_line` as
the prev_binding (not the step variable), and `prev_is_seq` should be false
(since `_line` is a single element, not a list).

### Gap 2: Subsumption conflates extract and create

Both `extract_archive` and `pack_archive` map to `shell_tar` with arity 1.
The subsumption layer has no concept of "mode" or "direction" — it just
maps an fs_op to a shell command.

**Fix options:**
- Add `flags` field to `SubsumptionEntry` (e.g., `"-xf"` for extract,
  `"-cf"` for create)
- Split into `shell_tar_extract` and `shell_tar_create` as separate ops
- Use the CLI fact pack's submodes (`submode_extract: "-xf"`,
  `submode_create: "-cf"`) — these already exist in `macos_cli.facts.yaml`

### Gap 3: Archive format awareness missing from Racket path

The shell executor (`src/executor.rs`) has `extract_archive_format()` which
reads the `TypeExpr` to determine the format (Cbz→unzip, TarGz→tar xzf,
Rar→unrar, etc.). The Racket executor has no equivalent — it blindly uses
`tar` for everything.

**Fix:** Port `extract_archive_format()` logic to the Racket codegen, or
better yet, make the subsumption entry format-aware by reading the
`CompiledStep.input_type` / `output_type`.

## Type System Assessment

The type chain is actually correct:

```
Dir(File(Archive(File(Image), Cbz)))
  → list_dir → Seq(Entry(Name, File(Archive(File(Image), Cbz))))
  → find_matching → Seq(Entry(Name, File(Archive(File(Image), Cbz))))
  → sort_by → Seq(Entry(Name, File(Archive(File(Image), Cbz))))
  → extract_archive: each → Seq(Entry(Name, Seq(Entry(Name, File(Image)))))
  → pack_archive → File(Archive(Seq(Entry(Name, File(Image))), fmt))
```

The workflow compiler accepts this because `pack_archive`'s polymorphic `a`
unifies with `Seq(Entry(Name, File(Image)))`. The type system is
**permissive** here — it doesn't enforce that the archive contents should be
flat files rather than nested sequences.

At the Racket level, the data is `List(String)` (flat file paths), so the
nesting is invisible. The type system's richness is lost at the codegen
boundary — which is fine, as long as the codegen is correct.

## Summary of Bugs

| # | Bug | Location | Severity |
|---|-----|----------|----------|
| 1 | Double wrapping: `each` + seq bridge both iterate | `racket_executor.rs:640` + `:227` | **Critical** |
| 2 | `_line` variable unused in each-mode lambda | `racket_executor.rs:640-643` | **Critical** |
| 3 | `extract_archive` → bare `tar` (no `-xf` flag) | `type_lowering.rs:136` | **Critical** |
| 4 | `pack_archive` → bare `tar` (no `-cf` flag) | `type_lowering.rs:137` | **Critical** |
| 5 | CBZ/CBR needs `unzip`/`unrar`, not `tar` | `type_lowering.rs:136-137` | **Critical** |
| 6 | `step-4` is list, passed to `shell-quote` (expects string) | `racket_executor.rs:~530` | **Critical** |
| 7 | `pack_archive` ignores `output` param | `racket_executor.rs` (subsumed path) | **High** |
| 8 | No version sort (`sort -V` equivalent) | `racket_executor.rs:293` | **Low** |

## Verdict

**Cadmus cannot accomplish this task in its current state via the Racket
path.** The workflow compiles and type-checks (the type system and workflow
compiler are sound), but the Racket codegen has 7 bugs that would produce a
non-functional script.

The bugs fall into two categories:

1. **Architectural** (Bugs 1-2): The `each` mode in `generate_racket_script`
   and the seq→string bridge in `subsumed_op_to_racket` are two independent
   mechanisms that both try to handle list iteration, and they conflict when
   both are active.

2. **Missing specialization** (Bugs 3-7): The subsumption of
   `extract_archive`/`pack_archive` to `shell_tar` is too coarse — it loses
   the extract-vs-create distinction and the format-specific tool selection
   (unzip for CBZ, unrar for CBR, tar for tarballs).

### Recommended Fixes

**Fix 1 (each-mode coordination):** When `is_each` is true in
`generate_racket_script`, pass `"_line"` as `prev_binding` and `false` as
`prev_is_seq` to `op_to_racket`. This makes the inner expression operate on
a single element, and the outer `map` handles the iteration.

**Fix 2 (archive specialization):** Don't subsume `extract_archive` and
`pack_archive` to `shell_tar`. Instead, handle them as special cases in
`op_to_racket` (like `sort_list` and `format_string`), reading
`extract_archive_format()` from the step's type to select the right tool
and flags.

**Fix 3 (pack_archive output):** Read the `output` param and generate:
```racket
(for-each (lambda (f) (system (string-append "zip -g "
  (shell-quote "combined.cbz") " " (shell-quote f)))) step-4)
```

With these 3 fixes (~50-80 lines), the pipeline would generate correct,
runnable Racket.
