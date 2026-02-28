# Agent-Distilled Code Editing Domain

Operations and plans distilled from ~200 pi agent sessions across 41 projects.

## What Was Added

### Ops Pack: `data/packs/ops/code_editing.ops.yaml`

16 operations covering the core coding loop:

| Category | Op | What It Does |
|----------|-----|-------------|
| Search | `grep_code` | Recursive grep across source files |
| Search | `find_definition` | Find where a fn/struct/class is defined |
| Search | `find_usages` | Find all references to a symbol |
| Search | `find_imports` | Find import/use statements for a module |
| Navigate | `list_symbols` / `file_outline` | Show definitions with line numbers |
| Navigate | `list_source_files` | List source files, excluding build dirs |
| Navigate | `recently_changed` | Files changed in last 5 git commits |
| Edit | `sed_replace` | Find/replace pattern in a file |
| Edit | `fix_import` | Replace an import path |
| Edit | `add_after` | Insert a line after a pattern match |
| Edit | `remove_lines` | Delete lines matching a pattern |
| Edit | `fix_assertion` | Update test expected values |
| Build | `build_project` | Auto-detect build system and build |
| Build | `test_project` | Auto-detect and run tests |
| Build | `lint_project` | Auto-detect and run linter |

Build/test/lint auto-detect the project type: Cargo.toml → cargo, package.json → npm, Makefile → make, go.mod → go.

### Plans: 14 in `data/plans/`

Single-op: `grep_code`, `find_definition`, `find_usages`, `file_outline`, `list_source_files`, `build_project`, `test_project`, `lint_project`, `recently_changed`, `fix_import`

Multi-step (session-distilled sequences):
- `edit_and_build` — replace pattern then build
- `edit_and_test` — replace pattern then test
- `fix_test_assertion` — fix assertion value then test
- `add_wiring_line` — insert line after pattern then build

### NL Wiring

- 12 words in `data/nl/nl_dictionary.yaml` (grep, definition, usages, symbol, outline, etc.)
- 11 verb entries in `data/nl/nl_lexicon.yaml` mapping to code editing actions
- 3 action recipes in `src/nl/intent_compiler.rs` (build → build_project, test → test_project, lint → lint_project)

### Rust Wiring

4 lines total — 2 in `src/fs_types.rs` and 2 in `src/racket_executor.rs` to load the new ops pack into both registries.

## How It Was Distilled

### Source data

Pi agent sessions stored in `~/.pi/agent/sessions/`. Each session is a JSONL file recording every user message, assistant response, and tool call (read, bash, edit, write) with full arguments.

### Step 1: Count edit shapes

Classified all 3,015 edits across sessions by what they do:

```
 851 (28%)  general code edit
 579 (19%)  modify annotation/comment
 476 (15%)  tweak 1-3 lines
 380 (12%)  change function signature
 210 ( 6%)  insert code block
 174 ( 5%)  fix import path
 120 ( 3%)  modify type definition
  63 ( 2%)  remove code block
  56 ( 1%)  replace function
  42 ( 1%)  modify config value
  41 ( 1%)  fix test assertion
```

This told us what the ops should be — the recurring edit patterns.

### Step 2: Find action sequences

Extracted tool call sequences and collapsed repeats to find the common pipelines:

```
 522×  search → read → edit       (find it, understand it, change it)
 242×  read → edit → build        (change it, verify it compiles)
 192×  edit → search → read → edit (multi-site edit, bouncing between files)
 295×  read → edit → read → edit  (multi-file edit)
```

This told us what the plans should be — the recurring step sequences.

### Step 3: Identify the tooling gap

Cadmus already had filesystem ops (list, filter, sort, archive), algorithms, text processing, and macOS tasks. But nothing for the things agents spend most of their time doing: searching code, navigating to definitions, making targeted edits, and running builds to verify.

### Step 4: Build and test

Created the ops with `racket_body` implementations that shell out to grep/sed/find. Each op uses `local-require racket/system` to be self-contained. Created plans as both single-op wrappers (for NL discoverability) and multi-step compositions (the distilled sequences). Iterated on NL vocabulary until all 282 plans pass autoregression at 100%.
