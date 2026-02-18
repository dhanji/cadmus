# Subsumption Plan: Shell-Callable Racket Forms

How the new shell-callable Racket forms relate to the existing `fs_ops.yaml`
and `power_tools_ops.yaml` packs, and the roadmap for consolidation.

---

## Architecture Overview

```
Layer 1: CLI Fact Pack          data/macos_cli_facts.yaml
         (12 tools, 45 submodes, 6 output-format classes)
              │
              ▼
Layer 2: Anchor Ops + Inference  data/racket_ops.yaml (category: shell)
         (6 anchors → 12 base ops → 45 submode ops)
              │
              ▼
Layer 3: Racket Wrappers         src/racket_executor.rs
         (shell-exec, shell-lines, shell-quote)
```

The key insight: **one fact pack drives the entire pipeline**. Adding a new
CLI tool means adding an entity to the YAML; the inference engine discovers
its ops and submodes automatically.

---

## Two-Tier Type Lowering Architecture (Phase 2 — Implemented)

The type lowering module (`src/type_lowering.rs`) bridges the gap between
the rich fs_ops type system and the flat shell-callable type system:

```
                    ┌─────────────────────────────┐
                    │   Racket-Native (logic)      │
                    │   filter → (filter pred lst) │
                    │   sort   → (sort lst cmp)    │
                    │   count  → (length lst)      │
                    │   unique → (remove-duplicates)│
                    │   map    → (map f lst)        │
                    └──────────┬──────────────────┘
                               │ operates on List(String)
                    ┌──────────┴──────────────────┐
                    │   Shell Bridges (world I/O)  │
                    │   walk_tree → shell_find     │
                    │   list_dir  → shell_ls       │
                    │   search_content → shell_grep│
                    │   read_file → shell_cat      │
                    │   download  → shell_curl     │
                    │   ...                        │
                    └─────────────────────────────┘
```

### Design Principle

**Shell ops are "anchors to the world"** — they touch the filesystem/network
and return `List(String)` or `String`. The inference engine discovers
symmetric ops from these anchors via the fact pack.

**Racket-native ops handle all intermediate logic** — `filter` is Racket's
`(filter pred lst)`, `sort_by` is `(sort lst string<?)`, `count` is
`(length lst)`. These are first-class Racket forms, no shell subprocess.

### Three Dispatch Tiers in racket_executor.rs

When `op_to_racket()` encounters an fs_op, it checks three tiers in order:

1. **Dual-behavior check** — ops like `head`, `tail`, `sort_by`, `count`
   that are shell bridges when used as the first step (no prev_binding)
   but Racket-native when used in a pipeline (has prev_binding).

2. **Subsumption lookup** — world-touching ops (`walk_tree`, `list_dir`,
   `search_content`, etc.) are routed through the shell-op codegen
   infrastructure via `extract_shell_meta()`.

3. **Racket-native lookup** — intermediate-logic ops (`filter`,
   `find_matching`, `unique`, `flatten_tree`) generate Racket primitive
   expressions.

4. **Residual fs_ops** — world-touching ops not yet in the CLI fact pack
   (`stat`, `checksum`, `open_file`, etc.) use direct shell command strings.

5. **Data-driven path** — pure Racket ops looked up by symbol and arity
   from the registry.

---

## Current Op Counts

| Pack                  | Ops | Notes                                    |
|-----------------------|-----|------------------------------------------|
| `fs_ops.yaml`         |  49 | Filesystem domain (list, read, walk, …)  |
| `power_tools_ops.yaml`|  64 | Git, tmux, jq, awk, sed, ps, du, …      |
| `racket_ops.yaml`     |  58 | 52 pure Racket + 6 shell anchors         |
| Shell (inferred)      | 133 | 6 anchors + 6 type-symmetric + 45 submodes + 76 non-anchor submodes |
| **Total registry**    | 246 | After all 4 inference phases             |

---

## Subsumption Map (Tier 1: Shell Bridges)

### fs_ops.yaml → Shell-Callable Forms

| fs_ops op         | Shell replacement          | Status     | Notes                                |
|-------------------|----------------------------|------------|--------------------------------------|
| `list_dir`        | `shell_ls` / `shell_ls_*`  | **Subsumed** | 6 submodes (long, all, by_time, …) |
| `walk_tree`       | `shell_find` / `shell_find_*` | **Subsumed** | 5 submodes (by_name, by_type, …) |
| `search_content`  | `shell_grep` / `shell_grep_*` | **Subsumed** | 5 submodes (recursive, count, …) |
| `head`            | `shell_head` / `shell_head_*` | **Subsumed** | 2 submodes (lines, bytes)          |
| `tail`            | `shell_tail` / `shell_tail_*` | **Subsumed** | 3 submodes (lines, follow, bytes)  |
| `get_size`        | `shell_du` / `shell_du_*`  | **Subsumed** | 4 submodes (summary, human, …)     |
| `count`           | `shell_wc` / `shell_wc_*`  | **Subsumed** | 4 submodes (lines, words, chars, …)|
| `sort_by`         | `shell_sort` / `shell_sort_*` | **Subsumed** | 4 submodes (reverse, numeric, …) |
| `download`        | `shell_curl` / `shell_curl_*` | **Subsumed** | 5 submodes (headers, silent, …)  |
| `read_file`       | `shell_cat` / `shell_cat_*`| **Subsumed** | 3 submodes (number, squeeze, …)    |

**10 of 49 fs_ops** are subsumed by shell-callable forms.

### Racket-Native Map (Tier 2: Intermediate Logic)

| fs_ops op         | Racket form                | Notes                                |
|-------------------|----------------------------|--------------------------------------|
| `filter`          | `(filter pred lst)`        | Uses `regexp-match?` predicate       |
| `find_matching`   | `(filter pred lst)`        | Uses `regexp-match?` on filename     |
| `unique`          | `(remove-duplicates lst)`  | Racket built-in                      |
| `flatten_tree`    | identity                   | Shell output is already flat         |

### Dual-Behavior Ops (Shell or Native depending on context)

| fs_ops op         | First step (shell)         | In pipeline (Racket)                 |
|-------------------|----------------------------|--------------------------------------|
| `head`            | `shell_head`               | `(take lst n)`                       |
| `tail`            | `shell_tail`               | `(take-right lst n)`                 |
| `sort_by`         | `shell_sort`               | `(sort lst string<?)`                |
| `count`           | `shell_wc`                 | `(length lst)`                       |
| `unique`          | (shell `sort -u`)          | `(remove-duplicates lst)`            |

### Residual Ops (Not yet in CLI fact pack)

| fs_ops op           | Shell command              | Notes                              |
|---------------------|----------------------------|------------------------------------|
| `stat`              | `stat`                     | File metadata                      |
| `walk_tree_hierarchy` | `find`                   | Find with hierarchy                |
| `checksum`          | `shasum -a 256`            | SHA-256 hash                       |
| `du_size`           | `du -sh`                   | Disk usage                         |
| `open_file`         | `open`                     | macOS open                         |
| `reveal`            | `open -R`                  | macOS Finder reveal                |
| `extract_archive`   | `tar xf`                   | Extract archive                    |
| `pack_archive`      | `tar cf archive.tar`       | Create archive                     |
| `copy`              | `cp -r`                    | Copy file/dir                      |
| `delete`            | `rm -rf`                   | Delete file/dir                    |
| `rename`            | `mv`                       | Rename/move                        |
| `move_entry`        | `mv`                       | Move entry                         |
| `create_dir`        | `mkdir -p`                 | Create directory                   |
| `write_file`        | `tee`                      | Write to file                      |
| `diff`              | `diff`                     | File diff                          |

### Ops with No Subsumption

| fs_ops op         | Status     | Notes                                |
|-------------------|------------|--------------------------------------|
| `stat`            | Residual   | No CLI equivalent in current pack    |
| `write_file`      | Residual   | Pure Racket `file_write` preferred   |
| `extract_archive` | Residual   | Needs tar/unzip fact pack entity     |
| `pack_archive`    | Residual   | Needs tar fact pack entity           |
| Others (20+)      | Residual   | macOS-specific or structural ops     |

### power_tools_ops.yaml → Shell-Callable Forms

| power_tools op    | Shell replacement          | Status     | Notes                                |
|-------------------|----------------------------|------------|--------------------------------------|
| `ps_list`         | `shell_ps` / `shell_ps_*`  | Subsumed   | 4 submodes (aux, by_cpu, by_mem, …)  |
| `df_usage`        | `shell_df` / `shell_df_*`  | Subsumed   | 4 submodes (human, inodes, …)        |
| `du_size`         | `shell_du` / `shell_du_*`  | Subsumed   | 4 submodes (summary, human, …)       |
| `wget_download`   | `shell_curl` / `shell_curl_*` | Subsumed | curl subsumes wget use cases         |
| `csv_sort`        | `shell_sort` / `shell_sort_*` | Partial  | sort covers basic cases              |
| Git ops (20)      | —                          | No change  | Needs `git` fact pack entity         |
| tmux/screen (5)   | —                          | No change  | Needs `tmux` fact pack entity        |
| jq/yq (5)         | —                          | No change  | Needs `jq` fact pack entity          |
| awk/sed (5)       | —                          | No change  | Needs `awk`/`sed` fact pack entities |
| Others (24)       | —                          | No change  | Various CLI tools                    |

**4 of 64 power_tools_ops** are fully subsumed today.

### Pure Racket Ops (No Subsumption)

The 52 pure Racket ops (`add`, `cons`, `racket_map`, `string_append`, etc.)
are **not candidates for subsumption**. They are first-class language
primitives, not CLI wrappers.

---

## Phase 1: Coexistence (Current)

Both systems are active simultaneously:

- **Old path**: `fs_ops.yaml` / `power_tools_ops.yaml` → `executor.rs` → shell script
- **New path**: `macos_cli_facts.yaml` → `racket_ops.yaml` → `racket_executor.rs` → Racket program

The old ops remain in the registry and are still resolved by the inference
engine. The new shell ops have distinct names (`shell_ls` vs `list_dir`) so
there are **zero collisions**.

### Why Coexist?

1. **Backward compatibility** — existing workflows reference old op names
2. **Incremental validation** — prove the new architecture before removing the old
3. **Different execution targets** — old ops generate shell scripts; new ops generate Racket

---

## Phase 2: Type Lowering (Implemented)

The type lowering module (`src/type_lowering.rs`) bridges the type gap:

- **fs_ops types**: `Dir(Bytes)` → `Seq(Entry(Name, Bytes))` → `File(Text)` etc.
- **Shell types**: `String` → `List(String)`

When the Racket executor encounters an fs_op in a workflow, it:

1. Checks the **subsumption map** — if the op is world-touching, delegates
   to the shell-op codegen path (same infrastructure as first-class shell ops)
2. Checks the **Racket-native map** — if the op is intermediate logic,
   generates Racket primitive expressions
3. Checks the **dual-behavior map** — ops like `head`/`tail` that switch
   between shell and Racket-native based on pipeline position
4. Falls back to **residual ops** — direct shell command strings for ops
   not yet in the CLI fact pack

### How to Add a New Subsumption

To subsume a new fs_op:

1. **Add the CLI tool to `data/macos_cli_facts.yaml`** as an entity with
   `op_name`, `racket_symbol`, `base_command`, `category_name`, and
   `type_symmetry_class` properties.

2. **Add the anchor op to `data/racket_ops.yaml`** with `category: shell`
   in its meta. The inference engine will discover submodes automatically.

3. **Add the entry to `SUBSUMPTION_MAP` in `src/type_lowering.rs`**:
   ```rust
   SubsumptionEntry {
       fs_op: "my_op",
       shell_op: "shell_my_tool",
       arity: 1,
       note: "my_tool — does something",
   },
   ```

4. **Remove the entry from `RESIDUAL_FS_OPS`** if it was there.

### How to Add a New Racket-Native Mapping

To map an fs_op to a Racket primitive:

1. **Add the entry to `RACKET_NATIVE_MAP` in `src/type_lowering.rs`**:
   ```rust
   RacketNativeEntry {
       fs_op: "my_op",
       kind: RacketNativeKind::FilterPredicate, // or other kind
       note: "description",
   },
   ```

2. **If the op has dual behavior** (shell when first step, native in pipeline),
   add it to `DUAL_BEHAVIOR_MAP` instead.

3. **If a new `RacketNativeKind` is needed**, add it to the enum and
   implement the codegen in `racket_native_op_to_racket()` in
   `src/racket_executor.rs`.

---

## Phase 3: Migration (Future)

When ready to fully consolidate:

1. **Add aliases** — map old op names to new shell ops (e.g., `list_dir → shell_ls`)
2. **Deprecate old ops** — mark subsumed ops with `deprecated: true` in YAML
3. **Update workflows** — rewrite any workflow referencing deprecated ops
4. **Remove old ops** — delete subsumed entries from `fs_ops.yaml` / `power_tools_ops.yaml`

### Expansion Targets

To subsume more ops, add entities to `macos_cli_facts.yaml`:

| CLI Tool | Would Subsume                              | Estimated Submodes |
|----------|--------------------------------------------|--------------------|
| `mv`     | `rename`, `move_entry`                     | 2-3                |
| `cp`     | `copy`                                     | 3-4                |
| `rm`     | `delete`                                   | 2-3                |
| `mkdir`  | `create_dir`                               | 1-2                |
| `diff`   | `diff`                                     | 4-5                |
| `tar`    | `extract_archive`, `pack_archive`          | 5-6                |
| `git`    | All 20 `git_*` ops                         | 20+                |
| `jq`     | `jq_query`, `jq_filter_seq`, `jq_transform` | 5-6              |
| `awk`    | `awk_extract`, `awk_aggregate`             | 3-4                |
| `sed`    | `sed_script`                               | 2-3                |
| `tmux`   | All 4 `tmux_*` ops                         | 4-5                |

Adding these ~11 entities would subsume **~45 additional ops** from the old packs,
and the residual ops list would shrink accordingly.

---

## Decision Log

| Date       | Decision                                                    |
|------------|-------------------------------------------------------------|
| 2025-02-17 | Shell-callable forms return `List(String)` (raw lines)      |
| 2025-02-17 | Submodes are inferred, not first-class ops                  |
| 2025-02-17 | 6 output-format classes for type symmetry                   |
| 2025-02-17 | Coexist in Phase 1; explicit migration path documented      |
| 2025-02-17 | ~12 core tools prove the pattern; expand via fact pack only |
| 2025-02-18 | Phase 2 type lowering implemented in `src/type_lowering.rs` |
| 2025-02-18 | Two-tier architecture: shell bridges + Racket-native logic  |
| 2025-02-18 | Dual-behavior ops switch based on pipeline position         |
| 2025-02-18 | Residual ops for not-yet-in-CLI-fact-pack world-touching ops|
