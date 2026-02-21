# Subsumption Plan: Shell-Callable Racket Forms

How the shell-callable Racket forms relate to the legacy `fs_ops.yaml`
and `power_tools_ops.yaml` packs, and the completed migration.

---

## Architecture Overview

```
Layer 1: CLI Fact Pack          data/packs/facts/macos_cli_facts.yaml
         (58 tools, 141 submodes, 11 output-format classes)
              │
              ▼
Layer 2: Anchor Ops + Inference  data/packs/ops/racket_ops.yaml (category: shell)
         (11 anchors → 58 base ops → 141 submode ops)
              │
              ▼
Layer 3: Racket Wrappers         src/racket_executor.rs
         (shell-exec, shell-lines, shell-quote)
```

The key insight: **one fact pack drives the entire pipeline**. Adding a new
CLI tool means adding an entity to the YAML; the inference engine discovers
its ops and submodes automatically.

---

## Two-Tier Type Lowering Architecture

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
                    │   concat → (append l1 l2)     │
                    └──────────┬──────────────────┘
                               │ operates on List(String)
                    ┌──────────┴──────────────────┐
                    │   Shell Bridges (world I/O)  │
                    │   walk_tree → shell_find     │
                    │   list_dir  → shell_ls       │
                    │   search_content → shell_grep│
                    │   read_file → shell_cat      │
                    │   download  → shell_curl     │
                    │   git_log   → shell_git      │
                    │   jq_query  → shell_jq       │
                    │   copy      → shell_cp       │
                    │   delete    → shell_rm       │
                    │   ...100+ more mappings...   │
                    └─────────────────────────────┘
```

### Three Dispatch Tiers in racket_executor.rs

When `op_to_racket()` encounters an op, it checks three tiers in order:

1. **Dual-behavior check** — ops like `head`, `tail`, `sort_by`, `count`
   that are shell bridges when used as the first step (no prev_binding)
   but Racket-native when used in a pipeline (has prev_binding).

2. **Subsumption lookup** — world-touching ops are routed through the
   shell-op codegen infrastructure via `extract_shell_meta()`.

3. **Racket-native lookup** — intermediate-logic ops generate Racket
   primitive expressions.

4. **Data-driven path** — pure Racket ops looked up by symbol and arity
   from the registry.

---

## Op Counts (Phase 3 — Complete)

| Source                        | Ops  | Notes                                    |
|-------------------------------|------|------------------------------------------|
| `data/packs/ops/fs_ops.yaml`    |   49 | Compatibility aliases (embedded)         |
| `data/packs/ops/power_tools_ops.yaml` | 64 | Compatibility aliases (embedded)   |
| `data/packs/ops/racket_ops.yaml`       |   63 | 52 pure Racket + 11 shell anchors       |
| Shell (inferred from facts)  | 200+ | 58 base ops + 141 submodes              |
| **Total registry**           | 350+ | After all inference phases               |

---

## Subsumption Map (Complete)

### fs_ops.yaml → Shell-Callable Forms (49 ops, all covered)

| Category | Ops | Shell Target | Status |
|----------|-----|-------------|--------|
| **Shell bridges** (10) | list_dir, read_file, head, tail, walk_tree, search_content, get_size, count, sort_by, download | shell_ls, shell_cat, shell_head, shell_tail, shell_find, shell_grep, shell_du, shell_wc, shell_sort, shell_curl | **Subsumed** |
| **Formerly residual** (15) | stat, walk_tree_hierarchy, checksum, du_size, open_file, reveal, extract_archive, pack_archive, write_file, copy, delete, rename, move_entry, create_dir, diff | shell_stat, shell_find, shell_shasum, shell_du, shell_open, shell_tar, shell_tee, shell_cp, shell_rm, shell_mv, shell_mkdir, shell_diff | **Subsumed** |
| **Newly subsumed** (18) | create_link, set_permissions, set_owner, replace, get_mtime, get_permissions, get_file_type, spotlight_search, get_xattr, set_xattr, remove_xattr, remove_quarantine, open_with, clipboard_copy, clipboard_paste, read_plist, write_plist, upload, sync, unique | shell_ln, shell_chmod, shell_chown, shell_sed, shell_stat, shell_file, shell_mdfind, shell_xattr, shell_open, shell_pbcopy, shell_pbpaste, shell_plutil, shell_curl, shell_rsync, shell_sort | **Subsumed** |
| **Racket-native** (6) | filter, find_matching, flatten_tree, concat_seq, map_entries, unique (in pipeline) | Racket primitives | **Native** |

### power_tools_ops.yaml → Shell-Callable Forms (64 ops, all covered)

| Category | Ops | Shell Target | Status |
|----------|-----|-------------|--------|
| **Git** (20) | git_init, git_clone, git_add, git_commit, git_log, git_log_range, git_diff, git_diff_commits, git_branch, git_checkout, git_merge, git_rebase, git_stash, git_stash_pop, git_push, git_pull, git_blame, git_bisect, git_tag, git_status | shell_git | **Subsumed** |
| **Tmux/Screen** (6) | tmux_new_session, tmux_attach, tmux_split, tmux_send_keys, screen_new_session, screen_attach | shell_tmux, shell_screen | **Subsumed** |
| **Structured data** (8) | jq_query, jq_filter_seq, jq_transform, yq_query, yq_convert, csv_cut, csv_join, csv_sort | shell_jq, shell_yq, shell_cut, shell_paste, shell_sort | **Subsumed** |
| **Text processing** (8) | awk_extract, awk_aggregate, sed_script, cut_fields, tr_replace, paste_merge, tee_split, column_format | shell_awk, shell_sed, shell_cut, shell_tr, shell_paste, shell_tee, shell_column | **Subsumed** |
| **Process/System** (10) | ps_list, kill_process, pkill_pattern, watch_command, df_usage, du_size, lsof_open, file_type_detect, uname_info, uptime_info | shell_ps, shell_kill, shell_pkill, shell_watch, shell_df, shell_du, shell_lsof, shell_file, shell_uname, shell_uptime | **Subsumed** |
| **Networking** (6) | ssh_exec, scp_transfer, wget_download, nc_connect, ping_host, dig_lookup | shell_ssh, shell_scp, shell_curl, shell_nc, shell_ping, shell_dig | **Subsumed** |
| **Compression/Crypto** (6) | gzip_compress, gzip_decompress, xz_compress, base64_encode, base64_decode, openssl_hash | shell_gzip, shell_xz, shell_base64, shell_openssl | **Subsumed** |

### Racket-Native Map

| fs_ops op         | Racket form                | Notes                                |
|-------------------|----------------------------|--------------------------------------|
| `filter`          | `(filter pred lst)`        | Uses `regexp-match?` predicate       |
| `find_matching`   | `(filter pred lst)`        | Uses `regexp-match?` on filename     |
| `unique`          | `(remove-duplicates lst)`  | Racket built-in                      |
| `flatten_tree`    | identity                   | Shell output is already flat         |
| `concat_seq`      | `(append lst1 lst2)`       | Racket built-in                      |
| `map_entries`     | `(map f lst)`              | Racket built-in                      |

### Dual-Behavior Ops

| fs_ops op         | First step (shell)         | In pipeline (Racket)                 |
|-------------------|----------------------------|--------------------------------------|
| `head`            | `shell_head`               | `(take lst n)`                       |
| `tail`            | `shell_tail`               | `(take-right lst n)`                 |
| `sort_by`         | `shell_sort`               | `(sort lst string<?)`                |
| `count`           | `shell_wc`                 | `(length lst)`                       |
| `unique`          | `shell_sort -u`            | `(remove-duplicates lst)`            |

---

## Output Format Classes (11)

| Class                  | Anchor       | Return Type     | Tools                                    |
|------------------------|-------------|-----------------|------------------------------------------|
| `shell_text_lines`     | `shell_ls`  | `List(String)`  | ls, cat, head, tail, sort, tee, diff, sed, awk, cut, tr, paste, column, base64, xattr, pbpaste, plutil, watch, ping, dig, openssl |
| `shell_tabular`        | `shell_ps`  | `List(String)`  | ps, df, lsof                             |
| `shell_tree`           | `shell_find`| `List(String)`  | find, mdfind                             |
| `shell_filtered_lines` | `shell_grep`| `List(String)`  | grep                                     |
| `shell_single_value`   | `shell_du`  | `String`        | du, wc, stat, shasum, file, uname, uptime|
| `shell_byte_stream`    | `shell_curl`| `String`        | curl                                     |
| `shell_exec`           | `shell_mv`  | `String`        | mv, cp, rm, mkdir, ln, chmod, chown, tar, rsync, open, pbcopy, kill, pkill, gzip, xz |
| `shell_vcs`            | `shell_git` | `List(String)`  | git                                      |
| `shell_structured`     | `shell_jq`  | `List(String)`  | jq, yq                                   |
| `shell_session`        | `shell_tmux`| `String`        | tmux, screen                             |
| `shell_network`        | `shell_ssh` | `List(String)`  | ssh, scp, nc                             |

---

## File Layout

```
data/
  packs/
    facts/
      macos_cli_facts.yaml   ← Primary: 58 CLI tool entities, 141 submodes
      racket_facts.yaml      ← 72 entities (Racket + shell ops)
      power_tools.yaml       ← Developer tools comparison data
      putin_stalin.yaml      ← Autocrats comparison data
      macos_fs.yaml          ← macOS filesystem knowledge
    ops/
      racket_ops.yaml        ← 52 pure Racket + 11 shell anchors
      fs_ops.yaml            ← 49 filesystem typed operations
      power_tools_ops.yaml   ← 64 dev tools typed operations
```

The ops pack files are compiled into the binary via `include_str!` and
serve to register the old op
names (list_dir, git_log, etc.) so the plan compiler and NL layer can
resolve them. Execution is routed through the subsumption map to shell ops.

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
| 2025-02-19 | **Phase 3 complete**: all 113 ops from fs_ops + power_tools subsumed |
| 2025-02-19 | 5 new output-format classes (shell_exec, shell_vcs, shell_structured, shell_session, shell_network) |
| 2025-02-19 | 46 new CLI tool entities (58 total), 141 submodes (up from 45) |
| 2025-02-19 | RESIDUAL_FS_OPS emptied — all ops fully subsumed or Racket-native |
| 2025-02-19 | fs_ops.yaml and power_tools_ops.yaml moved to data/packs/ops/ |
