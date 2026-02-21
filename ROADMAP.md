# Filesystem Reasoner Roadmap

## Status

- **Phase 1–5: COMPLETE** (see below)
- **Phase 6–8: DEFERRED** (architectural changes, not vocabulary)

## Architecture: YAML Ops Packs

All domain operations are now defined in **YAML ops packs** — no Rust
recompilation needed to add new ops. The layout:

```
data/
  fs.ops.yaml          ← filesystem ops (49 ops)
  comparison.ops.yaml   ← comparative reasoning ops (6 ops)
  coding.ops.yaml       ← code analysis ops (6 ops)
  macos_fs.facts.yaml         ← macOS fact pack (tool knowledge, claims)
  workflows/            ← workflow YAML definitions
```

The `TypeExpr` grammar (`Primitive`, `Constructor`, `Var`) is open — new
types are just strings. `Option(a)` is a first-class constructor alongside
`Seq`, `File`, `Dir`, `Entry`, `Archive`, `Tree`, `Match`.

## Design Principles

- **Ops are data, not code** — YAML ops packs define type signatures and
  algebraic properties. The core engine loads them at runtime.
- **Fact packs are separate** — tool knowledge, claims, and platform
  constraints live in fact packs (e.g., `macos_fs.facts.yaml`).
- **No grammar extensions needed** — new primitives like `Size`, `URL`,
  `Permissions` are just strings. New constructors like `Option(a)` are
  first-class.
- **Pluggable per domain** — a new domain (database, cloud) just drops in
  a `<domain>_ops.yaml` + `<domain>_facts.yaml`.

---

## Phase 1: Unblock Tree + File Lifecycle ✅ COMPLETE

**Priority: Critical — these block basic workflows**

### 1a. `flatten_tree` op

`walk_tree` produces `Tree(Entry(Name, a))` but nothing consumes `Tree`.
You can't filter, sort, or search walk results. This makes `walk_tree`
a dead end in any pipeline.

```
flatten_tree<a>: Tree(a) → Seq(a)
  idempotent: false
  hint: "# flatten tree to sequence"
```

Alternative: change `walk_tree` to return `Seq(Entry(Name, a))` directly,
which is what `find` actually does. `Tree` as a type is only useful if
you need to preserve hierarchy (e.g., for `tree` command display). Consider
keeping both:

```
walk_tree<a>:    Dir(a) → Seq(Entry(Name, a))    # find — flat list
walk_tree_hierarchy<a>: Dir(a) → Tree(Entry(Name, a))  # tree — preserves nesting
```

Decision: probably change `walk_tree` to return `Seq` and rename the current
one to `walk_tree_hierarchy`. Most workflows want flat results.

### 1b. File lifecycle ops

These are the most common CLI operations. Without them you can't express
"copy this", "delete that", "make a directory".

```
copy<a>:        Entry(Name, a), Path → Entry(Name, a)
  idempotent: true
  hint: "cp / cp -R / ditto — copy file or directory"

delete<a>:      Entry(Name, a) → ()
  idempotent: false  (second delete fails)
  hint: "rm / rm -rf — delete file or directory"

create_dir:     Path → Dir(Bytes)
  idempotent: true  (mkdir -p)
  hint: "mkdir -p — create directory (and parents)"

create_link:    Path, Path → Symlink
  idempotent: true
  hint: "ln -s — create symbolic link"

set_permissions: Entry(Name, a), Permissions → Entry(Name, a)
  idempotent: true
  hint: "chmod — set file permissions"

set_owner:      Entry(Name, a), Owner → Entry(Name, a)
  idempotent: true
  hint: "chown — set file owner"
```

New primitives referenced (no grammar changes needed):
- `Permissions` — chmod argument ("755", "u+x")
- `Owner` — chown argument ("user:group")
- `Symlink` — result of ln -s

### 1c. Fact pack additions

Add claims for `cp`, `rm`, `mkdir`, `ln`, `chmod`, `chown` to `macos_fs.facts.yaml`
under the `tools/core_utils` sub-axis. Include common flags, type signatures,
and platform notes (e.g., `cp -R` vs `ditto` for preserving macOS metadata).

### 1d. Workflow examples

```yaml
# Copy and reorganize
workflow: "Copy PDFs to archive"
inputs:
  source: "~/Documents"
  dest: "~/Archive/pdfs"
steps:
  - list_dir
  - find_matching:
      pattern: "*.pdf"
  - copy: $dest

# Clean up temp files
workflow: "Delete old temp files"
inputs:
  path: "/tmp"
steps:
  - walk_tree
  - find_matching:
      pattern: "*.tmp"
  - delete: each
```

---

## Phase 2: Content Transformation ✅ COMPLETE

**Priority: High — needed for text processing workflows**

### 2a. Text ops

```
replace:    Text, Pattern, Text → Text
  idempotent: false  (replacing "a" with "aa" is not idempotent)
  hint: "sed s/old/new/ — find and replace in text"

head<a>:    Seq(a), Count → Seq(a)
  idempotent: false  (head(head(x, 5), 3) ≠ head(x, 5))
  hint: "head -n — take first N elements"
  note: head(n) ∘ head(m) = head(min(n,m)) — absorption law

tail<a>:    Seq(a), Count → Seq(a)
  idempotent: false
  hint: "tail -n — take last N elements"

unique<a>:  Seq(a) → Seq(a)
  idempotent: true
  hint: "sort -u / uniq — remove duplicates"

count<a>:   Seq(a) → Count
  hint: "wc -l — count elements"

diff:       File(Text), File(Text) → Diff
  hint: "diff -u — compare two files"

checksum<a>: File(a) → Hash
  idempotent: true
  hint: "shasum / md5 — compute file hash"
```

New primitives referenced:
- `Count` — numeric count (wc output, head/tail argument)
- `Hash` — checksum value
- `Diff` — diff output

### 2b. Algebraic properties to declare

- `unique` is idempotent
- `checksum` is idempotent
- `sort_by` and `filter` commute (order doesn't matter for the final result)
- `head(n) ∘ head(m) = head(min(n,m))` — absorption (future: planner optimization)
- `pack_archive` and `extract_archive` are inverses (future: round-trip elimination)

### 2c. Fact pack additions

Add claims for `head`, `tail`, `wc`, `uniq`, `diff`, `shasum`, `md5`, `sed`
(sed already has a claim but no op).

### 2d. Workflow examples

```yaml
workflow: "Find duplicate files by checksum"
inputs:
  path: "~/Downloads"
steps:
  - walk_tree
  - checksum: each
  - sort_by: hash
  # (future: group_by would be useful here)

workflow: "Preview large log"
inputs:
  file: "app.log"
steps:
  - read_file
  - tail:
      count: 100
```

---

## Phase 3: Metadata Destructuring ✅ COMPLETE

**Priority: High — makes `stat` useful**

### 3a. The problem

`stat: Path → Metadata` exists but `Metadata` is opaque. You can't extract
size, timestamp, or permissions from it. This means you can't express
"find files larger than 100MB" or "find files modified today".

### 3b. Approach: accessor ops (not grammar changes)

Rather than making `Metadata` a constructor with fields, add accessor ops:

```
get_size:        Metadata → Size
  idempotent: true
  hint: "stat -f %z / du — get file size"

get_mtime:       Metadata → Timestamp
  idempotent: true
  hint: "stat -f %m — get modification time"

get_permissions: Metadata → Permissions
  idempotent: true
  hint: "stat -f %p — get file permissions"

get_file_type:   Metadata → FileType
  idempotent: true
  hint: "file — detect MIME type"
```

New primitives:
- `Size` — already referenced in workflow DSL's `infer_input_type`
- `Timestamp` — modification/creation time
- `FileType` — MIME type or file kind

### 3c. Comparison ops for filtering

To express "files larger than X" you need comparison:

```
compare_size:    Size, Size → Bool
compare_time:    Timestamp, Timestamp → Bool
```

Or more generally, a `predicate` step type in the workflow DSL:

```yaml
steps:
  - stat: each
  - filter:
      min_size: "100MB"
```

This is where the workflow DSL's param system earns its keep — `min_size`
is a filter parameter, not a type-level concept. The planner doesn't need
to understand "100MB"; it just needs to know that `filter` preserves the
sequence type.

---

## Phase 4: macOS-Specific Ops ✅ COMPLETE

**Priority: Medium — differentiates from generic POSIX**

### 4a. Spotlight search

```
spotlight_search: Pattern → Seq(Entry(Name, Bytes))
  idempotent: true
  hint: "mdfind — search using Spotlight index"
```

This is interesting because it's a *leaf* op that produces a sequence
without needing a directory input. The workflow DSL would need to handle
this — currently the first step always consumes the input literal.

### 4b. Extended attributes

```
get_xattr:   Path → Seq(Entry(Name, Text))
  idempotent: true
  hint: "xattr -l — list extended attributes"

set_xattr:   Path, Name, Text → Path
  idempotent: true
  hint: "xattr -w — set extended attribute"

remove_xattr: Path, Name → Path
  hint: "xattr -d — remove extended attribute"

remove_quarantine: Path → Path
  idempotent: true
  hint: "xattr -d com.apple.quarantine — remove quarantine flag"
```

`remove_quarantine` is a specialization but it's the single most common
xattr operation on macOS. Worth having as a named op.

### 4c. Desktop integration

```
open_file:    File(a) → ()
  hint: "open — open file with default application"

open_with:    File(a), App → ()
  hint: "open -a — open file with specific application"

reveal:       Path → ()
  hint: "open -R — reveal in Finder"

clipboard_copy:  Text → ()
  hint: "pbcopy — copy to clipboard"

clipboard_paste: () → Text
  hint: "pbpaste — paste from clipboard"
```

New primitive: `App` — application name.

### 4d. Property lists

```
read_plist:   File(Plist) → Tree(Entry(Name, Bytes))
  hint: "plutil -p / defaults read — read property list"

write_plist:  Tree(Entry(Name, Bytes)) → File(Plist)
  hint: "defaults write — write property list"
```

New primitive: `Plist` — property list content type.

### 4e. Fact pack additions

Expand `macos_fs.facts.yaml` with claims for:
- `open`, `pbcopy`, `pbpaste` — desktop integration
- `plutil`, `defaults` — plist tools
- `hdiutil` — disk image tools
- `codesign` — code signing
- `launchctl` — service management
- SIP restrictions (can't write to /System, /usr except /usr/local)
- APFS features (snapshots, clones)
- BSD vs GNU coreutils differences (macOS ships BSD, Homebrew can install GNU)

---

## Phase 5: Network + Download ✅ COMPLETE

**Priority: Medium — common in automation workflows**

```
download:     URL → File(Bytes)
  hint: "curl -O / wget — download file from URL"

upload:       File(a), URL → ()
  hint: "curl -T / scp — upload file"

sync:         Dir(a), Path → Dir(a)
  idempotent: true
  hint: "rsync -a — synchronize directories"
```

New primitives:
- `URL` — remote resource location

### Fact pack additions

Claims for `curl`, `wget` (brew), `scp`, `rsync`. Include common flags,
authentication patterns, and proxy configuration.

---

## Phase 6: Fact Pack ↔ Planner Integration

**Priority: Medium-High — this is where facts become useful**

Currently the fact pack is loaded but **never consulted during planning**.
The planner picks ops purely by type unification. This means:

- It doesn't know if a tool is installed
- It doesn't know platform constraints (mdfind = macOS only)
- It doesn't know which tool to prefer (ditto vs cp on macOS)
- It can't warn about SIP restrictions

### 6a. Tool availability checking

When the planner selects an op, it should check the fact pack:
- Is the backing tool installed? (confidence > 0)
- Is it available on this platform?
- Does it need `brew install`?

This doesn't change the type system — it's a post-planning validation
or a planning-time filter on candidate ops.

### 6b. Tool preference

When multiple tools can implement an op (e.g., `cp` vs `ditto` for copy),
the fact pack should express preferences:
- `ditto` preserves macOS metadata → prefer on macOS
- `rsync` is more efficient for large dirs → prefer for sync
- `gtar` (GNU tar) supports more formats → prefer if installed

### 6c. Constraint propagation

Fact pack claims about SIP, permissions, and filesystem boundaries should
propagate as constraints:
- Writing to /System → error
- Writing to /usr (except /usr/local) → error
- Modifying files owned by root → needs sudo

---

## Phase 7: Workflow DSL Extensions

**Priority: Lower — current linear pipeline covers most cases**

### 7a. Conditionals

```yaml
steps:
  - list_dir
  - filter:
      if_exists: true
      extension: ".pdf"
```

Or a dedicated `if` step:

```yaml
steps:
  - list_dir
  - if:
      test: extension ".pdf"
      then:
        - read_file: each
      else:
        - skip
```

This is a significant complexity increase. Defer unless workflows
clearly need it.

### 7b. Named intermediates

```yaml
steps:
  - list_dir
  - find_matching:
      pattern: "*.pdf"
  - as: pdf_list          # name this result
  - count                 # count them
  - as: pdf_count
```

Useful for reporting but adds state management to the compiler.

### 7c. Fan-out / fan-in

```yaml
steps:
  - list_dir
  - split:
      - branch:
          - find_matching: { pattern: "*.pdf" }
          - as: pdfs
      - branch:
          - find_matching: { pattern: "*.doc" }
          - as: docs
  - merge: [pdfs, docs]
```

This is essentially dataflow programming. Significant complexity.
Defer to Phase 8+.

### 7d. Error handling

```yaml
steps:
  - extract_archive
  - on_error: skip    # or: abort, retry, fallback
  - read_file: each
```

Needs `Result(a, Error)` or `Option(a)` in the type system, plus
planner awareness of fallibility.

---

## Phase 8: Compression as Distinct from Archiving

**Priority: Low — current archive model works for common cases**

Currently `Archive(content, format)` conflates archiving (bundling files)
with compression (reducing size). In reality:

- `tar` = archive only (no compression)
- `gzip` = compression only (single file)
- `tar.gz` = archive + compression
- `zip` = archive + compression (integrated)

A more precise model:

```
Compressed(content, algorithm)  — gzip, bzip2, xz, zstd
Archive(content, format)        — tar, zip, cpio

# gzip a single file
compress:   File(a), Algorithm → File(Compressed(a, Algorithm))
decompress: File(Compressed(a, Algorithm)) → File(a)

# tar.gz = compress(pack_archive(entries, Tar), Gzip)
```

This is more correct but adds complexity. The current model where
`TarGz` is a format tag works fine for most workflows. Only split
if users need to reason about compression algorithms independently.

---

## Summary: What to Build Next

| Phase | Effort | Impact | Files touched |
|-------|--------|--------|---------------|
| 1. Tree + lifecycle | Small | Critical | fs_types.rs, macos_fs.facts.yaml |
| 2. Content transform | Small | High | fs_types.rs, macos_fs.facts.yaml |
| 3. Metadata access | Small | High | fs_types.rs |
| 4. macOS-specific | Medium | Medium | fs_types.rs, macos_fs.facts.yaml |
| 5. Network | Small | Medium | fs_types.rs, macos_fs.facts.yaml |
| 6. Fact↔Planner | Medium | High | fs_strategy.rs, generic_planner.rs |
| 7. DSL extensions | Large | Medium | workflow.rs |
| 8. Compression | Small | Low | fs_types.rs |

Phases 1-3 are all just adding ops to `build_fs_registry()` and claims
to `macos_fs.facts.yaml`. No grammar changes, no planner changes, no DSL changes.
Pure vocabulary expansion.

Phase 6 is the most architecturally interesting — it connects the fact
pack to the planner, making claims actionable rather than decorative.

Phase 7 is the most complex — it changes the workflow DSL from a linear
pipeline to a control-flow graph.
