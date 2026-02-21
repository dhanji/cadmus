# Semantic Correctness Analysis

**Date:** Phase 7 — Semantic Audit  
**Test suite:** `tests/semantic_tests.rs` (46 tests)  
**Total tests:** 716 (all passing, zero warnings)

---

## Overview

This document traces the semantic fidelity of Cadmus's
**goal → plan → execution** pipeline across all four strategies. For each
strategy, we ask: *does the generated plan actually accomplish the stated goal?*

The engine has four strategies:
1. **NL → Filesystem Workflow** — natural language to typed workflow YAML
2. **Comparison** — fact-pack entity comparison with theory layer
3. **Coding** — code analysis, smell detection, refactoring planning
4. **Filesystem** — type-directed planning via generic planner

---

## Strategy 1: NL Pipeline → Workflow → Execution

### Pipeline

```
raw input → normalize (case fold, strip punct, expand contractions, ordinals, synonym map)
          → typo correct (cached SymSpell, 2473-word domain dictionary)
          → re-normalize (apply synonyms to corrected tokens)
          → parse intent (grammar matching)
          → extract slots (paths, patterns, keywords, anchors)
          → dialogue dispatch (create/edit/explain/approve/reject)
          → build_workflow() → YAML
          → compile_workflow() → type-checked pipeline
          → execute_workflow() → dry-run trace
```

### Traced Examples

#### "zip up everything in ~/Downloads"

| Stage | Output |
|-------|--------|
| **Goal** | Archive all files in ~/Downloads |
| **Normalize** | `zip up everything in ~/downloads` → synonym: `pack_archive` |
| **Intent** | `CreateWorkflow { op: pack_archive }` |
| **Slots** | `target_path: ~/Downloads` |
| **Plan (YAML)** | `walk_tree → pack_archive` with `path: ~/Downloads` |
| **Compile** | `Dir(Bytes) → Seq(Entry(Name, Bytes)) → File(Archive(Bytes, ...))` |
| **Execution** | `find → zip/tar -c` |

**Verdict: ✅ Correct.** The type chain is sound: directory → flat entry list → archive.

#### "find all PDFs in ~/Documents"

| Stage | Output |
|-------|--------|
| **Goal** | Locate PDF files recursively |
| **Intent** | `CreateWorkflow { op: find_matching }` |
| **Plan** | `walk_tree → find_matching(*.pdf) → sort_by(name)` |
| **Type chain** | `Dir(Bytes) → Seq(Entry(Name, Bytes)) → Seq(Entry(Name, Bytes))` |

**Verdict: ✅ Correct.** Walk + filter + sort produces a sorted list of matching entries.

#### "extract the archive at ~/comic.cbz"

| Stage | Output |
|-------|--------|
| **Goal** | Extract images from a CBZ comic archive |
| **Normalize** | Synonym: `extract` → `extract_archive` |
| **Plan** | `extract_archive` with `file: ~/comic.cbz` |
| **Type chain** | `File(Archive(File(Image), Cbz)) → Seq(Entry(Name, File(Image)))` |

**Verdict: ✅ Correct.** The filetypes dictionary recognizes `.cbz` as `Archive(File(Image), Cbz)`, and `extract_archive` produces the expected entry sequence.

#### "list ~/Downloads"

| Stage | Output |
|-------|--------|
| **Goal** | List directory contents |
| **Plan** | `list_dir` with `path: ~/Downloads` |
| **Type chain** | `Dir(Bytes) → Seq(Entry(Name, Bytes))` |

**Verdict: ✅ Correct.** Single-step workflow, type-sound.

#### "search for hello in ~/docs"

| Stage | Output |
|-------|--------|
| **Goal** | Search file contents for a keyword |
| **Plan** | `walk_tree → search_content(pattern: hello)` |
| **Type chain** | `Dir(File(Text)) → Seq(Entry(Name, File(Text))) → Seq(Match(Name, ...))` |

**Verdict: ✅ Correct.** Uses `textdir` input name to infer `Dir(File(Text))`.

### Typo Resilience

| Input | Corrected | Result |
|-------|-----------|--------|
| `extrct the archve at ~/comic.cbz` | `extract the archive at ~/comic.cbz` | ✅ `extract_archive` |
| `wlk the tre at ~/Documents` | `walk the tree at ~/Documents` | ✅ `walk_tree` |

Both typo-laden inputs produce correct, compilable workflows.

### Multi-Turn Conversation

```
Turn 1: "zip up everything in ~/Downloads"
  → PlanCreated: walk_tree → pack_archive (path: ~/Downloads)

Turn 2: "skip any subdirectory named .git"
  → PlanEdited: walk_tree → filter(exclude: .git) → pack_archive
  → Path preserved: ~/Downloads still in inputs
  → Workflow still compiles after edit

Turn 3: "lgtm"
  → Approved
```

**Verdict: ✅ Correct.** Edits insert filter steps at the right position (after walk_tree), preserve the original path, and the edited workflow compiles.

### NL Round-Trip Battery

All 7 tested NL inputs produce valid, compilable workflows:

| Input | Primary Op | Compiles |
|-------|-----------|----------|
| zip up everything in ~/Downloads | pack_archive | ✅ |
| find all PDFs in ~/Documents | find_matching | ✅ |
| list ~/Downloads | list_dir | ✅ |
| extract the archive at ~/comic.cbz | extract_archive | ✅ |
| compress file.txt | pack_archive | ✅ |
| walk the tree at ~/projects | walk_tree | ✅ |
| sort ~/Desktop by name | sort_by | ✅ |

---

## Strategy 2: Comparison

### Pipeline

```
Goal (entities + fact_pack_paths)
  → load fact packs (YAML)
  → extract claims, evidence, relations per entity per axis
  → theory layer: derive inferences (comparative scoring) + detect conflicts
  → assemble: claims + evidence + similarities + contrasts + uncertainties + summary per axis
  → gap analysis: check all obligation slots filled
```

### Traced Example: Putin vs Stalin

| Component | Count | Status |
|-----------|-------|--------|
| **Axes** | 7 (legitimacy, coercion, elite_control, ideology, economic_management, information_control, foreign_policy_risk) | ✅ All from fact pack |
| **Claims per axis** | 2 (one per entity) | ✅ Both entities represented |
| **Evidence** | Non-empty for all axes | ✅ |
| **Inferences** | 7 derived comparisons | ✅ Theory layer active |
| **Conflicts** | 6 sharp divergences | ✅ Detected automatically |
| **Similarities** | Present for all axes | ✅ |
| **Contrasts** | Present for all axes | ✅ |
| **Uncertainties** | Present for all axes | ✅ |
| **Summaries** | Present for all axes | ✅ |
| **Gaps** | 0 | ✅ All obligation slots fulfilled |

**Verdict: ✅ Correct.** The comparison strategy produces a complete, structured analysis with all obligation slots filled. The theory layer correctly derives comparative inferences from numeric property scores and detects divergences when the gap exceeds a threshold.

### Traced Example: Tiramisu vs Cheesecake

Same structure, different domain. All axes have claims for both entities, all obligation slots fulfilled.

**Verdict: ✅ Correct.** The strategy is domain-agnostic — it works for any fact pack.

### Error Handling

| Scenario | Result |
|----------|--------|
| Missing fact pack file | `Err("cannot read ...")` |
| Single entity | Works (degenerate comparison) |

---

## Strategy 3: Coding

### Pipeline

```
Source code + goal description
  → build registry (coding.ops.yaml + monomorphic exec bindings)
  → plan: SourceCode → parse_source → AST → analyze_types → TypeSignature
                                           → detect_smells → CodeSmell
                                           → plan_refactoring → Refactoring
                                           → generate_tests → TestCase
  → execute plan tree bottom-up
  → assemble results by type
```

### Traced Example: Extract Method

| Component | Result |
|-----------|--------|
| **Source preserved** | ✅ `result.source == EXAMPLE_LONG_FUNCTION` |
| **Tests generated** | ✅ Non-empty, contains test-related content |
| **Plan trace** | ✅ Shows execution path |

### Known Limitation: Intermediate Results Not Collected

**Finding:** The `run_strategy()` executor returns only the **root node** result (TestCase), not intermediate plan nodes (CodeSmell, Refactoring, TypeSignature). The `assemble()` function expects to categorize results by type, but only receives the final TestCase.

**Impact:** `result.smells`, `result.refactorings`, and `result.type_info` are always empty. The information exists in the plan tree but is lost during bottom-up execution because `execute_plan()` only returns the leaf-to-root result.

**Severity:** Medium. The strategy produces correct final output (tests) but loses intermediate analysis. A fix would require `execute_plan()` to collect all intermediate results, not just the root.

---

## Strategy 4: Filesystem (Type-Directed Planning)

### Pipeline

```
Goal type (TypeExpr) + available literals
  → generic planner: type-directed search with unification
  → plan nodes: sequence of ops that transform available types to goal type
  → execution: dry-run trace with command hints
```

### Traced Example: CBZ Extraction

| Component | Value |
|-----------|-------|
| **Goal** | `Seq(Entry(Name, File(Image)))` |
| **Available** | `comic.cbz : File(Archive(File(Image), Cbz))` |
| **Plan** | `extract_archive` |
| **Type chain** | `File(Archive(File(Image), Cbz)) → Seq(Entry(Name, File(Image)))` |

**Verdict: ✅ Correct.** The planner finds the single-step path via `extract_archive`.

### Traced Example: List Directory

| Component | Value |
|-----------|-------|
| **Goal** | `Seq(Entry(Name, Bytes))` |
| **Available** | `/home/user/docs : Dir(Bytes)` |
| **Plan** | `list_dir` |
| **Type chain** | `Dir(Bytes) → Seq(Entry(Name, Bytes))` |

**Verdict: ✅ Correct.**

### Type System Property: Polymorphic Unification

The generic planner uses polymorphic type variables. When an op has signature `Dir(a) → Seq(Entry(Name, a))`, the variable `a` unifies with any type. This means:

- `Dir(Bytes)` + `list_dir` → `Seq(Entry(Name, Bytes))` ✅
- `Dir(File(Text))` + `list_dir` → `Seq(Entry(Name, File(Text)))` ✅

This is correct behavior for a Hindley-Milner-style type system. The planner is sound: if it finds a plan, the types are consistent.

### Unreachable Types

When the goal type has no production path from available literals, the planner correctly fails:

```
Goal: CompletelyFakeType(Nonexistent)
Available: Dir(Bytes)
Result: Err (no producer)
```

---

## Workflow YAML Files: Semantic Audit

### All 11 Workflows

| Workflow | Steps | Compiles | Semantically Correct | Notes |
|----------|-------|----------|---------------------|-------|
| `extract_cbz.yaml` | extract_archive | ✅ | ✅ | CBZ → images |
| `find_pdfs.yaml` | list_dir → find_matching → sort_by | ✅ | ✅ | Finds PDFs by pattern |
| `find_large_files.yaml` | list_dir → sort_by | ✅ | ⚠️ | `min_size` input unused — no filter step |
| `find_duplicates.yaml` | walk_tree → filter → sort_by | ✅ | ⚠️ | No checksum/dedup step — just lists |
| `cleanup_temp.yaml` | walk_tree → filter(*.tmp) → sort_by | ✅ | ✅ | Finds .tmp files |
| `copy_and_organize.yaml` | walk_tree → filter(.pdf) → sort_by | ✅ | ⚠️ | `dest` input unused — no copy step |
| `download_and_extract.yaml` | extract_archive → sort_by | ✅ | ✅ | Extracts and sorts |
| `preview_log.yaml` | walk_tree → filter(.log) → sort_by | ✅ | ✅ | Finds log files |
| `spotlight_find.yaml` | spotlight_search → sort_by | ✅ | ✅ | Spotlight query |
| `git_log_search.yaml` | git_log → filter → sort_by | ✅ | ✅ | Git log with $pattern expansion |
| `process_logs.yaml` | awk_extract → sed_script | ✅ | ✅ | Text pipeline |

### Workflow Design Gaps (Not Compiler Bugs)

1. **`find_large_files.yaml`**: Has `min_size: 100MB` input but no filter step references it. The workflow lists and sorts but doesn't filter by size.

2. **`find_duplicates.yaml`**: Has `extension: *` filter (no-op) and no checksum or dedup operation. The workflow walks and sorts but doesn't actually find duplicates.

3. **`copy_and_organize.yaml`**: Has `dest: /tmp/organized` input but no copy/move step. The workflow walks and filters the source but doesn't copy to the destination.

These are aspirational workflows — they describe the intent in their names but the step sequences are incomplete. The type system and compiler correctly process what's there; the gap is in the workflow definitions themselves.

---

## Bug Found and Fixed

### `main.rs` Registry Mismatch

**Bug:** The `--workflow` CLI mode used `build_fs_registry()` (fs_ops only) instead of `build_full_registry()` (fs_ops + power_tools). This caused `git_log_search.yaml` and `process_logs.yaml` to fail with "unknown operation" errors in CLI mode.

**Root cause:** `run_workflow_mode()` in `main.rs` manually compiled workflows with the wrong registry. The `run_workflow()` function in `workflow.rs` already used `build_full_registry()`, so tests passed.

**Fix:** Changed `build_fs_registry()` → `build_full_registry()` in `main.rs`.

**Impact:** 2 of 11 workflow YAML files were broken in CLI mode but worked in tests.

---

## Summary of Findings

### Semantic Correctness Scorecard

| Strategy | Goal→Plan | Plan→Execution | Type Soundness | Overall |
|----------|-----------|----------------|----------------|---------|
| NL → Filesystem | ✅ | ✅ | ✅ | **✅ Sound** |
| Comparison | ✅ | ✅ | N/A (fact-based) | **✅ Sound** |
| Coding | ✅ | ⚠️ (intermediates lost) | ✅ | **⚠️ Partial** |
| Filesystem (generic) | ✅ | ✅ | ✅ | **✅ Sound** |

### Key Properties Verified

1. **Type soundness**: Every compiled workflow has a valid type chain from input to output. The compiler catches type mismatches at compile time (e.g., `list_dir → extract_archive` fails).

2. **NL fidelity**: Natural language inputs map to the correct operations. Synonym expansion, typo correction, and intent parsing produce semantically appropriate workflows.

3. **Typo resilience**: Misspelled inputs (`extrct`, `archve`, `wlk`, `tre`) are corrected to valid ops and produce compilable workflows.

4. **Multi-turn coherence**: Edits preserve the original path and intent. Filter steps are inserted at the correct position. The edited workflow compiles.

5. **Error handling**: Ambiguous input → `NeedsClarification`. Unknown ops → compile error. Missing fact packs → load error. Double approve → clarification. Edit without plan → clarification.

### Known Gaps

1. **Coding strategy intermediate results**: `smells`, `refactorings`, `type_info` are empty because `execute_plan()` only returns the root node result.

2. **Three workflow YAML files have incomplete step sequences**: `find_large_files`, `find_duplicates`, `copy_and_organize` describe more than they implement.

3. **No runtime execution**: All filesystem workflows produce dry-run traces, not actual file operations. This is by design (safety), but means semantic correctness is verified at the type level, not the I/O level.
