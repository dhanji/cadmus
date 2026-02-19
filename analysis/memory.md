# Workspace Memory
> Updated: 2026-02-19T23:03:45Z | Size: 49.5k chars

### Reasoning Engine Project (`/Users/dhanji/src/re`)
- `src/types.rs` — Core type system: OutputType(6), OperationKind(6 with typed I/O), Obligation, ReasoningStep, Goal, ProducedValue, AxisResult, ReasoningOutput, EngineError
- `src/fact_pack.rs` — YAML fact pack loader with FactPackIndex (indexed lookups by axis/entity/claim)
- `data/putin_stalin.yaml` — Putin vs Stalin fact pack: 7 axes, 28 claims, 17 evidence items, 3 relations, 7 uncertainties
- `src/theory.rs` — Theory layer: transitive closure, contradiction detection, hierarchy normalization, claim normalization
- `src/planner.rs` — Planner: goal → obligations → operations, completeness validation, gap detection
- `src/pipeline.rs` — Execution pipeline: load → enrich → plan → execute → assemble. 6 operation executors.
- `src/main.rs` — CLI demo entry point
- `tests/integration.rs` — 9 integration tests covering structural completeness, type correctness, theory features
- 23 total tests (14 unit + 9 integration), all passing

### Derived Uncertainties Feature
- `src/types.rs` [164..252] - `DerivedUncertainty` enum with 4 variants: EvidenceGap, OrdinalBoundary, CrossAxisTension, PropertyClaimMismatch. Display impl, axis()/axes() helpers.
- `src/fact_pack.rs` [32] - `Axis.polarity: Option<String>` — optional polarity field for tension derivation
- `src/theory.rs` [57..58] - `TheoryContext.derived_uncertainties: Vec<DerivedUncertainty>`
- `src/theory.rs` [112..123] - OrdinalBoundary gap=0 derivation
- `src/theory.rs` [163..173] - OrdinalBoundary gap=1 derivation
- `src/theory.rs` [218..250] - EvidenceGap derivation
- `src/theory.rs` [252..296] - CrossAxisTension derivation from polarity metadata
- `src/theory.rs` [298..342] - PropertyClaimMismatch derivation
- `src/pipeline.rs` [348..398] - `execute_surface_uncertainty()` merges 📝 authored + 🔧 derived
- `src/planner.rs` [157..178] - Uncertainty step always planned (no longer conditional)
- `data/tiramisu_cheesecake.yaml` - Dessert fact pack with polarity values on preparation (cost) and versatility (capability)
- 44 total tests (26 unit + 18 integration), all passing

### Strategy Pattern Feature - Codebase Analysis
- **Current architecture**: `pipeline::run()` is the entry point, hardcoded to fact-pack comparison
- **Current planner** (`src/planner.rs`): Hardcoded per-axis obligation generation, not generic type-directed search
- **Current types** (`src/types.rs`): `OutputType` (6 variants), `OperationKind` (6 variants) with `output_type()` and `input_types()` methods
- **Current theory** (`src/theory.rs`): `TheoryContext` with comparisons, divergences, hierarchies, normalized_claims, derived_uncertainties
- **Current pipeline** (`src/pipeline.rs`): load → enrich → plan → execute → assemble; 6 operation executors
- **Test baseline**: 44 tests (26 unit + 18 integration), all passing
- **New modules planned**: `src/registry.rs` (op registry + typed signatures), `src/generic_planner.rs` (type-directed backtracking), `src/algebra.rs` (canonicalization + inference), `src/strategy.rs` (Strategy trait + ComparisonStrategy), `src/coding_strategy.rs` (coding domain strategy)

### Strategy Pattern Feature - Complete
- **New modules**: `src/registry.rs`, `src/generic_planner.rs`, `src/algebra.rs`, `src/strategy.rs`, `src/coding_strategy.rs`
- **registry.rs**: TypeId (string-based), Literal, AlgebraicProperties, RewriteRule, OpSignature, ExecContext/ExecFn, OpEntry, OperationRegistry
- **generic_planner.rs**: GenericGoal (output type + constraints), PlanNode (Op/Leaf DAG), PlanError, type-directed backtracking with cycle detection
- **algebra.rs**: canonicalize() (flatten assoc, sort comm, drop identity, collapse absorbing, dedup idempotent), rewrite rules to fixpoint, dedup_plans(), infer() with Transitive/Symmetric/Reflexive rules, contradiction detection
- **strategy.rs**: ReasonerStrategy trait, ComparisonStrategy wrapping existing pipeline, run_strategy() generic runner, run_comparison() for fact-pack mode
- **coding_strategy.rs**: CodingStrategy with 6 ops (parse_source→AST, analyze_types→TypeSignature, detect_smells→CodeSmell, plan_refactoring→Refactoring, generate_tests→TestCase, compose(associative)), EXAMPLE_LONG_FUNCTION, EXAMPLE_DUPLICATE_CODE
- **pipeline.rs**: Reduced to thin delegation to strategy::run_comparison()
- **Test count**: 111 total (79 unit + 14 new integration + 18 original integration)
- **Backward compatible**: All 18 original integration tests pass unchanged

### Filesystem Type Grammar Feature (plan `filesystem-type-grammar`, committed)
- `src/type_expr.rs` [1..865] — `TypeExpr` enum (Primitive/Constructor/Var), `parse()`, `Display`, `unify()` with occurs check, `Substitution` with compose. 40 unit tests.
- `src/registry.rs` [367..538] — `PolyOpSignature`, `PolyOpEntry`, `PolyOpMatch`, `register_poly()`, `ops_for_output_expr()` with freshening+unification. `poly_ops: Vec<PolyOpEntry>` field on `OperationRegistry`. 4 new tests.
- `src/generic_planner.rs` [413..690] — `ExprGoal`, `ExprLiteral`, `ExprPlanNode` (Op/Leaf/Map/Fold), `plan_expr()` with unification-based backtracking, map insertion (Seq(A)→Seq(B)), fold insertion (Seq(B)→B). 7 new tests.
- `src/fs_types.rs` [1..382] — `build_fs_registry()` with 15 polymorphic ops: list_dir, read_file, write_file, stat, walk_tree, filter, sort_by, extract_archive, pack_archive, concat_seq, rename, move_entry, search_content, find_matching, map_entries. 11 tests.
- `data/macos_fs.yaml` [1..356] — 1 entity, 6 axes, 24 claims. Tools: ls/cat/mv/find/grep/sort/sed/zip/unzip/tar/ditto/mdfind/xattr/brew/tree.
- `src/fs_strategy.rs` [1..402] — `FilesystemStrategy`, `DryRunTrace`, `TraceStep`, `StepKind`. Dry-run only (no shell-out). 7 tests.
- `tests/fs_integration.rs` [1..299] — 7 integration tests: CBZ repack, zip round-trip, dir walk, content search, rename/sort, no-inputs error, registry check.
- `src/main.rs` — Updated to v0.3.0 with Strategy 3 filesystem dry-run demo.
- **Total: 187 tests** (148 unit + 7 fs_integration + 14 generic_planner + 18 original), all passing, zero warnings.
- **Key design**: TypeExpr is additive — existing TypeId-based strategies unchanged. Poly registry sits alongside monomorphic registry.

### Workflow YAML DSL
- `src/workflow.rs` [1-1200] - `WorkflowDef`, `RawStep`, `StepArgs`, `CompiledStep`, `CompiledWorkflow`, `WorkflowError`
  - `parse_workflow()` - custom serde deserializer for step syntax (bare string / scalar / map)
  - `compile_workflow()` - threads types through step chain, each mode, multi-input unification
  - `execute_workflow()` - produces `DryRunTrace` from compiled workflow
  - `run_workflow()` / `run_workflow_str()` - convenience functions
  - `infer_input_type()` - heuristic type inference from input name/value
- `data/workflows/` - example workflow YAML files (find_pdfs, extract_cbz, find_large_files)
- `tests/workflow_tests.rs` - 11 integration tests
- `src/main.rs` - `--workflow <path>` CLI flag, backward compatible no-args demo
- Total tests: 221 (171 unit + 7 fs_integration + 14 generic_planner + 18 integration + 11 workflow)

### YAML Ops Pack System
- `src/registry.rs` [546-695] - `load_ops_pack_str()`, `load_ops_pack()`, `load_ops_pack_str_into()`, `OpsPack`, `OpDef`, `OpDefProperties`, `OpsPackError`
- `data/fs_ops.yaml` - 49 filesystem ops (source of truth, no Rust recompilation needed)
- `data/comparison_ops.yaml` - 6 comparison reasoning ops
- `data/coding_ops.yaml` - 6 code analysis ops
- `src/fs_types.rs` uses `include_str!("../data/fs_ops.yaml")` as fallback + disk-first loading

### Option(a) Constructor
- `src/type_expr.rs:84-87` - `fn option(inner: TypeExpr) -> Self` — first-class constructor

### walk_tree Change
- `walk_tree` now returns `Seq(Entry(Name, a))` (flat)
- `walk_tree_hierarchy` returns `Tree(Entry(Name, a))` (preserves hierarchy)
- `flatten_tree` converts `Tree(a) → Seq(a)`

### Test Counts
- 263 total tests after Phases 1-5 commit (bbaa444)
- 187 unit + 27 fs_integration + 14 reasoner_tests + 18 tiramisu + 17 workflow

### Workflow Type Inference
- `src/workflow.rs:562` - `infer_input_type()` now handles URLs (http/https/ftp), .log/.csv/.json/.yaml/.yml/.toml/.xml/.html/.css/.sh extensions

### Documentation Files
- `ARCHITECTURE.md` [1..340] — concise architecture description: type system, registry (mono+poly), YAML ops packs, fact packs, both planners, strategies, algebra, theory, workflow DSL, module map
- `PLAYBOOK.md` [1..705] — step-by-step guide: adding ops packs, fact packs, workflows, common mistakes, database domain worked example, quick reference schemas

### Power Tools Pack Feature (plan `power-tools-pack`)
- `src/fact_pack.rs` [131..213] — `FactPack::merge()` with entity/axis/relation dedup, `FactPack::merge_all()`, `FactPack::empty()`, `Relation::id()` helper
- `src/fact_pack.rs` [268..296] — `load_fact_pack_str()`, `load_fact_packs()` multi-path loader
- `src/types.rs` [139..166] — `Goal.fact_pack_paths: Vec<String>` (replaces `fact_pack_path`), `Goal::with_single_fact_pack()` convenience
- `data/power_tools_ops.yaml` — 64 ops: 20 git, 6 tmux/screen, 8 jq/yq/csv, 8 awk/sed/cut/tr/paste/tee/column, 10 ps/kill/df/du/lsof/file/uname/uptime, 6 ssh/scp/wget/nc/ping/dig, 6 gzip/xz/base64/openssl
- `data/power_tools.yaml` — 10 entities, 5 axes, 80 claims, 24 evidence, 30+ properties, 8 relations, 5 uncertainties. Entity groups: tmux vs screen, rg vs grep vs ag, jq vs yq, awk vs sed, git solo
- `data/workflows/git_log_search.yaml` — git_log → filter → sort_by
- `data/workflows/process_logs.yaml` — awk_extract → sed_script
- `src/fs_types.rs` [29..60] — `build_fs_registry()` (fs-only) and `build_full_registry()` (fs+power_tools). Workflow system uses full registry.
- `src/workflow.rs` [598..650] — Extended `infer_input_type()` with Repo, File(Json), File(Yaml), File(Csv) types
- `tests/power_tools_tests.rs` — 20 integration tests (ops, facts, composition)
- `PLAYBOOK.md` — Added Section 0 (Audit Existing Packs), Section 3a (Fact Pack Composition), Section 6 (Power Tools Worked Example)
- **Total: 298 tests** (200 unit + 27 fs_integration + 14 generic_planner + 18 integration + 20 power_tools + 19 workflow), all passing, zero warnings

### NL UX Layer Feature (plan `nl-ux-layer`)
- `src/nl/mod.rs` [1..647] — Orchestrator: `process_input()`, `NlResponse` (8 variants: PlanCreated, PlanEdited, Explanation, Approved, Rejected, NeedsClarification, ParamSet, Error), `casual_ack()`, `get_op_explanation()`, `validate_workflow_yaml()`
- `src/nl/normalize.rs` [1..860] — `normalize()`, `NormalizedInput`, `CANONICAL_OPS` (113 ops), `is_canonical_op()`, `build_synonym_table()` (200+ entries), `apply_synonyms()` greedy longest-match, `tokenize()` with path case preservation, `expand_contractions()` (37 patterns), `canonicalize_ordinal()`. 26 tests.
- `src/nl/typo.rs` [1..518] — `SymSpellDict`, `build_domain_dict()` (~280 words), `correct()`, `correct_tokens()`, `generate_deletes()`, `edit_distance()` (Damerau-Levenshtein). Max edit distance 2, prefix length 7. 21 tests.
- `src/nl/intent.rs` [1..907] — `Intent` enum (8 variants: CreateWorkflow, EditStep, ExplainOp, Approve, Reject, AskQuestion, SetParam, NeedsClarification), `EditAction` enum (6 variants), `parse_intent()`, `recognize()`. Closed grammar with ranked patterns. 39 tests.
- `src/nl/slots.rs` [1..597] — `SlotValue` (7 variants: Path, OpName, StepRef, Pattern, Param, Modifier, Keyword), `StepRef`, `Modifier`, `Anchor`, `ExtractedSlots`, `extract_slots()`, `fuzzy_match_op()`, `edit_distance_bounded()`. 24 tests.
- `src/nl/dialogue.rs` [1..997] — `FocusStack`, `FocusEntry` (4 variants), `DialogueState`, `DialogueError`, `build_workflow()`, `apply_edit()` (skip/remove/add/move/change/insert), `workflow_to_yaml()`. 17 tests.
- `tests/nl_tests.rs` [1..466] — 41 integration tests: 11 diverse phrasings, typo correction, 4 explanations, 7 approve + 4 reject, 3 edits, 3 multi-turn conversations, 5 YAML compilation checks
- `src/main.rs` — `--chat` mode added (stdin line-by-line NL processing)
- **Total: 481 tests** (342 unit + 41 nl_integration + 27 fs_integration + 14 generic_planner + 18 integration + 20 power_tools + 19 workflow), all passing, zero warnings
- **Key design**: NL layer always produces WorkflowDef YAML → feeds through workflow::compile_workflow() → engine validates and type-checks. Never bypasses the reasoner.

### NL Bugfixes (plan: nl-bugfixes)
- `src/nl/typo.rs` [337..370] - Added ~60 common English words to SymSpell dictionary to prevent false corrections ("the"→"tee", "thing"→"tee", "that"→"what", "scrap"→"script")
- `src/nl/slots.rs` [259..310] - `is_path()` expanded: bare filenames with known extensions, trailing-slash dirs, URL-like paths. New `is_file_extension()` with ~60 extensions
- `src/nl/dialogue.rs` [193..355] - `build_workflow()` rewritten: categorizes ops as file/dir/entry/url/git, uses correct input names. New helpers: `is_url_op()`, `is_git_repo_op()`, `is_entry_op()`, `is_file_path()`, `dir_of()`, `filename_of()`
- `src/workflow.rs` [631..643] - Added `textdir` input type → `Dir(File(Text))` for search_content pipelines
- `src/nl/normalize.rs` [379..395] - Added 7 single-word git synonyms: clone, commit, checkout, merge, fetch, pull, push
- `src/nl/intent.rs` [384..399] - Compound sentence detection: "skip that, compress X instead" → CreateWorkflow
- `src/nl/intent.rs` [214..221] - Added "scrap that/it" to reject patterns
- `src/nl/intent.rs` [531..546] - Removed "do" from question starters
- `src/nl/mod.rs` [129..145] - Approve/reject now check state.current_workflow before firing
- 525 total tests, 44 new tests added, all passing, zero warnings

### Red-team findings for NL layer
- `src/nl/typo.rs` — "mind"→"find" (ed1), "never"→"need" (ed2), "todo"→"good" (ed2 via DL transposition), "fixme" lost. Root cause: these common words aren't in SymSpell dict
- `src/nl/intent.rs:168-200` — `is_approve()` doesn't match comma-separated phrases like "perfect, ship it" because tokens are ["perfect", "ship", "it"] and multi_approvals has "ship it" not "perfect ship it"
- `src/nl/intent.rs:202-227` — `is_reject()` has "never mind" but tokens become "need find" after typo correction
- `src/nl/mod.rs:129-135` — Approve handler doesn't clear `state.current_workflow`, allowing double-approve
- `src/nl/dialogue.rs:485` — `apply_skip` uses `slots.keywords.first()` which returns "skip" (the action word) instead of ".git" (the target)
- `src/nl/dialogue.rs:668-682` — `resolve_step_index` falls through to op-name lookup when no step_ref, so "remove the step" looks for op "delete" instead of defaulting to last step
- `src/nl/normalize.rs` — "remove" maps to "delete" (synonym), so "remove the step" becomes "delete the step" in canonical tokens

### Red-team plan (nl-redteam) completed
- `BUGS.md` — 12 bugs documented, 10 fixed, 1 deferred (multi-input ops), 1 by-design
- `src/nl/typo.rs` — Added ~20 more words to SymSpell dict (mind, never, todo, fixme, etc.)
- `src/nl/intent.rs` — Added 12 rejection patterns, compound approval logic with filler phrases
- `src/nl/mod.rs` — Clear current_workflow on approve (fixes double-approve)
- `src/nl/dialogue.rs` — apply_skip filters 25 action words; apply_remove defaults to last step for action-verb ops
- `tests/nl_tests.rs` — 21 new red-team integration tests
- **567 total tests, all passing, zero warnings**

### NL Hardening Phase (plan `nl-hardening`, all 6 items done)
- `src/nl/intent.rs` [447-460] - Filler prefix skipping in `try_edit()` (also/and/then/now/plus/etc.)
- `src/nl/intent.rs` [209-213] - Single-word tail check in compound approval (`is_approve()`)
- `src/nl/slots.rs` [142-144] - 15 conversational fillers added to stopwords in `extract_slots()`
- `src/workflow.rs` [558-590] - `has_known_file_extension()` helper function
- `src/workflow.rs` [665-681] - Image file check + catch-all extension check before directory heuristic
- `src/nl/dialogue.rs` [379-399] - `is_file_path()` synced with expanded extension list
- `src/nl/typo.rs` [240-955] - Dictionary expanded from ~388 to ~2000 unique words
- `BUGS.md` - 16 bugs total: 14 fixed, 1 deferred (BUG-009), 1 by-design (BUG-012)
- **591 total tests**, all passing, zero warnings
  - Unit: 406, NL integration: 87, FS integration: 27, Generic planner: 14, Integration: 18, Power tools: 20, Workflow: 19

### File Type Dictionary (Phase 5)
- `data/filetypes.yaml` — 197 entries, 14 categories, YAML-driven single source of truth
- `src/filetypes.rs` — thin loader, OnceLock singleton, `dictionary()` → `lookup()`, `lookup_by_path()`, `is_known_extension()`, `has_known_extension()`, `extensions_for_category()`, `describe_file_type()`, `all_extensions()`
- `src/lib.rs:16` — `pub mod filetypes;`
- `src/workflow.rs:553` — `infer_input_type()` now uses `filetypes::dictionary().lookup_by_path()` instead of hardcoded chains
- `src/nl/slots.rs:308` — `is_file_extension()` delegates to `filetypes::dictionary().is_known_extension()`
- `src/nl/dialogue.rs:379` — `is_file_path()` delegates to `filetypes::dictionary().has_known_extension()`
- Three hardcoded extension lists eliminated, replaced with single YAML source
- 637 total tests (46 new), all passing, zero warnings
- PLAYBOOK.md updated with Section 8: Adding File Types (Pure YAML — No Rust)

### Phase 6: YAML Externalization (completed)
- `data/nl/nl_vocab.yaml` — consolidated word lists (synonyms, contractions, ordinals, approvals, rejections, stopwords, filler_phrases, filler_prefixes)
- `data/nl/nl_dictionary.yaml` — ~2473 frequency-weighted words for SymSpell typo correction
- `src/nl/vocab.rs` — OnceLock singleton loader, disk-first + include_str! fallback
- `src/fs_types.rs:65-93` — `get_op_description()` cached from registry, `OP_DESCRIPTIONS` OnceLock
- `src/nl/normalize.rs:235-260` — `canonical_ops()` derived from registry via OnceLock<HashSet<String>>
- Key design: display names come from ops YAML `description` field, NOT a separate YAML file
- Key design: CANONICAL_OPS derived at load time from registry, NOT hardcoded
- 1337 lines of hardcoded data removed from Rust, 3547 lines of YAML data added
- 670 total tests, 33 new (19 vocab unit + 14 integration), zero warnings

### Semantic Correctness Audit (Phase 7)
- `tests/semantic_tests.rs` — 46 integration tests tracing goal→plan→execution across all 4 strategies
- `SEMANTICS.md` — comprehensive analysis document
- **Bug fixed**: `src/main.rs` used `build_fs_registry()` instead of `build_full_registry()` in `--workflow` CLI mode, breaking power_tools workflows (git_log_search, process_logs)
- **Finding**: Coding strategy's `execute_plan()` in `src/strategy.rs` only returns root node result; intermediate CodeSmell/Refactoring/TypeSignature lost in `assemble()`
- **Finding**: 3 workflow YAMLs have incomplete step sequences: find_large_files (min_size unused), find_duplicates (no dedup), copy_and_organize (no copy step)
- **Finding**: Polymorphic type system allows `Dir(Bytes)` → `Seq(Entry(Name, File(Image)))` via variable unification — this is by design, not a bug
- **Total tests**: 716 (all passing, zero warnings)
- **Commit**: `8754949`

### Shell Executor Module
- `src/executor.rs` [1-2112] - Full shell executor: `op_to_command()`, `generate_script()`, `run_script()`, `shell_quote()`, `extract_archive_format()`
  - `ShellCommand` struct: command string, reads_stdin, writes_stdout
  - `ExecutorError` enum: UnknownOp, MissingParam, ExecFailed
  - `ScriptResult` struct: exit_code, stdout, stderr
  - 113 ops mapped (49 fs_ops + 64 power_tools_ops)
  - Archive format detection from TypeExpr: `File(Archive(a, Cbz))` → "Cbz"
  - `shell_quote()` detects `$WORK_DIR` prefix → double quotes for variable expansion
  - `generate_script()` uses intermediate temp files: `$WORK_DIR/step_N.txt`
  - Single-step workflows: standalone command (no temp file overhead)
  - Multi-step: `set -e`, `mktemp -d`, cleanup trap, step comments
  - Each-mode: `while IFS= read -r _line; do ... done < prev > next`
  - `run_script()` executes via `/bin/sh -c`

### CLI Changes
- `src/main.rs` - Added `--execute` flag for workflow mode
  - `--workflow <path>` always shows generated script
  - `--workflow <path> --execute` runs the script
  - `--execute` without `--workflow` prints error
  - Chat mode: Approved → shows script → prompts "Run this? (y/n)"

### NL Changes
- `src/nl/mod.rs` - `NlResponse::Approved` changed from unit to struct variant
  - Now carries `script: Option<String>`
  - On approve: compiles workflow → generates script → returns in Approved
  - All test assertions updated to use `NlResponse::Approved { .. }`

### Test Files
- `tests/executor_tests.rs` [1-454] - 38 integration tests
  - 11 workflow→script tests (all YAML files)
  - 7 script structure tests (shebang, set -e, trap, work_dir, etc.)
  - 7 real execution tests (ls /tmp, echo, failing command, temp files)
  - 4 NL→script integration tests
  - 5 error/edge case tests
  - 4 archive format detection tests

### Test Counts
- Total: 795 (was 716)
- New: 79 (41 unit in executor.rs + 38 integration in executor_tests.rs)

### Project Rename: reasoning_engine → cadmus (commit `a5156b3`)
- Crate name is now `cadmus` in Cargo.toml
- Binary: `target/release/cadmus`
- All `use reasoning_engine::` → `use cadmus::` across src/ and tests/
- Banners: "CADMUS v0.x.0" (3 locations in src/main.rs)
- Usage: `cadmus --workflow <path.yaml> [--execute]`
- Generic uses of "reasoning" preserved (comparative, cross-entity, cross-domain, etc.)
- 795 tests, zero warnings, 5 commits on main

### NL Robustness: dir aliases + noun patterns (commit `bd48a06`)
- `data/nl/nl_vocab.yaml` — `dir_aliases` (15 entries) and `noun_patterns` (22 entries) sections
- `src/nl/vocab.rs` — `NlVocab.dir_aliases: HashMap<String, String>` and `NlVocab.noun_patterns: HashMap<String, Vec<String>>`
- `src/nl/slots.rs` — steps 9 (dir alias) and 10 (noun pattern) in `extract_slots()`
- Ambiguity: tokens that are both alias+noun prefer alias when no path set, noun when path exists
- SymSpell corrects plurals→singulars, so both forms needed in YAML tables
- 822 total tests, 6 commits on main

### NL: Quoted string support (commit `faf8338`)
- `src/nl/normalize.rs` — `tokenize()` refactored into pre-pass (quote extraction) + `tokenize_segment()`. Only double quotes supported (single quotes conflict with contractions like `don't`).
- `src/nl/slots.rs` — `is_path()` now returns true for tokens containing spaces (from quoted input)
- `src/nl/mod.rs` — `process_input()` re-quotes space-containing tokens when rejoining for second normalize pass
- 830 total tests, 7 commits on main

### NL: Path resolution chain (commit `86320e8`)
- `src/nl/dialogue.rs` — `resolve_path()` function: bare names → `/Volumes/<name>` if exists, else pass through
- Called from `build_workflow()` right after extracting `target_path` from slots
- Handles SD cards, USB drives, external volumes on macOS
- 836 total tests, 8 commits on main

### Red Team NL Pipeline (commit 0a81f50)
- **78 new tests** added across 5 attack surfaces, **914 total** tests
- **4 bugs fixed**:
  1. `src/executor.rs:112-133` — `shell_quote()` $WORK_DIR injection: now validates safe ASCII chars after prefix
  2. `src/executor.rs:112-133` — `shell_quote()` unicode bypass: `is_ascii_alphanumeric()` instead of `is_alphanumeric()`
  3. `src/executor.rs:234-265` — filter `exclude` param: executor now handles with `grep -v`
  4. `src/workflow.rs:562-568`, `src/nl/dialogue.rs:338-398` — type inference reordered: `pathref`→Path before extension lookup, `repo`→Repo before dir check; added `is_path_op()` and `is_seq_op()` in `build_workflow`
- **Test categories**: shell injection (25), chaos (19), conversation flows (8), E2E ops (19), path edge cases (7)
- Known remaining issue: `count` op expects `Seq(a)` but file types like `Csv` don't unify with `Seq(a)` — accepted as design limitation

### Racket Programmer Feature (plan `racket-programmer`, commit `537cb3e`)
- `data/racket_ops.yaml` — 47 ops: 9 arithmetic, 12 list, 9 set, 6 stdio, 8 higher-order, 4 boolean, 4 string. Only `add` has full metasignature template.
- `data/racket_facts.yaml` — 4 entities (op_add/sub/mul/div), 5 axes, 20 claims, 9 evidence, 30+ properties. `symmetric_partner` properties link +/- and */÷. Keyword maps: add/addition/plus/sum/total → add, subtract/subtraction/minus/difference/deduct → subtract, etc.
- `src/registry.rs` [437-476] — `MetaSignature` struct (params, return_type, invariants, category, effects), `MetaParam` struct. `register_poly_with_meta()` method. `OpDef.meta: Option<MetaSignature>`. Backward compatible.
- `src/racket_executor.rs` — `op_to_racket()` maps 47 ops to Racket s-expressions. `generate_racket_script()` produces `#!/usr/bin/env racket` + `#lang racket`. Single-step: bare `(displayln ...)`. Multi-step: `let*` bindings. 24 unit tests.
- `src/racket_strategy.rs` — `infer_symmetric_op()` derives type signature from partner's metasig (invariants NOT transferred). `promote_inferred_ops()` upgrades stubs. `load_keyword_map()` builds NL→op mapping from fact pack. `build_racket_registry()`. 17 unit tests.
- `src/nl/dialogue.rs` [299-338] — Arithmetic handler in `build_workflow()`: extracts numbers from step_refs, builds workflow with x/y params.
- `src/nl/intent.rs` [407-418] — Arithmetic detection in `try_edit()`: if first token is canonical op AND rest looks like numbers, falls through to CreateWorkflow.
- `src/workflow.rs` [581-589] — Number type inference: numeric values or x/y/a/b/n/m/left/right → `TypeExpr::prim("Number")`.
- `src/workflow.rs` [266-281] — `raw_step_to_op_params()` public helper.
- `src/fs_types.rs` [49-72] — `build_full_registry()` now merges racket_ops.yaml alongside fs_ops and power_tools.
- `data/nl/nl_vocab.yaml` [662-689] — Arithmetic synonyms: add/addition/plus/sum/total, subtract/subtraction/minus, multiply, divide, add together, add up, take away.
- `data/nl/nl_dictionary.yaml` — racket_arithmetic section with 14 words for SymSpell.
- `data/workflows/add_numbers.yaml`, `data/workflows/subtract_numbers.yaml` — Example workflow YAMLs.
- `src/main.rs` — `--racket` flag: `cadmus --workflow path.yaml --racket` produces Racket script.
- `tests/racket_tests.rs` — 30 integration tests: ops loading, fact pack, keyword resolution, inference, script generation, NL E2E, full pipeline.
- **Total: 988 tests** (74 new: 44 unit + 30 integration), all passing, zero warnings.

### Type-Symmetric Inference (new feature)
- `src/racket_strategy.rs` — `InferenceKind` enum (OpSymmetric | TypeSymmetric), `find_type_symmetric_peer()`, `infer_type_symmetric_op()`, three-phase `promote_inferred_ops()`
- `data/racket_facts.yaml` — `type_symmetry` axis with `type_symmetry_class=binop` on all 4 arithmetic ops; category gate prevents cross-domain leakage
- `src/registry.rs:646-649` — `get_poly()` uses `rfind` (last registration wins after promotion)
- `tests/trace_pipeline.rs` — annotated pipeline trace for 7 arithmetic inputs, shows inference kind
- Three-phase inference: Phase 1 op-symmetric (subtract←add), Phase 2 type-symmetric (multiply←add via class binop), Phase 3 op-symmetric replay (divide←multiply)

### Data-Driven Executor
- `src/racket_executor.rs` — `op_to_racket()` now takes `&OperationRegistry`, looks up `racket_symbol` and arity from ops pack
- `data/racket_ops.yaml` — all 52 ops have `racket_symbol` field (e.g., `add→"+"`, `set_member→"set-member?"`, `equal→"equal?"`)
- `src/registry.rs` — `OpDef.racket_symbol`, `PolyOpEntry.racket_symbol`, `OperationRegistry::racket_symbol()`, `set_racket_symbol()`
- Generic dispatch: arity 0→`(sym)`, arity 1→`(sym a)`, arity 2+→`(sym a b)`
- 11 special-case ops remain: sort_list, format_string, printf, racket_map, racket_filter, racket_foldl, racket_foldr, racket_for_each, racket_apply, andmap, ormap
- Adding a new op requires only a YAML entry — zero Rust code changes
- 1012 tests, 0 failures, 0 warnings

### Operator Discovery (Phase 0)
- `src/racket_strategy.rs` — `discover_ops()` scans fact pack for entities with `op_name` + `racket_symbol` not in registry, registers placeholders
- `src/racket_strategy.rs` — `meta_to_poly_signature()` builds `PolyOpSignature` from `MetaSignature`
- `data/racket_ops.yaml` — now 49 ops (subtract, multiply, divide removed — discovered from fact pack)
- `src/fs_types.rs` — `build_full_registry()` now runs `promote_inferred_ops` after loading ops packs
- Four-phase inference: Phase 0 (Discovery) → Phase 1 (Op-symmetric) → Phase 2 (Type-symmetric) → Phase 3 (Op-symmetric replay)
- multiply/divide inference path is non-deterministic (depends on HashMap iteration order) — both type-symmetric and op-symmetric paths produce identical results
- 1012 tests, 0 failures, 0 warnings

### List Op Discovery
- `src/registry.rs` — `MetaSignature` now has `type_params: Vec<String>` field (line ~518)
- `src/racket_strategy.rs` — `meta_to_poly_signature()` uses `TypeExpr::parse()` instead of `TypeExpr::prim()` for polymorphic type support
- `src/racket_strategy.rs` — `collect_type_vars()` auto-collects Var nodes from parsed TypeExprs into deduplicated sorted list
- `data/racket_ops.yaml` — 47 ops (was 49). cons and cdr are list anchors with metasigs. remove and list_reverse removed.
- `data/racket_facts.yaml` — 8 entities (4 arithmetic + 4 list). Type symmetry classes: list_elem_to_list (cons, remove), list_to_list (cdr, list_reverse)
- Discovery chain: cons anchor → remove via type-symmetric (list_elem_to_list), cdr anchor → list_reverse via type-symmetric (list_to_list)
- 5 total discovered ops: subtract, multiply, divide (arithmetic) + remove, list_reverse (list)
- 1020 tests, 0 failures

### Comparison Op Discovery
- `data/racket_ops.yaml` - `less_than` is comparison anchor with meta block (Number,Number→Boolean, category=comparison)
- `data/racket_facts.yaml` - 6 new entities: op_less_than, op_greater_than, op_less_than_or_equal, op_greater_than_or_equal, op_string_upcase, op_string_downcase
- `greater_than` discovered via op-symmetric from less_than
- `less_than_or_equal`, `greater_than_or_equal` discovered via type-symmetric (class: comparison_binop)
- `equal` is NOT in comparison_binop class (different input types: Any vs Number)

### String Op Discovery
- `data/racket_ops.yaml` - `string_upcase` is string anchor with meta block (String→String, category=string)
- `string_downcase` discovered via type-symmetric from string_upcase (class: string_to_string)
- `string_length` NOT affected (String→Number, different return type)

### Current Totals
- 47 ops in racket_ops.yaml
- 14 entities in racket_facts.yaml
- 9 discovered ops total: subtract, multiply, divide, remove, list_reverse, greater_than, less_than_or_equal, greater_than_or_equal, string_downcase
- 5 anchors: add (arithmetic), cons (list), cdr (list), less_than (comparison), string_upcase (string)
- 1031 tests passing
- 16 complex programs in tests/complex_programs.rs

### Complex Programs Test
- `tests/complex_programs.rs` - 16 programs covering all domains
- Programs 1-10: arithmetic, list, set, string, higher-order
- Programs 11-13: comparison ops (anchor, op-symmetric, type-symmetric)
- Programs 14-15: string ops (anchor, discovered + chain)
- Program 16: multi-domain chain (arithmetic → string → upcase)

### Shell-Callable Racket Forms (Complete)
- `data/macos_cli_facts.yaml` - CLI fact pack: 12 entities, 45 submodes, 6 output-format classes
- `data/racket_ops.yaml` [500..599] - 6 shell anchor ops (`shell_ls`, `shell_ps`, `shell_find`, `shell_grep`, `shell_du`, `shell_curl`)
- `data/racket_facts.yaml` [69..108] - 12 shell entities (6 anchors + 6 type-symmetric)
- `src/racket_strategy.rs` [636..770] - `discover_shell_submodes()`, `InferenceKind::ShellSubmode`
- `src/racket_executor.rs` [41..105] - `shell_preamble()`, `SHELL_PREAMBLE` constant
- `src/racket_executor.rs` [219..257] - `generate_shell_call()` with nullary/unary/binary dispatch
- `src/fs_types.rs` [50] - `MACOS_CLI_FACTS_YAML` embedded, [88..98] Phase 4 integration
- `tests/shell_callable_tests.rs` - 41 tests covering all layers
- `PLAYBOOK.md` [1223..1435] - Section 10: Shell-Callable Racket Forms
- `SUBSUMPTION.md` - Migration roadmap: 10/49 fs_ops + 4/64 power_tools_ops subsumed
- Total registry: 246 ops after 5 inference phases
- Total test suite: 1076 tests, 0 failures, 0 warnings

### NL → Racket Compilation Fix
- `src/racket_executor.rs` [108-345] - `is_fs_op()`, `fs_op_to_racket()`, `fs_path_operand()`, `fs_shell()`, `glob_to_grep()`, `shell_quote_for_racket()` — bridges 26 filesystem ops (walk_tree, find_matching, filter, sort_by, list_dir, etc.) to Racket shell-based expressions
- `has_shell_ops()` updated to detect both shell-callable ops AND filesystem ops
- All fs op paths use `(shell-quote ...)` for injection safety — never inline user paths into shell command strings
- `tests/repro_screenshot.rs` — 15 tests for NL→Racket roundtrip
- `tests/nl_tests.rs:53-98` — injection tests updated to accept `(shell-quote ...)` pattern
- **1091 total tests**, 0 failures

### Shell Anchor Architecture (Phase 2 context)
- 6 anchor ops in `data/racket_ops.yaml` lines 510-620: shell_ls, shell_ps, shell_find, shell_grep, shell_du, shell_curl
- 6 type-symmetric classes in `data/racket_facts.yaml` lines 715-940: shell_text_lines, shell_tabular, shell_tree, shell_filtered_lines, shell_single_value, shell_byte_stream
- 6 discovered ops via type-symmetric inference: shell_cat, shell_head, shell_tail, shell_sort, shell_df, shell_wc
- All shell ops use flat types: String → List(String) or String → String
- Anchors have metasignatures with `category: shell` and `effects: io`
- `discover_shell_submodes()` in `src/racket_strategy.rs:652` reads `submode_*` properties from CLI facts

### fs_ops Type Signatures (the ones being subsumed)
- `list_dir`: Dir(a) → Seq(Entry(Name, a))
- `walk_tree`: Dir(a) → Seq(Entry(Name, a))
- `read_file`: File(a) → a
- `search_content`: [Seq(Entry(Name, File(Text))), Pattern] → Seq(Match(Pattern, Line))
- `head`/`tail`: [File(Text), Count] → File(Text)
- `sort_by`: Seq(a) → Seq(a)
- `count`: Seq(a) → Count
- `get_size`: Metadata → Size
- `download`: URL → File(Bytes)

### Racket-Native Forms for Intermediate Logic
- `racket_filter` (symbol: "filter"): List(a) → List(a) — in `data/racket_ops.yaml:362`
- `racket_map` (symbol: "map"): List(a) → List(b) — line 354
- `sort_list` (symbol: "sort"): List(a) → List(a) — line 183
- `racket_foldl` (symbol: "foldl"): [b, List(a)] → b — line 370

### Key Design Principle
- Shell ops = "anchors to the world" (bridge in/out of OS)
- Racket-native ops = intermediate transformation/decision logic (filter, sort, map, fold)
- fs_ops like `filter`, `sort_by`, `find_matching`, `unique` should map to Racket-native, NOT shell bridges

### Phase 2 Type Lowering (Completed)
- `src/type_lowering.rs` — new module with two-tier architecture
  - `SUBSUMPTION_MAP` [60-108] — 10 world-touching fs_ops → shell ops
  - `RACKET_NATIVE_MAP` [160-180] — 4 intermediate-logic ops → Racket primitives
  - `DUAL_BEHAVIOR_MAP` [195-202] — 5 ops that switch shell/native based on pipeline position
  - `RESIDUAL_FS_OPS` [260-280] — 15 ops not yet in CLI fact pack
  - Key functions: `lookup_subsumption()`, `lookup_racket_native()`, `lookup_dual_behavior()`, `lookup_residual()`

### racket_executor.rs Rewrite
- Removed: `FS_OPS`, `is_fs_op()`, `fs_op_to_racket()` (monolithic 26-op bridge)
- Added: `subsumed_op_to_racket()` [120-170], `racket_native_op_to_racket()` [185-270]
- Dispatch order in `op_to_racket()`: special ops → shell ops → dual-behavior → subsumption → racket-native → residual → data-driven

### NL Approval Path Fix
- `src/nl/mod.rs:137-186` — added `discover_shell_submodes()` so extract_shell_meta() works for subsumed ops

### Test Count: 1133 (up from 1091)

### Racket Seq Threading Issue
- **Problem**: When a shell-bridge step (walk_tree→shell_find) returns `List(String)` and the next step is a residual op (pack_archive) or binary subsumed op (search_content/grep), the codegen passes the list variable to `shell-quote` which expects a `String`
- **Key insight**: `CompiledStep` has `input_type` and `output_type` as `TypeExpr` but the Racket executor currently ignores them
- **Existing each mechanism**: `src/workflow.rs` has `is_each` on `CompiledStep` and `StepArgs::is_each()` — the shell executor wraps in `while IFS= read -r _line` loops, the Racket executor wraps in `(map (lambda (_line) ...) prev)`
- **Residual ops path**: `src/racket_executor.rs:490-500` — uses `fs_path_operand(prev_binding, input_values)` which returns the raw binding name
- **Subsumed binary ops path**: `src/racket_executor.rs:155-170` — `search_content` (arity==2) passes `path` to `shell-quote` but `path` could be a list
- **Type info available**: `CompiledStep.output_type` has rich types like `Seq(Entry(Name, Bytes))`, can check for `Seq(...)` constructor
- `src/type_lowering.rs` [7000..10000] - `ResidualFsOp` struct with `is_exec` field
- `src/racket_executor.rs` [550..620] - `generate_racket_script()` iterates compiled steps

### Racket Seq→String Bridge (racket-seq-threading plan)
- **Problem solved**: When shell-bridge steps (walk_tree→shell_find) return `List(String)` and the next step is a residual op (pack_archive) or binary subsumed op (search_content/grep), the codegen now correctly bridges the type mismatch
- **Key function**: `is_seq_output(step, registry)` in `src/racket_executor.rs:137-174` — reads shell op metasig `return_type` via subsumption map → registry lookup, falls back to `CompiledStep.output_type.is_seq_or_list()`
- **Bridge strategies**:
  - Residual exec ops (pack_archive, copy): `(string-join (map shell-quote prev) " ")` — batch all files
  - Residual lines ops (stat, diff): `(append-map (lambda (_f) (shell-lines ...)) prev)` — iterate
  - Subsumed binary ops (search_content/grep): `(append-map (lambda (_f) (shell-lines (grep pattern _f))) prev)` — grep each file
  - Subsumed unary ops (read_file/cat): `(append-map (lambda (_f) (shell-lines (cat _f))) prev)` — cat each file
- **API change**: `op_to_racket()` gained `prev_is_seq: bool` parameter; `subsumed_op_to_racket()` also gained it
- `src/type_expr.rs` [92-106] - `TypeExpr::is_seq()`, `is_list()`, `is_seq_or_list()`
- `src/racket_executor.rs` [556-596] - residual ops bridge
- `src/racket_executor.rs` [197-245] - subsumed ops bridge (binary + unary)
- **Tests**: 1149 total passing, 20 new tests (8 unit for is_seq_output, 5 unit for bridges, 7 integration)
- **data/coding_ops.yaml** - created as empty stub to fix pre-existing compile error

### Stress Pipeline Tests
- `tests/stress_pipeline.rs` [1-1281] - 80 stress tests across all 7 pipeline subsystems
  - I1: NL end-to-end (21 tests) - `stress_nl_*`
  - I2: Workflow compiler (7 tests) - `stress_workflow_*`
  - I3: Racket codegen (12 tests) - `stress_racket_*`
  - I4: Generic planner (7 tests) - `stress_planner_*`, `stress_expr_planner_*`
  - I5: Inference engine (10 tests) - `stress_inference_*`
  - I6: Type unification (11 tests) - `stress_unify_*`
  - I7: Type lowering (10 tests) - `stress_subsumption_*`, `stress_residual_*`, `stress_has_lowering_*`, `stress_full_registry_*`

### Key findings from stress testing
- `Literal` struct has `type_id`, `key`, `value`, `metadata` — no `description` field
- `ExprLiteral` has `description` but `Literal` does not
- `poly_op_names()` returns `Vec<&str>` — use `contains(&"name")` not `contains("name")`
- `multiply` inference kind is non-deterministic (OpSymmetric or TypeSymmetric depending on HashMap order)
- `string_downcase` is always TypeSymmetric (reliable for testing)
- `filter` is racket-native only, NOT dual-behavior
- `sort_by`, `head`, `tail`, `count`, `unique` are dual-behavior ops
- `walk_tree` on default Dir produces `Seq(Entry(Name, Bytes))`, not `Seq(Entry(Name, File(Text)))`
- `parse_workflow` catches empty steps before `compile_workflow` runs
- Generic planner uses `TypeId` (string-based), not `TypeExpr` — need custom registries for planner tests

### Type Chain Promotion Fix
- `src/workflow.rs` [375-410] - `is_dir_bytes()`, `steps_need_file_text()` helpers
- `src/workflow.rs` [459-463] - Auto-promotion in `compile_workflow()`: Dir(Bytes) → Dir(File(Text)) when downstream steps need File(Text)
- FILE_CONTENT_OPS: `search_content`, `read_file` trigger promotion
- NL layer already used `textdir` input name for search_content (line 271 of dialogue.rs)
- `list_dir`, `sort_by` etc. do NOT trigger promotion — Dir(Bytes) preserved
- `read_file: each` unwraps File(Text) → Text, so you can't chain read_file:each → search_content (search_content reads files itself)

### Generalized Type Promotion (commit 2513fd6)
- `src/workflow.rs` [0..4800] - `contains_bytes()`, `replace_bytes_with_var()`, `try_promote_bytes()` — unification-based type promotion
- Old special-case helpers `is_dir_bytes()` and `steps_need_file_text()` removed
- Algorithm: replace `Bytes` with fresh `Var("_promote")`, simulate type chain forward, let unification discover what `_promote` should be
- `src/workflow.rs:553-555` — promotion call site in `compile_workflow()`, shadows `input_type` with promoted version
- `Substitution` now imported in workflow.rs
- 4 new tests in `tests/stress_pipeline.rs`: `stress_promotion_no_bytes_no_promotion`, `stress_promotion_bytes_with_polymorphic_only_no_binding`, `stress_promotion_discovered_via_unification`, `stress_promotion_each_mode_read_file`
- Total: 1231 tests, 0 failures, 0 warnings

### Executor Refactor (commit `6697124`)
- `src/shell_helpers.rs` — shared utilities: `shell_quote()`, `glob_to_grep()`, `sed_escape()`, `primary_input()`, `CodegenError` enum
- `src/executor.rs` — `ExecutorError` is now `type ExecutorError = CodegenError`
- `src/racket_executor.rs` — `RacketError` is now `type RacketError = CodegenError`
- `src/executor.rs` — `op_to_command()` decomposed into 16 category functions: `cmd_directory_file_io`, `cmd_filter_sort`, `cmd_archive`, `cmd_search`, `cmd_file_lifecycle`, `cmd_content_transform`, `cmd_metadata`, `cmd_macos`, `cmd_network`, `cmd_git`, `cmd_terminal_mux`, `cmd_structured_data`, `cmd_text_processing`, `cmd_process_system`, `cmd_networking`, `cmd_compression_crypto`
- `src/executor.rs` — `ShellCommand::stdout()`, `::exec()`, `::pipe()` constructors; `source()` and `require_param()` helpers
- `src/type_lowering.rs` — Removed dead `ResidualFsOp`, `RESIDUAL_FS_OPS`, `is_residual_fs_op()`, `lookup_residual()`
- 1237 total tests, all passing, zero warnings

### CLI Rich Formatting (plan `slick-cli`)
- `src/ui.rs` [1-740] — ANSI color helpers, geometric icon constants, formatting primitives. Zero external deps.
  - `color_enabled()` — OnceLock, checks NO_COLOR env var and TERM=dumb
  - `styled()` — core function, applies ANSI codes or returns plain text
  - 30 style functions: bold, dim, italic, red, green, blue, cyan, etc. + bold_* and badge_* variants
  - `icon` module: 40+ flat geometric Unicode icons (✓ ✗ △ ◆ ● ○ ▸ ▰ ◇ ▪ ↔ ⊘ ⊗ ⊢ ⊥ ├ └ │ ─ → ← ↓ λ ⚙)
  - Formatting: banner(), section(), subsection(), kv(), kv_dim(), step(), step_each(), status_ok/fail/warn/info/pending/active(), code_block(), code_block_numbered(), yaml_block(), tree_item/last/cont/blank(), bullet(), error(), warning(), info(), success(), rule(), prompt(), reset()
  - Axis helpers: axis_header(), axis_footer(), claim(), evidence(), similarity(), contrast_line(), uncertainty(), summary_line(), gap_line(), inference_line(), conflict_line()
  - Write helpers: write_step(), write_kv() for Display impls
  - 30 unit tests
- `src/main.rs` — Complete rewrite of all 3 modes (chat, workflow, demo) using ui:: helpers
  - VERSION constant "v0.7.0"
  - All bubbly emojis (✅❌🔍🔧📋📎🔗⚔❓📝📐🤔⚡⚠) replaced with geometric icons
  - Chat mode: compact banner, colored ◆ prompt, YAML in dim code block
  - Workflow mode: progressive status chain (● Loading → ✓ Compiled → ● Executing → ✓ Done)
  - Demo mode: geometric section headers, axis tree formatting
  - Error paths: red ✗ badges
- `src/workflow.rs:912` — CompiledWorkflow Display uses ui:: helpers
- `src/fs_strategy.rs:119-150` — DryRunTrace and TraceStep Display use ui:: helpers
- 1266 total tests, 0 failures, 0 warnings

### Archive Codegen Fix (plan `archive-codegen-fix`)
- `src/workflow.rs` [471-517] — `step_needs_map()`, `lookup_op_inputs()` — type-driven each-mode detection. Compares step.input_type against op signature. OnceLock fallback to full registry.
- `src/workflow.rs` [751-812] — `extract_archive_format()`, `resolve_archive_op()` — format resolution. Extracts fmt from TypeExpr, looks up format_family in filetypes.yaml, rewrites generic op to format-specific.
- `src/workflow.rs` — `is_each` removed from `CompiledStep` struct
- `src/racket_executor.rs` [625-660] — `generate_racket_script()` uses `step_needs_map` instead of `is_each`. Map steps pass `_line` as prev_binding with `prev_is_seq=false`.
- `data/filetypes.yaml` [35-55] — `format_families` section: Cbz→zip, Cbr→rar, TarGz→tar_gz, etc.
- `src/filetypes.rs` [303-315] — `FileTypeDictionary.format_family()` method
- `data/packs/ops/fs_ops.yaml` [82-137] — 9 format-specific ops: extract_zip, extract_tar, extract_tar_gz, extract_tar_bz2, extract_tar_xz, extract_rar, pack_zip, pack_tar, pack_tar_gz
- `src/type_lowering.rs` [136-151] — Subsumption entries for format-specific ops
- `data/packs/facts/macos_cli_facts.yaml` — cli_unzip, cli_zip, cli_unrar, cli_7z entities with submodes
- `data/packs/ops/racket_ops.yaml` [541-599] — shell_unzip, shell_zip, shell_unrar, shell_7z anchor ops
- `tests/archive_codegen_tests.rs` — 19 tests
- `data/workflows/repack_comics.yaml` — example comic repack workflow
- `src/main.rs` [310-325] — Racket registry now runs discover_shell_submodes (Phase 4)
- **Total: 1285 tests**, all passing, zero warnings

### Comic Repack E2E Pipeline (Session 2 completion)
- `data/nl/nl_dictionary.yaml` - SymSpell typo correction dictionary. "repack" was being corrected to "replace" (edit distance 2). Added repack, unpack, flattened.
- `src/nl/mod.rs:93` - `process_input()` does normalize → typo_correct → re-normalize → intent → slots. The typo correction step can change words before synonym mapping.
- `tests/nl_comic_trace.rs` - 5 E2E tests for NL comic repack: full pipeline, Racket codegen, simple extract regression, no-format-hint boundary
- `tests/archive_codegen_tests.rs` - 22 tests total. Added: pack_archive_without_output, find_matching_cbz_narrows_format, find_matching_txt_does_not_narrow
- `tests/semantic_tests.rs:128` - `test_semantic_find_pdfs_execution` updated to accept `File(PDF)` type (more specific than `Bytes`)

### Extract Interleave Fix
- `src/racket_executor.rs:113-118` - `is_extract_op()` helper detects `extract_*` ops
- `src/racket_executor.rs:690-720` - MAP-mode extract codegen creates per-archive temp dirs via `(make-temporary-directory)`, extracts with `-d`, lists via `find -type f`
- Non-MAP (single archive) extract unchanged
- Tests: `test_extract_map_mode_uses_temp_dirs`, `test_single_extract_no_temp_dir` in `tests/archive_codegen_tests.rs`
- Total tests: 1297 (was 1295)

### Isolate Flag (ops-layer collision prevention)
- `src/workflow.rs:545-563` - `CompiledStep` now has `pub isolate: bool` field
- `src/workflow.rs:844-854` - `needs_isolation()` helper: returns true for `extract_*` ops
- `src/workflow.rs:785-797` - Compiler sets `isolate = is_each && needs_isolation(&resolved_op)`
- `src/type_expr.rs:27-33` - `TypeExpr` now implements `Default` (returns `Primitive("Bytes")`)
- `src/racket_executor.rs:680-720` - Racket codegen checks `step.isolate` (not `is_extract_op`)
- `src/executor.rs:991-1017` - Bash codegen: `_td=$(mktemp -d)` + replaces `$WORK_DIR/extracted` with `$_td`
- `is_extract_op()` helper removed from racket_executor.rs
- All CompiledStep construction sites use `..Default::default()` for the new field
- Total tests: 1301

### Bash Executor Removal (plan `remove-bash-executor`)
- `src/executor.rs` — **DELETED** (1425 lines, 37 unit tests). Was: `op_to_command()`, `generate_script()`, `run_script()`, `ShellCommand`, `ScriptResult`, `extract_archive_format()`
- `tests/executor_tests.rs` — **DELETED** (668 lines, 62 integration tests)
- `src/lib.rs` — `pub mod executor;` removed
- `src/main.rs` — `--execute` and `--racket` flags removed. New `--run` flag executes via `racket -e`. Racket is sole codegen target.
- `src/shell_helpers.rs` — `ExecFailed` variant, `sed_escape()`, `primary_input()` removed. `shell_quote()`, `glob_to_grep()`, `CodegenError` kept (used by racket_executor).
- `tests/nl_tests.rs` — `nl_e2e_script()` rewritten to use `racket_executor::generate_racket_script()`. `build_racket_registry()` helper added.
- CLI usage: `cadmus --workflow <path.yaml> [--run]` (was `[--execute] [--racket]`)
- **Total: 1202 tests** (was 1301), 0 failures, 0 warnings