# Workspace Memory
> Updated: 2026-02-16T11:40:45Z | Size: 15.5k chars

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