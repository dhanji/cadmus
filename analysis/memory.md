# Workspace Memory
> Updated: 2026-02-28T00:09:00Z | Size: 24.6k chars

## Core Architecture

### Type System
- `src/type_expr.rs` — `TypeExpr` enum (Primitive/Constructor/Var), `parse()`, `Display`, `unify()` with occurs check, `Substitution` with compose
  - `is_seq()`, `is_list()`, `is_seq_or_list()` — type shape queries
  - `Default` impl returns `Primitive("Bytes")`
  - `option(inner)` — first-class Option(a) constructor
- `src/types.rs` — Core reasoning types: OutputType(6), OperationKind(6), Obligation, ReasoningStep, Goal, ProducedValue, AxisResult, ReasoningOutput, EngineError
  - `DerivedUncertainty` enum: EvidenceGap, OrdinalBoundary, CrossAxisTension, PropertyClaimMismatch
  - `Goal.fact_pack_paths: Vec<String>`, `Goal::with_single_fact_pack()`

### Operation Registry
- `src/registry.rs` — `OperationRegistry` with mono + poly ops
  - `PolyOpSignature`, `PolyOpEntry`, `PolyOpMatch`, `register_poly()`, `ops_for_output_expr()`
  - `MetaSignature` (params, return_type, invariants, category, effects, type_params), `MetaParam`
  - `OpDef` fields: `racket_symbol`, `racket_body`, `input_names`, `variadic`, `meta`
  - `load_ops_pack_str()`, `load_ops_pack_str_into()` — YAML ops pack loader
  - `get_poly()` uses `rfind` — last registration wins (inference shadows stubs)
  - `set_variadic()`, `set_racket_body()`, `set_input_names()`, `set_racket_symbol()`

### Registry Builder
- `src/fs_types.rs` — `build_fs_registry()` (fs-only), `build_full_registry()` (all packs + inference)
  - Loads: fs.ops, power_tools.ops, racket.ops, algorithm.ops, text_processing.ops, statistics.ops, macos_tasks.ops
  - Runs 5 inference phases via `promote_inferred_ops()` + `discover_shell_submodes()`
  - `get_op_description()` cached via `OP_DESCRIPTIONS` OnceLock
  - `MACOS_CLI_FACTS_YAML` embedded for Phase 4

### Plan Compiler
- `src/plan.rs` — `PlanDef`, `PlanInput`, `RawStep`, `StepArgs`, `CompiledStep`, `CompiledPlan`, `PlanError`
  - `compile_plan()` — threads types through step chain, multi-input unification
  - `load_plan()` — detects `.sexp` extension, routes to sexpr parser
  - `try_promote_bytes()` — unification-based: replaces Bytes with Var, simulates chain forward
  - `step_needs_map()`, `lookup_op_inputs()` — type-driven each-mode detection (OnceLock fallback)
  - `extract_archive_format()`, `resolve_archive_op()` — format resolution via filetypes.yaml
  - `infer_input_type()` — heuristic type inference from name/value/extension
  - `resolve_type_hint()` — falls through to `TypeExpr::parse()` for unrecognized types
  - `count_value_params()` — reset step detection (params fully specify inputs)
  - `$step-N` back-references validated, exempt from `$var` validation
  - `needs_isolation()` — true for `extract_*` ops only
  - `CompiledStep` fields: `isolate: bool`, `clause_params: HashMap<String, Vec<serde_yaml::Value>>`
  - `StepParam::Clauses(Vec<serde_yaml::Value>)` for cond/for_star
  - `PlanInput::bare()`, `PlanInput::typed()`, `PlanInput::from_legacy()`

### S-expression Plan DSL
- `src/sexpr.rs` — Full sexpr parser + lowering, sole plan parser entry point
  - `parse_sexpr_to_plan()` — public API
  - `plan_to_sexpr()` — serializer with `infer_type_for_serialization()`, `type_str_to_sexpr()`, `type_expr_to_sexpr()`
  - 10 core forms: define, bind, let, for/fold, for/each, cond, when, range, ref, set!, make, list
  - `LowerCtx` tracks: ref_prefix, vectors HashSet, env (var→ref mapping)
  - Scheme operators mapped: `+`→add, `-`→subtract, `*`→multiply, `/`→divide, `=`→equal, `<`→less_than
  - `check_no_recursion()` rejects self-calls
  - Keyword args: `:keyword value` pairs, `:each` modifier
  - Bare `(op)` call emitted when step op matches plan name and all args are `$var` refs
  - Sexpr type syntax: `(File (Archive (File Image) Cbz))` not `File(Archive(File(Image), Cbz))`

### Racket Codegen
- `src/racket_executor.rs` — `op_to_racket()`, `generate_racket_script()`
  - `build_racket_registry()` — shared helper, loads all ops packs + facts + shell submodes
  - Dispatch order: special → shell → dual-behavior → subsumption → racket-native → algorithm atom → data-driven
  - `subsumed_op_to_racket()` — bridges fs_ops to shell ops with seq handling
  - `racket_native_op_to_racket()` — intermediate logic ops
  - `generate_shell_call()` — nullary/unary/binary dispatch
  - `shell_preamble()`, `SHELL_PREAMBLE` constant
  - `is_seq_output()` — reads shell op metasig return_type, falls back to CompiledStep.output_type
  - Seq→String bridges: string-join for exec ops, append-map for lines/binary ops
  - `emit_racket_body_defines()` + `emit_raw_step_defines()` — recursive sub-step scanning
  - `compile_single_substep()` uses `input_names` order from registry (not alphabetical)
  - `racket_value()` handles `$step-N` → `step-N`, `+inf.0`, `-inf.0`, `+nan.0`, `-nan.0`
  - `get_one_operand()` prefers explicit `$step-N` refs over prev_binding
  - Codegen handlers: for_fold, for_list, for_sum, for_product, for_and, for_or, iterate, conditional, if_then, let_bind, begin, define, build_list, cond, for_range_down, when_do, for_star, ordered_params
  - Variadic ops split elements on whitespace
  - Empty scalar args → no-arg calls (e.g., `random: ""` → `(random)`)
  - MAP-mode extract: per-archive temp dirs via `(make-temporary-directory)`
  - 11 special-case ops: sort_list, format_string, printf, racket_map, racket_filter, racket_foldl, racket_foldr, racket_for_each, racket_apply, andmap, ormap

### Type Lowering
- `src/type_lowering.rs` — two-tier architecture
  - `SUBSUMPTION_MAP` — world-touching fs_ops → shell ops (includes format-specific archive ops)
  - `RACKET_NATIVE_MAP` — 4 intermediate-logic ops → Racket primitives
  - `DUAL_BEHAVIOR_MAP` — 5 ops (sort_by, head, tail, count, unique) switch shell/native by context
  - `lookup_subsumption()`, `lookup_racket_native()`, `lookup_dual_behavior()`

### Inference Engine
- `src/racket_strategy.rs` — 5-phase inference pipeline
  - Phase 0: `discover_ops()` — scans fact pack for entities with `op_name` + `racket_symbol` not in registry
  - Phase 1: Op-symmetric inference (subtract←add, greater_than←less_than)
  - Phase 2: Type-symmetric inference (multiply←add via class binop, remove←cons via list_elem_to_list)
  - Phase 3: Op-symmetric replay (divide←multiply)
  - Phase 4: `discover_shell_submodes()` — reads `submode_*` properties from CLI facts
  - `InferenceKind` enum: OpSymmetric, TypeSymmetric, ShellSubmode
  - `meta_to_poly_signature()` uses `TypeExpr::parse()` for polymorphic type support
  - `collect_type_vars()` auto-collects Var nodes into deduplicated sorted list
  - `load_keyword_map()` builds NL→op mapping from fact pack
  - multiply/divide inference path is non-deterministic (HashMap iteration order) — both paths produce identical results
  - 9 discovered ops: subtract, multiply, divide, remove, list_reverse, greater_than, less_than_or_equal, greater_than_or_equal, string_downcase
  - 5 anchors: add (arithmetic), cons (list), cdr (list), less_than (comparison), string_upcase (string)

### Shell Helpers
- `src/shell_helpers.rs` — `shell_quote()`, `glob_to_grep()`, `CodegenError` enum
  - `shell_quote()` special handling for `$WORK_DIR` — double quotes, validates safe ASCII after prefix

### Calling Frame
- `src/calling_frame.rs` — `CallingFrame` trait, `DefaultFrame` impl
  - `codegen(&plan) → Result<String>` — compile + generate script
  - `invoke(&plan) → Result<Execution>` — codegen + execute
  - `run_script(&script) → Result<Execution>` — execute existing script
  - `Execution` struct: script, stdout, stderr, success, exit_code
  - `InvokeError`: CompileError, CodegenError, ExecError
  - `exec_racket_script()` — temp-file racket execution
  - Atomic counter for unique temp file names (parallel test safety)

## Reasoning Engine

### Fact Packs
- `src/fact_pack.rs` — YAML fact pack loader with FactPackIndex
  - `FactPack::merge()` with entity/axis/relation dedup, `merge_all()`, `empty()`
  - `load_fact_pack_str()`, `load_fact_packs()` multi-path loader
  - `Axis.polarity: Option<String>` for tension derivation
  - `CompactValue` enum (Simple/Extended), `CompactProperties` (BTreeMap³), `expand_compact_properties()`
  - Compact format only: `compact_properties:` (entity → axis → key → value)

### Theory Layer
- `src/theory.rs` — `TheoryContext`: transitive closure, contradiction detection, hierarchy normalization, claim normalization
  - `derived_uncertainties: Vec<DerivedUncertainty>` — OrdinalBoundary, EvidenceGap, CrossAxisTension, PropertyClaimMismatch

### Planners
- `src/planner.rs` — Per-axis obligation generation, completeness validation, gap detection
- `src/generic_planner.rs` — Type-directed backtracking with cycle detection
  - `GenericGoal`, `PlanNode` (Op/Leaf DAG), `PlanError`
  - `ExprGoal`, `ExprLiteral`, `ExprPlanNode` (Op/Leaf/Map/Fold), `plan_expr()` with unification

### Strategies
- `src/strategy.rs` — `ReasonerStrategy` trait, `ComparisonStrategy`, `run_strategy()`, `run_comparison()`
- `src/coding_strategy.rs` — `CodingStrategy` with 6 ops (parse_source→AST, etc.)
- `src/fs_strategy.rs` — `FilesystemStrategy`, `DryRunTrace`, `TraceStep`, `StepKind` (dry-run only)
- `src/pipeline.rs` — Thin delegation to strategy::run_comparison()

### Algebra
- `src/algebra.rs` — canonicalize() (flatten assoc, sort comm, drop identity, collapse absorbing, dedup idempotent), rewrite rules to fixpoint, dedup_plans(), infer() with Transitive/Symmetric/Reflexive

## NL Pipeline

### Module Structure (current)
- `src/nl/mod.rs` [961 lines] — `process_input()`, `try_earley_create()`, `try_detect_edit()`, handle_approve/edit/explain
  - Pre-Earley short-circuit: scans phrase tokens for algorithm ops
  - Multi-strategy plan-name matching: consecutive joins, 2/3-token pairs, pluralization, token overlap
  - `find_plan_by_token_overlap()` — order-independent word matching with NAME_STOPWORDS
  - Early first-token check: tries first content token as direct plan name
  - Recipe query check runs BEFORE try_explain
  - `try_recipe_query()`, `handle_recipe_query()` — 6 command-query patterns
- `src/nl/dialogue.rs` [1121 lines] — `DialogueState`, `FocusStack`, `EditAction`, slot types + `extract_slots()`, `apply_edit()`, `plan_to_sexpr()`
  - `resolve_path()` — bare names → `/Volumes/<name>` if exists (macOS volumes)
- `src/nl/normalize.rs` [401 lines] — `normalize()`, `tokenize()`, `expand_contractions()`, `canonicalize_ordinal()`, `is_canonical_op()`
  - `tokenize()` has pre-pass for quote extraction (double quotes only)
  - `canonical_ops()` derived from registry via OnceLock
- `src/nl/vocab.rs` [283 lines] — `NlVocab` singleton (contractions, ordinals, stopwords, filler_phrases, dir_aliases, noun_patterns)
- `src/nl/lexicon.rs` [793 lines] — Earley lexicon + `is_approve()`, `is_reject()`, `try_explain()`
  - `TokenClassifier`, `PhraseGroup`, `PhraseGroupEntry`
- `src/nl/earley.rs` [788 lines] — Earley parser engine (predict/scan/complete, parse forest, ranked output)
- `src/nl/grammar.rs` [380 lines] — Command grammar builder (30+ rules, Verb Object Modifiers)
- `src/nl/intent_ir.rs` [687 lines] — IntentIR schema + `parse_trees_to_intents()`
- `src/nl/intent_compiler.rs` [1034 lines] — IntentIR → PlanDef
  - `compile_ir()` short-circuits for plan files and algorithm atoms
  - `compile_algorithm_op()`, `compile_algorithm_op_by_name()`
  - `try_load_plan_file()`, `try_load_plan_sexpr()` — search data/plans/ and data/plans/algorithms/
  - `generate_plan_name()` via `slugify_path()`
- `src/nl/phrase.rs` [319 lines] — Greedy longest-match phrase tokenizer with stopword stripping (55 stopwords)
- `src/nl/typo.rs` [612 lines] — SymSpell typo correction (max edit distance 2, prefix length 7)
- `src/nl/recipes.rs` [1-430] — `RecipeIndex`, `Recipe`, `recipe_index()` OnceLock
  - Builds from CLI fact pack: base_command + submode_* + keyword mappings
  - `score_recipe()` — distinct token matching, fuzzy match, coverage bonus

### NL Pipeline Flow
normalize → typo_correct → re-normalize → lexicon approve/reject/explain → try_detect_edit → recipe query → Earley parse
- Earley is sole path for plan creation; approve/reject/explain/edit use keyword/pattern matching
- Unimplemented Earley actions fall through to NeedsClarification

### NL Data Files
- `data/nl/nl_vocab.yaml` [488 lines] — contractions, ordinals, stopwords, filler_phrases, dir_aliases (15), noun_patterns (22)
- `data/nl/nl_lexicon.yaml` — 104 base verbs, 1186 total words, 116 phrase_groups, approvals, rejections, explain_triggers
- `data/nl/nl_dictionary.yaml` — SymSpell frequency dictionary (~2500+ words)

### NL Public API
- `process_input(input: &str, state: &mut DialogueState) -> NlResponse`
- `NlResponse` variants: PlanCreated, PlanEdited, Explanation, Approved{script}, Rejected, NeedsClarification, ParamSet, Error
- `DialogueState`: current_plan, focus stack, turn_count, last_intent, alternative_intents

## Data Files

### Ops Packs
- `data/packs/ops/fs.ops.yaml` — filesystem ops (49 base + 11 macOS tasks + 9 format-specific archive)
- `data/packs/ops/power_tools.ops.yaml` — 64 ops: git, tmux/screen, jq/yq/csv, awk/sed, ps/kill/df, ssh/scp, gzip/xz
- `data/packs/ops/racket.ops.yaml` — ~47 ops: arithmetic, list, set, stdio, higher-order, boolean, string, shell anchors
- `data/packs/ops/algorithm.ops.yaml` — 154 opaque ops with `racket_body` and `input_names`
- `data/packs/ops/text_processing.ops.yaml` — 16 ops: string_split, csv_parse_row, char_frequency, etc.
- `data/packs/ops/statistics.ops.yaml` — 19 ops: mean, median, mode, variance, stddev, correlation, etc.
- `data/packs/ops/macos_tasks.ops.yaml` — 13 ops with `racket_body` (shadows fs.ops for codegen)
- `data/packs/ops/comparison.ops.yaml` — 6 comparison reasoning ops
- `data/packs/ops/coding.ops.yaml` — 6 code analysis ops (stub)

### Fact Packs
- `data/packs/facts/racket.facts.yaml` — 14 entities (4 arithmetic, 4 list, 6 comparison/string + 12 shell), type_symmetry axis
- `data/packs/facts/macos_cli.facts.yaml` — CLI entities with submodes, output-format classes
- `data/packs/facts/macos_fs.facts.yaml` — macOS filesystem tools
- `data/packs/facts/putin_stalin.facts.yaml` — Example comparison fact pack

### Other Data
- `data/filetypes.yaml` — 197 entries, 14 categories, `format_families` section
- `data/domains.yaml` — 19 algorithm categories + pipeline, 11 suggested_next domains
- `data/plans/` — 24 pipeline .sexp plans + `data/plans/algorithms/` (194+ algorithm plans across 18 categories)

## File Type Dictionary
- `src/filetypes.rs` — OnceLock singleton: `dictionary()` → `lookup()`, `lookup_by_path()`, `is_known_extension()`, `has_known_extension()`, `extensions_for_category()`, `format_family()`

## CLI
- `src/main.rs` — 3 modes: `--chat` (NL readline), `--plan <path> [--run]` (plan compile/execute), default (demo)
  - `cadmus --plan <path.sexp> [--run]` — Racket is sole codegen target
- `src/line_editor.rs` — `LineEditor`, `ReadResult` — rustyline v15 wrapper, `~/.cadmus_history`
- `src/ui.rs` — ANSI color helpers, geometric icons, formatting primitives (zero external deps)
  - `color_enabled()` checks NO_COLOR/TERM=dumb
  - 30 style functions, axis helpers, tree formatting, status badges

## Key Design Principles

### Op Architecture
- Shell ops = anchors to the world (bridge in/out of OS)
- Racket-native ops = intermediate transformation logic (filter, sort, map, fold)
- Algorithm atoms = opaque Racket bodies with typed signatures
- `filter` is racket-native only, NOT dual-behavior
- `sort_by`, `head`, `tail`, `count`, `unique` are dual-behavior

### Type System
- `TypeExpr` is an open grammar — new types are just strings, no enum variants to add
- `walk_tree` returns `Seq(Entry(Name, a))` (flat); `walk_tree_hierarchy` returns `Tree(Entry(Name, a))`
- `walk_tree` on default Dir produces `Seq(Entry(Name, Bytes))`, not `Seq(Entry(Name, File(Text)))`
- `PlanDef.output` is stored but NOT used for type-checking — compiler infers from step chain
- Compiler auto-infers map mode via `step_needs_map()` — `each` keyword is a hint, not sole trigger

### Inference
- Phase ordering is deterministic, but HashMap iteration within phases is not
- `multiply` inference kind varies between runs — both paths produce identical results
- `string_downcase` is always TypeSymmetric (reliable for testing)
- Category gate prevents cross-domain inference leakage

### Plans
- All plans are .sexp format; `parse_sexpr_to_plan()` is sole parser entry point
- 268 total plans in NL autoregression, 100% pass rate
- Mutation pattern: make_vector + for_range + vector_set
- Conditional: cond with clauses list
- Accumulation: fold with acc/init/var/over/body
- Complex data: use bindings for literals, not list_new (avoids whitespace splitting)
- No-arg ops: `op: ""` → `(random)`

### Codegen
- racket_body define emission uses `poly.racket_symbol` (not `op.replace('_', "-")`)
- Ops with underscores in racket_symbol need matching define names
- Arity-1 Racket ops need scalar arg syntax `(op $ref)` not keyword syntax

## Gotchas & Subtle Behaviors
- `Literal` has `type_id`, `key`, `value`, `metadata` — no `description` field; `ExprLiteral` has `description`
- `poly_op_names()` returns `Vec<&str>` — use `contains(&"name")` not `contains("name")`
- Generic planner uses `TypeId` (string-based), not `TypeExpr` — need custom registries for planner tests
- `parse_plan` catches empty steps before `compile_plan` runs
- SymSpell corrects plurals→singulars, so both forms needed in YAML tables
- Typo correction runs BEFORE synonym mapping — corrected words may not match synonyms
- Only double quotes supported in NL (single quotes conflict with contractions)
- Early first-token check must be limited to FIRST content token only — later compounds cause regressions
- "pasta" → "paste" by SymSpell was false positive; fixed by counting distinct token matches

## Test Suite
- **1390 passed, 0 failed, 65 ignored** (current)
- Key test files:
  - `tests/nl_autoregression.rs` — 268/268 plans, hard 100% gate
  - `tests/nl_tests.rs` — NL integration (184 tests)
  - `tests/nl_earley_tests.rs` — Earley parser (72 tests)
  - `tests/stress_pipeline.rs` — 80 stress tests across 7 subsystems
  - `tests/algorithm_plans_tests.rs` — 20 tests (18 categories + aggregate + compile-only)
  - `tests/archive_codegen_tests.rs` — archive format codegen
  - `tests/shell_callable_tests.rs` — shell-callable Racket forms
  - `tests/recipe_tests.rs` — 32 command recipe tests
  - `tests/prompts_tests.rs` — 33 macOS desktop task tests
  - `tests/domain_autoregression.rs` — full-funnel domain report
  - `tests/semantic_tests.rs` — 46 goal→plan→execution tests
  - `tests/sexpr_tests.rs` — sexpr parser tests
  - `tests/compact_facts_tests.rs` — compact properties format
  - `tests/racket_tests.rs` — Racket codegen + inference
  - `tests/fs_integration.rs` — filesystem integration
  - `tests/integration.rs` — original reasoning engine tests
  - `tests/power_tools_tests.rs` — power tools ops/facts
  - `tests/plan_tests.rs` — plan compiler
  - `tests/repro_screenshot.rs` — NL→Racket roundtrip
  - `tests/generic_planner_tests.rs` — generic planner

## Documentation
- `ARCHITECTURE.md` — System architecture overview
- `PLAYBOOK.md` — Adding ops, facts, plans, file types, shell-callable forms, algorithm plans
- `RUNBOOK.md` — Machine-executable guide for adding new domains (6 phases, 3 appendices)
- `BUGS.md` — 16 bugs: 14 fixed, 1 deferred (BUG-009 multi-input ops), 1 by-design (BUG-012)
- `SEMANTICS.md` — Semantic correctness analysis

### Double-Define Codegen Fix (plan `fix-double-define-codegen`)
- `src/racket_executor.rs:1404-1412` — `emit_racket_body_defines()` now checks `body.trim().starts_with("(define ")` and emits verbatim if so
- `src/racket_executor.rs:1438-1446` — Same fix in `emit_raw_step_defines()`
- Algorithm ops (bare expression bodies) still get wrapped in `(define (fn params) body)`
- macOS task ops (full define bodies) emitted verbatim — no double nesting
- `data/packs/ops/macos_tasks.ops.yaml:198-266` — 5 new ops: git_add, git_commit, git_push, open_file (tilde expansion), sync (rsync -a, tilde expansion)
- `data/packs/ops/macos_tasks.ops.yaml` — Fixed `shell-result` → `shell-exec` in 3 ops (rename_by_date, find_duplicates_by_hash, backup_timestamped)
- `tests/macos_tasks_tests.rs` — 21 integration tests: codegen validity, no double-define, command correctness, single-step displayln, algorithm regression
- All 22 macOS task plans now produce valid Racket codegen (was 8/22)
- 1411 total tests, 0 failures, 65 ignored

### Type-Directed Intent Compiler (commit `4381400`)
- `src/nl/intent_compiler.rs` — Replaced hardcoded `match action` block with declarative recipe system
  - `ActionRecipe { ops: &[&str] }` — static slice of op names per action
  - `action_recipe(action, ir)` — maps 18 action labels to op sequences, context-dependent for compress
  - `try_action_recipe(action, ir_step, ir, prior_steps)` — applies recipe, extracts patterns, handles order needing walk_tree
  - `build_recipe_step_args(op, ir_step, pattern)` — builds StepArgs per op (sort_by reads by+direction→Scalar mode)
  - `extract_pattern_from_ir(ir_step, ir)` — resolves concept→pattern from IR params
  - Dead code deleted: `compile_select_step`, `compile_order_step`, `compile_compress_step` (102 lines)
- `src/generic_planner.rs:752-849` — `lower_to_plan_def()` converts ExprPlanNode tree to flat PlanDef
- 18 action labels: select, retrieve, traverse, enumerate, compress, decompress, search_text, deduplicate, count, order, read, delete, copy, move, rename, compare, checksum, download
- 39 intent_compiler tests (23 existing + 16 new recipe tests)
- 1431 total tests, 0 failures

### Auto-Approve Mode
- `src/main.rs:49` — `--auto` flag parsed from CLI args
- `src/main.rs:60-68` — Auto mode banner: "reasoning inference engine — auto mode"
- `src/main.rs:107-182` — Auto-approve path in `PlanCreated` handler: takes plan from `state.current_plan`, codegen, execute, prints thinking+execution times
- `src/ui.rs:293-314` — `format_duration(Duration)` and `timing(label, Duration)` helpers
  - `<1s` → `"142ms"`, `1-60s` → `"1.3s"`, `60s+` → `"1m 5.2s"`
- `tests/auto_approve_tests.rs` — 12 tests: format_duration edge cases, timing output, auto-approve NL flow
- 1472 total tests, 0 failures, 66 ignored

### Web Server Feature (plan `cadmus-http-server`)
- `data/packs/ops/web.ops.yaml` — 3 ops: http_server, add_route, format_html_code_block
  - All have `racket_body` with full `(define ...)` forms using web-server/servlet
  - http_server: Number → Void (starts server on port with hello world handler)
  - add_route: String, String → Void (registers route handler running pipeline program)
  - format_html_code_block: a → String (polymorphic, wraps text in HTML pre/code)
- `src/fs_types.rs:60` — `WEB_OPS_YAML` embedded constant
- `src/fs_types.rs:103-109` — `load_ops_pack_str_into` for web ops in `build_full_registry()`
- `src/racket_executor.rs:80-85` — web ops also loaded in `build_racket_registry()`
- `src/racket_executor.rs:123-133` — `WEB_PREAMBLE` const: `(require web-server/servlet web-server/servlet-env)`
- `src/racket_executor.rs:134` — `WEB_OPS` list: `["http_server", "add_route"]`
- `src/racket_executor.rs:185-189` — `has_web_ops()` function
- `src/racket_executor.rs:1517-1522` — web preamble emission in `generate_racket_script()`
- `src/racket_executor.rs:1308-1312` — Fixed racket_body ops to use `prev_binding` for pipeline chaining
- `data/plans/web_server.sexp` — Single-step plan: `(http_server :port "8080")`
- `data/plans/add_route_web_server.sexp` — Route registration plan
- `data/plans/error_log_pipeline.sexp` — Pipeline: read_file → format_html_code_block
- `data/nl/nl_dictionary.yaml:2502-2510` — Added web/server/route/handler/pipeline/html/localhost to SymSpell
- `src/nl/mod.rs:1250-1271` — NL tests for web server and add_route
- `tests/web_server_tests.rs` — 16 tests: compilation, codegen, NL routing, registry, type chain
- NL routing: "spin up a web server" → phrase tokenizer "run" + 2-token pair "web_server" → web_server.sexp
- Autoregression: 271/271 (100%), 1490 total tests, 0 failures