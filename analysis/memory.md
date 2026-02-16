# Workspace Memory
> Updated: 2026-02-15T23:54:57Z | Size: 6.3k chars

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