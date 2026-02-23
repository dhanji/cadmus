**Purpose**: Machine-specific instructions for AI agents working with this codebase.  
**For project overview, architecture, and usage**: See [README.md](README.md)

---

## 1. Critical Invariants

### MUST Hold

- **Type chain integrity**: Every `CompiledStep.output_type` must be a valid input for the next step's op signature. The plan compiler (`src/plan.rs::compile_plan`) enforces this via unification — do not bypass it.
- **Registry immutability after build**: `build_full_registry()` in `src/fs_types.rs` runs inference phases 0-4. Once built, the registry should not be mutated. The `OnceLock` pattern in `src/plan.rs` caches a single registry instance.
- **Shell quoting**: All user-provided paths in generated Racket scripts MUST go through `shell_quote()` (`src/shell_helpers.rs`) or `(shell-quote ...)` in Racket output. Never inline user paths into shell command strings.
- **Fact pack isolation**: Every claim, evidence item, and property is scoped to exactly ONE entity on ONE axis. The theory layer (`src/theory.rs`) derives all cross-entity comparisons. Never add cross-entity references to fact pack data.
- **Op name uniqueness**: Operation names must be unique across all loaded ops packs. The registry silently overwrites on collision (last registration wins via `rfind`).
- **YAML data files are the source of truth**: Operations, file types, NL vocabulary, and domain knowledge live in YAML. Hardcoded lists in Rust are legacy — prefer YAML.

### MUST NOT

- **Never modify `OnceLock` singletons after initialization**: `FileTypeDictionary`, `NlVocab`, `SymSpellDict`, `TokenClassifier`, `OP_DESCRIPTIONS`, and the plan registry are all `OnceLock`. They are initialized once and shared across threads.
- **Never add `unsafe` code**: The codebase is 100% safe Rust. The only occurrence of the word "unsafe" is in a test comment about shell injection.
- **Never add async/tokio**: The codebase is synchronous. The only external process call is `racket` via `std::process::Command` in `src/calling_frame.rs`.
- **Never store secrets in generated scripts**: Racket scripts are written to temp files and may be displayed to users.

### Performance Constraints

- **NL pipeline must be low-latency**: No network calls, no LLM. All processing is dictionary lookup and pattern matching. The SymSpell dictionary is cached via `OnceLock` (45x speedup from initial implementation).
- **Registry build is expensive**: `build_full_registry()` loads 5 YAML files and runs 5 inference phases. Cache the result — don't rebuild per-request.

---

## 2. Recommended Entry Points

### Understanding the System

| Goal | Start Here |
|------|-----------|
| How plans are compiled | `src/plan.rs::compile_plan` |
| How types work | `src/type_expr.rs::unify` |
| How NL input becomes a plan | `src/nl/mod.rs::process_input` |
| How plans become Racket code | `src/racket_executor.rs::generate_racket_script` |
| How ops are registered | `src/registry.rs::load_ops_pack_str` |
| How inference discovers ops | `src/racket_strategy.rs::promote_inferred_ops` |
| How the full registry is built | `src/fs_types.rs::build_full_registry` |

### Adding Features

| Feature | Files to Touch |
|---------|---------------|
| New operation | `data/packs/ops/<domain>.ops.yaml` only (zero Rust) |
| New file type | `data/filetypes.yaml` only (zero Rust) |
| New NL synonym | `data/nl/nl_vocab.yaml` only (zero Rust) |
| New Earley verb | `data/nl/nl_lexicon.yaml` + possibly `src/nl/intent_compiler.rs` |
| New plan | `data/plans/<name>.yaml` only (zero Rust) |
| New fact pack | `data/packs/facts/<name>.facts.yaml` only (zero Rust) |
| New Racket codegen pattern | `src/racket_executor.rs::op_to_racket` |
| New type lowering | `src/type_lowering.rs` (add to `SUBSUMPTION_MAP` or `RACKET_NATIVE_MAP`) |
| New strategy | Implement `ReasonerStrategy` trait in `src/strategy.rs` |

### Debugging

| Problem | Look Here |
|---------|-----------|
| Plan won't compile | `src/plan.rs::compile_plan` — check `PlanError` variant |
| Type mismatch | `src/type_expr.rs::unify` — check `UnifyError` |
| NL misunderstands input | `src/nl/normalize.rs` (synonym mapping), `src/nl/typo.rs` (correction), `src/nl/intent.rs` (intent recognition) |
| Racket script wrong | `src/racket_executor.rs::op_to_racket` — check dispatch order |
| Op not found | `src/fs_types.rs::build_full_registry` — check loading order |
| Inference not discovering op | `src/racket_strategy.rs` — check fact pack has `op_name`, `racket_symbol`, and `symmetric_partner` or `type_symmetry_class` properties |

---

## 3. Dangerous/Subtle Code Paths

### HashMap Iteration Order (`src/racket_strategy.rs`)

The inference engine iterates over `HashMap<String, ...>` to discover ops. This means the order of Phase 1 vs Phase 2 discovery is non-deterministic. The `multiply` op can be discovered via either op-symmetric (from `divide`) or type-symmetric (from `add`) depending on iteration order. Both paths produce identical results, but tests that assert the inference kind are flaky. **Known flaky test**: `test_type_symmetric_discovery_tabular` in `tests/shell_callable_tests.rs:209`.

### Registry `rfind` Shadowing (`src/registry.rs`)

`get_poly(name)` uses `rfind` — last registration wins. This is intentional: inference phases register promoted ops AFTER the initial YAML load, so the promoted version (with metasignature) shadows the stub. But it means duplicate registrations silently shadow earlier ones.

### Shell Quote Injection (`src/shell_helpers.rs`)

`shell_quote()` has special handling for `$WORK_DIR` — it uses double quotes for variable expansion. The security check validates that only safe ASCII chars follow `$WORK_DIR`. If this check is weakened, shell injection becomes possible via `$WORK_DIR$(malicious_command)`.

### Bytes Promotion Simulation (`src/plan.rs:37-80`)

`try_promote_bytes()` simulates the type chain forward to discover what `Bytes` should be. If the simulation fails (e.g., op not in registry), it silently returns the original type. This means a missing op can cause downstream type mismatches that appear unrelated.

### NL Typo Correction Before Synonyms (`src/nl/mod.rs:93-110`)

Typo correction runs BEFORE synonym mapping. This means a typo-corrected word might not match any synonym. The re-normalization in Stage 3 mitigates this, but edge cases exist where correction changes a word to something that maps to the wrong synonym.

### `OnceLock` Registry in Plan Compiler (`src/plan.rs`)

The plan compiler uses a `OnceLock<OperationRegistry>` for fallback registry lookups in `step_needs_map()` and `lookup_op_inputs()`. This registry is initialized on first call and never updated. If tests modify the registry, the cached version may be stale.

### Isolate Flag Only for Extract Ops (`src/plan.rs`)

`needs_isolation()` only returns `true` for `extract_*` ops. If new ops with filesystem side effects are added (e.g., `unpack_*`, `decompress_*`), they need to be added to this check or they'll collide in map mode.

---

## 4. Do's and Don'ts for Automated Changes

### Do

- **Run `cargo test` after every change** — 1452+ tests, catches most regressions
- **Add YAML data instead of Rust code** when possible — ops, file types, vocabulary, plans
- **Follow the function-framing YAML format** for new plans (top-level key is plan name)
- **Use `TypeExpr::parse()` to validate type expressions** before adding them to YAML
- **Add tests for new NL patterns** — the NL layer has extensive integration tests in `tests/nl_tests.rs`
- **Check `BUGS.md`** before investigating NL issues — it may already be documented

### Don't

- **Don't add ops with names that collide across packs** — the registry silently shadows
- **Don't hardcode extension lists** — use `data/filetypes.yaml` via `filetypes::dictionary()`
- **Don't hardcode synonym lists** — use `data/nl/nl_vocab.yaml` via `vocab::vocab()`
- **Don't bypass `compile_plan()`** — it's the type-checking gate; skipping it produces untyped steps
- **Don't add words to the SymSpell dictionary without checking for false corrections** — common words at high frequency can "correct" valid domain terms
- **Don't assume `build_full_registry()` is cheap** — it loads 5 YAML files and runs inference; cache the result

### Build & Test Commands

```bash
cargo build                    # Build debug
cargo build --release          # Build release
cargo test                     # Run all tests (~1452)
cargo test <test_name>         # Run specific test
cargo test --test nl_tests     # Run NL integration tests
cargo clippy                   # Lint
cargo fmt                      # Format
```

---

## 5. Common Incorrect Assumptions

| Assumption | Reality |
|-----------|---------|
| "Operations are defined in Rust" | Operations are defined in YAML (`data/packs/ops/*.ops.yaml`). Rust code loads and registers them. |
| "The Earley parser handles all NL input" | The Earley parser only handles plan creation. Approve/reject/explain/edit use keyword matching. And ~80% of verb actions fall through to the old pipeline. |
| "Types are an enum" | `TypeExpr` is an open grammar — new types are just strings. No enum variants to add. |
| "Each mode is explicit in YAML" | The compiler also infers map mode automatically via `step_needs_map()`. The `each` keyword is a hint, not the only trigger. |
| "The bash executor still exists" | It was removed. Racket is the sole codegen target. The `--execute` flag was replaced by `--run`. |
| "All 108 algorithm plans work" | Only 44/108 compile and 41/108 execute. The bottleneck is `infer_input_type()`. |
| "Inference is deterministic" | Phase ordering is deterministic, but HashMap iteration within phases is not. The `multiply` op's inference kind varies between runs. |
| "The output type declaration is checked" | `PlanDef.output` is stored but not used for type-checking. The compiler infers output from the step chain. |
| "`filter` is a shell op" | `filter` is Racket-native only. `sort_by`, `head`, `tail`, `count`, `unique` are dual-behavior (shell or Racket depending on context). |
| "Adding a new verb to the lexicon makes it work" | You also need to implement the action label in `src/nl/intent_compiler.rs::compile_ir()`. Without that, it falls through to the old pipeline. |
