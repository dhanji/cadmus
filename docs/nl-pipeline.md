# NL Pipeline

> **Purpose**: Detailed documentation of the Natural Language UX layer.  
> **Audience**: Developers extending NL capabilities or debugging NL behavior.  
> **Last updated**: 2025-02-23  
> **Source of truth**: `src/nl/` (13 modules, ~9435 lines), `data/nl/`

---

## Overview

The NL pipeline is a deterministic, low-latency adapter that converts chatty user input into structured `PlanDef` YAML. No LLM — all processing is mechanical pattern matching, dictionary lookup, and grammar parsing.

**Entry point**: `process_input(input: &str, state: &mut DialogueState) -> NlResponse`  
**Location**: `src/nl/mod.rs`

```
User input: "find all PDFs in ~/Documents"
    │
    ▼
┌─────────────────────────────────────────────────────┐
│ 1. Normalize    (case fold, contractions, synonyms) │
│ 2. Typo correct (SymSpell, ~2473 words)             │
│ 3. Re-normalize (synonyms on corrected tokens)      │
│ 4. Intent parse (approve/reject/explain/edit)       │
│ 5. Slot extract (paths, ops, modifiers)             │
│ 6. Focus update (anaphora resolution)               │
│ 7. Dispatch:                                        │
│    ├── Approve/Reject → keyword match               │
│    ├── Explain → keyword match                      │
│    ├── Edit → pattern match + slot extraction        │
│    └── Create Plan:                                 │
│         ├── Earley parser (primary)                 │
│         └── Old pipeline (fallback)                 │
└─────────────────────────────────────────────────────┘
    │
    ▼
NlResponse::PlanCreated { plan_yaml, summary, prompt }
```

## Pipeline Stages

### Stage 1: Normalization (`src/nl/normalize.rs`, 671 lines)

**Function**: `normalize(input: &str) -> NormalizedInput`

1. **Quote extraction** — pre-pass extracts double-quoted strings, preserving spaces
2. **Tokenization** — split on whitespace, strip punctuation (preserving paths)
3. **Case folding** — lowercase all tokens (except path-like tokens)
4. **Contraction expansion** — "don't" → "do not" (37 patterns from `nl_vocab.yaml`)
5. **Ordinal canonicalization** — "first" → "1", "second" → "2"
6. **Synonym mapping** — greedy longest-match against ~200+ synonym entries

**Output**: `NormalizedInput { tokens, canonical_tokens, original }`

- `tokens` — raw normalized tokens
- `canonical_tokens` — tokens after synonym mapping (op names are canonical)

**Key detail**: Only double quotes are supported for quoting. Single quotes conflict with contractions like `don't`.

### Stage 2: Typo Correction (`src/nl/typo.rs`, 612 lines)

**Function**: `SymSpellDict::correct_tokens(tokens) -> Vec<String>`

Uses the SymSpell algorithm with:
- Max edit distance: 2
- Prefix length: 7
- Dictionary: ~2473 frequency-weighted words from `data/nl/nl_dictionary.yaml`
- Distance metric: Damerau-Levenshtein (handles transpositions)

**Key detail**: Typo correction runs BEFORE synonym mapping. The corrected tokens are re-normalized in Stage 3 to apply synonyms to the corrected forms.

**Known issues** (from `BUGS.md`):
- Common English words not in the dictionary can be "corrected" to domain words
- The dictionary has been expanded to ~2000 unique words to mitigate this

### Stage 3: Re-normalization

The corrected tokens are rejoined and re-normalized to apply synonym mapping to the corrected forms. Space-containing tokens (from quoted input) are re-quoted.

### Stage 4: Intent Recognition (`src/nl/intent.rs`, 1082 lines)

**Function**: `parse_intent(normalized: &NormalizedInput) -> Intent`

Pattern-based intent classification:

```rust
enum Intent {
    CreatePlan { op: Option<String>, rest: Vec<String> },
    EditStep { action: EditAction, rest: Vec<String> },
    ExplainOp { subject: String, rest: Vec<String> },
    Approve,
    Reject,
    AskQuestion { question: String },
    SetParam { key: String, value: String },
    NeedsClarification,
}
```

**Recognition order**:
1. **Approve** — keyword match against approvals list + compound phrases ("perfect, ship it")
2. **Reject** — keyword match against rejections list ("never mind", "scrap that")
3. **Explain** — question patterns ("what does X do?", "explain X")
4. **Edit** — action verbs (skip, remove, add, move, change, insert) + filler prefix skipping
5. **CreatePlan** — everything else (if it contains an op name or looks like a command)

**Edit actions**:
```rust
enum EditAction { Add, Remove, Move, Skip, Change, Insert }
```

### Stage 5: Slot Extraction (`src/nl/slots.rs`, 926 lines)

**Function**: `extract_slots(tokens: &[String]) -> ExtractedSlots`

Extracts structured information from tokens:

```rust
struct ExtractedSlots {
    paths: Vec<String>,           // file/directory paths
    ops: Vec<String>,             // canonical operation names
    step_refs: Vec<StepRef>,      // step references ("step 3", "the last one")
    patterns: Vec<String>,        // glob/regex patterns
    params: Vec<(String, String)>,// key-value parameters
    modifiers: Vec<Modifier>,     // sort order, count limits
    keywords: Vec<String>,        // remaining significant tokens
}
```

**Extraction pipeline** (10 steps):
1. Filter stopwords (55 words)
2. Detect paths (absolute, relative, tilde, URLs, bare filenames with known extensions)
3. Detect canonical op names
4. Detect step references ("step 3", "the third step", "last step")
5. Detect patterns (glob: `*.pdf`, regex: `/pattern/`)
6. Detect parameters (key=value, key:value)
7. Detect modifiers (sort order, count limits)
8. Detect keywords (remaining significant tokens)
9. Resolve directory aliases ("desktop" → "~/Desktop", from `nl_vocab.yaml`)
10. Resolve noun patterns ("comics" → `*.cbz`, `*.cbr`, from `nl_vocab.yaml`)

### Stage 6: Focus Update

Updates the `DialogueState.focus` stack for anaphora resolution. The focus stack tracks:

```rust
enum FocusEntry {
    EditedStep { step_index, op },  // last edited step
    MentionedOp { op },             // last mentioned operation
    Artifact { path },              // last mentioned path
    WholePlan,                      // the entire plan
}
```

### Stage 7: Dispatch

Based on the recognized intent:

| Intent | Handler | Uses Earley? |
|--------|---------|-------------|
| `Approve` | `handle_approve()` — validates plan exists, compiles, generates script | No |
| `Reject` | `handle_reject()` — clears current plan | No |
| `ExplainOp` | `handle_explain()` — looks up op description from registry | No |
| `EditStep` | `handle_edit()` — applies edit to current plan | No |
| `CreatePlan` | `try_earley_or_old_pipeline()` — Earley first, then fallback | Yes |

## Earley Parser (`src/nl/earley.rs`, 788 lines)

### Architecture

The Earley parser is **additive** — it's tried first for plan creation, and falls back to the old pipeline on failure.

```
tokens → phrase_tokenize() → Earley parse → parse forest → IntentIR → PlanDef
                                                                ↓ (on failure)
                                                          Old pipeline
```

### Grammar (`src/nl/grammar.rs`, 360 lines)

**Function**: `build_command_grammar() -> Grammar`

~30 production rules following a **Verb Object Modifiers** pattern:

```
Command → Verb Object Modifiers
Object  → Determiner? Noun | Path | Noun Path
Modifiers → Preposition Object | Ordering | ε
```

Terminal symbols: `verb`, `noun`, `path_noun`, `preposition`, `determiner`, `ordering`, `quantifier`, `conjunction`, `filler`.

### Lexicon (`src/nl/lexicon.rs`, 569 lines)

**Function**: `lexicon() -> &'static TokenClassifier`

Loaded from `data/nl/nl_lexicon.yaml` via `OnceLock` singleton. Classifies tokens into grammar terminal categories. 104 base verbs with ~1186 total words.

### Phrase Tokenizer (`src/nl/phrase.rs`, 314 lines)

**Function**: `phrase_tokenize(tokens: &[String]) -> Vec<String>`

Greedy longest-match phrase grouping. Runs BEFORE the Earley parser.

Algorithm:
1. Scan tokens left-to-right
2. Match first skeleton word from phrase groups
3. Skip stopwords between skeleton words
4. Emit canonical token when full skeleton matched

**Example**: "make me a list of" → "list" + "of" (skeleton `[make, list]` → canonical `list`)

49 phrase groups defined in `data/nl/nl_lexicon.yaml`.

### Intent IR (`src/nl/intent_ir.rs`, 671 lines)

**Function**: `parse_trees_to_intents(parses: &[ParseTree]) -> IntentIRResult`

Converts Earley parse trees into structured intents:

```rust
struct IntentIR {
    output: String,              // expected output type
    inputs: Vec<IRInput>,        // named inputs with types
    steps: Vec<IRStep>,          // abstract processing steps
    constraints: Vec<String>,    // result constraints
    acceptance: Vec<String>,     // acceptance criteria
    score: f64,                  // parse score (for ranking)
}
```

### Intent Compiler (`src/nl/intent_compiler.rs`, 593 lines)

**Function**: `compile_intent(result: &IntentIRResult) -> CompileResult`

Maps abstract action labels to concrete plan steps:

| Action Label | Generated Steps |
|-------------|-----------------|
| `select` | `walk_tree` + `find_matching` |
| `compress` | `walk_tree` + `pack_archive` (dir) or `gzip_compress` (file) |
| `decompress` | `extract_archive` |
| `order` | `sort_by` |
| `search_text` | `walk_tree` + `search_content` |
| `enumerate` | `list_dir` |

**FACT**: Most of the 104 verb action labels are NOT implemented in the compiler. Unimplemented actions produce `CompileResult::Error`, which triggers fallback to the old pipeline.

```rust
enum CompileResult {
    Ok(PlanDef),
    Error(String),
    NoIntent,
}
```

## Old Pipeline (Fallback)

When the Earley parser can't parse the input or the intent compiler can't compile the IR, the old pipeline handles plan creation:

1. Use the `Intent::CreatePlan { op, rest }` from Stage 4
2. Extract slots from `rest` tokens
3. Call `dialogue::build_plan()` to construct a `PlanDef`

### `build_plan()` (`src/nl/dialogue.rs`, 1665 lines)

Constructs a `PlanDef` from an operation name and extracted slots:

1. Categorize the op (file/dir/entry/url/git)
2. Extract target path from slots (with directory alias resolution)
3. Resolve path via `resolve_path()` (bare names → `/Volumes/<name>` if exists)
4. Build input list with appropriate types
5. Generate step chain based on op category
6. Handle arithmetic ops (extract numbers, build x/y params)
7. Handle noun patterns (e.g., "comics" → filter for `*.cbz|*.cbr`)

### `apply_edit()` (`src/nl/dialogue.rs`)

Applies edits to the current plan:

| Edit Action | Behavior |
|-------------|----------|
| `Skip` | Remove step matching keyword (filters 25 action words) |
| `Remove` | Remove step by index or op name (defaults to last step) |
| `Add` | Append new step |
| `Insert` | Insert step at position |
| `Move` | Reorder step |
| `Change` | Replace step parameters |

### `plan_to_yaml()` (`src/nl/dialogue.rs`)

Serializes a `PlanDef` back to YAML string for display. Generates the function-framing format with typed inputs and bindings.

## Dialogue State

```rust
struct DialogueState {
    current_plan: Option<PlanDef>,
    alternative_intents: Vec<IntentIR>,
    focus: FocusStack,
    turn_count: usize,
    last_intent: Option<Intent>,
}
```

**Multi-turn flow**:
1. User says "find PDFs in ~/Documents" → `PlanCreated`
2. User says "skip the sort step" → `PlanEdited`
3. User says "looks good" → `Approved { script }`

The `focus` stack enables anaphora resolution — "skip that" refers to the last mentioned step.

## Response Types

```rust
enum NlResponse {
    PlanCreated { plan_yaml, summary, prompt },
    PlanEdited { plan_yaml, diff_description, prompt },
    Explanation { text },
    Approved { script: Option<String> },
    Rejected,
    NeedsClarification { needs: Vec<String> },
    ParamSet { description, plan_yaml: Option<String> },
    Error { message },
}
```

## Module Map

| Module | Lines | Purpose |
|--------|-------|---------|
| `mod.rs` | 791 | Entry point, dispatch, Earley integration |
| `dialogue.rs` | 1665 | State management, build_plan, apply_edit, plan_to_yaml |
| `intent.rs` | 1082 | Pattern-based intent recognition |
| `slots.rs` | 926 | Slot extraction (paths, ops, modifiers) |
| `earley.rs` | 788 | Earley parser engine (predict/scan/complete) |
| `intent_ir.rs` | 671 | Parse tree → structured intent |
| `normalize.rs` | 671 | Tokenization, synonyms, case folding |
| `intent_compiler.rs` | 646 | IntentIR → PlanDef |
| `typo.rs` | 612 | SymSpell typo correction |
| `lexicon.rs` | 569 | YAML lexicon loader |
| `grammar.rs` | 360 | Earley grammar builder |
| `vocab.rs` | 340 | Vocabulary YAML loader |
| `phrase.rs` | 314 | Multi-word phrase tokenizer |

## Known Issues

See `BUGS.md` for the full list (16 tracked, 14 fixed, 1 deferred, 1 by-design).

Key remaining issues:
- **BUG-009 (deferred)**: Multi-input ops (e.g., `search_content` with both path and pattern) — `build_plan()` only handles single-input ops well
- **BUG-012 (by-design)**: "remove" maps to "delete" via synonym table, so "remove the step" becomes "delete the step" in canonical tokens — this is intentional
- **Unimplemented Earley actions**: ~80% of verb action labels fall through to the old pipeline
- **Multiply inference non-determinism**: `test_type_symmetric_discovery_tabular` is flaky due to HashMap iteration order
