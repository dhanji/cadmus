# Configuration Guide

> **Purpose**: Reference for all YAML data files that configure Cadmus.  
> **Audience**: Developers extending Cadmus with new operations, domains, or vocabulary.  
> **Last updated**: 2025-02-23  
> **Source of truth**: `data/` directory, `src/registry.rs`, `src/fact_pack.rs`, `src/filetypes.rs`, `src/nl/vocab.rs`, `src/nl/lexicon.rs`

---

## Overview

Cadmus is configured entirely through YAML data files. No environment variables or config files are needed. All YAML files are loaded at runtime; the engine embeds fallback copies via `include_str!()` so it works without the data directory, but prefers on-disk files when available.

```
data/
  filetypes.yaml                    File type dictionary
  nl/
    nl_vocab.yaml                   NL synonyms, contractions, approvals
    nl_dictionary.yaml              SymSpell typo correction dictionary
    nl_lexicon.yaml                 Earley parser lexicon
  packs/
    ops/                            Operation type signatures
      fs.ops.yaml                     49 filesystem ops
      power_tools.ops.yaml            64 dev tool ops
      racket.ops.yaml                 47 Racket language ops
      comparison.ops.yaml             6 comparison reasoning ops
      coding.ops.yaml                 6 code analysis ops (stub)
    facts/                          Domain knowledge
      putin_stalin.facts.yaml         Political comparison
      macos_fs.facts.yaml             macOS tool knowledge
      macos_cli.facts.yaml            CLI tool submodes
      racket.facts.yaml               Racket op relationships
  plans/                            Plan YAML definitions
    *.yaml                            24 general-purpose plans
    algorithms/                       108 algorithm templates
```

---

## Ops Packs

**Files**: `data/packs/ops/*.ops.yaml`  
**Loader**: `src/registry.rs` — `load_ops_pack_str()`, `load_ops_pack_str_into()`  
**Naming convention**: `<domain>.ops.yaml`

### Schema

```yaml
name: <pack_name>                    # unique pack identifier
description: "<human-readable>"      # pack description

ops:
  - name: <op_name>                  # unique operation name (snake_case)
    type_params: [a, b]              # polymorphic type variables (empty [] for monomorphic)
    inputs: ["Dir(a)", "Pattern"]    # input type expressions (TypeExpr strings)
    output: "Seq(Entry(Name, a))"   # output type expression
    properties:                      # algebraic properties (all optional)
      commutative: false
      associative: false
      idempotent: true
      identity: null
      absorbing: null
    description: "ls — list dir"    # human-readable (shown in dry-run traces)
    racket_symbol: "ls"             # optional: Racket identifier for codegen
    meta:                           # optional: metasignature for inference engine
      params:
        - name: a
          type: "Number"
        - name: b
          type: "Number"
      return_type: "Number"
      invariants: ["a + b = b + a"]
      category: "arithmetic"
      effects: []
      type_params: []
```

### Type Expression Syntax

Type expressions are parsed by `TypeExpr::parse()` in `src/type_expr.rs`:

| Syntax | Meaning |
|--------|---------|
| `Bytes` | Primitive type |
| `File(Text)` | Constructor with one arg |
| `Entry(Name, File(Bytes))` | Constructor with multiple args |
| `Seq(a)` | Constructor with type variable |
| `Archive(a, Cbz)` | Constructor with mixed args |

### Current Ops Packs

| Pack | File | Ops | Domain |
|------|------|-----|--------|
| filesystem | `data/packs/ops/fs.ops.yaml` | 49 | POSIX/macOS filesystem |
| power_tools | `data/packs/ops/power_tools.ops.yaml` | 64 | Dev tools (git, tmux, jq, awk, etc.) |
| racket | `data/packs/ops/racket.ops.yaml` | 47 | Racket language (arithmetic, list, set, I/O) |
| comparison | `data/packs/ops/comparison.ops.yaml` | 6 | Comparative reasoning |
| coding | `data/packs/ops/coding.ops.yaml` | 6 | Code analysis (stub) |

### Adding an Ops Pack

See [PLAYBOOK.md](../PLAYBOOK.md) Section 1 for step-by-step instructions.

**Key rules**:
- Op names must be unique across all loaded packs
- Type expressions must parse successfully via `TypeExpr::parse()`
- The `racket_symbol` field is required for ops that will be code-generated to Racket
- The `meta` block is required for ops that participate in the inference engine

---

## Fact Packs

**Files**: `data/packs/facts/*.facts.yaml`  
**Loader**: `src/fact_pack.rs` — `load_fact_pack()`, `load_fact_pack_str()`  
**Naming convention**: `<domain>.facts.yaml`

### Schema

```yaml
entities:
  - id: <entity_id>                  # unique within pack (snake_case)
    name: "<display name>"
    description: "<description>"

axes:
  - id: <axis_id>                    # unique within pack
    name: "<display name>"
    description: "<description>"
    polarity: capability | cost      # optional: for CrossAxisTension derivation
    sub_axes:                        # optional
      - id: <sub_axis_id>
        name: "<display name>"

claims:
  - id: <claim_id>                   # unique within pack
    entity: <entity_id>             # must reference an entity above
    axis: <axis_id>                 # must reference an axis above
    text: "<claim text>"

evidence:
  - id: <evidence_id>
    entity: <entity_id>
    supports: <claim_id>            # must reference a claim above
    content: "<evidence text>"

# Compact properties format (entity → axis → key → value)
compact_properties:
  <entity_id>:
    <axis_id>:
      <key>: "<simple_value>"       # simple string value
      <key>:                        # extended value
        value: "<value>"
        ordinal: <integer>          # optional: for ordinal comparison
        note: "<explanation>"       # optional: human-readable note

relations:
  - type: hierarchy                 # or "ordinal"
    axis: <axis_id>
    entities: [<entity_id>, ...]
    description: "<description>"

uncertainties:
  - id: <uncertainty_id>
    entity: <entity_id>
    axis: <axis_id>
    text: "<uncertainty text>"
```

### Compact Properties

The `compact_properties` format is the only supported format. It groups properties by entity → axis → key:

```yaml
compact_properties:
  putin:
    coercion:
      repression_scale:
        value: targeted
        ordinal: 2
        note: "Selective targeting of opponents"
      method: "legal_prosecution"    # simple string value
    legitimacy:
      legitimacy_basis: electoral_managed
```

Properties are expanded into flat `Property` structs at load time by `expand_compact_properties()` in `src/fact_pack.rs`.

### Special Properties for Inference

The Racket fact pack (`data/packs/facts/racket.facts.yaml`) uses special properties that drive the inference engine:

| Property | Purpose |
|----------|---------|
| `op_name` | Maps entity to an operation name |
| `racket_symbol` | The Racket identifier for the op |
| `symmetric_partner` | Entity ID of the inverse operation |
| `type_symmetry_class` | Class name for type-symmetric inference |
| `keyword_map_*` | NL word → op name mapping |
| `submode_*` | CLI tool flag variants (in `macos_cli.facts.yaml`) |

### Merge Behavior

`FactPack::merge()` combines two packs:
- **Entities**: deduplicated by `id` (first wins)
- **Axes**: deduplicated by `id`, `sub_axes` unioned
- **Claims, evidence, properties**: concatenated
- **Relations**: deduplicated by `Relation::id()`
- **Uncertainties**: concatenated

### Current Fact Packs

| Pack | File | Entities | Purpose |
|------|------|----------|---------|
| Putin/Stalin | `data/packs/facts/putin_stalin.facts.yaml` | 2 | Political comparison |
| macOS FS | `data/packs/facts/macos_fs.facts.yaml` | 1 | macOS tool knowledge |
| macOS CLI | `data/packs/facts/macos_cli.facts.yaml` | 12+ | CLI tool submodes |
| Racket | `data/packs/facts/racket.facts.yaml` | 14 | Racket op relationships |

---

## File Type Dictionary

**File**: `data/filetypes.yaml`  
**Loader**: `src/filetypes.rs` — `FileTypeDictionary` singleton via `OnceLock`

### Schema

```yaml
categories:
  - image
  - video
  - audio
  - document
  - archive
  - source_code
  - data
  - config
  - markup
  - database
  - font
  - executable
  - disk_image
  - ebook

format_families:
  Cbz: zip
  Cbr: rar
  TarGz: tar_gz
  TarBz2: tar_bz2
  TarXz: tar_xz

types:
  - ext: pdf
    category: document
    description: "PDF document"
    type_expr: "File(PDF)"
    relevant_ops: [read_file, search_content]
    tools: [open, preview, pdftotext]

  - ext: tar.gz
    category: archive
    description: "Gzipped tar archive"
    type_expr: "File(Archive(Bytes, TarGz))"
    relevant_ops: [extract_tar_gz, pack_tar_gz]
    tools: [tar, gzip]
```

### Usage

The dictionary is the single source of truth for file type knowledge. It replaces three previously hardcoded extension lists:

- **Plan compiler** (`src/plan.rs`): `infer_input_type()` uses `lookup_by_path()` for type inference
- **NL slots** (`src/nl/slots.rs`): `is_file_extension()` delegates to `is_known_extension()`
- **NL dialogue** (`src/nl/dialogue.rs`): `is_file_path()` delegates to `has_known_extension()`

### Key API

| Function | Purpose |
|----------|---------|
| `dictionary()` | Get singleton `FileTypeDictionary` |
| `lookup(ext)` | Get file type info by extension |
| `lookup_by_path(path)` | Extract extension and look up |
| `is_known_extension(ext)` | Check if extension is known |
| `extensions_for_category(cat)` | All extensions in a category |
| `format_family(format)` | Archive format family (e.g., `Cbz` → `zip`) |

**197 entries** across 14 categories.

---

## NL Vocabulary

**File**: `data/nl/nl_vocab.yaml`  
**Loader**: `src/nl/vocab.rs` — `NlVocab` singleton via `OnceLock`

### Schema

```yaml
synonyms:                           # NL phrases → canonical op names
  - phrase: [zip, up, everything]
    op: pack_archive
  - phrase: [find]
    op: find_matching

contractions:                       # English contractions → expansions
  - from: "don't"
    to: "do not"

ordinals:                           # Ordinal words → numbers
  - word: first
    value: 1

approvals:                          # Words signaling user approval
  - ok
  - yes
  - approve
  - "looks good"

rejections:                         # Words signaling user rejection
  - no
  - reject
  - "never mind"
  - "scrap that"

stopwords:                          # Filtered during slot extraction
  - the
  - a
  - an

filler_phrases:                     # Approval tail phrases
  - "why not"
  - "that works"

filler_prefixes:                    # Edit-command prefixes to skip
  - also
  - and
  - then

dir_aliases:                        # Directory name aliases
  desktop: "~/Desktop"
  downloads: "~/Downloads"
  documents: "~/Documents"

noun_patterns:                      # Noun → file type patterns
  comic:
    - "*.cbz"
    - "*.cbr"
  photo:
    - "*.jpg"
    - "*.png"
```

### Sections

| Section | Entries | Purpose |
|---------|---------|---------|
| `synonyms` | ~200+ | Map NL phrases to canonical op names (greedy longest-match) |
| `contractions` | 37 | Expand English contractions |
| `ordinals` | ~20 | Map ordinal words to numbers |
| `approvals` | ~15 | Recognize approval intent |
| `rejections` | ~15 | Recognize rejection intent |
| `stopwords` | ~55 | Filter noise during slot extraction |
| `filler_phrases` | ~10 | Approval tail phrases |
| `filler_prefixes` | ~10 | Edit-command prefixes to skip |
| `dir_aliases` | 15 | Map directory names to paths |
| `noun_patterns` | 22 | Map nouns to file type glob patterns |

---

## NL Dictionary

**File**: `data/nl/nl_dictionary.yaml`  
**Loader**: `src/nl/typo.rs` — `SymSpellDict` via `build_domain_dict()`

### Schema

```yaml
<category_name>:
  <word>: <frequency>
  <word>: <frequency>
```

Higher frequency = stronger correction target. Op-related words: 60-100, common English: 20-40.

### Categories

| Category | Words | Purpose |
|----------|-------|---------|
| `op_components` | ~100 | Operation name parts (list, dir, walk, filter, ...) |
| `common_english` | ~200 | Common English words to prevent false corrections |
| `filesystem` | ~80 | Filesystem terms (directory, folder, archive, ...) |
| `programming` | ~60 | Programming terms (function, variable, compile, ...) |
| `git` | ~30 | Git terms (commit, branch, merge, ...) |
| `racket_arithmetic` | 14 | Arithmetic terms (add, subtract, multiply, ...) |
| ... | ... | Additional domain categories |

**~2473 total words**. The SymSpell algorithm uses max edit distance 2 and prefix length 7 for correction.

---

## NL Lexicon

**File**: `data/nl/nl_lexicon.yaml`  
**Loader**: `src/nl/lexicon.rs` — `TokenClassifier`

### Schema

```yaml
verbs:
  - word: find
    action: select                  # abstract action label
    synonyms: [locate, seek, discover, hunt, ...]

nouns:
  - word: file
    synonyms: [document, doc]
  - word: directory
    synonyms: [folder, dir]

path_nouns:
  - word: desktop
    synonyms: [desk]

orderings:
  - word: alphabetically
    synonyms: [alphabetical]

prepositions:
  - word: in
    synonyms: [inside, within]

determiners:
  - word: the
    synonyms: [a, an, every]

fillers:
  - word: please
    synonyms: [kindly]

phrase_groups:                       # Multi-word phrase tokenizer
  - skeleton: [find, match]         # content-word skeleton
    canonical: find_matching        # emitted token
  - skeleton: [zip, up]
    canonical: pack_archive
```

### Categories

| Category | Entries | Purpose |
|----------|---------|---------|
| `verbs` | 104 base verbs | Action words → abstract action labels |
| `nouns` | ~30 | Object nouns for grammar |
| `path_nouns` | ~10 | Directory/location nouns |
| `orderings` | ~10 | Sort order words |
| `prepositions` | ~10 | Spatial/logical prepositions |
| `determiners` | ~5 | Articles and quantifiers |
| `fillers` | ~5 | Politeness/filler words |
| `phrase_groups` | 49 | Multi-word phrase → single token |

**~1186 total words** across all categories. The lexicon drives the Earley parser's terminal symbol classification.

### Verb Action Labels

Action labels are abstract — they map to concrete plan steps in the intent compiler (`src/nl/intent_compiler.rs`):

| Action Label | Plan Steps |
|-------------|------------|
| `select` | `walk_tree` + `find_matching` |
| `compress` | `walk_tree` + `pack_archive` (dir) or `gzip_compress` (file) |
| `decompress` | `extract_archive` |
| `order` | `sort_by` |
| `search_text` | `walk_tree` + `search_content` |
| `enumerate` | `list_dir` |

**INFERENCE: Most of the 104 verb action labels are not yet implemented in the intent compiler.** Unimplemented actions fall through: Earley parses → compiler returns Error → falls back to old pipeline.

---

## Plan YAML Files

**Directory**: `data/plans/`  
**Loader**: `src/plan.rs` — `load_plan()`, `parse_plan()`

See [Plan DSL Reference](plan-dsl.md) for the full specification.

### Quick Reference

```yaml
plan-name:
  inputs:
    - bare_name                     # type inferred from name
    - typed_name: "Type"            # explicit type annotation
  bindings:                         # optional: literal input values
    typed_name: "42"
  output: "Seq(Entry(Name, File(PDF)))"  # optional: declared output type
  steps:
    - walk_tree                     # bare op
    - filter: { extension: ".pdf" } # op with named params
    - sort_by: name                 # op with scalar param
    - read_file: each               # map mode
```

### Current Plans

- **24 general-purpose plans** in `data/plans/` (find_pdfs, repack_comics, git_log_search, etc.)
- **108 algorithm templates** in `data/plans/algorithms/` across 14 categories (arithmetic, sorting, searching, graph, dynamic-programming, etc.)

---

## Loading Order

The `build_full_registry()` function in `src/fs_types.rs` loads data in this order:

1. `data/packs/ops/fs.ops.yaml` (embedded fallback)
2. `data/packs/ops/power_tools.ops.yaml` (embedded fallback)
3. `data/packs/ops/racket.ops.yaml` (disk-first, embedded fallback)
4. `data/packs/facts/racket.facts.yaml` → inference Phases 0-3
5. `data/packs/facts/macos_cli.facts.yaml` → inference Phase 4

NL data is loaded independently via `OnceLock` singletons on first access:
- `NlVocab` from `data/nl/nl_vocab.yaml`
- `SymSpellDict` from `data/nl/nl_dictionary.yaml`
- `TokenClassifier` from `data/nl/nl_lexicon.yaml`
- `FileTypeDictionary` from `data/filetypes.yaml`
