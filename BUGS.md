# NL Layer — Known Bugs

Tracked bugs in the Natural Language UX layer (`src/nl/`).
Discovered via 30 transcript demos, 46 red-team probes, and 4 hardening transcripts.

---

## 🔴 Critical

### ~~BUG-001: "never mind" not recognized as rejection~~ ✅ FIXED
- **File:** `src/nl/typo.rs`, `src/nl/intent.rs`
- **Symptom:** User says "never mind" after a plan is created → instead of rejecting, a *new* `find_matching` workflow is created.
- **Root cause:** SymSpell typo-corrects "never" → "need" (edit distance 2) and "mind" → "find" (edit distance 1). The tokens become `["need", "find"]`, which triggers `CreateWorkflow` with `find_matching`. The reject pattern `"never mind"` in `is_reject()` never matches because the tokens are already mangled.
- **Repro:** `process_input("never mind", &mut state)` after creating any plan.
- **Fix:** Add "mind", "never" to SymSpell dictionary as protected words.

### ~~BUG-002: "search for TODO" extracts wrong keyword~~ ✅ FIXED
- **File:** `src/nl/typo.rs`, `src/nl/dialogue.rs`
- **Symptom:** `search for TODO in ~/src` produces `search_content` with `pattern: "good"` instead of `pattern: "TODO"`.
- **Root cause:** SymSpell typo-corrects "todo" → "good" (Damerau-Levenshtein distance 2: transpose d↔o then t→g). The corrected token "good" becomes the first keyword and gets used as the search pattern.
- **Repro:** `process_input("search for TODO in ~/src", &mut state)`.
- **Fix:** Add "todo", "fixme", "hack", "note", "xxx" to SymSpell dictionary.

### ~~BUG-003: "search for FIXME" loses keyword entirely~~ ✅ FIXED
- **File:** `src/nl/typo.rs`
- **Symptom:** `search for FIXME in ~/src` produces `search_content` with no pattern at all.
- **Root cause:** "fixme" gets typo-corrected to something that's absorbed as a stopword or op component. No keyword survives to the slot extractor.
- **Repro:** `process_input("search for FIXME in ~/src", &mut state)`.
- **Fix:** Add "fixme" to SymSpell dictionary.

---

## 🟡 Medium

### ~~BUG-004: Comma-containing approve phrases not recognized~~ ✅ FIXED
- **File:** `src/nl/intent.rs`
- **Symptom:** "perfect, ship it" and "yep, run it" return `NeedsClarification` instead of `Approved`.
- **Root cause:** After punctuation stripping, tokens are `["perfect", "ship", "it"]`. The joined string `"perfect ship it"` doesn't match any multi-approval pattern. The patterns have `"ship it"` but not `"perfect ship it"`.
- **Repro:** `process_input("perfect, ship it", &mut state)` after creating a plan.
- **Fix:** Add compound approval patterns, or check if the first token is a single-word approval and the rest form a multi-word approval.

### ~~BUG-005: Several natural rejection phrases not recognized~~ ✅ FIXED
- **File:** `src/nl/intent.rs`
- **Symptom:** "actually no", "wait no", "nah I'm good", "forget about it" all return `NeedsClarification` instead of `Rejected`.
- **Root cause:** These phrases aren't in the `multi_rejects` list. After contraction expansion, "nah I'm good" becomes "nah i am good" which doesn't match any pattern. "actually no" and "wait no" are also missing.
- **Repro:** `process_input("actually no", &mut state)` after creating a plan.
- **Fix:** Add these phrases to the multi-reject list.

### ~~BUG-006: Double approve succeeds~~ ✅ FIXED
- **File:** `src/nl/mod.rs`
- **Symptom:** After approving a plan, approving again returns `Approved` a second time.
- **Root cause:** The `Intent::Approve` handler returns `NlResponse::Approved` but does NOT clear `state.current_workflow`. The workflow remains set, so the guard `state.current_workflow.is_some()` passes again.
- **Repro:** Create plan → approve → approve again.
- **Fix:** Set `state.current_workflow = None` after returning `Approved`.

### ~~BUG-007: Skip filter uses action word as pattern~~ ✅ FIXED
- **File:** `src/nl/dialogue.rs`, `src/nl/slots.rs`
- **Symptom:** "skip any subdirectory named .git" produces `filter: {exclude: "skip"}` instead of `filter: {exclude: ".git"}`.
- **Root cause:** `apply_skip()` uses `slots.keywords.first()`. The slot extractor puts "skip" into keywords (it's not a stopword), so it becomes the first keyword. The actual target ".git" is also a keyword but comes later (extracted via the "named X" pattern at position 3).
- **Repro:** Create a walk_tree plan, then `process_input("skip any subdirectory named .git", &mut state)`.
- **Fix:** Filter out edit-action words ("skip", "exclude", "ignore", "omit") from the keyword list before selecting the skip target, or use a dedicated extraction path.

### ~~BUG-008: "remove the step" looks for op named 'delete'~~ ✅ FIXED
- **File:** `src/nl/dialogue.rs`, `src/nl/normalize.rs`
- **Symptom:** "remove the step" on a workflow returns error "No step with operation 'delete' found."
- **Root cause:** Synonym mapping converts "remove" → "delete". The slot extractor sees "delete" as a canonical op name. `resolve_step_index()` has no step_ref (since "step" is the last token with no number after it), so it falls through to op-name lookup and searches for a step with op "delete".
- **Repro:** Create a multi-step plan, then `process_input("remove the step", &mut state)`.
- **Fix:** In `resolve_step_index()`, when the intent is Remove and no step_ref or op is found, default to the last step.

---

## 🟢 Minor / Polish

### BUG-009: Multi-input ops only capture first file
- **File:** `src/nl/slots.rs`, `src/nl/dialogue.rs`
- **Symptom:** "diff old.txt and new.txt" only captures `old.txt` as input. "copy report.txt to backup.txt" only captures `report.txt`. "rename old.txt to new.txt" fails validation entirely.
- **Root cause:** The slot extractor puts all detected paths into `target_path` (first one wins). There's no concept of a "second argument" or destination path. `build_workflow()` only wires the first path.
- **Repro:** `process_input("diff old.txt and new.txt", &mut state)`.
- **Status:** Deferred — requires significant slot extraction redesign (source/destination semantics).

### ~~BUG-010: "sure why not" and "yeah that works" not recognized as approval~~ ✅ FIXED
- **File:** `src/nl/intent.rs`
- **Symptom:** These natural approval phrases return `NeedsClarification`.
- **Root cause:** Not in the multi-approval patterns list.
- **Repro:** `process_input("sure why not", &mut state)` after creating a plan.
- **Fix:** Add to multi-approval list.

### ~~BUG-011: "I have something in mind" creates a workflow~~ ✅ FIXED
- **File:** `src/nl/typo.rs`
- **Symptom:** Casual English sentence creates a `find_matching` workflow.
- **Root cause:** "mind" → "find" via SymSpell, "have" is a stopword, so the intent parser sees "find" and creates a workflow.
- **Repro:** `process_input("I have something in mind", &mut state)`.
- **Fix:** Add "mind" to SymSpell dictionary (same fix as BUG-001).

### BUG-012: "compres teh flie" fails validation
- **File:** `src/nl/typo.rs`, `src/nl/slots.rs`
- **Symptom:** All three words are typo-corrected ("compres"→"compress", "teh"→"the", "flie"→"file"), but "file" is a common word, not a path. No path is detected, so `build_workflow` uses "." as default path, producing `Dir(Bytes)` which fails type-checking for `gzip_compress` (expects `File(a)`).
- **Root cause:** "file" after correction is a bare word, not recognized as a path (no extension, no slash). The user meant "the file" generically but there's no actual filename.
- **Status:** Working as designed — the system correctly asks for clarification when no specific file is provided. The error message could be friendlier.

### ~~BUG-013: "also skip X" not recognized as edit~~ ✅ FIXED
- **File:** `src/nl/intent.rs`
- **Symptom:** "also skip node_modules" after creating a plan returns `NeedsClarification` instead of `PlanEdited`.
- **Root cause:** `try_edit()` checks `tokens.first()` which is "also", not an edit keyword. The function returns `None` and the input falls through to the fallback.
- **Repro:** Create a walk_tree plan, then `process_input("also skip node_modules", &mut state)`.
- **Fix:** Skip filler prefixes ("also", "and", "then", "now", "plus", "additionally", "but", "oh", "hey", "ok", "okay", "so", "well") before checking the first meaningful token for edit keywords.

### ~~BUG-014: "ok search for TODO" captures "ok" as keyword~~ ✅ FIXED
- **File:** `src/nl/slots.rs`
- **Symptom:** `ok search for TODO in ~/src` produces `search_content` with `pattern: "ok"` instead of `pattern: "TODO"`.
- **Root cause:** "ok" is not in the stopwords list in `extract_slots()`. It gets classified as a `Keyword` and becomes the first keyword, which `build_workflow` uses as the search pattern.
- **Repro:** `process_input("ok search for TODO in ~/src", &mut state)`.
- **Fix:** Add conversational fillers ("ok", "okay", "sure", "yes", "yeah", "yep", "yea", "alright", "right", "well", "hmm", "um", "uh", "hey", "oh") to the stopwords list.

### ~~BUG-015: "ok lgtm" not recognized as approval~~ ✅ FIXED
- **File:** `src/nl/intent.rs`
- **Symptom:** "ok lgtm" returns `NeedsClarification` instead of `Approved`.
- **Root cause:** The compound approval logic checks if the tail is a multi-word approval, but "lgtm" is a single-word approval. The tail `"lgtm"` doesn't match any multi-word pattern.
- **Repro:** `process_input("ok lgtm", &mut state)` after creating a plan.
- **Fix:** Also check if the tail is a single-word approval (not just multi-word).

### ~~BUG-016: `~/backup/database.sql` typed as Dir(Bytes)~~ ✅ FIXED
- **File:** `src/workflow.rs`, `src/nl/dialogue.rs`
- **Symptom:** `compress ~/backup/database.sql` fails type-checking because the input is typed as `Dir(Bytes)` instead of `File(Bytes)`.
- **Root cause:** In `infer_input_type()`, the `value.starts_with("~/")` directory heuristic fires BEFORE extension-based file detection. A path like `~/backup/database.sql` matches the `~/` prefix and gets typed as a directory. The `.sql` extension check doesn't exist in the explicit extension list (only `.txt`, `.json`, `.yaml`, etc. are checked).
- **Repro:** `process_input("compress ~/backup/database.sql", &mut state)`.
- **Fix:** Added `has_known_file_extension()` check and Image file check before the directory heuristic. Also synced `is_file_path()` in `dialogue.rs` with expanded extension list.

---

## 📊 Summary: 16 bugs total — 14 fixed, 1 deferred, 1 by-design

## 📋 Discovered but not bugs

| Input | Behavior | Assessment |
|-------|----------|------------|
| YAML injection in path | Validation catches it | ✅ Safe |
| Unicode paths (résumé.pdf) | Works correctly | ✅ |
| Windows paths (C:\...) | Works correctly | ✅ |
| Relative paths (./src) | Works correctly | ✅ |
| Parent paths (../other) | Works correctly | ✅ |
| Empty/whitespace input | Returns NeedsClarification | ✅ |
| Punctuation-only input | Returns NeedsClarification | ✅ |
| Numbers-only input | Returns NeedsClarification | ✅ |
| Very long input (10k tokens) | Processes (slowly) | ⚠️ No length limit |
| Null bytes in path | Passes through | ⚠️ Edge case |
| Contradictory ops | First op wins | ⚠️ Acceptable |
| Op name as filename (delete.txt) | Correctly treated as file | ✅ |
| "hello" / "thanks" | NeedsClarification | ⚠️ Could be friendlier |
