//! Intent recognition via a small closed grammar over canonical edit instructions.
//!
//! Intents:
//! - **CreatePlan** — user wants to build a new plan (zip, find, list, etc.)
//! - **EditStep** — user wants to modify an existing plan step
//! - **ExplainOp** — user asks what an operation means
//! - **Approve** — user approves the current plan
//! - **Reject** — user rejects / wants to start over
//! - **AskQuestion** — user asks a general question
//! - **SetParam** — user wants to set/change a parameter
//!
//! The parser uses a ranked pattern list with greedy longest-match.
//! When multiple parses are possible, a small n-gram frequency table over
//! canonical intent/slot patterns biases selection. Otherwise, deterministic
//! backoff applies (CreatePlan if op detected, AskQuestion otherwise).

use crate::nl::normalize::{self, NormalizedInput, is_canonical_op};

// ---------------------------------------------------------------------------
// Intent types
// ---------------------------------------------------------------------------

/// A recognized user intent with extracted context.
#[derive(Debug, Clone, PartialEq)]
pub enum Intent {
    /// User wants to create a new plan.
    CreatePlan {
        /// The primary operation detected (canonical name).
        op: Option<String>,
        /// Raw tokens that weren't part of the op (potential targets/params).
        rest: Vec<String>,
    },
    /// User wants to edit an existing plan step.
    EditStep {
        /// The edit action (add, remove, move, skip, change, insert).
        action: EditAction,
        /// Remaining tokens for slot extraction.
        rest: Vec<String>,
    },
    /// User asks what an operation or concept means.
    ExplainOp {
        /// The operation or concept being asked about.
        subject: String,
        /// Remaining context tokens.
        rest: Vec<String>,
    },
    /// User approves the current plan.
    Approve,
    /// User rejects the current plan or wants to start over.
    Reject,
    /// User asks a general question.
    AskQuestion {
        /// The full question tokens.
        tokens: Vec<String>,
    },
    /// User wants to set or change a specific parameter.
    SetParam {
        /// Parameter name (if detected).
        param: Option<String>,
        /// Parameter value (if detected).
        value: Option<String>,
        /// Remaining tokens.
        rest: Vec<String>,
    },
    /// Input is ambiguous — we need clarification.
    NeedsClarification {
        /// What we need to know.
        needs: Vec<String>,
    },
}

/// Edit actions for EditStep intent.
#[derive(Debug, Clone, PartialEq)]
pub enum EditAction {
    Add,
    Remove,
    Move,
    Skip,
    Change,
    Insert,
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Parse a normalized input into an intent.
///
/// This is the main entry point for intent recognition. It takes the output
/// of the normalization + typo correction pipeline and produces a structured
/// intent.
pub fn parse_intent(normalized: &NormalizedInput) -> Intent {
    let tokens = &normalized.canonical_tokens;

    if tokens.is_empty() {
        return Intent::NeedsClarification {
            needs: vec!["I didn't catch that. What would you like to do?".to_string()],
        };
    }

    // 1. Check for exact-match intents (approve, reject) — highest priority
    if is_approve(tokens) {
        return Intent::Approve;
    }
    if is_reject(tokens) {
        return Intent::Reject;
    }

    // 2. Check for explain/question patterns — but NOT if the first token
    //    is a known verb/op (that's a command, not a question).
    //    E.g., "fibonacci fn where f 0 0..." is a command, not "explain fibonacci".
    let first_is_verb_or_op = tokens.first()
        .map(|t| is_canonical_op(t))
        .unwrap_or(false);
    if !first_is_verb_or_op {
        if let Some(intent) = try_explain(tokens) {
            return intent;
        }
    }

    // 3. Check for edit patterns (skip, add, remove, move, change, insert)
    if let Some(intent) = try_edit(tokens) {
        return intent;
    }

    // 4. Check for set-param patterns
    if let Some(intent) = try_set_param(tokens) {
        return intent;
    }

    // 5. Check for create-plan patterns (op name detected)
    if let Some(intent) = try_create_plan(tokens) {
        return intent;
    }

    // 6. Check for general question patterns
    if is_question(tokens) {
        return Intent::AskQuestion {
            tokens: tokens.clone(),
        };
    }

    // 7. Deterministic backoff: if any op name is present, assume CreatePlan
    for token in tokens {
        if is_canonical_op(token) {
            let op = token.clone();
            let rest: Vec<String> = tokens.iter().filter(|t| *t != token).cloned().collect();
            return Intent::CreatePlan {
                op: Some(op),
                rest,
            };
        }
    }

    // 8. Final fallback: NeedsClarification
    Intent::NeedsClarification {
        needs: vec![
            "I'm not sure what you'd like to do.".to_string(),
            "Try something like 'zip up ~/Downloads' or 'find all PDFs in ~/Documents'.".to_string(),
        ],
    }
}

/// Convenience: normalize + parse in one call.
pub fn recognize(input: &str) -> Intent {
    let normalized = normalize::normalize(input);
    parse_intent(&normalized)
}

// ---------------------------------------------------------------------------
// Approve / Reject patterns
// ---------------------------------------------------------------------------

/// Check if the input is an approval.
fn is_approve(tokens: &[String]) -> bool {
    let vocab = super::vocab::vocab();

    if tokens.len() == 1 {
        return vocab.approval_singles.contains(&tokens[0]);
    }

    // Multi-token approvals
    let joined = tokens.join(" ");

    if vocab.approval_multis.iter().any(|a| joined == *a || joined.starts_with(a)) {
        return true;
    }

    // Compound approval: single-word approval + multi-word approval tail
    if tokens.len() >= 2 {
        let first = &tokens[0];
        if vocab.approval_singles.contains(first) {
            let tail = tokens[1..].join(" ");
            if vocab.approval_multis.iter().any(|a| tail == *a || tail.starts_with(a)) {
                return true;
            }
            // Filler phrases: "sure why not", "yeah that works"
            if vocab.filler_phrases.iter().any(|f| tail == *f || tail.starts_with(f)) {
                return true;
            }
            // Compound: "ok lgtm", "sure yeah"
            let tail_tokens: Vec<&str> = tail.split_whitespace().collect();
            if tail_tokens.len() == 1 && vocab.approval_singles.contains(tail_tokens[0]) {
                return true;
            }
        }
    }

    false
}

/// Check if the input is a rejection.
fn is_reject(tokens: &[String]) -> bool {
    let vocab = super::vocab::vocab();

    if tokens.len() == 1 {
        return vocab.rejection_singles.contains(&tokens[0]);
    }

    let joined = tokens.join(" ");
    vocab.rejection_multis.iter().any(|r| joined == *r || joined.starts_with(r))
}

// ---------------------------------------------------------------------------
// Explain patterns
// ---------------------------------------------------------------------------

/// Try to parse an explain/question-about-op intent.
fn try_explain(tokens: &[String]) -> Option<Intent> {
    let joined = tokens.join(" ");

    // "what is X" / "what does X mean" / "explain X" / "describe X"
    // "what is walk" / "what does walk_tree mean" / "explain filter"

    // Pattern: "what is <subject>"
    if joined.starts_with("what is ") {
        let rest: Vec<String> = tokens[2..].to_vec();
        let subject = find_subject_in(&rest);
        if let Some(subj) = subject {
            return Some(Intent::ExplainOp {
                subject: subj,
                rest,
            });
        }
    }

    // Pattern: "what does <subject> mean" / "what does <subject> do"
    if joined.starts_with("what does ") || joined.starts_with("what do ") {
        let skip = if tokens.get(1).map(|s| s.as_str()) == Some("does") { 2 } else { 2 };
        let rest: Vec<String> = tokens[skip..]
            .iter()
            .filter(|t| *t != "mean" && *t != "do")
            .cloned()
            .collect();
        let subject = find_subject_in(&rest);
        if let Some(subj) = subject {
            return Some(Intent::ExplainOp {
                subject: subj,
                rest,
            });
        }
    }

    // Pattern: "explain <subject>" / "describe <subject>"
    if tokens.first().map(|s| s.as_str()) == Some("explain")
        || tokens.first().map(|s| s.as_str()) == Some("describe")
    {
        let rest: Vec<String> = tokens[1..].to_vec();
        let subject = find_subject_in(&rest);
        if let Some(subj) = subject {
            return Some(Intent::ExplainOp {
                subject: subj,
                rest,
            });
        }
    }

    // Pattern: "how does <subject> work"
    if joined.starts_with("how does ") || joined.starts_with("how do ") {
        let skip = 2;
        let rest: Vec<String> = tokens[skip..]
            .iter()
            .filter(|t| *t != "work" && *t != "works")
            .cloned()
            .collect();
        let subject = find_subject_in(&rest);
        if let Some(subj) = subject {
            return Some(Intent::ExplainOp {
                subject: subj,
                rest,
            });
        }
    }

    // Pattern: "if you are referring to <subject>" — this is our own output,
    // but handle it gracefully
    // Pattern: tokens contain a question word + an op name
    let question_words = ["what", "how", "why", "when", "where", "which", "who"];
    let has_question = tokens.iter().any(|t| question_words.contains(&t.as_str()));
    if has_question {
        // Look for an op name in the tokens
        for token in tokens {
            if is_canonical_op(token) {
                let rest: Vec<String> = tokens.iter().filter(|t| *t != token).cloned().collect();
                return Some(Intent::ExplainOp {
                    subject: token.clone(),
                    rest,
                });
            }
        }
    }

    None
}

/// Find the most likely subject (op name or concept) in a token list.
fn find_subject_in(tokens: &[String]) -> Option<String> {
    // First: look for a canonical op name
    for token in tokens {
        if is_canonical_op(token) {
            return Some(token.clone());
        }
    }
    // Second: look for a word that could be an op concept
    // (walking, filtering, sorting, etc.)
    let concept_map = [
        ("walking", "walk_tree"),
        ("filtering", "filter"),
        ("sorting", "sort_by"),
        ("searching", "search_content"),
        ("finding", "find_matching"),
        ("listing", "list_dir"),
        ("reading", "read_file"),
        ("writing", "write_file"),
        ("copying", "copy"),
        ("moving", "move_entry"),
        ("deleting", "delete"),
        ("removing", "delete"),
        ("renaming", "rename"),
        ("extracting", "extract_archive"),
        ("compressing", "pack_archive"),
        ("zipping", "pack_archive"),
        ("unzipping", "extract_archive"),
        ("downloading", "download"),
        ("uploading", "upload"),
        ("syncing", "sync"),
        ("diffing", "diff"),
        ("grepping", "search_content"),
    ];
    for token in tokens {
        for (gerund, op) in &concept_map {
            if token == *gerund {
                return Some(op.to_string());
            }
        }
    }
    // Third: return the first non-stopword token
    let stopwords = [
        "the", "a", "an", "is", "are", "was", "were", "be", "been",
        "being", "have", "has", "had", "do", "does", "did", "will",
        "would", "could", "should", "may", "might", "can", "shall",
        "to", "of", "in", "for", "on", "with", "at", "by", "from",
        "it", "its", "this", "that", "these", "those", "my", "your",
        "mean", "means", "work", "works",
    ];
    for token in tokens {
        if !stopwords.contains(&token.as_str()) && !token.is_empty() {
            return Some(token.clone());
        }
    }
    None
}

// ---------------------------------------------------------------------------
// Edit patterns
// ---------------------------------------------------------------------------

/// Try to parse an edit-step intent.
fn try_edit(tokens: &[String]) -> Option<Intent> {
    // Compound sentence detection: "skip that, compress X instead"
    // If the tokens contain both an edit-like prefix AND a canonical op name
    // with "instead"/"rather", this is a replacement — not an edit.
    // Let it fall through to try_create_plan.
    let has_instead = tokens.iter().any(|t| t == "instead" || t == "rather");
    let has_op = tokens.iter().any(|t| is_canonical_op(t));
    if has_instead && has_op {
        return None;
    }

    // Edit action keywords
    let action_map: &[(&[&str], EditAction)] = &[
        (&["skip", "exclude", "ignore", "omit"], EditAction::Skip),
        (&["remove", "delete", "drop", "cut"], EditAction::Remove),
        (&["add", "append", "include"], EditAction::Add),
        (&["move", "reorder", "swap", "rearrange"], EditAction::Move),
        (&["change", "modify", "update", "alter", "set", "use"], EditAction::Change),
        (&["insert", "prepend", "put"], EditAction::Insert),
    ];

    // Skip filler prefixes so "also skip X", "and remove step 2", "then add filter" work.
    let filler_prefixes = &super::vocab::vocab().filler_prefixes;
    let mut start = 0;
    while start < tokens.len() && filler_prefixes.contains(&tokens[start]) {
        start += 1;
    }
    if start >= tokens.len() {
        return None;
    }
    let first = &tokens[start];

    // "skip" / "remove" / "add" / etc. as first token
    for (keywords, action) in action_map {
        if keywords.contains(&first.as_str()) {
            // If the first token is also a canonical op AND the remaining tokens
            // look like arithmetic operands (numbers, "and", "together", "from", "by"),
            // this is a create-plan, not an edit.
            if is_canonical_op(first) {
                let rest_tokens = &tokens[start + 1..];
                let looks_arithmetic = rest_tokens.iter().all(|t|
                    t.parse::<f64>().is_ok() || matches!(t.as_str(), "and" | "together" | "from" | "by" | "with" | "to" | "of")
                );
                if looks_arithmetic && rest_tokens.iter().any(|t| t.parse::<f64>().is_ok()) {
                    return None; // Let it fall through to try_create_plan
                }
            }
            // But not if it's clearly a create-plan (e.g., "delete files in ~/tmp")
            // Heuristic: if there's a path-like token, it might be a create-plan
            // But if there's a step reference or "step" keyword, it's definitely an edit
            let has_step_ref = tokens.iter().any(|t| {
                t == "step" || t == "previous" || t == "next" || t == "last"
                    || t == "before" || t == "after"
                    || t.parse::<u32>().is_ok()
            });
            let has_named = tokens.iter().any(|t| t == "named" || t == "called" || t == "matching");
            let has_subdirectory = tokens.iter().any(|t| {
                t == "subdirectory" || t == "subdirectories" || t == "subfolder"
                    || t == "subdir"
            });

            if has_step_ref || has_named || has_subdirectory
                || (action.clone() == EditAction::Skip)
            {
                let rest: Vec<String> = tokens[start + 1..].to_vec();
                return Some(Intent::EditStep {
                    action: action.clone(),
                    rest,
                });
            }
        }
    }

    // "move step 2 before step 1" pattern
    if first == "move_entry" || first == "move" {
        let has_step = tokens.iter().any(|t| t == "step");
        if has_step {
            let rest: Vec<String> = tokens[start + 1..].to_vec();
            return Some(Intent::EditStep {
                action: EditAction::Move,
                rest,
            });
        }
    }

    None
}

// ---------------------------------------------------------------------------
// Set-param patterns
// ---------------------------------------------------------------------------

/// Try to parse a set-parameter intent.
fn try_set_param(tokens: &[String]) -> Option<Intent> {
    let joined = tokens.join(" ");

    // "set X to Y" / "use Y for X" / "change X to Y"
    // "set extension to .pdf" / "use .pdf for extension"

    // Pattern: "set <param> to <value>"
    if let Some(rest) = joined.strip_prefix("set ") {
        if let Some(to_pos) = rest.find(" to ") {
            let param = rest[..to_pos].trim().to_string();
            let value = rest[to_pos + 4..].trim().to_string();
            return Some(Intent::SetParam {
                param: Some(param),
                value: Some(value),
                rest: tokens.to_vec(),
            });
        }
    }

    // Pattern: "use <value> for <param>"
    if let Some(rest) = joined.strip_prefix("use ") {
        if let Some(for_pos) = rest.find(" for ") {
            let value = rest[..for_pos].trim().to_string();
            let param = rest[for_pos + 5..].trim().to_string();
            return Some(Intent::SetParam {
                param: Some(param),
                value: Some(value),
                rest: tokens.to_vec(),
            });
        }
    }

    None
}

// ---------------------------------------------------------------------------
// Create-plan patterns
// ---------------------------------------------------------------------------

/// Try to parse a create-plan intent.
fn try_create_plan(tokens: &[String]) -> Option<Intent> {
    // Look for a canonical op name in the tokens
    let mut op = None;
    let mut rest = Vec::new();

    for token in tokens {
        if op.is_none() && is_canonical_op(token) {
            op = Some(token.clone());
        } else {
            rest.push(token.clone());
        }
    }

    // Also check for action verbs that imply plan creation
    let create_verbs = [
        "make", "create", "build", "generate", "produce", "run",
        "do", "execute", "perform", "start", "begin", "please",
        "can", "could", "would", "want", "need", "like",
        "help", "i",
    ];

    let has_create_verb = tokens.iter().any(|t| create_verbs.contains(&t.as_str()));

    if op.is_some() {
        return Some(Intent::CreatePlan { op, rest });
    }

    // If there's a create verb but no op, check if there's a path
    // (implies a filesystem operation)
    if has_create_verb {
        let has_path = tokens.iter().any(|t| {
            t.starts_with("~/") || t.starts_with('/') || t.contains('.')
        });
        if has_path {
            return Some(Intent::CreatePlan { op: None, rest: tokens.to_vec() });
        }
    }

    None
}

// ---------------------------------------------------------------------------
// Question detection
// ---------------------------------------------------------------------------

/// Check if the input looks like a general question.
fn is_question(tokens: &[String]) -> bool {
    let question_starters = [
        "what", "how", "why", "when", "where", "which", "who",
        "can", "could", "would", "should", "is", "are", "does",
        "will", "did", "has", "have",
    ];

    // "do" is excluded — it's more commonly imperative ("do X") than
    // interrogative ("do you...") in this context. We only treat it as
    // a question if followed by "you" or similar.
    let starts_with_question = tokens
        .first()
        .map(|t| question_starters.contains(&t.as_str()))
        .unwrap_or(false);

    starts_with_question
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // -- Approve --

    #[test]
    fn test_approve_lgtm() {
        assert_eq!(recognize("lgtm"), Intent::Approve);
    }

    #[test]
    fn test_approve_yes() {
        assert_eq!(recognize("yes"), Intent::Approve);
    }

    #[test]
    fn test_approve_ok() {
        assert_eq!(recognize("ok"), Intent::Approve);
    }

    #[test]
    fn test_approve_sounds_good() {
        assert_eq!(recognize("sounds good"), Intent::Approve);
    }

    #[test]
    fn test_approve_looks_great() {
        assert_eq!(recognize("looks great"), Intent::Approve);
    }

    #[test]
    fn test_approve_do_it() {
        assert_eq!(recognize("do it"), Intent::Approve);
    }

    #[test]
    fn test_approve_ship_it() {
        assert_eq!(recognize("ship it"), Intent::Approve);
    }

    #[test]
    fn test_approve_go_ahead() {
        assert_eq!(recognize("go ahead"), Intent::Approve);
    }

    #[test]
    fn test_approve_sure() {
        assert_eq!(recognize("sure"), Intent::Approve);
    }

    #[test]
    fn test_approve_y() {
        assert_eq!(recognize("y"), Intent::Approve);
    }

    // -- Reject --

    #[test]
    fn test_reject_no() {
        assert_eq!(recognize("no"), Intent::Reject);
    }

    #[test]
    fn test_reject_nah() {
        assert_eq!(recognize("nah"), Intent::Reject);
    }

    #[test]
    fn test_reject_start_over() {
        assert_eq!(recognize("start over"), Intent::Reject);
    }

    #[test]
    fn test_reject_scratch_that() {
        assert_eq!(recognize("scratch that"), Intent::Reject);
    }

    #[test]
    fn test_reject_nope() {
        assert_eq!(recognize("nope"), Intent::Reject);
    }

    #[test]
    fn test_reject_cancel() {
        assert_eq!(recognize("cancel"), Intent::Reject);
    }

    // -- ExplainOp --

    #[test]
    fn test_explain_whats_walk_mean() {
        let intent = recognize("what's walk mean");
        match intent {
            Intent::ExplainOp { subject, .. } => {
                assert_eq!(subject, "walk_tree");
            }
            other => panic!("expected ExplainOp, got: {:?}", other),
        }
    }

    #[test]
    fn test_explain_what_is_filter() {
        let intent = recognize("what is filter");
        match intent {
            Intent::ExplainOp { subject, .. } => {
                assert_eq!(subject, "filter");
            }
            other => panic!("expected ExplainOp, got: {:?}", other),
        }
    }

    #[test]
    fn test_explain_what_does_walk_tree_do() {
        let intent = recognize("what does walk_tree do");
        match intent {
            Intent::ExplainOp { subject, .. } => {
                assert_eq!(subject, "walk_tree");
            }
            other => panic!("expected ExplainOp, got: {:?}", other),
        }
    }

    #[test]
    fn test_explain_explain_sort() {
        let intent = recognize("explain sort");
        match intent {
            Intent::ExplainOp { subject, .. } => {
                assert_eq!(subject, "sort_by");
            }
            other => panic!("expected ExplainOp, got: {:?}", other),
        }
    }

    #[test]
    fn test_explain_how_does_filter_work() {
        let intent = recognize("how does filter work");
        match intent {
            Intent::ExplainOp { subject, .. } => {
                assert_eq!(subject, "filter");
            }
            other => panic!("expected ExplainOp, got: {:?}", other),
        }
    }

    // -- EditStep --

    #[test]
    fn test_edit_skip_subdirectory() {
        let intent = recognize("skip any subdirectory named foo");
        match intent {
            Intent::EditStep { action, rest } => {
                assert_eq!(action, EditAction::Skip);
                assert!(rest.iter().any(|t| t == "foo"), "rest: {:?}", rest);
            }
            other => panic!("expected EditStep, got: {:?}", other),
        }
    }

    #[test]
    fn test_edit_remove_step_2() {
        let intent = recognize("remove step 2");
        match intent {
            Intent::EditStep { action, rest } => {
                assert_eq!(action, EditAction::Remove);
                assert!(rest.contains(&"step".to_string()));
                assert!(rest.contains(&"2".to_string()));
            }
            other => panic!("expected EditStep, got: {:?}", other),
        }
    }

    #[test]
    fn test_edit_move_step() {
        let intent = recognize("move step 2 before step 1");
        match intent {
            Intent::EditStep { action, rest } => {
                assert_eq!(action, EditAction::Move);
                assert!(rest.contains(&"2".to_string()));
                assert!(rest.contains(&"before".to_string()));
            }
            other => panic!("expected EditStep, got: {:?}", other),
        }
    }

    #[test]
    fn test_edit_add_step() {
        let intent = recognize("add a filter step after step 1");
        match intent {
            Intent::EditStep { action, rest } => {
                assert_eq!(action, EditAction::Add);
                assert!(rest.iter().any(|t| t == "filter"), "rest: {:?}", rest);
            }
            other => panic!("expected EditStep, got: {:?}", other),
        }
    }

    #[test]
    fn test_edit_change_named() {
        let intent = recognize("change the pattern named foo to bar");
        match intent {
            Intent::EditStep { action, rest } => {
                assert_eq!(action, EditAction::Change);
                assert!(rest.contains(&"foo".to_string()));
                assert!(rest.contains(&"bar".to_string()));
            }
            other => panic!("expected EditStep, got: {:?}", other),
        }
    }

    // -- CreatePlan --

    #[test]
    fn test_create_zip_up_downloads() {
        let intent = recognize("zip up everything in my downloads");
        match intent {
            Intent::CreatePlan { op, rest } => {
                assert_eq!(op, Some("pack_archive".to_string()));
                assert!(rest.iter().any(|t| t == "everything" || t == "in" || t == "my" || t == "downloads"),
                    "rest: {:?}", rest);
            }
            other => panic!("expected CreatePlan, got: {:?}", other),
        }
    }

    #[test]
    fn test_create_find_pdfs() {
        let intent = recognize("find all PDFs in ~/Documents");
        match intent {
            Intent::CreatePlan { op, .. } => {
                assert!(op.is_some(), "should detect an op");
            }
            other => panic!("expected CreatePlan, got: {:?}", other),
        }
    }

    #[test]
    fn test_create_list_dir() {
        let intent = recognize("list ~/Downloads");
        match intent {
            Intent::CreatePlan { op, rest } => {
                assert_eq!(op, Some("list_dir".to_string()));
                assert!(rest.iter().any(|t| t.contains("Downloads")), "rest: {:?}", rest);
            }
            other => panic!("expected CreatePlan, got: {:?}", other),
        }
    }

    #[test]
    fn test_create_walk_tree() {
        let intent = recognize("walk the directory tree in /tmp");
        match intent {
            Intent::CreatePlan { op, rest } => {
                assert_eq!(op, Some("walk_tree".to_string()));
                assert!(rest.iter().any(|t| t == "/tmp"), "rest: {:?}", rest);
            }
            other => panic!("expected CreatePlan, got: {:?}", other),
        }
    }

    #[test]
    fn test_create_extract_archive() {
        let intent = recognize("extract the archive at ~/comic.cbz");
        match intent {
            Intent::CreatePlan { op, rest } => {
                assert_eq!(op, Some("extract_archive".to_string()));
                assert!(rest.iter().any(|t| t.contains("comic.cbz")), "rest: {:?}", rest);
            }
            other => panic!("expected CreatePlan, got: {:?}", other),
        }
    }

    #[test]
    fn test_create_grep_for_errors() {
        let intent = recognize("grep for errors in /var/log/app.log");
        match intent {
            Intent::CreatePlan { op, .. } => {
                assert_eq!(op, Some("search_content".to_string()));
            }
            other => panic!("expected CreatePlan, got: {:?}", other),
        }
    }

    // -- SetParam --

    #[test]
    fn test_set_param_extension() {
        let intent = recognize("set extension to .pdf");
        match intent {
            Intent::SetParam { param, value, .. } => {
                assert_eq!(param, Some("extension".to_string()));
                assert_eq!(value, Some(".pdf".to_string()));
            }
            other => panic!("expected SetParam, got: {:?}", other),
        }
    }

    #[test]
    fn test_set_param_use_for() {
        let intent = recognize("use .txt for extension");
        match intent {
            Intent::SetParam { param, value, .. } => {
                assert_eq!(param, Some("extension".to_string()));
                assert_eq!(value, Some(".txt".to_string()));
            }
            other => panic!("expected SetParam, got: {:?}", other),
        }
    }

    // -- NeedsClarification --

    #[test]
    fn test_empty_needs_clarification() {
        let normalized = normalize::normalize("");
        let intent = parse_intent(&normalized);
        match intent {
            Intent::NeedsClarification { .. } => {}
            other => panic!("expected NeedsClarification, got: {:?}", other),
        }
    }

    #[test]
    fn test_gibberish_needs_clarification() {
        let intent = recognize("asdfghjkl qwerty");
        match intent {
            Intent::NeedsClarification { needs } => {
                assert!(!needs.is_empty());
            }
            other => panic!("expected NeedsClarification, got: {:?}", other),
        }
    }

    // -- AskQuestion --

    #[test]
    fn test_general_question() {
        let intent = recognize("how many files can I process");
        match intent {
            Intent::AskQuestion { .. } | Intent::ExplainOp { .. } => {}
            other => panic!("expected AskQuestion or ExplainOp, got: {:?}", other),
        }
    }

    // -- Edge cases --

    #[test]
    fn test_approve_case_insensitive() {
        assert_eq!(recognize("LGTM"), Intent::Approve);
        assert_eq!(recognize("Yes"), Intent::Approve);
        assert_eq!(recognize("OK"), Intent::Approve);
        assert_eq!(recognize("Sounds Good"), Intent::Approve);
    }

    #[test]
    fn test_reject_case_insensitive() {
        assert_eq!(recognize("NO"), Intent::Reject);
        assert_eq!(recognize("Nah"), Intent::Reject);
        assert_eq!(recognize("CANCEL"), Intent::Reject);
    }

    // -- B5 bugfix: intent parser gaps --

    #[test]
    fn test_do_the_thing_needs_clarification() {
        let intent = recognize("do the thing");
        assert!(matches!(intent, Intent::NeedsClarification { .. }),
            "expected NeedsClarification, got: {:?}", intent);
    }

    #[test]
    fn test_nah_scrap_that_is_reject() {
        assert_eq!(recognize("nah scrap that"), Intent::Reject);
    }

    #[test]
    fn test_scrap_that_is_reject() {
        assert_eq!(recognize("scrap that"), Intent::Reject);
    }

    #[test]
    fn test_compound_skip_compress_instead() {
        let intent = recognize("actually skip that, compress my documents instead");
        match intent {
            Intent::CreatePlan { op, .. } => {
                assert_eq!(op, Some("gzip_compress".to_string()),
                    "should detect gzip_compress, got: {:?}", op);
            }
            other => panic!("expected CreatePlan, got: {:?}", other),
        }
    }

    #[test]
    fn test_compress_file_still_creates_plan() {
        let intent = recognize("compress my_file.txt");
        match intent {
            Intent::CreatePlan { op, .. } => {
                assert_eq!(op, Some("gzip_compress".to_string()));
            }
            other => panic!("expected CreatePlan, got: {:?}", other),
        }
    }

    // -- Red-team: rejection patterns --

    #[test]
    fn test_reject_never_mind() {
        assert_eq!(recognize("never mind"), Intent::Reject);
    }

    #[test]
    fn test_reject_actually_no() {
        assert_eq!(recognize("actually no"), Intent::Reject);
    }

    #[test]
    fn test_reject_wait_no() {
        assert_eq!(recognize("wait no"), Intent::Reject);
    }

    #[test]
    fn test_reject_nah_i_am_good() {
        assert_eq!(recognize("nah i am good"), Intent::Reject);
    }

    #[test]
    fn test_reject_forget_about_it() {
        assert_eq!(recognize("forget about it"), Intent::Reject);
    }

    // -- Red-team: approval patterns --

    #[test]
    fn test_approve_perfect_ship_it() {
        // "perfect, ship it" → tokens ["perfect", "ship", "it"]
        assert_eq!(recognize("perfect ship it"), Intent::Approve);
    }

    #[test]
    fn test_approve_yep_run_it() {
        assert_eq!(recognize("yep run it"), Intent::Approve);
    }

    #[test]
    fn test_approve_sure_why_not() {
        assert_eq!(recognize("sure why not"), Intent::Approve);
    }

    #[test]
    fn test_approve_yeah_that_works() {
        assert_eq!(recognize("yeah that works"), Intent::Approve);
    }

    #[test]
    fn test_approve_ok_go_for_it() {
        assert_eq!(recognize("ok go for it"), Intent::Approve);
    }

    // -- Hardening: filler prefix edits (I1) --

    #[test]
    fn test_also_skip_node_modules() {
        let intent = recognize("also skip node_modules");
        match intent {
            Intent::EditStep { action, rest } => {
                assert_eq!(action, EditAction::Skip);
                assert!(rest.iter().any(|t| t == "node_modules"), "rest: {:?}", rest);
            }
            other => panic!("expected EditStep(Skip), got: {:?}", other),
        }
    }

    #[test]
    fn test_and_remove_last_step() {
        let intent = recognize("and remove the last step");
        match intent {
            Intent::EditStep { action, .. } => {
                assert_eq!(action, EditAction::Remove);
            }
            other => panic!("expected EditStep(Remove), got: {:?}", other),
        }
    }

    #[test]
    fn test_then_skip_hidden_files() {
        let intent = recognize("then skip hidden files");
        match intent {
            Intent::EditStep { action, .. } => {
                assert_eq!(action, EditAction::Skip);
            }
            other => panic!("expected EditStep(Skip), got: {:?}", other),
        }
    }

    // -- Hardening: compound approval with single-word tail (I3) --

    #[test]
    fn test_approve_ok_lgtm() {
        assert_eq!(recognize("ok lgtm"), Intent::Approve);
    }

    #[test]
    fn test_approve_sure_yeah() {
        assert_eq!(recognize("sure yeah"), Intent::Approve);
    }

    #[test]
    fn test_approve_fine_ok() {
        assert_eq!(recognize("fine ok"), Intent::Approve);
    }
}
