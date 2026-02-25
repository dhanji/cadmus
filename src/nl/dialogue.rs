        
//! Dialogue state, focus stack, slot extraction, and DSL generation for the
//! NL UX layer.
//!
//! Maintains lightweight state across conversation turns:
//! - **FocusStack** — ranked entries for anaphora resolution ("it", "that")
//! - **DialogueState** — current plan, focus stack, turn count, last intent
//! - **Delta transforms** — apply edits to existing PlanDef
//! - **DSL generation** — build PlanDef from intent + slots
//!
//! Also contains **slot extraction** — the lightweight structured extraction
//! of paths, step references, patterns, modifiers, and keywords from token
//! sequences. Used exclusively for edit handling.
//!
//! CRITICAL: All generated PlanDefs are validated by feeding them through
//! `plan::compile_plan()`. The NL layer never bypasses the engine.

use std::collections::HashMap;

/// Edit actions for the EditStep intent.
#[derive(Debug, Clone, PartialEq)]
pub enum EditAction {
    Add,
    Remove,
    Move,
    Skip,
    Change,
    Insert,
}

use crate::plan::{PlanDef, RawStep, StepArgs};

// ---------------------------------------------------------------------------
// Slot types (moved from slots.rs)
// ---------------------------------------------------------------------------

/// A typed slot value extracted from user input.
#[derive(Debug, Clone, PartialEq)]
pub enum SlotValue {
    /// A filesystem path (~/Downloads, /tmp/foo, etc.)
    Path(String),
    /// A canonical operation name.
    OpName(String),
    /// A reference to a plan step (by number or relative position).
    StepRef(StepRef),
    /// A glob or regex pattern (*.pdf, foo*, etc.)
    Pattern(String),
    /// A key-value parameter.
    Param(String, String),
    /// A modifier flag.
    Modifier(Modifier),
    /// A keyword/name that doesn't fit other categories.
    Keyword(String),
}

/// A reference to a plan step.
#[derive(Debug, Clone, PartialEq)]
pub enum StepRef {
    /// Absolute step number (1-indexed).
    Number(u32),
    /// Relative: the previous step.
    Previous,
    /// Relative: the next step.
    Next,
    /// Relative: the first step.
    First,
    /// Relative: the last step.
    Last,
}

/// A modifier flag that affects operation behavior.
#[derive(Debug, Clone, PartialEq)]
pub enum Modifier {
    Recursive,
    CaseInsensitive,
    Reverse,
    Verbose,
    DryRun,
    Force,
    Quiet,
    All,
    Each,
}

/// An anchor position for edit operations (where to insert/move).
#[derive(Debug, Clone, PartialEq)]
pub enum Anchor {
    Before(StepRef),
    After(StepRef),
    AtEnd,
    AtStart,
}

/// Extracted slots from a token sequence.
#[derive(Debug, Clone)]
pub struct ExtractedSlots {
    /// All extracted slot values, in order of appearance.
    pub slots: Vec<SlotValue>,
    /// The primary target path, if any.
    pub target_path: Option<String>,
    /// The primary operation, if any.
    pub primary_op: Option<String>,
    /// Step references found.
    pub step_refs: Vec<StepRef>,
    /// Anchor for edit operations.
    pub anchor: Option<Anchor>,
    /// Modifiers found.
    pub modifiers: Vec<Modifier>,
    /// Pattern arguments (globs, names).
    pub patterns: Vec<String>,
    /// Keywords (unclassified meaningful tokens).
    pub keywords: Vec<String>,
}

impl ExtractedSlots {
    fn new() -> Self {
        Self {
            slots: Vec::new(),
            target_path: None,
            primary_op: None,
            step_refs: Vec::new(),
            anchor: None,
            modifiers: Vec::new(),
            patterns: Vec::new(),
            keywords: Vec::new(),
        }
    }
}

// ---------------------------------------------------------------------------
// Slot extraction
// ---------------------------------------------------------------------------

/// Extract structured slots from a sequence of tokens.
pub fn extract_slots(tokens: &[String]) -> ExtractedSlots {
    let mut result = ExtractedSlots::new();

    let stopwords = &super::vocab::vocab().stopwords;

    let mut i = 0;
    while i < tokens.len() {
        let token = &tokens[i];

        // 1. Path detection
        if is_path(token) {
            let path = token.clone();
            result.slots.push(SlotValue::Path(path.clone()));
            if result.target_path.is_none() {
                result.target_path = Some(path);
            }
            i += 1;
            continue;
        }

        // 2. Canonical op name
        if super::normalize::is_canonical_op(token) {
            result.slots.push(SlotValue::OpName(token.clone()));
            if result.primary_op.is_none() {
                result.primary_op = Some(token.clone());
            }
            i += 1;
            continue;
        }

        // 3. Step reference: "step N" or "step previous/next/first/last"
        if token == "step" {
            if let Some(next) = tokens.get(i + 1) {
                if let Some(step_ref) = parse_step_ref(next) {
                    result.slots.push(SlotValue::StepRef(step_ref.clone()));
                    result.step_refs.push(step_ref);
                    i += 2;
                    continue;
                }
            }
        }

        // 4. Bare step reference (number that could be a step)
        if let Ok(n) = token.parse::<u32>() {
            if n > 0 && n <= 100 {
                let step_ref = StepRef::Number(n);
                result.slots.push(SlotValue::StepRef(step_ref.clone()));
                result.step_refs.push(step_ref);
                i += 1;
                continue;
            }
        }

        // 5. Relative step references
        if let Some(step_ref) = parse_relative_ref(token) {
            result.slots.push(SlotValue::StepRef(step_ref.clone()));
            result.step_refs.push(step_ref);
            i += 1;
            continue;
        }

        // 6. Anchor: "before X" / "after X"
        if token == "before" || token == "after" {
            if let Some(next) = tokens.get(i + 1) {
                let step_ref = if next == "step" {
                    tokens.get(i + 2).and_then(|t| parse_step_ref(t))
                } else {
                    parse_step_ref(next)
                };
                if let Some(sr) = step_ref {
                    let anchor = if token == "before" {
                        Anchor::Before(sr)
                    } else {
                        Anchor::After(sr)
                    };
                    result.anchor = Some(anchor);
                    i += if tokens.get(i + 1).map(|t| t == "step").unwrap_or(false) { 3 } else { 2 };
                    continue;
                }
            }
        }

        // 7. Modifier detection
        if let Some(modifier) = parse_modifier(token) {
            result.slots.push(SlotValue::Modifier(modifier.clone()));
            result.modifiers.push(modifier);
            i += 1;
            continue;
        }

        // 8. Pattern detection (glob-like)
        if is_pattern(token) {
            result.slots.push(SlotValue::Pattern(token.clone()));
            result.patterns.push(token.clone());
            i += 1;
            continue;
        }

        // 9. Directory alias: "my desktop" → ~/Desktop, or bare "desktop" → ~/Desktop
        {
            let vocab = &super::vocab::vocab();
            let dir_aliases = &vocab.dir_aliases;

            if token == "my" {
                if let Some(next) = tokens.get(i + 1) {
                    if let Some(path) = dir_aliases.get(next.as_str()) {
                        let path = path.clone();
                        result.slots.push(SlotValue::Path(path.clone()));
                        if result.target_path.is_none() {
                            result.target_path = Some(path);
                        }
                        i += 2;
                        continue;
                    }
                }
            }

            if let Some(alias_path) = dir_aliases.get(token.as_str()) {
                let also_noun = vocab.noun_patterns.contains_key(token.as_str());
                if !(also_noun && result.target_path.is_some()) {
                    let path = alias_path.clone();
                    result.slots.push(SlotValue::Path(path.clone()));
                    if result.target_path.is_none() {
                        result.target_path = Some(path);
                    }
                    i += 1;
                    continue;
                }
            }
        }

        // 10. Noun-to-filetype pattern: "screenshots" → *.png, "photos" → *.png *.jpg etc.
        {
            let noun_patterns = &super::vocab::vocab().noun_patterns;
            if i + 1 < tokens.len() {
                let bigram = format!("{} {}", token, tokens[i + 1]);
                if let Some(patterns) = noun_patterns.get(bigram.as_str()) {
                    for p in patterns {
                        result.slots.push(SlotValue::Pattern(p.clone()));
                        result.patterns.push(p.clone());
                    }
                    i += 2;
                    continue;
                }
            }
            if let Some(patterns) = noun_patterns.get(token.as_str()) {
                for p in patterns {
                    result.slots.push(SlotValue::Pattern(p.clone()));
                    result.patterns.push(p.clone());
                }
                i += 1;
                continue;
            }
        }

        // 11. "named X" / "called X" — extract the name as a keyword
        if (token == "named" || token == "called" || token == "matching") && i + 1 < tokens.len() {
            let name = tokens[i + 1].clone();
            result.slots.push(SlotValue::Keyword(name.clone()));
            result.keywords.push(name);
            i += 2;
            continue;
        }

        // 12. Fuzzy op name matching
        if let Some(op) = fuzzy_match_op(token) {
            result.slots.push(SlotValue::OpName(op.clone()));
            if result.primary_op.is_none() {
                result.primary_op = Some(op);
            }
            i += 1;
            continue;
        }

        // 13. Skip stopwords, keep meaningful keywords
        if !stopwords.contains(token.as_str()) && !token.is_empty() {
            result.slots.push(SlotValue::Keyword(token.clone()));
            result.keywords.push(token.clone());
        }

        i += 1;
    }

    result
}

// ---------------------------------------------------------------------------
// Slot detection helpers
// ---------------------------------------------------------------------------

/// Check if a token looks like a filesystem path.
fn is_path(token: &str) -> bool {
    if token.contains(' ') { return true; }
    if token.starts_with("~/") || token.starts_with('/') || token.starts_with("$HOME")
        || token.starts_with("$PWD") || token.contains("://") { return true; }
    if token.contains('/') && token.contains('.') { return true; }
    if token.ends_with('/') && token.len() > 1 { return true; }
    if let Some(dot_pos) = token.rfind('.') {
        let ext = &token[dot_pos + 1..];
        let name_part = &token[..dot_pos];
        if !name_part.is_empty() && !ext.is_empty() && !name_part.contains('*')
            && crate::filetypes::dictionary().is_known_extension(ext) { return true; }
    }
    if token.contains('/') && token.len() > 2 { return true; }
    false
}

/// Check if a token looks like a glob/pattern.
fn is_pattern(token: &str) -> bool {
    (token.starts_with("*.") || token.starts_with('*') || token.ends_with('*'))
        && !super::normalize::is_canonical_op(token)
}

/// Parse a step reference from a token.
fn parse_step_ref(token: &str) -> Option<StepRef> {
    if let Ok(n) = token.parse::<u32>() {
        if n > 0 && n <= 100 { return Some(StepRef::Number(n)); }
    }
    parse_relative_ref(token)
}

/// Parse a relative step reference.
fn parse_relative_ref(token: &str) -> Option<StepRef> {
    match token {
        "previous" | "prev" | "preceding" => Some(StepRef::Previous),
        "next" | "following" | "subsequent" => Some(StepRef::Next),
        "first" | "beginning" | "start" => Some(StepRef::First),
        "last" | "end" | "final" => Some(StepRef::Last),
        _ => None,
    }
}

/// Parse a modifier from a token.
fn parse_modifier(token: &str) -> Option<Modifier> {
    match token {
        "recursive" | "recursively" | "recurse" => Some(Modifier::Recursive),
        "case-insensitive" | "caseinsensitive" | "nocase" | "ignorecase" => Some(Modifier::CaseInsensitive),
        "reverse" | "reversed" | "descending" | "desc" => Some(Modifier::Reverse),
        "verbose" | "detailed" => Some(Modifier::Verbose),
        "dry-run" | "dryrun" | "simulate" | "preview" => Some(Modifier::DryRun),
        "force" | "forced" => Some(Modifier::Force),
        "quiet" | "silent" | "suppress" => Some(Modifier::Quiet),
        "each" => Some(Modifier::Each),
        _ => None,
    }
}

/// Try to fuzzy-match a token against known canonical op names.
/// Returns Some(canonical_name) if edit distance <= 2, None otherwise.
fn fuzzy_match_op(token: &str) -> Option<String> {
    if token.len() < 4 { return None; }
    static COMMON_WORDS: &[&str] = &[
        "step", "file", "name", "type", "size", "time", "date",
        "path", "text", "data", "line", "word", "char", "byte",
        "skip", "stop", "help", "show", "make", "take", "give",
        "keep", "save", "load", "send", "call", "mean", "work",
        "like", "want", "need", "have", "been", "done", "here",
        "there", "what", "when", "where", "which", "that", "this",
        "from", "into", "with", "about", "after", "before",
    ];
    if COMMON_WORDS.contains(&token) { return None; }

    let ops = super::normalize::canonical_ops();
    let mut best: Option<(String, usize)> = None;
    for op in ops.iter() {
        if let Some(d) = edit_distance_bounded(token, op.as_str(), 2) {
            match &best {
                Some((_, best_d)) if d < *best_d => best = Some((op.to_string(), d)),
                None => best = Some((op.to_string(), d)),
                _ => {}
            }
        }
    }
    best.map(|(op, _)| op)
}

/// Compute edit distance with early termination if > max_distance.
fn edit_distance_bounded(a: &str, b: &str, max_distance: usize) -> Option<usize> {
    let a_chars: Vec<char> = a.chars().collect();
    let b_chars: Vec<char> = b.chars().collect();
    let (a_len, b_len) = (a_chars.len(), b_chars.len());
    if a_len.abs_diff(b_len) > max_distance { return None; }

    let mut prev = vec![0usize; b_len + 1];
    let mut curr = vec![0usize; b_len + 1];
    for j in 0..=b_len { prev[j] = j; }

    for i in 1..=a_len {
        curr[0] = i;
        let mut min_in_row = curr[0];
        for j in 1..=b_len {
            let cost = if a_chars[i - 1] == b_chars[j - 1] { 0 } else { 1 };
            curr[j] = (prev[j] + 1).min(curr[j - 1] + 1).min(prev[j - 1] + cost);
            min_in_row = min_in_row.min(curr[j]);
        }
        if min_in_row > max_distance { return None; }
        std::mem::swap(&mut prev, &mut curr);
    }

    let result = prev[b_len];
    if result <= max_distance { Some(result) } else { None }
}

// ---------------------------------------------------------------------------
// Focus stack for anaphora resolution
// ---------------------------------------------------------------------------

/// An entry in the focus stack, ranked by recency and type.
#[derive(Debug, Clone, PartialEq)]
pub enum FocusEntry {
    /// The last step that was edited/added.
    EditedStep { step_index: usize, op: String },
    /// The last operation that was mentioned.
    MentionedOp { op: String },
    /// The last path/artifact that was mentioned.
    Artifact { path: String },
    /// The whole plan.
    WholePlan,
}

/// A ranked focus stack for resolving references like "it" and "that".
#[derive(Debug, Clone)]
pub struct FocusStack {
    entries: Vec<FocusEntry>,
}

impl FocusStack {
    pub fn new() -> Self {
        Self { entries: Vec::new() }
    }

    /// Push a new entry to the top of the stack.
    pub fn push(&mut self, entry: FocusEntry) {
        // Remove duplicates
        self.entries.retain(|e| e != &entry);
        // Push to front (most recent)
        self.entries.insert(0, entry);
        // Keep stack bounded
        if self.entries.len() > 10 {
            self.entries.truncate(10);
        }
    }

    /// Resolve "it" — returns the top of the stack.
    pub fn resolve_it(&self) -> Option<&FocusEntry> {
        self.entries.first()
    }

    /// Resolve "that" — returns the last mentioned item.
    pub fn resolve_that(&self) -> Option<&FocusEntry> {
        self.entries.iter().find(|e| matches!(e,
            FocusEntry::MentionedOp { .. } | FocusEntry::EditedStep { .. }
        ))
    }

    /// Resolve "the plan" — returns WholePlan if present.
    pub fn resolve_plan(&self) -> Option<&FocusEntry> {
        self.entries.iter().find(|e| matches!(e, FocusEntry::WholePlan))
    }

    /// Get the ranking order: last_edited > last_mentioned > last_artifact > whole_plan.
    pub fn ranked(&self) -> Vec<&FocusEntry> {
        let mut result = Vec::new();
        // Edited steps first
        for e in &self.entries {
            if matches!(e, FocusEntry::EditedStep { .. }) {
                result.push(e);
            }
        }
        // Then mentioned ops
        for e in &self.entries {
            if matches!(e, FocusEntry::MentionedOp { .. }) && !result.contains(&e) {
                result.push(e);
            }
        }
        // Then artifacts
        for e in &self.entries {
            if matches!(e, FocusEntry::Artifact { .. }) && !result.contains(&e) {
                result.push(e);
            }
        }
        // Then whole plan
        for e in &self.entries {
            if matches!(e, FocusEntry::WholePlan) && !result.contains(&e) {
                result.push(e);
            }
        }
        result
    }
}

// ---------------------------------------------------------------------------
// Dialogue state
// ---------------------------------------------------------------------------

/// The full dialogue state maintained across conversation turns.
#[derive(Debug, Clone)]
pub struct DialogueState {
    /// The current plan being built/edited (None if no plan yet).
    pub current_plan: Option<PlanDef>,
    /// Alternative intent interpretations from the Earley parser.
    pub alternative_intents: Vec<crate::nl::intent_ir::IntentIR>,
    /// Focus stack for anaphora resolution.
    pub focus: FocusStack,
    /// Number of turns in this conversation.
    pub turn_count: usize,
}

impl DialogueState {
    pub fn new() -> Self {
        Self {
            current_plan: None,
            alternative_intents: Vec::new(),
            focus: FocusStack::new(),
            turn_count: 0,
        }
    }

    /// Advance the turn counter.
    pub fn next_turn(&mut self) {
        self.turn_count += 1;
    }
}

// ---------------------------------------------------------------------------
// Dialogue errors
// ---------------------------------------------------------------------------

/// Errors from dialogue operations.
#[derive(Debug, Clone, PartialEq)]
pub enum DialogueError {
    /// Tried to edit but there's no current plan.
    NeedsContext(String),
    /// The edit target doesn't exist.
    InvalidTarget(String),
    /// Can't build a plan from the given intent.
    CannotBuild(String),
}

impl std::fmt::Display for DialogueError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DialogueError::NeedsContext(msg) => write!(f, "No current plan to edit: {}", msg),
            DialogueError::InvalidTarget(msg) => write!(f, "Invalid target: {}", msg),
            DialogueError::CannotBuild(msg) => write!(f, "Cannot build plan: {}", msg),
        }
    }
}

// ---------------------------------------------------------------------------
// Delta transforms: edit existing PlanDef
// ---------------------------------------------------------------------------

/// Apply an edit action to an existing PlanDef.
pub fn apply_edit(
    plan: &PlanDef,
    action: &EditAction,
    slots: &ExtractedSlots,
    state: &mut DialogueState,
) -> Result<(PlanDef, String), DialogueError> {
    let mut wf = plan.clone();

    match action {
        EditAction::Skip => apply_skip(&mut wf, slots, state),
        EditAction::Remove => apply_remove(&mut wf, slots, state),
        EditAction::Add => apply_add(&mut wf, slots, state),
        EditAction::Move => apply_move(&mut wf, slots, state),
        EditAction::Change => apply_change(&mut wf, slots, state),
        EditAction::Insert => apply_insert(&mut wf, slots, state),
    }
}

/// Add a filter step to skip items matching a pattern/name.
fn apply_skip(
    wf: &mut PlanDef,
    slots: &ExtractedSlots,
    state: &mut DialogueState,
) -> Result<(PlanDef, String), DialogueError> {
    // Determine what to skip
    // Filter out edit-action words — the user says "skip .git" but "skip" itself
    // ends up as a keyword. We want the actual target, not the action verb.
    let action_words = ["skip", "exclude", "ignore", "omit", "filter",
                        "remove", "delete", "drop", "hide", "subdirectory",
                        "subdirectories", "subfolder", "subdir", "directory",
                        "directories", "folder", "folders", "file", "files",
                        "named", "called", "matching", "any", "every"];
    let skip_target = slots.keywords.iter().find(|k| !action_words.contains(&k.as_str()))
        .or(slots.patterns.first())
        .ok_or_else(|| DialogueError::InvalidTarget(
            "What should I skip? Provide a name or pattern.".to_string()
        ))?;

    // Build a filter step that excludes the target
    let mut params = HashMap::new();
    params.insert("exclude".to_string(), skip_target.clone());

    // Find the best position to insert the filter (after walk_tree if present)
    let insert_pos = wf.steps.iter()
        .position(|s| s.op == "walk_tree" || s.op == "walk_tree_hierarchy" || s.op == "list_dir")
        .map(|p| p + 1)
        .unwrap_or(0);

    let new_step = RawStep {
        op: "filter".to_string(),
        args: StepArgs::from_string_map(params),
    };

    wf.steps.insert(insert_pos, new_step);

    // Update focus
    state.focus.push(FocusEntry::EditedStep {
        step_index: insert_pos,
        op: "filter".to_string(),
    });

    let desc = format!(
        "Added a filter step to skip entries matching \"{}\" (and case variants).",
        skip_target
    );

    Ok((wf.clone(), desc))
}

/// Remove a step from the plan.
fn apply_remove(
    wf: &mut PlanDef,
    slots: &ExtractedSlots,
    state: &mut DialogueState,
) -> Result<(PlanDef, String), DialogueError> {
    // Try to resolve the step. If the primary_op is an edit-action verb
    // (e.g. "remove" → "delete" via synonym), it won't match any plan step.
    // In that case, default to the last step — "remove the step" means "remove the last step".
    let step_idx = match resolve_step_index(wf, slots) {
        Ok(idx) => idx,
        Err(_) => {
            // Check if the "op" is actually an edit-action verb, not a real plan op
            let action_verbs = ["delete", "remove", "drop", "cut"];
            let is_action_verb = slots.primary_op.as_ref()
                .map(|op| action_verbs.contains(&op.as_str()))
                .unwrap_or(false);
            if is_action_verb && !wf.steps.is_empty() {
                wf.steps.len() - 1 // default to last step
            } else {
                return Err(DialogueError::InvalidTarget(
                    "Which step? Provide a step number or operation name.".to_string()
                ));
            }
        }
    };

    let removed_op = wf.steps[step_idx].op.clone();
    wf.steps.remove(step_idx);

    state.focus.push(FocusEntry::MentionedOp { op: removed_op.clone() });

    let desc = format!("Removed step {} ({}).", step_idx + 1, removed_op);
    Ok((wf.clone(), desc))
}

/// Add a new step to the plan.
fn apply_add(
    wf: &mut PlanDef,
    slots: &ExtractedSlots,
    state: &mut DialogueState,
) -> Result<(PlanDef, String), DialogueError> {
    let op = slots.primary_op.as_deref()
        .or_else(|| slots.keywords.first().map(|s| s.as_str()))
        .ok_or_else(|| DialogueError::InvalidTarget(
            "What step should I add? Provide an operation name.".to_string()
        ))?;

    let new_step = build_step_from_slots(op, slots);

    let insert_pos = match &slots.anchor {
        Some(Anchor::Before(sr)) => resolve_step_ref(wf, sr)?,
        Some(Anchor::After(sr)) => resolve_step_ref(wf, sr)? + 1,
        Some(Anchor::AtStart) => 0,
        Some(Anchor::AtEnd) | None => wf.steps.len(),
    };

    let insert_pos = insert_pos.min(wf.steps.len());
    wf.steps.insert(insert_pos, new_step);

    state.focus.push(FocusEntry::EditedStep {
        step_index: insert_pos,
        op: op.to_string(),
    });

    let desc = format!("Added {} at step {}.", op, insert_pos + 1);
    Ok((wf.clone(), desc))
}

/// Move a step to a new position.
fn apply_move(
    wf: &mut PlanDef,
    slots: &ExtractedSlots,
    state: &mut DialogueState,
) -> Result<(PlanDef, String), DialogueError> {
    let from_idx = resolve_step_index(wf, slots)?;
    let step = wf.steps.remove(from_idx);
    let op = step.op.clone();

    let to_idx = match &slots.anchor {
        Some(Anchor::Before(sr)) => resolve_step_ref(wf, sr)?,
        Some(Anchor::After(sr)) => resolve_step_ref(wf, sr)? + 1,
        Some(Anchor::AtStart) => 0,
        Some(Anchor::AtEnd) | None => wf.steps.len(),
    };

    let to_idx = to_idx.min(wf.steps.len());
    wf.steps.insert(to_idx, step);

    state.focus.push(FocusEntry::EditedStep {
        step_index: to_idx,
        op: op.clone(),
    });

    let desc = format!("Moved {} from step {} to step {}.", op, from_idx + 1, to_idx + 1);
    Ok((wf.clone(), desc))
}

/// Change a step's parameters.
fn apply_change(
    wf: &mut PlanDef,
    slots: &ExtractedSlots,
    state: &mut DialogueState,
) -> Result<(PlanDef, String), DialogueError> {
    let step_idx = resolve_step_index(wf, slots)?;

    // Update the step's args with new values from slots
    let step = &mut wf.steps[step_idx];
    let op = step.op.clone();

    if let Some(pattern) = slots.patterns.first() {
        let mut params = match &step.args {
            StepArgs::Map(m) => m.iter().filter_map(|(k, v)| v.as_str().map(|s| (k.clone(), s.to_string()))).collect(),
            _ => HashMap::new(),
        };
        params.insert("pattern".to_string(), pattern.clone());
        step.args = StepArgs::from_string_map(params);
    } else if slots.keywords.len() >= 2 {
        // "change X to Y" — keywords[0] is old value, keywords[1] is new value
        let new_val = slots.keywords.last().unwrap().clone();
        match &step.args {
            StepArgs::Scalar(_) => {
                step.args = StepArgs::Scalar(new_val.clone());
            }
            StepArgs::Map(m) => {
                let mut params: HashMap<String, String> = m.iter()
                    .filter_map(|(k, v)| v.as_str().map(|s| (k.clone(), s.to_string())))
                    .collect();
                // Try to find and update the matching param
                let old_val = &slots.keywords[0];
                for (_, v) in params.iter_mut() {
                    if v == old_val.as_str() {
                        *v = new_val.clone();
                        break;
                    }
                }
                step.args = StepArgs::from_string_map(params);
            }
            StepArgs::None => {
                step.args = StepArgs::Scalar(new_val.clone());
            }
        }
    }

    state.focus.push(FocusEntry::EditedStep {
        step_index: step_idx,
        op: op.clone(),
    });

    let desc = format!("Updated step {} ({}).", step_idx + 1, op);
    Ok((wf.clone(), desc))
}

/// Insert a step (alias for add, but with explicit position).
fn apply_insert(
    wf: &mut PlanDef,
    slots: &ExtractedSlots,
    state: &mut DialogueState,
) -> Result<(PlanDef, String), DialogueError> {
    apply_add(wf, slots, state)
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Resolve a step index from extracted slots.
fn resolve_step_index(wf: &PlanDef, slots: &ExtractedSlots) -> Result<usize, DialogueError> {
    if let Some(step_ref) = slots.step_refs.first() {
        resolve_step_ref(wf, step_ref)
    } else if let Some(op) = &slots.primary_op {
        // Find step by op name
        wf.steps.iter().position(|s| s.op == *op)
            .ok_or_else(|| DialogueError::InvalidTarget(
                format!("No step with operation '{}' found.", op)
            ))
    } else {
        Err(DialogueError::InvalidTarget(
            "Which step? Provide a step number or operation name.".to_string()
        ))
    }
}

/// Resolve a StepRef to a 0-indexed position.
fn resolve_step_ref(wf: &PlanDef, step_ref: &StepRef) -> Result<usize, DialogueError> {
    match step_ref {
        StepRef::Number(n) => {
            let idx = (*n as usize).saturating_sub(1);
            if idx < wf.steps.len() {
                Ok(idx)
            } else {
                Err(DialogueError::InvalidTarget(
                    format!("Step {} doesn't exist (plan has {} steps).", n, wf.steps.len())
                ))
            }
        }
        StepRef::Previous => {
            if wf.steps.len() > 1 {
                Ok(wf.steps.len() - 2)
            } else {
                Ok(0)
            }
        }
        StepRef::Next => Ok(wf.steps.len()), // past the end = append
        StepRef::First => Ok(0),
        StepRef::Last => {
            if wf.steps.is_empty() {
                Err(DialogueError::InvalidTarget("Plan has no steps.".to_string()))
            } else {
                Ok(wf.steps.len() - 1)
            }
        }
    }
}

/// Build a RawStep from an op name and extracted slots.
fn build_step_from_slots(op: &str, slots: &ExtractedSlots) -> RawStep {
    let mut params = HashMap::new();

    for slot in &slots.slots {
        match slot {
            SlotValue::Pattern(p) => { params.insert("pattern".to_string(), p.clone()); }
            SlotValue::Param(k, v) => { params.insert(k.clone(), v.clone()); }
            _ => {}
        }
    }

    let args = if params.is_empty() {
        StepArgs::None
    } else {
        StepArgs::from_string_map(params)
    };

    RawStep {
        op: op.to_string(),
        args,
    }
}

// ---------------------------------------------------------------------------
// Plan serialization to YAML
// ---------------------------------------------------------------------------

/// Serialize a PlanDef to sexpr string.
pub fn plan_to_sexpr(wf: &PlanDef) -> String {
    crate::sexpr::plan_to_sexpr(wf)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    // -- Plan YAML serialization --

    #[test]
    fn test_plan_to_sexpr_roundtrip() {
        let wf = PlanDef {
            name: "test-plan".to_string(),
            inputs: vec![crate::plan::PlanInput::bare("path")],
            output: None,
            steps: vec![
                RawStep { op: "walk_tree".to_string(), args: StepArgs::None },
                RawStep { op: "sort_by".to_string(), args: StepArgs::Scalar("name".to_string()) },
            ],
            bindings: HashMap::new(),
        };

        let sexpr = plan_to_sexpr(&wf);
        assert!(sexpr.contains("test-plan"), "got: {}", sexpr);
        assert!(sexpr.contains("walk_tree"), "got: {}", sexpr);
        assert!(sexpr.contains("sort_by"), "got: {}", sexpr);

        // Verify it can be parsed back
        let parsed = crate::sexpr::parse_sexpr_to_plan(&sexpr).unwrap();
        assert_eq!(parsed.steps.len(), 2);
        assert_eq!(parsed.steps[0].op, "walk_tree");
        assert_eq!(parsed.steps[1].op, "sort_by");
    }

    #[test]
    fn test_plan_to_sexpr_with_map_args() {
        let wf = PlanDef {
            name: "filter-test".to_string(),
            inputs: vec![crate::plan::PlanInput::bare("path")],
            output: None,
            steps: vec![
                RawStep { op: "list_dir".to_string(), args: StepArgs::None },
                RawStep {
                    op: "filter".to_string(),
                    args: StepArgs::from_string_map({
                        let mut m = HashMap::new();
                        m.insert("pattern".to_string(), "*.pdf".to_string());
                        m
                    }),
                },
            ],
            bindings: HashMap::new(),
        };

        let sexpr = plan_to_sexpr(&wf);
        assert!(sexpr.contains("filter"), "got: {}", sexpr);
        assert!(sexpr.contains("pattern"), "got: {}", sexpr);
    }

    // -- Edit operations --

    #[test]
    fn test_edit_on_empty_state_error() {
        let mut state = DialogueState::new();
        // Skip with no keywords should fail
        let slots_empty = extract_slots(&[]);
        let result_empty = apply_edit(
            &PlanDef {
                name: "test".to_string(),
                inputs: vec![],
                output: None,
                steps: vec![],
                bindings: HashMap::new(),
            },
            &EditAction::Skip,
            &slots_empty,
            &mut state,
        );
        assert!(result_empty.is_err(), "skip with no keywords should fail");
    }

    #[test]
    fn test_edit_skip_adds_filter() {
        let mut state = DialogueState::new();
        let wf = PlanDef {
            name: "test".to_string(),
            inputs: vec![],
            output: None,
            steps: vec![
                RawStep { op: "walk_tree".to_string(), args: StepArgs::None },
                RawStep { op: "pack_archive".to_string(), args: StepArgs::None },
            ],
            bindings: HashMap::new(),
        };

        let normalized = crate::nl::normalize::normalize("skip any subdirectory named foo");
        let extracted = extract_slots(&normalized.tokens);

        let (edited, desc) = apply_skip(&mut wf.clone(), &extracted, &mut state).unwrap();
        assert!(edited.steps.iter().any(|s| s.op == "filter"), "should have filter step");
        assert!(
            desc.contains("foo") || desc.contains("subdirectory") || desc.contains("skip"),
            "desc should mention the skip target: {}", desc
        );
        let walk_pos = edited.steps.iter().position(|s| s.op == "walk_tree").unwrap();
        let filter_pos = edited.steps.iter().position(|s| s.op == "filter").unwrap();
        assert!(filter_pos > walk_pos, "filter should be after walk_tree");
    }

    #[test]
    fn test_edit_remove_step() {
        let mut state = DialogueState::new();
        let wf = PlanDef {
            name: "test".to_string(),
            inputs: vec![],
            output: None,
            steps: vec![
                RawStep { op: "walk_tree".to_string(), args: StepArgs::None },
                RawStep { op: "filter".to_string(), args: StepArgs::None },
                RawStep { op: "sort_by".to_string(), args: StepArgs::Scalar("name".to_string()) },
            ],
            bindings: HashMap::new(),
        };

        let extracted = extract_slots(&["step".to_string(), "2".to_string()]);
        let (edited, desc) = apply_remove(&mut wf.clone(), &extracted, &mut state).unwrap();
        assert_eq!(edited.steps.len(), 2);
        assert_eq!(edited.steps[0].op, "walk_tree");
        assert_eq!(edited.steps[1].op, "sort_by");
        assert!(desc.contains("filter"));
    }

    #[test]
    fn test_edit_move_step() {
        let mut state = DialogueState::new();
        let wf = PlanDef {
            name: "test".to_string(),
            inputs: vec![],
            output: None,
            steps: vec![
                RawStep { op: "walk_tree".to_string(), args: StepArgs::None },
                RawStep { op: "sort_by".to_string(), args: StepArgs::Scalar("name".to_string()) },
                RawStep { op: "filter".to_string(), args: StepArgs::None },
            ],
            bindings: HashMap::new(),
        };

        let extracted = extract_slots(&[
            "step".to_string(), "3".to_string(),
            "before".to_string(), "step".to_string(), "2".to_string(),
        ]);

        let (edited, _desc) = apply_move(&mut wf.clone(), &extracted, &mut state).unwrap();
        assert_eq!(edited.steps[0].op, "walk_tree");
        assert_eq!(edited.steps[1].op, "filter");
        assert_eq!(edited.steps[2].op, "sort_by");
    }

    // -- Focus stack --

    #[test]
    fn test_focus_stack_resolve_it() {
        let mut stack = FocusStack::new();
        stack.push(FocusEntry::WholePlan);
        stack.push(FocusEntry::MentionedOp { op: "filter".to_string() });
        stack.push(FocusEntry::EditedStep { step_index: 1, op: "walk_tree".to_string() });

        let resolved = stack.resolve_it().unwrap();
        assert!(matches!(resolved, FocusEntry::EditedStep { op, .. } if op == "walk_tree"));
    }

    #[test]
    fn test_focus_stack_ranking() {
        let mut stack = FocusStack::new();
        stack.push(FocusEntry::WholePlan);
        stack.push(FocusEntry::Artifact { path: "~/Downloads".to_string() });
        stack.push(FocusEntry::MentionedOp { op: "filter".to_string() });
        stack.push(FocusEntry::EditedStep { step_index: 1, op: "walk_tree".to_string() });

        let ranked = stack.ranked();
        assert!(matches!(ranked[0], FocusEntry::EditedStep { .. }));
        assert!(matches!(ranked[1], FocusEntry::MentionedOp { .. }));
        assert!(matches!(ranked[2], FocusEntry::Artifact { .. }));
        assert!(matches!(ranked[3], FocusEntry::WholePlan));
    }

    #[test]
    fn test_focus_stack_dedup() {
        let mut stack = FocusStack::new();
        stack.push(FocusEntry::MentionedOp { op: "filter".to_string() });
        stack.push(FocusEntry::MentionedOp { op: "walk_tree".to_string() });
        stack.push(FocusEntry::MentionedOp { op: "filter".to_string() });

        assert_eq!(stack.entries.len(), 2);
        assert!(matches!(&stack.entries[0], FocusEntry::MentionedOp { op } if op == "filter"));
    }

    // -- Dialogue state --

    #[test]
    fn test_dialogue_state_new() {
        let state = DialogueState::new();
        assert!(state.current_plan.is_none());
        assert_eq!(state.turn_count, 0);
    }

    #[test]
    fn test_dialogue_state_turn_count() {
        let mut state = DialogueState::new();
        state.next_turn();
        state.next_turn();
        assert_eq!(state.turn_count, 2);
    }
}
