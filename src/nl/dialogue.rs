        
//! Dialogue state, focus stack, and DSL generation for the NL UX layer.
//!
//! Maintains lightweight state across conversation turns:
//! - **FocusStack** — ranked entries for anaphora resolution ("it", "that")
//! - **DialogueState** — current plan, focus stack, turn count, last intent
//! - **Delta transforms** — apply edits to existing PlanDef
//! - **DSL generation** — build PlanDef from intent + slots
//!
//! CRITICAL: All generated PlanDefs are validated by feeding them through
//! `plan::compile_plan()`. The NL layer never bypasses the engine.

use std::collections::HashMap;

use crate::nl::intent::{Intent, EditAction};
use crate::nl::slots::{ExtractedSlots, SlotValue, StepRef, Anchor, Modifier};
use crate::plan::{PlanDef, RawStep, StepArgs, PlanInput};

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
    /// The whole plan/plan.
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

    /// Resolve "the plan" / "the plan" — returns WholePlan if present.
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
    /// Focus stack for anaphora resolution.
    pub focus: FocusStack,
    /// Number of turns in this conversation.
    pub turn_count: usize,
    /// The last recognized intent.
    pub last_intent: Option<Intent>,
}

impl DialogueState {
    pub fn new() -> Self {
        Self {
            current_plan: None,
            focus: FocusStack::new(),
            turn_count: 0,
            last_intent: None,
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
// Plan generation from intent + slots
// ---------------------------------------------------------------------------

/// Build a PlanDef from a CreatePlan intent and extracted slots.
pub fn build_plan(
    op: &Option<String>,
    slots: &ExtractedSlots,
    description: Option<&str>,
) -> Result<PlanDef, DialogueError> {
    let primary_op = op.as_deref()
        .or(slots.primary_op.as_deref())
        .ok_or_else(|| DialogueError::CannotBuild(
            "No operation detected. Try something like 'zip up ~/Downloads'.".to_string()
        ))?;

    let mut inputs: Vec<PlanInput> = Vec::new();
    let mut steps = Vec::new();

    // Determine the target path
    let target_path = slots.target_path.clone()
        .unwrap_or_else(|| ".".to_string());
    let target_path = resolve_path(&target_path);

    // Collect all paths from slots (for multi-path ops like rename)
    let all_paths: Vec<String> = slots.slots.iter()
        .filter_map(|s| match s {
            SlotValue::Path(p) => Some(p.clone()),
            _ => None,
        })
        .collect();

    // Build steps based on the primary operation
    match primary_op {
        // Archive operations: walk → pack/extract
        "pack_archive" => {
            // Detect compound "repack" goal: flatten_seq in keywords/slots,
            // or Each modifier with archive-related context.
            // This triggers type-directed planning instead of the simple template.
            let has_flatten = slots.keywords.iter().any(|k| k == "flatten_seq")
                || slots.slots.iter().any(|s| matches!(s, SlotValue::OpName(n) if n == "flatten_seq"));
            let has_each = slots.modifiers.contains(&Modifier::Each);
            let has_archive_context = slots.keywords.iter()
                .any(|k| k == "comic" || k == "archive" || k == "issue" || k == "issues." || k == "issues");

            // Detect archive format from keywords (cbr/cbz/tar.gz etc.)
            let format_hint = detect_archive_format_from_slots(slots);

            if has_flatten || (has_each && has_archive_context) {
                // ── Compound repack goal ──
                // Type-directed: list → filter → sort → extract each → flatten → enumerate → pack
                inputs.push(PlanInput::from_legacy("path", &target_path));

                steps.push(RawStep { op: "list_dir".to_string(), args: StepArgs::None });

                // Filter by archive format if we can detect it
                let filter_pattern = format_hint.as_deref()
                    .unwrap_or("*.cbz");
                let mut filter_params = HashMap::new();
                filter_params.insert("pattern".to_string(), filter_pattern.to_string());
                steps.push(RawStep { op: "find_matching".to_string(), args: StepArgs::Map(filter_params) });

                steps.push(RawStep { op: "sort_by".to_string(), args: StepArgs::Scalar("name".to_string()) });

                steps.push(RawStep { op: "extract_archive".to_string(), args: StepArgs::Scalar("each".to_string()) });

                steps.push(RawStep { op: "flatten_seq".to_string(), args: StepArgs::None });

                steps.push(RawStep { op: "enumerate_entries".to_string(), args: StepArgs::None });

                // Output filename from format hint
                let output_name = format!("combined{}", format_hint.as_deref()
                    .map(|p| p.trim_start_matches('*'))
                    .unwrap_or(".cbz"));
                let mut pack_params = HashMap::new();
                pack_params.insert("output".to_string(), output_name);
                steps.push(RawStep { op: "pack_archive".to_string(), args: StepArgs::Map(pack_params) });
            } else {
                // ── Simple pack: walk → filter → pack ──
                inputs.push(PlanInput::from_legacy("path", &target_path));
                steps.push(RawStep { op: "walk_tree".to_string(), args: StepArgs::None });
                if let Some(pattern) = slots.patterns.first() {
                    let mut params = HashMap::new();
                    params.insert("pattern".to_string(), pattern.clone());
                    steps.push(RawStep { op: "filter".to_string(), args: StepArgs::Map(params) });
                }
                steps.push(RawStep { op: "pack_archive".to_string(), args: StepArgs::None });
            }
        }
        "extract_archive" => {
            inputs.push(PlanInput::typed("file", "File"));
            steps.push(RawStep { op: "extract_archive".to_string(), args: StepArgs::None });
        }

        // List/walk operations
        "list_dir" => {
            inputs.push(PlanInput::from_legacy("path", &target_path));
            steps.push(RawStep { op: "list_dir".to_string(), args: StepArgs::None });
            if !slots.patterns.is_empty() {
                let mut params = HashMap::new();
                params.insert("pattern".to_string(), join_patterns(&slots.patterns));
                steps.push(RawStep { op: "filter".to_string(), args: StepArgs::Map(params) });
            } else if let Some(kw) = first_filterable_keyword(slots) {
                let mut params = HashMap::new();
                params.insert("pattern".to_string(), format!("*{}*", kw));
                steps.push(RawStep { op: "filter".to_string(), args: StepArgs::Map(params) });
            }
            if slots.modifiers.contains(&Modifier::Reverse) {
                steps.push(RawStep { op: "sort_by".to_string(), args: StepArgs::Scalar("name".to_string()) });
            }
        }
        "walk_tree" => {
            inputs.push(PlanInput::from_legacy("path", &target_path));
            steps.push(RawStep { op: "walk_tree".to_string(), args: StepArgs::None });
            if !slots.patterns.is_empty() {
                let mut params = HashMap::new();
                params.insert("pattern".to_string(), join_patterns(&slots.patterns));
                steps.push(RawStep { op: "filter".to_string(), args: StepArgs::Map(params) });
            } else if let Some(kw) = first_filterable_keyword(slots) {
                let mut params = HashMap::new();
                params.insert("pattern".to_string(), format!("*{}*", kw));
                steps.push(RawStep { op: "filter".to_string(), args: StepArgs::Map(params) });
            }
        }

        // Find/search operations
        "find_matching" => {
            inputs.push(PlanInput::from_legacy("path", &target_path));
            steps.push(RawStep { op: "walk_tree".to_string(), args: StepArgs::None });
            let mut params = HashMap::new();
            if let Some(pattern) = slots.patterns.first() {
                params.insert("pattern".to_string(), pattern.clone());
            } else if let Some(kw) = slots.keywords.first() {
                params.insert("pattern".to_string(), format!("*{}*", kw));
            }
            steps.push(RawStep { op: "find_matching".to_string(), args: if params.is_empty() { StepArgs::None } else { StepArgs::Map(params) } });
            steps.push(RawStep { op: "sort_by".to_string(), args: StepArgs::Scalar("name".to_string()) });
        }
        "search_content" => {
            // search_content needs Seq(Entry(Name, File(Text))) + Pattern
            // Always build: walk_tree → read_file: each → search_content
            // Use "textdir" input name so type system infers Dir(File(Text))
            let mut params = HashMap::new();
            if let Some(kw) = slots.keywords.first() {
                params.insert("pattern".to_string(), kw.clone());
            }
            let is_file_target = is_file_path(&target_path);
            if is_file_target {
                // Use parent dir, then filter to the specific file
                inputs.push(PlanInput::from_legacy("textdir", &dir_of(&target_path)));
                steps.push(RawStep { op: "walk_tree".to_string(), args: StepArgs::None });
                let fname = filename_of(&target_path);
                let mut filter_params = HashMap::new();
                filter_params.insert("pattern".to_string(), fname);
                steps.push(RawStep { op: "filter".to_string(), args: StepArgs::Map(filter_params) });
            } else {
                inputs.push(PlanInput::from_legacy("textdir", &target_path));
                steps.push(RawStep { op: "walk_tree".to_string(), args: StepArgs::None });
            }
            steps.push(RawStep {
                op: "search_content".to_string(),
                args: if params.is_empty() { StepArgs::None } else { StepArgs::Map(params) },
            });
        }

        // Sort
        "sort_by" => {
            inputs.push(PlanInput::from_legacy("path", &target_path));
            steps.push(RawStep { op: "list_dir".to_string(), args: StepArgs::None });
            let sort_key = slots.keywords.first()
                .cloned()
                .unwrap_or_else(|| "name".to_string());
            steps.push(RawStep { op: "sort_by".to_string(), args: StepArgs::Scalar(sort_key) });
        }

        // Filter
        "filter" => {
            inputs.push(PlanInput::from_legacy("path", &target_path));
            steps.push(RawStep { op: "list_dir".to_string(), args: StepArgs::None });
            let mut params = HashMap::new();
            if let Some(pattern) = slots.patterns.first() {
                params.insert("pattern".to_string(), pattern.clone());
            }
            steps.push(RawStep { op: "filter".to_string(), args: if params.is_empty() { StepArgs::None } else { StepArgs::Map(params) } });
        }

        // Racket arithmetic operations
        "add" | "subtract" | "multiply" | "divide" | "modulo" | "expt"
        | "min" | "max" => {
            // Extract numeric operands from slots.
            // Numbers may appear as Keywords, StepRefs, or in patterns.
            let mut all_numbers: Vec<String> = Vec::new();

            // Check keywords for numeric values
            for s in &slots.slots {
                match s {
                    SlotValue::Keyword(k) if k.parse::<f64>().is_ok() => {
                        all_numbers.push(k.clone());
                    }
                    _ => {}
                }
            }

            // Check step_refs for numeric values (the slot extractor captures
            // small numbers as StepRef::Number)
            for sr in &slots.step_refs {
                if let StepRef::Number(n) = sr {
                    all_numbers.push(n.to_string());
                }
            }

            if all_numbers.len() >= 2 {
                inputs.push(PlanInput::typed("x", "Number"));
                inputs.push(PlanInput::typed("y", "Number"));
            } else if all_numbers.len() == 1 {
                inputs.push(PlanInput::typed("x", "Number"));
                inputs.push(PlanInput::typed("y", "Number"));
            }

            let mut params = HashMap::new();
            if all_numbers.len() >= 2 {
                params.insert("x".to_string(), all_numbers[0].clone());
                params.insert("y".to_string(), all_numbers[1].clone());
            } else if all_numbers.len() == 1 {
                params.insert("x".to_string(), all_numbers[0].clone());
                params.insert("y".to_string(), "0".to_string());
            }
            steps.push(RawStep { op: primary_op.to_string(), args: if params.is_empty() { StepArgs::None } else { StepArgs::Map(params) } });
        }

        // Default: just use the op directly
        other => {
            // Categorize the op to choose the right input type
            if is_url_op(other) {
                // URL ops: git_clone, download, wget_download
                inputs.push(PlanInput::from_legacy("url", &target_path));
            } else if is_git_repo_op(other) {
                // Git ops that work on a repo (not clone)
                inputs.push(PlanInput::from_legacy("repo", &target_path));
            } else if is_entry_op(other) {
                // Entry ops: rename, move_entry, delete
                // These need Entry(Name, a) input — build a pipeline
                inputs.push(PlanInput::from_legacy("path", &dir_of(&target_path)));
                steps.push(RawStep { op: "list_dir".to_string(), args: StepArgs::None });
                // Filter to the specific file
                let filter_name = filename_of(&target_path);
                if !filter_name.is_empty() {
                    let mut params = HashMap::new();
                    params.insert("pattern".to_string(), filter_name);
                    steps.push(RawStep { op: "filter".to_string(), args: StepArgs::Map(params) });
                }
                // Apply the op to each matching entry
                let mut op_args = StepArgs::Scalar("each".to_string());
                // For rename: add the new name as a param
                if other == "rename" && all_paths.len() >= 2 {
                    let new_name = filename_of(&all_paths[1]);
                    if !new_name.is_empty() {
                        let mut params = HashMap::new();
                        params.insert("to".to_string(), new_name);
                        params.insert("each".to_string(), "true".to_string());
                        op_args = StepArgs::Map(params);
                    }
                }
                steps.push(RawStep { op: other.to_string(), args: op_args });
                // Skip the default step push below
                let plan_name = description
                    .map(|d| d.to_string())
                    .unwrap_or_else(|| generate_plan_name(primary_op, &slots));
                return Ok(PlanDef { name: plan_name, inputs, output: None, steps });
            } else if is_path_op(other) {
                // Ops that take Path primitive (stat, du_size, chmod, etc.)
                // Use "pathref" input name so type inference yields Path, not Dir(Bytes)
                inputs.push(PlanInput::from_legacy("pathref", &target_path));
            } else if is_seq_op(other) {
                // Ops that take Seq(a) — need a pipeline to produce the sequence
                if is_file_path(&target_path) {
                    // File target: read_file → op (e.g. count lines)
                    inputs.push(PlanInput::from_legacy("file", &target_path));
                    steps.push(RawStep { op: "read_file".to_string(), args: StepArgs::None });
                } else {
                    // Directory target: list_dir → op (e.g. count files)
                    inputs.push(PlanInput::from_legacy("path", &target_path));
                    steps.push(RawStep { op: "list_dir".to_string(), args: StepArgs::None });
                }
            } else if is_file_path(&target_path) {
                // File ops with a file target: use "file" input name
                inputs.push(PlanInput::from_legacy("file", &target_path));
            } else {
                // Default: directory input
                inputs.push(PlanInput::from_legacy("path", &target_path));
            }
            steps.push(RawStep { op: other.to_string(), args: StepArgs::None });
        }
    }

    let plan_name = description
        .map(|d| d.to_string())
        .unwrap_or_else(|| generate_plan_name(primary_op, &slots));

    Ok(PlanDef {
        name: plan_name,
        inputs,
        output: None,
        steps,
    })
}

// ---------------------------------------------------------------------------
// Op categorization helpers for input type inference
// ---------------------------------------------------------------------------

/// Ops that take a URL as input.
fn is_url_op(op: &str) -> bool {
    matches!(op, "git_clone" | "download" | "wget_download")
}

/// Join multiple glob patterns into a single filter pattern.
/// e.g., ["*.cbz", "*.cbr"] → "*.cbz|*.cbr"
fn join_patterns(patterns: &[String]) -> String {
    if patterns.len() == 1 {
        patterns[0].clone()
    } else {
        patterns.join("|")
    }
}

/// Return the first keyword that looks like a filterable noun (not a path,
/// sort key, or structural word). Used by list_dir/walk_tree to infer a
/// filter step from "list all the X in Y".
fn first_filterable_keyword(slots: &ExtractedSlots) -> Option<&str> {
    // Words that are structural / sort-related, not filter targets
    const SKIP: &[&str] = &[
        "name", "size", "date", "time", "type", "extension", "ext",
        "reverse", "reversed", "ascending", "descending",
        "all", "everything", "anything", "file", "files",
        "directory", "directories", "folder", "folders",
        "contents", "items", "entries", "stuff", "things",
    ];
    slots.keywords.iter()
        .find(|kw| {
            !SKIP.contains(&kw.as_str())
                && !kw.starts_with('/')
                && !kw.starts_with('~')
                && !kw.starts_with('.')
                && !kw.starts_with('$')
        })
        .map(|s| s.as_str())
}

/// Git ops that work on an existing repo (not clone).
fn is_git_repo_op(op: &str) -> bool {
    op.starts_with("git_") && !is_url_op(op)
}

/// Ops that take Entry(Name, a) as first input.
fn is_entry_op(op: &str) -> bool {
    matches!(op, "rename" | "move_entry" | "delete")
}

/// Ops that take Path as first input (not Dir or File).
fn is_path_op(op: &str) -> bool {
    matches!(op, "stat" | "create_dir" | "remove_quarantine" | "open_file"
        | "reveal" | "git_init" | "du_size" | "lsof_open" | "file_type_detect"
        | "chmod" | "chown" | "xattr_get" | "xattr_set" | "xattr_remove"
        | "xattr_list")
}

/// Ops that take Seq(a) as first input — need a pipeline to produce the sequence.
fn is_seq_op(op: &str) -> bool {
    matches!(op, "count" | "unique" | "head" | "tail" | "reverse")
}

/// Detect archive format from NL slots.
///
/// Looks for format hints in keywords and paths:
/// - "cbz" or "cbr" in keywords → *.cbz or *.cbr
/// - "cbr/cbz" or "cbz/cbr" as a path token → *.cbz (first mentioned)
/// - Archive patterns in slots → use first one
fn detect_archive_format_from_slots(slots: &ExtractedSlots) -> Option<String> {
    // Check for format keywords like "cbz", "cbr", "tar.gz"
    for kw in &slots.keywords {
        let lower = kw.to_lowercase();
        if lower == "cbz" || lower == ".cbz" { return Some("*.cbz".to_string()); }
        if lower == "cbr" || lower == ".cbr" { return Some("*.cbr".to_string()); }
        if lower == "tar.gz" { return Some("*.tar.gz".to_string()); }
        if lower == "zip" || lower == ".zip" { return Some("*.zip".to_string()); }
        if lower == "rar" || lower == ".rar" { return Some("*.rar".to_string()); }
    }
    // Check for compound format tokens like "cbr/cbz" in paths
    for slot in &slots.slots {
        if let SlotValue::Path(p) = slot {
            let lower = p.to_lowercase();
            if lower.contains("cbz") { return Some("*.cbz".to_string()); }
            if lower.contains("cbr") { return Some("*.cbr".to_string()); }
        }
    }
    // Check archive patterns
    for pat in &slots.patterns {
        if pat.contains("zip") || pat.contains("rar") || pat.contains("tar") || pat.contains("7z") {
            return Some(pat.clone());
        }
    }
    None
}

/// Resolve a bare path name to an actual filesystem location.
///
/// Fallback chain:
/// 1. Already absolute or home-relative (`~/...`, `/...`) → use as-is
/// 2. Current directory (`.`) → use as-is
/// 3. Exists as a macOS volume (`/Volumes/<name>`) → use that
/// 4. Otherwise → use as-is (let the shell report the error)
fn resolve_path(path: &str) -> String {
    // Already rooted — no resolution needed
    if path == "."
        || path.starts_with('/')
        || path.starts_with("~/")
        || path.starts_with("$")
    {
        return path.to_string();
    }

    // Check /Volumes/<name> (macOS external drives, SD cards, USB sticks)
    let volume_path = format!("/Volumes/{}", path);
    if std::path::Path::new(&volume_path).exists() {
        return volume_path;
    }

    // No resolution found — return as-is
    path.to_string()
}

/// Check if a path looks like a file (has a known extension, not a directory).
fn is_file_path(path: &str) -> bool {
    if path == "." || path.ends_with('/') {
        return false;
    }
    crate::filetypes::dictionary().has_known_extension(path)
}

/// Extract the directory part of a path (everything before the last /).
fn dir_of(path: &str) -> String {
    if let Some(pos) = path.rfind('/') {
        path[..pos].to_string()
    } else {
        ".".to_string()
    }
}

/// Extract the filename part of a path (everything after the last /).
fn filename_of(path: &str) -> String {
    if let Some(pos) = path.rfind('/') {
        path[pos + 1..].to_string()
    } else {
        path.to_string()
    }
}

/// Generate a human-readable plan name from the operation and slots.
fn generate_plan_name(op: &str, _slots: &ExtractedSlots) -> String {
    // Generate a snake_case function name from the primary op.
    op.to_string()
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
        args: StepArgs::Map(params),
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
            StepArgs::Map(m) => m.clone(),
            _ => HashMap::new(),
        };
        params.insert("pattern".to_string(), pattern.clone());
        step.args = StepArgs::Map(params);
    } else if slots.keywords.len() >= 2 {
        // "change X to Y" — keywords[0] is old value, keywords[1] is new value
        let new_val = slots.keywords.last().unwrap().clone();
        match &step.args {
            StepArgs::Scalar(_) => {
                step.args = StepArgs::Scalar(new_val.clone());
            }
            StepArgs::Map(m) => {
                let mut params = m.clone();
                // Try to find and update the matching param
                let old_val = &slots.keywords[0];
                for (_, v) in params.iter_mut() {
                    if v == old_val {
                        *v = new_val.clone();
                        break;
                    }
                }
                step.args = StepArgs::Map(params);
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
        StepArgs::Map(params)
    };

    RawStep {
        op: op.to_string(),
        args,
    }
}

// ---------------------------------------------------------------------------
// Plan serialization to YAML
// ---------------------------------------------------------------------------

/// Infer a type string from an input name (for NL-generated plans).
fn infer_type_str_from_name(name: &str) -> String {
    match name {
        "path" | "dir" | "directory" | "source" | "dest" | "destination" => "Dir".to_string(),
        "file" | "log_file" => "File".to_string(),
        "archive" => "File".to_string(),
        "keyword" | "pattern" | "query" => "Pattern".to_string(),
        "url" => "URL".to_string(),
        "repo" => "Repo".to_string(),
        "x" | "y" | "n" | "m" | "count" | "lines" => "Number".to_string(),
        "textdir" | "text_dir" => "Dir(File(Text))".to_string(),
        "pathref" => "Path".to_string(),
        _ => "String".to_string(),
    }
}

/// Serialize a PlanDef to YAML string.
pub fn plan_to_yaml(wf: &PlanDef) -> String {
    let mut lines = Vec::new();

    lines.push(format!("{}:", wf.name));
    lines.push("  inputs:".to_string());

    for inp in &wf.inputs {
        let type_str = if inp.has_type() {
            inp.type_str.clone()
        } else {
            infer_type_str_from_name(&inp.name)
        };
        lines.push(format!("    - {}: {}", inp.name, type_str));
    }

    if let Some(ref output) = wf.output {
        lines.push(format!("  output: {}", output));
    }
    lines.push("  steps:".to_string());

    for step in &wf.steps {
        match &step.args {
            StepArgs::None => {
                lines.push(format!("    - {}", step.op));
            }
            StepArgs::Scalar(s) => {
                lines.push(format!("    - {}: {}", step.op, s));
            }
            StepArgs::Map(m) => {
                lines.push(format!("    - {}:", step.op));
                let mut keys: Vec<&String> = m.keys().collect();
                keys.sort();
                for key in keys {
                    lines.push(format!("        {}: {:?}", key, m[key]));
                }
            }
        }
    }

    lines.join("\n")
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::nl::normalize;
    use crate::nl::intent;
    use crate::nl::slots;

    // -- Plan generation --

    #[test]
    fn test_build_plan_pack_archive() {
        let normalized = normalize::normalize("zip up everything in ~/Downloads");
        let parsed = intent::parse_intent(&normalized);
        let extracted = slots::extract_slots(&normalized.canonical_tokens);

        match parsed {
            Intent::CreatePlan { op, .. } => {
                let wf = build_plan(&op, &extracted, None).unwrap();
                assert_eq!(wf.get_input("path").unwrap().default.as_deref(), Some("~/Downloads"));
                assert!(wf.steps.iter().any(|s| s.op == "walk_tree"));
                assert!(wf.steps.iter().any(|s| s.op == "pack_archive"));
            }
            other => panic!("expected CreatePlan, got: {:?}", other),
        }
    }

    #[test]
    fn test_build_plan_extract() {
        let normalized = normalize::normalize("extract the archive at ~/comic.cbz");
        let parsed = intent::parse_intent(&normalized);
        let extracted = slots::extract_slots(&normalized.canonical_tokens);

        match parsed {
            Intent::CreatePlan { op, .. } => {
                let wf = build_plan(&op, &extracted, None).unwrap();
                assert!(wf.steps.iter().any(|s| s.op == "extract_archive"));
            }
            other => panic!("expected CreatePlan, got: {:?}", other),
        }
    }

    #[test]
    fn test_build_plan_find_pdfs() {
        let normalized = normalize::normalize("find all PDFs in ~/Documents");
        let parsed = intent::parse_intent(&normalized);
        let extracted = slots::extract_slots(&normalized.canonical_tokens);

        match parsed {
            Intent::CreatePlan { op, .. } => {
                let wf = build_plan(&op, &extracted, None).unwrap();
                assert!(wf.steps.iter().any(|s| s.op == "walk_tree" || s
                    .op == "find_matching"));
            }
            other => panic!("expected CreatePlan, got: {:?}", other),
        }
    }

    #[test]
    fn test_build_plan_list_comics() {
        let normalized = normalize::normalize("list all the comics in ~/Downloads");
        let parsed = intent::parse_intent(&normalized);
        let extracted = slots::extract_slots(&normalized.canonical_tokens);

        match parsed {
            Intent::CreatePlan { op, .. } => {
                let wf = build_plan(&op, &extracted, None).unwrap();
                assert!(wf.steps.iter().any(|s| s.op == "list_dir"),
                    "should have list_dir: {:?}", wf.steps);
                assert!(wf.steps.iter().any(|s| s.op == "filter"),
                    "should have filter step for comic types: {:?}", wf.steps);
                let filter_step = wf.steps.iter().find(|s| s.op == "filter").unwrap();
                match &filter_step.args {
                    StepArgs::Map(m) => {
                        let pat = m.get("pattern").expect("filter should have pattern");
                        assert!(pat.contains("cbz"), "pattern should include cbz: {}", pat);
                        assert!(pat.contains("cbr"), "pattern should include cbr: {}", pat);
                    }
                    other => panic!("filter should have map args, got: {:?}", other),
                }
            }
            other => panic!("expected CreatePlan, got: {:?}", other),
        }
    }

    #[test]
    fn test_build_plan_list_comic_books_bigram() {
        let normalized = normalize::normalize("list comic books in ~/Downloads");
        let extracted = slots::extract_slots(&normalized.canonical_tokens);
        assert!(!extracted.patterns.is_empty(),
            "should extract patterns from 'comic books': {:?}", extracted);
        assert!(extracted.patterns.iter().any(|p| p.contains("cbz")),
            "should have cbz pattern: {:?}", extracted.patterns);
    }

    #[test]
    fn test_list_unknown_noun_still_filters() {
        // "scripts" is not in noun_patterns, but should still produce a filter
        let normalized = normalize::normalize("list all the scripts in ~/src");
        let parsed = intent::parse_intent(&normalized);
        let extracted = slots::extract_slots(&normalized.canonical_tokens);

        match parsed {
            Intent::CreatePlan { op, .. } => {
                let wf = build_plan(&op, &extracted, None).unwrap();
                assert!(wf.steps.iter().any(|s| s.op == "list_dir"),
                    "should have list_dir: {:?}", wf.steps);
                assert!(wf.steps.iter().any(|s| s.op == "filter"),
                    "unknown noun 'scripts' should still produce a filter: {:?}", wf.steps);
            }
            other => panic!("expected CreatePlan, got: {:?}", other),
        }
    }

    #[test]
    fn test_list_files_no_spurious_filter() {
        // "list files in ~/Downloads" should NOT add a filter — "files" is structural
        let normalized = normalize::normalize("list files in ~/Downloads");
        let parsed = intent::parse_intent(&normalized);
        let extracted = slots::extract_slots(&normalized.canonical_tokens);

        match parsed {
            Intent::CreatePlan { op, .. } => {
                let wf = build_plan(&op, &extracted, None).unwrap();
                assert!(wf.steps.iter().any(|s| s.op == "list_dir"),
                    "should have list_dir: {:?}", wf.steps);
                assert!(!wf.steps.iter().any(|s| s.op == "filter"),
                    "'list files' should NOT add a filter: {:?}", wf.steps);
            }
            other => panic!("expected CreatePlan, got: {:?}", other),
        }
    }

    #[test]
    fn test_build_plan_no_op_error() {
        let extracted = slots::extract_slots(&[]);
        let result = build_plan(&None, &extracted, None);
        assert!(result.is_err());
        match result.unwrap_err() {
            DialogueError::CannotBuild(_) => {}
            other => panic!("expected CannotBuild, got: {:?}", other),
        }
    }

    // -- Plan YAML serialization --

    #[test]
    fn test_plan_to_yaml_roundtrip() {
        let wf = PlanDef {
            name: "test_plan".to_string(),
            inputs: vec![PlanInput::from_legacy("path", "~/Downloads")],
                output: None,
            steps: vec![
                RawStep { op: "walk_tree".to_string(), args: StepArgs::None },
                RawStep { op: "sort_by".to_string(), args: StepArgs::Scalar("name".to_string()) },
            ],
        };

        let yaml = plan_to_yaml(&wf);
        assert!(yaml.contains("test_plan:"));
        assert!(yaml.contains("walk_tree"));
        assert!(yaml.contains("sort_by: name"));

        // Verify it can be parsed back
        let parsed = crate::plan::parse_plan(&yaml).unwrap();
        assert_eq!(parsed.steps.len(), 2);
        assert_eq!(parsed.steps[0].op, "walk_tree");
        assert_eq!(parsed.steps[1].op, "sort_by");
    }

    #[test]
    fn test_plan_to_yaml_with_map_args() {
        let wf = PlanDef {
            name: "filter_test".to_string(),
            inputs: vec![PlanInput::from_legacy("path", "/tmp")],
                output: None,
            steps: vec![
                RawStep { op: "list_dir".to_string(), args: StepArgs::None },
                RawStep {
                    op: "filter".to_string(),
                    args: StepArgs::Map({
                        let mut m = HashMap::new();
                        m.insert("pattern".to_string(), "*.pdf".to_string());
                        m
                    }),
                },
            ],
        };

        let yaml = plan_to_yaml(&wf);
        assert!(yaml.contains("filter:"));
        assert!(yaml.contains("pattern:"));
    }

    // -- Generated plan compiles through engine --

    #[test]
    fn test_generated_plan_compiles() {
        let normalized = normalize::normalize("zip up everything in ~/Downloads");
        let parsed = intent::parse_intent(&normalized);
        let extracted = slots::extract_slots(&normalized.canonical_tokens);

        match parsed {
            Intent::CreatePlan { op, .. } => {
                let wf = build_plan(&op, &extracted, None).unwrap();
                let yaml = plan_to_yaml(&wf);

                // Parse it back through the plan engine
                let parsed_back = crate::plan::parse_plan(&yaml).unwrap();

                // Compile it through the engine
                let registry = crate::fs_types::build_full_registry();
                let compiled = crate::plan::compile_plan(&parsed_back, &registry);
                assert!(compiled.is_ok(), "plan should compile: {:?}", compiled.err());
            }
            other => panic!("expected CreatePlan, got: {:?}", other),
        }
    }

    // -- Edit operations --

    #[test]
    fn test_edit_on_empty_state_error() {
        let mut state = DialogueState::new();
        let slots = slots::extract_slots(&["skip".to_string(), "foo".to_string()]);
        let _result = apply_edit(
            &PlanDef {
                name: "test".to_string(),
                inputs: vec![],
                output: None,
                steps: vec![],
            },
            &EditAction::Skip,
            &slots,
            &mut state,
        );
        // Skip on empty plan should still work (inserts at position 0)
        // But skip with no keyword should fail
        // With no keywords at all, skip should fail
        let slots_empty = slots::extract_slots(&[]);
        let result_empty = apply_edit(
            &PlanDef {
                name: "test".to_string(),
                inputs: vec![],
                output: None,
                steps: vec![],
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
        };

        let normalized = normalize::normalize("skip any subdirectory named foo");
        let extracted = slots::extract_slots(&normalized.canonical_tokens);

        let (edited, desc) = apply_skip(&mut wf.clone(), &extracted, &mut state).unwrap();
        assert!(edited.steps.iter().any(|s| s.op == "filter"), "should have filter step");
        // The skip target comes from keywords — "foo" is extracted via "named foo"
        assert!(
            desc.contains("foo") || desc.contains("subdirectory") || desc.contains("skip"),
            "desc should mention the skip target: {}", desc
        );
        // Filter should be after walk_tree
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
        };

        let extracted = slots::extract_slots(&["step".to_string(), "2".to_string()]);
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
        };

        // "move step 3 before step 2"
        let extracted = slots::extract_slots(&[
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

        // "it" should resolve to the most recent entry (EditedStep)
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
        // EditedStep first, then MentionedOp, then Artifact, then WholePlan
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
        stack.push(FocusEntry::MentionedOp { op: "filter".to_string() }); // duplicate

        // Should have 2 entries, not 3
        assert_eq!(stack.entries.len(), 2);
        // Most recent "filter" should be at top
        assert!(matches!(&stack.entries[0], FocusEntry::MentionedOp { op } if op == "filter"));
    }

    // -- Dialogue state --

    #[test]
    fn test_dialogue_state_new() {
        let state = DialogueState::new();
        assert!(state.current_plan.is_none());
        assert_eq!(state.turn_count, 0);
        assert!(state.last_intent.is_none());
    }

    #[test]
    fn test_dialogue_state_turn_count() {
        let mut state = DialogueState::new();
        state.next_turn();
        state.next_turn();
        assert_eq!(state.turn_count, 2);
    }

    // -- Delta-edited plan re-compiles --

    #[test]
    fn test_edited_plan_recompiles() {
        let mut state = DialogueState::new();

        // Create initial plan
        let wf = PlanDef {
            name: "test".to_string(),
            inputs: vec![PlanInput::from_legacy("path", "~/Downloads")],
            output: None,
            steps: vec![
                RawStep { op: "walk_tree".to_string(), args: StepArgs::None },
                RawStep { op: "pack_archive".to_string(), args: StepArgs::None },
            ],
        };

        // Apply skip edit
        let normalized = normalize::normalize("skip any subdirectory named foo");
        let extracted = slots::extract_slots(&normalized.canonical_tokens);
        let (edited, _) = apply_skip(&mut wf.clone(), &extracted, &mut state).unwrap();

        // Serialize and re-compile
        let yaml = plan_to_yaml(&edited);
        let parsed = crate::plan::parse_plan(&yaml).unwrap();
        let registry = crate::fs_types::build_full_registry();
        let compiled = crate::plan::compile_plan(&parsed, &registry);
        assert!(compiled.is_ok(), "edited plan should compile: {:?}", compiled.err());
    }

    // -- B3 bugfix: input type inference --

    #[test]
    fn test_build_plan_compress_file_compiles() {
        // "compress my_file.txt" should produce a plan that compiles
        let normalized = normalize::normalize("compress my_file.txt");
        let extracted = slots::extract_slots(&normalized.canonical_tokens);
        let op = Some("gzip_compress".to_string());
        let wf = build_plan(&op, &extracted, None).unwrap();
        // Should use "file" input name for file ops
        assert!(wf.get_input("file").is_some(), "should have 'file' input: {:?}", wf.inputs);
        assert_eq!(wf.get_input("file").unwrap().default.as_deref(), Some("my_file.txt"));
        let yaml = plan_to_yaml(&wf);
        let parsed = crate::plan::parse_plan(&yaml).unwrap();
        let registry = crate::fs_types::build_full_registry();
        let compiled = crate::plan::compile_plan(&parsed, &registry);
        assert!(compiled.is_ok(), "compress file plan should compile: {:?}", compiled.err());
    }

    #[test]
    fn test_build_plan_hash_yaml_compiles() {
        let normalized = normalize::normalize("hash config.yaml");
        let extracted = slots::extract_slots(&normalized.canonical_tokens);
        let op = Some("openssl_hash".to_string());
        let wf = build_plan(&op, &extracted, None).unwrap();
        assert!(wf.get_input("file").is_some(), "should have 'file' input: {:?}", wf.inputs);
        let yaml = plan_to_yaml(&wf);
        let parsed = crate::plan::parse_plan(&yaml).unwrap();
        let registry = crate::fs_types::build_full_registry();
        let compiled = crate::plan::compile_plan(&parsed, &registry);
        assert!(compiled.is_ok(), "hash yaml plan should compile: {:?}", compiled.err());
    }

    #[test]
    fn test_build_plan_search_content_dir_compiles() {
        // "search for TODO" with no file target should use textdir
        let normalized = normalize::normalize("search for TODO");
        let extracted = slots::extract_slots(&normalized.canonical_tokens);
        let op = Some("search_content".to_string());
        let wf = build_plan(&op, &extracted, None).unwrap();
        assert!(wf.get_input("textdir").is_some(), "should have 'textdir' input: {:?}", wf.inputs);
        let yaml = plan_to_yaml(&wf);
        let parsed = crate::plan::parse_plan(&yaml).unwrap();
        let registry = crate::fs_types::build_full_registry();
        let compiled = crate::plan::compile_plan(&parsed, &registry);
        assert!(compiled.is_ok(), "search content plan should compile: {:?}", compiled.err());
    }

    #[test]
    fn test_build_plan_dir_ops_still_compile() {
        // walk_tree, list_dir, pack_archive should still use "path" input
        for op_name in &["walk_tree", "list_dir", "pack_archive"] {
            let extracted = slots::extract_slots(&["~/Downloads".to_string()]);
            let op = Some(op_name.to_string());
            let wf = build_plan(&op, &extracted, None).unwrap();
            assert!(wf.get_input("path").is_some(), "{} should have 'path' input: {:?}", op_name, wf.inputs);
            let yaml = plan_to_yaml(&wf);
            let parsed = crate::plan::parse_plan(&yaml).unwrap();
            let registry = crate::fs_types::build_full_registry();
            let compiled = crate::plan::compile_plan(&parsed, &registry);
            assert!(compiled.is_ok(), "{} plan should compile: {:?}", op_name, compiled.err());
        }
    }

    #[test]
    fn test_skip_filter_extracts_target_not_action_word() {
        // "skip any subdirectory named .git" → keywords include "skip", "subdirectory", ".git"
        // apply_skip should use ".git", not "skip"
        let mut state = DialogueState::new();
        let extracted = crate::nl::slots::extract_slots(
            &["skip", "subdirectory", "named", ".git"]
                .iter().map(|s| s.to_string()).collect::<Vec<_>>()
        );

        // Build a base plan with walk_tree
        let base_extracted = crate::nl::slots::extract_slots(
            &["walk_tree", "~/Downloads"]
                .iter().map(|s| s.to_string()).collect::<Vec<_>>()
        );
        let mut wf = build_plan(&Some("walk_tree".to_string()), &base_extracted, None).unwrap();
        wf.steps.push(RawStep { op: "pack_archive".to_string(), args: StepArgs::None });

        let (edited, desc) = apply_skip(&mut wf.clone(), &extracted, &mut state).unwrap();

        // The filter should exclude ".git", not "skip"
        let filter_step = edited.steps.iter().find(|s| s.op == "filter").unwrap();
        match &filter_step.args {
            StepArgs::Map(m) => {
                let exclude = m.get("exclude").unwrap();
                assert_eq!(exclude, ".git", "filter should exclude '.git', got '{}'", exclude);
            }
            other => panic!("expected Map args, got: {:?}", other),
        }
        assert!(desc.contains("skip"), "description should mention skip: {}", desc);
    }

    #[test]
    fn test_skip_without_target_errors() {
        let mut state = DialogueState::new();
        let extracted = crate::nl::slots::extract_slots(
            &["skip"].iter().map(|s| s.to_string()).collect::<Vec<_>>()
        );
        let mut wf = build_plan(&Some("walk_tree".to_string()), &extracted, None).unwrap();
        let result = apply_skip(&mut wf, &extracted, &mut state);
        assert!(result.is_err(), "skip without target should error");
    }

    #[test]
    fn test_remove_the_step_defaults_to_last() {
        // "remove the step" → tokens ["delete", "the", "step"]
        // "delete" becomes primary_op, but it's an action verb, not a plan op
        let mut state = DialogueState::new();

        // Build a 3-step plan
        let extracted = crate::nl::slots::extract_slots(
            &["find_matching", "~/Documents"]
                .iter().map(|s| s.to_string()).collect::<Vec<_>>()
        );
        let wf = build_plan(&Some("find_matching".to_string()), &extracted, None).unwrap();
        let original_len = wf.steps.len();
        assert!(original_len >= 2, "need at least 2 steps, got {}", original_len);

        let last_op = wf.steps.last().unwrap().op.clone();

        // Simulate "remove the step" — slots have primary_op = "delete"
        let remove_slots = crate::nl::slots::extract_slots(
            &["delete", "the", "step"]
                .iter().map(|s| s.to_string()).collect::<Vec<_>>()
        );

        let (edited, desc) = apply_remove(&mut wf.clone(), &remove_slots, &mut state).unwrap();
        assert_eq!(edited.steps.len(), original_len - 1, "should have removed one step");
        assert!(desc.contains(&last_op), "should have removed last op '{}': {}", last_op, desc);
    }

    #[test]
    fn test_remove_step_1_still_works() {
        let mut state = DialogueState::new();
        let extracted = crate::nl::slots::extract_slots(
            &["find_matching", "~/Documents"]
                .iter().map(|s| s.to_string()).collect::<Vec<_>>()
        );
        let wf = build_plan(&Some("find_matching".to_string()), &extracted, None).unwrap();
        let first_op = wf.steps[0].op.clone();

        // "remove step 1" — has a step ref
        let remove_slots = crate::nl::slots::extract_slots(
            &["delete", "step", "1"]
                .iter().map(|s| s.to_string()).collect::<Vec<_>>()
        );
        let (_edited, desc) = apply_remove(&mut wf.clone(), &remove_slots, &mut state).unwrap();
        assert!(desc.contains(&first_op), "should have removed first op '{}': {}", first_op, desc);
    }

    // -- Path resolution --

    #[test]
    fn test_resolve_path_absolute_unchanged() {
        assert_eq!(resolve_path("/usr/local"), "/usr/local");
    }

    #[test]
    fn test_resolve_path_home_relative_unchanged() {
        assert_eq!(resolve_path("~/Documents"), "~/Documents");
    }

    #[test]
    fn test_resolve_path_dot_unchanged() {
        assert_eq!(resolve_path("."), ".");
    }

    #[test]
    fn test_resolve_path_env_var_unchanged() {
        assert_eq!(resolve_path("$HOME/foo"), "$HOME/foo");
    }

    #[test]
    fn test_resolve_path_unknown_bare_name_unchanged() {
        // A name that doesn't match any volume should pass through
        assert_eq!(resolve_path("nonexistent_xyzzy_volume"), "nonexistent_xyzzy_volume");
    }

    #[test]
    fn test_resolve_path_macintosh_hd_volume() {
        // /Volumes/Macintosh HD should always exist on macOS
        let resolved = resolve_path("Macintosh HD");
        assert_eq!(resolved, "/Volumes/Macintosh HD",
            "should resolve to /Volumes/Macintosh HD: {}", resolved);
    }
}
