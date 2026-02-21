//! Slot extraction and fuzzy name matching for the NL UX layer.
//!
//! Extracts structured slots from parsed intent tokens:
//! - **Paths** — ~/Downloads, /tmp, etc.
//! - **StepRef** — step 2, previous, next, last, first
//! - **OpName** — canonical operation names (with fuzzy matching)
//! - **Pattern** — *.pdf, foo*, etc.
//! - **Param** — key=value pairs or named parameters
//! - **Modifier** — recursive, case-insensitive, reverse, etc.
//! - **Keyword** — named tokens that aren't any of the above

use crate::nl::normalize::is_canonical_op;

// ---------------------------------------------------------------------------
// Slot types
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
// Public API
// ---------------------------------------------------------------------------

/// Extract structured slots from a sequence of canonical tokens.
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
        if is_canonical_op(token) {
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

            // "my <alias>" — possessive + directory name
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

            // Bare alias: "desktop" → ~/Desktop
            if let Some(alias_path) = dir_aliases.get(token.as_str()) {
                // If we already have a target path and this token is also a noun
                // pattern, prefer the noun-pattern interpretation.
                // e.g. "find music in ~/Downloads" → music = *.mp3, not ~/Music
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
            // Try bigram first: "comic books" etc.
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
            // Single token
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
// Detection helpers
// ---------------------------------------------------------------------------

/// Check if a token looks like a filesystem path.
fn is_path(token: &str) -> bool {
    // Quoted literal: tokens with spaces can only come from user-quoted strings
    // like "NO NAME" or 'my folder' — treat as a path/name literal.
    if token.contains(' ') {
        return true;
    }

    // Absolute or home-relative paths
    if token.starts_with("~/")
        || token.starts_with('/')
        || token.starts_with("$HOME")
        || token.starts_with("$PWD")
        || token.contains("://") {
        return true;
    }

    // Relative path with both / and . (e.g. src/foo.rs)
    if token.contains('/') && token.contains('.') {
        return true;
    }

    // Directory-like: ends with / (e.g. "src/", "docs/")
    if token.ends_with('/') && token.len() > 1 {
        return true;
    }

    // URL-like: contains dots and slashes (e.g. github.com/user/project)
    if token.contains('/') && token.contains('.') {
        return true;
    }

    // Bare filename with a known file extension
    // Must have a dot, and the part after the last dot must be a known extension
    if let Some(dot_pos) = token.rfind('.') {
        let ext = &token[dot_pos + 1..];
        let name_part = &token[..dot_pos];
        // Must have a non-empty name before the dot, a recognized extension,
        // and NOT be a glob pattern (starts with *)
        if !name_part.is_empty() && !ext.is_empty() && !name_part.contains('*') && is_file_extension(ext) {
            return true;
        }
    }

    // Bare directory path with / (e.g. "src/lib")
    if token.contains('/') && token.len() > 2 {
        return true;
    }

    false
}

/// Check if a string looks like a known file extension.
fn is_file_extension(ext: &str) -> bool {
    crate::filetypes::dictionary().is_known_extension(ext)
}

/// Check if a token looks like a glob/pattern.
fn is_pattern(token: &str) -> bool {
    (token.starts_with("*.") || token.starts_with("*") || token.ends_with("*"))
        && !is_canonical_op(token)
}

/// Parse a step reference from a token.
fn parse_step_ref(token: &str) -> Option<StepRef> {
    if let Ok(n) = token.parse::<u32>() {
        if n > 0 && n <= 100 {
            return Some(StepRef::Number(n));
        }
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
        "case-insensitive" | "caseinsensitive" | "nocase" | "ignorecase" => {
            Some(Modifier::CaseInsensitive)
        }
        "reverse" | "reversed" | "descending" | "desc" => Some(Modifier::Reverse),
        "verbose" | "detailed" => Some(Modifier::Verbose),
        "dry-run" | "dryrun" | "simulate" | "preview" => Some(Modifier::DryRun),
        "force" | "forced" => Some(Modifier::Force),
        "quiet" | "silent" | "suppress" => Some(Modifier::Quiet),
        "each" => Some(Modifier::Each),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Fuzzy op name matching
// ---------------------------------------------------------------------------

/// Try to fuzzy-match a token against known canonical op names.
/// Returns Some(canonical_name) if edit distance <= 2, None otherwise.
pub fn fuzzy_match_op(token: &str) -> Option<String> {
    // Don't try to match very short tokens (too ambiguous)
    if token.len() < 4 {
        return None;
    }

    // Don't match tokens that are common English words
    let common_words = [
        "step", "file", "name", "type", "size", "time", "date",
        "path", "text", "data", "line", "word", "char", "byte",
        "skip", "stop", "help", "show", "make", "take", "give",
        "keep", "save", "load", "send", "call", "mean", "work",
        "like", "want", "need", "have", "been", "done", "here",
        "there", "what", "when", "where", "which", "that", "this",
        "from", "into", "with", "about", "after", "before",
    ];
    if common_words.contains(&token) {
        return None;
    }

    let ops = crate::nl::normalize::canonical_ops();
    let mut best: Option<(String, usize)> = None;

    for op in ops.iter() {
        let dist = edit_distance_bounded(token, op.as_str(), 2);
        if let Some(d) = dist {
            match &best {
                Some((_, best_d)) if d < *best_d => {
                    best = Some((op.to_string(), d));
                }
                None => {
                    best = Some((op.to_string(), d));
                }
                _ => {}
            }
        }
    }

    best.map(|(op, _)| op)
}

/// Compute edit distance with early termination if > max_distance.
/// Returns None if distance exceeds max_distance.
fn edit_distance_bounded(a: &str, b: &str, max_distance: usize) -> Option<usize> {
    let a_chars: Vec<char> = a.chars().collect();
    let b_chars: Vec<char> = b.chars().collect();
    let a_len = a_chars.len();
    let b_len = b_chars.len();

    // Quick length check
    if a_len.abs_diff(b_len) > max_distance {
        return None;
    }

    let mut prev = vec![0usize; b_len + 1];
    let mut curr = vec![0usize; b_len + 1];

    for j in 0..=b_len {
        prev[j] = j;
    }

    for i in 1..=a_len {
        curr[0] = i;
        let mut min_in_row = curr[0];

        for j in 1..=b_len {
            let cost = if a_chars[i - 1] == b_chars[j - 1] { 0 } else { 1 };
            curr[j] = (prev[j] + 1)
                .min(curr[j - 1] + 1)
                .min(prev[j - 1] + cost);
            min_in_row = min_in_row.min(curr[j]);
        }

        // Early termination
        if min_in_row > max_distance {
            return None;
        }

        std::mem::swap(&mut prev, &mut curr);
    }

    let result = prev[b_len];
    if result <= max_distance {
        Some(result)
    } else {
        None
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // -- Path detection --

    #[test]
    fn test_extract_path_home() {
        let slots = extract_slots(&tokens(&["pack_archive", "~/Downloads"]));
        assert_eq!(slots.target_path, Some("~/Downloads".to_string()));
    }

    #[test]
    fn test_extract_path_absolute() {
        let slots = extract_slots(&tokens(&["list_dir", "/tmp/foo"]));
        assert_eq!(slots.target_path, Some("/tmp/foo".to_string()));
    }

    #[test]
    fn test_extract_op_and_path() {
        let slots = extract_slots(&tokens(&["pack_archive", "everything", "in", "~/Downloads"]));
        assert_eq!(slots.primary_op, Some("pack_archive".to_string()));
        assert_eq!(slots.target_path, Some("~/Downloads".to_string()));
    }

    // -- Step references --

    #[test]
    fn test_extract_step_number() {
        let slots = extract_slots(&tokens(&["move_entry", "step", "2", "before", "step", "1"]));
        assert_eq!(slots.step_refs[0], StepRef::Number(2));
        assert_eq!(slots.anchor, Some(Anchor::Before(StepRef::Number(1))));
    }

    #[test]
    fn test_extract_step_previous() {
        let slots = extract_slots(&tokens(&["move_entry", "step", "2", "before", "previous"]));
        assert_eq!(slots.step_refs[0], StepRef::Number(2));
        assert_eq!(slots.anchor, Some(Anchor::Before(StepRef::Previous)));
    }

    #[test]
    fn test_extract_step_last() {
        let slots = extract_slots(&tokens(&["add", "filter", "after", "last"]));
        assert_eq!(slots.anchor, Some(Anchor::After(StepRef::Last)));
    }

    // -- Patterns --

    #[test]
    fn test_extract_glob_pattern() {
        let slots = extract_slots(&tokens(&["find_matching", "*.pdf", "in", "~/Documents"]));
        assert!(slots.patterns.contains(&"*.pdf".to_string()));
    }

    #[test]
    fn test_extract_wildcard_pattern() {
        let slots = extract_slots(&tokens(&["filter", "foo*"]));
        assert!(slots.patterns.contains(&"foo*".to_string()));
    }

    // -- Modifiers --

    #[test]
    fn test_extract_modifier_recursive() {
        let slots = extract_slots(&tokens(&["find_matching", "recursively", "*.pdf"]));
        assert!(slots.modifiers.contains(&Modifier::Recursive));
    }

    #[test]
    fn test_extract_modifier_reverse() {
        let slots = extract_slots(&tokens(&["sort_by", "reverse"]));
        assert!(slots.modifiers.contains(&Modifier::Reverse));
    }

    // -- Keywords --

    #[test]
    fn test_extract_named_keyword() {
        let slots = extract_slots(&tokens(&["skip", "subdirectory", "named", "foo"]));
        assert!(slots.keywords.contains(&"foo".to_string()));
    }

    #[test]
    fn test_extract_called_keyword() {
        let slots = extract_slots(&tokens(&["delete", "files", "called", "temp"]));
        assert!(slots.keywords.contains(&"temp".to_string()));
    }

    // -- Fuzzy matching --

    #[test]
    fn test_fuzzy_match_walk_tre() {
        let result = fuzzy_match_op("walk_tre");
        assert_eq!(result, Some("walk_tree".to_string()));
    }

    #[test]
    fn test_fuzzy_match_pack_archiv() {
        let result = fuzzy_match_op("pack_archiv");
        assert_eq!(result, Some("pack_archive".to_string()));
    }

    #[test]
    fn test_fuzzy_match_exact() {
        // Exact match should also work
        let result = fuzzy_match_op("walk_tree");
        assert_eq!(result, Some("walk_tree".to_string()));
    }

    #[test]
    fn test_fuzzy_match_no_match() {
        let result = fuzzy_match_op("xyzzyplugh");
        assert_eq!(result, None);
    }

    #[test]
    fn test_fuzzy_match_short_word_skipped() {
        // Short words shouldn't fuzzy match (too ambiguous)
        let result = fuzzy_match_op("cat");
        assert_eq!(result, None);
    }

    // -- Empty input --

    #[test]
    fn test_extract_empty() {
        let slots = extract_slots(&[]);
        assert!(slots.slots.is_empty());
        assert!(slots.target_path.is_none());
        assert!(slots.primary_op.is_none());
    }

    // -- No recognizable slots --

    #[test]
    fn test_extract_stopwords_only() {
        let slots = extract_slots(&tokens(&["the", "a", "in", "on"]));
        assert!(slots.slots.is_empty());
    }

    // -- Complex extraction --

    #[test]
    fn test_extract_complex() {
        let slots = extract_slots(&tokens(&[
            "find_matching", "recursively", "*.pdf", "in", "~/Documents",
        ]));
        assert_eq!(slots.primary_op, Some("find_matching".to_string()));
        assert_eq!(slots.target_path, Some("~/Documents".to_string()));
        assert!(slots.patterns.contains(&"*.pdf".to_string()));
        assert!(slots.modifiers.contains(&Modifier::Recursive));
    }

    #[test]
    fn test_extract_move_step_2_before_previous() {
        let slots = extract_slots(&tokens(&[
            "move_entry", "step", "2", "before", "previous",
        ]));
        assert_eq!(slots.step_refs[0], StepRef::Number(2));
        assert_eq!(slots.anchor, Some(Anchor::Before(StepRef::Previous)));
    }

    // -- Edit distance --

    #[test]
    fn test_edit_distance_bounded_exact() {
        assert_eq!(edit_distance_bounded("hello", "hello", 2), Some(0));
    }

    #[test]
    fn test_edit_distance_bounded_one() {
        assert_eq!(edit_distance_bounded("hello", "helo", 2), Some(1));
    }

    #[test]
    fn test_edit_distance_bounded_exceeds() {
        assert_eq!(edit_distance_bounded("hello", "xyz", 2), None);
    }

    // Helper
    fn tokens(words: &[&str]) -> Vec<String> {
        words.iter().map(|w| w.to_string()).collect()
    }

    // -- B2 bugfix: expanded path detection --

    #[test]
    fn test_bare_filename_txt() {
        assert!(is_path("my_file.txt"));
    }

    #[test]
    fn test_bare_filename_yaml() {
        assert!(is_path("config.yaml"));
    }

    #[test]
    fn test_bare_filename_tar() {
        assert!(is_path("my_project.tar"));
    }

    #[test]
    fn test_bare_filename_json() {
        assert!(is_path("old_config.json"));
    }

    #[test]
    fn test_trailing_slash_dir() {
        assert!(is_path("src/"));
    }

    #[test]
    fn test_url_like_path() {
        assert!(is_path("github.com/user/project"));
    }

    #[test]
    fn test_plain_word_not_path() {
        assert!(!is_path("hello"));
        assert!(!is_path("compress"));
        assert!(!is_path("documents"));
    }

    #[test]
    fn test_abbreviation_not_path() {
        // Common abbreviations with dots should NOT be paths
        // "e.g" and "i.e" don't have known file extensions
        assert!(!is_path("e.g"));
        assert!(!is_path("i.e"));
    }

    #[test]
    fn test_extract_bare_filename_as_path() {
        let slots = extract_slots(&tokens(&["gzip_compress", "my_file.txt"]));
        assert_eq!(slots.target_path, Some("my_file.txt".to_string()));
    }

    // -- Hardening: conversational fillers as stopwords (I2) --

    #[test]
    fn test_ok_not_keyword() {
        let slots = extract_slots(&tokens(&["search_content", "ok", "todo", "~/src"]));
        assert!(!slots.keywords.contains(&"ok".to_string()), "keywords: {:?}", slots.keywords);
        assert!(slots.keywords.contains(&"todo".to_string()), "keywords: {:?}", slots.keywords);
    }

    #[test]
    fn test_so_not_keyword() {
        let slots = extract_slots(&tokens(&["find_matching", "so", "*.pdf", "~/Documents"]));
        assert!(!slots.keywords.contains(&"so".to_string()), "keywords: {:?}", slots.keywords);
    }

    #[test]
    fn test_yeah_not_keyword() {
        let slots = extract_slots(&tokens(&["search_content", "yeah", "error", "~/logs"]));
        assert!(!slots.keywords.contains(&"yeah".to_string()), "keywords: {:?}", slots.keywords);
        assert!(slots.keywords.contains(&"error".to_string()), "keywords: {:?}", slots.keywords);
    }

    // -- Directory alias resolution --

    #[test]
    fn test_my_desktop_becomes_path() {
        let slots = extract_slots(&tokens(&["find_matching", "my", "desktop"]));
        assert_eq!(slots.target_path, Some("~/Desktop".to_string()),
            "target_path: {:?}", slots.target_path);
    }

    #[test]
    fn test_bare_desktop_becomes_path() {
        let slots = extract_slots(&tokens(&["find_matching", "desktop"]));
        assert_eq!(slots.target_path, Some("~/Desktop".to_string()),
            "target_path: {:?}", slots.target_path);
    }

    #[test]
    fn test_my_downloads_becomes_path() {
        let slots = extract_slots(&tokens(&["walk_tree", "my", "downloads"]));
        assert_eq!(slots.target_path, Some("~/Downloads".to_string()),
            "target_path: {:?}", slots.target_path);
    }

    #[test]
    fn test_bare_downloads_becomes_path() {
        let slots = extract_slots(&tokens(&["walk_tree", "downloads"]));
        assert_eq!(slots.target_path, Some("~/Downloads".to_string()),
            "target_path: {:?}", slots.target_path);
    }

    #[test]
    fn test_my_documents_becomes_path() {
        let slots = extract_slots(&tokens(&["list_dir", "my", "documents"]));
        assert_eq!(slots.target_path, Some("~/Documents".to_string()),
            "target_path: {:?}", slots.target_path);
    }

    #[test]
    fn test_unknown_dir_not_path() {
        let slots = extract_slots(&tokens(&["find_matching", "my", "foobar"]));
        // "my" is a stopword, "foobar" becomes a keyword — no path
        assert!(slots.target_path.is_none(),
            "target_path should be None, got: {:?}", slots.target_path);
    }

    #[test]
    fn test_my_without_dir_alias() {
        // "my" followed by a non-alias word should not create a path
        let slots = extract_slots(&tokens(&["find_matching", "my", "stuff"]));
        assert!(slots.target_path.is_none(),
            "target_path should be None, got: {:?}", slots.target_path);
    }

    #[test]
    fn test_explicit_path_wins_over_alias() {
        // If an explicit path is given, it should be the target_path
        let slots = extract_slots(&tokens(&["walk_tree", "~/Projects", "desktop"]));
        assert_eq!(slots.target_path, Some("~/Projects".to_string()),
            "explicit path should win, got: {:?}", slots.target_path);
    }

    // -- Noun-to-filetype pattern mapping --

    #[test]
    fn test_screenshots_becomes_pattern() {
        let slots = extract_slots(&tokens(&["find_matching", "screenshots"]));
        assert!(slots.patterns.contains(&"*.png".to_string()),
            "patterns: {:?}", slots.patterns);
    }

    #[test]
    fn test_photos_becomes_multiple_patterns() {
        let slots = extract_slots(&tokens(&["find_matching", "photos"]));
        assert!(slots.patterns.contains(&"*.png".to_string()),
            "patterns: {:?}", slots.patterns);
        assert!(slots.patterns.contains(&"*.jpg".to_string()),
            "patterns: {:?}", slots.patterns);
    }

    #[test]
    fn test_pdfs_becomes_pattern() {
        let slots = extract_slots(&tokens(&["find_matching", "pdfs"]));
        assert!(slots.patterns.contains(&"*.pdf".to_string()),
            "patterns: {:?}", slots.patterns);
    }

    #[test]
    fn test_videos_becomes_pattern() {
        let slots = extract_slots(&tokens(&["find_matching", "videos"]));
        assert!(slots.patterns.contains(&"*.mp4".to_string()),
            "patterns: {:?}", slots.patterns);
        assert!(slots.patterns.contains(&"*.mov".to_string()),
            "patterns: {:?}", slots.patterns);
    }

    #[test]
    fn test_unknown_noun_not_pattern() {
        let slots = extract_slots(&tokens(&["find_matching", "widgets"]));
        assert!(slots.patterns.is_empty(),
            "patterns should be empty, got: {:?}", slots.patterns);
        assert!(slots.keywords.contains(&"widgets".to_string()),
            "keywords: {:?}", slots.keywords);
    }

    #[test]
    fn test_explicit_pattern_not_overridden_by_noun() {
        // Explicit glob pattern should be present alongside noun patterns
        let slots = extract_slots(&tokens(&["find_matching", "*.txt", "screenshots"]));
        assert!(slots.patterns.contains(&"*.txt".to_string()),
            "patterns: {:?}", slots.patterns);
        assert!(slots.patterns.contains(&"*.png".to_string()),
            "patterns: {:?}", slots.patterns);
    }

    #[test]
    fn test_dir_alias_and_noun_pattern_together() {
        // "find screenshots on desktop" → path ~/Desktop, pattern *.png
        let slots = extract_slots(&tokens(&["find_matching", "screenshots", "desktop"]));
        assert_eq!(slots.target_path, Some("~/Desktop".to_string()),
            "target_path: {:?}", slots.target_path);
        assert!(slots.patterns.contains(&"*.png".to_string()),
            "patterns: {:?}", slots.patterns);
    }

    #[test]
    fn test_music_as_dir_when_no_path() {
        // "find music" → path ~/Music (dir alias wins when no path set)
        let slots = extract_slots(&tokens(&["find_matching", "music"]));
        assert_eq!(slots.target_path, Some("~/Music".to_string()),
            "target_path: {:?}", slots.target_path);
    }

    #[test]
    fn test_music_as_noun_when_path_exists() {
        // When an explicit path appears BEFORE an ambiguous noun, the noun
        // becomes a pattern instead of a dir alias.
        let slots = extract_slots(&tokens(&["find_matching", "~/Downloads", "music"]));
        assert_eq!(slots.target_path, Some("~/Downloads".to_string()),
            "target_path: {:?}", slots.target_path);
        assert!(slots.patterns.contains(&"*.mp3".to_string()),
            "patterns: {:?}", slots.patterns);
    }

    // -- Quoted string as path --

    #[test]
    fn test_quoted_string_becomes_path() {
        // A token with spaces (from quoted input) should be treated as a path
        let toks = vec!["list_dir".to_string(), "NO NAME".to_string()];
        let slots = extract_slots(&toks);
        assert_eq!(slots.target_path, Some("NO NAME".to_string()),
            "quoted string should become target_path: {:?}", slots.target_path);
    }
}
