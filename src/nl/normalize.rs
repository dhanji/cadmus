//! Text normalization for the NL UX layer.
//!
//! Pipeline: raw input → case fold → strip punctuation → expand contractions
//! → canonicalize ordinals → map synonym phrases to canonical op names.
//!
//! Single-word synonyms that are common English words (e.g. "dir", "archive",
//! "order", "total") are guarded: they only fire in command position (first
//! content token, or after a conjunction like "then"/"and"/"also").
//!
//! All operations are pure string transforms — no allocation-heavy structures,
//! no external dependencies. Designed for grep-like latency.

use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::OnceLock;

// ---------------------------------------------------------------------------
// NormalizedInput — the output of normalization
// ---------------------------------------------------------------------------

/// Result of normalizing a raw user input string.
#[derive(Debug, Clone)]
pub struct NormalizedInput {
    /// The original raw input, untouched.
    pub raw: String,
    /// Tokens after case-fold + punctuation strip + contraction expansion.
    pub tokens: Vec<String>,
    /// Tokens after synonym mapping (op names canonicalized).
    pub canonical_tokens: Vec<String>,
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Normalize a raw user input string through the full pipeline.
pub fn normalize(input: &str) -> NormalizedInput {
    let raw = input.to_string();

    // 1. Case fold + strip punctuation + expand contractions + ordinals
    let tokens = tokenize(input);

    // 2. Map synonym phrases to canonical op names
    let canonical_tokens = apply_synonyms(&tokens);

    NormalizedInput {
        raw,
        tokens,
        canonical_tokens,
    }
}

// ---------------------------------------------------------------------------
// Tokenization: case fold, punctuation strip, contractions, ordinals
// ---------------------------------------------------------------------------

/// Tokenize input: lowercase, strip punctuation, expand contractions,
/// canonicalize ordinals.
fn tokenize(input: &str) -> Vec<String> {
    // Pre-pass: extract quoted strings as literal tokens (preserving case
    // and spaces). "NO NAME" or 'my folder' become single tokens.
    // Everything outside quotes goes through normal tokenization.
    let mut result = Vec::new();
    let mut segment = String::new();
    let mut chars = input.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '"' {
            // Tokenize the accumulated unquoted segment
            if !segment.is_empty() {
                result.extend(tokenize_segment(&segment));
                segment.clear();
            }
            // Collect until matching close quote
            let mut quoted = String::new();
            for qc in chars.by_ref() {
                if qc == c {
                    break;
                }
                quoted.push(qc);
            }
            // Emit the quoted content as a single literal token
            if !quoted.is_empty() {
                result.push(quoted);
            }
        } else {
            segment.push(c);
        }
    }
    // Tokenize any remaining unquoted text
    if !segment.is_empty() {
        result.extend(tokenize_segment(&segment));
    }

    result
}

/// Tokenize a segment of input that contains no quoted strings.
/// Lowercases, strips punctuation, expands contractions, canonicalizes ordinals.
fn tokenize_segment(input: &str) -> Vec<String> {
    let lower = input.to_lowercase();
    let expanded = expand_contractions(&lower);

    let orig_words: Vec<&str> = input.split_whitespace().collect();

    let mut tokens = Vec::new();
    let mut orig_idx = 0;
    for word in expanded.split_whitespace() {
        let stripped = strip_punctuation(word);
        if stripped.is_empty() {
            continue;
        }

        let is_path = stripped.starts_with("~/")
            || stripped.starts_with('/')
            || stripped.starts_with('$')
            || stripped.starts_with('*')
            || stripped.contains("://");

        if is_path {
            let orig = find_original_path(&orig_words, &mut orig_idx, &stripped);
            tokens.push(orig);
        } else if stripped.ends_with("'s") || stripped.ends_with("\u{2019}s") {
            // Strip possessive suffix: "floyd's" → "floyd", "kadane's" → "kadane"
            let base = stripped.trim_end_matches('s')
                .trim_end_matches('\'')
                .trim_end_matches('\u{2019}');
            if !base.is_empty() {
                tokens.push(base.to_string());
            }
        } else if stripped.contains('-') && !stripped.starts_with('-') {
            // Split hyphenated words into separate tokens
            // e.g., "newton-raphson" → ["newton", "raphson"]
            for part in stripped.split('-') {
                if !part.is_empty() {
                    tokens.push(part.to_string());
                }
            }
        } else if let Some(num) = canonicalize_ordinal(&stripped) {
            tokens.push(num.to_string());
        } else {
            tokens.push(stripped);
        }
    }
    tokens
}

/// Find the original-case version of a path token from the original words.
fn find_original_path(orig_words: &[&str], orig_idx: &mut usize, lowered: &str) -> String {
    while *orig_idx < orig_words.len() {
        let orig = orig_words[*orig_idx];
        let orig_stripped = strip_punctuation(&orig.to_lowercase());
        // Also try the original without lowering for path comparison
        let orig_preserved = strip_punctuation(orig);
        if orig_stripped == *lowered || orig_preserved.to_lowercase() == *lowered {
            *orig_idx += 1;
            return orig_preserved;
        }
        *orig_idx += 1;
    }
    // Fallback: return the lowered version
    lowered.to_string()
}

/// Strip leading/trailing punctuation from a token, preserving internal
/// characters that matter (hyphens in paths, dots in extensions, slashes
/// in paths, tildes in ~/paths, $ in variables, * in globs).
fn strip_punctuation(word: &str) -> String {
    // Preserve path-like tokens entirely
    if word.starts_with("~/")
        || word.starts_with('/')
        || word.starts_with("$")
        || word.starts_with("*")
        || word.contains("://")
    {
        // Just trim trailing punctuation that isn't path-relevant
        return word.trim_end_matches(|c: char| c == ',' || c == ';' || c == '!' || c == '?').to_string();
    }

    // For normal words: strip leading/trailing non-alphanumeric chars
    // but keep internal hyphens, underscores, dots
    let trimmed = word
        .trim_start_matches(|c: char| !c.is_alphanumeric() && c != '~' && c != '/' && c != '$' && c != '*' && c != '.')
        .trim_end_matches(|c: char| !c.is_alphanumeric() && c != '/' && c != '.' && c != '*');

    trimmed.to_string()
}

/// Expand common contractions.
fn expand_contractions(text: &str) -> String {
    let mut result = text.to_string();
    // Contractions loaded from YAML (sorted longest-first by vocab loader)
    for (contraction, expansion) in &super::vocab::vocab().contractions {
        result = result.replace(contraction, expansion);
    }
    result
}

/// Canonicalize ordinal words/suffixes to numeric form.
/// Returns Some(n) if the token is an ordinal, None otherwise.
fn canonicalize_ordinal(token: &str) -> Option<u32> {
    // Word ordinals loaded from YAML
    if let Some(&n) = super::vocab::vocab().ordinals.get(token) {
        return Some(n);
    }

    // Try to parse Nth pattern (e.g. "13th", "21st", "22nd", "33rd")
    let s = token.trim_end_matches("st")
        .trim_end_matches("nd")
        .trim_end_matches("rd")
        .trim_end_matches("th");
    if s != token && !s.is_empty() {
        s.parse::<u32>().ok()
    } else {
        None
    }
}

// ---------------------------------------------------------------------------
// Synonym mapping: multi-word phrases → canonical op names
// ---------------------------------------------------------------------------

/// Build the synonym table. This is a static mapping from natural language
/// phrases to canonical operation names from fs_ops + power_tools_ops.
///
/// The table maps multi-word phrases (longest match first) and single words.
/// Phrases are stored as space-separated lowercase tokens.
/// Apply synonym mapping to tokens using greedy longest-match.
///
/// Uses the synonym table from the YAML vocabulary pack.
/// Scans left-to-right. At each position, tries the longest synonym phrase
/// first. If a match is found, replaces the matched tokens with the canonical
/// op name and advances past them. Otherwise, keeps the token as-is.
fn apply_synonyms(tokens: &[String]) -> Vec<String> {
    let vocab = super::vocab::vocab();

    // ── Ambiguous single-word synonyms ──────────────────────────────────
    // These are common English words that also happen to be command names.
    // They should only fire in "command position" — first content token,
    // or after a conjunction/sequencing word ("then", "and", "also").
    // Multi-word phrases containing these words are NOT affected (the
    // multi-word context already disambiguates).
    static AMBIGUOUS: OnceLock<HashSet<&'static str>> = OnceLock::new();
    let ambiguous = AMBIGUOUS.get_or_init(|| {
        [
            "dir", "archive", "order", "total", "list", "sort", "tree",
            "walk", "find", "filter", "search", "copy", "move", "head",
            "tail", "link", "file", "type", "extract", "pack", "count",
            "diff", "hash", "sum", "add", "merge", "reveal", "replace",
            "rename", "remove", "delete", "info", "stat", "more", "less",
            "cut", "paste", "push", "pull", "fetch", "clone", "commit",
            "checkout", "narrow", "unique", "duplicate", "compress",
            "decompress", "download", "upload", "sync", "erase", "trash",
            "dedup", "deduplicate", "arrange", "sift", "recurse",
            "recursive", "traverse", "spotlight",
        ].into_iter().collect()
    });

    // Words that signal the next token is in command position.
    static SEQUENCERS: OnceLock<HashSet<&'static str>> = OnceLock::new();
    let sequencers = SEQUENCERS.get_or_init(|| {
        [
            "then", "and", "also", "next", "after", "before", "finally",
            "first", "now", "please", "instead", "that", "this",
            "to", "just", "go", "want", "need", "like", "can", "could",
            "should", "would", "will", "shall", "may", "might", "let",
            "gonna", "wanna", "gotta", "so", "ok", "okay",
        ].into_iter().collect()
    });

    // Meta-command words that make the following token command-like.
    static META_CMDS: OnceLock<HashSet<&'static str>> = OnceLock::new();
    let meta_cmds = META_CMDS.get_or_init(|| {
        [
            "explain", "what", "how", "describe", "show", "run", "do",
            "execute", "try", "use", "skip", "actually", "is", "does",
            "about", "mean", "means",
        ].into_iter().collect()
    });

    // Command position: the token is likely an op name, not prose.
    // True when:
    //   - Position 0 (first token)
    //   - Previous result token is a sequencer ("then", "and", ...)
    //   - Previous result token is a meta-command ("explain", "what", ...)
    //   - Previous result token is a canonical op (chaining: "walk filter")
    //   - Previous result token is a path ("/foo", "~/bar")
    let is_command_pos = |i: usize, result: &[String]| -> bool {
        if i == 0 { return true; }
        match result.last() {
            None => true,
            Some(prev) => {
                sequencers.contains(prev.as_str())
                    || meta_cmds.contains(prev.as_str())
                    || is_canonical_op(prev)
                    || prev.starts_with('/')
                    || prev.starts_with("~/")
                    || prev.starts_with('$')
            }
        }
    };

    // Group by first token for fast lookup
    let mut by_first: HashMap<&str, Vec<(&[String], &str)>> = HashMap::new();
    for (phrase, op) in &vocab.synonyms {
        if let Some(first) = phrase.first() {
            by_first
                .entry(first.as_str())
                .or_default()
                .push((phrase.as_slice(), op.as_str()));
        }
    }

    // Sort each group by phrase length descending (longest match first)
    for entries in by_first.values_mut() {
        entries.sort_by(|a, b| b.0.len().cmp(&a.0.len()));
    }

    let mut result = Vec::new();
    let mut i = 0;

    while i < tokens.len() {
        let token = tokens[i].as_str();

        if let Some(candidates) = by_first.get(token) {
            let mut matched = false;
            for (phrase, op) in candidates {
                // Guard: single-word ambiguous synonyms only in command position
                if phrase.len() == 1 && ambiguous.contains(token) {
                    if !is_command_pos(i, &result) {
                        continue;
                    }
                }
                let end = i + phrase.len();
                if end <= tokens.len() {
                    let matches = tokens[i..end]
                        .iter()
                        .zip(phrase.iter())
                        .all(|(t, p)| t == p);
                    if matches {
                        result.push(op.to_string());
                        i = end;
                        matched = true;
                        break;
                    }
                }
            }
            if !matched {
                result.push(tokens[i].clone());
                i += 1;
            }
        } else {
            result.push(tokens[i].clone());
            i += 1;
        }
    }

    result
}

/// Check if a token is a known canonical op name.
///
/// Derived at load time from the ops YAML packs (fs_ops, power_tools, coding,
/// comparison). Adding a new op to any YAML pack automatically makes it
/// recognizable here — no Rust code change needed.
pub fn is_canonical_op(token: &str) -> bool {
    canonical_ops().contains(token)
}

/// Cached set of all canonical op names, derived from the ops YAML packs.
static CANONICAL_OPS: OnceLock<HashSet<String>> = OnceLock::new();

/// Get the set of canonical op names (loaded once from the registry).
/// Get the set of canonical op names (loaded once from the registry).
pub fn canonical_ops() -> &'static HashSet<String> {
    CANONICAL_OPS.get_or_init(|| {
        let reg = crate::fs_types::build_full_registry();
        let mut ops: HashSet<String> = reg.poly_op_names().into_iter().map(|s| s.to_string()).collect();
        // Also include monomorphic ops
        for name in reg.op_names() {
            ops.insert(name.to_string());
        }
        ops
    })
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // -- Tokenization --

    #[test]
    fn test_basic_tokenize() {
        let tokens = tokenize("Hello World");
        assert_eq!(tokens, vec!["hello", "world"]);
    }

    #[test]
    fn test_punctuation_stripped() {
        let tokens = tokenize("Hello, World! How are you?");
        assert_eq!(tokens, vec!["hello", "world", "how", "are", "you"]);
    }

    #[test]
    fn test_contraction_expansion() {
        let tokens = tokenize("Don't walk the tree!");
        assert_eq!(tokens, vec!["do", "not", "walk", "the", "tree"]);
    }

    #[test]
    fn test_whats_expansion() {
        let tokens = tokenize("what's walk mean");
        assert_eq!(tokens, vec!["what", "is", "walk", "mean"]);
    }

    #[test]
    fn test_path_preserved() {
        let result = normalize("zip up ~/Downloads");
        assert!(result.canonical_tokens.contains(&"pack_archive".to_string()));
        // Path should preserve original case
        assert!(
            result.canonical_tokens.iter().any(|t| t == "~/Downloads"),
            "path should preserve case, got: {:?}",
            result.canonical_tokens
        );
    }

    #[test]
    fn test_absolute_path_preserved() {
        let result = normalize("list /tmp/foo");
        assert!(
            result.canonical_tokens.contains(&"list_dir".to_string()),
            "tokens: {:?}, canonical: {:?}",
            result.tokens, result.canonical_tokens
        );
        assert!(result.canonical_tokens.contains(&"/tmp/foo".to_string()));
    }

    #[test]
    fn test_empty_input() {
        let result = normalize("");
        assert!(result.tokens.is_empty());
        assert!(result.canonical_tokens.is_empty());
        assert_eq!(result.raw, "");
    }

    #[test]
    fn test_whitespace_only() {
        let result = normalize("   \t  \n  ");
        assert!(result.tokens.is_empty());
        assert!(result.canonical_tokens.is_empty());
    }

    // -- Ordinal canonicalization --

    #[test]
    fn test_ordinal_first() {
        let result = normalize("move first step");
        assert!(result.canonical_tokens.contains(&"move_entry".to_string()));
        assert!(result.canonical_tokens.contains(&"1".to_string()));
    }

    #[test]
    fn test_ordinal_2nd() {
        let tokens = tokenize("the 2nd step");
        assert_eq!(tokens[1], "2");
    }

    #[test]
    fn test_ordinal_third() {
        let tokens = tokenize("third item");
        assert_eq!(tokens[0], "3");
    }

    #[test]
    fn test_ordinal_21st() {
        let tokens = tokenize("the 21st entry");
        assert_eq!(tokens[1], "21");
    }

    #[test]
    fn test_non_ordinal_passes_through() {
        let tokens = tokenize("fast");
        assert_eq!(tokens[0], "fast");
    }

    // -- Synonym mapping --

    #[test]
    fn test_zip_up_synonym() {
        let result = normalize("zip up everything in my downloads");
        assert!(
            result.canonical_tokens.contains(&"pack_archive".to_string()),
            "should contain pack_archive, got: {:?}",
            result.canonical_tokens
        );
    }

    #[test]
    fn test_unzip_synonym() {
        let result = normalize("unzip the archive");
        assert!(
            result.canonical_tokens.contains(&"extract_archive".to_string()),
            "got: {:?}",
            result.canonical_tokens
        );
    }

    #[test]
    fn test_find_files_synonym() {
        let result = normalize("find files in ~/Documents");
        assert!(
            result.canonical_tokens.contains(&"find_matching".to_string()),
            "got: {:?}",
            result.canonical_tokens
        );
    }

    #[test]
    fn test_ls_synonym() {
        let result = normalize("ls ~/Downloads");
        assert!(
            result.canonical_tokens.contains(&"list_dir".to_string()),
            "got: {:?}",
            result.canonical_tokens
        );
    }

    #[test]
    fn test_grep_synonym() {
        let result = normalize("grep for errors in the log");
        assert!(
            result.canonical_tokens.contains(&"search_content".to_string()),
            "got: {:?}",
            result.canonical_tokens
        );
    }

    #[test]
    fn test_walk_synonym() {
        let result = normalize("walk the directory tree");
        assert!(
            result.canonical_tokens.contains(&"walk_tree".to_string()),
            "got: {:?}",
            result.canonical_tokens
        );
    }

    #[test]
    fn test_multi_word_longest_match() {
        // "zip up everything" (3 words) should match before "zip up" (2 words)
        let result = normalize("zip up everything");
        assert_eq!(
            result.canonical_tokens,
            vec!["pack_archive"],
            "should match 3-word phrase"
        );
    }

    #[test]
    fn test_mixed_case_punctuation_contractions() {
        let result = normalize("DON'T Walk_Tree!! Please?");
        // "don't" -> "do not", "walk_tree" stays, "please" stays
        assert!(result.tokens.contains(&"do".to_string()));
        assert!(result.tokens.contains(&"not".to_string()));
        assert!(result.tokens.contains(&"walk_tree".to_string()));
        assert!(result.tokens.contains(&"please".to_string()));
    }

    #[test]
    fn test_canonical_op_passthrough() {
        // Already-canonical op names should pass through
        let result = normalize("walk_tree then filter");
        assert!(result.canonical_tokens.contains(&"walk_tree".to_string()));
        assert!(result.canonical_tokens.contains(&"filter".to_string()));
    }

    #[test]
    fn test_is_canonical_op() {
        assert!(is_canonical_op("walk_tree"));
        assert!(is_canonical_op("pack_archive"));
        assert!(is_canonical_op("extract_archive"));
        assert!(!is_canonical_op("zip"));
        assert!(!is_canonical_op("nonsense"));
    }

    #[test]
    fn test_git_synonyms() {
        let result = normalize("git log for the repo");
        assert!(
            result.canonical_tokens.contains(&"git_log".to_string()),
            "got: {:?}",
            result.canonical_tokens
        );
    }

    #[test]
    fn test_extension_in_path() {
        let result = normalize("find *.pdf files");
        // *.pdf should be preserved as a token
        assert!(
            result.tokens.iter().any(|t| t.contains("pdf")),
            "got: {:?}",
            result.tokens
        );
    }

    #[test]
    fn test_dollar_var_preserved() {
        let tokens = tokenize("use $keyword for pattern");
        assert!(tokens.contains(&"$keyword".to_string()));
    }

    // -- B4 bugfix: single-word git synonyms --

    #[test]
    fn test_clone_single_word_maps_to_git_clone() {
        let result = normalize("clone the repo");
        assert!(result.canonical_tokens.contains(&"git_clone".to_string()),
            "should contain git_clone: {:?}", result.canonical_tokens);
    }

    #[test]
    fn test_git_clone_two_word_still_works() {
        let result = normalize("git clone the repo");
        assert!(result.canonical_tokens.contains(&"git_clone".to_string()),
            "should contain git_clone: {:?}", result.canonical_tokens);
    }

    #[test]
    fn test_push_single_word_maps_to_git_push() {
        let result = normalize("push my changes");
        assert!(result.canonical_tokens.contains(&"git_push".to_string()),
            "should contain git_push: {:?}", result.canonical_tokens);
    }

    #[test]
    fn test_commit_single_word_maps_to_git_commit() {
        let result = normalize("commit the files");
        assert!(result.canonical_tokens.contains(&"git_commit".to_string()),
            "should contain git_commit: {:?}", result.canonical_tokens);
    }

    // -- Quoted string handling --

    #[test]
    fn test_quoted_string_preserved_as_single_token() {
        let result = normalize(r#"list files in "NO NAME""#);
        assert!(result.tokens.contains(&"NO NAME".to_string()),
            "quoted string should be a single token: {:?}", result.tokens);
    }

    #[test]
    fn test_quoted_string_with_spaces() {
        let result = normalize(r#"find "my important folder""#);
        assert!(result.tokens.contains(&"my important folder".to_string()),
            "quoted string should preserve spaces: {:?}", result.tokens);
    }

    #[test]
    fn test_quoted_string_preserves_case() {
        let result = normalize(r#"list "My Documents""#);
        assert!(result.tokens.contains(&"My Documents".to_string()),
            "quoted string should preserve case: {:?}", result.tokens);
    }

    #[test]
    fn test_no_quotes_normal_tokenization() {
        // Without quotes, normal tokenization applies
        let result = normalize("list all the files");
        assert_eq!(result.tokens, vec!["list", "all", "the", "files"]);
    }

    #[test]
    fn test_contractions_still_work_with_quotes() {
        // Contractions with apostrophes should still expand
        let result = normalize(r#"don't walk "NO NAME""#);
        assert!(result.tokens.contains(&"do".to_string()), "tokens: {:?}", result.tokens);
        assert!(result.tokens.contains(&"not".to_string()), "tokens: {:?}", result.tokens);
        assert!(result.tokens.contains(&"NO NAME".to_string()), "tokens: {:?}", result.tokens);
    }
}
