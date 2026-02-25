//! Text normalization for the NL UX layer.
//!
//! Pipeline: raw input → case fold → strip punctuation → expand contractions →
//! canonicalize ordinals. Pure string transforms — no allocation-heavy
//! structures, no external dependencies. Designed for grep-like latency.

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
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Normalize a raw user input string through the full pipeline.
pub fn normalize(input: &str) -> NormalizedInput {
    let raw = input.to_string();

    // 1. Case fold + strip punctuation + expand contractions + ordinals
    let tokens = tokenize(input);

    NormalizedInput {
        raw,
        tokens,
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
        let result = normalize("list ~/Downloads");
        assert!(
            result.tokens.iter().any(|t| t == "~/Downloads"),
            "path should preserve case, got: {:?}", result.tokens
        );
    }

    #[test]
    fn test_absolute_path_preserved() {
        let result = normalize("list /tmp/foo");
        assert!(result.tokens.contains(&"/tmp/foo".to_string()));
    }

    #[test]
    fn test_empty_input() {
        let result = normalize("");
        assert!(result.tokens.is_empty());
        assert_eq!(result.raw, "");
    }

    #[test]
    fn test_whitespace_only() {
        let result = normalize("   \t  \n  ");
        assert!(result.tokens.is_empty());
    }

    // -- Ordinal canonicalization --

    #[test]
    fn test_ordinal_first() {
        let result = normalize("move first step");
        assert!(result.tokens.contains(&"1".to_string()));
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
    fn test_is_canonical_op() {
        // Derived from the ops YAML packs — no synonym mapping needed.
        assert!(is_canonical_op("walk_tree"));
        assert!(is_canonical_op("pack_archive"));
        assert!(is_canonical_op("extract_archive"));
        assert!(!is_canonical_op("zip"));
        assert!(!is_canonical_op("nonsense"));
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
