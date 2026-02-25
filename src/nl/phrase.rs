//! Phrase tokenizer for the Earley parser pre-processing stage.
//!
//! Groups multi-word verb phrases into single canonical tokens before
//! the Earley parser sees them. Uses the `phrase_groups` from the lexicon
//! to match content-word skeletons while stripping intervening stopwords.
//!
//! Example:
//!   Input:  ["make", "me", "a", "list", "of", "files"]
//!   Output: ["list", "of", "files"]
//!
//! The algorithm:
//!   1. Build an index of phrase groups keyed by first skeleton word.
//!   2. For each token in the input stream:
//!      a. If it matches the first word of any phrase skeleton, try to
//!         match the remaining skeleton words by scanning ahead and
//!         skipping stopwords.
//!      b. On match: emit the canonical token, advance past all consumed
//!         tokens (skeleton words + stopwords between them).
//!      c. On no match: emit the token unchanged.
//!   3. Greedy longest-match: when multiple skeletons share a first word,
//!      try longer skeletons first.

use std::collections::{HashMap, HashSet};

use crate::nl::lexicon::{self, PhraseGroup};

/// Stopwords that can appear between skeleton content words and are stripped.
/// This is a superset of fillers + determiners + pronouns.
fn stopwords() -> &'static HashSet<&'static str> {
    use std::sync::OnceLock;
    static STOPWORDS: OnceLock<HashSet<&'static str>> = OnceLock::new();
    STOPWORDS.get_or_init(|| {
        [
            // Determiners
            "a", "an", "the", "this", "that", "these", "those",
            "all", "any", "every", "each", "some",
            // Pronouns / objects
            "me", "i", "my", "you", "your", "us", "our", "it", "its",
            "them", "their", "him", "his", "her",
            // Fillers / auxiliaries
            "up", "out", "just", "please", "can", "could", "would",
            "want", "need", "like", "really", "actually", "basically",
            "okay", "ok", "so", "well", "right", "now",
            "go", "do", "be", "is", "are", "was", "were",
            "been", "being", "have", "has", "had",
            "will", "shall", "may", "might", "should",
            "let", "gonna", "wanna", "gotta",
            "thing", "things", "stuff",
            // Conjunctions
            "and", "or",
            // Prepositions that can appear inside phrases
            "for", "about", "to", "of", "on", "by", "with", "using",
            // Generic words that appear between algorithm name parts
            "test", "algorithm", "method", "technique", "approach",
            "function", "problem", "check", "simulation", "number",
            // Numeric fragments that appear in algorithm names
            "1a", "32", "64", "128", "256",
        ].into_iter().collect()
    })
}

/// Build a lookup index: first skeleton word → list of (skeleton, canonical),
/// sorted by skeleton length descending (longest match first).
fn build_index(groups: &[PhraseGroup]) -> HashMap<String, Vec<(Vec<String>, String)>> {
    let mut index: HashMap<String, Vec<(Vec<String>, String)>> = HashMap::new();
    for pg in groups {
        if let Some(first) = pg.skeleton.first() {
            index.entry(first.clone())
                .or_default()
                .push((pg.skeleton.clone(), pg.canonical.clone()));
        }
    }
    // Sort each bucket by skeleton length descending (longest match first)
    for entries in index.values_mut() {
        entries.sort_by(|a, b| b.0.len().cmp(&a.0.len()));
    }
    index
}

/// Apply phrase tokenization to a token stream.
///
/// Scans for multi-word phrase matches from the lexicon's `phrase_groups`,
/// stripping intervening stopwords between skeleton content words.
/// Matched phrases are replaced by their canonical single token.
/// Unmatched tokens pass through unchanged.
pub fn phrase_tokenize(tokens: &[String]) -> Vec<String> {
    if tokens.is_empty() {
        return Vec::new();
    }

    let lex = lexicon::lexicon();
    let index = build_index(&lex.phrase_groups);
    let stops = stopwords();

    let mut result = Vec::new();
    let mut i = 0;

    while i < tokens.len() {
        let token = tokens[i].to_lowercase();

        if let Some(candidates) = index.get(&token) {
            let mut matched = false;

            for (skeleton, canonical) in candidates {
                if let Some(consumed) = try_match(tokens, i, skeleton, stops) {
                    // Match found — emit canonical token, skip consumed tokens
                    result.push(canonical.clone());
                    i += consumed;
                    matched = true;
                    break;
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

/// Try to match a skeleton starting at position `start` in the token stream.
/// Returns the number of tokens consumed (skeleton words + stopwords) on
/// success, or None if the skeleton doesn't match.
///
/// The skeleton words must appear in order. Between skeleton words, any
/// number of stopwords may appear (and are consumed/skipped).
fn try_match(
    tokens: &[String],
    start: usize,
    skeleton: &[String],
    stops: &HashSet<&str>,
) -> Option<usize> {
    if skeleton.is_empty() {
        return None;
    }

    let mut ti = start; // token index
    let mut si = 0;     // skeleton index

    while si < skeleton.len() && ti < tokens.len() {
        let token_lower = tokens[ti].to_lowercase();

        if token_lower == skeleton[si] {
            // Skeleton word matched
            si += 1;
            ti += 1;
        } else if si > 0 && stops.contains(token_lower.as_str()) {
            // Stopword between skeleton words — skip it
            // (Only skip stopwords AFTER the first skeleton word has matched,
            //  so we don't consume leading stopwords before the phrase)
            ti += 1;
        } else {
            // Non-matching, non-stopword token — match fails
            return None;
        }
    }

    if si == skeleton.len() {
        // All skeleton words matched
        Some(ti - start)
    } else {
        // Ran out of tokens before matching all skeleton words
        None
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_input() {
        assert_eq!(phrase_tokenize(&[]), Vec::<String>::new());
    }

    #[test]
    fn test_no_match_passthrough() {
        let tokens = vec!["find".into(), "comics".into(), "in".into(), "downloads".into()];
        let result = phrase_tokenize(&tokens);
        assert_eq!(result, tokens);
    }

    #[test]
    fn test_make_a_list() {
        let tokens: Vec<String> = "make a list".split_whitespace().map(String::from).collect();
        let result = phrase_tokenize(&tokens);
        assert_eq!(result, vec!["list"]);
    }

    #[test]
    fn test_make_me_a_list() {
        let tokens: Vec<String> = "make me a list".split_whitespace().map(String::from).collect();
        let result = phrase_tokenize(&tokens);
        assert_eq!(result, vec!["list"]);
    }

    #[test]
    fn test_make_me_up_a_list() {
        let tokens: Vec<String> = "make me up a list".split_whitespace().map(String::from).collect();
        let result = phrase_tokenize(&tokens);
        assert_eq!(result, vec!["list"]);
    }

    #[test]
    fn test_phrase_with_trailing_tokens() {
        let tokens: Vec<String> = "make a list of files in downloads"
            .split_whitespace().map(String::from).collect();
        let result = phrase_tokenize(&tokens);
        // "make a list" → "list", then "of files in downloads" pass through
        assert_eq!(result[0], "list");
        assert!(result.contains(&"files".to_string()));
        assert!(result.contains(&"downloads".to_string()));
    }

    #[test]
    fn test_take_a_look() {
        let tokens: Vec<String> = "take a look".split_whitespace().map(String::from).collect();
        let result = phrase_tokenize(&tokens);
        assert_eq!(result, vec!["find"]);
    }

    #[test]
    fn test_take_a_look_at_files() {
        let tokens: Vec<String> = "take a look at files in downloads"
            .split_whitespace().map(String::from).collect();
        let result = phrase_tokenize(&tokens);
        assert_eq!(result[0], "find");
    }

    #[test]
    fn test_zip_up() {
        let tokens: Vec<String> = "zip up".split_whitespace().map(String::from).collect();
        let result = phrase_tokenize(&tokens);
        assert_eq!(result, vec!["zip"]);
    }

    #[test]
    fn test_zip_up_with_object() {
        let tokens: Vec<String> = "zip up files in downloads"
            .split_whitespace().map(String::from).collect();
        let result = phrase_tokenize(&tokens);
        assert_eq!(result[0], "zip");
        assert!(result.contains(&"files".to_string()));
    }

    #[test]
    fn test_clean_up() {
        let tokens: Vec<String> = "clean up downloads"
            .split_whitespace().map(String::from).collect();
        let result = phrase_tokenize(&tokens);
        assert_eq!(result[0], "clean");
        assert!(result.contains(&"downloads".to_string()));
    }

    #[test]
    fn test_back_up() {
        let tokens: Vec<String> = "back up files"
            .split_whitespace().map(String::from).collect();
        let result = phrase_tokenize(&tokens);
        assert_eq!(result[0], "backup");
    }

    #[test]
    fn test_set_up() {
        let tokens: Vec<String> = "set up".split_whitespace().map(String::from).collect();
        let result = phrase_tokenize(&tokens);
        assert_eq!(result, vec!["install"]);
    }

    #[test]
    fn test_phrase_at_end_of_stream() {
        let tokens: Vec<String> = "please make a list"
            .split_whitespace().map(String::from).collect();
        let result = phrase_tokenize(&tokens);
        // "please" passes through, "make a list" → "list"
        assert!(result.contains(&"list".to_string()));
    }

    #[test]
    fn test_case_insensitive() {
        let tokens: Vec<String> = "Make A List".split_whitespace().map(String::from).collect();
        let result = phrase_tokenize(&tokens);
        assert_eq!(result, vec!["list"]);
    }

    #[test]
    fn test_no_leading_stopword_consumption() {
        // "a" before "make" should NOT be consumed as part of the phrase
        let tokens: Vec<String> = "a make list".split_whitespace().map(String::from).collect();
        let result = phrase_tokenize(&tokens);
        // "a" passes through, then "make list" → "list"
        assert_eq!(result.len(), 2);
        assert_eq!(result[0], "a");
        assert_eq!(result[1], "list");
    }

    #[test]
    fn test_partial_match_no_consume() {
        // "make" alone without "list" following should pass through
        let tokens: Vec<String> = "make comics".split_whitespace().map(String::from).collect();
        let result = phrase_tokenize(&tokens);
        assert_eq!(result, vec!["make", "comics"]);
    }

    #[test]
    fn test_write_a_program() {
        let tokens: Vec<String> = "write a program".split_whitespace().map(String::from).collect();
        let result = phrase_tokenize(&tokens);
        assert_eq!(result, vec!["implement"]);
    }

    #[test]
    fn test_list_running_apps_phrase() {
        let tokens: Vec<String> = "list running apps".split_whitespace().map(String::from).collect();
        let result = phrase_tokenize(&tokens);
        eprintln!("phrase_tokenize({:?}) = {:?}", tokens, result);
        assert_eq!(result, vec!["list_running_apps"]);
    }
}
