//! SymSpell-inspired typo correction for the NL UX layer.
//!
//! A domain-bounded dictionary built from operation names, type names, and
//! common filesystem/action vocabulary. Uses delete-neighborhood pre-computation
//! for O(1) lookup per token at query time.
//!
//! Design constraints:
//! - Max edit distance: 2
//! - Prefix length: 7 (only first 7 chars generate deletes)
//! - Dictionary is ~2000 words (domain + common English + slang + tech jargon)
//! - No heap allocation per query beyond the result Vec
//! - Words with no close match pass through unchanged (no false corrections)

use std::collections::HashMap;
use std::sync::OnceLock;

// ---------------------------------------------------------------------------
// Embedded fallback
// ---------------------------------------------------------------------------

const EMBEDDED_DICTIONARY: &str = include_str!("../../data/nl/nl_dictionary.yaml");

// ---------------------------------------------------------------------------
// SymSpell dictionary
// ---------------------------------------------------------------------------

/// A pre-computed SymSpell dictionary for domain-bounded typo correction.
pub struct SymSpellDict {
    /// Maps delete-variants → list of (original_word, word_frequency).
    /// Higher frequency = preferred correction.
    deletes: HashMap<String, Vec<(String, u32)>>,
    /// The original dictionary words (for exact-match short-circuit).
    words: HashMap<String, u32>,
    /// Maximum edit distance for corrections.
    max_edit_distance: usize,
    /// Prefix length for delete generation.
    prefix_length: usize,
}

impl SymSpellDict {
    /// Build a new SymSpell dictionary from a list of (word, frequency) pairs.
    pub fn new(entries: &[(&str, u32)], max_edit_distance: usize, prefix_length: usize) -> Self {
        let mut words = HashMap::new();
        let mut deletes: HashMap<String, Vec<(String, u32)>> = HashMap::new();

        for &(word, freq) in entries {
            let w = word.to_lowercase();
            words.insert(w.clone(), freq);

            // Generate all deletes within max_edit_distance
            let delete_variants = generate_deletes(&w, max_edit_distance, prefix_length);
            for variant in delete_variants {
                deletes
                    .entry(variant)
                    .or_default()
                    .push((w.clone(), freq));
            }
        }

        Self {
            deletes,
            words,
            max_edit_distance,
            prefix_length,
        }
    }

    /// Look up the best correction for a word.
    /// Returns the original word if it's already correct or no close match exists.
    pub fn correct(&self, word: &str) -> String {
        let w = word.to_lowercase();

        // Short words (1-2 chars) — don't correct, too ambiguous
        if w.len() <= 2 {
            return word.to_string();
        }

        // Exact match — no correction needed
        if self.words.contains_key(&w) {
            return w;
        }

        // Generate deletes of the input word and look up candidates
        let input_deletes = generate_deletes(&w, self.max_edit_distance, self.prefix_length);

        let mut best: Option<(String, u32, usize)> = None; // (word, freq, distance)

        // Also check the input itself as a delete variant
        let mut candidates_to_check = input_deletes;
        candidates_to_check.push(w.clone());

        for variant in &candidates_to_check {
            if let Some(entries) = self.deletes.get(variant) {
                for (dict_word, freq) in entries {
                    let dist = edit_distance(&w, dict_word);
                    if dist <= self.max_edit_distance {
                        let dominated = match &best {
                            Some((_, best_freq, best_dist)) => {
                                dist < *best_dist
                                    || (dist == *best_dist && *freq > *best_freq)
                            }
                            None => true,
                        };
                        if dominated {
                            best = Some((dict_word.clone(), *freq, dist));
                        }
                    }
                }
            }
        }

        match best {
            Some((corrected, _, _)) => corrected,
            None => word.to_string(), // No match — pass through unchanged
        }
    }

    /// Correct a sequence of tokens.
    pub fn correct_tokens(&self, tokens: &[String]) -> Vec<String> {
        tokens
            .iter()
            .map(|t| {
                // Don't correct path-like tokens, variables, globs, or numbers
                if t.starts_with("~/")
                    || t.starts_with('/')
                    || t.starts_with('$')
                    || t.starts_with('*')
                    || t.contains("://")
                    || t.parse::<f64>().is_ok()
                    || t.contains('_') // already an op name like walk_tree
                    || t.contains('.')  // file extension like .pdf
                {
                    t.clone()
                } else {
                    self.correct(t)
                }
            })
            .collect()
    }
}

// ---------------------------------------------------------------------------
// Delete generation
// ---------------------------------------------------------------------------

/// Generate all delete variants of a word within max_edit_distance.
/// Only considers the first `prefix_length` characters for efficiency.
fn generate_deletes(word: &str, max_distance: usize, prefix_length: usize) -> Vec<String> {
    let mut result = Vec::new();
    let chars: Vec<char> = word.chars().collect();
    let effective_len = chars.len().min(prefix_length);

    // Generate deletes recursively up to max_distance
    let mut queue: Vec<String> = vec![chars[..effective_len].iter().collect()];
    let mut seen = std::collections::HashSet::new();

    for _distance in 0..max_distance {
        let mut next_queue = Vec::new();
        for current in &queue {
            let current_chars: Vec<char> = current.chars().collect();
            for i in 0..current_chars.len() {
                let mut deleted: String = String::with_capacity(current_chars.len() - 1);
                for (j, &c) in current_chars.iter().enumerate() {
                    if j != i {
                        deleted.push(c);
                    }
                }
                if !deleted.is_empty() && seen.insert(deleted.clone()) {
                    result.push(deleted.clone());
                    next_queue.push(deleted);
                }
            }
        }
        queue = next_queue;
    }

    result
}

// ---------------------------------------------------------------------------
// Edit distance (Damerau-Levenshtein)
// ---------------------------------------------------------------------------

/// Compute the Damerau-Levenshtein edit distance between two strings.
/// Supports insertions, deletions, substitutions, and transpositions.
fn edit_distance(a: &str, b: &str) -> usize {
    let a_chars: Vec<char> = a.chars().collect();
    let b_chars: Vec<char> = b.chars().collect();
    let a_len = a_chars.len();
    let b_len = b_chars.len();

    if a_len == 0 {
        return b_len;
    }
    if b_len == 0 {
        return a_len;
    }

    // Use a 2D matrix for Damerau-Levenshtein
    let mut d = vec![vec![0usize; b_len + 1]; a_len + 1];

    for i in 0..=a_len {
        d[i][0] = i;
    }
    for j in 0..=b_len {
        d[0][j] = j;
    }

    for i in 1..=a_len {
        for j in 1..=b_len {
            let cost = if a_chars[i - 1] == b_chars[j - 1] {
                0
            } else {
                1
            };

            d[i][j] = (d[i - 1][j] + 1) // deletion
                .min(d[i][j - 1] + 1)     // insertion
                .min(d[i - 1][j - 1] + cost); // substitution

            // Transposition
            if i > 1
                && j > 1
                && a_chars[i - 1] == b_chars[j - 2]
                && a_chars[i - 2] == b_chars[j - 1]
            {
                d[i][j] = d[i][j].min(d[i - 2][j - 2] + cost);
            }
        }
    }

    d[a_len][b_len]
}

// ---------------------------------------------------------------------------
// Domain dictionary builder
// ---------------------------------------------------------------------------

/// Build the domain-bounded SymSpell dictionary.
///
/// Sources:
/// - All op names from fs_ops + power_tools (split on _)
/// - Common type names (file, dir, seq, entry, etc.)
/// - Common filesystem vocabulary
/// - Common action words
// ---------------------------------------------------------------------------
// Cached singleton
// ---------------------------------------------------------------------------

static DOMAIN_DICT: OnceLock<SymSpellDict> = OnceLock::new();

/// Get the cached domain dictionary (loaded once, reused forever).
pub fn domain_dict() -> &'static SymSpellDict {
    DOMAIN_DICT.get_or_init(build_domain_dict)
}

/// Build the domain-bounded SymSpell dictionary from YAML.
///
/// Loads word frequencies from `data/nl/nl_dictionary.yaml` (disk-first,
/// embedded fallback). All categories are flattened into a single dictionary.
pub fn build_domain_dict() -> SymSpellDict {
    let yaml_str = std::fs::read_to_string("data/nl/nl_dictionary.yaml")
        .ok()
        .unwrap_or_else(|| EMBEDDED_DICTIONARY.to_string());

    build_dict_from_yaml(&yaml_str).unwrap_or_else(|e| {
        eprintln!("WARN: failed to parse nl_dictionary.yaml from disk ({}), using embedded", e);
        build_dict_from_yaml(EMBEDDED_DICTIONARY)
            .expect("embedded nl_dictionary.yaml must parse")
    })
}

/// Parse the dictionary YAML and build a SymSpellDict.
///
/// The YAML schema is a map of category names to word→frequency maps:
/// ```yaml
/// op_components:
///   list: 100
///   dir: 100
/// common_english:
///   the: 100
///   that: 100
/// ```
fn build_dict_from_yaml(yaml_str: &str) -> Result<SymSpellDict, String> {
    let categories: HashMap<String, HashMap<String, u32>> =
        serde_yaml::from_str(yaml_str).map_err(|e| format!("YAML parse error: {}", e))?;

    // Flatten all categories, dedup by keeping highest frequency per word
    let mut merged: HashMap<String, u32> = HashMap::new();
    for (_category, words) in &categories {
        for (word, freq) in words {
            let entry = merged.entry(word.clone()).or_insert(0);
            if *freq > *entry {
                *entry = *freq;
            }
        }
    }

    // Sort for deterministic dictionary construction
    let mut entries: Vec<(String, u32)> = merged.into_iter().collect();
    entries.sort_by(|a, b| a.0.cmp(&b.0));

    let entry_refs: Vec<(&str, u32)> = entries.iter().map(|(w, f)| (w.as_str(), *f)).collect();
    Ok(SymSpellDict::new(&entry_refs, 2, 7))
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn dict() -> SymSpellDict {
        build_domain_dict()
    }

    // -- Edit distance --

    #[test]
    fn test_edit_distance_identical() {
        assert_eq!(edit_distance("hello", "hello"), 0);
    }

    #[test]
    fn test_edit_distance_one_sub() {
        assert_eq!(edit_distance("hello", "hallo"), 1);
    }

    #[test]
    fn test_edit_distance_one_delete() {
        assert_eq!(edit_distance("hello", "helo"), 1);
    }

    #[test]
    fn test_edit_distance_one_insert() {
        assert_eq!(edit_distance("helo", "hello"), 1);
    }

    #[test]
    fn test_edit_distance_transposition() {
        assert_eq!(edit_distance("ab", "ba"), 1);
    }

    #[test]
    fn test_edit_distance_two() {
        assert_eq!(edit_distance("kitten", "sitting"), 3);
    }

    #[test]
    fn test_edit_distance_empty() {
        assert_eq!(edit_distance("", "abc"), 3);
        assert_eq!(edit_distance("abc", ""), 3);
        assert_eq!(edit_distance("", ""), 0);
    }

    // -- Correction --

    #[test]
    fn test_correct_exact_match() {
        let d = dict();
        assert_eq!(d.correct("extract"), "extract");
        assert_eq!(d.correct("archive"), "archive");
        assert_eq!(d.correct("filter"), "filter");
    }

    #[test]
    fn test_correct_one_char_typo() {
        let d = dict();
        // Missing letter
        assert_eq!(d.correct("extrct"), "extract");
        // Wrong letter
        assert_eq!(d.correct("archivr"), "archive");
        // Extra letter
        assert_eq!(d.correct("ffilter"), "filter");
    }

    #[test]
    fn test_correct_two_char_typo() {
        let d = dict();
        // "archve" → "archive" (missing 'i')
        assert_eq!(d.correct("archve"), "archive");
    }

    #[test]
    fn test_correct_extrct_archve() {
        let d = dict();
        let tokens = vec!["extrct".to_string(), "archve".to_string()];
        let corrected = d.correct_tokens(&tokens);
        assert_eq!(corrected, vec!["extract", "archive"]);
    }

    #[test]
    fn test_correct_already_correct_passthrough() {
        let d = dict();
        assert_eq!(d.correct("walk"), "walk");
        assert_eq!(d.correct("tree"), "tree");
        assert_eq!(d.correct("file"), "file");
        assert_eq!(d.correct("sort"), "sort");
    }

    #[test]
    fn test_correct_gibberish_passthrough() {
        let d = dict();
        // Completely unrelated words with no close match
        assert_eq!(d.correct("xyzzyplugh"), "xyzzyplugh");
        assert_eq!(d.correct("qqqqqqqq"), "qqqqqqqq");
    }

    #[test]
    fn test_correct_edit_distance_3_no_correction() {
        let d = dict();
        // "xtract" is distance 2 from "extract" (missing 'e' + no other change)
        // Actually "xtract" → "extract" is distance 1 (missing 'e')
        // Let's use something truly distance 3
        // "xtrct" → "extract" = missing 'e', missing 'a' = distance 2... 
        // "xrct" → "extract" = 3 edits
        let result = d.correct("xrct");
        // Should NOT correct to "extract" (distance 3+)
        // It might match something else at distance 2, or pass through
        // The key invariant: it doesn't produce a wrong correction
        assert!(result == "xrct" || edit_distance(&result, "xrct") <= 2);
    }

    #[test]
    fn test_correct_short_words_passthrough() {
        let d = dict();
        // Very short words shouldn't be corrected (too ambiguous)
        assert_eq!(d.correct("a"), "a");
        assert_eq!(d.correct("xy"), "xy");
    }

    #[test]
    fn test_correct_tokens_preserves_paths() {
        let d = dict();
        let tokens = vec![
            "extrct".to_string(),
            "~/Downloads".to_string(),
            "/tmp/foo".to_string(),
            "$keyword".to_string(),
            "*.pdf".to_string(),
        ];
        let corrected = d.correct_tokens(&tokens);
        assert_eq!(corrected[0], "extract");
        assert_eq!(corrected[1], "~/Downloads"); // path preserved
        assert_eq!(corrected[2], "/tmp/foo");     // path preserved
        assert_eq!(corrected[3], "$keyword");     // var preserved
        assert_eq!(corrected[4], "*.pdf");        // glob preserved
    }

    #[test]
    fn test_correct_tokens_preserves_op_names() {
        let d = dict();
        let tokens = vec!["walk_tree".to_string(), "pack_archive".to_string()];
        let corrected = d.correct_tokens(&tokens);
        assert_eq!(corrected[0], "walk_tree");     // underscore = op name, preserved
        assert_eq!(corrected[1], "pack_archive");
    }

    #[test]
    fn test_correct_tokens_preserves_numbers() {
        let d = dict();
        let tokens = vec!["2".to_string(), "100".to_string()];
        let corrected = d.correct_tokens(&tokens);
        assert_eq!(corrected[0], "2");
        assert_eq!(corrected[1], "100");
    }

    #[test]
    fn test_correct_common_typos() {
        let d = dict();
        assert_eq!(d.correct("direcotry"), "directory");
        assert_eq!(d.correct("fiel"), "file");
        assert_eq!(d.correct("serach"), "search");
        assert_eq!(d.correct("donwload"), "download");
    }

    #[test]
    fn test_correct_transposition() {
        let d = dict();
        // "fiel" → "file" (transposition of 'i' and 'l'... actually 'l' and 'e')
        assert_eq!(d.correct("fiel"), "file");
        // "wlak" → "walk" (transposition)
        assert_eq!(d.correct("wlak"), "walk");
    }

    #[test]
    fn test_dict_has_reasonable_size() {
        let d = dict();
        // Should have ~2000 words after expansion
        assert!(d.words.len() >= 1900, "dict too small: {}", d.words.len());
        assert!(d.words.len() < 3000, "dict too large: {}", d.words.len());
        // Deletes table should be larger (each word generates multiple deletes)
        assert!(d.deletes.len() > d.words.len());
    }

    // -- B1 bugfix: common English words must NOT be corrected to domain ops --

    #[test]
    fn test_the_not_corrected_to_tee() {
        let d = dict();
        assert_eq!(d.correct("the"), "the");
    }

    #[test]
    fn test_thing_not_corrected_to_tee() {
        let d = dict();
        assert_eq!(d.correct("thing"), "thing");
    }

    #[test]
    fn test_that_not_corrected_to_what() {
        let d = dict();
        assert_eq!(d.correct("that"), "that");
    }

    #[test]
    fn test_scrap_not_corrected_to_script() {
        let d = dict();
        assert_eq!(d.correct("scrap"), "scrap");
    }

    #[test]
    fn test_do_the_thing_tokens_unchanged() {
        let d = dict();
        let tokens = d.correct_tokens(&["do".into(), "the".into(), "thing".into()]);
        assert_eq!(tokens, vec!["do", "the", "thing"]);
    }

    #[test]
    fn test_nah_scrap_that_tokens_unchanged() {
        let d = dict();
        let tokens = d.correct_tokens(&["nah".into(), "scrap".into(), "that".into()]);
        assert_eq!(tokens, vec!["nah", "scrap", "that"]);
    }

    #[test]
    fn test_mind_not_corrected_to_find() {
        let d = dict();
        assert_eq!(d.correct("mind"), "mind");
    }

    #[test]
    fn test_never_not_corrected() {
        let d = dict();
        assert_eq!(d.correct("never"), "never");
    }

    #[test]
    fn test_todo_not_corrected_to_good() {
        let d = dict();
        assert_eq!(d.correct("todo"), "todo");
    }

    #[test]
    fn test_fixme_not_corrected() {
        let d = dict();
        assert_eq!(d.correct("fixme"), "fixme");
    }

    #[test]
    fn test_never_mind_tokens_unchanged() {
        let d = dict();
        let tokens = d.correct_tokens(&["never".into(), "mind".into()]);
        assert_eq!(tokens, vec!["never", "mind"]);
    }

    // -- Hardening: slang/interjection passthrough (I5) --

    #[test]
    fn test_slang_passthrough() {
        let d = dict();
        assert_eq!(d.correct("yikes"), "yikes");
        assert_eq!(d.correct("whoa"), "whoa");
        assert_eq!(d.correct("gonna"), "gonna");
        assert_eq!(d.correct("wanna"), "wanna");
        assert_eq!(d.correct("gotta"), "gotta");
        assert_eq!(d.correct("lemme"), "lemme");
        assert_eq!(d.correct("gimme"), "gimme");
        assert_eq!(d.correct("kinda"), "kinda");
        assert_eq!(d.correct("sorta"), "sorta");
        assert_eq!(d.correct("dunno"), "dunno");
    }

    #[test]
    fn test_interjections_passthrough() {
        let d = dict();
        assert_eq!(d.correct("oops"), "oops");
        assert_eq!(d.correct("geez"), "geez");
        assert_eq!(d.correct("meh"), "meh");
        assert_eq!(d.correct("welp"), "welp");
        assert_eq!(d.correct("dang"), "dang");
    }

    #[test]
    fn test_typos_still_correct_with_expanded_dict() {
        let d = dict();
        // These must still work after adding 1600+ words
        assert_eq!(d.correct("compres"), "compress");
        assert_eq!(d.correct("extrct"), "extract");
        assert_eq!(d.correct("direcotry"), "directory");
        assert_eq!(d.correct("serach"), "search");
        assert_eq!(d.correct("donwload"), "download");
        assert_eq!(d.correct("archivr"), "archive");
        assert_eq!(d.correct("ffilter"), "filter");
        assert_eq!(d.correct("wlak"), "walk");
        assert_eq!(d.correct("fiel"), "file");
    }
}



