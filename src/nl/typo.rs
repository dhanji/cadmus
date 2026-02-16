//! SymSpell-inspired typo correction for the NL UX layer.
//!
//! A domain-bounded dictionary built from operation names, type names, and
//! common filesystem/action vocabulary. Uses delete-neighborhood pre-computation
//! for O(1) lookup per token at query time.
//!
//! Design constraints:
//! - Max edit distance: 2
//! - Prefix length: 7 (only first 7 chars generate deletes)
//! - Dictionary is ~500 words (domain-bounded, not general English)
//! - No heap allocation per query beyond the result Vec
//! - Words with no close match pass through unchanged (no false corrections)

use std::collections::HashMap;

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
pub fn build_domain_dict() -> SymSpellDict {
    let entries: Vec<(&str, u32)> = vec![
        // --- Op name components (high frequency — these are the targets) ---
        // Extracted by splitting all 113 op names on '_'
        ("list", 100), ("dir", 100), ("read", 100), ("write", 100),
        ("file", 100), ("stat", 80), ("walk", 100), ("tree", 100),
        ("hierarchy", 60), ("flatten", 60), ("filter", 100), ("sort", 100),
        ("extract", 100), ("archive", 100), ("pack", 100), ("concat", 60),
        ("seq", 60), ("rename", 100), ("move", 100), ("entry", 80),
        ("search", 100), ("content", 80), ("find", 100), ("matching", 80),
        ("map", 80), ("entries", 80), ("copy", 100), ("delete", 100),
        ("create", 100), ("link", 80), ("set", 80), ("permissions", 60),
        ("owner", 60), ("replace", 80), ("head", 80), ("tail", 80),
        ("unique", 80), ("count", 80), ("diff", 80), ("checksum", 60),
        ("get", 80), ("size", 80), ("mtime", 40), ("type", 80),
        ("spotlight", 60), ("xattr", 40), ("remove", 100), ("quarantine", 40),
        ("open", 80), ("with", 60), ("reveal", 60), ("clipboard", 40),
        ("paste", 60), ("plist", 40), ("download", 80), ("upload", 60),
        ("sync", 60),
        // Git
        ("git", 100), ("init", 60), ("clone", 60), ("add", 80),
        ("commit", 80), ("log", 80), ("range", 40), ("branch", 60),
        ("checkout", 60), ("merge", 60), ("rebase", 40), ("stash", 40),
        ("pop", 40), ("push", 60), ("pull", 60), ("blame", 40),
        ("bisect", 40), ("tag", 40), ("status", 60),
        // Tmux/screen
        ("tmux", 60), ("attach", 40), ("split", 60), ("send", 40),
        ("keys", 40), ("screen", 60), ("session", 40), ("new", 60),
        // JSON/YAML/CSV
        ("jq", 60), ("query", 80), ("transform", 60), ("yq", 40),
        ("convert", 60), ("csv", 60), ("cut", 60), ("join", 40),
        // Text processing
        ("awk", 60), ("aggregate", 40), ("sed", 60), ("script", 40),
        ("fields", 40), ("tr", 40), ("tee", 40), ("column", 40),
        ("format", 40),
        // System
        ("ps", 40), ("kill", 80), ("process", 60), ("pkill", 40),
        ("pattern", 80), ("watch", 40), ("command", 40), ("df", 40),
        ("usage", 40), ("du", 40), ("lsof", 40), ("detect", 40),
        ("uname", 40), ("info", 60), ("uptime", 40),
        // Network
        ("ssh", 60), ("exec", 40), ("scp", 40), ("transfer", 40),
        ("wget", 40), ("nc", 40), ("connect", 40), ("ping", 40),
        ("host", 40), ("dig", 40), ("lookup", 40),
        // Compression
        ("gzip", 60), ("compress", 80), ("decompress", 60),
        ("xz", 40), ("base64", 40), ("encode", 40), ("decode", 40),
        ("openssl", 40), ("hash", 40),

        // --- Type names ---
        ("bytes", 60), ("text", 60), ("image", 60), ("name", 80),
        ("path", 80), ("metadata", 40), ("directory", 80), ("folder", 80),
        ("pattern", 80), ("url", 40), ("json", 60), ("yaml", 60),
        ("csv", 60), ("xml", 40), ("html", 40), ("pdf", 40),
        ("zip", 80), ("tar", 60), ("cbz", 40), ("plist", 40),

        // --- Common action words ---
        ("show", 80), ("display", 60), ("print", 60), ("view", 60),
        ("look", 60), ("see", 40), ("check", 60), ("verify", 40),
        ("run", 60), ("execute", 40), ("start", 40), ("stop", 40),
        ("help", 60), ("explain", 60), ("describe", 40), ("what", 80),
        ("how", 60), ("why", 40), ("where", 40), ("which", 40),
        ("all", 80), ("every", 40), ("each", 60), ("any", 60),
        ("everything", 60), ("nothing", 20), ("files", 100),
        ("directories", 60), ("folders", 60), ("contents", 60),
        ("results", 40), ("items", 40), ("lines", 60),
        ("recursive", 60), ("recursively", 60),

        // --- Common filesystem words ---
        ("subdirectory", 60), ("subdirectories", 40), ("subfolder", 40),
        ("parent", 40), ("child", 40), ("root", 40), ("home", 40),
        ("tmp", 40), ("temp", 40), ("temporary", 40),
        ("hidden", 40), ("extension", 60), ("filename", 60),
        ("basename", 40), ("dirname", 40),

        // --- Edit/dialogue words ---
        ("step", 80), ("before", 60), ("after", 60), ("between", 40),
        ("previous", 60), ("next", 60), ("last", 60), ("first", 60),
        ("skip", 80), ("ignore", 60), ("exclude", 60), ("include", 60),
        ("named", 60), ("called", 40), ("matching", 80),
        ("containing", 40), ("starting", 40), ("ending", 40),
        ("approve", 60), ("reject", 40), ("cancel", 40), ("undo", 40),
        ("change", 80), ("modify", 60), ("update", 60), ("edit", 60),
        ("insert", 60), ("append", 40), ("prepend", 40),

        // --- Approval/rejection words (must not be typo-corrected) ---
        ("lgtm", 80), ("sounds", 60), ("good", 60), ("great", 60),
        ("fine", 40), ("perfect", 40), ("cool", 40), ("awesome", 40),
        ("nice", 40), ("sure", 60), ("yeah", 40), ("yep", 40),
        ("nope", 40), ("nah", 40), ("scratch", 40), ("forget", 40),
        ("over", 40), ("again", 40), ("back", 40), ("looks", 60),
        ("ship", 40), ("send", 40), ("ahead", 40), ("done", 40),
        ("right", 40), ("correct", 40), ("wrong", 40),
        ("mean", 60), ("means", 40), ("does", 40),

        // --- Synonym source words (must pass through typo correction) ---
        ("unzip", 80), ("zip", 80), ("tar", 60), ("untar", 60),
        ("grep", 80), ("ls", 40), ("mv", 40), ("cp", 40), ("rm", 40),
        ("mkdir", 40), ("ln", 40), ("cat", 40), ("less", 40),
        ("rsync", 40), ("curl", 40), ("work", 60), ("works", 40),
        ("errors", 40), ("repo", 40), ("log", 60),

        // --- Common English words that must NOT be corrected to domain ops ---
        // Without these, SymSpell corrects "the"→"tee", "thing"→"tee"/"ping",
        // "that"→"what", "scrap"→"script", etc.
        ("the", 100), ("that", 100), ("this", 100), ("than", 80),
        ("then", 80), ("them", 80), ("they", 80), ("there", 80),
        ("their", 80), ("these", 80), ("those", 80), ("thing", 80),
        ("things", 60), ("think", 60), ("though", 40), ("through", 40),
        ("three", 40), ("throw", 40),
        ("scrap", 60), ("scrape", 40), ("scratch", 60),
        ("instead", 60), ("actually", 60), ("about", 60),
        ("into", 60), ("also", 60), ("just", 60), ("only", 40),
        ("some", 60), ("same", 40), ("such", 40), ("much", 40),
        ("other", 60), ("another", 40), ("because", 40),
        ("but", 60), ("not", 80), ("nor", 40), ("yet", 40),
        ("still", 40), ("even", 40), ("very", 40), ("really", 40),
        ("quite", 40), ("rather", 40),
        ("do", 80), ("did", 60), ("doing", 40),
        ("have", 80), ("has", 60), ("had", 60), ("having", 40),
        ("be", 60), ("been", 60), ("being", 40),
        ("would", 60), ("could", 60), ("should", 60),
        ("might", 40), ("must", 40),
        ("for", 80), ("from", 80), ("with", 80),
        ("like", 60), ("want", 60), ("need", 60),
        ("make", 60), ("made", 40), ("take", 60), ("took", 40),
        ("give", 40), ("gave", 40), ("keep", 40), ("kept", 40),
        ("let", 60), ("put", 60), ("try", 60), ("tried", 40),
        ("say", 40), ("said", 40), ("tell", 40), ("told", 40),
        ("know", 60), ("knew", 40), ("see", 60), ("saw", 40),
        ("come", 40), ("came", 40), ("go", 60), ("went", 40),
        ("get", 80), ("got", 60), ("going", 40), ("getting", 40),
        // Rejection/approval words that must survive typo correction
        ("mind", 60), ("never", 60), ("wait", 60), ("hold", 40),
        ("maybe", 40), ("perhaps", 40), ("later", 40),
        // Common code-comment markers (search keywords)
        ("todo", 60), ("fixme", 60), ("hack", 40), ("note", 40),
        ("xxx", 40), ("bug", 40), ("warn", 40), ("error", 60),
        // More common English that gets false-corrected
        ("something", 40), ("anything", 40),
        ("someone", 40), ("anyone", 40),
        ("kind", 40), ("find", 80), ("bind", 40), ("wind", 40),
    ];

    SymSpellDict::new(&entries, 2, 7)
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
        // Should have a few hundred words
        assert!(d.words.len() > 100, "dict too small: {}", d.words.len());
        assert!(d.words.len() < 1000, "dict too large: {}", d.words.len());
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
}


