//! NL vocabulary loader — loads word lists from YAML.
//!
//! Single consolidated loader for all NL word-list data:
//! contractions, ordinals, approvals, rejections, stopwords, filler phrases,
//! filler prefixes, directory aliases, and noun patterns.
//!
//! Uses the standard disk-first + `include_str!` fallback pattern.

use serde::Deserialize;
use std::collections::{HashMap, HashSet};
use std::sync::OnceLock;

// ---------------------------------------------------------------------------
// Embedded fallback
// ---------------------------------------------------------------------------

const EMBEDDED_VOCAB: &str = include_str!("../../data/nl/nl_vocab.yaml");

// ---------------------------------------------------------------------------
// YAML schema types
// ---------------------------------------------------------------------------

#[derive(Debug, Deserialize)]
struct VocabYaml {
    contractions: HashMap<String, String>,
    ordinals: HashMap<String, u32>,
    approvals: ApprovalRejectionYaml,
    rejections: ApprovalRejectionYaml,
    stopwords: Vec<String>,
    filler_phrases: Vec<String>,
    filler_prefixes: Vec<String>,
    #[serde(default)]
    dir_aliases: HashMap<String, String>,
    #[serde(default)]
    noun_patterns: HashMap<String, Vec<String>>,
}

#[derive(Debug, Deserialize)]
struct ApprovalRejectionYaml {
    single: Vec<String>,
    multi: Vec<String>,
}

// ---------------------------------------------------------------------------
// Runtime vocabulary — the loaded, indexed form
// ---------------------------------------------------------------------------

/// Loaded NL vocabulary, indexed for fast lookup.
#[derive(Debug)]
pub struct NlVocab {
    /// Contractions: sorted longest-first for replacement order.
    pub contractions: Vec<(String, String)>,
    /// Ordinal words → numeric values.
    pub ordinals: HashMap<String, u32>,
    /// Single-word approval tokens.
    pub approval_singles: HashSet<String>,
    /// Multi-word approval phrases.
    pub approval_multis: Vec<String>,
    /// Single-word rejection tokens.
    pub rejection_singles: HashSet<String>,
    /// Multi-word rejection phrases.
    pub rejection_multis: Vec<String>,
    /// Stopwords for slot extraction.
    pub stopwords: HashSet<String>,
    /// Filler phrases (approval tails like "why not", "that works").
    pub filler_phrases: Vec<String>,
    /// Filler prefixes (edit-command prefixes like "also", "and", "then").
    pub filler_prefixes: HashSet<String>,
    /// Directory aliases: common names → filesystem paths (e.g. "desktop" → "~/Desktop").
    pub dir_aliases: HashMap<String, String>,
    /// Noun-to-filetype patterns: common nouns → glob patterns (e.g. "screenshots" → ["*.png"]).
    pub noun_patterns: HashMap<String, Vec<String>>,
}

// ---------------------------------------------------------------------------
// Singleton
// ---------------------------------------------------------------------------

static VOCAB: OnceLock<NlVocab> = OnceLock::new();

/// Get the loaded NL vocabulary (singleton, loaded on first call).
pub fn vocab() -> &'static NlVocab {
    VOCAB.get_or_init(load_vocab)
}

// ---------------------------------------------------------------------------
// Loader
// ---------------------------------------------------------------------------

fn load_vocab() -> NlVocab {
    // Disk-first, embedded fallback
    let yaml_str = std::fs::read_to_string("data/nl/nl_vocab.yaml")
        .ok()
        .unwrap_or_else(|| EMBEDDED_VOCAB.to_string());

    parse_vocab(&yaml_str).unwrap_or_else(|e| {
        eprintln!("WARN: failed to parse nl_vocab.yaml from disk ({}), using embedded", e);
        parse_vocab(EMBEDDED_VOCAB).expect("embedded nl_vocab.yaml must parse")
    })
}

fn parse_vocab(yaml_str: &str) -> Result<NlVocab, String> {
    let raw: VocabYaml = serde_yaml::from_str(yaml_str)
        .map_err(|e| format!("YAML parse error: {}", e))?;

    // Build contractions — sorted by key length descending (longest first)
    let mut contractions: Vec<(String, String)> = raw.contractions.into_iter().collect();
    contractions.sort_by(|a, b| b.0.len().cmp(&a.0.len()));

    let ordinals = raw.ordinals;

    let approval_singles: HashSet<String> = raw.approvals.single.into_iter().collect();
    let approval_multis = raw.approvals.multi;
    let rejection_singles: HashSet<String> = raw.rejections.single.into_iter().collect();
    let rejection_multis = raw.rejections.multi;
    let stopwords: HashSet<String> = raw.stopwords.into_iter().collect();
    let filler_phrases = raw.filler_phrases;
    let filler_prefixes: HashSet<String> = raw.filler_prefixes.into_iter().collect();
    let dir_aliases = raw.dir_aliases;
    let noun_patterns = raw.noun_patterns;

    Ok(NlVocab {
        contractions,
        ordinals,
        approval_singles,
        approval_multis,
        rejection_singles,
        rejection_multis,
        stopwords,
        filler_phrases,
        filler_prefixes,
        dir_aliases,
        noun_patterns,
    })
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vocab_loads() {
        let v = vocab();
        assert!(!v.contractions.is_empty(), "contractions should not be empty");
        assert!(!v.ordinals.is_empty(), "ordinals should not be empty");
        assert!(!v.approval_singles.is_empty(), "approval_singles should not be empty");
        assert!(!v.rejection_singles.is_empty(), "rejection_singles should not be empty");
        assert!(!v.stopwords.is_empty(), "stopwords should not be empty");
    }

    #[test]
    fn test_contractions_dont() {
        let v = vocab();
        let found = v.contractions.iter().find(|(k, _)| k == "don't");
        assert!(found.is_some(), "should have don't contraction");
        assert_eq!(found.unwrap().1, "do not");
    }

    #[test]
    fn test_contractions_sorted_longest_first() {
        let v = vocab();
        for window in v.contractions.windows(2) {
            assert!(
                window[0].0.len() >= window[1].0.len(),
                "contractions should be sorted longest-first: '{}' before '{}'",
                window[0].0, window[1].0
            );
        }
    }

    #[test]
    fn test_ordinals() {
        let v = vocab();
        assert_eq!(v.ordinals.get("first"), Some(&1));
        assert_eq!(v.ordinals.get("twelfth"), Some(&12));
        assert_eq!(v.ordinals.get("nonexistent"), None);
    }

    #[test]
    fn test_approval_singles() {
        let v = vocab();
        assert!(v.approval_singles.contains("lgtm"));
        assert!(v.approval_singles.contains("yes"));
        assert!(v.approval_singles.contains("ok"));
        assert!(!v.approval_singles.contains("no"));
    }

    #[test]
    fn test_approval_multis() {
        let v = vocab();
        assert!(v.approval_multis.contains(&"sounds good".to_string()));
        assert!(v.approval_multis.contains(&"ship it".to_string()));
    }

    #[test]
    fn test_rejection_singles() {
        let v = vocab();
        assert!(v.rejection_singles.contains("no"));
        assert!(v.rejection_singles.contains("nah"));
        assert!(v.rejection_singles.contains("cancel"));
        assert!(!v.rejection_singles.contains("yes"));
    }

    #[test]
    fn test_rejection_multis() {
        let v = vocab();
        assert!(v.rejection_multis.contains(&"scratch that".to_string()));
        assert!(v.rejection_multis.contains(&"forget it".to_string()));
    }

    #[test]
    fn test_stopwords() {
        let v = vocab();
        assert!(v.stopwords.contains("the"));
        assert!(v.stopwords.contains("a"));
        assert!(v.stopwords.contains("ok"));
        assert!(v.stopwords.contains("hmm"));
        assert!(!v.stopwords.contains("walk_tree"));
    }

    #[test]
    fn test_filler_phrases() {
        let v = vocab();
        assert!(v.filler_phrases.contains(&"why not".to_string()));
        assert!(v.filler_phrases.contains(&"that works".to_string()));
    }

    #[test]
    fn test_filler_prefixes() {
        let v = vocab();
        assert!(v.filler_prefixes.contains("also"));
        assert!(v.filler_prefixes.contains("and"));
        assert!(v.filler_prefixes.contains("then"));
    }

    #[test]
    fn test_contraction_count() {
        let v = vocab();
        assert_eq!(v.contractions.len(), 38, "expected 38 contractions, got {}", v.contractions.len());
    }

    #[test]
    fn test_ordinal_count() {
        let v = vocab();
        assert_eq!(v.ordinals.len(), 12, "expected 12 ordinals, got {}", v.ordinals.len());
    }

    #[test]
    fn test_parse_embedded_always_works() {
        // Directly parse the embedded YAML — must never fail
        let result = parse_vocab(EMBEDDED_VOCAB);
        assert!(result.is_ok(), "embedded vocab YAML must parse: {:?}", result.err());
    }

    #[test]
    fn test_parse_malformed_yaml_returns_error() {
        let result = parse_vocab("not: valid: yaml: [[[");
        assert!(result.is_err());
    }

    #[test]
    fn test_dir_aliases_loaded() {
        let v = vocab();
        assert!(!v.dir_aliases.is_empty(), "dir_aliases should not be empty");
        assert_eq!(v.dir_aliases.get("desktop"), Some(&"~/Desktop".to_string()));
        assert_eq!(v.dir_aliases.get("downloads"), Some(&"~/Downloads".to_string()));
        assert_eq!(v.dir_aliases.get("documents"), Some(&"~/Documents".to_string()));
    }

    #[test]
    fn test_noun_patterns_loaded() {
        let v = vocab();
        assert!(!v.noun_patterns.is_empty(), "noun_patterns should not be empty");
        assert_eq!(v.noun_patterns.get("screenshots"), Some(&vec!["*.png".to_string()]));
        let photos = v.noun_patterns.get("photos").expect("photos should exist");
        assert!(photos.contains(&"*.png".to_string()));
        assert!(photos.contains(&"*.jpg".to_string()));
    }
}
