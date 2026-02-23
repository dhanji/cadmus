//! Grammar construction for the NL Earley parser.
//!
//! Builds a context-free grammar for command-oriented English from the
//! lexicon categories. The grammar is small and focused:
//!
//! ```text
//! Command      → Verb Object Modifiers?
//! Command      → Verb Modifiers?
//! Command      → Verb Filler* Object Modifiers?
//! Verb         → 'verb'
//! Object       → NounPhrase
//! Object       → 'path'
//! Object       → 'pattern'
//! NounPhrase   → 'noun'
//! NounPhrase   → 'determiner' 'noun'
//! NounPhrase   → 'quantifier' 'noun'
//! NounPhrase   → 'possessive' 'noun'
//! Modifiers    → Modifier+
//! Modifier     → LocationMod | OrderMod | PatternMod
//! LocationMod  → 'preposition' 'possessive'? PathExpr 'filler'?
//! PathExpr     → 'path_noun' | 'path' | 'noun'
//! OrderMod     → 'ordering'+
//! PatternMod   → 'pattern'
//! ```
//!
//! The grammar is intentionally permissive — it accepts many phrasings
//! and relies on the Intent IR layer to validate semantics.

use crate::nl::earley::{Grammar, Rule, Symbol};

/// Non-terminal symbol helper.
fn nt(name: &str) -> Symbol {
    Symbol::NonTerminal(name.to_string())
}

/// Terminal symbol helper.
fn t(category: &str) -> Symbol {
    Symbol::Terminal(category.to_string())
}

/// Build the command grammar for the Earley parser.
///
/// This grammar is lexicon-driven: terminal symbols reference lexicon
/// categories (verb, noun, preposition, etc.), not specific words.
pub fn build_command_grammar() -> Grammar {
    let mut rules = Vec::new();

    // ── Command (top-level) ──────────────────────────────────────────
    // Command → Verb Object Modifiers   (highest weight: full command)
    rules.push(Rule::weighted("Command", vec![
        nt("Verb"), nt("Object"), nt("Modifiers"),
    ], 3.0));

    // Command → Verb Object             (common: "find comics")
    rules.push(Rule::weighted("Command", vec![
        nt("Verb"), nt("Object"),
    ], 2.0));

    // Command → Verb Modifiers          ("sort newest first")
    rules.push(Rule::weighted("Command", vec![
        nt("Verb"), nt("Modifiers"),
    ], 1.5));

    // Command → Verb                    (bare verb: "list")
    rules.push(Rule::weighted("Command", vec![
        nt("Verb"),
    ], 0.5));

    // Command → Verb FillerSeq Object Modifiers  ("zip up everything in downloads")
    rules.push(Rule::weighted("Command", vec![
        nt("Verb"), nt("FillerSeq"), nt("Object"), nt("Modifiers"),
    ], 2.8));

    // Command → Verb FillerSeq Object   ("zip up everything")
    rules.push(Rule::weighted("Command", vec![
        nt("Verb"), nt("FillerSeq"), nt("Object"),
    ], 1.8));

    // Command → Verb Object PatternMod Modifiers  ("find files *.pdf in downloads")
    rules.push(Rule::weighted("Command", vec![
        nt("Verb"), nt("Object"), nt("PatternMod"), nt("Modifiers"),
    ], 2.9));

    // Command → Verb Object PatternMod  ("find files *.pdf")
    rules.push(Rule::weighted("Command", vec![
        nt("Verb"), nt("Object"), nt("PatternMod"),
    ], 2.1));

    // Command → FillerSeq Verb Object Modifiers  ("please find comics in downloads")
    rules.push(Rule::weighted("Command", vec![
        nt("FillerSeq"), nt("Verb"), nt("Object"), nt("Modifiers"),
    ], 2.5));

    // Command → FillerSeq Verb Object   ("please find comics")
    rules.push(Rule::weighted("Command", vec![
        nt("FillerSeq"), nt("Verb"), nt("Object"),
    ], 1.5));

    // ── Verb ─────────────────────────────────────────────────────────
    rules.push(Rule::new("Verb", vec![t("verb")]));

    // ── Object ───────────────────────────────────────────────────────
    rules.push(Rule::weighted("Object", vec![nt("NounPhrase")], 1.0));
    rules.push(Rule::weighted("Object", vec![t("path")], 1.2));
    rules.push(Rule::weighted("Object", vec![t("pattern")], 0.8));

    // ── NounPhrase ───────────────────────────────────────────────────
    // NounPhrase → 'noun'
    rules.push(Rule::new("NounPhrase", vec![t("noun")]));
    // NounPhrase → 'determiner' 'noun'
    rules.push(Rule::weighted("NounPhrase", vec![t("determiner"), t("noun")], 1.1));
    // NounPhrase → 'quantifier' 'noun'
    rules.push(Rule::weighted("NounPhrase", vec![t("quantifier"), t("noun")], 1.1));
    // NounPhrase → 'possessive' 'noun'
    rules.push(Rule::weighted("NounPhrase", vec![t("possessive"), t("noun")], 1.1));
    // NounPhrase → 'determiner' 'noun' 'noun'  ("the comic books" — bigram nouns)
    rules.push(Rule::weighted("NounPhrase", vec![t("determiner"), t("noun"), t("noun")], 1.2));
    // NounPhrase → 'noun' 'noun'  ("comic books" — bigram nouns without determiner)
    rules.push(Rule::weighted("NounPhrase", vec![t("noun"), t("noun")], 1.0));

    // ── Modifiers (one or more) ──────────────────────────────────────
    rules.push(Rule::new("Modifiers", vec![nt("Modifier")]));
    rules.push(Rule::weighted("Modifiers", vec![
        nt("Modifier"), nt("Modifiers"),
    ], 1.1));

    // ── Modifier variants ────────────────────────────────────────────
    rules.push(Rule::new("Modifier", vec![nt("LocationMod")]));
    rules.push(Rule::new("Modifier", vec![nt("OrderMod")]));
    rules.push(Rule::new("Modifier", vec![nt("PatternMod")]));

    // ── LocationMod ──────────────────────────────────────────────────
    // "in downloads"
    rules.push(Rule::weighted("LocationMod", vec![
        t("preposition"), nt("PathExpr"),
    ], 2.0));
    // "in my downloads"
    rules.push(Rule::weighted("LocationMod", vec![
        t("preposition"), t("possessive"), nt("PathExpr"),
    ], 2.5));
    // "in my downloads folder" (noun suffix like "folder", "directory")
    rules.push(Rule::weighted("LocationMod", vec![
        t("preposition"), t("possessive"), nt("PathExpr"), t("noun"),
    ], 2.6));
    // "in my downloads folder" (filler suffix)
    rules.push(Rule::weighted("LocationMod", vec![
        t("preposition"), t("possessive"), nt("PathExpr"), t("filler"),
    ], 2.4));
    // "in downloads folder"
    rules.push(Rule::weighted("LocationMod", vec![
        t("preposition"), nt("PathExpr"), t("noun"),
    ], 2.1));
    // "in downloads folder" (filler suffix)
    rules.push(Rule::weighted("LocationMod", vec![
        t("preposition"), nt("PathExpr"), t("filler"),
    ], 2.0));
    // "at ~/path"
    rules.push(Rule::weighted("LocationMod", vec![
        t("preposition"), t("path"),
    ], 2.5));

    // ── PathExpr ─────────────────────────────────────────────────────
    rules.push(Rule::weighted("PathExpr", vec![t("path_noun")], 2.0));
    rules.push(Rule::weighted("PathExpr", vec![t("path")], 2.0));
    // Fallback: a bare noun as path (e.g., "documents" when not in path_nouns)
    rules.push(Rule::weighted("PathExpr", vec![t("noun")], 0.5));

    // ── OrderMod ─────────────────────────────────────────────────────
    // Single ordering word: "newest", "alphabetically"
    rules.push(Rule::new("OrderMod", vec![t("ordering")]));
    // Two-word ordering: "newest first", "largest first"
    rules.push(Rule::weighted("OrderMod", vec![
        t("ordering"), t("ordering"),
    ], 1.5));
    // "by name", "by size", "by date"
    rules.push(Rule::weighted("OrderMod", vec![
        t("preposition"), t("ordering"),
    ], 1.2));

    // ── PatternMod ───────────────────────────────────────────────────
    rules.push(Rule::new("PatternMod", vec![t("pattern")]));

    // ── FillerSeq (one or more filler words) ─────────────────────────
    rules.push(Rule::new("FillerSeq", vec![t("filler")]));
    rules.push(Rule::new("FillerSeq", vec![t("filler"), nt("FillerSeq")]));

    // ── Tail (absorbs any remaining tokens after a verb) ─────────────
    // Used for algorithm descriptions like "fibonacci f(n) where f(0)=0..."
    // where only the verb matters and the rest is noise.
    // Low weight so structured parses (Verb Object Modifiers) are preferred.
    rules.push(Rule::new("Tail", vec![Symbol::AnyToken]));
    rules.push(Rule::new("Tail", vec![Symbol::AnyToken, nt("Tail")]));

    // Command → Verb Tail  (verb + noise: "fibonacci f(n) where...")
    rules.push(Rule::weighted("Command", vec![
        nt("Verb"), nt("Tail"),
    ], 0.1));
    // Command → FillerSeq Verb Tail  ("please compute fibonacci f(n)...")
    rules.push(Rule::weighted("Command", vec![
        nt("FillerSeq"), nt("Verb"), nt("Tail"),
    ], 0.1));

    Grammar::new("Command", rules)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::nl::earley;
    use crate::nl::lexicon::lexicon;

    fn parse_input(input: &str) -> Vec<earley::RankedParse> {
        let grammar = build_command_grammar();
        let lex = lexicon();
        let tokens: Vec<String> = input.split_whitespace().map(|s| s.to_string()).collect();
        earley::parse(&grammar, &tokens, lex)
    }

    #[test]
    fn test_find_comics() {
        let parses = parse_input("find comics");
        assert!(!parses.is_empty(), "should parse 'find comics'");
        let tree = &parses[0].tree;
        assert_eq!(tree.label(), "Command");
        let verb = tree.find("Verb").expect("should have Verb");
        assert_eq!(verb.children()[0].token(), Some("find"));
    }

    #[test]
    fn test_find_comics_in_downloads() {
        let parses = parse_input("find comics in downloads");
        assert!(!parses.is_empty(), "should parse 'find comics in downloads'");
        let tree = &parses[0].tree;
        let loc = tree.find("LocationMod").expect("should have LocationMod");
        let path_tokens: Vec<&str> = loc.leaf_tokens();
        assert!(path_tokens.contains(&"downloads"), "location should contain downloads: {:?}", path_tokens);
    }

    #[test]
    fn test_find_comics_in_my_downloads() {
        let parses = parse_input("find comics in my downloads");
        assert!(!parses.is_empty(), "should parse 'find comics in my downloads'");
        let tree = &parses[0].tree;
        let loc = tree.find("LocationMod").expect("should have LocationMod");
        let tokens: Vec<&str> = loc.leaf_tokens();
        assert!(tokens.contains(&"my"), "should contain possessive 'my': {:?}", tokens);
        assert!(tokens.contains(&"downloads"), "should contain 'downloads': {:?}", tokens);
    }

    #[test]
    fn test_find_comics_in_my_downloads_folder_newest_first() {
        let parses = parse_input("find comics in my downloads folder newest first");
        assert!(!parses.is_empty(),
            "should parse 'find comics in my downloads folder newest first'");
        let tree = &parses[0].tree;

        // Should have verb
        let verb = tree.find("Verb").expect("should have Verb");
        assert_eq!(verb.children()[0].token(), Some("find"));

        // Should have object (comics)
        let obj = tree.find("Object").expect("should have Object");
        let obj_tokens: Vec<&str> = obj.leaf_tokens();
        assert!(obj_tokens.contains(&"comics"), "object should contain comics: {:?}", obj_tokens);

        // Should have location modifier
        let loc = tree.find("LocationMod").expect("should have LocationMod");
        let loc_tokens: Vec<&str> = loc.leaf_tokens();
        assert!(loc_tokens.contains(&"downloads"), "location should contain downloads: {:?}", loc_tokens);

        // Should have order modifier
        let order = tree.find("OrderMod").expect("should have OrderMod");
        let order_tokens: Vec<&str> = order.leaf_tokens();
        assert!(order_tokens.contains(&"newest"), "order should contain newest: {:?}", order_tokens);
    }

    #[test]
    fn test_list_bare_verb() {
        let parses = parse_input("list");
        assert!(!parses.is_empty(), "should parse bare verb 'list'");
        let tree = &parses[0].tree;
        assert_eq!(tree.label(), "Command");
    }

    #[test]
    fn test_sort_files_newest_first() {
        let parses = parse_input("sort files newest first");
        assert!(!parses.is_empty(), "should parse 'sort files newest first'");
        let tree = &parses[0].tree;
        let order = tree.find("OrderMod").expect("should have OrderMod");
        let tokens: Vec<&str> = order.leaf_tokens();
        assert!(tokens.contains(&"newest"), "order should contain newest: {:?}", tokens);
    }

    #[test]
    fn test_zip_up_everything_in_downloads() {
        let parses = parse_input("zip up everything in downloads");
        assert!(!parses.is_empty(), "should parse 'zip up everything in downloads'");
        let tree = &parses[0].tree;
        let verb = tree.find("Verb").expect("should have Verb");
        assert_eq!(verb.children()[0].token(), Some("zip"));
    }

    #[test]
    fn test_extract_path() {
        let parses = parse_input("extract ~/comic.cbz");
        assert!(!parses.is_empty(), "should parse 'extract ~/comic.cbz'");
        let tree = &parses[0].tree;
        let verb = tree.find("Verb").expect("should have Verb");
        assert_eq!(verb.children()[0].token(), Some("extract"));
    }

    #[test]
    fn test_find_with_glob_pattern() {
        let parses = parse_input("find *.pdf in documents");
        assert!(!parses.is_empty(), "should parse 'find *.pdf in documents'");
    }

    #[test]
    fn test_empty_input() {
        let parses = parse_input("");
        // split_whitespace on "" gives empty vec
        assert!(parses.is_empty(), "empty input should produce no parses");
    }

    #[test]
    fn test_gibberish() {
        let parses = parse_input("asdfghjkl qwerty");
        assert!(parses.is_empty(), "gibberish should produce no parses");
    }

    #[test]
    fn test_please_find_comics() {
        let parses = parse_input("please find comics");
        assert!(!parses.is_empty(), "should parse 'please find comics'");
        let tree = &parses[0].tree;
        let verb = tree.find("Verb").expect("should have Verb");
        assert_eq!(verb.children()[0].token(), Some("find"));
    }

    #[test]
    fn test_find_the_comics() {
        let parses = parse_input("find the comics");
        assert!(!parses.is_empty(), "should parse 'find the comics'");
        let tree = &parses[0].tree;
        let np = tree.find("NounPhrase").expect("should have NounPhrase");
        let tokens: Vec<&str> = np.leaf_tokens();
        assert!(tokens.contains(&"the"), "NP should contain 'the': {:?}", tokens);
        assert!(tokens.contains(&"comics"), "NP should contain 'comics': {:?}", tokens);
    }

    #[test]
    fn test_find_at_explicit_path() {
        let parses = parse_input("find comics at ~/Documents");
        assert!(!parses.is_empty(), "should parse 'find comics at ~/Documents'");
        let tree = &parses[0].tree;
        let loc = tree.find("LocationMod").expect("should have LocationMod");
        let tokens: Vec<&str> = loc.leaf_tokens();
        assert!(tokens.contains(&"~/Documents"), "location should contain path: {:?}", tokens);
    }

    #[test]
    fn test_multiple_parses_ranked() {
        // "find comics in downloads" could parse multiple ways
        let parses = parse_input("find comics in downloads");
        if parses.len() > 1 {
            assert!(parses[0].score >= parses[1].score,
                "parses should be ranked highest first: {} vs {}",
                parses[0].score, parses[1].score);
        }
    }
}
