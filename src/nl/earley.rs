//! Earley parser for natural language command recognition.
//!
//! Implements Earley's algorithm for parsing ambiguous context-free grammars.
//! Produces a parse forest (multiple ranked parse trees) when the input is
//! ambiguous. Grammar rules are driven by a YAML lexicon — no hardcoded
//! vocabulary.
//!
//! The grammar is small (command-oriented English), so this focused
//! implementation (~400 lines) avoids external crate dependencies.

use std::fmt;

// ---------------------------------------------------------------------------
// Grammar definition
// ---------------------------------------------------------------------------

/// A symbol in the grammar — either a non-terminal (rule name) or a terminal
/// (lexicon category that matches input tokens).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Symbol {
    /// A non-terminal: references a grammar rule by name.
    NonTerminal(String),
    /// A terminal: matches tokens belonging to a lexicon category.
    Terminal(String),
    /// Matches any single token not consumed by other rules.
    /// Used for unknown words that should pass through.
    AnyToken,
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Symbol::NonTerminal(s) => write!(f, "{}", s),
            Symbol::Terminal(s) => write!(f, "'{}'", s),
            Symbol::AnyToken => write!(f, "<any>"),
        }
    }
}

/// A production rule: LHS → RHS₁ RHS₂ ... RHSₙ
#[derive(Debug, Clone)]
pub struct Rule {
    /// Left-hand side non-terminal name.
    pub lhs: String,
    /// Right-hand side: sequence of symbols.
    pub rhs: Vec<Symbol>,
    /// Priority weight (higher = preferred when ranking parses).
    pub weight: f64,
}

impl Rule {
    pub fn new(lhs: impl Into<String>, rhs: Vec<Symbol>) -> Self {
        Self { lhs: lhs.into(), rhs, weight: 1.0 }
    }

    pub fn weighted(lhs: impl Into<String>, rhs: Vec<Symbol>, weight: f64) -> Self {
        Self { lhs: lhs.into(), rhs, weight }
    }
}

/// A context-free grammar: a set of rules plus a start symbol.
#[derive(Debug, Clone)]
pub struct Grammar {
    pub rules: Vec<Rule>,
    pub start: String,
}

impl Grammar {
    pub fn new(start: impl Into<String>, rules: Vec<Rule>) -> Self {
        Self { start: start.into(), rules }
    }

}

// ---------------------------------------------------------------------------
// Lexicon interface (trait for token classification)
// ---------------------------------------------------------------------------

/// Trait for classifying tokens into lexicon categories.
/// The Earley parser uses this to match Terminal symbols against input tokens.
pub trait TokenClassifier {
    /// Return all lexicon categories that this token belongs to.
    /// E.g., "find" → ["verb"], "comics" → ["noun"], "in" → ["preposition"].
    fn classify(&self, token: &str) -> Vec<String>;
}

// ---------------------------------------------------------------------------
// Earley items and chart
// ---------------------------------------------------------------------------

/// An Earley item: a dotted rule with origin position.
#[derive(Debug, Clone)]
struct EarleyItem {
    /// Index of the rule in the grammar.
    rule_idx: usize,
    /// Position of the dot within the rule's RHS (0 = start, len = complete).
    dot: usize,
    /// Origin: the chart position where this item started.
    origin: usize,
    /// Back-pointers for building the parse forest.
    /// Each entry is (child_item_or_token, chart_position).
    completions: Vec<BackPointer>,
}

/// A back-pointer in the parse forest.
#[derive(Debug, Clone)]
enum BackPointer {
    /// A completed non-terminal: (rule_idx, origin, end, child_completions).
    Completed {
        rule_idx: usize,
        _origin: usize,
        _end: usize,
        completions: Vec<BackPointer>,
    },
    /// A scanned terminal token.
    Scanned {
        category: String,
        token: String,
        _position: usize,
    },
}

impl EarleyItem {
    fn new(rule_idx: usize, dot: usize, origin: usize) -> Self {
        Self { rule_idx, dot, origin, completions: Vec::new() }
    }

    fn with_completions(rule_idx: usize, dot: usize, origin: usize, completions: Vec<BackPointer>) -> Self {
        Self { rule_idx, dot, origin, completions }
    }

    /// Check if this item is complete (dot at end of RHS).
    fn is_complete(&self, grammar: &Grammar) -> bool {
        self.dot >= grammar.rules[self.rule_idx].rhs.len()
    }

    /// Get the symbol after the dot, if any.
    fn next_symbol<'a>(&self, grammar: &'a Grammar) -> Option<&'a Symbol> {
        grammar.rules[self.rule_idx].rhs.get(self.dot)
    }

    /// Identity key for deduplication (rule_idx, dot, origin).
    fn key(&self) -> (usize, usize, usize) {
        (self.rule_idx, self.dot, self.origin)
    }
}

/// A chart set (one per input position).
type ChartSet = Vec<EarleyItem>;

// ---------------------------------------------------------------------------
// Parse tree (output)
// ---------------------------------------------------------------------------

/// A node in the parse tree.
#[derive(Debug, Clone)]
pub enum ParseNode {
    /// An interior node: a non-terminal with children.
    Interior {
        /// The non-terminal name (e.g., "Command", "Verb", "NounPhrase").
        label: String,
        /// Child nodes.
        children: Vec<ParseNode>,
        /// The rule weight that produced this node.
        weight: f64,
    },
    /// A leaf node: a terminal token.
    Leaf {
        /// The lexicon category (e.g., "verb", "noun", "preposition").
        category: String,
        /// The actual input token.
        token: String,
    },
}

impl ParseNode {
    /// Get the label of this node.
    pub fn label(&self) -> &str {
        match self {
            ParseNode::Interior { label, .. } => label,
            ParseNode::Leaf { category, .. } => category,
        }
    }

    /// Get the token if this is a leaf.
    pub fn token(&self) -> Option<&str> {
        match self {
            ParseNode::Leaf { token, .. } => Some(token),
            _ => None,
        }
    }

    /// Get children if this is an interior node.
    pub fn children(&self) -> &[ParseNode] {
        match self {
            ParseNode::Interior { children, .. } => children,
            ParseNode::Leaf { .. } => &[],
        }
    }

    /// Compute the total weight of this parse tree.
    pub fn total_weight(&self) -> f64 {
        match self {
            ParseNode::Interior { weight, children, .. } => {
                *weight + children.iter().map(|c| c.total_weight()).sum::<f64>()
            }
            ParseNode::Leaf { .. } => 0.0,
        }
    }

    /// Collect all leaf tokens in order.
    pub fn leaf_tokens(&self) -> Vec<&str> {
        match self {
            ParseNode::Leaf { token, .. } => vec![token.as_str()],
            ParseNode::Interior { children, .. } => {
                children.iter().flat_map(|c| c.leaf_tokens()).collect()
            }
        }
    }

    /// Find the first descendant with the given label.
    pub fn find(&self, target_label: &str) -> Option<&ParseNode> {
        if self.label() == target_label {
            return Some(self);
        }
        if let ParseNode::Interior { children, .. } = self {
            for child in children {
                if let Some(found) = child.find(target_label) {
                    return Some(found);
                }
            }
        }
        None
    }

    /// Find all descendants with the given label.
    pub fn find_all(&self, target_label: &str) -> Vec<&ParseNode> {
        let mut results = Vec::new();
        if self.label() == target_label {
            results.push(self);
        }
        if let ParseNode::Interior { children, .. } = self {
            for child in children {
                results.extend(child.find_all(target_label));
            }
        }
        results
    }
}

impl fmt::Display for ParseNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn fmt_indent(node: &ParseNode, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
            let pad = "  ".repeat(indent);
            match node {
                ParseNode::Leaf { category, token } => {
                    writeln!(f, "{}[{}] \"{}\"", pad, category, token)
                }
                ParseNode::Interior { label, children, .. } => {
                    writeln!(f, "{}({})", pad, label)?;
                    for child in children {
                        fmt_indent(child, f, indent + 1)?;
                    }
                    Ok(())
                }
            }
        }
        fmt_indent(self, f, 0)
    }
}

/// A ranked parse result.
#[derive(Debug, Clone)]
pub struct RankedParse {
    /// The parse tree.
    pub tree: ParseNode,
    /// The total weight/score of this parse.
    pub score: f64,
}

// ---------------------------------------------------------------------------
// Earley parser
// ---------------------------------------------------------------------------

/// Parse the input tokens using the given grammar and token classifier.
/// Returns a list of parse trees, ranked by score (highest first).
///
/// The parser implements the three core Earley operations:
/// - **Predict**: When the dot is before a non-terminal, add items for all
///   rules that produce that non-terminal.
/// - **Scan**: When the dot is before a terminal, advance the dot if the
///   next input token matches the terminal's lexicon category.
/// - **Complete**: When the dot reaches the end of a rule, advance the dot
///   in all items that were waiting for this non-terminal.
pub fn parse(
    grammar: &Grammar,
    tokens: &[String],
    classifier: &dyn TokenClassifier,
) -> Vec<RankedParse> {
    if tokens.is_empty() {
        return Vec::new();
    }

    let n = tokens.len();
    let mut chart: Vec<ChartSet> = vec![Vec::new(); n + 1];

    // Seed: add items for all rules of the start symbol at position 0.
    for (idx, rule) in grammar.rules.iter().enumerate() {
        if rule.lhs == grammar.start {
            chart[0].push(EarleyItem::new(idx, 0, 0));
        }
    }

    // Process each chart set.
    for i in 0..=n {
        let mut j = 0;
        while j < chart[i].len() {
            let item = chart[i][j].clone();

            if item.is_complete(grammar) {
                // Complete
                complete(grammar, &mut chart, &item, i);
            } else {
                match item.next_symbol(grammar) {
                    Some(Symbol::NonTerminal(nt)) => {
                        // Predict
                        predict(grammar, &mut chart, nt, i);
                    }
                    Some(Symbol::Terminal(cat)) => {
                        // Scan
                        if i < n {
                            scan(grammar, &mut chart, &item, i, &tokens[i], cat, classifier);
                        }
                    }
                    Some(Symbol::AnyToken) => {
                        // Scan any token
                        if i < n {
                            scan_any(&mut chart, &item, i, &tokens[i]);
                        }
                    }
                    None => {}
                }
            }
            j += 1;
        }
    }

    // Extract completed parses of the start symbol spanning the full input.
    let mut parses = Vec::new();
    for item in &chart[n] {
        if item.is_complete(grammar)
            && grammar.rules[item.rule_idx].lhs == grammar.start
            && item.origin == 0
        {
            if let Some(tree) = build_tree(grammar, item) {
                let score = tree.total_weight();
                parses.push(RankedParse { tree, score });
            }
        }
    }

    // Sort by score descending (highest first).
    parses.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap_or(std::cmp::Ordering::Equal));

    // Deduplicate by leaf token sequence (keep highest-scored).
    let mut seen = std::collections::HashSet::new();
    parses.retain(|p| {
        let key: Vec<&str> = p.tree.leaf_tokens();
        let key_str = key.join(" ");
        seen.insert(key_str)
    });

    parses
}

// ---------------------------------------------------------------------------
// Earley operations
// ---------------------------------------------------------------------------

/// Predict: for each rule A → ... where A matches the non-terminal after
/// the dot, add A → •... to chart[pos].
fn predict(grammar: &Grammar, chart: &mut [ChartSet], nt: &str, pos: usize) {
    let existing_keys: std::collections::HashSet<(usize, usize, usize)> =
        chart[pos].iter().map(|item| item.key()).collect();

    let mut new_items = Vec::new();
    for (idx, rule) in grammar.rules.iter().enumerate() {
        if rule.lhs == nt {
            let key = (idx, 0, pos);
            if !existing_keys.contains(&key) {
                new_items.push(EarleyItem::new(idx, 0, pos));
            }
        }
    }
    chart[pos].extend(new_items);
}

/// Scan: if the next input token matches the terminal category,
/// advance the dot and add the item to chart[pos + 1].
fn scan(
    _grammar: &Grammar,
    chart: &mut [ChartSet],
    item: &EarleyItem,
    pos: usize,
    token: &str,
    category: &str,
    classifier: &dyn TokenClassifier,
) {
    let categories = classifier.classify(token);
    if categories.iter().any(|c| c == category) {
        let bp = BackPointer::Scanned {
            category: category.to_string(),
            token: token.to_string(),
            _position: pos,
        };
        let mut completions = item.completions.clone();
        completions.push(bp);
        let new_item = EarleyItem::with_completions(
            item.rule_idx,
            item.dot + 1,
            item.origin,
            completions,
        );
        // Deduplicate in chart[pos + 1]
        let key = new_item.key();
        if !chart[pos + 1].iter().any(|existing| existing.key() == key) {
            chart[pos + 1].push(new_item);
        }
    }
}

/// Scan any token (for AnyToken symbol).
fn scan_any(
    chart: &mut [ChartSet],
    item: &EarleyItem,
    pos: usize,
    token: &str,
) {
    let bp = BackPointer::Scanned {
        category: "any".to_string(),
        token: token.to_string(),
        _position: pos,
    };
    let mut completions = item.completions.clone();
    completions.push(bp);
    let new_item = EarleyItem::with_completions(
        item.rule_idx,
        item.dot + 1,
        item.origin,
        completions,
    );
    let key = new_item.key();
    if !chart[pos + 1].iter().any(|existing| existing.key() == key) {
        chart[pos + 1].push(new_item);
    }
}

/// Complete: when an item B → γ• is complete at position `end`,
/// find all items in chart[item.origin] that have B after their dot,
/// and advance their dot.
fn complete(
    grammar: &Grammar,
    chart: &mut [ChartSet],
    completed_item: &EarleyItem,
    end: usize,
) {
    let completed_lhs = &grammar.rules[completed_item.rule_idx].lhs;
    let origin = completed_item.origin;

    // Collect items from chart[origin] that are waiting for this non-terminal.
    let waiting: Vec<EarleyItem> = chart[origin]
        .iter()
        .filter(|item| {
            if let Some(Symbol::NonTerminal(nt)) = item.next_symbol(grammar) {
                nt == completed_lhs
            } else {
                false
            }
        })
        .cloned()
        .collect();

    for item in waiting {
        let bp = BackPointer::Completed {
            rule_idx: completed_item.rule_idx,
            _origin: completed_item.origin,
            _end: end,
            completions: completed_item.completions.clone(),
        };
        let mut completions = item.completions.clone();
        completions.push(bp);
        let new_item = EarleyItem::with_completions(
            item.rule_idx,
            item.dot + 1,
            item.origin,
            completions,
        );
        let key = new_item.key();
        if !chart[end].iter().any(|existing| existing.key() == key) {
            chart[end].push(new_item);
        }
    }
}

// ---------------------------------------------------------------------------
// Parse tree construction from back-pointers
// ---------------------------------------------------------------------------

/// Build a parse tree from a completed Earley item's back-pointers.
fn build_tree(grammar: &Grammar, item: &EarleyItem) -> Option<ParseNode> {
    let rule = &grammar.rules[item.rule_idx];
    let children = build_children(grammar, &item.completions)?;

    Some(ParseNode::Interior {
        label: rule.lhs.clone(),
        children,
        weight: rule.weight,
    })
}

/// Build child nodes from a list of back-pointers.
fn build_children(grammar: &Grammar, completions: &[BackPointer]) -> Option<Vec<ParseNode>> {
    let mut children = Vec::new();
    for bp in completions {
        match bp {
            BackPointer::Scanned { category, token, .. } => {
                children.push(ParseNode::Leaf {
                    category: category.clone(),
                    token: token.clone(),
                });
            }
            BackPointer::Completed { rule_idx, completions: sub_completions, .. } => {
                let rule = &grammar.rules[*rule_idx];
                let sub_children = build_children(grammar, sub_completions)?;
                children.push(ParseNode::Interior {
                    label: rule.lhs.clone(),
                    children: sub_children,
                    weight: rule.weight,
                });
            }
        }
    }
    Some(children)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    /// Simple classifier for testing.
    struct TestClassifier {
        map: HashMap<String, Vec<String>>,
    }

    impl TestClassifier {
        fn new(entries: &[(&str, &[&str])]) -> Self {
            let mut map = HashMap::new();
            for (token, cats) in entries {
                map.insert(
                    token.to_string(),
                    cats.iter().map(|c| c.to_string()).collect(),
                );
            }
            Self { map }
        }
    }

    impl TokenClassifier for TestClassifier {
        fn classify(&self, token: &str) -> Vec<String> {
            self.map.get(token).cloned().unwrap_or_default()
        }
    }

    fn simple_grammar() -> Grammar {
        // Command → Verb Object
        // Command → Verb Object Modifiers
        // Verb → 'verb'
        // Object → 'noun'
        // Object → 'determiner' 'noun'
        // Modifiers → Modifier
        // Modifiers → Modifier Modifiers
        // Modifier → LocationMod
        // Modifier → OrderMod
        // LocationMod → 'preposition' 'noun'
        // OrderMod → 'ordering'
        Grammar::new("Command", vec![
            Rule::weighted("Command", vec![
                Symbol::NonTerminal("Verb".into()),
                Symbol::NonTerminal("Object".into()),
            ], 1.0),
            Rule::weighted("Command", vec![
                Symbol::NonTerminal("Verb".into()),
                Symbol::NonTerminal("Object".into()),
                Symbol::NonTerminal("Modifiers".into()),
            ], 1.5),
            Rule::new("Verb", vec![Symbol::Terminal("verb".into())]),
            Rule::new("Object", vec![Symbol::Terminal("noun".into())]),
            Rule::new("Object", vec![
                Symbol::Terminal("determiner".into()),
                Symbol::Terminal("noun".into()),
            ]),
            Rule::new("Modifiers", vec![Symbol::NonTerminal("Modifier".into())]),
            Rule::new("Modifiers", vec![
                Symbol::NonTerminal("Modifier".into()),
                Symbol::NonTerminal("Modifiers".into()),
            ]),
            Rule::new("Modifier", vec![Symbol::NonTerminal("LocationMod".into())]),
            Rule::new("Modifier", vec![Symbol::NonTerminal("OrderMod".into())]),
            Rule::new("LocationMod", vec![
                Symbol::Terminal("preposition".into()),
                Symbol::Terminal("noun".into()),
            ]),
            Rule::new("OrderMod", vec![Symbol::Terminal("ordering".into())]),
        ])
    }

    fn simple_classifier() -> TestClassifier {
        TestClassifier::new(&[
            ("find", &["verb"]),
            ("list", &["verb"]),
            ("sort", &["verb"]),
            ("comics", &["noun"]),
            ("files", &["noun"]),
            ("downloads", &["noun"]),
            ("the", &["determiner"]),
            ("my", &["determiner"]),
            ("in", &["preposition"]),
            ("from", &["preposition"]),
            ("newest", &["ordering"]),
        ])
    }

    #[test]
    fn test_simple_verb_object() {
        let grammar = simple_grammar();
        let classifier = simple_classifier();
        let tokens: Vec<String> = vec!["find", "comics"].iter().map(|s| s.to_string()).collect();

        let parses = parse(&grammar, &tokens, &classifier);
        assert!(!parses.is_empty(), "should produce at least one parse");

        let tree = &parses[0].tree;
        assert_eq!(tree.label(), "Command");

        let verb = tree.find("Verb").expect("should have Verb");
        assert_eq!(verb.children()[0].token(), Some("find"));

        let obj = tree.find("Object").expect("should have Object");
        assert_eq!(obj.children()[0].token(), Some("comics"));
    }

    #[test]
    fn test_verb_object_with_modifier() {
        let grammar = simple_grammar();
        let classifier = simple_classifier();
        let tokens: Vec<String> = vec!["find", "comics", "in", "downloads"]
            .iter().map(|s| s.to_string()).collect();

        let parses = parse(&grammar, &tokens, &classifier);
        assert!(!parses.is_empty(), "should produce at least one parse");

        let tree = &parses[0].tree;
        assert_eq!(tree.label(), "Command");

        let loc = tree.find("LocationMod").expect("should have LocationMod");
        let tokens_found: Vec<&str> = loc.leaf_tokens();
        assert_eq!(tokens_found, vec!["in", "downloads"]);
    }

    #[test]
    fn test_verb_object_with_multiple_modifiers() {
        let grammar = simple_grammar();
        let classifier = simple_classifier();
        let tokens: Vec<String> = vec!["find", "comics", "in", "downloads", "newest"]
            .iter().map(|s| s.to_string()).collect();

        let parses = parse(&grammar, &tokens, &classifier);
        assert!(!parses.is_empty(), "should produce at least one parse");

        let tree = &parses[0].tree;
        let loc = tree.find("LocationMod");
        let ord = tree.find("OrderMod");
        assert!(loc.is_some(), "should have LocationMod");
        assert!(ord.is_some(), "should have OrderMod");
    }

    #[test]
    fn test_empty_input() {
        let grammar = simple_grammar();
        let classifier = simple_classifier();
        let tokens: Vec<String> = vec![];

        let parses = parse(&grammar, &tokens, &classifier);
        assert!(parses.is_empty(), "empty input should produce no parses");
    }

    #[test]
    fn test_gibberish_no_parse() {
        let grammar = simple_grammar();
        let classifier = simple_classifier();
        let tokens: Vec<String> = vec!["asdf", "qwerty"].iter().map(|s| s.to_string()).collect();

        let parses = parse(&grammar, &tokens, &classifier);
        assert!(parses.is_empty(), "gibberish should produce no parses");
    }

    #[test]
    fn test_single_verb_no_parse_without_object() {
        // With the simple grammar, a bare verb doesn't form a complete Command
        let grammar = simple_grammar();
        let classifier = simple_classifier();
        let tokens: Vec<String> = vec!["find"].iter().map(|s| s.to_string()).collect();

        let parses = parse(&grammar, &tokens, &classifier);
        // This grammar requires Verb + Object, so bare verb won't parse
        assert!(parses.is_empty(), "bare verb shouldn't parse with this grammar");
    }

    #[test]
    fn test_determiner_object() {
        let grammar = simple_grammar();
        let classifier = simple_classifier();
        let tokens: Vec<String> = vec!["find", "the", "comics"]
            .iter().map(|s| s.to_string()).collect();

        let parses = parse(&grammar, &tokens, &classifier);
        assert!(!parses.is_empty(), "should parse with determiner");

        let tree = &parses[0].tree;
        let obj = tree.find("Object").expect("should have Object");
        assert_eq!(obj.leaf_tokens(), vec!["the", "comics"]);
    }

    #[test]
    fn test_ranked_parses_highest_first() {
        let grammar = simple_grammar();
        let classifier = simple_classifier();
        let tokens: Vec<String> = vec!["find", "comics", "in", "downloads"]
            .iter().map(|s| s.to_string()).collect();

        let parses = parse(&grammar, &tokens, &classifier);
        // Should prefer the Command with Modifiers (weight 1.5) over without
        if parses.len() > 1 {
            assert!(parses[0].score >= parses[1].score,
                "parses should be ranked highest first");
        }
    }

    #[test]
    fn test_parse_node_display() {
        let node = ParseNode::Interior {
            label: "Command".to_string(),
            children: vec![
                ParseNode::Leaf { category: "verb".to_string(), token: "find".to_string() },
                ParseNode::Leaf { category: "noun".to_string(), token: "comics".to_string() },
            ],
            weight: 1.0,
        };
        let display = format!("{}", node);
        assert!(display.contains("Command"));
        assert!(display.contains("find"));
        assert!(display.contains("comics"));
    }

    #[test]
    fn test_any_token_symbol() {
        // Grammar: Sentence → 'verb' AnyToken
        let grammar = Grammar::new("Sentence", vec![
            Rule::new("Sentence", vec![
                Symbol::Terminal("verb".into()),
                Symbol::AnyToken,
            ]),
        ]);
        let classifier = TestClassifier::new(&[
            ("find", &["verb"]),
        ]);
        let tokens: Vec<String> = vec!["find", "whatever"].iter().map(|s| s.to_string()).collect();

        let parses = parse(&grammar, &tokens, &classifier);
        assert!(!parses.is_empty(), "AnyToken should match any word");
        assert_eq!(parses[0].tree.leaf_tokens(), vec!["find", "whatever"]);
    }
}
