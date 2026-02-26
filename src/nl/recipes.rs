// ---------------------------------------------------------------------------
// Command recipe lookup — pulls commands from existing CLI fact packs
// ---------------------------------------------------------------------------
//
// Each recipe is a (command_string, description) pair derived from the
// `base_command` + `submode_*` properties in `macos_cli.facts.yaml`.
// The lookup matches input tokens against keywords extracted from entity
// names, base commands, and submode names.

use std::collections::HashMap;
use std::sync::OnceLock;

use crate::fact_pack::FactPack;

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

/// A command recipe: a concrete shell command with a human description.
#[derive(Debug, Clone)]
pub struct Recipe {
    /// The full shell command string (e.g., "git cherry-pick <commit>")
    pub command: String,
    /// Whether this is a submode recipe (has flags beyond the base command)
    pub is_submode: bool,
    /// Human-readable description of what the command does
    pub description: String,
    /// Keywords for matching (lowercase, split on _ and -)
    pub keywords: Vec<String>,
    /// The base CLI command (e.g., "git", "ps", "find")
    pub base_command: String,
}

/// The recipe index: a flat list of recipes built from fact pack data.
#[derive(Debug)]
pub struct RecipeIndex {
    recipes: Vec<Recipe>,
}

// ---------------------------------------------------------------------------
// Singleton
// ---------------------------------------------------------------------------

static RECIPE_INDEX: OnceLock<RecipeIndex> = OnceLock::new();

const MACOS_CLI_FACTS_YAML: &str =
    include_str!("../../data/packs/facts/macos_cli.facts.yaml");

/// Get the global recipe index (built once from the CLI fact pack).
pub fn recipe_index() -> &'static RecipeIndex {
    RECIPE_INDEX.get_or_init(|| {
        let yaml = std::fs::read_to_string("data/packs/facts/macos_cli.facts.yaml")
            .unwrap_or_else(|_| MACOS_CLI_FACTS_YAML.to_string());
        let pack: FactPack = serde_yaml::from_str(&yaml)
            .expect("embedded macos_cli.facts.yaml should always parse");
        RecipeIndex::build(&pack)
    })
}

// ---------------------------------------------------------------------------
// Index construction
// ---------------------------------------------------------------------------

impl RecipeIndex {
    /// Build the recipe index from a CLI fact pack.
    pub fn build(pack: &FactPack) -> Self {
        let mut recipes = Vec::new();
        let task_keywords = build_task_keywords();

        // Group properties by entity
        let mut base_commands: HashMap<&str, &str> = HashMap::new();
        let mut submodes: HashMap<&str, Vec<(&str, &str)>> = HashMap::new();
        let mut entity_descriptions: HashMap<&str, &str> = HashMap::new();

        for entity in &pack.entities {
            entity_descriptions.insert(&entity.id, &entity.description);
        }

        for prop in &pack.properties {
            if prop.key == "base_command" {
                base_commands.insert(&prop.entity, &prop.value);
            } else if let Some(submode_name) = prop.key.strip_prefix("submode_") {
                submodes
                    .entry(&prop.entity)
                    .or_default()
                    .push((submode_name, &prop.value));
            }
        }

        // For each entity with a base_command, create recipes
        for (entity_id, base_cmd) in &base_commands {
            let desc = entity_descriptions
                .get(entity_id)
                .copied()
                .unwrap_or("");

            // Recipe for the base command itself
            let base_keywords = split_keywords(base_cmd);
            // Also add words from the entity name (strip cli_ prefix)
            let entity_name = entity_id.strip_prefix("cli_").unwrap_or(entity_id);
            let mut keywords = base_keywords;
            keywords.extend(split_keywords(entity_name));
            // Add content words from the entity description
            for word in split_description_keywords(desc) {
                keywords.push(word);
            }
            // Add task-oriented keywords for this command
            if let Some(task_kw) = task_keywords.get(base_cmd.to_lowercase().as_str()) {
                keywords.extend(task_kw.iter().map(|s| s.to_string()));
            }
            keywords.sort();
            keywords.dedup();

            recipes.push(Recipe {
                command: base_cmd.to_string(),
                is_submode: false,
                description: desc.to_string(),
                keywords,
                base_command: base_cmd.to_string(),
            });

            // Recipe for each submode
            if let Some(subs) = submodes.get(entity_id) {
                for (submode_name, flags) in subs {
                    let command = format!("{} {}", base_cmd, flags);
                    let submode_desc = format!(
                        "{} ({})",
                        desc,
                        submode_name.replace('_', " ")
                    );

                    let mut kw = split_keywords(base_cmd);
                    kw.extend(split_keywords(entity_name));
                    kw.extend(split_keywords(submode_name));
                    // Add content words from the entity description
                    for word in split_description_keywords(desc) {
                        kw.push(word);
                    }
                    // Add task-oriented keywords for this submode
                    if let Some(task_kw) = task_keywords.get(format!("{}_{}", base_cmd, submode_name).as_str()) {
                        kw.extend(task_kw.iter().map(|s| s.to_string()));
                    }
                    // Also add the raw flags as keywords (e.g., "aux", "-r")
                    for word in flags.split_whitespace() {
                        let clean = word.trim_start_matches('-');
                        if !clean.is_empty() && clean.len() > 1 {
                            kw.push(clean.to_lowercase());
                        }
                    }
                    kw.sort();
                    kw.dedup();

                    recipes.push(Recipe {
                        command,
                        is_submode: true,
                        description: submode_desc,
                        keywords: kw,
                        base_command: base_cmd.to_string(),
                    });
                }
            }
        }

        RecipeIndex { recipes }
    }

    /// Look up a recipe by matching input tokens against recipe keywords.
    /// Returns the best-matching recipe if the score exceeds the threshold.
    ///
    /// Scoring: count of input tokens that appear in the recipe's keywords,
    /// weighted by keyword specificity (longer keywords score higher).
    /// Minimum 2 keyword matches required to avoid false positives.
    pub fn lookup(&self, tokens: &[String]) -> Option<&Recipe> {
        let lower_tokens: Vec<String> = tokens.iter()
            .map(|t| t.to_lowercase())
            .collect();

        let mut best: Option<(f64, usize)> = None;

        for (i, recipe) in self.recipes.iter().enumerate() {
            let score = score_recipe(recipe, &lower_tokens);
            if score > 0.0 {
                if best.is_none() || score > best.unwrap().0 {
                    best = Some((score, i));
                }
            }
        }

        best.map(|(_, i)| &self.recipes[i])
    }

    /// Number of recipes in the index.
    pub fn len(&self) -> usize {
        self.recipes.len()
    }
}

// ---------------------------------------------------------------------------
// Scoring
// ---------------------------------------------------------------------------

/// Score a recipe against input tokens.
/// Returns 0.0 if fewer than 2 keywords match.
fn score_recipe(recipe: &Recipe, tokens: &[String]) -> f64 {
    let mut matched_tokens = 0;
    let mut score = 0.0;

    // Count distinct tokens that match at least one keyword
    for t in tokens {
        if recipe.keywords.iter().any(|kw| fuzzy_keyword_match(t, kw)) {
            matched_tokens += 1;
            score += 1.0 + (t.len() as f64 * 0.1);
        }
    }

    // Require at least 2 distinct token matches to avoid false positives
    if matched_tokens < 2 {
        return 0.0;
    }

    // Bonus for matching a higher fraction of the recipe's keywords
    let coverage = matched_tokens as f64 / recipe.keywords.len().max(1) as f64;
    let mut total = score * (1.0 + coverage);
    // Submode recipes are more specific → small bonus to prefer them over base
    if recipe.is_submode { total += 0.5; }
    total
}

/// Fuzzy keyword match: exact, contains, or stem-like match.
fn fuzzy_keyword_match(token: &str, keyword: &str) -> bool {
    if token == keyword {
        return true;
    }
    // Token contains keyword — only if keyword is substantial (>= 4 chars)
    // and the token isn't much longer (avoids "unzip" matching "zip")
    if keyword.len() >= 4 && token.contains(keyword) {
        return true;
    }
    // Keyword contains token (e.g., "processes" contains "process")
    if keyword.len() > token.len() && token.len() >= 3 && keyword.contains(token) {
        return true;
    }
    false
}

// ---------------------------------------------------------------------------
// Task-oriented keyword mapping
// ---------------------------------------------------------------------------

/// Build a mapping of CLI commands/submodes to task-oriented keywords.
/// These are common natural language words people use when asking about commands.
fn build_task_keywords() -> HashMap<&'static str, Vec<&'static str>> {
    let mut m = HashMap::new();
    // Base commands
    m.insert("ps", vec!["processes", "running", "list", "show"]);
    m.insert("du", vec!["disk", "usage", "space", "size"]);
    m.insert("df", vec!["disk", "free", "space", "filesystem"]);
    m.insert("tail", vec!["log", "follow", "watch", "monitor"]);
    m.insert("find", vec!["search", "locate", "files"]);
    m.insert("diff", vec!["compare", "differences", "directories", "folders"]);
    m.insert("ls", vec!["list", "files", "directory", "show"]);
    m.insert("cp", vec!["copy", "duplicate", "folder"]);
    m.insert("mv", vec!["move", "rename"]);
    m.insert("rm", vec!["remove", "delete"]);
    m.insert("zip", vec!["compress", "archive", "folder"]);
    m.insert("unzip", vec!["extract", "decompress", "archive"]);
    m.insert("tar", vec!["archive", "compress", "extract"]);
    m.insert("git", vec!["repository", "repo", "version", "control"]);
    m.insert("caffeinate", vec!["sleep", "sleeping", "prevent", "awake", "keep", "stop", "computer", "disable"]);
    m.insert("defaults", vec!["preferences", "settings", "hidden", "show"]);
    m.insert("fswatch", vec!["watch", "monitor", "changes", "filesystem"]);
    m.insert("grep", vec!["search", "pattern", "text", "find"]);
    m.insert("chmod", vec!["permissions", "access", "mode"]);
    m.insert("rsync", vec!["sync", "backup", "copy"]);
    m.insert("curl", vec!["download", "url", "http", "request"]);
    // Submodes (base_submode format)
    m.insert("ps_aux", vec!["all", "running", "processes", "list"]);
    m.insert("du_summary", vec!["disk", "usage", "total", "size"]);
    m.insert("du_human", vec!["disk", "usage", "readable", "human", "size"]);
    m.insert("tail_follow", vec!["follow", "log", "watch", "live", "stream"]);
    m.insert("find_by_size", vec!["large", "files", "size", "big"]);
    m.insert("find_by_mtime", vec!["modified", "recent", "recently", "changed", "today"]);
    m.insert("find_delete", vec!["delete", "remove", "clean"]);
    m.insert("diff_recursive", vec!["directories", "folders", "compare", "recursive"]);
    m.insert("ls_by_size", vec!["sorted", "size", "largest", "biggest"]);
    m.insert("cp_recursive", vec!["folder", "directory", "recursive", "copy"]);
    m.insert("git_reset", vec!["reset", "undo", "revert", "repo"]);
    m.insert("git_reset_soft", vec!["soft", "reset", "undo", "uncommit"]);
    m.insert("git_cherry_pick", vec!["cherry", "pick", "commit", "apply"]);
    m.insert("git_ls_files", vec!["tracked", "files", "list", "index"]);
    m.insert("git_log_graph", vec!["log", "graph", "history", "visual", "tree"]);
    m.insert("defaults_write", vec!["write", "set", "hidden", "show", "preference"]);
    m.insert("zip_create", vec!["create", "compress", "folder", "recursive"]);
    m.insert("unzip_extract", vec!["extract", "decompress", "archive"]);
    m.insert("caffeinate_idle", vec!["idle", "sleep", "sleeping", "prevent", "stop", "computer", "disable"]);
    m.insert("caffeinate_run_command", vec!["command", "run", "while", "awake"]);
    m
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Split a string into lowercase keyword tokens on `_`, `-`, and whitespace.
fn split_keywords(s: &str) -> Vec<String> {
    s.split(|c: char| c == '_' || c == '-' || c.is_whitespace())
        .filter(|w| !w.is_empty() && w.len() > 1)
        .map(|w| w.to_lowercase())
        .collect()
}

/// Stopwords to exclude from description keyword extraction.
const DESC_STOPWORDS: &[&str] = &[
    "a", "an", "the", "and", "or", "of", "to", "in", "on", "for",
    "from", "with", "by", "at", "is", "are", "was", "were", "be",
    "been", "being", "has", "have", "had", "do", "does", "did",
    "will", "would", "could", "should", "may", "might", "can",
    "not", "no", "its", "it", "this", "that", "these", "those",
    "via", "per",
];

/// Extract content keywords from a description string, filtering stopwords.
fn split_description_keywords(desc: &str) -> Vec<String> {
    desc.split(|c: char| !c.is_alphanumeric())
        .filter(|w| !w.is_empty() && w.len() > 1)
        .map(|w| w.to_lowercase())
        .filter(|w| !DESC_STOPWORDS.contains(&w.as_str()))
        .collect()
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn test_pack() -> FactPack {
        serde_yaml::from_str(MACOS_CLI_FACTS_YAML).unwrap()
    }

    #[test]
    fn test_recipe_index_builds() {
        let pack = test_pack();
        let idx = RecipeIndex::build(&pack);
        assert!(idx.len() > 100, "should have many recipes, got {}", idx.len());
    }

    #[test]
    fn test_lookup_git_cherry_pick() {
        let pack = test_pack();
        let idx = RecipeIndex::build(&pack);
        let tokens = vec!["git".into(), "cherry".into(), "pick".into()];
        let recipe = idx.lookup(&tokens);
        assert!(recipe.is_some(), "should find git cherry-pick");
        let r = recipe.unwrap();
        assert!(r.command.contains("git"), "command should contain git");
        assert!(r.command.contains("cherry-pick"), "command should contain cherry-pick");
    }

    #[test]
    fn test_lookup_git_reset() {
        let pack = test_pack();
        let idx = RecipeIndex::build(&pack);
        let tokens = vec!["git".into(), "reset".into()];
        let recipe = idx.lookup(&tokens);
        assert!(recipe.is_some(), "should find git reset");
        let r = recipe.unwrap();
        assert!(r.command.contains("git"), "command should contain git");
        assert!(r.command.contains("reset"), "command should contain reset");
    }

    #[test]
    fn test_lookup_caffeinate() {
        let pack = test_pack();
        let idx = RecipeIndex::build(&pack);
        let tokens = vec!["caffeinate".into(), "prevent".into(), "sleep".into()];
        let recipe = idx.lookup(&tokens);
        assert!(recipe.is_some(), "should find caffeinate");
        assert!(recipe.unwrap().command.contains("caffeinate"));
    }

    #[test]
    fn test_lookup_ps_aux() {
        let pack = test_pack();
        let idx = RecipeIndex::build(&pack);
        let tokens = vec!["ps".into(), "aux".into(), "processes".into()];
        let recipe = idx.lookup(&tokens);
        assert!(recipe.is_some(), "should find ps aux");
        let r = recipe.unwrap();
        assert!(r.command.contains("ps"), "command should contain ps");
        assert!(r.command.contains("aux"), "command should contain aux");
    }

    #[test]
    fn test_lookup_no_match() {
        let pack = test_pack();
        let idx = RecipeIndex::build(&pack);
        let tokens = vec!["make".into(), "pasta".into()];
        assert!(idx.lookup(&tokens).is_none(), "should not match pasta");
    }

    #[test]
    fn test_lookup_single_word_no_match() {
        let pack = test_pack();
        let idx = RecipeIndex::build(&pack);
        let tokens = vec!["command".into()];
        assert!(idx.lookup(&tokens).is_none(), "single word should not match");
    }

    #[test]
    fn test_lookup_find_delete() {
        let pack = test_pack();
        let idx = RecipeIndex::build(&pack);
        let tokens = vec!["find".into(), "delete".into()];
        let recipe = idx.lookup(&tokens);
        assert!(recipe.is_some(), "should find find -delete");
        let r = recipe.unwrap();
        assert!(r.command.contains("find"), "command should contain find");
        assert!(r.command.contains("-delete"), "command should contain -delete");
    }

    #[test]
    fn test_lookup_du_human() {
        let pack = test_pack();
        let idx = RecipeIndex::build(&pack);
        let tokens = vec!["du".into(), "disk".into(), "usage".into(), "human".into()];
        let recipe = idx.lookup(&tokens);
        assert!(recipe.is_some(), "should find du -h");
        let r = recipe.unwrap();
        assert!(r.command.contains("du"));
    }

    #[test]
    fn test_lookup_tail_follow() {
        let pack = test_pack();
        let idx = RecipeIndex::build(&pack);
        let tokens = vec!["tail".into(), "follow".into()];
        let recipe = idx.lookup(&tokens);
        assert!(recipe.is_some(), "should find tail -f");
        let r = recipe.unwrap();
        assert!(r.command.contains("tail"));
        assert!(r.command.contains("-f"));
    }

    #[test]
    fn test_lookup_ls_by_size() {
        let pack = test_pack();
        let idx = RecipeIndex::build(&pack);
        let tokens = vec!["ls".into(), "size".into(), "sort".into()];
        let recipe = idx.lookup(&tokens);
        assert!(recipe.is_some(), "should find ls -lS");
        let r = recipe.unwrap();
        assert!(r.command.contains("ls"));
    }

    #[test]
    fn test_lookup_diff_recursive() {
        let pack = test_pack();
        let idx = RecipeIndex::build(&pack);
        let tokens = vec!["diff".into(), "recursive".into()];
        let recipe = idx.lookup(&tokens);
        assert!(recipe.is_some(), "should find diff -r");
        let r = recipe.unwrap();
        assert!(r.command.contains("diff"));
        assert!(r.command.contains("-r"));
    }

    #[test]
    fn test_lookup_cp_recursive() {
        let pack = test_pack();
        let idx = RecipeIndex::build(&pack);
        let tokens = vec!["cp".into(), "recursive".into(), "copy".into()];
        let recipe = idx.lookup(&tokens);
        assert!(recipe.is_some(), "should find cp -r");
        let r = recipe.unwrap();
        assert!(r.command.contains("cp"));
    }

    #[test]
    fn test_split_keywords() {
        assert_eq!(split_keywords("cherry_pick"), vec!["cherry", "pick"]);
        assert_eq!(split_keywords("log --oneline"), vec!["log", "oneline"]);
        assert_eq!(split_keywords("git"), vec!["git"]);
    }
}
