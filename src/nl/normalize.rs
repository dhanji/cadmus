//! Text normalization for the NL UX layer.
//!
//! Pipeline: raw input → case fold → strip punctuation → expand contractions
//! → canonicalize ordinals → map synonym phrases to canonical op names.
//!
//! All operations are pure string transforms — no allocation-heavy structures,
//! no external dependencies. Designed for grep-like latency.

use std::collections::HashMap;

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
    // We need to preserve original case for paths, so we work with
    // the original input for path detection, then lowercase the rest.
    let lower = input.to_lowercase();
    let expanded = expand_contractions(&lower);

    // Build a map from lowercased words to their original-case equivalents
    // for path-like tokens. We split both original and lowered in parallel.
    let orig_words: Vec<&str> = input.split_whitespace().collect();

    let mut tokens = Vec::new();
    // The expanded form may have more words than original (contractions expand),
    // so we track position in the original separately.
    let mut orig_idx = 0;
    for word in expanded.split_whitespace() {
        let stripped = strip_punctuation(word);
        if stripped.is_empty() {
            continue;
        }

        // Check if this is a path-like token — if so, use original case
        let is_path = stripped.starts_with("~/")
            || stripped.starts_with('/')
            || stripped.starts_with('$')
            || stripped.starts_with('*')
            || stripped.contains("://");

        if is_path {
            // Find the corresponding original token
            let orig = find_original_path(&orig_words, &mut orig_idx, &stripped);
            tokens.push(orig);
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
    // Order matters: longer patterns first
    let contractions = [
        ("won't", "will not"),
        ("can't", "cannot"),
        ("couldn't", "could not"),
        ("shouldn't", "should not"),
        ("wouldn't", "would not"),
        ("didn't", "did not"),
        ("doesn't", "does not"),
        ("don't", "do not"),
        ("isn't", "is not"),
        ("aren't", "are not"),
        ("wasn't", "was not"),
        ("weren't", "were not"),
        ("hasn't", "has not"),
        ("haven't", "have not"),
        ("hadn't", "had not"),
        ("i'm", "i am"),
        ("i've", "i have"),
        ("i'll", "i will"),
        ("i'd", "i would"),
        ("you're", "you are"),
        ("you've", "you have"),
        ("you'll", "you will"),
        ("you'd", "you would"),
        ("he's", "he is"),
        ("she's", "she is"),
        ("it's", "it is"),
        ("we're", "we are"),
        ("we've", "we have"),
        ("we'll", "we will"),
        ("they're", "they are"),
        ("they've", "they have"),
        ("they'll", "they will"),
        ("that's", "that is"),
        ("there's", "there is"),
        ("here's", "here is"),
        ("what's", "what is"),
        ("who's", "who is"),
        ("let's", "let us"),
    ];
    for (contraction, expansion) in &contractions {
        result = result.replace(contraction, expansion);
    }
    result
}

/// Canonicalize ordinal words/suffixes to numeric form.
/// Returns Some(n) if the token is an ordinal, None otherwise.
fn canonicalize_ordinal(token: &str) -> Option<u32> {
    // Word ordinals
    match token {
        "first" | "1st" => Some(1),
        "second" | "2nd" => Some(2),
        "third" | "3rd" => Some(3),
        "fourth" | "4th" => Some(4),
        "fifth" | "5th" => Some(5),
        "sixth" | "6th" => Some(6),
        "seventh" | "7th" => Some(7),
        "eighth" | "8th" => Some(8),
        "ninth" | "9th" => Some(9),
        "tenth" | "10th" => Some(10),
        "eleventh" | "11th" => Some(11),
        "twelfth" | "12th" => Some(12),
        _ => {
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
fn build_synonym_table() -> Vec<(Vec<&'static str>, &'static str)> {
    // Sorted by phrase length (longest first) for greedy matching.
    // Each entry: (phrase_tokens, canonical_op_name)
    vec![
        // --- Multi-word phrases (3+ words) ---
        (vec!["zip", "up", "everything"], "pack_archive"),
        (vec!["zip", "up", "all"], "pack_archive"),
        (vec!["zip", "up", "files"], "pack_archive"),
        (vec!["compress", "all", "files"], "pack_archive"),
        (vec!["make", "a", "zip"], "pack_archive"),
        (vec!["make", "an", "archive"], "pack_archive"),
        (vec!["create", "a", "zip"], "pack_archive"),
        (vec!["create", "an", "archive"], "pack_archive"),
        (vec!["create", "a", "directory"], "create_dir"),
        (vec!["make", "a", "directory"], "create_dir"),
        (vec!["make", "a", "folder"], "create_dir"),
        (vec!["create", "a", "folder"], "create_dir"),
        (vec!["create", "a", "link"], "create_link"),
        (vec!["make", "a", "link"], "create_link"),
        (vec!["walk", "the", "directory"], "walk_tree"),
        (vec!["walk", "the", "tree"], "walk_tree"),
        (vec!["walk", "the", "dirtree"], "walk_tree"),
        (vec!["walk", "dir", "tree"], "walk_tree"),
        (vec!["walk", "directory", "tree"], "walk_tree"),
        (vec!["search", "for", "files"], "find_matching"),
        (vec!["search", "for", "text"], "search_content"),
        (vec!["search", "for", "content"], "search_content"),
        (vec!["look", "for", "files"], "find_matching"),
        (vec!["look", "for", "text"], "search_content"),
        (vec!["sort", "by", "name"], "sort_by"),
        (vec!["sort", "by", "date"], "sort_by"),
        (vec!["sort", "by", "size"], "sort_by"),
        (vec!["sort", "by", "time"], "sort_by"),
        (vec!["read", "the", "file"], "read_file"),
        (vec!["open", "the", "file"], "open_file"),
        (vec!["show", "the", "file"], "read_file"),
        (vec!["show", "me", "the"], "read_file"),
        (vec!["get", "file", "info"], "stat"),
        (vec!["get", "file", "size"], "get_size"),
        (vec!["get", "file", "type"], "get_file_type"),
        (vec!["file", "type", "of"], "get_file_type"),
        (vec!["check", "disk", "space"], "df_usage"),
        (vec!["check", "disk", "usage"], "du_size"),
        (vec!["disk", "usage", "of"], "du_size"),
        (vec!["list", "open", "files"], "lsof_open"),
        (vec!["list", "all", "processes"], "ps_list"),
        (vec!["kill", "the", "process"], "kill_process"),
        (vec!["git", "log", "search"], "git_log"),
        (vec!["git", "commit", "history"], "git_log"),
        (vec!["copy", "to", "clipboard"], "clipboard_copy"),
        (vec!["paste", "from", "clipboard"], "clipboard_paste"),
        (vec!["remove", "the", "quarantine"], "remove_quarantine"),

        // --- Two-word phrases ---
        (vec!["zip", "up"], "pack_archive"),
        (vec!["zip", "everything"], "pack_archive"),
        (vec!["zip", "files"], "pack_archive"),
        (vec!["unzip", "files"], "extract_archive"),
        (vec!["unzip", "archive"], "extract_archive"),
        (vec!["extract", "archive"], "extract_archive"),
        (vec!["extract", "files"], "extract_archive"),
        (vec!["extract", "images"], "extract_archive"),
        (vec!["uncompress", "files"], "extract_archive"),
        (vec!["decompress", "files"], "extract_archive"),
        (vec!["pack", "files"], "pack_archive"),
        (vec!["pack", "archive"], "pack_archive"),
        (vec!["compress", "files"], "pack_archive"),
        (vec!["archive", "files"], "pack_archive"),
        (vec!["list", "files"], "list_dir"),
        (vec!["list", "directory"], "list_dir"),
        (vec!["list", "folder"], "list_dir"),
        (vec!["list", "dir"], "list_dir"),
        (vec!["list", "contents"], "list_dir"),
        (vec!["show", "files"], "list_dir"),
        (vec!["show", "directory"], "list_dir"),
        (vec!["show", "contents"], "list_dir"),
        (vec!["read", "file"], "read_file"),
        (vec!["cat", "file"], "read_file"),
        (vec!["view", "file"], "read_file"),
        (vec!["open", "file"], "open_file"),
        (vec!["write", "file"], "write_file"),
        (vec!["save", "file"], "write_file"),
        (vec!["walk", "tree"], "walk_tree"),
        (vec!["walk", "directory"], "walk_tree"),
        (vec!["walk", "dir"], "walk_tree"),
        (vec!["walk", "recursively"], "walk_tree"),
        (vec!["find", "files"], "find_matching"),
        (vec!["find", "matching"], "find_matching"),
        (vec!["search", "files"], "find_matching"),
        (vec!["search", "content"], "search_content"),
        (vec!["search", "text"], "search_content"),
        (vec!["grep", "for"], "search_content"),
        (vec!["filter", "files"], "filter"),
        (vec!["filter", "by"], "filter"),
        (vec!["filter", "out"], "filter"),
        (vec!["sort", "files"], "sort_by"),
        (vec!["sort", "entries"], "sort_by"),
        (vec!["sort", "results"], "sort_by"),
        (vec!["rename", "file"], "rename"),
        (vec!["rename", "files"], "rename"),
        (vec!["move", "file"], "move_entry"),
        (vec!["move", "files"], "move_entry"),
        (vec!["move", "entry"], "move_entry"),
        (vec!["copy", "file"], "copy"),
        (vec!["copy", "files"], "copy"),
        (vec!["delete", "file"], "delete"),
        (vec!["delete", "files"], "delete"),
        (vec!["remove", "file"], "delete"),
        (vec!["remove", "files"], "delete"),
        (vec!["make", "dir"], "create_dir"),
        (vec!["make", "directory"], "create_dir"),
        (vec!["make", "folder"], "create_dir"),
        (vec!["create", "dir"], "create_dir"),
        (vec!["create", "directory"], "create_dir"),
        (vec!["create", "folder"], "create_dir"),
        (vec!["create", "link"], "create_link"),
        (vec!["make", "link"], "create_link"),
        (vec!["symlink", "to"], "create_link"),
        (vec!["file", "info"], "stat"),
        (vec!["file", "metadata"], "stat"),
        (vec!["file", "stats"], "stat"),
        (vec!["file", "size"], "get_size"),
        (vec!["file", "type"], "get_file_type"),
        (vec!["file", "permissions"], "get_permissions"),
        (vec!["set", "permissions"], "set_permissions"),
        (vec!["change", "permissions"], "set_permissions"),
        (vec!["change", "owner"], "set_owner"),
        (vec!["set", "owner"], "set_owner"),
        (vec!["find", "replace"], "replace"),
        (vec!["search", "replace"], "replace"),
        (vec!["head", "of"], "head"),
        (vec!["tail", "of"], "tail"),
        (vec!["first", "lines"], "head"),
        (vec!["last", "lines"], "tail"),
        (vec!["unique", "lines"], "unique"),
        (vec!["count", "lines"], "count"),
        (vec!["count", "files"], "count"),
        (vec!["diff", "files"], "diff"),
        (vec!["compare", "files"], "diff"),
        (vec!["compute", "checksum"], "checksum"),
        (vec!["file", "checksum"], "checksum"),
        (vec!["spotlight", "search"], "spotlight_search"),
        (vec!["spotlight", "find"], "spotlight_search"),
        (vec!["open", "with"], "open_with"),
        (vec!["reveal", "in"], "reveal"),
        (vec!["show", "in"], "reveal"),
        (vec!["read", "plist"], "read_plist"),
        (vec!["write", "plist"], "write_plist"),
        (vec!["download", "file"], "download"),
        (vec!["download", "from"], "download"),
        (vec!["upload", "file"], "upload"),
        (vec!["upload", "to"], "upload"),
        (vec!["sync", "files"], "sync"),
        (vec!["sync", "directories"], "sync"),
        // Git
        (vec!["git", "init"], "git_init"),
        (vec!["git", "clone"], "git_clone"),
        // Single-word git shortcuts — users often say "clone the repo"
        // without the "git" prefix. Two-word forms above take priority
        // via longest-match.
        (vec!["clone"], "git_clone"),
        (vec!["commit"], "git_commit"),
        (vec!["checkout"], "git_checkout"),
        (vec!["merge"], "git_merge"),
        (vec!["fetch"], "git_fetch"),
        (vec!["pull"], "git_pull"),
        (vec!["push"], "git_push"),
        (vec!["git", "add"], "git_add"),
        (vec!["git", "commit"], "git_commit"),
        (vec!["git", "log"], "git_log"),
        (vec!["git", "diff"], "git_diff"),
        (vec!["git", "branch"], "git_branch"),
        (vec!["git", "checkout"], "git_checkout"),
        (vec!["git", "merge"], "git_merge"),
        (vec!["git", "rebase"], "git_rebase"),
        (vec!["git", "stash"], "git_stash"),
        (vec!["git", "push"], "git_push"),
        (vec!["git", "pull"], "git_pull"),
        (vec!["git", "blame"], "git_blame"),
        (vec!["git", "bisect"], "git_bisect"),
        (vec!["git", "tag"], "git_tag"),
        (vec!["git", "status"], "git_status"),
        // Tmux/screen
        (vec!["tmux", "session"], "tmux_new_session"),
        (vec!["tmux", "attach"], "tmux_attach"),
        (vec!["tmux", "split"], "tmux_split"),
        (vec!["screen", "session"], "screen_new_session"),
        (vec!["screen", "attach"], "screen_attach"),
        // JSON/YAML/CSV
        (vec!["jq", "query"], "jq_query"),
        (vec!["jq", "filter"], "jq_filter_seq"),
        (vec!["jq", "transform"], "jq_transform"),
        (vec!["yq", "query"], "yq_query"),
        (vec!["yq", "convert"], "yq_convert"),
        (vec!["csv", "cut"], "csv_cut"),
        (vec!["csv", "join"], "csv_join"),
        (vec!["csv", "sort"], "csv_sort"),
        // Text processing
        (vec!["awk", "extract"], "awk_extract"),
        (vec!["awk", "aggregate"], "awk_aggregate"),
        (vec!["sed", "script"], "sed_script"),
        (vec!["cut", "fields"], "cut_fields"),
        // System
        (vec!["disk", "space"], "df_usage"),
        (vec!["disk", "usage"], "du_size"),
        (vec!["process", "list"], "ps_list"),
        (vec!["kill", "process"], "kill_process"),
        (vec!["open", "files"], "lsof_open"),
        (vec!["system", "info"], "uname_info"),
        // Network
        (vec!["ssh", "exec"], "ssh_exec"),
        (vec!["scp", "transfer"], "scp_transfer"),
        (vec!["wget", "download"], "wget_download"),
        (vec!["ping", "host"], "ping_host"),
        (vec!["dns", "lookup"], "dig_lookup"),
        // Compression
        (vec!["gzip", "compress"], "gzip_compress"),
        (vec!["gzip", "decompress"], "gzip_decompress"),
        (vec!["xz", "compress"], "xz_compress"),
        (vec!["base64", "encode"], "base64_encode"),
        (vec!["base64", "decode"], "base64_decode"),

        // --- Single-word synonyms ---
        (vec!["ls"], "list_dir"),
        (vec!["dir"], "list_dir"),
        (vec!["list"], "list_dir"),
        (vec!["cat"], "read_file"),
        (vec!["less"], "read_file"),
        (vec!["more"], "read_file"),
        (vec!["type"], "read_file"),
        (vec!["find"], "find_matching"),
        (vec!["grep"], "search_content"),
        (vec!["rg"], "search_content"),
        (vec!["ag"], "search_content"),
        (vec!["ack"], "search_content"),
        (vec!["mv"], "move_entry"),
        (vec!["cp"], "copy"),
        (vec!["rm"], "delete"),
        (vec!["del"], "delete"),
        (vec!["erase"], "delete"),
        (vec!["trash"], "delete"),
        (vec!["mkdir"], "create_dir"),
        (vec!["mkd"], "create_dir"),
        (vec!["ln"], "create_link"),
        (vec!["link"], "create_link"),
        (vec!["symlink"], "create_link"),
        (vec!["zip"], "pack_archive"),
        (vec!["unzip"], "extract_archive"),
        (vec!["tar"], "pack_archive"),
        (vec!["untar"], "extract_archive"),
        (vec!["extract"], "extract_archive"),
        (vec!["compress"], "gzip_compress"),
        (vec!["decompress"], "gzip_decompress"),
        (vec!["uncompress"], "gzip_decompress"),
        (vec!["pack"], "pack_archive"),
        (vec!["unpack"], "extract_archive"),
        (vec!["archive"], "pack_archive"),
        (vec!["walk"], "walk_tree"),
        (vec!["traverse"], "walk_tree"),
        (vec!["recurse"], "walk_tree"),
        (vec!["recursive"], "walk_tree"),
        (vec!["tree"], "walk_tree"),
        (vec!["sort"], "sort_by"),
        (vec!["order"], "sort_by"),
        (vec!["arrange"], "sort_by"),
        (vec!["filter"], "filter"),
        (vec!["sift"], "filter"),
        (vec!["narrow"], "filter"),
        (vec!["search"], "search_content"),
        (vec!["rename"], "rename"),
        (vec!["move"], "move_entry"),
        (vec!["copy"], "copy"),
        (vec!["duplicate"], "copy"),
        (vec!["delete"], "delete"),
        (vec!["remove"], "delete"),
        (vec!["stat"], "stat"),
        (vec!["info"], "stat"),
        (vec!["metadata"], "stat"),
        (vec!["head"], "head"),
        (vec!["tail"], "tail"),
        (vec!["unique"], "unique"),
        (vec!["uniq"], "unique"),
        (vec!["dedup"], "unique"),
        (vec!["deduplicate"], "unique"),
        (vec!["count"], "count"),
        (vec!["wc"], "count"),
        (vec!["diff"], "diff"),
        (vec!["checksum"], "checksum"),
        (vec!["md5"], "checksum"),
        (vec!["sha256"], "checksum"),
        (vec!["hash"], "openssl_hash"),
        (vec!["download"], "download"),
        (vec!["upload"], "upload"),
        (vec!["sync"], "sync"),
        (vec!["rsync"], "sync"),
        (vec!["reveal"], "reveal"),
        (vec!["spotlight"], "spotlight_search"),
        (vec!["mdfind"], "spotlight_search"),
        (vec!["replace"], "replace"),
        (vec!["sed"], "sed_script"),
        (vec!["awk"], "awk_extract"),
        (vec!["cut"], "cut_fields"),
        (vec!["paste"], "paste_merge"),
        (vec!["tee"], "tee_split"),
        (vec!["column"], "column_format"),
        (vec!["tr"], "tr_replace"),
        (vec!["ps"], "ps_list"),
        (vec!["kill"], "kill_process"),
        (vec!["pkill"], "pkill_pattern"),
        (vec!["lsof"], "lsof_open"),
        (vec!["df"], "df_usage"),
        (vec!["du"], "du_size"),
        (vec!["file"], "file_type_detect"),
        (vec!["uname"], "uname_info"),
        (vec!["uptime"], "uptime_info"),
        (vec!["ssh"], "ssh_exec"),
        (vec!["scp"], "scp_transfer"),
        (vec!["wget"], "wget_download"),
        (vec!["curl"], "wget_download"),
        (vec!["nc"], "nc_connect"),
        (vec!["netcat"], "nc_connect"),
        (vec!["ping"], "ping_host"),
        (vec!["dig"], "dig_lookup"),
        (vec!["nslookup"], "dig_lookup"),
        (vec!["gzip"], "gzip_compress"),
        (vec!["gunzip"], "gzip_decompress"),
        (vec!["xz"], "xz_compress"),
        (vec!["base64"], "base64_encode"),
        (vec!["openssl"], "openssl_hash"),
        (vec!["jq"], "jq_query"),
        (vec!["yq"], "yq_query"),
        (vec!["watch"], "watch_command"),
    ]
}

/// Apply synonym mapping to tokens using greedy longest-match.
///
/// Scans left-to-right. At each position, tries the longest synonym phrase
/// first. If a match is found, replaces the matched tokens with the canonical
/// op name and advances past them. Otherwise, keeps the token as-is.
fn apply_synonyms(tokens: &[String]) -> Vec<String> {
    let table = build_synonym_table();

    // Group by first token for fast lookup
    let mut by_first: HashMap<&str, Vec<(&[&str], &str)>> = HashMap::new();
    for (phrase, op) in &table {
        by_first
            .entry(phrase[0])
            .or_default()
            .push((phrase.as_slice(), op));
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
                let end = i + phrase.len();
                if end <= tokens.len() {
                    let window: Vec<&str> = tokens[i..end].iter().map(|s| s.as_str()).collect();
                    if window.as_slice() == *phrase {
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
pub fn is_canonical_op(token: &str) -> bool {
    CANONICAL_OPS.contains(&token)
}

/// All canonical operation names from fs_ops + power_tools_ops.
pub const CANONICAL_OPS: &[&str] = &[
    "awk_aggregate", "awk_extract", "base64_decode", "base64_encode",
    "checksum", "clipboard_copy", "clipboard_paste", "column_format",
    "concat_seq", "copy", "count", "create_dir", "create_link",
    "csv_cut", "csv_join", "csv_sort", "cut_fields", "delete",
    "df_usage", "diff", "dig_lookup", "download", "du_size",
    "extract_archive", "file_type_detect", "filter", "find_matching",
    "flatten_tree", "get_file_type", "get_mtime", "get_permissions",
    "get_size", "get_xattr", "git_add", "git_bisect", "git_blame",
    "git_branch", "git_checkout", "git_clone", "git_commit", "git_diff",
    "git_diff_commits", "git_init", "git_log", "git_log_range",
    "git_merge", "git_pull", "git_push", "git_rebase", "git_stash",
    "git_stash_pop", "git_status", "git_tag", "gzip_compress",
    "gzip_decompress", "head", "jq_filter_seq", "jq_query",
    "jq_transform", "kill_process", "list_dir", "lsof_open",
    "map_entries", "move_entry", "nc_connect", "open_file", "open_with",
    "openssl_hash", "pack_archive", "paste_merge", "ping_host",
    "pkill_pattern", "ps_list", "read_file", "read_plist",
    "remove_quarantine", "remove_xattr", "rename", "replace", "reveal",
    "scp_transfer", "screen_attach", "screen_new_session",
    "search_content", "sed_script", "set_owner", "set_permissions",
    "set_xattr", "sort_by", "spotlight_search", "ssh_exec", "stat",
    "sync", "tail", "tee_split", "tmux_attach", "tmux_new_session",
    "tmux_send_keys", "tmux_split", "tr_replace", "uname_info",
    "unique", "upload", "uptime_info", "walk_tree",
    "walk_tree_hierarchy", "watch_command", "wget_download",
    "write_file", "write_plist", "xz_compress", "yq_convert", "yq_query",
];

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
}
