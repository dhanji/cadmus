// ---------------------------------------------------------------------------
// Type Lowering — Phase 2 Subsumption
// ---------------------------------------------------------------------------
//
// Bridges the gap between the rich fs_ops type system and the flat
// shell-callable type system.
//
// Two-tier architecture:
//
//   Tier 1: Shell Bridges (world I/O)
//     fs_ops that touch the filesystem/network are *subsumed* by shell ops.
//     walk_tree → shell_find, list_dir → shell_ls, etc.
//     These lower rich types (Dir(Bytes), Seq(Entry(Name,a))) to flat
//     shell types (String, List(String)) and delegate to the existing
//     shell-op codegen infrastructure.
//
//   Tier 2: Racket-Native (intermediate logic)
//     fs_ops that do in-memory transformation map to Racket primitives.
//     filter → (filter pred lst), sort_by → (sort lst cmp), etc.
//     No shell subprocess needed — these are first-class Racket forms.
//
// The design principle: shell ops are "anchors to the world" with
// metasignatures that the inference engine uses to discover symmetric
// ops from the fact pack. Racket-native ops handle all decision and
// transformation logic between those anchors.

// ---------------------------------------------------------------------------
// Tier 1: Shell Bridge Subsumption Map
// ---------------------------------------------------------------------------

/// A subsumption entry mapping an fs_op to its shell-callable equivalent.
///
/// The shell op lives in the registry (discovered from racket_ops.yaml +
/// fact pack inference) and has flat types: String → List(String).
/// The fs_op has rich types: Dir(Bytes) → Seq(Entry(Name, Bytes)).
///
/// At codegen time, the executor looks up the subsumption, then delegates
/// to the shell-op codegen path (which reads `extract_shell_meta()` for
/// the base command and flags).
#[derive(Debug, Clone)]
pub struct SubsumptionEntry {
    /// The fs_ops.yaml operation name (e.g., "walk_tree")
    pub fs_op: &'static str,
    /// The shell-callable operation name (e.g., "shell_find")
    pub shell_op: &'static str,
    /// Number of String inputs the shell op expects (0, 1, or 2)
    pub arity: usize,
    /// Human-readable note about the mapping
    pub note: &'static str,
}

/// The static subsumption map: world-touching fs_ops + power_tools_ops → shell ops.
///
/// All world-touching ops from fs_ops.yaml and power_tools_ops.yaml that have
/// direct shell equivalents. Phase 3 migration: complete coverage.
/// The shell ops are anchors (or type-symmetric peers of anchors) in the
/// inference pipeline.
const SUBSUMPTION_MAP: &[SubsumptionEntry] = &[
    // =====================================================================
    // fs_ops.yaml — shell_text_lines class
    // =====================================================================
    SubsumptionEntry {
        fs_op: "list_dir",
        shell_op: "shell_ls",
        arity: 1,
        note: "ls — list directory contents",
    },
    SubsumptionEntry {
        fs_op: "read_file",
        shell_op: "shell_cat",
        arity: 1,
        note: "cat — read file contents",
    },
    SubsumptionEntry {
        fs_op: "head",
        shell_op: "shell_head",
        arity: 1,
        note: "head — first N lines of file",
    },
    SubsumptionEntry {
        fs_op: "tail",
        shell_op: "shell_tail",
        arity: 1,
        note: "tail — last N lines of file",
    },
    // --- shell_tree class ---
    SubsumptionEntry {
        fs_op: "walk_tree",
        shell_op: "shell_find",
        arity: 1,
        note: "find — recursively walk directory tree",
    },
    // --- shell_filtered_lines class ---
    SubsumptionEntry {
        fs_op: "search_content",
        shell_op: "shell_grep",
        arity: 2,
        note: "grep — search file contents for pattern",
    },
    // --- shell_single_value class ---
    SubsumptionEntry {
        fs_op: "get_size",
        shell_op: "shell_du",
        arity: 1,
        note: "du — estimate file space usage",
    },
    SubsumptionEntry {
        fs_op: "count",
        shell_op: "shell_wc",
        arity: 1,
        note: "wc — count lines/words/chars",
    },
    // --- shell_text_lines class (sort) ---
    SubsumptionEntry {
        fs_op: "sort_by",
        shell_op: "shell_sort",
        arity: 1,
        note: "sort — sort lines of text",
    },
    // --- shell_byte_stream class (curl) ---
    SubsumptionEntry {
        fs_op: "download",
        shell_op: "shell_curl",
        arity: 1,
        note: "curl — download URL contents",
    },
    // =====================================================================
    // fs_ops.yaml — formerly residual ops (now fully subsumed)
    // =====================================================================
    SubsumptionEntry { fs_op: "stat", shell_op: "shell_stat", arity: 1, note: "stat — file metadata" },
    SubsumptionEntry { fs_op: "walk_tree_hierarchy", shell_op: "shell_find", arity: 1, note: "find — hierarchy walk" },
    SubsumptionEntry { fs_op: "checksum", shell_op: "shell_shasum", arity: 1, note: "shasum — SHA hash" },
    SubsumptionEntry { fs_op: "du_size", shell_op: "shell_du", arity: 1, note: "du -sh — disk usage" },
    SubsumptionEntry { fs_op: "open_file", shell_op: "shell_open", arity: 1, note: "open — macOS open" },
    SubsumptionEntry { fs_op: "reveal", shell_op: "shell_open", arity: 1, note: "open -R — Finder reveal" },
    SubsumptionEntry { fs_op: "extract_archive", shell_op: "shell_tar", arity: 1, note: "tar xf — extract" },
    SubsumptionEntry { fs_op: "pack_archive", shell_op: "shell_tar", arity: 1, note: "tar cf — create archive" },
    SubsumptionEntry { fs_op: "write_file", shell_op: "shell_tee", arity: 1, note: "tee — write to file" },
    SubsumptionEntry { fs_op: "copy", shell_op: "shell_cp", arity: 2, note: "cp — copy file/dir" },
    SubsumptionEntry { fs_op: "delete", shell_op: "shell_rm", arity: 1, note: "rm — delete file/dir" },
    SubsumptionEntry { fs_op: "rename", shell_op: "shell_mv", arity: 2, note: "mv — rename" },
    SubsumptionEntry { fs_op: "move_entry", shell_op: "shell_mv", arity: 2, note: "mv — move entry" },
    SubsumptionEntry { fs_op: "create_dir", shell_op: "shell_mkdir", arity: 1, note: "mkdir -p — create dir" },
    SubsumptionEntry { fs_op: "diff", shell_op: "shell_diff", arity: 2, note: "diff — compare files" },
    // =====================================================================
    // fs_ops.yaml — remaining uncovered ops
    // =====================================================================
    SubsumptionEntry { fs_op: "create_link", shell_op: "shell_ln", arity: 2, note: "ln -s — symbolic link" },
    SubsumptionEntry { fs_op: "set_permissions", shell_op: "shell_chmod", arity: 2, note: "chmod — set permissions" },
    SubsumptionEntry { fs_op: "set_owner", shell_op: "shell_chown", arity: 2, note: "chown — set owner" },
    SubsumptionEntry { fs_op: "replace", shell_op: "shell_sed", arity: 1, note: "sed s/// — replace content" },
    SubsumptionEntry { fs_op: "get_mtime", shell_op: "shell_stat", arity: 1, note: "stat -f %m — modification time" },
    SubsumptionEntry { fs_op: "get_permissions", shell_op: "shell_stat", arity: 1, note: "stat -f %p — permissions" },
    SubsumptionEntry { fs_op: "get_file_type", shell_op: "shell_file", arity: 1, note: "file — detect type" },
    SubsumptionEntry { fs_op: "spotlight_search", shell_op: "shell_mdfind", arity: 1, note: "mdfind — Spotlight search" },
    SubsumptionEntry { fs_op: "get_xattr", shell_op: "shell_xattr", arity: 2, note: "xattr -p — read xattr" },
    SubsumptionEntry { fs_op: "set_xattr", shell_op: "shell_xattr", arity: 2, note: "xattr -w — write xattr" },
    SubsumptionEntry { fs_op: "remove_xattr", shell_op: "shell_xattr", arity: 2, note: "xattr -d — remove xattr" },
    SubsumptionEntry { fs_op: "remove_quarantine", shell_op: "shell_xattr", arity: 1, note: "xattr -d quarantine" },
    SubsumptionEntry { fs_op: "open_with", shell_op: "shell_open", arity: 2, note: "open -a — open with app" },
    SubsumptionEntry { fs_op: "clipboard_copy", shell_op: "shell_pbcopy", arity: 1, note: "pbcopy — copy to clipboard" },
    SubsumptionEntry { fs_op: "clipboard_paste", shell_op: "shell_pbpaste", arity: 0, note: "pbpaste — paste from clipboard" },
    SubsumptionEntry { fs_op: "read_plist", shell_op: "shell_plutil", arity: 1, note: "plutil -p — read plist" },
    SubsumptionEntry { fs_op: "write_plist", shell_op: "shell_plutil", arity: 2, note: "plutil — write plist" },
    SubsumptionEntry { fs_op: "upload", shell_op: "shell_curl", arity: 2, note: "curl -T — upload" },
    SubsumptionEntry { fs_op: "sync", shell_op: "shell_rsync", arity: 2, note: "rsync -a — sync dir" },
    SubsumptionEntry { fs_op: "unique", shell_op: "shell_sort", arity: 1, note: "sort -u — deduplicate" },
    // =====================================================================
    // power_tools_ops.yaml — git
    // =====================================================================
    SubsumptionEntry { fs_op: "git_init", shell_op: "shell_git", arity: 1, note: "git init" },
    SubsumptionEntry { fs_op: "git_clone", shell_op: "shell_git", arity: 1, note: "git clone" },
    SubsumptionEntry { fs_op: "git_add", shell_op: "shell_git", arity: 1, note: "git add" },
    SubsumptionEntry { fs_op: "git_commit", shell_op: "shell_git", arity: 1, note: "git commit" },
    SubsumptionEntry { fs_op: "git_log", shell_op: "shell_git", arity: 1, note: "git log" },
    SubsumptionEntry { fs_op: "git_log_range", shell_op: "shell_git", arity: 1, note: "git log A..B" },
    SubsumptionEntry { fs_op: "git_diff", shell_op: "shell_git", arity: 1, note: "git diff" },
    SubsumptionEntry { fs_op: "git_diff_commits", shell_op: "shell_git", arity: 1, note: "git diff A B" },
    SubsumptionEntry { fs_op: "git_branch", shell_op: "shell_git", arity: 1, note: "git branch" },
    SubsumptionEntry { fs_op: "git_checkout", shell_op: "shell_git", arity: 1, note: "git checkout" },
    SubsumptionEntry { fs_op: "git_merge", shell_op: "shell_git", arity: 1, note: "git merge" },
    SubsumptionEntry { fs_op: "git_rebase", shell_op: "shell_git", arity: 1, note: "git rebase" },
    SubsumptionEntry { fs_op: "git_stash", shell_op: "shell_git", arity: 0, note: "git stash" },
    SubsumptionEntry { fs_op: "git_stash_pop", shell_op: "shell_git", arity: 0, note: "git stash pop" },
    SubsumptionEntry { fs_op: "git_push", shell_op: "shell_git", arity: 1, note: "git push" },
    SubsumptionEntry { fs_op: "git_pull", shell_op: "shell_git", arity: 1, note: "git pull" },
    SubsumptionEntry { fs_op: "git_blame", shell_op: "shell_git", arity: 1, note: "git blame" },
    SubsumptionEntry { fs_op: "git_bisect", shell_op: "shell_git", arity: 1, note: "git bisect" },
    SubsumptionEntry { fs_op: "git_tag", shell_op: "shell_git", arity: 1, note: "git tag" },
    SubsumptionEntry { fs_op: "git_status", shell_op: "shell_git", arity: 1, note: "git status" },
    // =====================================================================
    // power_tools_ops.yaml — tmux / screen
    // =====================================================================
    SubsumptionEntry { fs_op: "tmux_new_session", shell_op: "shell_tmux", arity: 1, note: "tmux new-session" },
    SubsumptionEntry { fs_op: "tmux_attach", shell_op: "shell_tmux", arity: 1, note: "tmux attach" },
    SubsumptionEntry { fs_op: "tmux_split", shell_op: "shell_tmux", arity: 1, note: "tmux split-window" },
    SubsumptionEntry { fs_op: "tmux_send_keys", shell_op: "shell_tmux", arity: 2, note: "tmux send-keys" },
    SubsumptionEntry { fs_op: "screen_new_session", shell_op: "shell_screen", arity: 1, note: "screen -S" },
    SubsumptionEntry { fs_op: "screen_attach", shell_op: "shell_screen", arity: 1, note: "screen -r" },
    // =====================================================================
    // power_tools_ops.yaml — jq / yq / csv
    // =====================================================================
    SubsumptionEntry { fs_op: "jq_query", shell_op: "shell_jq", arity: 2, note: "jq filter file" },
    SubsumptionEntry { fs_op: "jq_filter_seq", shell_op: "shell_jq", arity: 2, note: "jq .[] filter" },
    SubsumptionEntry { fs_op: "jq_transform", shell_op: "shell_jq", arity: 2, note: "jq transform" },
    SubsumptionEntry { fs_op: "yq_query", shell_op: "shell_yq", arity: 2, note: "yq filter file" },
    SubsumptionEntry { fs_op: "yq_convert", shell_op: "shell_yq", arity: 1, note: "yq -o=json" },
    SubsumptionEntry { fs_op: "csv_cut", shell_op: "shell_cut", arity: 2, note: "cut -d, — CSV columns" },
    SubsumptionEntry { fs_op: "csv_join", shell_op: "shell_paste", arity: 2, note: "paste — join CSV" },
    SubsumptionEntry { fs_op: "csv_sort", shell_op: "shell_sort", arity: 1, note: "sort — CSV sort" },
    // =====================================================================
    // power_tools_ops.yaml — awk / sed / text processing
    // =====================================================================
    SubsumptionEntry { fs_op: "awk_extract", shell_op: "shell_awk", arity: 2, note: "awk extract fields" },
    SubsumptionEntry { fs_op: "awk_aggregate", shell_op: "shell_awk", arity: 2, note: "awk aggregate" },
    SubsumptionEntry { fs_op: "sed_script", shell_op: "shell_sed", arity: 2, note: "sed -f script" },
    SubsumptionEntry { fs_op: "cut_fields", shell_op: "shell_cut", arity: 2, note: "cut -d -f" },
    SubsumptionEntry { fs_op: "tr_replace", shell_op: "shell_tr", arity: 2, note: "tr SET1 SET2" },
    SubsumptionEntry { fs_op: "paste_merge", shell_op: "shell_paste", arity: 2, note: "paste -d" },
    SubsumptionEntry { fs_op: "tee_split", shell_op: "shell_tee", arity: 2, note: "tee — split output" },
    SubsumptionEntry { fs_op: "column_format", shell_op: "shell_column", arity: 1, note: "column -t" },
    // =====================================================================
    // power_tools_ops.yaml — process / system management
    // =====================================================================
    SubsumptionEntry { fs_op: "ps_list", shell_op: "shell_ps", arity: 0, note: "ps aux" },
    SubsumptionEntry { fs_op: "kill_process", shell_op: "shell_kill", arity: 2, note: "kill -SIG PID" },
    SubsumptionEntry { fs_op: "pkill_pattern", shell_op: "shell_pkill", arity: 1, note: "pkill pattern" },
    SubsumptionEntry { fs_op: "watch_command", shell_op: "shell_watch", arity: 2, note: "watch -n cmd" },
    SubsumptionEntry { fs_op: "df_usage", shell_op: "shell_df", arity: 0, note: "df -h" },
    SubsumptionEntry { fs_op: "lsof_open", shell_op: "shell_lsof", arity: 1, note: "lsof path" },
    SubsumptionEntry { fs_op: "file_type_detect", shell_op: "shell_file", arity: 1, note: "file --mime-type" },
    SubsumptionEntry { fs_op: "uname_info", shell_op: "shell_uname", arity: 0, note: "uname -a" },
    SubsumptionEntry { fs_op: "uptime_info", shell_op: "shell_uptime", arity: 0, note: "uptime" },
    // =====================================================================
    // power_tools_ops.yaml — networking
    // =====================================================================
    SubsumptionEntry { fs_op: "ssh_exec", shell_op: "shell_ssh", arity: 2, note: "ssh host cmd" },
    SubsumptionEntry { fs_op: "scp_transfer", shell_op: "shell_scp", arity: 2, note: "scp file host:path" },
    SubsumptionEntry { fs_op: "wget_download", shell_op: "shell_curl", arity: 1, note: "curl (wget equiv)" },
    SubsumptionEntry { fs_op: "nc_connect", shell_op: "shell_nc", arity: 2, note: "nc host port" },
    SubsumptionEntry { fs_op: "ping_host", shell_op: "shell_ping", arity: 2, note: "ping -c N host" },
    SubsumptionEntry { fs_op: "dig_lookup", shell_op: "shell_dig", arity: 2, note: "dig host TYPE" },
    // =====================================================================
    // power_tools_ops.yaml — compression / crypto
    // =====================================================================
    SubsumptionEntry { fs_op: "gzip_compress", shell_op: "shell_gzip", arity: 1, note: "gzip" },
    SubsumptionEntry { fs_op: "gzip_decompress", shell_op: "shell_gzip", arity: 1, note: "gunzip" },
    SubsumptionEntry { fs_op: "xz_compress", shell_op: "shell_xz", arity: 1, note: "xz" },
    SubsumptionEntry { fs_op: "base64_encode", shell_op: "shell_base64", arity: 1, note: "base64 encode" },
    SubsumptionEntry { fs_op: "base64_decode", shell_op: "shell_base64", arity: 1, note: "base64 -D decode" },
    SubsumptionEntry { fs_op: "openssl_hash", shell_op: "shell_openssl", arity: 2, note: "openssl dgst" },
];

/// Look up the subsumption entry for an fs_op name.
///
/// Returns `Some(&SubsumptionEntry)` if the op is a world-touching op
/// that is subsumed by a shell-callable form, `None` otherwise.
pub fn lookup_subsumption(fs_op: &str) -> Option<&'static SubsumptionEntry> {
    SUBSUMPTION_MAP.iter().find(|e| e.fs_op == fs_op)
}

/// Check if an fs_op is subsumed by a shell-callable form.
pub fn is_subsumed(op: &str) -> bool {
    SUBSUMPTION_MAP.iter().any(|e| e.fs_op == op)
}

/// Get all subsumption entries (for iteration/testing).
pub fn all_subsumptions() -> &'static [SubsumptionEntry] {
    SUBSUMPTION_MAP
}

// ---------------------------------------------------------------------------
// Tier 2: Racket-Native Map
// ---------------------------------------------------------------------------

/// A Racket-native mapping for an fs_op that does intermediate logic.
///
/// These ops operate on in-memory data (typically `List(String)` from a
/// prior shell bridge step) and map to Racket's own primitives.
#[derive(Debug, Clone)]
pub struct RacketNativeEntry {
    /// The fs_ops.yaml operation name (e.g., "filter")
    pub fs_op: &'static str,
    /// The kind of Racket-native expression to generate
    pub kind: RacketNativeKind,
    /// Human-readable note
    pub note: &'static str,
}

/// The kind of Racket-native expression to generate.
#[derive(Debug, Clone, PartialEq)]
pub enum RacketNativeKind {
    /// `(filter (lambda (line) (regexp-match? (regexp pat) line)) lst)`
    /// Requires a "pattern" or "extension" param.
    FilterPredicate,
    /// `(sort lst string<?)`
    SortComparator,
    /// `(remove-duplicates lst)`
    RemoveDuplicates,
    /// `(length lst)`
    Length,
    /// `(take lst n)`
    Take,
    /// `(take-right lst n)`
    TakeRight,
    /// `(flatten lst)` — identity in flat list context
    Flatten,
    /// `(append lst1 lst2)` — concatenate two lists
    Append,
    /// `(map f lst)` — transform each element
    Map,
}

/// The static Racket-native map: intermediate fs_ops → Racket primitives.
const RACKET_NATIVE_MAP: &[RacketNativeEntry] = &[
    RacketNativeEntry {
        fs_op: "filter",
        kind: RacketNativeKind::FilterPredicate,
        note: "(filter pred lst) with regexp-match?",
    },
    RacketNativeEntry {
        fs_op: "find_matching",
        kind: RacketNativeKind::FilterPredicate,
        note: "(filter pred lst) with regexp-match? on filename pattern",
    },
    RacketNativeEntry {
        fs_op: "unique",
        kind: RacketNativeKind::RemoveDuplicates,
        note: "(remove-duplicates lst)",
    },
    RacketNativeEntry {
        fs_op: "flatten_tree",
        kind: RacketNativeKind::Flatten,
        note: "identity — shell output is already flat",
    },
    RacketNativeEntry {
        fs_op: "concat_seq",
        kind: RacketNativeKind::Append,
        note: "(append lst1 lst2) — concatenate sequences",
    },
    RacketNativeEntry {
        fs_op: "map_entries",
        kind: RacketNativeKind::Map,
        note: "(map f lst) — transform entry values",
    },
];

/// Ops that have dual behavior: shell bridge when used as first step
/// (no prev_binding), Racket-native when used in a pipeline (has prev_binding).
///
/// These are looked up by the executor to decide which path to take.
const DUAL_BEHAVIOR_MAP: &[(&str, RacketNativeKind)] = &[
    ("head", RacketNativeKind::Take),
    ("tail", RacketNativeKind::TakeRight),
    ("sort_by", RacketNativeKind::SortComparator),
    ("count", RacketNativeKind::Length),
    ("unique", RacketNativeKind::RemoveDuplicates),
];

/// Look up the Racket-native entry for an fs_op name.
///
/// Returns `Some` if the op is an intermediate-logic op that should use
/// Racket primitives instead of shell commands.
pub fn lookup_racket_native(fs_op: &str) -> Option<&'static RacketNativeEntry> {
    RACKET_NATIVE_MAP.iter().find(|e| e.fs_op == fs_op)
}

/// Look up the dual-behavior Racket-native kind for an op that can be
/// either a shell bridge (first step) or Racket-native (in pipeline).
///
/// Returns `Some(kind)` if the op has dual behavior.
pub fn lookup_dual_behavior(fs_op: &str) -> Option<&'static RacketNativeKind> {
    DUAL_BEHAVIOR_MAP.iter()
        .find(|(op, _)| *op == fs_op)
        .map(|(_, kind)| kind)
}

/// Check if an fs_op has any type-lowering mapping (subsumption or native).
pub fn has_lowering(op: &str) -> bool {
    is_subsumed(op)
        || lookup_racket_native(op).is_some()
        || lookup_dual_behavior(op).is_some()
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // --- Tier 1: Subsumption Map ---

    #[test]
    fn test_lookup_walk_tree() {
        let entry = lookup_subsumption("walk_tree").unwrap();
        assert_eq!(entry.shell_op, "shell_find");
        assert_eq!(entry.arity, 1);
    }

    #[test]
    fn test_lookup_list_dir() {
        let entry = lookup_subsumption("list_dir").unwrap();
        assert_eq!(entry.shell_op, "shell_ls");
        assert_eq!(entry.arity, 1);
    }

    #[test]
    fn test_lookup_search_content_binary() {
        let entry = lookup_subsumption("search_content").unwrap();
        assert_eq!(entry.shell_op, "shell_grep");
        assert_eq!(entry.arity, 2);
    }

    #[test]
    fn test_lookup_read_file() {
        let entry = lookup_subsumption("read_file").unwrap();
        assert_eq!(entry.shell_op, "shell_cat");
        assert_eq!(entry.arity, 1);
    }

    #[test]
    fn test_lookup_download() {
        let entry = lookup_subsumption("download").unwrap();
        assert_eq!(entry.shell_op, "shell_curl");
        assert_eq!(entry.arity, 1);
    }

    #[test]
    fn test_rename_is_subsumed() {
        assert!(lookup_subsumption("rename").is_some());
    }

    #[test]
    fn test_filter_not_subsumed() {
        // filter is Racket-native, not a shell bridge
        assert!(lookup_subsumption("filter").is_none());
    }

    #[test]
    fn test_find_matching_not_subsumed() {
        assert!(lookup_subsumption("find_matching").is_none());
    }

    #[test]
    fn test_is_subsumed() {
        assert!(is_subsumed("walk_tree"));
        assert!(is_subsumed("list_dir"));
        assert!(is_subsumed("shell_find") == false);
        assert!(is_subsumed("filter") == false);
        assert!(is_subsumed("nonexistent") == false);
    }

    #[test]
    fn test_all_subsumption_entries_present() {
        let entries = all_subsumptions();
        // Phase 3: complete migration — all fs_ops + power_tools ops
        assert!(entries.len() >= 100, "expected at least 100 subsumption entries, got {}", entries.len());

        let fs_ops: Vec<&str> = entries.iter().map(|e| e.fs_op).collect();
        // Original 10
        assert!(fs_ops.contains(&"list_dir"));
        assert!(fs_ops.contains(&"walk_tree"));
        assert!(fs_ops.contains(&"search_content"));
        assert!(fs_ops.contains(&"head"));
        assert!(fs_ops.contains(&"tail"));
        assert!(fs_ops.contains(&"sort_by"));
        assert!(fs_ops.contains(&"count"));
        assert!(fs_ops.contains(&"get_size"));
        assert!(fs_ops.contains(&"download"));
        assert!(fs_ops.contains(&"read_file"));
        // Formerly residual
        assert!(fs_ops.contains(&"stat"));
        assert!(fs_ops.contains(&"copy"));
        assert!(fs_ops.contains(&"delete"));
        assert!(fs_ops.contains(&"diff"));
        // Power tools
        assert!(fs_ops.contains(&"git_log"));
        assert!(fs_ops.contains(&"git_commit"));
        assert!(fs_ops.contains(&"jq_query"));
        assert!(fs_ops.contains(&"tmux_new_session"));
        assert!(fs_ops.contains(&"ssh_exec"));
        assert!(fs_ops.contains(&"gzip_compress"));
    }

    #[test]
    fn test_all_shell_ops_valid_names() {
        for entry in all_subsumptions() {
            assert!(entry.shell_op.starts_with("shell_"),
                "shell_op '{}' should start with 'shell_'", entry.shell_op);
            assert!(entry.arity <= 2,
                "arity {} for {} seems too high", entry.arity, entry.fs_op);
        }
    }

    // --- Tier 2: Racket-Native Map ---

    #[test]
    fn test_lookup_filter_native() {
        let entry = lookup_racket_native("filter").unwrap();
        assert_eq!(entry.kind, RacketNativeKind::FilterPredicate);
    }

    #[test]
    fn test_lookup_find_matching_native() {
        let entry = lookup_racket_native("find_matching").unwrap();
        assert_eq!(entry.kind, RacketNativeKind::FilterPredicate);
    }

    #[test]
    fn test_lookup_unique_native() {
        let entry = lookup_racket_native("unique").unwrap();
        assert_eq!(entry.kind, RacketNativeKind::RemoveDuplicates);
    }

    #[test]
    fn test_walk_tree_not_native() {
        // walk_tree is a shell bridge, not Racket-native
        assert!(lookup_racket_native("walk_tree").is_none());
    }

    // --- Dual behavior ---

    #[test]
    fn test_head_dual_behavior() {
        let kind = lookup_dual_behavior("head").unwrap();
        assert_eq!(*kind, RacketNativeKind::Take);
    }

    #[test]
    fn test_tail_dual_behavior() {
        let kind = lookup_dual_behavior("tail").unwrap();
        assert_eq!(*kind, RacketNativeKind::TakeRight);
    }

    #[test]
    fn test_sort_by_dual_behavior() {
        let kind = lookup_dual_behavior("sort_by").unwrap();
        assert_eq!(*kind, RacketNativeKind::SortComparator);
    }

    #[test]
    fn test_count_dual_behavior() {
        let kind = lookup_dual_behavior("count").unwrap();
        assert_eq!(*kind, RacketNativeKind::Length);
    }

    #[test]
    fn test_walk_tree_no_dual() {
        assert!(lookup_dual_behavior("walk_tree").is_none());
    }

    // --- has_lowering ---

    #[test]
    fn test_has_lowering_subsumed() {
        assert!(has_lowering("walk_tree"));
        assert!(has_lowering("list_dir"));
    }

    #[test]
    fn test_has_lowering_native() {
        assert!(has_lowering("filter"));
        assert!(has_lowering("find_matching"));
    }

    #[test]
    fn test_has_lowering_dual() {
        assert!(has_lowering("head"));
        assert!(has_lowering("tail"));
        assert!(has_lowering("sort_by"));
        assert!(has_lowering("count"));
    }

    #[test]
    fn test_has_lowering_unknown() {
        assert!(!has_lowering("nonexistent"));
        assert!(!has_lowering("some_unknown_op"));
        assert!(!has_lowering(""));
    }
}
