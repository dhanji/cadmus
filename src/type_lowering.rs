use crate::registry::{AlgebraicProperties, OperationRegistry, PolyOpSignature};
use crate::type_expr::TypeExpr;
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
        || is_residual_fs_op(op)
}

// ---------------------------------------------------------------------------
// Residual fs_ops — world-touching ops not yet in the CLI fact pack
// ---------------------------------------------------------------------------
//
// These ops can be expressed as shell commands but don't have formal
// shell-callable ops in the registry yet. They use direct shell command
// strings rather than routing through the shell-op infrastructure.

/// A residual fs_op that maps directly to a shell command string.
#[derive(Debug, Clone)]
pub struct ResidualFsOp {
    /// The fs_ops.yaml operation name
    pub fs_op: &'static str,
    /// The shell command template (e.g., "stat", "open", "shasum -a 256")
    pub shell_cmd: &'static str,
    /// Whether this is an exec (no output capture) vs lines (capture output)
    pub is_exec: bool,
    /// Human-readable note
    pub note: &'static str,
}

/// Phase 3 migration complete: all residual ops are now fully subsumed.
/// This array is kept empty for backward compatibility with any code that
/// checks `is_residual_fs_op()`.
const RESIDUAL_FS_OPS: &[ResidualFsOp] = &[];

/// Check if an op is a residual fs_op (world-touching, not yet in CLI fact pack).
pub fn is_residual_fs_op(op: &str) -> bool {
    RESIDUAL_FS_OPS.iter().any(|e| e.fs_op == op)
}

/// Look up a residual fs_op.
pub fn lookup_residual(fs_op: &str) -> Option<&'static ResidualFsOp> {
    RESIDUAL_FS_OPS.iter().find(|e| e.fs_op == fs_op)
}

// ---------------------------------------------------------------------------
// Legacy op registration — replaces data/compat/*.yaml
// ---------------------------------------------------------------------------
//
// These functions programmatically register all 113 legacy op names
// (formerly defined in fs_ops.yaml and power_tools_ops.yaml) with their
// rich type signatures. This allows the workflow compiler to type-check
// workflows using old op names, while execution is routed through the
// subsumption map to shell-callable forms.

/// Helper: parse a type expression, panicking on failure (compile-time constant strings).
fn tp(s: &str) -> TypeExpr {
    TypeExpr::parse(s).unwrap_or_else(|e| panic!("bad type expr '{}': {}", s, e))
}

/// Register all legacy filesystem ops (formerly fs_ops.yaml) into the registry.
///
/// These are the 49 ops from the original filesystem operations pack.
/// Their rich type signatures enable the workflow compiler to do proper
/// type checking before the subsumption map routes execution to shell ops.
pub fn register_fs_legacy_ops(reg: &mut OperationRegistry) {
    // --- Directory & File I/O ---
    reg.register_poly(
        "list_dir",
        PolyOpSignature::new(vec!["a".into()], vec![tp("Dir(a)")], tp("Seq(Entry(Name, a))")),
        AlgebraicProperties::none(),
        "ls — list directory contents",
    );
    reg.register_poly(
        "read_file",
        PolyOpSignature::new(vec!["a".into()], vec![tp("File(a)")], tp("a")),
        AlgebraicProperties::none(),
        "cat — read file contents",
    );
    reg.register_poly(
        "write_file",
        PolyOpSignature::new(vec!["a".into()], vec![tp("a"), tp("Path")], tp("File(a)")),
        AlgebraicProperties::none(),
        "write content to file at path",
    );
    reg.register_poly(
        "stat",
        PolyOpSignature::mono(vec![tp("Path")], tp("Metadata")),
        AlgebraicProperties { idempotent: true, ..Default::default() },
        "stat — get file metadata",
    );
    reg.register_poly(
        "walk_tree",
        PolyOpSignature::new(vec!["a".into()], vec![tp("Dir(a)")], tp("Seq(Entry(Name, a))")),
        AlgebraicProperties::none(),
        "find — recursively walk directory tree (flattened)",
    );
    reg.register_poly(
        "walk_tree_hierarchy",
        PolyOpSignature::new(vec!["a".into()], vec![tp("Dir(a)")], tp("Tree(Entry(Name, a))")),
        AlgebraicProperties::none(),
        "find — recursively walk directory tree (preserving hierarchy)",
    );
    reg.register_poly(
        "flatten_tree",
        PolyOpSignature::new(vec!["a".into()], vec![tp("Tree(a)")], tp("Seq(a)")),
        AlgebraicProperties { idempotent: true, ..Default::default() },
        "flatten tree structure into flat sequence",
    );
    // --- Filtering & Sorting ---
    reg.register_poly(
        "filter",
        PolyOpSignature::new(vec!["a".into()], vec![tp("Seq(a)"), tp("Pattern")], tp("Seq(a)")),
        AlgebraicProperties { idempotent: true, ..Default::default() },
        "grep/filter — filter sequence by pattern",
    );
    reg.register_poly(
        "sort_by",
        PolyOpSignature::new(vec!["a".into()], vec![tp("Seq(a)")], tp("Seq(a)")),
        AlgebraicProperties { idempotent: true, ..Default::default() },
        "sort — sort sequence elements",
    );
    // --- Archive Operations ---
    reg.register_poly(
        "extract_archive",
        PolyOpSignature::new(vec!["a".into(), "fmt".into()], vec![tp("File(Archive(a, fmt))")], tp("Seq(Entry(Name, a))")),
        AlgebraicProperties::none(),
        "unzip/tar -x — extract archive contents",
    );
    reg.register_poly(
        "pack_archive",
        PolyOpSignature::new(vec!["a".into(), "fmt".into()], vec![tp("Seq(Entry(Name, a))"), tp("fmt")], tp("File(Archive(a, fmt))")),
        AlgebraicProperties::none(),
        "zip/tar -c — pack entries into archive",
    );
    // --- Sequence Operations ---
    reg.register_poly(
        "concat_seq",
        PolyOpSignature::new(vec!["a".into()], vec![tp("Seq(a)"), tp("Seq(a)")], tp("Seq(a)")),
        AlgebraicProperties { associative: true, identity: Some("[]".into()), ..Default::default() },
        "cat/concat — concatenate sequences (order-preserving)",
    );
    // --- Entry Operations ---
    reg.register_poly(
        "rename",
        PolyOpSignature::new(vec!["a".into()], vec![tp("Entry(Name, a)"), tp("Name")], tp("Entry(Name, a)")),
        AlgebraicProperties::none(),
        "mv — rename entry",
    );
    reg.register_poly(
        "move_entry",
        PolyOpSignature::new(vec!["a".into()], vec![tp("Entry(Name, a)"), tp("Path")], tp("Entry(Name, a)")),
        AlgebraicProperties::none(),
        "mv — move entry to new path",
    );
    // --- Search ---
    reg.register_poly(
        "search_content",
        PolyOpSignature::mono(vec![tp("Seq(Entry(Name, File(Text)))"), tp("Pattern")], tp("Seq(Match(Pattern, Line))")),
        AlgebraicProperties::none(),
        "grep — search file contents for pattern",
    );
    reg.register_poly(
        "find_matching",
        PolyOpSignature::new(vec!["a".into()], vec![tp("Pattern"), tp("Seq(Entry(Name, a))")], tp("Seq(Entry(Name, a))")),
        AlgebraicProperties { idempotent: true, ..Default::default() },
        "find -name — filter entries by name pattern",
    );
    // --- Transformation ---
    reg.register_poly(
        "map_entries",
        PolyOpSignature::new(vec!["a".into(), "b".into()], vec![tp("Seq(Entry(Name, a))")], tp("Seq(Entry(Name, b))")),
        AlgebraicProperties::none(),
        "xargs/map — transform entry values",
    );
    // --- Phase 1: File Lifecycle ---
    reg.register_poly(
        "copy",
        PolyOpSignature::new(vec!["a".into()], vec![tp("File(a)"), tp("Path")], tp("File(a)")),
        AlgebraicProperties { idempotent: true, ..Default::default() },
        "cp — copy file to destination path",
    );
    reg.register_poly(
        "delete",
        PolyOpSignature::new(vec!["a".into()], vec![tp("Entry(Name, a)")], tp("Unit")),
        AlgebraicProperties::none(),
        "rm — delete file or directory",
    );
    reg.register_poly(
        "create_dir",
        PolyOpSignature::mono(vec![tp("Path")], tp("Dir(Bytes)")),
        AlgebraicProperties { idempotent: true, ..Default::default() },
        "mkdir -p — create directory (and parents)",
    );
    reg.register_poly(
        "create_link",
        PolyOpSignature::mono(vec![tp("Path"), tp("Path")], tp("Entry(Name, Bytes)")),
        AlgebraicProperties::none(),
        "ln -s — create symbolic link (target, link_name)",
    );
    reg.register_poly(
        "set_permissions",
        PolyOpSignature::mono(vec![tp("Path"), tp("Permissions")], tp("Unit")),
        AlgebraicProperties { idempotent: true, ..Default::default() },
        "chmod — set file permissions",
    );
    reg.register_poly(
        "set_owner",
        PolyOpSignature::mono(vec![tp("Path"), tp("Owner")], tp("Unit")),
        AlgebraicProperties { idempotent: true, ..Default::default() },
        "chown — set file owner",
    );
    // --- Phase 2: Content Transformation ---
    reg.register_poly(
        "replace",
        PolyOpSignature::new(vec!["a".into()], vec![tp("File(a)"), tp("Pattern"), tp("Text")], tp("File(a)")),
        AlgebraicProperties::none(),
        "sed s/pattern/replacement/ — replace content in file",
    );
    reg.register_poly(
        "head",
        PolyOpSignature::mono(vec![tp("File(Text)"), tp("Count")], tp("File(Text)")),
        AlgebraicProperties { idempotent: true, ..Default::default() },
        "head -n — first N lines of file",
    );
    reg.register_poly(
        "tail",
        PolyOpSignature::mono(vec![tp("File(Text)"), tp("Count")], tp("File(Text)")),
        AlgebraicProperties { idempotent: true, ..Default::default() },
        "tail -n — last N lines of file",
    );
    reg.register_poly(
        "unique",
        PolyOpSignature::mono(vec![tp("Seq(Line)")], tp("Seq(Line)")),
        AlgebraicProperties { idempotent: true, ..Default::default() },
        "sort -u — deduplicate lines",
    );
    reg.register_poly(
        "count",
        PolyOpSignature::new(vec!["a".into()], vec![tp("Seq(a)")], tp("Count")),
        AlgebraicProperties::none(),
        "wc -l — count elements in sequence",
    );
    reg.register_poly(
        "diff",
        PolyOpSignature::mono(vec![tp("File(Text)"), tp("File(Text)")], tp("Diff")),
        AlgebraicProperties { commutative: false, ..Default::default() },
        "diff — compare two text files",
    );
    reg.register_poly(
        "checksum",
        PolyOpSignature::new(vec!["a".into()], vec![tp("File(a)")], tp("Hash")),
        AlgebraicProperties { idempotent: true, ..Default::default() },
        "shasum — compute file checksum",
    );
    // --- Phase 3: Metadata Accessors ---
    reg.register_poly(
        "get_size",
        PolyOpSignature::mono(vec![tp("Metadata")], tp("Size")),
        AlgebraicProperties { idempotent: true, ..Default::default() },
        "stat -f %z — extract file size from metadata",
    );
    reg.register_poly(
        "get_mtime",
        PolyOpSignature::mono(vec![tp("Metadata")], tp("Timestamp")),
        AlgebraicProperties { idempotent: true, ..Default::default() },
        "stat -f %m — extract modification time from metadata",
    );
    reg.register_poly(
        "get_permissions",
        PolyOpSignature::mono(vec![tp("Metadata")], tp("Permissions")),
        AlgebraicProperties { idempotent: true, ..Default::default() },
        "stat -f %p — extract permissions from metadata",
    );
    reg.register_poly(
        "get_file_type",
        PolyOpSignature::mono(vec![tp("Metadata")], tp("FileType")),
        AlgebraicProperties { idempotent: true, ..Default::default() },
        "stat -f %T — extract file type from metadata",
    );
    // --- Phase 4: macOS-Specific ---
    reg.register_poly("spotlight_search", PolyOpSignature::mono(vec![tp("Pattern")], tp("Seq(Entry(Name, Bytes))")), AlgebraicProperties::none(), "mdfind — Spotlight search");
    reg.register_poly("get_xattr", PolyOpSignature::mono(vec![tp("Path"), tp("XattrKey")], tp("XattrValue")), AlgebraicProperties { idempotent: true, ..Default::default() }, "xattr -p — read extended attribute");
    reg.register_poly("set_xattr", PolyOpSignature::mono(vec![tp("Path"), tp("XattrKey"), tp("XattrValue")], tp("Unit")), AlgebraicProperties { idempotent: true, ..Default::default() }, "xattr -w — set extended attribute");
    reg.register_poly("remove_xattr", PolyOpSignature::mono(vec![tp("Path"), tp("XattrKey")], tp("Unit")), AlgebraicProperties { idempotent: true, ..Default::default() }, "xattr -d — remove extended attribute");
    reg.register_poly("remove_quarantine", PolyOpSignature::mono(vec![tp("Path")], tp("Unit")), AlgebraicProperties { idempotent: true, ..Default::default() }, "xattr -d com.apple.quarantine");
    reg.register_poly("open_file", PolyOpSignature::mono(vec![tp("Path")], tp("Unit")), AlgebraicProperties::none(), "open — open file with default application");
    reg.register_poly("open_with", PolyOpSignature::mono(vec![tp("Path"), tp("App")], tp("Unit")), AlgebraicProperties::none(), "open -a — open with specific application");
    reg.register_poly("reveal", PolyOpSignature::mono(vec![tp("Path")], tp("Unit")), AlgebraicProperties::none(), "open -R — reveal file in Finder");
    reg.register_poly(
        "clipboard_copy",
        PolyOpSignature::new(vec!["a".into()], vec![tp("a")], tp("Unit")),
        AlgebraicProperties::none(),
        "pbcopy — copy content to clipboard",
    );
    reg.register_poly("clipboard_paste", PolyOpSignature::leaf(tp("Text")), AlgebraicProperties::none(), "pbpaste — paste from clipboard");
    reg.register_poly("read_plist", PolyOpSignature::mono(vec![tp("File(Plist)")], tp("Plist")), AlgebraicProperties { idempotent: true, ..Default::default() }, "plutil — read plist file");
    reg.register_poly("write_plist", PolyOpSignature::mono(vec![tp("Plist"), tp("Path")], tp("File(Plist)")), AlgebraicProperties::none(), "plutil — write plist file");
    // --- Phase 5: Network ---
    reg.register_poly("download", PolyOpSignature::mono(vec![tp("URL")], tp("File(Bytes)")), AlgebraicProperties::none(), "curl -O — download file from URL");
    reg.register_poly(
        "upload",
        PolyOpSignature::new(vec!["a".into()], vec![tp("File(a)"), tp("URL")], tp("Unit")),
        AlgebraicProperties::none(),
        "curl -T / scp — upload file to URL",
    );
    reg.register_poly(
        "sync",
        PolyOpSignature::new(vec!["a".into()], vec![tp("Dir(a)"), tp("Path")], tp("Dir(a)")),
        AlgebraicProperties { idempotent: true, ..Default::default() },
        "rsync -a — synchronize directory to destination",
    );
}

/// Register all legacy power tools ops (formerly power_tools_ops.yaml) into the registry.
///
/// These are the 64 ops from the power tools operations pack.
pub fn register_power_tools_legacy_ops(reg: &mut OperationRegistry) {
    // --- Git ---
    reg.register_poly("git_init", PolyOpSignature::mono(vec![tp("Path")], tp("Repo")), AlgebraicProperties::none(), "git init — initialize repository");
    reg.register_poly("git_clone", PolyOpSignature::mono(vec![tp("URL")], tp("Repo")), AlgebraicProperties::none(), "git clone — clone remote repository");
    reg.register_poly(
        "git_add",
        PolyOpSignature::new(vec!["a".into()], vec![tp("Repo"), tp("Seq(Entry(Name, a))")], tp("StagingArea")),
        AlgebraicProperties::none(),
        "git add — stage files for commit",
    );
    reg.register_poly("git_commit", PolyOpSignature::mono(vec![tp("Repo"), tp("StagingArea"), tp("Message")], tp("Commit")), AlgebraicProperties::none(), "git commit — create a commit");
    reg.register_poly("git_log", PolyOpSignature::mono(vec![tp("Repo")], tp("Seq(Commit)")), AlgebraicProperties { idempotent: true, ..Default::default() }, "git log — list commit history");
    reg.register_poly("git_log_range", PolyOpSignature::mono(vec![tp("Repo"), tp("CommitRef"), tp("CommitRef")], tp("Seq(Commit)")), AlgebraicProperties { idempotent: true, ..Default::default() }, "git log A..B — commits in range");
    reg.register_poly("git_diff", PolyOpSignature::mono(vec![tp("Repo")], tp("Diff")), AlgebraicProperties { idempotent: true, ..Default::default() }, "git diff — show unstaged changes");
    reg.register_poly("git_diff_commits", PolyOpSignature::mono(vec![tp("Commit"), tp("Commit")], tp("Diff")), AlgebraicProperties { idempotent: true, ..Default::default() }, "git diff A B");
    reg.register_poly("git_branch", PolyOpSignature::mono(vec![tp("Repo"), tp("Name")], tp("Branch")), AlgebraicProperties::none(), "git branch — create branch");
    reg.register_poly("git_checkout", PolyOpSignature::mono(vec![tp("Repo"), tp("Branch")], tp("Repo")), AlgebraicProperties::none(), "git checkout — switch branch");
    reg.register_poly("git_merge", PolyOpSignature::mono(vec![tp("Repo"), tp("Branch"), tp("MergeStrategy")], tp("MergeResult")), AlgebraicProperties::none(), "git merge");
    reg.register_poly("git_rebase", PolyOpSignature::mono(vec![tp("Repo"), tp("Branch")], tp("Repo")), AlgebraicProperties::none(), "git rebase");
    reg.register_poly("git_stash", PolyOpSignature::mono(vec![tp("Repo")], tp("Stash")), AlgebraicProperties::none(), "git stash");
    reg.register_poly("git_stash_pop", PolyOpSignature::mono(vec![tp("Repo"), tp("Stash")], tp("Repo")), AlgebraicProperties::none(), "git stash pop");
    reg.register_poly("git_push", PolyOpSignature::mono(vec![tp("Repo"), tp("Remote"), tp("Branch")], tp("Unit")), AlgebraicProperties::none(), "git push");
    reg.register_poly("git_pull", PolyOpSignature::mono(vec![tp("Repo"), tp("Remote"), tp("Branch")], tp("Repo")), AlgebraicProperties::none(), "git pull");
    reg.register_poly("git_blame", PolyOpSignature::mono(vec![tp("Repo"), tp("Path")], tp("Seq(BlameLine)")), AlgebraicProperties { idempotent: true, ..Default::default() }, "git blame");
    reg.register_poly("git_bisect", PolyOpSignature::mono(vec![tp("Repo"), tp("Commit"), tp("Commit")], tp("Commit")), AlgebraicProperties::none(), "git bisect");
    reg.register_poly("git_tag", PolyOpSignature::mono(vec![tp("Repo"), tp("Name"), tp("Commit")], tp("Tag")), AlgebraicProperties::none(), "git tag");
    reg.register_poly("git_status", PolyOpSignature::mono(vec![tp("Repo")], tp("GitStatus")), AlgebraicProperties { idempotent: true, ..Default::default() }, "git status");
    // --- Tmux / Screen ---
    reg.register_poly("tmux_new_session", PolyOpSignature::mono(vec![tp("Name")], tp("Session")), AlgebraicProperties::none(), "tmux new-session -s");
    reg.register_poly("tmux_attach", PolyOpSignature::mono(vec![tp("Name")], tp("Session")), AlgebraicProperties::none(), "tmux attach -t");
    reg.register_poly("tmux_split", PolyOpSignature::mono(vec![tp("Session"), tp("SplitDirection")], tp("Pane")), AlgebraicProperties::none(), "tmux split-window");
    reg.register_poly("tmux_send_keys", PolyOpSignature::mono(vec![tp("Pane"), tp("Text")], tp("Unit")), AlgebraicProperties::none(), "tmux send-keys");
    reg.register_poly("screen_new_session", PolyOpSignature::mono(vec![tp("Name")], tp("Session")), AlgebraicProperties::none(), "screen -S");
    reg.register_poly("screen_attach", PolyOpSignature::mono(vec![tp("Name")], tp("Session")), AlgebraicProperties::none(), "screen -r");
    // --- Structured Data (jq/yq/csv) ---
    reg.register_poly("jq_query", PolyOpSignature::mono(vec![tp("File(Json)"), tp("JqFilter")], tp("Json")), AlgebraicProperties { idempotent: true, ..Default::default() }, "jq — query JSON");
    reg.register_poly("jq_filter_seq", PolyOpSignature::mono(vec![tp("File(Json)"), tp("JqFilter")], tp("Seq(Json)")), AlgebraicProperties { idempotent: true, ..Default::default() }, "jq .[] — extract array");
    reg.register_poly("jq_transform", PolyOpSignature::mono(vec![tp("Json"), tp("JqFilter")], tp("Json")), AlgebraicProperties::none(), "jq — transform JSON");
    reg.register_poly("yq_query", PolyOpSignature::mono(vec![tp("File(Yaml)"), tp("YqFilter")], tp("Yaml")), AlgebraicProperties { idempotent: true, ..Default::default() }, "yq — query YAML");
    reg.register_poly("yq_convert", PolyOpSignature::mono(vec![tp("File(Yaml)")], tp("File(Json)")), AlgebraicProperties { idempotent: true, ..Default::default() }, "yq -o=json — YAML to JSON");
    reg.register_poly("csv_cut", PolyOpSignature::mono(vec![tp("File(Csv)"), tp("ColumnList")], tp("File(Csv)")), AlgebraicProperties { idempotent: true, ..Default::default() }, "csvcut — select columns");
    reg.register_poly("csv_join", PolyOpSignature::mono(vec![tp("File(Csv)"), tp("File(Csv)"), tp("ColumnList")], tp("File(Csv)")), AlgebraicProperties::none(), "csvjoin — join CSV files");
    reg.register_poly("csv_sort", PolyOpSignature::mono(vec![tp("File(Csv)"), tp("ColumnList")], tp("File(Csv)")), AlgebraicProperties { idempotent: true, ..Default::default() }, "csvsort — sort CSV");
    // --- Advanced Text Processing ---
    reg.register_poly("awk_extract", PolyOpSignature::mono(vec![tp("File(Text)"), tp("AwkProgram")], tp("File(Text)")), AlgebraicProperties::none(), "awk extract fields");
    reg.register_poly("awk_aggregate", PolyOpSignature::mono(vec![tp("File(Text)"), tp("AwkProgram")], tp("Text")), AlgebraicProperties::none(), "awk aggregate");
    reg.register_poly("sed_script", PolyOpSignature::mono(vec![tp("File(Text)"), tp("SedScript")], tp("File(Text)")), AlgebraicProperties::none(), "sed -f script");
    reg.register_poly("cut_fields", PolyOpSignature::mono(vec![tp("File(Text)"), tp("Delimiter"), tp("FieldList")], tp("File(Text)")), AlgebraicProperties { idempotent: true, ..Default::default() }, "cut -d -f");
    reg.register_poly("tr_replace", PolyOpSignature::mono(vec![tp("Text"), tp("CharSet"), tp("CharSet")], tp("Text")), AlgebraicProperties::none(), "tr SET1 SET2");
    reg.register_poly("paste_merge", PolyOpSignature::mono(vec![tp("File(Text)"), tp("File(Text)"), tp("Delimiter")], tp("File(Text)")), AlgebraicProperties::none(), "paste -d");
    reg.register_poly(
        "tee_split",
        PolyOpSignature::new(vec!["a".into()], vec![tp("a"), tp("Path")], tp("a")),
        AlgebraicProperties::none(),
        "tee — copy stdin to file while passing through",
    );
    reg.register_poly("column_format", PolyOpSignature::mono(vec![tp("File(Text)"), tp("Delimiter")], tp("File(Text)")), AlgebraicProperties { idempotent: true, ..Default::default() }, "column -t — format columns");
    // --- Process & System Management ---
    reg.register_poly("ps_list", PolyOpSignature::leaf(tp("Seq(Process)")), AlgebraicProperties { idempotent: true, ..Default::default() }, "ps aux — list processes");
    reg.register_poly("kill_process", PolyOpSignature::mono(vec![tp("Pid"), tp("Signal")], tp("Unit")), AlgebraicProperties::none(), "kill -SIG PID");
    reg.register_poly("pkill_pattern", PolyOpSignature::mono(vec![tp("Pattern")], tp("Count")), AlgebraicProperties::none(), "pkill — kill by pattern");
    reg.register_poly("watch_command", PolyOpSignature::mono(vec![tp("Command"), tp("Interval")], tp("Seq(Text)")), AlgebraicProperties::none(), "watch -n — repeat command");
    reg.register_poly("df_usage", PolyOpSignature::leaf(tp("Seq(DiskUsage)")), AlgebraicProperties { idempotent: true, ..Default::default() }, "df -h — disk usage");
    reg.register_poly("du_size", PolyOpSignature::mono(vec![tp("Path")], tp("Size")), AlgebraicProperties { idempotent: true, ..Default::default() }, "du -sh — directory size");
    reg.register_poly("lsof_open", PolyOpSignature::mono(vec![tp("Path")], tp("Seq(OpenFile)")), AlgebraicProperties { idempotent: true, ..Default::default() }, "lsof — open files");
    reg.register_poly("file_type_detect", PolyOpSignature::mono(vec![tp("Path")], tp("MimeType")), AlgebraicProperties { idempotent: true, ..Default::default() }, "file --mime-type");
    reg.register_poly("uname_info", PolyOpSignature::leaf(tp("SystemInfo")), AlgebraicProperties { idempotent: true, ..Default::default() }, "uname -a");
    reg.register_poly("uptime_info", PolyOpSignature::leaf(tp("UptimeInfo")), AlgebraicProperties { idempotent: true, ..Default::default() }, "uptime");
    // --- Networking ---
    reg.register_poly("ssh_exec", PolyOpSignature::mono(vec![tp("Host"), tp("Command")], tp("Text")), AlgebraicProperties::none(), "ssh host command");
    reg.register_poly(
        "scp_transfer",
        PolyOpSignature::new(vec!["a".into()], vec![tp("File(a)"), tp("Host"), tp("Path")], tp("Unit")),
        AlgebraicProperties::none(),
        "scp file host:path",
    );
    reg.register_poly("wget_download", PolyOpSignature::mono(vec![tp("URL")], tp("File(Bytes)")), AlgebraicProperties::none(), "wget — download file");
    reg.register_poly("nc_connect", PolyOpSignature::mono(vec![tp("Host"), tp("Port")], tp("Connection")), AlgebraicProperties::none(), "nc host port");
    reg.register_poly("ping_host", PolyOpSignature::mono(vec![tp("Host"), tp("Count")], tp("Seq(PingResult)")), AlgebraicProperties::none(), "ping -c N host");
    reg.register_poly("dig_lookup", PolyOpSignature::mono(vec![tp("Hostname"), tp("RecordType")], tp("Seq(DnsRecord)")), AlgebraicProperties { idempotent: true, ..Default::default() }, "dig hostname TYPE");
    // --- Compression & Crypto ---
    reg.register_poly(
        "gzip_compress",
        PolyOpSignature::new(vec!["a".into()], vec![tp("File(a)")], tp("File(Gzip(a))")),
        AlgebraicProperties::none(),
        "gzip — compress file",
    );
    reg.register_poly(
        "gzip_decompress",
        PolyOpSignature::new(vec!["a".into()], vec![tp("File(Gzip(a))")], tp("File(a)")),
        AlgebraicProperties::none(),
        "gunzip — decompress gzip file",
    );
    reg.register_poly(
        "xz_compress",
        PolyOpSignature::new(vec!["a".into()], vec![tp("File(a)")], tp("File(Xz(a))")),
        AlgebraicProperties::none(),
        "xz — compress file",
    );
    reg.register_poly("base64_encode", PolyOpSignature::mono(vec![tp("Bytes")], tp("Text")), AlgebraicProperties::none(), "base64 — encode");
    reg.register_poly("base64_decode", PolyOpSignature::mono(vec![tp("Text")], tp("Bytes")), AlgebraicProperties::none(), "base64 -d — decode");
    reg.register_poly(
        "openssl_hash",
        PolyOpSignature::new(vec!["a".into()], vec![tp("File(a)"), tp("HashAlgorithm")], tp("Hash")),
        AlgebraicProperties { idempotent: true, ..Default::default() },
        "openssl dgst — cryptographic hash",
    );
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
