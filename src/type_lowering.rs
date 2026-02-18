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

/// The static subsumption map: 10 world-touching fs_ops → shell ops.
///
/// These are the ops from SUBSUMPTION.md that have direct shell equivalents.
/// The shell ops are anchors (or type-symmetric peers of anchors) in the
/// inference pipeline.
const SUBSUMPTION_MAP: &[SubsumptionEntry] = &[
    // --- shell_text_lines class ---
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
    // --- shell_byte_stream class ---
    SubsumptionEntry {
        fs_op: "download",
        shell_op: "shell_curl",
        arity: 1,
        note: "curl — download URL contents",
    },
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

const RESIDUAL_FS_OPS: &[ResidualFsOp] = &[
    ResidualFsOp { fs_op: "stat", shell_cmd: "stat", is_exec: false, note: "file metadata" },
    ResidualFsOp { fs_op: "walk_tree_hierarchy", shell_cmd: "find", is_exec: false, note: "find (hierarchy)" },
    ResidualFsOp { fs_op: "checksum", shell_cmd: "shasum -a 256", is_exec: false, note: "SHA-256 hash" },
    ResidualFsOp { fs_op: "du_size", shell_cmd: "du -sh", is_exec: false, note: "disk usage" },
    ResidualFsOp { fs_op: "open_file", shell_cmd: "open", is_exec: true, note: "macOS open" },
    ResidualFsOp { fs_op: "reveal", shell_cmd: "open -R", is_exec: true, note: "macOS reveal in Finder" },
    ResidualFsOp { fs_op: "extract_archive", shell_cmd: "tar xf", is_exec: true, note: "extract archive" },
    ResidualFsOp { fs_op: "pack_archive", shell_cmd: "tar cf archive.tar", is_exec: true, note: "create archive" },
    ResidualFsOp { fs_op: "write_file", shell_cmd: "tee", is_exec: true, note: "write to file" },
    ResidualFsOp { fs_op: "copy", shell_cmd: "cp -r", is_exec: true, note: "copy file/dir" },
    ResidualFsOp { fs_op: "delete", shell_cmd: "rm -rf", is_exec: true, note: "delete file/dir" },
    ResidualFsOp { fs_op: "rename", shell_cmd: "mv", is_exec: true, note: "rename/move" },
    ResidualFsOp { fs_op: "move_entry", shell_cmd: "mv", is_exec: true, note: "move entry" },
    ResidualFsOp { fs_op: "create_dir", shell_cmd: "mkdir -p", is_exec: true, note: "create directory" },
    ResidualFsOp { fs_op: "diff", shell_cmd: "diff", is_exec: false, note: "file diff" },
];

/// Check if an op is a residual fs_op (world-touching, not yet in CLI fact pack).
pub fn is_residual_fs_op(op: &str) -> bool {
    RESIDUAL_FS_OPS.iter().any(|e| e.fs_op == op)
}

/// Look up a residual fs_op.
pub fn lookup_residual(fs_op: &str) -> Option<&'static ResidualFsOp> {
    RESIDUAL_FS_OPS.iter().find(|e| e.fs_op == fs_op)
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
    fn test_rename_not_subsumed() {
        assert!(lookup_subsumption("rename").is_none());
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
    fn test_all_10_entries_present() {
        let entries = all_subsumptions();
        assert_eq!(entries.len(), 10);

        let fs_ops: Vec<&str> = entries.iter().map(|e| e.fs_op).collect();
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
