/// Integration tests for macOS task plans.
///
/// Verifies all 22 plans load → compile → codegen without errors.
/// Checks for the double-define bug (nested `(define (define ...))`) and
/// validates that generated Racket scripts contain correct shell commands.

use cadmus::calling_frame::{CallingFrame, DefaultFrame};
use cadmus::plan;
use cadmus::racket_executor;
use std::fs;

/// Helper: load a plan, compile it, and generate a Racket script.
/// Returns the script string or panics with a clear error.
fn codegen_plan(plan_path: &str) -> String {
    let content = fs::read_to_string(plan_path)
        .unwrap_or_else(|e| panic!("Failed to read {}: {}", plan_path, e));

    let def = cadmus::sexpr::parse_sexpr_to_plan(&content)
        .unwrap_or_else(|e| panic!("Parse failed for {}: {}", plan_path, e));

    let frame = DefaultFrame::from_plan(&def);
    frame.codegen(&def)
        .unwrap_or_else(|e| panic!("Codegen failed for {}: {}", plan_path, e))
}

/// Helper: check that a script has no double-define pattern.
fn assert_no_double_define(script: &str, plan_name: &str) {
    // The bug pattern: (define (fn ...) followed by another (define (fn ...) inside
    // Look for "(define (" appearing inside another "(define (" block
    let lines: Vec<&str> = script.lines().collect();
    for (i, line) in lines.iter().enumerate() {
        let trimmed = line.trim();
        if trimmed.starts_with("(define (") {
            // Check the next non-empty line — if it also starts with (define, that's the bug
            for j in (i + 1)..lines.len() {
                let next = lines[j].trim();
                if next.is_empty() { continue; }
                if next.starts_with("(define (") {
                    panic!(
                        "Double-define detected in {}: line {} '{}' followed by line {} '{}'",
                        plan_name, i + 1, trimmed, j + 1, next
                    );
                }
                break;
            }
        }
    }
}

// =========================================================================
// Codegen validity: all 22 plans produce valid Racket scripts
// =========================================================================

const MACOS_TASK_PLANS: &[&str] = &[
    "tmux_attach",
    "commit_all",
    "commit_and_push",
    "open_downloads",
    "disk_usage",
    "find_recent_pdf",
    "delete_ds_store",
    "rename_photos_by_date",
    "organize_by_type",
    "show_hidden_files",
    "clean_desktop",
    "zip_folder",
    "unzip_here",
    "find_large_files_threshold",
    "files_changed_today",
    "copy_to_documents",
    "trash_file",
    "compare_folders",
    "recent_files",
    "backup_folder",
    "replace_spaces_in_filenames",
    "find_duplicate_files",
];

#[test]
fn test_all_plans_codegen_succeeds() {
    let mut failures = Vec::new();
    for name in MACOS_TASK_PLANS {
        let path = format!("data/plans/{}.sexp", name);
        match std::panic::catch_unwind(|| codegen_plan(&path)) {
            Ok(_) => {}
            Err(_) => failures.push(*name),
        }
    }
    assert!(
        failures.is_empty(),
        "Codegen failed for {} plans: {:?}",
        failures.len(),
        failures
    );
}

#[test]
fn test_no_double_define_in_any_plan() {
    for name in MACOS_TASK_PLANS {
        let path = format!("data/plans/{}.sexp", name);
        let script = codegen_plan(&path);
        assert_no_double_define(&script, name);
    }
}

// =========================================================================
// Specific command correctness checks
// =========================================================================

#[test]
fn test_commit_and_push_generates_git_commands() {
    let script = codegen_plan("data/plans/commit_and_push.sexp");
    assert!(script.contains("git add"), "Missing 'git add' in script");
    assert!(script.contains("git commit -m"), "Missing 'git commit -m' in script");
    assert!(script.contains("git push"), "Missing 'git push' in script");
}

#[test]
fn test_commit_all_generates_git_add_and_commit() {
    let script = codegen_plan("data/plans/commit_all.sexp");
    assert!(script.contains("git add"), "Missing 'git add' in script");
    assert!(script.contains("git commit -m"), "Missing 'git commit -m' in script");
}

#[test]
fn test_open_downloads_generates_open_command() {
    let script = codegen_plan("data/plans/open_downloads.sexp");
    assert!(script.contains("open "), "Missing 'open' command in script");
    assert!(script.contains("Downloads"), "Missing 'Downloads' path in script");
}

#[test]
fn test_trash_file_generates_mv_to_trash() {
    let script = codegen_plan("data/plans/trash_file.sexp");
    assert!(script.contains("mv "), "Missing 'mv' in script");
    assert!(script.contains(".Trash"), "Missing '.Trash' in script");
}

#[test]
fn test_delete_ds_store_has_map_mode() {
    let script = codegen_plan("data/plans/delete_ds_store.sexp");
    assert!(script.contains("map"), "Missing map mode in script");
    assert!(script.contains("rm "), "Missing 'rm' in script");
    assert!(script.contains("DS_Store"), "Missing 'DS_Store' pattern in script");
}

#[test]
fn test_compare_folders_generates_diff() {
    let script = codegen_plan("data/plans/compare_folders.sexp");
    assert!(script.contains("diff -rq"), "Missing 'diff -rq' in script");
}

#[test]
fn test_zip_folder_generates_zip() {
    let script = codegen_plan("data/plans/zip_folder.sexp");
    assert!(script.contains("zip"), "Missing 'zip' in script");
}

#[test]
fn test_unzip_here_generates_unzip() {
    let script = codegen_plan("data/plans/unzip_here.sexp");
    assert!(script.contains("unzip"), "Missing 'unzip' in script");
}

#[test]
fn test_find_recent_pdf_generates_find_and_filter() {
    let script = codegen_plan("data/plans/find_recent_pdf.sexp");
    assert!(script.contains("find "), "Missing 'find' in script");
    assert!(script.contains("pdf"), "Missing 'pdf' pattern in script");
}

#[test]
fn test_disk_usage_generates_du_and_sort() {
    let script = codegen_plan("data/plans/disk_usage.sexp");
    assert!(script.contains("du "), "Missing 'du' in script");
    assert!(script.contains("sort"), "Missing 'sort' in script");
}

#[test]
fn test_show_hidden_files_generates_defaults_write() {
    let script = codegen_plan("data/plans/show_hidden_files.sexp");
    assert!(script.contains("defaults write"), "Missing 'defaults write' in script");
    assert!(script.contains("AppleShowAllFiles"), "Missing 'AppleShowAllFiles' in script");
}

#[test]
fn test_backup_folder_generates_cp() {
    let script = codegen_plan("data/plans/backup_folder.sexp");
    assert!(script.contains("cp -a"), "Missing 'cp -a' in script");
    assert!(script.contains("date"), "Missing 'date' for timestamp in script");
}

#[test]
fn test_copy_to_documents_generates_rsync() {
    let script = codegen_plan("data/plans/copy_to_documents.sexp");
    assert!(script.contains("rsync -a"), "Missing 'rsync -a' in script");
    assert!(script.contains("Documents"), "Missing 'Documents' in script");
}

#[test]
fn test_replace_spaces_generates_mv() {
    let script = codegen_plan("data/plans/replace_spaces_in_filenames.sexp");
    assert!(script.contains("string-replace"), "Missing 'string-replace' in script");
    assert!(script.contains("mv "), "Missing 'mv' in script");
}

#[test]
fn test_find_duplicate_files_generates_shasum() {
    let script = codegen_plan("data/plans/find_duplicate_files.sexp");
    assert!(script.contains("shasum"), "Missing 'shasum' in script");
}

// =========================================================================
// Single-step plans use displayln, not let*
// =========================================================================

#[test]
fn test_single_step_plans_use_displayln() {
    let single_step_plans = [
        "open_downloads",
        "trash_file",
        "show_hidden_files",
        "zip_folder",
        "unzip_here",
    ];
    for name in &single_step_plans {
        let path = format!("data/plans/{}.sexp", name);
        let script = codegen_plan(&path);
        assert!(
            script.contains("(displayln"),
            "{} should use displayln for single-step plan",
            name
        );
        // Check that the script-level structure doesn't use let* for step chaining.
        // (let* inside define bodies is fine — we only care about the step-binding let*)
        let after_defines = script.split(";; Step 1:").last().unwrap_or("");
        assert!(!after_defines.contains("(let*"),
            "{} should NOT use let* for step chaining in single-step plan", name);
    }
}

// =========================================================================
// Multi-step plans use let* binding chain
// =========================================================================

#[test]
fn test_multi_step_plans_use_let_star() {
    let multi_step_plans = [
        "commit_all",
        "commit_and_push",
        "disk_usage",
        "find_recent_pdf",
        "delete_ds_store",
        "find_large_files_threshold",
        "files_changed_today",
        "recent_files",
    ];
    for name in &multi_step_plans {
        let path = format!("data/plans/{}.sexp", name);
        let script = codegen_plan(&path);
        assert!(
            script.contains("(let*"),
            "{} should use let* for multi-step plan",
            name
        );
    }
}

// =========================================================================
// Shell-quote injection safety: all user paths go through shell-quote
// =========================================================================

#[test]
fn test_all_scripts_include_shell_quote() {
    for name in MACOS_TASK_PLANS {
        let path = format!("data/plans/{}.sexp", name);
        let script = codegen_plan(&path);
        assert!(
            script.contains("shell-quote"),
            "{} should include shell-quote for injection safety",
            name
        );
    }
}

// =========================================================================
// Algorithm ops still work (regression check)
// =========================================================================

#[test]
fn test_algorithm_op_not_broken_by_define_fix() {
    // base_conversion uses a racket_body that is a bare expression (no define wrapper)
    let script = codegen_plan("data/plans/algorithms/arithmetic/base_conversion.sexp");
    // Should have exactly one (define (base_conversion ...) wrapping the body
    let define_count = script.matches("(define (base_conversion").count();
    assert_eq!(
        define_count, 1,
        "base_conversion should have exactly 1 define, got {}",
        define_count
    );
    assert_no_double_define(&script, "base_conversion");
}
