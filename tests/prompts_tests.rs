// ===========================================================================
// Integration tests for the 22 prompts from examples/prompts.txt
// ===========================================================================
// Each test verifies that a natural language prompt produces the correct plan
// via the NL → plan → compile → codegen pipeline.

use cadmus::nl::{NlResponse, process_input};
use cadmus::nl::dialogue::DialogueState;

/// Helper: parse a plan string — tries sexpr first, falls back to YAML.
fn parse_plan_any(src: &str) -> Result<cadmus::plan::PlanDef, String> {
    cadmus::sexpr::parse_sexpr_to_plan(src)
        .map_err(|e| e.to_string())
}

/// Helper: assert PlanCreated and that the plan compiles, checking expected ops.
fn assert_plan_compiles(response: &NlResponse, expected_ops: &[&str]) {
    match response {
        NlResponse::PlanCreated { plan_sexpr, .. } => {
            for op in expected_ops {
                assert!(
                    plan_sexpr.contains(op),
                    "plan should contain '{}': {}",
                    op, plan_sexpr
                );
            }
            let parsed = parse_plan_any(plan_sexpr)
                .expect("should parse");
            let registry = cadmus::fs_types::build_full_registry();
            cadmus::plan::compile_plan(&parsed, &registry)
                .unwrap_or_else(|e| panic!(
                    "plan should compile: {:?}\nSexpr:\n{}", e, plan_sexpr
                ));
        }
        other => panic!("expected PlanCreated, got: {:?}", other),
    }
}

/// Helper: run NL input and return the response.
fn nl(input: &str) -> NlResponse {
    let mut state = DialogueState::new();
    process_input(input, &mut state)
}

/// Helper: run NL input through create → approve, return the generated script.
fn nl_to_script(input: &str) -> Option<String> {
    let mut state = DialogueState::new();
    let r1 = process_input(input, &mut state);
    match r1 {
        NlResponse::PlanCreated { .. } => {
            let r2 = process_input("yes", &mut state);
            match r2 {
                NlResponse::Approved { script } => script,
                _ => None,
            }
        }
        _ => None,
    }
}

// ===========================================================================
// Prompt 1: Attach to tmux session
// ===========================================================================

#[test]
fn test_prompt_tmux_attach() {
    let r = nl("attach to an existing tmux session");
    assert_plan_compiles(&r, &["tmux_attach"]);
}

#[test]
fn test_prompt_tmux_attach_alt() {
    let r = nl("tmux attach to session");
    assert_plan_compiles(&r, &["tmux_attach"]);
}

// ===========================================================================
// Prompt 2: Commit all changes
// ===========================================================================

#[test]
fn test_prompt_commit_all() {
    let r = nl("commit all my changes");
    assert_plan_compiles(&r, &["git_add", "git_commit"]);
}

#[test]
fn test_prompt_commit_all_alt() {
    let r = nl("commit all changes");
    assert_plan_compiles(&r, &["git_add", "git_commit"]);
}

// ===========================================================================
// Prompt 3: Commit and push
// ===========================================================================

#[test]
fn test_prompt_commit_and_push() {
    let r = nl("commit and push to remote");
    assert_plan_compiles(&r, &["git_add", "git_commit", "git_push"]);
}

// ===========================================================================
// Prompt 4: Open downloads folder
// ===========================================================================

#[test]
fn test_prompt_open_downloads() {
    let r = nl("open the downloads folder");
    assert_plan_compiles(&r, &["open_file"]);
}

#[test]
fn test_prompt_open_downloads_alt() {
    let r = nl("open my downloads");
    assert_plan_compiles(&r, &["open_file"]);
}

// ===========================================================================
// Prompt 5: Show disk usage
// ===========================================================================

#[test]
fn test_prompt_disk_usage() {
    let r = nl("show disk usage for the home directory");
    assert_plan_compiles(&r, &["du_size"]);
}

#[test]
fn test_prompt_disk_usage_alt() {
    let r = nl("disk usage for home directory");
    assert_plan_compiles(&r, &["du_size"]);
}

// ===========================================================================
// Prompt 6: Find recent PDF
// ===========================================================================

#[test]
fn test_prompt_find_recent_pdf() {
    let r = nl("find recent PDF files");
    assert_plan_compiles(&r, &["find_recent"]);
}

// ===========================================================================
// Prompt 7: Delete .DS_Store files
// ===========================================================================

#[test]
fn test_prompt_delete_ds_store() {
    let r = nl("delete DS_Store files from this directory");
    assert_plan_compiles(&r, &["walk_tree"]);
}

#[test]
fn test_prompt_delete_ds_store_alt() {
    let r = nl("delete ds_store recursively");
    assert_plan_compiles(&r, &["walk_tree"]);
}

// ===========================================================================
// Prompt 8: Rename photos by date
// ===========================================================================

#[test]
fn test_prompt_rename_photos_by_date() {
    let r = nl("rename photos by date");
    assert_plan_compiles(&r, &["rename_by_date"]);
}

// ===========================================================================
// Prompt 9: Organize by file type
// ===========================================================================

#[test]
fn test_prompt_organize_by_type() {
    let r = nl("organize files by type into subfolders");
    assert_plan_compiles(&r, &["organize_by_extension"]);
}

// ===========================================================================
// Prompt 10: Show hidden files
// ===========================================================================

#[test]
fn test_prompt_show_hidden_files() {
    let r = nl("show hidden files in Finder");
    assert_plan_compiles(&r, &["show_hidden_files"]);
}

// ===========================================================================
// Prompt 11: Clean up desktop
// ===========================================================================

#[test]
fn test_prompt_clean_desktop() {
    let r = nl("clean up my desktop");
    assert_plan_compiles(&r, &["organize_by_extension"]);
}

// ===========================================================================
// Prompt 12: Zip this folder
// ===========================================================================

#[test]
fn test_prompt_zip_folder() {
    let r = nl("zip this folder");
    assert_plan_compiles(&r, &["pack_zip"]);
}

// ===========================================================================
// Prompt 13: Unzip here
// ===========================================================================

#[test]
fn test_prompt_unzip_here() {
    let r = nl("unzip this archive here");
    assert_plan_compiles(&r, &["extract_zip"]);
}

// ===========================================================================
// Prompt 14: Find large files
// ===========================================================================

#[test]
fn test_prompt_find_large_files_threshold() {
    let r = nl("find files larger than a size threshold");
    assert_plan_compiles(&r, &["find_by_size"]);
}

// ===========================================================================
// Prompt 15: Files changed today
// ===========================================================================

#[test]
fn test_prompt_files_changed_today() {
    let r = nl("show files changed today");
    assert_plan_compiles(&r, &["find_modified_since"]);
}

// ===========================================================================
// Prompt 16: Copy to documents
// ===========================================================================

#[test]
fn test_prompt_copy_to_documents() {
    let r = nl("copy this folder to documents");
    assert_plan_compiles(&r, &["sync"]);
}

// ===========================================================================
// Prompt 17: Trash file
// ===========================================================================

#[test]
fn test_prompt_trash_file() {
    let r = nl("move this file to the trash");
    assert_plan_compiles(&r, &["trash"]);
}

#[test]
fn test_prompt_trash_file_alt() {
    let r = nl("trash this file please");
    assert_plan_compiles(&r, &["trash"]);
}

// ===========================================================================
// Prompt 18: Compare folders
// ===========================================================================

#[test]
fn test_prompt_compare_folders() {
    let r = nl("compare these two folders");
    assert_plan_compiles(&r, &["compare_dirs"]);
}

// ===========================================================================
// Prompt 19: Recent files
// ===========================================================================

#[test]
fn test_prompt_recent_files() {
    let r = nl("show me recent files");
    assert_plan_compiles(&r, &["find_recent"]);
}

// ===========================================================================
// Prompt 20: Backup folder
// ===========================================================================

#[test]
fn test_prompt_backup_folder() {
    let r = nl("backup this folder with a timestamp");
    assert_plan_compiles(&r, &["backup_timestamped"]);
}

// ===========================================================================
// Prompt 21: Replace spaces in filenames
// ===========================================================================

#[test]
fn test_prompt_replace_spaces_in_filenames() {
    let r = nl("replace spaces in filenames with dashes");
    assert_plan_compiles(&r, &["replace_in_filenames"]);
}

// ===========================================================================
// Prompt 22: Find duplicate files
// ===========================================================================

#[test]
fn test_prompt_find_duplicate_files() {
    let r = nl("find duplicate files in this directory");
    assert_plan_compiles(&r, &["find_duplicates_by_hash"]);
}

// ===========================================================================
// Compilation tests: verify all 22 plan files compile directly
// ===========================================================================

#[test]
fn test_all_prompt_plans_compile() {
    let plans = vec![
        "tmux_attach", "commit_all", "commit_and_push", "open_downloads",
        "disk_usage", "find_recent_pdf", "delete_ds_store", "rename_photos_by_date",
        "organize_by_type", "show_hidden_files", "clean_desktop", "zip_folder",
        "unzip_here", "find_large_files_threshold", "files_changed_today",
        "copy_to_documents", "trash_file", "compare_folders", "recent_files",
        "backup_folder", "replace_spaces_in_filenames", "find_duplicate_files",
    ];
    let registry = cadmus::fs_types::build_full_registry();
    let mut failures = Vec::new();
    for name in &plans {
        let path = format!("data/plans/{}.sexp", name);
        let content = std::fs::read_to_string(&path)
            .unwrap_or_else(|_| panic!("plan file not found: {}", path));
        let plan = cadmus::sexpr::parse_sexpr_to_plan(&content)
            .unwrap_or_else(|e| panic!("parse failed for {}: {}", name, e));
        if let Err(e) = cadmus::plan::compile_plan(&plan, &registry) {
            failures.push(format!("{}: {}", name, e));
        }
    }
    assert!(failures.is_empty(), "Plan compilation failures:\n{}", failures.join("\n"));
}

// ===========================================================================
// Codegen tests: verify plans produce Racket scripts
// ===========================================================================

#[test]
fn test_prompt_plans_generate_racket() {
    let plans = vec![
        "tmux_attach", "commit_all", "commit_and_push", "open_downloads",
        "disk_usage", "show_hidden_files", "zip_folder", "unzip_here",
        "trash_file", "rename_photos_by_date", "organize_by_type",
        "find_duplicate_files", "compare_folders", "backup_folder",
    ];
    let registry = cadmus::racket_executor::build_racket_registry();
    let full_registry = cadmus::fs_types::build_full_registry();
    let mut failures = Vec::new();
    for name in &plans {
        let path = format!("data/plans/{}.sexp", name);
        let content = std::fs::read_to_string(&path).unwrap();
        let plan = cadmus::sexpr::parse_sexpr_to_plan(&content).unwrap();
        let compiled = match cadmus::plan::compile_plan(&plan, &full_registry) {
            Ok(c) => c,
            Err(e) => {
                failures.push(format!("{}: compile error: {}", name, e));
                continue;
            }
        };
        match cadmus::racket_executor::generate_racket_script(
            &compiled, &plan, &registry
        ) {
            Ok(script) => {
                assert!(script.contains("#lang racket") || script.contains("#!/"),
                    "{}: script missing header", name);
            }
            Err(e) => {
                failures.push(format!("{}: codegen error: {}", name, e));
            }
        }
    }
    assert!(failures.is_empty(), "Codegen failures:\n{}", failures.join("\n"));
}

// ===========================================================================
// Negative tests: malformed input doesn't panic
// ===========================================================================

#[test]
fn test_prompt_empty_input_no_panic() {
    let r = nl("");
    // Should not panic — may return NeedsClarification or Error
    match r {
        NlResponse::NeedsClarification { .. } | NlResponse::Error { .. } => {}
        _ => {} // Any response is fine as long as no panic
    }
}

#[test]
fn test_prompt_gibberish_no_panic() {
    let r = nl("asdfghjkl qwertyuiop zxcvbnm");
    match r {
        NlResponse::NeedsClarification { .. } | NlResponse::Error { .. } => {}
        _ => {} // Any response is fine
    }
}

#[test]
fn test_prompt_injection_no_panic() {
    let r = nl("delete $(rm -rf /) from the system");
    match r {
        NlResponse::NeedsClarification { .. } | NlResponse::Error { .. } | NlResponse::PlanCreated { .. } => {}
        _ => {}
    }
}
