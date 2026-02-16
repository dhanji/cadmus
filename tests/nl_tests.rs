
// ===========================================================================
// B7: Bugfix integration tests — multi-turn conversations
// ===========================================================================

/// Helper: assert PlanCreated and that the YAML compiles
fn assert_plan_compiles(response: &NlResponse, expected_ops: &[&str]) {
    match response {
        NlResponse::PlanCreated { workflow_yaml, .. } => {
            for op in expected_ops {
                assert!(
                    workflow_yaml.contains(op),
                    "workflow should contain '{}': {}",
                    op, workflow_yaml
                );
            }
            let parsed = reasoning_engine::workflow::parse_workflow(workflow_yaml)
                .expect("should parse");
            let registry = reasoning_engine::fs_types::build_full_registry();
            reasoning_engine::workflow::compile_workflow(&parsed, &registry)
                .unwrap_or_else(|e| panic!(
                    "workflow should compile: {:?}\nYAML:\n{}", e, workflow_yaml
                ));
        }
        other => panic!("expected PlanCreated, got: {:?}", other),
    }
}

// -- Conversation: compress a file (B2+B3 fix) --

#[test]
fn test_bugfix_compress_file_compiles() {
    let mut state = DialogueState::new();
    let r = process_input("compress my_file.txt", &mut state);
    assert_plan_compiles(&r, &["gzip_compress"]);
}

#[test]
fn test_bugfix_compress_tar_compiles() {
    let mut state = DialogueState::new();
    let r = process_input("compress my_project.tar", &mut state);
    assert_plan_compiles(&r, &["gzip_compress"]);
}

// -- Conversation: hash a yaml file (B2+B3 fix) --

#[test]
fn test_bugfix_hash_yaml_compiles() {
    let mut state = DialogueState::new();
    let r = process_input("hash config.yaml", &mut state);
    assert_plan_compiles(&r, &["openssl_hash"]);
}

// -- Conversation: grep for errors in a file (B3 fix) --

#[test]
fn test_bugfix_grep_file_compiles() {
    let mut state = DialogueState::new();
    let r = process_input("grep for errors in /var/log/app.log", &mut state);
    assert_plan_compiles(&r, &["search_content"]);
}

#[test]
fn test_bugfix_search_dir_compiles() {
    let mut state = DialogueState::new();
    let r = process_input("search for TODO in all my files", &mut state);
    assert_plan_compiles(&r, &["search_content"]);
}

// -- Conversation: clone a repo (B4 fix) --

#[test]
fn test_bugfix_clone_repo_creates_plan() {
    let mut state = DialogueState::new();
    let r = process_input("clone the repo at github.com/user/project", &mut state);
    assert_plan_created(&r, &["git_clone"]);
}

// -- Conversation: "do the thing" (B1+B5 fix) --

#[test]
fn test_bugfix_do_the_thing_clarifies() {
    let mut state = DialogueState::new();
    let r = process_input("do the thing", &mut state);
    assert!(matches!(r, NlResponse::NeedsClarification { .. }),
        "expected NeedsClarification, got: {:?}", r);
}

// -- Conversation: "nah scrap that" (B1+B5 fix) --

#[test]
fn test_bugfix_nah_scrap_that_rejects() {
    let mut state = DialogueState::new();
    process_input("zip up ~/Downloads", &mut state);
    let r = process_input("nah scrap that", &mut state);
    assert!(matches!(r, NlResponse::Rejected),
        "expected Rejected, got: {:?}", r);
}

// -- Conversation: approve without plan (B6 fix) --

#[test]
fn test_bugfix_approve_without_plan() {
    let mut state = DialogueState::new();
    let r = process_input("approve", &mut state);
    assert!(matches!(r, NlResponse::NeedsClarification { .. }),
        "expected NeedsClarification, got: {:?}", r);
}

// -- Conversation: compound sentence (B5 fix) --

#[test]
fn test_bugfix_compound_skip_compress_instead() {
    let mut state = DialogueState::new();
    process_input("zip up ~/Downloads", &mut state);
    let r = process_input("actually skip that, compress my_data.tar instead", &mut state);
    // Should create a new plan with gzip_compress, not edit the old one
    assert_plan_created(&r, &["gzip_compress"]);
}

// -- Multi-turn conversation: full flow with bugfixes --

#[test]
fn test_bugfix_full_conversation_compress_approve() {
    let mut state = DialogueState::new();
    
    // Turn 1: compress a file (B2+B3 fix)
    let r1 = process_input("compress my_file.txt", &mut state);
    assert_plan_compiles(&r1, &["gzip_compress"]);
    
    // Turn 2: approve
    let r2 = process_input("looks good, go ahead", &mut state);
    assert!(matches!(r2, NlResponse::Approved));
}

#[test]
fn test_bugfix_full_conversation_search_approve() {
    let mut state = DialogueState::new();
    
    // Turn 1: search for TODO
    let r1 = process_input("search for TODO in my project", &mut state);
    assert_plan_compiles(&r1, &["search_content"]);
    
    // Turn 2: approve
    let r2 = process_input("ship it", &mut state);
    assert!(matches!(r2, NlResponse::Approved));
}

#[test]
fn test_bugfix_full_conversation_ambiguous_then_specific() {
    let mut state = DialogueState::new();
    
    // Turn 1: ambiguous input (B5 fix)
    let r1 = process_input("do the thing", &mut state);
    assert!(matches!(r1, NlResponse::NeedsClarification { .. }));
    
    // Turn 2: specific input (B2+B3 fix)
    let r2 = process_input("compress my_file.txt", &mut state);
    assert_plan_compiles(&r2, &["gzip_compress"]);
    
    // Turn 3: approve
    let r3 = process_input("ok go ahead", &mut state);
    assert!(matches!(r3, NlResponse::Approved));
}
// Integration tests for the NL UX layer.
//
// Tests the full pipeline end-to-end with diverse phrasings, typo correction,
// explanations, edits, approvals, rejections, and multi-turn conversations.

use reasoning_engine::nl::dialogue::DialogueState;
use reasoning_engine::nl::{NlResponse, process_input};

// ---------------------------------------------------------------------------
// Diverse phrasings → CreateWorkflow
// ---------------------------------------------------------------------------

#[test]
fn test_nl_zip_up_downloads() {
    let mut state = DialogueState::new();
    let r = process_input("zip up everything in my downloads", &mut state);
    assert_plan_created(&r, &["walk_tree", "pack_archive"]);
}

#[test]
fn test_nl_zip_up_tilde_path() {
    let mut state = DialogueState::new();
    let r = process_input("zip up ~/Downloads", &mut state);
    assert_plan_created(&r, &["pack_archive"]);
    if let NlResponse::PlanCreated { workflow_yaml, .. } = &r {
        assert!(workflow_yaml.contains("~/Downloads"), "yaml: {}", workflow_yaml);
    }
}

#[test]
fn test_nl_find_pdfs() {
    let mut state = DialogueState::new();
    let r = process_input("find all PDFs in ~/Documents", &mut state);
    assert_plan_created(&r, &["walk_tree"]);
}

#[test]
fn test_nl_list_directory() {
    let mut state = DialogueState::new();
    let r = process_input("list ~/Downloads", &mut state);
    assert_plan_created(&r, &["list_dir"]);
}

#[test]
fn test_nl_walk_the_tree() {
    let mut state = DialogueState::new();
    let r = process_input("walk the directory tree in /tmp", &mut state);
    assert_plan_created(&r, &["walk_tree"]);
}

#[test]
fn test_nl_extract_archive() {
    let mut state = DialogueState::new();
    let r = process_input("extract the archive at ~/comic.cbz", &mut state);
    assert_plan_created(&r, &["extract_archive"]);
}

#[test]
fn test_nl_grep_for_errors() {
    let mut state = DialogueState::new();
    let r = process_input("grep for errors in /var/log/app.log", &mut state);
    assert_plan_created(&r, &["search_content"]);
}

#[test]
fn test_nl_ls_downloads() {
    let mut state = DialogueState::new();
    let r = process_input("ls ~/Downloads", &mut state);
    assert_plan_created(&r, &["list_dir"]);
}

#[test]
fn test_nl_unzip_archive() {
    let mut state = DialogueState::new();
    let r = process_input("unzip ~/archive.zip", &mut state);
    assert_plan_created(&r, &["extract_archive"]);
}

#[test]
fn test_nl_sort_files() {
    let mut state = DialogueState::new();
    let r = process_input("sort files in ~/Downloads by name", &mut state);
    assert_plan_created(&r, &["sort_by"]);
}

#[test]
fn test_nl_git_log() {
    let mut state = DialogueState::new();
    let r = process_input("git log for the repo", &mut state);
    assert_plan_created(&r, &["git_log"]);
}

// ---------------------------------------------------------------------------
// Typo correction
// ---------------------------------------------------------------------------

#[test]
fn test_nl_typo_extract_archive() {
    let mut state = DialogueState::new();
    let r = process_input("extrct the archve at ~/comic.cbz", &mut state);
    assert_plan_created(&r, &["extract_archive"]);
}

#[test]
fn test_nl_typo_walk_tree() {
    let mut state = DialogueState::new();
    let r = process_input("wlak the direcotry tree in /tmp", &mut state);
    // Even with typos, should produce a valid workflow
    match &r {
        NlResponse::PlanCreated { workflow_yaml, .. } => {
            assert!(
                workflow_yaml.contains("walk_tree") || workflow_yaml.contains("list_dir"),
                "should have a dir operation: {}", workflow_yaml
            );
        }
        NlResponse::NeedsClarification { .. } => {
            // Acceptable if typos are too severe
        }
        other => panic!("expected PlanCreated or NeedsClarification, got: {:?}", other),
    }
}

// ---------------------------------------------------------------------------
// Explanations
// ---------------------------------------------------------------------------

#[test]
fn test_nl_explain_walk() {
    let mut state = DialogueState::new();
    let r = process_input("what's walk mean", &mut state);
    match r {
        NlResponse::Explanation { text } => {
            assert!(
                text.contains("directory") || text.contains("walk") || text.contains("tree"),
                "explanation should be about walking: {}", text
            );
        }
        other => panic!("expected Explanation, got: {:?}", other),
    }
}

#[test]
fn test_nl_explain_filter() {
    let mut state = DialogueState::new();
    let r = process_input("explain filter", &mut state);
    match r {
        NlResponse::Explanation { text } => {
            assert!(text.contains("filter") || text.contains("match"), "text: {}", text);
        }
        other => panic!("expected Explanation, got: {:?}", other),
    }
}

#[test]
fn test_nl_explain_what_does_sort_do() {
    let mut state = DialogueState::new();
    let r = process_input("what does sort do", &mut state);
    match r {
        NlResponse::Explanation { text } => {
            assert!(text.contains("sort") || text.contains("Sort"), "text: {}", text);
        }
        other => panic!("expected Explanation, got: {:?}", other),
    }
}

#[test]
fn test_nl_explain_how_does_grep_work() {
    let mut state = DialogueState::new();
    let r = process_input("how does grep work", &mut state);
    match r {
        NlResponse::Explanation { text } => {
            assert!(
                text.contains("search") || text.contains("content") || text.contains("grep"),
                "text: {}", text
            );
        }
        other => panic!("expected Explanation, got: {:?}", other),
    }
}

// ---------------------------------------------------------------------------
// Approve / Reject
// ---------------------------------------------------------------------------

#[test]
fn test_nl_approve_lgtm() {
    let mut state = DialogueState::new();
    process_input("zip up ~/Downloads", &mut state);
    assert!(matches!(process_input("lgtm", &mut state), NlResponse::Approved));
}

#[test]
fn test_nl_approve_sounds_good() {
    let mut state = DialogueState::new();
    process_input("zip up ~/Downloads", &mut state);
    assert!(matches!(process_input("sounds good", &mut state), NlResponse::Approved));
}

#[test]
fn test_nl_approve_yes() {
    let mut state = DialogueState::new();
    process_input("zip up ~/Downloads", &mut state);
    assert!(matches!(process_input("yes", &mut state), NlResponse::Approved));
}

#[test]
fn test_nl_approve_ok() {
    let mut state = DialogueState::new();
    process_input("zip up ~/Downloads", &mut state);
    assert!(matches!(process_input("ok", &mut state), NlResponse::Approved));
}

#[test]
fn test_nl_approve_ship_it() {
    let mut state = DialogueState::new();
    process_input("zip up ~/Downloads", &mut state);
    assert!(matches!(process_input("ship it", &mut state), NlResponse::Approved));
}

#[test]
fn test_nl_approve_do_it() {
    let mut state = DialogueState::new();
    process_input("zip up ~/Downloads", &mut state);
    assert!(matches!(process_input("do it", &mut state), NlResponse::Approved));
}

#[test]
fn test_nl_approve_go_ahead() {
    let mut state = DialogueState::new();
    process_input("zip up ~/Downloads", &mut state);
    assert!(matches!(process_input("go ahead", &mut state), NlResponse::Approved));
}

#[test]
fn test_nl_reject_no() {
    let mut state = DialogueState::new();
    assert!(matches!(process_input("no", &mut state), NlResponse::Rejected));
}

#[test]
fn test_nl_reject_nah() {
    let mut state = DialogueState::new();
    assert!(matches!(process_input("nah", &mut state), NlResponse::Rejected));
}

#[test]
fn test_nl_reject_cancel() {
    let mut state = DialogueState::new();
    assert!(matches!(process_input("cancel", &mut state), NlResponse::Rejected));
}

#[test]
fn test_nl_reject_start_over() {
    let mut state = DialogueState::new();
    assert!(matches!(process_input("start over", &mut state), NlResponse::Rejected));
}

// ---------------------------------------------------------------------------
// Edit operations
// ---------------------------------------------------------------------------

#[test]
fn test_nl_edit_skip_subdirectory() {
    let mut state = DialogueState::new();

    // Create a workflow first
    let r1 = process_input("zip up everything in ~/Downloads", &mut state);
    assert!(matches!(r1, NlResponse::PlanCreated { .. }));

    // Edit: skip a subdirectory
    let r2 = process_input("skip any subdirectory named .git", &mut state);
    match r2 {
        NlResponse::PlanEdited { workflow_yaml, .. } => {
            assert!(workflow_yaml.contains("filter"), "should have filter: {}", workflow_yaml);
        }
        other => panic!("expected PlanEdited, got: {:?}", other),
    }
}

#[test]
fn test_nl_edit_remove_step() {
    let mut state = DialogueState::new();

    // Create a workflow with multiple steps
    let r1 = process_input("find all files in ~/Documents", &mut state);
    assert!(matches!(r1, NlResponse::PlanCreated { .. }));

    let step_count_before = state.current_workflow.as_ref().unwrap().steps.len();

    // Remove a step
    let r2 = process_input("remove step 2", &mut state);
    match r2 {
        NlResponse::PlanEdited { .. } => {
            let step_count_after = state.current_workflow.as_ref().unwrap().steps.len();
            assert_eq!(step_count_after, step_count_before - 1);
        }
        other => panic!("expected PlanEdited, got: {:?}", other),
    }
}

#[test]
fn test_nl_edit_without_workflow() {
    let mut state = DialogueState::new();
    let r = process_input("skip any subdirectory named foo", &mut state);
    assert!(matches!(r, NlResponse::NeedsClarification { .. }),
        "edit without workflow should need clarification: {:?}", r);
}

// ---------------------------------------------------------------------------
// Multi-turn conversations
// ---------------------------------------------------------------------------

#[test]
fn test_nl_full_conversation_zip() {
    let mut state = DialogueState::new();

    // Turn 1: Create
    let r1 = process_input("zip up everything in ~/Downloads", &mut state);
    assert!(matches!(&r1, NlResponse::PlanCreated { .. }));
    assert!(state.current_workflow.is_some());

    // Turn 2: Ask about an op
    let r2 = process_input("what's walk mean", &mut state);
    assert!(matches!(&r2, NlResponse::Explanation { .. }));
    // Workflow should still be there
    assert!(state.current_workflow.is_some());

    // Turn 3: Edit
    let r3 = process_input("skip any subdirectory named .git", &mut state);
    assert!(matches!(&r3, NlResponse::PlanEdited { .. }));

    // Turn 4: Approve
    let r4 = process_input("lgtm", &mut state);
    assert!(matches!(r4, NlResponse::Approved));
}

#[test]
fn test_nl_conversation_create_reject_recreate() {
    let mut state = DialogueState::new();

    // Create
    let r1 = process_input("zip up ~/Downloads", &mut state);
    assert!(matches!(&r1, NlResponse::PlanCreated { .. }));

    // Reject
    let r2 = process_input("nah", &mut state);
    assert!(matches!(r2, NlResponse::Rejected));
    assert!(state.current_workflow.is_none());

    // Create again
    let r3 = process_input("list ~/Documents", &mut state);
    assert!(matches!(&r3, NlResponse::PlanCreated { .. }));
    assert!(state.current_workflow.is_some());
}

#[test]
fn test_nl_conversation_multiple_edits() {
    let mut state = DialogueState::new();

    // Create
    process_input("zip up everything in ~/Downloads", &mut state);

    // Edit 1: skip .git
    let r1 = process_input("skip any subdirectory named .git", &mut state);
    assert!(matches!(&r1, NlResponse::PlanEdited { .. }));

    // Edit 2: skip node_modules
    let r2 = process_input("skip any subdirectory named node_modules", &mut state);
    assert!(matches!(&r2, NlResponse::PlanEdited { .. }));

    // The workflow should have multiple filter steps
    let wf = state.current_workflow.as_ref().unwrap();
    let filter_count = wf.steps.iter().filter(|s| s.op == "filter").count();
    assert!(filter_count >= 2, "should have at least 2 filter steps, got {}", filter_count);
}

// ---------------------------------------------------------------------------
// NeedsClarification
// ---------------------------------------------------------------------------

#[test]
fn test_nl_gibberish() {
    let mut state = DialogueState::new();
    let r = process_input("asdfghjkl qwerty zxcvbnm", &mut state);
    assert!(matches!(r, NlResponse::NeedsClarification { .. }));
}

#[test]
fn test_nl_empty_input() {
    let mut state = DialogueState::new();
    let r = process_input("", &mut state);
    assert!(matches!(r, NlResponse::NeedsClarification { .. }));
}

// ---------------------------------------------------------------------------
// YAML validation: generated YAML compiles through the engine
// ---------------------------------------------------------------------------

#[test]
fn test_nl_generated_yaml_compiles_zip() {
    let mut state = DialogueState::new();
    let r = process_input("zip up everything in ~/Downloads", &mut state);
    assert_yaml_compiles(&r);
}

// =========================================================================
// Hardening: transcript bug fixes
// =========================================================================

// -- "also skip X" continuation edits --

#[test]
fn test_hardening_also_skip_as_edit() {
    let mut state = DialogueState::new();
    let r1 = process_input("walk the directory tree in ~/src", &mut state);
    assert!(matches!(r1, NlResponse::PlanCreated { .. }));
    let r2 = process_input("also skip node_modules", &mut state);
    match &r2 {
        NlResponse::PlanEdited { diff_description, .. } => {
            let desc_lower = diff_description.to_lowercase();
            assert!(desc_lower.contains("skip") || desc_lower.contains("filter") || desc_lower.contains("exclude"),
                "should describe skip edit: {}", diff_description);
        }
        other => panic!("expected PlanEdited for 'also skip', got: {:?}", other),
    }
}

#[test]
fn test_hardening_and_then_skip() {
    let mut state = DialogueState::new();
    let _ = process_input("find all PDFs in ~/Documents", &mut state);
    let r = process_input("and then skip hidden files", &mut state);
    match &r {
        NlResponse::PlanEdited { .. } => {} // success
        other => panic!("expected PlanEdited for 'and then skip', got: {:?}", other),
    }
}

// -- "ok search for TODO" keyword extraction --

#[test]
fn test_hardening_ok_search_todo_keyword() {
    let mut state = DialogueState::new();
    let r = process_input("ok search for TODO in ~/src", &mut state);
    match &r {
        NlResponse::PlanCreated { workflow_yaml, .. } => {
            let yaml_lower = workflow_yaml.to_lowercase();
            assert!(!yaml_lower.contains("pattern: \"ok\"") && !yaml_lower.contains("pattern: ok"),
                "'ok' should NOT be the search pattern: {}", workflow_yaml);
            // "todo" should be the keyword
            assert!(yaml_lower.contains("todo"),
                "should contain 'todo' as keyword: {}", workflow_yaml);
        }
        other => panic!("expected PlanCreated, got: {:?}", other),
    }
}

#[test]
fn test_hardening_so_find_pdfs_no_so_keyword() {
    let mut state = DialogueState::new();
    let r = process_input("so find all PDFs in ~/Documents", &mut state);
    match &r {
        NlResponse::PlanCreated { workflow_yaml, .. } => {
            // "so" should not appear as a keyword/pattern
            assert!(!workflow_yaml.contains("pattern: \"so\"") && !workflow_yaml.contains("pattern: so\n"),
                "'so' should NOT be a keyword: {}", workflow_yaml);
        }
        other => panic!("expected PlanCreated, got: {:?}", other),
    }
}

// -- "ok lgtm" compound approval --

#[test]
fn test_hardening_ok_lgtm_approves() {
    let mut state = DialogueState::new();
    let _ = process_input("compress file.txt", &mut state);
    let r = process_input("ok lgtm", &mut state);
    assert!(matches!(r, NlResponse::Approved),
        "'ok lgtm' should approve, got: {:?}", r);
}

#[test]
fn test_hardening_sure_yeah_approves() {
    let mut state = DialogueState::new();
    let _ = process_input("compress file.txt", &mut state);
    let r = process_input("sure yeah", &mut state);
    assert!(matches!(r, NlResponse::Approved),
        "'sure yeah' should approve, got: {:?}", r);
}

#[test]
fn test_hardening_fine_ok_approves() {
    let mut state = DialogueState::new();
    let _ = process_input("compress file.txt", &mut state);
    let r = process_input("fine ok", &mut state);
    assert!(matches!(r, NlResponse::Approved),
        "'fine ok' should approve, got: {:?}", r);
}

// -- ~/path/file.sql type inference --

#[test]
fn test_hardening_tilde_sql_file_compiles() {
    let mut state = DialogueState::new();
    let r = process_input("compress ~/backup/database.sql", &mut state);
    assert!(matches!(r, NlResponse::PlanCreated { .. }),
        "~/backup/database.sql should create plan: {:?}", r);
    assert_yaml_compiles(&r);
}

#[test]
fn test_hardening_tilde_tar_gz_compiles() {
    let mut state = DialogueState::new();
    let r = process_input("extract ~/archive.tar.gz", &mut state);
    assert!(matches!(r, NlResponse::PlanCreated { .. }),
        "~/archive.tar.gz should create plan: {:?}", r);
    assert_yaml_compiles(&r);
}

#[test]
fn test_hardening_tilde_dir_still_works() {
    let mut state = DialogueState::new();
    let r = process_input("list ~/Documents", &mut state);
    assert!(matches!(r, NlResponse::PlanCreated { .. }),
        "~/Documents should create plan: {:?}", r);
    assert_yaml_compiles(&r);
}

// -- Dictionary expansion: slang doesn't break things --

#[test]
fn test_hardening_slang_doesnt_create_workflow() {
    let mut state = DialogueState::new();
    let r = process_input("yikes whoa that's crazy", &mut state);
    assert!(!matches!(r, NlResponse::PlanCreated { .. }),
        "slang should NOT create workflow: {:?}", r);
}

#[test]
fn test_hardening_gonna_wanna_passthrough() {
    let mut state = DialogueState::new();
    // "gonna" and "wanna" should not be corrected to domain ops
    let r = process_input("I'm gonna wanna compress file.txt", &mut state);
    assert!(matches!(r, NlResponse::PlanCreated { .. }),
        "should still create workflow with slang: {:?}", r);
    assert_yaml_compiles(&r);
}

#[test]
fn test_nl_generated_yaml_compiles_list() {
    let mut state = DialogueState::new();
    let r = process_input("list ~/Documents", &mut state);
    assert_yaml_compiles(&r);
}

#[test]
fn test_nl_generated_yaml_compiles_extract() {
    let mut state = DialogueState::new();
    let r = process_input("extract ~/comic.cbz", &mut state);
    assert_yaml_compiles(&r);
}

#[test]
fn test_nl_generated_yaml_compiles_walk() {
    let mut state = DialogueState::new();
    let r = process_input("walk the directory tree in /tmp", &mut state);
    assert_yaml_compiles(&r);
}

#[test]
fn test_nl_edited_yaml_compiles() {
    let mut state = DialogueState::new();
    process_input("zip up everything in ~/Downloads", &mut state);
    let r = process_input("skip any subdirectory named .git", &mut state);
    assert_yaml_compiles_edited(&r);
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn assert_plan_created(response: &NlResponse, expected_ops: &[&str]) {
    match response {
        NlResponse::PlanCreated { workflow_yaml, .. } => {
            for op in expected_ops {
                assert!(
                    workflow_yaml.contains(op),
                    "workflow should contain '{}': {}",
                    op, workflow_yaml
                );
            }
        }
        other => panic!("expected PlanCreated, got: {:?}", other),
    }
}

fn assert_yaml_compiles(response: &NlResponse) {
    if let NlResponse::PlanCreated { workflow_yaml, .. } = response {
        let parsed = reasoning_engine::workflow::parse_workflow(workflow_yaml)
            .expect("should parse");
        let registry = reasoning_engine::fs_types::build_full_registry();
        reasoning_engine::workflow::compile_workflow(&parsed, &registry)
            .expect("should compile");
    }
}

fn assert_yaml_compiles_edited(response: &NlResponse) {
    if let NlResponse::PlanEdited { workflow_yaml, .. } = response {
        let parsed = reasoning_engine::workflow::parse_workflow(workflow_yaml)
            .expect("should parse");
        let registry = reasoning_engine::fs_types::build_full_registry();
        reasoning_engine::workflow::compile_workflow(&parsed, &registry)
            .expect("should compile");
    }
}

// ==========================================================================
// Red-team integration tests
// ==========================================================================

// -- BUG-001: "never mind" rejection --

#[test]
fn test_redteam_never_mind_rejects() {
    let mut state = DialogueState::new();
    let r1 = process_input("compress file.txt", &mut state);
    assert!(matches!(r1, NlResponse::PlanCreated { .. }));
    let r2 = process_input("never mind", &mut state);
    assert!(matches!(r2, NlResponse::Rejected),
        "never mind should reject, got: {:?}", r2);
}

// -- BUG-002/003: search keyword extraction --

#[test]
fn test_redteam_search_todo_keyword() {
    let mut state = DialogueState::new();
    let r = process_input("search for TODO in ~/src", &mut state);
    if let NlResponse::PlanCreated { workflow_yaml, .. } = &r {
        assert!(workflow_yaml.contains("todo") || workflow_yaml.contains("TODO"),
            "search pattern should contain 'todo', got:\n{}", workflow_yaml);
        assert!(!workflow_yaml.contains("good"),
            "search pattern should NOT contain 'good', got:\n{}", workflow_yaml);
    } else {
        panic!("expected PlanCreated, got: {:?}", r);
    }
}

#[test]
fn test_redteam_search_fixme_keyword() {
    let mut state = DialogueState::new();
    let r = process_input("search for FIXME in ~/src", &mut state);
    if let NlResponse::PlanCreated { workflow_yaml, .. } = &r {
        assert!(workflow_yaml.contains("fixme") || workflow_yaml.contains("FIXME"),
            "search pattern should contain 'fixme', got:\n{}", workflow_yaml);
    } else {
        panic!("expected PlanCreated, got: {:?}", r);
    }
}

// -- BUG-004: comma-containing approve phrases --

#[test]
fn test_redteam_perfect_comma_ship_it_approves() {
    let mut state = DialogueState::new();
    let _ = process_input("compress file.txt", &mut state);
    let r = process_input("perfect, ship it", &mut state);
    assert!(matches!(r, NlResponse::Approved),
        "perfect, ship it should approve, got: {:?}", r);
}

#[test]
fn test_redteam_yep_comma_run_it_approves() {
    let mut state = DialogueState::new();
    let _ = process_input("compress file.txt", &mut state);
    let r = process_input("yep, run it", &mut state);
    assert!(matches!(r, NlResponse::Approved),
        "yep, run it should approve, got: {:?}", r);
}

// -- BUG-005: natural rejection phrases --

#[test]
fn test_redteam_actually_no_rejects() {
    let mut state = DialogueState::new();
    let _ = process_input("compress file.txt", &mut state);
    let r = process_input("actually no", &mut state);
    assert!(matches!(r, NlResponse::Rejected),
        "actually no should reject, got: {:?}", r);
}

#[test]
fn test_redteam_forget_about_it_rejects() {
    let mut state = DialogueState::new();
    let _ = process_input("compress file.txt", &mut state);
    let r = process_input("forget about it", &mut state);
    assert!(matches!(r, NlResponse::Rejected),
        "forget about it should reject, got: {:?}", r);
}

#[test]
fn test_redteam_wait_no_rejects() {
    let mut state = DialogueState::new();
    let _ = process_input("compress file.txt", &mut state);
    let r = process_input("wait no", &mut state);
    assert!(matches!(r, NlResponse::Rejected),
        "wait no should reject, got: {:?}", r);
}

// -- BUG-006: double approve --

#[test]
fn test_redteam_double_approve_fails() {
    let mut state = DialogueState::new();
    let _ = process_input("compress file.txt", &mut state);
    let r1 = process_input("yes", &mut state);
    assert!(matches!(r1, NlResponse::Approved));
    let r2 = process_input("yes", &mut state);
    assert!(matches!(r2, NlResponse::NeedsClarification { .. }),
        "second approve should need clarification, got: {:?}", r2);
}

// -- BUG-008: "remove the step" --

#[test]
fn test_redteam_remove_the_step_removes_last() {
    let mut state = DialogueState::new();
    let r1 = process_input("find all PDFs in ~/Documents", &mut state);
    assert!(matches!(r1, NlResponse::PlanCreated { .. }));
    let r2 = process_input("remove the last step", &mut state);
    match &r2 {
        NlResponse::PlanEdited { diff_description, .. } => {
            assert!(diff_description.contains("Removed"),
                "should describe removal: {}", diff_description);
        }
        other => panic!("expected PlanEdited, got: {:?}", other),
    }
}

// -- BUG-010: "sure why not" and "yeah that works" approval --

#[test]
fn test_redteam_sure_why_not_approves() {
    let mut state = DialogueState::new();
    let _ = process_input("compress file.txt", &mut state);
    let r = process_input("sure why not", &mut state);
    assert!(matches!(r, NlResponse::Approved),
        "sure why not should approve, got: {:?}", r);
}

#[test]
fn test_redteam_yeah_that_works_approves() {
    let mut state = DialogueState::new();
    let _ = process_input("compress file.txt", &mut state);
    let r = process_input("yeah that works", &mut state);
    assert!(matches!(r, NlResponse::Approved),
        "yeah that works should approve, got: {:?}", r);
}

// -- BUG-011: casual English shouldn't create workflows --

#[test]
fn test_redteam_mind_not_workflow() {
    let mut state = DialogueState::new();
    let r = process_input("I have something in mind", &mut state);
    assert!(!matches!(r, NlResponse::PlanCreated { .. }),
        "casual English should NOT create workflow, got: {:?}", r);
}

// -- Adversarial inputs --

#[test]
fn test_redteam_unicode_path() {
    let mut state = DialogueState::new();
    let r = process_input("compress /Users/me/résumé.pdf", &mut state);
    assert!(matches!(r, NlResponse::PlanCreated { .. }),
        "unicode path should work: {:?}", r);
    assert_yaml_compiles(&r);
}

#[test]
fn test_redteam_only_punctuation() {
    let mut state = DialogueState::new();
    let r = process_input("!@#$%^&*()", &mut state);
    assert!(matches!(r, NlResponse::NeedsClarification { .. }),
        "punctuation-only should clarify: {:?}", r);
}

#[test]
fn test_redteam_relative_path() {
    let mut state = DialogueState::new();
    let r = process_input("compress ./src/main.rs", &mut state);
    assert!(matches!(r, NlResponse::PlanCreated { .. }));
    assert_yaml_compiles(&r);
}

#[test]
fn test_redteam_dotdot_path() {
    let mut state = DialogueState::new();
    let r = process_input("compress ../other/file.txt", &mut state);
    assert!(matches!(r, NlResponse::PlanCreated { .. }));
    assert_yaml_compiles(&r);
}

// -- Multi-turn state transitions --

#[test]
fn test_redteam_create_reject_create_approve() {
    let mut state = DialogueState::new();
    let r1 = process_input("compress file.txt", &mut state);
    assert!(matches!(r1, NlResponse::PlanCreated { .. }));
    let r2 = process_input("no", &mut state);
    assert!(matches!(r2, NlResponse::Rejected));
    let r3 = process_input("list ~/Desktop", &mut state);
    assert!(matches!(r3, NlResponse::PlanCreated { .. }));
    let r4 = process_input("yes", &mut state);
    assert!(matches!(r4, NlResponse::Approved));
}

#[test]
fn test_redteam_create_overwrite_approve() {
    let mut state = DialogueState::new();
    let r1 = process_input("compress file.txt", &mut state);
    assert!(matches!(r1, NlResponse::PlanCreated { .. }));
    // Second create overwrites first
    let r2 = process_input("list ~/Desktop", &mut state);
    assert!(matches!(r2, NlResponse::PlanCreated { .. }));
    if let NlResponse::PlanCreated { workflow_yaml, .. } = &r2 {
        assert!(workflow_yaml.contains("list_dir"),
            "second plan should be list_dir: {}", workflow_yaml);
    }
    let r3 = process_input("yes", &mut state);
    assert!(matches!(r3, NlResponse::Approved));
}

// -- Typo correction edge cases --

#[test]
fn test_redteam_heavy_typos_still_work() {
    let mut state = DialogueState::new();
    let r = process_input("extrct the archve at ~/comic.cbz", &mut state);
    assert!(matches!(r, NlResponse::PlanCreated { .. }),
        "heavy typos should still create plan: {:?}", r);
    assert_yaml_compiles(&r);
}

#[test]
fn test_redteam_transposed_letters() {
    let mut state = DialogueState::new();
    let r = process_input("wlak teh direcotry tree in /var", &mut state);
    assert!(matches!(r, NlResponse::PlanCreated { .. }),
        "transposed letters should still create plan: {:?}", r);
    assert_yaml_compiles(&r);
}
