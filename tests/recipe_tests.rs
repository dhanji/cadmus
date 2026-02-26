// ---------------------------------------------------------------------------
// Integration tests for command recipe lookup
// ---------------------------------------------------------------------------
//
// Tests the full NL pipeline: input → normalize → typo correct → recipe query
// detection → fact pack lookup → (displayln "command") Racket program.

use cadmus::nl::{self, dialogue::DialogueState, NlResponse};

// ---------------------------------------------------------------------------
// Helper
// ---------------------------------------------------------------------------

fn recipe_query(input: &str) -> Option<String> {
    let mut state = DialogueState::new();
    match nl::process_input(input, &mut state) {
        NlResponse::Explanation { text } => {
            if text.contains("displayln") {
                Some(text)
            } else {
                None
            }
        }
        _ => None,
    }
}

fn assert_recipe(input: &str, expected_fragments: &[&str]) {
    let result = recipe_query(input);
    assert!(
        result.is_some(),
        "Expected recipe for '{}', got no Explanation with displayln",
        input
    );
    let text = result.unwrap();
    for frag in expected_fragments {
        assert!(
            text.contains(frag),
            "Recipe for '{}' should contain '{}', got:\n{}",
            input, frag, text
        );
    }
}

fn assert_not_recipe(input: &str) {
    let result = recipe_query(input);
    assert!(
        result.is_none(),
        "Expected NO recipe for '{}', but got:\n{}",
        input,
        result.unwrap_or_default()
    );
}

// ===========================================================================
// 1. The 20 informational prompts
// ===========================================================================

#[test]
fn test_recipe_01_git_reset_head() {
    assert_recipe(
        "give me the command to reset my git repo",
        &["git", "reset"],
    );
}

#[test]
fn test_recipe_02_git_cherry_pick() {
    assert_recipe(
        "what's the command for git cherry pick",
        &["git", "cherry-pick"],
    );
}

#[test]
fn test_recipe_03_caffeinate() {
    assert_recipe(
        "give me the command to prevent my mac from sleeping",
        &["caffeinate"],
    );
}

#[test]
fn test_recipe_04_git_ls_files() {
    assert_recipe(
        "what's the command to list tracked git files",
        &["git", "ls-files"],
    );
}

#[test]
fn test_recipe_05_git_reset_soft() {
    assert_recipe(
        "give me the command for git soft reset",
        &["git", "reset"],
    );
}

#[test]
fn test_recipe_06_find_by_size() {
    assert_recipe(
        "what's the command to find files by size",
        &["find", "-size"],
    );
}

#[test]
fn test_recipe_07_defaults_write_hidden_files() {
    assert_recipe(
        "give me the command to show hidden files using defaults",
        &["defaults", "write"],
    );
}

#[test]
fn test_recipe_08_diff_recursive() {
    assert_recipe(
        "what's the command to diff two directories recursively",
        &["diff", "-r"],
    );
}

#[test]
fn test_recipe_09_ls_by_size() {
    assert_recipe(
        "give me the command to list files sorted by size",
        &["ls", "-lS"],
    );
}

#[test]
fn test_recipe_10_tail_follow() {
    assert_recipe(
        "what's the command to follow a log file",
        &["tail", "-f"],
    );
}

#[test]
fn test_recipe_11_du_sorted() {
    assert_recipe(
        "give me the command for disk usage sorted by size",
        &["du"],
    );
}

#[test]
fn test_recipe_12_rename_loop() {
    // This is a complex command — the recipe should at least return "mv"
    // since rename is done via mv in the shell
    assert_recipe(
        "show me the command to rename files in a loop",
        &["mv"],
    );
}

#[test]
fn test_recipe_13_find_delete_ds_store() {
    assert_recipe(
        "give me the command to find and delete DS_Store files",
        &["find", "-delete"],
    );
}

#[test]
fn test_recipe_14_ps_aux() {
    assert_recipe(
        "what's the command to list all running processes",
        &["ps", "aux"],
    );
}

#[test]
fn test_recipe_15_zip_recursive() {
    assert_recipe(
        "give me the command to zip a folder recursively",
        &["zip"],
    );
}

#[test]
fn test_recipe_16_unzip() {
    assert_recipe(
        "what's the command to unzip an archive",
        &["unzip"],
    );
}

#[test]
fn test_recipe_17_cp_recursive() {
    assert_recipe(
        "give me the command to copy a folder recursively",
        &["cp", "-r"],
    );
}

#[test]
fn test_recipe_18_find_by_mtime() {
    assert_recipe(
        "what's the command to find files modified recently",
        &["find", "-mtime"],
    );
}

#[test]
fn test_recipe_19_caffeinate_with_command() {
    assert_recipe(
        "give me the command to keep mac awake while running a command",
        &["caffeinate"],
    );
}

#[test]
fn test_recipe_20_git_log_graph() {
    assert_recipe(
        "what's the command for git log with a graph view",
        &["git", "log", "graph"],
    );
}

// ===========================================================================
// 2. Alternate phrasings
// ===========================================================================

#[test]
fn test_recipe_alt_how_do_i_from_terminal() {
    assert_recipe(
        "how do I cherry pick a commit from terminal",
        &["git", "cherry-pick"],
    );
}

#[test]
fn test_recipe_alt_show_me_the_command() {
    assert_recipe(
        "show me the command for listing processes",
        &["ps"],
    );
}

#[test]
fn test_recipe_alt_what_command_do_i_use() {
    assert_recipe(
        "what command do I use to check disk usage",
        &["disk"],
    );
}

#[test]
fn test_recipe_alt_how_do_i_from_command_line() {
    assert_recipe(
        "how do I reset git from the command line",
        &["git", "reset"],
    );
}

// ===========================================================================
// 3. Negative cases — should NOT return recipes
// ===========================================================================

#[test]
fn test_no_recipe_for_pasta() {
    assert_not_recipe("give me the command to make pasta");
}

#[test]
fn test_no_recipe_for_action_prompt() {
    // Action-oriented prompts should create plans, not recipes
    assert_not_recipe("zip up my downloads folder");
}

#[test]
fn test_no_recipe_for_plain_explain() {
    // "explain filter" should go through the explain path, not recipe
    let mut state = DialogueState::new();
    let response = nl::process_input("explain filter", &mut state);
    match response {
        NlResponse::Explanation { text } => {
            // Should be an op explanation, not a recipe
            assert!(!text.contains("displayln"), "explain filter should not be a recipe");
        }
        other => panic!("expected Explanation for 'explain filter', got {:?}", other),
    }
}

#[test]
fn test_no_recipe_for_short_input() {
    assert_not_recipe("hi");
}

// ===========================================================================
// 4. Regression: existing explain still works
// ===========================================================================

#[test]
fn test_explain_walk_tree_still_works() {
    let mut state = DialogueState::new();
    let response = nl::process_input("explain walk_tree", &mut state);
    assert!(matches!(response, NlResponse::Explanation { .. }));
}

#[test]
fn test_explain_filter_still_works() {
    let mut state = DialogueState::new();
    let response = nl::process_input("what is filter", &mut state);
    assert!(matches!(response, NlResponse::Explanation { .. }));
}

// ===========================================================================
// 5. Racket program format
// ===========================================================================

#[test]
fn test_recipe_contains_racket_program() {
    let result = recipe_query("give me the command to list processes");
    assert!(result.is_some());
    let text = result.unwrap();
    assert!(text.contains("#!/usr/bin/env racket"), "should have shebang");
    assert!(text.contains("#lang racket"), "should have #lang");
    assert!(text.contains("(displayln"), "should have displayln");
}

#[test]
fn test_recipe_displayln_has_command() {
    let result = recipe_query("give me the command for git cherry pick");
    assert!(result.is_some());
    let text = result.unwrap();
    // The displayln should contain the actual command
    assert!(text.contains("git cherry-pick"), "displayln should contain the command");
}

// ===========================================================================
// 6. "how do I ..." without terminal suffix
// ===========================================================================

#[test]
fn test_how_do_i_stop_computer_sleeping() {
    let result = recipe_query("how do i stop the computer from sleeping");
    assert!(result.is_some(), "should match caffeinate recipe");
    let text = result.unwrap();
    assert!(text.contains("caffeinate"), "should mention caffeinate: {}", text);
}

#[test]
fn test_how_do_i_prevent_sleep() {
    let result = recipe_query("how do i prevent sleep");
    assert!(result.is_some(), "should match caffeinate recipe");
    let text = result.unwrap();
    assert!(text.contains("caffeinate"), "should mention caffeinate: {}", text);
}

#[test]
fn test_how_do_i_keep_awake() {
    let result = recipe_query("how do i keep the computer awake");
    assert!(result.is_some(), "should match caffeinate recipe");
    let text = result.unwrap();
    assert!(text.contains("caffeinate"), "should mention caffeinate: {}", text);
}

#[test]
fn test_how_does_not_trigger_recipe() {
    // "how does filter work" should NOT be a recipe query
    let result = recipe_query("how does filter work");
    assert!(result.is_none(), "'how does X work' should not match recipe query");
}
