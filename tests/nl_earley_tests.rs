// ===========================================================================
// Integration tests for the Earley NL pipeline.
//
// These tests exercise the full path:
//   user input → normalize → typo correct → Earley parse → Intent IR
//   → Intent Compiler → PlanDef → YAML → parse_plan → compile_plan
//
// They verify that the Earley parser produces valid, compilable plans
// and that the old pipeline fallback still works.
// ===========================================================================

use cadmus::nl::dialogue::DialogueState;
use cadmus::nl::{NlResponse, process_input};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Run input through process_input and assert PlanCreated.
/// Returns the plan YAML.
fn expect_plan(input: &str) -> String {
    let mut state = DialogueState::new();
    let response = process_input(input, &mut state);
    match response {
        NlResponse::PlanCreated { plan_yaml, .. } => {
            // Verify the YAML round-trips through parse + compile
            let parsed = cadmus::plan::parse_plan(&plan_yaml)
                .unwrap_or_else(|e| panic!(
                    "plan should parse for '{}': {:?}\nYAML:\n{}", input, e, plan_yaml
                ));
            let registry = cadmus::fs_types::build_full_registry();
            cadmus::plan::compile_plan(&parsed, &registry)
                .unwrap_or_else(|e| panic!(
                    "plan should compile for '{}': {:?}\nYAML:\n{}", input, e, plan_yaml
                ));
            plan_yaml
        }
        other => panic!("expected PlanCreated for '{}', got: {:?}", input, other),
    }
}

/// Run input and assert it produces PlanCreated containing all expected ops.
fn expect_plan_with_ops(input: &str, expected_ops: &[&str]) {
    let yaml = expect_plan(input);
    for op in expected_ops {
        assert!(
            yaml.contains(op),
            "plan for '{}' should contain '{}'\nYAML:\n{}", input, op, yaml
        );
    }
}

/// Run input and assert NeedsClarification.
fn expect_clarification(input: &str) {
    let mut state = DialogueState::new();
    let response = process_input(input, &mut state);
    assert!(
        matches!(response, NlResponse::NeedsClarification { .. }),
        "expected NeedsClarification for '{}', got: {:?}", input, response
    );
}

/// Full create → approve cycle, returns the generated script (if any).
fn create_and_approve(input: &str) -> Option<String> {
    let mut state = DialogueState::new();
    let r1 = process_input(input, &mut state);
    match r1 {
        NlResponse::PlanCreated { .. } => {
            let r2 = process_input("yes", &mut state);
            match r2 {
                NlResponse::Approved { script } => script,
                other => panic!("expected Approved for '{}', got: {:?}", input, other),
            }
        }
        other => panic!("expected PlanCreated for '{}', got: {:?}", input, other),
    }
}

// ===========================================================================
// Happy path: Earley-parsed commands produce valid plans
// ===========================================================================

#[test]
fn test_earley_find_comics_in_downloads_newest_first() {
    let yaml = expect_plan("find comics in my downloads folder newest first");
    assert!(yaml.contains("walk_tree"), "should have walk_tree:\n{}", yaml);
    assert!(yaml.contains("find_matching"), "should have find_matching:\n{}", yaml);
    assert!(yaml.contains("sort_by"), "should have sort_by:\n{}", yaml);
}

#[test]
fn test_earley_find_pdfs_in_documents() {
    expect_plan_with_ops("find pdfs in documents", &["walk_tree", "find_matching"]);
}

#[test]
fn test_earley_zip_up_downloads() {
    expect_plan_with_ops("zip up everything in downloads", &["walk_tree", "pack_archive"]);
}

#[test]
fn test_earley_extract_archive() {
    expect_plan_with_ops("extract ~/comic.cbz", &["extract_archive"]);
}

#[test]
fn test_earley_list_directory() {
    expect_plan_with_ops("list ~/Downloads", &["list_dir"]);
}

#[test]
fn test_earley_sort_files_newest_first() {
    expect_plan_with_ops("sort files newest first", &["sort_by"]);
}

#[test]
fn test_earley_compress_file() {
    // Single file compression uses gzip_compress (not pack_archive which is for directories)
    expect_plan_with_ops("compress ~/file.log", &["gzip_compress"]);
}

#[test]
fn test_earley_find_photos() {
    expect_plan_with_ops("find photos in ~/Pictures", &["walk_tree", "find_matching"]);
}

// ===========================================================================
// Alternative phrasings produce equivalent plans
// ===========================================================================

#[test]
fn test_phrasing_find_vs_locate_comics() {
    let yaml1 = expect_plan("find comics in downloads");
    let yaml2 = expect_plan("locate comics in downloads");
    // Both should have the same core ops
    assert!(yaml1.contains("walk_tree") && yaml1.contains("find_matching"));
    assert!(yaml2.contains("walk_tree") && yaml2.contains("find_matching"));
}

#[test]
fn test_phrasing_zip_vs_compress_folder() {
    let yaml1 = expect_plan("zip up ~/Projects");
    let yaml2 = expect_plan("compress ~/Projects");
    // Both should produce pack_archive
    assert!(yaml1.contains("pack_archive") || yaml1.contains("gzip_compress"),
        "zip should produce archive op:\n{}", yaml1);
    assert!(yaml2.contains("pack_archive") || yaml2.contains("gzip_compress"),
        "compress should produce archive op:\n{}", yaml2);
}

#[test]
fn test_phrasing_with_please() {
    // "please" is a filler word — should be ignored
    let yaml = expect_plan("please find comics in downloads");
    assert!(yaml.contains("walk_tree") && yaml.contains("find_matching"),
        "please should be ignored:\n{}", yaml);
}

#[test]
fn test_phrasing_with_determiner() {
    // "the" is a determiner — should be ignored
    let yaml = expect_plan("find the comics in downloads");
    assert!(yaml.contains("walk_tree") && yaml.contains("find_matching"),
        "determiner should be ignored:\n{}", yaml);
}

// ===========================================================================
// Negative: gibberish, empty, nonsense
// ===========================================================================

#[test]
fn test_earley_gibberish_needs_clarification() {
    expect_clarification("asdfghjkl qwerty zxcvbnm");
}

#[test]
fn test_earley_empty_input() {
    expect_clarification("");
}

#[test]
fn test_earley_only_fillers() {
    // "please" alone shouldn't produce a plan
    expect_clarification("please");
}

#[test]
fn test_earley_numbers_only() {
    expect_clarification("123 456 789");
}

// ===========================================================================
// Boundary: Earley fallback to old pipeline
// ===========================================================================

#[test]
fn test_fallback_search_content() {
    // "search" with a pattern — may use old pipeline or Earley
    let yaml = expect_plan("search for TODO in ~/Projects");
    assert!(yaml.contains("walk_tree") || yaml.contains("search_text") || yaml.contains("find_matching"),
        "search should produce some plan:\n{}", yaml);
}

#[test]
fn test_fallback_git_log() {
    // git commands go through old pipeline
    let yaml = expect_plan("git log");
    assert!(yaml.contains("git_log") || yaml.contains("shell_git"),
        "git log should produce git op:\n{}", yaml);
}

// ===========================================================================
// Multi-turn: create → edit → approve with Earley
// ===========================================================================

#[test]
fn test_earley_create_then_approve() {
    let mut state = DialogueState::new();

    // Create via Earley
    let r1 = process_input("find comics in downloads", &mut state);
    assert!(matches!(r1, NlResponse::PlanCreated { .. }),
        "should create plan: {:?}", r1);
    assert!(state.current_plan.is_some());

    // Approve
    let r2 = process_input("yes", &mut state);
    assert!(matches!(r2, NlResponse::Approved { .. }),
        "should approve: {:?}", r2);
    assert!(state.current_plan.is_none());
}

#[test]
fn test_earley_create_then_reject() {
    let mut state = DialogueState::new();

    let r1 = process_input("find comics in downloads", &mut state);
    assert!(matches!(r1, NlResponse::PlanCreated { .. }));

    let r2 = process_input("nah", &mut state);
    assert!(matches!(r2, NlResponse::Rejected));
    assert!(state.current_plan.is_none());
    assert!(state.alternative_intents.is_empty(),
        "reject should clear alternatives");
}

#[test]
fn test_earley_create_edit_approve() {
    let mut state = DialogueState::new();

    // Create
    let r1 = process_input("zip up everything in ~/Downloads", &mut state);
    assert!(matches!(r1, NlResponse::PlanCreated { .. }));

    // Edit (uses old pipeline pattern matching)
    let r2 = process_input("skip any subdirectory named .git", &mut state);
    match &r2 {
        NlResponse::PlanEdited { plan_yaml, .. } => {
            assert!(plan_yaml.contains("filter"),
                "edit should add filter:\n{}", plan_yaml);
        }
        other => panic!("expected PlanEdited, got: {:?}", other),
    }

    // Approve
    let r3 = process_input("lgtm", &mut state);
    assert!(matches!(r3, NlResponse::Approved { .. }));
}

#[test]
fn test_earley_replace_plan() {
    let mut state = DialogueState::new();

    // Create first plan
    let r1 = process_input("find comics in downloads", &mut state);
    assert!(matches!(r1, NlResponse::PlanCreated { .. }));

    // Create a different plan (replaces the first)
    let r2 = process_input("zip up ~/Projects", &mut state);
    assert!(matches!(r2, NlResponse::PlanCreated { .. }));

    // The current plan should be the zip one
    if let NlResponse::PlanCreated { plan_yaml, .. } = r2 {
        assert!(plan_yaml.contains("pack_archive") || plan_yaml.contains("gzip_compress"),
            "should be zip plan:\n{}", plan_yaml);
    }
}

// ===========================================================================
// Regression: existing scenarios still work
// ===========================================================================

#[test]
fn test_regression_zip_up_downloads_has_walk_and_pack() {
    // This is the canonical test from the old pipeline
    let yaml = expect_plan("zip up everything in my downloads");
    assert!(yaml.contains("walk_tree"), "should have walk_tree:\n{}", yaml);
    assert!(yaml.contains("pack_archive"), "should have pack_archive:\n{}", yaml);
}

#[test]
fn test_regression_list_desktop() {
    let yaml = expect_plan("list desktop");
    assert!(yaml.contains("list_dir"), "should have list_dir:\n{}", yaml);
}

#[test]
fn test_regression_compress_file_txt() {
    let yaml = expect_plan("compress file.txt");
    // Should produce some compression op
    assert!(yaml.contains("pack_archive") || yaml.contains("gzip_compress"),
        "should have compression op:\n{}", yaml);
}

#[test]
fn test_regression_extract_cbz() {
    let yaml = expect_plan("extract ~/comic.cbz");
    assert!(yaml.contains("extract_archive"),
        "should have extract_archive:\n{}", yaml);
}

#[test]
fn test_regression_explain_still_works() {
    let mut state = DialogueState::new();
    let response = process_input("explain walk_tree", &mut state);
    assert!(matches!(response, NlResponse::Explanation { .. }),
        "explain should still work: {:?}", response);
}

#[test]
fn test_regression_approve_without_plan() {
    let mut state = DialogueState::new();
    let response = process_input("approve", &mut state);
    assert!(matches!(response, NlResponse::NeedsClarification { .. }),
        "approve without plan should clarify: {:?}", response);
}

// ===========================================================================
// Full end-to-end: create → approve → script generation
// ===========================================================================

#[test]
fn test_e2e_earley_find_comics_produces_script() {
    let script = create_and_approve("find comics in my downloads folder newest first");
    // Script may or may not be generated depending on Racket codegen support,
    // but the pipeline should complete without panic
    if let Some(s) = &script {
        assert!(!s.is_empty(), "script should not be empty");
    }
}

#[test]
fn test_e2e_earley_zip_produces_script() {
    let script = create_and_approve("zip up ~/Projects");
    if let Some(s) = &script {
        assert!(!s.is_empty(), "script should not be empty");
    }
}

#[test]
fn test_e2e_earley_extract_produces_script() {
    let script = create_and_approve("extract ~/archive.tar.gz");
    if let Some(s) = &script {
        assert!(!s.is_empty(), "script should not be empty");
    }
}

// ===========================================================================
// DialogueState: alternative_intents tracking
// ===========================================================================

#[test]
fn test_alternatives_stored_in_state() {
    let mut state = DialogueState::new();
    let r = process_input("find comics in downloads", &mut state);
    assert!(matches!(r, NlResponse::PlanCreated { .. }));
    // Alternatives may or may not be present depending on parse ambiguity,
    // but the field should exist and not panic
    let _ = state.alternative_intents.len();
}

#[test]
fn test_alternatives_cleared_on_approve() {
    let mut state = DialogueState::new();
    let _ = process_input("find comics in downloads", &mut state);
    let _ = process_input("yes", &mut state);
    assert!(state.alternative_intents.is_empty(),
        "alternatives should be cleared after approve");
}

#[test]
fn test_alternatives_cleared_on_reject() {
    let mut state = DialogueState::new();
    let _ = process_input("find comics in downloads", &mut state);
    let _ = process_input("nah", &mut state);
    assert!(state.alternative_intents.is_empty(),
        "alternatives should be cleared after reject");
}

// ===========================================================================
// Typo correction feeds into Earley parser
// ===========================================================================

#[test]
fn test_typo_correction_earley() {
    // "fnd" should be corrected to "find" before Earley parsing
    let mut state = DialogueState::new();
    let response = process_input("fnd comics in downloads", &mut state);
    // Should produce a plan (either via Earley or fallback)
    assert!(
        matches!(response, NlResponse::PlanCreated { .. }),
        "typo-corrected input should produce plan: {:?}", response
    );
}

// ===========================================================================
// Plan YAML format: function-framing
// ===========================================================================

#[test]
fn test_earley_plan_yaml_has_function_framing() {
    let yaml = expect_plan("find comics in downloads");
    // Function-framing: plan name as top-level key
    let lines: Vec<&str> = yaml.lines().collect();
    assert!(!lines.is_empty());
    // First line should be "plan-name:" (no indentation)
    let first = lines[0].trim();
    assert!(first.ends_with(':'), "first line should be plan name: {}", first);
    assert!(!first.starts_with(' '), "first line should not be indented: {}", first);
}

#[test]
fn test_earley_plan_yaml_has_inputs() {
    let yaml = expect_plan("find comics in downloads");
    assert!(yaml.contains("inputs:"), "should have inputs section:\n{}", yaml);
    assert!(yaml.contains("path"), "should have path input:\n{}", yaml);
}

#[test]
fn test_earley_plan_yaml_has_steps() {
    let yaml = expect_plan("find comics in downloads");
    assert!(yaml.contains("steps:"), "should have steps section:\n{}", yaml);
}
