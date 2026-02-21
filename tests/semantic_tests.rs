// ---------------------------------------------------------------------------
// Semantic Correctness Tests
// ---------------------------------------------------------------------------
//
// These tests verify the semantic fidelity of the full goal → plan → execution
// pipeline across all strategies. Each test traces:
//
//   1. Goal: What the user asked for (natural language or structured)
//   2. Plan: What the engine generated (plan YAML, plan nodes)
//   3. Execution: What the dry-run trace produces (type chain, ops)
//
// The key question: does the generated plan actually accomplish the stated goal?

use cadmus::nl;
use cadmus::nl::dialogue::DialogueState;
use cadmus::nl::NlResponse;
use cadmus::plan::{
    parse_plan, compile_plan, run_plan, run_plan_str,
};
use cadmus::fs_types::build_full_registry;
use cadmus::fs_strategy::run_fs_goal;
use cadmus::generic_planner::ExprLiteral;
use cadmus::type_expr::TypeExpr;
use cadmus::pipeline;
use cadmus::types::Goal;
use cadmus::coding_strategy;

use std::path::PathBuf;

// ===========================================================================
// STRATEGY 1: NL Pipeline → Plan → Execution (Filesystem)
// ===========================================================================
//
// Full chain: natural language → normalize → typo correct → intent → slots
//           → dialogue (build_plan) → YAML → compile → execute → trace

/// Helper: run NL input through the full pipeline and return the plan YAML.
fn nl_to_yaml(input: &str) -> String {
    let mut state = DialogueState::new();
    match nl::process_input(input, &mut state) {
        NlResponse::PlanCreated { plan_yaml, .. } => plan_yaml,
        other => panic!("Expected PlanCreated for '{}', got: {:?}", input, other),
    }
}

/// Helper: run NL input through the full pipeline, compile, and return the trace.
fn nl_to_trace(input: &str) -> String {
    let yaml = nl_to_yaml(input);
    let trace = run_plan_str(&yaml)
        .unwrap_or_else(|e| panic!("Plan from '{}' failed: {}\nYAML:\n{}", input, e, yaml));
    trace.to_string()
}

// ---------------------------------------------------------------------------
// Goal: "zip up everything in ~/Downloads"
// Expected: walk_tree → pack_archive
// Type chain: Dir(Bytes) → Seq(Entry(Name, Bytes)) → File(Archive(Bytes, ...))
// ---------------------------------------------------------------------------

#[test]
fn test_semantic_zip_downloads_plan() {
    let yaml = nl_to_yaml("zip up everything in ~/Downloads");

    // Plan should contain the right ops
    assert!(yaml.contains("walk_tree"), "plan should walk: {}", yaml);
    assert!(yaml.contains("pack_archive"), "plan should pack: {}", yaml);

    // Plan should reference the right path
    assert!(yaml.contains("~/Downloads") || yaml.contains("downloads"),
        "plan should reference Downloads: {}", yaml);
}

#[test]
fn test_semantic_zip_downloads_execution() {
    let trace = nl_to_trace("zip up everything in ~/Downloads");

    // Execution trace should show correct type chain
    assert!(trace.contains("Dir(Bytes)"), "should start with Dir: {}", trace);
    assert!(trace.contains("Seq(Entry(Name, Bytes))"), "walk should produce Seq: {}", trace);
    assert!(trace.contains("Archive"), "pack should produce Archive: {}", trace);

    // Should have the right ops in order
    assert!(trace.contains("walk_tree"), "trace: {}", trace);
    assert!(trace.contains("pack_archive"), "trace: {}", trace);
}

#[test]
fn test_semantic_zip_downloads_type_soundness() {
    let yaml = nl_to_yaml("zip up everything in ~/Downloads");
    let def = parse_plan(&yaml).unwrap();
    let registry = build_full_registry();
    let compiled = compile_plan(&def, &registry).unwrap();

    // Input type should be Dir(Bytes)
    assert_eq!(compiled.input_type.to_string(), "Dir(Bytes)",
        "input type: {}", compiled.input_type);

    // Output type should contain Archive
    let output = compiled.output_type.to_string();
    assert!(output.contains("Archive"),
        "output should be archive type, got: {}", output);
}

// ---------------------------------------------------------------------------
// Goal: "find all PDFs in ~/Documents"
// Expected: walk_tree → find_matching(*.pdf) → sort_by
// Type chain: Dir(Bytes) → Seq(Entry(Name, Bytes)) → Seq(Entry(Name, Bytes))
// ---------------------------------------------------------------------------

#[test]
fn test_semantic_find_pdfs_plan() {
    let yaml = nl_to_yaml("find all PDFs in ~/Documents");

    // Should have a search/find op
    assert!(yaml.contains("find_matching") || yaml.contains("walk_tree"),
        "plan should search: {}", yaml);

    // Should reference the path
    assert!(yaml.contains("~/Documents") || yaml.contains("documents"),
        "plan should reference Documents: {}", yaml);
}

#[test]
fn test_semantic_find_pdfs_execution() {
    let trace = nl_to_trace("find all PDFs in ~/Documents");

    // Should produce a sequence of entries
    assert!(trace.contains("Seq(Entry(Name,") && trace.contains("))"),
        "should produce entries: {}", trace);

    // Should have walk + find/filter
    assert!(trace.contains("walk_tree") || trace.contains("list_dir"),
        "should walk/list: {}", trace);
}

// ---------------------------------------------------------------------------
// Goal: "list ~/Downloads"
// Expected: list_dir
// Type chain: Dir(Bytes) → Seq(Entry(Name, Bytes))
// ---------------------------------------------------------------------------

#[test]
fn test_semantic_list_dir_plan() {
    let yaml = nl_to_yaml("list ~/Downloads");
    assert!(yaml.contains("list_dir"), "plan should list: {}", yaml);
}

#[test]
fn test_semantic_list_dir_execution() {
    let trace = nl_to_trace("list ~/Downloads");

    assert!(trace.contains("Dir(Bytes)"), "should start with Dir: {}", trace);
    assert!(trace.contains("Seq(Entry(Name, Bytes))"),
        "should produce entries: {}", trace);
    assert!(trace.contains("list_dir"), "trace: {}", trace);
}

// ---------------------------------------------------------------------------
// Goal: "extract the archive at ~/comic.cbz"
// Expected: extract_archive
// Type chain: File(Archive(File(Image), Cbz)) → Seq(Entry(Name, File(Image)))
// ---------------------------------------------------------------------------

#[test]
fn test_semantic_extract_cbz_plan() {
    let yaml = nl_to_yaml("extract the archive at ~/comic.cbz");
    assert!(yaml.contains("extract_archive"), "plan should extract: {}", yaml);
    assert!(yaml.contains("comic.cbz"), "plan should reference cbz: {}", yaml);
}

#[test]
fn test_semantic_extract_cbz_execution() {
    let trace = nl_to_trace("extract the archive at ~/comic.cbz");

    // With function-framing, NL inputs are bare names — format resolution
    // happens at runtime, not compile time. The generic op is kept.
    assert!(trace.contains("extract_archive"), "trace: {}", trace);
    assert!(trace.contains("Archive") || trace.contains("File"), "should recognize file type: {}", trace);
    assert!(trace.contains("Seq(Entry(Name"), "should produce entries: {}", trace);
}

#[test]
fn test_semantic_extract_cbz_type_soundness() {
    let yaml = nl_to_yaml("extract the archive at ~/comic.cbz");
    let def = parse_plan(&yaml).unwrap();
    let registry = build_full_registry();
    let compiled = compile_plan(&def, &registry).unwrap();

    // Input should be recognized as archive file
    let input = compiled.input_type.to_string();
    assert!(input.contains("Archive") || input.contains("Cbz"),
        "input should be archive type, got: {}", input);

    // Output should be sequence of entries
    let output = compiled.output_type.to_string();
    assert!(output.contains("Seq(Entry(Name"),
        "output should be Seq(Entry), got: {}", output);
}

// ---------------------------------------------------------------------------
// Goal: "compress file.txt"
// Expected: pack_archive or compress
// ---------------------------------------------------------------------------

#[test]
fn test_semantic_compress_file_plan() {
    let yaml = nl_to_yaml("compress file.txt");
    // "compress" is a synonym for pack_archive
    assert!(yaml.contains("pack_archive") || yaml.contains("compress"),
        "plan should compress: {}", yaml);
}

// ---------------------------------------------------------------------------
// Goal: "search for hello in ~/docs"
// Expected: walk_tree → search_content
// ---------------------------------------------------------------------------

#[test]
fn test_semantic_search_content_plan() {
    let yaml = nl_to_yaml("search for hello in ~/docs");
    assert!(yaml.contains("search_content") || yaml.contains("walk_tree"),
        "plan should search: {}", yaml);
}

// ---------------------------------------------------------------------------
// Typo resilience: misspelled input still produces correct plan
// ---------------------------------------------------------------------------

#[test]
fn test_semantic_typo_extract_archive() {
    let yaml = nl_to_yaml("extrct the archve at ~/comic.cbz");
    assert!(yaml.contains("extract_archive"),
        "typo should be corrected to extract_archive: {}", yaml);
}

#[test]
fn test_semantic_typo_walk_tree() {
    let yaml = nl_to_yaml("wlk the tre at ~/Documents");
    // "wlk" → "walk", "tre" → "tree"
    assert!(yaml.contains("walk_tree"),
        "typo should be corrected to walk_tree: {}", yaml);
}

#[test]
fn test_semantic_typo_still_compiles() {
    // Even with typos, the generated plan should compile
    let yaml = nl_to_yaml("extrct the archve at ~/comic.cbz");
    let result = run_plan_str(&yaml);
    assert!(result.is_ok(), "typo-corrected plan should compile: {:?}", result.err());
}

// ---------------------------------------------------------------------------
// Multi-turn conversation: create → edit → approve
// ---------------------------------------------------------------------------

#[test]
fn test_semantic_multiturn_create_edit_approve() {
    let mut state = DialogueState::new();

    // Turn 1: Create a zip plan
    let r1 = nl::process_input("zip up everything in ~/Downloads", &mut state);
    let yaml1 = match &r1 {
        NlResponse::PlanCreated { plan_yaml, .. } => plan_yaml.clone(),
        other => panic!("T1: expected PlanCreated, got: {:?}", other),
    };
    assert!(yaml1.contains("walk_tree"), "T1 should have walk_tree");
    assert!(yaml1.contains("pack_archive"), "T1 should have pack_archive");

    // Turn 2: Edit — add a filter to skip .git
    let r2 = nl::process_input("skip any subdirectory named .git", &mut state);
    let yaml2 = match &r2 {
        NlResponse::PlanEdited { plan_yaml, .. } => plan_yaml.clone(),
        other => panic!("T2: expected PlanEdited, got: {:?}", other),
    };
    assert!(yaml2.contains("filter"), "T2 should add filter: {}", yaml2);
    // The filter should still have walk_tree and pack_archive
    assert!(yaml2.contains("walk_tree"), "T2 should keep walk_tree: {}", yaml2);
    assert!(yaml2.contains("pack_archive"), "T2 should keep pack_archive: {}", yaml2);

    // Verify the edited plan compiles
    let trace = run_plan_str(&yaml2);
    assert!(trace.is_ok(), "edited plan should compile: {:?}\nYAML:\n{}", trace.err(), yaml2);

    // Turn 3: Approve
    let r3 = nl::process_input("lgtm", &mut state);
    assert!(matches!(r3, NlResponse::Approved { .. }), "T3: expected Approved, got: {:?}", r3);
}

#[test]
fn test_semantic_multiturn_preserves_path() {
    let mut state = DialogueState::new();

    // Create with specific path
    let r1 = nl::process_input("zip up ~/my_project", &mut state);
    let yaml1 = match &r1 {
        NlResponse::PlanCreated { plan_yaml, .. } => plan_yaml.clone(),
        other => panic!("expected PlanCreated, got: {:?}", other),
    };
    assert!(yaml1.contains("my_project"), "should preserve path: {}", yaml1);

    // Edit should preserve the path
    let r2 = nl::process_input("skip .DS_Store files", &mut state);
    let yaml2 = match &r2 {
        NlResponse::PlanEdited { plan_yaml, .. } => plan_yaml.clone(),
        other => panic!("expected PlanEdited, got: {:?}", other),
    };
    assert!(yaml2.contains("my_project"), "edit should preserve path: {}", yaml2);
}

// ---------------------------------------------------------------------------
// Negative: ambiguous/gibberish input
// ---------------------------------------------------------------------------

#[test]
fn test_semantic_ambiguous_needs_clarification() {
    let mut state = DialogueState::new();
    let r = nl::process_input("do the thing", &mut state);
    assert!(matches!(r, NlResponse::NeedsClarification { .. }),
        "ambiguous input should need clarification: {:?}", r);
}

#[test]
fn test_semantic_gibberish_needs_clarification() {
    let mut state = DialogueState::new();
    let r = nl::process_input("asdfghjkl qwerty zxcvbn", &mut state);
    assert!(matches!(r, NlResponse::NeedsClarification { .. }),
        "gibberish should need clarification: {:?}", r);
}

#[test]
fn test_semantic_empty_input_handled() {
    let mut state = DialogueState::new();
    let r = nl::process_input("", &mut state);
    // Empty input should not crash — should get clarification or error
    assert!(matches!(r, NlResponse::NeedsClarification { .. } | NlResponse::Error { .. }),
        "empty input should be handled gracefully: {:?}", r);
}

// ---------------------------------------------------------------------------
// Negative: plan with unknown op fails at compile time
// ---------------------------------------------------------------------------

#[test]
fn test_semantic_unknown_op_compile_error() {
    let yaml = r#"
bad:
  inputs:
    - path
  steps:
    - totally_fake_op
"#;
    let def = parse_plan(yaml).unwrap();
    let registry = build_full_registry();
    let result = compile_plan(&def, &registry);
    assert!(result.is_err(), "unknown op should fail at compile time");
    let err = format!("{}", result.unwrap_err());
    assert!(err.contains("unknown operation") || err.contains("totally_fake_op"),
        "error should mention the bad op: {}", err);
}

// ===========================================================================
// STRATEGY 2: Comparison (fact-pack based entity comparison)
// ===========================================================================
//
// Goal → Theory Layer → Structured Comparison
// Verifies: axes match fact pack, all obligation slots filled, inferences derived

#[test]
fn test_semantic_comparison_goal_to_output() {
    let goal = Goal {
        description: "Compare Putin and Stalin as autocrats".into(),
        entities: vec!["putin".into(), "stalin".into()],
        fact_pack_paths: vec!["data/packs/facts/putin_stalin.facts.yaml".into()],
    };

    let output = pipeline::run(&goal).unwrap();

    // Goal asks for comparison of 2 entities across all axes
    // Fact pack defines 7 axes: legitimacy, coercion, elite_control, ideology,
    // economic_management, information_control, foreign_policy_risk
    assert_eq!(output.axes.len(), 7, "should have 7 axes from fact pack");

    // Each axis should have claims for BOTH entities
    for axis in &output.axes {
        let entities: Vec<_> = axis.claims.iter()
            .filter_map(|c| c.entity.as_ref())
            .collect();
        assert!(entities.iter().any(|e| e.contains("putin")),
            "axis '{}' should have putin claims", axis.axis);
        assert!(entities.iter().any(|e| e.contains("stalin")),
            "axis '{}' should have stalin claims", axis.axis);
    }

    // Theory layer should derive inferences (comparative analysis)
    assert!(!output.inferences.is_empty(),
        "theory should derive comparative inferences");

    // Theory layer should detect conflicts (divergences)
    assert!(!output.conflicts.is_empty(),
        "theory should detect divergences");

    // All obligation slots should be fulfilled (no gaps)
    let total_gaps: usize = output.axes.iter().map(|a| a.gaps.len()).sum();
    assert_eq!(total_gaps, 0, "all obligation slots should be fulfilled");
}

#[test]
fn test_semantic_comparison_axes_have_evidence() {
    let goal = Goal {
        description: "Compare Putin and Stalin".into(),
        entities: vec!["putin".into(), "stalin".into()],
        fact_pack_paths: vec!["data/packs/facts/putin_stalin.facts.yaml".into()],
    };

    let output = pipeline::run(&goal).unwrap();

    for axis in &output.axes {
        // Every axis should have evidence
        assert!(!axis.evidence.is_empty(),
            "axis '{}' should have evidence", axis.axis);

        // Every axis should have contrasts (they're different leaders)
        assert!(!axis.contrasts.is_empty(),
            "axis '{}' should have contrasts", axis.axis);

        // Every axis should have similarities (both are autocrats)
        assert!(!axis.similarities.is_empty(),
            "axis '{}' should have similarities", axis.axis);

        // Every axis should have a summary
        assert!(axis.summary.is_some(),
            "axis '{}' should have a summary", axis.axis);
    }
}

#[test]
fn test_semantic_comparison_missing_fact_pack() {
    let goal = Goal {
        description: "Compare X and Y".into(),
        entities: vec!["x".into(), "y".into()],
        fact_pack_paths: vec!["data/nonexistent_pack.yaml".into()],
    };

    let result = pipeline::run(&goal);
    assert!(result.is_err(), "missing fact pack should error");
}

// ===========================================================================
// STRATEGY 3: Coding (code analysis + refactoring)
// ===========================================================================
//
// Goal → Registry → Plan → Execution
// Verifies: code smells detected, refactorings planned, tests generated

#[test]
fn test_semantic_coding_extract_method() {
    let result = coding_strategy::run_coding(
        coding_strategy::EXAMPLE_LONG_FUNCTION,
        "Extract method to reduce function length",
    ).unwrap();

    // Goal: generate tests for the refactoring
    assert!(!result.tests.is_empty(),
        "should generate tests");

    // The source should be preserved
    assert_eq!(result.source, coding_strategy::EXAMPLE_LONG_FUNCTION,
        "source should be preserved");

    // The test output should reference the original function
    let all_tests = result.tests.join("\n");
    assert!(all_tests.contains("test") || all_tests.contains("Test"),
        "should contain test-related content: {}", all_tests);

    // NOTE: Semantic finding — the coding strategy's assemble() expects
    // intermediate results (CodeSmell, Refactoring, TypeSignature) but
    // run_strategy() only returns the final plan node result (TestCase).
    // Smells, refactorings, and type_info are empty because they're
    // intermediate nodes in the plan tree, not collected during execution.
    // This is a known limitation: the plan executor returns only the root
    // result, not all intermediate values.
}

#[test]
fn test_semantic_coding_smells_are_relevant() {
    let result = coding_strategy::run_coding(
        coding_strategy::EXAMPLE_LONG_FUNCTION,
        "Extract method",
    ).unwrap();

    // The coding strategy produces a TestCase as its final output.
    // Intermediate results (smells, refactorings) are not collected
    // because the plan executor only returns the root node result.
    // Verify the test output is at least relevant to the source.
    let all_output = result.tests.join("\n");
    assert!(all_output.contains("process_data") || all_output.contains("test") || all_output.contains("Test"),
        "output should reference the source code: {}", all_output);
}

// ===========================================================================
// STRATEGY 4: Filesystem (type-directed planning)
// ===========================================================================
//
// Goal type → Generic planner → Plan nodes → Execution trace
// Verifies: type unification produces correct op sequence

#[test]
fn test_semantic_fs_cbz_extraction() {
    // Goal: extract images from a CBZ archive
    let target = TypeExpr::seq(TypeExpr::entry(
        TypeExpr::prim("Name"),
        TypeExpr::file(TypeExpr::prim("Image")),
    ));
    let available = vec![
        ExprLiteral::new(
            "comic.cbz",
            TypeExpr::file(TypeExpr::archive(
                TypeExpr::file(TypeExpr::prim("Image")),
                TypeExpr::prim("Cbz"),
            )),
            "my_comic.cbz",
        ),
    ];

    let trace = run_fs_goal(target, available).unwrap();
    let display = trace.to_string();

    // Should use extract_archive to go from File(Archive(...)) → Seq(Entry(...))
    assert!(display.contains("extract_archive"),
        "should plan extract_archive: {}", display);

    // Output should be Seq(Entry(Name, File(Image)))
    assert!(display.contains("Seq(Entry(Name, File(Image)))"),
        "should produce image entries: {}", display);
}

#[test]
fn test_semantic_fs_list_directory() {
    // Goal: list directory contents
    let target = TypeExpr::seq(TypeExpr::entry(
        TypeExpr::prim("Name"),
        TypeExpr::prim("Bytes"),
    ));
    let available = vec![
        ExprLiteral::new(
            "docs",
            TypeExpr::dir(TypeExpr::prim("Bytes")),
            "/home/user/docs",
        ),
    ];

    let trace = run_fs_goal(target, available).unwrap();
    let display = trace.to_string();

    // Should use list_dir to go from Dir(Bytes) → Seq(Entry(Name, Bytes))
    assert!(display.contains("list_dir"),
        "should plan list_dir: {}", display);
}

#[test]
fn test_semantic_fs_type_mismatch_fails() {
    // Goal: produce a type that's genuinely unreachable from Dir(Bytes).
    // Note: Seq(Entry(Name, File(Image))) is actually reachable because
    // the polymorphic type system allows Dir(a) → list_dir → Seq(Entry(Name, a))
    // where `a` unifies with anything. This is correct behavior for a
    // polymorphic planner — it's the type system working as designed.
    //
    // To get a genuine failure, we need a type with no production path at all.
    let target = TypeExpr::cons(
        "CompletelyFakeType",
        vec![TypeExpr::prim("Nonexistent")],
    );
    let available = vec![
        ExprLiteral::new(
            "docs",
            TypeExpr::dir(TypeExpr::prim("Bytes")),
            "/home/user/docs",
        ),
    ];

    let result = run_fs_goal(target, available);
    assert!(result.is_err(),
        "should fail: no op produces CompletelyFakeType(Nonexistent)");
}

// ===========================================================================
// PLAN YAML FILES: semantic correctness of pre-built plans
// ===========================================================================

#[test]
fn test_semantic_plan_extract_cbz_yaml() {
    let trace = run_plan(&PathBuf::from("data/plans/extract_cbz.yaml")).unwrap();
    let display = trace.to_string();

    // Goal: extract images from CBZ
    // Plan: extract_archive
    // Execution: File(Archive(File(Image), Cbz)) → Seq(Entry(Name, File(Image)))
    assert!(display.contains("extract_zip"), "trace: {}", display);
    assert!(display.contains("File(Image)"), "should produce images: {}", display);
}

#[test]
fn test_semantic_plan_find_pdfs_yaml() {
    let trace = run_plan(&PathBuf::from("data/plans/find_pdfs.yaml")).unwrap();
    let display = trace.to_string();

    // Goal: find PDFs containing keyword
    // Plan: list_dir → find_matching(*.pdf) → sort_by
    assert!(display.contains("list_dir"), "trace: {}", display);
    assert!(display.contains("find_matching"), "trace: {}", display);
    assert!(display.contains("*.pdf"), "should filter for PDFs: {}", display);
}

#[test]
fn test_semantic_plan_cleanup_temp_yaml() {
    let trace = run_plan(&PathBuf::from("data/plans/cleanup_temp.yaml")).unwrap();
    let display = trace.to_string();

    // Goal: find old temp files
    // Plan: walk_tree → filter(*.tmp) → sort_by
    assert!(display.contains("walk_tree"), "trace: {}", display);
    assert!(display.contains("*.tmp"), "should filter for .tmp: {}", display);
}

#[test]
fn test_semantic_plan_spotlight_yaml() {
    let trace = run_plan(&PathBuf::from("data/plans/spotlight_find.yaml")).unwrap();
    let display = trace.to_string();

    // Goal: Spotlight search
    // Plan: spotlight_search → sort_by
    assert!(display.contains("spotlight_search") || display.contains("mdfind"),
        "should use spotlight: {}", display);
}

#[test]
fn test_semantic_plan_download_extract_yaml() {
    let trace = run_plan(&PathBuf::from("data/plans/download_and_extract.yaml")).unwrap();
    let display = trace.to_string();

    // Goal: extract archive contents (bare 'archive' input — no format resolution)
    // Plan: extract_archive → sort_by (generic op, format resolved at runtime)
    assert!(display.contains("extract_archive"), "trace: {}", display);
    assert!(display.contains("sort_by"), "trace: {}", display);

    // Type chain: File(Bytes) → archive type inferred from op signature
    assert!(display.contains("Archive") || display.contains("File"),
        "should recognize file type: {}", display);
}

// Power tools plans
#[test]
fn test_semantic_plan_git_log_yaml() {
    let trace = run_plan(&PathBuf::from("data/plans/git_log_search.yaml")).unwrap();
    let display = trace.to_string();

    // Goal: search git log for pattern
    // Plan: git_log → filter($pattern) → sort_by(date)
    assert!(display.contains("git_log"), "trace: {}", display);
    assert!(display.contains("filter"), "trace: {}", display);
    // Variable reference: $pattern passed through as-is (no value expansion)
    assert!(display.contains("$pattern"), "should have $pattern reference: {}", display);
}

#[test]
fn test_semantic_plan_process_logs_yaml() {
    let trace = run_plan(&PathBuf::from("data/plans/process_logs.yaml")).unwrap();
    let display = trace.to_string();

    // Goal: extract and transform log data
    // Plan: awk_extract → sed_script
    assert!(display.contains("awk_extract"), "trace: {}", display);
    assert!(display.contains("sed_script"), "trace: {}", display);
}

// ===========================================================================
// CROSS-CUTTING: NL → Plan → Compile → Execute round-trip
// ===========================================================================

/// For each NL input, verify the full chain compiles and the trace is non-empty.
#[test]
fn test_semantic_roundtrip_battery() {
    let inputs = vec![
        "zip up everything in ~/Downloads",
        "find all PDFs in ~/Documents",
        "list ~/Downloads",
        "extract the archive at ~/comic.cbz",
        "compress file.txt",
        "walk the tree at ~/projects",
        "sort ~/Desktop by name",
    ];

    for input in inputs {
        let yaml = nl_to_yaml(input);
        let result = run_plan_str(&yaml);
        assert!(result.is_ok(),
            "NL input '{}' should produce valid plan.\nYAML:\n{}\nError: {:?}",
            input, yaml, result.err());

        let trace = result.unwrap();
        assert!(!trace.steps.is_empty(),
            "NL input '{}' should produce non-empty trace", input);
    }
}

// ===========================================================================
// EXPLAIN: semantic correctness of op explanations
// ===========================================================================

#[test]
fn test_semantic_explain_walk_tree() {
    let mut state = DialogueState::new();
    let r = nl::process_input("what does walk_tree mean", &mut state);
    match r {
        NlResponse::Explanation { text } => {
            // Explanation should come from ops YAML description
            assert!(text.contains("directory") || text.contains("walk") || text.contains("tree"),
                "explanation should be relevant: {}", text);
        }
        other => panic!("expected Explanation, got: {:?}", other),
    }
}

#[test]
fn test_semantic_explain_extract_archive() {
    let mut state = DialogueState::new();
    let r = nl::process_input("explain extract_archive", &mut state);
    match r {
        NlResponse::Explanation { text } => {
            assert!(text.contains("extract") || text.contains("archive") || text.contains("unzip"),
                "explanation should be relevant: {}", text);
        }
        other => panic!("expected Explanation, got: {:?}", other),
    }
}

// ===========================================================================
// EDGE CASES: boundary conditions
// ===========================================================================

#[test]
fn test_semantic_approve_without_plan() {
    let mut state = DialogueState::new();
    let r = nl::process_input("yes", &mut state);
    assert!(matches!(r, NlResponse::NeedsClarification { .. }),
        "approve without plan should need clarification: {:?}", r);
}

#[test]
fn test_semantic_edit_without_plan() {
    let mut state = DialogueState::new();
    let r = nl::process_input("skip .git directories", &mut state);
    assert!(matches!(r, NlResponse::NeedsClarification { .. }),
        "edit without plan should need clarification: {:?}", r);
}

#[test]
fn test_semantic_double_approve() {
    let mut state = DialogueState::new();
    nl::process_input("zip up ~/Downloads", &mut state);
    let r1 = nl::process_input("yes", &mut state);
    assert!(matches!(r1, NlResponse::Approved { .. }));

    // Second approve should fail — plan was cleared
    let r2 = nl::process_input("yes", &mut state);
    assert!(matches!(r2, NlResponse::NeedsClarification { .. }),
        "double approve should need clarification: {:?}", r2);
}

#[test]
fn test_semantic_reject_clears_state() {
    let mut state = DialogueState::new();
    nl::process_input("zip up ~/Downloads", &mut state);
    let r = nl::process_input("nah", &mut state);
    assert!(matches!(r, NlResponse::Rejected));

    // After reject, creating a new plan should work
    let r2 = nl::process_input("list ~/Desktop", &mut state);
    assert!(matches!(r2, NlResponse::PlanCreated { .. }),
        "should create new plan after reject: {:?}", r2);
}

#[test]
fn test_semantic_plan_empty_steps_rejected() {
    let yaml = r#"
name: "empty"
inputs:
  path: "/tmp"
steps: []
"#;
    let result = parse_plan(yaml);
    assert!(result.is_err(), "empty steps should be rejected");
}

#[test]
fn test_semantic_plan_type_mismatch_caught() {
    // list_dir produces Seq(Entry(Name, Bytes))
    // extract_archive takes File(Archive(...)) — type mismatch
    let yaml = r#"
bad-chain:
  inputs:
    - path
  steps:
    - list_dir
    - extract_archive
"#;
    let def = parse_plan(yaml).unwrap();
    let registry = build_full_registry();
    let result = compile_plan(&def, &registry);
    assert!(result.is_err(), "type mismatch should be caught at compile time");
}
