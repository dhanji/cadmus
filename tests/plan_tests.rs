use cadmus::plan::{
    compile_plan, execute_plan, run_plan,
    PlanError,
};
use cadmus::sexpr::parse_sexpr_to_plan;
use cadmus::fs_types::build_fs_registry;
use cadmus::fs_strategy::StepKind;
use std::path::PathBuf;

// ---------------------------------------------------------------------------
// End-to-end: load and run each example plan file
// ---------------------------------------------------------------------------

#[test]
fn test_plan_extract_cbz() {
    let path = PathBuf::from("data/plans/extract_cbz.sexp");
    let trace = run_plan(&path).unwrap();
    let display = trace.to_string();

    assert!(display.contains("extract_zip"), "trace: {}", display);
    assert!(display.contains("Seq(Entry(Name, File(Image)))"), "trace: {}", display);
    assert_eq!(trace.steps.len(), 2); // input + extract_archive
}

#[test]
fn test_plan_find_pdfs() {
    let path = PathBuf::from("data/plans/find_pdfs.sexp");
    let trace = run_plan(&path).unwrap();
    let display = trace.to_string();

    assert!(display.contains("list_dir"), "trace: {}", display);
    assert!(display.contains("find_matching"), "trace: {}", display);
    assert!(display.contains("sort_by"), "trace: {}", display);
    assert_eq!(trace.steps.len(), 4); // input + list_dir + find_matching + sort_by
}

#[test]
fn test_plan_find_large_files() {
    let path = PathBuf::from("data/plans/find_large_files.sexp");
    let trace = run_plan(&path).unwrap();
    let display = trace.to_string();

    assert!(display.contains("list_dir"), "trace: {}", display);
    assert!(display.contains("sort_by"), "trace: {}", display);
    assert_eq!(trace.steps.len(), 3); // input + list_dir + sort_by
}

// ---------------------------------------------------------------------------
// Plan with each mode
// ---------------------------------------------------------------------------

#[test]
fn test_plan_each_mode_produces_map_step() {
    let sexpr = r#"
(define (extract-and-read (archive : File))
  (extract_archive)
  (read_file :each))
"#;
    let def = parse_sexpr_to_plan(sexpr).unwrap();
    let registry = build_fs_registry();
    let compiled = compile_plan(&def, &registry).unwrap();
    let trace = execute_plan(&compiled, &registry).unwrap();

    let map_steps: Vec<_> = trace.steps.iter()
        .filter(|s| s.kind == StepKind::Map)
        .collect();
    assert_eq!(map_steps.len(), 1, "should have exactly 1 MAP step");
    assert!(map_steps[0].op_name.contains("read_file"));
}

// ---------------------------------------------------------------------------
// Error cases
// ---------------------------------------------------------------------------

#[test]
fn test_plan_unknown_op_error() {
    let sexpr = r#"
(define (bad (path : Dir))
  (totally_fake_op))
"#;
    let def = parse_sexpr_to_plan(sexpr).unwrap();
    let registry = build_fs_registry();
    let result = compile_plan(&def, &registry);
    assert!(result.is_err());
    match result.unwrap_err() {
        PlanError::UnknownOp { op, .. } => {
            assert_eq!(op, "totally_fake_op");
        }
        other => panic!("expected UnknownOp, got: {}", other),
    }
}

#[test]
fn test_plan_type_mismatch_error() {
    let sexpr = r#"
(define (bad-chain (path : Dir))
  (list_dir)
  (extract_archive))
"#;
    // list_dir produces Seq(Entry(Name, Bytes))
    // extract_archive takes File(Archive(...)) — should not unify
    let def = parse_sexpr_to_plan(sexpr).unwrap();
    let registry = build_fs_registry();
    let result = compile_plan(&def, &registry);
    assert!(result.is_err(), "extract_archive after list_dir should fail");
    match result.unwrap_err() {
        PlanError::TypeMismatch { step, op, .. } => {
            assert_eq!(step, 1);
            assert_eq!(op, "extract_archive");
        }
        other => panic!("expected TypeMismatch, got: {}", other),
    }
}

#[test]
fn test_plan_unknown_var_error() {
    let sexpr = r#"
(define (bad-var (path : Dir))
  (filter :pattern $nonexistent))
"#;
    let result = parse_sexpr_to_plan(sexpr);
    // The sexpr parser may or may not catch unknown vars at parse time.
    // If it parses OK, the plan compiler should catch it.
    match result {
        Err(_) => {} // sexpr parser caught it
        Ok(def) => {
            let registry = build_fs_registry();
            let result = compile_plan(&def, &registry);
            assert!(result.is_err(), "should fail with unknown var");
        }
    }
}

#[test]
fn test_plan_empty_steps_error() {
    let sexpr = r#"
(define (empty (path : Dir)))
"#;
    let result = parse_sexpr_to_plan(sexpr);
    assert!(result.is_err(), "empty plan should fail: {:?}", result);
}

#[test]
fn test_plan_missing_file_error() {
    let path = PathBuf::from("data/plans/nonexistent.sexp");
    let result = run_plan(&path);
    assert!(result.is_err());
}

// ---------------------------------------------------------------------------
// Compiled plan type threading
// ---------------------------------------------------------------------------

#[test]
fn test_plan_types_thread_correctly() {
    let sexpr = r#"
(define (chain-test (path : Dir))
  (list_dir)
  (filter :extension ".txt")
  (sort_by :key "name"))
"#;
    let def = parse_sexpr_to_plan(sexpr).unwrap();
    let registry = build_fs_registry();
    let compiled = compile_plan(&def, &registry).unwrap();

    // All steps should have Seq output types
    for step in &compiled.steps {
        assert!(
            step.output_type.to_string().contains("Seq"),
            "step {} ({}) output should be Seq, got: {}",
            step.index, step.op, step.output_type
        );
    }

    // Input type should be Dir(Bytes)
    assert_eq!(compiled.input_type.to_string(), "Dir(Bytes)");

    // Final output should be Seq(Entry(Name, Bytes))
    assert!(compiled.output_type.to_string().contains("Seq(Entry(Name, Bytes))"),
        "final output: {}", compiled.output_type);
}

// ---------------------------------------------------------------------------
// Var expansion in params
// ---------------------------------------------------------------------------

#[test]
fn test_plan_var_expansion_in_trace() {
    let sexpr = r#"
(define (search-with-keyword (path : Dir) (keyword : String))
  (list_dir)
  (find_matching :pattern $keyword))
"#;
    let def = parse_sexpr_to_plan(sexpr).unwrap();
    let registry = build_fs_registry();
    let compiled = compile_plan(&def, &registry).unwrap();
    let trace = execute_plan(&compiled, &registry).unwrap();

    // The find_matching step should have the $keyword reference in its hint
    let fm_step = trace.steps.iter()
        .find(|s| s.op_name == "find_matching")
        .expect("should have find_matching step");
    assert!(fm_step.command_hint.contains("$keyword"),
        "hint should contain var reference: {}", fm_step.command_hint);
}

// ---------------------------------------------------------------------------
// Plan examples (Phases 1-5)
// ---------------------------------------------------------------------------

#[test]
fn test_plan_copy_and_organize() {
    let path = PathBuf::from("data/plans/copy_and_organize.sexp");
    let trace = run_plan(&path).unwrap();
    let display = trace.to_string();

    assert!(display.contains("walk_tree") || display.contains("list_dir"),
        "should walk/list: {}", display);
    assert!(display.contains("sort_by"), "should sort: {}", display);
}

#[test]
fn test_plan_preview_log() {
    let path = PathBuf::from("data/plans/preview_log.sexp");
    let trace = run_plan(&path).unwrap();
    let display = trace.to_string();

    assert!(display.contains("walk_tree") || display.contains("find"),
        "should walk: {}", display);
    assert!(display.contains("sort_by"),
        "should sort: {}", display);
}

#[test]
fn test_plan_find_duplicates() {
    let path = PathBuf::from("data/plans/find_duplicates.sexp");
    let trace = run_plan(&path).unwrap();
    let display = trace.to_string();

    assert!(display.contains("walk_tree") || display.contains("find"),
        "should walk: {}", display);
    assert!(display.contains("sort_by"), "should sort: {}", display);
}

#[test]
fn test_plan_cleanup_temp() {
    let path = PathBuf::from("data/plans/cleanup_temp.sexp");
    let trace = run_plan(&path).unwrap();
    let display = trace.to_string();

    assert!(display.contains("walk_tree") || display.contains("find"),
        "should walk: {}", display);
    assert!(display.contains("sort_by"), "should sort: {}", display);
}

#[test]
fn test_plan_spotlight_find() {
    let path = PathBuf::from("data/plans/spotlight_find.sexp");
    let trace = run_plan(&path).unwrap();
    let display = trace.to_string();

    assert!(display.contains("spotlight_search") || display.contains("mdfind"),
        "should spotlight search: {}", display);
    assert!(display.contains("sort_by"), "should sort: {}", display);
}

#[test]
fn test_plan_download_and_extract() {
    let path = PathBuf::from("data/plans/download_and_extract.sexp");
    let trace = run_plan(&path).unwrap();
    let display = trace.to_string();

    assert!(display.contains("extract_archive") || display.contains("unzip"),
        "should extract: {}", display);
    assert!(display.contains("sort_by"), "should sort: {}", display);
}

// ---------------------------------------------------------------------------
// Power Tools Plan Tests
// ---------------------------------------------------------------------------

#[test]
fn test_plan_git_log_search() {
    let path = PathBuf::from("data/plans/git_log_search.sexp");
    let trace = run_plan(&path).unwrap();
    let display = trace.to_string();

    assert!(display.contains("git_log"), "should have git_log step: {}", display);
    assert!(display.contains("filter"), "should have filter step: {}", display);
    assert!(display.contains("sort_by"), "should have sort_by step: {}", display);
    // Variable expansion
    assert!(display.contains("$pattern"), "should have $pattern reference: {}", display);
}

#[test]
fn test_plan_process_logs() {
    let path = PathBuf::from("data/plans/process_logs.sexp");
    let trace = run_plan(&path).unwrap();
    let display = trace.to_string();

    assert!(display.contains("awk_extract"), "should have awk_extract step: {}", display);
    assert!(display.contains("sed_script"), "should have sed_script step: {}", display);
}
