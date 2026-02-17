
// ---------------------------------------------------------------------------
// Power Tools Workflow Tests
// ---------------------------------------------------------------------------

#[test]
fn test_workflow_git_log_search() {
    let path = PathBuf::from("data/workflows/git_log_search.yaml");
    let trace = run_workflow(&path).unwrap();
    let display = trace.to_string();

    assert!(display.contains("git_log"), "should have git_log step: {}", display);
    assert!(display.contains("filter"), "should have filter step: {}", display);
    assert!(display.contains("sort_by"), "should have sort_by step: {}", display);
    // Variable expansion
    assert!(display.contains("fix(auth)"), "should expand $pattern: {}", display);
}

#[test]
fn test_workflow_process_logs() {
    let path = PathBuf::from("data/workflows/process_logs.yaml");
    let trace = run_workflow(&path).unwrap();
    let display = trace.to_string();

    assert!(display.contains("awk_extract"), "should have awk_extract step: {}", display);
    assert!(display.contains("sed_script"), "should have sed_script step: {}", display);
}use cadmus::workflow::{
    parse_workflow, compile_workflow, execute_workflow, run_workflow,
    WorkflowError,
};
use cadmus::fs_types::build_fs_registry;
use cadmus::fs_strategy::StepKind;
use std::path::PathBuf;

// ---------------------------------------------------------------------------
// End-to-end: load and run each example workflow file
// ---------------------------------------------------------------------------

#[test]
fn test_workflow_extract_cbz() {
    let path = PathBuf::from("data/workflows/extract_cbz.yaml");
    let trace = run_workflow(&path).unwrap();
    let display = trace.to_string();

    assert!(display.contains("extract_archive"), "trace: {}", display);
    assert!(display.contains("Seq(Entry(Name, File(Image)))"), "trace: {}", display);
    assert_eq!(trace.steps.len(), 2); // input + extract_archive
}

#[test]
fn test_workflow_find_pdfs() {
    let path = PathBuf::from("data/workflows/find_pdfs.yaml");
    let trace = run_workflow(&path).unwrap();
    let display = trace.to_string();

    assert!(display.contains("list_dir"), "trace: {}", display);
    assert!(display.contains("find_matching"), "trace: {}", display);
    assert!(display.contains("sort_by"), "trace: {}", display);
    assert_eq!(trace.steps.len(), 4); // input + list_dir + find_matching + sort_by
}

#[test]
fn test_workflow_find_large_files() {
    let path = PathBuf::from("data/workflows/find_large_files.yaml");
    let trace = run_workflow(&path).unwrap();
    let display = trace.to_string();

    assert!(display.contains("list_dir"), "trace: {}", display);
    assert!(display.contains("sort_by"), "trace: {}", display);
    assert_eq!(trace.steps.len(), 3); // input + list_dir + sort_by
}

// ---------------------------------------------------------------------------
// Workflow with each mode
// ---------------------------------------------------------------------------

#[test]
fn test_workflow_each_mode_produces_map_step() {
    let yaml = r#"
workflow: "Extract and read"
inputs:
  archive: "photos.cbz"
steps:
  - extract_archive
  - read_file: each
"#;
    let def = parse_workflow(yaml).unwrap();
    let registry = build_fs_registry();
    let compiled = compile_workflow(&def, &registry).unwrap();
    let trace = execute_workflow(&compiled, &registry).unwrap();

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
fn test_workflow_unknown_op_error() {
    let yaml = r#"
workflow: "bad"
inputs:
  path: "/tmp"
steps:
  - totally_fake_op
"#;
    let def = parse_workflow(yaml).unwrap();
    let registry = build_fs_registry();
    let result = compile_workflow(&def, &registry);
    assert!(result.is_err());
    match result.unwrap_err() {
        WorkflowError::UnknownOp { op, .. } => {
            assert_eq!(op, "totally_fake_op");
        }
        other => panic!("expected UnknownOp, got: {}", other),
    }
}

#[test]
fn test_workflow_type_mismatch_error() {
    let yaml = r#"
workflow: "bad chain"
inputs:
  path: "~/docs"
steps:
  - list_dir
  - extract_archive
"#;
    // list_dir produces Seq(Entry(Name, Bytes))
    // extract_archive takes File(Archive(...)) — should not unify
    let def = parse_workflow(yaml).unwrap();
    let registry = build_fs_registry();
    let result = compile_workflow(&def, &registry);
    assert!(result.is_err(), "extract_archive after list_dir should fail");
    match result.unwrap_err() {
        WorkflowError::TypeMismatch { step, op, .. } => {
            assert_eq!(step, 1);
            assert_eq!(op, "extract_archive");
        }
        other => panic!("expected TypeMismatch, got: {}", other),
    }
}

#[test]
fn test_workflow_unknown_var_error() {
    let yaml = r#"
workflow: "bad var"
inputs:
  path: "/tmp"
steps:
  - filter:
      pattern: $nonexistent
"#;
    let result = parse_workflow(yaml);
    assert!(result.is_err());
    match result.unwrap_err() {
        WorkflowError::UnknownVar { var_name, .. } => {
            assert_eq!(var_name, "nonexistent");
        }
        other => panic!("expected UnknownVar, got: {}", other),
    }
}

#[test]
fn test_workflow_empty_steps_error() {
    let yaml = r#"
workflow: "empty"
inputs:
  path: "/tmp"
steps: []
"#;
    let result = parse_workflow(yaml);
    assert!(result.is_err());
    match result.unwrap_err() {
        WorkflowError::EmptySteps => {}
        other => panic!("expected EmptySteps, got: {}", other),
    }
}

#[test]
fn test_workflow_missing_file_error() {
    let path = PathBuf::from("data/workflows/nonexistent.yaml");
    let result = run_workflow(&path);
    assert!(result.is_err());
}

// ---------------------------------------------------------------------------
// Compiled workflow type threading
// ---------------------------------------------------------------------------

#[test]
fn test_workflow_types_thread_correctly() {
    let yaml = r#"
workflow: "Chain test"
inputs:
  path: "~/docs"
steps:
  - list_dir
  - filter:
      extension: ".txt"
  - sort_by: name
"#;
    let def = parse_workflow(yaml).unwrap();
    let registry = build_fs_registry();
    let compiled = compile_workflow(&def, &registry).unwrap();

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
fn test_workflow_var_expansion_in_trace() {
    let yaml = r#"
workflow: "Search with keyword"
inputs:
  path: "~/docs"
  keyword: "hello"
steps:
  - list_dir
  - find_matching:
      pattern: $keyword
"#;
    let def = parse_workflow(yaml).unwrap();
    let registry = build_fs_registry();
    let compiled = compile_workflow(&def, &registry).unwrap();
    let trace = execute_workflow(&compiled, &registry).unwrap();

    // The find_matching step should have the expanded keyword in its hint
    let fm_step = trace.steps.iter()
        .find(|s| s.op_name == "find_matching")
        .expect("should have find_matching step");
    assert!(fm_step.command_hint.contains("hello"),
        "hint should contain expanded var: {}", fm_step.command_hint);
}

// ---------------------------------------------------------------------------
// New workflow examples (Phases 1-5)
// ---------------------------------------------------------------------------

#[test]
fn test_workflow_copy_and_organize() {
    let path = PathBuf::from("data/workflows/copy_and_organize.yaml");
    let trace = run_workflow(&path).unwrap();
    let display = trace.to_string();

    assert!(display.contains("walk_tree") || display.contains("list_dir"),
        "should walk/list: {}", display);
    assert!(display.contains("sort_by"), "should sort: {}", display);
}

#[test]
fn test_workflow_preview_log() {
    let path = PathBuf::from("data/workflows/preview_log.yaml");
    let trace = run_workflow(&path).unwrap();
    let display = trace.to_string();

    assert!(display.contains("walk_tree") || display.contains("find"),
        "should walk: {}", display);
    assert!(display.contains("sort_by"),
        "should sort: {}", display);
}

#[test]
fn test_workflow_find_duplicates() {
    let path = PathBuf::from("data/workflows/find_duplicates.yaml");
    let trace = run_workflow(&path).unwrap();
    let display = trace.to_string();

    assert!(display.contains("walk_tree") || display.contains("find"),
        "should walk: {}", display);
    assert!(display.contains("sort_by"), "should sort: {}", display);
}

#[test]
fn test_workflow_cleanup_temp() {
    let path = PathBuf::from("data/workflows/cleanup_temp.yaml");
    let trace = run_workflow(&path).unwrap();
    let display = trace.to_string();

    assert!(display.contains("walk_tree") || display.contains("find"),
        "should walk: {}", display);
    assert!(display.contains("sort_by"), "should sort: {}", display);
}

#[test]
fn test_workflow_spotlight_find() {
    let path = PathBuf::from("data/workflows/spotlight_find.yaml");
    let trace = run_workflow(&path).unwrap();
    let display = trace.to_string();

    assert!(display.contains("spotlight_search") || display.contains("mdfind"),
        "should spotlight search: {}", display);
    assert!(display.contains("sort_by"), "should sort: {}", display);
}

#[test]
fn test_workflow_download_and_extract() {
    let path = PathBuf::from("data/workflows/download_and_extract.yaml");
    let trace = run_workflow(&path).unwrap();
    let display = trace.to_string();

    assert!(display.contains("extract_archive") || display.contains("unzip"),
        "should extract: {}", display);
    assert!(display.contains("sort_by"), "should sort: {}", display);
}
