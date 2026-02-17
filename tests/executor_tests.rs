// ---------------------------------------------------------------------------
// Executor Integration Tests
// ---------------------------------------------------------------------------
//
// Tests the full pipeline: workflow YAML → compile → generate_script → verify.
// Also tests real execution of safe commands.

use std::path::Path;

use reasoning_engine::executor::{
    generate_script, run_script, shell_quote,
    extract_archive_format, ExecutorError,
};
use reasoning_engine::fs_types::build_full_registry;
use reasoning_engine::type_expr::TypeExpr;
use reasoning_engine::workflow::{self, CompiledStep, CompiledWorkflow, WorkflowDef};
use reasoning_engine::nl::{self, NlResponse};
use reasoning_engine::nl::dialogue::DialogueState;

use std::collections::HashMap;

// ---------------------------------------------------------------------------
// Helper: load, compile, and generate script from a workflow YAML
// ---------------------------------------------------------------------------

fn script_for_workflow(yaml_path: &str) -> String {
    let def = workflow::load_workflow(Path::new(yaml_path))
        .unwrap_or_else(|e| panic!("failed to load {}: {}", yaml_path, e));
    let registry = build_full_registry();
    let compiled = workflow::compile_workflow(&def, &registry)
        .unwrap_or_else(|e| panic!("failed to compile {}: {}", yaml_path, e));
    generate_script(&compiled, &def)
        .unwrap_or_else(|e| panic!("failed to generate script for {}: {}", yaml_path, e))
}

// ===========================================================================
// Workflow → Script integration tests (all 11 YAML files)
// ===========================================================================

#[test]
fn test_script_find_pdfs() {
    let script = script_for_workflow("data/workflows/find_pdfs.yaml");
    assert!(script.starts_with("#!/bin/sh\n"));
    assert!(script.contains("set -e"));
    assert!(script.contains("WORK_DIR=$(mktemp -d)"));
    assert!(script.contains("trap cleanup EXIT"));
    // Step 1: list_dir → ls
    assert!(script.contains("ls ~/Documents"));
    // Step 2: find_matching → grep
    assert!(script.contains("grep"));
    assert!(script.contains(".pdf"));
    // Step 3: sort_by → sort
    assert!(script.contains("sort"));
    // Intermediate files
    assert!(script.contains("step_1.txt"));
    assert!(script.contains("step_2.txt"));
    assert!(script.contains("step_3.txt"));
}

#[test]
fn test_script_extract_cbz() {
    let script = script_for_workflow("data/workflows/extract_cbz.yaml");
    assert!(script.starts_with("#!/bin/sh\n"));
    // Single step — should be a standalone command, no temp files
    assert!(script.contains("unzip"));
    assert!(script.contains("comic.cbz"));
    // Single-step workflows don't use step_N.txt intermediates
    assert!(!script.contains("step_2.txt"));
}

#[test]
fn test_script_cleanup_temp() {
    let script = script_for_workflow("data/workflows/cleanup_temp.yaml");
    assert!(script.contains("find /tmp -type f"));
    assert!(script.contains("grep"));
    assert!(script.contains(".tmp"));
    assert!(script.contains("sort"));
    assert!(script.contains("step_1.txt"));
    assert!(script.contains("step_2.txt"));
    assert!(script.contains("step_3.txt"));
}

#[test]
fn test_script_download_and_extract() {
    let script = script_for_workflow("data/workflows/download_and_extract.yaml");
    assert!(script.contains("unzip") || script.contains("tar"));
    assert!(script.contains("archive.zip"));
    assert!(script.contains("sort"));
}

#[test]
fn test_script_copy_and_organize() {
    let script = script_for_workflow("data/workflows/copy_and_organize.yaml");
    assert!(script.contains("find ~/Downloads -type f"));
    assert!(script.contains("grep"));
    assert!(script.contains("sort"));
}

#[test]
fn test_script_find_duplicates() {
    let script = script_for_workflow("data/workflows/find_duplicates.yaml");
    assert!(script.contains("find ~/Documents -type f"));
    assert!(script.contains("sort"));
}

#[test]
fn test_script_find_large_files() {
    let script = script_for_workflow("data/workflows/find_large_files.yaml");
    assert!(script.contains("ls ~/Downloads"));
    assert!(script.contains("sort"));
}

#[test]
fn test_script_git_log_search() {
    let script = script_for_workflow("data/workflows/git_log_search.yaml");
    assert!(script.contains("git log"));
    assert!(script.contains("grep"));
    assert!(script.contains("fix(auth)"));
    assert!(script.contains("sort"));
}

#[test]
fn test_script_preview_log() {
    let script = script_for_workflow("data/workflows/preview_log.yaml");
    assert!(script.contains("find /var/log -type f"));
    assert!(script.contains("grep"));
    assert!(script.contains(".log"));
    assert!(script.contains("sort"));
}

#[test]
fn test_script_process_logs() {
    let script = script_for_workflow("data/workflows/process_logs.yaml");
    assert!(script.contains("awk"));
    assert!(script.contains("sed"));
    assert!(script.contains("/var/log/app.log"));
}

#[test]
fn test_script_spotlight_find() {
    let script = script_for_workflow("data/workflows/spotlight_find.yaml");
    assert!(script.contains("mdfind"));
    assert!(script.contains("sort"));
}

// ===========================================================================
// Script structure verification
// ===========================================================================

#[test]
fn test_script_has_shebang() {
    let script = script_for_workflow("data/workflows/find_pdfs.yaml");
    assert!(script.starts_with("#!/bin/sh\n"));
}

#[test]
fn test_script_has_set_e() {
    let script = script_for_workflow("data/workflows/find_pdfs.yaml");
    assert!(script.contains("\nset -e\n"));
}

#[test]
fn test_script_has_cleanup_trap() {
    let script = script_for_workflow("data/workflows/find_pdfs.yaml");
    assert!(script.contains("trap cleanup EXIT"));
    assert!(script.contains("cleanup()"));
    assert!(script.contains("rm -rf \"$WORK_DIR\""));
}

#[test]
fn test_script_has_work_dir() {
    let script = script_for_workflow("data/workflows/find_pdfs.yaml");
    assert!(script.contains("WORK_DIR=$(mktemp -d)"));
}

#[test]
fn test_script_intermediate_files_sequential() {
    let script = script_for_workflow("data/workflows/cleanup_temp.yaml");
    // 3-step workflow: step_1 → step_2 → step_3
    let step1_pos = script.find("step_1.txt").unwrap();
    let step2_pos = script.find("step_2.txt").unwrap();
    let step3_pos = script.find("step_3.txt").unwrap();
    assert!(step1_pos < step2_pos);
    assert!(step2_pos < step3_pos);
}

#[test]
fn test_script_final_output_cat() {
    let script = script_for_workflow("data/workflows/find_pdfs.yaml");
    // Last line should cat the final step's output
    let lines: Vec<&str> = script.lines().collect();
    let last_non_empty = lines.iter().rev().find(|l| !l.is_empty()).unwrap();
    assert!(last_non_empty.contains("cat") && last_non_empty.contains("step_3.txt"),
        "last line should cat final step output, got: {}", last_non_empty);
}

#[test]
fn test_script_step_comments() {
    let script = script_for_workflow("data/workflows/find_pdfs.yaml");
    assert!(script.contains("# Step 1: list_dir"));
    assert!(script.contains("# Step 2: find_matching"));
    assert!(script.contains("# Step 3: sort_by"));
}

#[test]
fn test_script_param_comments() {
    let script = script_for_workflow("data/workflows/find_pdfs.yaml");
    assert!(script.contains("#   params:"));
}

// ===========================================================================
// Real execution tests (safe commands only)
// ===========================================================================

#[test]
fn test_execute_ls_tmp() {
    let script = "#!/bin/sh\nls /tmp\n";
    let result = run_script(script).unwrap();
    assert_eq!(result.exit_code, 0);
    assert!(!result.stdout.is_empty(), "ls /tmp should produce output");
}

#[test]
fn test_execute_echo() {
    let script = "#!/bin/sh\necho hello_from_executor\n";
    let result = run_script(script).unwrap();
    assert_eq!(result.exit_code, 0);
    assert!(result.stdout.contains("hello_from_executor"));
}

#[test]
fn test_execute_failing_command() {
    let script = "#!/bin/sh\nset -e\nfalse\n";
    let result = run_script(script).unwrap();
    assert_ne!(result.exit_code, 0, "false should exit non-zero");
}

#[test]
fn test_execute_nonexistent_command() {
    let script = "#!/bin/sh\nthis_command_does_not_exist_xyz123\n";
    let result = run_script(script).unwrap();
    assert_ne!(result.exit_code, 0);
    assert!(!result.stderr.is_empty(), "should have stderr for missing command");
}

#[test]
fn test_execute_with_temp_files() {
    // Test the actual temp file pattern
    let script = r#"#!/bin/sh
set -e
WORK_DIR=$(mktemp -d)
echo "one" > "$WORK_DIR/step_1.txt"
echo "two" >> "$WORK_DIR/step_1.txt"
echo "three" >> "$WORK_DIR/step_1.txt"
sort "$WORK_DIR/step_1.txt" > "$WORK_DIR/step_2.txt"
cat "$WORK_DIR/step_2.txt"
rm -rf "$WORK_DIR"
"#;
    let result = run_script(script).unwrap();
    assert_eq!(result.exit_code, 0);
    let lines: Vec<&str> = result.stdout.trim().lines().collect();
    assert_eq!(lines, vec!["one", "three", "two"]);
}

#[test]
fn test_execute_captures_stderr() {
    let script = "#!/bin/sh\necho error_msg >&2\n";
    let result = run_script(script).unwrap();
    assert_eq!(result.exit_code, 0);
    assert!(result.stderr.contains("error_msg"));
}

// ===========================================================================
// NL → Script integration
// ===========================================================================

#[test]
fn test_nl_approve_generates_script() {
    let mut state = DialogueState::new();
    let _ = nl::process_input("zip up ~/Downloads", &mut state);
    let response = nl::process_input("lgtm", &mut state);
    match response {
        NlResponse::Approved { script } => {
            assert!(script.is_some(), "Approved should carry a script");
            let s = script.unwrap();
            assert!(s.starts_with("#!/bin/sh\n"));
            assert!(s.contains("zip") || s.contains("tar"));
        }
        other => panic!("expected Approved, got: {:?}", other),
    }
}

#[test]
fn test_nl_approve_without_plan_no_script() {
    let mut state = DialogueState::new();
    let response = nl::process_input("lgtm", &mut state);
    match response {
        NlResponse::NeedsClarification { needs } => {
            assert!(!needs.is_empty());
        }
        other => panic!("expected NeedsClarification, got: {:?}", other),
    }
}

#[test]
fn test_nl_find_pdfs_generates_script() {
    let mut state = DialogueState::new();
    let _ = nl::process_input("find all PDFs in ~/Documents", &mut state);
    let response = nl::process_input("yes", &mut state);
    match response {
        NlResponse::Approved { script } => {
            assert!(script.is_some(), "Approved should carry a script");
            let s = script.unwrap();
            assert!(s.contains("#!/bin/sh"));
        }
        other => panic!("expected Approved, got: {:?}", other),
    }
}

#[test]
fn test_nl_extract_cbz_generates_script() {
    let mut state = DialogueState::new();
    let _ = nl::process_input("extract comic.cbz", &mut state);
    let response = nl::process_input("ok", &mut state);
    match response {
        NlResponse::Approved { script } => {
            assert!(script.is_some(), "Approved should carry a script");
            let s = script.unwrap();
            assert!(s.contains("unzip"));
            assert!(s.contains("comic.cbz"));
        }
        other => panic!("expected Approved, got: {:?}", other),
    }
}

// ===========================================================================
// Error cases
// ===========================================================================

#[test]
fn test_script_generation_unknown_op_fails() {
    // Build a CompiledWorkflow with an unknown op
    let compiled = CompiledWorkflow {
        name: "test".to_string(),
        input_type: TypeExpr::prim("Bytes"),
        input_description: "/tmp".to_string(),
        steps: vec![CompiledStep {
            index: 0,
            op: "nonexistent_op_xyz".to_string(),
            is_each: false,
            input_type: TypeExpr::prim("Bytes"),
            output_type: TypeExpr::prim("Bytes"),
            params: HashMap::new(),
        }],
        output_type: TypeExpr::prim("Bytes"),
    };
    let def = WorkflowDef {
        workflow: "test".to_string(),
        inputs: {
            let mut m = HashMap::new();
            m.insert("path".to_string(), "/tmp".to_string());
            m
        },
        steps: vec![],
    };
    let result = generate_script(&compiled, &def);
    assert!(result.is_err());
    match result {
        Err(ExecutorError::UnknownOp(name)) => assert_eq!(name, "nonexistent_op_xyz"),
        other => panic!("expected UnknownOp, got: {:?}", other),
    }
}

#[test]
fn test_empty_workflow_generates_minimal_script() {
    let compiled = CompiledWorkflow {
        name: "empty".to_string(),
        input_type: TypeExpr::prim("Bytes"),
        input_description: ".".to_string(),
        steps: vec![],
        output_type: TypeExpr::prim("Bytes"),
    };
    let def = WorkflowDef {
        workflow: "empty".to_string(),
        inputs: HashMap::new(),
        steps: vec![],
    };
    let script = generate_script(&compiled, &def).unwrap();
    assert!(script.starts_with("#!/bin/sh\n"));
    assert!(script.contains("# (no steps)"));
}

// ===========================================================================
// Shell quoting edge cases
// ===========================================================================

#[test]
fn test_shell_quote_work_dir_expansion() {
    // $WORK_DIR paths should be double-quoted for variable expansion
    let quoted = shell_quote("$WORK_DIR/step_1.txt");
    assert!(quoted.contains("$WORK_DIR"), "should preserve $WORK_DIR");
    assert!(quoted.starts_with('"'), "should use double quotes for $WORK_DIR paths");
}

#[test]
fn test_shell_quote_normal_path() {
    assert_eq!(shell_quote("/usr/bin/ls"), "/usr/bin/ls");
    assert_eq!(shell_quote("file.txt"), "file.txt");
}

#[test]
fn test_shell_quote_path_with_spaces() {
    let quoted = shell_quote("/path/to/my file.txt");
    assert!(quoted.starts_with('\''), "should use single quotes for paths with spaces");
    assert!(quoted.contains("my file.txt"));
}

// ===========================================================================
// Archive format detection
// ===========================================================================

#[test]
fn test_archive_format_cbz() {
    let ty = TypeExpr::Constructor("File".into(), vec![
        TypeExpr::Constructor("Archive".into(), vec![
            TypeExpr::Constructor("File".into(), vec![TypeExpr::prim("Image")]),
            TypeExpr::prim("Cbz"),
        ]),
    ]);
    assert_eq!(extract_archive_format(&ty), Some("Cbz"));
}

#[test]
fn test_archive_format_tar_gz() {
    let ty = TypeExpr::Constructor("File".into(), vec![
        TypeExpr::Constructor("Archive".into(), vec![
            TypeExpr::prim("Bytes"),
            TypeExpr::prim("TarGz"),
        ]),
    ]);
    assert_eq!(extract_archive_format(&ty), Some("TarGz"));
}

#[test]
fn test_archive_format_none_for_text() {
    let ty = TypeExpr::Constructor("File".into(), vec![TypeExpr::prim("Text")]);
    assert_eq!(extract_archive_format(&ty), None);
}

#[test]
fn test_archive_format_none_for_primitive() {
    let ty = TypeExpr::prim("Bytes");
    assert_eq!(extract_archive_format(&ty), None);
}
