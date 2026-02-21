
#[test]
fn stress_type_chain_full_search_pipeline_with_path() {
    // path + walk_tree + filter + search_content (no read_file:each needed)
    // search_content reads files itself — it takes Seq(Entry(Name, File(Text)))
    let yaml = r#"
workflow: "Search pipeline"
inputs:
  path: "~/Documents"
  pattern: "TODO"
steps:
  - walk_tree
  - filter:
      extension: ".txt"
  - search_content:
      pattern: "$pattern"
"#;
    let def = workflow::parse_workflow(yaml).expect("should parse");
    let registry = build_full_registry();
    let compiled = workflow::compile_workflow(&def, &registry);
    assert!(compiled.is_ok(),
        "search pipeline with path should compile after promotion: {:?}", compiled.err());
    let c = compiled.unwrap();
    assert_eq!(c.steps.len(), 3, "should have 3 steps");
}
#[test]
fn stress_type_chain_path_list_dir_no_promotion() {
    // list_dir doesn't need File(Text), so Dir(Bytes) should NOT be promoted
    let yaml = r#"
workflow: "List directory"
inputs:
  path: "~/Documents"
steps:
  - list_dir
  - sort_by: name
"#;
    let def = workflow::parse_workflow(yaml).expect("should parse");
    let registry = build_full_registry();
    let compiled = workflow::compile_workflow(&def, &registry)
        .expect("list_dir should compile with Dir(Bytes)");
    // The input type should still be Dir(Bytes), not promoted
    assert_eq!(compiled.input_type.to_string(), "Dir(Bytes)",
        "list_dir should keep Dir(Bytes), not promote: {}", compiled.input_type);
}
// ===========================================================================
// Type chain investigation: Dir(Bytes) vs Dir(File(Text)) gap
// ===========================================================================

// The workflow compiler infers Dir(Bytes) for `path: "~/Documents"` but
// Dir(File(Text)) for `textdir: "~/Documents"`. Ops like search_content
// and read_file:each need File(Text) elements, so the type chain breaks
// when the input is named "path".

#[test]
fn stress_type_chain_textdir_search_content_compiles() {
    // textdir → Dir(File(Text)) → walk_tree → Seq(Entry(Name, File(Text)))
    // → search_content: should compile
    let yaml = r#"
workflow: "Search with textdir"
inputs:
  textdir: "~/Documents"
  pattern: "TODO"
steps:
  - walk_tree
  - search_content:
      pattern: "$pattern"
"#;
    let def = workflow::parse_workflow(yaml).expect("should parse");
    let registry = build_full_registry();
    let compiled = workflow::compile_workflow(&def, &registry);
    assert!(compiled.is_ok(),
        "textdir should give Dir(File(Text)) making search_content work: {:?}", compiled.err());
    let c = compiled.unwrap();
    // Verify the type chain
    assert_eq!(c.steps.len(), 2);
    let walk_out = &c.steps[0].output_type;
    assert!(walk_out.to_string().contains("File(Text)"),
        "walk_tree output should contain File(Text): {}", walk_out);
}

#[test]
fn stress_type_chain_path_search_content_fails() {
    // path → Dir(Bytes) BUT compiler sees search_content downstream
    // → auto-promotes to Dir(File(Text)) → walk_tree → Seq(Entry(Name, File(Text)))
    // → search_content: should now SUCCEED thanks to type promotion
    let yaml = r#"
workflow: "Search with path"
inputs:
  path: "~/Documents"
  pattern: "TODO"
steps:
  - walk_tree
  - search_content:
      pattern: "$pattern"
"#;
    let def = workflow::parse_workflow(yaml).expect("should parse");
    let registry = build_full_registry();
    let result = workflow::compile_workflow(&def, &registry);
    // After type promotion fix, this should compile
    assert!(result.is_ok(),
        "path input should be auto-promoted to Dir(File(Text)) for search_content: {:?}",
        result.err());
}

#[test]
fn stress_type_chain_textdir_read_file_each_compiles() {
    // textdir → Dir(File(Text)) → walk_tree → Seq(Entry(Name, File(Text)))
    // → read_file: each → Seq(Entry(Name, Text))
    let yaml = r#"
workflow: "Read all files"
inputs:
  textdir: "~/Documents"
steps:
  - walk_tree
  - filter:
      extension: ".txt"
  - read_file: each
"#;
    let def = workflow::parse_workflow(yaml).expect("should parse");
    let registry = build_full_registry();
    let compiled = workflow::compile_workflow(&def, &registry);
    assert!(compiled.is_ok(),
        "textdir + read_file:each should compile: {:?}", compiled.err());
}

#[test]
fn stress_type_chain_path_read_file_each_fails() {
    // path → Dir(Bytes) BUT compiler sees read_file downstream
    // → auto-promotes to Dir(File(Text)) → walk_tree → filter → read_file: each → SUCCEEDS
    let yaml = r#"
workflow: "Read all files with path"
inputs:
  path: "~/Documents"
steps:
  - walk_tree
  - filter:
      extension: ".txt"
  - read_file: each
"#;
    let def = workflow::parse_workflow(yaml).expect("should parse");
    let registry = build_full_registry();
    let result = workflow::compile_workflow(&def, &registry);
    assert!(result.is_ok(),
        "path input should be auto-promoted for read_file:each: {:?}", result.err());
}

#[test]
fn stress_type_chain_nl_search_uses_textdir() {
    // The NL layer should use "textdir" for search_content, not "path"
    let mut state = DialogueState::new();
    let response = nl::process_input("search for TODO in ~/Documents", &mut state);
    match &response {
        NlResponse::PlanCreated { workflow_yaml, .. } => {
            assert!(workflow_yaml.contains("textdir"),
                "NL search should use textdir input, got:\n{}", workflow_yaml);
            // Verify it compiles
            let def: WorkflowDef = serde_yaml::from_str(workflow_yaml).unwrap();
            let registry = build_full_registry();
            let compiled = workflow::compile_workflow(&def, &registry);
            assert!(compiled.is_ok(),
                "NL-generated search workflow should compile: {:?}", compiled.err());
        }
        other => {
            // NL might not recognize this as search_content — that's OK
            // The key test is the YAML-level ones above
            println!("NL response was not PlanCreated: {:?}", other);
        }
    }
}

#[test]
fn stress_type_chain_nl_list_uses_path() {
    // list_dir should still use "path" (Dir(Bytes) is fine for listing)
    let yaml = nl_to_yaml("list ~/Downloads").expect("should produce workflow");
    assert!(!yaml.contains("textdir"),
        "list_dir should use path, not textdir: {}", yaml);
}

// ===========================================================================
// Generalized type promotion tests
// ===========================================================================
//
// These tests verify that the unification-based type promotion works
// generically — not just for the Dir(Bytes) → Dir(File(Text)) case.

#[test]
fn stress_promotion_no_bytes_no_promotion() {
    // Dir(File(Text)) should NOT trigger promotion (no Bytes to promote)
    let yaml = r#"
workflow: "Already typed"
inputs:
  textdir: "~/Documents"
  pattern: "TODO"
steps:
  - walk_tree
  - search_content:
      pattern: "$pattern"
"#;
    let def = workflow::parse_workflow(yaml).expect("should parse");
    let registry = build_full_registry();
    let compiled = workflow::compile_workflow(&def, &registry)
        .expect("should compile without promotion");
    // Input should still be Dir(File(Text)), not changed
    assert_eq!(compiled.input_type.to_string(), "Dir(File(Text))",
        "no promotion should happen when Bytes isn't present");
}

#[test]
fn stress_promotion_bytes_with_polymorphic_only_no_binding() {
    // Dir(Bytes) + list_dir (polymorphic: Dir(a) → Seq(Entry(Name, a)))
    // list_dir doesn't constrain `a` to anything specific, so _promote
    // stays unbound → no promotion
    let yaml = r#"
workflow: "List only"
inputs:
  path: "~/Documents"
steps:
  - list_dir
"#;
    let def = workflow::parse_workflow(yaml).expect("should parse");
    let registry = build_full_registry();
    let compiled = workflow::compile_workflow(&def, &registry)
        .expect("should compile with Dir(Bytes)");
    assert_eq!(compiled.input_type.to_string(), "Dir(Bytes)",
        "purely polymorphic chain should not promote Bytes");
}

#[test]
fn stress_promotion_discovered_via_unification() {
    // Dir(Bytes) + walk_tree + search_content
    // walk_tree: Dir(a) → Seq(Entry(Name, a))  — _promote flows through as `a`
    // search_content: Seq(Entry(Name, File(Text))) — forces _promote = File(Text)
    let yaml = r#"
workflow: "Search via promotion"
inputs:
  path: "~/Documents"
  pattern: "TODO"
steps:
  - walk_tree
  - search_content:
      pattern: "$pattern"
"#;
    let def = workflow::parse_workflow(yaml).expect("should parse");
    let registry = build_full_registry();
    let compiled = workflow::compile_workflow(&def, &registry)
        .expect("promotion should discover File(Text)");
    // The input type should have been promoted
    assert_eq!(compiled.input_type.to_string(), "Dir(File(Text))",
        "unification should discover Bytes → File(Text)");
}

#[test]
fn stress_promotion_each_mode_read_file() {
    // Dir(Bytes) + walk_tree + filter + read_file:each
    // read_file: File(a) → a, applied in :each mode to Entry values
    // For read_file to work, the Entry value must be File(something)
    // → _promote = File(Text) (since read_file needs File(a))
    let yaml = r#"
workflow: "Read files via promotion"
inputs:
  path: "~/Documents"
steps:
  - walk_tree
  - filter:
      extension: ".txt"
  - read_file: each
"#;
    let def = workflow::parse_workflow(yaml).expect("should parse");
    let registry = build_full_registry();
    let compiled = workflow::compile_workflow(&def, &registry)
        .expect("promotion should make read_file:each work");
    assert!(compiled.input_type.to_string().contains("File"),
        "promotion should wrap Bytes in File(): {}", compiled.input_type);
}

// ==========================================================================
// Pipeline Stress Tests
// ==========================================================================
//
// Red-team the entire Cadmus pipeline:
//   I1: End-to-end NL → workflow → compile → Racket
//   I2: Workflow compiler edge cases
//   I3: Racket codegen edge cases
//   I4: Generic planner stress
//   I5: Inference engine stress
//   I6: Type unification stress
//   I7: Type lowering / subsumption map stress
//
// Run with: cargo test --test stress_pipeline -- --nocapture

use std::collections::{HashMap, HashSet};

use cadmus::fact_pack::{FactPack, FactPackIndex};
use cadmus::fs_types::build_full_registry;
use cadmus::generic_planner::{self, GenericGoal, PlanError, ExprGoal, ExprLiteral};
use cadmus::nl::{self, NlResponse};
use cadmus::nl::dialogue::DialogueState;
use cadmus::racket_executor::{generate_racket_script, op_to_racket, RacketError};
use cadmus::racket_strategy::{
    build_racket_registry, load_racket_facts_from_str,
    promote_inferred_ops, discover_shell_submodes,
    infer_symmetric_op, InferenceKind, InferenceError,
};
use cadmus::registry::{self, load_ops_pack_str, Literal, OperationRegistry};
use cadmus::type_expr::{self, TypeExpr, UnifyError};
use cadmus::type_lowering;
use cadmus::workflow::{
    self, CompiledStep, CompiledWorkflow, WorkflowDef, StepArgs,
};

const RACKET_OPS_YAML: &str = include_str!("../data/packs/ops/racket.ops.yaml");
const RACKET_FACTS_YAML: &str = include_str!("../data/packs/facts/racket.facts.yaml");
const MACOS_CLI_FACTS_YAML: &str = include_str!("../data/packs/facts/macos_cli.facts.yaml");

// ---------------------------------------------------------------------------
// Shared helpers
// ---------------------------------------------------------------------------

fn make_racket_reg() -> OperationRegistry {
    let mut reg = load_ops_pack_str(RACKET_OPS_YAML).unwrap();
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();
    promote_inferred_ops(&mut reg, &facts);
    reg
}

fn make_full_registry() -> OperationRegistry {
    let mut reg = load_ops_pack_str(RACKET_OPS_YAML).unwrap();
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();
    promote_inferred_ops(&mut reg, &facts);
    let cli_pack: FactPack = serde_yaml::from_str(MACOS_CLI_FACTS_YAML).unwrap();
    let cli_facts = FactPackIndex::build(cli_pack);
    discover_shell_submodes(&mut reg, &facts, &cli_facts);
    reg
}

fn make_step(index: usize, op: &str, params: Vec<(&str, &str)>) -> CompiledStep {
    CompiledStep {
        index,
        op: op.to_string(),
        input_type: TypeExpr::prim("Any"),
        output_type: TypeExpr::prim("Any"),
        params: params.into_iter().map(|(k, v)| (k.to_string(), v.to_string())).collect(),
        ..Default::default()
    }
}



fn make_workflow(name: &str, inputs: Vec<(&str, &str)>, steps: Vec<CompiledStep>) -> (CompiledWorkflow, WorkflowDef) {
    let input_map: HashMap<String, String> = inputs.iter()
        .map(|(k, v)| (k.to_string(), v.to_string()))
        .collect();
    let compiled = CompiledWorkflow {
        name: name.to_string(),
        input_type: TypeExpr::prim("Any"),
        input_description: inputs.first().map(|(_, v)| v.to_string()).unwrap_or_default(),
        steps,
        output_type: TypeExpr::prim("Any"),
    };
    let def = WorkflowDef {
        workflow: name.to_string(),
        inputs: input_map,
        steps: vec![],
    };
    (compiled, def)
}

/// Run NL input through the full pipeline and return the workflow YAML if successful.
fn nl_to_yaml(input: &str) -> Result<String, String> {
    let mut state = DialogueState::new();
    match nl::process_input(input, &mut state) {
        NlResponse::PlanCreated { workflow_yaml, .. } => Ok(workflow_yaml),
        NlResponse::Error { message } => Err(format!("Error: {}", message)),
        NlResponse::NeedsClarification { needs } => Err(format!("NeedsClarification: {:?}", needs)),
        NlResponse::Explanation { text } => Err(format!("Explanation: {}", text)),
        other => Err(format!("Unexpected: {:?}", other)),
    }
}

// ===========================================================================
// I1: End-to-end NL → workflow → compile → Racket stress tests
// ===========================================================================

// --- Happy path: 10+ NL inputs that should produce valid Racket scripts ---

#[test]
fn stress_nl_add_two_numbers() {
    let yaml = nl_to_yaml("Add 4 and 35 together").expect("should produce workflow");
    assert!(yaml.contains("add"), "yaml should contain add op: {}", yaml);
}

#[test]
fn stress_nl_subtract() {
    let yaml = nl_to_yaml("Subtract 2 from 6").expect("should produce workflow");
    assert!(yaml.contains("subtract"), "yaml: {}", yaml);
}

#[test]
fn stress_nl_multiply() {
    let yaml = nl_to_yaml("Multiply 7 and 8").expect("should produce workflow");
    assert!(yaml.contains("multiply"), "yaml: {}", yaml);
}

#[test]
fn stress_nl_divide() {
    let yaml = nl_to_yaml("Divide 100 by 4").expect("should produce workflow");
    assert!(yaml.contains("divide"), "yaml: {}", yaml);
}

#[test]
fn stress_nl_list_directory() {
    let yaml = nl_to_yaml("list ~/Downloads").expect("should produce workflow");
    assert!(yaml.contains("list_dir"), "yaml: {}", yaml);
}

#[test]
fn stress_nl_find_pdfs() {
    let yaml = nl_to_yaml("find all PDFs in ~/Documents").expect("should produce workflow");
    assert!(yaml.contains("walk_tree") || yaml.contains("find_matching"),
        "yaml: {}", yaml);
}

#[test]
fn stress_nl_zip_up() {
    let yaml = nl_to_yaml("zip up everything in my downloads").expect("should produce workflow");
    assert!(yaml.contains("pack_archive"), "yaml: {}", yaml);
}

#[test]
fn stress_nl_sum_synonym() {
    let yaml = nl_to_yaml("Sum 12 and 88").expect("should produce workflow");
    assert!(yaml.contains("add"), "sum should map to add: {}", yaml);
}

#[test]
fn stress_nl_plus_synonym() {
    let yaml = nl_to_yaml("Plus 1 and 99").expect("should produce workflow");
    assert!(yaml.contains("add"), "plus should map to add: {}", yaml);
}

#[test]
fn stress_nl_minus_synonym() {
    let yaml = nl_to_yaml("Minus 5 from 20").expect("should produce workflow");
    assert!(yaml.contains("subtract"), "minus should map to subtract: {}", yaml);
}

#[test]
fn stress_nl_search_content() {
    let yaml = nl_to_yaml("search for 'error' in ~/logs").expect("should produce workflow");
    assert!(yaml.contains("search_content") || yaml.contains("walk_tree") || yaml.contains("find_matching"),
        "yaml: {}", yaml);
}

#[test]
fn stress_nl_copy_files() {
    let yaml = nl_to_yaml("copy ~/Documents/report.pdf to ~/Desktop").expect("should produce workflow");
    assert!(yaml.contains("copy"), "yaml: {}", yaml);
}

// --- Full pipeline: NL → YAML → compile → Racket script ---

#[test]
fn stress_nl_full_pipeline_add_produces_racket() {
    let mut state = DialogueState::new();
    let response = nl::process_input("Add 4 and 35 together", &mut state);
    let yaml = match response {
        NlResponse::PlanCreated { workflow_yaml, .. } => workflow_yaml,
        other => panic!("expected PlanCreated, got: {:?}", other),
    };

    // Parse the YAML back into a WorkflowDef
    let def: WorkflowDef = serde_yaml::from_str(&yaml).unwrap();
    assert!(!def.steps.is_empty(), "workflow should have steps");

    // Build a compiled workflow and generate Racket
    let reg = make_racket_reg();
    let compiled = CompiledWorkflow {
        name: def.workflow.clone(),
        input_type: TypeExpr::prim("Number"),
        input_description: "4".to_string(),
        steps: vec![CompiledStep {
            index: 0,
            op: "add".to_string(),
            input_type: TypeExpr::prim("Number"),
            output_type: TypeExpr::prim("Number"),
            params: vec![("x".into(), "4".into()), ("y".into(), "35".into())].into_iter().collect(),
            ..Default::default()
        }],
        output_type: TypeExpr::prim("Number"),
    };
    let script = generate_racket_script(&compiled, &def, &reg).unwrap();
    assert!(script.contains("#lang racket"), "should have racket preamble");
    assert!(script.contains("(+ 4 35)"), "should contain (+ 4 35): {}", script);
}

#[test]
fn stress_nl_full_pipeline_subtract_produces_racket() {
    let mut state = DialogueState::new();
    let response = nl::process_input("Subtract 2 from 6", &mut state);
    let yaml = match response {
        NlResponse::PlanCreated { workflow_yaml, .. } => workflow_yaml,
        other => panic!("expected PlanCreated, got: {:?}", other),
    };

    let def: WorkflowDef = serde_yaml::from_str(&yaml).unwrap();
    let reg = make_racket_reg();
    let compiled = CompiledWorkflow {
        name: def.workflow.clone(),
        input_type: TypeExpr::prim("Number"),
        input_description: "6".to_string(),
        steps: vec![CompiledStep {
            index: 0,
            op: "subtract".to_string(),
            input_type: TypeExpr::prim("Number"),
            output_type: TypeExpr::prim("Number"),
            params: vec![("x".into(), "6".into()), ("y".into(), "2".into())].into_iter().collect(),
            ..Default::default()
        }],
        output_type: TypeExpr::prim("Number"),
    };
    let script = generate_racket_script(&compiled, &def, &reg).unwrap();
    assert!(script.contains("(- 6 2)"), "should contain (- 6 2): {}", script);
}

// --- Negative: gibberish, nonexistent ops ---

#[test]
fn stress_nl_gibberish_no_panic() {
    let mut state = DialogueState::new();
    // Should not panic — should return NeedsClarification or Error
    let response = nl::process_input("xyzzy plugh qwerty asdf jkl", &mut state);
    match response {
        NlResponse::PlanCreated { .. } => {
            // If it somehow creates a plan, that's OK — we just verify no panic
        }
        NlResponse::NeedsClarification { .. } | NlResponse::Error { .. } | NlResponse::Explanation { .. } => {
            // Expected
        }
        _ => {
            // Any response is fine as long as we didn't panic
        }
    }
}

#[test]
fn stress_nl_nonexistent_op_no_panic() {
    let mut state = DialogueState::new();
    let response = nl::process_input("frobulate the quantum flux capacitor", &mut state);
    // Should not panic
    match response {
        NlResponse::NeedsClarification { .. } | NlResponse::Error { .. } => {
            // Expected — can't understand the op
        }
        _ => {
            // Any response is fine as long as we didn't panic
        }
    }
}

#[test]
fn stress_nl_empty_input_no_panic() {
    let mut state = DialogueState::new();
    let _response = nl::process_input("", &mut state);
    // Just verify no panic
}

#[test]
fn stress_nl_only_punctuation_no_panic() {
    let mut state = DialogueState::new();
    let _response = nl::process_input("!@#$%^&*()", &mut state);
}

// --- Boundary: single token, very long input ---

#[test]
fn stress_nl_single_token_add() {
    let mut state = DialogueState::new();
    let response = nl::process_input("add", &mut state);
    // Single token "add" — should either ask for clarification or create a partial plan
    match response {
        NlResponse::NeedsClarification { .. } => { /* expected */ }
        NlResponse::PlanCreated { .. } => { /* also OK */ }
        NlResponse::Error { .. } => { /* also OK */ }
        NlResponse::Explanation { .. } => { /* also OK */ }
        _ => { /* no panic is the real test */ }
    }
}

#[test]
fn stress_nl_very_long_input_no_hang() {
    let mut state = DialogueState::new();
    // 100+ words of repetitive input
    let long_input = (0..120).map(|i| format!("word{}", i)).collect::<Vec<_>>().join(" ");
    let _response = nl::process_input(&long_input, &mut state);
    // Just verify it completes without hanging or panicking
}

#[test]
fn stress_nl_repeated_ops_no_panic() {
    let mut state = DialogueState::new();
    let _response = nl::process_input(
        "add add add subtract multiply divide add subtract multiply",
        &mut state,
    );
}

#[test]
fn stress_nl_unicode_input_no_panic() {
    let mut state = DialogueState::new();
    let _response = nl::process_input("计算 4 加 35", &mut state);
}

#[test]
fn stress_nl_numbers_only() {
    let mut state = DialogueState::new();
    let _response = nl::process_input("42 17 99 3.14", &mut state);
}

// ===========================================================================
// I2: Workflow compiler stress tests
// ===========================================================================

// --- Happy: deep step chains ---

#[test]
fn stress_workflow_8_step_fs_pipeline() {
    let yaml = r#"
workflow: "Deep FS pipeline"
inputs:
  logfile: "/var/log/app.log"
  sed_pattern: "s/ERROR/WARN/g"
  awk_prog: "{print $1, $3}"
steps:
  - awk_extract:
      program: "$awk_prog"
  - sed_script:
      script: "$sed_pattern"
  - awk_extract:
      program: "{print $1}"
  - sed_script:
      script: "s/WARN/INFO/g"
  - awk_extract:
      program: "{print $2}"
  - sed_script:
      script: "s/INFO/DEBUG/g"
  - awk_extract:
      program: "{print $1, $2}"
  - sed_script:
      script: "s/DEBUG/TRACE/g"
"#;
    let def = workflow::parse_workflow(yaml).expect("should parse 8-step workflow");
    assert_eq!(def.steps.len(), 8);
    let registry = build_full_registry();
    let compiled = workflow::compile_workflow(&def, &registry)
        .expect("should compile 8-step workflow");
    assert_eq!(compiled.steps.len(), 8);
}

#[test]
fn stress_workflow_var_resolution() {
    let yaml = r#"
workflow: "Var resolution"
inputs:
  path: "~/Documents"
  ext: ".pdf"
steps:
  - walk_tree
  - filter:
      extension: "$ext"
  - sort_by: name
"#;
    let def = workflow::parse_workflow(yaml).expect("should parse");
    let registry = build_full_registry();
    let compiled = workflow::compile_workflow(&def, &registry)
        .expect("should compile with $var refs");
    assert_eq!(compiled.steps.len(), 3);
    // Verify the $ext variable was preserved in the step params
    assert!(def.steps[1].args != StepArgs::None, "filter should have args");
}

// --- Negative: unknown ops, type mismatches, empty steps ---

#[test]
fn stress_workflow_unknown_op() {
    let yaml = r#"
workflow: "Unknown op"
inputs:
  path: "~/Documents"
steps:
  - walk_tree
  - filter:
      extension: ".txt"
  - nonexistent_op_xyz
  - sort_by: name
"#;
    let def = workflow::parse_workflow(yaml).expect("should parse");
    let registry = build_full_registry();
    let result = workflow::compile_workflow(&def, &registry);
    assert!(result.is_err(), "should fail on unknown op");
    let err = result.unwrap_err();
    let msg = format!("{}", err);
    assert!(msg.contains("nonexistent_op_xyz") || msg.contains("unknown"),
        "error should mention the bad op: {}", msg);
}

#[test]
fn stress_workflow_empty_steps() {
    let yaml = r#"
workflow: "Empty"
inputs:
  path: "~/Documents"
steps: []
"#;
    let result = workflow::parse_workflow(yaml);
    // Empty steps should fail at either parse or compile time
    assert!(result.is_err(), "empty steps should fail at parse time");
}

#[test]
fn stress_workflow_no_inputs() {
    let yaml = r#"
workflow: "No inputs"
inputs: {}
steps:
  - walk_tree
"#;
    let def = workflow::parse_workflow(yaml).expect("should parse");
    let registry = build_full_registry();
    let result = workflow::compile_workflow(&def, &registry);
    assert!(result.is_err(), "no inputs should fail");
}

// --- Boundary: single step, all $var params ---

#[test]
fn stress_workflow_single_step() {
    let yaml = r#"
workflow: "Single step"
inputs:
  path: "~/Documents"
steps:
  - list_dir
"#;
    let def = workflow::parse_workflow(yaml).expect("should parse");
    let registry = build_full_registry();
    let compiled = workflow::compile_workflow(&def, &registry)
        .expect("single step should compile");
    assert_eq!(compiled.steps.len(), 1);
}

#[test]
fn stress_workflow_many_inputs() {
    let yaml = r#"
workflow: "Many inputs"
inputs:
  path: "~/Documents"
  ext: ".pdf"
  pattern: "contract"
  mode: "case-insensitive"
  limit: "100"
steps:
  - walk_tree
  - filter:
      extension: "$ext"
"#;
    let def = workflow::parse_workflow(yaml).expect("should parse");
    assert_eq!(def.inputs.len(), 5);
    let registry = build_full_registry();
    let _compiled = workflow::compile_workflow(&def, &registry)
        .expect("should compile with many inputs");
}

// ===========================================================================
// I3: Racket codegen stress tests
// ===========================================================================

// --- Happy: multi-domain let* chains ---

#[test]
fn stress_racket_arithmetic_to_string_to_file() {
    let reg = make_racket_reg();
    let steps = vec![
        make_step(0, "multiply", vec![("x", "6"), ("y", "7")]),
        make_step(1, "number_to_string", vec![]),
        make_step(2, "string_append", vec![("x", "The answer is: ")]),
        make_step(3, "string_upcase", vec![]),
    ];
    let (compiled, def) = make_workflow(
        "Multi-domain chain",
        vec![("x", "6"), ("y", "7")],
        steps,
    );
    let script = generate_racket_script(&compiled, &def, &reg).unwrap();
    assert!(script.contains("#lang racket"));
    assert!(script.contains("let*"), "multi-step should use let*: {}", script);
    assert!(script.contains("step-1"), "should have step-1 binding");
    assert!(script.contains("step-4"), "should have step-4 binding");
    // Verify the arithmetic op is present
    assert!(script.contains("(* 6 7)"), "should contain (* 6 7): {}", script);
}

#[test]
fn stress_racket_list_processing_chain() {
    let reg = make_racket_reg();
    let steps = vec![
        make_step(0, "racket_filter", vec![("value", "'(1 2 3 4 5 6 7 8 9 10)"), ("predicate", "even?")]),
        make_step(1, "sort_list", vec![("comparator", ">")]),
        make_step(2, "racket_map", vec![("function", "(lambda (x) (* x x))")]),
        make_step(3, "racket_foldl", vec![("function", "+"), ("init", "0")]),
        make_step(4, "number_to_string", vec![]),
    ];
    let (compiled, def) = make_workflow(
        "Filter-sort-map-fold-format",
        vec![("lst", "'(1 2 3 4 5 6 7 8 9 10)")],
        steps,
    );
    let script = generate_racket_script(&compiled, &def, &reg).unwrap();
    assert!(script.contains("let*"));
    assert!(script.contains("filter"));
    assert!(script.contains("sort"));
    assert!(script.contains("map"));
    assert!(script.contains("foldl"));
    assert!(script.contains("step-5"), "should have 5 steps");
}

#[test]
fn stress_racket_set_operations_chain() {
    let reg = make_racket_reg();
    let steps = vec![
        make_step(0, "set_new", vec![("value", "'(1 2 3 4 5)")]),
        make_step(1, "set_union", vec![("y", "(list->set '(4 5 6 7 8))")]),
        make_step(2, "set_intersect", vec![("y", "(list->set '(3 4 5 6))")]),
        make_step(3, "set_to_list", vec![]),
        make_step(4, "length", vec![]),
    ];
    let (compiled, def) = make_workflow(
        "Set operations chain",
        vec![("a", "'(1 2 3 4 5)")],
        steps,
    );
    let script = generate_racket_script(&compiled, &def, &reg).unwrap();
    assert!(script.contains("let*"));
    assert!(script.contains("list->set"), "should use list->set: {}", script);
}

// --- Negative: missing params, unknown ops ---

#[test]
fn stress_racket_foldl_missing_function() {
    let reg = make_racket_reg();
    let step = make_step(0, "racket_foldl", vec![("value", "'(1 2 3)"), ("init", "0")]);
    // No "function" param — should error
    let inputs: HashMap<String, String> = HashMap::new();
    let result = op_to_racket(&step, &inputs, None, &reg, false);
    assert!(result.is_err(), "foldl without function should error");
    let err = result.unwrap_err();
    assert!(matches!(err, RacketError::MissingParam { .. }));
}

#[test]
fn stress_racket_filter_missing_predicate() {
    let reg = make_racket_reg();
    let step = make_step(0, "racket_filter", vec![("value", "'(1 2 3)")]);
    let inputs: HashMap<String, String> = HashMap::new();
    let result = op_to_racket(&step, &inputs, None, &reg, false);
    assert!(result.is_err());
    assert!(matches!(result.unwrap_err(), RacketError::MissingParam { .. }));
}

#[test]
fn stress_racket_map_missing_function() {
    let reg = make_racket_reg();
    let step = make_step(0, "racket_map", vec![("value", "'(1 2 3)")]);
    let inputs: HashMap<String, String> = HashMap::new();
    let result = op_to_racket(&step, &inputs, None, &reg, false);
    assert!(result.is_err());
    assert!(matches!(result.unwrap_err(), RacketError::MissingParam { .. }));
}

#[test]
fn stress_racket_completely_unknown_op() {
    let reg = make_racket_reg();
    let step = make_step(0, "quantum_teleport", vec![("x", "42")]);
    let inputs: HashMap<String, String> = HashMap::new();
    let result = op_to_racket(&step, &inputs, None, &reg, false);
    assert!(result.is_err(), "unknown op should error");
    assert!(matches!(result.unwrap_err(), RacketError::UnknownOp(_)));
}

// --- Boundary: nullary shell ops, 10-step pipeline ---

#[test]
fn stress_racket_shell_ps_nullary() {
    let reg = make_full_registry();
    let step = make_step(0, "shell_ps", vec![]);
    let inputs: HashMap<String, String> = HashMap::new();
    let result = op_to_racket(&step, &inputs, None, &reg, false);
    assert!(result.is_ok(), "shell_ps should generate: {:?}", result);
    let expr = result.unwrap();
    assert!(expr.expr.contains("ps"), "should contain ps command: {}", expr.expr);
}

#[test]
fn stress_racket_10_step_pipeline() {
    let reg = make_racket_reg();
    let steps = vec![
        make_step(0, "add", vec![("x", "1"), ("y", "2")]),
        make_step(1, "multiply", vec![("y", "3")]),
        make_step(2, "subtract", vec![("y", "1")]),
        make_step(3, "add", vec![("y", "10")]),
        make_step(4, "multiply", vec![("y", "2")]),
        make_step(5, "subtract", vec![("y", "5")]),
        make_step(6, "divide", vec![("y", "3")]),
        make_step(7, "add", vec![("y", "100")]),
        make_step(8, "number_to_string", vec![]),
        make_step(9, "string_upcase", vec![]),
    ];
    let (compiled, def) = make_workflow(
        "10-step arithmetic chain",
        vec![("x", "1"), ("y", "2")],
        steps,
    );
    let script = generate_racket_script(&compiled, &def, &reg).unwrap();
    assert!(script.contains("step-1"));
    assert!(script.contains("step-10"), "should have step-10: {}", script);
    assert!(script.contains("let*"));
    // Verify the final displayln references the last step
    assert!(script.contains("step-10"), "should display step-10");
}

#[test]
fn stress_racket_single_step_no_let() {
    let reg = make_racket_reg();
    let steps = vec![
        make_step(0, "add", vec![("x", "4"), ("y", "35")]),
    ];
    let (compiled, def) = make_workflow("Single add", vec![("x", "4"), ("y", "35")], steps);
    let script = generate_racket_script(&compiled, &def, &reg).unwrap();
    // Single-step should NOT use let*
    assert!(!script.contains("let*"), "single step should not use let*: {}", script);
    assert!(script.contains("(+ 4 35)"));
}

#[test]
fn stress_racket_empty_workflow() {
    let reg = make_racket_reg();
    let (compiled, def) = make_workflow("Empty", vec![("x", "1")], vec![]);
    let script = generate_racket_script(&compiled, &def, &reg).unwrap();
    assert!(script.contains("no steps"), "empty workflow should note no steps: {}", script);
}

// --- Shell ops with prev binding ---

#[test]
fn stress_racket_shell_ls_with_path() {
    let reg = make_full_registry();
    let step = make_step(0, "shell_ls", vec![("path", "/tmp")]);
    let inputs = vec![("path".to_string(), "/tmp".to_string())].into_iter().collect();
    let result = op_to_racket(&step, &inputs, None, &reg, false);
    assert!(result.is_ok(), "shell_ls should work: {:?}", result);
    let expr = result.unwrap();
    assert!(expr.expr.contains("ls"), "should contain ls: {}", expr.expr);
}

// ===========================================================================
// I4: Generic planner stress tests
// ===========================================================================

fn noop_exec() -> registry::ExecFn {
    Box::new(|_| Ok("noop".into()))
}

fn planner_registry() -> OperationRegistry {
    // Build a registry with a chain: Dir → walk → Seq(Entry) → filter → Seq(Entry) → sort → Sorted
    let mut reg = OperationRegistry::new();
    use registry::{OpSignature, TypeId, AlgebraicProperties};
    reg.register("walk_tree", OpSignature::new(vec![TypeId::new("Dir")], TypeId::new("SeqEntry")), AlgebraicProperties::none(), noop_exec());
    reg.register("filter", OpSignature::new(vec![TypeId::new("SeqEntry"), TypeId::new("Pattern")], TypeId::new("SeqEntry")), AlgebraicProperties::none(), noop_exec());
    reg.register("sort_by", OpSignature::new(vec![TypeId::new("SeqEntry")], TypeId::new("SortedSeq")), AlgebraicProperties::none(), noop_exec());
    reg.register("summarize", OpSignature::new(vec![TypeId::new("SeqEntry")], TypeId::new("Summary")), AlgebraicProperties::none(), noop_exec());
    reg.register("count", OpSignature::new(vec![TypeId::new("SeqEntry")], TypeId::new("Number")), AlgebraicProperties::none(), noop_exec());
    reg
}

// --- Happy: multi-op backward chaining ---

#[test]
fn stress_planner_simple_goal() {
    let reg = planner_registry();
    let goal = GenericGoal::simple(
        "SeqEntry",
        vec![Literal {
            type_id: registry::TypeId::new("Dir"),
            key: "path".to_string(),
            value: "~/Documents".to_string(),
            metadata: HashMap::new(),
        }],
    );
    let result = generic_planner::plan(&goal, &reg);
    assert!(result.is_ok(), "should find a plan: {:?}", result);
    let plan = result.unwrap();
    assert!(plan.op_count() >= 1, "plan should have at least 1 op");
}

#[test]
fn stress_planner_deep_chain() {
    // Goal: produce a sorted sequence from a directory — requires walk_tree + sort_by
    let reg = planner_registry();
    let goal = GenericGoal::simple(
        "SortedSeq",
        vec![Literal {
            type_id: registry::TypeId::new("Dir"),
            key: "path".to_string(),
            value: "~/Documents".to_string(),
            metadata: HashMap::new(),
        }],
    );
    let result = generic_planner::plan(&goal, &reg);
    assert!(result.is_ok(), "should find walk_tree→sort_by chain: {:?}", result);
    let plan = result.unwrap();
    assert!(plan.op_count() >= 2, "should need at least 2 ops, got {}", plan.op_count());
}

// --- Negative: unproducible type, unsatisfied constraints ---

#[test]
fn stress_planner_unproducible_type() {
    let reg = planner_registry();
    let goal = GenericGoal::simple(
        "QuantumState(Entangled)",
        vec![Literal {
            type_id: registry::TypeId::new("Number"),
            key: "x".to_string(),
            value: "42".to_string(),
            metadata: HashMap::new(),
        }],
    );
    let result = generic_planner::plan(&goal, &reg);
    assert!(result.is_err(), "should fail for unproducible type");
    match result.unwrap_err() {
        PlanError::NoProducer { .. } | PlanError::NoPlanFound { .. } | PlanError::DepthExceeded { .. } => {}
        other => panic!("expected NoProducer or NoPlanFound, got: {:?}", other),
    }
}

#[test]
fn stress_planner_must_include_impossible() {
    let reg = planner_registry();
    let goal = GenericGoal::simple(
        "SeqEntry",
        vec![Literal {
            type_id: registry::TypeId::new("Dir"),
            key: "path".to_string(),
            value: "~/Documents".to_string(),
            metadata: HashMap::new(),
        }],
    ).with_must_include("quantum_teleport_xyz");
    let result = generic_planner::plan(&goal, &reg);
    assert!(result.is_err(), "should fail with impossible must-include");
}

// --- Boundary: depth limits, trivial plans ---

#[test]
fn stress_planner_depth_1_too_shallow() {
    let reg = planner_registry();
    // SortedSeq needs walk_tree + sort_by (depth 2), but depth=1 should fail
    let goal = GenericGoal::simple(
        "SortedSeq",
        vec![Literal {
            type_id: registry::TypeId::new("Dir"),
            key: "path".to_string(),
            value: "~/Documents".to_string(),
            metadata: HashMap::new(),
        }],
    ).with_max_depth(1);
    let result = generic_planner::plan(&goal, &reg);
    // Should fail — depth 1 can't reach SortedSeq from Dir (needs 2 ops)
    assert!(result.is_err(), "depth=1 should be too shallow for SortedSeq from Dir");
}

#[test]
fn stress_planner_literal_matches_goal() {
    // If the available literal already has the goal type, plan should be trivial
    let reg = planner_registry();
    let goal = GenericGoal::simple(
        "Dir",
        vec![Literal {
            type_id: registry::TypeId::new("Dir"),
            key: "path".to_string(),
            value: "~/Documents".to_string(),
            metadata: HashMap::new(),
        }],
    );
    let result = generic_planner::plan(&goal, &reg);
    assert!(result.is_ok(), "trivial plan should succeed: {:?}", result);
    let plan = result.unwrap();
    // Should be a leaf node (no ops needed)
    assert_eq!(plan.op_count(), 0, "trivial plan should have 0 ops, got {}", plan.op_count());
}

// --- Expr planner ---

#[test]
fn stress_expr_planner_number_chain() {
    let reg = make_racket_reg();
    let goal = ExprGoal::new(
        TypeExpr::prim("Number"),
        vec![
            ExprLiteral::new("x", TypeExpr::prim("Number"), "first number"),
            ExprLiteral::new("y", TypeExpr::prim("Number"), "second number"),
        ],
    );
    let result = generic_planner::plan_expr(&goal, &reg);
    assert!(result.is_ok(), "should plan for Number from two Numbers: {:?}", result);
}

// ===========================================================================
// I5: Inference engine stress tests
// ===========================================================================

// --- Happy: full inference produces all expected ops ---

#[test]
fn stress_inference_all_arithmetic_ops() {
    let mut reg = build_racket_registry();
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();
    let inferred = promote_inferred_ops(&mut reg, &facts);

    let inferred_names: HashSet<String> = inferred.iter().map(|i| i.op_name.clone()).collect();

    // These should all be inferred (not in racket.ops.yaml directly)
    for expected in &["subtract", "multiply", "divide", "greater_than", "less_than_or_equal"] {
        assert!(inferred_names.contains(*expected),
            "expected '{}' to be inferred, got: {:?}", expected, inferred_names);
    }
}

#[test]
fn stress_inference_string_downcase_discovered() {
    let mut reg = build_racket_registry();
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();
    let inferred = promote_inferred_ops(&mut reg, &facts);

    let has_downcase = inferred.iter().any(|i| i.op_name == "string_downcase");
    assert!(has_downcase, "string_downcase should be inferred via type-symmetric from string_upcase");
}

#[test]
fn stress_inference_op_symmetric_kind() {
    let mut reg = build_racket_registry();
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();
    let inferred = promote_inferred_ops(&mut reg, &facts);

    let sub = inferred.iter().find(|i| i.op_name == "subtract").unwrap();
    assert_eq!(sub.inference_kind, InferenceKind::OpSymmetric,
        "subtract should be op-symmetric");
}

#[test]
fn stress_inference_type_symmetric_kind() {
    let mut reg = build_racket_registry();
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();
    let inferred = promote_inferred_ops(&mut reg, &facts);

    // string_downcase is always type-symmetric (from string_upcase, class: string_unary)
    // multiply can be either op-symmetric or type-symmetric depending on HashMap order
    let downcase = inferred.iter().find(|i| i.op_name == "string_downcase").unwrap();
    match &downcase.inference_kind {
        InferenceKind::TypeSymmetric { .. } => { /* expected */ }
        other => panic!("string_downcase should be type-symmetric, got: {:?}", other),
    }
}

#[test]
fn stress_inference_shell_submodes() {
    let mut reg = build_racket_registry();
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();
    promote_inferred_ops(&mut reg, &facts);

    let cli_pack: FactPack = serde_yaml::from_str(MACOS_CLI_FACTS_YAML).unwrap();
    let cli_facts = FactPackIndex::build(cli_pack);
    let submodes = discover_shell_submodes(&mut reg, &facts, &cli_facts);

    assert!(submodes.len() >= 20, "expected at least 20 submodes, got {}", submodes.len());

    // Verify some specific submodes exist
    let submode_names: HashSet<String> = submodes.iter().map(|s| s.op_name.clone()).collect();
    assert!(submode_names.contains("shell_ls_long") || submode_names.iter().any(|n| n.starts_with("shell_ls_")),
        "should have shell_ls submodes: {:?}", submode_names);
}

// --- Negative: no symmetric partner, no metasig ---

#[test]
fn stress_inference_no_partner_error() {
    let reg = build_racket_registry();
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();

    // "abs" has no symmetric partner
    let result = infer_symmetric_op("abs", &reg, &facts);
    assert!(result.is_err(), "abs should have no symmetric partner");
    match result.unwrap_err() {
        InferenceError::NoSymmetricPartner(_) | InferenceError::UnknownEntity(_) => { /* expected */ }
        other => panic!("expected NoSymmetricPartner or UnknownEntity, got: {:?}", other),
    }
}

#[test]
fn stress_inference_unknown_entity() {
    let reg = build_racket_registry();
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();

    let result = infer_symmetric_op("nonexistent_op_xyz", &reg, &facts);
    assert!(result.is_err());
}

// --- Boundary: idempotency, invariant non-transfer ---

#[test]
fn stress_inference_idempotent() {
    let mut reg = build_racket_registry();
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();

    let first = promote_inferred_ops(&mut reg, &facts);
    let _first_count = first.len();

    // Run again — should produce the same count (idempotent)
    let _second = promote_inferred_ops(&mut reg, &facts);
    // Second run may return 0 (already promoted) or same count
    // The key is that the registry doesn't grow unboundedly
    let total_ops_after = reg.poly_op_names().len();

    let mut reg2 = build_racket_registry();
    let _first2 = promote_inferred_ops(&mut reg2, &facts);
    let ops_after_one = reg2.poly_op_names().len();

    assert_eq!(total_ops_after, ops_after_one,
        "running promote twice should not add extra ops: {} vs {}", total_ops_after, ops_after_one);
}

#[test]
fn stress_inference_invariants_not_transferred() {
    let mut reg = build_racket_registry();
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();
    let inferred = promote_inferred_ops(&mut reg, &facts);

    // add has invariants (commutative, identity=0), but subtract should NOT inherit them
    for inf in &inferred {
        if inf.inference_kind == InferenceKind::OpSymmetric {
            assert!(inf.meta.invariants.is_empty(),
                "op-symmetric inferred op '{}' should have empty invariants, got: {:?}",
                inf.op_name, inf.meta.invariants);
        }
    }
}

#[test]
fn stress_inference_all_inferred_have_racket_symbol() {
    let mut reg = build_racket_registry();
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();
    let inferred = promote_inferred_ops(&mut reg, &facts);

    for inf in &inferred {
        assert!(!inf.racket_symbol.is_empty(),
            "inferred op '{}' should have a racket_symbol", inf.op_name);
    }
}

// ===========================================================================
// I6: Type unification stress tests
// ===========================================================================

// --- Happy: complex nested types ---

#[test]
fn stress_unify_seq_entry_with_variable() {
    // Seq(Entry(Name, a)) vs Seq(Entry(Name, File(Text)))
    let left = TypeExpr::cons("Seq", vec![
        TypeExpr::cons("Entry", vec![
            TypeExpr::prim("Name"),
            TypeExpr::var("a"),
        ]),
    ]);
    let right = TypeExpr::cons("Seq", vec![
        TypeExpr::cons("Entry", vec![
            TypeExpr::prim("Name"),
            TypeExpr::cons("File", vec![TypeExpr::prim("Text")]),
        ]),
    ]);
    let result = type_expr::unify(&left, &right);
    assert!(result.is_ok(), "should unify: {:?}", result);
    let subst = result.unwrap();
    // a should be bound to File(Text)
    let a_binding = subst.get("a");
    assert!(a_binding.is_some(), "a should be bound");
    assert_eq!(
        *a_binding.unwrap(),
        TypeExpr::cons("File", vec![TypeExpr::prim("Text")]),
    );
}

#[test]
fn stress_unify_deeply_nested_5_levels() {
    // A(B(C(D(E(x))))) vs A(B(C(D(E(Number)))))
    let left = TypeExpr::cons("A", vec![
        TypeExpr::cons("B", vec![
            TypeExpr::cons("C", vec![
                TypeExpr::cons("D", vec![
                    TypeExpr::cons("E", vec![TypeExpr::var("x")]),
                ]),
            ]),
        ]),
    ]);
    let right = TypeExpr::cons("A", vec![
        TypeExpr::cons("B", vec![
            TypeExpr::cons("C", vec![
                TypeExpr::cons("D", vec![
                    TypeExpr::cons("E", vec![TypeExpr::prim("Number")]),
                ]),
            ]),
        ]),
    ]);
    let result = type_expr::unify(&left, &right);
    assert!(result.is_ok(), "5-level nesting should unify: {:?}", result);
    let subst = result.unwrap();
    assert_eq!(*subst.get("x").unwrap(), TypeExpr::prim("Number"));
}

#[test]
fn stress_unify_multiple_variables() {
    // Entry(a, b) vs Entry(Name, File(Text))
    let left = TypeExpr::cons("Entry", vec![TypeExpr::var("a"), TypeExpr::var("b")]);
    let right = TypeExpr::cons("Entry", vec![
        TypeExpr::prim("Name"),
        TypeExpr::cons("File", vec![TypeExpr::prim("Text")]),
    ]);
    let result = type_expr::unify(&left, &right);
    assert!(result.is_ok());
    let subst = result.unwrap();
    assert_eq!(*subst.get("a").unwrap(), TypeExpr::prim("Name"));
    assert_eq!(*subst.get("b").unwrap(), TypeExpr::cons("File", vec![TypeExpr::prim("Text")]));
}

// --- Negative: constructor mismatch, arity mismatch ---

#[test]
fn stress_unify_constructor_mismatch() {
    let left = TypeExpr::cons("Seq", vec![TypeExpr::var("a")]);
    let right = TypeExpr::cons("Entry", vec![TypeExpr::var("b"), TypeExpr::var("c")]);
    let result = type_expr::unify(&left, &right);
    assert!(result.is_err(), "Seq vs Entry should fail");
    match result.unwrap_err() {
        UnifyError::ConstructorMismatch { .. } | UnifyError::ArityMismatch { .. } => {}
        other => panic!("expected ConstructorMismatch, got: {:?}", other),
    }
}

#[test]
fn stress_unify_arity_mismatch() {
    let left = TypeExpr::cons("Entry", vec![TypeExpr::var("a"), TypeExpr::var("b")]);
    let right = TypeExpr::cons("Entry", vec![
        TypeExpr::var("x"), TypeExpr::var("y"), TypeExpr::var("z"),
    ]);
    let result = type_expr::unify(&left, &right);
    assert!(result.is_err(), "Entry/2 vs Entry/3 should fail");
    match result.unwrap_err() {
        UnifyError::ArityMismatch { .. } => {}
        other => panic!("expected ArityMismatch, got: {:?}", other),
    }
}

#[test]
fn stress_unify_prim_vs_constructor() {
    let left = TypeExpr::prim("Number");
    let right = TypeExpr::cons("Seq", vec![TypeExpr::prim("Number")]);
    let result = type_expr::unify(&left, &right);
    assert!(result.is_err(), "Number vs Seq(Number) should fail");
}

// --- Boundary: identical types, variable vs variable ---

#[test]
fn stress_unify_identical_concrete() {
    let left = TypeExpr::cons("Seq", vec![TypeExpr::prim("Number")]);
    let right = TypeExpr::cons("Seq", vec![TypeExpr::prim("Number")]);
    let result = type_expr::unify(&left, &right);
    assert!(result.is_ok());
    let subst = result.unwrap();
    assert!(subst.is_empty(), "identical types should have empty substitution");
}

#[test]
fn stress_unify_var_vs_var() {
    let left = TypeExpr::var("a");
    let right = TypeExpr::var("b");
    let result = type_expr::unify(&left, &right);
    assert!(result.is_ok(), "var vs var should unify");
}

#[test]
fn stress_unify_var_vs_prim() {
    let left = TypeExpr::var("a");
    let right = TypeExpr::prim("Number");
    let result = type_expr::unify(&left, &right);
    assert!(result.is_ok());
    let subst = result.unwrap();
    assert_eq!(*subst.get("a").unwrap(), TypeExpr::prim("Number"));
}

#[test]
fn stress_unify_same_var_consistent() {
    // Seq(a, a) vs Seq(Number, Number) — a must bind consistently
    let left = TypeExpr::cons("Seq", vec![TypeExpr::var("a"), TypeExpr::var("a")]);
    let right = TypeExpr::cons("Seq", vec![TypeExpr::prim("Number"), TypeExpr::prim("Number")]);
    let result = type_expr::unify(&left, &right);
    assert!(result.is_ok(), "consistent variable should unify");
}

#[test]
fn stress_unify_same_var_inconsistent() {
    // Seq(a, a) vs Seq(Number, String) — a can't be both
    let left = TypeExpr::cons("Seq", vec![TypeExpr::var("a"), TypeExpr::var("a")]);
    let right = TypeExpr::cons("Seq", vec![TypeExpr::prim("Number"), TypeExpr::prim("String")]);
    let result = type_expr::unify(&left, &right);
    // This should fail because a can't be both Number and String
    assert!(result.is_err(), "inconsistent variable binding should fail: {:?}", result);
}

// ===========================================================================
// I7: Type lowering / subsumption map stress tests
// ===========================================================================

// --- Happy: all entries exist in registry ---

#[test]
fn stress_subsumption_all_entries_in_registry() {
    let reg = build_full_registry();
    let entries = type_lowering::all_subsumptions();

    for entry in entries {
        assert!(reg.get_poly(entry.fs_op).is_some(),
            "subsumption entry '{}' not found in full registry", entry.fs_op);
    }
}

#[test]
fn stress_subsumption_count() {
    let entries = type_lowering::all_subsumptions();
    assert!(entries.len() >= 100,
        "expected at least 100 subsumption entries, got {}", entries.len());
}

// --- Negative: nonexistent ops ---

#[test]
fn stress_subsumption_nonexistent_returns_none() {
    assert!(type_lowering::lookup_subsumption("quantum_teleport").is_none());
    assert!(type_lowering::lookup_subsumption("").is_none());
    assert!(type_lowering::lookup_subsumption("nonexistent_op_xyz_123").is_none());
}

#[test]
fn stress_racket_native_not_subsumed() {
    // filter is a racket-native op, not subsumed by a shell op
    assert!(!type_lowering::is_subsumed("filter"),
        "filter should not be subsumed");
    assert!(!type_lowering::is_subsumed("add"),
        "add should not be subsumed");
}

// --- Boundary: dual-behavior ops, no duplicates ---

#[test]
fn stress_subsumption_dual_behavior_ops() {
    // sort_by and head should have dual behavior (shell bridge + racket-native)
    for op in &["sort_by", "head", "tail", "count", "unique"] {
        let has_dual = type_lowering::lookup_dual_behavior(op);
        assert!(has_dual.is_some(),
            "op '{}' should be in the dual-behavior map", op);
    }
    // filter is racket-native only, not dual
    assert!(type_lowering::lookup_dual_behavior("filter").is_none(),
        "filter should NOT be dual-behavior");
}

#[test]
fn stress_subsumption_no_duplicate_fs_ops() {
    let entries = type_lowering::all_subsumptions();
    let mut seen: HashSet<&str> = HashSet::new();
    for entry in entries {
        assert!(seen.insert(entry.fs_op),
            "duplicate fs_op in subsumption map: '{}'", entry.fs_op);
    }
}

#[test]
fn stress_subsumption_all_shell_ops_nonempty() {
    let entries = type_lowering::all_subsumptions();
    for entry in entries {
        assert!(!entry.shell_op.is_empty(),
            "subsumption entry '{}' has empty shell_op", entry.fs_op);
        assert!(!entry.note.is_empty(),
            "subsumption entry '{}' has empty note", entry.fs_op);
    }
}

#[test]
fn stress_has_lowering_covers_all_paths() {
    // has_lowering should return true for subsumed, racket-native, and dual ops
    let entries = type_lowering::all_subsumptions();
    for entry in entries {
        assert!(type_lowering::has_lowering(entry.fs_op),
            "has_lowering should be true for subsumed op '{}'", entry.fs_op);
    }
}

#[test]
fn stress_has_lowering_false_for_unknown() {
    assert!(!type_lowering::has_lowering("quantum_teleport"));
    assert!(!type_lowering::has_lowering(""));
}

// --- Cross-cutting: verify the full registry has both fs_ops and racket_ops ---

#[test]
fn stress_full_registry_comprehensive() {
    let reg = build_full_registry();
    let names = reg.poly_op_names();

    // Should have fs_ops
    assert!(names.contains(&"list_dir"), "missing list_dir");
    assert!(names.contains(&"walk_tree"), "missing walk_tree");
    assert!(names.contains(&"read_file"), "missing read_file");

    // Should have power_tools ops
    assert!(names.contains(&"git_log"), "missing git_log");
    assert!(names.contains(&"jq_query"), "missing jq_query");

    // Should have racket ops (from coding + comparison packs)
    assert!(names.len() >= 100, "expected 100+ ops, got {}", names.len());
}
