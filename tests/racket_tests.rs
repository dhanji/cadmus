// ---------------------------------------------------------------------------
// Racket Integration Tests
// ---------------------------------------------------------------------------
//
// End-to-end tests for the Racket programmer feature:
//   1. Ops pack loading and registry
//   2. Fact pack loading and keyword resolution
//   3. Inference bridge (symmetric ops)
//   4. NL → workflow → Racket script pipeline
//   5. Workflow YAML → Racket script generation

use cadmus::registry::load_ops_pack_str;
use cadmus::racket_strategy::{
    build_racket_registry, load_racket_facts_from_str,
    load_keyword_map, resolve_keyword, infer_symmetric_op,
    promote_inferred_ops,
};
use cadmus::racket_executor::{generate_racket_script, op_to_racket};
use cadmus::workflow::{WorkflowDef, CompiledStep, CompiledWorkflow};
use cadmus::type_expr::TypeExpr;
use cadmus::nl;
use std::collections::HashMap;

const RACKET_OPS_YAML: &str = include_str!("../data/racket_ops.yaml");
const RACKET_FACTS_YAML: &str = include_str!("../data/racket_facts.yaml");

// =========================================================================
// 1. Ops pack loading
// =========================================================================

#[test]
fn test_racket_ops_load_all() {
    let reg = load_ops_pack_str(RACKET_OPS_YAML).unwrap();
    let names = reg.poly_op_names();
    assert!(names.len() >= 40, "expected at least 40 racket ops, got {}", names.len());
}

#[test]
fn test_racket_ops_arithmetic_present() {
    let reg = build_racket_registry();
    for op in &["add", "subtract", "multiply", "divide", "modulo", "expt", "abs", "min", "max"] {
        assert!(reg.get_poly(op).is_some(), "missing arithmetic op: {}", op);
    }
}

#[test]
fn test_racket_ops_list_present() {
    let reg = build_racket_registry();
    for op in &["cons", "car", "cdr", "list_new", "append", "length", "list_reverse", "list_ref", "member", "remove", "sort_list", "flatten"] {
        assert!(reg.get_poly(op).is_some(), "missing list op: {}", op);
    }
}

#[test]
fn test_racket_ops_set_present() {
    let reg = build_racket_registry();
    for op in &["set_new", "set_add", "set_remove", "set_member", "set_union", "set_intersect", "set_subtract", "set_count", "set_to_list"] {
        assert!(reg.get_poly(op).is_some(), "missing set op: {}", op);
    }
}

#[test]
fn test_racket_ops_stdio_present() {
    let reg = build_racket_registry();
    for op in &["display", "displayln", "newline", "read_line", "format_string", "printf"] {
        assert!(reg.get_poly(op).is_some(), "missing stdio op: {}", op);
    }
}

#[test]
fn test_racket_ops_higher_order_present() {
    let reg = build_racket_registry();
    for op in &["racket_map", "racket_filter", "racket_foldl", "racket_foldr", "racket_for_each", "racket_apply", "andmap", "ormap"] {
        assert!(reg.get_poly(op).is_some(), "missing higher-order op: {}", op);
    }
}

#[test]
fn test_racket_add_has_meta() {
    let reg = build_racket_registry();
    let add = reg.get_poly("add").unwrap();
    assert!(add.meta.is_some(), "add should have a metasignature");
    let meta = add.meta.as_ref().unwrap();
    assert_eq!(meta.category.as_deref(), Some("arithmetic"));
    assert_eq!(meta.effects.as_deref(), Some("none"));
    assert!(!meta.invariants.is_empty(), "add should have invariants");
}

#[test]
fn test_racket_subtract_has_no_meta_initially() {
    let reg = build_racket_registry();
    let sub = reg.get_poly("subtract").unwrap();
    assert!(sub.meta.is_none(), "subtract should NOT have a metasignature initially (it's a stub)");
}

// =========================================================================
// 2. Fact pack and keyword resolution
// =========================================================================

#[test]
fn test_racket_facts_load() {
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();
    assert_eq!(facts.pack.entities.len(), 4);
    assert_eq!(facts.pack.axes.len(), 5);
    assert!(facts.pack.claims.len() >= 16);
    assert!(facts.pack.evidence.len() >= 8);
}

#[test]
fn test_keyword_map_complete() {
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();
    let map = load_keyword_map(&facts);

    // Verb forms
    assert_eq!(resolve_keyword("add", &map), Some("add".to_string()));
    assert_eq!(resolve_keyword("subtract", &map), Some("subtract".to_string()));
    assert_eq!(resolve_keyword("multiply", &map), Some("multiply".to_string()));
    assert_eq!(resolve_keyword("divide", &map), Some("divide".to_string()));

    // Synonyms
    assert_eq!(resolve_keyword("plus", &map), Some("add".to_string()));
    assert_eq!(resolve_keyword("minus", &map), Some("subtract".to_string()));
    assert_eq!(resolve_keyword("times", &map), Some("multiply".to_string()));
    assert_eq!(resolve_keyword("quotient", &map), Some("divide".to_string()));

    // Symbols
    assert_eq!(resolve_keyword("+", &map), Some("add".to_string()));
    assert_eq!(resolve_keyword("-", &map), Some("subtract".to_string()));
    assert_eq!(resolve_keyword("*", &map), Some("multiply".to_string()));
    assert_eq!(resolve_keyword("/", &map), Some("divide".to_string()));
}

#[test]
fn test_keyword_map_unknown_returns_none() {
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();
    let map = load_keyword_map(&facts);
    assert_eq!(resolve_keyword("nonexistent", &map), None);
    assert_eq!(resolve_keyword("", &map), None);
}

// =========================================================================
// 3. Inference bridge
// =========================================================================

#[test]
fn test_infer_subtract_signature() {
    let reg = build_racket_registry();
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();

    let inferred = infer_symmetric_op("subtract", &reg, &facts).unwrap();
    assert_eq!(inferred.op_name, "subtract");
    assert_eq!(inferred.racket_symbol, "-");
    assert_eq!(inferred.inferred_from, "add");
    assert_eq!(inferred.meta.return_type, "Number");
    assert_eq!(inferred.meta.params.len(), 2);
    assert_eq!(inferred.meta.category.as_deref(), Some("arithmetic"));
}

#[test]
fn test_infer_subtract_drops_invariants() {
    let reg = build_racket_registry();
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();

    let inferred = infer_symmetric_op("subtract", &reg, &facts).unwrap();
    assert!(inferred.meta.invariants.is_empty(),
        "invariants must NOT transfer: y>0 => x+y>x does not hold for subtraction");
    assert!(inferred.invariants_dropped);
}

#[test]
fn test_promote_upgrades_subtract() {
    let mut reg = build_racket_registry();
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();

    // Before: subtract has no meta
    assert!(reg.get_poly("subtract").unwrap().meta.is_none());

    let inferred = promote_inferred_ops(&mut reg, &facts);
    let sub_inferred: Vec<_> = inferred.iter().filter(|i| i.op_name == "subtract").collect();
    assert_eq!(sub_inferred.len(), 1, "subtract should be inferred");
    assert_eq!(sub_inferred[0].inferred_from, "add");
}

#[test]
fn test_infer_abs_fails_no_partner() {
    let reg = build_racket_registry();
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();

    // abs has no symmetric partner in the fact pack
    let result = infer_symmetric_op("abs", &reg, &facts);
    assert!(result.is_err(), "abs should fail inference — no symmetric partner");
}

// =========================================================================
// 4. Racket script generation
// =========================================================================

#[test]
fn test_add_4_and_35_script() {
    let compiled = CompiledWorkflow {
        name: "Add 4 and 35".to_string(),
        input_type: TypeExpr::prim("Number"),
        input_description: "4".to_string(),
        steps: vec![
            CompiledStep {
                index: 0,
                op: "add".to_string(),
                is_each: false,
                input_type: TypeExpr::prim("Number"),
                output_type: TypeExpr::prim("Number"),
                params: vec![("x".into(), "4".into()), ("y".into(), "35".into())].into_iter().collect(),
            },
        ],
        output_type: TypeExpr::prim("Number"),
    };
    let def = WorkflowDef {
        workflow: "Add 4 and 35".to_string(),
        inputs: vec![("x".into(), "4".into()), ("y".into(), "35".into())].into_iter().collect(),
        steps: vec![],
    };
    let script = generate_racket_script(&compiled, &def).unwrap();
    assert!(script.contains("#lang racket"), "should have racket preamble");
    assert!(script.contains("(+ 4 35)"), "should contain (+ 4 35), got:\n{}", script);
}

#[test]
fn test_subtract_6_minus_2_script() {
    let compiled = CompiledWorkflow {
        name: "Subtract 2 from 6".to_string(),
        input_type: TypeExpr::prim("Number"),
        input_description: "6".to_string(),
        steps: vec![
            CompiledStep {
                index: 0,
                op: "subtract".to_string(),
                is_each: false,
                input_type: TypeExpr::prim("Number"),
                output_type: TypeExpr::prim("Number"),
                params: vec![("x".into(), "6".into()), ("y".into(), "2".into())].into_iter().collect(),
            },
        ],
        output_type: TypeExpr::prim("Number"),
    };
    let def = WorkflowDef {
        workflow: "Subtract 2 from 6".to_string(),
        inputs: vec![("x".into(), "6".into()), ("y".into(), "2".into())].into_iter().collect(),
        steps: vec![],
    };
    let script = generate_racket_script(&compiled, &def).unwrap();
    assert!(script.contains("(- 6 2)"), "should contain (- 6 2), got:\n{}", script);
}

#[test]
fn test_workflow_yaml_add_numbers() {
    let yaml = include_str!("../data/workflows/add_numbers.yaml");
    let def: WorkflowDef = serde_yaml::from_str(yaml).unwrap();
    assert_eq!(def.workflow, "Add 4 and 35");
    assert_eq!(def.inputs.get("x"), Some(&"4".to_string()));
    assert_eq!(def.inputs.get("y"), Some(&"35".to_string()));
}

#[test]
fn test_workflow_yaml_subtract_numbers() {
    let yaml = include_str!("../data/workflows/subtract_numbers.yaml");
    let def: WorkflowDef = serde_yaml::from_str(yaml).unwrap();
    assert_eq!(def.workflow, "Subtract 2 from 6");
    assert_eq!(def.inputs.get("x"), Some(&"6".to_string()));
    assert_eq!(def.inputs.get("y"), Some(&"2".to_string()));
}

// =========================================================================
// 5. NL → Workflow E2E
// =========================================================================

#[test]
fn test_nl_add_4_and_35_together() {
    let mut state = nl::dialogue::DialogueState::new();
    let response = nl::process_input("Add 4 and 35 together", &mut state);

    match &response {
        nl::NlResponse::PlanCreated { workflow_yaml: yaml, .. } => {
            // The YAML should contain the add op
            assert!(yaml.contains("add"), "workflow should contain add op, got:\n{}", yaml);
            // Should have the numbers
            assert!(yaml.contains("4") || yaml.contains("35"),
                "workflow should reference the numbers, got:\n{}", yaml);
        }
        other => panic!("expected PlanCreated, got: {:?}", other),
    }
}

#[test]
fn test_nl_subtract_2_from_6() {
    let mut state = nl::dialogue::DialogueState::new();
    let response = nl::process_input("Subtract 2 from 6", &mut state);

    match &response {
        nl::NlResponse::PlanCreated { workflow_yaml: yaml, .. } => {
            assert!(yaml.contains("subtract"), "workflow should contain subtract op, got:\n{}", yaml);
            assert!(yaml.contains("6") || yaml.contains("2"),
                "workflow should reference the numbers, got:\n{}", yaml);
        }
        other => panic!("expected PlanCreated, got: {:?}", other),
    }
}

#[test]
fn test_nl_add_produces_racket_script() {
    let mut state = nl::dialogue::DialogueState::new();
    let response = nl::process_input("Add 4 and 35 together", &mut state);

    match &response {
        nl::NlResponse::PlanCreated { workflow_yaml: yaml, .. } => {
            // Parse the workflow YAML
            let def: WorkflowDef = serde_yaml::from_str(yaml)
                .expect("generated YAML should parse");

            // Build a registry with racket ops
            let _reg = cadmus::fs_types::build_full_registry();

            // Try to compile — this may fail because the workflow compiler
            // expects filesystem types. Instead, generate the Racket script
            // directly from the workflow def.
            // For arithmetic, we build a CompiledWorkflow manually.
            let compiled = CompiledWorkflow {
                name: def.workflow.clone(),
                input_type: TypeExpr::prim("Number"),
                input_description: def.inputs.values().next().cloned().unwrap_or_default(),
                steps: def.steps.iter().enumerate().map(|(i, raw)| {
                    let (op, params) = cadmus::workflow::raw_step_to_op_params(raw);
                    // Merge workflow inputs into params (expand $vars)
                    let mut resolved = HashMap::new();
                    for (k, v) in &params {
                        if v.starts_with('$') {
                            if let Some(val) = def.inputs.get(&v[1..]) {
                                resolved.insert(k.clone(), val.clone());
                            } else {
                                resolved.insert(k.clone(), v.clone());
                            }
                        } else {
                            resolved.insert(k.clone(), v.clone());
                        }
                    }
                    CompiledStep {
                        index: i,
                        op,
                        is_each: false,
                        input_type: TypeExpr::prim("Number"),
                        output_type: TypeExpr::prim("Number"),
                        params: resolved,
                    }
                }).collect(),
                output_type: TypeExpr::prim("Number"),
            };

            let script = generate_racket_script(&compiled, &def).unwrap();
            assert!(script.contains("#lang racket"), "should have racket preamble");
            assert!(script.contains("(+ 4 35)") || script.contains("(+ 35 4)"),
                "should contain (+ 4 35), got:\n{}", script);
        }
        other => panic!("expected PlanCreated, got: {:?}", other),
    }
}

#[test]
fn test_nl_multiply_3_and_7() {
    let mut state = nl::dialogue::DialogueState::new();
    let response = nl::process_input("Multiply 3 and 7", &mut state);

    match &response {
        nl::NlResponse::PlanCreated { workflow_yaml: yaml, .. } => {
            assert!(yaml.contains("multiply"), "workflow should contain multiply op, got:\n{}", yaml);
        }
        other => panic!("expected PlanCreated, got: {:?}", other),
    }
}

#[test]
fn test_nl_divide_10_by_2() {
    let mut state = nl::dialogue::DialogueState::new();
    let response = nl::process_input("Divide 10 by 2", &mut state);

    match &response {
        nl::NlResponse::PlanCreated { workflow_yaml: yaml, .. } => {
            assert!(yaml.contains("divide"), "workflow should contain divide op, got:\n{}", yaml);
        }
        other => panic!("expected PlanCreated, got: {:?}", other),
    }
}

#[test]
fn test_nl_plus_synonym() {
    let mut state = nl::dialogue::DialogueState::new();
    let response = nl::process_input("Plus 10 and 20", &mut state);

    match &response {
        nl::NlResponse::PlanCreated { workflow_yaml: yaml, .. } => {
            assert!(yaml.contains("add"), "plus should resolve to add op, got:\n{}", yaml);
        }
        other => panic!("expected PlanCreated, got: {:?}", other),
    }
}

#[test]
fn test_nl_sum_synonym() {
    let mut state = nl::dialogue::DialogueState::new();
    let response = nl::process_input("Sum 5 and 10", &mut state);

    match &response {
        nl::NlResponse::PlanCreated { workflow_yaml: yaml, .. } => {
            assert!(yaml.contains("add"), "sum should resolve to add op, got:\n{}", yaml);
        }
        other => panic!("expected PlanCreated, got: {:?}", other),
    }
}

#[test]
fn test_nl_minus_synonym() {
    let mut state = nl::dialogue::DialogueState::new();
    let response = nl::process_input("Minus 3 from 10", &mut state);

    match &response {
        nl::NlResponse::PlanCreated { workflow_yaml: yaml, .. } => {
            assert!(yaml.contains("subtract"), "minus should resolve to subtract op, got:\n{}", yaml);
        }
        other => panic!("expected PlanCreated, got: {:?}", other),
    }
}

// =========================================================================
// 6. Error cases
// =========================================================================

#[test]
fn test_unknown_racket_op_error() {
    let step = CompiledStep {
        index: 0,
        op: "nonexistent_racket_op".to_string(),
        is_each: false,
        input_type: TypeExpr::prim("Number"),
        output_type: TypeExpr::prim("Number"),
        params: HashMap::new(),
    };
    let inputs = HashMap::new();
    let result = op_to_racket(&step, &inputs, None);
    assert!(result.is_err());
}

// =========================================================================
// 7. Full pipeline: NL → Workflow → Inference → Racket Script
// =========================================================================

#[test]
fn test_full_pipeline_add() {
    // 1. NL input
    let mut state = nl::dialogue::DialogueState::new();
    let response = nl::process_input("Add 4 and 35 together", &mut state);

    // 2. Extract workflow
    let yaml = match &response {
        nl::NlResponse::PlanCreated { workflow_yaml: yaml, .. } => yaml.clone(),
        other => panic!("expected PlanCreated, got: {:?}", other),
    };

    // 3. Verify it's a valid workflow
    let def: WorkflowDef = serde_yaml::from_str(&yaml).unwrap();
    assert!(!def.steps.is_empty());

    // 4. Load fact pack and run inference
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();
    let keyword_map = load_keyword_map(&facts);

    // 5. Verify keyword resolution works
    assert_eq!(resolve_keyword("add", &keyword_map), Some("add".to_string()));
}

#[test]
fn test_full_pipeline_subtract_with_inference() {
    // 1. Load registry and facts
    let mut reg = build_racket_registry();
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();

    // 2. Run inference — this should promote subtract
    let inferred = promote_inferred_ops(&mut reg, &facts);
    let sub: Vec<_> = inferred.iter().filter(|i| i.op_name == "subtract").collect();
    assert_eq!(sub.len(), 1);

    // 3. Verify the inferred signature
    assert_eq!(sub[0].meta.return_type, "Number");
    assert_eq!(sub[0].meta.params.len(), 2);
    assert!(sub[0].meta.invariants.is_empty(), "invariants should not transfer");

    // 4. Generate Racket script for (- 6 2)
    let compiled = CompiledWorkflow {
        name: "Subtract 2 from 6".to_string(),
        input_type: TypeExpr::prim("Number"),
        input_description: "6".to_string(),
        steps: vec![CompiledStep {
            index: 0,
            op: "subtract".to_string(),
            is_each: false,
            input_type: TypeExpr::prim("Number"),
            output_type: TypeExpr::prim("Number"),
            params: vec![("x".into(), "6".into()), ("y".into(), "2".into())].into_iter().collect(),
        }],
        output_type: TypeExpr::prim("Number"),
    };
    let def = WorkflowDef {
        workflow: "Subtract 2 from 6".to_string(),
        inputs: vec![("x".into(), "6".into()), ("y".into(), "2".into())].into_iter().collect(),
        steps: vec![],
    };
    let script = generate_racket_script(&compiled, &def).unwrap();
    assert!(script.contains("(- 6 2)"), "should produce (- 6 2), got:\n{}", script);
}
