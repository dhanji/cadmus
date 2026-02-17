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
    InferenceKind, infer_type_symmetric_op,
};
use cadmus::racket_executor::{generate_racket_script, op_to_racket};
use cadmus::workflow::{WorkflowDef, CompiledStep, CompiledWorkflow};
use cadmus::type_expr::TypeExpr;
use cadmus::nl;
use std::collections::HashMap;

const RACKET_OPS_YAML: &str = include_str!("../data/racket_ops.yaml");
const RACKET_FACTS_YAML: &str = include_str!("../data/racket_facts.yaml");

fn make_racket_reg() -> cadmus::registry::OperationRegistry {
    let mut reg = load_ops_pack_str(RACKET_OPS_YAML).unwrap();
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();
    promote_inferred_ops(&mut reg, &facts);
    reg
}

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
    // subtract, multiply, divide are discovered from fact pack, not in ops pack
    for op in &["add", "modulo", "expt", "abs", "min", "max"] {
        assert!(reg.get_poly(op).is_some(), "missing arithmetic op: {}", op);
    }
    for op in &["subtract", "multiply", "divide"] {
        assert!(reg.get_poly(op).is_none(), "{} should be discovered, not in ops pack", op);
    }
}

#[test]
fn test_racket_ops_list_present() {
    let reg = build_racket_registry();
    // Ops-pack list ops (anchors + unique shapes)
    for op in &["cons", "car", "cdr", "list_new", "append", "length", "list_ref", "member", "sort_list", "flatten"] {
        assert!(reg.get_poly(op).is_some(), "missing list op from ops pack: {}", op);
    }
    // remove and list_reverse are NOT in the ops pack — they are discovered
    // from the fact pack by the inference engine.
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();
    let mut reg_with_inference = reg;
    promote_inferred_ops(&mut reg_with_inference, &facts);
    for op in &["remove", "list_reverse"] {
        assert!(reg_with_inference.get_poly(op).is_some(),
            "missing discovered list op: {}", op);
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
fn test_racket_subtract_not_in_ops_pack() {
    // subtract is discovered from the fact pack, not listed in racket_ops.yaml
    let reg = build_racket_registry();
    assert!(reg.get_poly("subtract").is_none(),
        "subtract should NOT be in the ops pack — it's discovered from the fact pack");
}

// =========================================================================
// 2. Fact pack and keyword resolution
// =========================================================================

#[test]
fn test_racket_facts_load() {
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();
    assert_eq!(facts.pack.entities.len(), 14);
    assert_eq!(facts.pack.axes.len(), 6);
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

    // Before: subtract is not in the registry (discovered from fact pack)
    assert!(reg.get_poly("subtract").is_none());

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
    let racket_reg = make_racket_reg();
    let script = generate_racket_script(&compiled, &def, &racket_reg).unwrap();
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
    let racket_reg = make_racket_reg();
    let script = generate_racket_script(&compiled, &def, &racket_reg).unwrap();
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

            let racket_reg = make_racket_reg();
    let script = generate_racket_script(&compiled, &def, &racket_reg).unwrap();
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
    let racket_reg = make_racket_reg();
    let result = op_to_racket(&step, &inputs, None, &racket_reg);
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
    let racket_reg = make_racket_reg();
    let script = generate_racket_script(&compiled, &def, &racket_reg).unwrap();
    assert!(script.contains("(- 6 2)"), "should produce (- 6 2), got:\n{}", script);
}

// =========================================================================
// 8. Type-symmetric inference chain
// =========================================================================

#[test]
fn test_full_inference_chain_all_four_ops() {
    let mut reg = build_racket_registry();
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();

    // Before promotion: only add has meta
    assert!(reg.get_poly("add").unwrap().meta.is_some());
    assert!(reg.get_poly("subtract").is_none(), "subtract discovered, not in ops pack");
    assert!(reg.get_poly("multiply").is_none(), "multiply discovered, not in ops pack");
    assert!(reg.get_poly("divide").is_none(), "divide discovered, not in ops pack");

    let inferred = promote_inferred_ops(&mut reg, &facts);

    // After promotion: all four have meta
    for op in &["add", "subtract", "multiply", "divide"] {
        assert!(reg.get_poly(op).unwrap().meta.is_some(),
            "{} should have meta after promotion", op);
    }

    // Verify inference paths
    let sub = inferred.iter().find(|i| i.op_name == "subtract").unwrap();
    assert_eq!(sub.inference_kind, InferenceKind::OpSymmetric);
    assert_eq!(sub.inferred_from, "add");

    let mul = inferred.iter().find(|i| i.op_name == "multiply").unwrap();
    // multiply can be inferred via type-symmetric (from add, class=binop)
    // OR via op-symmetric (from divide, if divide was inferred first).
    // HashMap iteration order makes this non-deterministic — both are valid.
    let mul_is_type_sym = matches!(&mul.inference_kind, InferenceKind::TypeSymmetric { class } if class == "binop");
    let mul_is_op_sym = matches!(&mul.inference_kind, InferenceKind::OpSymmetric);
    assert!(mul_is_type_sym || mul_is_op_sym,
        "multiply should be inferred via type-symmetric or op-symmetric, got {:?}", mul.inference_kind);

    let div = inferred.iter().find(|i| i.op_name == "divide").unwrap();
    // divide can be inferred via op-symmetric (from multiply) or type-symmetric.
    let div_is_op_sym = matches!(&div.inference_kind, InferenceKind::OpSymmetric);
    let div_is_type_sym = matches!(&div.inference_kind, InferenceKind::TypeSymmetric { .. });
    assert!(div_is_op_sym || div_is_type_sym,
        "divide should be inferred via op-symmetric or type-symmetric, got {:?}", div.inference_kind);

    // Arithmetic: 3 inferred (subtract, multiply, divide)
    let arith_inferred: Vec<_> = inferred.iter()
        .filter(|i| ["subtract", "multiply", "divide"].contains(&i.op_name.as_str()))
        .collect();
    assert_eq!(arith_inferred.len(), 3, "should infer exactly 3 arithmetic ops");

    // List: 2 inferred (remove, list_reverse) — from fact pack list entities
    let list_inferred: Vec<_> = inferred.iter()
        .filter(|i| ["remove", "list_reverse"].contains(&i.op_name.as_str()))
        .collect();
    assert_eq!(list_inferred.len(), 2, "should infer exactly 2 list ops");

    // Comparison: 4 inferred (greater_than, less_than_or_equal, greater_than_or_equal)
    // greater_than via op-symmetric from less_than; lte/gte via type-symmetric
    let cmp_inferred: Vec<_> = inferred.iter()
        .filter(|i| ["greater_than", "less_than_or_equal", "greater_than_or_equal"].contains(&i.op_name.as_str()))
        .collect();
    assert_eq!(cmp_inferred.len(), 3, "should infer exactly 3 comparison ops");

    // String: 1 inferred (string_downcase via type-symmetric from string_upcase)
    let str_inferred: Vec<_> = inferred.iter()
        .filter(|i| i.op_name == "string_downcase")
        .collect();
    assert_eq!(str_inferred.len(), 1, "should infer exactly 1 string op");
}

#[test]
fn test_type_symmetric_multiply_from_add() {
    let reg = build_racket_registry();
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();

    let inferred = infer_type_symmetric_op("multiply", &reg, &facts).unwrap();
    assert_eq!(inferred.op_name, "multiply");
    assert_eq!(inferred.inferred_from, "add");
    assert_eq!(inferred.racket_symbol, "*");
    assert_eq!(inferred.meta.return_type, "Number");
    assert_eq!(inferred.meta.params.len(), 2);
    assert_eq!(inferred.meta.category.as_deref(), Some("arithmetic"));
    assert!(inferred.meta.invariants.is_empty(),
        "invariants must not transfer across type symmetry");
    assert!(inferred.invariants_dropped);
    assert_eq!(inferred.inference_kind, InferenceKind::TypeSymmetric {
        class: "binop".to_string(),
    });
}

#[test]
fn test_divide_unblocked_by_type_symmetric_chain() {
    // divide's symmetric partner is multiply, which starts as a stub.
    // After type-symmetric inference promotes multiply, op-symmetric
    // inference can then promote divide.
    let mut reg = build_racket_registry();
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();

    // Direct op-symmetric fails (multiply has no meta)
    assert!(infer_symmetric_op("divide", &reg, &facts).is_err());

    // Run full promotion
    let inferred = promote_inferred_ops(&mut reg, &facts);

    // divide should now have meta, inferred from multiply
    let div = inferred.iter().find(|i| i.op_name == "divide").unwrap();
    // divide may be inferred via op-symmetric from multiply (if multiply was promoted first)
    // or via type-symmetric from add (if iteration order puts divide before multiply).
    // Both are correct — the result is the same (Number, Number) → Number signature.
    assert!(div.inferred_from == "multiply" || div.inferred_from == "add",
        "divide should be inferred from multiply or add, got: {}", div.inferred_from);
    assert_eq!(div.meta.return_type, "Number");
}

#[test]
fn test_promotion_preserves_add_invariants() {
    let mut reg = build_racket_registry();
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();

    let add_before = reg.get_poly("add").unwrap().meta.clone().unwrap();
    promote_inferred_ops(&mut reg, &facts);
    let add_after = reg.get_poly("add").unwrap().meta.clone().unwrap();

    assert_eq!(add_before.invariants, add_after.invariants,
        "add's invariants must survive promotion");
    assert_eq!(add_before.invariants, vec!["y>0 => x+y>x"]);
}

#[test]
fn test_modulo_not_inferred() {
    let mut reg = build_racket_registry();
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();

    promote_inferred_ops(&mut reg, &facts);

    // modulo has no symmetric partner and no type_symmetry_class
    assert!(reg.get_poly("modulo").unwrap().meta.is_none(),
        "modulo should remain a stub");
}
