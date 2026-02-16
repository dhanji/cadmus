use reasoning_engine::algebra::{self, Fact, InferenceKind, InferenceRule};
use reasoning_engine::coding_strategy::{self, CodingStrategy, EXAMPLE_LONG_FUNCTION};
use reasoning_engine::generic_planner::{self, GenericGoal, PlanError, PlanNode};
use reasoning_engine::registry::{
    AlgebraicProperties, Literal, OpSignature, OperationRegistry, TypeId,
};
use reasoning_engine::strategy::ReasonerStrategy;

// ---------------------------------------------------------------------------
// Generic planner integration tests
// ---------------------------------------------------------------------------

fn noop_exec() -> reasoning_engine::registry::ExecFn {
    Box::new(|_| Ok("noop".into()))
}

#[test]
fn test_generic_planner_chain_integration() {
    // Build a 3-level chain: retrieve→Evidence, summarize(Evidence)→Claim, compare(Claim,Claim)→Contrast
    let mut reg = OperationRegistry::new();
    reg.register(
        "retrieve",
        OpSignature::leaf(TypeId::new("Evidence")),
        AlgebraicProperties::none(),
        noop_exec(),
    );
    reg.register(
        "summarize",
        OpSignature::new(vec![TypeId::new("Evidence")], TypeId::new("Claim")),
        AlgebraicProperties::none(),
        noop_exec(),
    );
    reg.register(
        "compare",
        OpSignature::new(
            vec![TypeId::new("Claim"), TypeId::new("Claim")],
            TypeId::new("Contrast"),
        ),
        AlgebraicProperties::commutative(),
        noop_exec(),
    );

    let goal = GenericGoal::simple("Contrast", vec![]);
    let plan = generic_planner::plan(&goal, &reg).unwrap();

    // Plan should be: compare(summarize(retrieve()), summarize(retrieve()))
    assert_eq!(*plan.output_type(), TypeId::new("Contrast"));
    assert_eq!(plan.op_count(), 5); // compare + 2*summarize + 2*retrieve
    assert!(plan.op_names().contains("compare"));
    assert!(plan.op_names().contains("summarize"));
    assert!(plan.op_names().contains("retrieve"));
}

#[test]
fn test_generic_planner_no_producer_integration() {
    let reg = OperationRegistry::new();
    let goal = GenericGoal::simple("NonExistent", vec![]);
    let result = generic_planner::plan(&goal, &reg);

    assert!(result.is_err());
    match result.unwrap_err() {
        PlanError::NoProducer { type_id } => {
            assert_eq!(type_id, TypeId::new("NonExistent"));
        }
        other => panic!("expected NoProducer, got: {:?}", other),
    }
}

#[test]
fn test_generic_planner_cycle_detection_integration() {
    let mut reg = OperationRegistry::new();
    reg.register(
        "a_from_b",
        OpSignature::new(vec![TypeId::new("B")], TypeId::new("A")),
        AlgebraicProperties::none(),
        noop_exec(),
    );
    reg.register(
        "b_from_a",
        OpSignature::new(vec![TypeId::new("A")], TypeId::new("B")),
        AlgebraicProperties::none(),
        noop_exec(),
    );

    let goal = GenericGoal::simple("A", vec![]).with_max_depth(10);
    let result = generic_planner::plan(&goal, &reg);
    assert!(result.is_err(), "should detect cycle or exceed depth");
}

// ---------------------------------------------------------------------------
// Algebra canonicalization integration tests
// ---------------------------------------------------------------------------

#[test]
fn test_algebra_dedup_commutative_plans_integration() {
    let mut reg = OperationRegistry::new();
    reg.register(
        "add",
        OpSignature::new(
            vec![TypeId::new("Num"), TypeId::new("Num")],
            TypeId::new("Num"),
        ),
        AlgebraicProperties::commutative(),
        noop_exec(),
    );

    // Two plans that differ only in operand order
    let plan_a = PlanNode::Op {
        op_name: "add".into(),
        output_type: TypeId::new("Num"),
        children: vec![
            PlanNode::Leaf { key: "3".into(), output_type: TypeId::new("Num") },
            PlanNode::Leaf { key: "2".into(), output_type: TypeId::new("Num") },
        ],
    };
    let plan_b = PlanNode::Op {
        op_name: "add".into(),
        output_type: TypeId::new("Num"),
        children: vec![
            PlanNode::Leaf { key: "2".into(), output_type: TypeId::new("Num") },
            PlanNode::Leaf { key: "3".into(), output_type: TypeId::new("Num") },
        ],
    };

    let unique = algebra::dedup_plans(vec![plan_a, plan_b], &reg);
    assert_eq!(unique.len(), 1, "commutative variants should dedup to 1 plan");
}

#[test]
fn test_algebra_associative_flatten_integration() {
    let mut reg = OperationRegistry::new();
    reg.register(
        "concat",
        OpSignature::new(
            vec![TypeId::new("Str"), TypeId::new("Str")],
            TypeId::new("Str"),
        ),
        AlgebraicProperties::associative(),
        noop_exec(),
    );

    // concat(concat("a", "b"), "c") should flatten to concat("a", "b", "c")
    let plan = PlanNode::Op {
        op_name: "concat".into(),
        output_type: TypeId::new("Str"),
        children: vec![
            PlanNode::Op {
                op_name: "concat".into(),
                output_type: TypeId::new("Str"),
                children: vec![
                    PlanNode::Leaf { key: "a".into(), output_type: TypeId::new("Str") },
                    PlanNode::Leaf { key: "b".into(), output_type: TypeId::new("Str") },
                ],
            },
            PlanNode::Leaf { key: "c".into(), output_type: TypeId::new("Str") },
        ],
    };

    let canonical = algebra::canonicalize(&plan, &reg);
    match &canonical {
        PlanNode::Op { children, .. } => {
            assert_eq!(children.len(), 3, "should flatten to 3 children");
        }
        _ => panic!("expected Op"),
    }
}

#[test]
fn test_algebra_identity_and_absorbing_integration() {
    let mut reg = OperationRegistry::new();
    reg.register(
        "mul",
        OpSignature::new(
            vec![TypeId::new("Num"), TypeId::new("Num")],
            TypeId::new("Num"),
        ),
        AlgebraicProperties {
            commutative: true,
            associative: true,
            identity: Some("1".to_string()),
            absorbing: Some("0".to_string()),
            idempotent: false,
        },
        noop_exec(),
    );

    // mul(x, 1) → x (identity removal)
    let plan_identity = PlanNode::Op {
        op_name: "mul".into(),
        output_type: TypeId::new("Num"),
        children: vec![
            PlanNode::Leaf { key: "x".into(), output_type: TypeId::new("Num") },
            PlanNode::Leaf { key: "1".into(), output_type: TypeId::new("Num") },
        ],
    };
    let canon = algebra::canonicalize(&plan_identity, &reg);
    match &canon {
        PlanNode::Leaf { key, .. } => assert_eq!(key, "x"),
        _ => panic!("expected identity removal to yield Leaf(x)"),
    }

    // mul(x, 0) → 0 (absorbing)
    let plan_absorb = PlanNode::Op {
        op_name: "mul".into(),
        output_type: TypeId::new("Num"),
        children: vec![
            PlanNode::Leaf { key: "x".into(), output_type: TypeId::new("Num") },
            PlanNode::Leaf { key: "0".into(), output_type: TypeId::new("Num") },
        ],
    };
    let canon = algebra::canonicalize(&plan_absorb, &reg);
    match &canon {
        PlanNode::Leaf { key, .. } => assert_eq!(key, "0"),
        _ => panic!("expected absorbing to yield Leaf(0)"),
    }
}

// ---------------------------------------------------------------------------
// Inference rule integration tests
// ---------------------------------------------------------------------------

#[test]
fn test_inference_transitive_chain_integration() {
    let facts = vec![
        Fact::new("gt", "A", "B"),
        Fact::new("gt", "B", "C"),
        Fact::new("gt", "C", "D"),
        Fact::new("gt", "D", "E"),
    ];
    let rules = vec![InferenceRule {
        name: "gt_transitive".into(),
        relation: "gt".into(),
        kind: InferenceKind::Transitive,
    }];

    let result = algebra::infer(&facts, &rules);

    // Should derive A>C, A>D, A>E, B>D, B>E, C>E
    let has = |l: &str, r: &str| {
        result.facts.iter().any(|f| f.left == l && f.right == r)
    };
    assert!(has("A", "C"), "should derive A>C");
    assert!(has("A", "D"), "should derive A>D");
    assert!(has("A", "E"), "should derive A>E");
    assert!(has("B", "D"), "should derive B>D");
    assert!(has("B", "E"), "should derive B>E");
    assert!(has("C", "E"), "should derive C>E");

    assert!(!result.derived.is_empty());
    assert!(result.conflicts.is_empty(), "no contradictions expected");
}

#[test]
fn test_inference_contradiction_integration() {
    let facts = vec![
        Fact::new("gt", "X", "Y"),
        Fact::new("gt", "Y", "X"),
    ];
    let rules = vec![InferenceRule {
        name: "gt_trans".into(),
        relation: "gt".into(),
        kind: InferenceKind::Transitive,
    }];

    let result = algebra::infer(&facts, &rules);
    assert!(!result.conflicts.is_empty(), "should detect X>Y and Y>X contradiction");
}

// ---------------------------------------------------------------------------
// CodingStrategy integration tests
// ---------------------------------------------------------------------------

#[test]
fn test_coding_strategy_end_to_end() {
    let output = coding_strategy::run_coding(
        EXAMPLE_LONG_FUNCTION,
        "Extract method to reduce function length",
    )
    .unwrap();

    assert!(!output.tests.is_empty(), "should produce test cases");
    assert_eq!(output.source, EXAMPLE_LONG_FUNCTION);
}

#[test]
fn test_coding_strategy_plan_uses_all_ops() {
    let strategy = CodingStrategy::new(EXAMPLE_LONG_FUNCTION, "refactor");
    let registry = strategy.build_registry();
    let goals = strategy.goals();

    let plan = generic_planner::plan(&goals[0], &registry).unwrap();
    let ops = plan.op_names();

    // The plan for TestCase should chain through all ops
    assert!(ops.contains("parse_source"));
    assert!(ops.contains("analyze_types"));
    assert!(ops.contains("detect_smells"));
    assert!(ops.contains("plan_refactoring"));
    assert!(ops.contains("generate_tests"));
}

#[test]
fn test_coding_strategy_compose_flattens() {
    let strategy = CodingStrategy::new("fn foo() {}", "test");
    let registry = strategy.build_registry();

    // Manually build nested compose and verify it flattens
    let nested = PlanNode::Op {
        op_name: "compose".into(),
        output_type: TypeId::new("Refactoring"),
        children: vec![
            PlanNode::Op {
                op_name: "compose".into(),
                output_type: TypeId::new("Refactoring"),
                children: vec![
                    PlanNode::Leaf { key: "r1".into(), output_type: TypeId::new("Refactoring") },
                    PlanNode::Leaf { key: "r2".into(), output_type: TypeId::new("Refactoring") },
                ],
            },
            PlanNode::Op {
                op_name: "compose".into(),
                output_type: TypeId::new("Refactoring"),
                children: vec![
                    PlanNode::Leaf { key: "r3".into(), output_type: TypeId::new("Refactoring") },
                    PlanNode::Leaf { key: "r4".into(), output_type: TypeId::new("Refactoring") },
                ],
            },
        ],
    };

    let canonical = algebra::canonicalize(&nested, &registry);
    match &canonical {
        PlanNode::Op { children, .. } => {
            assert_eq!(children.len(), 4, "compose(compose(r1,r2), compose(r3,r4)) should flatten to compose(r1,r2,r3,r4)");
        }
        _ => panic!("expected Op"),
    }
}

#[test]
fn test_coding_strategy_no_producer_for_unknown_type() {
    let strategy = CodingStrategy::new("fn foo() {}", "test");
    let registry = strategy.build_registry();

    let goal = GenericGoal::simple(TypeId::new("Deployment"), strategy.available_literals());
    let result = generic_planner::plan(&goal, &registry);
    assert!(result.is_err());
}

// ---------------------------------------------------------------------------
// Cross-strategy integration: both strategies work through same engine
// ---------------------------------------------------------------------------

#[test]
fn test_both_strategies_use_same_registry_interface() {
    // ComparisonStrategy
    let comp_goal = reasoning_engine::types::Goal {
        description: "test".into(),
        entities: vec!["putin".into(), "stalin".into()],
        fact_pack_paths: vec!["data/putin_stalin.yaml".into()],
    };
    let comp_strategy = reasoning_engine::strategy::ComparisonStrategy::new(comp_goal).unwrap();
    let comp_reg = comp_strategy.build_registry();

    // CodingStrategy
    let code_strategy = CodingStrategy::new("fn foo() {}", "test");
    let code_reg = code_strategy.build_registry();

    // Both registries should be valid OperationRegistries
    assert!(!comp_reg.op_names().is_empty());
    assert!(!code_reg.op_names().is_empty());

    // They should have different type vocabularies
    assert!(comp_reg.has_producer(&TypeId::new("Evidence")));
    assert!(!code_reg.has_producer(&TypeId::new("Evidence")));

    assert!(code_reg.has_producer(&TypeId::new("AST")));
    assert!(!comp_reg.has_producer(&TypeId::new("AST")));
}

#[test]
fn test_generic_planner_with_literals() {
    let mut reg = OperationRegistry::new();
    reg.register(
        "transform",
        OpSignature::new(vec![TypeId::new("Input")], TypeId::new("Output")),
        AlgebraicProperties::none(),
        noop_exec(),
    );

    let goal = GenericGoal::simple(
        "Output",
        vec![Literal::new("Input", "my_input", "data")],
    );
    let plan = generic_planner::plan(&goal, &reg).unwrap();

    assert_eq!(plan.op_count(), 1);
    assert!(plan.leaf_keys().contains("my_input"));
}
