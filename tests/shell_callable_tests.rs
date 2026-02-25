// ==========================================================================
// Shell Callable Tests
// ==========================================================================
//
// Tests for the shell-callable ops feature:
//   - Fact pack loading (macos_cli.facts.yaml)
//   - Anchor registration (shell_ls, shell_ps, etc.)
//   - Type-symmetric discovery (cat, head, tail, sort, df, wc)
//   - Phase 4 submode discovery (shell_ls_long, shell_ps_aux, etc.)
//   - Racket wrapper generation (shell-exec, shell-lines, shell-quote)
//   - Mixed pipelines (shell → racket → shell)
//   - Edge cases and error handling

use std::collections::HashMap;

use cadmus::fact_pack::{FactPack, FactPackIndex};
use cadmus::racket_executor::{op_to_racket, generate_racket_script};
use cadmus::racket_strategy::{
    load_racket_facts_from_str, promote_inferred_ops,
    discover_shell_submodes, InferenceKind,
};
use cadmus::registry::{load_ops_pack_str, OperationRegistry};
use cadmus::type_expr::TypeExpr;
use cadmus::plan::{CompiledStep, CompiledPlan, PlanDef};

const RACKET_OPS_YAML: &str = include_str!("../data/packs/ops/racket.ops.yaml");
const RACKET_FACTS_YAML: &str = include_str!("../data/packs/facts/racket.facts.yaml");
const MACOS_CLI_FACTS_YAML: &str = include_str!("../data/packs/facts/macos_cli.facts.yaml");

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn make_full_registry() -> OperationRegistry {
    let mut reg = load_ops_pack_str(RACKET_OPS_YAML).unwrap();
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();
    promote_inferred_ops(&mut reg, &facts);
    let cli_pack: FactPack = serde_yaml::from_str(MACOS_CLI_FACTS_YAML).unwrap();
    let cli_facts = FactPackIndex::build(cli_pack);
    discover_shell_submodes(&mut reg, &facts, &cli_facts);
    reg
}

fn make_step(op: &str, params: Vec<(&str, &str)>) -> CompiledStep {
    CompiledStep {
        index: 0,
        op: op.to_string(),
        input_type: TypeExpr::prim("String"),
        output_type: TypeExpr::cons("List", vec![TypeExpr::prim("String")]),
        params: params.into_iter().map(|(k, v)| (k.to_string(), v.to_string())).collect(),
    ..Default::default()
    }
}

fn make_inputs(pairs: Vec<(&str, &str)>) -> Vec<cadmus::plan::PlanInput> {
    pairs.into_iter().map(|(k, _v)| cadmus::plan::PlanInput::bare(k)).collect()
}

// ===========================================================================
// 1. Fact Pack Loading
// ===========================================================================

#[test]
fn test_cli_facts_load() {
    let pack: FactPack = serde_yaml::from_str(MACOS_CLI_FACTS_YAML).unwrap();
    assert_eq!(pack.entities.len(), 74, "expected 74 CLI tool entities");
    assert_eq!(pack.axes.len(), 5);
    assert!(pack.claims.len() >= 60);
    assert!(pack.evidence.len() >= 5);
}

#[test]
fn test_cli_facts_entities() {
    let pack: FactPack = serde_yaml::from_str(MACOS_CLI_FACTS_YAML).unwrap();
    let ids: Vec<&str> = pack.entities.iter().map(|e| e.id.as_str()).collect();
    for expected in &["cli_ls", "cli_ps", "cli_find", "cli_grep", "cli_du", "cli_df",
                       "cli_cat", "cli_head", "cli_tail", "cli_sort", "cli_wc", "cli_curl"] {
        assert!(ids.contains(expected), "missing entity: {}", expected);
    }
}

#[test]
fn test_cli_facts_submodes_count() {
    let pack: FactPack = serde_yaml::from_str(MACOS_CLI_FACTS_YAML).unwrap();
    let submodes: Vec<_> = pack.properties.iter()
        .filter(|p| p.key.starts_with("submode_"))
        .collect();
    assert_eq!(submodes.len(), 176, "expected 141 submode properties");
}

#[test]
fn test_cli_facts_ls_has_six_submodes() {
    let pack: FactPack = serde_yaml::from_str(MACOS_CLI_FACTS_YAML).unwrap();
    let ls_submodes: Vec<_> = pack.properties.iter()
        .filter(|p| p.entity == "cli_ls" && p.key.starts_with("submode_"))
        .collect();
    assert_eq!(ls_submodes.len(), 6, "ls should have 6 submodes");
}

#[test]
fn test_cli_facts_type_symmetry_classes() {
    let pack: FactPack = serde_yaml::from_str(MACOS_CLI_FACTS_YAML).unwrap();
    let mut classes: HashMap<String, Vec<String>> = HashMap::new();
    for p in &pack.properties {
        if p.key == "type_symmetry_class" {
            classes.entry(p.value.clone()).or_default().push(p.entity.clone());
        }
    }
    assert_eq!(classes.len(), 11, "expected 11 type symmetry classes");
    assert_eq!(classes["shell_text_lines"].len(), 27, "text_lines should have 27 tools");
    assert_eq!(classes["shell_tabular"].len(), 3, "tabular should have 3 tools");
    assert_eq!(classes["shell_tree"].len(), 2, "tree should have 2 tools");
}

#[test]
fn test_cli_facts_all_entities_have_base_command() {
    let pack: FactPack = serde_yaml::from_str(MACOS_CLI_FACTS_YAML).unwrap();
    for entity in &pack.entities {
        let has_cmd = pack.properties.iter()
            .any(|p| p.entity == entity.id && p.key == "base_command");
        assert!(has_cmd, "entity {} missing base_command property", entity.id);
    }
}

#[test]
fn test_cli_facts_all_entities_have_category() {
    let pack: FactPack = serde_yaml::from_str(MACOS_CLI_FACTS_YAML).unwrap();
    for entity in &pack.entities {
        let cat = pack.properties.iter()
            .find(|p| p.entity == entity.id && p.key == "category_name");
        assert!(cat.is_some(), "entity {} missing category_name", entity.id);
        assert_eq!(cat.unwrap().value, "shell", "entity {} should have category=shell", entity.id);
    }
}

// ===========================================================================
// 2. Anchor Registration
// ===========================================================================

#[test]
fn test_anchor_ops_in_registry() {
    let reg = load_ops_pack_str(RACKET_OPS_YAML).unwrap();
    for name in &["shell_ls", "shell_ps", "shell_find", "shell_grep", "shell_du", "shell_curl"] {
        let entry = reg.get_poly(name).unwrap_or_else(|| panic!("missing anchor: {}", name));
        assert!(entry.meta.is_some(), "{} should have metasig", name);
        let meta = entry.meta.as_ref().unwrap();
        assert_eq!(meta.category.as_deref(), Some("shell"), "{} should have category=shell", name);
        assert_eq!(meta.effects.as_deref(), Some("io"), "{} should have effects=io", name);
    }
}

#[test]
fn test_anchor_racket_symbols() {
    let reg = load_ops_pack_str(RACKET_OPS_YAML).unwrap();
    assert_eq!(reg.get_poly("shell_ls").unwrap().racket_symbol.as_deref(), Some("shell-ls"));
    assert_eq!(reg.get_poly("shell_ps").unwrap().racket_symbol.as_deref(), Some("shell-ps"));
    assert_eq!(reg.get_poly("shell_find").unwrap().racket_symbol.as_deref(), Some("shell-find"));
    assert_eq!(reg.get_poly("shell_grep").unwrap().racket_symbol.as_deref(), Some("shell-grep"));
    assert_eq!(reg.get_poly("shell_du").unwrap().racket_symbol.as_deref(), Some("shell-du"));
    assert_eq!(reg.get_poly("shell_curl").unwrap().racket_symbol.as_deref(), Some("shell-curl"));
}

#[test]
fn test_anchor_no_name_collisions() {
    let reg = load_ops_pack_str(RACKET_OPS_YAML).unwrap();
    // shell_ls should not collide with list_dir
    assert!(reg.get_poly("list_dir").is_none(), "list_dir is in fs_ops, not racket_ops");
    assert!(reg.get_poly("shell_ls").is_some());
    assert!(reg.get_poly("add").is_some(), "existing ops should still be there");
}

#[test]
fn test_anchor_coexist_with_existing_ops() {
    let reg = load_ops_pack_str(RACKET_OPS_YAML).unwrap();
    let names = reg.poly_op_names();
    assert!(names.len() >= 58, "expected at least 58 ops (52 original + 6 anchors), got {}", names.len());
}

// ===========================================================================
// 3. Type-Symmetric Discovery
// ===========================================================================

#[test]
fn test_type_symmetric_discovery_text_lines() {
    let mut reg = load_ops_pack_str(RACKET_OPS_YAML).unwrap();
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();
    let inferred = promote_inferred_ops(&mut reg, &facts);

    // cat, head, tail, sort should be discovered via type-symmetric from shell_text_lines class.
    // The specific anchor entity depends on property iteration order (alphabetical in compact
    // format), so we check the class but not the specific inferred_from entity.
    for name in &["shell_cat", "shell_head", "shell_tail", "shell_sort"] {
        let inf = inferred.iter().find(|i| i.op_name == *name);
        assert!(inf.is_some(), "{} should be inferred", name);
        let inf = inf.unwrap();
        assert!(matches!(inf.inference_kind, InferenceKind::TypeSymmetric { ref class } if class == "shell_text_lines"),
            "{} should be type-symmetric from shell_text_lines", name);
    }
}

#[test]
fn test_type_symmetric_discovery_tabular() {
    let mut reg = load_ops_pack_str(RACKET_OPS_YAML).unwrap();
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();
    let inferred = promote_inferred_ops(&mut reg, &facts);

    // shell_df can be discovered via type-symmetric or op-symmetric depending on
    // HashMap iteration order. Both paths produce identical signatures.
    assert!(inferred.iter().any(|i| i.op_name == "shell_df"), "shell_df must be inferred");
    assert!(reg.get_poly("shell_df").is_some(), "shell_df must be registered");
    assert!(reg.racket_symbol("shell_df").is_some(), "shell_df must have a racket_symbol");
}

#[test]
fn test_type_symmetric_discovery_single_value() {
    let mut reg = load_ops_pack_str(RACKET_OPS_YAML).unwrap();
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();
    let inferred = promote_inferred_ops(&mut reg, &facts);

    let wc = inferred.iter().find(|i| i.op_name == "shell_wc").unwrap();
    assert!(matches!(wc.inference_kind, InferenceKind::TypeSymmetric { ref class } if class == "shell_single_value"));
    assert_eq!(wc.inferred_from, "shell_du");
}

#[test]
fn test_all_12_shell_ops_registered_after_inference() {
    let mut reg = load_ops_pack_str(RACKET_OPS_YAML).unwrap();
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();
    promote_inferred_ops(&mut reg, &facts);

    for name in &["shell_ls", "shell_cat", "shell_head", "shell_tail", "shell_sort",
                   "shell_ps", "shell_df", "shell_find", "shell_grep",
                   "shell_du", "shell_wc", "shell_curl"] {
        let entry = reg.get_poly(name);
        assert!(entry.is_some(), "{} should be in registry", name);
        assert!(entry.unwrap().meta.is_some(), "{} should have meta", name);
    }
}

// ===========================================================================
// 4. Phase 4: Submode Discovery
// ===========================================================================

#[test]
fn test_submode_discovery_ls() {
    let reg = make_full_registry();
    for name in &["shell_ls_long", "shell_ls_all", "shell_ls_recursive",
                   "shell_ls_by_size", "shell_ls_by_time", "shell_ls_all_long"] {
        let entry = reg.get_poly(name);
        assert!(entry.is_some(), "{} should be discovered", name);
        assert!(entry.unwrap().meta.is_some(), "{} should have meta", name);
    }
}

#[test]
fn test_submode_discovery_ps() {
    let reg = make_full_registry();
    for name in &["shell_ps_aux", "shell_ps_every", "shell_ps_full",
                   "shell_ps_by_cpu", "shell_ps_by_mem"] {
        assert!(reg.get_poly(name).is_some(), "{} should be discovered", name);
    }
}

#[test]
fn test_submode_discovery_grep() {
    let reg = make_full_registry();
    for name in &["shell_grep_recursive", "shell_grep_case_insensitive",
                   "shell_grep_line_numbers", "shell_grep_filenames_only",
                   "shell_grep_count", "shell_grep_invert", "shell_grep_extended_regex"] {
        assert!(reg.get_poly(name).is_some(), "{} should be discovered", name);
    }
}

#[test]
fn test_submode_discovery_non_anchor_tools() {
    // cat, head, tail, sort, df, wc are discovered via type-symmetric,
    // then their submodes should also be discovered in phase 4
    let reg = make_full_registry();
    assert!(reg.get_poly("shell_cat_number").is_some(), "cat -n submode");
    assert!(reg.get_poly("shell_head_lines").is_some(), "head -n submode");
    assert!(reg.get_poly("shell_tail_lines").is_some(), "tail -n submode");
    assert!(reg.get_poly("shell_tail_follow").is_some(), "tail -f submode");
    assert!(reg.get_poly("shell_sort_numeric").is_some(), "sort -n submode");
    assert!(reg.get_poly("shell_sort_reverse").is_some(), "sort -r submode");
    assert!(reg.get_poly("shell_df_human").is_some(), "df -h submode");
    assert!(reg.get_poly("shell_wc_lines").is_some(), "wc -l submode");
}

#[test]
fn test_submode_total_count() {
    let mut reg = load_ops_pack_str(RACKET_OPS_YAML).unwrap();
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();
    promote_inferred_ops(&mut reg, &facts);
    let cli_pack: FactPack = serde_yaml::from_str(MACOS_CLI_FACTS_YAML).unwrap();
    let cli_facts = FactPackIndex::build(cli_pack);
    let submodes = discover_shell_submodes(&mut reg, &facts, &cli_facts);
    assert_eq!(submodes.len(), 176, "expected 141 submode ops");
}

#[test]
fn test_submode_inference_kind() {
    let mut reg = load_ops_pack_str(RACKET_OPS_YAML).unwrap();
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();
    promote_inferred_ops(&mut reg, &facts);
    let cli_pack: FactPack = serde_yaml::from_str(MACOS_CLI_FACTS_YAML).unwrap();
    let cli_facts = FactPackIndex::build(cli_pack);
    let submodes = discover_shell_submodes(&mut reg, &facts, &cli_facts);

    let ls_long = submodes.iter().find(|i| i.op_name == "shell_ls_long").unwrap();
    assert!(matches!(ls_long.inference_kind, InferenceKind::ShellSubmode { ref base_op, ref flags }
        if base_op == "shell_ls" && flags == "-l"));
    assert_eq!(ls_long.inferred_from, "shell_ls");
}

#[test]
fn test_submode_meta_has_flags() {
    let reg = make_full_registry();
    let entry = reg.get_poly("shell_ls_long").unwrap();
    let meta = entry.meta.as_ref().unwrap();
    assert!(meta.invariants.iter().any(|i| i == "flags: -l"), "should have flags: -l");
    assert!(meta.invariants.iter().any(|i| i == "base_command: ls"), "should have base_command: ls");
}

#[test]
fn test_submode_compound_flags() {
    let reg = make_full_registry();
    let entry = reg.get_poly("shell_ls_by_time").unwrap();
    let meta = entry.meta.as_ref().unwrap();
    assert!(meta.invariants.iter().any(|i| i == "flags: -lt"), "ls_by_time should have flags: -lt");
}

#[test]
fn test_submode_ps_compound_flags() {
    let reg = make_full_registry();
    let entry = reg.get_poly("shell_ps_by_cpu").unwrap();
    let meta = entry.meta.as_ref().unwrap();
    assert!(meta.invariants.iter().any(|i| i == "flags: aux --sort=-%cpu"),
        "ps_by_cpu should have compound flags");
}

// ===========================================================================
// 5. Racket Wrapper Generation
// ===========================================================================

#[test]
fn test_shell_ls_generates_shell_lines() {
    let reg = make_full_registry();
    let step = make_step("shell_ls", vec![("path", "/tmp")]);
    let inputs = make_inputs(vec![]);
    let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
    assert!(expr.expr.contains("shell-lines"), "should use shell-lines");
    assert!(expr.expr.contains("shell-quote"), "should use shell-quote for safety");
    assert!(expr.expr.contains("ls"), "should contain ls command");
}

#[test]
fn test_shell_ls_long_includes_flags() {
    let reg = make_full_registry();
    let step = make_step("shell_ls_long", vec![("path", "/tmp")]);
    let inputs = make_inputs(vec![]);
    let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
    assert!(expr.expr.contains("ls -l"), "should contain 'ls -l'");
}

#[test]
fn test_shell_ps_nullary() {
    let reg = make_full_registry();
    let step = make_step("shell_ps", vec![]);
    let inputs = make_inputs(vec![]);
    let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
    assert_eq!(expr.expr, "(shell-lines \"ps\")");
    assert!(!expr.uses_prev);
}

#[test]
fn test_shell_ps_aux_nullary_with_flags() {
    let reg = make_full_registry();
    let step = make_step("shell_ps_aux", vec![]);
    let inputs = make_inputs(vec![]);
    let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
    assert_eq!(expr.expr, "(shell-lines \"ps aux\")");
}

#[test]
fn test_shell_grep_binary() {
    let reg = make_full_registry();
    let step = make_step("shell_grep", vec![("x", "error"), ("y", "/var/log/system.log")]);
    let inputs = make_inputs(vec![]);
    let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
    assert!(expr.expr.contains("grep"), "should contain grep");
    assert!(expr.expr.contains("shell-quote"), "should quote arguments");
}

#[test]
fn test_shell_op_with_prev_binding() {
    let reg = make_full_registry();
    let step = make_step("shell_ls", vec![]);
    let inputs = make_inputs(vec![]);
    let expr = op_to_racket(&step, &inputs, Some("step-1"), &reg, false, &HashMap::new()).unwrap();
    assert!(expr.uses_prev, "should use prev binding");
    assert!(expr.expr.contains("step-1"), "should reference step-1");
}

#[test]
fn test_shell_quote_prevents_injection() {
    let reg = make_full_registry();
    // Path with spaces and special chars
    let step = make_step("shell_ls", vec![("path", "/path/with spaces/and;rm -rf /")]);
    let inputs = make_inputs(vec![]);
    let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
    assert!(expr.expr.contains("shell-quote"), "must use shell-quote for safety");
    // The path is passed through shell-quote at runtime — the Racket string
    // contains the raw value but shell-quote escapes it before shell execution
}

// ===========================================================================
// 6. Script Generation
// ===========================================================================

#[test]
fn test_script_preamble_emitted_for_shell_ops() {
    let reg = make_full_registry();
    let compiled = CompiledPlan {
        name: "list files".to_string(),
        input_type: TypeExpr::prim("String"),
        input_description: "/tmp".to_string(),
        steps: vec![make_step("shell_ls", vec![("path", "/tmp")])],
        output_type: TypeExpr::cons("List", vec![TypeExpr::prim("String")]),
    };
    let def = PlanDef {
        name: "list files".to_string(),
        inputs: vec![],
        output: None,
        steps: vec![],
    bindings: HashMap::new(),
    };
    let script = generate_racket_script(&compiled, &def, &reg).unwrap();
    assert!(script.contains("(require racket/system)"), "should have require");
    assert!(script.contains("shell-exec"), "should define shell-exec");
    assert!(script.contains("shell-lines"), "should define shell-lines");
    assert!(script.contains("shell-quote"), "should define shell-quote");
}

#[test]
fn test_script_no_preamble_for_pure_racket() {
    let reg = make_full_registry();
    let compiled = CompiledPlan {
        name: "add numbers".to_string(),
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
    let def = PlanDef {
        name: "add numbers".to_string(),
        inputs: vec![],
        output: None,
        steps: vec![],
    bindings: HashMap::new(),
    };
    let script = generate_racket_script(&compiled, &def, &reg).unwrap();
    assert!(!script.contains("racket/system"), "pure Racket script should NOT have shell preamble");
}

#[test]
fn test_mixed_pipeline_script() {
    let reg = make_full_registry();
    let compiled = CompiledPlan {
        name: "list and count".to_string(),
        input_type: TypeExpr::prim("String"),
        input_description: "/tmp".to_string(),
        steps: vec![
            CompiledStep {
                index: 0,
                op: "shell_ls".to_string(),
                input_type: TypeExpr::prim("String"),
                output_type: TypeExpr::cons("List", vec![TypeExpr::prim("String")]),
                params: vec![("path".into(), "/tmp".into())].into_iter().collect(),
            ..Default::default()
            },
            CompiledStep {
                index: 1,
                op: "length".to_string(),
                input_type: TypeExpr::cons("List", vec![TypeExpr::prim("String")]),
                output_type: TypeExpr::prim("Number"),
                params: HashMap::new(),
            ..Default::default()
            },
        ],
        output_type: TypeExpr::prim("Number"),
    };
    let def = PlanDef {
        name: "list and count".to_string(),
        inputs: vec![],
        output: None,
        steps: vec![],
    bindings: HashMap::new(),
    };
    let script = generate_racket_script(&compiled, &def, &reg).unwrap();
    assert!(script.contains("(require racket/system)"), "mixed pipeline needs preamble");
    assert!(script.contains("shell-lines"), "step 1 should use shell-lines");
    assert!(script.contains("(length"), "step 2 should use pure Racket length");
    assert!(script.contains("let*"), "multi-step should use let*");
}

// ===========================================================================
// 7. Full Registry Integration
// ===========================================================================

#[test]
fn test_full_registry_includes_shell_ops() {
    let reg = cadmus::fs_types::build_full_registry();
    // Check anchors
    for name in &["shell_ls", "shell_ps", "shell_find", "shell_grep", "shell_du", "shell_curl"] {
        assert!(reg.get_poly(name).is_some(), "full registry missing anchor: {}", name);
    }
    // Check discovered
    for name in &["shell_cat", "shell_head", "shell_tail", "shell_sort", "shell_df", "shell_wc"] {
        assert!(reg.get_poly(name).is_some(), "full registry missing discovered: {}", name);
    }
    // Check submodes
    for name in &["shell_ls_long", "shell_ps_aux", "shell_grep_recursive", "shell_wc_lines"] {
        assert!(reg.get_poly(name).is_some(), "full registry missing submode: {}", name);
    }
}

#[test]
fn test_full_registry_op_count() {
    let reg = cadmus::fs_types::build_full_registry();
    let total = reg.poly_op_names().len();
    // Should be significantly more than the pre-shell count
    assert!(total >= 200, "expected at least 200 ops, got {}", total);
}

// ===========================================================================
// 8. Edge Cases
// ===========================================================================

#[test]
fn test_existing_ops_unaffected() {
    let reg = make_full_registry();
    // Arithmetic ops still work
    assert!(reg.get_poly("add").is_some());
    assert!(reg.get_poly("subtract").is_some());
    // List ops still work
    assert!(reg.get_poly("cons").is_some());
    assert!(reg.get_poly("car").is_some());
    // Higher-order ops still work
    assert!(reg.get_poly("racket_map").is_some());
    assert!(reg.get_poly("racket_filter").is_some());
}

#[test]
fn test_submode_idempotent() {
    // Running discover_shell_submodes twice should not create duplicates
    let mut reg = load_ops_pack_str(RACKET_OPS_YAML).unwrap();
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();
    promote_inferred_ops(&mut reg, &facts);
    let cli_pack: FactPack = serde_yaml::from_str(MACOS_CLI_FACTS_YAML).unwrap();
    let cli_facts = FactPackIndex::build(cli_pack);

    let first = discover_shell_submodes(&mut reg, &facts, &cli_facts);
    let count_after_first = reg.poly_op_names().len();

    // Run again — should discover 0 new ops (all already registered)
    let cli_pack2: FactPack = serde_yaml::from_str(MACOS_CLI_FACTS_YAML).unwrap();
    let cli_facts2 = FactPackIndex::build(cli_pack2);
    let second = discover_shell_submodes(&mut reg, &facts, &cli_facts2);
    let count_after_second = reg.poly_op_names().len();

    assert_eq!(first.len(), 176);
    assert_eq!(second.len(), 0, "second run should discover 0 new ops");
    assert_eq!(count_after_first, count_after_second, "op count should not change");
}

#[test]
fn test_curl_submodes() {
    let reg = make_full_registry();
    assert!(reg.get_poly("shell_curl_silent").is_some());
    assert!(reg.get_poly("shell_curl_follow_redirects").is_some());
    assert!(reg.get_poly("shell_curl_headers_only").is_some());
    assert!(reg.get_poly("shell_curl_download").is_some());
}

#[test]
fn test_find_submodes() {
    let reg = make_full_registry();
    assert!(reg.get_poly("shell_find_files_only").is_some());
    assert!(reg.get_poly("shell_find_dirs_only").is_some());
    assert!(reg.get_poly("shell_find_by_name").is_some());
    assert!(reg.get_poly("shell_find_by_size").is_some());
    assert!(reg.get_poly("shell_find_by_mtime").is_some());
    assert!(reg.get_poly("shell_find_max_depth").is_some());
}

#[test]
fn test_sort_submodes() {
    let reg = make_full_registry();
    assert!(reg.get_poly("shell_sort_numeric").is_some());
    assert!(reg.get_poly("shell_sort_reverse").is_some());
    assert!(reg.get_poly("shell_sort_unique").is_some());
    assert!(reg.get_poly("shell_sort_numeric_reverse").is_some());
}

// ===========================================================================
// 10. Peekaboo Desktop Automation
// ===========================================================================

#[test]
fn test_peekaboo_base_ops_discovered() {
    let reg = cadmus::fs_types::build_full_registry();
    // Base peekaboo ops should be discovered via type-symmetric inference
    for name in &[
        "shell_peekaboo_list", "shell_peekaboo_see", "shell_peekaboo_click",
        "shell_peekaboo_type", "shell_peekaboo_hotkey", "shell_peekaboo_press",
        "shell_peekaboo_scroll", "shell_peekaboo_drag", "shell_peekaboo_app",
        "shell_peekaboo_window", "shell_peekaboo_clipboard", "shell_peekaboo_open",
    ] {
        assert!(reg.get_poly(name).is_some(), "missing peekaboo base op: {}", name);
    }
}

#[test]
fn test_peekaboo_submodes_discovered() {
    let reg = cadmus::fs_types::build_full_registry();
    // Peekaboo submodes should be discovered from macos_cli fact pack
    for name in &[
        "shell_peekaboo_list_apps", "shell_peekaboo_list_windows",
        "shell_peekaboo_list_json", "shell_peekaboo_see_json",
        "shell_peekaboo_see_annotate", "shell_peekaboo_click_double",
        "shell_peekaboo_click_right", "shell_peekaboo_type_return",
        "shell_peekaboo_type_clear", "shell_peekaboo_app_launch",
        "shell_peekaboo_app_quit", "shell_peekaboo_app_switch",
        "shell_peekaboo_window_focus", "shell_peekaboo_window_close",
        "shell_peekaboo_window_move", "shell_peekaboo_window_resize",
        "shell_peekaboo_clipboard_read", "shell_peekaboo_clipboard_write",
    ] {
        assert!(reg.get_poly(name).is_some(), "missing peekaboo submode: {}", name);
    }
}

#[test]
fn test_peekaboo_ops_have_shell_meta() {
    let reg = cadmus::fs_types::build_full_registry();
    let op = reg.get_poly("shell_peekaboo_click").unwrap();
    assert!(op.meta.is_some(), "peekaboo_click should have meta");
    let meta = op.meta.as_ref().unwrap();
    assert_eq!(meta.category.as_deref(), Some("shell"));
}

#[test]
fn test_peekaboo_type_signatures() {
    let reg = cadmus::fs_types::build_full_registry();

    // Query ops: String → List(String) (like shell_ls)
    let list = reg.get_poly("shell_peekaboo_list").unwrap();
    assert_eq!(format!("{}", list.signature.output), "List(String)");

    // Action ops: String → String (like shell_tmux)
    let click = reg.get_poly("shell_peekaboo_click").unwrap();
    assert_eq!(format!("{}", click.signature.output), "String");

    // Submodes inherit parent type
    let click_double = reg.get_poly("shell_peekaboo_click_double").unwrap();
    assert_eq!(format!("{}", click_double.signature.output), "String");

    let list_apps = reg.get_poly("shell_peekaboo_list_apps").unwrap();
    assert_eq!(format!("{}", list_apps.signature.output), "List(String)");
}

#[test]
fn test_peekaboo_submode_completeness() {
    // Every peekaboo entity in macos_cli.facts.yaml should have its submodes discovered
    let reg = cadmus::fs_types::build_full_registry();
    let peekaboo_ops: Vec<String> = reg.poly_op_names().into_iter()
        .filter(|n| n.contains("peekaboo"))
        .map(|s| s.to_string())
        .collect();

    // 12 base + 25 submodes = 37 total
    assert!(peekaboo_ops.len() >= 37,
        "expected at least 37 peekaboo ops, got {}: {:?}", peekaboo_ops.len(), peekaboo_ops);
}
