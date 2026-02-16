use std::collections::HashSet;
use std::path::PathBuf;

use reasoning_engine::fact_pack;
use reasoning_engine::registry::{load_ops_pack_str, load_ops_pack_str_into};
use reasoning_engine::type_expr::TypeExpr;

// ===========================================================================
// Power Tools Integration Tests
// ===========================================================================

const POWER_TOOLS_OPS: &str = include_str!("../data/power_tools_ops.yaml");
const FS_OPS: &str = include_str!("../data/fs_ops.yaml");

// ---------------------------------------------------------------------------
// 1. Ops Pack Tests
// ---------------------------------------------------------------------------

#[test]
fn test_power_tools_ops_load_count() {
    let reg = load_ops_pack_str(POWER_TOOLS_OPS).unwrap();
    let names = reg.poly_op_names();
    assert!(
        names.len() >= 55,
        "expected at least 55 ops, got {}",
        names.len()
    );
}

#[test]
fn test_power_tools_ops_all_categories_present() {
    let reg = load_ops_pack_str(POWER_TOOLS_OPS).unwrap();

    // Git
    assert!(reg.get_poly("git_init").is_some());
    assert!(reg.get_poly("git_commit").is_some());
    assert!(reg.get_poly("git_log").is_some());
    assert!(reg.get_poly("git_merge").is_some());
    assert!(reg.get_poly("git_bisect").is_some());
    assert!(reg.get_poly("git_status").is_some());

    // Terminal multiplexers
    assert!(reg.get_poly("tmux_new_session").is_some());
    assert!(reg.get_poly("tmux_split").is_some());
    assert!(reg.get_poly("screen_new_session").is_some());

    // Structured data
    assert!(reg.get_poly("jq_query").is_some());
    assert!(reg.get_poly("yq_query").is_some());
    assert!(reg.get_poly("csv_cut").is_some());

    // Advanced text
    assert!(reg.get_poly("awk_extract").is_some());
    assert!(reg.get_poly("sed_script").is_some());
    assert!(reg.get_poly("cut_fields").is_some());
    assert!(reg.get_poly("tr_replace").is_some());

    // Process & system
    assert!(reg.get_poly("ps_list").is_some());
    assert!(reg.get_poly("kill_process").is_some());
    assert!(reg.get_poly("df_usage").is_some());
    assert!(reg.get_poly("uname_info").is_some());

    // Networking
    assert!(reg.get_poly("ssh_exec").is_some());
    assert!(reg.get_poly("scp_transfer").is_some());
    assert!(reg.get_poly("dig_lookup").is_some());

    // Compression & crypto
    assert!(reg.get_poly("gzip_compress").is_some());
    assert!(reg.get_poly("base64_encode").is_some());
    assert!(reg.get_poly("openssl_hash").is_some());
}

#[test]
fn test_power_tools_ops_git_commit_signature() {
    let reg = load_ops_pack_str(POWER_TOOLS_OPS).unwrap();
    let op = reg.get_poly("git_commit").unwrap();

    // git_commit: Repo, StagingArea, Message → Commit
    assert_eq!(op.signature.inputs.len(), 3);
    assert_eq!(op.signature.output, TypeExpr::prim("Commit"));
}

#[test]
fn test_power_tools_ops_git_merge_3_inputs() {
    let reg = load_ops_pack_str(POWER_TOOLS_OPS).unwrap();
    let op = reg.get_poly("git_merge").unwrap();

    // git_merge: Repo, Branch, MergeStrategy → MergeResult
    assert_eq!(op.signature.inputs.len(), 3);
    assert_eq!(op.signature.output, TypeExpr::prim("MergeResult"));
}

#[test]
fn test_power_tools_ops_leaf_ops_no_inputs() {
    let reg = load_ops_pack_str(POWER_TOOLS_OPS).unwrap();

    // ps_list, df_usage, uname_info, uptime_info have no inputs
    for name in &["ps_list", "df_usage", "uname_info", "uptime_info"] {
        let op = reg.get_poly(name).expect(name);
        assert_eq!(
            op.signature.inputs.len(),
            0,
            "{} should have no inputs",
            name
        );
    }
}

#[test]
fn test_power_tools_ops_polymorphic_ops() {
    let reg = load_ops_pack_str(POWER_TOOLS_OPS).unwrap();

    // tee_split: a, Path → a (polymorphic)
    let tee = reg.get_poly("tee_split").unwrap();
    assert!(
        !tee.signature.type_params.is_empty(),
        "tee_split should be polymorphic"
    );

    // gzip_compress: File(a) → File(Gzip(a))
    let gz = reg.get_poly("gzip_compress").unwrap();
    assert!(
        !gz.signature.type_params.is_empty(),
        "gzip_compress should be polymorphic"
    );

    // scp_transfer: File(a), Host, Path → Unit
    let scp = reg.get_poly("scp_transfer").unwrap();
    assert!(
        !scp.signature.type_params.is_empty(),
        "scp_transfer should be polymorphic"
    );
}

#[test]
fn test_power_tools_ops_nonexistent_returns_none() {
    let reg = load_ops_pack_str(POWER_TOOLS_OPS).unwrap();
    assert!(reg.get_poly("nonexistent_op").is_none());
    assert!(reg.get_poly("docker_run").is_none());
    assert!(reg.get_poly("cargo_build").is_none());
}

#[test]
fn test_power_tools_ops_no_collisions_with_fs_ops() {
    let pt_reg = load_ops_pack_str(POWER_TOOLS_OPS).unwrap();
    let fs_reg = load_ops_pack_str(FS_OPS).unwrap();

    let pt_names: HashSet<&str> = pt_reg.poly_op_names().into_iter().collect();
    let fs_names: HashSet<&str> = fs_reg.poly_op_names().into_iter().collect();

    let collisions: Vec<&&str> = pt_names.intersection(&fs_names).collect();
    assert!(
        collisions.is_empty(),
        "op name collisions between power_tools and fs_ops: {:?}",
        collisions
    );
}

#[test]
fn test_power_tools_ops_merged_registry() {
    let mut reg = load_ops_pack_str(FS_OPS).unwrap();
    load_ops_pack_str_into(POWER_TOOLS_OPS, &mut reg).unwrap();

    // Should have ops from both packs
    assert!(reg.get_poly("list_dir").is_some(), "missing fs op list_dir");
    assert!(reg.get_poly("git_log").is_some(), "missing power_tools op git_log");
    assert!(reg.get_poly("jq_query").is_some(), "missing power_tools op jq_query");
}

#[test]
fn test_power_tools_ops_unification_lookup() {
    let reg = load_ops_pack_str(POWER_TOOLS_OPS).unwrap();

    // Looking for ops that produce Seq(Commit)
    let target = TypeExpr::seq(TypeExpr::prim("Commit"));
    let matches = reg.ops_for_output_expr(&target);
    assert!(
        !matches.is_empty(),
        "should find ops producing Seq(Commit)"
    );
    let names: Vec<&str> = matches.iter().map(|m| m.op.name.as_str()).collect();
    assert!(names.contains(&"git_log"), "git_log should produce Seq(Commit)");
}

#[test]
fn test_power_tools_ops_unification_no_match() {
    let reg = load_ops_pack_str(POWER_TOOLS_OPS).unwrap();

    // Looking for ops that produce a type not in the pack.
    // Note: polymorphic ops like tee_split (a → a) match any type,
    // so we verify that *monomorphic* ops don't spuriously match.
    let target = TypeExpr::prim("SpreadsheetCell");
    let matches = reg.ops_for_output_expr(&target);
    // Only polymorphic pass-through ops (tee_split) should match
    let mono_matches: Vec<_> = matches.iter()
        .filter(|m| m.op.signature.type_params.is_empty())
        .collect();
    assert!(
        mono_matches.is_empty(),
        "no monomorphic op should produce SpreadsheetCell, got: {:?}",
        mono_matches.iter().map(|m| &m.op.name).collect::<Vec<_>>()
    );
}

// ---------------------------------------------------------------------------
// 2. Fact Pack Tests
// ---------------------------------------------------------------------------

#[test]
fn test_power_tools_fact_pack_loads() {
    let path = PathBuf::from("data/power_tools.yaml");
    let idx = fact_pack::load_fact_pack(&path).unwrap();

    assert_eq!(idx.pack.entities.len(), 10);
    assert_eq!(idx.pack.axes.len(), 5);
    assert!(idx.pack.claims.len() >= 80);
    assert!(idx.pack.evidence.len() >= 20);
    assert!(idx.pack.properties.len() >= 25);
    assert_eq!(idx.pack.relations.len(), 8);
    assert_eq!(idx.pack.uncertainties.len(), 5);
}

#[test]
fn test_power_tools_fact_pack_entity_coverage() {
    let path = PathBuf::from("data/power_tools.yaml");
    let idx = fact_pack::load_fact_pack(&path).unwrap();

    let entity_ids: Vec<&str> = idx.pack.entities.iter().map(|e| e.id.as_str()).collect();
    for expected in &["git", "tmux", "screen", "ripgrep", "grep", "ag", "jq", "yq", "awk", "sed"] {
        assert!(
            entity_ids.contains(expected),
            "missing entity: {}",
            expected
        );
    }
}

#[test]
fn test_power_tools_fact_pack_no_cross_entity_contamination() {
    let path = PathBuf::from("data/power_tools.yaml");
    let idx = fact_pack::load_fact_pack(&path).unwrap();

    // Every claim should reference an entity that exists
    let entity_ids: HashSet<&str> = idx.pack.entities.iter().map(|e| e.id.as_str()).collect();
    for claim in &idx.pack.claims {
        assert!(
            entity_ids.contains(claim.entity.as_str()),
            "claim {} references unknown entity '{}'",
            claim.id,
            claim.entity
        );
    }

    // Every evidence item should reference a claim that exists
    let claim_ids: HashSet<&str> = idx.pack.claims.iter().map(|c| c.id.as_str()).collect();
    for ev in &idx.pack.evidence {
        assert!(
            claim_ids.contains(ev.supports.as_str()),
            "evidence {} supports unknown claim '{}'",
            ev.id,
            ev.supports
        );
    }
}

#[test]
fn test_power_tools_fact_pack_three_way_search_comparison() {
    let path = PathBuf::from("data/power_tools.yaml");
    let idx = fact_pack::load_fact_pack(&path).unwrap();

    // All three search tools should have performance claims
    for entity in &["ripgrep", "grep", "ag"] {
        let key = ("performance".to_string(), entity.to_string());
        let claims = idx.claims_by_axis_entity.get(&key);
        assert!(
            claims.is_some() && !claims.unwrap().is_empty(),
            "{} should have performance claims",
            entity
        );
    }

    // All three should have search_speed properties with ordinals
    for entity in &["ripgrep", "grep", "ag"] {
        let key = (
            "performance".to_string(),
            "search_speed".to_string(),
            entity.to_string(),
        );
        let prop_idx = idx.properties_by_axis_key_entity.get(&key);
        assert!(
            prop_idx.is_some(),
            "{} should have search_speed property",
            entity
        );
        let prop = &idx.pack.properties[*prop_idx.unwrap()];
        assert!(
            prop.ordinal.is_some(),
            "{} search_speed should have ordinal",
            entity
        );
    }

    // ripgrep should have highest ordinal
    let rg_ord = idx.pack.properties[*idx
        .properties_by_axis_key_entity
        .get(&("performance".into(), "search_speed".into(), "ripgrep".into()))
        .unwrap()]
    .ordinal
    .unwrap();
    let grep_ord = idx.pack.properties[*idx
        .properties_by_axis_key_entity
        .get(&("performance".into(), "search_speed".into(), "grep".into()))
        .unwrap()]
    .ordinal
    .unwrap();
    assert!(
        rg_ord > grep_ord,
        "ripgrep ({}) should have higher search_speed ordinal than grep ({})",
        rg_ord,
        grep_ord
    );
}

#[test]
fn test_power_tools_fact_pack_solo_entity_git() {
    let path = PathBuf::from("data/power_tools.yaml");
    let idx = fact_pack::load_fact_pack(&path).unwrap();

    // Git should have claims on all 5 axes
    for axis in &["performance", "composability", "learning_curve", "ecosystem", "platform_support"] {
        let key = (axis.to_string(), "git".to_string());
        let claims = idx.claims_by_axis_entity.get(&key);
        assert!(
            claims.is_some() && !claims.unwrap().is_empty(),
            "git should have claims on axis '{}'",
            axis
        );
    }
}

// ---------------------------------------------------------------------------
// 3. Fact Pack Composition Tests
// ---------------------------------------------------------------------------

#[test]
fn test_compose_power_tools_and_macos_fs() {
    let pt_path = PathBuf::from("data/power_tools.yaml");
    let ps_path = PathBuf::from("data/putin_stalin.yaml");

    let pt_idx = fact_pack::load_fact_pack(&pt_path).unwrap();
    let ps_idx = fact_pack::load_fact_pack(&ps_path).unwrap();

    let pt_entity_count = pt_idx.pack.entities.len();

    // Merge using load_fact_packs
    let merged_idx = fact_pack::load_fact_packs(&[&pt_path, &ps_path]).unwrap();

    // Merged should have entities from both packs (no overlap expected)
    assert!(
        merged_idx.pack.entities.len() == pt_entity_count + ps_idx.pack.entities.len(),
        "merged should have entities from both packs: {} + {} = {}, got {}",
        pt_entity_count, ps_idx.pack.entities.len(),
        pt_entity_count + ps_idx.pack.entities.len(),
        merged_idx.pack.entities.len()
    );

    // Claims from both packs should be present
    let pt_claims = pt_idx.pack.claims.len();
    let ps_claims = ps_idx.pack.claims.len();
    assert_eq!(
        merged_idx.pack.claims.len(),
        pt_claims + ps_claims,
        "merged claims should be sum of both packs"
    );
}

#[test]
fn test_compose_shared_axis_dedup() {
    let pt_path = PathBuf::from("data/power_tools.yaml");
    let ps_path = PathBuf::from("data/putin_stalin.yaml");

    let merged_idx = fact_pack::load_fact_packs(&[&pt_path, &ps_path]).unwrap();

    // Check no duplicate axis ids
    let axis_ids: Vec<&str> = merged_idx.pack.axes.iter().map(|a| a.id.as_str()).collect();
    let unique_ids: HashSet<&str> = axis_ids.iter().copied().collect();
    assert_eq!(
        axis_ids.len(),
        unique_ids.len(),
        "merged axes should have no duplicate ids, got: {:?}",
        axis_ids
    );
}

#[test]
fn test_compose_cross_pack_queries() {
    let pt_path = PathBuf::from("data/power_tools.yaml");
    let ps_path = PathBuf::from("data/putin_stalin.yaml");

    let merged_idx = fact_pack::load_fact_packs(&[&pt_path, &ps_path]).unwrap();

    // Should be able to query power_tools entities in merged index
    let git_claims = merged_idx
        .claims_by_axis_entity
        .get(&("performance".into(), "git".into()));
    assert!(
        git_claims.is_some(),
        "should find git performance claims in merged index"
    );

    // Should be able to query putin_stalin entities in merged index
    let putin_claims = merged_idx
        .claims_by_axis_entity
        .get(&("legitimacy".into(), "putin".into()));
    assert!(
        putin_claims.is_some(),
        "should find putin legitimacy claims in merged index"
    );
}

#[test]
fn test_compose_via_goal_paths() {
    use reasoning_engine::types::Goal;
    use reasoning_engine::strategy::ComparisonStrategy;

    let goal = Goal {
        description: "Compare across merged packs".into(),
        entities: vec!["git".into(), "putin".into()],
        fact_pack_paths: vec![
            "data/power_tools.yaml".into(),
            "data/putin_stalin.yaml".into(),
        ],
    };

    let strategy = ComparisonStrategy::new(goal).unwrap();
    let idx = strategy.index();

    // Verify both packs' data is accessible via the strategy
    assert!(
        idx.claims_by_axis_entity
            .get(&("performance".into(), "git".into()))
            .is_some(),
        "git claims should be in merged index"
    );
    assert!(
        idx.claims_by_axis_entity
            .get(&("legitimacy".into(), "putin".into()))
            .is_some(),
        "putin claims should be in merged index"
    );
}
