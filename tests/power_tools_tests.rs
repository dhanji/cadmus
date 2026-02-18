use std::collections::HashSet;

use cadmus::registry::{load_ops_pack_str, load_ops_pack_str_into};
use cadmus::type_expr::TypeExpr;

// ===========================================================================
// Power Tools Integration Tests
// ===========================================================================

const POWER_TOOLS_OPS: &str = include_str!("../data/packs/ops/power_tools_ops.yaml");
const FS_OPS: &str = include_str!("../data/packs/ops/fs_ops.yaml");

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
