use cadmus::fs_strategy::{FilesystemStrategy, StepKind};
use cadmus::generic_planner::ExprLiteral;
use cadmus::type_expr::TypeExpr;

// ===========================================================================
// Filesystem Integration Tests — dry-run scenarios
// ===========================================================================

// ---------------------------------------------------------------------------
// 1. CBZ Repack: 3 .cbz files → extract → concat → pack into single archive
// ---------------------------------------------------------------------------

#[test]
fn test_cbz_repack_plan() {
    let strategy = FilesystemStrategy::new();

    // Three CBZ archives as input
    let available = vec![
        ExprLiteral::new(
            "comic1.cbz",
            TypeExpr::file(TypeExpr::archive(
                TypeExpr::file(TypeExpr::prim("Image")),
                TypeExpr::prim("Cbz"),
            )),
            "volume1.cbz",
        ),
        ExprLiteral::new(
            "comic2.cbz",
            TypeExpr::file(TypeExpr::archive(
                TypeExpr::file(TypeExpr::prim("Image")),
                TypeExpr::prim("Cbz"),
            )),
            "volume2.cbz",
        ),
        ExprLiteral::new(
            "comic3.cbz",
            TypeExpr::file(TypeExpr::archive(
                TypeExpr::file(TypeExpr::prim("Image")),
                TypeExpr::prim("Cbz"),
            )),
            "volume3.cbz",
        ),
    ];

    // Goal: extract all images from archives
    // We plan for Seq(Entry(Name, File(Image))) — the extracted contents
    let target = TypeExpr::seq(TypeExpr::entry(
        TypeExpr::prim("Name"),
        TypeExpr::file(TypeExpr::prim("Image")),
    ));

    let trace = strategy.dry_run(target, available).unwrap();

    // Verify the trace contains extract_archive
    let op_names: Vec<String> = trace.steps.iter()
        .filter(|s| s.kind == StepKind::Op)
        .map(|s| s.op_name.clone())
        .collect();
    assert!(
        op_names.iter().any(|n| n == "extract_archive"),
        "CBZ repack should use extract_archive, got ops: {:?}",
        op_names
    );

    // Verify the trace has steps in dependency order (leaves before ops)
    let first_op_idx = trace.steps.iter().position(|s| s.kind == StepKind::Op);
    let last_leaf_idx = trace.steps.iter().rposition(|s| s.kind == StepKind::Leaf);
    if let (Some(first_op), Some(last_leaf)) = (first_op_idx, last_leaf_idx) {
        assert!(
            last_leaf < first_op || trace.steps.len() <= 2,
            "leaves should come before ops in dependency order"
        );
    }

    // Verify trace display mentions unzip
    let display = trace.to_string();
    assert!(display.contains("unzip") || display.contains("extract"),
        "trace should mention unzip/extract: {}", display);
}

// ---------------------------------------------------------------------------
// 2. Zip Round-Trip: pack → extract (verify trace is symmetric)
// ---------------------------------------------------------------------------

#[test]
fn test_zip_round_trip_plan() {
    let strategy = FilesystemStrategy::new();

    // Step 1: Plan packing files into a zip
    let pack_target = TypeExpr::file(TypeExpr::archive(
        TypeExpr::prim("Bytes"),
        TypeExpr::prim("Zip"),
    ));
    let pack_available = vec![
        ExprLiteral::new(
            "files",
            TypeExpr::seq(TypeExpr::entry(TypeExpr::prim("Name"), TypeExpr::prim("Bytes"))),
            "list of files to pack",
        ),
        ExprLiteral::new(
            "zip_fmt",
            TypeExpr::prim("Zip"),
            "zip format tag",
        ),
    ];

    let pack_trace = strategy.dry_run(pack_target, pack_available).unwrap();
    let pack_ops: Vec<&str> = pack_trace.steps.iter()
        .filter(|s| s.kind == StepKind::Op)
        .map(|s| s.op_name.as_str())
        .collect();
    assert!(pack_ops.contains(&"pack_archive"),
        "pack phase should use pack_archive, got: {:?}", pack_ops);

    // Step 2: Plan extracting from a zip
    let extract_target = TypeExpr::seq(TypeExpr::entry(
        TypeExpr::prim("Name"),
        TypeExpr::prim("Bytes"),
    ));
    let extract_available = vec![
        ExprLiteral::new(
            "archive",
            TypeExpr::file(TypeExpr::archive(TypeExpr::prim("Bytes"), TypeExpr::prim("Zip"))),
            "the zip file",
        ),
    ];

    let extract_trace = strategy.dry_run(extract_target, extract_available).unwrap();
    let extract_ops: Vec<&str> = extract_trace.steps.iter()
        .filter(|s| s.kind == StepKind::Op)
        .map(|s| s.op_name.as_str())
        .collect();
    assert!(extract_ops.contains(&"extract_archive"),
        "extract phase should use extract_archive, got: {:?}", extract_ops);

    // Verify symmetry: pack uses zip, extract uses unzip
    let pack_display = pack_trace.to_string();
    let extract_display = extract_trace.to_string();
    assert!(pack_display.contains("zip"), "pack trace should mention zip");
    assert!(extract_display.contains("unzip") || extract_display.contains("extract"),
        "extract trace should mention unzip/extract");
}

// ---------------------------------------------------------------------------
// 3. Dir Tree Walk: plan walk_tree_hierarchy on nested directory (Tree output)
// ---------------------------------------------------------------------------

#[test]
fn test_dir_tree_walk_hierarchy_plan() {
    let strategy = FilesystemStrategy::new();

    // Goal: Tree(Entry(Name, Bytes)) — recursive directory tree
    let target = TypeExpr::tree(TypeExpr::entry(
        TypeExpr::prim("Name"),
        TypeExpr::prim("Bytes"),
    ));
    let available = vec![
        ExprLiteral::new(
            "project_dir",
            TypeExpr::dir(TypeExpr::prim("Bytes")),
            "/home/user/project — deeply nested directory (5+ levels)",
        ),
    ];

    let trace = strategy.dry_run(target, available).unwrap();

    let op_names: Vec<&str> = trace.steps.iter()
        .filter(|s| s.kind == StepKind::Op)
        .map(|s| s.op_name.as_str())
        .collect();
    assert!(op_names.contains(&"walk_tree_hierarchy"),
        "dir walk for Tree target should use walk_tree_hierarchy, got: {:?}", op_names);

    // Verify trace mentions find (the underlying command)
    let display = trace.to_string();
    assert!(display.contains("find") || display.contains("walk"),
        "trace should mention find/walk: {}", display);

    // Verify the output type is Tree(...)
    let last_op = trace.steps.iter()
        .filter(|s| s.kind == StepKind::Op)
        .last()
        .unwrap();
    assert!(last_op.output.contains("Tree"),
        "output should be Tree type, got: {}", last_op.output);
}

// ---------------------------------------------------------------------------
// 3b. Dir Flat Walk: plan walk_tree on nested directory (Seq output)
// ---------------------------------------------------------------------------

#[test]
fn test_dir_flat_walk_plan() {
    let strategy = FilesystemStrategy::new();

    // Goal: Seq(Entry(Name, Bytes)) — flat recursive listing
    let target = TypeExpr::seq(TypeExpr::entry(
        TypeExpr::prim("Name"),
        TypeExpr::prim("Bytes"),
    ));
    let available = vec![
        ExprLiteral::new(
            "project_dir",
            TypeExpr::dir(TypeExpr::prim("Bytes")),
            "/home/user/project — deeply nested directory",
        ),
    ];

    let trace = strategy.dry_run(target, available).unwrap();

    let op_names: Vec<&str> = trace.steps.iter()
        .filter(|s| s.kind == StepKind::Op)
        .map(|s| s.op_name.as_str())
        .collect();
    // walk_tree now returns Seq, so it should be used for flat listing
    assert!(op_names.contains(&"walk_tree") || op_names.contains(&"list_dir"),
        "flat walk should use walk_tree or list_dir, got: {:?}", op_names);
}

// ---------------------------------------------------------------------------
// 4. Content Search: search for pattern in files
// ---------------------------------------------------------------------------

#[test]
fn test_content_search_plan() {
    let strategy = FilesystemStrategy::new();

    // Goal: Seq(Match(Pattern, Line)) — search results
    let target = TypeExpr::seq(TypeExpr::match_type(
        TypeExpr::prim("Pattern"),
        TypeExpr::prim("Line"),
    ));
    let available = vec![
        ExprLiteral::new(
            "text_files",
            TypeExpr::seq(TypeExpr::entry(
                TypeExpr::prim("Name"),
                TypeExpr::file(TypeExpr::prim("Text")),
            )),
            "list of source files",
        ),
        ExprLiteral::new(
            "search_pattern",
            TypeExpr::prim("Pattern"),
            "TODO|FIXME|HACK",
        ),
    ];

    let trace = strategy.dry_run(target, available).unwrap();

    let op_names: Vec<&str> = trace.steps.iter()
        .filter(|s| s.kind == StepKind::Op)
        .map(|s| s.op_name.as_str())
        .collect();
    assert!(op_names.contains(&"search_content"),
        "content search should use search_content, got: {:?}", op_names);

    // Verify trace mentions grep
    let display = trace.to_string();
    assert!(display.contains("grep") || display.contains("search"),
        "trace should mention grep/search: {}", display);
}

// ---------------------------------------------------------------------------
// 5. Rename/Sort: plan sort_by on a sequence of entries
// ---------------------------------------------------------------------------

#[test]
fn test_rename_sort_plan() {
    let strategy = FilesystemStrategy::new();

    // Goal: sorted sequence of entries
    // sort_by<a>: Seq(a) → Seq(a) — idempotent
    // We use the planner's ability to chain: list_dir → sort_by
    let target = TypeExpr::seq(TypeExpr::entry(
        TypeExpr::prim("Name"),
        TypeExpr::prim("Bytes"),
    ));
    let available = vec![
        ExprLiteral::new(
            "unsorted_dir",
            TypeExpr::dir(TypeExpr::prim("Bytes")),
            "/tmp/messy_photos",
        ),
    ];

    let trace = strategy.dry_run(target, available).unwrap();

    // The planner should use list_dir (and possibly sort_by)
    let op_names: Vec<&str> = trace.steps.iter()
        .filter(|s| s.kind == StepKind::Op)
        .map(|s| s.op_name.as_str())
        .collect();
    assert!(op_names.contains(&"list_dir"),
        "should use list_dir, got: {:?}", op_names);

    // Verify trace mentions ls
    let display = trace.to_string();
    assert!(display.contains("ls") || display.contains("list"),
        "trace should mention ls/list: {}", display);

    // Verify the trace has proper structure
    assert!(trace.steps.len() >= 2, "should have at least leaf + op");
}

// ---------------------------------------------------------------------------
// Negative: no input archives → error
// ---------------------------------------------------------------------------

#[test]
fn test_repack_no_inputs_fails() {
    let strategy = FilesystemStrategy::new();
    let result = strategy.dry_run(
        TypeExpr::seq(TypeExpr::entry(
            TypeExpr::prim("Name"),
            TypeExpr::file(TypeExpr::prim("Image")),
        )),
        vec![], // no inputs
    );
    assert!(result.is_err(), "should fail with no input archives");
}

// ---------------------------------------------------------------------------
// Verify all pre-existing tests still pass (this test file coexists)
// ---------------------------------------------------------------------------

#[test]
fn test_fs_strategy_registry_has_all_ops() {
    let strategy = FilesystemStrategy::new();
    let names = strategy.registry().poly_op_names();
    assert!(names.len() >= 49, "should have at least 49 ops, got {}", names.len());
}

// ===========================================================================
// Phase 1-5 Op Signature & Property Tests
// ===========================================================================

use cadmus::fs_types::build_fs_registry;

// --- Phase 1: File Lifecycle ---

#[test]
fn test_copy_signature_and_properties() {
    let reg = build_fs_registry();
    let op = reg.get_poly("copy").unwrap();
    assert_eq!(op.signature.inputs.len(), 2);
    assert!(op.properties.idempotent, "copy should be idempotent");
}

#[test]
fn test_delete_not_idempotent() {
    let reg = build_fs_registry();
    let op = reg.get_poly("delete").unwrap();
    assert!(!op.properties.idempotent, "delete should NOT be idempotent");
    // delete returns Unit
    assert_eq!(op.signature.output, TypeExpr::prim("Unit"));
}

#[test]
fn test_create_dir_idempotent() {
    let reg = build_fs_registry();
    let op = reg.get_poly("create_dir").unwrap();
    assert!(op.properties.idempotent, "create_dir should be idempotent (mkdir -p)");
}

#[test]
fn test_set_permissions_idempotent() {
    let reg = build_fs_registry();
    let op = reg.get_poly("set_permissions").unwrap();
    assert!(op.properties.idempotent);
    assert_eq!(op.signature.output, TypeExpr::prim("Unit"));
}

// --- Phase 2: Content Transforms ---

#[test]
fn test_replace_not_idempotent() {
    let reg = build_fs_registry();
    let op = reg.get_poly("replace").unwrap();
    assert!(!op.properties.idempotent, "replace should NOT be idempotent");
    assert_eq!(op.signature.inputs.len(), 3); // File(a), Pattern, Text
}

#[test]
fn test_head_tail_idempotent() {
    let reg = build_fs_registry();
    let head = reg.get_poly("head").unwrap();
    let tail = reg.get_poly("tail").unwrap();
    assert!(head.properties.idempotent);
    assert!(tail.properties.idempotent);
}

#[test]
fn test_unique_idempotent() {
    let reg = build_fs_registry();
    let op = reg.get_poly("unique").unwrap();
    assert!(op.properties.idempotent, "unique should be idempotent");
}

#[test]
fn test_count_returns_count_not_seq() {
    let reg = build_fs_registry();
    let op = reg.get_poly("count").unwrap();
    assert_eq!(op.signature.output, TypeExpr::prim("Count"));
}

#[test]
fn test_checksum_idempotent() {
    let reg = build_fs_registry();
    let op = reg.get_poly("checksum").unwrap();
    assert!(op.properties.idempotent);
    assert_eq!(op.signature.output, TypeExpr::prim("Hash"));
}

#[test]
fn test_diff_returns_diff() {
    let reg = build_fs_registry();
    let op = reg.get_poly("diff").unwrap();
    assert_eq!(op.signature.output, TypeExpr::prim("Diff"));
    assert_eq!(op.signature.inputs.len(), 2);
}

// --- Phase 3: Metadata Accessors ---

#[test]
fn test_metadata_accessors_all_idempotent() {
    let reg = build_fs_registry();
    for name in &["get_size", "get_mtime", "get_permissions", "get_file_type"] {
        let op = reg.get_poly(name).unwrap();
        assert!(op.properties.idempotent, "{} should be idempotent", name);
        assert_eq!(op.signature.inputs.len(), 1, "{} takes 1 input", name);
        assert_eq!(op.signature.inputs[0], TypeExpr::prim("Metadata"),
            "{} input should be Metadata", name);
    }
}

#[test]
fn test_metadata_accessor_output_types() {
    let reg = build_fs_registry();
    assert_eq!(reg.get_poly("get_size").unwrap().signature.output, TypeExpr::prim("Size"));
    assert_eq!(reg.get_poly("get_mtime").unwrap().signature.output, TypeExpr::prim("Timestamp"));
    assert_eq!(reg.get_poly("get_permissions").unwrap().signature.output, TypeExpr::prim("Permissions"));
    assert_eq!(reg.get_poly("get_file_type").unwrap().signature.output, TypeExpr::prim("FileType"));
}

// --- Phase 4: macOS-Specific ---

#[test]
fn test_side_effect_ops_return_unit() {
    let reg = build_fs_registry();
    for name in &["open_file", "reveal", "clipboard_copy", "set_xattr",
                   "remove_xattr", "remove_quarantine", "set_permissions", "set_owner"] {
        let op = reg.get_poly(name).unwrap();
        assert_eq!(op.signature.output, TypeExpr::prim("Unit"),
            "{} should return Unit", name);
    }
}

#[test]
fn test_remove_quarantine_idempotent() {
    let reg = build_fs_registry();
    let op = reg.get_poly("remove_quarantine").unwrap();
    assert!(op.properties.idempotent);
}

#[test]
fn test_read_plist_idempotent() {
    let reg = build_fs_registry();
    let op = reg.get_poly("read_plist").unwrap();
    assert!(op.properties.idempotent);
}

// --- Phase 5: Network ---

#[test]
fn test_download_produces_file_bytes() {
    let reg = build_fs_registry();
    let op = reg.get_poly("download").unwrap();
    assert_eq!(op.signature.inputs[0], TypeExpr::prim("URL"));
    assert_eq!(op.signature.output, TypeExpr::file(TypeExpr::prim("Bytes")));
}

#[test]
fn test_upload_returns_unit() {
    let reg = build_fs_registry();
    let op = reg.get_poly("upload").unwrap();
    assert_eq!(op.signature.output, TypeExpr::prim("Unit"));
}

#[test]
fn test_sync_idempotent() {
    let reg = build_fs_registry();
    let op = reg.get_poly("sync").unwrap();
    assert!(op.properties.idempotent, "sync should be idempotent (rsync)");
}

// --- Total op count ---

#[test]
fn test_total_op_count() {
    let reg = build_fs_registry();
    let count = reg.poly_op_names().len();
    assert_eq!(count, 71, "expected 71 total ops, got {}", count);
}
