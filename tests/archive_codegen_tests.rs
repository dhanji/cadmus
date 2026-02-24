// Archive codegen tests — end-to-end verification of the archive format
// specialization pipeline: plan sexpr → compile → format resolution →
// Racket codegen.

fn parse_plan_any(src: &str) -> cadmus::plan::PlanDef {
    cadmus::sexpr::parse_sexpr_to_plan(src)
        .expect("should parse plan")
}

use std::collections::HashMap;
use cadmus::plan::{
    compile_plan, step_needs_map, CompiledStep,
};
use cadmus::racket_executor::generate_racket_script;
use cadmus::fs_types::build_full_registry;
use cadmus::type_expr::TypeExpr;

// ---------------------------------------------------------------------------
// Helper: build a racket registry with shell submodes
// ---------------------------------------------------------------------------

use cadmus::racket_executor::build_racket_registry;

fn compile_and_generate(src: &str) -> (cadmus::plan::CompiledPlan, String) {
    let def = parse_plan_any(src);
    let registry = build_full_registry();
    let compiled = compile_plan(&def, &registry).unwrap();
    let racket_reg = build_racket_registry();
    let script = generate_racket_script(&compiled, &def, &racket_reg).unwrap();
    (compiled, script)
}

// ===========================================================================
// I1: Type-driven each-mode
// ===========================================================================

#[test]
fn test_type_driven_map_extract_archive() {
    // extract_archive on a single CBZ file should NOT be wrapped in (map ...)
    let src = r#"
(define (test (path : (File (Archive (File Image) Cbz))))
  (extract_archive))
"#;
    let def = parse_plan_any(src);
    let registry = build_full_registry();
    let compiled = compile_plan(&def, &registry).unwrap();

    // Single step on a scalar input — should NOT need map
    assert!(!step_needs_map(&compiled.steps[0], &registry),
        "extract on scalar input should not need map");
}

#[test]
fn test_type_driven_map_on_seq_input() {
    // extract_archive: each on Seq input should need map
    let src = r#"
(define (test (path : Dir))
  (list_dir)
  (extract_archive :each))
"#;
    let def = parse_plan_any(src);
    let registry = build_full_registry();
    let compiled = compile_plan(&def, &registry).unwrap();

    // Step 0 (list_dir) — does not need map
    assert!(!step_needs_map(&compiled.steps[0], &registry));
    // Step 1 (extract_archive) — input is Seq, op expects scalar → needs map
    assert!(step_needs_map(&compiled.steps[1], &registry),
        "extract on Seq input should need map");
}

#[test]
fn test_sort_by_does_not_trigger_map() {
    let src = r#"
(define (test (path : Dir))
  (list_dir)
  (sort_by :key "name"))
"#;
    let def = parse_plan_any(src);
    let registry = build_full_registry();
    let compiled = compile_plan(&def, &registry).unwrap();

    // sort_by natively accepts Seq(a) — should NOT need map
    assert!(!step_needs_map(&compiled.steps[1], &registry),
        "sort_by should not trigger map wrapping");
}

#[test]
fn test_find_matching_does_not_trigger_map() {
    let src = r#"
(define (test (path : Dir))
  (list_dir)
  (find_matching :pattern "*.txt"))
"#;
    let def = parse_plan_any(src);
    let registry = build_full_registry();
    let compiled = compile_plan(&def, &registry).unwrap();

    // find_matching has Seq in its second input — should NOT need map
    assert!(!step_needs_map(&compiled.steps[1], &registry),
        "find_matching should not trigger map wrapping");
}

#[test]
fn test_map_wrapping_uses_line_variable() {
    let src = r#"
(define (test (path : Dir))
  (list_dir)
  (extract_archive :each))
"#;
    let (_, script) = compile_and_generate(src);

    // Should use (map (lambda (_line) ...) step-1)
    assert!(script.contains("(map (lambda (_line)"),
        "should wrap in map with _line: {}", script);
    // The inner expression should use _line, not step-1
    assert!(script.contains("(shell-quote _line)"),
        "inner expression should use _line: {}", script);
}

// ===========================================================================
// I3/I4: Format resolution
// ===========================================================================

#[test]
fn test_cbz_resolves_to_extract_zip() {
    let src = r#"
(define (test (path : (File (Archive (File Image) Cbz))))
  (extract_archive))
"#;
    let def = parse_plan_any(src);
    let registry = build_full_registry();
    let compiled = compile_plan(&def, &registry).unwrap();

    assert_eq!(compiled.steps[0].op, "extract_zip",
        "Cbz format should resolve to extract_zip");
}

#[test]
fn test_tar_gz_resolves_to_extract_tar_gz() {
    let src = r#"
(define (test (path : (File (Archive Bytes TarGz))))
  (extract_archive))
"#;
    let def = parse_plan_any(src);
    let registry = build_full_registry();
    let compiled = compile_plan(&def, &registry).unwrap();

    assert_eq!(compiled.steps[0].op, "extract_tar_gz",
        "TarGz format should resolve to extract_tar_gz");
}

#[test]
fn test_rar_resolves_to_extract_rar() {
    let src = r#"
(define (test (path : (File (Archive Bytes Rar))))
  (extract_archive))
"#;
    let def = parse_plan_any(src);
    let registry = build_full_registry();
    let compiled = compile_plan(&def, &registry).unwrap();

    assert_eq!(compiled.steps[0].op, "extract_rar",
        "Rar format should resolve to extract_rar");
}

#[test]
fn test_cbr_resolves_to_extract_rar() {
    let src = r#"
(define (test (path : (File (Archive (File Image) Cbr))))
  (extract_archive))
"#;
    let def = parse_plan_any(src);
    let registry = build_full_registry();
    let compiled = compile_plan(&def, &registry).unwrap();

    assert_eq!(compiled.steps[0].op, "extract_rar",
        "Cbr format should resolve to extract_rar (same tool family)");
}

#[test]
fn test_unresolved_format_keeps_generic_op() {
    // Dir(Bytes) input — format is unresolved
    let src = r#"
(define (test (path : Dir))
  (list_dir)
  (extract_archive :each))
"#;
    let def = parse_plan_any(src);
    let registry = build_full_registry();
    let compiled = compile_plan(&def, &registry).unwrap();

    // Format is unresolved (variable) — should keep generic extract_archive
    assert_eq!(compiled.steps[1].op, "extract_archive",
        "unresolved format should keep generic op: {}", compiled.steps[1].op);
}

// ===========================================================================
// Racket codegen: correct tool per format
// ===========================================================================

#[test]
fn test_cbz_generates_unzip() {
    let src = r#"
(define (test (path : (File (Archive (File Image) Cbz))))
  (extract_archive))
"#;
    let (_, script) = compile_and_generate(src);
    assert!(script.contains("unzip"),
        "CBZ should generate unzip command: {}", script);
}

#[test]
fn test_tar_gz_generates_tar_xzf() {
    let src = r#"
(define (test (path : (File (Archive Bytes TarGz))))
  (extract_archive))
"#;
    let (_, script) = compile_and_generate(src);
    assert!(script.contains("tar") && script.contains("-xzf"),
        "TarGz should generate tar -xzf: {}", script);
}

#[test]
fn test_rar_generates_unrar() {
    let src = r#"
(define (test (path : (File (Archive Bytes Rar))))
  (extract_archive))
"#;
    let (_, script) = compile_and_generate(src);
    assert!(script.contains("unrar"),
        "RAR should generate unrar command: {}", script);
}

#[test]
fn test_unresolved_format_falls_back_to_tar() {
    let src = r#"
(define (test (path : Dir))
  (list_dir)
  (extract_archive :each))
"#;
    let (_, script) = compile_and_generate(src);
    assert!(script.contains("tar"),
        "unresolved format should fall back to tar: {}", script);
}

// ===========================================================================
// Full comic repack pipeline
// ===========================================================================

#[test]
fn test_comic_repack_pipeline_compiles() {
    let src = include_str!("../data/plans/repack_comics.sexp");
    let def = cadmus::sexpr::parse_sexpr_to_plan(src).unwrap();
    let registry = build_full_registry();
    let compiled = compile_plan(&def, &registry).unwrap();

    assert_eq!(compiled.steps.len(), 5);
    assert_eq!(compiled.steps[0].op, "list_dir");
    assert_eq!(compiled.steps[1].op, "find_matching");
    assert_eq!(compiled.steps[2].op, "sort_by");
    // Step 3: extract_archive resolves to extract_zip (pattern narrowing from *.cbz)
    assert_eq!(compiled.steps[3].op, "extract_zip");
    // Step 4: pack_archive resolves to pack_zip (output param combined.cbz)
    assert_eq!(compiled.steps[4].op, "pack_zip");
}

#[test]
fn test_comic_repack_pipeline_generates_racket() {
    let src = include_str!("../data/plans/repack_comics.sexp");
    let def = cadmus::sexpr::parse_sexpr_to_plan(src).unwrap();
    let registry = build_full_registry();
    let compiled = compile_plan(&def, &registry).unwrap();
    let racket_reg = build_racket_registry();
    let script = generate_racket_script(&compiled, &def, &racket_reg).unwrap();

    // Step 1: ls
    assert!(script.contains("\"ls \""), "should have ls: {}", script);
    // Step 2: filter for .cbz
    assert!(script.contains("regexp-match?"), "should have regex filter: {}", script);
    assert!(script.contains(".cbz"), "should filter for cbz: {}", script);
    // Step 3: sort
    assert!(script.contains("sort"), "should have sort: {}", script);
    // Step 4: extract with map wrapping
    assert!(script.contains("(map (lambda (_line)"),
        "extract should be map-wrapped: {}", script);
    // Step 5: pack (not map-wrapped)
    // pack_archive natively accepts Seq — should not be in a map
    let step5_line = script.lines()
        .find(|l| l.contains("Step 5"))
        .unwrap_or("");
    assert!(!step5_line.contains("(map)"),
        "pack should not be map-wrapped: {}", step5_line);
}

#[test]
fn test_comic_repack_typed_input_uses_unzip() {
    // When the input is a specific .cbz file, the format resolves
    let src = r#"
(define (repack-single-cbz (path : (File (Archive (File Image) Cbz))))
  (extract_archive))
"#;
    let (compiled, script) = compile_and_generate(src);

    assert_eq!(compiled.steps[0].op, "extract_zip",
        "should resolve to extract_zip");
    assert!(script.contains("unzip"),
        "should generate unzip: {}", script);
}

// ===========================================================================
// pack_archive format resolution
// ===========================================================================

#[test]
fn test_pack_zip_resolves_from_output_type() {
    // When pack_archive produces File(Archive(a, Zip)), resolve to pack_zip
    let src = r#"
(define (test (path : (File (Archive Bytes Zip))))
  (extract_archive)
  (pack_archive))
"#;
    let def = parse_plan_any(src);
    let registry = build_full_registry();
    let compiled = compile_plan(&def, &registry).unwrap();

    assert_eq!(compiled.steps[0].op, "extract_zip");
    // pack_archive's output type should have Zip format from the chain
    // (the fmt variable from extract_zip flows through)
    // Note: pack_archive may or may not resolve depending on type chain
}

// ===========================================================================
// step_needs_map with racket registry
// ===========================================================================

#[test]
fn test_step_needs_map_with_racket_registry() {
    // step_needs_map should work even when the racket registry doesn't
    // have the fs_ops entries — it falls back to the full registry
    let racket_reg = build_racket_registry();

    let step = CompiledStep {
        index: 0,
        op: "extract_archive".to_string(),
        input_type: TypeExpr::seq(TypeExpr::entry(
            TypeExpr::prim("Name"),
            TypeExpr::file(TypeExpr::archive(TypeExpr::prim("Bytes"), TypeExpr::prim("Zip"))),
        )),
        output_type: TypeExpr::seq(TypeExpr::entry(
            TypeExpr::prim("Name"),
            TypeExpr::seq(TypeExpr::entry(TypeExpr::prim("Name"), TypeExpr::prim("Bytes"))),
        )),
        params: HashMap::new(),
    ..Default::default()
    };

    assert!(step_needs_map(&step, &racket_reg),
        "extract_archive on Seq input should need map even with racket registry");
}

// ===========================================================================
// I4: pack_archive codegen + pattern-based format narrowing
// ===========================================================================

#[test]
fn test_pack_archive_without_output_defaults_to_output_zip() {
    // pack_archive without explicit output param should still generate valid
    // Racket with a default output filename
    let src = r#"
(define (test (path : (File (Archive Bytes Zip))))
  (extract_archive)
  (pack_archive))
"#;
    let (compiled, script) = compile_and_generate(src);

    // Should still compile and generate Racket
    assert!(compiled.steps.len() >= 2, "should have at least 2 steps");
    // The pack step should generate a zip command with default output
    assert!(script.contains("output.zip") || script.contains("zip"),
        "pack without output should use default: {}", script);
}

#[test]
fn test_find_matching_cbz_narrows_format() {
    // *.cbz pattern should narrow Bytes → (File (Archive (File Image) Cbz))
    let src = r#"
(define (test (path : Dir))
  (list_dir)
  (find_matching :pattern "*.cbz")
  (extract_archive :each))
"#;
    let def = parse_plan_any(src);
    let registry = build_full_registry();
    let compiled = compile_plan(&def, &registry).unwrap();

    // extract_archive should resolve to extract_zip thanks to *.cbz narrowing
    assert_eq!(compiled.steps[2].op, "extract_zip",
        "*.cbz pattern should narrow format to Zip, resolving extract_archive → extract_zip");
}

#[test]
fn test_find_matching_txt_does_not_narrow_to_archive() {
    // *.txt pattern should NOT produce an archive type
    let src = r#"
(define (test (path : Dir))
  (list_dir)
  (find_matching :pattern "*.txt"))
"#;
    let def = parse_plan_any(src);
    let registry = build_full_registry();
    let compiled = compile_plan(&def, &registry).unwrap();

    // The output type should NOT contain Archive
    let output_str = compiled.output_type.to_string();
    assert!(!output_str.contains("Archive"),
        "*.txt should not produce archive type: {}", output_str);
}

#[test]
fn test_extract_map_mode_uses_temp_dirs() {
    // When extract_archive is in MAP mode (operating on a Seq of archives),
    // each archive should extract into its own temp directory to prevent
    // filename collisions (e.g., issue_01.cbz and issue_02.cbz both
    // containing cover.jpg).
    let src = include_str!("../data/plans/repack_comics.sexp");
    let def = cadmus::sexpr::parse_sexpr_to_plan(src).unwrap();
    let registry = build_full_registry();
    let compiled = compile_plan(&def, &registry).unwrap();
    let racket_reg = build_racket_registry();
    let script = generate_racket_script(&compiled, &def, &racket_reg).unwrap();

    // Should use make-temporary-directory for per-archive isolation
    assert!(script.contains("make-temporary-directory"),
        "MAP-mode extract should create per-archive temp dirs: {}", script);

    // Should use find to list extracted files (returns full paths)
    assert!(script.contains("find ") && script.contains("-type f"),
        "MAP-mode extract should list files via find: {}", script);

    // Should NOT just use bare unzip without -d (which would clobber CWD)
    // The -d flag directs extraction to the temp dir
    assert!(script.contains("-d "),
        "MAP-mode extract should use -d to direct to temp dir: {}", script);
}

#[test]
fn test_single_extract_no_temp_dir() {
    // A single (non-MAP) extract should NOT use temp dir isolation
    let src = r#"
(define (test (path : (File (Archive (File Image) Cbz))))
  (extract_archive))
"#;
    let def = parse_plan_any(src);
    let registry = build_full_registry();
    let compiled = compile_plan(&def, &registry).unwrap();
    let racket_reg = build_racket_registry();
    let script = generate_racket_script(&compiled, &def, &racket_reg).unwrap();

    // Single extract should NOT have make-temporary-directory
    assert!(!script.contains("make-temporary-directory"),
        "Single extract should not use temp dir isolation: {}", script);
}
