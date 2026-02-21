// End-to-end tests for the comic book repack NL pipeline.
// Verifies: NL prompt → normalize → intent → slots → build_plan → compile → Racket codegen
//
// Run: cargo test --test nl_comic_trace -- --nocapture

use cadmus::nl::normalize;
use cadmus::nl::intent;
use cadmus::nl::slots;
use cadmus::nl::dialogue;
use cadmus::nl::dialogue::DialogueState;
use cadmus::nl;
use cadmus::nl::NlResponse;
use cadmus::plan;
use cadmus::fs_types;
use cadmus::racket_executor;

// ===========================================================================
// Helper: full NL → plan YAML
// ===========================================================================

fn nl_to_yaml(input: &str) -> String {
    let mut state = DialogueState::new();
    match nl::process_input(input, &mut state) {
        NlResponse::PlanCreated { plan_yaml, .. } => plan_yaml,
        other => panic!("Expected PlanCreated for '{}', got: {:?}", input, other),
    }
}

fn nl_to_compiled(input: &str) -> (plan::PlanDef, plan::CompiledPlan) {
    let yaml = nl_to_yaml(input);
    let def = plan::parse_plan(&yaml)
        .unwrap_or_else(|e| panic!("Parse failed for '{}': {}\nYAML:\n{}", input, e, yaml));
    let registry = fs_types::build_full_registry();
    let compiled = plan::compile_plan(&def, &registry)
        .unwrap_or_else(|e| panic!("Compile failed for '{}': {}\nYAML:\n{}", input, e, yaml));
    (def, compiled)
}

fn build_racket_registry() -> cadmus::registry::OperationRegistry {
    let mut reg = cadmus::registry::load_ops_pack_str(
        include_str!("../data/packs/ops/racket_ops.yaml")
    ).expect("racket_ops.yaml");
    let facts = cadmus::racket_strategy::load_racket_facts_from_str(
        include_str!("../data/packs/facts/racket_facts.yaml")
    ).expect("racket_facts.yaml");
    cadmus::racket_strategy::promote_inferred_ops(&mut reg, &facts);

    let cli_yaml = include_str!("../data/packs/facts/macos_cli_facts.yaml");
    if let Ok(cli_pack) = serde_yaml::from_str::<cadmus::fact_pack::FactPack>(cli_yaml) {
        let cli_facts = cadmus::fact_pack::FactPackIndex::build(cli_pack);
        cadmus::racket_strategy::discover_shell_submodes(&mut reg, &facts, &cli_facts);
    }
    reg
}

// ===========================================================================
// HAPPY PATH: Full NL comic repack prompt → 7-step pipeline → Racket
// ===========================================================================

#[test]
fn test_nl_comic_repack_full_pipeline() {
    let input = "In dir /comics I have a series of comic book files stored in cbr/cbz format. \
                 Repack them into a flattened archive preserving the order of images in each \
                 original archive as well as the total order of issues.";

    let (def, compiled) = nl_to_compiled(input);

    // Should produce a 7-step pipeline
    assert_eq!(compiled.steps.len(), 7,
        "Expected 7 steps, got {}: {:?}",
        compiled.steps.len(),
        compiled.steps.iter().map(|s| &s.op).collect::<Vec<_>>());

    // Verify step ops in order
    assert_eq!(compiled.steps[0].op, "list_dir");
    assert_eq!(compiled.steps[1].op, "find_matching");
    assert_eq!(compiled.steps[2].op, "sort_by");
    // extract_archive should resolve to extract_zip (from *.cbz pattern narrowing)
    assert_eq!(compiled.steps[3].op, "extract_zip",
        "extract_archive should resolve to extract_zip via pattern narrowing");
    assert_eq!(compiled.steps[4].op, "flatten_seq");
    assert_eq!(compiled.steps[5].op, "enumerate_entries");
    // pack_archive should resolve to pack_zip
    assert_eq!(compiled.steps[6].op, "pack_zip",
        "pack_archive should resolve to pack_zip");

    // Verify input path
    // In the new plan format, inputs are typed parameters (not values).
    // The NL layer infers Dir type for "path".
    assert!(def.get_input("path").is_some(), "Should have 'path' input");

    // Verify extract_archive is in map mode
    let registry = fs_types::build_full_registry();
    assert!(plan::step_needs_map(&compiled.steps[3], &registry),
        "extract_zip should be in map mode (operating on Seq of archives)");

    // Verify extract step has isolate=true (filesystem isolation for map mode)
    assert!(compiled.steps[3].isolate,
        "extract_zip in map mode should have isolate=true to prevent filename collisions");
    assert!(!compiled.steps[4].isolate,
        "flatten_seq should NOT have isolate (no filesystem side effects)");

    // Verify flatten_seq is NOT in map mode
    assert!(!plan::step_needs_map(&compiled.steps[4], &registry),
        "flatten_seq should NOT be in map mode");
}

#[test]
fn test_nl_comic_repack_generates_valid_racket() {
    let input = "In dir /comics I have a series of comic book files stored in cbr/cbz format. \
                 Repack them into a flattened archive preserving the order of images in each \
                 original archive as well as the total order of issues.";

    let (def, compiled) = nl_to_compiled(input);
    let racket_reg = build_racket_registry();
    let script = racket_executor::generate_racket_script(&compiled, &def, &racket_reg)
        .expect("Racket codegen should succeed");

    // Should be a valid Racket script
    assert!(script.starts_with("#!/usr/bin/env racket"),
        "Should start with shebang: {}", &script[..80]);
    assert!(script.contains("#lang racket"),
        "Should have #lang racket");

    // Verify key operations in generated Racket
    assert!(script.contains("\"ls \""),
        "Step 1 should list dir: {}", script);
    assert!(script.contains("regexp-match?") && script.contains(".cbz"),
        "Step 2 should filter for .cbz: {}", script);
    assert!(script.contains("sort"),
        "Step 3 should sort: {}", script);
    assert!(script.contains("(map (lambda (_line)"),
        "Step 4 should be map-wrapped extract: {}", script);
    assert!(script.contains("unzip"),
        "Step 4 should use unzip (not tar): {}", script);
    assert!(script.contains("make-temporary-directory"),
        "Step 4 should create per-archive temp dirs to prevent filename collisions: {}", script);
    assert!(script.contains("find ") && script.contains("-type f"),
        "Step 4 should list extracted files via find: {}", script);
    assert!(script.contains("(apply append"),
        "Step 5 should flatten: {}", script);
    assert!(script.contains("for/list"),
        "Step 6 should enumerate: {}", script);
    assert!(script.contains("zip -r") && script.contains("combined.cbz"),
        "Step 7 should pack with zip: {}", script);
}

// ===========================================================================
// NEGATIVE: Simple extract still works (regression check)
// ===========================================================================

#[test]
fn test_nl_simple_extract_still_works() {
    // A simple "extract foo.cbz" should NOT trigger the compound repack planner
    let input = "extract archive.cbz";

    let (_def, compiled) = nl_to_compiled(input);

    // Should be a simple 1-step plan
    assert!(compiled.steps.len() <= 2,
        "Simple extract should have 1-2 steps, got {}: {:?}",
        compiled.steps.len(),
        compiled.steps.iter().map(|s| &s.op).collect::<Vec<_>>());

    // Should have extract_zip (resolved from .cbz extension)
    let has_extract = compiled.steps.iter().any(|s| s.op.starts_with("extract_"));
    assert!(has_extract,
        "Should have an extract step: {:?}",
        compiled.steps.iter().map(|s| &s.op).collect::<Vec<_>>());
}

// ===========================================================================
// BOUNDARY: Prompt without format hint
// ===========================================================================

#[test]
fn test_nl_repack_without_format_hint() {
    // When no specific format is mentioned, the planner should still build
    // a pipeline with a default format (*.cbz or generic)
    let input = "In dir /archives repack all archives into a flattened archive preserving order";

    let (def, compiled) = nl_to_compiled(input);

    // Should still produce a multi-step pipeline (the compound repack path)
    assert!(compiled.steps.len() >= 5,
        "Repack without format should still produce multi-step pipeline, got {}: {:?}",
        compiled.steps.len(),
        compiled.steps.iter().map(|s| &s.op).collect::<Vec<_>>());

    // Should have the core ops
    let ops: Vec<&str> = compiled.steps.iter().map(|s| s.op.as_str()).collect();
    assert!(ops.contains(&"list_dir"), "Should have list_dir: {:?}", ops);
    assert!(ops.contains(&"flatten_seq"), "Should have flatten_seq: {:?}", ops);

    // Input path should be /archives
    assert!(def.get_input("path").is_some(),
        "Should have 'path' input");
}

// ===========================================================================
// Diagnostic trace (for --nocapture debugging)
// ===========================================================================

#[test]
fn trace_nl_comic_repack() {
    let input = "In dir /comics I have a series of comic book files stored in cbr/cbz format. \
                 Repack them into a flattened archive preserving the order of images in each \
                 original archive as well as the total order of issues.";

    println!("\n=== INPUT ===");
    println!("{}", input);

    // Stage 1: Normalize
    println!("\n=== NORMALIZE ===");
    let normalized = normalize::normalize(input);
    println!("tokens: {:?}", normalized.tokens);
    println!("canonical: {:?}", normalized.canonical_tokens);

    // Stage 2: Intent
    println!("\n=== INTENT ===");
    let parsed = intent::parse_intent(&normalized);
    println!("{:?}", parsed);

    // Stage 3: Slots
    println!("\n=== SLOTS ===");
    let extracted = slots::extract_slots(&normalized.canonical_tokens);
    println!("primary_op:  {:?}", extracted.primary_op);
    println!("target_path: {:?}", extracted.target_path);
    println!("patterns:    {:?}", extracted.patterns);
    println!("keywords:    {:?}", extracted.keywords);
    println!("modifiers:   {:?}", extracted.modifiers);
    println!("all slots:   {:?}", extracted.slots);

    // Stage 4: Build plan
    println!("\n=== BUILD PLAN ===");
    match &parsed {
        intent::Intent::CreatePlan { op, .. } => {
            match dialogue::build_plan(op, &extracted, Some("Repack comics")) {
                Ok(wf) => {
                    println!("plan: {:?}", wf.name);
                    println!("inputs: {:?}", wf.inputs);
                    for (i, s) in wf.steps.iter().enumerate() {
                        println!("  step {}: {} {:?}", i+1, s.op, s.args);
                    }

                    // Stage 5: Compile
                    println!("\n=== COMPILE ===");
                    let registry = fs_types::build_full_registry();
                    match plan::compile_plan(&wf, &registry) {
                        Ok(compiled) => {
                            println!("input_type:  {}", compiled.input_type);
                            println!("output_type: {}", compiled.output_type);
                            for cs in &compiled.steps {
                                let is_map = plan::step_needs_map(cs, &registry);
                                println!("  step {}: {}{}{} :: {} -> {}",
                                    cs.index+1, cs.op,
                                    if is_map { " [MAP]" } else { "" },
                                    if cs.isolate { " [ISOLATE]" } else { "" },
                                    cs.input_type, cs.output_type);
                            }

                            // Stage 6: Racket codegen
                            println!("\n=== RACKET ===");
                            match racket_executor::generate_racket_script(&compiled, &wf, &registry) {
                                Ok(script) => println!("{}", script),
                                Err(e) => println!("CODEGEN ERROR: {:?}", e),
                            }
                        }
                        Err(e) => println!("COMPILE ERROR: {:?}", e),
                    }
                }
                Err(e) => println!("BUILD ERROR: {:?}", e),
            }
        }
        other => println!("NOT A CreatePlan: {:?}", other),
    }
}
