// Full pipeline trace: NL → Intent → Plan YAML → Compiled Steps → Racket
// Run with: cargo test --test pipeline_trace -- --nocapture

use cadmus::nl::normalize;
use cadmus::nl::intent::{self, Intent};
use cadmus::nl::slots;
use cadmus::nl::dialogue::DialogueState;
use cadmus::nl::{self, NlResponse};
use cadmus::plan;
use cadmus::fs_types;
use cadmus::racket_executor;

#[test]
fn trace_full_pipeline() {
    println!("\n╔══════════════════════════════════════════════════════════╗");
    println!("║  CADMUS: Full Pipeline Trace — Comic Book Archives      ║");
    println!("╚══════════════════════════════════════════════════════════╝");

    // ═══════════════════════════════════════════════════════════
    //  STAGE 1: Natural Language → Intent + Slots
    // ═══════════════════════════════════════════════════════════
    println!("\n══════════════════════════════════════════════════════════");
    println!("  STAGE 1: Natural Language → Intent + Slots");
    println!("══════════════════════════════════════════════════════════\n");

    let nl_input = "extract the archive at ~/comic.cbz";
    println!("  NL input: {:?}\n", nl_input);

    // 1a. Normalize: case fold, strip punctuation, expand contractions, synonym map
    let normalized = normalize::normalize(nl_input);
    println!("  1a. Normalize:");
    println!("      raw tokens:       {:?}", normalized.tokens);
    println!("      canonical tokens: {:?}", normalized.canonical_tokens);

    // 1b. Intent recognition: classify what the user wants
    let parsed_intent = intent::parse_intent(&normalized);
    println!("\n  1b. Intent recognition:");
    match &parsed_intent {
        Intent::CreatePlan { op, rest } => {
            println!("      intent: CreatePlan");
            println!("      op:     {:?}", op);
            println!("      rest:   {:?}", rest);
        }
        other => println!("      intent: {:?}", other),
    }

    // 1c. Slot extraction: pull out paths, patterns, params
    let extracted = slots::extract_slots(&normalized.canonical_tokens);
    println!("\n  1c. Slot extraction:");
    println!("      primary_op:  {:?}", extracted.primary_op);
    println!("      target_path: {:?}", extracted.target_path);
    println!("      patterns:    {:?}", extracted.patterns);

    // ═══════════════════════════════════════════════════════════
    //  STAGE 2: NL → PlanDef via Earley pipeline
    // ═══════════════════════════════════════════════════════════
    println!("\n══════════════════════════════════════════════════════════");
    println!("  STAGE 2: NL → PlanDef via Earley pipeline");
    println!("══════════════════════════════════════════════════════════\n");

    let mut state = DialogueState::new();
    let response = nl::process_input(nl_input, &mut state);
    match &response {
        NlResponse::PlanCreated { plan_yaml, .. } => {
            println!("  Generated Plan YAML:");
            for line in plan_yaml.lines() {
                println!("    {}", line);
            }
            // Parse and show structure
            if let Ok(wf) = plan::parse_plan(plan_yaml) {
                println!("\n  Parsed PlanDef:");
                println!("    name: {:?}", wf.name);
                println!("    inputs:");
                for input in &wf.inputs {
                    println!("      {}: {:?}", input.name, input.type_hint);
                }
                println!("    steps:");
                for (i, s) in wf.steps.iter().enumerate() {
                    println!("      {}: {} {:?}", i + 1, s.op, s.args);
                }
            }
        }
        NlResponse::NeedsClarification { needs } => {
            println!("  NeedsClarification: {:?}", needs);
            println!("  (Earley parser could not produce a plan for this input)");
        }
        NlResponse::Error { message } => {
            println!("  Error: {}", message);
        }
        other => {
            println!("  Other: {:?}", other);
        }
    }

    // ═══════════════════════════════════════════════════════════
    //  STAGE 3: PlanDef → Compiled Steps (Type Unification)
    // ═══════════════════════════════════════════════════════════
    println!("\n══════════════════════════════════════════════════════════");
    println!("  STAGE 3: PlanDef → Compiled Steps (Type Unification)");
    println!("══════════════════════════════════════════════════════════\n");

    // Use a direct CBZ plan to show format resolution
    let yaml = r#"
extract-a-cbz-comic:
  inputs:
    - path: "File(Archive(File(Image), Cbz))"
  steps:
    - extract_archive
"#;
    println!("  Input YAML:");
    for line in yaml.trim().lines() {
        println!("    {}", line);
    }
    println!();

    let def = plan::parse_plan(yaml).unwrap();
    let registry = fs_types::build_full_registry();
    let compiled = plan::compile_plan(&def, &registry).unwrap();

    println!("  Compilation result:");
    println!("    input_type:  {}", compiled.input_type);
    println!("    output_type: {}", compiled.output_type);
    println!();
    for cs in &compiled.steps {
        let is_map = plan::step_needs_map(cs, &registry);
        println!("    Step {}: {}{}", cs.index + 1, cs.op,
            if is_map { " [MAP]" } else { "" });
        println!("      input:  {}", cs.input_type);
        println!("      output: {}", cs.output_type);
        if !cs.params.is_empty() {
            for (k, v) in &cs.params {
                println!("      param:  {} = {}", k, v);
            }
        }
    }

    // ═══════════════════════════════════════════════════════════
    //  STAGE 4: Format Resolution Chain
    // ═══════════════════════════════════════════════════════════
    println!("\n══════════════════════════════════════════════════════════");
    println!("  STAGE 4: Format Resolution Chain");
    println!("══════════════════════════════════════════════════════════\n");

    println!("  issue_001.cbz");
    println!("    ↓ filetypes.yaml: .cbz → Archive(Image, Cbz)");
    println!("    ↓ type unification: fmt binds to Cbz");
    println!("    ↓ format_families: Cbz → zip");
    println!("    ↓ resolve_archive_op: extract_archive → extract_zip");
    println!("    ↓ type_lowering subsumption: extract_zip → shell_unzip_extract");
    println!("    ↓ racket.ops.yaml: shell_unzip_extract → \"unzip -o\"");
    println!("    = Racket: (shell-lines (string-append \"unzip -o \" (shell-quote path)))");
    println!();
    println!("  Compiled op name: {} (was: extract_archive)", compiled.steps[0].op);

    // ═══════════════════════════════════════════════════════════
    //  STAGE 5: Compiled Steps → Racket Script
    // ═══════════════════════════════════════════════════════════
    println!("\n══════════════════════════════════════════════════════════");
    println!("  STAGE 5: Compiled Steps → Racket Script");
    println!("══════════════════════════════════════════════════════════\n");

    let script = racket_executor::generate_racket_script(&compiled, &def, &registry).unwrap();
    for line in script.lines() {
        println!("  {}", line);
    }

    // ═══════════════════════════════════════════════════════════
    //  ALL FORMAT PATHS
    // ═══════════════════════════════════════════════════════════
    println!("\n══════════════════════════════════════════════════════════");
    println!("  All Format Resolution Paths");
    println!("══════════════════════════════════════════════════════════\n");

    let cases: Vec<(&str, &str, &str)> = vec![
        ("issue.cbz",       "File(Archive(File(Image), Cbz))",  "Cbz → zip → unzip -o"),
        ("issue.cbr",       "File(Archive(File(Image), Cbr))",  "Cbr → rar → unrar x"),
        ("archive.tar.gz",  "File(Archive(Bytes, TarGz))",      "TarGz → tar_gz → tar -xzf"),
        ("archive.tar.bz2", "File(Archive(Bytes, TarBz2))",     "TarBz2 → tar_bz2 → tar -xjf"),
        ("archive.tar.xz",  "File(Archive(Bytes, TarXz))",      "TarXz → tar_xz → tar -xJf"),
    ];

    for (ext, type_hint, expected_chain) in &cases {
        let yaml = format!(r#"
test:
  inputs:
    - path: "{}"
  steps:
    - extract_archive
"#, type_hint);
        let def = plan::parse_plan(&yaml).unwrap();
        let compiled = plan::compile_plan(&def, &registry).unwrap();
        let script = racket_executor::generate_racket_script(&compiled, &def, &registry).unwrap();

        let tool_line = script.lines()
            .filter(|l| l.contains("string-append"))
            .last()
            .map(|l| l.trim())
            .unwrap_or("(not found)");

        println!("  {:<20} → op: {:<18} chain: {}", ext, compiled.steps[0].op, expected_chain);
        println!("  {:<20}   racket: {}\n", "", tool_line);
    }

    // ═══════════════════════════════════════════════════════════
    //  FULL 5-STEP COMIC REPACK PIPELINE
    // ═══════════════════════════════════════════════════════════
    println!("\n══════════════════════════════════════════════════════════");
    println!("  Full 5-Step Comic Repack Pipeline");
    println!("══════════════════════════════════════════════════════════\n");

    let yaml = std::fs::read_to_string("data/plans/repack_comics.yaml").unwrap();
    println!("  Source YAML (data/plans/repack_comics.yaml):");
    for line in yaml.trim().lines() {
        println!("    {}", line);
    }
    println!();

    let def = plan::parse_plan(&yaml).unwrap();
    let compiled = plan::compile_plan(&def, &registry).unwrap();

    println!("  Compiled pipeline:");
    println!("    input: path (type: {})\n", compiled.input_type);
    for cs in &compiled.steps {
        let is_map = plan::step_needs_map(cs, &registry);
        println!("    Step {}: {}{}", cs.index + 1, cs.op,
            if is_map { " [MAP over each element]" } else { "" });
        println!("      {} → {}", cs.input_type, cs.output_type);
        for (k, v) in &cs.params {
            println!("      param: {} = {}", k, v);
        }
    }

    println!("\n  Generated Racket script:\n");
    let script = racket_executor::generate_racket_script(&compiled, &def, &registry).unwrap();
    for line in script.lines() {
        println!("    {}", line);
    }

    println!("\n  Note: In the multi-step pipeline, input is a directory (bare 'path'),");
    println!("  so the format type variable stays unbound → falls back to generic tar.");
    println!("  With a typed input like File(Archive(File(Image), Cbz)), it resolves to unzip.");
    println!();
}
