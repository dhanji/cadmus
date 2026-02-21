// Full pipeline trace: NL → Intent → Workflow YAML → Compiled Steps → Racket
// Run with: cargo test --test pipeline_trace -- --nocapture

use cadmus::nl::normalize;
use cadmus::nl::intent::{self, Intent};
use cadmus::nl::slots;
use cadmus::nl::dialogue;
use cadmus::workflow;
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
        Intent::CreateWorkflow { op, rest } => {
            println!("      intent: CreateWorkflow");
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
    //  STAGE 2: Intent + Slots → WorkflowDef (YAML structure)
    // ═══════════════════════════════════════════════════════════
    println!("\n══════════════════════════════════════════════════════════");
    println!("  STAGE 2: Intent + Slots → WorkflowDef");
    println!("══════════════════════════════════════════════════════════\n");

    if let Intent::CreateWorkflow { ref op, .. } = parsed_intent {
        match dialogue::build_workflow(op, &extracted, Some("Extract comic archive")) {
            Ok(wf) => {
                println!("  Generated WorkflowDef:");
                println!("    workflow: {:?}", wf.workflow);
                println!("    inputs:");
                for (k, v) in &wf.inputs {
                    println!("      {}: {:?}", k, v);
                }
                println!("    steps:");
                for (i, s) in wf.steps.iter().enumerate() {
                    println!("      {}: {} {:?}", i + 1, s.op, s.args);
                }

                // This is what would be serialized to YAML
                println!("\n  Equivalent YAML:");
                println!("    workflow: {:?}", wf.workflow);
                println!("    inputs:");
                for (k, v) in &wf.inputs {
                    println!("      {}: {:?}", k, v);
                }
                println!("    steps:");
                for s in &wf.steps {
                    match &s.args {
                        workflow::StepArgs::None => println!("      - {}", s.op),
                        workflow::StepArgs::Scalar(v) => println!("      - {}: {}", s.op, v),
                        workflow::StepArgs::Map(m) => {
                            println!("      - {}:", s.op);
                            for (k, v) in m {
                                println!("          {}: {}", k, v);
                            }
                        }
                    }
                }
            }
            Err(e) => println!("  Build error: {:?}", e),
        }
    }

    // ═══════════════════════════════════════════════════════════
    //  STAGE 3: WorkflowDef → Compiled Steps (Type Unification)
    // ═══════════════════════════════════════════════════════════
    println!("\n══════════════════════════════════════════════════════════");
    println!("  STAGE 3: WorkflowDef → Compiled Steps (Type Unification)");
    println!("══════════════════════════════════════════════════════════\n");

    // Use a direct CBZ workflow to show format resolution
    let yaml = r#"
workflow: "Extract a CBZ comic"
inputs:
  path: "issue_001.cbz"
steps:
  - extract_archive
"#;
    println!("  Input YAML:");
    for line in yaml.trim().lines() {
        println!("    {}", line);
    }
    println!();

    let def = workflow::parse_workflow(yaml).unwrap();
    let registry = fs_types::build_full_registry();
    let compiled = workflow::compile_workflow(&def, &registry).unwrap();

    println!("  Compilation result:");
    println!("    input_type:  {}", compiled.input_type);
    println!("    output_type: {}", compiled.output_type);
    println!();
    for cs in &compiled.steps {
        let is_map = workflow::step_needs_map(cs, &registry);
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

    let cases = vec![
        ("issue.cbz",       "Cbz → zip → unzip -o"),
        ("issue.cbr",       "Cbr → rar → unrar x"),
        ("archive.tar.gz",  "TarGz → tar_gz → tar -xzf"),
        ("archive.tar.bz2", "TarBz2 → tar_bz2 → tar -xjf"),
        ("archive.tar.xz",  "TarXz → tar_xz → tar -xJf"),
    ];

    for (ext, expected_chain) in &cases {
        let yaml = format!(r#"
workflow: "test"
inputs:
  path: "{}"
steps:
  - extract_archive
"#, ext);
        let def = workflow::parse_workflow(&yaml).unwrap();
        let compiled = workflow::compile_workflow(&def, &registry).unwrap();
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

    let yaml = std::fs::read_to_string("data/workflows/repack_comics.yaml").unwrap();
    println!("  Source YAML (data/workflows/repack_comics.yaml):");
    for line in yaml.trim().lines() {
        println!("    {}", line);
    }
    println!();

    let def = workflow::parse_workflow(&yaml).unwrap();
    let compiled = workflow::compile_workflow(&def, &registry).unwrap();

    println!("  Compiled pipeline:");
    println!("    input: {} (type: {})\n", def.inputs["path"], compiled.input_type);
    for cs in &compiled.steps {
        let is_map = workflow::step_needs_map(cs, &registry);
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

    println!("\n  Note: In the multi-step pipeline, input is '/comics' (a directory),");
    println!("  so the format type variable stays unbound → falls back to generic tar.");
    println!("  With a known-extension input like 'issue.cbz', it resolves to unzip.");
    println!();
}
