use std::env;
use std::path::Path;
use std::process;

use cadmus::coding_strategy;

use cadmus::fs_strategy::FilesystemStrategy;
use cadmus::generic_planner::ExprLiteral;
use cadmus::pipeline;
use cadmus::type_expr::TypeExpr;
use cadmus::types::Goal;
use cadmus::ui;
use cadmus::plan;

const VERSION: &str = "v0.7.0";

fn main() {
    let args: Vec<String> = env::args().collect();

    // --chat mode: interactive NL UX
    if args.iter().any(|a| a == "--chat") {
        run_chat_mode();
        return;
    }

    // --plan <path> mode: load and execute a plan YAML file
    if let Some(pos) = args.iter().position(|a| a == "--plan") {
        let path = args.get(pos + 1).unwrap_or_else(|| {
            eprintln!("{}", ui::error("Missing plan path"));
            eprintln!();
            eprintln!("  {} cadmus --plan <path.yaml> [--run]", ui::dim("usage:"));
            eprintln!();
            eprintln!("  {} cadmus --plan data/plans/find_pdfs.yaml", ui::dim("$"));
            eprintln!("  {} cadmus --plan data/plans/find_pdfs.yaml --run", ui::dim("$"));
            process::exit(1);
        });

        let run = args.iter().any(|a| a == "--run");
        run_plan_mode(Path::new(path), run);
        return;
    }

    if args.iter().any(|a| a == "--run") {
        eprintln!("{}", ui::error("--run requires --plan <path.yaml>"));
        process::exit(1);
    }

    // Default: run all three strategy demos
    run_demo_mode();
}

// ---------------------------------------------------------------------------
// Chat mode (NL UX)
// ---------------------------------------------------------------------------

fn run_chat_mode() {
    use std::io::{self, BufRead, Write};
    use cadmus::nl;
    use cadmus::nl::dialogue::DialogueState;

    println!();
    println!("{}", ui::banner("CADMUS", VERSION, "Natural Language"));
    println!();
    println!("  {} {}", ui::dim("try:"), ui::dim_white("zip up everything in ~/Downloads"));
    println!("  {}  {}", ui::dim("   "), ui::dim_white("find all PDFs in ~/Documents"));
    println!("  {}  {}", ui::dim("   "), ui::dim_white("what does walk_tree mean?"));
    println!("  {}  {}", ui::dim("   "), ui::dim_white("quit"));
    println!();

    let mut state = DialogueState::new();
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    loop {
        print!("{}{}", ui::prompt(), ui::reset());
        stdout.flush().unwrap();

        let mut line = String::new();
        match stdin.lock().read_line(&mut line) {
            Ok(0) => break,
            Ok(_) => {}
            Err(e) => {
                eprintln!("{}", ui::error(&format!("Read error: {}", e)));
                break;
            }
        }

        let input = line.trim();
        if input.is_empty() {
            continue;
        }
        if input == "quit" || input == "exit" || input == "q" {
            println!("{}", ui::dim("bye."));
            break;
        }

        let response = nl::process_input(input, &mut state);

        match response {
            nl::NlResponse::PlanCreated { plan_yaml, summary: _, prompt: _ } => {
                println!();
                println!("  {}", ui::status_ok("Plan created"));
                println!();
                println!("{}", ui::yaml_block(&plan_yaml));
                println!();
                println!("  {}", ui::dim("approve, edit, or reject?"));
                println!();
            }
            nl::NlResponse::PlanEdited { plan_yaml, diff_description, .. } => {
                println!();
                println!("  {}", ui::status_info(&format!("Edited: {}", diff_description)));
                println!();
                println!("{}", ui::yaml_block(&plan_yaml));
                println!();
                println!("  {}", ui::dim("approve?"));
                println!();
            }
            nl::NlResponse::Explanation { text } => {
                println!();
                for line in text.lines() {
                    println!("  {}", ui::cyan(line));
                }
                println!();
            }
            nl::NlResponse::Approved { script } => {
                println!();
                println!("  {}", ui::status_ok("Approved"));
                if let Some(ref s) = script {
                    println!();
                    println!("  {}", ui::subsection("Generated Racket Program"));
                    println!();
                    println!("{}", ui::code_block(s));
                    println!();
                    print!("  {} ", ui::dim("run this? (y/n)"));
                    stdout.flush().unwrap();
                    let mut confirm = String::new();
                    if stdin.lock().read_line(&mut confirm).is_ok() {
                        let answer = confirm.trim().to_lowercase();
                        if answer == "y" || answer == "yes" {
                            println!();
                            println!("  {}", ui::status_active("Running..."));
                            match std::process::Command::new("racket")
                                .arg("-e")
                                .arg(s)
                                .output()
                            {
                                Ok(output) => {
                                    let stdout_str = String::from_utf8_lossy(&output.stdout);
                                    let stderr_str = String::from_utf8_lossy(&output.stderr);
                                    if !stdout_str.is_empty() {
                                        println!();
                                        println!("{}", ui::code_block(&stdout_str));
                                    }
                                    if !stderr_str.is_empty() {
                                        eprintln!("{}", ui::dim(&stderr_str));
                                    }
                                    println!();
                                    if output.status.success() {
                                        println!("  {}", ui::status_ok("Done"));
                                    } else {
                                        let code = output.status.code().unwrap_or(1);
                                        println!("  {}", ui::status_fail(&format!("Exit code {}", code)));
                                    }
                                }
                                Err(e) => {
                                    println!("  {}", ui::error(&format!("Failed to run racket: {}", e)));
                                    println!("  {}", ui::dim("Is Racket installed? Try: brew install racket"));
                                }
                            }
                        } else {
                            println!("  {}", ui::dim("skipped."));
                        }
                    }
                } else {
                    println!("  {}", ui::dim("(could not compile to script)"));
                }
                println!();
            }
            nl::NlResponse::Rejected => {
                println!();
                println!("  {}", ui::dim("Plan discarded. Start fresh."));
                println!();
            }
            nl::NlResponse::NeedsClarification { needs } => {
                println!();
                for need in &needs {
                    println!("  {} {}", ui::yellow(ui::icon::DIAMOND), need);
                }
                println!();
            }
            nl::NlResponse::ParamSet { description, .. } => {
                println!();
                println!("  {}", ui::status_info(&description));
                println!();
            }
            nl::NlResponse::Error { message } => {
                println!();
                println!("  {}", ui::error(&message));
                println!();
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Plan mode
// ---------------------------------------------------------------------------

fn run_plan_mode(path: &Path, run: bool) {
    println!();
    println!("{}", ui::banner("CADMUS", VERSION, &format!("Plan {} Racket", ui::icon::ARROW_RIGHT)));
    println!();

    // Load
    println!("  {}", ui::status_active("Loading"));
    let def = match plan::load_plan(path) {
        Ok(def) => def,
        Err(e) => {
            println!("  {}", ui::status_fail("Load failed"));
            eprintln!("  {}", ui::error(&format!("{}", e)));
            process::exit(1);
        }
    };

    println!("  {}", ui::kv("plan", &def.name));
    println!("  {}", ui::kv("source", &path.display().to_string()));
    for input in &def.inputs {
        if let Some(hint) = &input.type_hint {
            println!("  {}", ui::kv_dim(&input.name, hint));
        } else {
            println!("  {}", ui::kv_dim(&input.name, "(inferred)"));
        }
    }
    println!();

    // Show steps
    println!("  {}", ui::subsection(&format!("Steps ({})", def.steps.len())));
    for (i, s) in def.steps.iter().enumerate() {
        let args_str = match &s.args {
            plan::StepArgs::None => String::new(),
            plan::StepArgs::Scalar(v) => format!("{} {}", ui::icon::ARROW_RIGHT, v),
            plan::StepArgs::Map(m) => {
                let pairs: Vec<String> = m.iter()
                    .map(|(k, v)| format!("{}={}", k, v))
                    .collect();
                format!("{{{}}}", pairs.join(", "))
            }
        };
        println!("{}", ui::step(i + 1, &s.op, &args_str));
    }
    println!();

    // Compile
    println!("  {}", ui::status_active("Compiling"));
    let registry = cadmus::fs_types::build_full_registry();
    let compiled = match plan::compile_plan(&def, &registry) {
        Ok(c) => c,
        Err(e) => {
            println!("  {}", ui::status_fail("Compile failed"));
            eprintln!("  {}", ui::error(&format!("{}", e)));
            process::exit(1);
        }
    };
    println!("  {}", ui::status_ok("Compiled"));
    println!();

    // Show compiled type chain
    println!("  {}", ui::subsection("Type Chain"));
    println!("  {}", ui::kv_dim("input", &compiled.input_type.to_string()));
    for cs in &compiled.steps {
        let type_info = format!("{} {} {}", cs.input_type, ui::icon::ARROW_RIGHT, cs.output_type);
        let is_map = cadmus::plan::step_needs_map(cs, &registry);
        if is_map {
            println!("{}", ui::step_each(cs.index + 1, &cs.op, &type_info));
        } else {
            println!("{}", ui::step(cs.index + 1, &cs.op, &type_info));
        }
        for (k, v) in &cs.params {
            println!("         {} {}", ui::dim(&format!("{}:", k)), ui::dim(v));
        }
    }
    println!("  {}", ui::kv_dim("output", &compiled.output_type.to_string()));
    println!();

    // Dry-run trace
    match plan::execute_plan(&compiled, &registry) {
        Ok(trace) => {
            println!("  {}", ui::subsection(&format!("Dry-Run Trace ({} steps)", trace.steps.len())));
            for ts in &trace.steps {
                let kind_tag = match ts.kind {
                    cadmus::fs_strategy::StepKind::Op => ui::dim("[op]"),
                    cadmus::fs_strategy::StepKind::Leaf => ui::dim("[input]"),
                    cadmus::fs_strategy::StepKind::Map => ui::dim("[map]"),
                    cadmus::fs_strategy::StepKind::Fold => ui::dim("[fold]"),
                };
                println!("{}", ui::step(ts.step, &ts.op_name, &format!("{} {}", kind_tag, ui::dim(&ts.command_hint))));
            }
            println!();
        }
        Err(e) => {
            println!("  {}", ui::warning(&format!("Trace: {}", e)));
            println!();
        }
    }

    // Generate Racket script
    println!("  {}", ui::subsection("Racket Script"));
    println!();

    let mut racket_reg = cadmus::registry::load_ops_pack_str(
        include_str!("../data/packs/ops/racket.ops.yaml")
    ).expect("failed to load racket.ops.yaml");
    let racket_facts = cadmus::racket_strategy::load_racket_facts_from_str(
        include_str!("../data/packs/facts/racket.facts.yaml")
    ).expect("failed to load racket.facts.yaml");
    cadmus::racket_strategy::promote_inferred_ops(&mut racket_reg, &racket_facts);

    // Discover shell submodes (Phase 4) — needed for archive tool dispatch
    let cli_yaml = std::fs::read_to_string("data/packs/facts/macos_cli.facts.yaml")
        .unwrap_or_else(|_| include_str!("../data/packs/facts/macos_cli.facts.yaml").to_string());
    if let Ok(cli_pack) = serde_yaml::from_str::<cadmus::fact_pack::FactPack>(&cli_yaml) {
        let cli_facts = cadmus::fact_pack::FactPackIndex::build(cli_pack);
        cadmus::racket_strategy::discover_shell_submodes(
            &mut racket_reg, &racket_facts, &cli_facts,
        );
    }
    let script = match cadmus::racket_executor::generate_racket_script(&compiled, &def, &racket_reg) {
        Ok(s) => s,
        Err(e) => {
            println!("  {}", ui::status_fail("Codegen failed"));
            eprintln!("  {}", ui::error(&format!("{}", e)));
            process::exit(1);
        }
    };

    println!("{}", ui::code_block(&script));
    println!();

    if run {
        println!("  {}", ui::status_active("Running..."));
        println!();

        match std::process::Command::new("racket")
            .arg("-e")
            .arg(&script)
            .output()
        {
            Ok(output) => {
                let stdout_str = String::from_utf8_lossy(&output.stdout);
                let stderr_str = String::from_utf8_lossy(&output.stderr);
                if !stdout_str.is_empty() {
                    println!("{}", ui::code_block(&stdout_str));
                }
                if !stderr_str.is_empty() {
                    eprintln!("{}", ui::dim(&stderr_str));
                }
                println!();
                if output.status.success() {
                    println!("  {}", ui::status_ok("Done"));
                } else {
                    let code = output.status.code().unwrap_or(1);
                    println!("  {}", ui::status_fail(&format!("Exit code {}", code)));
                    process::exit(code);
                }
            }
            Err(e) => {
                println!("  {}", ui::error(&format!("Failed to run racket: {}", e)));
                println!("  {}", ui::dim("Is Racket installed? Try: brew install racket"));
                process::exit(1);
            }
        }
    } else {
        println!("  {}", ui::dim("use --run to execute, or `racket <file.rkt>`"));
    }
    println!();
}

// ---------------------------------------------------------------------------
// Demo mode
// ---------------------------------------------------------------------------

fn run_demo_mode() {
    println!();
    println!("{}", ui::banner("CADMUS", VERSION, "Strategy Demo"));
    println!();

    // -----------------------------------------------------------------------
    // Strategy 1: Comparison
    // -----------------------------------------------------------------------
    println!("{}", ui::section("Strategy 1: Comparison"));
    println!();

    let goal = Goal {
        description: "Produce a structured comparison of Putin and Stalin as autocrats".into(),
        entities: vec!["putin".into(), "stalin".into()],
        fact_pack_paths: vec!["data/packs/facts/putin_stalin.facts.yaml".into()],
    };

    println!("{}", ui::kv("goal", &goal.description));
    println!("{}", ui::kv("entities", &goal.entities.join(", ")));
    println!("{}", ui::kv_dim("fact pack", &goal.fact_pack_paths.join(", ")));
    println!();

    match pipeline::run(&goal) {
        Ok(output) => {
            // Theory layer
            if !output.inferences.is_empty() || !output.conflicts.is_empty() {
                println!("  {}", ui::subsection("Theory Layer"));
                for inf in &output.inferences {
                    println!("{}", ui::inference_line(inf));
                }
                for c in &output.conflicts {
                    println!("{}", ui::conflict_line(c));
                }
                println!();
            }

            // Axes
            println!("  {}", ui::subsection(&format!("Comparison ({} axes)", output.axes.len())));
            println!();

            for axis in &output.axes {
                println!("{}", ui::axis_header(&axis.axis));

                for c in &axis.claims {
                    let entity = c.entity.as_deref().unwrap_or("?");
                    println!("{}", ui::claim(entity, &c.content));
                }

                for ev in &axis.evidence {
                    let entity = ev.entity.as_deref().unwrap_or("?");
                    for line in ev.content.lines() {
                        println!("{}", ui::evidence(entity, line));
                    }
                }

                for sim in &axis.similarities {
                    println!("{}", ui::similarity(&sim.content));
                }

                for con in &axis.contrasts {
                    for line in con.content.lines() {
                        println!("{}", ui::contrast_line(line));
                    }
                    if con.inferred {
                        println!("{}", ui::contrast_line(&ui::dim("[inferred]")));
                    }
                }

                for unc in &axis.uncertainties {
                    for line in unc.content.lines() {
                        println!("{}", ui::uncertainty(line));
                    }
                }

                if let Some(ref sum) = axis.summary {
                    for line in sum.content.lines() {
                        println!("{}", ui::summary_line(line));
                    }
                }

                for gap in &axis.gaps {
                    println!("{}", ui::gap_line(gap));
                }

                println!("{}", ui::axis_footer());
                println!();
            }

            let total_gaps: usize = output.axes.iter().map(|a| a.gaps.len()).sum();
            if total_gaps == 0 {
                println!("  {}", ui::status_ok("Comparison complete — all obligations fulfilled"));
            } else {
                println!("  {}", ui::status_warn(&format!("{} unfulfilled obligation(s)", total_gaps)));
            }
        }
        Err(e) => {
            println!("  {}", ui::status_fail("Comparison failed"));
            eprintln!("  {}", ui::error(&format!("{}", e)));
        }
    }

    println!();

    // -----------------------------------------------------------------------
    // Strategy 2: Coding
    // -----------------------------------------------------------------------
    println!("{}", ui::section("Strategy 2: Coding"));
    println!();

    let line_count = coding_strategy::EXAMPLE_LONG_FUNCTION.lines().count();
    println!("{}", ui::kv("goal", "Analyze code + plan extract-method refactoring"));
    println!("{}", ui::kv_dim("source", &format!("{} lines of Rust", line_count)));
    println!();

    match coding_strategy::run_coding(
        coding_strategy::EXAMPLE_LONG_FUNCTION,
        "Extract method to reduce function length",
    ) {
        Ok(output) => {
            // Source preview
            println!("  {}", ui::subsection("Source Preview"));
            let preview: String = output.source.lines().take(5)
                .enumerate()
                .map(|(i, l)| format!("  {} {} {}", ui::dim(&format!("{:>4}", i + 1)), ui::dim("│"), l))
                .collect::<Vec<_>>()
                .join("\n");
            println!("{}", preview);
            let remaining = output.source.lines().count().saturating_sub(5);
            if remaining > 0 {
                println!("  {}  {} {}", ui::dim("    "), ui::dim("│"), ui::dim(&format!("... {} more lines", remaining)));
            }
            println!();

            if !output.smells.is_empty() {
                println!("  {}", ui::subsection("Code Smells"));
                for (i, smell) in output.smells.iter().enumerate() {
                    if i == output.smells.len() - 1 {
                        println!("{}", ui::tree_last(smell));
                    } else {
                        println!("{}", ui::tree_item(smell));
                    }
                }
                println!();
            }

            if !output.refactorings.is_empty() {
                println!("  {}", ui::subsection("Planned Refactorings"));
                for (i, r) in output.refactorings.iter().enumerate() {
                    if i == output.refactorings.len() - 1 {
                        println!("{}", ui::tree_last(r));
                    } else {
                        println!("{}", ui::tree_item(r));
                    }
                }
                println!();
            }

            if !output.type_info.is_empty() {
                println!("  {}", ui::subsection("Type Information"));
                for (i, info) in output.type_info.iter().enumerate() {
                    if i == output.type_info.len() - 1 {
                        println!("{}", ui::tree_last(info));
                    } else {
                        println!("{}", ui::tree_item(info));
                    }
                }
                println!();
            }

            if !output.tests.is_empty() {
                println!("  {}", ui::subsection("Generated Tests"));
                for test in &output.tests {
                    println!("{}", ui::code_block(test));
                    println!();
                }
            }

            println!("  {}", ui::status_ok("Coding analysis complete"));
        }
        Err(e) => {
            println!("  {}", ui::status_fail("Coding analysis failed"));
            eprintln!("  {}", ui::error(&format!("{}", e)));
        }
    }

    println!();

    // -----------------------------------------------------------------------
    // Strategy 3: Filesystem (dry-run)
    // -----------------------------------------------------------------------
    println!("{}", ui::section("Strategy 3: Filesystem"));
    println!();

    // Goal A: CBZ extraction
    println!("{}", ui::kv("goal", "Extract images from a CBZ comic archive"));
    println!();

    let fs_target_a = TypeExpr::seq(TypeExpr::entry(
        TypeExpr::prim("Name"),
        TypeExpr::file(TypeExpr::prim("Image")),
    ));
    let fs_available_a = vec![
        ExprLiteral::new(
            "comic.cbz",
            TypeExpr::file(TypeExpr::archive(
                TypeExpr::file(TypeExpr::prim("Image")),
                TypeExpr::prim("Cbz"),
            )),
            "my_comic.cbz",
        ),
    ];

    let strategy_a = FilesystemStrategy::new();
    match strategy_a.dry_run(fs_target_a, fs_available_a) {
        Ok(trace) => {
            print_trace(&trace);
        }
        Err(e) => {
            println!("  {}", ui::status_fail("Planning failed"));
            eprintln!("  {}", ui::error(&format!("{}", e)));
        }
    }

    // Goal B: List directory
    println!("{}", ui::kv("goal", "List directory contents"));
    println!();

    let strategy = FilesystemStrategy::new();
    match strategy.dry_run(
        TypeExpr::seq(TypeExpr::entry(
            TypeExpr::prim("Name"),
            TypeExpr::prim("Bytes"),
        )),
        vec![ExprLiteral::new(
            "/home/user/docs",
            TypeExpr::dir(TypeExpr::prim("Bytes")),
            "/home/user/docs",
        )],
    ) {
        Ok(trace) => {
            print_trace(&trace);
        }
        Err(e) => {
            println!("  {}", ui::status_fail("Planning failed"));
            eprintln!("  {}", ui::error(&format!("{}", e)));
        }
    }

    println!("{}", ui::rule());
    println!("  {}", ui::status_ok("All strategies complete"));
    println!();
}

/// Print a DryRunTrace with rich formatting.
fn print_trace(trace: &cadmus::fs_strategy::DryRunTrace) {
    println!("  {}", ui::kv_dim("target", &trace.goal.to_string()));
    for ts in &trace.steps {
        let kind_tag = match ts.kind {
            cadmus::fs_strategy::StepKind::Op => "op",
            cadmus::fs_strategy::StepKind::Leaf => "input",
            cadmus::fs_strategy::StepKind::Map => "map",
            cadmus::fs_strategy::StepKind::Fold => "fold",
        };
        let detail = format!(
            "{} {} {}",
            ui::dim(&format!("[{}]", kind_tag)),
            ui::dim(&ts.command_hint),
            if ts.inputs.is_empty() { String::new() } else { ui::dim(&format!("{} {}", ui::icon::ARROW_LEFT, ts.inputs.join(", "))) },
        );
        println!("{}", ui::step(ts.step, &ts.op_name, &detail));
    }
    println!("  {}", ui::kv_dim("output", &trace.steps.last().map(|s| s.output.as_str()).unwrap_or("?")));
    println!();
}
