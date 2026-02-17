use std::env;
use std::path::Path;
use std::process;

use cadmus::coding_strategy;
use cadmus::executor;
use cadmus::fs_strategy::{FilesystemStrategy, run_fs_goal};
use cadmus::generic_planner::ExprLiteral;
use cadmus::pipeline;
use cadmus::type_expr::TypeExpr;
use cadmus::types::Goal;
use cadmus::workflow;

fn main() {
    let args: Vec<String> = env::args().collect();

    // --chat mode: interactive NL UX
    if args.iter().any(|a| a == "--chat") {
        run_chat_mode();
        return;
    }

    // --workflow <path> mode: load and execute a workflow YAML file
    if let Some(pos) = args.iter().position(|a| a == "--workflow") {
        let path = args.get(pos + 1).unwrap_or_else(|| {
            eprintln!("Usage: cadmus --workflow <path.yaml> [--execute] [--racket]");
            eprintln!();
            eprintln!("Example:");
            eprintln!("  cargo run -- --workflow data/workflows/find_pdfs.yaml");
            eprintln!("  cargo run -- --workflow data/workflows/find_pdfs.yaml --execute");
            eprintln!("  cargo run -- --workflow data/workflows/add_numbers.yaml --racket");
            process::exit(1);
        });

        let execute = args.iter().any(|a| a == "--execute");
        let racket = args.iter().any(|a| a == "--racket");
        run_workflow_mode(Path::new(path), execute, racket);
        return;
    }

    if args.iter().any(|a| a == "--execute" || a == "--racket") {
        eprintln!("Error: --execute and --racket require --workflow <path.yaml>");
        process::exit(1);
    }

    // Default: run all three strategy demos
    run_demo_mode();
}

// ---------------------------------------------------------------------------
// Workflow mode
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Chat mode (NL UX)
// ---------------------------------------------------------------------------

fn run_chat_mode() {
    use std::io::{self, BufRead, Write};
    use cadmus::nl;
    use cadmus::nl::dialogue::DialogueState;

    println!("╔══════════════════════════════════════════════════════════════╗");
    println!("║              CADMUS v0.5.0                                  ║");
    println!("║    Natural Language UX                                      ║");
    println!("╚══════════════════════════════════════════════════════════════╝");
    println!();
    println!("Type a command in natural language. Examples:");
    println!("  zip up everything in ~/Downloads");
    println!("  find all PDFs in ~/Documents");
    println!("  what's walk mean");
    println!();
    println!("Type 'quit' or 'exit' to leave.");
    println!();

    let mut state = DialogueState::new();
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    loop {
        print!("> ");
        stdout.flush().unwrap();

        let mut line = String::new();
        match stdin.lock().read_line(&mut line) {
            Ok(0) => break, // EOF
            Ok(_) => {}
            Err(e) => {
                eprintln!("Error reading input: {}", e);
                break;
            }
        }

        let input = line.trim();
        if input.is_empty() {
            continue;
        }
        if input == "quit" || input == "exit" || input == "q" {
            println!("Bye!");
            break;
        }

        let response = nl::process_input(input, &mut state);

        match response {
            nl::NlResponse::PlanCreated { prompt, .. } => {
                println!();
                println!("{}", prompt);
                println!();
            }
            nl::NlResponse::PlanEdited { prompt, .. } => {
                println!();
                println!("{}", prompt);
                println!();
            }
            nl::NlResponse::Explanation { text } => {
                println!();
                println!("{}", text);
                println!();
            }
            nl::NlResponse::Approved { script } => {
                println!();
                println!("✅ Plan approved!");
                if let Some(ref s) = script {
                    println!();
                    println!("═══ Generated Racket Program ═══");
                    println!();
                    println!("{}", s);
                    println!();
                    print!("Run this program? (y/n) ");
                    stdout.flush().unwrap();
                    let mut confirm = String::new();
                    if stdin.lock().read_line(&mut confirm).is_ok() {
                        let answer = confirm.trim().to_lowercase();
                        if answer == "y" || answer == "yes" {
                            println!();
                            match std::process::Command::new("racket")
                                .arg("-e")
                                .arg(s)
                                .output()
                            {
                                Ok(output) => {
                                    let stdout_str = String::from_utf8_lossy(&output.stdout);
                                    let stderr_str = String::from_utf8_lossy(&output.stderr);
                                    if !stdout_str.is_empty() {
                                        println!("{}", stdout_str);
                                    }
                                    if !stderr_str.is_empty() {
                                        eprintln!("{}", stderr_str);
                                    }
                                    if output.status.success() {
                                        println!("✅ Program completed successfully.");
                                    } else {
                                        let code = output.status.code().unwrap_or(1);
                                        println!("⚠ Program exited with code {}", code);
                                    }
                                }
                                Err(e) => {
                                    eprintln!("❌ Failed to run racket: {}", e);
                                    eprintln!("   Is Racket installed? Try: brew install racket");
                                }
                            }
                        } else {
                            println!("Program not executed.");
                        }
                    }
                } else {
                    println!("(workflow could not be compiled to a script)");
                }
                println!();
            }
            nl::NlResponse::Rejected => {
                println!();
                println!("Plan discarded. Start fresh!");
                println!();
            }
            nl::NlResponse::NeedsClarification { needs } => {
                println!();
                for need in &needs {
                    println!("🤔 {}", need);
                }
                println!();
            }
            nl::NlResponse::ParamSet { description, .. } => {
                println!();
                println!("{}", description);
                println!();
            }
            nl::NlResponse::Error { message } => {
                println!();
                println!("❌ {}", message);
                println!();
            }
        }
    }
}


fn run_workflow_mode(path: &Path, execute: bool, racket: bool) {
    println!("╔══════════════════════════════════════════════════════════════╗");
    println!("║              CADMUS v0.6.0                                  ║");
    println!("║    Workflow DSL + Shell Executor                            ║");
    println!("╚══════════════════════════════════════════════════════════════╝");
    println!();

    println!("Loading workflow: {}", path.display());
    println!();

    let def = match workflow::load_workflow(path) {
        Ok(def) => def,
        Err(e) => {
            eprintln!("ERROR: {}", e);
            process::exit(1);
        }
    };

    println!("Workflow: {}", def.workflow);
    println!("Inputs:");
    for (k, v) in &def.inputs {
        println!("  {} = {}", k, v);
    }
    println!("Steps: {}", def.steps.len());
    for (i, step) in def.steps.iter().enumerate() {
        let args_str = match &step.args {
            workflow::StepArgs::None => String::new(),
            workflow::StepArgs::Scalar(s) => format!(": {}", s),
            workflow::StepArgs::Map(m) => {
                let pairs: Vec<String> = m.iter()
                    .map(|(k, v)| format!("{}={}", k, v))
                    .collect();
                format!(": {{{}}}", pairs.join(", "))
            }
        };
        println!("  {}. {}{}", i + 1, step.op, args_str);
    }
    println!();

    // Compile
    let registry = cadmus::fs_types::build_full_registry();
    let compiled = match workflow::compile_workflow(&def, &registry) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("COMPILE ERROR: {}", e);
            process::exit(1);
        }
    };

    println!("═══ Compiled Workflow ═══");
    println!("{}", compiled);

    // Execute
    let trace = match workflow::execute_workflow(&compiled, &registry) {
        Ok(t) => t,
        Err(e) => {
            eprintln!("EXECUTION ERROR: {}", e);
            process::exit(1);
        }
    };

    println!("{}", trace);

    // Generate script (Racket or Shell)
    if racket {
        println!();
        println!("═══ Generated Racket Script ═══");
        println!();

        let mut racket_reg = cadmus::registry::load_ops_pack_str(
            include_str!("../data/racket_ops.yaml")
        ).expect("failed to load racket_ops.yaml");
        let racket_facts = cadmus::racket_strategy::load_racket_facts_from_str(
            include_str!("../data/racket_facts.yaml")
        ).expect("failed to load racket_facts.yaml");
        cadmus::racket_strategy::promote_inferred_ops(&mut racket_reg, &racket_facts);
        let script = match cadmus::racket_executor::generate_racket_script(&compiled, &def, &racket_reg) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("RACKET SCRIPT GENERATION ERROR: {}", e);
                process::exit(1);
            }
        };

        println!("{}", script);
        println!("(Racket script generated — use `racket <file.rkt>` to run)");
        return;
    }

    // Default: generate shell script
    println!();
    println!("═══ Generated Shell Script ═══");
    println!();

    let script = match executor::generate_script(&compiled, &def) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("SCRIPT GENERATION ERROR: {}", e);
            process::exit(1);
        }
    };

    println!("{}", script);

    if execute {
        println!("═══ Executing Script ═══");
        println!();

        let result = match executor::run_script(&script) {
            Ok(r) => r,
            Err(e) => {
                eprintln!("EXECUTION ERROR: {}", e);
                process::exit(1);
            }
        };

        if !result.stdout.is_empty() {
            println!("{}", result.stdout);
        }
        if !result.stderr.is_empty() {
            eprintln!("{}", result.stderr);
        }

        if result.exit_code != 0 {
            eprintln!("Script exited with code {}", result.exit_code);
            process::exit(result.exit_code);
        } else {
            println!("✅ Script completed successfully.");
        }
    } else {
        println!("(dry-run: use --execute to run this script)");
    }
}

// ---------------------------------------------------------------------------
// Demo mode (original behavior)
// ---------------------------------------------------------------------------

fn run_demo_mode() {
    println!("╔══════════════════════════════════════════════════════════════╗");
    println!("║              CADMUS v0.4.0                                  ║");
    println!("║    Strategy Pattern + Workflow DSL                          ║");
    println!("╚══════════════════════════════════════════════════════════════╝");
    println!();

    // -----------------------------------------------------------------------
    // Strategy 1: Comparison (fact-pack based entity comparison)
    // -----------------------------------------------------------------------
    println!("━━━ STRATEGY 1: Comparison ━━━");
    println!();

    let goal = Goal {
        description: "Produce a structured comparison of Putin and Stalin as autocrats".into(),
        entities: vec!["putin".into(), "stalin".into()],
        fact_pack_paths: vec!["data/putin_stalin.yaml".into()],
    };

    println!("Goal: {}", goal.description);
    println!("Entities: {}", goal.entities.join(", "));
    println!("Fact packs: {}", goal.fact_pack_paths.join(", "));
    println!();

    match pipeline::run(&goal) {
        Ok(output) => {
            println!("═══ THEORY LAYER ═══");
            println!();
            if !output.inferences.is_empty() {
                println!("Inferences ({}):", output.inferences.len());
                for inf in &output.inferences {
                    println!("  → {}", inf);
                }
                println!();
            }
            if !output.conflicts.is_empty() {
                println!("Conflicts ({}):", output.conflicts.len());
                for c in &output.conflicts {
                    println!("  ⚡ {}", c);
                }
                println!();
            }

            println!("═══ STRUCTURED COMPARISON ({} axes) ═══", output.axes.len());
            println!();

            for axis in &output.axes {
                println!("┌─── {} ───", axis.axis);
                println!("│");

                for claim in &axis.claims {
                    let entity = claim.entity.as_deref().unwrap_or("?");
                    println!("│  📋 [{}] {}", entity.to_uppercase(), claim.content);
                }
                println!("│");

                for ev in &axis.evidence {
                    let entity = ev.entity.as_deref().unwrap_or("?");
                    println!("│  📎 Evidence ({}): ", entity);
                    for line in ev.content.lines() {
                        println!("│     {}", line);
                    }
                }
                println!("│");

                for sim in &axis.similarities {
                    println!("│  🔗 Similarity: {}", sim.content);
                }
                println!("│");

                for con in &axis.contrasts {
                    println!("│  ⚔ Contrast:");
                    for line in con.content.lines() {
                        println!("│     {}", line);
                    }
                    if con.inferred {
                        println!("│     [includes theory-layer inferences]");
                    }
                }
                println!("│");

                for unc in &axis.uncertainties {
                    println!("│  ❓ Uncertainty:");
                    for line in unc.content.lines() {
                        println!("│     {}", line);
                    }
                }
                println!("│");

                if let Some(ref sum) = axis.summary {
                    println!("│  📝 Summary:");
                    for line in sum.content.lines() {
                        println!("│     {}", line);
                    }
                }

                if !axis.gaps.is_empty() {
                    println!("│");
                    println!("│  ⚠ GAPS:");
                    for gap in &axis.gaps {
                        println!("│     - {}", gap);
                    }
                }

                println!("│");
                println!("└───────────────────────────────────────");
                println!();
            }

            let total_gaps: usize = output.axes.iter().map(|a| a.gaps.len()).sum();
            if total_gaps == 0 {
                println!("✅ Comparison complete — all obligation slots fulfilled.");
            } else {
                println!("⚠ {} unfulfilled obligation(s).", total_gaps);
            }
        }
        Err(e) => {
            eprintln!("ERROR (comparison): {}", e);
        }
    }

    println!();
    println!();

    // -----------------------------------------------------------------------
    // Strategy 2: Coding (code analysis + refactoring)
    // -----------------------------------------------------------------------
    println!("━━━ STRATEGY 2: Coding ━━━");
    println!();
    println!("Goal: Analyze code and plan extract-method refactoring");
    println!("Source: {} lines of Rust code", coding_strategy::EXAMPLE_LONG_FUNCTION.lines().count());
    println!();

    match coding_strategy::run_coding(
        coding_strategy::EXAMPLE_LONG_FUNCTION,
        "Extract method to reduce function length",
    ) {
        Ok(output) => {
            println!("═══ CODE ANALYSIS RESULTS ═══");
            println!();

            println!("Source ({} chars):", output.source.len());
            for (i, line) in output.source.lines().take(5).enumerate() {
                println!("  {:>3} │ {}", i + 1, line);
            }
            if output.source.lines().count() > 5 {
                println!("  ... │ ({} more lines)", output.source.lines().count() - 5);
            }
            println!();

            if !output.smells.is_empty() {
                println!("Code Smells:");
                for smell in &output.smells {
                    println!("  🔍 {}", smell);
                }
                println!();
            }

            if !output.refactorings.is_empty() {
                println!("Planned Refactorings:");
                for refactoring in &output.refactorings {
                    println!("  🔧 {}", refactoring);
                }
                println!();
            }

            if !output.type_info.is_empty() {
                println!("Type Information:");
                for info in &output.type_info {
                    println!("  📐 {}", info);
                }
                println!();
            }

            println!("Generated Tests:");
            for test in &output.tests {
                for line in test.lines() {
                    println!("  │ {}", line);
                }
                println!();
            }

            println!("✅ Coding analysis complete.");
        }
        Err(e) => {
            eprintln!("ERROR (coding): {}", e);
        }
    }

    println!();
    println!();

    // -----------------------------------------------------------------------
    // Strategy 3: Filesystem (dry-run planning)
    // -----------------------------------------------------------------------
    println!("━━━ STRATEGY 3: Filesystem (Dry-Run) ━━━");
    println!();
    println!("Goal: Extract images from a CBZ comic archive");
    println!();

    let fs_target = TypeExpr::seq(TypeExpr::entry(
        TypeExpr::prim("Name"),
        TypeExpr::file(TypeExpr::prim("Image")),
    ));
    let fs_available = vec![
        ExprLiteral::new(
            "comic.cbz",
            TypeExpr::file(TypeExpr::archive(
                TypeExpr::file(TypeExpr::prim("Image")),
                TypeExpr::prim("Cbz"),
            )),
            "my_comic.cbz",
        ),
    ];

    match run_fs_goal(fs_target, fs_available) {
        Ok(trace) => {
            println!("{}", trace);
        }
        Err(e) => {
            eprintln!("ERROR (filesystem): {}", e);
        }
    }

    println!();
    println!("Goal: List directory contents");
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
            println!("{}", trace);
        }
        Err(e) => {
            eprintln!("ERROR (filesystem): {}", e);
        }
    }

    println!();
    println!("═══ ENGINE COMPLETE ═══");
    println!("All strategies executed through the unified pipeline.");
}
