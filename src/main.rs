use reasoning_engine::coding_strategy;
use reasoning_engine::fs_strategy::{FilesystemStrategy, run_fs_goal};
use reasoning_engine::generic_planner::ExprLiteral;
use reasoning_engine::pipeline;
use reasoning_engine::type_expr::TypeExpr;
use reasoning_engine::types::Goal;

fn main() {
    println!("╔══════════════════════════════════════════════════════════════╗");
    println!("║              REASONING ENGINE v0.3.0                        ║");
    println!("║    Strategy Pattern + Filesystem Type Grammar               ║");
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
        fact_pack_path: "data/putin_stalin.yaml".into(),
    };

    println!("Goal: {}", goal.description);
    println!("Entities: {}", goal.entities.join(", "));
    println!("Fact pack: {}", goal.fact_pack_path);
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
    println!("All three strategies executed through the unified pipeline.");
}
