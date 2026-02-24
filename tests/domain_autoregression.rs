//! Domain Autoregression Report
//!
//! Full-funnel validation for algorithm plan domains. For each plan in every
//! category, traces the pipeline:
//!
//!   NL match → compile → codegen → execute → correct output
//!
//! Produces a human-readable report showing where each plan succeeds or fails,
//! plus category-level and overall percentages.
//!
//! Usage:
//!   cargo test --test domain_autoregression -- --nocapture
//!
//! The report prints to stdout. An agent reads it to decide what to fix.

use std::path::Path;

use cadmus::calling_frame::{CallingFrame, DefaultFrame};
use cadmus::fs_types::build_full_registry;
use cadmus::nl::dialogue::DialogueState;
use cadmus::nl::{self, NlResponse};
use cadmus::plan::{self, PlanDef, RawStep};
use cadmus::racket_executor::{build_racket_registry, generate_racket_script};
use cadmus::registry::OperationRegistry;

// ─── Funnel stages ──────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
enum Stage {
    /// NL matched the correct plan
    NlMatch,
    /// NL produced a plan but wrong one
    NlWrong { got_ops: Vec<String> },
    /// NL returned NeedsClarification
    NlClarify,
    /// NL returned an error
    NlError { message: String },
    /// Plan compiled successfully
    Compiled,
    /// Plan failed to compile
    CompileError { message: String },
    /// Racket script generated successfully
    Codegen,
    /// Racket codegen failed
    CodegenError { message: String },
    /// Racket executed successfully (exit 0)
    Executed,
    /// Racket execution failed
    ExecError { message: String },
    /// Output matches expected
    Correct,
    /// Output doesn't match expected
    Wrong { got: String, expected: String },
    /// No expected comment in plan file
    NoExpected,
}

impl Stage {
    fn label(&self) -> &'static str {
        match self {
            Stage::NlMatch => "NL:OK",
            Stage::NlWrong { .. } => "NL:WRONG",
            Stage::NlClarify => "NL:CLARIFY",
            Stage::NlError { .. } => "NL:ERROR",
            Stage::Compiled => "COMPILE:OK",
            Stage::CompileError { .. } => "COMPILE:FAIL",
            Stage::Codegen => "CODEGEN:OK",
            Stage::CodegenError { .. } => "CODEGEN:FAIL",
            Stage::Executed => "EXEC:OK",
            Stage::ExecError { .. } => "EXEC:FAIL",
            Stage::Correct => "OUTPUT:OK",
            Stage::Wrong { .. } => "OUTPUT:WRONG",
            Stage::NoExpected => "NO_EXPECTED",
        }
    }

    fn is_ok(&self) -> bool {
        matches!(
            self,
            Stage::NlMatch
                | Stage::Compiled
                | Stage::Codegen
                | Stage::Executed
                | Stage::Correct
        )
    }

    fn detail(&self) -> String {
        match self {
            Stage::NlWrong { got_ops } => format!("got ops: [{}]", got_ops.join(", ")),
            Stage::NlError { message } => message.clone(),
            Stage::CompileError { message } => message.clone(),
            Stage::CodegenError { message } => message.clone(),
            Stage::ExecError { message } => message.clone(),
            Stage::Wrong { got, expected } => {
                format!("got '{}', expected '{}'", truncate(got, 40), truncate(expected, 40))
            }
            _ => String::new(),
        }
    }
}

// ─── Plan result ────────────────────────────────────────────────────────────

struct PlanResult {
    name: String,
    category: String,
    description: String,
    stages: Vec<Stage>,
}

impl PlanResult {
    fn furthest_ok_stage(&self) -> &'static str {
        let mut last = "NONE";
        for s in &self.stages {
            if s.is_ok() {
                last = s.label();
            }
        }
        last
    }

    fn all_ok(&self) -> bool {
        self.stages.iter().all(|s| s.is_ok())
    }

    fn first_failure(&self) -> Option<&Stage> {
        self.stages.iter().find(|s| !s.is_ok())
    }
}

// ─── Loading ────────────────────────────────────────────────────────────────

fn parse_plan_file(content: &str, _path: &Path) -> Option<PlanDef> {
    cadmus::sexpr::parse_sexpr_to_plan(content).ok()
}

fn extract_description(content: &str) -> Option<String> {
    content
        .lines()
        .find(|line| line.starts_with(";; ") || line.starts_with("# "))
        .and_then(|line| line.strip_prefix(";; ").or_else(|| line.strip_prefix("# ")))
        .map(|s| s.to_string())
        .filter(|s| !s.is_empty() && !s.starts_with("expected:"))
}

fn extract_expected(content: &str) -> Option<String> {
    content
        .lines()
        .find(|l| l.starts_with("# expected:") || l.starts_with(";; expected:"))
        .map(|l| {
            l.trim_start_matches("# expected:")
                .trim_start_matches(";; expected:")
                .trim()
                .to_string()
        })
}

fn extract_expected_ops(steps: &[RawStep]) -> Vec<String> {
    steps.iter().map(|s| s.op.clone()).collect()
}

fn collect_plans(dir: &Path) -> Vec<(String, String)> {
    let mut files: Vec<_> = match std::fs::read_dir(dir) {
        Ok(rd) => rd
            .filter_map(|e| e.ok())
            .filter(|e| {
                e.path()
                    .extension()
                    .map(|ext| ext == "yaml" || ext == "sexp")
                    .unwrap_or(false)
            })
            .collect(),
        Err(_) => return vec![],
    };
    files.sort_by_key(|e| e.file_name());

    // Deduplicate: prefer .sexp over .yaml
    let mut seen = std::collections::HashSet::new();
    let mut result = Vec::new();
    for f in files {
        let path = f.path();
        let stem = path.file_stem().unwrap().to_string_lossy().to_string();
        if seen.insert(stem) {
            let content = std::fs::read_to_string(&path).unwrap_or_default();
            result.push((path.to_string_lossy().to_string(), content));
        }
    }
    result
}

// ─── NL matching (reused from nl_autoregression) ───────────────────────────

fn normalize_op(op: &str) -> &str {
    match op {
        "for_fold" | "foldl" | "foldr" | "racket_foldl" | "racket_foldr" => "fold",
        "for_list" | "racket_map" => "map",
        "for_each" | "racket_for_each" => "for_each",
        "filter" | "racket_filter" => "filter",
        "sort_by" | "sort_list" => "sort",
        "unique" | "remove_duplicates" => "unique",
        "count" | "count_entries" => "count",
        _ => op,
    }
}

fn subsequence_match(expected: &[String], got: &[String]) -> (usize, usize) {
    let mut ei = 0;
    let mut matched = 0;
    for g in got {
        let gn = normalize_op(g);
        while ei < expected.len() {
            if normalize_op(&expected[ei]) == gn {
                matched += 1;
                ei += 1;
                break;
            }
            ei += 1;
        }
    }
    (matched, expected.len())
}

fn check_nl_match(
    description: &str,
    expected_name: &str,
    expected_ops: &[String],
) -> Stage {
    let mut state = DialogueState::new();
    let response = nl::process_input(description, &mut state);

    match &response {
        NlResponse::PlanCreated { plan_sexpr, .. } => {
            let parsed = cadmus::sexpr::parse_sexpr_to_plan(plan_sexpr);

            match parsed {
                Ok(plan) => {
                    let got_ops: Vec<String> =
                        plan.steps.iter().map(|s| s.op.clone()).collect();

                    // Strategy 1: name match
                    let pn = plan.name.to_lowercase().replace('-', "_");
                    let en = expected_name.to_lowercase().replace('-', "_");
                    let name_match = pn == en || pn.contains(&en) || en.contains(&pn);

                    // Strategy 2: any got op matches expected name
                    let op_name_match = got_ops
                        .iter()
                        .any(|op| normalize_op(op) == normalize_op(expected_name));

                    // Strategy 3: subsequence
                    let (matched, total) = subsequence_match(expected_ops, &got_ops);
                    let subseq_match = if total <= 1 {
                        matched == total
                    } else {
                        matched * 2 >= total
                    };

                    if name_match || op_name_match || subseq_match {
                        Stage::NlMatch
                    } else {
                        Stage::NlWrong { got_ops }
                    }
                }
                Err(e) => Stage::NlError {
                    message: format!("Parse: {}", e),
                },
            }
        }
        NlResponse::NeedsClarification { .. } => Stage::NlClarify,
        NlResponse::Error { message } => Stage::NlError {
            message: message.clone(),
        },
        other => Stage::NlError {
            message: format!("Unexpected: {:?}", other),
        },
    }
}

// ─── Pipeline runner ────────────────────────────────────────────────────────

fn run_plan_funnel(
    path_str: &str,
    content: &str,
    category: &str,
    registry: &OperationRegistry,
    racket_registry: &OperationRegistry,
) -> PlanResult {
    let path = Path::new(path_str);
    let name = path
        .file_stem()
        .unwrap()
        .to_string_lossy()
        .to_string();

    let description = extract_description(content).unwrap_or_default();
    let expected = extract_expected(content);

    let def = match parse_plan_file(content, path) {
        Some(d) => d,
        None => {
            return PlanResult {
                name,
                category: category.to_string(),
                description,
                stages: vec![Stage::CompileError {
                    message: "Failed to parse plan file".to_string(),
                }],
            };
        }
    };

    let expected_ops = extract_expected_ops(&def.steps);
    let mut stages = Vec::new();

    // Stage 1: NL match
    if !description.is_empty() {
        stages.push(check_nl_match(&description, &def.name, &expected_ops));
    }

    // Stage 2: Compile
    match plan::compile_plan(&def, registry) {
        Ok(compiled) => {
            stages.push(Stage::Compiled);

            // Stage 3: Codegen
            match generate_racket_script(&compiled, &def, racket_registry) {
                Ok(_script) => {
                    stages.push(Stage::Codegen);

                    // Stage 4: Execute
                    match expected {
                        Some(expected_val) => {
                            let frame = DefaultFrame::from_plan(&def);
                            match frame.invoke(&def) {
                                Ok(execution) => {
                                    if execution.success {
                                        stages.push(Stage::Executed);

                                        // Stage 5: Correct output
                                        let actual = execution.stdout.trim().to_string();
                                        if actual == expected_val {
                                            stages.push(Stage::Correct);
                                        } else {
                                            stages.push(Stage::Wrong {
                                                got: actual,
                                                expected: expected_val,
                                            });
                                        }
                                    } else {
                                        let stderr = execution.stderr.lines().next()
                                            .unwrap_or("unknown error").to_string();
                                        stages.push(Stage::ExecError { message: stderr });
                                    }
                                }
                                Err(e) => {
                                    stages.push(Stage::ExecError {
                                        message: format!("{}", e),
                                    });
                                }
                            }
                        }
                        None => {
                            stages.push(Stage::NoExpected);
                        }
                    }
                }
                Err(e) => {
                    stages.push(Stage::CodegenError {
                        message: format!("{}", e),
                    });
                }
            }
        }
        Err(e) => {
            stages.push(Stage::CompileError {
                message: format!("{}", e),
            });
        }
    }

    PlanResult {
        name,
        category: category.to_string(),
        description,
        stages,
    }
}

// ─── Report ─────────────────────────────────────────────────────────────────

fn print_report(results: &[PlanResult]) {
    // Collect categories
    let mut categories: Vec<String> = results
        .iter()
        .map(|r| r.category.clone())
        .collect::<std::collections::HashSet<_>>()
        .into_iter()
        .collect();
    categories.sort();

    let total = results.len();
    let nl_ok = results.iter().filter(|r| r.stages.iter().any(|s| matches!(s, Stage::NlMatch))).count();
    let compile_ok = results.iter().filter(|r| r.stages.iter().any(|s| matches!(s, Stage::Compiled))).count();
    let codegen_ok = results.iter().filter(|r| r.stages.iter().any(|s| matches!(s, Stage::Codegen))).count();
    let exec_ok = results.iter().filter(|r| r.stages.iter().any(|s| matches!(s, Stage::Executed))).count();
    let correct = results.iter().filter(|r| r.stages.iter().any(|s| matches!(s, Stage::Correct))).count();
    let all_ok = results.iter().filter(|r| r.all_ok()).count();

    // ── Banner ──
    println!();
    println!("╔══════════════════════════════════════════════════════════════════╗");
    println!("║              DOMAIN AUTOREGRESSION REPORT                       ║");
    println!("╠══════════════════════════════════════════════════════════════════╣");
    println!("║  Plans: {:>3}   Categories: {:>2}                                   ║", total, categories.len());
    println!("╠══════════════════════════════════════════════════════════════════╣");
    println!("║  NL Match:  {:>3}/{:<3} ({:>5.1}%)                                   ║", nl_ok, total, pct(nl_ok, total));
    println!("║  Compile:   {:>3}/{:<3} ({:>5.1}%)                                   ║", compile_ok, total, pct(compile_ok, total));
    println!("║  Codegen:   {:>3}/{:<3} ({:>5.1}%)                                   ║", codegen_ok, total, pct(codegen_ok, total));
    println!("║  Execute:   {:>3}/{:<3} ({:>5.1}%)                                   ║", exec_ok, total, pct(exec_ok, total));
    println!("║  Correct:   {:>3}/{:<3} ({:>5.1}%)                                   ║", correct, total, pct(correct, total));
    println!("║  Full pass: {:>3}/{:<3} ({:>5.1}%)                                   ║", all_ok, total, pct(all_ok, total));
    println!("╚══════════════════════════════════════════════════════════════════╝");

    // ── Per-category breakdown ──
    println!();
    println!("  {:24} {:>6} {:>6} {:>6} {:>6} {:>6} {:>6}",
        "Category", "NL", "Comp", "CGen", "Exec", "Corr", "Full");
    println!("  {:24} {:>6} {:>6} {:>6} {:>6} {:>6} {:>6}",
        "────────────────────────", "──────", "──────", "──────", "──────", "──────", "──────");

    for cat in &categories {
        let cat_results: Vec<&PlanResult> = results.iter().filter(|r| r.category == *cat).collect();
        let n = cat_results.len();
        let c_nl = cat_results.iter().filter(|r| r.stages.iter().any(|s| matches!(s, Stage::NlMatch))).count();
        let c_comp = cat_results.iter().filter(|r| r.stages.iter().any(|s| matches!(s, Stage::Compiled))).count();
        let c_cgen = cat_results.iter().filter(|r| r.stages.iter().any(|s| matches!(s, Stage::Codegen))).count();
        let c_exec = cat_results.iter().filter(|r| r.stages.iter().any(|s| matches!(s, Stage::Executed))).count();
        let c_corr = cat_results.iter().filter(|r| r.stages.iter().any(|s| matches!(s, Stage::Correct))).count();
        let c_full = cat_results.iter().filter(|r| r.all_ok()).count();

        println!("  {:24} {:>3}/{:<2} {:>3}/{:<2} {:>3}/{:<2} {:>3}/{:<2} {:>3}/{:<2} {:>3}/{:<2}",
            cat, c_nl, n, c_comp, n, c_cgen, n, c_exec, n, c_corr, n, c_full, n);
    }

    // ── Failures ──
    let failures: Vec<&PlanResult> = results.iter().filter(|r| !r.all_ok()).collect();
    if !failures.is_empty() {
        println!();
        println!("  Failures ({}):", failures.len());
        for r in &failures {
            if let Some(fail) = r.first_failure() {
                let detail = fail.detail();
                if detail.is_empty() {
                    println!("    {:12} {:30} {}", fail.label(), r.name, r.category);
                } else {
                    println!("    {:12} {:30} {} — {}", fail.label(), r.name, r.category, detail);
                }
            }
        }
    }

    println!();
}

fn pct(n: usize, total: usize) -> f64 {
    if total == 0 { 0.0 } else { n as f64 / total as f64 * 100.0 }
}

fn truncate(s: &str, max: usize) -> String {
    if s.len() <= max {
        s.to_string()
    } else {
        format!("{}…", &s[..max])
    }
}

// ─── Test ───────────────────────────────────────────────────────────────────

#[test]
fn test_domain_autoregression_report() {
    let registry = build_full_registry();
    let racket_registry = build_racket_registry();

    let algo_base = Path::new("data/plans/algorithms");
    let mut results = Vec::new();

    if algo_base.exists() {
        let mut categories: Vec<_> = std::fs::read_dir(algo_base)
            .unwrap()
            .filter_map(|e| e.ok())
            .filter(|e| e.file_type().map(|t| t.is_dir()).unwrap_or(false))
            .collect();
        categories.sort_by_key(|e| e.file_name());

        for cat_entry in categories {
            let category = cat_entry.file_name().to_string_lossy().to_string();
            let plans = collect_plans(&cat_entry.path());

            for (path_str, content) in &plans {
                let result = run_plan_funnel(
                    path_str,
                    content,
                    &category,
                    &registry,
                    &racket_registry,
                );
                results.push(result);
            }
        }
    }

    // Also include pipeline plans
    let pipeline_dir = Path::new("data/plans");
    if pipeline_dir.exists() {
        let plans = collect_plans(pipeline_dir);
        for (path_str, content) in &plans {
            let result = run_plan_funnel(
                path_str,
                content,
                "pipeline",
                &registry,
                &racket_registry,
            );
            results.push(result);
        }
    }

    print_report(&results);

    // Soft assertion: report runs without panic. The hard gates are in
    // nl_autoregression.rs and algorithm_plans_tests.rs.
    assert!(
        results.len() >= 200,
        "Expected at least 200 plans, got {}",
        results.len()
    );
}
