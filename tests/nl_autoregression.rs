//! NL Autoregression Test Harness
//!
//! Feeds each of the 108 algorithm plan descriptions through `process_input()`
//! and classifies the result:
//!   - MATCH: NL produced a plan whose primary op matches the expected plan
//!   - WRONG_PLAN: NL produced a plan but with wrong ops
//!   - CLARIFY: NL returned NeedsClarification
//!   - ERROR: NL returned an error or unexpected response
//!
//! This is a measurement tool, not a pass/fail test. The single #[test]
//! function always passes but prints a structured report.

use std::collections::HashMap;
use std::path::Path;

use cadmus::nl::dialogue::DialogueState;
use cadmus::nl::{self, NlResponse};
use cadmus::plan::parse_plan;

/// Classification of an NL result.
#[derive(Debug, Clone, PartialEq)]
enum Classification {
    /// NL produced a plan with the correct primary op or plan name.
    Match,
    /// NL produced a plan but with wrong ops.
    WrongPlan { got_ops: Vec<String> },
    /// NL returned NeedsClarification.
    Clarify,
    /// NL returned an error.
    Error { message: String },
}

impl Classification {
    fn label(&self) -> &'static str {
        match self {
            Classification::Match => "MATCH",
            Classification::WrongPlan { .. } => "WRONG_PLAN",
            Classification::Clarify => "CLARIFY",
            Classification::Error { .. } => "ERROR",
        }
    }
}

/// A single test case: plan file → NL description → expected plan name.
struct TestCase {
    /// Path to the plan YAML file.
    path: String,
    /// The NL description (first comment line).
    description: String,
    /// The expected plan name (top-level YAML key).
    plan_name: String,
    /// Category (e.g., "sorting", "graph").
    category: String,
}

/// Load all 108 algorithm plan test cases.
fn load_test_cases() -> Vec<TestCase> {
    let base = Path::new("data/plans/algorithms");
    let mut cases = Vec::new();

    if !base.exists() {
        return cases;
    }

    // Walk category directories
    let mut categories: Vec<_> = std::fs::read_dir(base)
        .unwrap()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().map(|t| t.is_dir()).unwrap_or(false))
        .collect();
    categories.sort_by_key(|e| e.file_name());

    for cat_entry in categories {
        let category = cat_entry.file_name().to_string_lossy().to_string();

        let mut files: Vec<_> = std::fs::read_dir(cat_entry.path())
            .unwrap()
            .filter_map(|e| e.ok())
            .filter(|e| {
                e.path()
                    .extension()
                    .map(|ext| ext == "yaml" || ext == "sexp")
                    .unwrap_or(false)
            })
            .collect();
        files.sort_by_key(|e| e.file_name());

        for file_entry in files {
            let path = file_entry.path();
            let content = std::fs::read_to_string(&path).unwrap();

            // Extract description from first comment line
            let description = content
                .lines()
                .next()
                .and_then(|line| line.strip_prefix("# ").or_else(|| line.strip_prefix(";; ")))
                .unwrap_or("")
                .to_string();

            // Extract plan name
            let plan_name = if path.extension().map(|e| e == "sexp").unwrap_or(false) {
                // For .sexp: parse to get the name
                cadmus::sexpr::parse_sexpr_to_plan(&content)
                    .map(|def| def.name.clone())
                    .unwrap_or_default()
            } else {
                // For .yaml: first non-comment key ending with ':'
                content
                    .lines()
                    .find(|line| !line.starts_with('#') && !line.is_empty() && line.ends_with(':'))
                    .map(|line| line.trim_end_matches(':').to_string())
                    .unwrap_or_default()
            };

            if !description.is_empty() && !plan_name.is_empty() {
                cases.push(TestCase {
                    path: path.to_string_lossy().to_string(),
                    description,
                    plan_name,
                    category: category.clone(),
                });
            }
        }
    }

    // Deduplicate: if both .sexp and .yaml exist for same plan, prefer .sexp
    let mut seen = std::collections::HashSet::new();
    cases.sort_by(|a, b| a.path.cmp(&b.path)); // .sexp sorts before .yaml
    cases.retain(|c| {
        let stem = std::path::Path::new(&c.path)
            .file_stem().unwrap().to_string_lossy().to_string();
        seen.insert(stem)
    });

    cases
}

/// Classify an NL response against the expected plan.
fn classify(response: &NlResponse, expected_plan_name: &str) -> Classification {
    match response {
        NlResponse::PlanCreated { plan_sexpr, .. } => {
            // Parse the generated YAML to extract ops
            match cadmus::sexpr::parse_sexpr_to_plan(plan_sexpr)
                .map_err(|e| e.to_string())
                .or_else(|_| parse_plan(plan_sexpr).map_err(|e| e.to_string())) {
                Ok(plan) => {
                    let ops: Vec<String> = plan.steps.iter().map(|s| s.op.clone()).collect();

                    // Check if the plan name matches
                    if plan.name == expected_plan_name {
                        return Classification::Match;
                    }

                    // Check if any step uses the expected op
                    if ops.iter().any(|op| op == expected_plan_name) {
                        return Classification::Match;
                    }

                    // Check if the plan name contains the expected name or vice versa
                    let plan_lower = plan.name.to_lowercase().replace('-', "_");
                    let expected_lower = expected_plan_name.to_lowercase();
                    if plan_lower.contains(&expected_lower)
                        || expected_lower.contains(&plan_lower)
                    {
                        return Classification::Match;
                    }

                    Classification::WrongPlan { got_ops: ops }
                }
                Err(e) => Classification::Error {
                    message: format!("YAML parse error: {}", e),
                },
            }
        }
        NlResponse::NeedsClarification { .. } => Classification::Clarify,
        NlResponse::Error { message } => Classification::Error {
            message: message.clone(),
        },
        other => Classification::Error {
            message: format!("Unexpected response type: {:?}", other),
        },
    }
}

/// Run the full autoregression suite and return results.
fn run_autoregression() -> Vec<(TestCase, Classification)> {
    let cases = load_test_cases();
    let mut results = Vec::new();

    for case in cases {
        let mut state = DialogueState::new();
        let response = nl::process_input(&case.description, &mut state);
        let classification = classify(&response, &case.plan_name);
        results.push((case, classification));
    }

    results
}

#[test]
fn test_nl_autoregression_report() {
    let results = run_autoregression();

    // Count by classification
    let mut counts: HashMap<&str, usize> = HashMap::new();
    let mut by_category: HashMap<String, HashMap<&str, usize>> = HashMap::new();

    for (case, class) in &results {
        *counts.entry(class.label()).or_insert(0) += 1;
        *by_category
            .entry(case.category.clone())
            .or_default()
            .entry(class.label())
            .or_insert(0) += 1;
    }

    let total = results.len();
    let matches = counts.get("MATCH").copied().unwrap_or(0);
    let wrong = counts.get("WRONG_PLAN").copied().unwrap_or(0);
    let clarify = counts.get("CLARIFY").copied().unwrap_or(0);
    let errors = counts.get("ERROR").copied().unwrap_or(0);

    // Print summary
    println!("\n╔══════════════════════════════════════════════════╗");
    println!("║         NL AUTOREGRESSION REPORT                ║");
    println!("╠══════════════════════════════════════════════════╣");
    println!(
        "║  Total: {:>3}  MATCH: {:>3}  WRONG: {:>3}  CLARIFY: {:>3}  ERROR: {:>3} ║",
        total, matches, wrong, clarify, errors
    );
    println!(
        "║  Score: {:.1}%                                      ║",
        if total > 0 {
            matches as f64 / total as f64 * 100.0
        } else {
            0.0
        }
    );
    println!("╚══════════════════════════════════════════════════╝");

    // Per-category breakdown
    println!("\n  Per-category breakdown:");
    let mut cats: Vec<_> = by_category.keys().cloned().collect();
    cats.sort();
    for cat in &cats {
        let cat_counts = &by_category[cat];
        let cat_match = cat_counts.get("MATCH").copied().unwrap_or(0);
        let cat_total: usize = cat_counts.values().sum();
        println!(
            "    {:<20} {:>2}/{:<2}  ({:.0}%)",
            cat,
            cat_match,
            cat_total,
            if cat_total > 0 {
                cat_match as f64 / cat_total as f64 * 100.0
            } else {
                0.0
            }
        );
    }

    // Detailed failures
    println!("\n  Failures:");
    for (case, class) in &results {
        match class {
            Classification::Match => {} // skip successes
            Classification::WrongPlan { got_ops } => {
                println!(
                    "    WRONG  {:<35} expected={:<25} got=[{}]",
                    case.plan_name,
                    case.plan_name,
                    got_ops.join(", ")
                );
            }
            Classification::Clarify => {
                println!(
                    "    CLRFY  {:<35} desc=\"{}\"",
                    case.plan_name,
                    truncate(&case.description, 50)
                );
            }
            Classification::Error { message } => {
                println!(
                    "    ERROR  {:<35} {}",
                    case.plan_name,
                    truncate(message, 60)
                );
            }
        }
    }

    // Assert we loaded all 108 plans
    assert!(
        total >= 108,
        "Expected at least 108 test cases, got {}",
        total
    );

    // This test always passes — it's a measurement tool.
    // The score assertion will be added in I6 after fixes.
    println!(
        "\n  Baseline: {}/{} ({:.1}%)",
        matches,
        total,
        if total > 0 {
            matches as f64 / total as f64 * 100.0
        } else {
            0.0
        }
    );
}

fn truncate(s: &str, max: usize) -> String {
    if s.len() <= max {
        s.to_string()
    } else {
        format!("{}...", &s[..max])
    }
}
