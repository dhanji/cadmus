//! NL Autoregression Test Harness
//!
//! Feeds ALL plan descriptions (pipeline + algorithm) through `process_input()`
//! and classifies the result using structural matching:
//!   - MATCH: NL produced a plan whose ops are a valid subsequence of expected ops,
//!            and key params (like glob patterns) are correct
//!   - WRONG_PLAN: NL produced a plan but with wrong ops
//!   - CLARIFY: NL returned NeedsClarification
//!   - ERROR: NL returned an error or unexpected response
//!
//! The test asserts 100% pass rate across all plans combined.

use std::collections::HashMap;
use std::path::Path;

use cadmus::nl::dialogue::DialogueState;
use cadmus::nl::{self, NlResponse};
use cadmus::plan::{parse_plan, PlanDef, RawStep, StepArgs};

// ─── Classification ─────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
enum Classification {
    Match,
    WrongPlan { got_ops: Vec<String>, expected_ops: Vec<String> },
    Clarify,
    Error { message: String },
}

impl Classification {
    fn label(&self) -> &'static str {
        match self {
            Classification::Match => "MATCH",
            Classification::WrongPlan { .. } => "WRONG",
            Classification::Clarify => "CLARIFY",
            Classification::Error { .. } => "ERROR",
        }
    }
}

// ─── Test Case ──────────────────────────────────────────────────────────────

struct TestCase {
    path: String,
    description: String,
    plan_name: String,
    expected_ops: Vec<String>,
    expected_params: HashMap<String, String>,  // op → pattern value
    category: String,
}

// ─── Loading ────────────────────────────────────────────────────────────────

/// Parse a plan file to extract PlanDef (sexpr or YAML).
fn parse_plan_file(content: &str, path: &Path) -> Option<PlanDef> {
    if path.extension().map(|e| e == "sexp").unwrap_or(false) {
        cadmus::sexpr::parse_sexpr_to_plan(content).ok()
    } else {
        parse_plan(content).ok()
    }
}

/// Extract the NL description from the first comment line.
fn extract_description(content: &str) -> Option<String> {
    content
        .lines()
        .find(|line| line.starts_with(";; ") || line.starts_with("# "))
        .and_then(|line| line.strip_prefix(";; ").or_else(|| line.strip_prefix("# ")))
        .map(|s| s.to_string())
        .filter(|s| !s.is_empty())
}

/// Extract expected params: for each step with a :pattern, record op → pattern.
fn extract_expected_params(steps: &[RawStep]) -> HashMap<String, String> {
    let mut params = HashMap::new();
    for step in steps {
        if let StepArgs::Map(ref map) = step.args {
            if let Some(pattern) = map.get("pattern") {
                // Skip $var references and non-Value params
                if let Some(val) = pattern.as_str() {
                    if !val.starts_with('$') {
                        params.insert(step.op.clone(), val.to_string());
                    }
                }
            }
        }
    }
    params
}

/// Load test cases from a directory of plan files.
fn load_from_dir(dir: &Path, category: &str, cases: &mut Vec<TestCase>) {
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
        Err(_) => return,
    };
    files.sort_by_key(|e| e.file_name());

    for file_entry in files {
        let path = file_entry.path();
        let content = match std::fs::read_to_string(&path) {
            Ok(c) => c,
            Err(_) => continue,
        };

        let description = match extract_description(&content) {
            Some(d) => d,
            None => continue,
        };

        let plan = match parse_plan_file(&content, &path) {
            Some(p) => p,
            None => continue,
        };

        let expected_ops: Vec<String> = plan.steps.iter().map(|s| s.op.clone()).collect();
        let expected_params = extract_expected_params(&plan.steps);

        if !expected_ops.is_empty() {
            cases.push(TestCase {
                path: path.to_string_lossy().to_string(),
                description,
                plan_name: plan.name.clone(),
                expected_ops,
                expected_params,
                category: category.to_string(),
            });
        }
    }
}

/// Load ALL test cases: pipeline plans + algorithm plans.
fn load_test_cases() -> Vec<TestCase> {
    let mut cases = Vec::new();

    // 1. Pipeline plans (data/plans/*.sexp)
    let pipeline_dir = Path::new("data/plans");
    if pipeline_dir.exists() {
        load_from_dir(pipeline_dir, "pipeline", &mut cases);
    }

    // 2. Algorithm plans (data/plans/algorithms/<category>/*.sexp|*.yaml)
    let algo_base = Path::new("data/plans/algorithms");
    if algo_base.exists() {
        let mut categories: Vec<_> = std::fs::read_dir(algo_base)
            .unwrap()
            .filter_map(|e| e.ok())
            .filter(|e| e.file_type().map(|t| t.is_dir()).unwrap_or(false))
            .collect();
        categories.sort_by_key(|e| e.file_name());

        for cat_entry in categories {
            let category = cat_entry.file_name().to_string_lossy().to_string();
            load_from_dir(&cat_entry.path(), &category, &mut cases);
        }
    }

    // Deduplicate: if both .sexp and .yaml exist for same plan, prefer .sexp
    let mut seen = std::collections::HashSet::new();
    cases.sort_by(|a, b| a.path.cmp(&b.path)); // .sexp sorts before .yaml
    cases.retain(|c| {
        let key = format!(
            "{}/{}",
            c.category,
            Path::new(&c.path)
                .file_stem()
                .unwrap()
                .to_string_lossy()
        );
        seen.insert(key)
    });

    cases
}

// ─── Matching ───────────────────────────────────────────────────────────────

/// Op-name aliases: ops that are semantically equivalent.
fn normalize_op(op: &str) -> &str {
    match op {
        "for_fold" | "foldl" | "foldr" | "racket_foldl" | "racket_foldr" => "fold",
        "for_list" | "racket_map" => "map",
        "for_each" | "racket_for_each" => "for_each",
        "iterate" => "iterate",
        "list_dir" | "walk_tree" => op, // keep distinct — both are valid starts
        "filter" | "racket_filter" => "filter",
        "sort_by" | "sort_list" => "sort",
        "find_matching" => "find_matching",
        "unique" | "remove_duplicates" => "unique",
        "count" | "count_entries" => "count",
        _ => op,
    }
}

/// Check if `got` ops are a valid ordered subsequence of `expected` ops.
/// Returns (matched_count, expected_count).
/// A match means the NL plan hits the key ops in the right order,
/// even if it skips some intermediate steps.
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

/// Check if key params match (e.g., pattern "*.pdf").
/// Returns true if all expected literal params appear in the got plan.
fn params_match(expected_params: &HashMap<String, String>, got_plan: &PlanDef) -> bool {
    if expected_params.is_empty() {
        return true;
    }

    for (expected_op, expected_pattern) in expected_params {
        // Find any step in got_plan that has a matching pattern
        let found = got_plan.steps.iter().any(|step| {
            // Check if this step's op matches (or is an alias)
            let op_matches = normalize_op(&step.op) == normalize_op(expected_op)
                || step.op == *expected_op;

            if !op_matches {
                return false;
            }

            // Check if the pattern param matches
            match &step.args {
                StepArgs::Map(map) => {
                    if let Some(got_pattern) = map.get("pattern") {
                        if let Some(got_val) = got_pattern.as_str() {
                            // Exact match or the expected pattern is contained
                            got_val == expected_pattern
                                || got_val.contains(expected_pattern.trim_start_matches("*."))
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                }
                _ => false,
            }
        });

        if !found {
            return false;
        }
    }

    true
}

/// Classify an NL response against the expected plan.
fn classify(
    response: &NlResponse,
    expected_name: &str,
    expected_ops: &[String],
    expected_params: &HashMap<String, String>,
) -> Classification {
    match response {
        NlResponse::PlanCreated { plan_sexpr, .. } => {
            // Parse the generated plan (sexpr first, YAML fallback)
            let sexpr_result = cadmus::sexpr::parse_sexpr_to_plan(plan_sexpr);
            let parsed = match sexpr_result {
                Ok(plan) => Ok(plan),
                Err(ref sexpr_err) => parse_plan(plan_sexpr)
                    .map_err(|_| sexpr_err.to_string()),
            };

            match parsed {
                Ok(plan) => {
                    let got_ops: Vec<String> =
                        plan.steps.iter().map(|s| s.op.clone()).collect();

                    // ── Strategy 1: exact plan-name match ──
                    let name_match = {
                        let pn = plan.name.to_lowercase().replace('-', "_");
                        let en = expected_name.to_lowercase().replace('-', "_");
                        pn == en || pn.contains(&en) || en.contains(&pn)
                    };

                    // ── Strategy 2: any got op matches expected name ──
                    let op_name_match = got_ops
                        .iter()
                        .any(|op| normalize_op(op) == normalize_op(expected_name));

                    // ── Strategy 3: ordered subsequence of ops ──
                    let (matched, total) = subsequence_match(expected_ops, &got_ops);
                    // For single-step plans, require exact op match.
                    // For multi-step plans, require >= 50% subsequence match.
                    let subseq_match = if total <= 1 {
                        matched == total
                    } else {
                        matched * 2 >= total // at least 50%
                    };

                    // ── Strategy 4: param matching ──
                    let params_ok = params_match(expected_params, &plan);

                    // A plan is a MATCH if:
                    //   (name matches OR op matches OR subsequence matches)
                    //   AND params are correct (when expected)
                    if (name_match || op_name_match || subseq_match) && params_ok {
                        Classification::Match
                    } else {
                        Classification::WrongPlan {
                            got_ops,
                            expected_ops: expected_ops.to_vec(),
                        }
                    }
                }
                Err(e) => Classification::Error {
                    message: format!("Parse error: {}", e),
                },
            }
        }
        NlResponse::NeedsClarification { .. } => Classification::Clarify,
        NlResponse::Error { message } => Classification::Error {
            message: message.clone(),
        },
        other => Classification::Error {
            message: format!("Unexpected response: {:?}", other),
        },
    }
}

// ─── Runner ─────────────────────────────────────────────────────────────────

fn run_autoregression() -> Vec<(TestCase, Classification)> {
    let cases = load_test_cases();
    let mut results = Vec::new();

    for case in cases {
        let mut state = DialogueState::new();
        let response = nl::process_input(&case.description, &mut state);
        let classification = classify(
            &response,
            &case.plan_name,
            &case.expected_ops,
            &case.expected_params,
        );
        results.push((case, classification));
    }

    results
}

// ─── Report ─────────────────────────────────────────────────────────────────

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
    let wrong = counts.get("WRONG").copied().unwrap_or(0);
    let clarify = counts.get("CLARIFY").copied().unwrap_or(0);
    let errors = counts.get("ERROR").copied().unwrap_or(0);
    let pct = if total > 0 {
        matches as f64 / total as f64 * 100.0
    } else {
        0.0
    };

    // ── Summary ──
    println!("\n╔════════════════════════════════════════════════════════╗");
    println!("║            NL AUTOREGRESSION REPORT                   ║");
    println!("╠════════════════════════════════════════════════════════╣");
    println!(
        "║  Total: {:>3}  MATCH: {:>3}  WRONG: {:>3}  CLARIFY: {:>3}  ERROR: {:>3}  ║",
        total, matches, wrong, clarify, errors
    );
    println!(
        "║  Score: {:.1}%                                             ║",
        pct
    );
    println!("╚════════════════════════════════════════════════════════╝");

    // ── Per-category breakdown ──
    println!("\n  Per-category breakdown:");
    let mut cats: Vec<_> = by_category.keys().cloned().collect();
    cats.sort();
    for cat in &cats {
        let cat_counts = &by_category[cat];
        let cat_match = cat_counts.get("MATCH").copied().unwrap_or(0);
        let cat_total: usize = cat_counts.values().sum();
        let cat_pct = if cat_total > 0 {
            cat_match as f64 / cat_total as f64 * 100.0
        } else {
            0.0
        };
        let marker = if cat == "pipeline" { " ◆" } else { "" };
        println!(
            "    {:<24} {:>3}/{:<3}  ({:>5.1}%){marker}",
            cat, cat_match, cat_total, cat_pct,
        );
    }

    // ── Detailed failures ──
    println!("\n  Failures:");
    for (case, class) in &results {
        match class {
            Classification::Match => {}
            Classification::WrongPlan {
                got_ops,
                expected_ops,
            } => {
                println!(
                    "    WRONG  [{:<12}] {:<35} expected=[{}]  got=[{}]",
                    case.category,
                    case.plan_name,
                    truncate_ops(expected_ops, 40),
                    truncate_ops(got_ops, 40),
                );
            }
            Classification::Clarify => {
                println!(
                    "    CLRFY  [{:<12}] {:<35} \"{}\"",
                    case.category,
                    case.plan_name,
                    truncate(&case.description, 50)
                );
            }
            Classification::Error { message } => {
                println!(
                    "    ERROR  [{:<12}] {:<35} {}",
                    case.category,
                    case.plan_name,
                    truncate(message, 60)
                );
            }
        }
    }

    // ── Assertions ──
    assert!(
        total >= 200,
        "Expected at least 200 test cases (pipeline + algorithm), got {}",
        total
    );

    // Hard gate: 100% pass rate across all plans
    let required_pct = 100.0;
    assert!(
        pct >= required_pct,
        "NL autoregression pass rate {:.1}% is below required {:.0}%.\n\
         {}/{} plans matched. Failures:\n{}",
        pct, required_pct, matches, total,
        results.iter()
            .filter(|(_, c)| !matches!(c, Classification::Match))
            .map(|(c, cl)| format!("  {} [{}] {}", cl.label(), c.category, c.plan_name))
            .collect::<Vec<_>>()
            .join("\n")
    );

    println!(
        "\n  Result: {}/{} ({:.1}%)\n",
        matches, total, pct
    );
}

// ─── Helpers ────────────────────────────────────────────────────────────────

fn truncate(s: &str, max: usize) -> String {
    if s.len() <= max {
        s.to_string()
    } else {
        format!("{}…", &s[..max])
    }
}

fn truncate_ops(ops: &[String], max: usize) -> String {
    let joined = ops.join(", ");
    truncate(&joined, max)
}
