/// Integration tests for all algorithm plans.
///
/// Each test loads a plan YAML, compiles it, generates a Racket script,
/// executes it, and verifies the output matches the expected value
/// from the `# expected:` comment in the YAML.

use cadmus::calling_frame::{CallingFrame, DefaultFrame};
use cadmus::plan::parse_plan;
use std::fs;
use std::path::Path;

/// Run a single algorithm plan and return (actual_output, expected_output).
fn run_algorithm_plan(yaml_path: &str) -> (String, String) {
    let yaml = fs::read_to_string(yaml_path)
        .unwrap_or_else(|e| panic!("Failed to read {}: {}", yaml_path, e));

    // Extract expected output from `# expected: ...` comment
    let expected = yaml.lines()
        .find(|l| l.starts_with("# expected:"))
        .map(|l| l.trim_start_matches("# expected:").trim().to_string())
        .unwrap_or_else(|| panic!("No '# expected:' comment in {}", yaml_path));

    // Parse and compile
    let def = parse_plan(&yaml)
        .unwrap_or_else(|e| panic!("Parse failed for {}: {}", yaml_path, e));

    let frame = DefaultFrame::from_plan(&def);
    let execution = frame.invoke(&def)
        .unwrap_or_else(|e| panic!("Invoke failed for {}: {}", yaml_path, e));

    let actual = execution.stdout.trim().to_string();
    (actual, expected)
}

/// Collect all YAML files in a directory.
fn collect_plans(dir: &str) -> Vec<String> {
    let path = Path::new(dir);
    if !path.exists() {
        return vec![];
    }
    let mut plans: Vec<String> = fs::read_dir(path)
        .unwrap()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().extension().map(|x| x == "yaml").unwrap_or(false))
        .map(|e| e.path().to_string_lossy().to_string())
        .collect();
    plans.sort();
    plans
}

// ============================================================================
// Per-category tests
// ============================================================================

macro_rules! category_test {
    ($name:ident, $dir:expr) => {
        #[test]
        fn $name() {
            let plans = collect_plans($dir);
            assert!(!plans.is_empty(), "No plans found in {}", $dir);
            let mut failures = Vec::new();
            for plan_path in &plans {
                let plan_name = Path::new(plan_path)
                    .file_stem().unwrap().to_string_lossy().to_string();
                let (actual, expected) = run_algorithm_plan(plan_path);
                if actual != expected {
                    failures.push(format!(
                        "  {} — got '{}', expected '{}'",
                        plan_name, actual, expected
                    ));
                }
            }
            if !failures.is_empty() {
                panic!(
                    "{} failures in {}:\n{}",
                    failures.len(), $dir, failures.join("\n")
                );
            }
        }
    };
}

category_test!(test_arithmetic, "data/plans/algorithms/arithmetic");
category_test!(test_number_theory, "data/plans/algorithms/number-theory");
category_test!(test_sorting, "data/plans/algorithms/sorting");
category_test!(test_searching, "data/plans/algorithms/searching");
category_test!(test_dynamic_programming, "data/plans/algorithms/dynamic-programming");
category_test!(test_graph, "data/plans/algorithms/graph");
category_test!(test_string, "data/plans/algorithms/string");
category_test!(test_bitwise, "data/plans/algorithms/bitwise");
category_test!(test_combinatorics, "data/plans/algorithms/combinatorics");
category_test!(test_geometry, "data/plans/algorithms/geometry");
category_test!(test_data_structures, "data/plans/algorithms/data-structures");
category_test!(test_encoding, "data/plans/algorithms/encoding");
category_test!(test_hashing, "data/plans/algorithms/hashing");
category_test!(test_probability, "data/plans/algorithms/probability");
category_test!(test_matrix, "data/plans/algorithms/matrix");
category_test!(test_backtracking, "data/plans/algorithms/backtracking");
category_test!(test_interval, "data/plans/algorithms/interval");
category_test!(test_tree, "data/plans/algorithms/tree");

// ============================================================================
// Aggregate test: all plans
// ============================================================================

static ALL_CATEGORIES: &[&str] = &[
    "data/plans/algorithms/arithmetic",
    "data/plans/algorithms/number-theory",
    "data/plans/algorithms/sorting",
    "data/plans/algorithms/searching",
    "data/plans/algorithms/dynamic-programming",
    "data/plans/algorithms/graph",
    "data/plans/algorithms/string",
    "data/plans/algorithms/bitwise",
    "data/plans/algorithms/combinatorics",
    "data/plans/algorithms/geometry",
    "data/plans/algorithms/data-structures",
    "data/plans/algorithms/encoding",
    "data/plans/algorithms/hashing",
    "data/plans/algorithms/probability",
    "data/plans/algorithms/matrix",
    "data/plans/algorithms/backtracking",
    "data/plans/algorithms/interval",
    "data/plans/algorithms/tree",
];

#[test]
fn test_all_algorithm_plans() {
    let mut all_plans = Vec::new();
    for cat in ALL_CATEGORIES {
        all_plans.extend(collect_plans(cat));
    }

    eprintln!("Running {} algorithm plans...", all_plans.len());

    let mut pass = 0;
    let mut failures = Vec::new();

    for plan_path in &all_plans {
        let plan_name = Path::new(plan_path)
            .file_stem().unwrap().to_string_lossy().to_string();
        let (actual, expected) = run_algorithm_plan(plan_path);
        if actual == expected {
            pass += 1;
        } else {
            failures.push(format!(
                "  {} — got '{}', expected '{}'",
                plan_name, actual, expected
            ));
        }
    }

    eprintln!("{}/{} passed", pass, all_plans.len());

    if !failures.is_empty() {
        panic!(
            "{}/{} passed, {} failures:\n{}",
            pass, all_plans.len(), failures.len(), failures.join("\n")
        );
    }
}

// ============================================================================
// Compile-only test: verify all plans at least compile
// ============================================================================

#[test]
fn test_all_plans_compile() {
    let mut all_plans = Vec::new();
    for cat in ALL_CATEGORIES {
        all_plans.extend(collect_plans(cat));
    }

    let registry = cadmus::fs_types::build_full_registry();
    let mut failures = Vec::new();

    for plan_path in &all_plans {
        let yaml = fs::read_to_string(&plan_path).unwrap();
        let def = match parse_plan(&yaml) {
            Ok(d) => d,
            Err(e) => {
                failures.push(format!("  {} — parse: {}", plan_path, e));
                continue;
            }
        };
        if let Err(e) = cadmus::plan::compile_plan(&def, &registry) {
            failures.push(format!("  {} — compile: {}", plan_path, e));
        }
    }

    if !failures.is_empty() {
        panic!(
            "{} compile failures:\n{}",
            failures.len(), failures.join("\n")
        );
    }
}
