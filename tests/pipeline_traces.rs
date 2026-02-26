//! Pipeline Trace Tests
//!
//! Traces real prompts through every stage of the Cadmus pipeline:
//!   NL input → normalize → typo correct → phrase tokenize → Earley parse
//!   → IntentIR → compile → PlanDef → compile_plan → codegen → execute
//!
//! Each test prints a human-readable trace, then asserts correctness.

use cadmus::nl::normalize;
use cadmus::nl::typo;
use cadmus::nl::phrase;
use cadmus::nl::earley;
use cadmus::nl::grammar;
use cadmus::nl::lexicon;
use cadmus::nl::intent_ir;
use cadmus::nl::dialogue::{self, DialogueState};
use cadmus::nl::{self, NlResponse};
use cadmus::plan;
use cadmus::racket_executor;
use cadmus::calling_frame::{CallingFrame, DefaultFrame};

// ─── Helpers ────────────────────────────────────────────────────────────────

/// Full pipeline trace: captures each stage's output for inspection.
struct PipelineTrace {
    input: String,
    // Stage 1-3: NL preprocessing
    normalized_tokens: Vec<String>,
    corrected_tokens: Vec<String>,
    phrase_tokens: Vec<String>,
    // Stage 4: Earley parse (may be skipped for algorithm short-circuit)
    earley_parse_count: usize,
    earley_summary: String,
    // Stage 5-6: IR + plan creation (via process_input)
    plan_name: String,
    plan_ops: Vec<String>,
    plan_sexpr: String,
    // Stage 7: Type chain from compile_plan
    type_chain: Vec<String>,
    // Stage 8: Racket codegen
    racket_script: String,
    // Stage 9: Execution output
    execution_output: String,
}

fn trace_prompt(input: &str) -> PipelineTrace {
    // ── Stage 1: Normalize ──
    let norm = normalize::normalize(input);
    let normalized_tokens = norm.tokens.clone();

    // ── Stage 2: Typo correction ──
    let dict = typo::domain_dict();
    let corrected_tokens = dict.correct_tokens(&norm.tokens);

    // ── Stage 3: Phrase tokenization ──
    let phrase_tokens = phrase::phrase_tokenize(&corrected_tokens);

    // ── Stage 4: Earley parse (informational — may not be the path taken) ──
    let gram = grammar::build_command_grammar();
    let lex = lexicon::lexicon();
    let parses = earley::parse(&gram, &phrase_tokens, lex);
    let earley_parse_count = parses.len();
    let earley_summary = if parses.is_empty() {
        "(no parse — will use pre-Earley short-circuit)".to_string()
    } else {
        format!("{} parse(s), best score: {:.1}", parses.len(), parses[0].score)
    };

    // ── Stage 5-6: Full NL pipeline via process_input ──
    // This is the real path — it includes pre-Earley algorithm matching,
    // plan file lookup, Earley parsing, and recipe compilation.
    let mut state = DialogueState::new();
    let response = nl::process_input(input, &mut state);

    let (plan_def, plan_sexpr) = match response {
        NlResponse::PlanCreated { plan_sexpr, .. } => {
            let plan = state.current_plan.clone().expect("PlanCreated but no current_plan");
            (plan, plan_sexpr)
        }
        other => panic!(
            "NL pipeline did not produce a plan for '{}'. Got: {:?}",
            input,
            match &other {
                NlResponse::NeedsClarification { needs } => format!("NeedsClarification: {:?}", needs),
                NlResponse::Error { message } => format!("Error: {}", message),
                NlResponse::Explanation { text } => format!("Explanation: {}", text),
                _ => format!("{:?}", other),
            }
        ),
    };

    let plan_name = plan_def.name.clone();
    let plan_ops: Vec<String> = plan_def.steps.iter().map(|s| s.op.clone()).collect();

    // ── Stage 7: compile_plan (type checking via unification) ──
    let registry = cadmus::fs_types::build_full_registry();
    let compiled = plan::compile_plan(&plan_def, &registry)
        .unwrap_or_else(|e| panic!("compile_plan failed for '{}': {}", input, e));

    let type_chain: Vec<String> = compiled.steps.iter().map(|s| {
        format!("{}: {} → {}", s.op, s.input_type, s.output_type)
    }).collect();

    // ── Stage 8: Racket codegen ──
    let racket_registry = racket_executor::build_racket_registry();
    let script = racket_executor::generate_racket_script(
        &compiled, &plan_def, &racket_registry,
    ).unwrap_or_else(|e| panic!("codegen failed for '{}': {:?}", input, e));

    // ── Stage 9: Execute ──
    let frame = DefaultFrame::from_plan(&plan_def);
    let execution_output = match frame.run_script(&script) {
        Ok(exec) => exec.stdout.trim().to_string(),
        Err(e) => format!("(exec error: {})", e),
    };

    PipelineTrace {
        input: input.to_string(),
        normalized_tokens,
        corrected_tokens,
        phrase_tokens,
        earley_parse_count,
        earley_summary,
        plan_name,
        plan_ops,
        plan_sexpr,
        type_chain,
        racket_script: script,
        execution_output,
    }
}

fn print_trace(t: &PipelineTrace) {
    eprintln!();
    eprintln!("========================================================================");
    eprintln!("  PROMPT: \"{}\"", t.input);
    eprintln!("========================================================================");
    eprintln!();
    eprintln!("  1. Normalize:      {:?}", t.normalized_tokens);
    eprintln!("  2. Typo correct:   {:?}", t.corrected_tokens);
    eprintln!("  3. Phrase tokens:  {:?}", t.phrase_tokens);
    eprintln!("  4. Earley parse:   {}", t.earley_summary);
    eprintln!("  5. Plan name:      {}", t.plan_name);
    eprintln!("  6. Plan ops:       [{}]", t.plan_ops.join(" → "));
    eprintln!("  7. Type chain:");
    for tc in &t.type_chain {
        eprintln!("       {}", tc);
    }
    eprintln!("  8. Racket script:  ({} chars)", t.racket_script.len());
    eprintln!("  9. Output:         {}", if t.execution_output.is_empty() { "(empty)" } else { &t.execution_output });
    eprintln!();
}

// ═══════════════════════════════════════════════════════════════════════════
// Full pipeline traces — NL prompt → normalize → compile → codegen → execute
// ═══════════════════════════════════════════════════════════════════════════

/// Trace 1: "find all PDFs in ~/Documents"
///
/// Path: Earley parse → IR(select, type=pdf) → recipe(walk_tree, find_matching, sort_by)
///       → compile: Dir(File(PDF)) threads through chain
///       → Racket: find + grep + sort
#[test]
fn trace_find_pdfs() {
    let t = trace_prompt("find all PDFs in ~/Documents");
    print_trace(&t);

    assert!(t.corrected_tokens.contains(&"find".to_string()));
    assert!(t.plan_ops.iter().any(|op| op == "list_dir" || op == "walk_tree" || op == "find_matching"),
        "Expected filesystem ops, got: {:?}", t.plan_ops);
    assert!(!t.racket_script.is_empty());
    assert!(t.racket_script.contains("shell-quote"),
        "Expected shell-quote for path safety");
}

/// Trace 2: "zip up everything in ~/Downloads"
///
/// Path: phrase tokenize("zip up" → "compress") → Earley → IR(compress)
///       → recipe(walk_tree, pack_archive) → compile → Racket: find + tar
#[test]
#[ignore] // slow: actually zips ~/Downloads
fn trace_zip_downloads() {
    let t = trace_prompt("zip up everything in ~/Downloads");
    print_trace(&t);

    assert!(t.plan_ops.iter().any(|op| op == "walk_tree"),
        "Expected walk_tree, got: {:?}", t.plan_ops);
    assert!(t.plan_ops.iter().any(|op| op == "pack_archive"),
        "Expected pack_archive, got: {:?}", t.plan_ops);
}

/// Trace 3: "sort files by size biggest first"
///
/// Path: Earley → IR(order, by=size, direction=desc)
///       → recipe(walk_tree, sort_by) with mode "size_desc"
///       → compile → Racket: find + sort
#[test]
fn trace_sort_by_size() {
    let t = trace_prompt("sort files by size biggest first");
    print_trace(&t);

    assert!(t.plan_ops.iter().any(|op| op == "sort_by"),
        "Expected sort_by, got: {:?}", t.plan_ops);
}

/// Trace 4: "compute the factorial"
///
/// Path: pre-Earley short-circuit → "factorial" matches plan file
///       → loads algorithms/arithmetic/factorial.sexp
///       → compile: Number chain → Racket: for/fold
///       → output: 3628800
#[test]
fn trace_factorial() {
    let t = trace_prompt("compute the factorial");
    print_trace(&t);

    assert_eq!(t.plan_name, "factorial");
    assert!(t.execution_output.contains("3628800"),
        "Expected 3628800, got: {}", t.execution_output);
}

/// Trace 5: "add numbers"
///
/// Path: pre-Earley → token pair "add_numbers" matches plan file
///       → loads data/plans/add_numbers.sexp (with bind x=4, y=35)
///       → compile: Number → Racket: (+ 4 35) → output: 39
#[test]
fn trace_add_numbers() {
    let t = trace_prompt("add numbers");
    print_trace(&t);

    assert!(t.plan_ops.contains(&"add".to_string()),
        "Expected add op, got: {:?}", t.plan_ops);
    // The plan file has bind x=4, y=35 → output 39
    assert!(t.execution_output.contains("39"),
        "Expected 39, got: {}", t.execution_output);
}

/// Trace 6: "run dijkstra shortest path"
///
/// Path: pre-Earley → token pair "dijkstra_shortest" or triple match
///       → loads algorithms/graph/dijkstra_shortest_path.sexp
///       → compile + execute → output: (0 3 1 4 7)
#[test]
fn trace_dijkstra() {
    let t = trace_prompt("run dijkstra shortest path");
    print_trace(&t);

    assert!(t.plan_name.contains("dijkstra"),
        "Expected dijkstra plan, got: {}", t.plan_name);
    assert!(t.execution_output.contains("0") && t.execution_output.contains("3"),
        "Expected shortest path distances, got: {}", t.execution_output);
}

/// Trace 7: "list files in /tmp"
///
/// Path: Earley → IR(enumerate, location=/tmp)
///       → recipe(list_dir) → compile: Dir(Bytes) → Seq(Entry(Name, Bytes))
///       → Racket: ls /tmp → output: actual directory listing
#[test]
fn trace_list_tmp() {
    let t = trace_prompt("list files in /tmp");
    print_trace(&t);

    assert!(t.plan_ops.iter().any(|op| op == "list_dir"),
        "Expected list_dir, got: {:?}", t.plan_ops);
    // /tmp should exist and produce some output
    assert!(!t.racket_script.is_empty());
}

/// Trace 8: "fibonacci" (direct plan file match)
///
/// Path: pre-Earley → first token "fibonacci" matches plan file
///       → loads algorithms/arithmetic/fibonacci.sexp
///       → includes (bind n 10) → execute → output: 55
#[test]
fn trace_fibonacci() {
    let t = trace_prompt("fibonacci");
    print_trace(&t);

    assert!(t.plan_name.contains("fibonacci"),
        "Expected fibonacci plan, got: {}", t.plan_name);
    assert!(t.execution_output.contains("55"),
        "Expected 55, got: {}", t.execution_output);
}

/// Trace 9: "merge sort a list"
///
/// Path: pre-Earley → token pair "merge_sort" matches plan file
///       → loads algorithms/sorting/merge_sort.sexp
///       → execute: sorts (5 3 8 1 9 2 7 4 6)
///       → output: (1 2 3 4 5 6 7 8 9)
#[test]
fn trace_merge_sort() {
    let t = trace_prompt("merge sort a list");
    print_trace(&t);

    assert!(t.plan_name.contains("merge_sort") || t.plan_name.contains("sort"),
        "Expected merge_sort plan, got: {}", t.plan_name);
    assert!(t.execution_output.contains("1 2 3 4 5 6 7 8 9"),
        "Expected sorted list, got: {}", t.execution_output);
}

/// Trace 10: "binary search for 7"
///
/// Path: pre-Earley → "binary_search" matches plan file
///       → loads algorithms/searching/binary_search.sexp
///       → execute: search 7 in (1 3 5 7 9 11 13)
///       → output: 3 (index)
#[test]
fn trace_binary_search() {
    let t = trace_prompt("binary search for 7");
    print_trace(&t);

    assert!(t.plan_name.contains("binary_search"),
        "Expected binary_search plan, got: {}", t.plan_name);
    assert!(t.execution_output.contains("3"),
        "Expected index 3, got: {}", t.execution_output);
}

/// Trace 11: "calculate the mean of a list"
///
/// Path: pre-Earley → "mean_list" or "mean" matches plan/op
///       → loads algorithms/statistics/mean_list.sexp
///       → execute → numeric output
#[test]
fn trace_mean() {
    let t = trace_prompt("calculate the mean of a list");
    print_trace(&t);

    assert!(t.plan_name.contains("mean"),
        "Expected mean plan, got: {}", t.plan_name);
    assert!(!t.execution_output.is_empty(),
        "Expected numeric output");
}

/// Trace 12: "word count"
///
/// Path: pre-Earley → token pair "word_count" matches plan file
///       → loads algorithms/text-processing/word_count.sexp
///       → execute: word_count("the quick brown fox")
///       → output: 4
#[test]
fn trace_word_count() {
    let t = trace_prompt("word count");
    print_trace(&t);

    assert!(t.plan_name.contains("word_count") || t.plan_name.contains("word"),
        "Expected word_count plan, got: {}", t.plan_name);
    assert!(t.execution_output.contains("4"),
        "Expected 4, got: {}", t.execution_output);
}

/// Trace 13: "find large files"
///
/// Path: Earley → IR(select, type=file, modifier=large)
///       → recipe or plan file match
///       → compile → Racket
#[test]
fn trace_find_large_files() {
    let t = trace_prompt("find large files");
    print_trace(&t);

    assert!(!t.plan_ops.is_empty(),
        "Expected some ops, got empty plan");
}

/// Trace 14: "euler totient"
///
/// Path: pre-Earley → "euler_totient" matches algorithm plan
///       → loads algorithms/number-theory/euler_totient.sexp
///       → execute → output: 4
#[test]
fn trace_euler_totient() {
    let t = trace_prompt("euler totient");
    print_trace(&t);

    assert!(t.plan_name.contains("euler_totient"),
        "Expected euler_totient plan, got: {}", t.plan_name);
    assert!(t.execution_output.contains("4"),
        "Expected 4, got: {}", t.execution_output);
}

/// Trace 15: "generate permutations"
///
/// Path: pre-Earley → "generate_permutations" matches plan
///       → execute → all orderings of (1 2 3)
#[test]
fn trace_permutations() {
    let t = trace_prompt("generate permutations");
    print_trace(&t);

    assert!(t.plan_name.contains("permutation"),
        "Expected permutations plan, got: {}", t.plan_name);
    assert!(t.execution_output.contains("1 2 3") && t.execution_output.contains("3 2 1"),
        "Expected all permutations, got: {}", t.execution_output);
}

// ═══════════════════════════════════════════════════════════════════════════
// Stage-level unit tests — verify each pipeline stage in isolation
// ═══════════════════════════════════════════════════════════════════════════

/// Verify normalization preserves paths and handles contractions.
#[test]
fn trace_stage_normalize() {
    let n = normalize::normalize("don't delete ~/My Files/report.pdf");
    assert!(n.tokens.contains(&"do".to_string()));
    assert!(n.tokens.contains(&"not".to_string()));
    assert!(n.tokens.iter().any(|t| t.contains("My Files") || t.contains("report")));
}

/// Verify typo correction fixes common misspellings.
#[test]
fn trace_stage_typo() {
    let dict = typo::domain_dict();
    let tokens: Vec<String> = vec!["findd", "fiels", "in", "tmp"]
        .into_iter().map(String::from).collect();
    let corrected = dict.correct_tokens(&tokens);
    assert!(corrected.contains(&"find".to_string()),
        "Expected 'findd' → 'find', got: {:?}", corrected);
}

/// Verify phrase tokenization groups multi-word verbs.
#[test]
fn trace_stage_phrase() {
    let tokens: Vec<String> = vec!["zip", "up", "everything"]
        .into_iter().map(String::from).collect();
    let phrased = phrase::phrase_tokenize(&tokens);
    // "zip up" should be grouped into "compress"
    assert!(phrased.len() < tokens.len() || phrased.iter().any(|t| t == "compress"),
        "Expected phrase grouping, got: {:?}", phrased);
}

/// Verify Earley parser produces parse trees for a simple command.
#[test]
fn trace_stage_earley() {
    let gram = grammar::build_command_grammar();
    let lex = lexicon::lexicon();
    let tokens: Vec<String> = vec!["find", "files", "in", "/tmp"]
        .into_iter().map(String::from).collect();
    let parses = earley::parse(&gram, &tokens, lex);
    assert!(!parses.is_empty(),
        "Expected at least one parse for 'find files in /tmp'");
}

/// Verify IntentIR extraction from parse trees.
#[test]
fn trace_stage_ir() {
    let gram = grammar::build_command_grammar();
    let lex = lexicon::lexicon();
    let tokens: Vec<String> = vec!["find", "pdfs", "in", "/tmp"]
        .into_iter().map(String::from).collect();
    let parses = earley::parse(&gram, &tokens, lex);
    assert!(!parses.is_empty());

    let ir = intent_ir::parse_trees_to_intents(&parses);
    assert!(ir.primary.is_some(),
        "Expected primary IR for 'find pdfs in /tmp'");
    let primary = ir.primary.unwrap();
    assert!(!primary.steps.is_empty(),
        "Expected at least one IR step");
    assert!(primary.steps[0].action == "select" || primary.steps[0].action == "traverse",
        "Expected select/traverse action, got: {}", primary.steps[0].action);
}

/// Verify plan compilation produces valid type chain.
#[test]
fn trace_stage_compile() {
    let plan_src = r#"
;; Test plan
(define (test-trace (path : Dir))
  (walk_tree)
  (find_matching :pattern "*.rs")
  (sort_by "name")
  (count))
"#;
    let plan = cadmus::sexpr::parse_sexpr_to_plan(plan_src).unwrap();
    let registry = cadmus::fs_types::build_full_registry();
    let compiled = plan::compile_plan(&plan, &registry).unwrap();

    assert_eq!(compiled.steps.len(), 4);
    assert_eq!(compiled.steps[0].op, "walk_tree");
    assert_eq!(compiled.steps[3].op, "count");
    // Final output should be Count
    assert!(compiled.steps[3].output_type.to_string().contains("Count"),
        "Expected Count output, got: {}", compiled.steps[3].output_type);
}

/// Verify Racket codegen produces valid script.
#[test]
fn trace_stage_codegen() {
    let plan_src = r#"
;; Test codegen
(define (test-codegen) : Number
  (add :x "3" :y "7"))
"#;
    let plan = cadmus::sexpr::parse_sexpr_to_plan(plan_src).unwrap();
    let registry = cadmus::fs_types::build_full_registry();
    let compiled = plan::compile_plan(&plan, &registry).unwrap();
    let racket_reg = racket_executor::build_racket_registry();
    let script = racket_executor::generate_racket_script(&compiled, &plan, &racket_reg).unwrap();

    assert!(script.contains("#lang racket"));
    assert!(script.contains("(+ 3 7)") || script.contains("(+"),
        "Expected addition in script, got:\n{}", script);
}
