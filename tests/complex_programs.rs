// Generate complex Racket programs through the racket_executor.
// Run with: cargo test --test complex_programs -- --nocapture

use cadmus::racket_strategy::{load_racket_facts_from_str,
    promote_inferred_ops,
};
use cadmus::racket_executor::generate_racket_script;
use cadmus::registry::load_ops_pack_str;
use cadmus::workflow::{CompiledStep, CompiledWorkflow, WorkflowDef};
use cadmus::type_expr::TypeExpr;
use std::collections::HashMap;

const RACKET_FACTS_YAML: &str = include_str!("../data/packs/facts/racket.facts.yaml");
const RACKET_OPS_YAML: &str = include_str!("../data/packs/ops/racket.ops.yaml");

fn make_registry() -> cadmus::registry::OperationRegistry {
    let mut reg = load_ops_pack_str(RACKET_OPS_YAML).unwrap();
    let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();
    promote_inferred_ops(&mut reg, &facts);
    reg
}

fn make_step(index: usize, op: &str, params: Vec<(&str, &str)>) -> CompiledStep {
    CompiledStep {
        index,
        op: op.to_string(),
        input_type: TypeExpr::prim("Any"),
        output_type: TypeExpr::prim("Any"),
        params: params.into_iter().map(|(k, v)| (k.to_string(), v.to_string())).collect(),
        ..Default::default()
    }
}

fn make_workflow(name: &str, inputs: Vec<(&str, &str)>, steps: Vec<CompiledStep>) -> (CompiledWorkflow, WorkflowDef) {
    let input_map: HashMap<String, String> = inputs.iter()
        .map(|(k, v)| (k.to_string(), v.to_string()))
        .collect();
    let compiled = CompiledWorkflow {
        name: name.to_string(),
        input_type: TypeExpr::prim("Any"),
        input_description: inputs.first().map(|(_, v)| v.to_string()).unwrap_or_default(),
        steps,
        output_type: TypeExpr::prim("Any"),
    };
    let def = WorkflowDef {
        workflow: name.to_string(),
        inputs: input_map,
        steps: vec![],  // not used by generate_racket_script
    };
    (compiled, def)
}

fn print_program(title: &str, nl_prompt: &str, description: &str, script: &str) {
    println!("╔══════════════════════════════════════════════════════════════╗");
    println!("║  {}",  format!("{:61}║", title));
    println!("╚══════════════════════════════════════════════════════════════╝");
    println!();
    println!("  NL prompt:  \"{}\"", nl_prompt);
    println!("  What it does: {}", description);
    println!();
    for line in script.lines() {
        println!("  {}", line);
    }
    println!();
    println!();
}

#[test]
fn generate_complex_programs() {
    let reg = make_registry();

    // ── Program 1: Sort and reverse a list ──
    {
        let steps = vec![
            make_step(0, "sort_list", vec![("value", "'(5 3 1 4 2)"), ("comparator", "<")]),
            make_step(1, "list_reverse", vec![]),  // chains from prev
        ];
        let (compiled, def) = make_workflow(
            "Sort descending",
            vec![("lst", "'(5 3 1 4 2)")],
            steps,
        );
        let script = generate_racket_script(&compiled, &def, &reg).unwrap();
        print_program(
            "Program 1: Sort Descending",
            "Sort the list 5 3 1 4 2 in descending order",
            "Sorts ascending with <, then reverses → descending.\n                list_reverse is DISCOVERED (inferred from cdr).",
            &script,
        );
    }

    // ── Program 2: Filter evens, count them ──
    {
        let steps = vec![
            make_step(0, "racket_filter", vec![("value", "'(1 2 3 4 5 6 7 8 9 10)"), ("predicate", "even?")]),
            make_step(1, "length", vec![]),
        ];
        let (compiled, def) = make_workflow(
            "Count even numbers",
            vec![("lst", "'(1 2 3 4 5 6 7 8 9 10)")],
            steps,
        );
        let script = generate_racket_script(&compiled, &def, &reg).unwrap();
        print_program(
            "Program 2: Count Even Numbers",
            "How many even numbers are in 1 through 10?",
            "Filters with even?, then counts with length.",
            &script,
        );
    }

    // ── Program 3: Map, fold — sum of squares ──
    {
        let steps = vec![
            make_step(0, "racket_map", vec![("value", "'(1 2 3 4 5)"), ("function", "(lambda (x) (* x x))")]),
            make_step(1, "racket_foldl", vec![("function", "+"), ("init", "0")]),
        ];
        let (compiled, def) = make_workflow(
            "Sum of squares",
            vec![("lst", "'(1 2 3 4 5)")],
            steps,
        );
        let script = generate_racket_script(&compiled, &def, &reg).unwrap();
        print_program(
            "Program 3: Sum of Squares",
            "Compute the sum of squares of 1 through 5",
            "Maps x² over the list, then folds with + to sum.\n                multiply (*) is DISCOVERED (inferred from add).",
            &script,
        );
    }

    // ── Program 4: Build a set from a list, count unique elements ──
    {
        let steps = vec![
            make_step(0, "set_new", vec![("value", "'(1 2 2 3 3 3 4 4 4 4)")]),
            make_step(1, "set_count", vec![]),
        ];
        let (compiled, def) = make_workflow(
            "Count unique elements",
            vec![("lst", "'(1 2 2 3 3 3 4 4 4 4)")],
            steps,
        );
        let script = generate_racket_script(&compiled, &def, &reg).unwrap();
        print_program(
            "Program 4: Count Unique Elements",
            "How many unique values in 1 2 2 3 3 3 4 4 4 4?",
            "Converts list to set (deduplicates), then counts.",
            &script,
        );
    }

    // ── Program 5: String processing — build a greeting ──
    {
        let steps = vec![
            make_step(0, "string_append", vec![("x", "Hello, "), ("y", "World")]),
            make_step(1, "string_length", vec![]),
        ];
        let (compiled, def) = make_workflow(
            "Greeting length",
            vec![("a", "Hello, "), ("b", "World")],
            steps,
        );
        let script = generate_racket_script(&compiled, &def, &reg).unwrap();
        print_program(
            "Program 5: Greeting Length",
            "How long is 'Hello, World'?",
            "Appends two strings, then measures the length.",
            &script,
        );
    }

    // ── Program 6: Remove duplicates, sort, take first ──
    {
        let steps = vec![
            make_step(0, "set_new", vec![("value", "'(9 1 5 1 9 3 5 7)")]),
            make_step(1, "set_to_list", vec![]),
            make_step(2, "sort_list", vec![("comparator", "<")]),
            make_step(3, "car", vec![]),
        ];
        let (compiled, def) = make_workflow(
            "Find minimum",
            vec![("lst", "'(9 1 5 1 9 3 5 7)")],
            steps,
        );
        let script = generate_racket_script(&compiled, &def, &reg).unwrap();
        print_program(
            "Program 6: Find Minimum (4 steps)",
            "Find the smallest unique value in 9 1 5 1 9 3 5 7",
            "Dedup via set → back to list → sort ascending → take first.",
            &script,
        );
    }

    // ── Program 7: Flatten nested list, remove element, get length ──
    {
        let steps = vec![
            make_step(0, "flatten", vec![("value", "'((1 2) (3 4) (5 6))")]),
            make_step(1, "remove", vec![("x", "3")]),
            make_step(2, "length", vec![]),
        ];
        let (compiled, def) = make_workflow(
            "Flatten and count without 3",
            vec![("lst", "'((1 2) (3 4) (5 6))")],
            steps,
        );
        let script = generate_racket_script(&compiled, &def, &reg).unwrap();
        print_program(
            "Program 7: Flatten, Remove, Count",
            "Flatten nested lists, remove 3, how many left?",
            "Flattens → removes 3 → counts remaining.\n                remove is DISCOVERED (inferred from cons).",
            &script,
        );
    }

    // ── Program 8: Arithmetic chain — all discovered ops ──
    {
        let steps = vec![
            make_step(0, "multiply", vec![("x", "6"), ("y", "7")]),
            make_step(1, "subtract", vec![("y", "2")]),
            make_step(2, "divide", vec![("y", "5")]),
            make_step(3, "expt", vec![("y", "2")]),
        ];
        let (compiled, def) = make_workflow(
            "Arithmetic chain",
            vec![("x", "6"), ("y", "7")],
            steps,
        );
        let script = generate_racket_script(&compiled, &def, &reg).unwrap();
        print_program(
            "Program 8: Arithmetic Chain (all discovered)",
            "Take 6 times 7, subtract 2, divide by 5, square it",
            "multiply→subtract→divide→expt. First 3 ops are ALL\n                DISCOVERED from the fact pack, not in racket.ops.yaml.",
            &script,
        );
    }

    // ── Program 9: Check if all elements satisfy a predicate ──
    {
        let steps = vec![
            make_step(0, "racket_filter", vec![("value", "'(2 4 6 8 10 11)"), ("predicate", "even?")]),
            make_step(1, "length", vec![]),
        ];
        let (compiled, def) = make_workflow(
            "Count evens in mixed list",
            vec![("lst", "'(2 4 6 8 10 11)")],
            steps,
        );
        let script = generate_racket_script(&compiled, &def, &reg).unwrap();
        print_program(
            "Program 9: Filter and Count",
            "How many even numbers in 2 4 6 8 10 11?",
            "Filters evens, counts survivors. (11 is odd → dropped)",
            &script,
        );
    }

    // ── Program 10: Set operations — union and count ──
    {
        let steps = vec![
            make_step(0, "set_new", vec![("value", "'(1 2 3)")]),
            make_step(1, "set_union", vec![("y", "(list->set '(3 4 5))")]),
            make_step(2, "set_count", vec![]),
        ];
        let (compiled, def) = make_workflow(
            "Union two sets",
            vec![("a", "'(1 2 3)"), ("b", "'(3 4 5)")],
            steps,
        );
        let script = generate_racket_script(&compiled, &def, &reg).unwrap();
        print_program(
            "Program 10: Set Union",
            "How many unique values in {1,2,3} ∪ {3,4,5}?",
            "Builds set A, unions with set B, counts. Answer: 5.",
            &script,
        );
    }

    // ── Program 11: Less-than — comparison anchor ──
    {
        let steps = vec![
            make_step(0, "less_than", vec![("x", "3"), ("y", "5")]),
        ];
        let (compiled, def) = make_workflow(
            "Simple comparison",
            vec![("x", "3"), ("y", "5")],
            steps,
        );
        let script = generate_racket_script(&compiled, &def, &reg).unwrap();
        print_program(
            "Program 11: Less-Than (anchor)",
            "Is 3 less than 5?",
            "less_than is the comparison ANCHOR with a metasig.",
            &script,
        );
    }

    // ── Program 12: Greater-than — discovered via op-symmetric ──
    {
        let steps = vec![
            make_step(0, "greater_than", vec![("x", "10"), ("y", "7")]),
        ];
        let (compiled, def) = make_workflow(
            "Greater than check",
            vec![("x", "10"), ("y", "7")],
            steps,
        );
        let script = generate_racket_script(&compiled, &def, &reg).unwrap();
        print_program(
            "Program 12: Greater-Than (discovered, op-symmetric)",
            "Is 10 greater than 7?",
            "greater_than is DISCOVERED via op-symmetric from less_than.\n                Not in racket.ops.yaml.",
            &script,
        );
    }

    // ── Program 13: Less-than-or-equal — discovered via type-symmetric ──
    {
        let steps = vec![
            make_step(0, "less_than_or_equal", vec![("x", "5"), ("y", "5")]),
        ];
        let (compiled, def) = make_workflow(
            "LTE check",
            vec![("x", "5"), ("y", "5")],
            steps,
        );
        let script = generate_racket_script(&compiled, &def, &reg).unwrap();
        print_program(
            "Program 13: Less-Than-Or-Equal (discovered, type-symmetric)",
            "Is 5 ≤ 5?",
            "less_than_or_equal is DISCOVERED via type-symmetric from\n                less_than (class: comparison_binop). Brand new op!",
            &script,
        );
    }

    // ── Program 14: String upcase — string anchor ──
    {
        let steps = vec![
            make_step(0, "string_upcase", vec![("value", "hello world")]),
        ];
        let (compiled, def) = make_workflow(
            "Uppercase string",
            vec![("s", "hello world")],
            steps,
        );
        let script = generate_racket_script(&compiled, &def, &reg).unwrap();
        print_program(
            "Program 14: String Upcase (anchor)",
            "Convert 'hello world' to uppercase",
            "string_upcase is the string ANCHOR with a metasig.",
            &script,
        );
    }

    // ── Program 15: String downcase + length — cross-domain chain ──
    {
        let steps = vec![
            make_step(0, "string_downcase", vec![("value", "HELLO WORLD")]),
            make_step(1, "string_length", vec![]),
        ];
        let (compiled, def) = make_workflow(
            "Downcase and measure",
            vec![("s", "HELLO WORLD")],
            steps,
        );
        let script = generate_racket_script(&compiled, &def, &reg).unwrap();
        print_program(
            "Program 15: String Downcase + Length (discovered → ops-pack)",
            "How long is 'HELLO WORLD' in lowercase?",
            "string_downcase is DISCOVERED (type-symmetric from string_upcase),\n                then chains to string_length (in ops pack). Cross-domain!",
            &script,
        );
    }

    // ── Program 16: Multi-domain — arithmetic + string + upcase ──
    {
        let steps = vec![
            make_step(0, "multiply", vec![("x", "6"), ("y", "7")]),
            make_step(1, "number_to_string", vec![]),
            make_step(2, "string_append", vec![("x", "The answer is: ")]),
            make_step(3, "string_upcase", vec![]),
        ];
        let (compiled, def) = make_workflow(
            "Multi-domain chain",
            vec![("x", "6"), ("y", "7")],
            steps,
        );
        let script = generate_racket_script(&compiled, &def, &reg).unwrap();
        print_program(
            "Program 16: Multi-Domain (arithmetic → string → upcase)",
            "Multiply 6 by 7, format as 'THE ANSWER IS: 42'",
            "multiply (DISCOVERED arithmetic) → number_to_string (ops pack)\n                → string_append (ops pack) → string_upcase (anchor).\n                Three domains, two discovered ops.",
            &script,
        );
    }

    // ════════════════════════════════════════════════════════════════════
    // FILE I/O PROGRAMS
    // ════════════════════════════════════════════════════════════════════

    // ── Program 17: Read a file and count its length ──
    {
        let steps = vec![
            make_step(0, "file_read", vec![("value", "input.txt")]),
            make_step(1, "string_length", vec![]),
        ];
        let (compiled, def) = make_workflow(
            "File size in chars",
            vec![("path", "input.txt")],
            steps,
        );
        let script = generate_racket_script(&compiled, &def, &reg).unwrap();
        print_program(
            "Program 17: Read File → String Length",
            "How many characters are in input.txt?",
            "Reads entire file as string, then measures length.",
            &script,
        );
    }

    // ── Program 18: Read file, uppercase it, write to new file ──
    {
        let steps = vec![
            make_step(0, "file_read", vec![("value", "input.txt")]),
            make_step(1, "string_upcase", vec![]),
            make_step(2, "file_write", vec![("y", "output.txt")]),
        ];
        let (compiled, def) = make_workflow(
            "Uppercase file contents",
            vec![("src", "input.txt"), ("dst", "output.txt")],
            steps,
        );
        let script = generate_racket_script(&compiled, &def, &reg).unwrap();
        print_program(
            "Program 18: Read → Upcase → Write",
            "Read input.txt, convert to uppercase, save as output.txt",
            "file_read → string_upcase (ANCHOR) → file_write.\n                Full file transformation pipeline.",
            &script,
        );
    }

    // ── Program 19: Read file as lines, count lines ──
    {
        let steps = vec![
            make_step(0, "file_read_lines", vec![("value", "data.csv")]),
            make_step(1, "length", vec![]),
        ];
        let (compiled, def) = make_workflow(
            "Count lines in file",
            vec![("path", "data.csv")],
            steps,
        );
        let script = generate_racket_script(&compiled, &def, &reg).unwrap();
        print_program(
            "Program 19: Read Lines → Count",
            "How many lines are in data.csv?",
            "Reads file as list of lines, then counts with length.",
            &script,
        );
    }

    // ── Program 20: Read lines, filter, count ──
    {
        let steps = vec![
            make_step(0, "file_read_lines", vec![("value", "log.txt")]),
            make_step(1, "racket_filter", vec![("predicate", "(lambda (line) (string-contains? line \"ERROR\"))")]),
            make_step(2, "length", vec![]),
        ];
        let (compiled, def) = make_workflow(
            "Count error lines",
            vec![("path", "log.txt")],
            steps,
        );
        let script = generate_racket_script(&compiled, &def, &reg).unwrap();
        print_program(
            "Program 20: Read Log → Filter ERRORs → Count",
            "How many ERROR lines are in log.txt?",
            "Reads file as lines, filters for ERROR, counts matches.\n                Combines file I/O with higher-order ops.",
            &script,
        );
    }

    // ── Program 21: Read file, transform, write — full ETL ──
    {
        let steps = vec![
            make_step(0, "file_read_lines", vec![("value", "names.txt")]),
            make_step(1, "racket_map", vec![("function", "string-upcase")]),
            make_step(2, "racket_foldl", vec![("function", "(lambda (line acc) (string-append acc line \"\\n\"))"), ("init", "\"\"")]),
            make_step(3, "file_write", vec![("y", "names_upper.txt")]),
        ];
        let (compiled, def) = make_workflow(
            "Uppercase all names",
            vec![("src", "names.txt"), ("dst", "names_upper.txt")],
            steps,
        );
        let script = generate_racket_script(&compiled, &def, &reg).unwrap();
        print_program(
            "Program 21: File ETL — Read Lines → Map Upcase → Fold → Write",
            "Read names.txt, uppercase every line, save to names_upper.txt",
            "Full ETL pipeline: file→lines → map string-upcase →\n                fold into single string → write to file.\n                4 steps across file I/O + higher-order + string domains.",
            &script,
        );
    }

    // ── Program 22: Check file exists, read, get length ──
    {
        let steps = vec![
            make_step(0, "file_read", vec![("value", "config.yaml")]),
            make_step(1, "string_length", vec![]),
            make_step(2, "number_to_string", vec![]),
            make_step(3, "string_append", vec![("x", "Config size: ")]),
        ];
        let (compiled, def) = make_workflow(
            "Report config size",
            vec![("path", "config.yaml")],
            steps,
        );
        let script = generate_racket_script(&compiled, &def, &reg).unwrap();
        print_program(
            "Program 22: Read Config → Measure → Format Message",
            "How big is config.yaml? Format as 'Config size: N'",
            "file_read → string_length → number_to_string → string_append.\n                File I/O → string → arithmetic → string. Four domains!",
            &script,
        );
    }
}
