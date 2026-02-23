// Trace the NL → Racket pipeline for arithmetic inputs.
// Run with: cargo test --test trace_pipeline -- --nocapture
//
// NOTE: This test is a diagnostic trace, not a pass/fail test.
// It traces the full pipeline for arithmetic inputs through the
// Earley parser (the sole plan creation path).

use cadmus::nl::dialogue::DialogueState;
use cadmus::nl::{self, NlResponse};

#[test]
fn trace_all_seven_inputs() {
    let inputs = [
        "Add 4 and 35 together",
        "Subtract 2 from 6",
        "Multiply 7 and 8",
        "Divide 100 by 4",
        "Sum 12 and 88",
        "Plus 1 and 99",
        "Minus 5 from 20",
    ];

    for input in &inputs {
        println!("\n=== Input: {:?} ===", input);
        let mut state = DialogueState::new();
        let response = nl::process_input(input, &mut state);
        match &response {
            NlResponse::PlanCreated { plan_yaml, .. } => {
                println!("  PlanCreated:");
                for line in plan_yaml.lines() {
                    println!("    {}", line);
                }
            }
            NlResponse::NeedsClarification { needs } => {
                println!("  NeedsClarification: {:?}", needs);
            }
            NlResponse::Error { message } => {
                println!("  Error: {}", message);
            }
            other => {
                println!("  Other: {:?}", other);
            }
        }
    }
}
