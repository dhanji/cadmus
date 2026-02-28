// ---------------------------------------------------------------------------
// Tests for auto-approve mode and timing helpers
// ---------------------------------------------------------------------------

use std::time::Duration;

use cadmus::ui;
use cadmus::nl;
use cadmus::nl::dialogue::DialogueState;

// ---------------------------------------------------------------------------
// format_duration tests
// ---------------------------------------------------------------------------

#[test]
fn test_format_duration_zero() {
    assert_eq!(ui::format_duration(Duration::ZERO), "0ms");
}

#[test]
fn test_format_duration_sub_second() {
    assert_eq!(ui::format_duration(Duration::from_millis(142)), "142ms");
    assert_eq!(ui::format_duration(Duration::from_millis(1)), "1ms");
    assert_eq!(ui::format_duration(Duration::from_millis(999)), "999ms");
}

#[test]
fn test_format_duration_one_second_boundary() {
    // 1000ms should show as seconds
    assert_eq!(ui::format_duration(Duration::from_millis(1000)), "1.0s");
}

#[test]
fn test_format_duration_seconds() {
    assert_eq!(ui::format_duration(Duration::from_millis(1500)), "1.5s");
    assert_eq!(ui::format_duration(Duration::from_millis(2300)), "2.3s");
    assert_eq!(ui::format_duration(Duration::from_secs(59)), "59.0s");
}

#[test]
fn test_format_duration_minutes() {
    assert_eq!(ui::format_duration(Duration::from_secs(60)), "1m 0.0s");
    assert_eq!(ui::format_duration(Duration::from_secs(65)), "1m 5.0s");
    assert_eq!(ui::format_duration(Duration::from_millis(90_200)), "1m 30.2s");
}

#[test]
fn test_format_duration_sub_millisecond() {
    // Microseconds round down to 0ms
    assert_eq!(ui::format_duration(Duration::from_micros(500)), "0ms");
}

// ---------------------------------------------------------------------------
// timing() output tests
// ---------------------------------------------------------------------------

#[test]
fn test_timing_contains_label_and_duration() {
    let output = ui::timing("thinking", Duration::from_millis(142));
    assert!(output.contains("thinking"), "should contain label: {}", output);
    assert!(output.contains("142ms"), "should contain formatted duration: {}", output);
}

#[test]
fn test_timing_execution_label() {
    let output = ui::timing("execution", Duration::from_millis(2500));
    assert!(output.contains("execution"), "should contain label: {}", output);
    assert!(output.contains("2.5s"), "should contain formatted duration: {}", output);
}

// ---------------------------------------------------------------------------
// Auto-approve NL flow tests
// ---------------------------------------------------------------------------

/// In auto mode, the plan is created and then immediately approved.
/// We simulate this by: (1) creating a plan, (2) taking it from state (as auto does).
#[test]
fn test_auto_approve_plan_available_after_create() {
    let mut state = DialogueState::new();
    let response = nl::process_input("find all PDFs in ~/Documents", &mut state);
    assert!(
        matches!(response, nl::NlResponse::PlanCreated { .. }),
        "should create plan, got: {:?}", response
    );
    // In auto mode, we take the plan from state
    assert!(state.current_plan.is_some(), "plan should be in dialogue state after PlanCreated");
    let plan_def = state.current_plan.take().unwrap();
    assert!(!plan_def.steps.is_empty(), "plan should have steps");
}

/// Codegen should succeed for the auto-approved plan.
#[test]
fn test_auto_approve_codegen_succeeds() {
    use cadmus::calling_frame::{CallingFrame, DefaultFrame};

    let mut state = DialogueState::new();
    let response = nl::process_input("list everything in ~/Downloads", &mut state);
    assert!(
        matches!(response, nl::NlResponse::PlanCreated { .. }),
        "should create plan, got: {:?}", response
    );
    let plan_def = state.current_plan.take().unwrap();
    let frame = DefaultFrame::from_plan(&plan_def);
    let script = frame.codegen(&plan_def);
    assert!(script.is_ok(), "codegen should succeed: {:?}", script.err());
    let script = script.unwrap();
    assert!(script.contains("#lang racket"), "should produce Racket script");
}

/// Non-plan responses (clarification) should not have a plan to auto-approve.
#[test]
fn test_auto_approve_no_plan_for_clarification() {
    let mut state = DialogueState::new();
    let response = nl::process_input("do the thing", &mut state);
    assert!(
        matches!(response, nl::NlResponse::NeedsClarification { .. }),
        "ambiguous input should need clarification, got: {:?}", response
    );
    assert!(state.current_plan.is_none(), "no plan should exist after clarification");
}

/// Error responses should not have a plan to auto-approve.
#[test]
fn test_auto_approve_no_plan_for_error() {
    let mut state = DialogueState::new();
    // Approve without a plan
    let response = nl::process_input("approve", &mut state);
    assert!(
        matches!(response, nl::NlResponse::NeedsClarification { .. }),
        "approve without plan should clarify, got: {:?}", response
    );
    assert!(state.current_plan.is_none(), "no plan should exist");
}
