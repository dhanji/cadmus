//! Natural Language UX layer.
//!
//! A deterministic, low-latency adapter that converts chatty user input
//! into structured plan YAML / Goal DSL instructions for Cadmus.
//! Operates in four stages:
//!
//! 1. **Normalization** — case fold, punctuation strip, ordinal canonicalization,
//!    synonym mapping (`normalize`)
//! 2. **Typo correction** — domain-bounded SymSpell dictionary (`typo`)
//! 3. **Intent recognition** — closed grammar over canonical edit instructions (`intent`)
//! 4. **Slot extraction** — targets, anchors, parameters, modifiers (`slots`)
//!
//! Plus dialogue state management (`dialogue`) for multi-turn conversations.

pub mod normalize;
pub mod typo;
pub mod intent;
pub mod slots;
pub mod dialogue;
pub mod vocab;
pub mod earley;
pub mod grammar;
pub mod lexicon;
pub mod intent_ir;

use dialogue::{DialogueState, DialogueError, FocusEntry};
use intent::Intent;
use intent::EditAction;
use slots::ExtractedSlots;

// ---------------------------------------------------------------------------
// NlResponse — the output of the NL UX layer
// ---------------------------------------------------------------------------

/// The response from processing a user input through the NL layer.
#[derive(Debug, Clone)]
pub enum NlResponse {
    /// A new plan plan was created.
    PlanCreated {
        /// The plan YAML string.
        plan_yaml: String,
        /// Human-readable summary of what the plan does.
        summary: String,
        /// The prompt to show the user.
        prompt: String,
    },
    /// An existing plan plan was edited.
    PlanEdited {
        /// The revised plan YAML string.
        plan_yaml: String,
        /// Description of what changed.
        diff_description: String,
        /// The prompt to show the user.
        prompt: String,
    },
    /// An explanation of an operation or concept.
    Explanation {
        /// The explanation text.
        text: String,
    },
    /// The user approved the current plan.
    Approved {
        /// The generated shell script (if plan was compiled successfully).
        script: Option<String>,
    },
    /// The user rejected the current plan.
    Rejected,
    /// The input was ambiguous — we need clarification.
    NeedsClarification {
        /// What we need to know.
        needs: Vec<String>,
    },
    /// A parameter was set.
    ParamSet {
        /// Description of what was set.
        description: String,
        /// The revised plan YAML (if a plan exists).
        plan_yaml: Option<String>,
    },
    /// An error occurred.
    Error {
        /// Error message.
        message: String,
    },
}

// ---------------------------------------------------------------------------
// Public API — the main entry point
// ---------------------------------------------------------------------------

/// Process a user input through the full NL pipeline.
///
/// Pipeline: normalize → typo_correct → parse_intent → extract_slots
/// → resolve_references → execute.
///
/// This is the single entry point for the NL UX layer.
pub fn process_input(input: &str, state: &mut DialogueState) -> NlResponse {
    state.next_turn();

    // Pipeline: normalize → typo correct → re-normalize (for synonyms) → intent → slots
    //
    // 1. First normalize: case fold, strip punctuation, expand contractions, ordinals
    let first_pass = normalize::normalize(input);

    // 2. Typo correction on the raw tokens (before synonym mapping)
    let dict = typo::domain_dict();
    let corrected_tokens = dict.correct_tokens(&first_pass.tokens);

    // 3. Re-normalize the corrected tokens to apply synonym mapping
    //    We join with spaces, preserving path tokens that have original case
    //    Re-quote tokens that contain spaces (they came from quoted input)
    let corrected_input = corrected_tokens.iter()
        .map(|t| if t.contains(' ') { format!("\"{}\"", t) } else { t.clone() })
        .collect::<Vec<_>>()
        .join(" ");
    let normalized = normalize::normalize(&corrected_input);

    // 4. Parse intent from the fully normalized+corrected tokens
    let intent_result = intent::parse_intent(&normalized);

    // 5. Extract slots from canonical tokens
    let extracted = slots::extract_slots(&normalized.canonical_tokens);

    // 5. Update focus stack with mentioned ops/paths
    update_focus(state, &extracted);

    // 6. Store last intent
    state.last_intent = Some(intent_result.clone());

    // 7. Dispatch based on intent
    match intent_result {
        Intent::CreatePlan { op, rest: _ } => {
            handle_create_plan(op, &extracted, state)
        }
        Intent::EditStep { action, rest: _ } => {
            handle_edit_step(action, &extracted, state)
        }
        Intent::ExplainOp { subject, rest: _ } => {
            handle_explain(&subject)
        }
        Intent::Approve => {
            if let Some(wf) = state.current_plan.take() {
                // Generate a Racket program from the plan.
                // Use a full registry that includes shell ops so that
                // subsumed fs_ops can route through extract_shell_meta().
                let script = {
                    let registry = crate::fs_types::build_full_registry();
                    match crate::plan::compile_plan(&wf, &registry) {
                        Ok(compiled) => {
                            // Build a Racket registry with shell ops included.
                            // This runs the same inference pipeline as build_full_registry
                            // but for the Racket-specific ops.
                            let mut racket_reg = crate::registry::load_ops_pack_str(
                                include_str!("../../data/packs/ops/racket.ops.yaml")
                            ).unwrap_or_default();
                            let racket_facts_yaml = include_str!("../../data/packs/facts/racket.facts.yaml");
                            if let Ok(facts) = crate::racket_strategy::load_racket_facts_from_str(
                                racket_facts_yaml
                            ) {
                                crate::racket_strategy::promote_inferred_ops(&mut racket_reg, &facts);

                                // Also discover shell submodes so extract_shell_meta()
                                // works for subsumed fs_ops (Phase 2 type lowering).
                                let cli_yaml = include_str!("../../data/packs/facts/macos_cli.facts.yaml");
                                if let Ok(cli_pack) = serde_yaml::from_str::<crate::fact_pack::FactPack>(cli_yaml) {
                                    let cli_facts = crate::fact_pack::FactPackIndex::build(cli_pack);
                                    crate::racket_strategy::discover_shell_submodes(
                                        &mut racket_reg, &facts, &cli_facts,
                                    );
                                }
                            }
                            match crate::racket_executor::generate_racket_script(&compiled, &wf, &racket_reg) {
                                Ok(s) => Some(s),
                                Err(_) => None,
                            }
                        }
                        Err(_) => None,
                    }
                };
                NlResponse::Approved { script }
            } else {
                NlResponse::NeedsClarification {
                    needs: vec![
                        "There's nothing to approve yet.".to_string(),
                        "Try creating a plan first, like 'zip up ~/Downloads'.".to_string(),
                    ],
                }
            }
        }
        Intent::Reject => {
            state.current_plan = None;
            NlResponse::Rejected
        }
        Intent::AskQuestion { tokens } => {
            handle_question(&tokens)
        }
        Intent::SetParam { param, value, rest: _ } => {
            handle_set_param(param, value, &extracted, state)
        }
        Intent::NeedsClarification { needs } => {
            NlResponse::NeedsClarification { needs }
        }
    }
}

// ---------------------------------------------------------------------------
// Intent handlers
// ---------------------------------------------------------------------------

fn handle_create_plan(
    op: Option<String>,
    slots: &ExtractedSlots,
    state: &mut DialogueState,
) -> NlResponse {
    match dialogue::build_plan(&op, slots, None) {
        Ok(wf) => {
            let yaml = dialogue::plan_to_yaml(&wf);

            // Validate: try to compile through the engine
            match validate_plan_yaml(&yaml) {
                Ok(()) => {
                    let summary = format_summary(&wf);
                    let prompt = format!("{}\n\n{}\n\n{}",
                        casual_ack(state.turn_count),
                        yaml,
                        "Approve? Or edit plan?"
                    );

                    state.current_plan = Some(wf);
                    state.focus.push(FocusEntry::WholePlan);

                    NlResponse::PlanCreated {
                        plan_yaml: yaml,
                        summary,
                        prompt,
                    }
                }
                Err(e) => NlResponse::Error {
                    message: format!("Generated plan failed validation: {}", e),
                },
            }
        }
        Err(DialogueError::CannotBuild(msg)) => {
            NlResponse::NeedsClarification {
                needs: vec![msg],
            }
        }
        Err(e) => NlResponse::Error {
            message: format!("{}", e),
        },
    }
}

fn handle_edit_step(
    action: EditAction,
    slots: &ExtractedSlots,
    state: &mut DialogueState,
) -> NlResponse {
    let wf = match &state.current_plan {
        Some(wf) => wf.clone(),
        None => {
            return NlResponse::NeedsClarification {
                needs: vec![
                    "There's no current plan to edit.".to_string(),
                    "Try creating one first, like 'zip up ~/Downloads'.".to_string(),
                ],
            };
        }
    };

    match dialogue::apply_edit(&wf, &action, slots, state) {
        Ok((edited_wf, diff_desc)) => {
            let yaml = dialogue::plan_to_yaml(&edited_wf);

            match validate_plan_yaml(&yaml) {
                Ok(()) => {
                    let prompt = format!("{}\n\n{}\n\nApprove?",
                        diff_desc,
                        yaml,
                    );

                    state.current_plan = Some(edited_wf);

                    NlResponse::PlanEdited {
                        plan_yaml: yaml,
                        diff_description: diff_desc,
                        prompt,
                    }
                }
                Err(e) => NlResponse::Error {
                    message: format!("Edited plan failed validation: {}", e),
                },
            }
        }
        Err(DialogueError::NeedsContext(msg)) => {
            NlResponse::NeedsClarification {
                needs: vec![msg],
            }
        }
        Err(DialogueError::InvalidTarget(msg)) => {
            NlResponse::NeedsClarification {
                needs: vec![msg],
            }
        }
        Err(e) => NlResponse::Error {
            message: format!("{}", e),
        },
    }
}

fn handle_explain(subject: &str) -> NlResponse {
    let text = get_op_explanation(subject);
    NlResponse::Explanation { text }
}

fn handle_question(tokens: &[String]) -> NlResponse {
    // Check if any token is an op name — if so, explain it
    for token in tokens {
        if normalize::is_canonical_op(token) {
            return NlResponse::Explanation {
                text: get_op_explanation(token),
            };
        }
    }

    NlResponse::NeedsClarification {
        needs: vec![
            "I'm not sure how to answer that.".to_string(),
            "Try asking about a specific operation, like 'what does walk_tree mean?'".to_string(),
        ],
    }
}

fn handle_set_param(
    param: Option<String>,
    value: Option<String>,
    _slots: &ExtractedSlots,
    state: &mut DialogueState,
) -> NlResponse {
    let wf = match &state.current_plan {
        Some(wf) => wf.clone(),
        None => {
            return NlResponse::NeedsClarification {
                needs: vec!["There's no current plan to modify.".to_string()],
            };
        }
    };

    let desc = match (&param, &value) {
        (Some(p), Some(v)) => format!("Set {} to {}.", p, v),
        _ => "Parameter updated.".to_string(),
    };

    // For now, just acknowledge — full param editing would modify plan inputs
    if let (Some(p), Some(v)) = (param, value) {
        let mut edited = wf;
        // Update existing input or add new one
        if let Some(existing) = edited.inputs.iter_mut().find(|i| i.name == *p) {
            existing.type_hint = Some(v.clone());
        } else {
            edited.inputs.push(crate::plan::PlanInput::bare(p.clone()));
        }
        let yaml = dialogue::plan_to_yaml(&edited);
        state.current_plan = Some(edited);

        NlResponse::ParamSet {
            description: desc,
            plan_yaml: Some(yaml),
        }
    } else {
        NlResponse::NeedsClarification {
            needs: vec!["What parameter and value? Try 'set <param> to <value>'.".to_string()],
        }
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Update the focus stack based on extracted slots.
fn update_focus(state: &mut DialogueState, slots: &ExtractedSlots) {
    if let Some(op) = &slots.primary_op {
        state.focus.push(FocusEntry::MentionedOp { op: op.clone() });
    }
    if let Some(path) = &slots.target_path {
        state.focus.push(FocusEntry::Artifact { path: path.clone() });
    }
}

/// Validate a plan YAML string by parsing and compiling it.
fn validate_plan_yaml(yaml: &str) -> Result<(), String> {
    let parsed = crate::plan::parse_plan(yaml)
        .map_err(|e| format!("Parse error: {}", e))?;
    let registry = crate::fs_types::build_full_registry();
    crate::plan::compile_plan(&parsed, &registry)
        .map_err(|e| format!("Compile error: {}", e))?;
    Ok(())
}

/// Generate a casual acknowledgment phrase, varying by turn count.
fn casual_ack(turn: usize) -> &'static str {
    const ACKS: &[&str] = &[
        "Right on it!",
        "Here's what I've got:",
        "Sure thing!",
        "Coming right up!",
        "On it!",
        "Here you go:",
        "Let's do this!",
        "Got it!",
        "Alright, here's the plan:",
        "No problem!",
    ];
    ACKS[turn % ACKS.len()]
}

/// Generate a human-readable summary of a plan.
fn format_summary(wf: &crate::plan::PlanDef) -> String {
    let steps_desc: Vec<String> = wf.steps.iter()
        .map(|s| s.op.replace('_', " "))
        .collect();

    let path = wf.get_input("path")
        .and_then(|i| i.type_hint.as_deref())
        .unwrap_or(".");

    format!(
        "Plan: {} — {} step(s) operating on {}:\n  {}",
        wf.name,
        wf.steps.len(),
        path,
        steps_desc.join(" → "),
    )
}

/// Get an explanation for an operation.
fn get_op_explanation(op: &str) -> String {
    // Look up the description from the ops YAML packs (fs_ops, power_tools, etc.)
    if let Some(desc) = crate::fs_types::get_op_description(op) {
        desc.to_string()
    } else {
        format!("An operation in Cadmus's toolkit ({}).", op)
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // -- Full round-trip: create plan --

    #[test]
    fn test_process_zip_up_downloads() {
        let mut state = DialogueState::new();
        let response = process_input("zip up everything in my downloads", &mut state);

        match response {
            NlResponse::PlanCreated { plan_yaml, summary: _, prompt } => {
                assert!(plan_yaml.contains("walk_tree"));
                assert!(plan_yaml.contains("pack_archive"));
                assert!(prompt.contains("Approve"));
                assert!(state.current_plan.is_some());
            }
            other => panic!("expected PlanCreated, got: {:?}", other),
        }
    }

    #[test]
    fn test_process_find_pdfs() {
        let mut state = DialogueState::new();
        let response = process_input("find all PDFs in ~/Documents", &mut state);

        match response {
            NlResponse::PlanCreated { plan_yaml, .. } => {
                assert!(plan_yaml.contains("walk_tree") || plan_yaml.contains("find_matching"));
            }
            other => panic!("expected PlanCreated, got: {:?}", other),
        }
    }

    #[test]
    fn test_process_list_dir() {
        let mut state = DialogueState::new();
        let response = process_input("list ~/Downloads", &mut state);

        match response {
            NlResponse::PlanCreated { plan_yaml, .. } => {
                assert!(plan_yaml.contains("list_dir"));
            }
            other => panic!("expected PlanCreated, got: {:?}", other),
        }
    }

    // -- Explain --

    #[test]
    fn test_process_whats_walk_mean() {
        let mut state = DialogueState::new();
        let response = process_input("what's walk mean", &mut state);

        match response {
            NlResponse::Explanation { text } => {
                assert!(text.contains("directory") || text.contains("walk"),
                    "explanation should mention directory/walk: {}", text);
            }
            other => panic!("expected Explanation, got: {:?}", other),
        }
    }

    #[test]
    fn test_process_explain_filter() {
        let mut state = DialogueState::new();
        let response = process_input("explain filter", &mut state);

        match response {
            NlResponse::Explanation { text } => {
                assert!(text.contains("filter") || text.contains("match"),
                    "text: {}", text);
            }
            other => panic!("expected Explanation, got: {:?}", other),
        }
    }

    // -- Approve / Reject --

    #[test]
    fn test_process_lgtm() {
        let mut state = DialogueState::new();
        process_input("zip up ~/Downloads", &mut state);
        let response = process_input("lgtm", &mut state);
        assert!(matches!(response, NlResponse::Approved { .. }));
    }

    #[test]
    fn test_process_sounds_good() {
        let mut state = DialogueState::new();
        process_input("zip up ~/Downloads", &mut state);
        let response = process_input("sounds good", &mut state);
        assert!(matches!(response, NlResponse::Approved { .. }));
    }

    #[test]
    fn test_process_nah_start_over() {
        let mut state = DialogueState::new();
        // First create a plan
        process_input("zip up ~/Downloads", &mut state);
        assert!(state.current_plan.is_some());

        // Then reject
        let response = process_input("nah", &mut state);
        assert!(matches!(response, NlResponse::Rejected));
        assert!(state.current_plan.is_none());
    }

    // -- Edit --

    #[test]
    fn test_process_skip_subdirectory() {
        let mut state = DialogueState::new();

        // First create a plan
        let r1 = process_input("zip up everything in ~/Downloads", &mut state);
        assert!(matches!(r1, NlResponse::PlanCreated { .. }));

        // Then edit it
        let r2 = process_input("skip any subdirectory named foo", &mut state);
        match r2 {
            NlResponse::PlanEdited { plan_yaml, .. } => {
                assert!(plan_yaml.contains("filter"),
                    "should have filter step: {}", plan_yaml);
            }
            other => panic!("expected PlanEdited, got: {:?}", other),
        }
    }

    // -- Typo correction --

    #[test]
    fn test_process_with_typos() {
        let mut state = DialogueState::new();
        // Use a realistic command with a path so the plan compiles
        let response = process_input("extrct the archve at ~/comic.cbz", &mut state);

        match response {
            NlResponse::PlanCreated { plan_yaml, .. } => {
                assert!(plan_yaml.contains("extract_archive"),
                    "typo should be corrected: {}", plan_yaml);
            }
            other => panic!("expected PlanCreated, got: {:?}", other),
        }
    }

    // -- NeedsClarification --

    #[test]
    fn test_process_gibberish() {
        let mut state = DialogueState::new();
        let response = process_input("asdfghjkl qwerty", &mut state);
        assert!(matches!(response, NlResponse::NeedsClarification { .. }));
    }

    // -- Three-turn conversation --

    #[test]
    fn test_three_turn_conversation() {
        let mut state = DialogueState::new();

        // Turn 1: Create
        let r1 = process_input("zip up everything in ~/Downloads", &mut state);
        assert!(matches!(r1, NlResponse::PlanCreated { .. }));
        assert!(state.current_plan.is_some());

        // Turn 2: Edit
        let r2 = process_input("skip any subdirectory named .git", &mut state);
        match &r2 {
            NlResponse::PlanEdited { plan_yaml, .. } => {
                assert!(plan_yaml.contains("filter"));
            }
            other => panic!("expected PlanEdited, got: {:?}", other),
        }

        // Turn 3: Approve
        let r3 = process_input("lgtm", &mut state);
        assert!(matches!(r3, NlResponse::Approved { .. }));
    }

    // -- Casual ack variation --

    #[test]
    fn test_casual_ack_varies() {
        let ack1 = casual_ack(0);
        let ack2 = casual_ack(1);
        let ack3 = casual_ack(2);
        // At least some should be different
        assert!(ack1 != ack2 || ack2 != ack3, "acks should vary");
    }

    // -- Generated YAML round-trips --

    #[test]
    fn test_generated_yaml_roundtrips() {
        let mut state = DialogueState::new();
        let response = process_input("zip up everything in ~/Downloads", &mut state);

        if let NlResponse::PlanCreated { plan_yaml, .. } = response {
            // Parse it back
            let parsed = crate::plan::parse_plan(&plan_yaml);
            assert!(parsed.is_ok(), "should parse: {:?}", parsed.err());
        }
    }

    // -- Edit on empty state --

    #[test]
    fn test_edit_without_plan() {
        let mut state = DialogueState::new();
        let response = process_input("skip any subdirectory named foo", &mut state);
        // Should get clarification, not an error
        assert!(matches!(response, NlResponse::NeedsClarification { .. }));
    }

    // -- B6 bugfix: approve/reject without plan --

    #[test]
    fn test_approve_without_plan_needs_clarification() {
        let mut state = DialogueState::new();
        let response = process_input("approve", &mut state);
        assert!(matches!(response, NlResponse::NeedsClarification { .. }),
            "approve without plan should need clarification, got: {:?}", response);
    }

    #[test]
    fn test_approve_after_plan_created_works() {
        let mut state = DialogueState::new();
        let r1 = process_input("zip up everything in ~/Downloads", &mut state);
        assert!(matches!(r1, NlResponse::PlanCreated { .. }));
        let r2 = process_input("approve", &mut state);
        assert!(matches!(r2, NlResponse::Approved { .. }));
    }

    #[test]
    fn test_approve_after_error_needs_clarification() {
        let mut state = DialogueState::new();
        // This should fail validation (no plan stored)
        let r1 = process_input("do the thing", &mut state);
        assert!(matches!(r1, NlResponse::NeedsClarification { .. }),
            "do the thing should need clarification: {:?}", r1);
        assert!(state.current_plan.is_none());
        // Now approve should also need clarification
        let r2 = process_input("approve", &mut state);
        assert!(matches!(r2, NlResponse::NeedsClarification { .. }),
            "approve after error should need clarification, got: {:?}", r2);
    }

    #[test]
    fn test_double_approve_fails() {
        let mut state = DialogueState::new();
        let r1 = process_input("compress file.txt", &mut state);
        assert!(matches!(r1, NlResponse::PlanCreated { .. }));
        let r2 = process_input("yes", &mut state);
        assert!(matches!(r2, NlResponse::Approved { .. }), "first approve should succeed: {:?}", r2);
        // Second approve should fail — plan was cleared
        let r3 = process_input("yes", &mut state);
        assert!(matches!(r3, NlResponse::NeedsClarification { .. }),
            "second approve should need clarification, got: {:?}", r3);
    }

    #[test]
    fn test_approve_clears_then_new_plan_works() {
        let mut state = DialogueState::new();
        let _ = process_input("compress file.txt", &mut state);
        let _ = process_input("yes", &mut state);
        // After approve+clear, creating a new plan should work
        let r = process_input("list ~/Desktop", &mut state);
        assert!(matches!(r, NlResponse::PlanCreated { .. }), "new plan after approve: {:?}", r);
        let r2 = process_input("ok", &mut state);
        assert!(matches!(r2, NlResponse::Approved { .. }), "approve new plan: {:?}", r2);
    }
}
