use crate::fact_pack::FactPackIndex;
use crate::theory::TheoryContext;
use crate::types::*;

// ---------------------------------------------------------------------------
// Reasoning plan — the output of the planner
// ---------------------------------------------------------------------------

#[derive(Debug)]
pub struct ReasoningPlan {
    pub obligations: Vec<Obligation>,
    pub steps: Vec<ReasoningStep>,
    pub gaps: Vec<String>,
}

impl ReasoningPlan {
    pub fn is_complete(&self) -> bool {
        self.gaps.is_empty() && self.obligations.iter().all(|o| o.fulfilled)
    }
}

// ---------------------------------------------------------------------------
// Planner
// ---------------------------------------------------------------------------

/// Expand a goal into typed obligations and select operations to fulfill them.
pub fn plan(
    goal: &Goal,
    index: &FactPackIndex,
    theory: &TheoryContext,
) -> Result<ReasoningPlan> {
    let mut obligations = Vec::new();
    let mut steps = Vec::new();
    let mut gaps = Vec::new();

    for axis in &index.pack.axes {
        let axis_id = &axis.id;

        // --- Per-entity obligations: Claim + Evidence ---
        for entity in &goal.entities {
            let has_claims = index
                .claims_by_axis_entity
                .get(&(axis_id.clone(), entity.clone()))
                .map(|v| !v.is_empty())
                .unwrap_or(false);

            // Evidence obligation (retrieve from fact pack)
            let ev_slot = format!("{}_evidence_{}", entity, axis_id);
            obligations.push(Obligation::new(
                &ev_slot,
                axis_id,
                Some(entity.clone()),
                OutputType::Evidence,
            ));

            if has_claims {
                // We can fulfill evidence by retrieval
                steps.push(ReasoningStep {
                    operation: OperationKind::RetrieveEvidence,
                    axis: axis_id.clone(),
                    entity: Some(entity.clone()),
                    input_slots: vec![],
                    output_slot: ev_slot.clone(),
                });
                // Mark fulfilled
                obligations.last_mut().unwrap().fulfilled = true;
            } else {
                gaps.push(format!(
                    "No claims for entity '{}' on axis '{}' — evidence cannot be retrieved",
                    entity, axis_id
                ));
            }

            // Claim obligation (summarize evidence into claim)
            let claim_slot = format!("{}_claim_{}", entity, axis_id);
            obligations.push(Obligation::new(
                &claim_slot,
                axis_id,
                Some(entity.clone()),
                OutputType::Claim,
            ));

            if has_claims {
                steps.push(ReasoningStep {
                    operation: OperationKind::SummarizeEvidence,
                    axis: axis_id.clone(),
                    entity: Some(entity.clone()),
                    input_slots: vec![ev_slot],
                    output_slot: claim_slot.clone(),
                });
                obligations.last_mut().unwrap().fulfilled = true;
            }
        }

        // --- Cross-entity obligations: Contrast, Similarity ---
        if goal.entities.len() >= 2 {
            let e0 = &goal.entities[0];
            let e1 = &goal.entities[1];
            let claim_a = format!("{}_claim_{}", e0, axis_id);
            let claim_b = format!("{}_claim_{}", e1, axis_id);

            let both_have_claims = index
                .claims_by_axis_entity
                .get(&(axis_id.clone(), e0.clone()))
                .map(|v| !v.is_empty())
                .unwrap_or(false)
                && index
                    .claims_by_axis_entity
                    .get(&(axis_id.clone(), e1.clone()))
                    .map(|v| !v.is_empty())
                    .unwrap_or(false);

            // Contrast (at least one per axis)
            let contrast_slot = format!("contrast_{}", axis_id);
            obligations.push(Obligation::new(
                &contrast_slot,
                axis_id,
                None,
                OutputType::Contrast,
            ));
            if both_have_claims {
                steps.push(ReasoningStep {
                    operation: OperationKind::CompareClaims,
                    axis: axis_id.clone(),
                    entity: None,
                    input_slots: vec![claim_a.clone(), claim_b.clone()],
                    output_slot: contrast_slot,
                });
                obligations.last_mut().unwrap().fulfilled = true;
            } else {
                gaps.push(format!(
                    "Cannot produce contrast for axis '{}' — missing claims for one or both entities",
                    axis_id
                ));
            }

            // Similarity (at least one per axis)
            let sim_slot = format!("similarity_{}", axis_id);
            obligations.push(Obligation::new(
                &sim_slot,
                axis_id,
                None,
                OutputType::Similarity,
            ));
            if both_have_claims {
                steps.push(ReasoningStep {
                    operation: OperationKind::FindSimilarity,
                    axis: axis_id.clone(),
                    entity: None,
                    input_slots: vec![claim_a.clone(), claim_b.clone()],
                    output_slot: sim_slot,
                });
                obligations.last_mut().unwrap().fulfilled = true;
            } else {
                gaps.push(format!(
                    "Cannot produce similarity for axis '{}' — missing claims for one or both entities",
                    axis_id
                ));
            }
        }

        // --- Uncertainty obligation ---
        let unc_slot = format!("uncertainty_{}", axis_id);
        obligations.push(Obligation::new(
            &unc_slot,
            axis_id,
            None,
            OutputType::Uncertainty,
        ));
        // Always plan an uncertainty step — the executor merges authored
        // (from YAML) and derived (from theory layer) uncertainties.
        let has_any_claims = index
            .claims_by_axis
            .get(axis_id)
            .map(|v| !v.is_empty())
            .unwrap_or(false);
        let input = if !goal.entities.is_empty() && has_any_claims {
            vec![format!("{}_claim_{}", goal.entities[0], axis_id)]
        } else {
            vec![]
        };
        steps.push(ReasoningStep {
            operation: OperationKind::SurfaceUncertainty,
            axis: axis_id.clone(),
            entity: None,
            input_slots: input,
            output_slot: unc_slot,
        });
        obligations.last_mut().unwrap().fulfilled = true;

        // --- Summary obligation ---
        let sum_slot = format!("summary_{}", axis_id);
        obligations.push(Obligation::new(
            &sum_slot,
            axis_id,
            None,
            OutputType::Summary,
        ));
        if has_any_claims {
            let mut inputs = Vec::new();
            for entity in &goal.entities {
                inputs.push(format!("{}_claim_{}", entity, axis_id));
            }
            inputs.push(format!("contrast_{}", axis_id));
            inputs.push(format!("similarity_{}", axis_id));
            steps.push(ReasoningStep {
                operation: OperationKind::SummarizeAxis,
                axis: axis_id.clone(),
                entity: None,
                input_slots: inputs,
                output_slot: sum_slot,
            });
            obligations.last_mut().unwrap().fulfilled = true;
        } else {
            gaps.push(format!(
                "Cannot summarize axis '{}' — no claims available",
                axis_id
            ));
        }
    }

    // Validate: check for theory-derived divergences that need surfacing
    for divergence in &theory.divergences {
        let has_contrast = steps.iter().any(|s| {
            s.operation == OperationKind::CompareClaims && s.axis == divergence.axis
        });
        if !has_contrast {
            gaps.push(format!(
                "Theory divergence on axis '{}' ({}) but no contrast step planned",
                divergence.axis, divergence.property_key
            ));
        }
    }

    Ok(ReasoningPlan {
        obligations,
        steps,
        gaps,
    })
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fact_pack::load_fact_pack;
    use crate::theory::build_theory_context;
    use std::path::PathBuf;

    fn make_goal() -> Goal {
        Goal {
            description: "Compare Putin and Stalin as autocrats".into(),
            entities: vec!["putin".into(), "stalin".into()],
            fact_pack_paths: vec!["data/putin_stalin.yaml".into()],
        }
    }

    #[test]
    fn test_plan_all_axes_fulfilled() {
        let path = PathBuf::from("data/putin_stalin.yaml");
        let idx = load_fact_pack(&path).unwrap();
        let theory = build_theory_context(&idx);
        let goal = make_goal();
        let plan = plan(&goal, &idx, &theory).unwrap();

        // All 7 axes should produce obligations
        let axes: Vec<&str> = vec![
            "legitimacy", "coercion", "elite_control", "ideology",
            "economic_management", "information_control", "foreign_policy_risk",
        ];
        for axis in &axes {
            let axis_obligations: Vec<_> = plan.obligations.iter().filter(|o| o.axis == *axis).collect();
            assert!(!axis_obligations.is_empty(), "no obligations for axis {}", axis);
            let all_fulfilled = axis_obligations.iter().all(|o| o.fulfilled);
            assert!(all_fulfilled, "unfulfilled obligations for axis {}", axis);
        }

        assert!(plan.is_complete(), "plan should be complete, gaps: {:?}", plan.gaps);
    }

    #[test]
    fn test_plan_obligation_types() {
        let path = PathBuf::from("data/putin_stalin.yaml");
        let idx = load_fact_pack(&path).unwrap();
        let theory = build_theory_context(&idx);
        let goal = make_goal();
        let plan = plan(&goal, &idx, &theory).unwrap();

        // Each axis should have: 2 evidence, 2 claims, 1 contrast, 1 similarity, 1 uncertainty, 1 summary
        for axis in &idx.pack.axes {
            let axis_obs: Vec<_> = plan.obligations.iter().filter(|o| o.axis == axis.id).collect();
            let count = |t: OutputType| axis_obs.iter().filter(|o| o.required_type == t).count();
            assert_eq!(count(OutputType::Evidence), 2, "axis {} evidence", axis.id);
            assert_eq!(count(OutputType::Claim), 2, "axis {} claims", axis.id);
            assert_eq!(count(OutputType::Contrast), 1, "axis {} contrast", axis.id);
            assert_eq!(count(OutputType::Similarity), 1, "axis {} similarity", axis.id);
            assert_eq!(count(OutputType::Uncertainty), 1, "axis {} uncertainty", axis.id);
            assert_eq!(count(OutputType::Summary), 1, "axis {} summary", axis.id);
        }
    }

    #[test]
    fn test_plan_single_entity_axis_gaps() {
        // Fact pack where only one entity has claims on an axis
        let yaml = r#"
entities:
  - id: a
    name: A
    description: test
  - id: b
    name: B
    description: test
axes:
  - id: x
    name: X
    description: test
    sub_axes: []
claims:
  - id: c1
    entity: a
    axis: x
    text: "A claim"
evidence: []
relations: []
uncertainties: []
"#;
        let pack: crate::fact_pack::FactPack = serde_yaml::from_str(yaml).unwrap();
        let idx = crate::fact_pack::FactPackIndex::build(pack);
        let theory = crate::theory::build_theory_context(&idx);
        let goal = Goal {
            description: "Compare A and B".into(),
            entities: vec!["a".into(), "b".into()],
            fact_pack_paths: vec!["test".into()],
        };
        let plan = plan(&goal, &idx, &theory).unwrap();

        // Should NOT be complete — entity B has no claims
        assert!(!plan.is_complete(), "plan should have gaps");
        assert!(!plan.gaps.is_empty(), "expected gap messages");
        // Should mention entity b
        let has_b_gap = plan.gaps.iter().any(|g| g.contains("'b'"));
        assert!(has_b_gap, "expected gap mentioning entity b, got: {:?}", plan.gaps);
    }
}
