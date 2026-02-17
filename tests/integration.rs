use cadmus::pipeline;
use cadmus::types::{Goal, OutputType};

fn putin_stalin_goal() -> Goal {
    Goal {
        description: "Produce a structured comparison of Putin and Stalin as autocrats".into(),
        entities: vec!["putin".into(), "stalin".into()],
        fact_pack_paths: vec!["data/putin_stalin.yaml".into()],
    }
}

// ---------------------------------------------------------------------------
// Derived uncertainty integration tests
// ---------------------------------------------------------------------------

fn tiramisu_cheesecake_goal() -> Goal {
    Goal {
        description: "Compare tiramisu and cheesecake as desserts".into(),
        entities: vec!["tiramisu".into(), "cheesecake".into()],
        fact_pack_paths: vec!["data/tiramisu_cheesecake.yaml".into()],
    }
}

#[test]
fn test_derived_uncertainties_appear_in_output() {
    let goal = putin_stalin_goal();
    let output = pipeline::run(&goal).unwrap();

    // Putin has no evidence on ideology or information_control
    // so those axes should have derived evidence gap uncertainties
    let ideology = output.axes.iter().find(|a| a.axis == "Ideology").unwrap();
    let has_derived = ideology.uncertainties.iter().any(|u| u.content.contains("🔧"));
    assert!(has_derived, "ideology should have derived uncertainty for putin's missing evidence");
}

#[test]
fn test_derived_uncertainties_evidence_gap_tiramisu() {
    let goal = tiramisu_cheesecake_goal();
    let output = pipeline::run(&goal).unwrap();

    // Texture and flavor_profile have no evidence for either entity
    let texture = output.axes.iter().find(|a| a.axis == "Texture").unwrap();
    let unc_content = texture.uncertainties.iter().map(|u| u.content.as_str()).collect::<Vec<_>>().join("\n");
    let gap_count = unc_content.matches("No evidence supports").count();
    assert!(gap_count >= 2, "texture should have evidence gaps for both entities, got: {}",
        unc_content);
}

#[test]
fn test_ordinal_boundary_in_output() {
    let goal = putin_stalin_goal();
    let output = pipeline::run(&goal).unwrap();

    // Legitimacy has gap=1 (electoral_managed[3] vs revolutionary_mythic[4])
    let legitimacy = output.axes.iter().find(|a| a.axis == "Legitimacy").unwrap();
    let has_boundary = legitimacy.uncertainties.iter()
        .any(|u| u.content.contains("differ by only"));
    assert!(has_boundary, "legitimacy should have ordinal boundary uncertainty, got: {:?}",
        legitimacy.uncertainties.iter().map(|u| &u.content).collect::<Vec<_>>());
}

#[test]
fn test_authored_and_derived_both_present() {
    let goal = putin_stalin_goal();
    let output = pipeline::run(&goal).unwrap();

    // Legitimacy has both an authored uncertainty and a derived ordinal boundary
    let legitimacy = output.axes.iter().find(|a| a.axis == "Legitimacy").unwrap();
    let has_authored = legitimacy.uncertainties.iter().any(|u| u.content.contains("📝"));
    let has_derived = legitimacy.uncertainties.iter().any(|u| u.content.contains("🔧"));
    assert!(has_authored, "legitimacy should have authored uncertainty");
    assert!(has_derived, "legitimacy should have derived uncertainty");
}

#[test]
fn test_tiramisu_pipeline_structurally_complete() {
    let goal = tiramisu_cheesecake_goal();
    let output = pipeline::run(&goal).unwrap();

    assert_eq!(output.axes.len(), 6, "expected 6 axes for dessert comparison");
    for axis in &output.axes {
        assert!(!axis.claims.is_empty(), "axis '{}' has no claims", axis.axis);
        assert!(!axis.uncertainties.is_empty(), "axis '{}' has no uncertainties", axis.axis);
        assert!(axis.gaps.is_empty(), "axis '{}' has gaps: {:?}", axis.axis, axis.gaps);
    }
}

#[test]
fn test_tiramisu_derived_uncertainties_on_axes_without_yaml() {
    // The tiramisu fact pack has authored uncertainties on all 6 axes,
    // but derived uncertainties should also appear (evidence gaps, ordinal boundaries)
    let goal = tiramisu_cheesecake_goal();
    let output = pipeline::run(&goal).unwrap();

    let total_derived: usize = output.axes.iter()
        .map(|a| a.uncertainties.iter().filter(|u| u.content.contains("🔧")).count())
        .sum();

    assert!(total_derived > 0, "expected at least some derived uncertainties in tiramisu fact pack");
}

#[test]
fn test_no_evidence_gap_on_fully_evidenced_axis() {
    let goal = putin_stalin_goal();
    let output = pipeline::run(&goal).unwrap();

    // Coercion has evidence for both putin and stalin
    let coercion = output.axes.iter().find(|a| a.axis == "Coercion").unwrap();
    let evidence_gap_count = coercion.uncertainties.iter()
        .filter(|u| u.content.contains("No evidence supports"))
        .count();
    assert_eq!(evidence_gap_count, 0, "coercion should have no evidence gaps");
}

#[test]
fn test_full_pipeline_structural_completeness() {
    let goal = putin_stalin_goal();
    let output = pipeline::run(&goal).expect("pipeline should succeed");

    assert_eq!(output.axes.len(), 7, "expected 7 axes");

    let expected_axes = [
        "Legitimacy",
        "Coercion",
        "Elite Control",
        "Ideology",
        "Economic Management",
        "Information Control",
        "Foreign Policy Risk",
    ];

    for expected in &expected_axes {
        let found = output.axes.iter().any(|a| a.axis == *expected);
        assert!(found, "missing axis: {}", expected);
    }
}

#[test]
fn test_every_axis_has_required_slots() {
    let goal = putin_stalin_goal();
    let output = pipeline::run(&goal).unwrap();

    for axis in &output.axes {
        assert!(
            axis.claims.len() >= 2,
            "axis '{}' needs >=2 claims, got {}",
            axis.axis,
            axis.claims.len()
        );
        assert!(
            axis.evidence.len() >= 2,
            "axis '{}' needs >=2 evidence, got {}",
            axis.axis,
            axis.evidence.len()
        );
        assert!(
            !axis.contrasts.is_empty(),
            "axis '{}' has no contrasts",
            axis.axis
        );
        assert!(
            !axis.similarities.is_empty(),
            "axis '{}' has no similarities",
            axis.axis
        );
        assert!(
            !axis.uncertainties.is_empty(),
            "axis '{}' has no uncertainties",
            axis.axis
        );
        assert!(
            axis.summary.is_some(),
            "axis '{}' has no summary",
            axis.axis
        );
        assert!(
            axis.gaps.is_empty(),
            "axis '{}' has unfulfilled obligations: {:?}",
            axis.axis,
            axis.gaps
        );
    }
}

#[test]
fn test_output_types_correct() {
    let goal = putin_stalin_goal();
    let output = pipeline::run(&goal).unwrap();

    for axis in &output.axes {
        for c in &axis.claims {
            assert_eq!(c.output_type, OutputType::Claim);
        }
        for e in &axis.evidence {
            assert_eq!(e.output_type, OutputType::Evidence);
        }
        for s in &axis.similarities {
            assert_eq!(s.output_type, OutputType::Similarity);
        }
        for c in &axis.contrasts {
            assert_eq!(c.output_type, OutputType::Contrast);
        }
        for u in &axis.uncertainties {
            assert_eq!(u.output_type, OutputType::Uncertainty);
        }
        if let Some(ref s) = axis.summary {
            assert_eq!(s.output_type, OutputType::Summary);
        }
    }
}

#[test]
fn test_theory_derives_ordinal_comparisons() {
    let goal = putin_stalin_goal();
    let output = pipeline::run(&goal).unwrap();

    // Inferences are now derived ordinal comparisons, not pre-baked relations
    assert!(
        !output.inferences.is_empty(),
        "expected theory-derived ordinal comparisons"
    );

    // Should derive a coercion comparison (repression_scale)
    let has_repression = output
        .inferences
        .iter()
        .any(|i| i.contains("repression") || i.contains("coercion"));
    assert!(
        has_repression,
        "expected derived repression comparison, got: {:?}",
        output.inferences
    );

    // Should derive an ideology comparison
    let has_ideology = output
        .inferences
        .iter()
        .any(|i| i.contains("ideolog"));
    assert!(has_ideology, "expected derived ideology comparison");
}

#[test]
fn test_theory_derives_divergences() {
    let goal = putin_stalin_goal();
    let output = pipeline::run(&goal).unwrap();

    // Conflicts are now derived divergences (large ordinal gaps), not pre-declared
    assert!(
        !output.conflicts.is_empty(),
        "expected theory-derived divergences"
    );

    // Ideology should have a large gap (ordinal 1 vs 5 = gap of 4)
    let has_ideology_div = output
        .conflicts
        .iter()
        .any(|c| c.contains("ideolog"));
    assert!(
        has_ideology_div,
        "expected ideology divergence, got: {:?}",
        output.conflicts
    );

    // Coercion should have a large gap (ordinal 2 vs 5 = gap of 3)
    let has_coercion_div = output
        .conflicts
        .iter()
        .any(|c| c.contains("coercion") || c.contains("repression"));
    assert!(
        has_coercion_div,
        "expected coercion divergence, got: {:?}",
        output.conflicts
    );
}

#[test]
fn test_uncertainty_annotations_present() {
    let goal = putin_stalin_goal();
    let output = pipeline::run(&goal).unwrap();

    for axis in &output.axes {
        for unc in &axis.uncertainties {
            assert!(
                unc.content.contains("📝") || unc.content.contains("🔧"),
                "uncertainty on axis '{}' should have 📝 or 🔧 marker",
                axis.axis
            );
        }
    }
}

#[test]
fn test_coercion_axis_has_hierarchy_evidence() {
    let goal = putin_stalin_goal();
    let output = pipeline::run(&goal).unwrap();

    let coercion = output
        .axes
        .iter()
        .find(|a| a.axis == "Coercion")
        .expect("coercion axis should exist");

    let has_sub_axis = coercion
        .evidence
        .iter()
        .any(|e| e.content.contains("sub-axis:"));
    assert!(
        has_sub_axis,
        "coercion evidence should include hierarchy-normalized sub-axis annotations"
    );
}

#[test]
fn test_missing_fact_pack_returns_error() {
    let goal = Goal {
        description: "test".into(),
        entities: vec!["a".into()],
        fact_pack_paths: vec!["data/does_not_exist.yaml".into()],
    };
    let result = pipeline::run(&goal);
    assert!(result.is_err(), "should fail with missing fact pack");
    let err = format!("{}", result.unwrap_err());
    assert!(
        err.contains("cannot read"),
        "error should mention file read failure, got: {}",
        err
    );
}

#[test]
fn test_entities_in_output_match_goal() {
    let goal = putin_stalin_goal();
    let output = pipeline::run(&goal).unwrap();

    assert_eq!(output.entities, vec!["putin", "stalin"]);
    assert_eq!(
        output.goal,
        "Produce a structured comparison of Putin and Stalin as autocrats"
    );
}

#[test]
fn test_contrasts_contain_derived_annotations() {
    let goal = putin_stalin_goal();
    let output = pipeline::run(&goal).unwrap();

    // Coercion contrast should contain theory-derived comparison
    let coercion = output
        .axes
        .iter()
        .find(|a| a.axis == "Coercion")
        .unwrap();
    let has_derived = coercion
        .contrasts
        .iter()
        .any(|c| c.content.contains("DERIVED") || c.content.contains("DIVERGENCE"));
    assert!(
        has_derived,
        "coercion contrast should contain theory-derived annotations"
    );

    // Ideology contrast should contain divergence
    let ideology = output
        .axes
        .iter()
        .find(|a| a.axis == "Ideology")
        .unwrap();
    let has_div = ideology
        .contrasts
        .iter()
        .any(|c| c.content.contains("DIVERGENCE"));
    assert!(
        has_div,
        "ideology contrast should contain DIVERGENCE annotation"
    );
}

#[test]
fn test_no_cross_entity_contamination_in_claims() {
    let goal = putin_stalin_goal();
    let output = pipeline::run(&goal).unwrap();

    // Every claim should only reference its own entity
    for axis in &output.axes {
        for claim in &axis.claims {
            let entity = claim.entity.as_deref().unwrap_or("?");
            let other = if entity == "putin" { "stalin" } else { "putin" };
            // The claim content (from fact pack) should not mention the other entity
            assert!(
                !claim.content.to_lowercase().contains(other),
                "claim for {} on axis '{}' mentions {}: {}",
                entity,
                axis.axis,
                other,
                claim.content
            );
        }
    }
}
