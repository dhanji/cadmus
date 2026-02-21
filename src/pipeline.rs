use crate::strategy;
use crate::types::*;

// ---------------------------------------------------------------------------
// Pipeline: the single entry point for all reasoning
// ---------------------------------------------------------------------------

/// Run the full reasoning pipeline from a Goal to structured output.
/// Delegates to the ComparisonStrategy via the strategy module.
/// This is the single code path — all reasoning goes through strategies.
pub fn run(goal: &Goal) -> Result<ReasoningOutput> {
    strategy::run_comparison(goal)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn make_goal() -> Goal {
        Goal {
            description: "Compare Putin and Stalin as autocrats".into(),
            entities: vec!["putin".into(), "stalin".into()],
            fact_pack_paths: vec!["data/packs/facts/putin_stalin.facts.yaml".into()],
        }
    }

    #[test]
    fn test_full_pipeline() {
        let goal = make_goal();
        let output = run(&goal).unwrap();

        assert_eq!(output.axes.len(), 7);
        for axis in &output.axes {
            assert!(!axis.claims.is_empty(), "axis '{}' has no claims", axis.axis);
            assert!(
                !axis.evidence.is_empty(),
                "axis '{}' has no evidence",
                axis.axis
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
            assert!(axis.summary.is_some(), "axis '{}' has no summary", axis.axis);
            assert!(
                axis.gaps.is_empty(),
                "axis '{}' has gaps: {:?}",
                axis.axis,
                axis.gaps
            );
        }

        // Theory should derive comparisons and divergences
        assert!(!output.inferences.is_empty(), "expected derived comparisons");
        assert!(!output.conflicts.is_empty(), "expected derived divergences");
    }

    #[test]
    fn test_missing_fact_pack() {
        let goal = Goal {
            description: "test".into(),
            entities: vec!["a".into()],
            fact_pack_paths: vec!["data/nonexistent.facts.yaml".into()],
        };
        let result = run(&goal);
        assert!(result.is_err());
        let err = format!("{}", result.unwrap_err());
        assert!(err.contains("cannot read"), "got: {}", err);
    }

    #[test]
    fn test_pipeline_zero_evidence() {
        let dir = std::path::PathBuf::from("tmp");
        std::fs::create_dir_all(&dir).ok();
        let path = dir.join("no_evidence.facts.yaml");
        std::fs::write(
            &path,
            r#"
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
  - id: c2
    entity: b
    axis: x
    text: "B claim"
evidence: []
relations: []
uncertainties:
  - axis: x
    text: "Some uncertainty"
"#,
        )
        .unwrap();

        let goal = Goal {
            description: "test".into(),
            entities: vec!["a".into(), "b".into()],
            fact_pack_paths: vec![path.to_str().unwrap().into()],
        };
        let output = run(&goal).unwrap();
        assert_eq!(output.axes.len(), 1);
        let axis = &output.axes[0];
        assert!(!axis.evidence.is_empty());
        assert!(axis.gaps.is_empty(), "unexpected gaps: {:?}", axis.gaps);
    }
}
