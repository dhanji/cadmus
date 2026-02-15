use std::collections::HashMap;

use crate::fact_pack::{FactPackIndex, Relation};
use crate::types::DerivedUncertainty;

// ---------------------------------------------------------------------------
// Theory context — all comparisons DERIVED from per-entity data
// ---------------------------------------------------------------------------

/// An ordinal comparison derived by comparing per-entity property values.
/// Neither entity's data references the other — the theory layer performs
/// the cross-entity reasoning.
#[derive(Debug, Clone)]
pub struct OrdinalComparison {
    pub axis: String,
    pub property_key: String,
    pub higher_entity: String,
    pub higher_value: String,
    pub higher_ordinal: i32,
    pub lower_entity: String,
    pub lower_value: String,
    pub lower_ordinal: i32,
    pub direction: String,
    pub description: String,
}

/// A divergence detected when two entities have qualitatively different
/// property values on the same axis+key, derived purely from per-entity data.
#[derive(Debug, Clone)]
pub struct Divergence {
    pub axis: String,
    pub property_key: String,
    pub entity_a: String,
    pub value_a: String,
    pub entity_b: String,
    pub value_b: String,
    pub description: String,
}

/// Hierarchy normalization: maps sub-axis evidence to parent axis.
#[derive(Debug, Clone)]
pub struct HierarchyMapping {
    pub parent_axis: String,
    pub children: Vec<String>,
}

/// The full theory context — everything here is DERIVED, not declared.
#[derive(Debug)]
pub struct TheoryContext {
    /// Ordinal comparisons derived from per-entity property values
    pub comparisons: Vec<OrdinalComparison>,
    /// Qualitative divergences where entities differ sharply
    pub divergences: Vec<Divergence>,
    /// Axis hierarchies (structural, entity-blind)
    pub hierarchies: HashMap<String, HierarchyMapping>,
    /// Claims normalized under parent axes (claim_id → parent_axis)
    pub normalized_claims: HashMap<String, String>,
    /// Mechanically derived uncertainties (evidence gaps, ordinal boundaries, tensions, mismatches)
    pub derived_uncertainties: Vec<DerivedUncertainty>,
}

// ---------------------------------------------------------------------------
// Theory engine — derives all cross-entity reasoning
// ---------------------------------------------------------------------------

pub fn build_theory_context(index: &FactPackIndex) -> TheoryContext {
    let mut comparisons = Vec::new();
    let mut divergences = Vec::new();
    let mut derived_uncertainties: Vec<DerivedUncertainty> = Vec::new();
    let mut hierarchies = HashMap::new();

    // Collect ordinal relation declarations (entity-blind axis metadata)
    let mut ordinal_decls: Vec<(String, String, String)> = Vec::new(); // (axis, key, direction)

    for rel in &index.pack.relations {
        match rel {
            Relation::Hierarchy {
                axis,
                parent,
                children,
                ..
            } => {
                hierarchies.insert(
                    parent.clone(),
                    HierarchyMapping {
                        parent_axis: axis.clone(),
                        children: children.clone(),
                    },
                );
            }
            Relation::Ordinal {
                axis,
                property_key,
                direction,
                ..
            } => {
                ordinal_decls.push((axis.clone(), property_key.clone(), direction.clone()));
            }
        }
    }

    // Derive ordinal comparisons from per-entity property values
    for (axis, key, direction) in &ordinal_decls {
        let prop_indices = index
            .properties_by_axis_key
            .get(&(axis.clone(), key.clone()))
            .cloned()
            .unwrap_or_default();

        if prop_indices.len() < 2 {
            continue;
        }

        // Compare all pairs
        for i in 0..prop_indices.len() {
            for j in (i + 1)..prop_indices.len() {
                let pa = &index.pack.properties[prop_indices[i]];
                let pb = &index.pack.properties[prop_indices[j]];

                let ord_a = pa.ordinal.unwrap_or(0);
                let ord_b = pb.ordinal.unwrap_or(0);

                // Gap = 0: no ordinal comparison, but emit OrdinalBoundary
                if ord_a == ord_b {
                    derived_uncertainties.push(DerivedUncertainty::OrdinalBoundary {
                        axis: axis.clone(),
                        property_key: key.clone(),
                        gap: 0,
                        higher_entity: pa.entity.clone(),
                        lower_entity: pb.entity.clone(),
                    });
                    continue;
                }

                let (higher, lower) = if ord_a > ord_b {
                    (pa, pb)
                } else {
                    (pb, pa)
                };
                let h_ord = higher.ordinal.unwrap_or(0);
                let l_ord = lower.ordinal.unwrap_or(0);

                comparisons.push(OrdinalComparison {
                    axis: axis.clone(),
                    property_key: key.clone(),
                    higher_entity: higher.entity.clone(),
                    higher_value: higher.value.clone(),
                    higher_ordinal: h_ord,
                    lower_entity: lower.entity.clone(),
                    lower_value: lower.value.clone(),
                    lower_ordinal: l_ord,
                    direction: direction.clone(),
                    description: format!(
                        "On {}, {} scores higher on {} ({} [{}]) vs {} ({} [{}])",
                        axis.replace('_', " "),
                        higher.entity,
                        key.replace('_', " "),
                        higher.value,
                        h_ord,
                        lower.entity,
                        lower.value,
                        l_ord,
                    ),
                });

                // If the gap is large (>=2), also record as a divergence
                if (h_ord - l_ord).abs() >= 2 {
                    divergences.push(Divergence {
                        axis: axis.clone(),
                        property_key: key.clone(),
                        entity_a: higher.entity.clone(),
                        value_a: higher.value.clone(),
                        entity_b: lower.entity.clone(),
                        value_b: lower.value.clone(),
                        description: format!(
                            "Sharp divergence on {} {}: {} is '{}' vs {} is '{}' (gap: {})",
                            axis.replace('_', " "),
                            key.replace('_', " "),
                            higher.entity,
                            higher.value,
                            lower.entity,
                            lower.value,
                            h_ord - l_ord,
                        ),
                    });
                }

                // If the gap is small (<=1), record as an ordinal boundary uncertainty
                if (h_ord - l_ord).abs() <= 1 {
                    derived_uncertainties.push(DerivedUncertainty::OrdinalBoundary {
                        axis: axis.clone(),
                        property_key: key.clone(),
                        gap: (h_ord - l_ord).abs(),
                        higher_entity: higher.entity.clone(),
                        lower_entity: lower.entity.clone(),
                    });
                }
            }
        }
    }

    // Normalize claims: if a claim has a sub_axis that belongs to a hierarchy,
    // record it under the parent axis.
    let mut normalized_claims = HashMap::new();
    for claim in &index.pack.claims {
        if let Some(ref sub) = claim.sub_axis {
            for (parent, mapping) in &hierarchies {
                if mapping.children.contains(sub) {
                    normalized_claims.insert(claim.id.clone(), parent.clone());
                }
            }
        }
    }

    // -----------------------------------------------------------------------
    // Derive EvidenceGap uncertainties
    // -----------------------------------------------------------------------

    for axis in &index.pack.axes {
        // Collect all entities that have claims on this axis
        let entities_on_axis: Vec<&str> = index
            .pack
            .entities
            .iter()
            .filter(|e| {
                index.claims_by_axis_entity
                    .get(&(axis.id.clone(), e.id.clone()))
                    .map(|v| !v.is_empty())
                    .unwrap_or(false)
            })
            .map(|e| e.id.as_str())
            .collect();

        for entity_id in &entities_on_axis {
            let claim_indices = index
                .claims_by_axis_entity
                .get(&(axis.id.clone(), entity_id.to_string()))
                .cloned()
                .unwrap_or_default();

            let has_evidence = claim_indices.iter().any(|ci| {
                let claim = &index.pack.claims[*ci];
                index.evidence_by_claim.get(&claim.id).map(|v| !v.is_empty()).unwrap_or(false)
            });

            if !has_evidence {
                derived_uncertainties.push(DerivedUncertainty::EvidenceGap {
                    axis: axis.id.clone(),
                    entity: entity_id.to_string(),
                });
            }
        }
    }

    // -----------------------------------------------------------------------
    // Derive CrossAxisTension from axis polarity metadata
    // -----------------------------------------------------------------------
    // Build a map of axis_id → polarity for axes that have one
    let axis_polarities: HashMap<String, String> = index
        .pack
        .axes
        .iter()
        .filter_map(|a| a.polarity.as_ref().map(|p| (a.id.clone(), p.clone())))
        .collect();

    if !axis_polarities.is_empty() {
        // For each entity, collect axes where it is the higher scorer with gap >= 2
        let mut entity_high_axes: HashMap<String, Vec<(String, String)>> = HashMap::new(); // entity → [(axis, polarity)]

        for cmp in &comparisons {
            let gap = (cmp.higher_ordinal - cmp.lower_ordinal).abs();
            if gap >= 2 {
                if let Some(polarity) = axis_polarities.get(&cmp.axis) {
                    entity_high_axes
                        .entry(cmp.higher_entity.clone())
                        .or_default()
                        .push((cmp.axis.clone(), polarity.clone()));
                }
            }
        }

        // For each entity, check all pairs of high-scoring axes for polarity tension
        for (entity, high_axes) in &entity_high_axes {
            for i in 0..high_axes.len() {
                for j in (i + 1)..high_axes.len() {
                    let (ref axis_a, ref pol_a) = high_axes[i];
                    let (ref axis_b, ref pol_b) = high_axes[j];
                    if pol_a != pol_b {
                        derived_uncertainties.push(DerivedUncertainty::CrossAxisTension {
                            entity: entity.clone(),
                            axis_a: axis_a.clone(),
                            axis_b: axis_b.clone(),
                            polarity_a: pol_a.clone(),
                            polarity_b: pol_b.clone(),
                        });
                    }
                }
            }
        }
    }

    // -----------------------------------------------------------------------
    // Derive PropertyClaimMismatch uncertainties
    // -----------------------------------------------------------------------
    // For each entity+axis, check if claim text contains the OTHER entity's
    // property value word (suggesting the claim contradicts its own property).
    for ((_axis, key), prop_indices) in &index.properties_by_axis_key {
        if prop_indices.len() < 2 {
            continue;
        }
        // Build a map: entity → (value, ordinal) for this axis+key
        let entity_props: Vec<(&str, &str)> = prop_indices
            .iter()
            .map(|&i| {
                let p = &index.pack.properties[i];
                (p.entity.as_str(), p.value.as_str())
            })
            .collect();

        for &(entity, own_value) in &entity_props {
            // Collect other entities' values as "contradictory" keywords
            let other_values: Vec<&str> = entity_props
                .iter()
                .filter(|(e, _)| *e != entity)
                .map(|(_, v)| *v)
                .collect();

            // Get this entity's claims on this axis
            let prop = &index.pack.properties[prop_indices[0]];
            let axis = &prop.axis;
            if let Some(claim_indices) = index.claims_by_axis_entity.get(&(axis.clone(), entity.to_string())) {
                for &ci in claim_indices {
                    let claim = &index.pack.claims[ci];
                    let claim_lower = claim.text.to_lowercase();
                    for other_val in &other_values {
                        let other_lower = other_val.to_lowercase().replace('_', " ");
                        if claim_lower.contains(&other_lower) && own_value.to_lowercase() != other_lower {
                            derived_uncertainties.push(DerivedUncertainty::PropertyClaimMismatch {
                                axis: axis.clone(),
                                entity: entity.to_string(),
                                claim_fragment: other_val.to_string(),
                                property_key: key.clone(),
                                property_value: own_value.to_string(),
                            });
                        }
                    }
                }
            }
        }
    }

    TheoryContext {
        comparisons,
        divergences,
        hierarchies,
        normalized_claims,
        derived_uncertainties,
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fact_pack::load_fact_pack;
    use std::path::PathBuf;

    #[test]
    fn test_theory_from_putin_stalin() {
        let path = PathBuf::from("data/putin_stalin.yaml");
        let idx = load_fact_pack(&path).unwrap();
        let ctx = build_theory_context(&idx);

        // Should derive ordinal comparisons from properties
        assert!(
            !ctx.comparisons.is_empty(),
            "expected derived ordinal comparisons"
        );

        // Should find coercion comparison (stalin higher than putin)
        let coercion_cmp = ctx
            .comparisons
            .iter()
            .find(|c| c.axis == "coercion" && c.property_key == "repression_scale");
        assert!(coercion_cmp.is_some(), "expected coercion comparison");
        let cc = coercion_cmp.unwrap();
        assert_eq!(cc.higher_entity, "stalin");
        assert_eq!(cc.lower_entity, "putin");

        // Should detect divergences (large ordinal gaps)
        assert!(
            !ctx.divergences.is_empty(),
            "expected divergences for large gaps"
        );
        let ideology_div = ctx
            .divergences
            .iter()
            .find(|d| d.axis == "ideology");
        assert!(
            ideology_div.is_some(),
            "expected ideology divergence (gap of 4)"
        );

        // Should have hierarchy for coercion
        assert!(
            ctx.hierarchies.contains_key("coercion"),
            "expected coercion hierarchy"
        );
        assert_eq!(ctx.hierarchies["coercion"].children.len(), 3);

        // Claims with sub_axis should be normalized
        assert!(
            !ctx.normalized_claims.is_empty(),
            "expected normalized claims"
        );
    }

    #[test]
    fn test_ordinal_comparison_derivation() {
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
claims: []
evidence: []
properties:
  - entity: a
    axis: x
    key: intensity
    value: low
    ordinal: 1
  - entity: b
    axis: x
    key: intensity
    value: high
    ordinal: 4
relations:
  - id: r1
    kind: ordinal
    axis: x
    property_key: intensity
    direction: higher_is_more
uncertainties: []
"#;
        let pack: crate::fact_pack::FactPack = serde_yaml::from_str(yaml).unwrap();
        let idx = crate::fact_pack::FactPackIndex::build(pack);
        let ctx = build_theory_context(&idx);

        assert_eq!(ctx.comparisons.len(), 1);
        assert_eq!(ctx.comparisons[0].higher_entity, "b");
        assert_eq!(ctx.comparisons[0].lower_entity, "a");
        assert_eq!(ctx.comparisons[0].higher_ordinal, 4);
        assert_eq!(ctx.comparisons[0].lower_ordinal, 1);

        // Gap of 3 → should also be a divergence
        assert_eq!(ctx.divergences.len(), 1);
        assert_eq!(ctx.divergences[0].entity_a, "b");
        assert_eq!(ctx.divergences[0].entity_b, "a");
    }

    #[test]
    fn test_no_divergence_for_small_gap() {
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
claims: []
evidence: []
properties:
  - entity: a
    axis: x
    key: level
    value: low
    ordinal: 2
  - entity: b
    axis: x
    key: level
    value: medium
    ordinal: 3
relations:
  - id: r1
    kind: ordinal
    axis: x
    property_key: level
    direction: higher_is_more
uncertainties: []
"#;
        let pack: crate::fact_pack::FactPack = serde_yaml::from_str(yaml).unwrap();
        let idx = crate::fact_pack::FactPackIndex::build(pack);
        let ctx = build_theory_context(&idx);

        assert_eq!(ctx.comparisons.len(), 1);
        // Gap of 1 → no divergence
        assert!(ctx.divergences.is_empty(), "gap of 1 should not trigger divergence");
    }

    #[test]
    fn test_hierarchy_single_leaf() {
        let yaml = r#"
entities: []
axes:
  - id: parent
    name: Parent
    description: test
    sub_axes:
      - id: child
        name: Child
claims:
  - id: c1
    entity: x
    axis: parent
    sub_axis: child
    text: "test claim"
evidence: []
relations:
  - id: h1
    kind: hierarchy
    axis: parent
    parent: parent
    children: [child]
uncertainties: []
"#;
        let pack: crate::fact_pack::FactPack = serde_yaml::from_str(yaml).unwrap();
        let idx = crate::fact_pack::FactPackIndex::build(pack);
        let ctx = build_theory_context(&idx);
        assert!(ctx.hierarchies.contains_key("parent"));
        assert_eq!(ctx.hierarchies["parent"].children, vec!["child"]);
        assert_eq!(
            ctx.normalized_claims.get("c1"),
            Some(&"parent".to_string())
        );
    }

    // ===================================================================
    // Derived Uncertainty Tests
    // ===================================================================

    #[test]
    fn test_evidence_gap_one_entity_missing() {
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
  - id: c2
    entity: b
    axis: x
    text: "B claim"
evidence:
  - id: ev1
    supports: c1
    text: "Evidence for A"
relations: []
uncertainties: []
"#;
        let pack: crate::fact_pack::FactPack = serde_yaml::from_str(yaml).unwrap();
        let idx = crate::fact_pack::FactPackIndex::build(pack);
        let ctx = build_theory_context(&idx);

        let evidence_gaps: Vec<_> = ctx.derived_uncertainties.iter().filter(|du| {
            matches!(du, DerivedUncertainty::EvidenceGap { .. })
        }).collect();

        // Only entity b should have an evidence gap
        assert_eq!(evidence_gaps.len(), 1, "expected 1 evidence gap, got {:?}", evidence_gaps);
        match &evidence_gaps[0] {
            DerivedUncertainty::EvidenceGap { entity, axis } => {
                assert_eq!(entity, "b");
                assert_eq!(axis, "x");
            }
            _ => panic!("expected EvidenceGap"),
        }
    }

    #[test]
    fn test_evidence_gap_both_entities_missing() {
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
  - id: c2
    entity: b
    axis: x
    text: "B claim"
evidence: []
relations: []
uncertainties: []
"#;
        let pack: crate::fact_pack::FactPack = serde_yaml::from_str(yaml).unwrap();
        let idx = crate::fact_pack::FactPackIndex::build(pack);
        let ctx = build_theory_context(&idx);

        let evidence_gaps: Vec<_> = ctx.derived_uncertainties.iter().filter(|du| {
            matches!(du, DerivedUncertainty::EvidenceGap { .. })
        }).collect();

        assert_eq!(evidence_gaps.len(), 2, "expected 2 evidence gaps");
    }

    #[test]
    fn test_no_evidence_gap_when_fully_evidenced() {
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
  - id: c2
    entity: b
    axis: x
    text: "B claim"
evidence:
  - id: ev1
    supports: c1
    text: "Evidence for A"
  - id: ev2
    supports: c2
    text: "Evidence for B"
relations: []
uncertainties: []
"#;
        let pack: crate::fact_pack::FactPack = serde_yaml::from_str(yaml).unwrap();
        let idx = crate::fact_pack::FactPackIndex::build(pack);
        let ctx = build_theory_context(&idx);

        let evidence_gaps: Vec<_> = ctx.derived_uncertainties.iter().filter(|du| {
            matches!(du, DerivedUncertainty::EvidenceGap { .. })
        }).collect();

        assert!(evidence_gaps.is_empty(), "expected no evidence gaps when fully evidenced");
    }

    #[test]
    fn test_ordinal_boundary_gap_1() {
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
claims: []
evidence: []
properties:
  - entity: a
    axis: x
    key: level
    value: low
    ordinal: 3
  - entity: b
    axis: x
    key: level
    value: medium
    ordinal: 4
relations:
  - id: r1
    kind: ordinal
    axis: x
    property_key: level
    direction: higher_is_more
uncertainties: []
"#;
        let pack: crate::fact_pack::FactPack = serde_yaml::from_str(yaml).unwrap();
        let idx = crate::fact_pack::FactPackIndex::build(pack);
        let ctx = build_theory_context(&idx);

        let boundaries: Vec<_> = ctx.derived_uncertainties.iter().filter(|du| {
            matches!(du, DerivedUncertainty::OrdinalBoundary { .. })
        }).collect();

        assert_eq!(boundaries.len(), 1, "expected 1 ordinal boundary");
        match &boundaries[0] {
            DerivedUncertainty::OrdinalBoundary { gap, .. } => {
                assert_eq!(*gap, 1);
            }
            _ => panic!("expected OrdinalBoundary"),
        }
    }

    #[test]
    fn test_ordinal_boundary_gap_0() {
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
claims: []
evidence: []
properties:
  - entity: a
    axis: x
    key: level
    value: same
    ordinal: 3
  - entity: b
    axis: x
    key: level
    value: same
    ordinal: 3
relations:
  - id: r1
    kind: ordinal
    axis: x
    property_key: level
    direction: higher_is_more
uncertainties: []
"#;
        let pack: crate::fact_pack::FactPack = serde_yaml::from_str(yaml).unwrap();
        let idx = crate::fact_pack::FactPackIndex::build(pack);
        let ctx = build_theory_context(&idx);

        let boundaries: Vec<_> = ctx.derived_uncertainties.iter().filter(|du| {
            matches!(du, DerivedUncertainty::OrdinalBoundary { .. })
        }).collect();

        assert_eq!(boundaries.len(), 1, "expected 1 ordinal boundary for gap=0");
        match &boundaries[0] {
            DerivedUncertainty::OrdinalBoundary { gap, .. } => {
                assert_eq!(*gap, 0);
            }
            _ => panic!("expected OrdinalBoundary"),
        }
        // Display should say "no meaningful difference"
        let display = format!("{}", boundaries[0]);
        assert!(display.contains("no meaningful difference"), "got: {}", display);
    }

    #[test]
    fn test_no_ordinal_boundary_for_large_gap() {
        // Reuse the existing ordinal comparison test — gap of 3
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
claims: []
evidence: []
properties:
  - entity: a
    axis: x
    key: intensity
    value: low
    ordinal: 1
  - entity: b
    axis: x
    key: intensity
    value: high
    ordinal: 4
relations:
  - id: r1
    kind: ordinal
    axis: x
    property_key: intensity
    direction: higher_is_more
uncertainties: []
"#;
        let pack: crate::fact_pack::FactPack = serde_yaml::from_str(yaml).unwrap();
        let idx = crate::fact_pack::FactPackIndex::build(pack);
        let ctx = build_theory_context(&idx);

        let boundaries: Vec<_> = ctx.derived_uncertainties.iter().filter(|du| {
            matches!(du, DerivedUncertainty::OrdinalBoundary { .. })
        }).collect();

        assert!(boundaries.is_empty(), "gap of 3 should not produce OrdinalBoundary");
    }

    #[test]
    fn test_cross_axis_tension() {
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
    polarity: cost
    sub_axes: []
  - id: y
    name: Y
    description: test
    polarity: capability
    sub_axes: []
claims: []
evidence: []
properties:
  - entity: a
    axis: x
    key: difficulty
    value: high
    ordinal: 4
  - entity: b
    axis: x
    key: difficulty
    value: low
    ordinal: 1
  - entity: a
    axis: y
    key: adaptability
    value: high
    ordinal: 5
  - entity: b
    axis: y
    key: adaptability
    value: low
    ordinal: 2
relations:
  - id: r1
    kind: ordinal
    axis: x
    property_key: difficulty
    direction: higher_is_more
  - id: r2
    kind: ordinal
    axis: y
    property_key: adaptability
    direction: higher_is_more
uncertainties: []
"#;
        let pack: crate::fact_pack::FactPack = serde_yaml::from_str(yaml).unwrap();
        let idx = crate::fact_pack::FactPackIndex::build(pack);
        let ctx = build_theory_context(&idx);

        let tensions: Vec<_> = ctx.derived_uncertainties.iter().filter(|du| {
            matches!(du, DerivedUncertainty::CrossAxisTension { .. })
        }).collect();

        assert_eq!(tensions.len(), 1, "expected 1 tension");
        match &tensions[0] {
            DerivedUncertainty::CrossAxisTension { entity, polarity_a, polarity_b, .. } => {
                assert_eq!(entity, "a");
                assert!(polarity_a != polarity_b, "polarities should differ");
            }
            _ => panic!("expected CrossAxisTension"),
        }
    }

    #[test]
    fn test_no_tension_same_polarity() {
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
    polarity: cost
    sub_axes: []
  - id: y
    name: Y
    description: test
    polarity: cost
    sub_axes: []
claims: []
evidence: []
properties:
  - entity: a
    axis: x
    key: difficulty
    value: high
    ordinal: 4
  - entity: b
    axis: x
    key: difficulty
    value: low
    ordinal: 1
  - entity: a
    axis: y
    key: complexity
    value: high
    ordinal: 5
  - entity: b
    axis: y
    key: complexity
    value: low
    ordinal: 2
relations:
  - id: r1
    kind: ordinal
    axis: x
    property_key: difficulty
    direction: higher_is_more
  - id: r2
    kind: ordinal
    axis: y
    property_key: complexity
    direction: higher_is_more
uncertainties: []
"#;
        let pack: crate::fact_pack::FactPack = serde_yaml::from_str(yaml).unwrap();
        let idx = crate::fact_pack::FactPackIndex::build(pack);
        let ctx = build_theory_context(&idx);

        let tensions: Vec<_> = ctx.derived_uncertainties.iter().filter(|du| {
            matches!(du, DerivedUncertainty::CrossAxisTension { .. })
        }).collect();

        assert!(tensions.is_empty(), "same polarity should not produce tension");
    }

    #[test]
    fn test_no_tension_without_polarity() {
        // Axes without polarity should never produce tensions
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
  - id: y
    name: Y
    description: test
    sub_axes: []
claims: []
evidence: []
properties:
  - entity: a
    axis: x
    key: level
    value: high
    ordinal: 5
  - entity: b
    axis: x
    key: level
    value: low
    ordinal: 1
  - entity: a
    axis: y
    key: score
    value: high
    ordinal: 5
  - entity: b
    axis: y
    key: score
    value: low
    ordinal: 1
relations:
  - id: r1
    kind: ordinal
    axis: x
    property_key: level
    direction: higher_is_more
  - id: r2
    kind: ordinal
    axis: y
    property_key: score
    direction: higher_is_more
uncertainties: []
"#;
        let pack: crate::fact_pack::FactPack = serde_yaml::from_str(yaml).unwrap();
        let idx = crate::fact_pack::FactPackIndex::build(pack);
        let ctx = build_theory_context(&idx);

        let tensions: Vec<_> = ctx.derived_uncertainties.iter().filter(|du| {
            matches!(du, DerivedUncertainty::CrossAxisTension { .. })
        }).collect();

        assert!(tensions.is_empty(), "no polarity = no tensions");
    }

    #[test]
    fn test_property_claim_mismatch() {
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
    text: "A is actually quite dense and heavy"
  - id: c2
    entity: b
    axis: x
    text: "B is very light and airy"
evidence: []
properties:
  - entity: a
    axis: x
    key: weight
    value: light
    ordinal: 2
  - entity: b
    axis: x
    key: weight
    value: dense
    ordinal: 4
relations:
  - id: r1
    kind: ordinal
    axis: x
    property_key: weight
    direction: higher_is_more
uncertainties: []
"#;
        let pack: crate::fact_pack::FactPack = serde_yaml::from_str(yaml).unwrap();
        let idx = crate::fact_pack::FactPackIndex::build(pack);
        let ctx = build_theory_context(&idx);

        let mismatches: Vec<_> = ctx.derived_uncertainties.iter().filter(|du| {
            matches!(du, DerivedUncertainty::PropertyClaimMismatch { .. })
        }).collect();

        // Entity a's claim says "dense" but a's property is "light" → mismatch
        // Entity b's claim says "light" but b's property is "dense" → mismatch
        assert_eq!(mismatches.len(), 2, "expected 2 mismatches, got {:?}", mismatches);
    }

    #[test]
    fn test_no_mismatch_when_consistent() {
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
    text: "A is light and airy"
  - id: c2
    entity: b
    axis: x
    text: "B is dense and heavy"
evidence: []
properties:
  - entity: a
    axis: x
    key: weight
    value: light
    ordinal: 2
  - entity: b
    axis: x
    key: weight
    value: dense
    ordinal: 4
relations:
  - id: r1
    kind: ordinal
    axis: x
    property_key: weight
    direction: higher_is_more
uncertainties: []
"#;
        let pack: crate::fact_pack::FactPack = serde_yaml::from_str(yaml).unwrap();
        let idx = crate::fact_pack::FactPackIndex::build(pack);
        let ctx = build_theory_context(&idx);

        let mismatches: Vec<_> = ctx.derived_uncertainties.iter().filter(|du| {
            matches!(du, DerivedUncertainty::PropertyClaimMismatch { .. })
        }).collect();

        assert!(mismatches.is_empty(), "consistent claims should not trigger mismatch");
    }

    #[test]
    fn test_no_mismatch_when_no_property_mention() {
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
    text: "A has interesting characteristics"
  - id: c2
    entity: b
    axis: x
    text: "B is quite remarkable"
evidence: []
properties:
  - entity: a
    axis: x
    key: weight
    value: light
    ordinal: 2
  - entity: b
    axis: x
    key: weight
    value: dense
    ordinal: 4
relations:
  - id: r1
    kind: ordinal
    axis: x
    property_key: weight
    direction: higher_is_more
uncertainties: []
"#;
        let pack: crate::fact_pack::FactPack = serde_yaml::from_str(yaml).unwrap();
        let idx = crate::fact_pack::FactPackIndex::build(pack);
        let ctx = build_theory_context(&idx);

        let mismatches: Vec<_> = ctx.derived_uncertainties.iter().filter(|du| {
            matches!(du, DerivedUncertainty::PropertyClaimMismatch { .. })
        }).collect();

        assert!(mismatches.is_empty(), "no property value mentions = no false positives");
    }
}
