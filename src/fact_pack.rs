use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;

use crate::types::{EngineError, Result};

// ---------------------------------------------------------------------------
// YAML-deserialisable fact pack structures
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Entity {
    pub id: String,
    pub name: String,
    pub description: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SubAxis {
    pub id: String,
    pub name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Axis {
    pub id: String,
    pub name: String,
    pub description: String,
    #[serde(default)]
    pub polarity: Option<String>,
    #[serde(default)]
    pub sub_axes: Vec<SubAxis>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Claim {
    pub id: String,
    pub entity: String,
    pub axis: String,
    #[serde(default)]
    pub sub_axis: Option<String>,
    pub text: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Evidence {
    pub id: String,
    pub supports: String,
    pub text: String,
    #[serde(default)]
    pub source: Option<String>,
}

/// A per-entity, per-axis measurable property.
/// The theory layer compares property values across entities to derive
/// ordinal comparisons and detect divergence — the fact pack itself
/// never references more than one entity in a single entry.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Property {
    pub entity: String,
    pub axis: String,
    pub key: String,
    pub value: String,
    #[serde(default)]
    pub ordinal: Option<i32>,
    #[serde(default)]
    pub note: Option<String>,
}

/// Entity-blind structural metadata about an axis.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "kind")]
pub enum Relation {
    /// Axis hierarchy: parent axis subdivides into children.
    #[serde(rename = "hierarchy")]
    Hierarchy {
        id: String,
        axis: String,
        parent: String,
        children: Vec<String>,
    },
    /// Ordinal property declaration: a named property key on an axis
    /// whose values are ordinal-comparable across entities.
    #[serde(rename = "ordinal")]
    Ordinal {
        id: String,
        axis: String,
        property_key: String,
        direction: String,
        #[serde(default)]
        note: Option<String>,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UncertaintyEntry {
    pub axis: String,
    pub text: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FactPack {
    pub entities: Vec<Entity>,
    pub axes: Vec<Axis>,
    pub claims: Vec<Claim>,
    pub evidence: Vec<Evidence>,
    #[serde(default)]
    pub properties: Vec<Property>,
    #[serde(default)]
    pub relations: Vec<Relation>,
    #[serde(default)]
    pub uncertainties: Vec<UncertaintyEntry>,
}

// ---------------------------------------------------------------------------
// Indexed view for efficient lookups
// ---------------------------------------------------------------------------

#[derive(Debug)]
pub struct FactPackIndex {
    pub pack: FactPack,
    /// claims grouped by (axis, entity)
    pub claims_by_axis_entity: HashMap<(String, String), Vec<usize>>,
    /// claims grouped by axis
    pub claims_by_axis: HashMap<String, Vec<usize>>,
    /// evidence keyed by the claim id it supports
    pub evidence_by_claim: HashMap<String, Vec<usize>>,
    /// uncertainties by axis
    pub uncertainties_by_axis: HashMap<String, Vec<usize>>,
    /// properties by (axis, property_key, entity)
    pub properties_by_axis_key_entity: HashMap<(String, String, String), usize>,
    /// properties by (axis, property_key) → list of indices (one per entity)
    pub properties_by_axis_key: HashMap<(String, String), Vec<usize>>,
}

impl FactPackIndex {
    pub fn build(pack: FactPack) -> Self {
        let mut claims_by_axis_entity: HashMap<(String, String), Vec<usize>> = HashMap::new();
        let mut claims_by_axis: HashMap<String, Vec<usize>> = HashMap::new();
        for (i, c) in pack.claims.iter().enumerate() {
            claims_by_axis_entity
                .entry((c.axis.clone(), c.entity.clone()))
                .or_default()
                .push(i);
            claims_by_axis
                .entry(c.axis.clone())
                .or_default()
                .push(i);
        }

        let mut evidence_by_claim: HashMap<String, Vec<usize>> = HashMap::new();
        for (i, e) in pack.evidence.iter().enumerate() {
            evidence_by_claim
                .entry(e.supports.clone())
                .or_default()
                .push(i);
        }

        let mut uncertainties_by_axis: HashMap<String, Vec<usize>> = HashMap::new();
        for (i, u) in pack.uncertainties.iter().enumerate() {
            uncertainties_by_axis
                .entry(u.axis.clone())
                .or_default()
                .push(i);
        }

        let mut properties_by_axis_key_entity: HashMap<(String, String, String), usize> =
            HashMap::new();
        let mut properties_by_axis_key: HashMap<(String, String), Vec<usize>> = HashMap::new();
        for (i, p) in pack.properties.iter().enumerate() {
            properties_by_axis_key_entity
                .insert((p.axis.clone(), p.key.clone(), p.entity.clone()), i);
            properties_by_axis_key
                .entry((p.axis.clone(), p.key.clone()))
                .or_default()
                .push(i);
        }

        Self {
            pack,
            claims_by_axis_entity,
            claims_by_axis,
            evidence_by_claim,
            uncertainties_by_axis,
            properties_by_axis_key_entity,
            properties_by_axis_key,
        }
    }
}

// ---------------------------------------------------------------------------
// Loader
// ---------------------------------------------------------------------------

pub fn load_fact_pack(path: &Path) -> Result<FactPackIndex> {
    let content = std::fs::read_to_string(path).map_err(|e| {
        EngineError::FactPack(format!("cannot read {}: {}", path.display(), e))
    })?;
    let pack: FactPack = serde_yaml::from_str(&content).map_err(|e| {
        EngineError::FactPack(format!("parse error in {}: {}", path.display(), e))
    })?;
    Ok(FactPackIndex::build(pack))
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_load_putin_stalin() {
        let path = PathBuf::from("data/putin_stalin.yaml");
        let idx = load_fact_pack(&path).expect("should load fact pack");
        assert_eq!(idx.pack.entities.len(), 2);
        assert_eq!(idx.pack.axes.len(), 7);
        assert!(
            idx.pack.claims.len() >= 14,
            "expected at least 2 claims per axis per entity"
        );
        assert!(!idx.pack.evidence.is_empty());
        assert!(!idx.pack.relations.is_empty());
        assert!(!idx.pack.properties.is_empty());
        assert_eq!(idx.pack.uncertainties.len(), 7);

        // Check claim index
        let putin_leg =
            idx.claims_by_axis_entity.get(&("legitimacy".into(), "putin".into()));
        assert!(putin_leg.is_some());
        assert!(putin_leg.unwrap().len() >= 2);

        // Check property index
        let prop = idx.properties_by_axis_key_entity.get(&(
            "coercion".into(),
            "repression_scale".into(),
            "stalin".into(),
        ));
        assert!(prop.is_some(), "expected stalin coercion property");
        let p = &idx.pack.properties[*prop.unwrap()];
        assert_eq!(p.value, "mass");
    }

    #[test]
    fn test_malformed_yaml() {
        let dir = PathBuf::from("tmp");
        std::fs::create_dir_all(&dir).ok();
        let path = dir.join("bad.yaml");
        std::fs::write(&path, "{{{{not yaml").unwrap();
        let result = load_fact_pack(&path);
        assert!(result.is_err());
        let err_msg = format!("{}", result.unwrap_err());
        assert!(err_msg.contains("parse error"), "got: {}", err_msg);
    }

    #[test]
    fn test_missing_file() {
        let path = PathBuf::from("data/nonexistent.yaml");
        let result = load_fact_pack(&path);
        assert!(result.is_err());
        let err_msg = format!("{}", result.unwrap_err());
        assert!(err_msg.contains("cannot read"), "got: {}", err_msg);
    }

    #[test]
    fn test_empty_claims_axis() {
        let yaml = r#"
entities:
  - id: a
    name: A
    description: test
axes:
  - id: x
    name: X
    description: test axis
    sub_axes: []
claims: []
evidence: []
relations: []
uncertainties: []
"#;
        let pack: FactPack = serde_yaml::from_str(yaml).unwrap();
        let idx = FactPackIndex::build(pack);
        assert!(idx.claims_by_axis.is_empty());
    }
}
