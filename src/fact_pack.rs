use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::collections::HashSet;
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
// Fact pack composition — merge multiple packs into one
// ---------------------------------------------------------------------------

impl Relation {
    /// Extract the id from either variant.
    pub fn id(&self) -> &str {
        match self {
            Relation::Hierarchy { id, .. } => id,
            Relation::Ordinal { id, .. } => id,
        }
    }
}

impl FactPack {
    /// Create an empty fact pack.
    pub fn empty() -> Self {
        Self {
            entities: Vec::new(),
            axes: Vec::new(),
            claims: Vec::new(),
            evidence: Vec::new(),
            properties: Vec::new(),
            relations: Vec::new(),
            uncertainties: Vec::new(),
        }
    }

    /// Merge another fact pack into this one.
    ///
    /// Deduplication rules:
    /// - **Entities**: dedup by `id` — first occurrence wins, later duplicates skipped.
    /// - **Axes**: dedup by `id` — if both packs define the same axis, sub_axes are
    ///   unioned (first axis keeps its name/description/polarity, new sub_axes appended).
    /// - **Relations**: dedup by `id` — first occurrence wins.
    /// - **Claims, evidence, properties, uncertainties**: concatenated (no dedup —
    ///   these are scoped by entity+axis so duplicates across packs are unlikely
    ///   and harmless if present).
    pub fn merge(mut self, other: FactPack) -> Self {
        // --- Entities: dedup by id ---
        let existing_entity_ids: HashSet<String> =
            self.entities.iter().map(|e| e.id.clone()).collect();
        for entity in other.entities {
            if !existing_entity_ids.contains(&entity.id) {
                self.entities.push(entity);
            }
        }

        // --- Axes: dedup by id, union sub_axes ---
        let mut axis_index: HashMap<String, usize> = self
            .axes
            .iter()
            .enumerate()
            .map(|(i, a)| (a.id.clone(), i))
            .collect();
        for axis in other.axes {
            if let Some(&idx) = axis_index.get(&axis.id) {
                // Merge sub_axes: add any sub_axes not already present
                let existing_sub_ids: HashSet<String> = self.axes[idx]
                    .sub_axes
                    .iter()
                    .map(|s| s.id.clone())
                    .collect();
                for sub in axis.sub_axes {
                    if !existing_sub_ids.contains(&sub.id) {
                        self.axes[idx].sub_axes.push(sub);
                    }
                }
            } else {
                axis_index.insert(axis.id.clone(), self.axes.len());
                self.axes.push(axis);
            }
        }

        // --- Relations: dedup by id ---
        let existing_relation_ids: HashSet<String> =
            self.relations.iter().map(|r| r.id().to_string()).collect();
        for relation in other.relations {
            if !existing_relation_ids.contains(relation.id()) {
                self.relations.push(relation);
            }
        }

        // --- Claims, evidence, properties, uncertainties: concatenate ---
        self.claims.extend(other.claims);
        self.evidence.extend(other.evidence);
        self.properties.extend(other.properties);
        self.uncertainties.extend(other.uncertainties);

        self
    }

    /// Merge multiple fact packs into one. Packs are merged left-to-right,
    /// so the first pack's entities/axes take precedence on id conflicts.
    pub fn merge_all(packs: impl IntoIterator<Item = FactPack>) -> Self {
        let mut result = FactPack::empty();
        for pack in packs {
            result = result.merge(pack);
        }
        result
    }
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

/// Load a fact pack from a YAML string.
pub fn load_fact_pack_str(yaml: &str) -> Result<FactPackIndex> {
    let pack: FactPack = serde_yaml::from_str(yaml).map_err(|e| {
        EngineError::FactPack(format!("parse error: {}", e))
    })?;
    Ok(FactPackIndex::build(pack))
}

/// Load and merge multiple fact packs from file paths.
///
/// Packs are loaded in order and merged left-to-right: the first pack's
/// entities and axes take precedence on id conflicts, sub_axes are unioned,
/// and claims/evidence/properties/uncertainties are concatenated.
///
/// Returns an error if any path fails to load or parse.
pub fn load_fact_packs(paths: &[impl AsRef<Path>]) -> Result<FactPackIndex> {
    let mut packs = Vec::with_capacity(paths.len());
    for path in paths {
        let content = std::fs::read_to_string(path.as_ref()).map_err(|e| {
            EngineError::FactPack(format!("cannot read {}: {}", path.as_ref().display(), e))
        })?;
        let pack: FactPack = serde_yaml::from_str(&content).map_err(|e| {
            EngineError::FactPack(format!("parse error in {}: {}", path.as_ref().display(), e))
        })?;
        packs.push(pack);
    }
    Ok(FactPackIndex::build(FactPack::merge_all(packs)))
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
        let path = PathBuf::from("data/packs/facts/putin_stalin.yaml");
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

    // -----------------------------------------------------------------------
    // Merge / composition tests
    // -----------------------------------------------------------------------

    fn pack_a() -> FactPack {
        serde_yaml::from_str(r#"
entities:
  - id: tmux
    name: "tmux"
    description: "terminal multiplexer"
  - id: screen
    name: "GNU Screen"
    description: "terminal multiplexer"
axes:
  - id: performance
    name: "Performance"
    description: "Speed and resource usage"
    sub_axes:
      - id: startup
        name: "Startup Time"
  - id: ecosystem
    name: "Ecosystem"
    description: "Plugins and community"
    sub_axes: []
claims:
  - id: tmux_perf
    entity: tmux
    axis: performance
    text: "tmux starts in ~50ms"
  - id: screen_perf
    entity: screen
    axis: performance
    text: "screen starts in ~30ms"
evidence:
  - id: ev_tmux_perf
    supports: tmux_perf
    text: "Measured on macOS 14"
properties:
  - entity: tmux
    axis: performance
    key: startup_ms
    value: "50"
    ordinal: 50
relations:
  - kind: ordinal
    id: rel_startup
    axis: performance
    property_key: startup_ms
    direction: lower_is_better
uncertainties:
  - axis: performance
    text: "Startup time varies by config"
"#).unwrap()
    }

    fn pack_b() -> FactPack {
        serde_yaml::from_str(r#"
entities:
  - id: tmux
    name: "tmux (duplicate)"
    description: "should be skipped"
  - id: git
    name: "Git"
    description: "version control"
axes:
  - id: performance
    name: "Performance (dup)"
    description: "should keep original"
    sub_axes:
      - id: startup
        name: "Startup (dup)"
      - id: throughput
        name: "Throughput"
  - id: composability
    name: "Composability"
    description: "How well it pipes"
    sub_axes: []
claims:
  - id: git_perf
    entity: git
    axis: performance
    text: "git status is fast"
  - id: tmux_compose
    entity: tmux
    axis: composability
    text: "tmux integrates with shell scripts"
evidence:
  - id: ev_git_perf
    supports: git_perf
    text: "Measured on large repo"
properties:
  - entity: git
    axis: performance
    key: startup_ms
    value: "100"
    ordinal: 100
relations:
  - kind: ordinal
    id: rel_startup
    axis: performance
    property_key: startup_ms
    direction: lower_is_better
  - kind: hierarchy
    id: rel_perf_hierarchy
    axis: performance
    parent: performance
    children: [startup, throughput]
uncertainties:
  - axis: composability
    text: "Composability is subjective"
"#).unwrap()
    }

    #[test]
    fn test_merge_entities_dedup() {
        let merged = pack_a().merge(pack_b());
        // tmux should appear once (from pack_a), git added from pack_b
        assert_eq!(merged.entities.len(), 3); // tmux, screen, git
        let ids: Vec<&str> = merged.entities.iter().map(|e| e.id.as_str()).collect();
        assert_eq!(ids, vec!["tmux", "screen", "git"]);
        // First occurrence wins — tmux keeps pack_a's description
        assert_eq!(merged.entities[0].description, "terminal multiplexer");
    }

    #[test]
    fn test_merge_axes_dedup_and_union_sub_axes() {
        let merged = pack_a().merge(pack_b());
        // performance (merged), ecosystem (from a), composability (from b) = 3
        assert_eq!(merged.axes.len(), 3);
        let perf = merged.axes.iter().find(|a| a.id == "performance").unwrap();
        // Original had [startup], pack_b adds [throughput] (startup is dup, skipped)
        let sub_ids: Vec<&str> = perf.sub_axes.iter().map(|s| s.id.as_str()).collect();
        assert_eq!(sub_ids, vec!["startup", "throughput"]);
        // First axis keeps its name/description
        assert_eq!(perf.name, "Performance");
    }

    #[test]
    fn test_merge_relations_dedup() {
        let merged = pack_a().merge(pack_b());
        // rel_startup appears in both — should be deduped. rel_perf_hierarchy is new.
        assert_eq!(merged.relations.len(), 2);
        let ids: Vec<&str> = merged.relations.iter().map(|r| r.id()).collect();
        assert!(ids.contains(&"rel_startup"));
        assert!(ids.contains(&"rel_perf_hierarchy"));
    }

    #[test]
    fn test_merge_claims_concatenated() {
        let merged = pack_a().merge(pack_b());
        // pack_a: 2 claims, pack_b: 2 claims = 4 total
        assert_eq!(merged.claims.len(), 4);
    }

    #[test]
    fn test_merge_evidence_concatenated() {
        let merged = pack_a().merge(pack_b());
        assert_eq!(merged.evidence.len(), 2);
    }

    #[test]
    fn test_merge_properties_concatenated() {
        let merged = pack_a().merge(pack_b());
        assert_eq!(merged.properties.len(), 2);
    }

    #[test]
    fn test_merge_uncertainties_concatenated() {
        let merged = pack_a().merge(pack_b());
        assert_eq!(merged.uncertainties.len(), 2);
    }

    #[test]
    fn test_merge_all_empty() {
        let merged = FactPack::merge_all(Vec::<FactPack>::new());
        assert!(merged.entities.is_empty());
        assert!(merged.axes.is_empty());
        assert!(merged.claims.is_empty());
    }

    #[test]
    fn test_merge_all_single_pack() {
        let single = pack_a();
        let claims_count = single.claims.len();
        let merged = FactPack::merge_all(vec![single]);
        assert_eq!(merged.claims.len(), claims_count);
    }

    #[test]
    fn test_merged_index_queries_work() {
        let merged = pack_a().merge(pack_b());
        let idx = FactPackIndex::build(merged);

        // Claims from pack_a
        let tmux_perf = idx.claims_by_axis_entity.get(&("performance".into(), "tmux".into()));
        assert!(tmux_perf.is_some());
        assert_eq!(tmux_perf.unwrap().len(), 1);

        // Claims from pack_b
        let git_perf = idx.claims_by_axis_entity.get(&("performance".into(), "git".into()));
        assert!(git_perf.is_some());
        assert_eq!(git_perf.unwrap().len(), 1);

        // Cross-pack: tmux composability claim from pack_b
        let tmux_comp = idx.claims_by_axis_entity.get(&("composability".into(), "tmux".into()));
        assert!(tmux_comp.is_some());

        // Evidence from both packs
        assert!(idx.evidence_by_claim.contains_key("tmux_perf"));
        assert!(idx.evidence_by_claim.contains_key("git_perf"));

        // Properties from both packs
        let tmux_startup = idx.properties_by_axis_key_entity.get(
            &("performance".into(), "startup_ms".into(), "tmux".into())
        );
        assert!(tmux_startup.is_some());
        let git_startup = idx.properties_by_axis_key_entity.get(
            &("performance".into(), "startup_ms".into(), "git".into())
        );
        assert!(git_startup.is_some());
    }
}
