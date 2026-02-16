use serde::{Deserialize, Serialize};
use std::fmt;
use std::collections::HashSet;

// ---------------------------------------------------------------------------
// Output types — the "currency" of the reasoning engine
// ---------------------------------------------------------------------------

/// Every obligation slot and every operation output is tagged with one of these.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum OutputType {
    Claim,
    Evidence,
    Contrast,
    Similarity,
    Summary,
    Uncertainty,
}

impl fmt::Display for OutputType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Claim => write!(f, "Claim"),
            Self::Evidence => write!(f, "Evidence"),
            Self::Contrast => write!(f, "Contrast"),
            Self::Similarity => write!(f, "Similarity"),
            Self::Summary => write!(f, "Summary"),
            Self::Uncertainty => write!(f, "Uncertainty"),
        }
    }
}

// ---------------------------------------------------------------------------
// Obligations — what the plan *requires*
// ---------------------------------------------------------------------------

/// A single typed slot that must be filled for the plan to be complete.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Obligation {
    /// Human-readable slot name, e.g. "putin_claim_legitimacy"
    pub slot: String,
    /// The axis this obligation belongs to (e.g. "legitimacy")
    pub axis: String,
    /// Which entity this concerns (if any)
    pub entity: Option<String>,
    /// Required output type
    pub required_type: OutputType,
    /// Has an operation been assigned that produces this type?
    pub fulfilled: bool,
}

impl Obligation {
    pub fn new(slot: impl Into<String>, axis: impl Into<String>, entity: Option<String>, required_type: OutputType) -> Self {
        Self {
            slot: slot.into(),
            axis: axis.into(),
            entity,
            required_type,
            fulfilled: false,
        }
    }
}

// ---------------------------------------------------------------------------
// Operations — typed transformations the engine can execute
// ---------------------------------------------------------------------------

/// Each operation declares what output type it produces.
/// The planner matches operations to obligations by type.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum OperationKind {
    /// Look up raw evidence from the fact pack
    RetrieveEvidence,
    /// Synthesize a claim from evidence
    SummarizeEvidence,
    /// Compare two claims to produce a contrast
    CompareClaims,
    /// Compare two claims to find a similarity
    FindSimilarity,
    /// Produce a summary for an axis
    SummarizeAxis,
    /// Surface uncertainty or counterpoints
    SurfaceUncertainty,
}

impl OperationKind {
    /// The output type this operation produces.
    pub fn output_type(&self) -> OutputType {
        match self {
            Self::RetrieveEvidence => OutputType::Evidence,
            Self::SummarizeEvidence => OutputType::Claim,
            Self::CompareClaims => OutputType::Contrast,
            Self::FindSimilarity => OutputType::Similarity,
            Self::SummarizeAxis => OutputType::Summary,
            Self::SurfaceUncertainty => OutputType::Uncertainty,
        }
    }

    /// Input types this operation requires to run.
    pub fn input_types(&self) -> Vec<OutputType> {
        match self {
            Self::RetrieveEvidence => vec![],                         // reads from fact pack directly
            Self::SummarizeEvidence => vec![OutputType::Evidence],
            Self::CompareClaims => vec![OutputType::Claim, OutputType::Claim],
            Self::FindSimilarity => vec![OutputType::Claim, OutputType::Claim],
            Self::SummarizeAxis => vec![OutputType::Claim, OutputType::Contrast, OutputType::Similarity],
            Self::SurfaceUncertainty => vec![OutputType::Claim],
        }
    }
}

// ---------------------------------------------------------------------------
// Reasoning steps — a concrete planned action
// ---------------------------------------------------------------------------

/// A single step in the reasoning plan: an operation bound to specific slots.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReasoningStep {
    /// Which operation to perform
    pub operation: OperationKind,
    /// The axis this step concerns
    pub axis: String,
    /// Entity (if scoped to one leader)
    pub entity: Option<String>,
    /// Slot names this step reads from (inputs)
    pub input_slots: Vec<String>,
    /// Slot name this step writes to (output)
    pub output_slot: String,
}

// ---------------------------------------------------------------------------
// Goal — the top-level task
// ---------------------------------------------------------------------------

/// Structured representation of what the user wants.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Goal {
    /// Free-form description of the goal
    pub description: String,
    /// The entities to compare
    pub entities: Vec<String>,
    /// Paths to fact pack files (loaded and merged in order).
    /// Use `fact_pack_paths` for multiple packs, or the convenience
    /// `fact_pack_path` alias for a single pack.
    #[serde(default)]
    pub fact_pack_paths: Vec<String>,
}

impl Goal {
    /// Convenience: create a Goal with a single fact pack path.
    /// This is the backward-compatible constructor matching the old
    /// `fact_pack_path: String` field.
    pub fn with_single_fact_pack(
        description: impl Into<String>,
        entities: Vec<String>,
        fact_pack_path: impl Into<String>,
    ) -> Self {
        Self {
            description: description.into(),
            entities,
            fact_pack_paths: vec![fact_pack_path.into()],
        }
    }
}

// ---------------------------------------------------------------------------
// Produced values — the results of executing operations
// ---------------------------------------------------------------------------

/// A concrete value produced by executing an operation.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProducedValue {
    pub slot: String,
    pub output_type: OutputType,
    pub axis: String,
    pub entity: Option<String>,
    pub content: String,
    /// If this was inferred by the theory layer rather than directly from facts
    pub inferred: bool,
}

// ---------------------------------------------------------------------------
// Derived uncertainties — mechanically generated by the theory layer
// ---------------------------------------------------------------------------

/// A structured uncertainty derived by the theory layer, not authored in YAML.
/// Each variant carries the data needed to render a human-readable warning.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DerivedUncertainty {
    /// Claims exist on an axis for an entity, but no evidence supports them.
    EvidenceGap {
        axis: String,
        entity: String,
    },
    /// Two entities are very close on an ordinal property (gap ≤ 1),
    /// so the distinction may not be meaningful.
    OrdinalBoundary {
        axis: String,
        property_key: String,
        gap: i32,
        higher_entity: String,
        lower_entity: String,
    },
    /// An entity scores high on two axes with different polarities,
    /// suggesting a tension (e.g., high difficulty + high versatility).
    CrossAxisTension {
        entity: String,
        axis_a: String,
        axis_b: String,
        polarity_a: String,
        polarity_b: String,
    },
    /// A claim's text contains a keyword that contradicts the entity's
    /// own property value on the same axis.
    PropertyClaimMismatch {
        axis: String,
        entity: String,
        claim_fragment: String,
        property_key: String,
        property_value: String,
    },
}

impl DerivedUncertainty {
    /// The axis this uncertainty pertains to.
    /// CrossAxisTension returns axis_a (it spans two axes).
    pub fn axis(&self) -> &str {
        match self {
            Self::EvidenceGap { axis, .. } => axis,
            Self::OrdinalBoundary { axis, .. } => axis,
            Self::CrossAxisTension { axis_a, .. } => axis_a,
            Self::PropertyClaimMismatch { axis, .. } => axis,
        }
    }

    /// All axes this uncertainty touches (usually 1, but 2 for tensions).
    pub fn axes(&self) -> HashSet<String> {
        match self {
            Self::CrossAxisTension { axis_a, axis_b, .. } => {
                let mut s = HashSet::new();
                s.insert(axis_a.clone());
                s.insert(axis_b.clone());
                s
            }
            other => {
                let mut s = HashSet::new();
                s.insert(other.axis().to_string());
                s
            }
        }
    }
}

impl fmt::Display for DerivedUncertainty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EvidenceGap { axis, entity } => {
                write!(f, "🔧 No evidence supports {}'s claims on {} — assertions are unsupported", entity, axis.replace('_', " "))
            }
            Self::OrdinalBoundary { axis: _, property_key, gap, higher_entity, lower_entity } => {
                if *gap == 0 {
                    write!(f, "🔧 {} and {} show no meaningful difference on {}", higher_entity, lower_entity, property_key.replace('_', " "))
                } else {
                    write!(f, "🔧 {} and {} differ by only {} on {} — this distinction may not be meaningful", higher_entity, lower_entity, gap, property_key.replace('_', " "))
                }
            }
            Self::CrossAxisTension { entity, axis_a, axis_b, polarity_a, polarity_b } => {
                write!(f, "🔧 Tension: {} scores high on {} ({}) and {} ({}) — these pull in opposite directions",
                    entity, axis_a.replace('_', " "), polarity_a, axis_b.replace('_', " "), polarity_b)
            }
            Self::PropertyClaimMismatch { axis, entity, claim_fragment, property_key, property_value } => {
                write!(f, "🔧 Mismatch: {}'s claim on {} mentions '{}' but its {} property is '{}'",
                    entity, axis.replace('_', " "), claim_fragment, property_key.replace('_', " "), property_value)
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Structured output — the final result
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AxisResult {
    pub axis: String,
    pub claims: Vec<ProducedValue>,
    pub evidence: Vec<ProducedValue>,
    pub similarities: Vec<ProducedValue>,
    pub contrasts: Vec<ProducedValue>,
    pub uncertainties: Vec<ProducedValue>,
    pub summary: Option<ProducedValue>,
    /// Obligations that could not be fulfilled
    pub gaps: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReasoningOutput {
    pub goal: String,
    pub entities: Vec<String>,
    pub axes: Vec<AxisResult>,
    /// Theory-layer inferences that were applied
    pub inferences: Vec<String>,
    /// Conflicts detected by the theory layer
    pub conflicts: Vec<String>,
}

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug, thiserror::Error)]
pub enum EngineError {
    #[error("fact pack error: {0}")]
    FactPack(String),

    #[error("planning error: {0}")]
    Planning(String),

    #[error("execution error: {0}")]
    Execution(String),

    #[error("io error: {0}")]
    Io(#[from] std::io::Error),

    #[error("yaml parse error: {0}")]
    Yaml(#[from] serde_yaml::Error),
}

pub type Result<T> = std::result::Result<T, EngineError>;
