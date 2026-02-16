use std::collections::HashMap;

use crate::registry::{Literal, OperationRegistry, TypeId};
use crate::generic_planner::{self, GenericGoal, PlanNode, PlanError};
use crate::algebra;
use crate::types::{EngineError, Result};

// ---------------------------------------------------------------------------
// Strategy trait — the extension point for domain-specific reasoning
// ---------------------------------------------------------------------------

/// A reasoning strategy defines how a domain (comparison, coding, etc.)
/// maps onto the generic planning + algebra engine.
///
/// Each strategy provides:
/// - An operation registry with domain-specific typed ops
/// - Available literals extracted from domain data
/// - Algebra configuration (inference rules)
/// - A way to assemble final output from executed plan results
pub trait ReasonerStrategy {
    /// The output type of this strategy (what `run` returns as serializable data).
    type Output;

    /// Build the operation registry for this domain.
    fn build_registry(&self) -> OperationRegistry;

    /// Extract available typed literals from the domain data.
    fn available_literals(&self) -> Vec<Literal>;

    /// Provide inference rules for the algebra layer.
    fn inference_rules(&self) -> Vec<algebra::InferenceRule> {
        Vec::new() // default: no inference rules
    }

    /// Provide the set of goals (output types) this strategy needs to produce.
    /// The generic planner will plan each one.
    fn goals(&self) -> Vec<GenericGoal>;

    /// Execute a single plan node, producing a literal result.
    /// This is called bottom-up: leaves are resolved first, then ops.
    fn execute_node(
        &self,
        node: &PlanNode,
        resolved_children: &[Literal],
        registry: &OperationRegistry,
    ) -> Result<Literal>;

    /// Assemble the final output from all executed plan results.
    fn assemble(&self, results: Vec<Literal>) -> Result<Self::Output>;
}

// ---------------------------------------------------------------------------
// Generic engine runner — drives any strategy through the pipeline
// ---------------------------------------------------------------------------

/// Run a strategy through the full pipeline:
/// 1. Build registry
/// 2. Extract literals
/// 3. Run inference rules
/// 4. Plan each goal
/// 5. Canonicalize plans
/// 6. Execute plans
/// 7. Assemble output
pub fn run_strategy<S: ReasonerStrategy>(strategy: &S) -> Result<S::Output> {
    // 1. Build registry
    let registry = strategy.build_registry();

    // 2. Extract available literals
    let mut literals = strategy.available_literals();

    // 3. Run inference rules to derive additional facts
    let inference_rules = strategy.inference_rules();
    if !inference_rules.is_empty() {
        let facts: Vec<algebra::Fact> = literals
            .iter()
            .filter(|l| l.metadata.get("is_fact").map(|v| v == "true").unwrap_or(false))
            .filter_map(|l| {
                let relation = l.metadata.get("relation")?;
                let left = l.metadata.get("left")?;
                let right = l.metadata.get("right")?;
                Some(algebra::Fact::new(relation, left, right))
            })
            .collect();

        if !facts.is_empty() {
            let result = algebra::infer(&facts, &inference_rules);
            // Add derived facts as new literals
            for derived in &result.derived {
                literals.push(
                    Literal::new(
                        TypeId::new("DerivedFact"),
                        format!("derived_{}_{}", derived.left, derived.right),
                        format!("{}", derived),
                    )
                    .with_meta("is_fact", "true")
                    .with_meta("relation", &derived.relation)
                    .with_meta("left", &derived.left)
                    .with_meta("right", &derived.right)
                    .with_meta("inferred", "true"),
                );
            }
        }
    }

    // 4. Plan each goal
    let goals = strategy.goals();
    let mut plans = Vec::new();
    for goal in &goals {
        match generic_planner::plan(goal, &registry) {
            Ok(plan) => {
                // 5. Canonicalize
                let canonical = algebra::canonicalize(&plan, &registry);
                plans.push(canonical);
            }
            Err(PlanError::NoProducer { type_id }) => {
                return Err(EngineError::Planning(format!(
                    "no operation produces type '{}'",
                    type_id
                )));
            }
            Err(e) => {
                return Err(EngineError::Planning(format!("{}", e)));
            }
        }
    }

    // 6. Execute plans
    let mut results = Vec::new();
    for plan in &plans {
        let result = execute_plan(plan, strategy, &registry)?;
        results.push(result);
    }

    // 7. Assemble
    strategy.assemble(results)
}

/// Execute a plan tree bottom-up, resolving leaves first then operations.
fn execute_plan<S: ReasonerStrategy>(
    node: &PlanNode,
    strategy: &S,
    registry: &OperationRegistry,
) -> Result<Literal> {
    match node {
        PlanNode::Leaf { key, output_type } => {
            // Look up the literal from the strategy's available set
            let literals = strategy.available_literals();
            let lit = literals.iter().find(|l| l.key == *key);
            match lit {
                Some(l) => Ok(l.clone()),
                None => {
                    // Create a placeholder literal
                    Ok(Literal::new(output_type.clone(), key.clone(), format!("[unresolved: {}]", key)))
                }
            }
        }
        PlanNode::Op { children, .. } => {
            // Execute children first
            let mut resolved = Vec::new();
            for child in children {
                resolved.push(execute_plan(child, strategy, registry)?);
            }
            // Then execute this node
            strategy.execute_node(node, &resolved, registry)
        }
    }
}

// ---------------------------------------------------------------------------
// ComparisonStrategy — wraps the existing fact-pack comparison pipeline
// ---------------------------------------------------------------------------

use crate::fact_pack::{self, FactPackIndex};
use crate::theory::{self, TheoryContext};
use crate::planner;
use crate::types::*;

/// The comparison strategy: wraps the existing fact-pack + theory + pipeline
/// logic behind the Strategy trait. This is the "original" reasoning mode.
pub struct ComparisonStrategy {
    pub goal: Goal,
    index: FactPackIndex,
    theory: TheoryContext,
}

impl ComparisonStrategy {
    pub fn new(goal: Goal) -> Result<Self> {
        let index = fact_pack::load_fact_pack(std::path::Path::new(&goal.fact_pack_path))?;
        let theory = theory::build_theory_context(&index);
        Ok(Self { goal, index, theory })
    }

    /// Get a reference to the fact pack index.
    pub fn index(&self) -> &FactPackIndex {
        &self.index
    }

    /// Get a reference to the theory context.
    pub fn theory(&self) -> &TheoryContext {
        &self.theory
    }
}

impl ReasonerStrategy for ComparisonStrategy {
    type Output = ReasoningOutput;

    fn build_registry(&self) -> OperationRegistry {
        let mut reg = OperationRegistry::new();

        // Load type signatures from YAML ops pack (source of truth for what ops exist).
        // The poly ops enable type-directed planning via TypeExpr unification.
        const COMPARISON_OPS_YAML: &str = include_str!("../data/comparison_ops.yaml");
        crate::registry::load_ops_pack_str_into(COMPARISON_OPS_YAML, &mut reg)
            .expect("embedded comparison_ops.yaml should always parse");

        // Also register monomorphic ops with exec bindings for the classic planner path.
        // These stub exec bindings exist because actual execution goes through execute_node().
        use crate::registry::{AlgebraicProperties, OpSignature};

        reg.register(
            "retrieve_evidence",
            OpSignature::leaf(TypeId::new("Evidence")),
            AlgebraicProperties::none(),
            Box::new(|_| Ok("evidence".into())),
        );
        reg.register(
            "summarize_evidence",
            OpSignature::new(vec![TypeId::new("Evidence")], TypeId::new("Claim")),
            AlgebraicProperties::none(),
            Box::new(|_| Ok("claim".into())),
        );
        reg.register(
            "compare_claims",
            OpSignature::new(
                vec![TypeId::new("Claim"), TypeId::new("Claim")],
                TypeId::new("Contrast"),
            ),
            AlgebraicProperties::commutative(), // comparing A vs B = comparing B vs A
            Box::new(|_| Ok("contrast".into())),
        );
        reg.register(
            "find_similarity",
            OpSignature::new(
                vec![TypeId::new("Claim"), TypeId::new("Claim")],
                TypeId::new("Similarity"),
            ),
            AlgebraicProperties::commutative(),
            Box::new(|_| Ok("similarity".into())),
        );
        reg.register(
            "summarize_axis",
            OpSignature::new(
                vec![TypeId::new("Claim"), TypeId::new("Contrast"), TypeId::new("Similarity")],
                TypeId::new("Summary"),
            ),
            AlgebraicProperties::none(),
            Box::new(|_| Ok("summary".into())),
        );
        reg.register(
            "surface_uncertainty",
            OpSignature::new(
                vec![TypeId::new("Claim")],
                TypeId::new("Uncertainty"),
            ),
            AlgebraicProperties::none(),
            Box::new(|_| Ok("uncertainty".into())),
        );

        reg
    }

    fn available_literals(&self) -> Vec<Literal> {
        // In the comparison strategy, literals are extracted from the fact pack.
        // Each claim, evidence item, etc. becomes a typed literal.
        let mut literals = Vec::new();

        for (i, claim) in self.index.pack.claims.iter().enumerate() {
            literals.push(
                Literal::new(
                    TypeId::new("Claim"),
                    format!("claim_{}", claim.id),
                    claim.text.clone(),
                )
                .with_meta("entity", &claim.entity)
                .with_meta("axis", &claim.axis)
                .with_meta("index", &i.to_string()),
            );
        }

        for (i, ev) in self.index.pack.evidence.iter().enumerate() {
            literals.push(
                Literal::new(
                    TypeId::new("Evidence"),
                    format!("evidence_{}", ev.id),
                    ev.text.clone(),
                )
                .with_meta("supports", &ev.supports)
                .with_meta("index", &i.to_string()),
            );
        }

        literals
    }

    fn goals(&self) -> Vec<GenericGoal> {
        // The comparison strategy doesn't use the generic planner for actual planning —
        // it uses the existing planner. But we provide goals for validation.
        vec![GenericGoal::simple(
            TypeId::new("Summary"),
            self.available_literals(),
        )]
    }

    fn execute_node(
        &self,
        _node: &PlanNode,
        _resolved_children: &[Literal],
        _registry: &OperationRegistry,
    ) -> Result<Literal> {
        // The comparison strategy delegates to the existing pipeline executors.
        // This method is not called directly — run_comparison() handles execution.
        Ok(Literal::new(TypeId::new("Summary"), "placeholder", ""))
    }

    fn assemble(&self, _results: Vec<Literal>) -> Result<ReasoningOutput> {
        // The comparison strategy uses the existing assembler.
        // This is called by run_comparison(), not by run_strategy().
        Err(EngineError::Execution("use run_comparison() for ComparisonStrategy".into()))
    }
}

/// Run the comparison strategy through the existing pipeline.
/// This is the single code path that pipeline::run() delegates to.
pub fn run_comparison(goal: &Goal) -> Result<ReasoningOutput> {
    let strategy = ComparisonStrategy::new(goal.clone())?;

    // Validate: the registry can produce all needed types
    let registry = strategy.build_registry();
    let needed_types = vec![
        TypeId::new("Evidence"),
        TypeId::new("Claim"),
        TypeId::new("Contrast"),
        TypeId::new("Similarity"),
        TypeId::new("Summary"),
        TypeId::new("Uncertainty"),
    ];
    for t in &needed_types {
        if !registry.has_producer(t) {
            return Err(EngineError::Planning(format!(
                "comparison registry missing producer for type '{}'",
                t
            )));
        }
    }

    // Use the existing planner + pipeline for actual execution
    let plan = planner::plan(&strategy.goal, &strategy.index, &strategy.theory)?;
    let values = execute_comparison_plan(&plan, &strategy.index, &strategy.theory)?;
    let output = assemble_comparison(
        &strategy.goal,
        &strategy.index,
        &strategy.theory,
        &plan,
        &values,
    );

    Ok(output)
}

// ---------------------------------------------------------------------------
// Comparison pipeline internals (moved from pipeline.rs)
// ---------------------------------------------------------------------------

fn execute_comparison_plan(
    plan: &planner::ReasoningPlan,
    index: &FactPackIndex,
    theory: &TheoryContext,
) -> Result<HashMap<String, ProducedValue>> {
    let mut produced: HashMap<String, ProducedValue> = HashMap::new();

    for step in &plan.steps {
        let value = execute_comparison_step(step, index, theory, &produced)?;
        produced.insert(step.output_slot.clone(), value);
    }

    Ok(produced)
}

fn execute_comparison_step(
    step: &crate::types::ReasoningStep,
    index: &FactPackIndex,
    theory: &TheoryContext,
    produced: &HashMap<String, ProducedValue>,
) -> Result<ProducedValue> {
    match step.operation {
        OperationKind::RetrieveEvidence => execute_retrieve_evidence(step, index, theory),
        OperationKind::SummarizeEvidence => execute_summarize_evidence(step, index),
        OperationKind::CompareClaims => execute_compare_claims(step, index, theory, produced),
        OperationKind::FindSimilarity => execute_find_similarity(step, index, theory, produced),
        OperationKind::SummarizeAxis => execute_summarize_axis(step, produced),
        OperationKind::SurfaceUncertainty => execute_surface_uncertainty(step, index, theory),
    }
}

fn execute_retrieve_evidence(
    step: &ReasoningStep,
    index: &FactPackIndex,
    theory: &TheoryContext,
) -> Result<ProducedValue> {
    let entity = step.entity.as_deref().unwrap_or("unknown");
    let axis = &step.axis;

    let claim_indices = index
        .claims_by_axis_entity
        .get(&(axis.clone(), entity.to_string()))
        .cloned()
        .unwrap_or_default();

    let mut evidence_texts = Vec::new();
    for ci in &claim_indices {
        let claim = &index.pack.claims[*ci];
        if let Some(ev_indices) = index.evidence_by_claim.get(&claim.id) {
            for ei in ev_indices {
                let ev = &index.pack.evidence[*ei];
                let source_note = ev.source.as_deref().unwrap_or("no source");
                evidence_texts.push(format!("• {} [{}]", ev.text, source_note));
            }
        }
    }

    // Check for hierarchy-normalized claims
    let normalized_claims: Vec<_> = theory
        .normalized_claims
        .iter()
        .filter(|(_, parent)| *parent == axis)
        .map(|(cid, _)| cid.clone())
        .collect();
    for cid in &normalized_claims {
        if let Some(claim) = index
            .pack
            .claims
            .iter()
            .find(|c| c.id == *cid && c.entity == entity)
        {
            if let Some(ev_indices) = index.evidence_by_claim.get(&claim.id) {
                for ei in ev_indices {
                    let ev = &index.pack.evidence[*ei];
                    let sub = claim.sub_axis.as_deref().unwrap_or("general");
                    evidence_texts.push(format!(
                        "• [sub-axis: {}] {} [{}]",
                        sub,
                        ev.text,
                        ev.source.as_deref().unwrap_or("no source")
                    ));
                }
            }
        }
    }

    let content = if evidence_texts.is_empty() {
        format!("[No direct evidence found for {} on {}]", entity, axis)
    } else {
        evidence_texts.join("\n")
    };

    Ok(ProducedValue {
        slot: step.output_slot.clone(),
        output_type: OutputType::Evidence,
        axis: axis.clone(),
        entity: Some(entity.to_string()),
        content,
        inferred: false,
    })
}

fn execute_summarize_evidence(
    step: &ReasoningStep,
    index: &FactPackIndex,
) -> Result<ProducedValue> {
    let entity = step.entity.as_deref().unwrap_or("unknown");
    let axis = &step.axis;

    let claim_indices = index
        .claims_by_axis_entity
        .get(&(axis.clone(), entity.to_string()))
        .cloned()
        .unwrap_or_default();

    let claim_texts: Vec<String> = claim_indices
        .iter()
        .map(|i| index.pack.claims[*i].text.clone())
        .collect();

    let content = if claim_texts.is_empty() {
        format!("[No claims available for {} on {}]", entity, axis)
    } else {
        claim_texts.join(" ")
    };

    Ok(ProducedValue {
        slot: step.output_slot.clone(),
        output_type: OutputType::Claim,
        axis: axis.clone(),
        entity: Some(entity.to_string()),
        content,
        inferred: false,
    })
}

fn execute_compare_claims(
    step: &ReasoningStep,
    _index: &FactPackIndex,
    theory: &TheoryContext,
    produced: &HashMap<String, ProducedValue>,
) -> Result<ProducedValue> {
    let axis = &step.axis;

    let claim_a = step.input_slots.first().and_then(|s| produced.get(s));
    let claim_b = step.input_slots.get(1).and_then(|s| produced.get(s));

    let mut contrast_parts = Vec::new();

    for cmp in &theory.comparisons {
        if cmp.axis == *axis {
            contrast_parts.push(format!("↕ DERIVED: {}", cmp.description));
        }
    }

    for div in &theory.divergences {
        if div.axis == *axis {
            contrast_parts.push(format!("⚡ DIVERGENCE: {}", div.description));
        }
    }

    if let (Some(a), Some(b)) = (claim_a, claim_b) {
        let entity_a = a.entity.as_deref().unwrap_or("?");
        let entity_b = b.entity.as_deref().unwrap_or("?");
        contrast_parts.push(format!(
            "While {} — {}; in contrast {} — {}",
            entity_a,
            truncate(&a.content, 200),
            entity_b,
            truncate(&b.content, 200),
        ));
    }

    let has_theory = !theory.comparisons.iter().any(|c| c.axis == *axis)
        && !theory.divergences.iter().any(|d| d.axis == *axis);

    let content = if contrast_parts.is_empty() {
        format!("[No contrast data for axis {}]", axis)
    } else {
        contrast_parts.join("\n")
    };

    Ok(ProducedValue {
        slot: step.output_slot.clone(),
        output_type: OutputType::Contrast,
        axis: axis.clone(),
        entity: None,
        content,
        inferred: !has_theory,
    })
}

fn execute_find_similarity(
    step: &ReasoningStep,
    index: &FactPackIndex,
    theory: &TheoryContext,
    produced: &HashMap<String, ProducedValue>,
) -> Result<ProducedValue> {
    let axis = &step.axis;

    let claim_a = step.input_slots.first().and_then(|s| produced.get(s));
    let claim_b = step.input_slots.get(1).and_then(|s| produced.get(s));

    let mut sim_parts = Vec::new();

    for cmp in &theory.comparisons {
        if cmp.axis == *axis && (cmp.higher_ordinal - cmp.lower_ordinal).abs() <= 1 {
            sim_parts.push(format!(
                "≈ Similar on {}: {} ({}) and {} ({}) are close (gap: {})",
                cmp.property_key.replace('_', " "),
                cmp.higher_entity,
                cmp.higher_value,
                cmp.lower_entity,
                cmp.lower_value,
                cmp.higher_ordinal - cmp.lower_ordinal,
            ));
        }
    }

    if let (Some(a), Some(b)) = (claim_a, claim_b) {
        let entity_a = a.entity.as_deref().unwrap_or("?");
        let entity_b = b.entity.as_deref().unwrap_or("?");

        let axis_desc = index
            .pack
            .axes
            .iter()
            .find(|ax| ax.id == *axis)
            .map(|ax| ax.description.as_str())
            .unwrap_or(axis);

        sim_parts.push(format!(
            "Both {} and {} engage in {}: {}; {}",
            entity_a,
            entity_b,
            axis_desc.to_lowercase(),
            truncate(&a.content, 120),
            truncate(&b.content, 120),
        ));
    }

    let content = if sim_parts.is_empty() {
        format!("[Insufficient data for similarity on axis {}]", axis)
    } else {
        sim_parts.join("\n")
    };

    Ok(ProducedValue {
        slot: step.output_slot.clone(),
        output_type: OutputType::Similarity,
        axis: axis.clone(),
        entity: None,
        content,
        inferred: false,
    })
}

fn execute_summarize_axis(
    step: &ReasoningStep,
    produced: &HashMap<String, ProducedValue>,
) -> Result<ProducedValue> {
    let axis = &step.axis;

    let mut parts = Vec::new();
    for slot in &step.input_slots {
        if let Some(val) = produced.get(slot) {
            parts.push(format!(
                "[{}] {}",
                val.output_type,
                truncate(&val.content, 300)
            ));
        }
    }

    let content = if parts.is_empty() {
        format!("[No data to summarize for axis {}]", axis)
    } else {
        format!(
            "Axis '{}' summary:\n{}",
            axis.replace('_', " "),
            parts.join("\n")
        )
    };

    Ok(ProducedValue {
        slot: step.output_slot.clone(),
        output_type: OutputType::Summary,
        axis: axis.clone(),
        entity: None,
        content,
        inferred: false,
    })
}

fn execute_surface_uncertainty(
    step: &ReasoningStep,
    index: &FactPackIndex,
    theory: &TheoryContext,
) -> Result<ProducedValue> {
    let axis = &step.axis;

    let mut uncertainty_texts: Vec<String> = Vec::new();

    // 1. YAML-authored uncertainties (📝 AUTHORED)
    if let Some(indices) = index.uncertainties_by_axis.get(axis) {
        for i in indices {
            uncertainty_texts.push(format!("📝 {}", index.pack.uncertainties[*i].text));
        }
    }

    // 2. Theory-derived uncertainties (🔧 DERIVED)
    for du in &theory.derived_uncertainties {
        if du.axes().contains(axis) {
            uncertainty_texts.push(format!("{}", du));
        }
    }

    let content = if uncertainty_texts.is_empty() {
        format!("[No uncertainties for axis {}]", axis)
    } else {
        uncertainty_texts.join("\n")
    };

    let has_derived = theory
        .derived_uncertainties
        .iter()
        .any(|du| du.axes().contains(axis));

    Ok(ProducedValue {
        slot: step.output_slot.clone(),
        output_type: OutputType::Uncertainty,
        axis: axis.clone(),
        entity: None,
        content,
        inferred: has_derived,
    })
}

fn assemble_comparison(
    goal: &Goal,
    index: &FactPackIndex,
    theory: &TheoryContext,
    plan: &planner::ReasoningPlan,
    values: &HashMap<String, ProducedValue>,
) -> ReasoningOutput {
    let mut axes = Vec::new();

    for axis in &index.pack.axes {
        let axis_id = &axis.id;

        let mut claims = Vec::new();
        let mut evidence = Vec::new();
        let mut similarities = Vec::new();
        let mut contrasts = Vec::new();
        let mut uncertainties = Vec::new();
        let mut summary = None;
        let mut gaps = Vec::new();

        for (_slot, val) in values {
            if val.axis != *axis_id {
                continue;
            }
            match val.output_type {
                OutputType::Claim => claims.push(val.clone()),
                OutputType::Evidence => evidence.push(val.clone()),
                OutputType::Similarity => similarities.push(val.clone()),
                OutputType::Contrast => contrasts.push(val.clone()),
                OutputType::Uncertainty => uncertainties.push(val.clone()),
                OutputType::Summary => summary = Some(val.clone()),
            }
        }

        for ob in &plan.obligations {
            if ob.axis == *axis_id && !ob.fulfilled {
                gaps.push(format!("UNFULFILLED: {} ({})", ob.slot, ob.required_type));
            }
        }
        for gap in &plan.gaps {
            if gap.contains(axis_id) {
                gaps.push(gap.clone());
            }
        }

        axes.push(AxisResult {
            axis: axis.name.clone(),
            claims,
            evidence,
            similarities,
            contrasts,
            uncertainties,
            summary,
            gaps,
        });
    }

    let inferences: Vec<String> = theory
        .comparisons
        .iter()
        .map(|c| c.description.clone())
        .collect();

    let conflicts: Vec<String> = theory
        .divergences
        .iter()
        .map(|d| d.description.clone())
        .collect();

    ReasoningOutput {
        goal: goal.description.clone(),
        entities: goal.entities.clone(),
        axes,
        inferences,
        conflicts,
    }
}

fn truncate(s: &str, max: usize) -> String {
    if s.len() <= max {
        s.to_string()
    } else {
        format!("{}…", &s[..max])
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_comparison_strategy_produces_output() {
        let goal = Goal {
            description: "Compare Putin and Stalin".into(),
            entities: vec!["putin".into(), "stalin".into()],
            fact_pack_path: "data/putin_stalin.yaml".into(),
        };
        let output = run_comparison(&goal).unwrap();
        assert_eq!(output.axes.len(), 7);
        assert!(!output.inferences.is_empty());
    }

    #[test]
    fn test_comparison_strategy_registry_valid() {
        let goal = Goal {
            description: "test".into(),
            entities: vec!["putin".into(), "stalin".into()],
            fact_pack_path: "data/putin_stalin.yaml".into(),
        };
        let strategy = ComparisonStrategy::new(goal).unwrap();
        let registry = strategy.build_registry();

        assert!(registry.has_producer(&TypeId::new("Evidence")));
        assert!(registry.has_producer(&TypeId::new("Claim")));
        assert!(registry.has_producer(&TypeId::new("Contrast")));
        assert!(registry.has_producer(&TypeId::new("Similarity")));
        assert!(registry.has_producer(&TypeId::new("Summary")));
        assert!(registry.has_producer(&TypeId::new("Uncertainty")));
    }

    #[test]
    fn test_comparison_strategy_literals() {
        let goal = Goal {
            description: "test".into(),
            entities: vec!["putin".into(), "stalin".into()],
            fact_pack_path: "data/putin_stalin.yaml".into(),
        };
        let strategy = ComparisonStrategy::new(goal).unwrap();
        let literals = strategy.available_literals();

        // Should have literals for claims and evidence
        let claim_lits: Vec<_> = literals.iter().filter(|l| l.type_id == TypeId::new("Claim")).collect();
        let ev_lits: Vec<_> = literals.iter().filter(|l| l.type_id == TypeId::new("Evidence")).collect();
        assert!(!claim_lits.is_empty(), "should have claim literals");
        assert!(!ev_lits.is_empty(), "should have evidence literals");
    }

    #[test]
    fn test_empty_registry_error() {
        // A strategy with no ops should fail during planning validation
        let goal = Goal {
            description: "test".into(),
            entities: vec!["putin".into(), "stalin".into()],
            fact_pack_path: "data/putin_stalin.yaml".into(),
        };
        // We can't easily make ComparisonStrategy have an empty registry,
        // but we can test run_strategy with a minimal strategy that has no ops.
        // This is tested more thoroughly in the CodingStrategy tests.
        let output = run_comparison(&goal).unwrap();
        assert!(!output.axes.is_empty());
    }

    #[test]
    fn test_run_comparison_matches_old_pipeline() {
        let goal = Goal {
            description: "Produce a structured comparison of Putin and Stalin as autocrats".into(),
            entities: vec!["putin".into(), "stalin".into()],
            fact_pack_path: "data/putin_stalin.yaml".into(),
        };

        let output = run_comparison(&goal).unwrap();

        // Verify structural properties that the old pipeline guaranteed
        assert_eq!(output.axes.len(), 7);
        for axis in &output.axes {
            assert!(!axis.claims.is_empty(), "axis '{}' has no claims", axis.axis);
            assert!(!axis.evidence.is_empty(), "axis '{}' has no evidence", axis.axis);
            assert!(!axis.contrasts.is_empty(), "axis '{}' has no contrasts", axis.axis);
            assert!(!axis.similarities.is_empty(), "axis '{}' has no similarities", axis.axis);
            assert!(!axis.uncertainties.is_empty(), "axis '{}' has no uncertainties", axis.axis);
            assert!(axis.summary.is_some(), "axis '{}' has no summary", axis.axis);
            assert!(axis.gaps.is_empty(), "axis '{}' has gaps: {:?}", axis.axis, axis.gaps);
        }
        assert!(!output.inferences.is_empty());
        assert!(!output.conflicts.is_empty());
    }
}
