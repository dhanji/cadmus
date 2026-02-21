// Trace the NL → Racket pipeline for arithmetic inputs.
// Run with: cargo test --test trace_pipeline -- --nocapture

use cadmus::nl::normalize;
use cadmus::nl::typo;
use cadmus::nl::intent;
use cadmus::nl::slots;
use cadmus::nl::dialogue::{build_workflow, workflow_to_yaml};
use cadmus::racket_strategy::{
    build_racket_registry, load_racket_facts_from_str,
    promote_inferred_ops, InferenceKind,
};
use cadmus::racket_executor::generate_racket_script;
use cadmus::registry::load_ops_pack_str;
use cadmus::workflow::{CompiledStep, CompiledWorkflow, raw_step_to_op_params};
use cadmus::type_expr::TypeExpr;
use std::collections::HashMap;

const RACKET_FACTS_YAML: &str = include_str!("../data/packs/facts/racket.facts.yaml");
const RACKET_OPS_YAML: &str = include_str!("../data/packs/ops/racket.ops.yaml");

fn trace_input(input: &str) {
    println!("╔══════════════════════════════════════════════════════════════╗");
    println!("║  Input: {:53}║", format!("{:?}", input));
    println!("╚══════════════════════════════════════════════════════════════╝");
    println!();

    // ── Step 1: NL Normalize ──
    println!("━━━ Step 1: NL Normalize + Typo Correct ━━━");
    println!();

    let first_pass = normalize::normalize(input);
    println!("  Raw tokens:       {:?}", first_pass.tokens);
    println!("  Canonical tokens: {:?}", first_pass.canonical_tokens);

    // Typo correction
    let dict = typo::domain_dict();
    let corrected = dict.correct_tokens(&first_pass.tokens);
    let changed = corrected != first_pass.tokens;
    if changed {
        println!("  Typo-corrected:   {:?}", corrected);
    } else {
        println!("  Typo correction:  (no changes)");
    }

    // Re-normalize after typo correction
    let corrected_input = corrected.iter()
        .map(|t| if t.contains(' ') { format!("\"{}\"", t) } else { t.clone() })
        .collect::<Vec<_>>()
        .join(" ");
    let normalized = normalize::normalize(&corrected_input);
    println!("  Final canonical:  {:?}", normalized.canonical_tokens);
    println!();

    // ── Step 2: Intent Recognition ──
    println!("━━━ Step 2: Intent Recognition ━━━");
    println!();

    let intent_result = intent::parse_intent(&normalized);
    println!("  Intent: {:?}", intent_result);
    println!();

    // ── Step 3: Slot Extraction ──
    println!("━━━ Step 3: Slot Extraction ━━━");
    println!();

    let extracted = slots::extract_slots(&normalized.canonical_tokens);
    println!("  Primary op:   {:?}", extracted.primary_op);
    println!("  Target path:  {:?}", extracted.target_path);
    println!("  Keywords:     {:?}", extracted.keywords);
    println!("  Patterns:     {:?}", extracted.patterns);
    println!("  Step refs:    {:?}", extracted.step_refs);
    println!("  Slots:        {:?}", extracted.slots);
    println!();

    // ── Step 4: Workflow Generation ──
    println!("━━━ Step 4: Workflow YAML Generation ━━━");
    println!();

    let op = match &intent_result {
        intent::Intent::CreateWorkflow { op, .. } => op.clone(),
        _ => None,
    };

    match build_workflow(&op, &extracted, None) {
        Ok(wf) => {
            let yaml = workflow_to_yaml(&wf);
            for line in yaml.lines() {
                println!("  {}", line);
            }
            println!();

            // ── Step 5: Inference Check ──
            println!("━━━ Step 5: Inference Check ━━━");
            println!();

            let primary_op = wf.steps.first().map(|s| s.op.as_str()).unwrap_or("?");
            println!("  Primary op: {}", primary_op);

            let reg = build_racket_registry();
            let has_meta = reg.get_poly(primary_op)
                .and_then(|e| e.meta.as_ref())
                .is_some();

            if has_meta {
                let entry = reg.get_poly(primary_op).unwrap();
                let meta = entry.meta.as_ref().unwrap();
                println!("  Status: ✅ has metasignature (defined directly)");
                let params: Vec<String> = meta.params.iter()
                    .map(|p| format!("{}:{}{}", p.name, p.type_name, if p.variadic { " ..." } else { "" }))
                    .collect();
                println!("  Params: {:?}", params);
                println!("  Return: {}", meta.return_type);
                println!("  Category: {:?}", meta.category);
                if !meta.invariants.is_empty() {
                    println!("  Invariants: {:?}", meta.invariants);
                }
            } else {
                println!("  Status: ⚠ stub — no metasignature, attempting inference...");

                let facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();

                // Run the full three-phase promotion to get the correct
                // inference path (op-symmetric → type-symmetric → op-symmetric replay)
                let mut reg_copy = build_racket_registry();
                let all_inferred = promote_inferred_ops(&mut reg_copy, &facts);
                let inferred_result = all_inferred.into_iter()
                    .find(|i| i.op_name == primary_op);

                match inferred_result {
                    Some(inferred) => {
                        let kind_desc = match &inferred.inference_kind {
                            InferenceKind::OpSymmetric =>
                                format!("{} (symmetric partner)", inferred.inferred_from),
                            InferenceKind::TypeSymmetric { class } =>
                                format!("{} (type-symmetric peer, class: {})", inferred.inferred_from, class),
                            InferenceKind::ShellSubmode { base_op, flags } =>
                                format!("{} (shell submode, flags: {})", base_op, flags),
                        };
                        println!("  ✅ Inferred from: {}", kind_desc);
                        println!("  Racket symbol: {}", inferred.racket_symbol);
                        let params: Vec<String> = inferred.meta.params.iter()
                            .map(|p| format!("{}:{}{}", p.name, p.type_name, if p.variadic { " ..." } else { "" }))
                            .collect();
                        println!("  Params: {:?}", params);
                        println!("  Return: {}", inferred.meta.return_type);
                        println!("  Category: {:?}", inferred.meta.category);
                        println!("  Effects: {:?}", inferred.meta.effects);
                        let dropped = if inferred.invariants_dropped { "yes — op-specific, not symmetric" } else { "no" };
                        println!("  Invariants transferred: {} (dropped: {})",
                            inferred.meta.invariants.len(), dropped);
                    }
                    None => {
                        println!("  ❌ No inference path available");
                        println!("  (proceeding with stub signature)");
                    }
                }
            }
            println!();

            // ── Step 6: Racket Code Generation ──
            println!("━━━ Step 6: Executable Racket Code ━━━");
            println!();

            let compiled = CompiledWorkflow {
                name: wf.workflow.clone(),
                input_type: TypeExpr::prim("Number"),
                input_description: wf.inputs.values().next().cloned().unwrap_or_default(),
                steps: wf.steps.iter().enumerate().map(|(i, raw)| {
                    let (op_name, params) = raw_step_to_op_params(raw);
                    let mut resolved = HashMap::new();
                    for (k, v) in &params {
                        if v.starts_with('$') {
                            if let Some(val) = wf.inputs.get(&v[1..]) {
                                resolved.insert(k.clone(), val.clone());
                            } else {
                                resolved.insert(k.clone(), v.clone());
                            }
                        } else {
                            resolved.insert(k.clone(), v.clone());
                        }
                    }
                    CompiledStep {
                        index: i,
                        op: op_name,
                        input_type: TypeExpr::prim("Number"),
                        output_type: TypeExpr::prim("Number"),
                        params: resolved,
                    ..Default::default()
                    }
                }).collect(),
                output_type: TypeExpr::prim("Number"),
            };

            let mut racket_reg = load_ops_pack_str(RACKET_OPS_YAML).unwrap();
            let exec_facts = load_racket_facts_from_str(RACKET_FACTS_YAML).unwrap();
            promote_inferred_ops(&mut racket_reg, &exec_facts);
            match generate_racket_script(&compiled, &wf, &racket_reg) {
                Ok(script) => {
                    for line in script.lines() {
                        println!("  {}", line);
                    }
                }
                Err(e) => {
                    println!("  ❌ Script generation failed: {}", e);
                }
            }
        }
        Err(e) => {
            println!("  ❌ Workflow generation failed: {}", e);
        }
    }

    println!();
    println!();
}

#[test]
fn trace_all_seven_inputs() {
    let inputs = [
        "Add 4 and 35 together",
        "Subtract 2 from 6",
        "Multiply 7 and 8",
        "Divide 100 by 4",
        "Sum 12 and 88",
        "Plus 1 and 99",
        "Minus 5 from 20",
    ];

    for input in &inputs {
        trace_input(input);
    }
}
