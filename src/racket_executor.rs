/// Get the first param value that is NOT a function/predicate/format param.
/// These are "control" params, not data operands.
fn first_value_param(params: &HashMap<String, String>) -> Option<&str> {
    const CONTROL_PARAMS: &[&str] = &[
        "function", "f", "predicate", "pred", "format", "fmt",
        "comparator", "init",
    ];
    for (k, v) in params {
        if !CONTROL_PARAMS.contains(&k.as_str()) {
            return Some(v.as_str());
        }
    }
    None
}
// ---------------------------------------------------------------------------
// Racket Executor
// ---------------------------------------------------------------------------
//
// Converts a CompiledWorkflow into a runnable Racket script.
//
// Design:
//   1. Each op maps to a Racket s-expression via `op_to_racket()`
//   2. `generate_racket_script()` produces a #!/usr/bin/env racket script
//      with `#lang racket` preamble and let*-bindings for multi-step chains
//   3. Single-step workflows emit a bare expression (no let overhead)
//   4. Multi-step workflows use (let* ([step_1 ...] [step_2 ...]) ...)
//
// Analogous to src/executor.rs but targets Racket instead of /bin/sh.

use std::collections::HashMap;
use std::fmt;

use crate::workflow::{CompiledWorkflow, CompiledStep, WorkflowDef};

// ---------------------------------------------------------------------------
// Error type
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub enum RacketError {
    /// Op is not recognized / has no Racket mapping
    UnknownOp(String),
    /// A required parameter is missing
    MissingParam { op: String, param: String },
}

impl fmt::Display for RacketError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RacketError::UnknownOp(op) =>
                write!(f, "unknown op '{}': no Racket expression mapping", op),
            RacketError::MissingParam { op, param } =>
                write!(f, "op '{}' requires param '{}' but it was not provided", op, param),
        }
    }
}

impl std::error::Error for RacketError {}

// ---------------------------------------------------------------------------
// RacketExpr — one step's Racket expression
// ---------------------------------------------------------------------------

/// A single Racket expression generated from a compiled step.
#[derive(Debug, Clone)]
pub struct RacketExpr {
    /// The Racket s-expression string (e.g., "(+ 4 35)")
    pub expr: String,
    /// Whether this expression references the previous step's result
    pub uses_prev: bool,
}

// ---------------------------------------------------------------------------
// Op → Racket mapping
// ---------------------------------------------------------------------------

/// Convert a compiled step into a Racket s-expression.
///
/// `prev_binding` is the variable name holding the previous step's result
/// (e.g., "step_1"). For the first step, this is None and the expression
/// uses the workflow's input values directly.
pub fn op_to_racket(
    step: &CompiledStep,
    input_values: &HashMap<String, String>,
    prev_binding: Option<&str>,
) -> Result<RacketExpr, RacketError> {
    let op = step.op.as_str();
    let params = &step.params;

    match op {
        // ===================================================================
        // Arithmetic
        // ===================================================================
        "add" => {
            let (a, b) = get_two_operands(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(+ {} {})", a, b), uses_prev: prev_binding.is_some() })
        }
        "subtract" => {
            let (a, b) = get_two_operands(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(- {} {})", a, b), uses_prev: prev_binding.is_some() })
        }
        "multiply" => {
            let (a, b) = get_two_operands(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(* {} {})", a, b), uses_prev: prev_binding.is_some() })
        }
        "divide" => {
            let (a, b) = get_two_operands(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(/ {} {})", a, b), uses_prev: prev_binding.is_some() })
        }
        "modulo" => {
            let (a, b) = get_two_operands(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(modulo {} {})", a, b), uses_prev: prev_binding.is_some() })
        }
        "expt" => {
            let (a, b) = get_two_operands(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(expt {} {})", a, b), uses_prev: prev_binding.is_some() })
        }
        "abs" => {
            let a = get_one_operand(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(abs {})", a), uses_prev: prev_binding.is_some() })
        }
        "min" => {
            let (a, b) = get_two_operands(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(min {} {})", a, b), uses_prev: prev_binding.is_some() })
        }
        "max" => {
            let (a, b) = get_two_operands(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(max {} {})", a, b), uses_prev: prev_binding.is_some() })
        }

        // ===================================================================
        // List operations
        // ===================================================================
        "cons" => {
            let (a, b) = get_two_operands(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(cons {} {})", a, b), uses_prev: prev_binding.is_some() })
        }
        "car" => {
            let a = get_one_operand(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(car {})", a), uses_prev: prev_binding.is_some() })
        }
        "cdr" => {
            let a = get_one_operand(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(cdr {})", a), uses_prev: prev_binding.is_some() })
        }
        "list_new" => {
            let a = get_one_operand(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(list {})", a), uses_prev: prev_binding.is_some() })
        }
        "append" => {
            let (a, b) = get_two_operands(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(append {} {})", a, b), uses_prev: prev_binding.is_some() })
        }
        "length" => {
            let a = get_one_operand(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(length {})", a), uses_prev: prev_binding.is_some() })
        }
        "list_reverse" => {
            let a = get_one_operand(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(reverse {})", a), uses_prev: prev_binding.is_some() })
        }
        "list_ref" => {
            let (a, b) = get_two_operands(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(list-ref {} {})", a, b), uses_prev: prev_binding.is_some() })
        }
        "member" => {
            let (a, b) = get_two_operands(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(member {} {})", a, b), uses_prev: prev_binding.is_some() })
        }
        "remove" => {
            let (a, b) = get_two_operands(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(remove {} {})", a, b), uses_prev: prev_binding.is_some() })
        }
        "sort_list" => {
            let a = get_one_operand(step, input_values, prev_binding)?;
            let cmp = params.get("comparator").map(|s| s.as_str()).unwrap_or("<");
            Ok(RacketExpr { expr: format!("(sort {} {})", a, cmp), uses_prev: prev_binding.is_some() })
        }
        "flatten" => {
            let a = get_one_operand(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(flatten {})", a), uses_prev: prev_binding.is_some() })
        }

        // ===================================================================
        // Set operations
        // ===================================================================
        "set_new" => {
            let a = get_one_operand(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(list->set {})", a), uses_prev: prev_binding.is_some() })
        }
        "set_add" => {
            let (a, b) = get_two_operands(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(set-add {} {})", a, b), uses_prev: prev_binding.is_some() })
        }
        "set_remove" => {
            let (a, b) = get_two_operands(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(set-remove {} {})", a, b), uses_prev: prev_binding.is_some() })
        }
        "set_member" => {
            let (a, b) = get_two_operands(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(set-member? {} {})", a, b), uses_prev: prev_binding.is_some() })
        }
        "set_union" => {
            let (a, b) = get_two_operands(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(set-union {} {})", a, b), uses_prev: prev_binding.is_some() })
        }
        "set_intersect" => {
            let (a, b) = get_two_operands(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(set-intersect {} {})", a, b), uses_prev: prev_binding.is_some() })
        }
        "set_subtract" => {
            let (a, b) = get_two_operands(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(set-subtract {} {})", a, b), uses_prev: prev_binding.is_some() })
        }
        "set_count" => {
            let a = get_one_operand(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(set-count {})", a), uses_prev: prev_binding.is_some() })
        }
        "set_to_list" => {
            let a = get_one_operand(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(set->list {})", a), uses_prev: prev_binding.is_some() })
        }

        // ===================================================================
        // Printing / Stdio
        // ===================================================================
        "display" => {
            let a = get_one_operand(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(display {})", a), uses_prev: prev_binding.is_some() })
        }
        "displayln" => {
            let a = get_one_operand(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(displayln {})", a), uses_prev: prev_binding.is_some() })
        }
        "newline" => {
            Ok(RacketExpr { expr: "(newline)".to_string(), uses_prev: false })
        }
        "read_line" => {
            Ok(RacketExpr { expr: "(read-line)".to_string(), uses_prev: false })
        }
        "format_string" => {
            let fmt = params.get("format").or_else(|| params.get("fmt"))
                .ok_or_else(|| RacketError::MissingParam {
                    op: "format_string".into(), param: "format".into()
                })?;
            let a = get_one_operand(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(format {} {})", racket_string(fmt), a), uses_prev: prev_binding.is_some() })
        }
        "printf" => {
            let fmt = params.get("format").or_else(|| params.get("fmt"))
                .ok_or_else(|| RacketError::MissingParam {
                    op: "printf".into(), param: "format".into()
                })?;
            let a = get_one_operand(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(printf {} {})", racket_string(fmt), a), uses_prev: prev_binding.is_some() })
        }

        // ===================================================================
        // Higher-order / Composable
        // ===================================================================
        "racket_map" => {
            let a = get_one_operand(step, input_values, prev_binding)?;
            let func = params.get("function").or_else(|| params.get("f"))
                .ok_or_else(|| RacketError::MissingParam {
                    op: "racket_map".into(), param: "function".into()
                })?;
            Ok(RacketExpr { expr: format!("(map {} {})", func, a), uses_prev: prev_binding.is_some() })
        }
        "racket_filter" => {
            let a = get_one_operand(step, input_values, prev_binding)?;
            let pred = params.get("predicate").or_else(|| params.get("pred"))
                .ok_or_else(|| RacketError::MissingParam {
                    op: "racket_filter".into(), param: "predicate".into()
                })?;
            Ok(RacketExpr { expr: format!("(filter {} {})", pred, a), uses_prev: prev_binding.is_some() })
        }
        "racket_foldl" => {
            let a = get_one_operand(step, input_values, prev_binding)?;
            let func = params.get("function").or_else(|| params.get("f"))
                .ok_or_else(|| RacketError::MissingParam {
                    op: "racket_foldl".into(), param: "function".into()
                })?;
            let init = params.get("init").unwrap_or(&"0".to_string()).clone();
            Ok(RacketExpr { expr: format!("(foldl {} {} {})", func, init, a), uses_prev: prev_binding.is_some() })
        }
        "racket_foldr" => {
            let a = get_one_operand(step, input_values, prev_binding)?;
            let func = params.get("function").or_else(|| params.get("f"))
                .ok_or_else(|| RacketError::MissingParam {
                    op: "racket_foldr".into(), param: "function".into()
                })?;
            let init = params.get("init").unwrap_or(&"0".to_string()).clone();
            Ok(RacketExpr { expr: format!("(foldr {} {} {})", func, init, a), uses_prev: prev_binding.is_some() })
        }
        "racket_for_each" => {
            let a = get_one_operand(step, input_values, prev_binding)?;
            let func = params.get("function").or_else(|| params.get("f"))
                .ok_or_else(|| RacketError::MissingParam {
                    op: "racket_for_each".into(), param: "function".into()
                })?;
            Ok(RacketExpr { expr: format!("(for-each {} {})", func, a), uses_prev: prev_binding.is_some() })
        }
        "racket_apply" => {
            let a = get_one_operand(step, input_values, prev_binding)?;
            let func = params.get("function").or_else(|| params.get("f"))
                .ok_or_else(|| RacketError::MissingParam {
                    op: "racket_apply".into(), param: "function".into()
                })?;
            Ok(RacketExpr { expr: format!("(apply {} {})", func, a), uses_prev: prev_binding.is_some() })
        }
        "andmap" => {
            let a = get_one_operand(step, input_values, prev_binding)?;
            let pred = params.get("predicate").or_else(|| params.get("pred"))
                .ok_or_else(|| RacketError::MissingParam {
                    op: "andmap".into(), param: "predicate".into()
                })?;
            Ok(RacketExpr { expr: format!("(andmap {} {})", pred, a), uses_prev: prev_binding.is_some() })
        }
        "ormap" => {
            let a = get_one_operand(step, input_values, prev_binding)?;
            let pred = params.get("predicate").or_else(|| params.get("pred"))
                .ok_or_else(|| RacketError::MissingParam {
                    op: "ormap".into(), param: "predicate".into()
                })?;
            Ok(RacketExpr { expr: format!("(ormap {} {})", pred, a), uses_prev: prev_binding.is_some() })
        }

        // ===================================================================
        // Boolean / Comparison
        // ===================================================================
        "not" => {
            let a = get_one_operand(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(not {})", a), uses_prev: prev_binding.is_some() })
        }
        "equal" => {
            let (a, b) = get_two_operands(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(equal? {} {})", a, b), uses_prev: prev_binding.is_some() })
        }
        "less_than" => {
            let (a, b) = get_two_operands(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(< {} {})", a, b), uses_prev: prev_binding.is_some() })
        }
        "greater_than" => {
            let (a, b) = get_two_operands(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(> {} {})", a, b), uses_prev: prev_binding.is_some() })
        }

        // ===================================================================
        // String operations
        // ===================================================================
        "string_append" => {
            let (a, b) = get_two_operands(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(string-append {} {})", a, b), uses_prev: prev_binding.is_some() })
        }
        "string_length" => {
            let a = get_one_operand(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(string-length {})", a), uses_prev: prev_binding.is_some() })
        }
        "number_to_string" => {
            let a = get_one_operand(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(number->string {})", a), uses_prev: prev_binding.is_some() })
        }
        "string_to_number" => {
            let a = get_one_operand(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("(string->number {})", a), uses_prev: prev_binding.is_some() })
        }

        _ => Err(RacketError::UnknownOp(op.to_string())),
    }
}

// ---------------------------------------------------------------------------
// Script generation
// ---------------------------------------------------------------------------

/// Generate a complete Racket script from a compiled workflow.
///
/// The script uses `#lang racket` and follows this structure:
/// - Single-step: bare expression wrapped in `(displayln ...)`
/// - Multi-step: `(let* ([step_1 ...] [step_2 ...] ...) (displayln step_N))`
///
/// This is the Racket analogue of `executor::generate_script()`.
pub fn generate_racket_script(
    compiled: &CompiledWorkflow,
    def: &WorkflowDef,
) -> Result<String, RacketError> {
    let mut script = String::new();

    // Shebang and preamble
    script.push_str("#!/usr/bin/env racket\n");
    script.push_str("#lang racket\n");
    script.push_str("\n");
    script.push_str(&format!(";; Generated by cadmus: {}\n", compiled.name));
    script.push_str(";;\n");

    // Collect input values for operand resolution
    let input_values = &def.inputs;

    let num_steps = compiled.steps.len();

    if num_steps == 0 {
        script.push_str(";; (no steps)\n");
        return Ok(script);
    }

    // Single-step workflow: bare expression, print result
    if num_steps == 1 {
        let step = &compiled.steps[0];
        script.push_str(&format!(";; Step 1: {}\n", step.op));
        let expr = op_to_racket(step, input_values, None)?;
        script.push_str(&format!("(displayln {})\n", expr.expr));
        return Ok(script);
    }

    // Multi-step workflow: use let* bindings
    script.push_str("(let*\n");
    script.push_str("  (\n");

    for (i, step) in compiled.steps.iter().enumerate() {
        let step_num = i + 1;
        let binding_name = format!("step_{}", step_num);
        let prev_binding = if i == 0 { None } else { Some(format!("step_{}", i)) };

        script.push_str(&format!("    ;; Step {}: {}{}\n", step_num, step.op,
            if step.is_each { " (each)" } else { "" }));

        let expr = op_to_racket(step, input_values, prev_binding.as_deref())?;

        if step.is_each {
            // Map-each: wrap in (map (lambda (_line) ...) prev)
            let prev = prev_binding.as_deref().unwrap_or("'()");
            script.push_str(&format!("    [{} (map (lambda (_line) {}) {})]\n",
                binding_name, expr.expr, prev));
        } else {
            script.push_str(&format!("    [{} {}]\n", binding_name, expr.expr));
        }
    }

    script.push_str("  )\n");

    // Print the final step's result
    let final_binding = format!("step_{}", num_steps);
    script.push_str(&format!("  (displayln {}))\n", final_binding));

    Ok(script)
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Get a single operand for a unary operation.
///
/// Priority: prev_binding > first param value > first input value
fn get_one_operand(
    step: &CompiledStep,
    input_values: &HashMap<String, String>,
    prev_binding: Option<&str>,
) -> Result<String, RacketError> {
    if let Some(prev) = prev_binding {
        return Ok(prev.to_string());
    }

    // Check params for a value operand (skip function/predicate params)
    if let Some(val) = first_value_param(&step.params) {
        return Ok(racket_value(val));
    }

    // Fall back to first input value
    if let Some(val) = input_values.values().next() {
        return Ok(racket_value(val));
    }

    Err(RacketError::MissingParam {
        op: step.op.clone(),
        param: "operand".into(),
    })
}

/// Get two operands for a binary operation.
///
/// For arithmetic ops, operands come from:
/// 1. Named params "x" and "y" (or "a" and "b")
/// 2. Params "left" and "right"
/// 3. First two input values
/// 4. prev_binding as first operand + first param/input as second
fn get_two_operands(
    step: &CompiledStep,
    input_values: &HashMap<String, String>,
    prev_binding: Option<&str>,
) -> Result<(String, String), RacketError> {
    let params = &step.params;

    // Try named params: x/y, a/b, left/right
    if let (Some(x), Some(y)) = (params.get("x"), params.get("y")) {
        return Ok((racket_value(x), racket_value(y)));
    }
    if let (Some(a), Some(b)) = (params.get("a"), params.get("b")) {
        return Ok((racket_value(a), racket_value(b)));
    }
    if let (Some(l), Some(r)) = (params.get("left"), params.get("right")) {
        return Ok((racket_value(l), racket_value(r)));
    }

    // If prev_binding exists, use it as first operand
    if let Some(prev) = prev_binding {
        // Second operand from params or inputs
        if let Some(val) = params.values().next() {
            return Ok((prev.to_string(), racket_value(val)));
        }
        if let Some(val) = input_values.values().next() {
            return Ok((prev.to_string(), racket_value(val)));
        }
    }

    // Try to get two values from inputs
    let mut vals: Vec<&String> = input_values.values().collect();
    vals.sort(); // deterministic order
    if vals.len() >= 2 {
        return Ok((racket_value(vals[0]), racket_value(vals[1])));
    }

    // Single param with two space-separated values (e.g., "4 35")
    if let Some(val) = params.values().next() {
        let parts: Vec<&str> = val.split_whitespace().collect();
        if parts.len() >= 2 {
            return Ok((racket_value(parts[0]), racket_value(parts[1])));
        }
    }

    Err(RacketError::MissingParam {
        op: step.op.clone(),
        param: "two operands".into(),
    })
}

/// Convert a string value to a Racket literal.
///
/// Numbers pass through as-is. Strings get quoted.
/// Racket identifiers (starting with lowercase or special chars) pass through.
fn racket_value(s: &str) -> String {
    // If it parses as a number, use it directly
    if s.parse::<f64>().is_ok() {
        return s.to_string();
    }
    // If it looks like a Racket expression (starts with '(' or '#' or '\'')
    if s.starts_with('(') || s.starts_with('#') || s.starts_with('\'') {
        return s.to_string();
    }
    // Otherwise, quote it as a string
    racket_string(s)
}

/// Quote a string for Racket.
fn racket_string(s: &str) -> String {
    // If already quoted, return as-is
    if s.starts_with('"') && s.ends_with('"') {
        return s.to_string();
    }
    format!("\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\""))
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;
    use crate::workflow::CompiledStep;
    use crate::type_expr::TypeExpr;

    fn make_step(op: &str, params: Vec<(&str, &str)>) -> CompiledStep {
        CompiledStep {
            index: 0,
            op: op.to_string(),
            is_each: false,
            input_type: TypeExpr::prim("Number"),
            output_type: TypeExpr::prim("Number"),
            params: params.into_iter().map(|(k, v)| (k.to_string(), v.to_string())).collect(),
        }
    }

    fn make_inputs(pairs: Vec<(&str, &str)>) -> HashMap<String, String> {
        pairs.into_iter().map(|(k, v)| (k.to_string(), v.to_string())).collect()
    }

    // --- op_to_racket tests ---

    #[test]
    fn test_add_with_named_params() {
        let step = make_step("add", vec![("x", "4"), ("y", "35")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None).unwrap();
        assert_eq!(expr.expr, "(+ 4 35)");
        assert!(!expr.uses_prev);
    }

    #[test]
    fn test_subtract_with_named_params() {
        let step = make_step("subtract", vec![("x", "6"), ("y", "2")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None).unwrap();
        assert_eq!(expr.expr, "(- 6 2)");
    }

    #[test]
    fn test_multiply_with_named_params() {
        let step = make_step("multiply", vec![("x", "3"), ("y", "7")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None).unwrap();
        assert_eq!(expr.expr, "(* 3 7)");
    }

    #[test]
    fn test_divide_with_named_params() {
        let step = make_step("divide", vec![("x", "10"), ("y", "2")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None).unwrap();
        assert_eq!(expr.expr, "(/ 10 2)");
    }

    #[test]
    fn test_add_with_prev_binding() {
        let step = make_step("add", vec![("y", "10")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, Some("step_1")).unwrap();
        assert_eq!(expr.expr, "(+ step_1 10)");
        assert!(expr.uses_prev);
    }

    #[test]
    fn test_display_with_prev() {
        let step = make_step("display", vec![]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, Some("step_1")).unwrap();
        assert_eq!(expr.expr, "(display step_1)");
    }

    #[test]
    fn test_displayln_with_value() {
        let step = make_step("displayln", vec![("value", "hello")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None).unwrap();
        assert_eq!(expr.expr, "(displayln \"hello\")");
    }

    #[test]
    fn test_unknown_op() {
        let step = make_step("nonexistent_op", vec![]);
        let inputs = make_inputs(vec![]);
        let result = op_to_racket(&step, &inputs, None);
        assert!(result.is_err());
        match result.unwrap_err() {
            RacketError::UnknownOp(name) => assert_eq!(name, "nonexistent_op"),
            _ => panic!("expected UnknownOp"),
        }
    }

    #[test]
    fn test_racket_filter_with_pred() {
        let step = make_step("racket_filter", vec![("predicate", "even?")]);
        let inputs = make_inputs(vec![("lst", "'(1 2 3 4 5)")]);
        let expr = op_to_racket(&step, &inputs, None).unwrap();
        assert_eq!(expr.expr, "(filter even? '(1 2 3 4 5))");
    }

    #[test]
    fn test_racket_map_with_func() {
        let step = make_step("racket_map", vec![("function", "add1")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, Some("step_1")).unwrap();
        assert_eq!(expr.expr, "(map add1 step_1)");
    }

    #[test]
    fn test_racket_foldl_with_init() {
        let step = make_step("racket_foldl", vec![("function", "+"), ("init", "0")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, Some("step_1")).unwrap();
        assert_eq!(expr.expr, "(foldl + 0 step_1)");
    }

    #[test]
    fn test_set_union() {
        let step = make_step("set_union", vec![("x", "(set 1 2 3)"), ("y", "(set 3 4 5)")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None).unwrap();
        assert_eq!(expr.expr, "(set-union (set 1 2 3) (set 3 4 5))");
    }

    #[test]
    fn test_cons_with_inputs() {
        let step = make_step("cons", vec![("x", "42"), ("y", "'()")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None).unwrap();
        assert_eq!(expr.expr, "(cons 42 '())");
    }

    #[test]
    fn test_racket_value_number() {
        assert_eq!(racket_value("42"), "42");
        assert_eq!(racket_value("3.14"), "3.14");
        assert_eq!(racket_value("-7"), "-7");
    }

    #[test]
    fn test_racket_value_string() {
        assert_eq!(racket_value("hello"), "\"hello\"");
    }

    #[test]
    fn test_racket_value_expression() {
        assert_eq!(racket_value("(+ 1 2)"), "(+ 1 2)");
        assert_eq!(racket_value("#t"), "#t");
        assert_eq!(racket_value("'(1 2 3)"), "'(1 2 3)");
    }

    // --- generate_racket_script tests ---

    #[test]
    fn test_single_step_script() {
        let compiled = CompiledWorkflow {
            name: "add numbers".to_string(),
            input_type: TypeExpr::prim("Number"),
            input_description: "4".to_string(),
            steps: vec![
                CompiledStep {
                    index: 0,
                    op: "add".to_string(),
                    is_each: false,
                    input_type: TypeExpr::prim("Number"),
                    output_type: TypeExpr::prim("Number"),
                    params: vec![("x".into(), "4".into()), ("y".into(), "35".into())].into_iter().collect(),
                },
            ],
            output_type: TypeExpr::prim("Number"),
        };
        let def = WorkflowDef {
            workflow: "add numbers".to_string(),
            inputs: vec![("x".into(), "4".into()), ("y".into(), "35".into())].into_iter().collect(),
            steps: vec![],
        };
        let script = generate_racket_script(&compiled, &def).unwrap();
        assert!(script.contains("#lang racket"));
        assert!(script.contains("(displayln (+ 4 35))"));
        // Single-step: no let* binding
        assert!(!script.contains("let*"));
    }

    #[test]
    fn test_multi_step_script() {
        let compiled = CompiledWorkflow {
            name: "add then display".to_string(),
            input_type: TypeExpr::prim("Number"),
            input_description: "4".to_string(),
            steps: vec![
                CompiledStep {
                    index: 0,
                    op: "add".to_string(),
                    is_each: false,
                    input_type: TypeExpr::prim("Number"),
                    output_type: TypeExpr::prim("Number"),
                    params: vec![("x".into(), "4".into()), ("y".into(), "35".into())].into_iter().collect(),
                },
                CompiledStep {
                    index: 1,
                    op: "multiply".to_string(),
                    is_each: false,
                    input_type: TypeExpr::prim("Number"),
                    output_type: TypeExpr::prim("Number"),
                    params: vec![("y".into(), "2".into())].into_iter().collect(),
                },
            ],
            output_type: TypeExpr::prim("Number"),
        };
        let def = WorkflowDef {
            workflow: "add then multiply".to_string(),
            inputs: HashMap::new(),
            steps: vec![],
        };
        let script = generate_racket_script(&compiled, &def).unwrap();
        assert!(script.contains("#lang racket"));
        assert!(script.contains("let*"));
        assert!(script.contains("[step_1 (+ 4 35)]"));
        assert!(script.contains("[step_2 (* step_1 2)]"));
        assert!(script.contains("(displayln step_2)"));
    }

    #[test]
    fn test_empty_workflow_script() {
        let compiled = CompiledWorkflow {
            name: "empty".to_string(),
            input_type: TypeExpr::prim("Number"),
            input_description: "".to_string(),
            steps: vec![],
            output_type: TypeExpr::prim("Number"),
        };
        let def = WorkflowDef {
            workflow: "empty".to_string(),
            inputs: HashMap::new(),
            steps: vec![],
        };
        let script = generate_racket_script(&compiled, &def).unwrap();
        assert!(script.contains(";; (no steps)"));
    }

    #[test]
    fn test_string_operations_script() {
        let step = make_step("string_append", vec![("x", "hello"), ("y", " world")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None).unwrap();
        assert_eq!(expr.expr, "(string-append \"hello\" \" world\")");
    }

    #[test]
    fn test_number_to_string() {
        let step = make_step("number_to_string", vec![("value", "42")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None).unwrap();
        assert_eq!(expr.expr, "(number->string 42)");
    }

    #[test]
    fn test_abs_operation() {
        let step = make_step("abs", vec![("value", "-5")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None).unwrap();
        assert_eq!(expr.expr, "(abs -5)");
    }

    #[test]
    fn test_equal_operation() {
        let step = make_step("equal", vec![("x", "42"), ("y", "42")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None).unwrap();
        assert_eq!(expr.expr, "(equal? 42 42)");
    }

    #[test]
    fn test_less_than_operation() {
        let step = make_step("less_than", vec![("x", "1"), ("y", "2")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None).unwrap();
        assert_eq!(expr.expr, "(< 1 2)");
    }
}
