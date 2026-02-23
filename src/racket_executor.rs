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
// Converts a CompiledPlan into a runnable Racket script.
//
// Design:
//   1. Each op maps to a Racket s-expression via `op_to_racket()`
//   2. `generate_racket_script()` produces a #!/usr/bin/env racket script
//      with `#lang racket` preamble and let*-bindings for multi-step chains
//   3. Single-step plans emit a bare expression (no let overhead)
//   4. Multi-step plans use (let* ([step-1 ...] [step-2 ...]) ...)
//
// The executor is data-driven: it reads the Racket symbol and arity from
// the OperationRegistry (populated from racket.ops.yaml) instead of
// hardcoding op→symbol mappings. Only a handful of "special" ops that
// need extra parameters (function, predicate, format, init) have explicit
// match arms.

use std::collections::HashMap;


use crate::registry::OperationRegistry;
use crate::plan::{CompiledPlan, CompiledStep, PlanDef, PlanInput};

// ---------------------------------------------------------------------------
// Racket registry builder (shared by all call sites)
// ---------------------------------------------------------------------------

/// Build a fully configured Racket `OperationRegistry`.
///
/// Loads the racket ops pack, promotes inferred ops from the racket facts
/// pack, and discovers shell submodes from the macOS CLI facts pack.
///
/// This is the single source of truth — used by `DefaultFrame::invoke`,
/// `handle_approve`, and `run_plan_mode` instead of duplicating the
/// setup inline.
pub fn build_racket_registry() -> OperationRegistry {
    let mut reg = crate::registry::load_ops_pack_str(
        include_str!("../data/packs/ops/racket.ops.yaml")
    ).unwrap_or_default();

    let racket_facts_yaml = include_str!("../data/packs/facts/racket.facts.yaml");
    if let Ok(facts) = crate::racket_strategy::load_racket_facts_from_str(racket_facts_yaml) {
        crate::racket_strategy::promote_inferred_ops(&mut reg, &facts);

        let cli_yaml = include_str!("../data/packs/facts/macos_cli.facts.yaml");
        if let Ok(cli_pack) = serde_yaml::from_str::<crate::fact_pack::FactPack>(cli_yaml) {
            let cli_facts = crate::fact_pack::FactPackIndex::build(cli_pack);
            crate::racket_strategy::discover_shell_submodes(
                &mut reg, &facts, &cli_facts,
            );
        }
    }

    reg
}

// ---------------------------------------------------------------------------
// Shell op helpers
// ---------------------------------------------------------------------------

/// The Racket preamble for shell-callable ops.
///
/// Defines helper functions for executing shell commands and capturing output.
/// Only emitted when the script contains at least one shell op.
const SHELL_PREAMBLE: &str = r#"(require racket/system)

;; Shell helpers: execute command, capture output as string or lines
(define (shell-exec cmd)
  (with-output-to-string (lambda () (system cmd))))
(define (shell-lines cmd)
  (filter (lambda (s) (not (string=? s "")))
          (string-split (shell-exec cmd) "\n")))
(define (shell-quote s)
  (string-append "'" (string-replace s "'" "'\\''") "'"))
"#;

/// Check if an op is a shell-callable op by examining its metasignature.
///
/// Shell ops have `category: shell` in their meta.
fn is_shell_op(op: &str, registry: &OperationRegistry) -> bool {
    registry.get_poly(op)
        .and_then(|e| e.meta.as_ref())
        .and_then(|m| m.category.as_deref())
        .map(|c| c == "shell")
        .unwrap_or(false)
}

/// Extract shell metadata from an op's metasignature invariants.
///
/// Returns (base_command, flags) if the op has shell metadata.
/// Base ops have no flags; submode ops have both.
fn extract_shell_meta(op: &str, registry: &OperationRegistry) -> Option<(String, Option<String>)> {
    let meta = registry.get_poly(op)?.meta.as_ref()?;

    let mut base_command = None;
    let mut flags = None;

    for inv in &meta.invariants {
        if let Some(cmd) = inv.strip_prefix("base_command: ") {
            base_command = Some(cmd.to_string());
        }
        if let Some(f) = inv.strip_prefix("flags: ") {
            flags = Some(f.to_string());
        }
    }

    // For base ops (anchors), derive command from the racket_symbol
    // e.g., "shell-ls" → "ls"
    if base_command.is_none() {
        let sym = registry.get_poly(op)?.racket_symbol.as_deref()?;
        if sym.starts_with("shell-") {
            base_command = Some(sym["shell-".len()..].to_string());
        }
    }

    base_command.map(|cmd| (cmd, flags))
}

/// Check if any step in a compiled plan uses a shell op or subsumed fs_op
/// (all of which need the shell preamble).
fn has_shell_ops(compiled: &CompiledPlan, registry: &OperationRegistry) -> bool {
    compiled.steps.iter().any(|s| {
        is_shell_op(&s.op, registry)
            || crate::type_lowering::is_subsumed(&s.op)
    })
}

// ---------------------------------------------------------------------------
// Seq/List output detection — reads existing metasigs
// ---------------------------------------------------------------------------

/// Check whether a compiled step produces a list/sequence output at the
/// Racket level.
///
/// Uses two sources of truth (no new fields needed):
///   1. If the op is subsumed, look up the shell op's `return_type` in its
///      metasig from the registry (e.g., shell_find → "List(String)").
///   2. Fall back to the CompiledStep's `output_type` from the plan
///      compiler (e.g., `Seq(Entry(Name, Bytes))`).
///
/// Racket-native ops (filter, find_matching, sort_by in pipeline) also
/// produce lists — their CompiledStep.output_type is Seq(...).
pub fn is_seq_output(step: &CompiledStep, registry: &OperationRegistry) -> bool {
    // Path 1: Check the shell op's metasig return_type via subsumption map.
    // This is the most authoritative source for subsumed ops since it reflects
    // the actual Racket-level type (List(String) vs String).
    if let Some(entry) = crate::type_lowering::lookup_subsumption(&step.op) {
        if let Some(poly) = registry.get_poly(entry.shell_op) {
            if let Some(meta) = &poly.meta {
                let rt = &meta.return_type;
                if rt.starts_with("List(") || rt.starts_with("Seq(") {
                    return true;
                }
                // Explicit non-list return type (e.g., "String" for shell_du)
                return false;
            }
        }
    }

    // Path 2: Fall back to the CompiledStep's output_type from the plan
    // compiler. This covers Racket-native ops and any op not in the
    // subsumption map.
    step.output_type.is_seq_or_list()
}

// ---------------------------------------------------------------------------
// Tier 1: Subsumed fs_op → shell-op codegen (via type_lowering map)
// ---------------------------------------------------------------------------
//
// World-touching fs_ops (walk_tree, list_dir, etc.) are subsumed by shell
// ops (shell_find, shell_ls, etc.). When the executor encounters a subsumed
// fs_op, it delegates to the shell-op codegen path — the same infrastructure
// used for first-class shell ops.

/// Generate a Racket expression for a subsumed fs_op by delegating to
/// the shell-op codegen path.
///
/// Looks up the SubsumptionEntry to find the shell op, then uses
/// `extract_shell_meta()` on the shell op to get the base command and
/// flags. Falls back to deriving the command from the shell op name
/// if the shell op isn't in the registry (e.g., when called from the
/// NL path with a partial registry).
fn subsumed_op_to_racket(
    step: &CompiledStep,
    entry: &crate::type_lowering::SubsumptionEntry,
    input_values: &[PlanInput],
    prev_binding: Option<&str>,
    registry: &OperationRegistry,
    prev_is_seq: bool,
    bindings: &HashMap<String, String>,
) -> Result<RacketExpr, RacketError> {
    let uses_prev = prev_binding.is_some();
    let path = fs_path_operand(prev_binding, input_values, bindings);

    // Try to get the base command from the shell op's metadata in the registry.
    // If the shell op is registered, extract_shell_meta gives us the command + flags.
    // If not (partial registry), derive from the shell op name: "shell_find" → "find".
    let (base_cmd, flags) = extract_shell_meta(entry.shell_op, registry)
        .unwrap_or_else(|| {
            let cmd = entry.shell_op
                .strip_prefix("shell_")
                .unwrap_or(entry.shell_op)
                .to_string();
            (cmd, None)
        });

    let cmd_parts = if let Some(ref f) = flags {
        format!("{} {}", base_cmd, f)
    } else {
        base_cmd
    };

    // Special handling for search_content (binary: pattern + path)
    // Special handling for pack_* ops: take file list from prev, output from param
    if step.op.starts_with("pack_") {
        let output = step.params.get("output")
            .map(|s| s.as_str())
            .unwrap_or("output.zip");
        let prev = prev_binding.unwrap_or("'()");
        // Generate: zip -r output.cbz file1 file2 ...
        // Using string-join to pass all files as arguments
        let expr = format!(
            "(shell-exec (string-append \"{} \" (shell-quote {}) \" \" (string-join (map shell-quote {}) \" \")))",
            cmd_parts, racket_string(output), prev
        );
        return Ok(RacketExpr { expr, uses_prev: true });
    }

    if entry.arity == 2 {
        let pattern = step.params.get("pattern")
            .ok_or_else(|| RacketError::MissingParam {
                op: step.op.clone(),
                param: "pattern".to_string(),
            })?;

        // Seq→String bridge for binary ops: when the path argument is a list
        // (e.g., walk_tree output), iterate over each file individually.
        // E.g.: (append-map (lambda (_f) (shell-lines (string-append "grep "
        //          (shell-quote pattern) " " (shell-quote _f)))) step-1)
        if prev_is_seq && prev_binding.is_some() {
            let prev = prev_binding.unwrap();
            let expr = format!(
                "(append-map (lambda (_f) (shell-lines (string-append \"{} \" (shell-quote {}) \" \" (shell-quote _f)))) {})",
                cmd_parts, racket_string(pattern), prev
            );
            return Ok(RacketExpr { expr, uses_prev: true });
        }

        // Normal binary path: prev is a scalar string or no prev
        let expr = format!(
            "(shell-lines (string-append \"{} \" (shell-quote {}) \" \" (shell-quote {})))",
            cmd_parts, racket_string(pattern), path
        );
        return Ok(RacketExpr { expr, uses_prev });
    }

    // Unary shell op: (shell-lines (string-append "cmd " (shell-quote path)))
    // Seq→String bridge for unary ops: when the path argument is a list,
    // iterate over each item. E.g.:
    //   (append-map (lambda (_f) (shell-lines (string-append "cat " (shell-quote _f)))) step-1)
    if prev_is_seq && prev_binding.is_some() {
        let prev = prev_binding.unwrap();
        let expr = format!(
            "(append-map (lambda (_f) (shell-lines (string-append \"{} \" (shell-quote _f)))) {})",
            cmd_parts, prev
        );
        return Ok(RacketExpr { expr, uses_prev: true });
    }

    // Normal unary path: prev is a scalar string or no prev
    let expr = format!(
        "(shell-lines (string-append \"{} \" (shell-quote {})))",
        cmd_parts, path
    );
    Ok(RacketExpr { expr, uses_prev })
}

// ---------------------------------------------------------------------------
// Tier 2: Racket-native fs_op codegen (via type_lowering map)
// ---------------------------------------------------------------------------
//
// Intermediate-logic fs_ops (filter, find_matching, unique, etc.) map to
// Racket's own primitives. No shell subprocess — these operate on in-memory
// List(String) data from a prior shell bridge step.

/// Generate a Racket expression for a Racket-native fs_op.
///
/// These ops operate on in-memory data (typically the result of a prior
/// shell bridge step) using Racket's built-in functions.
fn racket_native_op_to_racket(
    step: &CompiledStep,
    kind: &crate::type_lowering::RacketNativeKind,
    _input_values: &[PlanInput],
    prev_binding: Option<&str>,
) -> Result<RacketExpr, RacketError> {
    use crate::type_lowering::RacketNativeKind;

    let params = &step.params;
    let prev = prev_binding.unwrap_or_else(|| {
        // If no prev binding, try to get a path from inputs
        // (shouldn't normally happen for native ops, but be safe)
        "'()"
    });

    match kind {
        RacketNativeKind::FilterPredicate => {
            let exclude = params.get("exclude");
            let pattern = exclude
                .or_else(|| params.get("pattern"))
                .or_else(|| params.get("extension"))
                .ok_or_else(|| RacketError::MissingParam {
                    op: step.op.clone(),
                    param: "pattern".to_string(),
                })?;
            let grep_pattern = glob_to_grep(pattern);
            let negate = if exclude.is_some() { "not " } else { "" };
            let expr = format!(
                "(filter (lambda (line) ({}regexp-match? (regexp {}) line)) {})",
                negate, racket_string(&grep_pattern), prev
            );
            Ok(RacketExpr { expr, uses_prev: true })
        }
        RacketNativeKind::SortComparator => {
            Ok(RacketExpr {
                expr: format!("(sort {} string<?)", prev),
                uses_prev: true,
            })
        }
        RacketNativeKind::RemoveDuplicates => {
            Ok(RacketExpr {
                expr: format!("(remove-duplicates {})", prev),
                uses_prev: true,
            })
        }
        RacketNativeKind::Length => {
            Ok(RacketExpr {
                expr: format!("(length {})", prev),
                uses_prev: true,
            })
        }
        RacketNativeKind::Take => {
            let n = params.get("count")
                .or_else(|| params.get("n"))
                .map(|s| s.as_str())
                .unwrap_or("10");
            Ok(RacketExpr {
                expr: format!("(take {} {})", prev, n),
                uses_prev: true,
            })
        }
        RacketNativeKind::TakeRight => {
            let n = params.get("count")
                .or_else(|| params.get("n"))
                .map(|s| s.as_str())
                .unwrap_or("10");
            Ok(RacketExpr {
                expr: format!("(take-right {} {})", prev, n),
                uses_prev: true,
            })
        }
        RacketNativeKind::Flatten => {
            // In the flat List(String) world, flatten is identity
            Ok(RacketExpr {
                expr: prev.to_string(),
                uses_prev: true,
            })
        }
        RacketNativeKind::Append => {
            // (append lst1 lst2) — concatenate two lists
            // Second list comes from inputs if available
            let other = "'()".to_string();
            Ok(RacketExpr {
                expr: format!("(append {} {})", prev, other),
                uses_prev: true,
            })
        }
        RacketNativeKind::Map => {
            // (map f lst) — identity map in the absence of a specific transform
            Ok(RacketExpr {
                expr: format!("(map values {})", prev),
                uses_prev: true,
            })
        }
        RacketNativeKind::FlattenSeq => {
            // (apply append lst) — flatten Seq(Seq(a)) → Seq(a)
            Ok(RacketExpr {
                expr: format!("(apply append {})", prev),
                uses_prev: true,
            })
        }
        RacketNativeKind::EnumerateEntries => {
            // Rename entries with zero-padded sequential numbers.
            // Each entry is a filename string; we preserve the extension
            // and replace the basename with a padded index.
            // (for/list ([f (in-list lst)] [i (in-naturals 1)])
            //   (let ([ext (path-get-extension (string->path f))])
            //     (string-append (~r i #:min-width 4 #:pad-string "0")
            //                    (if ext (bytes->string/utf-8 ext) ""))))
            Ok(RacketExpr {
                expr: format!(
                    "(for/list ([f (in-list {})] [i (in-naturals 1)]) (let ([ext (path-get-extension (string->path f))]) (string-append (~r i #:min-width 4 #:pad-string \"0\") (if ext (bytes->string/utf-8 ext) \"\"))))",
                    prev
                ),
                uses_prev: true,
            })
        }
    }
}

/// Resolve the path operand for a filesystem op.
/// Returns a Racket expression: either a variable name (prev binding)
/// or a quoted string literal from the plan inputs.
fn fs_path_operand(prev: Option<&str>, inputs: &[PlanInput], bindings: &HashMap<String, String>) -> String {
    if let Some(p) = prev {
        return p.to_string();
    }
    // Resolve from bindings via the calling frame
    // Try each input name in order — first bound value wins
    for input in inputs {
        if let Some(bound) = bindings.get(&input.name) {
            return racket_string(&expand_tilde(bound));
        }
    }
    // No bindings — use default
    racket_string(".")
}

/// Expand a leading `~` or `~/` to the user's home directory.
///
/// Shell-quoted strings (via `shell-quote`) prevent tilde expansion,
/// so we resolve `~` at codegen time to an absolute path.
fn expand_tilde(path: &str) -> String {
    if path == "~" || path.starts_with("~/") {
        if let Some(home) = std::env::var_os("HOME") {
            let home = home.to_string_lossy();
            return if path == "~" {
                home.into_owned()
            } else {
                format!("{}{}", home, &path[1..])
            };
        }
    }
    path.to_string()
}


use crate::shell_helpers::glob_to_grep;

// ---------------------------------------------------------------------------
// Error type
// ---------------------------------------------------------------------------

/// Backward-compatible alias for the shared codegen error type.
pub type RacketError = crate::shell_helpers::CodegenError;

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
// Op → Racket mapping (data-driven)
// ---------------------------------------------------------------------------

/// Convert a compiled step into a Racket s-expression.
///
/// Uses the `OperationRegistry` to look up the Racket symbol and arity for
/// each op. Only "special" ops that need extra parameters (function, predicate,
/// format, init, comparator) have explicit match arms.
///
/// `prev_binding` is the variable name holding the previous step's result
/// (e.g., "step-1"). For the first step, this is None and the expression
/// uses the plan's input values directly.
///
/// `prev_is_seq` indicates whether the previous step's output is a list/sequence
/// (checked via `is_seq_output`). Used to bridge Seq→String type mismatches.
pub fn op_to_racket(
    step: &CompiledStep,
    input_values: &[PlanInput],
    prev_binding: Option<&str>,
    registry: &OperationRegistry,
    prev_is_seq: bool,
    bindings: &HashMap<String, String>,
) -> Result<RacketExpr, RacketError> {
    let op = step.op.as_str();
    let params = &step.params;

    // -----------------------------------------------------------------------
    // Special ops: these need extra parameters beyond simple operands.
    // They can't be handled by the generic data-driven path.
    // -----------------------------------------------------------------------
    match op {
        "sort_list" => {
            let sym = registry.racket_symbol(op).unwrap_or("sort");
            let a = get_one_operand(step, input_values, prev_binding)?;
            let cmp = params.get("comparator").map(|s| s.as_str()).unwrap_or("<");
            return Ok(RacketExpr { expr: format!("({} {} {})", sym, a, cmp), uses_prev: prev_binding.is_some() });
        }
        "format_string" | "printf" => {
            let sym = registry.racket_symbol(op).unwrap_or(op);
            let fmt = params.get("format").or_else(|| params.get("fmt"))
                .ok_or_else(|| RacketError::MissingParam {
                    op: op.into(), param: "format".into()
                })?;
            let a = get_one_operand(step, input_values, prev_binding)?;
            return Ok(RacketExpr { expr: format!("({} {} {})", sym, racket_string(fmt), a), uses_prev: prev_binding.is_some() });
        }
        "racket_map" | "racket_for_each" | "racket_apply" => {
            let sym = registry.racket_symbol(op).unwrap_or(op);
            let a = get_one_operand(step, input_values, prev_binding)?;
            let func = params.get("function").or_else(|| params.get("f"))
                .ok_or_else(|| RacketError::MissingParam {
                    op: op.into(), param: "function".into()
                })?;
            return Ok(RacketExpr { expr: format!("({} {} {})", sym, func, a), uses_prev: prev_binding.is_some() });
        }
        "racket_filter" | "andmap" | "ormap" => {
            let sym = registry.racket_symbol(op).unwrap_or(op);
            let a = get_one_operand(step, input_values, prev_binding)?;
            let pred = params.get("predicate").or_else(|| params.get("pred"))
                .ok_or_else(|| RacketError::MissingParam {
                    op: op.into(), param: "predicate".into()
                })?;
            return Ok(RacketExpr { expr: format!("({} {} {})", sym, pred, a), uses_prev: prev_binding.is_some() });
        }
        "racket_foldl" | "racket_foldr" => {
            let sym = registry.racket_symbol(op).unwrap_or(op);
            let a = get_one_operand(step, input_values, prev_binding)?;
            let func = params.get("function").or_else(|| params.get("f"))
                .ok_or_else(|| RacketError::MissingParam {
                    op: op.into(), param: "function".into()
                })?;
            let init = params.get("init").unwrap_or(&"0".to_string()).clone();
            return Ok(RacketExpr { expr: format!("({} {} {} {})", sym, func, init, a), uses_prev: prev_binding.is_some() });
        }
        _ => {}
    }

    // -----------------------------------------------------------------------
    // Iteration forms: for/fold, for/list, for/sum, for/product, for/and, for/or
    // -----------------------------------------------------------------------
    match op {
        "for_fold" => {
            let bindings = params.get("bindings")
                .ok_or_else(|| RacketError::MissingParam { op: op.into(), param: "bindings".into() })?;
            let sequence = params.get("sequence")
                .ok_or_else(|| RacketError::MissingParam { op: op.into(), param: "sequence".into() })?;
            let body = params.get("body")
                .ok_or_else(|| RacketError::MissingParam { op: op.into(), param: "body".into() })?;
            let var = params.get("var").map(|s| s.as_str()).unwrap_or("x");
            let expr = format!("(for/fold ({}) ([{} {}]) {})", bindings, var, sequence, body);
            return Ok(RacketExpr { expr, uses_prev: false });
        }
        "for_list" => {
            let sequence = params.get("sequence")
                .ok_or_else(|| RacketError::MissingParam { op: op.into(), param: "sequence".into() })?;
            let body = params.get("body")
                .ok_or_else(|| RacketError::MissingParam { op: op.into(), param: "body".into() })?;
            let var = params.get("var").map(|s| s.as_str()).unwrap_or("x");
            let expr = format!("(for/list ([{} {}]) {})", var, sequence, body);
            return Ok(RacketExpr { expr, uses_prev: false });
        }
        "for_sum" | "for_product" | "for_and" | "for_or" => {
            let sym = registry.racket_symbol(op).unwrap_or(op);
            let sequence = params.get("sequence")
                .ok_or_else(|| RacketError::MissingParam { op: op.into(), param: "sequence".into() })?;
            let body = params.get("body")
                .ok_or_else(|| RacketError::MissingParam { op: op.into(), param: "body".into() })?;
            let var = params.get("var").map(|s| s.as_str()).unwrap_or("x");
            let expr = format!("({} ([{} {}]) {})", sym, var, sequence, body);
            return Ok(RacketExpr { expr, uses_prev: false });
        }
        "iterate" => {
            let name = params.get("name")
                .ok_or_else(|| RacketError::MissingParam { op: op.into(), param: "name".into() })?;
            let bindings = params.get("bindings")
                .ok_or_else(|| RacketError::MissingParam { op: op.into(), param: "bindings".into() })?;
            let body = params.get("body")
                .ok_or_else(|| RacketError::MissingParam { op: op.into(), param: "body".into() })?;
            // Parse bindings: "a=48 b=18" → ([a 48] [b 18])
            let binding_parts: Vec<String> = bindings.split_whitespace()
                .map(|pair| {
                    if let Some((var, val)) = pair.split_once('=') {
                        format!("[{} {}]", var, racket_value(val))
                    } else {
                        format!("[{} 0]", pair)
                    }
                })
                .collect();
            let expr = format!("(let {} ({}) {})", name, binding_parts.join(" "), body);
            return Ok(RacketExpr { expr, uses_prev: false });
        }
        _ => {}
    }

    // -----------------------------------------------------------------------
    // Control flow and binding: conditional, if_then, let_bind, begin, define
    // -----------------------------------------------------------------------
    match op {
        "conditional" => {
            let clauses = params.get("clauses")
                .ok_or_else(|| RacketError::MissingParam { op: op.into(), param: "clauses".into() })?;
            let expr = format!("(cond {})", clauses);
            return Ok(RacketExpr { expr, uses_prev: false });
        }
        "if_then" => {
            let test = params.get("test")
                .ok_or_else(|| RacketError::MissingParam { op: op.into(), param: "test".into() })?;
            let then = params.get("then")
                .ok_or_else(|| RacketError::MissingParam { op: op.into(), param: "then".into() })?;
            let else_branch = params.get("else").map(|s| s.as_str()).unwrap_or("#f");
            let expr = format!("(if {} {} {})", test, then, else_branch);
            return Ok(RacketExpr { expr, uses_prev: false });
        }
        "let_bind" => {
            let bindings = params.get("bindings")
                .ok_or_else(|| RacketError::MissingParam { op: op.into(), param: "bindings".into() })?;
            let body = params.get("body")
                .ok_or_else(|| RacketError::MissingParam { op: op.into(), param: "body".into() })?;
            let expr = format!("(let* ({}) {})", bindings, body);
            return Ok(RacketExpr { expr, uses_prev: false });
        }
        "begin" => {
            let body = params.get("body")
                .ok_or_else(|| RacketError::MissingParam { op: op.into(), param: "body".into() })?;
            let expr = format!("(begin {})", body);
            return Ok(RacketExpr { expr, uses_prev: false });
        }
        "define" => {
            let name = params.get("name")
                .ok_or_else(|| RacketError::MissingParam { op: op.into(), param: "name".into() })?;
            let args = params.get("args").map(|s| s.as_str()).unwrap_or("");
            let body = params.get("body")
                .ok_or_else(|| RacketError::MissingParam { op: op.into(), param: "body".into() })?;
            let expr = format!("(define ({} {}) {})", name, args, body);
            return Ok(RacketExpr { expr, uses_prev: false });
        }
        "build_list" => {
            let n = get_one_operand(step, input_values, prev_binding)?;
            let func = params.get("function").or_else(|| params.get("f"))
                .ok_or_else(|| RacketError::MissingParam { op: op.into(), param: "function".into() })?;
            let expr = format!("(build-list {} {})", n, func);
            return Ok(RacketExpr { expr, uses_prev: prev_binding.is_some() });
        }
        _ => {}
    }

    // -----------------------------------------------------------------------
    // Shell ops: generate (shell-lines "cmd flags path") or (shell-exec ...)
    // -----------------------------------------------------------------------
    if is_shell_op(op, registry) {
        if let Some((base_cmd, flags)) = extract_shell_meta(op, registry) {
            let poly = registry.get_poly(op)
                .ok_or_else(|| RacketError::UnknownOp(op.to_string()))?;
            let arity = poly.signature.inputs.len();

            // Build the shell command string
            let cmd_parts = if let Some(ref f) = flags {
                format!("{} {}", base_cmd, f)
            } else {
                base_cmd.clone()
            };

            if arity == 0 {
                // Nullary shell op (e.g., ps, df): (shell-lines "cmd flags")
                let expr = format!("(shell-lines \"{}\")", cmd_parts);
                return Ok(RacketExpr { expr, uses_prev: false });
            } else if arity >= 2 {
                // Binary+ shell op (e.g., grep pattern path):
                // (shell-lines (string-append "cmd flags " (shell-quote pattern) " " (shell-quote path)))
                let (a, b) = get_two_operands(step, input_values, prev_binding)?;
                let expr = format!(
                    "(shell-lines (string-append \"{} \" (shell-quote {}) \" \" (shell-quote {})))",
                    cmd_parts, a, b
                );
                return Ok(RacketExpr { expr, uses_prev: prev_binding.is_some() });
            } else {
                // Unary+ shell op: (shell-lines (string-append "cmd flags " (shell-quote path)))
                let a = get_one_operand(step, input_values, prev_binding)?;
                let expr = format!(
                    "(shell-lines (string-append \"{} \" (shell-quote {})))",
                    cmd_parts, a
                );
                return Ok(RacketExpr { expr, uses_prev: prev_binding.is_some() });
            }
        }
    }

    // -----------------------------------------------------------------------
    // Tier 1: Subsumed fs_ops → shell-op codegen via type_lowering map.
    // World-touching ops (walk_tree, list_dir, search_content, etc.) are
    // routed through the shell-callable infrastructure.
    // -----------------------------------------------------------------------
    //
    // Dual-behavior ops: if there's a prev_binding, use Racket-native form
    // (in-memory operation). If no prev_binding, use the shell bridge.
    if let Some(dual_kind) = crate::type_lowering::lookup_dual_behavior(op) {
        if prev_binding.is_some() {
            return racket_native_op_to_racket(step, dual_kind, input_values, prev_binding);
        }
        // Fall through to shell bridge below
    }

    if let Some(entry) = crate::type_lowering::lookup_subsumption(op) {
        return subsumed_op_to_racket(step, entry, input_values, prev_binding, registry, prev_is_seq, bindings);
    }

    // -----------------------------------------------------------------------
    // Tier 2: Racket-native fs_ops → Racket primitive codegen.
    // Intermediate-logic ops (filter, find_matching, unique, flatten_tree)
    // use Racket's own functions on in-memory List(String) data.
    // -----------------------------------------------------------------------
    if let Some(native) = crate::type_lowering::lookup_racket_native(op) {
        return racket_native_op_to_racket(step, &native.kind, input_values, prev_binding);
    }

    // -----------------------------------------------------------------------
    // Data-driven path: look up symbol and arity from the registry.
    // -----------------------------------------------------------------------
    let poly = registry.get_poly(op)
        .ok_or_else(|| RacketError::UnknownOp(op.to_string()))?;

    let sym = poly.racket_symbol.as_deref()
        .ok_or_else(|| RacketError::UnknownOp(format!("{} (no racket_symbol)", op)))?;

    // Variadic ops: split the first value param on whitespace and emit each
    // as a separate argument.  (list 1 2 3) not (list "1 2 3").
    if poly.variadic {
        let elements_str = params.get("elements")
            .cloned()
            .or_else(|| first_value_param(params).map(|s| s.to_string()))
            .unwrap_or_default();
        if elements_str.is_empty() {
            // Empty variadic call: (list)
            return Ok(RacketExpr { expr: format!("({})", sym), uses_prev: false });
        }
        let args: Vec<String> = elements_str
            .split_whitespace()
            .map(|s| racket_value(s))
            .collect();
        let expr = format!("({} {})", sym, args.join(" "));
        return Ok(RacketExpr { expr, uses_prev: false });
    }

    let arity = poly.signature.inputs.len();

    match arity {
        0 => {
            // Nullary: (symbol)
            Ok(RacketExpr { expr: format!("({})", sym), uses_prev: false })
        }
        1 => {
            // Unary: (symbol a)
            let a = get_one_operand(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("({} {})", sym, a), uses_prev: prev_binding.is_some() })
        }
        _ => {
            // Binary (or more): (symbol a b)
            let (a, b) = get_two_operands(step, input_values, prev_binding)?;
            Ok(RacketExpr { expr: format!("({} {} {})", sym, a, b), uses_prev: prev_binding.is_some() })
        }
    }
}

// ---------------------------------------------------------------------------
// Script generation
// ---------------------------------------------------------------------------

/// Generate a complete Racket script from a compiled plan.
///
/// The script uses `#lang racket` and follows this structure:
/// - Single-step: bare expression wrapped in `(displayln ...)`
/// - Multi-step: `(let* ([step-1 ...] [step-2 ...] ...) (displayln step-N))`
///
/// This is the Racket analogue of `executor::generate_script()`.
pub fn generate_racket_script(
    compiled: &CompiledPlan,
    def: &PlanDef,
    registry: &OperationRegistry,
) -> Result<String, RacketError> {
    let mut script = String::new();

    // Shebang and preamble
    script.push_str("#!/usr/bin/env racket\n");
    script.push_str("#lang racket\n");
    script.push_str("\n");
    script.push_str(&format!(";; Generated by cadmus: {}\n", compiled.name));
    script.push_str(";;\n");

    // Emit shell preamble if any step uses a shell op
    if has_shell_ops(compiled, registry) {
        script.push_str("\n");
        script.push_str(SHELL_PREAMBLE);
    }

    // Collect input values for operand resolution
    let input_values = &def.inputs;

    let num_steps = compiled.steps.len();

    if num_steps == 0 {
        script.push_str(";; (no steps)\n");
        return Ok(script);
    }

    // Single-step plan: bare expression, print result
    if num_steps == 1 {
        let step = &compiled.steps[0];
        script.push_str(&format!(";; Step 1: {}\n", step.op));
        let expr = op_to_racket(step, input_values, None, registry, false, &def.bindings)?;
        script.push_str(&format!("(displayln {})\n", expr.expr));
        return Ok(script);
    }

    // Multi-step plan: use let* bindings
    script.push_str("(let*\n");
    script.push_str("  (\n");

    for (i, step) in compiled.steps.iter().enumerate() {
        let step_num = i + 1;
        let binding_name = format!("step-{}", step_num);
        let prev_binding = if i == 0 { None } else { Some(format!("step-{}", i)) };

        // Type-driven each-mode detection: if the step's input is Seq(X)
        // but the op's registered signature expects a non-Seq first input,
        // we need to wrap in (map ...).
        let is_map = crate::plan::step_needs_map(step, registry);

        script.push_str(&format!("    ;; Step {}: {}{}\n", step_num, step.op,
            if is_map { " (map)" } else { "" }));

        if is_map {
            let prev = prev_binding.as_deref().unwrap_or("'()");

            if step.isolate {
                // Isolated map mode: the compiler determined this op has
                // filesystem side effects that can collide when run over a
                // Seq (e.g., extract_zip on multiple archives with
                // identically-named files like cover.jpg).
                //
                // Generate:
                //   (map (lambda (_line)
                //     (let ([_td (path->string (make-temporary-directory))])
                //       (begin
                //         (shell-exec (string-append "CMD " (shell-quote _line) " -d " (shell-quote _td)))
                //         (shell-lines (string-append "find " (shell-quote _td) " -type f")))))
                //     prev)
                let entry = crate::type_lowering::lookup_subsumption(&step.op);
                let cmd_parts = entry.map(|e| {
                    extract_shell_meta(e.shell_op, registry)
                        .unwrap_or_else(|| {
                            let cmd = e.shell_op.strip_prefix("shell_").unwrap_or(e.shell_op).to_string();
                            (cmd, None)
                        })
                }).map(|(cmd, flags)| match flags {
                    Some(f) => format!("{} {}", cmd, f),
                    None => cmd,
                }).unwrap_or_else(|| "unzip -o".to_string());

                script.push_str(&format!(
                    "    [{} (map (lambda (_line) (let ([_td (path->string (make-temporary-directory))]) (begin (shell-exec (string-append \"{} \" (shell-quote _line) \" -d \" (shell-quote _td))) (shell-lines (string-append \"find \" (shell-quote _td) \" -type f\"))))) {})]\n",
                    binding_name, cmd_parts, prev));
            } else {
                // Normal map-each: generate the inner expression with _line as the
                // scalar prev_binding (not the list), and prev_is_seq=false
                // since _line is a single element.
                let expr = op_to_racket(step, input_values, Some("_line"), registry, false, &def.bindings)?;
                script.push_str(&format!("    [{} (map (lambda (_line) {}) {})]\n",
                    binding_name, expr.expr, prev));
            }
        } else {
            // Normal step: pass the previous binding directly.
            let prev_is_seq = if i > 0 {
                is_seq_output(&compiled.steps[i - 1], registry)
            } else { false };
            let expr = op_to_racket(step, input_values, prev_binding.as_deref(), registry, prev_is_seq, &def.bindings)?;
            script.push_str(&format!("    [{} {}]\n", binding_name, expr.expr));
        }
    }

    script.push_str("  )\n");

    // Print the final step's result
    let final_binding = format!("step-{}", num_steps);
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
    _input_values: &[PlanInput],
    prev_binding: Option<&str>,
) -> Result<String, RacketError> {
    if let Some(prev) = prev_binding {
        return Ok(prev.to_string());
    }

    // Check params for a value operand (skip function/predicate params)
    if let Some(val) = first_value_param(&step.params) {
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
    _input_values: &[PlanInput],
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
    // Step back-references: $step-N → step-N (Racket binding name)
    if let Some(rest) = s.strip_prefix('$') {
        if rest.starts_with("step-") {
            return rest.to_string();
        }
    }
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
    use crate::plan::CompiledStep;
    use crate::type_expr::TypeExpr;
    use crate::registry::load_ops_pack_str;
    use crate::racket_strategy::{load_racket_facts_from_str, promote_inferred_ops};

    fn make_registry() -> OperationRegistry {
        let mut reg = load_ops_pack_str(include_str!("../data/packs/ops/racket.ops.yaml")).unwrap();
        let facts = load_racket_facts_from_str(include_str!("../data/packs/facts/racket.facts.yaml")).unwrap();
        promote_inferred_ops(&mut reg, &facts);
        reg
    }

    fn make_step(op: &str, params: Vec<(&str, &str)>) -> CompiledStep {
        CompiledStep {
            index: 0,
            op: op.to_string(),
            input_type: TypeExpr::prim("Number"),
            output_type: TypeExpr::prim("Number"),
            params: params.into_iter().map(|(k, v)| (k.to_string(), v.to_string())).collect(),
            ..Default::default()
        }
    }

    fn make_inputs(pairs: Vec<(&str, &str)>) -> Vec<PlanInput> {
        pairs.into_iter().map(|(k, _v)| PlanInput::bare(k)).collect()
    }

    // --- op_to_racket tests ---

    #[test]
    fn test_add_with_named_params() {
        let reg = make_registry();
        let step = make_step("add", vec![("x", "4"), ("y", "35")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(+ 4 35)");
        assert!(!expr.uses_prev);
    }

    #[test]
    fn test_remove_discovered_executor() {
        // remove is discovered from the fact pack — verify it works through the executor
        let reg = make_registry();
        let step = make_step("remove", vec![("x", "3"), ("y", "'(1 2 3 4)")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(remove 3 '(1 2 3 4))");
    }

    #[test]
    fn test_list_reverse_discovered_executor() {
        // list_reverse is discovered from the fact pack — verify it works through the executor
        let reg = make_registry();
        let step = make_step("list_reverse", vec![("value", "'(1 2 3)")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(reverse '(1 2 3))");
    }

    #[test]
    fn test_subtract_with_named_params() {
        let reg = make_registry();
        let step = make_step("subtract", vec![("x", "6"), ("y", "2")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(- 6 2)");
    }

    #[test]
    fn test_multiply_with_named_params() {
        let reg = make_registry();
        let step = make_step("multiply", vec![("x", "3"), ("y", "7")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(* 3 7)");
    }

    #[test]
    fn test_divide_with_named_params() {
        let reg = make_registry();
        let step = make_step("divide", vec![("x", "10"), ("y", "2")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(/ 10 2)");
    }

    #[test]
    fn test_add_with_prev_binding() {
        let reg = make_registry();
        let step = make_step("add", vec![("y", "10")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, Some("step-1"), &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(+ step-1 10)");
        assert!(expr.uses_prev);
    }

    #[test]
    fn test_display_with_prev() {
        let reg = make_registry();
        let step = make_step("display", vec![]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, Some("step-1"), &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(display step-1)");
    }

    #[test]
    fn test_displayln_with_value() {
        let reg = make_registry();
        let step = make_step("displayln", vec![("value", "hello")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(displayln \"hello\")");
    }

    #[test]
    fn test_unknown_op() {
        let reg = make_registry();
        let step = make_step("nonexistent_op", vec![]);
        let inputs = make_inputs(vec![]);
        let result = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new());
        assert!(result.is_err());
        match result.unwrap_err() {
            RacketError::UnknownOp(name) => assert_eq!(name, "nonexistent_op"),
            _ => panic!("expected UnknownOp"),
        }
    }

    #[test]
    fn test_racket_filter_with_pred() {
        let reg = make_registry();
        let step = make_step("racket_filter", vec![("predicate", "even?")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, Some("'(1 2 3 4 5)"), &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(filter even? '(1 2 3 4 5))");
    }

    #[test]
    fn test_racket_map_with_func() {
        let reg = make_registry();
        let step = make_step("racket_map", vec![("function", "add1")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, Some("step-1"), &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(map add1 step-1)");
    }

    #[test]
    fn test_racket_foldl_with_init() {
        let reg = make_registry();
        let step = make_step("racket_foldl", vec![("function", "+"), ("init", "0")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, Some("step-1"), &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(foldl + 0 step-1)");
    }

    #[test]
    fn test_set_union() {
        let reg = make_registry();
        let step = make_step("set_union", vec![("x", "(set 1 2 3)"), ("y", "(set 3 4 5)")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(set-union (set 1 2 3) (set 3 4 5))");
    }

    #[test]
    fn test_cons_with_inputs() {
        let reg = make_registry();
        let step = make_step("cons", vec![("x", "42"), ("y", "'()")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
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
        let reg = make_registry();
        let compiled = CompiledPlan {
            name: "add numbers".to_string(),
            input_type: TypeExpr::prim("Number"),
            input_description: "4".to_string(),
            steps: vec![
                CompiledStep {
                    index: 0,
                    op: "add".to_string(),
                    input_type: TypeExpr::prim("Number"),
                    output_type: TypeExpr::prim("Number"),
                    params: vec![("x".into(), "4".into()), ("y".into(), "35".into())].into_iter().collect(),
                    ..Default::default()
                },
            ],
            output_type: TypeExpr::prim("Number"),
        };
        let def = PlanDef {
            name: "add numbers".to_string(),
            inputs: vec![],
            output: None,
            steps: vec![],
            bindings: HashMap::new(),
        };
        let script = generate_racket_script(&compiled, &def, &reg).unwrap();
        assert!(script.contains("#lang racket"));
        assert!(script.contains("(displayln (+ 4 35))"));
        // Single-step: no let* binding
        assert!(!script.contains("let*"));
    }

    #[test]
    fn test_multi_step_script() {
        let reg = make_registry();
        let compiled = CompiledPlan {
            name: "add then display".to_string(),
            input_type: TypeExpr::prim("Number"),
            input_description: "4".to_string(),
            steps: vec![
                CompiledStep {
                    index: 0,
                    op: "add".to_string(),
                    input_type: TypeExpr::prim("Number"),
                    output_type: TypeExpr::prim("Number"),
                    params: vec![("x".into(), "4".into()), ("y".into(), "35".into())].into_iter().collect(),
                    ..Default::default()
                },
                CompiledStep {
                    index: 1,
                    op: "multiply".to_string(),
                    input_type: TypeExpr::prim("Number"),
                    output_type: TypeExpr::prim("Number"),
                    params: vec![("y".into(), "2".into())].into_iter().collect(),
                    ..Default::default()
                },
            ],
            output_type: TypeExpr::prim("Number"),
        };
        let def = PlanDef {
            name: "add then multiply".to_string(),
            inputs: vec![],
            output: None,
            steps: vec![],
            bindings: HashMap::new(),
        };
        let script = generate_racket_script(&compiled, &def, &reg).unwrap();
        assert!(script.contains("#lang racket"));
        assert!(script.contains("let*"));
        assert!(script.contains("[step-1 (+ 4 35)]"));
        assert!(script.contains("[step-2 (* step-1 2)]"));
        assert!(script.contains("(displayln step-2)"));
    }

    #[test]
    fn test_empty_plan_script() {
        let reg = make_registry();
        let compiled = CompiledPlan {
            name: "empty".to_string(),
            input_type: TypeExpr::prim("Number"),
            input_description: "".to_string(),
            steps: vec![],
            output_type: TypeExpr::prim("Number"),
        };
        let def = PlanDef {
            name: "empty".to_string(),
            inputs: vec![],
            output: None,
            steps: vec![],
            bindings: HashMap::new(),
        };
        let script = generate_racket_script(&compiled, &def, &reg).unwrap();
        assert!(script.contains(";; (no steps)"));
    }

    #[test]
    fn test_string_operations_script() {
        let reg = make_registry();
        let step = make_step("string_append", vec![("x", "hello"), ("y", " world")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(string-append \"hello\" \" world\")");
    }

    #[test]
    fn test_number_to_string() {
        let reg = make_registry();
        let step = make_step("number_to_string", vec![("value", "42")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(number->string 42)");
    }

    #[test]
    fn test_abs_operation() {
        let reg = make_registry();
        let step = make_step("abs", vec![("value", "-5")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(abs -5)");
    }

    #[test]
    fn test_equal_operation() {
        let reg = make_registry();
        let step = make_step("equal", vec![("x", "42"), ("y", "42")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(equal? 42 42)");
    }

    #[test]
    fn test_less_than_operation() {
        let reg = make_registry();
        let step = make_step("less_than", vec![("x", "1"), ("y", "2")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(< 1 2)");
    }

    // --- Data-driven specific tests ---

    #[test]
    fn test_newline_nullary() {
        let reg = make_registry();
        let step = make_step("newline", vec![]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(newline)");
        assert!(!expr.uses_prev);
    }

    #[test]
    fn test_read_line_nullary() {
        let reg = make_registry();
        let step = make_step("read_line", vec![]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(read-line)");
        assert!(!expr.uses_prev);
    }

    #[test]
    fn test_set_member_question_mark() {
        let reg = make_registry();
        let step = make_step("set_member", vec![("x", "(set 1 2 3)"), ("y", "2")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(set-member? (set 1 2 3) 2)");
    }

    #[test]
    fn test_list_to_set_arrow() {
        let reg = make_registry();
        let step = make_step("set_new", vec![("value", "'(1 2 3)")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(list->set '(1 2 3))");
    }

    #[test]
    fn test_list_reverse_symbol() {
        let reg = make_registry();
        let step = make_step("list_reverse", vec![("value", "'(1 2 3)")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(reverse '(1 2 3))");
    }

    #[test]
    fn test_sort_list_with_comparator() {
        let reg = make_registry();
        let step = make_step("sort_list", vec![("value", "'(3 1 2)"), ("comparator", ">")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(sort '(3 1 2) >)");
    }

    #[test]
    fn test_greater_than_symbol() {
        let reg = make_registry();
        let step = make_step("greater_than", vec![("x", "5"), ("y", "3")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(> 5 3)");
    }

    #[test]
    fn test_string_to_number_arrow() {
        let reg = make_registry();
        let step = make_step("string_to_number", vec![("value", "42")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(string->number 42)");
    }

    #[test]
    fn test_less_than_or_equal_discovered_executor() {
        let reg = make_registry();
        let step = make_step("less_than_or_equal", vec![("x", "3"), ("y", "5")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(<= 3 5)");
    }

    #[test]
    fn test_greater_than_or_equal_discovered_executor() {
        let reg = make_registry();
        let step = make_step("greater_than_or_equal", vec![("x", "7"), ("y", "2")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(>= 7 2)");
    }

    #[test]
    fn test_string_upcase_executor() {
        let reg = make_registry();
        let step = make_step("string_upcase", vec![("value", "hello")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(string-upcase \"hello\")");
    }

    #[test]
    fn test_string_downcase_discovered_executor() {
        let reg = make_registry();
        let step = make_step("string_downcase", vec![("value", "HELLO")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(string-downcase \"HELLO\")");
    }

    #[test]
    fn test_file_read_executor() {
        let reg = make_registry();
        let step = make_step("file_read", vec![("value", "data.txt")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(file->string \"data.txt\")");
    }

    #[test]
    fn test_file_read_lines_executor() {
        let reg = make_registry();
        let step = make_step("file_read_lines", vec![("value", "data.txt")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(file->lines \"data.txt\")");
    }

    #[test]
    fn test_file_write_executor() {
        let reg = make_registry();
        let step = make_step("file_write", vec![("x", "hello world"), ("y", "output.txt")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(display-to-file \"hello world\" \"output.txt\")");
    }

    #[test]
    fn test_file_exists_executor() {
        let reg = make_registry();
        let step = make_step("file_exists", vec![("value", "data.txt")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(file-exists? \"data.txt\")");
    }

    // --- is_seq_output tests ---

    /// Helper: make a step with a specific output_type.
    fn make_step_with_output(op: &str, output_type: TypeExpr) -> CompiledStep {
        CompiledStep {
            index: 0,
            op: op.to_string(),
            input_type: TypeExpr::prim("Bytes"),
            output_type,
            params: HashMap::new(),
            ..Default::default()
        }
    }

    #[test]
    fn test_is_seq_output_walk_tree_via_metasig() {
        // walk_tree is subsumed to shell_find, whose metasig has return_type="List(String)"
        let reg = make_full_registry();
        let step = make_step_with_output("walk_tree",
            TypeExpr::seq(TypeExpr::entry(TypeExpr::prim("Name"), TypeExpr::prim("Bytes"))));
        assert!(is_seq_output(&step, &reg));
    }

    #[test]
    fn test_is_seq_output_get_size_scalar() {
        // get_size is subsumed to shell_du, whose metasig has return_type="String"
        let reg = make_full_registry();
        let step = make_step_with_output("get_size", TypeExpr::prim("String"));
        assert!(!is_seq_output(&step, &reg));
    }

    #[test]
    fn test_is_seq_output_filter_via_output_type() {
        // filter is Racket-native (not subsumed), but output_type is Seq(...)
        let reg = make_full_registry();
        let step = make_step_with_output("filter",
            TypeExpr::seq(TypeExpr::entry(TypeExpr::prim("Name"), TypeExpr::prim("Bytes"))));
        assert!(is_seq_output(&step, &reg));
    }

    #[test]
    fn test_is_seq_output_add_not_seq() {
        // add is a pure Racket op, output_type is Number — not a seq
        let reg = make_registry();
        let step = make_step_with_output("add", TypeExpr::prim("Number"));
        assert!(!is_seq_output(&step, &reg));
    }

    #[test]
    fn test_is_seq_output_list_dir_via_metasig() {
        // list_dir is subsumed to shell_ls, return_type="List(String)"
        let reg = make_full_registry();
        let step = make_step_with_output("list_dir",
            TypeExpr::seq(TypeExpr::entry(TypeExpr::prim("Name"), TypeExpr::prim("Bytes"))));
        assert!(is_seq_output(&step, &reg));
    }

    /// Build a full registry with shell submodes (needed for metasig lookups).
    fn make_full_registry() -> OperationRegistry {
        let mut reg = load_ops_pack_str(include_str!("../data/packs/ops/racket.ops.yaml")).unwrap();
        let facts = load_racket_facts_from_str(include_str!("../data/packs/facts/racket.facts.yaml")).unwrap();
        promote_inferred_ops(&mut reg, &facts);
        let cli_yaml = include_str!("../data/packs/facts/macos_cli.facts.yaml");
        if let Ok(cli_pack) = serde_yaml::from_str::<crate::fact_pack::FactPack>(cli_yaml) {
            let cli_facts = crate::fact_pack::FactPackIndex::build(cli_pack);
            crate::racket_strategy::discover_shell_submodes(&mut reg, &facts, &cli_facts);
        }
        reg
    }

    // --- Seq→String bridge tests ---

    #[test]
    fn test_subsumed_bridge_pack_archive_with_seq_prev() {
        let reg = make_full_registry();
        let step = CompiledStep {
            index: 1,
            op: "pack_archive".to_string(),
            input_type: TypeExpr::seq(TypeExpr::entry(
                TypeExpr::prim("Name"), TypeExpr::prim("Bytes"))),
            output_type: TypeExpr::prim("File"),
            params: HashMap::new(),
            ..Default::default()
        };
        let inputs = make_inputs(vec![("path", "~/Downloads")]);
        // prev_is_seq=true: step-1 is a List(String) from walk_tree
        let expr = op_to_racket(&step, &inputs, Some("step-1"), &reg, true, &HashMap::new()).unwrap();
        // Should bridge the seq — either string-join or append-map
        assert!(expr.expr.contains("string-join") || expr.expr.contains("map shell-quote") || expr.expr.contains("append-map"),
            "pack_archive with seq prev should bridge: {}", expr.expr);
        assert!(!expr.expr.contains("(shell-quote step-1)"),
            "should NOT raw shell-quote the list variable: {}", expr.expr);
    }

    #[test]
    fn test_subsumed_stat_single_step() {
        let reg = make_full_registry();
        let step = CompiledStep {
            index: 0,
            op: "stat".to_string(),
            input_type: TypeExpr::prim("Path"),
            output_type: TypeExpr::prim("String"),
            params: HashMap::new(),
            ..Default::default()
        };
        let inputs = make_inputs(vec![]);
        // Use prev_binding to provide the path
        let expr = op_to_racket(&step, &inputs, Some("\"~/readme.md\""), &reg, false, &HashMap::new()).unwrap();
        assert!(expr.expr.contains("shell-quote"),
            "stat single step should use shell-quote: {}", expr.expr);
        assert!(expr.expr.contains("~/readme.md"),
            "stat should reference the input path: {}", expr.expr);
    }

    #[test]
    fn test_subsumed_bridge_search_content_with_seq_prev() {
        let reg = make_full_registry();
        let step = CompiledStep {
            index: 1,
            op: "search_content".to_string(),
            input_type: TypeExpr::seq(TypeExpr::entry(
                TypeExpr::prim("Name"), TypeExpr::prim("Bytes"))),
            output_type: TypeExpr::seq(TypeExpr::prim("String")),
            params: vec![("pattern".to_string(), "TODO".to_string())].into_iter().collect(),
            ..Default::default()
        };
        let inputs = make_inputs(vec![("textdir", "~/Projects")]);
        // prev_is_seq=true: step-1 is a List(String) from walk_tree
        let expr = op_to_racket(&step, &inputs, Some("step-1"), &reg, true, &HashMap::new()).unwrap();
        // Should use append-map to grep each file individually
        assert!(expr.expr.contains("append-map"),
            "search_content with seq prev should use append-map: {}", expr.expr);
        assert!(expr.expr.contains("grep") || expr.expr.contains("shell-lines"),
            "should still use grep/shell-lines: {}", expr.expr);
        assert!(!expr.expr.contains("(shell-quote step-1)"),
            "should NOT raw shell-quote the list variable: {}", expr.expr);
    }

    #[test]
    fn test_subsumed_search_content_no_bridge_first_step() {
        let reg = make_full_registry();
        let step = CompiledStep {
            index: 0,
            op: "search_content".to_string(),
            input_type: TypeExpr::prim("String"),
            output_type: TypeExpr::seq(TypeExpr::prim("String")),
            params: vec![("pattern".to_string(), "TODO".to_string())].into_iter().collect(),
            ..Default::default()
        };
        let inputs = make_inputs(vec![]);
        // Use prev_binding to provide the path
        let expr = op_to_racket(&step, &inputs, Some("\"~/Projects\""), &reg, false, &HashMap::new()).unwrap();
        assert!(expr.expr.contains("~/Projects"),
            "search_content first step should use input path: {}", expr.expr);
    }

    #[test]
    fn test_subsumed_bridge_read_file_with_seq_prev() {
        // walk_tree → read_file: cat each file in the list
        let reg = make_full_registry();
        let step = CompiledStep {
            index: 1,
            op: "read_file".to_string(),
            input_type: TypeExpr::seq(TypeExpr::prim("String")),
            output_type: TypeExpr::seq(TypeExpr::prim("String")),
            params: HashMap::new(),
            ..Default::default()
        };
        let inputs = make_inputs(vec![("path", "~/Projects")]);
        let expr = op_to_racket(&step, &inputs, Some("step-1"), &reg, true, &HashMap::new()).unwrap();
        // Should use append-map to cat each file
        assert!(expr.expr.contains("append-map"),
            "read_file with seq prev should use append-map: {}", expr.expr);
        assert!(expr.expr.contains("cat"),
            "should use cat command: {}", expr.expr);
    }

    // --- expand_tilde tests ---

    #[test]
    fn test_expand_tilde_home_prefix() {
        let expanded = expand_tilde("~/Downloads");
        assert!(!expanded.starts_with("~/"), "should expand tilde: {}", expanded);
        assert!(expanded.ends_with("/Downloads"), "should keep path suffix: {}", expanded);
    }

    #[test]
    fn test_expand_tilde_bare() {
        let expanded = expand_tilde("~");
        assert!(!expanded.starts_with("~"), "should expand bare tilde: {}", expanded);
        assert!(!expanded.is_empty());
    }

    #[test]
    fn test_expand_tilde_absolute_path_unchanged() {
        assert_eq!(expand_tilde("/usr/local"), "/usr/local");
    }

    #[test]
    fn test_expand_tilde_relative_path_unchanged() {
        assert_eq!(expand_tilde("foo/bar"), "foo/bar");
    }

    #[test]
    fn test_expand_tilde_dot_unchanged() {
        assert_eq!(expand_tilde("."), ".");
    }

    #[test]
    fn test_fs_path_operand_expands_tilde() {
        let inputs = vec![PlanInput::bare("path")];
        let mut bindings = HashMap::new();
        bindings.insert("path".to_string(), "~/Documents".to_string());
        let result = fs_path_operand(None, &inputs, &bindings);
        assert!(!result.contains("~/"), "should expand tilde in operand: {}", result);
        assert!(result.contains("/Documents"), "should keep path suffix: {}", result);
    }

    // --- Variadic ops tests ---

    #[test]
    fn test_list_new_variadic_numbers() {
        let reg = make_registry();
        let step = make_step("list_new", vec![("elements", "1 2 3 4 5")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(list 1 2 3 4 5)");
    }

    #[test]
    fn test_list_new_variadic_strings() {
        let reg = make_registry();
        let step = make_step("list_new", vec![("elements", "A B C")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(list \"A\" \"B\" \"C\")");
    }

    #[test]
    fn test_list_new_variadic_empty() {
        let reg = make_registry();
        let step = make_step("list_new", vec![]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(list)");
    }

    #[test]
    fn test_list_new_variadic_single() {
        let reg = make_registry();
        let step = make_step("list_new", vec![("elements", "42")]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(list 42)");
    }

    // --- Iteration form tests ---

    #[test]
    fn test_for_fold_sum() {
        let reg = make_registry();
        let step = make_step("for_fold", vec![
            ("bindings", "([acc 0])"),
            ("sequence", "(in-range 1 11)"),
            ("body", "(+ acc x)"),
        ]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(for/fold (([acc 0])) ([x (in-range 1 11)]) (+ acc x))");
    }

    #[test]
    fn test_for_list() {
        let reg = make_registry();
        let step = make_step("for_list", vec![
            ("sequence", "(in-range 1 6)"),
            ("body", "(* x x)"),
        ]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(for/list ([x (in-range 1 6)]) (* x x))");
    }

    #[test]
    fn test_for_sum() {
        let reg = make_registry();
        let step = make_step("for_sum", vec![
            ("sequence", "(in-range 1 11)"),
            ("body", "x"),
        ]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(for/sum ([x (in-range 1 11)]) x)");
    }

    #[test]
    fn test_iterate_gcd() {
        let reg = make_registry();
        let step = make_step("iterate", vec![
            ("name", "loop"),
            ("bindings", "a=48 b=18"),
            ("body", "(if (= b 0) a (loop b (modulo a b)))"),
        ]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(let loop ([a 48] [b 18]) (if (= b 0) a (loop b (modulo a b))))");
    }

    #[test]
    fn test_for_fold_missing_body() {
        let reg = make_registry();
        let step = make_step("for_fold", vec![
            ("bindings", "([acc 0])"),
            ("sequence", "(in-range 1 11)"),
        ]);
        let inputs = make_inputs(vec![]);
        let result = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new());
        assert!(result.is_err());
    }

    // --- Control flow and utility op tests ---

    #[test]
    fn test_conditional() {
        let reg = make_registry();
        let step = make_step("conditional", vec![
            ("clauses", "[(= x 0) \"zero\"] [(= x 1) \"one\"] [else \"other\"]"),
        ]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert!(expr.expr.starts_with("(cond "));
    }

    #[test]
    fn test_if_then() {
        let reg = make_registry();
        let step = make_step("if_then", vec![
            ("test", "(> x 0)"),
            ("then", "\"positive\""),
            ("else", "\"non-positive\""),
        ]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(if (> x 0) \"positive\" \"non-positive\")");
    }

    #[test]
    fn test_let_bind() {
        let reg = make_registry();
        let step = make_step("let_bind", vec![
            ("bindings", "[x 10] [y 20]"),
            ("body", "(+ x y)"),
        ]);
        let inputs = make_inputs(vec![]);
        let expr = op_to_racket(&step, &inputs, None, &reg, false, &HashMap::new()).unwrap();
        assert_eq!(expr.expr, "(let* ([x 10] [y 20]) (+ x y))");
    }
}
