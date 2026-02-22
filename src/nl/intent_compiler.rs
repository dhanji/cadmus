//! Intent IR → PlanDef compiler.
//!
//! Maps abstract actions to concrete cadmus ops:
//! - `select` → `walk_tree` + `find_matching` (or `filter`)
//! - `order` → `sort_by`
//! - `compress` → `walk_tree` + `pack_archive`
//! - `decompress` → `extract_archive`
//! - `enumerate` → `list_dir`
//! - `traverse` → `walk_tree`
//! - `count` → `walk_tree` + `count_entries`
//! - `read` → `read_file`
//! - `delete` → `delete`
//! - `copy` → `copy`
//! - `move` → `move_entry`
//! - `rename` → `rename`
//! - `deduplicate` → `walk_tree` + `find_duplicates`
//!
//! Concept labels are resolved to file patterns via the NL vocab's
//! `noun_patterns` table (e.g., `comic_issue_archive` → `*.cbz`, `*.cbr`).

use std::collections::HashMap;

use crate::nl::intent_ir::{IntentIR, IntentIRResult}
;
use crate::nl::vocab;
use crate::plan::{PlanDef, PlanInput, RawStep, StepArgs};

// ---------------------------------------------------------------------------
// Compilation result
// ---------------------------------------------------------------------------

/// Result of compiling an Intent IR to a PlanDef.
#[derive(Debug)]
pub enum CompileResult {
    /// Successfully compiled to a PlanDef.
    Ok(PlanDef),
    /// Compilation failed with an error message.
    Error(String),
    /// No intent to compile (empty input / gibberish).
    NoIntent,
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Compile an IntentIRResult to a PlanDef.
///
/// Uses the primary intent. Alternatives are preserved in DialogueState
/// by the caller.
pub fn compile_intent(result: &IntentIRResult) -> CompileResult {
    match &result.primary {
        None => CompileResult::NoIntent,
        Some(ir) => compile_ir(ir),
    }
}

/// Compile a single IntentIR to a PlanDef.
pub fn compile_ir(ir: &IntentIR) -> CompileResult {
    let mut inputs: Vec<PlanInput> = Vec::new();
    let mut steps: Vec<RawStep> = Vec::new();

    // Extract the target path from the IR's input selector
    let target_path = ir.inputs.first()
        .and_then(|i| i.selector.scope.as_ref())
        .map(|s| s.dir.clone())
        .unwrap_or_else(|| ".".to_string());

    // Add input — "file" for file targets, "path" for directories
    if is_file_path(&target_path) {
        inputs.push(PlanInput::bare("file"));
    } else {
        inputs.push(PlanInput::bare("path"));
    }

    // Process each IR step
    for ir_step in &ir.steps {
        match ir_step.action.as_str() {
            "select" => {
                compile_select_step(ir_step, &target_path, &mut steps);
            }
            "order" => {
                compile_order_step(ir_step, &mut steps);
            }
            "compress" => {
                compile_compress_step(&target_path, &mut steps);
            }
            "search_text" => {
                steps.push(RawStep {
                    op: "walk_tree".to_string(),
                    args: StepArgs::None,
                });
                steps.push(RawStep {
                    op: "search_content".to_string(),
                    args: StepArgs::None,
                });
            }
            "decompress" => {
                compile_decompress_step(&mut steps);
            }
            "enumerate" => {
                steps.push(RawStep {
                    op: "list_dir".to_string(),
                    args: StepArgs::None,
                });
            }
            "traverse" => {
                steps.push(RawStep {
                    op: "walk_tree".to_string(),
                    args: StepArgs::None,
                });
            }
            "count" => {
                steps.push(RawStep {
                    op: "walk_tree".to_string(),
                    args: StepArgs::None,
                });
                steps.push(RawStep {
                    op: "count_entries".to_string(),
                    args: StepArgs::None,
                });
            }
            "read" => {
                steps.push(RawStep {
                    op: "read_file".to_string(),
                    args: StepArgs::None,
                });
            }
            "delete" => {
                steps.push(RawStep {
                    op: "delete".to_string(),
                    args: StepArgs::None,
                });
            }
            "copy" => {
                steps.push(RawStep {
                    op: "copy".to_string(),
                    args: StepArgs::None,
                });
            }
            "move" => {
                steps.push(RawStep {
                    op: "move_entry".to_string(),
                    args: StepArgs::None,
                });
            }
            "rename" => {
                steps.push(RawStep {
                    op: "rename".to_string(),
                    args: StepArgs::None,
                });
            }
            "deduplicate" => {
                steps.push(RawStep {
                    op: "walk_tree".to_string(),
                    args: StepArgs::None,
                });
                steps.push(RawStep {
                    op: "find_duplicates".to_string(),
                    args: StepArgs::None,
                });
            }
            "checksum" => {
                steps.push(RawStep {
                    op: "checksum".to_string(),
                    args: StepArgs::None,
                });
            }
            "compare" => {
                steps.push(RawStep {
                    op: "diff".to_string(),
                    args: StepArgs::None,
                });
            }
            "download" => {
                steps.push(RawStep {
                    op: "download".to_string(),
                    args: StepArgs::None,
                });
            }
            unknown => {
                return CompileResult::Error(format!(
                    "Unknown action '{}'. Try a command like 'find', 'sort', 'zip', or 'extract'.",
                    unknown
                ));
            }
        }
    }

    // If no steps were generated, that's an error
    if steps.is_empty() {
        return CompileResult::Error(
            "Could not determine what to do. Try something like 'find comics in downloads'.".to_string()
        );
    }

    // Generate a plan name from the steps and target path
    let name = generate_plan_name(&steps, &target_path);

    // Bind path literals to inputs (the calling frame)
    let mut bindings = HashMap::new();
    if target_path != "." {
        // Bind the extracted path literal to the input name
        let input_name = inputs.first()
            .map(|i| i.name.clone())
            .unwrap_or_else(|| "path".to_string());
        bindings.insert(input_name, target_path);
    }

    CompileResult::Ok(PlanDef {
        name,
        inputs,
        output: None,
        steps,
        bindings,
    })
}

// ---------------------------------------------------------------------------
// Step compilation helpers
// ---------------------------------------------------------------------------

/// Compile a "select" action to walk_tree + find_matching/filter steps.
fn compile_select_step(
    ir_step: &crate::nl::intent_ir::IRStep,
    _target_path: &str,
    steps: &mut Vec<RawStep>,
) {
    // Always start with walk_tree
    steps.push(RawStep {
        op: "walk_tree".to_string(),
        args: StepArgs::None,
    });

    // Resolve concept to file patterns
    let concept = ir_step.params.get("where")
        .and_then(|w| {
            // Parse "kind: \"concept_label\"" format
            w.strip_prefix("kind: \"")
                .and_then(|s| s.strip_suffix('"'))
                .map(|s| s.to_string())
        });

    let patterns = if let Some(ref concept_label) = concept {
        resolve_concept_to_patterns(concept_label)
    } else {
        Vec::new()
    };

    // Check for explicit pattern in params
    let explicit_pattern = ir_step.params.get("pattern").cloned();

    // Build the filter step
    if !patterns.is_empty() {
        // Use find_matching with the resolved patterns
        let pattern_str = patterns.join(",");
        let mut params = HashMap::new();
        params.insert("pattern".to_string(), pattern_str);
        steps.push(RawStep {
            op: "find_matching".to_string(),
            args: StepArgs::Map(params),
        });
    } else if let Some(ref pat) = explicit_pattern {
        let mut params = HashMap::new();
        params.insert("pattern".to_string(), pat.clone());
        steps.push(RawStep {
            op: "find_matching".to_string(),
            args: StepArgs::Map(params),
        });
    }
    // If no patterns and no concept, still add find_matching (no filter)
    // This ensures the plan has at least 2 steps for edit operations.
    if patterns.is_empty() && explicit_pattern.is_none() {
        steps.push(RawStep {
            op: "find_matching".to_string(),
            args: StepArgs::None,
        });
    }
}

/// Compile an "order" action to a sort_by step.
fn compile_order_step(
    ir_step: &crate::nl::intent_ir::IRStep,
    steps: &mut Vec<RawStep>,
) {
    let field = ir_step.params.get("by").cloned().unwrap_or_else(|| "name".to_string());
    let direction = ir_step.params.get("direction").cloned().unwrap_or_else(|| "ascending".to_string());

    // Map field names to sort_by modes
    let mode = match field.as_str() {
        "modification_time" | "mtime" | "date" | "time" => {
            if direction == "descending" { "mtime_desc" } else { "mtime" }
        }
        "size" => {
            if direction == "descending" { "size_desc" } else { "size" }
        }
        "name" | "alphabetical" => {
            if direction == "descending" { "name_desc" } else { "name" }
        }
        _ => "name",
    };

    steps.push(RawStep {
        op: "sort_by".to_string(),
        args: StepArgs::Scalar(mode.to_string()),
    });
}

/// Compile a "compress" action.
/// If the target looks like a file path, use gzip_compress.
/// Otherwise, use walk_tree + pack_archive for directory archiving.
fn compile_compress_step(
    target_path: &str,
    steps: &mut Vec<RawStep>,
) {
    if is_file_path(target_path) {
        steps.push(RawStep {
            op: "gzip_compress".to_string(),
            args: StepArgs::None,
        });
    } else {
        steps.push(RawStep {
            op: "walk_tree".to_string(),
            args: StepArgs::None,
        });
        steps.push(RawStep {
            op: "pack_archive".to_string(),
            args: StepArgs::None,
        });
    }
}

/// Check if a path looks like a file (has an extension).
fn is_file_path(path: &str) -> bool {
    if let Some(last) = path.rsplit('/').next() {
        last.contains('.') && !last.starts_with('.')
    } else {
        path.contains('.') && !path.starts_with('.')
    }
}

/// Compile a "decompress" action to extract_archive.
fn compile_decompress_step(steps: &mut Vec<RawStep>) {
    steps.push(RawStep {
        op: "extract_archive".to_string(),
        args: StepArgs::None,
    });
}

// ---------------------------------------------------------------------------
// Concept → pattern resolution
// ---------------------------------------------------------------------------

/// Resolve a concept label to file glob patterns using the NL vocab's
/// noun_patterns table.
///
/// E.g., "comic_issue_archive" → ["*.cbz", "*.cbr"]
///       "pdf_document" → ["*.pdf"]
///       "photograph" → ["*.png", "*.jpg", "*.jpeg", "*.heic"]
fn resolve_concept_to_patterns(concept: &str) -> Vec<String> {
    let v = vocab::vocab();

    // Direct lookup in noun_patterns by concept label
    // The noun_patterns table maps surface nouns to patterns,
    // so we need to find which noun maps to this concept and use its patterns.
    let lex = crate::nl::lexicon::lexicon();

    // Find all nouns that map to this concept
    for (word, info) in &lex.nouns {
        if info.concept == concept {
            // Look up this word in noun_patterns
            if let Some(patterns) = v.noun_patterns.get(word.as_str()) {
                return patterns.clone();
            }
        }
    }

    // Fallback: try the concept label itself as a noun_pattern key
    if let Some(patterns) = v.noun_patterns.get(concept) {
        return patterns.clone();
    }

    Vec::new()
}

/// Generate a plan name from the steps and target path.
fn generate_plan_name(steps: &[RawStep], target_path: &str) -> String {
    if steps.is_empty() {
        return "unnamed-plan".to_string();
    }

    let ops: Vec<&str> = steps.iter().map(|s| s.op.as_str()).collect();

    // Use the primary operation for the name
    let primary = if ops.contains(&"pack_archive") {
        "archive"
    } else if ops.contains(&"extract_archive") {
        "extract"
    } else if ops.contains(&"find_matching") {
        "find"
    } else if ops.contains(&"sort_by") {
        "sort"
    } else if ops.contains(&"list_dir") {
        "list"
    } else if ops.contains(&"walk_tree") {
        "walk"
    } else {
        ops.first().unwrap_or(&"plan")
    };

    // Add filter info if present
    let filter_info: Option<String> = steps.iter()
        .find(|s| s.op == "find_matching")
        .and_then(|s| match &s.args {
            StepArgs::Map(m) => m.get("pattern").cloned(),
            _ => None,
        });

    if let Some(pattern) = filter_info {
        let clean = pattern.replace("*.", "").replace(',', "-");
        if target_path != "." {
            format!("{}-{} in {}", primary, clean, target_path)
        } else {
            format!("{}-{}", primary, clean)
        }
    } else if target_path != "." {
        format!("{} in {}", primary, target_path)
    } else {
        primary.to_string()
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::nl::earley;
    use crate::nl::grammar::build_command_grammar;
    use crate::nl::intent_ir;
    use crate::nl::lexicon::lexicon;

    fn compile_input(input: &str) -> CompileResult {
        let grammar = build_command_grammar();
        let lex = lexicon();
        let tokens: Vec<String> = input.split_whitespace().map(|s| s.to_string()).collect();
        let parses = earley::parse(&grammar, &tokens, lex);
        let ir_result = intent_ir::parse_trees_to_intents(&parses);
        compile_intent(&ir_result)
    }

    fn expect_plan(input: &str) -> PlanDef {
        match compile_input(input) {
            CompileResult::Ok(plan) => plan,
            CompileResult::Error(e) => panic!("expected Ok, got Error: {}", e),
            CompileResult::NoIntent => panic!("expected Ok, got NoIntent"),
        }
    }

    #[test]
    fn test_find_comics_in_downloads_newest_first() {
        let plan = expect_plan("find comics in my downloads folder newest first");

        // Should have walk_tree, find_matching, sort_by
        let ops: Vec<&str> = plan.steps.iter().map(|s| s.op.as_str()).collect();
        assert!(ops.contains(&"walk_tree"), "should have walk_tree: {:?}", ops);
        assert!(ops.contains(&"find_matching"), "should have find_matching: {:?}", ops);
        assert!(ops.contains(&"sort_by"), "should have sort_by: {:?}", ops);

        // find_matching should have cbz/cbr patterns
        let filter = plan.steps.iter().find(|s| s.op == "find_matching").unwrap();
        match &filter.args {
            StepArgs::Map(m) => {
                let pattern = m.get("pattern").expect("should have pattern");
                assert!(pattern.contains("cbz") || pattern.contains("cbr"),
                    "pattern should contain cbz/cbr: {}", pattern);
            }
            _ => panic!("find_matching should have Map args"),
        }

        // sort_by should be mtime_desc
        let sort = plan.steps.iter().find(|s| s.op == "sort_by").unwrap();
        match &sort.args {
            StepArgs::Scalar(mode) => {
                assert!(mode.contains("mtime") && mode.contains("desc"),
                    "sort mode should be mtime_desc: {}", mode);
            }
            _ => panic!("sort_by should have Scalar args"),
        }
    }

    #[test]
    fn test_find_pdfs_in_documents() {
        let plan = expect_plan("find pdfs in documents");

        let ops: Vec<&str> = plan.steps.iter().map(|s| s.op.as_str()).collect();
        assert!(ops.contains(&"walk_tree"));
        assert!(ops.contains(&"find_matching"));

        let filter = plan.steps.iter().find(|s| s.op == "find_matching").unwrap();
        match &filter.args {
            StepArgs::Map(m) => {
                let pattern = m.get("pattern").expect("should have pattern");
                assert!(pattern.contains("pdf"), "pattern should contain pdf: {}", pattern);
            }
            _ => panic!("find_matching should have Map args"),
        }
    }

    #[test]
    fn test_zip_up_everything_in_downloads() {
        let plan = expect_plan("zip up everything in downloads");

        let ops: Vec<&str> = plan.steps.iter().map(|s| s.op.as_str()).collect();
        assert!(ops.contains(&"walk_tree"), "should have walk_tree: {:?}", ops);
        assert!(ops.contains(&"pack_archive"), "should have pack_archive: {:?}", ops);
    }

    #[test]
    fn test_extract_archive() {
        let plan = expect_plan("extract ~/comic.cbz");

        let ops: Vec<&str> = plan.steps.iter().map(|s| s.op.as_str()).collect();
        assert!(ops.contains(&"extract_archive"), "should have extract_archive: {:?}", ops);
    }

    #[test]
    fn test_list_bare_verb() {
        let plan = expect_plan("list");

        let ops: Vec<&str> = plan.steps.iter().map(|s| s.op.as_str()).collect();
        assert!(ops.contains(&"list_dir"), "should have list_dir: {:?}", ops);
    }

    #[test]
    fn test_sort_files_newest_first() {
        let plan = expect_plan("sort files newest first");

        let ops: Vec<&str> = plan.steps.iter().map(|s| s.op.as_str()).collect();
        assert!(ops.contains(&"sort_by"), "should have sort_by: {:?}", ops);
    }

    #[test]
    fn test_empty_input_no_intent() {
        match compile_input("") {
            CompileResult::NoIntent => {} // expected
            other => panic!("expected NoIntent, got: {:?}", other),
        }
    }

    #[test]
    fn test_gibberish_no_intent() {
        match compile_input("asdfghjkl qwerty") {
            CompileResult::NoIntent => {} // expected
            other => panic!("expected NoIntent, got: {:?}", other),
        }
    }

    #[test]
    fn test_concept_resolution_comics() {
        let patterns = resolve_concept_to_patterns("comic_issue_archive");
        assert!(!patterns.is_empty(), "should resolve comic_issue_archive to patterns");
        assert!(patterns.iter().any(|p| p.contains("cbz")),
            "should include *.cbz: {:?}", patterns);
    }

    #[test]
    fn test_concept_resolution_pdf() {
        let patterns = resolve_concept_to_patterns("pdf_document");
        assert!(!patterns.is_empty(), "should resolve pdf_document to patterns");
        assert!(patterns.iter().any(|p| p.contains("pdf")),
            "should include *.pdf: {:?}", patterns);
    }

    #[test]
    fn test_concept_resolution_unknown() {
        let patterns = resolve_concept_to_patterns("nonexistent_concept");
        assert!(patterns.is_empty(), "unknown concept should resolve to empty: {:?}", patterns);
    }

    #[test]
    fn test_plan_has_path_input() {
        let plan = expect_plan("find comics in downloads");
        assert!(!plan.inputs.is_empty());
        assert_eq!(plan.inputs[0].name, "path");
    }

    #[test]
    fn test_plan_name_generated() {
        let plan = expect_plan("find comics in downloads");
        assert!(!plan.name.is_empty(), "plan name should not be empty");
        assert!(plan.name.contains("find") || plan.name.contains("cbz"),
            "plan name should be descriptive: {}", plan.name);
    }

    #[test]
    fn test_no_steps_produces_walk_tree() {
        // "find files" with generic concept should still produce walk_tree
        let plan = expect_plan("find files");
        let ops: Vec<&str> = plan.steps.iter().map(|s| s.op.as_str()).collect();
        assert!(ops.contains(&"walk_tree"), "should have walk_tree: {:?}", ops);
    }

    #[test]
    fn test_bindings_populated_with_path() {
        let plan = expect_plan("find comics in downloads");
        assert!(!plan.bindings.is_empty(), "bindings should not be empty for path literal");
        let input_name = &plan.inputs[0].name;
        assert!(plan.bindings.contains_key(input_name),
            "bindings should contain input '{}': {:?}", input_name, plan.bindings);
        let bound_value = &plan.bindings[input_name];
        assert!(bound_value.contains("ownload"),
            "bound value should contain 'ownload': {}", bound_value);
    }

    #[test]
    fn test_bindings_empty_when_no_path() {
        // "find files" has no location → target_path defaults to "." → no binding
        let plan = expect_plan("find files");
        assert!(plan.bindings.is_empty(),
            "bindings should be empty when no path literal: {:?}", plan.bindings);
    }

    #[test]
    fn test_bindings_shown_in_yaml() {
        use crate::nl::dialogue::plan_to_yaml;
        let plan = expect_plan("find comics in downloads");
        let yaml = plan_to_yaml(&plan);
        // The YAML should show the bound path value
        assert!(yaml.contains("ownload"),
            "YAML should contain bound path value: {}", yaml);
    }

    #[test]
    fn test_bindings_with_home_dir() {
        let plan = expect_plan("list files in home");
        // "home" should resolve to ~/
        if !plan.bindings.is_empty() {
            let input_name = &plan.inputs[0].name;
            let bound = &plan.bindings[input_name];
            assert!(!bound.is_empty(), "bound path should not be empty");
        }
    }
}
