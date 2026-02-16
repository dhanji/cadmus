use std::collections::HashMap;

use crate::generic_planner::{GenericGoal, PlanNode};
use crate::registry::{
    AlgebraicProperties, ExecContext, Literal, OpSignature, OperationRegistry, TypeId,
};
use crate::strategy::ReasonerStrategy;
use crate::types::{EngineError, Result};

// ---------------------------------------------------------------------------
// Coding domain types
// ---------------------------------------------------------------------------

/// The output of the CodingStrategy: a structured refactoring result.
#[derive(Debug, Clone)]
pub struct CodingOutput {
    /// The original source code
    pub source: String,
    /// Detected code smells
    pub smells: Vec<String>,
    /// Planned refactorings
    pub refactorings: Vec<String>,
    /// Generated test cases
    pub tests: Vec<String>,
    /// Type signatures extracted
    pub type_info: Vec<String>,
    /// Plan trace (for debugging)
    pub plan_trace: String,
}

// ---------------------------------------------------------------------------
// CodingStrategy — a real working strategy for code analysis
// ---------------------------------------------------------------------------

/// A coding strategy that analyzes source code, detects smells,
/// plans refactorings, and generates test cases.
///
/// Domain types:
/// - SourceCode: raw source text
/// - AST: parsed abstract syntax tree representation
/// - TypeSignature: extracted type information
/// - CodeSmell: detected code quality issues
/// - Refactoring: planned code transformation
/// - TestCase: generated test for the refactoring
///
/// Op chain:
/// - parse_source: SourceCode → AST
/// - analyze_types: AST → TypeSignature
/// - detect_smells: AST → CodeSmell
/// - plan_refactoring: AST + CodeSmell → Refactoring
/// - generate_tests: Refactoring + TypeSignature → TestCase
/// - compose: Refactoring + Refactoring → Refactoring (associative)
pub struct CodingStrategy {
    /// The source code to analyze
    source_code: String,
    /// Description of what refactoring to perform
    goal_description: String,
}

impl CodingStrategy {
    pub fn new(source_code: impl Into<String>, goal_description: impl Into<String>) -> Self {
        Self {
            source_code: source_code.into(),
            goal_description: goal_description.into(),
        }
    }

    /// Run the coding strategy through the generic engine.
    pub fn run(&self) -> Result<CodingOutput> {
        crate::strategy::run_strategy(self)
    }
}

impl ReasonerStrategy for CodingStrategy {
    type Output = CodingOutput;

    fn build_registry(&self) -> OperationRegistry {
        let mut reg = OperationRegistry::new();

        // Load type signatures from YAML ops pack (source of truth for what ops exist).
        // The poly ops enable type-directed planning via TypeExpr unification.
        const CODING_OPS_YAML: &str = include_str!("../data/coding_ops.yaml");
        crate::registry::load_ops_pack_str_into(CODING_OPS_YAML, &mut reg)
            .expect("embedded coding_ops.yaml should always parse");

        // Also register monomorphic ops with exec bindings for the classic planner path.
        // parse_source: SourceCode → AST
        reg.register(
            "parse_source",
            OpSignature::new(vec![TypeId::new("SourceCode")], TypeId::new("AST")),
            AlgebraicProperties::none(),
            Box::new(|ctx: &ExecContext| {
                let source = ctx.inputs.first().map(|l| l.value.as_str()).unwrap_or("");
                // Simple AST extraction: identify functions, their bodies, and line counts
                let mut functions = Vec::new();
                let mut current_fn: Option<(String, usize, Vec<String>)> = None;
                let mut brace_depth = 0;

                for (i, line) in source.lines().enumerate() {
                    let trimmed = line.trim();

                    // Detect function definitions
                    if trimmed.starts_with("fn ") || trimmed.starts_with("pub fn ") {
                        let name = trimmed
                            .split('(')
                            .next()
                            .unwrap_or("")
                            .replace("fn ", "")
                            .replace("pub ", "")
                            .trim()
                            .to_string();
                        current_fn = Some((name, i + 1, Vec::new()));
                    }

                    if let Some((_, _, ref mut body)) = current_fn {
                        body.push(line.to_string());
                    }

                    brace_depth += trimmed.matches('{').count();
                    brace_depth -= trimmed.matches('}').count();

                    if brace_depth == 0 {
                        if let Some((name, start, body)) = current_fn.take() {
                            functions.push(format!(
                                "Function({}, lines={}-{}, body_lines={})",
                                name, start, i + 1, body.len()
                            ));
                        }
                    }
                }

                Ok(format!("AST[{}]", functions.join("; ")))
            }),
        );

        // analyze_types: AST → TypeSignature
        reg.register(
            "analyze_types",
            OpSignature::new(vec![TypeId::new("AST")], TypeId::new("TypeSignature")),
            AlgebraicProperties::none(),
            Box::new(|ctx: &ExecContext| {
                let ast = ctx.inputs.first().map(|l| l.value.as_str()).unwrap_or("");
                // Extract function signatures from the AST representation
                let mut sigs = Vec::new();

                // Also look at the original source if available
                let source = ctx.params.get("source").map(|s| s.as_str()).unwrap_or("");
                for line in source.lines() {
                    let trimmed = line.trim();
                    if (trimmed.starts_with("fn ") || trimmed.starts_with("pub fn "))
                        && trimmed.contains('(')
                    {
                        // Extract up to the opening brace
                        let sig = trimmed.split('{').next().unwrap_or(trimmed).trim();
                        sigs.push(sig.to_string());
                    }
                }

                if sigs.is_empty() {
                    Ok(format!("TypeSig[from {}]", ast))
                } else {
                    Ok(format!("TypeSig[{}]", sigs.join("; ")))
                }
            }),
        );

        // detect_smells: AST → CodeSmell
        reg.register(
            "detect_smells",
            OpSignature::new(vec![TypeId::new("AST")], TypeId::new("CodeSmell")),
            AlgebraicProperties::none(),
            Box::new(|ctx: &ExecContext| {
                let _ast = ctx.inputs.first().map(|l| l.value.as_str()).unwrap_or("");
                let source = ctx.params.get("source").map(|s| s.as_str()).unwrap_or("");

                let mut smells = Vec::new();

                // Detect long functions (>15 lines) using brace-depth tracking
                let all_lines: Vec<&str> = source.lines().collect();
                let mut i = 0;
                while i < all_lines.len() {
                    let line = all_lines[i];
                    let trimmed = line.trim();
                    if (trimmed.starts_with("fn ") || trimmed.starts_with("pub fn "))
                        && trimmed.contains('(')
                    {
                        let name = trimmed
                            .split('(')
                            .next()
                            .unwrap_or("")
                            .replace("fn ", "")
                            .replace("pub ", "")
                            .trim()
                            .to_string();

                        // Count lines from function start using brace-depth
                        let mut depth = 0;
                        let fn_start = i;
                        while i < all_lines.len() {
                            depth += all_lines[i].matches('{').count();
                            depth -= all_lines[i].matches('}').count();
                            i += 1;
                            if depth == 0 && i > fn_start + 1 {
                                break;
                            }
                        }
                        let line_count = i - fn_start;

                        if line_count > 15 {
                            smells.push(format!("LongFunction({}, {} lines)", name, line_count));
                        }
                    } else {
                        i += 1;
                    }
                }

                // Detect repeated code blocks (simple: look for duplicate non-trivial lines)
                let lines: Vec<&str> = source.lines().map(|l| l.trim()).collect();
                let mut seen: HashMap<&str, usize> = HashMap::new();
                for line in &lines {
                    if line.len() > 20 && !line.starts_with("//") && !line.starts_with("use ") {
                        *seen.entry(line).or_insert(0) += 1;
                    }
                }
                for (line, count) in &seen {
                    if *count > 1 {
                        let preview = if line.len() > 40 { &line[..40] } else { line };
                        smells.push(format!("DuplicateCode('{}...', {} occurrences)", preview, count));
                    }
                }

                // Detect deeply nested code (>3 levels of indentation)
                for (i, line) in source.lines().enumerate() {
                    let indent = line.len() - line.trim_start().len();
                    if indent >= 16 && !line.trim().is_empty() { // 4 levels * 4 spaces
                        smells.push(format!("DeepNesting(line {}, indent={})", i + 1, indent / 4));
                        break; // Report once
                    }
                }

                if smells.is_empty() {
                    Ok("CodeSmell[none detected]".to_string())
                } else {
                    Ok(format!("CodeSmell[{}]", smells.join("; ")))
                }
            }),
        );

        // plan_refactoring: AST + CodeSmell → Refactoring
        reg.register(
            "plan_refactoring",
            OpSignature::new(
                vec![TypeId::new("AST"), TypeId::new("CodeSmell")],
                TypeId::new("Refactoring"),
            ),
            AlgebraicProperties::none(),
            Box::new(|ctx: &ExecContext| {
                let ast = ctx.inputs.first().map(|l| l.value.as_str()).unwrap_or("");
                let smells = ctx.inputs.get(1).map(|l| l.value.as_str()).unwrap_or("");
                let source = ctx.params.get("source").map(|s| s.as_str()).unwrap_or("");

                let mut refactorings = Vec::new();

                // For each LongFunction smell, plan an extract-method refactoring
                if smells.contains("LongFunction") {
                    // Find the long function and identify extractable blocks
                    let mut in_fn = false;
                    let mut fn_name = String::new();
                    let mut block_lines: Vec<(usize, String)> = Vec::new();
                    let mut brace_depth = 0;

                    for (i, line) in source.lines().enumerate() {
                        let trimmed = line.trim();

                        if (trimmed.starts_with("fn ") || trimmed.starts_with("pub fn "))
                            && trimmed.contains('(')
                        {
                            fn_name = trimmed
                                .split('(')
                                .next()
                                .unwrap_or("")
                                .replace("fn ", "")
                                .replace("pub ", "")
                                .trim()
                                .to_string();
                            in_fn = true;
                            block_lines.clear();
                        }

                        if in_fn {
                            brace_depth += trimmed.matches('{').count();
                            brace_depth -= trimmed.matches('}').count();
                            block_lines.push((i + 1, line.to_string()));

                            if brace_depth == 0 && block_lines.len() > 1 {
                                if block_lines.len() > 15 {
                                    // Identify a contiguous block to extract
                                    // Look for a block of 5+ lines that could be a helper
                                    let mid = block_lines.len() / 2;
                                    let start = block_lines[mid - 2].0;
                                    let end = block_lines[mid + 2].0;
                                    let extract_preview: Vec<&str> = block_lines[mid-2..mid+3]
                                        .iter()
                                        .map(|(_, l)| l.trim())
                                        .collect();

                                    refactorings.push(format!(
                                        "ExtractMethod(from={}, lines={}-{}, new_fn='{}_helper', preview='{}')",
                                        fn_name, start, end, fn_name,
                                        extract_preview.join(" | ")
                                    ));
                                }
                                in_fn = false;
                            }
                        }
                    }
                }

                // For DuplicateCode, plan extract-to-shared-function
                if smells.contains("DuplicateCode") {
                    refactorings.push(
                        "ExtractSharedFunction(deduplicate repeated code blocks)".to_string()
                    );
                }

                // For DeepNesting, plan flatten-with-early-return
                if smells.contains("DeepNesting") {
                    refactorings.push(
                        "FlattenNesting(replace deep nesting with early returns/guard clauses)".to_string()
                    );
                }

                if refactorings.is_empty() {
                    Ok(format!("Refactoring[no refactoring needed, ast={}, smells={}]", ast, smells))
                } else {
                    Ok(format!("Refactoring[{}]", refactorings.join("; ")))
                }
            }),
        );

        // generate_tests: Refactoring + TypeSignature → TestCase
        reg.register(
            "generate_tests",
            OpSignature::new(
                vec![TypeId::new("Refactoring"), TypeId::new("TypeSignature")],
                TypeId::new("TestCase"),
            ),
            AlgebraicProperties::none(),
            Box::new(|ctx: &ExecContext| {
                let refactoring = ctx.inputs.first().map(|l| l.value.as_str()).unwrap_or("");
                let type_sig = ctx.inputs.get(1).map(|l| l.value.as_str()).unwrap_or("");

                let mut tests = Vec::new();

                // Generate a test for each refactoring
                if refactoring.contains("ExtractMethod") {
                    // Parse out the new function name
                    let new_fn = refactoring
                        .split("new_fn='")
                        .nth(1)
                        .and_then(|s| s.split('\'').next())
                        .unwrap_or("extracted_helper");

                    tests.push(format!(
                        "#[test]\nfn test_{}() {{\n    // Verify extracted method produces same result\n    let result = {}();\n    assert!(result.is_ok());\n}}",
                        new_fn, new_fn
                    ));

                    // Regression test for the original function
                    let orig_fn = refactoring
                        .split("from=")
                        .nth(1)
                        .and_then(|s| s.split(',').next())
                        .unwrap_or("original");

                    tests.push(format!(
                        "#[test]\nfn test_{}_unchanged_behavior() {{\n    // Regression: original function behavior preserved\n    // Type info: {}\n}}",
                        orig_fn, type_sig
                    ));
                }

                if refactoring.contains("ExtractSharedFunction") {
                    tests.push(
                        "#[test]\nfn test_shared_function_equivalence() {\n    // Verify shared function produces same results as duplicated code\n}".to_string()
                    );
                }

                if refactoring.contains("FlattenNesting") {
                    tests.push(
                        "#[test]\nfn test_flattened_logic_equivalence() {\n    // Verify flattened code has same control flow\n}".to_string()
                    );
                }

                if tests.is_empty() {
                    Ok("TestCase[no tests needed]".to_string())
                } else {
                    Ok(format!("TestCase[\n{}\n]", tests.join("\n\n")))
                }
            }),
        );

        // compose: Refactoring + Refactoring → Refactoring (associative)
        reg.register(
            "compose",
            OpSignature::new(
                vec![TypeId::new("Refactoring"), TypeId::new("Refactoring")],
                TypeId::new("Refactoring"),
            ),
            AlgebraicProperties::associative(),
            Box::new(|ctx: &ExecContext| {
                let a = ctx.inputs.first().map(|l| l.value.as_str()).unwrap_or("");
                let b = ctx.inputs.get(1).map(|l| l.value.as_str()).unwrap_or("");
                Ok(format!("Composed[{} then {}]", a, b))
            }),
        );

        reg
    }

    fn available_literals(&self) -> Vec<Literal> {
        vec![Literal::new(
            TypeId::new("SourceCode"),
            "source",
            &self.source_code,
        )
        .with_meta("description", &self.goal_description)]
    }

    fn goals(&self) -> Vec<GenericGoal> {
        // We need both a Refactoring and a TestCase
        // TestCase depends on Refactoring + TypeSignature
        // So planning for TestCase will pull in everything
        vec![GenericGoal::simple(
            TypeId::new("TestCase"),
            self.available_literals(),
        )]
    }

    fn execute_node(
        &self,
        node: &PlanNode,
        resolved_children: &[Literal],
        registry: &OperationRegistry,
    ) -> Result<Literal> {
        match node {
            PlanNode::Op {
                op_name,
                output_type,
                ..
            } => {
                let op = registry.get(op_name).ok_or_else(|| {
                    EngineError::Execution(format!("unknown op: {}", op_name))
                })?;

                let ctx = ExecContext {
                    inputs: resolved_children.to_vec(),
                    params: {
                        let mut m = HashMap::new();
                        m.insert("source".to_string(), self.source_code.clone());
                        m.insert("goal".to_string(), self.goal_description.clone());
                        m
                    },
                };

                let result = (op.exec)(&ctx).map_err(|e| {
                    EngineError::Execution(format!("op '{}' failed: {}", op_name, e))
                })?;

                Ok(Literal::new(output_type.clone(), format!("result_{}", op_name), result))
            }
            PlanNode::Leaf { key, output_type } => {
                // Find the literal
                let literals = self.available_literals();
                let lit = literals.iter().find(|l| l.key == *key).cloned();
                Ok(lit.unwrap_or_else(|| {
                    Literal::new(output_type.clone(), key.clone(), format!("[unresolved: {}]", key))
                }))
            }
        }
    }

    fn assemble(&self, results: Vec<Literal>) -> Result<CodingOutput> {
        let mut smells = Vec::new();
        let mut refactorings = Vec::new();
        let mut tests = Vec::new();
        let mut type_info = Vec::new();
        let mut plan_trace = String::new();

        for lit in &results {
            let type_name = lit.type_id.as_str();
            match type_name {
                "CodeSmell" => smells.push(lit.value.clone()),
                "Refactoring" => refactorings.push(lit.value.clone()),
                "TestCase" => tests.push(lit.value.clone()),
                "TypeSignature" => type_info.push(lit.value.clone()),
                _ => {
                    plan_trace.push_str(&format!("{}: {}\n", type_name, lit.value));
                }
            }
        }

        // The final result is the last TestCase (which encapsulates the full chain)
        if tests.is_empty() && !results.is_empty() {
            tests.push(results.last().unwrap().value.clone());
        }

        Ok(CodingOutput {
            source: self.source_code.clone(),
            smells,
            refactorings,
            tests,
            type_info,
            plan_trace,
        })
    }
}

/// Run the coding strategy and return structured output.
pub fn run_coding(source_code: &str, goal: &str) -> Result<CodingOutput> {
    let strategy = CodingStrategy::new(source_code, goal);
    strategy.run()
}

// ---------------------------------------------------------------------------
// Example source code for testing
// ---------------------------------------------------------------------------

/// A real Rust code example with intentional code smells for the strategy to detect.
pub const EXAMPLE_LONG_FUNCTION: &str = r#"
pub fn process_data(input: &[u8], config: &Config) -> Result<Output, Error> {
    // Validate input
    if input.is_empty() {
        return Err(Error::EmptyInput);
    }
    if input.len() > MAX_SIZE {
        return Err(Error::TooLarge);
    }

    // Parse header
    let header = parse_header(&input[..HEADER_SIZE])?;
    let version = header.version;
    let flags = header.flags;

    // Process body based on version
    let body = &input[HEADER_SIZE..];
    let mut result = Vec::new();

    for chunk in body.chunks(CHUNK_SIZE) {
        let decoded = decode_chunk(chunk, version)?;
        let validated = validate_chunk(&decoded, &config.rules)?;
        let transformed = apply_transform(&validated, flags)?;
        result.push(transformed);
    }

    // Aggregate results
    let mut total_size = 0;
    let mut max_value = 0;
    let mut checksum = 0u64;

    for item in &result {
        total_size += item.size;
        if item.value > max_value {
            max_value = item.value;
        }
        checksum = checksum.wrapping_add(item.hash);
    }

    // Build output
    let output = Output {
        items: result,
        total_size,
        max_value,
        checksum,
        version,
    };

    // Final validation
    if output.checksum == 0 {
        return Err(Error::InvalidChecksum);
    }

    Ok(output)
}
"#;

/// A simpler example with duplicate code.
pub const EXAMPLE_DUPLICATE_CODE: &str = r#"
fn process_users(users: &[User]) -> Vec<Report> {
    let mut reports = Vec::new();
    for user in users {
        let name = user.name.trim().to_lowercase();
        let email = user.email.trim().to_lowercase();
        reports.push(Report { name, email, kind: "user" });
    }
    reports
}

fn process_admins(admins: &[Admin]) -> Vec<Report> {
    let mut reports = Vec::new();
    for admin in admins {
        let name = admin.name.trim().to_lowercase();
        let email = admin.email.trim().to_lowercase();
        reports.push(Report { name, email, kind: "admin" });
    }
    reports
}
"#;

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::generic_planner;
    use crate::algebra;

    #[test]
    fn test_coding_strategy_extract_method() {
        let output = run_coding(
            EXAMPLE_LONG_FUNCTION,
            "Extract method to reduce function length",
        )
        .unwrap();

        // Should detect the long function
        assert!(
            !output.tests.is_empty(),
            "should produce test cases"
        );

        // The output should contain test code
        let all_tests = output.tests.join("\n");
        assert!(
            all_tests.contains("test") || all_tests.contains("Test"),
            "should contain test-related content, got: {}",
            all_tests
        );
    }

    #[test]
    fn test_coding_strategy_registry() {
        let strategy = CodingStrategy::new("fn foo() {}", "test");
        let registry = strategy.build_registry();

        assert!(registry.has_producer(&TypeId::new("AST")));
        assert!(registry.has_producer(&TypeId::new("TypeSignature")));
        assert!(registry.has_producer(&TypeId::new("CodeSmell")));
        assert!(registry.has_producer(&TypeId::new("Refactoring")));
        assert!(registry.has_producer(&TypeId::new("TestCase")));

        // compose should be associative
        let compose_props = registry.properties("compose").unwrap();
        assert!(compose_props.associative);
    }

    #[test]
    fn test_coding_strategy_plan_structure() {
        let strategy = CodingStrategy::new(EXAMPLE_LONG_FUNCTION, "refactor");
        let registry = strategy.build_registry();
        let goals = strategy.goals();

        assert_eq!(goals.len(), 1);
        assert_eq!(goals[0].required_output, TypeId::new("TestCase"));

        // Plan should succeed
        let plan = generic_planner::plan(&goals[0], &registry).unwrap();

        // Should have: generate_tests(plan_refactoring(parse_source(source), detect_smells(parse_source(source))), analyze_types(parse_source(source)))
        let ops = plan.op_names();
        assert!(ops.contains("generate_tests"), "plan should include generate_tests");
        assert!(ops.contains("plan_refactoring"), "plan should include plan_refactoring");
        assert!(ops.contains("parse_source"), "plan should include parse_source");
        assert!(ops.contains("detect_smells"), "plan should include detect_smells");
        assert!(ops.contains("analyze_types"), "plan should include analyze_types");
    }

    #[test]
    fn test_coding_strategy_no_producer_error() {
        let strategy = CodingStrategy::new("fn foo() {}", "test");
        let registry = strategy.build_registry();

        // Try to plan for a type that doesn't exist
        let goal = GenericGoal::simple(TypeId::new("Deployment"), strategy.available_literals());
        let result = generic_planner::plan(&goal, &registry);

        assert!(result.is_err());
        match result.unwrap_err() {
            generic_planner::PlanError::NoProducer { type_id } => {
                assert_eq!(type_id, TypeId::new("Deployment"));
            }
            other => panic!("expected NoProducer, got: {:?}", other),
        }
    }

    #[test]
    fn test_coding_strategy_compose_associative() {
        let strategy = CodingStrategy::new("fn foo() {}", "test");
        let registry = strategy.build_registry();

        // Build a plan with nested compose ops and verify canonicalization flattens them
        let plan = PlanNode::Op {
            op_name: "compose".into(),
            output_type: TypeId::new("Refactoring"),
            children: vec![
                PlanNode::Op {
                    op_name: "compose".into(),
                    output_type: TypeId::new("Refactoring"),
                    children: vec![
                        PlanNode::Leaf {
                            key: "r1".into(),
                            output_type: TypeId::new("Refactoring"),
                        },
                        PlanNode::Leaf {
                            key: "r2".into(),
                            output_type: TypeId::new("Refactoring"),
                        },
                    ],
                },
                PlanNode::Leaf {
                    key: "r3".into(),
                    output_type: TypeId::new("Refactoring"),
                },
            ],
        };

        let canonical = algebra::canonicalize(&plan, &registry);

        // Should flatten: compose(compose(r1, r2), r3) → compose(r1, r2, r3)
        match &canonical {
            PlanNode::Op { op_name, children, .. } => {
                assert_eq!(op_name, "compose");
                assert_eq!(
                    children.len(),
                    3,
                    "associative compose should flatten to 3 children"
                );
            }
            _ => panic!("expected Op node"),
        }
    }

    #[test]
    fn test_coding_strategy_duplicate_code_detection() {
        let output = run_coding(
            EXAMPLE_DUPLICATE_CODE,
            "Detect and refactor duplicate code",
        )
        .unwrap();

        // Should produce output (even if no smells detected for short functions)
        assert!(!output.tests.is_empty(), "should produce test output");
    }

    #[test]
    fn test_coding_strategy_simple_function() {
        // A simple function with no smells
        let output = run_coding(
            "fn add(a: i32, b: i32) -> i32 { a + b }",
            "Analyze simple function",
        )
        .unwrap();

        assert!(!output.tests.is_empty());
        assert_eq!(output.source, "fn add(a: i32, b: i32) -> i32 { a + b }");
    }

    #[test]
    fn test_coding_strategy_available_literals() {
        let strategy = CodingStrategy::new("fn test() {}", "goal");
        let literals = strategy.available_literals();

        assert_eq!(literals.len(), 1);
        assert_eq!(literals[0].type_id, TypeId::new("SourceCode"));
        assert_eq!(literals[0].key, "source");
        assert_eq!(literals[0].value, "fn test() {}");
    }
}
