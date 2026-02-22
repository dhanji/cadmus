// ---------------------------------------------------------------------------
// calling_frame — resolve input bindings for plan execution
// ---------------------------------------------------------------------------
//
// A CallingFrame determines how plan inputs are resolved at execution time,
// and how a plan is compiled and turned into a runnable Racket script.
//
// Frames:
//   - DefaultFrame: resolves from literal bindings on the PlanDef
//     (the NL interactive case — user says "find comics in ~/Downloads"
//      and the path literal is bound directly).
//
// Future frames (not yet implemented):
//   - HttpFrame: resolve from HTTP request parameters
//   - EventFrame: resolve from event trigger payload
//   - EnvFrame: resolve from environment variables

use std::collections::HashMap;

use crate::plan::PlanDef;

// ---------------------------------------------------------------------------
// Trait
// ---------------------------------------------------------------------------

/// A calling frame resolves plan input names to concrete values and
/// orchestrates compilation of a plan into a runnable Racket script.
pub trait CallingFrame {
    /// Resolve an input by name. Returns the bound value, or a default
    /// if the input is not bound.
    fn resolve_input(&self, name: &str) -> String;

    /// Check if an input has an explicit binding.
    fn has_binding(&self, name: &str) -> bool;

    /// Get all bindings as a map.
    fn bindings(&self) -> &HashMap<String, String>;

    /// Compile a plan and generate a Racket script.
    ///
    /// This is the main entry point for plan execution. It:
    ///   1. Compiles the PlanDef through the type-checking pipeline
    ///   2. Builds the Racket operation registry
    ///   3. Generates a complete Racket script with bound parameters
    ///
    /// Returns the Racket script string on success. The caller is
    /// responsible for writing it to a file and executing it (since
    /// that involves IO and UI concerns).
    fn invoke(&self, plan: &PlanDef) -> Result<String, InvokeError>;
}

// ---------------------------------------------------------------------------
// InvokeError
// ---------------------------------------------------------------------------

/// Errors that can occur during plan invocation.
#[derive(Debug)]
pub enum InvokeError {
    /// Plan failed to compile (type errors, unknown ops, etc.)
    CompileError(String),
    /// Racket code generation failed
    CodegenError(String),
}

impl std::fmt::Display for InvokeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InvokeError::CompileError(msg) => write!(f, "compile error: {}", msg),
            InvokeError::CodegenError(msg) => write!(f, "codegen error: {}", msg),
        }
    }
}

// ---------------------------------------------------------------------------
// DefaultFrame — resolve from PlanDef literal bindings
// ---------------------------------------------------------------------------

/// The default calling frame: resolves inputs from literal bindings
/// stored on the PlanDef. This is the frame used in interactive NL mode
/// where the user provides path literals in their natural language input.
///
/// Unbound inputs resolve to a sensible default:
///   - "path", "dir", "textdir" → "."
///   - "file" → error (no sensible default for a file)
///   - anything else → "."
pub struct DefaultFrame {
    bindings: HashMap<String, String>,
}

impl DefaultFrame {
    /// Create a DefaultFrame from a bindings map.
    pub fn new(bindings: HashMap<String, String>) -> Self {
        Self { bindings }
    }

    /// Create a DefaultFrame from a PlanDef's bindings.
    pub fn from_plan(plan: &PlanDef) -> Self {
        Self::new(plan.bindings.clone())
    }

    /// Create an empty frame (no bindings — all inputs use defaults).
    pub fn empty() -> Self {
        Self { bindings: HashMap::new() }
    }

    /// Default value for an unbound input based on its name.
    fn default_for(name: &str) -> &'static str {
        match name {
            "path" | "dir" | "textdir" | "pathref" => ".",
            _ => ".",
        }
    }
}

impl CallingFrame for DefaultFrame {
    fn resolve_input(&self, name: &str) -> String {
        self.bindings.get(name)
            .cloned()
            .unwrap_or_else(|| Self::default_for(name).to_string())
    }

    fn has_binding(&self, name: &str) -> bool {
        self.bindings.contains_key(name)
    }

    fn bindings(&self) -> &HashMap<String, String> {
        &self.bindings
    }

    fn invoke(&self, plan: &PlanDef) -> Result<String, InvokeError> {
        // 1. Compile the plan through the type-checking pipeline
        let registry = crate::fs_types::build_full_registry();
        let compiled = crate::plan::compile_plan(plan, &registry)
            .map_err(|e| InvokeError::CompileError(format!("{}", e)))?;

        // 2. Build the Racket operation registry
        let racket_reg = crate::racket_executor::build_racket_registry();

        // 3. Generate the Racket script
        crate::racket_executor::generate_racket_script(&compiled, plan, &racket_reg)
            .map_err(|e| InvokeError::CodegenError(format!("{}", e)))
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_frame_resolves_bound_path() {
        let mut bindings = HashMap::new();
        bindings.insert("path".to_string(), "~/Downloads".to_string());
        let frame = DefaultFrame::new(bindings);
        assert_eq!(frame.resolve_input("path"), "~/Downloads");
    }

    #[test]
    fn test_default_frame_unbound_path_returns_dot() {
        let frame = DefaultFrame::empty();
        assert_eq!(frame.resolve_input("path"), ".");
    }

    #[test]
    fn test_default_frame_unbound_dir_returns_dot() {
        let frame = DefaultFrame::empty();
        assert_eq!(frame.resolve_input("dir"), ".");
        assert_eq!(frame.resolve_input("textdir"), ".");
        assert_eq!(frame.resolve_input("pathref"), ".");
    }

    #[test]
    fn test_default_frame_has_binding() {
        let mut bindings = HashMap::new();
        bindings.insert("path".to_string(), "~/Downloads".to_string());
        let frame = DefaultFrame::new(bindings);
        assert!(frame.has_binding("path"));
        assert!(!frame.has_binding("file"));
    }

    #[test]
    fn test_default_frame_from_plan() {
        let mut bindings = HashMap::new();
        bindings.insert("path".to_string(), "/tmp/test".to_string());
        let plan = PlanDef {
            name: "test".to_string(),
            inputs: vec![crate::plan::PlanInput::bare("path")],
            output: None,
            steps: vec![],
            bindings,
        };
        let frame = DefaultFrame::from_plan(&plan);
        assert_eq!(frame.resolve_input("path"), "/tmp/test");
    }

    #[test]
    fn test_default_frame_empty_bindings() {
        let frame = DefaultFrame::empty();
        assert!(frame.bindings().is_empty());
        assert_eq!(frame.resolve_input("anything"), ".");
    }

    #[test]
    fn test_default_frame_multiple_bindings() {
        let mut bindings = HashMap::new();
        bindings.insert("path".to_string(), "~/Downloads".to_string());
        bindings.insert("file".to_string(), "~/doc.pdf".to_string());
        let frame = DefaultFrame::new(bindings);
        assert_eq!(frame.resolve_input("path"), "~/Downloads");
        assert_eq!(frame.resolve_input("file"), "~/doc.pdf");
    }

    #[test]
    fn test_default_frame_path_with_spaces() {
        let mut bindings = HashMap::new();
        bindings.insert("path".to_string(), "~/My Documents".to_string());
        let frame = DefaultFrame::new(bindings);
        assert_eq!(frame.resolve_input("path"), "~/My Documents");
    }

    // -- invoke tests --

    #[test]
    fn test_invoke_produces_racket_script() {
        let mut bindings = HashMap::new();
        bindings.insert("path".to_string(), "~/Downloads".to_string());
        let plan = PlanDef {
            name: "list-downloads".to_string(),
            inputs: vec![crate::plan::PlanInput::bare("path")],
            output: None,
            steps: vec![
                crate::plan::RawStep {
                    op: "list_dir".to_string(),
                    args: crate::plan::StepArgs::None,
                },
            ],
            bindings,
        };
        let frame = DefaultFrame::from_plan(&plan);
        let script = frame.invoke(&plan).expect("invoke should succeed");
        assert!(script.contains("#lang racket"), "should have racket preamble");
        assert!(script.contains("~/Downloads"), "should use bound path: {}", script);
    }

    #[test]
    fn test_invoke_empty_bindings_uses_defaults() {
        let plan = PlanDef {
            name: "list-cwd".to_string(),
            inputs: vec![crate::plan::PlanInput::bare("path")],
            output: None,
            steps: vec![
                crate::plan::RawStep {
                    op: "list_dir".to_string(),
                    args: crate::plan::StepArgs::None,
                },
            ],
            bindings: HashMap::new(),
        };
        let frame = DefaultFrame::from_plan(&plan);
        let script = frame.invoke(&plan).expect("invoke should succeed");
        assert!(script.contains("#lang racket"));
        // With no bindings, fs_path_operand falls back to "."
        assert!(script.contains("\".\""), "should use default dot path: {}", script);
    }

    #[test]
    fn test_invoke_invalid_plan_returns_error() {
        let plan = PlanDef {
            name: "bad-plan".to_string(),
            inputs: vec![],
            output: None,
            steps: vec![
                crate::plan::RawStep {
                    op: "nonexistent_op_xyz".to_string(),
                    args: crate::plan::StepArgs::None,
                },
            ],
            bindings: HashMap::new(),
        };
        let frame = DefaultFrame::from_plan(&plan);
        let result = frame.invoke(&plan);
        assert!(result.is_err(), "should fail for unknown op");
        let err = result.unwrap_err();
        assert!(matches!(err, InvokeError::CompileError(_)));
    }

    #[test]
    fn test_invoke_multi_step_plan() {
        let mut bindings = HashMap::new();
        bindings.insert("path".to_string(), "~/Documents".to_string());
        let plan = PlanDef {
            name: "find-pdfs".to_string(),
            inputs: vec![crate::plan::PlanInput::bare("path")],
            output: None,
            steps: vec![
                crate::plan::RawStep {
                    op: "walk_tree".to_string(),
                    args: crate::plan::StepArgs::None,
                },
                crate::plan::RawStep {
                    op: "find_matching".to_string(),
                    args: crate::plan::StepArgs::Map(
                        [("pattern".to_string(), "*.pdf".to_string())].into_iter().collect()
                    ),
                },
            ],
            bindings,
        };
        let frame = DefaultFrame::from_plan(&plan);
        let script = frame.invoke(&plan).expect("invoke should succeed");
        assert!(script.contains("#lang racket"));
        assert!(script.contains("~/Documents"), "should use bound path: {}", script);
        assert!(script.contains("step-1"), "should have let* bindings for multi-step");
    }
}
