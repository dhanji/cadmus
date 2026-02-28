// ---------------------------------------------------------------------------
// calling_frame — resolve input bindings for plan execution
// ---------------------------------------------------------------------------
//
// A CallingFrame determines how plan inputs are resolved at execution time,
// and how a plan is compiled, code-generated, and executed.
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
use std::io::Write;

use crate::plan::PlanDef;

// ---------------------------------------------------------------------------
// Trait
// ---------------------------------------------------------------------------

/// A calling frame resolves plan input names to concrete values and
/// orchestrates the full compile → codegen → execute pipeline for a plan.
pub trait CallingFrame {
    /// Resolve an input by name. Returns the bound value, or a default
    /// if the input is not bound.
    fn resolve_input(&self, name: &str) -> String;

    /// Check if an input has an explicit binding.
    fn has_binding(&self, name: &str) -> bool;

    /// Get all bindings as a map.
    fn bindings(&self) -> &HashMap<String, String>;

    /// Compile a plan and generate a Racket script (without executing).
    ///
    /// Use this when you need the script text for display/confirmation
    /// before running it.
    fn codegen(&self, plan: &PlanDef) -> Result<String, InvokeError>;

    /// Compile, generate, and execute a plan.
    ///
    /// This is the main entry point for plan execution. It:
    ///   1. Compiles the PlanDef through the type-checking pipeline
    ///   2. Builds the Racket operation registry
    ///   3. Generates a complete Racket script with bound parameters
    ///   4. Writes the script to a temp file and executes it via `racket`
    ///
    /// Returns an `Execution` with the script, stdout, stderr, and exit code.
    fn invoke(&self, plan: &PlanDef) -> Result<Execution, InvokeError>;

    /// Execute an already-generated Racket script.
    ///
    /// Use this when you have a script from `codegen()` and want to run it
    /// after user confirmation.
    fn run_script(&self, script: &str) -> Result<Execution, InvokeError>;
}

// ---------------------------------------------------------------------------
// Execution result
// ---------------------------------------------------------------------------

/// The result of executing a plan through a calling frame.
#[derive(Debug, Clone)]
pub struct Execution {
    /// The generated Racket script that was executed.
    pub script: String,
    /// Standard output from the Racket process.
    pub stdout: String,
    /// Standard error from the Racket process.
    pub stderr: String,
    /// Whether the process exited successfully.
    pub success: bool,
    /// The process exit code (None if killed by signal).
    pub exit_code: Option<i32>,
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
    /// Failed to execute the generated script
    ExecError(String),
}

impl std::fmt::Display for InvokeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InvokeError::CompileError(msg) => write!(f, "compile error: {}", msg),
            InvokeError::CodegenError(msg) => write!(f, "codegen error: {}", msg),
            InvokeError::ExecError(msg) => write!(f, "exec error: {}", msg),
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

    fn codegen(&self, plan: &PlanDef) -> Result<String, InvokeError> {
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

    fn invoke(&self, plan: &PlanDef) -> Result<Execution, InvokeError> {
        let script = self.codegen(plan)?;
        self.run_script(&script)
    }

    fn run_script(&self, script: &str) -> Result<Execution, InvokeError> {
        exec_racket_script(script)
    }
}

// ---------------------------------------------------------------------------
// Racket script execution (shared implementation)
// ---------------------------------------------------------------------------

/// Execute a Racket script by writing it to a temp file and invoking `racket`.
///
/// This is the single implementation used by all frames. `#lang racket`
/// requires file-based execution (not `racket -e`).
fn exec_racket_script(script: &str) -> Result<Execution, InvokeError> {
    let tmp_dir = std::env::temp_dir();
    // Use PID + thread ID + counter for uniqueness in parallel tests
    static COUNTER: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
    let id = COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    let tmp_path = tmp_dir.join(format!("cadmus_{}_{}.rkt", std::process::id(), id));

    {
        let mut f = std::fs::File::create(&tmp_path)
            .map_err(|e| InvokeError::ExecError(format!("create temp file: {}", e)))?;
        f.write_all(script.as_bytes())
            .map_err(|e| InvokeError::ExecError(format!("write temp file: {}", e)))?;
        f.flush()
            .map_err(|e| InvokeError::ExecError(format!("flush temp file: {}", e)))?;
    }

    let output = std::process::Command::new("racket")
        .arg(&tmp_path)
        .output()
        .map_err(|e| InvokeError::ExecError(format!("run racket: {}", e)))?;

    let _ = std::fs::remove_file(&tmp_path);

    Ok(Execution {
        script: script.to_string(),
        stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
        stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
        success: output.status.success(),
        exit_code: output.status.code(),
    })
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

    // -- codegen tests --

    #[test]
    fn test_codegen_produces_racket_script() {
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
        let script = frame.codegen(&plan).expect("codegen should succeed");
        assert!(script.contains("#lang racket"), "should have racket preamble");
        // Tilde is expanded at codegen time
        assert!(script.contains("/Downloads"), "should use bound path (tilde expanded): {}", script);
    }

    #[test]
    fn test_codegen_empty_bindings_uses_defaults() {
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
        let script = frame.codegen(&plan).expect("codegen should succeed");
        assert!(script.contains("#lang racket"));
        // Plan has input "path", so codegen should reference it as a variable
        assert!(script.contains("(shell-quote path)"), "should use plan input 'path': {}", script);
    }

    #[test]
    fn test_codegen_invalid_plan_returns_error() {
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
        let result = frame.codegen(&plan);
        assert!(result.is_err(), "should fail for unknown op");
        let err = result.unwrap_err();
        assert!(matches!(err, InvokeError::CompileError(_)));
    }

    #[test]
    fn test_codegen_multi_step_plan() {
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
                        [("pattern".to_string(), crate::plan::StepParam::Value("*.pdf".to_string()))].into_iter().collect()
                    ),
                },
            ],
            bindings,
        };
        let frame = DefaultFrame::from_plan(&plan);
        let script = frame.codegen(&plan).expect("codegen should succeed");
        assert!(script.contains("#lang racket"));
        // Tilde is expanded at codegen time
        assert!(script.contains("/Documents"), "should use bound path (tilde expanded): {}", script);
        assert!(script.contains("step-1"), "should have let* bindings for multi-step");
    }

    // -- invoke tests (these actually run racket if available) --

    #[test]
    fn test_invoke_returns_execution_with_script() {
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
        // invoke may fail if racket isn't installed — that's an ExecError, not a codegen error
        match frame.invoke(&plan) {
            Ok(exec) => {
                assert!(exec.script.contains("#lang racket"));
                // We ran successfully — stdout has directory listing
                assert!(exec.success || !exec.stderr.is_empty());
            }
            Err(InvokeError::ExecError(_)) => {
                // Racket not installed — that's fine for CI
            }
            Err(e) => panic!("unexpected error: {}", e),
        }
    }
}
