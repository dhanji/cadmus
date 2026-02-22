// ---------------------------------------------------------------------------
// calling_frame — resolve input bindings for plan execution
// ---------------------------------------------------------------------------
//
// A CallingFrame determines how plan inputs are resolved at execution time.
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

// ---------------------------------------------------------------------------
// Trait
// ---------------------------------------------------------------------------

/// A calling frame resolves plan input names to concrete values.
pub trait CallingFrame {
    /// Resolve an input by name. Returns the bound value, or a default
    /// if the input is not bound.
    fn resolve_input(&self, name: &str) -> String;

    /// Check if an input has an explicit binding.
    fn has_binding(&self, name: &str) -> bool;

    /// Get all bindings as a map.
    fn bindings(&self) -> &HashMap<String, String>;
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
    pub fn from_plan(plan: &crate::plan::PlanDef) -> Self {
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
        let plan = crate::plan::PlanDef {
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
}
