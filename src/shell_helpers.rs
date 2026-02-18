// ---------------------------------------------------------------------------
// Shell Helpers — shared utilities for shell and Racket executors
// ---------------------------------------------------------------------------

use std::fmt;

// ---------------------------------------------------------------------------
// Shared error type for both shell and Racket executors
// ---------------------------------------------------------------------------

/// Error type shared by both the shell executor and the Racket executor.
#[derive(Debug, Clone)]
pub enum CodegenError {
    /// Op is not recognized / has no mapping
    UnknownOp(String),
    /// A required parameter is missing
    MissingParam { op: String, param: String },
    /// Script execution failed (shell executor only)
    ExecFailed { exit_code: Option<i32>, stderr: String },
}

impl fmt::Display for CodegenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CodegenError::UnknownOp(op) =>
                write!(f, "unknown op '{}': no command mapping", op),
            CodegenError::MissingParam { op, param } =>
                write!(f, "op '{}' requires param '{}' but it was not provided", op, param),
            CodegenError::ExecFailed { exit_code, stderr } =>
                write!(f, "script failed (exit {:?}): {}", exit_code, stderr),
        }
    }
}

impl std::error::Error for CodegenError {}

/// Quote a string for safe use in a shell command.
/// Uses single quotes, escaping any embedded single quotes.
/// If the string contains `$WORK_DIR`, wraps in double quotes
/// to allow variable expansion.
pub fn shell_quote(s: &str) -> String {
    if s.is_empty() {
        return "''".to_string();
    }
    // Shell variable references need double quotes for expansion.
    // SECURITY: only allow safe chars after $WORK_DIR to prevent injection
    // via $(cmd) or `cmd` embedded after the variable reference.
    if s.starts_with("$WORK_DIR") && s[9..].chars().all(|c| c.is_ascii_alphanumeric()
        || c == '/' || c == '.' || c == '-' || c == '_')
    {
        return format!("\"{}\"", s);
    }
    // If the string is "safe" (only ASCII safe chars), return as-is.
    // Use is_ascii_alphanumeric() to avoid treating unicode as safe.
    if s.chars().all(|c| c.is_ascii_alphanumeric() || c == '/' || c == '.' || c == '-' || c == '_' || c == '~') {
        return s.to_string();
    }
    // Otherwise, wrap in single quotes, escaping embedded single quotes
    format!("'{}'", s.replace('\'', "'\\''"))
}

/// Convert a glob-like pattern to a grep-compatible regex.
/// e.g., "*.pdf" → "\\.pdf$", "*.tmp" → "\\.tmp$"
pub fn glob_to_grep(pattern: &str) -> String {
    if pattern.starts_with("*.") {
        // *.ext → match lines ending with .ext
        let ext = &pattern[1..]; // ".ext"
        format!("{}$", ext.replace('.', "\\."))
    } else if pattern.starts_with('.') {
        // .ext → match lines ending with .ext
        format!("{}$", pattern.replace('.', "\\."))
    } else {
        // Use as-is (it's already a regex or literal)
        pattern.to_string()
    }
}

/// Escape special characters for use inside a sed s/.../ expression.
pub fn sed_escape(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '/' | '\\' | '&' | '.' | '*' | '[' | ']' | '^' | '$' => {
                out.push('\\');
                out.push(c);
            }
            _ => out.push(c),
        }
    }
    out
}

/// Extract the primary input path from a WorkflowDef.
/// Looks for common input keys: path, archive, logfile, repo, source, query, url.
pub fn primary_input(inputs: &std::collections::HashMap<String, String>) -> String {
    for key in &["path", "archive", "logfile", "repo", "source", "query", "url"] {
        if let Some(v) = inputs.get(*key) {
            return v.clone();
        }
    }
    inputs.values().next()
        .cloned()
        .unwrap_or_else(|| ".".to_string())
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // --- Shell quoting ---

    #[test]
    fn test_shell_quote_simple() {
        assert_eq!(shell_quote("hello"), "hello");
        assert_eq!(shell_quote("/usr/bin/ls"), "/usr/bin/ls");
        assert_eq!(shell_quote("~/Downloads"), "~/Downloads");
    }

    #[test]
    fn test_shell_quote_spaces() {
        assert_eq!(shell_quote("my file.txt"), "'my file.txt'");
        assert_eq!(shell_quote("/path/to/my dir"), "'/path/to/my dir'");
    }

    #[test]
    fn test_shell_quote_special_chars() {
        assert_eq!(shell_quote("hello world; rm -rf /"), "'hello world; rm -rf /'");
        assert_eq!(shell_quote("$(whoami)"), "'$(whoami)'");
    }

    #[test]
    fn test_shell_quote_single_quotes() {
        assert_eq!(shell_quote("it's"), "'it'\\''s'");
    }

    #[test]
    fn test_shell_quote_empty() {
        assert_eq!(shell_quote(""), "''");
    }

    #[test]
    fn test_shell_quote_work_dir() {
        assert_eq!(shell_quote("$WORK_DIR/step_1.txt"), "\"$WORK_DIR/step_1.txt\"");
    }

    #[test]
    fn test_shell_quote_work_dir_injection() {
        // $WORK_DIR followed by unsafe chars should NOT get double-quoted
        let result = shell_quote("$WORK_DIR$(whoami)");
        assert!(result.starts_with('\''), "injection attempt should be single-quoted: {}", result);
    }

    // --- Glob to grep ---

    #[test]
    fn test_glob_to_grep_star_ext() {
        assert_eq!(glob_to_grep("*.pdf"), "\\.pdf$");
        assert_eq!(glob_to_grep("*.tmp"), "\\.tmp$");
    }

    #[test]
    fn test_glob_to_grep_dot_ext() {
        assert_eq!(glob_to_grep(".log"), "\\.log$");
    }

    #[test]
    fn test_glob_to_grep_literal() {
        assert_eq!(glob_to_grep("error"), "error");
    }

    // --- Sed escape ---

    #[test]
    fn test_sed_escape_basic() {
        assert_eq!(sed_escape("hello"), "hello");
        assert_eq!(sed_escape("a.b"), "a\\.b");
        assert_eq!(sed_escape("a/b"), "a\\/b");
    }

    #[test]
    fn test_sed_escape_special() {
        assert_eq!(sed_escape("foo*bar"), "foo\\*bar");
        assert_eq!(sed_escape("[test]"), "\\[test\\]");
    }

    // --- Primary input ---

    #[test]
    fn test_primary_input_path() {
        let mut inputs = std::collections::HashMap::new();
        inputs.insert("path".to_string(), "/tmp/test".to_string());
        inputs.insert("other".to_string(), "ignored".to_string());
        assert_eq!(primary_input(&inputs), "/tmp/test");
    }

    #[test]
    fn test_primary_input_fallback() {
        let inputs = std::collections::HashMap::new();
        assert_eq!(primary_input(&inputs), ".");
    }
}
