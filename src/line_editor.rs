// ---------------------------------------------------------------------------
// line_editor — rustyline wrapper for cadmus CLI
// ---------------------------------------------------------------------------
//
// Provides readline-style line editing for the interactive REPL:
//   - Up/Down arrow: history cycling
//   - Ctrl-A / Ctrl-E: beginning / end of line
//   - Ctrl-K: kill to end of line
//   - Ctrl-U: kill to beginning of line
//   - Ctrl-W: delete word backward
//   - Left/Right arrow: cursor movement
//   - Ctrl-D: EOF
//   - Ctrl-C: interrupt (re-prompt)
//
// History is persisted to ~/.cadmus_history (max 1000 entries).

use rustyline::error::ReadlineError;
use rustyline::{Config, DefaultEditor, EditMode};
use std::path::PathBuf;

/// Maximum number of history entries to retain.
const MAX_HISTORY: usize = 1000;

/// History file name (stored in user's home directory).
const HISTORY_FILE: &str = ".cadmus_history";

/// Result of a single line read.
pub enum ReadResult {
    /// User entered a line of text.
    Line(String),
    /// User pressed Ctrl-C (interrupt).
    Interrupted,
    /// User pressed Ctrl-D or stdin closed (EOF).
    Eof,
}

/// Wrapper around rustyline's DefaultEditor with cadmus-specific config.
pub struct LineEditor {
    editor: DefaultEditor,
    history_path: Option<PathBuf>,
}

impl LineEditor {
    /// Create a new line editor with Emacs-mode keybindings.
    ///
    /// Loads history from `~/.cadmus_history` if it exists.
    /// If the history file is missing, corrupt, or unreadable, starts with
    /// empty history (no crash).
    pub fn new() -> Self {
        let config = Config::builder()
            .edit_mode(EditMode::Emacs)
            .max_history_size(MAX_HISTORY)
            .expect("valid history size")
            .auto_add_history(false) // we control when to add
            .build();

        let mut editor = DefaultEditor::with_config(config)
            .expect("failed to create line editor");

        // Resolve history path: ~/.cadmus_history
        let history_path = dirs_home().map(|home| home.join(HISTORY_FILE));

        // Load existing history (ignore errors — corrupt/missing is fine)
        if let Some(ref path) = history_path {
            let _ = editor.load_history(path);
        }

        LineEditor {
            editor,
            history_path,
        }
    }

    /// Read a line with the given prompt string.
    ///
    /// The prompt is displayed as-is (include ANSI codes for color).
    /// The line is NOT automatically added to history — call `add_history`
    /// explicitly for lines you want remembered.
    pub fn read_line(&mut self, prompt: &str) -> ReadResult {
        match self.editor.readline(prompt) {
            Ok(line) => ReadResult::Line(line),
            Err(ReadlineError::Interrupted) => ReadResult::Interrupted,
            Err(ReadlineError::Eof) => ReadResult::Eof,
            Err(_) => ReadResult::Eof, // treat other errors as EOF
        }
    }

    /// Add a line to the in-memory history and persist to disk.
    ///
    /// Silently ignores errors (e.g. unwritable history file).
    pub fn add_history(&mut self, line: &str) {
        let _ = self.editor.add_history_entry(line);
        self.save_history();
    }

    /// Persist history to disk. Called automatically by `add_history`.
    pub fn save_history(&mut self) {
        if let Some(ref path) = self.history_path {
            let _ = self.editor.save_history(path);
        }
    }
}

/// Get the user's home directory.
fn dirs_home() -> Option<PathBuf> {
    // Use $HOME on Unix (covers macOS + Linux)
    std::env::var_os("HOME").map(PathBuf::from)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dirs_home_returns_something() {
        // On any dev machine, $HOME should be set
        let home = dirs_home();
        assert!(home.is_some(), "HOME should be set in test environment");
    }

    #[test]
    fn test_history_path_is_in_home() {
        if let Some(home) = dirs_home() {
            let expected = home.join(HISTORY_FILE);
            assert!(expected.to_str().unwrap().contains(".cadmus_history"));
        }
    }

    #[test]
    fn test_editor_creates_without_panic() {
        // This verifies the config is valid and editor initializes
        let _editor = LineEditor::new();
    }

    #[test]
    fn test_add_history_does_not_panic() {
        let mut editor = LineEditor::new();
        editor.add_history("test command");
        editor.add_history("another command");
    }

    #[test]
    fn test_max_history_constant() {
        assert_eq!(MAX_HISTORY, 1000);
    }

    #[test]
    fn test_corrupt_history_file_does_not_crash() {
        // Write garbage to a temp file, point editor at it, verify no crash
        let tmp = std::env::temp_dir().join("cadmus_test_corrupt_history");
        std::fs::write(&tmp, b"\xff\xfe\x00\x01binary garbage\n\x80\x90").unwrap();

        let config = Config::builder()
            .edit_mode(EditMode::Emacs)
            .max_history_size(MAX_HISTORY)
            .expect("valid")
            .build();
        let mut ed = DefaultEditor::with_config(config).unwrap();
        // load_history on corrupt file should not panic
        let _ = ed.load_history(&tmp);
        let _ = std::fs::remove_file(&tmp);
    }
}
