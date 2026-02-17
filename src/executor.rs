// ---------------------------------------------------------------------------
// Shell Executor
// ---------------------------------------------------------------------------
//
// Converts a CompiledWorkflow into a runnable shell script.
//
// Design:
//   1. Each op maps to a concrete shell command via `op_to_command()`
//   2. `generate_script()` produces a #!/bin/sh script with intermediate
//      temp files between steps for debuggability
//   3. `run_script()` executes the script via /bin/sh and captures output
//
// Safety model:
//   - Default: generate and print the script (dry-run)
//   - --execute flag or second confirmation: actually run it

use std::fmt;

use crate::type_expr::TypeExpr;
use crate::workflow::{CompiledWorkflow, CompiledStep, WorkflowDef};

// ---------------------------------------------------------------------------
// Error type
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub enum ExecutorError {
    /// Op is not recognized / has no shell mapping
    UnknownOp(String),
    /// A required parameter is missing
    MissingParam { op: String, param: String },
    /// Script execution failed
    ExecFailed { exit_code: Option<i32>, stderr: String },
}

impl fmt::Display for ExecutorError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExecutorError::UnknownOp(op) =>
                write!(f, "unknown op '{}': no shell command mapping", op),
            ExecutorError::MissingParam { op, param } =>
                write!(f, "op '{}' requires param '{}' but it was not provided", op, param),
            ExecutorError::ExecFailed { exit_code, stderr } =>
                write!(f, "script failed (exit {:?}): {}", exit_code, stderr),
        }
    }
}

impl std::error::Error for ExecutorError {}

// ---------------------------------------------------------------------------
// ShellCommand — one step's shell command
// ---------------------------------------------------------------------------

/// A single shell command generated from a compiled step.
#[derive(Debug, Clone)]
pub struct ShellCommand {
    /// The full command string (e.g., "find ~/Documents -type f")
    pub command: String,
    /// Whether this command reads from stdin (i.e., reads from prev step's file)
    pub reads_stdin: bool,
    /// Whether this command writes to stdout (i.e., output should be captured)
    pub writes_stdout: bool,
}

// ---------------------------------------------------------------------------
// Archive format detection from TypeExpr
// ---------------------------------------------------------------------------

/// Extract the archive format name from a resolved TypeExpr.
///
/// e.g. `File(Archive(File(Image), Cbz))` → Some("Cbz")
///      `File(Archive(a, Zip))` → Some("Zip")
///      `File(Text)` → None
pub fn extract_archive_format(ty: &TypeExpr) -> Option<&str> {
    match ty {
        TypeExpr::Constructor(name, args) if name == "File" && args.len() == 1 => {
            extract_archive_format_inner(&args[0])
        }
        TypeExpr::Constructor(name, args) if name == "Archive" && args.len() == 2 => {
            format_name(&args[1])
        }
        _ => None,
    }
}

fn extract_archive_format_inner(ty: &TypeExpr) -> Option<&str> {
    match ty {
        TypeExpr::Constructor(name, args) if name == "Archive" && args.len() == 2 => {
            format_name(&args[1])
        }
        _ => None,
    }
}

fn format_name(ty: &TypeExpr) -> Option<&str> {
    match ty {
        TypeExpr::Primitive(name) => Some(name.as_str()),
        TypeExpr::Constructor(name, _) => Some(name.as_str()),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Shell quoting
// ---------------------------------------------------------------------------

/// Quote a string for safe use in a shell command.
/// Uses single quotes, escaping any embedded single quotes.
/// If the string contains `$` (shell variable), wraps in double quotes
/// to allow variable expansion.
pub fn shell_quote(s: &str) -> String {
    if s.is_empty() {
        return "''".to_string();
    }
    // Shell variable references need double quotes for expansion
    if s.starts_with("$WORK_DIR") {
        return format!("\"{}\"", s);
    }
    // If the string is "safe" (no special chars), return as-is
    if s.chars().all(|c| c.is_alphanumeric() || c == '/' || c == '.' || c == '-' || c == '_' || c == '~') {
        return s.to_string();
    }
    // Otherwise, wrap in single quotes, escaping embedded single quotes
    format!("'{}'", s.replace('\'', "'\\''"))
}

// ---------------------------------------------------------------------------
// Op → Shell Command mapping
// ---------------------------------------------------------------------------

/// Convert a compiled step into a shell command.
///
/// Arguments:
///   - `step`: the compiled step (op name, params, types)
///   - `input_path`: the path/value to use as input (first step gets the
///     workflow input, subsequent steps get the previous step's temp file)
///   - `prev_file`: if Some, the previous step's output temp file path
///     (used as stdin source for commands that read from previous output)
pub fn op_to_command(
    step: &CompiledStep,
    input_path: &str,
    prev_file: Option<&str>,
) -> Result<ShellCommand, ExecutorError> {
    let op = step.op.as_str();
    let params = &step.params;

    match op {
        // --- Directory & File I/O ---
        "list_dir" => Ok(ShellCommand {
            command: format!("ls {}", shell_quote(input_path)),
            reads_stdin: false,
            writes_stdout: true,
        }),

        "read_file" => {
            if let Some(pf) = prev_file {
                // In a pipeline: cat each file listed in prev output
                Ok(ShellCommand {
                    command: format!("cat {}", shell_quote(pf)),
                    reads_stdin: false,
                    writes_stdout: true,
                })
            } else {
                Ok(ShellCommand {
                    command: format!("cat {}", shell_quote(input_path)),
                    reads_stdin: false,
                    writes_stdout: true,
                })
            }
        }

        "write_file" => {
            let dest = params.get("path")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "path".to_string()
                })?;
            if let Some(pf) = prev_file {
                Ok(ShellCommand {
                    command: format!("cp {} {}", shell_quote(pf), shell_quote(dest)),
                    reads_stdin: false,
                    writes_stdout: false,
                })
            } else {
                Ok(ShellCommand {
                    command: format!("cp {} {}", shell_quote(input_path), shell_quote(dest)),
                    reads_stdin: false,
                    writes_stdout: false,
                })
            }
        }

        "stat" => Ok(ShellCommand {
            command: format!("stat {}", shell_quote(input_path)),
            reads_stdin: false,
            writes_stdout: true,
        }),

        "walk_tree" => Ok(ShellCommand {
            command: format!("find {} -type f", shell_quote(input_path)),
            reads_stdin: false,
            writes_stdout: true,
        }),

        "walk_tree_hierarchy" => Ok(ShellCommand {
            command: format!("find {}", shell_quote(input_path)),
            reads_stdin: false,
            writes_stdout: true,
        }),

        "flatten_tree" => {
            // Already flat in shell (find output is flat lines)
            if let Some(pf) = prev_file {
                Ok(ShellCommand {
                    command: format!("cat {}", shell_quote(pf)),
                    reads_stdin: false,
                    writes_stdout: true,
                })
            } else {
                Ok(ShellCommand {
                    command: "cat".to_string(),
                    reads_stdin: true,
                    writes_stdout: true,
                })
            }
        }

        // --- Filtering & Sorting ---
        "filter" => {
            let pattern = params.get("pattern")
                .or_else(|| params.get("extension"))
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "pattern".to_string()
                })?;
            // Convert glob-like pattern to grep regex
            let grep_pattern = glob_to_grep(pattern);
            if let Some(pf) = prev_file {
                Ok(ShellCommand {
                    command: format!("grep {} {}", shell_quote(&grep_pattern), shell_quote(pf)),
                    reads_stdin: false,
                    writes_stdout: true,
                })
            } else {
                Ok(ShellCommand {
                    command: format!("grep {}", shell_quote(&grep_pattern)),
                    reads_stdin: true,
                    writes_stdout: true,
                })
            }
        }

        "sort_by" => {
            let flag = match params.get("field").map(|s| s.as_str())
                .or_else(|| params.get("mode").map(|s| s.as_str()))
            {
                Some("size") => " -n",
                Some("date") | Some("time") | Some("mtime") => " -t",
                Some("name") | Some(_) | None => "",
            };
            if let Some(pf) = prev_file {
                Ok(ShellCommand {
                    command: format!("sort{} {}", flag, shell_quote(pf)),
                    reads_stdin: false,
                    writes_stdout: true,
                })
            } else {
                Ok(ShellCommand {
                    command: format!("sort{}", flag),
                    reads_stdin: true,
                    writes_stdout: true,
                })
            }
        }

        // --- Archive Operations ---
        "extract_archive" => {
            let fmt = extract_archive_format(&step.input_type)
                .unwrap_or("Zip"); // default to zip
            let src = if let Some(pf) = prev_file { pf } else { input_path };
            let cmd = match fmt {
                "Zip" | "Cbz" | "Cbr" | "Jar" | "Epub" =>
                    format!("unzip -o {} -d \"$WORK_DIR/extracted\"", shell_quote(src)),
                "TarGz" | "Tgz" =>
                    format!("tar xzf {} -C \"$WORK_DIR/extracted\"", shell_quote(src)),
                "Tar" =>
                    format!("tar xf {} -C \"$WORK_DIR/extracted\"", shell_quote(src)),
                "TarBz2" =>
                    format!("tar xjf {} -C \"$WORK_DIR/extracted\"", shell_quote(src)),
                "TarXz" =>
                    format!("tar xJf {} -C \"$WORK_DIR/extracted\"", shell_quote(src)),
                "Gz" | "Gzip" =>
                    format!("gunzip -k -c {} > \"$WORK_DIR/extracted/output\"", shell_quote(src)),
                "Bz2" =>
                    format!("bunzip2 -k -c {} > \"$WORK_DIR/extracted/output\"", shell_quote(src)),
                "Xz" =>
                    format!("xz -dk -c {} > \"$WORK_DIR/extracted/output\"", shell_quote(src)),
                "Rar" =>
                    format!("unrar x {} \"$WORK_DIR/extracted/\"", shell_quote(src)),
                "SevenZ" | "7z" =>
                    format!("7z x {} -o\"$WORK_DIR/extracted\"", shell_quote(src)),
                _ =>
                    format!("unzip -o {} -d \"$WORK_DIR/extracted\"", shell_quote(src)),
            };
            Ok(ShellCommand {
                // mkdir + extract + list
                command: format!(
                    "mkdir -p \"$WORK_DIR/extracted\" && {} && find \"$WORK_DIR/extracted\" -type f",
                    cmd
                ),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        "pack_archive" => {
            let fmt = extract_archive_format(&step.output_type)
                .unwrap_or("Zip");
            let output_name = params.get("output")
                .map(|s| s.as_str())
                .unwrap_or("archive");
            let src = if let Some(pf) = prev_file { pf } else { input_path };
            let cmd = match fmt {
                "Zip" | "Cbz" =>
                    format!("cat {} | while IFS= read -r f; do zip -g {} \"$f\"; done",
                        shell_quote(src), shell_quote(output_name)),
                "TarGz" | "Tgz" =>
                    format!("tar czf {} -T {}", shell_quote(output_name), shell_quote(src)),
                "Tar" =>
                    format!("tar cf {} -T {}", shell_quote(output_name), shell_quote(src)),
                "TarBz2" =>
                    format!("tar cjf {} -T {}", shell_quote(output_name), shell_quote(src)),
                "TarXz" =>
                    format!("tar cJf {} -T {}", shell_quote(output_name), shell_quote(src)),
                _ =>
                    format!("tar czf {} -T {}", shell_quote(output_name), shell_quote(src)),
            };
            Ok(ShellCommand {
                command: cmd,
                reads_stdin: false,
                writes_stdout: false,
            })
        }

        // --- Sequence Operations ---
        "concat_seq" => {
            if let Some(pf) = prev_file {
                Ok(ShellCommand {
                    command: format!("cat {}", shell_quote(pf)),
                    reads_stdin: false,
                    writes_stdout: true,
                })
            } else {
                Ok(ShellCommand {
                    command: "cat".to_string(),
                    reads_stdin: true,
                    writes_stdout: true,
                })
            }
        }

        // --- Entry Operations ---
        "rename" => {
            let new_name = params.get("name")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "name".to_string()
                })?;
            Ok(ShellCommand {
                command: format!("mv {} {}", shell_quote(input_path), shell_quote(new_name)),
                reads_stdin: false,
                writes_stdout: false,
            })
        }

        "move_entry" => {
            let dest = params.get("path").or_else(|| params.get("dest"))
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "path".to_string()
                })?;
            Ok(ShellCommand {
                command: format!("mv {} {}", shell_quote(input_path), shell_quote(dest)),
                reads_stdin: false,
                writes_stdout: false,
            })
        }

        // --- Search ---
        "search_content" => {
            let pattern = params.get("pattern")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "pattern".to_string()
                })?;
            let case_flag = match params.get("mode").map(|s| s.as_str()) {
                Some("case-insensitive") | Some("ci") => "-i ",
                _ => "",
            };
            if let Some(pf) = prev_file {
                Ok(ShellCommand {
                    command: format!("grep -rn {}{} {}", case_flag, shell_quote(pattern), shell_quote(pf)),
                    reads_stdin: false,
                    writes_stdout: true,
                })
            } else {
                Ok(ShellCommand {
                    command: format!("grep -rn {}{} {}", case_flag, shell_quote(pattern), shell_quote(input_path)),
                    reads_stdin: false,
                    writes_stdout: true,
                })
            }
        }

        "find_matching" => {
            let pattern = params.get("pattern")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "pattern".to_string()
                })?;
            let grep_pattern = glob_to_grep(pattern);
            if let Some(pf) = prev_file {
                Ok(ShellCommand {
                    command: format!("grep {} {}", shell_quote(&grep_pattern), shell_quote(pf)),
                    reads_stdin: false,
                    writes_stdout: true,
                })
            } else {
                Ok(ShellCommand {
                    command: format!("find {} -name {}", shell_quote(input_path), shell_quote(pattern)),
                    reads_stdin: false,
                    writes_stdout: true,
                })
            }
        }

        "map_entries" => {
            // Generic map — in shell, this is a pass-through or xargs
            if let Some(pf) = prev_file {
                Ok(ShellCommand {
                    command: format!("cat {}", shell_quote(pf)),
                    reads_stdin: false,
                    writes_stdout: true,
                })
            } else {
                Ok(ShellCommand {
                    command: "cat".to_string(),
                    reads_stdin: true,
                    writes_stdout: true,
                })
            }
        }

        // --- File Lifecycle ---
        "copy" => {
            let dest = params.get("path").or_else(|| params.get("dest"))
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "path".to_string()
                })?;
            Ok(ShellCommand {
                command: format!("cp {} {}", shell_quote(input_path), shell_quote(dest)),
                reads_stdin: false,
                writes_stdout: false,
            })
        }

        "delete" => {
            if let Some(pf) = prev_file {
                // Delete files listed in prev output
                Ok(ShellCommand {
                    command: format!("xargs rm -f < {}", shell_quote(pf)),
                    reads_stdin: false,
                    writes_stdout: false,
                })
            } else {
                Ok(ShellCommand {
                    command: format!("rm -rf {}", shell_quote(input_path)),
                    reads_stdin: false,
                    writes_stdout: false,
                })
            }
        }

        "create_dir" => Ok(ShellCommand {
            command: format!("mkdir -p {}", shell_quote(input_path)),
            reads_stdin: false,
            writes_stdout: false,
        }),

        "create_link" => {
            let link_name = params.get("link_name").or_else(|| params.get("name"))
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "link_name".to_string()
                })?;
            Ok(ShellCommand {
                command: format!("ln -s {} {}", shell_quote(input_path), shell_quote(link_name)),
                reads_stdin: false,
                writes_stdout: false,
            })
        }

        "set_permissions" => {
            let mode = params.get("mode").or_else(|| params.get("permissions"))
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "mode".to_string()
                })?;
            Ok(ShellCommand {
                command: format!("chmod {} {}", shell_quote(mode), shell_quote(input_path)),
                reads_stdin: false,
                writes_stdout: false,
            })
        }

        "set_owner" => {
            let owner = params.get("owner")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "owner".to_string()
                })?;
            Ok(ShellCommand {
                command: format!("chown {} {}", shell_quote(owner), shell_quote(input_path)),
                reads_stdin: false,
                writes_stdout: false,
            })
        }

        // --- Content Transformation ---
        "replace" => {
            let pattern = params.get("pattern")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "pattern".to_string()
                })?;
            let replacement = params.get("replacement").or_else(|| params.get("text"))
                .unwrap_or(&String::new())
                .clone();
            let src = if let Some(pf) = prev_file { pf } else { input_path };
            Ok(ShellCommand {
                command: format!("sed 's/{}/{}/g' {}", sed_escape(pattern), sed_escape(&replacement), shell_quote(src)),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        "head" => {
            let n = params.get("count").or_else(|| params.get("n"))
                .unwrap_or(&"10".to_string())
                .clone();
            let src = if let Some(pf) = prev_file { pf } else { input_path };
            Ok(ShellCommand {
                command: format!("head -n {} {}", n, shell_quote(src)),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        "tail" => {
            let n = params.get("count").or_else(|| params.get("n"))
                .unwrap_or(&"10".to_string())
                .clone();
            let src = if let Some(pf) = prev_file { pf } else { input_path };
            Ok(ShellCommand {
                command: format!("tail -n {} {}", n, shell_quote(src)),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        "unique" => {
            if let Some(pf) = prev_file {
                Ok(ShellCommand {
                    command: format!("sort -u {}", shell_quote(pf)),
                    reads_stdin: false,
                    writes_stdout: true,
                })
            } else {
                Ok(ShellCommand {
                    command: "sort -u".to_string(),
                    reads_stdin: true,
                    writes_stdout: true,
                })
            }
        }

        "count" => {
            if let Some(pf) = prev_file {
                Ok(ShellCommand {
                    command: format!("wc -l < {}", shell_quote(pf)),
                    reads_stdin: false,
                    writes_stdout: true,
                })
            } else {
                Ok(ShellCommand {
                    command: "wc -l".to_string(),
                    reads_stdin: true,
                    writes_stdout: true,
                })
            }
        }

        "diff" => {
            let file2 = params.get("file2").or_else(|| params.get("other"))
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "file2".to_string()
                })?;
            Ok(ShellCommand {
                command: format!("diff {} {}", shell_quote(input_path), shell_quote(file2)),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        "checksum" => {
            let src = if let Some(pf) = prev_file { pf } else { input_path };
            Ok(ShellCommand {
                command: format!("shasum -a 256 {}", shell_quote(src)),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        // --- Metadata Accessors ---
        "get_size" => Ok(ShellCommand {
            command: format!("stat -f %z {}", shell_quote(input_path)),
            reads_stdin: false,
            writes_stdout: true,
        }),

        "get_mtime" => Ok(ShellCommand {
            command: format!("stat -f %m {}", shell_quote(input_path)),
            reads_stdin: false,
            writes_stdout: true,
        }),

        "get_permissions" => Ok(ShellCommand {
            command: format!("stat -f %p {}", shell_quote(input_path)),
            reads_stdin: false,
            writes_stdout: true,
        }),

        "get_file_type" => Ok(ShellCommand {
            command: format!("stat -f %T {}", shell_quote(input_path)),
            reads_stdin: false,
            writes_stdout: true,
        }),

        // --- macOS-Specific ---
        "spotlight_search" => {
            let query = params.get("query")
                .map(|s| s.as_str())
                .unwrap_or(input_path);
            Ok(ShellCommand {
                command: format!("mdfind {}", shell_quote(query)),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        "get_xattr" => {
            let key = params.get("key")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "key".to_string()
                })?;
            Ok(ShellCommand {
                command: format!("xattr -p {} {}", shell_quote(key), shell_quote(input_path)),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        "set_xattr" => {
            let key = params.get("key")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "key".to_string()
                })?;
            let value = params.get("value")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "value".to_string()
                })?;
            Ok(ShellCommand {
                command: format!("xattr -w {} {} {}", shell_quote(key), shell_quote(value), shell_quote(input_path)),
                reads_stdin: false,
                writes_stdout: false,
            })
        }

        "remove_xattr" => {
            let key = params.get("key")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "key".to_string()
                })?;
            Ok(ShellCommand {
                command: format!("xattr -d {} {}", shell_quote(key), shell_quote(input_path)),
                reads_stdin: false,
                writes_stdout: false,
            })
        }

        "remove_quarantine" => Ok(ShellCommand {
            command: format!("xattr -d com.apple.quarantine {}", shell_quote(input_path)),
            reads_stdin: false,
            writes_stdout: false,
        }),

        "open_file" => Ok(ShellCommand {
            command: format!("open {}", shell_quote(input_path)),
            reads_stdin: false,
            writes_stdout: false,
        }),

        "open_with" => {
            let app = params.get("app")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "app".to_string()
                })?;
            Ok(ShellCommand {
                command: format!("open -a {} {}", shell_quote(app), shell_quote(input_path)),
                reads_stdin: false,
                writes_stdout: false,
            })
        }

        "reveal" => Ok(ShellCommand {
            command: format!("open -R {}", shell_quote(input_path)),
            reads_stdin: false,
            writes_stdout: false,
        }),

        "clipboard_copy" => {
            if let Some(pf) = prev_file {
                Ok(ShellCommand {
                    command: format!("pbcopy < {}", shell_quote(pf)),
                    reads_stdin: false,
                    writes_stdout: false,
                })
            } else {
                Ok(ShellCommand {
                    command: "pbcopy".to_string(),
                    reads_stdin: true,
                    writes_stdout: false,
                })
            }
        }

        "clipboard_paste" => Ok(ShellCommand {
            command: "pbpaste".to_string(),
            reads_stdin: false,
            writes_stdout: true,
        }),

        "read_plist" => {
            let src = if let Some(pf) = prev_file { pf } else { input_path };
            Ok(ShellCommand {
                command: format!("plutil -p {}", shell_quote(src)),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        "write_plist" => {
            let dest = params.get("path")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "path".to_string()
                })?;
            if let Some(pf) = prev_file {
                Ok(ShellCommand {
                    command: format!("plutil -convert xml1 -o {} {}", shell_quote(dest), shell_quote(pf)),
                    reads_stdin: false,
                    writes_stdout: false,
                })
            } else {
                Ok(ShellCommand {
                    command: format!("plutil -convert xml1 -o {} {}", shell_quote(dest), shell_quote(input_path)),
                    reads_stdin: false,
                    writes_stdout: false,
                })
            }
        }

        // --- Network & Download ---
        "download" => Ok(ShellCommand {
            command: format!("curl -fsSL -O {}", shell_quote(input_path)),
            reads_stdin: false,
            writes_stdout: false,
        }),

        "upload" => {
            let url = params.get("url")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "url".to_string()
                })?;
            Ok(ShellCommand {
                command: format!("curl -T {} {}", shell_quote(input_path), shell_quote(url)),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        "sync" => {
            let dest = params.get("path").or_else(|| params.get("dest"))
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "path".to_string()
                })?;
            Ok(ShellCommand {
                command: format!("rsync -a {} {}", shell_quote(input_path), shell_quote(dest)),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        // =====================================================================
        // Power Tools Operations
        // =====================================================================

        // --- Git ---
        "git_init" => Ok(ShellCommand {
            command: format!("git init {}", shell_quote(input_path)),
            reads_stdin: false,
            writes_stdout: true,
        }),

        "git_clone" => Ok(ShellCommand {
            command: format!("git clone {}", shell_quote(input_path)),
            reads_stdin: false,
            writes_stdout: true,
        }),

        "git_add" => {
            let files = params.get("files").unwrap_or(&".".to_string()).clone();
            Ok(ShellCommand {
                command: format!("cd {} && git add {}", shell_quote(input_path), shell_quote(&files)),
                reads_stdin: false,
                writes_stdout: false,
            })
        }

        "git_commit" => {
            let msg = params.get("message").or_else(|| params.get("msg"))
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "message".to_string()
                })?;
            Ok(ShellCommand {
                command: format!("cd {} && git commit -m {}", shell_quote(input_path), shell_quote(msg)),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        "git_log" => Ok(ShellCommand {
            command: format!("cd {} && git log --oneline", shell_quote(input_path)),
            reads_stdin: false,
            writes_stdout: true,
        }),

        "git_log_range" => {
            let from = params.get("from")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "from".to_string()
                })?;
            let to = params.get("to")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "to".to_string()
                })?;
            Ok(ShellCommand {
                command: format!("cd {} && git log --oneline {}..{}", shell_quote(input_path), from, to),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        "git_diff" => Ok(ShellCommand {
            command: format!("cd {} && git diff", shell_quote(input_path)),
            reads_stdin: false,
            writes_stdout: true,
        }),

        "git_diff_commits" => {
            let a = params.get("a").or_else(|| params.get("from"))
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "a".to_string()
                })?;
            let b = params.get("b").or_else(|| params.get("to"))
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "b".to_string()
                })?;
            Ok(ShellCommand {
                command: format!("cd {} && git diff {} {}", shell_quote(input_path), a, b),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        "git_branch" => {
            let name = params.get("name")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "name".to_string()
                })?;
            Ok(ShellCommand {
                command: format!("cd {} && git branch {}", shell_quote(input_path), shell_quote(name)),
                reads_stdin: false,
                writes_stdout: false,
            })
        }

        "git_checkout" => {
            let branch = params.get("branch")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "branch".to_string()
                })?;
            Ok(ShellCommand {
                command: format!("cd {} && git checkout {}", shell_quote(input_path), shell_quote(branch)),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        "git_merge" => {
            let branch = params.get("branch")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "branch".to_string()
                })?;
            let strategy = params.get("strategy")
                .map(|s| format!(" --strategy={}", s))
                .unwrap_or_default();
            Ok(ShellCommand {
                command: format!("cd {} && git merge{} {}", shell_quote(input_path), strategy, shell_quote(branch)),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        "git_rebase" => {
            let branch = params.get("branch")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "branch".to_string()
                })?;
            Ok(ShellCommand {
                command: format!("cd {} && git rebase {}", shell_quote(input_path), shell_quote(branch)),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        "git_stash" => Ok(ShellCommand {
            command: format!("cd {} && git stash", shell_quote(input_path)),
            reads_stdin: false,
            writes_stdout: true,
        }),

        "git_stash_pop" => Ok(ShellCommand {
            command: format!("cd {} && git stash pop", shell_quote(input_path)),
            reads_stdin: false,
            writes_stdout: true,
        }),

        "git_push" => {
            let remote = params.get("remote").unwrap_or(&"origin".to_string()).clone();
            let branch = params.get("branch").unwrap_or(&"HEAD".to_string()).clone();
            Ok(ShellCommand {
                command: format!("cd {} && git push {} {}", shell_quote(input_path), remote, branch),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        "git_pull" => {
            let remote = params.get("remote").unwrap_or(&"origin".to_string()).clone();
            let branch = params.get("branch").unwrap_or(&"HEAD".to_string()).clone();
            Ok(ShellCommand {
                command: format!("cd {} && git pull {} {}", shell_quote(input_path), remote, branch),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        "git_blame" => {
            let file = params.get("file").or_else(|| params.get("path"))
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "file".to_string()
                })?;
            Ok(ShellCommand {
                command: format!("cd {} && git blame {}", shell_quote(input_path), shell_quote(file)),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        "git_bisect" => {
            let good = params.get("good")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "good".to_string()
                })?;
            let bad = params.get("bad")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "bad".to_string()
                })?;
            Ok(ShellCommand {
                command: format!("cd {} && git bisect start {} {}", shell_quote(input_path), bad, good),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        "git_tag" => {
            let name = params.get("name")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "name".to_string()
                })?;
            let commit = params.get("commit").unwrap_or(&"HEAD".to_string()).clone();
            Ok(ShellCommand {
                command: format!("cd {} && git tag {} {}", shell_quote(input_path), shell_quote(name), commit),
                reads_stdin: false,
                writes_stdout: false,
            })
        }

        "git_status" => Ok(ShellCommand {
            command: format!("cd {} && git status --short", shell_quote(input_path)),
            reads_stdin: false,
            writes_stdout: true,
        }),

        // --- Terminal Multiplexers ---
        "tmux_new_session" => Ok(ShellCommand {
            command: format!("tmux new-session -d -s {}", shell_quote(input_path)),
            reads_stdin: false,
            writes_stdout: false,
        }),

        "tmux_attach" => Ok(ShellCommand {
            command: format!("tmux attach -t {}", shell_quote(input_path)),
            reads_stdin: false,
            writes_stdout: false,
        }),

        "tmux_split" => {
            let dir = params.get("direction")
                .map(|d| if d == "vertical" { "-v" } else { "-h" })
                .unwrap_or("-h");
            Ok(ShellCommand {
                command: format!("tmux split-window {}", dir),
                reads_stdin: false,
                writes_stdout: false,
            })
        }

        "tmux_send_keys" => {
            let keys = params.get("keys").or_else(|| params.get("text"))
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "keys".to_string()
                })?;
            Ok(ShellCommand {
                command: format!("tmux send-keys {} Enter", shell_quote(keys)),
                reads_stdin: false,
                writes_stdout: false,
            })
        }

        "screen_new_session" => Ok(ShellCommand {
            command: format!("screen -dmS {}", shell_quote(input_path)),
            reads_stdin: false,
            writes_stdout: false,
        }),

        "screen_attach" => Ok(ShellCommand {
            command: format!("screen -r {}", shell_quote(input_path)),
            reads_stdin: false,
            writes_stdout: false,
        }),

        // --- Structured Data Processing ---
        "jq_query" => {
            let filter = params.get("filter")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "filter".to_string()
                })?;
            let src = if let Some(pf) = prev_file { pf } else { input_path };
            Ok(ShellCommand {
                command: format!("jq {} {}", shell_quote(filter), shell_quote(src)),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        "jq_filter_seq" => {
            let filter = params.get("filter")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "filter".to_string()
                })?;
            let src = if let Some(pf) = prev_file { pf } else { input_path };
            Ok(ShellCommand {
                command: format!("jq '.[] | {}' {}", filter, shell_quote(src)),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        "jq_transform" => {
            let filter = params.get("filter")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "filter".to_string()
                })?;
            if let Some(pf) = prev_file {
                Ok(ShellCommand {
                    command: format!("jq {} {}", shell_quote(filter), shell_quote(pf)),
                    reads_stdin: false,
                    writes_stdout: true,
                })
            } else {
                Ok(ShellCommand {
                    command: format!("jq {}", shell_quote(filter)),
                    reads_stdin: true,
                    writes_stdout: true,
                })
            }
        }

        "yq_query" => {
            let filter = params.get("filter")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "filter".to_string()
                })?;
            let src = if let Some(pf) = prev_file { pf } else { input_path };
            Ok(ShellCommand {
                command: format!("yq {} {}", shell_quote(filter), shell_quote(src)),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        "yq_convert" => {
            let src = if let Some(pf) = prev_file { pf } else { input_path };
            Ok(ShellCommand {
                command: format!("yq -o=json {}", shell_quote(src)),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        "csv_cut" => {
            let cols = params.get("columns")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "columns".to_string()
                })?;
            let src = if let Some(pf) = prev_file { pf } else { input_path };
            Ok(ShellCommand {
                command: format!("cut -d, -f{} {}", cols, shell_quote(src)),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        "csv_join" => {
            let file2 = params.get("file2")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "file2".to_string()
                })?;
            let cols = params.get("columns").unwrap_or(&"1".to_string()).clone();
            Ok(ShellCommand {
                command: format!("join -t, -j {} {} {}", cols, shell_quote(input_path), shell_quote(file2)),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        "csv_sort" => {
            let cols = params.get("columns").unwrap_or(&"1".to_string()).clone();
            let src = if let Some(pf) = prev_file { pf } else { input_path };
            Ok(ShellCommand {
                command: format!("sort -t, -k{} {}", cols, shell_quote(src)),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        // --- Advanced Text Processing ---
        "awk_extract" => {
            let program = params.get("program")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "program".to_string()
                })?;
            let src = if let Some(pf) = prev_file { pf } else { input_path };
            Ok(ShellCommand {
                command: format!("awk {} {}", shell_quote(program), shell_quote(src)),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        "awk_aggregate" => {
            let program = params.get("program")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "program".to_string()
                })?;
            let src = if let Some(pf) = prev_file { pf } else { input_path };
            Ok(ShellCommand {
                command: format!("awk {} {}", shell_quote(program), shell_quote(src)),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        "sed_script" => {
            let script = params.get("script")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "script".to_string()
                })?;
            let src = if let Some(pf) = prev_file { pf } else { input_path };
            Ok(ShellCommand {
                command: format!("sed {} {}", shell_quote(script), shell_quote(src)),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        "cut_fields" => {
            let delim = params.get("delimiter").unwrap_or(&"\t".to_string()).clone();
            let fields = params.get("fields")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "fields".to_string()
                })?;
            let src = if let Some(pf) = prev_file { pf } else { input_path };
            Ok(ShellCommand {
                command: format!("cut -d{} -f{} {}", shell_quote(&delim), fields, shell_quote(src)),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        "tr_replace" => {
            let set1 = params.get("set1").or_else(|| params.get("from"))
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "set1".to_string()
                })?;
            let set2 = params.get("set2").or_else(|| params.get("to"))
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "set2".to_string()
                })?;
            if let Some(pf) = prev_file {
                Ok(ShellCommand {
                    command: format!("tr {} {} < {}", shell_quote(set1), shell_quote(set2), shell_quote(pf)),
                    reads_stdin: false,
                    writes_stdout: true,
                })
            } else {
                Ok(ShellCommand {
                    command: format!("tr {} {}", shell_quote(set1), shell_quote(set2)),
                    reads_stdin: true,
                    writes_stdout: true,
                })
            }
        }

        "paste_merge" => {
            let file2 = params.get("file2")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "file2".to_string()
                })?;
            let delim = params.get("delimiter").unwrap_or(&"\t".to_string()).clone();
            Ok(ShellCommand {
                command: format!("paste -d{} {} {}", shell_quote(&delim), shell_quote(input_path), shell_quote(file2)),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        "tee_split" => {
            let dest = params.get("path")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "path".to_string()
                })?;
            if let Some(pf) = prev_file {
                Ok(ShellCommand {
                    command: format!("tee {} < {}", shell_quote(dest), shell_quote(pf)),
                    reads_stdin: false,
                    writes_stdout: true,
                })
            } else {
                Ok(ShellCommand {
                    command: format!("tee {}", shell_quote(dest)),
                    reads_stdin: true,
                    writes_stdout: true,
                })
            }
        }

        "column_format" => {
            let delim = params.get("delimiter").unwrap_or(&"\t".to_string()).clone();
            let src = if let Some(pf) = prev_file { pf } else { input_path };
            Ok(ShellCommand {
                command: format!("column -t -s {} {}", shell_quote(&delim), shell_quote(src)),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        // --- Process & System Management ---
        "ps_list" => Ok(ShellCommand {
            command: "ps aux".to_string(),
            reads_stdin: false,
            writes_stdout: true,
        }),

        "kill_process" => {
            let signal = params.get("signal").unwrap_or(&"TERM".to_string()).clone();
            Ok(ShellCommand {
                command: format!("kill -{} {}", signal, input_path),
                reads_stdin: false,
                writes_stdout: false,
            })
        }

        "pkill_pattern" => {
            let pattern = params.get("pattern")
                .map(|s| s.as_str())
                .unwrap_or(input_path);
            Ok(ShellCommand {
                command: format!("pkill {}", shell_quote(pattern)),
                reads_stdin: false,
                writes_stdout: false,
            })
        }

        "watch_command" => {
            let cmd = params.get("command")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "command".to_string()
                })?;
            let interval = params.get("interval").unwrap_or(&"2".to_string()).clone();
            Ok(ShellCommand {
                command: format!("watch -n {} {}", interval, shell_quote(cmd)),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        "df_usage" => Ok(ShellCommand {
            command: "df -h".to_string(),
            reads_stdin: false,
            writes_stdout: true,
        }),

        "du_size" => Ok(ShellCommand {
            command: format!("du -sh {}", shell_quote(input_path)),
            reads_stdin: false,
            writes_stdout: true,
        }),

        "lsof_open" => Ok(ShellCommand {
            command: format!("lsof {}", shell_quote(input_path)),
            reads_stdin: false,
            writes_stdout: true,
        }),

        "file_type_detect" => Ok(ShellCommand {
            command: format!("file --mime-type {}", shell_quote(input_path)),
            reads_stdin: false,
            writes_stdout: true,
        }),

        "uname_info" => Ok(ShellCommand {
            command: "uname -a".to_string(),
            reads_stdin: false,
            writes_stdout: true,
        }),

        "uptime_info" => Ok(ShellCommand {
            command: "uptime".to_string(),
            reads_stdin: false,
            writes_stdout: true,
        }),

        // --- Networking ---
        "ssh_exec" => {
            let host = params.get("host")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "host".to_string()
                })?;
            let cmd = params.get("command")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "command".to_string()
                })?;
            Ok(ShellCommand {
                command: format!("ssh {} {}", shell_quote(host), shell_quote(cmd)),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        "scp_transfer" => {
            let host = params.get("host")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "host".to_string()
                })?;
            let dest = params.get("path").unwrap_or(&"~".to_string()).clone();
            Ok(ShellCommand {
                command: format!("scp {} {}:{}", shell_quote(input_path), shell_quote(host), shell_quote(&dest)),
                reads_stdin: false,
                writes_stdout: false,
            })
        }

        "wget_download" => Ok(ShellCommand {
            command: format!("wget {}", shell_quote(input_path)),
            reads_stdin: false,
            writes_stdout: false,
        }),

        "nc_connect" => {
            let host = params.get("host")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "host".to_string()
                })?;
            let port = params.get("port")
                .ok_or_else(|| ExecutorError::MissingParam {
                    op: op.to_string(), param: "port".to_string()
                })?;
            Ok(ShellCommand {
                command: format!("nc {} {}", shell_quote(host), port),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        "ping_host" => {
            let count = params.get("count").unwrap_or(&"4".to_string()).clone();
            Ok(ShellCommand {
                command: format!("ping -c {} {}", count, shell_quote(input_path)),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        "dig_lookup" => {
            let record_type = params.get("type").unwrap_or(&"A".to_string()).clone();
            Ok(ShellCommand {
                command: format!("dig {} {}", shell_quote(input_path), record_type),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        // --- Compression & Crypto ---
        "gzip_compress" => {
            let src = if let Some(pf) = prev_file { pf } else { input_path };
            Ok(ShellCommand {
                command: format!("gzip -k {}", shell_quote(src)),
                reads_stdin: false,
                writes_stdout: false,
            })
        }

        "gzip_decompress" => {
            let src = if let Some(pf) = prev_file { pf } else { input_path };
            Ok(ShellCommand {
                command: format!("gunzip -k {}", shell_quote(src)),
                reads_stdin: false,
                writes_stdout: false,
            })
        }

        "xz_compress" => {
            let src = if let Some(pf) = prev_file { pf } else { input_path };
            Ok(ShellCommand {
                command: format!("xz -k {}", shell_quote(src)),
                reads_stdin: false,
                writes_stdout: false,
            })
        }

        "base64_encode" => {
            if let Some(pf) = prev_file {
                Ok(ShellCommand {
                    command: format!("base64 < {}", shell_quote(pf)),
                    reads_stdin: false,
                    writes_stdout: true,
                })
            } else {
                Ok(ShellCommand {
                    command: "base64".to_string(),
                    reads_stdin: true,
                    writes_stdout: true,
                })
            }
        }

        "base64_decode" => {
            if let Some(pf) = prev_file {
                Ok(ShellCommand {
                    command: format!("base64 -d < {}", shell_quote(pf)),
                    reads_stdin: false,
                    writes_stdout: true,
                })
            } else {
                Ok(ShellCommand {
                    command: "base64 -d".to_string(),
                    reads_stdin: true,
                    writes_stdout: true,
                })
            }
        }

        "openssl_hash" => {
            let algo = params.get("algorithm").unwrap_or(&"sha256".to_string()).clone();
            let src = if let Some(pf) = prev_file { pf } else { input_path };
            Ok(ShellCommand {
                command: format!("openssl dgst -{} {}", algo, shell_quote(src)),
                reads_stdin: false,
                writes_stdout: true,
            })
        }

        // --- Unknown op ---
        _ => Err(ExecutorError::UnknownOp(op.to_string())),
    }
}

// ---------------------------------------------------------------------------
// Script generator
// ---------------------------------------------------------------------------

/// Generate a complete shell script from a compiled workflow.
///
/// The script uses intermediate temp files between steps for debuggability:
///   - Each step writes output to `$WORK_DIR/step_N.txt`
///   - The next step reads from the previous step's output file
///   - On error, temp files are preserved for inspection
///   - On success, the final step's output is printed to stdout
pub fn generate_script(
    compiled: &CompiledWorkflow,
    def: &WorkflowDef,
) -> Result<String, ExecutorError> {
    let mut script = String::new();

    // Shebang and header
    script.push_str("#!/bin/sh\n");
    script.push_str(&format!("# Generated by reasoning-engine: {}\n", compiled.name));
    script.push_str("#\n");
    script.push_str("# Intermediate files are written to $WORK_DIR for debuggability.\n");
    script.push_str("# If a step fails, inspect $WORK_DIR/step_N.txt to see partial output.\n");
    script.push_str("\n");

    // Strict mode and work directory
    script.push_str("set -e\n");
    script.push_str("\n");
    script.push_str("WORK_DIR=$(mktemp -d)\n");
    script.push_str("echo \"Work directory: $WORK_DIR\"\n");
    script.push_str("\n");

    // Cleanup trap — only on success; on failure, preserve for debugging
    script.push_str("cleanup() {\n");
    script.push_str("  if [ $? -eq 0 ]; then\n");
    script.push_str("    rm -rf \"$WORK_DIR\"\n");
    script.push_str("  else\n");
    script.push_str("    echo \"Script failed. Intermediate files preserved in: $WORK_DIR\" >&2\n");
    script.push_str("  fi\n");
    script.push_str("}\n");
    script.push_str("trap cleanup EXIT\n");
    script.push_str("\n");

    // Determine the primary input path
    let input_path = primary_input(def);

    let num_steps = compiled.steps.len();

    if num_steps == 0 {
        script.push_str("# (no steps)\n");
        return Ok(script);
    }

    // Single-step workflow: just run the command directly
    if num_steps == 1 {
        let step = &compiled.steps[0];
        script.push_str(&format!("# Step 1: {}\n", step.op));

        if step.is_each {
            // each-mode on a single step doesn't make sense without prior output,
            // but handle it gracefully
            let cmd = op_to_command(step, &input_path, None)?;
            script.push_str(&format!("{}\n", cmd.command));
        } else {
            let cmd = op_to_command(step, &input_path, None)?;
            script.push_str(&format!("{}\n", cmd.command));
        }
        return Ok(script);
    }

    // Multi-step workflow: use intermediate temp files
    for (i, step) in compiled.steps.iter().enumerate() {
        let step_num = i + 1;
        let out_file = format!("$WORK_DIR/step_{}.txt", step_num);
        let prev_file = if i == 0 { None } else { Some(format!("$WORK_DIR/step_{}.txt", i)) };

        script.push_str(&format!("# Step {}: {}{}\n", step_num, step.op,
            if step.is_each { " (each)" } else { "" }));

        // Format params as a comment
        if !step.params.is_empty() {
            let param_str: Vec<String> = step.params.iter()
                .map(|(k, v)| format!("{}={}", k, v))
                .collect();
            script.push_str(&format!("#   params: {}\n", param_str.join(", ")));
        }

        if step.is_each {
            // Map-each: read lines from prev file, run command on each, collect output
            let fallback = format!("$WORK_DIR/step_{}.txt", i);
            let prev = prev_file.as_deref()
                .unwrap_or(&fallback);
            // For each-mode, we generate a while-read loop
            // The command operates on each line (file path) from the previous step
            script.push_str(&format!("while IFS= read -r _line; do\n"));

            // Build command with _line as input
            let cmd = op_to_command(step, "$_line", None)?;
            script.push_str(&format!("  {}\n", cmd.command));

            script.push_str(&format!("done < \"{}\" > \"{}\"\n", prev, out_file));
        } else {
            let cmd = op_to_command(
                step,
                &input_path,
                prev_file.as_deref(),
            )?;

            if cmd.writes_stdout {
                script.push_str(&format!("{} > \"{}\"\n", cmd.command, out_file));
            } else {
                // Command doesn't write to stdout (e.g., mv, rm)
                // Still create the output file for chain continuity
                script.push_str(&format!("{}\n", cmd.command));
                if i < num_steps - 1 {
                    // Copy prev file forward so next step has input
                    if let Some(ref pf) = prev_file {
                        script.push_str(&format!("cp \"{}\" \"{}\"\n", pf, out_file));
                    } else {
                        script.push_str(&format!("echo \"done\" > \"{}\"\n", out_file));
                    }
                }
            }
        }

        script.push_str("\n");
    }

    // Print the final step's output
    let final_file = format!("$WORK_DIR/step_{}.txt", num_steps);
    script.push_str(&format!("# === Output ===\n"));
    script.push_str(&format!("cat \"{}\"\n", final_file));

    Ok(script)
}

// ---------------------------------------------------------------------------
// Script execution
// ---------------------------------------------------------------------------

/// Result of running a generated script.
#[derive(Debug, Clone)]
pub struct ScriptResult {
    /// The exit code (0 = success)
    pub exit_code: i32,
    /// Captured stdout
    pub stdout: String,
    /// Captured stderr
    pub stderr: String,
}

/// Execute a shell script string via /bin/sh.
pub fn run_script(script: &str) -> Result<ScriptResult, ExecutorError> {
    use std::process::Command;

    let output = Command::new("/bin/sh")
        .arg("-c")
        .arg(script)
        .output()
        .map_err(|e| ExecutorError::ExecFailed {
            exit_code: None,
            stderr: format!("failed to spawn /bin/sh: {}", e),
        })?;

    let exit_code = output.status.code().unwrap_or(-1);
    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();

    Ok(ScriptResult {
        exit_code,
        stdout,
        stderr,
    })
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Extract the primary input path from a WorkflowDef.
/// Looks for common input keys: path, archive, logfile, repo, source, query, url.
fn primary_input(def: &WorkflowDef) -> String {
    // Try common keys in priority order
    for key in &["path", "archive", "logfile", "repo", "source", "query", "url"] {
        if let Some(v) = def.inputs.get(*key) {
            return v.clone();
        }
    }
    // Fall back to first input value
    def.inputs.values().next()
        .cloned()
        .unwrap_or_else(|| ".".to_string())
}

/// Convert a glob-like pattern to a grep-compatible regex.
/// e.g., "*.pdf" → "\\.pdf$", "*.tmp" → "\\.tmp$"
fn glob_to_grep(pattern: &str) -> String {
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
fn sed_escape(s: &str) -> String {
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

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn make_step(op: &str, params: &[(&str, &str)]) -> CompiledStep {
        CompiledStep {
            index: 0,
            op: op.to_string(),
            is_each: false,
            input_type: TypeExpr::prim("Bytes"),
            output_type: TypeExpr::prim("Bytes"),
            params: params.iter().map(|(k, v)| (k.to_string(), v.to_string())).collect(),
        }
    }

    fn make_step_typed(op: &str, input_type: TypeExpr, output_type: TypeExpr, params: &[(&str, &str)]) -> CompiledStep {
        CompiledStep {
            index: 0,
            op: op.to_string(),
            is_each: false,
            input_type,
            output_type,
            params: params.iter().map(|(k, v)| (k.to_string(), v.to_string())).collect(),
        }
    }

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

    // --- Archive format detection ---

    #[test]
    fn test_extract_archive_format_cbz() {
        // File(Archive(File(Image), Cbz))
        let ty = TypeExpr::Constructor("File".into(), vec![
            TypeExpr::Constructor("Archive".into(), vec![
                TypeExpr::Constructor("File".into(), vec![TypeExpr::prim("Image")]),
                TypeExpr::prim("Cbz"),
            ]),
        ]);
        assert_eq!(extract_archive_format(&ty), Some("Cbz"));
    }

    #[test]
    fn test_extract_archive_format_zip() {
        let ty = TypeExpr::Constructor("File".into(), vec![
            TypeExpr::Constructor("Archive".into(), vec![
                TypeExpr::prim("Bytes"),
                TypeExpr::prim("Zip"),
            ]),
        ]);
        assert_eq!(extract_archive_format(&ty), Some("Zip"));
    }

    #[test]
    fn test_extract_archive_format_targz() {
        let ty = TypeExpr::Constructor("File".into(), vec![
            TypeExpr::Constructor("Archive".into(), vec![
                TypeExpr::prim("Bytes"),
                TypeExpr::prim("TarGz"),
            ]),
        ]);
        assert_eq!(extract_archive_format(&ty), Some("TarGz"));
    }

    #[test]
    fn test_extract_archive_format_not_archive() {
        let ty = TypeExpr::Constructor("File".into(), vec![TypeExpr::prim("Text")]);
        assert_eq!(extract_archive_format(&ty), None);
    }

    // --- Op to command: basic ops ---

    #[test]
    fn test_list_dir() {
        let step = make_step("list_dir", &[]);
        let cmd = op_to_command(&step, "~/Downloads", None).unwrap();
        assert_eq!(cmd.command, "ls ~/Downloads");
        assert!(cmd.writes_stdout);
    }

    #[test]
    fn test_walk_tree() {
        let step = make_step("walk_tree", &[]);
        let cmd = op_to_command(&step, "~/Documents", None).unwrap();
        assert_eq!(cmd.command, "find ~/Documents -type f");
    }

    #[test]
    fn test_filter_with_pattern() {
        let step = make_step("filter", &[("pattern", "*.pdf")]);
        let cmd = op_to_command(&step, ".", Some("/tmp/step_1.txt")).unwrap();
        assert!(cmd.command.contains("grep"));
        assert!(cmd.command.contains("/tmp/step_1.txt"));
    }

    #[test]
    fn test_filter_missing_pattern() {
        let step = make_step("filter", &[]);
        let result = op_to_command(&step, ".", None);
        assert!(result.is_err());
        match result {
            Err(ExecutorError::MissingParam { op, param }) => {
                assert_eq!(op, "filter");
                assert_eq!(param, "pattern");
            }
            _ => panic!("expected MissingParam"),
        }
    }

    #[test]
    fn test_sort_by_name() {
        let step = make_step("sort_by", &[("field", "name")]);
        let cmd = op_to_command(&step, ".", Some("/tmp/step_1.txt")).unwrap();
        assert_eq!(cmd.command, "sort /tmp/step_1.txt");
    }

    #[test]
    fn test_sort_by_size() {
        let step = make_step("sort_by", &[("field", "size")]);
        let cmd = op_to_command(&step, ".", Some("/tmp/step_1.txt")).unwrap();
        assert_eq!(cmd.command, "sort -n /tmp/step_1.txt");
    }

    // --- Archive ops ---

    #[test]
    fn test_extract_archive_cbz() {
        let input_type = TypeExpr::Constructor("File".into(), vec![
            TypeExpr::Constructor("Archive".into(), vec![
                TypeExpr::Constructor("File".into(), vec![TypeExpr::prim("Image")]),
                TypeExpr::prim("Cbz"),
            ]),
        ]);
        let step = make_step_typed("extract_archive", input_type, TypeExpr::prim("Bytes"), &[]);
        let cmd = op_to_command(&step, "comic.cbz", None).unwrap();
        assert!(cmd.command.contains("unzip"));
        assert!(cmd.command.contains("comic.cbz"));
    }

    #[test]
    fn test_extract_archive_targz() {
        let input_type = TypeExpr::Constructor("File".into(), vec![
            TypeExpr::Constructor("Archive".into(), vec![
                TypeExpr::prim("Bytes"),
                TypeExpr::prim("TarGz"),
            ]),
        ]);
        let step = make_step_typed("extract_archive", input_type, TypeExpr::prim("Bytes"), &[]);
        let cmd = op_to_command(&step, "data.tar.gz", None).unwrap();
        assert!(cmd.command.contains("tar xzf"));
        assert!(cmd.command.contains("data.tar.gz"));
    }

    #[test]
    fn test_extract_archive_zip() {
        let input_type = TypeExpr::Constructor("File".into(), vec![
            TypeExpr::Constructor("Archive".into(), vec![
                TypeExpr::prim("Bytes"),
                TypeExpr::prim("Zip"),
            ]),
        ]);
        let step = make_step_typed("extract_archive", input_type, TypeExpr::prim("Bytes"), &[]);
        let cmd = op_to_command(&step, "archive.zip", None).unwrap();
        assert!(cmd.command.contains("unzip"));
    }

    // --- Git ops ---

    #[test]
    fn test_git_log() {
        let step = make_step("git_log", &[]);
        let cmd = op_to_command(&step, "my-project.git", None).unwrap();
        assert!(cmd.command.contains("git log"));
        assert!(cmd.command.contains("my-project.git"));
    }

    #[test]
    fn test_git_diff() {
        let step = make_step("git_diff", &[]);
        let cmd = op_to_command(&step, ".", None).unwrap();
        assert!(cmd.command.contains("git diff"));
    }

    #[test]
    fn test_git_status() {
        let step = make_step("git_status", &[]);
        let cmd = op_to_command(&step, ".", None).unwrap();
        assert!(cmd.command.contains("git status"));
    }

    // --- Unknown op ---

    #[test]
    fn test_unknown_op() {
        let step = make_step("nonexistent_op", &[]);
        let result = op_to_command(&step, ".", None);
        assert!(result.is_err());
        match result {
            Err(ExecutorError::UnknownOp(name)) => assert_eq!(name, "nonexistent_op"),
            _ => panic!("expected UnknownOp"),
        }
    }

    // --- Path with spaces ---

    #[test]
    fn test_path_with_spaces() {
        let step = make_step("list_dir", &[]);
        let cmd = op_to_command(&step, "/path/to/my dir", None).unwrap();
        assert_eq!(cmd.command, "ls '/path/to/my dir'");
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

    // --- Power tools ---

    #[test]
    fn test_awk_extract() {
        let step = make_step("awk_extract", &[("program", "{print $1, $3, $5}")]);
        let cmd = op_to_command(&step, "/var/log/app.log", None).unwrap();
        assert!(cmd.command.contains("awk"));
        assert!(cmd.command.contains("/var/log/app.log"));
    }

    #[test]
    fn test_sed_script() {
        let step = make_step("sed_script", &[("script", "s/ERROR/🔴 ERROR/g")]);
        let cmd = op_to_command(&step, "input.txt", None).unwrap();
        assert!(cmd.command.contains("sed"));
    }

    #[test]
    fn test_jq_query() {
        let step = make_step("jq_query", &[("filter", ".name")]);
        let cmd = op_to_command(&step, "data.json", None).unwrap();
        assert!(cmd.command.contains("jq"));
        assert!(cmd.command.contains("data.json"));
    }

    // --- System ops ---

    #[test]
    fn test_ps_list() {
        let step = make_step("ps_list", &[]);
        let cmd = op_to_command(&step, ".", None).unwrap();
        assert_eq!(cmd.command, "ps aux");
    }

    #[test]
    fn test_df_usage() {
        let step = make_step("df_usage", &[]);
        let cmd = op_to_command(&step, ".", None).unwrap();
        assert_eq!(cmd.command, "df -h");
    }

    #[test]
    fn test_du_size() {
        let step = make_step("du_size", &[]);
        let cmd = op_to_command(&step, "/var/log", None).unwrap();
        assert_eq!(cmd.command, "du -sh /var/log");
    }

    // --- Compression ---

    #[test]
    fn test_gzip_compress() {
        let step = make_step("gzip_compress", &[]);
        let cmd = op_to_command(&step, "file.txt", None).unwrap();
        assert!(cmd.command.contains("gzip"));
    }

    #[test]
    fn test_base64_encode() {
        let step = make_step("base64_encode", &[]);
        let cmd = op_to_command(&step, ".", Some("/tmp/step_1.txt")).unwrap();
        assert!(cmd.command.contains("base64"));
        assert!(cmd.command.contains("/tmp/step_1.txt"));
    }

    // --- Networking ---

    #[test]
    fn test_ping_host() {
        let step = make_step("ping_host", &[("count", "3")]);
        let cmd = op_to_command(&step, "google.com", None).unwrap();
        assert_eq!(cmd.command, "ping -c 3 google.com");
    }

    #[test]
    fn test_dig_lookup() {
        let step = make_step("dig_lookup", &[("type", "MX")]);
        let cmd = op_to_command(&step, "example.com", None).unwrap();
        assert_eq!(cmd.command, "dig example.com MX");
    }

    // --- macOS ops ---

    #[test]
    fn test_spotlight_search() {
        let step = make_step("spotlight_search", &[("query", "kind:pdf author:Smith")]);
        let cmd = op_to_command(&step, ".", None).unwrap();
        assert!(cmd.command.contains("mdfind"));
        assert!(cmd.command.contains("kind:pdf author:Smith"));
    }

    #[test]
    fn test_open_file() {
        let step = make_step("open_file", &[]);
        let cmd = op_to_command(&step, "document.pdf", None).unwrap();
        assert_eq!(cmd.command, "open document.pdf");
    }

    #[test]
    fn test_reveal() {
        let step = make_step("reveal", &[]);
        let cmd = op_to_command(&step, "~/Downloads", None).unwrap();
        assert_eq!(cmd.command, "open -R ~/Downloads");
    }

    #[test]
    fn test_clipboard_copy_from_file() {
        let step = make_step("clipboard_copy", &[]);
        let cmd = op_to_command(&step, ".", Some("/tmp/step_1.txt")).unwrap();
        assert_eq!(cmd.command, "pbcopy < /tmp/step_1.txt");
    }

    #[test]
    fn test_clipboard_paste() {
        let step = make_step("clipboard_paste", &[]);
        let cmd = op_to_command(&step, ".", None).unwrap();
        assert_eq!(cmd.command, "pbpaste");
    }
}
