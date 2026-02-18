// ---------------------------------------------------------------------------
/// Choose the effective source: previous step's output file if in a pipeline,
/// otherwise the workflow's input path.
fn source<'a>(prev_file: Option<&'a str>, input_path: &'a str) -> &'a str {
    prev_file.unwrap_or(input_path)
}

impl ShellCommand {
    /// A command that writes to stdout (the most common pattern).
    fn stdout(command: String) -> Self {
        ShellCommand { command, reads_stdin: false, writes_stdout: true }
    }
    /// A command that reads from stdin and writes to stdout.
    fn pipe(command: String) -> Self {
        ShellCommand { command, reads_stdin: true, writes_stdout: true }
    }
    /// A command that produces no captured output (side-effect only).
    fn exec(command: String) -> Self {
        ShellCommand { command, reads_stdin: false, writes_stdout: false }
    }
}
// Shell Executor
/// Require a parameter from the step's params map, returning MissingParam error if absent.
fn require_param<'a>(params: &'a std::collections::HashMap<String, String>, op: &str, key: &str) -> Result<&'a str, ExecutorError> {
    params.get(key)
        .map(|s| s.as_str())
        .ok_or_else(|| ExecutorError::MissingParam { op: op.to_string(), param: key.to_string() })
}

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

use crate::type_expr::TypeExpr;
use crate::shell_helpers::{glob_to_grep, sed_escape, primary_input, CodegenError};
pub use crate::shell_helpers::shell_quote;
use crate::workflow::{CompiledWorkflow, CompiledStep, WorkflowDef};

// ---------------------------------------------------------------------------
// Error type
// ---------------------------------------------------------------------------

/// Backward-compatible alias for the shared codegen error type.
pub type ExecutorError = CodegenError;

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

// ---------------------------------------------------------------------------
// Op → Shell Command mapping
// ---------------------------------------------------------------------------

/// Convert a compiled step into a shell command.
///
/// Arguments:
///   - `step`: the compiled step (op name, params, types)
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
    let src = source(prev_file, input_path);

    if let Some(r) = cmd_directory_file_io(op, params, input_path, prev_file, step) { return r; }
    if let Some(r) = cmd_filter_sort(op, params, prev_file) { return r; }
    if let Some(r) = cmd_archive(op, params, prev_file, input_path, step) { return r; }
    if let Some(r) = cmd_search(op, params, input_path, prev_file) { return r; }
    if let Some(r) = cmd_file_lifecycle(op, params, input_path, prev_file) { return r; }
    if let Some(r) = cmd_content_transform(op, params, prev_file, src) { return r; }
    if let Some(r) = cmd_metadata(op, input_path) { return r; }
    if let Some(r) = cmd_macos(op, params, input_path, prev_file, src) { return r; }
    if let Some(r) = cmd_network(op, params, input_path) { return r; }
    if let Some(r) = cmd_git(op, params, input_path) { return r; }
    if let Some(r) = cmd_terminal_mux(op, params, input_path) { return r; }
    if let Some(r) = cmd_structured_data(op, params, prev_file, input_path, src) { return r; }
    if let Some(r) = cmd_text_processing(op, params, input_path, prev_file, src) { return r; }
    if let Some(r) = cmd_process_system(op, params, input_path) { return r; }
    if let Some(r) = cmd_networking(op, params, input_path) { return r; }
    if let Some(r) = cmd_compression_crypto(op, params, prev_file, src) { return r; }

    Err(ExecutorError::UnknownOp(op.to_string()))
}

// ---------------------------------------------------------------------------
// Category: Directory & File I/O
// ---------------------------------------------------------------------------

fn cmd_directory_file_io(
    op: &str, _params: &std::collections::HashMap<String, String>,
    input_path: &str, prev_file: Option<&str>, step: &CompiledStep,
) -> Option<Result<ShellCommand, ExecutorError>> {
    match op {
        "list_dir" => Some(Ok(ShellCommand::stdout(format!("ls {}", shell_quote(input_path))))),
        "read_file" => Some(Ok(ShellCommand::stdout(
            format!("cat {}", shell_quote(source(prev_file, input_path)))))),
        "write_file" => {
            let dest = match require_param(&step.params, op, "path") {
                Ok(d) => d,
                Err(e) => return Some(Err(e)),
            };
            Some(Ok(ShellCommand::exec(
                format!("cp {} {}", shell_quote(source(prev_file, input_path)), shell_quote(dest)))))
        }
        "stat" => Some(Ok(ShellCommand::stdout(format!("stat {}", shell_quote(input_path))))),
        "walk_tree" => Some(Ok(ShellCommand::stdout(format!("find {} -type f", shell_quote(input_path))))),
        "walk_tree_hierarchy" => Some(Ok(ShellCommand::stdout(format!("find {}", shell_quote(input_path))))),
        "flatten_tree" | "concat_seq" | "map_entries" => {
            // Pass-through: already flat in shell
            Some(Ok(match prev_file {
                Some(pf) => ShellCommand::stdout(format!("cat {}", shell_quote(pf))),
                None => ShellCommand::pipe("cat".to_string()),
            }))
        }
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Category: Filtering & Sorting
// ---------------------------------------------------------------------------

fn cmd_filter_sort(
    op: &str, params: &std::collections::HashMap<String, String>,
    prev_file: Option<&str>,
) -> Option<Result<ShellCommand, ExecutorError>> {
    match op {
        "filter" => {
            let exclude = params.get("exclude");
            let pattern = match exclude.or_else(|| params.get("pattern"))
                .or_else(|| params.get("extension"))
            {
                Some(p) => p,
                None => return Some(Err(ExecutorError::MissingParam {
                    op: op.to_string(), param: "pattern".to_string()
                })),
            };
            let grep_pattern = glob_to_grep(pattern);
            let grep_flag = if exclude.is_some() { " -v" } else { "" };
            Some(Ok(match prev_file {
                Some(pf) => ShellCommand::stdout(
                    format!("grep{} {} {}", grep_flag, shell_quote(&grep_pattern), shell_quote(pf))),
                None => ShellCommand::pipe(
                    format!("grep{} {}", grep_flag, shell_quote(&grep_pattern))),
            }))
        }
        "sort_by" => {
            let flag = match params.get("field").map(|s| s.as_str())
                .or_else(|| params.get("mode").map(|s| s.as_str()))
            {
                Some("size") => " -n",
                Some("date") | Some("time") | Some("mtime") => " -t",
                Some("name") | Some(_) | None => "",
            };
            Some(Ok(match prev_file {
                Some(pf) => ShellCommand::stdout(format!("sort{} {}", flag, shell_quote(pf))),
                None => ShellCommand::pipe(format!("sort{}", flag)),
            }))
        }
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Category: Archive Operations
// ---------------------------------------------------------------------------

fn cmd_archive(
    op: &str, params: &std::collections::HashMap<String, String>,
    prev_file: Option<&str>, input_path: &str, step: &CompiledStep,
) -> Option<Result<ShellCommand, ExecutorError>> {
    match op {
        "extract_archive" => {
            let fmt = extract_archive_format(&step.input_type).unwrap_or("Zip");
            let src = source(prev_file, input_path);
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
            Some(Ok(ShellCommand::stdout(format!(
                "mkdir -p \"$WORK_DIR/extracted\" && {} && find \"$WORK_DIR/extracted\" -type f", cmd
            ))))
        }
        "pack_archive" => {
            let fmt = extract_archive_format(&step.output_type).unwrap_or("Zip");
            let output_name = params.get("output").map(|s| s.as_str()).unwrap_or("archive");
            let src = source(prev_file, input_path);
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
            Some(Ok(ShellCommand::exec(cmd)))
        }
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Category: Search
// ---------------------------------------------------------------------------

fn cmd_search(
    op: &str, params: &std::collections::HashMap<String, String>,
    input_path: &str, prev_file: Option<&str>,
) -> Option<Result<ShellCommand, ExecutorError>> {
    match op {
        "search_content" => {
            let pattern = match require_param(params, op, "pattern") {
                Ok(p) => p,
                Err(e) => return Some(Err(e)),
            };
            let case_flag = match params.get("mode").map(|s| s.as_str()) {
                Some("case-insensitive") | Some("ci") => "-i ",
                _ => "",
            };
            let target = source(prev_file, input_path);
            Some(Ok(ShellCommand::stdout(
                format!("grep -rn {}{} {}", case_flag, shell_quote(pattern), shell_quote(target)))))
        }
        "find_matching" => {
            let pattern = match require_param(params, op, "pattern") {
                Ok(p) => p,
                Err(e) => return Some(Err(e)),
            };
            Some(Ok(match prev_file {
                Some(pf) => ShellCommand::stdout(
                    format!("grep {} {}", shell_quote(&glob_to_grep(pattern)), shell_quote(pf))),
                None => ShellCommand::stdout(
                    format!("find {} -name {}", shell_quote(input_path), shell_quote(pattern))),
            }))
        }
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Category: File Lifecycle
// ---------------------------------------------------------------------------

fn cmd_file_lifecycle(
    op: &str, params: &std::collections::HashMap<String, String>,
    input_path: &str, prev_file: Option<&str>,
) -> Option<Result<ShellCommand, ExecutorError>> {
    match op {
        "rename" => {
            let name = match require_param(params, op, "name") {
                Ok(n) => n, Err(e) => return Some(Err(e)),
            };
            Some(Ok(ShellCommand::exec(format!("mv {} {}", shell_quote(input_path), shell_quote(name)))))
        }
        "move_entry" => {
            let dest = match params.get("path").or_else(|| params.get("dest")) {
                Some(d) => d.as_str(),
                None => return Some(Err(ExecutorError::MissingParam {
                    op: op.to_string(), param: "path".to_string()
                })),
            };
            Some(Ok(ShellCommand::exec(format!("mv {} {}", shell_quote(input_path), shell_quote(dest)))))
        }
        "copy" => {
            let dest = match params.get("path").or_else(|| params.get("dest")) {
                Some(d) => d.as_str(),
                None => return Some(Err(ExecutorError::MissingParam {
                    op: op.to_string(), param: "path".to_string()
                })),
            };
            Some(Ok(ShellCommand::exec(format!("cp {} {}", shell_quote(input_path), shell_quote(dest)))))
        }
        "delete" => Some(Ok(match prev_file {
            Some(pf) => ShellCommand::exec(format!("xargs rm -f < {}", shell_quote(pf))),
            None => ShellCommand::exec(format!("rm -rf {}", shell_quote(input_path))),
        })),
        "create_dir" => Some(Ok(ShellCommand::exec(format!("mkdir -p {}", shell_quote(input_path))))),
        "create_link" => {
            let name = match params.get("link_name").or_else(|| params.get("name")) {
                Some(n) => n.as_str(),
                None => return Some(Err(ExecutorError::MissingParam {
                    op: op.to_string(), param: "link_name".to_string()
                })),
            };
            Some(Ok(ShellCommand::exec(format!("ln -s {} {}", shell_quote(input_path), shell_quote(name)))))
        }
        "set_permissions" => {
            let mode = match params.get("mode").or_else(|| params.get("permissions")) {
                Some(m) => m.as_str(),
                None => return Some(Err(ExecutorError::MissingParam {
                    op: op.to_string(), param: "mode".to_string()
                })),
            };
            Some(Ok(ShellCommand::exec(format!("chmod {} {}", shell_quote(mode), shell_quote(input_path)))))
        }
        "set_owner" => {
            let owner = match require_param(params, op, "owner") {
                Ok(o) => o, Err(e) => return Some(Err(e)),
            };
            Some(Ok(ShellCommand::exec(format!("chown {} {}", shell_quote(owner), shell_quote(input_path)))))
        }
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Category: Content Transformation
// ---------------------------------------------------------------------------

fn cmd_content_transform(
    op: &str, params: &std::collections::HashMap<String, String>,
    prev_file: Option<&str>, src: &str,
) -> Option<Result<ShellCommand, ExecutorError>> {
    match op {
        "replace" => {
            let pattern = match require_param(params, op, "pattern") {
                Ok(p) => p, Err(e) => return Some(Err(e)),
            };
            let replacement = params.get("replacement").or_else(|| params.get("text"))
                .map(|s| s.as_str()).unwrap_or("");
            Some(Ok(ShellCommand::stdout(
                format!("sed 's/{}/{}/g' {}", sed_escape(pattern), sed_escape(replacement), shell_quote(src)))))
        }
        "head" => {
            let n = params.get("count").or_else(|| params.get("n"))
                .map(|s| s.as_str()).unwrap_or("10");
            Some(Ok(ShellCommand::stdout(format!("head -n {} {}", n, shell_quote(src)))))
        }
        "tail" => {
            let n = params.get("count").or_else(|| params.get("n"))
                .map(|s| s.as_str()).unwrap_or("10");
            Some(Ok(ShellCommand::stdout(format!("tail -n {} {}", n, shell_quote(src)))))
        }
        "unique" => Some(Ok(match prev_file {
            Some(pf) => ShellCommand::stdout(format!("sort -u {}", shell_quote(pf))),
            None => ShellCommand::pipe("sort -u".to_string()),
        })),
        "count" => Some(Ok(match prev_file {
            Some(pf) => ShellCommand::stdout(format!("wc -l < {}", shell_quote(pf))),
            None => ShellCommand::pipe("wc -l".to_string()),
        })),
        "diff" => {
            let file2 = match params.get("file2").or_else(|| params.get("other")) {
                Some(f) => f.as_str(),
                None => return Some(Err(ExecutorError::MissingParam {
                    op: op.to_string(), param: "file2".to_string()
                })),
            };
            // diff always uses input_path (first arg), not src
            Some(Ok(ShellCommand::stdout(format!("diff {} {}", shell_quote(src), shell_quote(file2)))))
        }
        "checksum" => Some(Ok(ShellCommand::stdout(format!("shasum -a 256 {}", shell_quote(src))))),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Category: Metadata Accessors
// ---------------------------------------------------------------------------

fn cmd_metadata(op: &str, input_path: &str) -> Option<Result<ShellCommand, ExecutorError>> {
    let ip = shell_quote(input_path);
    match op {
        "get_size" => Some(Ok(ShellCommand::stdout(format!("stat -f %z {}", ip)))),
        "get_mtime" => Some(Ok(ShellCommand::stdout(format!("stat -f %m {}", ip)))),
        "get_permissions" => Some(Ok(ShellCommand::stdout(format!("stat -f %p {}", ip)))),
        "get_file_type" => Some(Ok(ShellCommand::stdout(format!("stat -f %T {}", ip)))),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Category: macOS-Specific
// ---------------------------------------------------------------------------

fn cmd_macos(
    op: &str, params: &std::collections::HashMap<String, String>,
    input_path: &str, prev_file: Option<&str>, src: &str,
) -> Option<Result<ShellCommand, ExecutorError>> {
    let ip = shell_quote(input_path);
    match op {
        "spotlight_search" => {
            let query = params.get("query").map(|s| s.as_str()).unwrap_or(input_path);
            Some(Ok(ShellCommand::stdout(format!("mdfind {}", shell_quote(query)))))
        }
        "get_xattr" => {
            let key = match require_param(params, op, "key") {
                Ok(k) => k, Err(e) => return Some(Err(e)),
            };
            Some(Ok(ShellCommand::stdout(format!("xattr -p {} {}", shell_quote(key), ip))))
        }
        "set_xattr" => {
            let key = match require_param(params, op, "key") {
                Ok(k) => k, Err(e) => return Some(Err(e)),
            };
            let value = match require_param(params, op, "value") {
                Ok(v) => v, Err(e) => return Some(Err(e)),
            };
            Some(Ok(ShellCommand::exec(format!("xattr -w {} {} {}", shell_quote(key), shell_quote(value), ip))))
        }
        "remove_xattr" => {
            let key = match require_param(params, op, "key") {
                Ok(k) => k, Err(e) => return Some(Err(e)),
            };
            Some(Ok(ShellCommand::exec(format!("xattr -d {} {}", shell_quote(key), ip))))
        }
        "remove_quarantine" => Some(Ok(ShellCommand::exec(
            format!("xattr -d com.apple.quarantine {}", ip)))),
        "open_file" => Some(Ok(ShellCommand::exec(format!("open {}", ip)))),
        "open_with" => {
            let app = match require_param(params, op, "app") {
                Ok(a) => a, Err(e) => return Some(Err(e)),
            };
            Some(Ok(ShellCommand::exec(format!("open -a {} {}", shell_quote(app), ip))))
        }
        "reveal" => Some(Ok(ShellCommand::exec(format!("open -R {}", ip)))),
        "clipboard_copy" => Some(Ok(match prev_file {
            Some(pf) => ShellCommand::exec(format!("pbcopy < {}", shell_quote(pf))),
            None => ShellCommand { command: "pbcopy".to_string(), reads_stdin: true, writes_stdout: false },
        })),
        "clipboard_paste" => Some(Ok(ShellCommand::stdout("pbpaste".to_string()))),
        "read_plist" => Some(Ok(ShellCommand::stdout(format!("plutil -p {}", shell_quote(src))))),
        "write_plist" => {
            let dest = match require_param(params, op, "path") {
                Ok(d) => d, Err(e) => return Some(Err(e)),
            };
            Some(Ok(ShellCommand::exec(
                format!("plutil -convert xml1 -o {} {}", shell_quote(dest), shell_quote(source(prev_file, input_path))))))
        }
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Category: Network & Download
// ---------------------------------------------------------------------------

fn cmd_network(
    op: &str, params: &std::collections::HashMap<String, String>,
    input_path: &str,
) -> Option<Result<ShellCommand, ExecutorError>> {
    let ip = shell_quote(input_path);
    match op {
        "download" => Some(Ok(ShellCommand::exec(format!("curl -fsSL -O {}", ip)))),
        "upload" => {
            let url = match require_param(params, op, "url") {
                Ok(u) => u, Err(e) => return Some(Err(e)),
            };
            Some(Ok(ShellCommand::stdout(format!("curl -T {} {}", ip, shell_quote(url)))))
        }
        "sync" => {
            let dest = match params.get("path").or_else(|| params.get("dest")) {
                Some(d) => d.as_str(),
                None => return Some(Err(ExecutorError::MissingParam {
                    op: op.to_string(), param: "path".to_string()
                })),
            };
            Some(Ok(ShellCommand::stdout(format!("rsync -a {} {}", ip, shell_quote(dest)))))
        }
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Category: Git
// ---------------------------------------------------------------------------

fn cmd_git(
    op: &str, params: &std::collections::HashMap<String, String>,
    input_path: &str,
) -> Option<Result<ShellCommand, ExecutorError>> {
    let ip = shell_quote(input_path);
    let cd = format!("cd {}", ip);
    match op {
        "git_init" => Some(Ok(ShellCommand::stdout(format!("git init {}", ip)))),
        "git_clone" => Some(Ok(ShellCommand::stdout(format!("git clone {}", ip)))),
        "git_add" => {
            let files = params.get("files").map(|s| s.as_str()).unwrap_or(".");
            Some(Ok(ShellCommand::exec(format!("{} && git add {}", cd, shell_quote(files)))))
        }
        "git_commit" => {
            let msg = match params.get("message").or_else(|| params.get("msg")) {
                Some(m) => m.as_str(),
                None => return Some(Err(ExecutorError::MissingParam {
                    op: op.to_string(), param: "message".to_string()
                })),
            };
            Some(Ok(ShellCommand::stdout(format!("{} && git commit -m {}", cd, shell_quote(msg)))))
        }
        "git_log" => Some(Ok(ShellCommand::stdout(format!("{} && git log --oneline", cd)))),
        "git_log_range" => {
            let from = match require_param(params, op, "from") { Ok(v) => v, Err(e) => return Some(Err(e)) };
            let to = match require_param(params, op, "to") { Ok(v) => v, Err(e) => return Some(Err(e)) };
            Some(Ok(ShellCommand::stdout(format!("{} && git log --oneline {}..{}", cd, from, to))))
        }
        "git_diff" => Some(Ok(ShellCommand::stdout(format!("{} && git diff", cd)))),
        "git_diff_commits" => {
            let a = match params.get("a").or_else(|| params.get("from")) {
                Some(v) => v.as_str(),
                None => return Some(Err(ExecutorError::MissingParam { op: op.to_string(), param: "a".to_string() })),
            };
            let b = match params.get("b").or_else(|| params.get("to")) {
                Some(v) => v.as_str(),
                None => return Some(Err(ExecutorError::MissingParam { op: op.to_string(), param: "b".to_string() })),
            };
            Some(Ok(ShellCommand::stdout(format!("{} && git diff {} {}", cd, a, b))))
        }
        "git_branch" => {
            let name = match require_param(params, op, "name") { Ok(v) => v, Err(e) => return Some(Err(e)) };
            Some(Ok(ShellCommand::exec(format!("{} && git branch {}", cd, shell_quote(name)))))
        }
        "git_checkout" => {
            let branch = match require_param(params, op, "branch") { Ok(v) => v, Err(e) => return Some(Err(e)) };
            Some(Ok(ShellCommand::stdout(format!("{} && git checkout {}", cd, shell_quote(branch)))))
        }
        "git_merge" => {
            let branch = match require_param(params, op, "branch") { Ok(v) => v, Err(e) => return Some(Err(e)) };
            let strategy = params.get("strategy")
                .map(|s| format!(" --strategy={}", s))
                .unwrap_or_default();
            Some(Ok(ShellCommand::stdout(format!("{} && git merge{} {}", cd, strategy, shell_quote(branch)))))
        }
        "git_rebase" => {
            let branch = match require_param(params, op, "branch") { Ok(v) => v, Err(e) => return Some(Err(e)) };
            Some(Ok(ShellCommand::stdout(format!("{} && git rebase {}", cd, shell_quote(branch)))))
        }
        "git_stash" => Some(Ok(ShellCommand::stdout(format!("{} && git stash", cd)))),
        "git_stash_pop" => Some(Ok(ShellCommand::stdout(format!("{} && git stash pop", cd)))),
        "git_push" => {
            let remote = params.get("remote").map(|s| s.as_str()).unwrap_or("origin");
            let branch = params.get("branch").map(|s| s.as_str()).unwrap_or("HEAD");
            Some(Ok(ShellCommand::stdout(format!("{} && git push {} {}", cd, remote, branch))))
        }
        "git_pull" => {
            let remote = params.get("remote").map(|s| s.as_str()).unwrap_or("origin");
            let branch = params.get("branch").map(|s| s.as_str()).unwrap_or("HEAD");
            Some(Ok(ShellCommand::stdout(format!("{} && git pull {} {}", cd, remote, branch))))
        }
        "git_blame" => {
            let file = match params.get("file").or_else(|| params.get("path")) {
                Some(f) => f.as_str(),
                None => return Some(Err(ExecutorError::MissingParam { op: op.to_string(), param: "file".to_string() })),
            };
            Some(Ok(ShellCommand::stdout(format!("{} && git blame {}", cd, shell_quote(file)))))
        }
        "git_bisect" => {
            let good = match require_param(params, op, "good") { Ok(v) => v, Err(e) => return Some(Err(e)) };
            let bad = match require_param(params, op, "bad") { Ok(v) => v, Err(e) => return Some(Err(e)) };
            Some(Ok(ShellCommand::stdout(format!("{} && git bisect start {} {}", cd, bad, good))))
        }
        "git_tag" => {
            let name = match require_param(params, op, "name") { Ok(v) => v, Err(e) => return Some(Err(e)) };
            let commit = params.get("commit").map(|s| s.as_str()).unwrap_or("HEAD");
            Some(Ok(ShellCommand::exec(format!("{} && git tag {} {}", cd, shell_quote(name), commit))))
        }
        "git_status" => Some(Ok(ShellCommand::stdout(format!("{} && git status --short", cd)))),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Category: Terminal Multiplexers
// ---------------------------------------------------------------------------

fn cmd_terminal_mux(
    op: &str, params: &std::collections::HashMap<String, String>,
    input_path: &str,
) -> Option<Result<ShellCommand, ExecutorError>> {
    let ip = shell_quote(input_path);
    match op {
        "tmux_new_session" => Some(Ok(ShellCommand::exec(format!("tmux new-session -d -s {}", ip)))),
        "tmux_attach" => Some(Ok(ShellCommand::exec(format!("tmux attach -t {}", ip)))),
        "tmux_split" => {
            let dir = params.get("direction")
                .map(|d| if d == "vertical" { "-v" } else { "-h" })
                .unwrap_or("-h");
            Some(Ok(ShellCommand::exec(format!("tmux split-window {}", dir))))
        }
        "tmux_send_keys" => {
            let keys = match params.get("keys").or_else(|| params.get("text")) {
                Some(k) => k.as_str(),
                None => return Some(Err(ExecutorError::MissingParam {
                    op: op.to_string(), param: "keys".to_string()
                })),
            };
            Some(Ok(ShellCommand::exec(format!("tmux send-keys {} Enter", shell_quote(keys)))))
        }
        "screen_new_session" => Some(Ok(ShellCommand::exec(format!("screen -dmS {}", ip)))),
        "screen_attach" => Some(Ok(ShellCommand::exec(format!("screen -r {}", ip)))),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Category: Structured Data Processing
// ---------------------------------------------------------------------------

fn cmd_structured_data(
    op: &str, params: &std::collections::HashMap<String, String>,
    prev_file: Option<&str>, input_path: &str, src: &str,
) -> Option<Result<ShellCommand, ExecutorError>> {
    match op {
        "jq_query" => {
            let filter = match require_param(params, op, "filter") { Ok(v) => v, Err(e) => return Some(Err(e)) };
            Some(Ok(ShellCommand::stdout(format!("jq {} {}", shell_quote(filter), shell_quote(src)))))
        }
        "jq_filter_seq" => {
            let filter = match require_param(params, op, "filter") { Ok(v) => v, Err(e) => return Some(Err(e)) };
            Some(Ok(ShellCommand::stdout(format!("jq '.[] | {}' {}", filter, shell_quote(src)))))
        }
        "jq_transform" => {
            let filter = match require_param(params, op, "filter") { Ok(v) => v, Err(e) => return Some(Err(e)) };
            Some(Ok(match prev_file {
                Some(pf) => ShellCommand::stdout(format!("jq {} {}", shell_quote(filter), shell_quote(pf))),
                None => ShellCommand::pipe(format!("jq {}", shell_quote(filter))),
            }))
        }
        "yq_query" => {
            let filter = match require_param(params, op, "filter") { Ok(v) => v, Err(e) => return Some(Err(e)) };
            Some(Ok(ShellCommand::stdout(format!("yq {} {}", shell_quote(filter), shell_quote(src)))))
        }
        "yq_convert" => Some(Ok(ShellCommand::stdout(format!("yq -o=json {}", shell_quote(src))))),
        "csv_cut" => {
            let cols = match require_param(params, op, "columns") { Ok(v) => v, Err(e) => return Some(Err(e)) };
            Some(Ok(ShellCommand::stdout(format!("cut -d, -f{} {}", cols, shell_quote(src)))))
        }
        "csv_join" => {
            let file2 = match require_param(params, op, "file2") { Ok(v) => v, Err(e) => return Some(Err(e)) };
            let cols = params.get("columns").map(|s| s.as_str()).unwrap_or("1");
            Some(Ok(ShellCommand::stdout(
                format!("join -t, -j {} {} {}", cols, shell_quote(input_path), shell_quote(file2)))))
        }
        "csv_sort" => {
            let cols = params.get("columns").map(|s| s.as_str()).unwrap_or("1");
            Some(Ok(ShellCommand::stdout(format!("sort -t, -k{} {}", cols, shell_quote(src)))))
        }
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Category: Advanced Text Processing
// ---------------------------------------------------------------------------

fn cmd_text_processing(
    op: &str, params: &std::collections::HashMap<String, String>,
    input_path: &str, prev_file: Option<&str>, src: &str,
) -> Option<Result<ShellCommand, ExecutorError>> {
    match op {
        "awk_extract" | "awk_aggregate" => {
            let program = match require_param(params, op, "program") { Ok(v) => v, Err(e) => return Some(Err(e)) };
            Some(Ok(ShellCommand::stdout(format!("awk {} {}", shell_quote(program), shell_quote(src)))))
        }
        "sed_script" => {
            let script = match require_param(params, op, "script") { Ok(v) => v, Err(e) => return Some(Err(e)) };
            Some(Ok(ShellCommand::stdout(format!("sed {} {}", shell_quote(script), shell_quote(src)))))
        }
        "cut_fields" => {
            let delim = params.get("delimiter").map(|s| s.as_str()).unwrap_or("\t");
            let fields = match require_param(params, op, "fields") { Ok(v) => v, Err(e) => return Some(Err(e)) };
            Some(Ok(ShellCommand::stdout(format!("cut -d{} -f{} {}", shell_quote(delim), fields, shell_quote(src)))))
        }
        "tr_replace" => {
            let set1 = match params.get("set1").or_else(|| params.get("from")) {
                Some(s) => s.as_str(),
                None => return Some(Err(ExecutorError::MissingParam { op: op.to_string(), param: "set1".to_string() })),
            };
            let set2 = match params.get("set2").or_else(|| params.get("to")) {
                Some(s) => s.as_str(),
                None => return Some(Err(ExecutorError::MissingParam { op: op.to_string(), param: "set2".to_string() })),
            };
            Some(Ok(match prev_file {
                Some(pf) => ShellCommand::stdout(
                    format!("tr {} {} < {}", shell_quote(set1), shell_quote(set2), shell_quote(pf))),
                None => ShellCommand::pipe(format!("tr {} {}", shell_quote(set1), shell_quote(set2))),
            }))
        }
        "paste_merge" => {
            let file2 = match require_param(params, op, "file2") { Ok(v) => v, Err(e) => return Some(Err(e)) };
            let delim = params.get("delimiter").map(|s| s.as_str()).unwrap_or("\t");
            Some(Ok(ShellCommand::stdout(
                format!("paste -d{} {} {}", shell_quote(delim), shell_quote(input_path), shell_quote(file2)))))
        }
        "tee_split" => {
            let dest = match require_param(params, op, "path") { Ok(v) => v, Err(e) => return Some(Err(e)) };
            Some(Ok(match prev_file {
                Some(pf) => ShellCommand::stdout(format!("tee {} < {}", shell_quote(dest), shell_quote(pf))),
                None => ShellCommand::pipe(format!("tee {}", shell_quote(dest))),
            }))
        }
        "column_format" => {
            let delim = params.get("delimiter").map(|s| s.as_str()).unwrap_or("\t");
            Some(Ok(ShellCommand::stdout(
                format!("column -t -s {} {}", shell_quote(delim), shell_quote(src)))))
        }
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Category: Process & System Management
// ---------------------------------------------------------------------------

fn cmd_process_system(
    op: &str, params: &std::collections::HashMap<String, String>,
    input_path: &str,
) -> Option<Result<ShellCommand, ExecutorError>> {
    let ip = shell_quote(input_path);
    match op {
        "ps_list" => Some(Ok(ShellCommand::stdout("ps aux".to_string()))),
        "kill_process" => {
            let signal = params.get("signal").map(|s| s.as_str()).unwrap_or("TERM");
            Some(Ok(ShellCommand::exec(format!("kill -{} {}", signal, input_path))))
        }
        "pkill_pattern" => {
            let pattern = params.get("pattern").map(|s| s.as_str()).unwrap_or(input_path);
            Some(Ok(ShellCommand::exec(format!("pkill {}", shell_quote(pattern)))))
        }
        "watch_command" => {
            let cmd = match require_param(params, op, "command") { Ok(v) => v, Err(e) => return Some(Err(e)) };
            let interval = params.get("interval").map(|s| s.as_str()).unwrap_or("2");
            Some(Ok(ShellCommand::stdout(format!("watch -n {} {}", interval, shell_quote(cmd)))))
        }
        "df_usage" => Some(Ok(ShellCommand::stdout("df -h".to_string()))),
        "du_size" => Some(Ok(ShellCommand::stdout(format!("du -sh {}", ip)))),
        "lsof_open" => Some(Ok(ShellCommand::stdout(format!("lsof {}", ip)))),
        "file_type_detect" => Some(Ok(ShellCommand::stdout(format!("file --mime-type {}", ip)))),
        "uname_info" => Some(Ok(ShellCommand::stdout("uname -a".to_string()))),
        "uptime_info" => Some(Ok(ShellCommand::stdout("uptime".to_string()))),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Category: Networking (remote)
// ---------------------------------------------------------------------------

fn cmd_networking(
    op: &str, params: &std::collections::HashMap<String, String>,
    input_path: &str,
) -> Option<Result<ShellCommand, ExecutorError>> {
    let ip = shell_quote(input_path);
    match op {
        "ssh_exec" => {
            let host = match require_param(params, op, "host") { Ok(v) => v, Err(e) => return Some(Err(e)) };
            let cmd = match require_param(params, op, "command") { Ok(v) => v, Err(e) => return Some(Err(e)) };
            Some(Ok(ShellCommand::stdout(format!("ssh {} {}", shell_quote(host), shell_quote(cmd)))))
        }
        "scp_transfer" => {
            let host = match require_param(params, op, "host") { Ok(v) => v, Err(e) => return Some(Err(e)) };
            let dest = params.get("path").map(|s| s.as_str()).unwrap_or("~");
            Some(Ok(ShellCommand::exec(format!("scp {} {}:{}", ip, shell_quote(host), shell_quote(dest)))))
        }
        "wget_download" => Some(Ok(ShellCommand::exec(format!("wget {}", ip)))),
        "nc_connect" => {
            let host = match require_param(params, op, "host") { Ok(v) => v, Err(e) => return Some(Err(e)) };
            let port = match require_param(params, op, "port") { Ok(v) => v, Err(e) => return Some(Err(e)) };
            Some(Ok(ShellCommand::stdout(format!("nc {} {}", shell_quote(host), port))))
        }
        "ping_host" => {
            let count = params.get("count").map(|s| s.as_str()).unwrap_or("4");
            Some(Ok(ShellCommand::stdout(format!("ping -c {} {}", count, ip))))
        }
        "dig_lookup" => {
            let record_type = params.get("type").map(|s| s.as_str()).unwrap_or("A");
            Some(Ok(ShellCommand::stdout(format!("dig {} {}", ip, record_type))))
        }
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Category: Compression & Crypto
// ---------------------------------------------------------------------------

fn cmd_compression_crypto(
    op: &str, params: &std::collections::HashMap<String, String>,
    prev_file: Option<&str>, src: &str,
) -> Option<Result<ShellCommand, ExecutorError>> {
    match op {
        "gzip_compress" => Some(Ok(ShellCommand::exec(format!("gzip -k {}", shell_quote(src))))),
        "gzip_decompress" => Some(Ok(ShellCommand::exec(format!("gunzip -k {}", shell_quote(src))))),
        "xz_compress" => Some(Ok(ShellCommand::exec(format!("xz -k {}", shell_quote(src))))),
        "base64_encode" => Some(Ok(match prev_file {
            Some(pf) => ShellCommand::stdout(format!("base64 < {}", shell_quote(pf))),
            None => ShellCommand::pipe("base64".to_string()),
        })),
        "base64_decode" => Some(Ok(match prev_file {
            Some(pf) => ShellCommand::stdout(format!("base64 -d < {}", shell_quote(pf))),
            None => ShellCommand::pipe("base64 -d".to_string()),
        })),
        "openssl_hash" => {
            let algo = params.get("algorithm").map(|s| s.as_str()).unwrap_or("sha256");
            Some(Ok(ShellCommand::stdout(format!("openssl dgst -{} {}", algo, shell_quote(src)))))
        }
        _ => None,
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
    script.push_str(&format!("# Generated by cadmus: {}\n", compiled.name));
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
    let input_path = primary_input(&def.inputs);

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
