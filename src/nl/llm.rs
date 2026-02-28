//! Local LLM frontend for NL → op + slot extraction.
//!
//! Replaces the deterministic Earley parser when the `llm` feature is enabled.
//! A small local GGUF model reads user input, picks op(s), extracts parameter
//! values, and outputs a simple key-value format. Rust then mechanically
//! builds the plan sexp from that.
//!
//! Zero network calls. Model loaded once, cached in OnceLock.

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::OnceLock;

use llama_cpp_2::context::params::LlamaContextParams;
use llama_cpp_2::llama_backend::LlamaBackend;
use llama_cpp_2::llama_batch::LlamaBatch;
use llama_cpp_2::model::params::LlamaModelParams;
use llama_cpp_2::model::{AddBos, LlamaModel};
use llama_cpp_2::sampling::LlamaSampler;
use llama_cpp_2::{send_logs_to_tracing, LogOptions};

// ---------------------------------------------------------------------------
// Configuration
// ---------------------------------------------------------------------------

const MODEL_PATH_ENV: &str = "CADMUS_LLM_MODEL";
const DEFAULT_MODEL_PATH: &str = concat!(env!("HOME"), "/.models/Qwen2.5-3B-Instruct-Q4_K_M.gguf");
const MAX_TOKENS: i32 = 64;

// ---------------------------------------------------------------------------
// Cached model (loaded once, lazily)
// ---------------------------------------------------------------------------

struct LlmState {
    backend: LlamaBackend,
    model: LlamaModel,
}

unsafe impl Send for LlmState {}
unsafe impl Sync for LlmState {}

static LLM: OnceLock<Option<LlmState>> = OnceLock::new();

fn get_llm() -> Option<&'static LlmState> {
    LLM.get_or_init(|| {
        let path = std::env::var(MODEL_PATH_ENV)
            .map(PathBuf::from)
            .unwrap_or_else(|_| PathBuf::from(DEFAULT_MODEL_PATH));

        if !path.exists() {
            eprintln!("[llm] model not found at {}", path.display());
            return None;
        }

        // Suppress llama.cpp's verbose logging
        send_logs_to_tracing(LogOptions::default().with_logs_enabled(false));

        eprint!("[llm] loading model... ");

        let backend = match LlamaBackend::init() {
            Ok(b) => b,
            Err(e) => {
                eprintln!("[llm] backend init failed: {e}");
                return None;
            }
        };

        let params = LlamaModelParams::default();
        let model = match LlamaModel::load_from_file(&backend, &path, &params) {
            Ok(m) => m,
            Err(e) => {
                eprintln!("[llm] model load failed: {e}");
                return None;
            }
        };

        eprintln!("ready.");
        Some(LlmState { backend, model })
    })
    .as_ref()
}

// ---------------------------------------------------------------------------
// Op catalog — build from registry, focused set
// ---------------------------------------------------------------------------

/// Ops the LLM can choose from. Focused set to keep prompt small.
const LLM_OPS: &[&str] = &[
    // code editing
    "grep_code", "find_definition", "find_usages", "find_imports",
    "list_symbols", "file_outline", "list_source_files", "recently_changed",
    "sed_replace", "fix_import", "add_after", "remove_lines", "fix_assertion",
    "build_project", "test_project", "lint_project",
    // filesystem
    "walk_tree", "search_content", "find_matching", "list_dir",
    "read_file", "copy", "move_entry", "rename", "delete", "diff",
    "pack_archive", "extract_archive", "gzip_compress",
    "sort_by", "head", "tail", "count", "unique", "filter",
    "checksum", "download",
];

fn build_catalog() -> String {
    let reg = crate::fs_types::build_full_registry();
    let mut catalog = String::new();

    for &name in LLM_OPS {
        if let Some(entry) = reg.get_poly(name) {
            let params = if entry.input_names.is_empty() {
                entry.signature.inputs.iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            } else {
                entry.input_names.join(", ")
            };
            let desc = if entry.description.is_empty() {
                "".to_string()
            } else {
                let d = &entry.description;
                d.find(" — ")
                    .map(|i| d[i + 5..].to_string())
                    .unwrap_or_else(|| d.clone())
            };
            catalog.push_str(&format!("{name}({params}): {desc}\n"));
        }
    }

    catalog
}

// ---------------------------------------------------------------------------
// Prompt construction
// ---------------------------------------------------------------------------

fn build_prompt(user_input: &str) -> String {
    let catalog = build_catalog();

    format!(
        r#"<|im_start|>system
You extract a structured command from the user's request.

Available operations:
{catalog}
Output format — op name on first line, then param=value lines. ONLY output the structured lines, nothing else. If multiple ops, list them comma-separated on the op line.

Examples:

User: search for TODO in my code
op: grep_code
dir: .
pattern: TODO

User: where is compile_plan defined in src
op: find_definition
dir: src
name: compile_plan

User: show me the outline of src/plan.rs
op: file_outline
file: src/plan.rs

User: replace old_api with new_api in src/main.rs and then build
op: sed_replace, build_project
file: src/main.rs
find: old_api
replace: new_api
dir: .

User: run the tests
op: test_project
dir: .

User: what files changed recently
op: recently_changed
dir: .

User: find all uses of MyStruct in the project
op: find_usages
dir: .
symbol: MyStruct

User: fix the import from super::foo to crate::foo in lib.rs
op: fix_import
file: lib.rs
old_path: super::foo
new_path: crate::foo

User: zip up my downloads
op: pack_archive
(Dir Bytes): ~/Downloads

User: find PDFs in documents
op: walk_tree, find_matching
(Dir Bytes): ~/Documents
<|im_end|>
<|im_start|>user
{user_input}<|im_end|>
<|im_start|>assistant
"#
    )
}

// ---------------------------------------------------------------------------
// Inference
// ---------------------------------------------------------------------------

fn generate(llm: &LlmState, prompt: &str) -> Option<String> {
    let ctx_params = LlamaContextParams::default()
        .with_n_ctx(std::num::NonZeroU32::new(4096));

    let mut ctx = llm.model.new_context(&llm.backend, ctx_params).ok()?;
    let tokens = llm.model.str_to_token(prompt, AddBos::Always).ok()?;

    let mut batch = LlamaBatch::new(4096, 1);
    let last_index = tokens.len() as i32 - 1;
    for (i, token) in (0_i32..).zip(tokens.into_iter()) {
        batch.add(token, i, &[0], i == last_index).ok()?;
    }
    ctx.decode(&mut batch).ok()?;

    let mut n_cur = batch.n_tokens();
    let n_max = n_cur + MAX_TOKENS;
    let mut sampler = LlamaSampler::greedy();
    let mut gen_tokens = Vec::new();

    while n_cur <= n_max {
        let token = sampler.sample(&ctx, batch.n_tokens() - 1);
        sampler.accept(token);
        if llm.model.is_eog_token(token) {
            break;
        }
        gen_tokens.push(token);
        batch.clear();
        batch.add(token, n_cur, &[0], true).ok()?;
        n_cur += 1;
        ctx.decode(&mut batch).ok()?;
    }

    let mut decoder = encoding_rs::UTF_8.new_decoder();
    let mut output = String::new();
    for token in &gen_tokens {
        if let Ok(piece) = llm.model.token_to_piece(*token, &mut decoder, true, None) {
            output.push_str(&piece);
        }
    }

    Some(output)
}

// ---------------------------------------------------------------------------
// Parse LLM output → (ops, slots)
// ---------------------------------------------------------------------------

/// Parsed intent from LLM output.
#[derive(Debug)]
pub struct LlmIntent {
    pub ops: Vec<String>,
    pub slots: HashMap<String, String>,
}

fn parse_llm_output(raw: &str) -> Option<LlmIntent> {
    let mut ops = Vec::new();
    let mut slots = HashMap::new();

    for line in raw.trim().lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        if let Some(rest) = line.strip_prefix("op:") {
            for op in rest.split(',') {
                let op = op.trim().to_string();
                if !op.is_empty() {
                    ops.push(op);
                }
            }
        } else if let Some(colon_pos) = line.find(':') {
            let key = line[..colon_pos].trim().to_string();
            let value = line[colon_pos + 1..].trim().to_string();
            if !key.is_empty() && !value.is_empty() {
                slots.insert(key, value);
            }
        }
    }

    if ops.is_empty() {
        None
    } else {
        Some(LlmIntent { ops, slots })
    }
}

// ---------------------------------------------------------------------------
// Build plan sexp from intent
// ---------------------------------------------------------------------------

fn build_plan_sexpr(intent: &LlmIntent) -> Option<String> {
    let reg = crate::fs_types::build_full_registry();

    // Validate all ops exist
    for op_name in &intent.ops {
        if reg.get_poly(op_name).is_none() {
            eprintln!("[llm] unknown op: {op_name}");
            return None;
        }
    }

    // Collect all parameter names across all ops
    let mut all_params: Vec<(String, String)> = Vec::new(); // (name, type)
    let mut seen_params = std::collections::HashSet::new();

    for op_name in &intent.ops {
        if let Some(entry) = reg.get_poly(op_name) {
            for (pname, ptype) in entry.input_names.iter().zip(entry.signature.inputs.iter()) {
                if seen_params.insert(pname.clone()) {
                    all_params.push((pname.clone(), ptype.to_string()));
                }
            }
        }
    }

    // Build plan name from first op
    let plan_name = intent.ops.join("-and-");

    // Build the define header
    let params_str: Vec<String> = all_params
        .iter()
        .map(|(name, ty)| format!("({name} : {ty})"))
        .collect();
    let header = format!("(define ({plan_name} {})", params_str.join(" "));

    // Build bind lines — use slot values from LLM, or "." / "" defaults
    let mut binds = Vec::new();
    for (pname, _ptype) in &all_params {
        let value = intent
            .slots
            .get(pname)
            .cloned()
            .unwrap_or_else(|| {
                if pname == "dir" { ".".to_string() }
                else { String::new() }
            });
        if !value.is_empty() {
            binds.push(format!("  (bind {pname} \"{}\")", value.replace('"', "\\\"")));
        }
    }

    // Build op call lines
    let mut steps = Vec::new();
    for (i, op_name) in intent.ops.iter().enumerate() {
        if let Some(entry) = reg.get_poly(op_name) {
            // For second+ ops, pass parameters that overlap via :param "$var"
            if i > 0 && !entry.input_names.is_empty() {
                let mut param_refs = Vec::new();
                for pname in &entry.input_names {
                    if intent.slots.contains_key(pname) || pname == "dir" {
                        param_refs.push(format!(":{pname} \"${pname}\""));
                    }
                }
                if param_refs.is_empty() {
                    steps.push(format!("  ({op_name})"));
                } else {
                    steps.push(format!("  ({op_name} {})", param_refs.join(" ")));
                }
            } else {
                steps.push(format!("  ({op_name})"));
            }
        }
    }

    let mut plan = header;
    plan.push('\n');
    for b in &binds {
        plan.push_str(b);
        plan.push('\n');
    }
    for s in &steps {
        plan.push_str(s);
        plan.push('\n');
    }
    // Close the outer paren
    plan.truncate(plan.trim_end().len());
    plan.push(')');

    Some(plan)
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Process user input through the local LLM.
/// Returns Some((plan_sexpr, ops_used)) on success.
pub fn process(user_input: &str) -> Option<(String, Vec<String>)> {
    let llm = get_llm()?;
    let prompt = build_prompt(user_input);
    let raw = generate(llm, &prompt)?;

    let intent = parse_llm_output(&raw)?;

    let ops_display = intent.ops.join(", ");
    let slots_display: Vec<String> = intent.slots.iter()
        .map(|(k, v)| format!("{k}={v}"))
        .collect();
    eprintln!("[llm] → {ops_display}  {}", slots_display.join("  "));

    let sexpr = build_plan_sexpr(&intent)?;
    let ops = intent.ops.clone();
    Some((sexpr, ops))
}
