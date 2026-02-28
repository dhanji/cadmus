//! Web Server Tests
//!
//! Tests for the HTTP web server ops: plan compilation, codegen, NL routing,
//! and autoregression integration.

use cadmus::nl::dialogue::DialogueState;
use cadmus::nl::{self, NlResponse};
use cadmus::plan;
use cadmus::racket_executor;

// ---------------------------------------------------------------------------
// Plan compilation tests
// ---------------------------------------------------------------------------

#[test]
fn test_web_server_plan_compiles() {
    let content = std::fs::read_to_string("data/plans/web_server.sexp")
        .expect("web_server.sexp should exist");
    let plan = cadmus::sexpr::parse_sexpr_to_plan(&content)
        .expect("web_server.sexp should parse");
    let registry = cadmus::fs_types::build_full_registry();
    let compiled = plan::compile_plan(&plan, &registry)
        .expect("web_server plan should compile");
    assert_eq!(compiled.steps.len(), 1);
    assert_eq!(compiled.steps[0].op, "http_server");
}

#[test]
fn test_add_route_plan_compiles() {
    let content = std::fs::read_to_string("data/plans/add_route_web_server.sexp")
        .expect("add_route_web_server.sexp should exist");
    let plan = cadmus::sexpr::parse_sexpr_to_plan(&content)
        .expect("add_route_web_server.sexp should parse");
    let registry = cadmus::fs_types::build_full_registry();
    let compiled = plan::compile_plan(&plan, &registry)
        .expect("add_route_web_server plan should compile");
    assert_eq!(compiled.steps.len(), 1);
    assert_eq!(compiled.steps[0].op, "add_route");
}

#[test]
fn test_error_log_pipeline_compiles() {
    let content = std::fs::read_to_string("data/plans/error_log_pipeline.sexp")
        .expect("error_log_pipeline.sexp should exist");
    let plan = cadmus::sexpr::parse_sexpr_to_plan(&content)
        .expect("error_log_pipeline.sexp should parse");
    let registry = cadmus::fs_types::build_full_registry();
    let compiled = plan::compile_plan(&plan, &registry)
        .expect("error_log_pipeline plan should compile");
    assert_eq!(compiled.steps.len(), 2);
    assert_eq!(compiled.steps[0].op, "read_file");
    assert_eq!(compiled.steps[1].op, "format_html_code_block");
}

// ---------------------------------------------------------------------------
// Codegen tests
// ---------------------------------------------------------------------------

#[test]
fn test_web_server_codegen_has_web_preamble() {
    let content = std::fs::read_to_string("data/plans/web_server.sexp")
        .expect("web_server.sexp should exist");
    let plan = cadmus::sexpr::parse_sexpr_to_plan(&content)
        .expect("web_server.sexp should parse");
    let registry = cadmus::fs_types::build_full_registry();
    let compiled = plan::compile_plan(&plan, &registry)
        .expect("web_server plan should compile");
    let script = racket_executor::generate_racket_script(&compiled, &plan, &racket_executor::build_racket_registry())
        .expect("codegen should succeed");

    assert!(script.contains("(require racket/system web-server/servlet web-server/servlet-env)"),
        "script should contain web-server require, got:\n{}", script);
    assert!(script.contains("(define (http-server port)"),
        "script should contain http-server define, got:\n{}", script);
    assert!(script.contains("serve/servlet"),
        "script should contain serve/servlet call, got:\n{}", script);
    assert!(script.contains("response/output"),
        "script should contain response/output, got:\n{}", script);
    assert!(script.contains("\"hello world\""),
        "script should contain hello world text, got:\n{}", script);
    assert!(script.contains("text/plain"),
        "script should contain text/plain mime type, got:\n{}", script);
    assert!(script.contains("(http-server 8080)"),
        "script should call http-server with port 8080, got:\n{}", script);
}

#[test]
fn test_add_route_codegen_has_web_preamble() {
    let content = std::fs::read_to_string("data/plans/add_route_web_server.sexp")
        .expect("add_route_web_server.sexp should exist");
    let plan = cadmus::sexpr::parse_sexpr_to_plan(&content)
        .expect("add_route_web_server.sexp should parse");
    let registry = cadmus::fs_types::build_full_registry();
    let compiled = plan::compile_plan(&plan, &registry)
        .expect("add_route plan should compile");
    let script = racket_executor::generate_racket_script(&compiled, &plan, &racket_executor::build_racket_registry())
        .expect("codegen should succeed");

    assert!(script.contains("(require racket/system web-server/servlet web-server/servlet-env)"),
        "script should contain web-server require, got:\n{}", script);
    assert!(script.contains("(define (add-route path handler-program)"),
        "script should contain add-route define, got:\n{}", script);
    assert!(script.contains("text/html"),
        "script should contain text/html mime type, got:\n{}", script);
}

#[test]
fn test_non_web_plan_no_web_preamble() {
    // A non-web plan should NOT get the web preamble
    let content = std::fs::read_to_string("data/plans/disk_usage.sexp")
        .expect("disk_usage.sexp should exist");
    let plan = cadmus::sexpr::parse_sexpr_to_plan(&content)
        .expect("disk_usage.sexp should parse");
    let registry = cadmus::fs_types::build_full_registry();
    let compiled = plan::compile_plan(&plan, &registry)
        .expect("disk_usage plan should compile");
    let script = racket_executor::generate_racket_script(&compiled, &plan, &racket_executor::build_racket_registry())
        .expect("codegen should succeed");

    assert!(!script.contains("web-server/servlet"),
        "non-web script should NOT contain web-server require, got:\n{}", script);
}

#[test]
fn test_error_log_pipeline_codegen() {
    let content = std::fs::read_to_string("data/plans/error_log_pipeline.sexp")
        .expect("error_log_pipeline.sexp should exist");
    let plan = cadmus::sexpr::parse_sexpr_to_plan(&content)
        .expect("error_log_pipeline.sexp should parse");
    let registry = cadmus::fs_types::build_full_registry();
    let compiled = plan::compile_plan(&plan, &registry)
        .expect("error_log_pipeline plan should compile");
    let script = racket_executor::generate_racket_script(&compiled, &plan, &racket_executor::build_racket_registry())
        .expect("codegen should succeed");

    // Pipeline plan should have format-html-code-block define
    assert!(script.contains("format-html-code-block"),
        "script should contain format-html-code-block, got:\n{}", script);
    // Pipeline should chain steps: step-2 should reference step-1
    assert!(script.contains("(format-html-code-block step-1)"),
        "format-html-code-block should reference step-1, got:\n{}", script);
    // Should NOT have web preamble (no web ops in this pipeline)
    assert!(!script.contains("web-server/servlet"),
        "pipeline script should NOT contain web-server require, got:\n{}", script);
}

// ---------------------------------------------------------------------------
// NL routing tests
// ---------------------------------------------------------------------------

#[test]
fn test_nl_spin_up_web_server() {
    let mut state = DialogueState::new();
    let response = nl::process_input(
        "Spin up a web server on localhost port 8080 serving hello world",
        &mut state,
    );
    match response {
        NlResponse::PlanCreated { plan_sexpr, .. } => {
            assert!(plan_sexpr.contains("http_server"),
                "expected http_server in plan, got: {}", plan_sexpr);
        }
        other => panic!("expected PlanCreated, got: {:?}", other),
    }
}

#[test]
fn test_nl_start_web_server() {
    let mut state = DialogueState::new();
    let response = nl::process_input(
        "start a web server",
        &mut state,
    );
    match response {
        NlResponse::PlanCreated { plan_sexpr, .. } => {
            assert!(plan_sexpr.contains("http_server"),
                "expected http_server in plan, got: {}", plan_sexpr);
        }
        other => panic!("expected PlanCreated, got: {:?}", other),
    }
}

#[test]
fn test_nl_add_route_to_web_server() {
    let mut state = DialogueState::new();
    let response = nl::process_input(
        "Add route to web server with pipeline: read file, match errors, return as html code block",
        &mut state,
    );
    match response {
        NlResponse::PlanCreated { plan_sexpr, .. } => {
            assert!(plan_sexpr.contains("add_route") || plan_sexpr.contains("add-route"),
                "expected add_route in plan, got: {}", plan_sexpr);
        }
        other => panic!("expected PlanCreated, got: {:?}", other),
    }
}

#[test]
fn test_nl_error_log_pipeline() {
    let mut state = DialogueState::new();
    let response = nl::process_input(
        "Error log pipeline: read log file, match error lines, format as html code block",
        &mut state,
    );
    match response {
        NlResponse::PlanCreated { plan_sexpr, .. } => {
            assert!(plan_sexpr.contains("format_html_code_block") || plan_sexpr.contains("format-html-code-block"),
                "expected format_html_code_block in plan, got: {}", plan_sexpr);
        }
        other => panic!("expected PlanCreated, got: {:?}", other),
    }
}

// ---------------------------------------------------------------------------
// Registry tests
// ---------------------------------------------------------------------------

#[test]
fn test_web_ops_in_registry() {
    let registry = cadmus::fs_types::build_full_registry();
    assert!(registry.get_poly("http_server").is_some(),
        "http_server should be in registry");
    assert!(registry.get_poly("add_route").is_some(),
        "add_route should be in registry");
    assert!(registry.get_poly("format_html_code_block").is_some(),
        "format_html_code_block should be in registry");
}

#[test]
fn test_web_ops_have_racket_body() {
    let registry = cadmus::fs_types::build_full_registry();
    let http = registry.get_poly("http_server").unwrap();
    assert!(http.racket_body.is_some(), "http_server should have racket_body");
    assert_eq!(http.racket_symbol.as_deref(), Some("http-server"));

    let route = registry.get_poly("add_route").unwrap();
    assert!(route.racket_body.is_some(), "add_route should have racket_body");
    assert_eq!(route.racket_symbol.as_deref(), Some("add-route"));

    let fmt = registry.get_poly("format_html_code_block").unwrap();
    assert!(fmt.racket_body.is_some(), "format_html_code_block should have racket_body");
}

// ---------------------------------------------------------------------------
// Type chain tests
// ---------------------------------------------------------------------------

#[test]
fn test_http_server_type_chain() {
    let registry = cadmus::fs_types::build_full_registry();
    let content = std::fs::read_to_string("data/plans/web_server.sexp").unwrap();
    let plan = cadmus::sexpr::parse_sexpr_to_plan(&content).unwrap();
    let compiled = plan::compile_plan(&plan, &registry).unwrap();

    // http_server: Number → Void
    assert_eq!(compiled.steps[0].output_type.to_string(), "Void");
}

#[test]
fn test_add_route_type_chain() {
    let registry = cadmus::fs_types::build_full_registry();
    let content = std::fs::read_to_string("data/plans/add_route_web_server.sexp").unwrap();
    let plan = cadmus::sexpr::parse_sexpr_to_plan(&content).unwrap();
    let compiled = plan::compile_plan(&plan, &registry).unwrap();

    // add_route: String → Void
    assert_eq!(compiled.steps[0].output_type.to_string(), "Void");
}

#[test]
fn test_error_log_pipeline_type_chain() {
    let registry = cadmus::fs_types::build_full_registry();
    let content = std::fs::read_to_string("data/plans/error_log_pipeline.sexp").unwrap();
    let plan = cadmus::sexpr::parse_sexpr_to_plan(&content).unwrap();
    let compiled = plan::compile_plan(&plan, &registry).unwrap();

    // read_file: File(Text) → Text
    // format_html_code_block: Text → String
    assert_eq!(compiled.steps.len(), 2);
    assert_eq!(compiled.steps[1].output_type.to_string(), "String");
}
