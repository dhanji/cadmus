/// Integration tests: sexpr → PlanDef → compile → codegen → execute
use cadmus::sexpr::parse_sexpr_to_plan;
use cadmus::calling_frame::{CallingFrame, DefaultFrame};

fn run_sexpr(src: &str) -> (String, String) {
    // Extract expected from ;; expected: comment
    let expected = src.lines()
        .find(|l| l.starts_with(";; expected:"))
        .map(|l| l.trim_start_matches(";; expected:").trim().to_string())
        .unwrap_or_default();

    let plan = parse_sexpr_to_plan(src)
        .unwrap_or_else(|e| panic!("Parse failed: {}", e));

    let frame = DefaultFrame::from_plan(&plan);
    let exec = frame.invoke(&plan)
        .unwrap_or_else(|e| panic!("Invoke failed: {}", e));

    (exec.stdout.trim().to_string(), expected)
}

// ============================================================================
// The 3 representative plans
// ============================================================================

#[test]
fn test_sexpr_factorial() {
    let src = r#"
;; expected: 3628800
(define (factorial (n : Number)) : Number
  (bind n 10)
  (for/fold ([acc 1]) ([i (range 1 (+ n 1))])
    (* acc i)))
"#;
    let (actual, expected) = run_sexpr(src);
    assert_eq!(actual, expected, "factorial: got '{}', expected '{}'", actual, expected);
}

#[test]
fn test_sexpr_euler_totient() {
    let src = r#"
;; expected: 4
(define (euler_totient (n : Number)) : Number
  (bind n 12)
  (for/fold ([total 0]) ([i (range 1 n)])
    (cond
      [(= (gcd i n) 1) (+ total 1)]
      [else total])))
"#;
    let (actual, expected) = run_sexpr(src);
    assert_eq!(actual, expected, "euler_totient: got '{}', expected '{}'", actual, expected);
}

#[test]
fn test_sexpr_longest_increasing_subsequence() {
    let src = r#"
;; expected: 4
(define (longest_increasing_subsequence (lst : (List Number))) : Number
  (bind lst (list 10 9 2 5 3 7 101 18))
  (let ([n (length lst)]
        [dp (make n 1)])
    (for/each ([i (range 1 n)])
      (for/each ([j (range 0 i)])
        (when (< (ref lst j) (ref lst i))
          (let ([cur (ref dp i)]
                [prev (ref dp j)])
            (set! dp i (max cur (+ prev 1)))))))
    (for/fold ([mx 0]) ([i (range 0 n)])
      (max mx (ref dp i)))))
"#;
    let (actual, expected) = run_sexpr(src);
    assert_eq!(actual, expected, "LIS: got '{}', expected '{}'", actual, expected);
}

// ============================================================================
// Error cases
// ============================================================================

#[test]
fn test_sexpr_malformed_parse_error() {
    let src = "(define (bad (n : Number)) : Number";
    let err = parse_sexpr_to_plan(src);
    assert!(err.is_err());
    let msg = err.unwrap_err().to_string();
    assert!(msg.contains("unbalanced") || msg.contains("parse error"), "got: {}", msg);
}

#[test]
fn test_sexpr_undefined_var_error() {
    let src = r#"
(define (bad (n : Number)) : Number
  (+ n undefined_var))
"#;
    let err = parse_sexpr_to_plan(src);
    assert!(err.is_err());
    assert!(err.unwrap_err().to_string().contains("undefined variable"));
}

#[test]
fn test_sexpr_recursion_error() {
    let src = r#"
(define (bad (n : Number)) : Number
  (bad (- n 1)))
"#;
    let err = parse_sexpr_to_plan(src);
    assert!(err.is_err());
    assert!(err.unwrap_err().to_string().contains("recursion"));
}

// ============================================================================
// File-based tests (read .sexp files)
// ============================================================================

#[test]
fn test_sexpr_file_factorial() {
    let src = std::fs::read_to_string("data/plans/algorithms/arithmetic/factorial.sexp").unwrap();
    let (actual, expected) = run_sexpr(&src);
    assert_eq!(actual, expected, "factorial.sexp: got '{}', expected '{}'", actual, expected);
}

#[test]
fn test_sexpr_file_euler_totient() {
    let src = std::fs::read_to_string("data/plans/algorithms/number-theory/euler_totient.sexp").unwrap();
    let (actual, expected) = run_sexpr(&src);
    assert_eq!(actual, expected, "euler_totient.sexp: got '{}', expected '{}'", actual, expected);
}

#[test]
fn test_sexpr_file_lis() {
    let src = std::fs::read_to_string("data/plans/algorithms/dynamic-programming/longest_increasing_subsequence.sexp").unwrap();
    let (actual, expected) = run_sexpr(&src);
    assert_eq!(actual, expected, "lis.sexp: got '{}', expected '{}'", actual, expected);
}
