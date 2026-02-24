// ---------------------------------------------------------------------------
// S-expression plan DSL — parser, AST, and lowering
// ---------------------------------------------------------------------------
//
// A minimal Scheme-like surface syntax for Cadmus plans.
//
// Grammar:
//
//   program     = define
//   define      = '(' 'define' signature ':' type-expr bind* body+ ')'
//   signature   = '(' name param* ')'
//   param       = '(' name ':' type-expr ')'
//   bind        = '(' 'bind' name expr ')'
//   body        = expr
//   expr        = atom | list-form
//   list-form   = '(' head expr* ')'
//   head        = 'let' | 'for/fold' | 'for/each' | 'cond' | 'when'
//               | 'range' | 'ref' | 'set!' | 'make' | 'list' | op-name
//   atom        = number | string | boolean | symbol
//
// Core forms (10):
//   define, bind, let (sequential/let*), for/fold, for/each,
//   cond, when, range, ref, set!, make
//
// Everything else is an op call looked up in the registry.

use std::collections::HashMap;
use std::fmt;

// ═══════════════════════════════════════════════════════════════════════════
// Tokens
// ═══════════════════════════════════════════════════════════════════════════

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    LParen,        // (
    RParen,        // )
    LBracket,      // [
    RBracket,      // ]
    Colon,         // :
    Symbol(String), // identifiers, operators like +, *, >=, etc.
    Number(String), // numeric literals (kept as string for fidelity)
    Str(String),   // "string literals"
    Bool(bool),    // #t, #f
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::Colon => write!(f, ":"),
            Token::Symbol(s) => write!(f, "{}", s),
            Token::Number(n) => write!(f, "{}", n),
            Token::Str(s) => write!(f, "\"{}\"", s),
            Token::Bool(b) => write!(f, "{}", if *b { "#t" } else { "#f" }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Located<T> {
    pub value: T,
    pub pos: usize, // byte offset in source
}

// ═══════════════════════════════════════════════════════════════════════════
// Tokenizer
// ═══════════════════════════════════════════════════════════════════════════

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub pos: Option<usize>,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.pos {
            Some(p) => write!(f, "parse error at byte {}: {}", p, self.message),
            None => write!(f, "parse error: {}", self.message),
        }
    }
}

impl ParseError {
    fn at(pos: usize, msg: impl Into<String>) -> Self {
        Self { message: msg.into(), pos: Some(pos) }
    }
    fn eof(msg: impl Into<String>) -> Self {
        Self { message: msg.into(), pos: None }
    }
}

/// Tokenize source text into a sequence of located tokens.
pub fn tokenize(src: &str) -> Result<Vec<Located<Token>>, ParseError> {
    let mut tokens = Vec::new();
    let bytes = src.as_bytes();
    let mut i = 0;

    while i < bytes.len() {
        let b = bytes[i];

        // Skip whitespace
        if b.is_ascii_whitespace() {
            i += 1;
            continue;
        }

        // Skip line comments
        if b == b';' {
            while i < bytes.len() && bytes[i] != b'\n' {
                i += 1;
            }
            continue;
        }

        let pos = i;

        match b {
            b'(' => {
                tokens.push(Located { value: Token::LParen, pos });
                i += 1;
            }
            b')' => {
                tokens.push(Located { value: Token::RParen, pos });
                i += 1;
            }
            b'[' => {
                tokens.push(Located { value: Token::LBracket, pos });
                i += 1;
            }
            b']' => {
                tokens.push(Located { value: Token::RBracket, pos });
                i += 1;
            }
            b':' => {
                // Check if this is a standalone colon (followed by space/paren/bracket)
                // or part of a symbol
                let next = if i + 1 < bytes.len() { bytes[i + 1] } else { b' ' };
                if next.is_ascii_whitespace() || next == b'(' || next == b')' 
                    || next == b'[' || next == b']' 
                {
                    tokens.push(Located { value: Token::Colon, pos });
                    i += 1;
                } else {
                    // Part of a symbol — fall through to symbol parsing
                    let start = i;
                    while i < bytes.len() && !is_delimiter(bytes[i]) {
                        i += 1;
                    }
                    let s = std::str::from_utf8(&bytes[start..i])
                        .map_err(|_| ParseError::at(start, "invalid UTF-8"))?;
                    tokens.push(Located { value: Token::Symbol(s.to_string()), pos: start });
                }
            }
            b'"' => {
                // String literal
                i += 1; // skip opening quote
                let start = i;
                while i < bytes.len() && bytes[i] != b'"' {
                    if bytes[i] == b'\\' {
                        i += 1; // skip escaped char
                    }
                    i += 1;
                }
                if i >= bytes.len() {
                    return Err(ParseError::at(pos, "unterminated string literal"));
                }
                let s = std::str::from_utf8(&bytes[start..i])
                    .map_err(|_| ParseError::at(start, "invalid UTF-8 in string"))?;
                tokens.push(Located { value: Token::Str(s.to_string()), pos });
                i += 1; // skip closing quote
            }
            b'#' => {
                // Boolean: #t or #f
                if i + 1 < bytes.len() {
                    match bytes[i + 1] {
                        b't' => {
                            tokens.push(Located { value: Token::Bool(true), pos });
                            i += 2;
                        }
                        b'f' => {
                            tokens.push(Located { value: Token::Bool(false), pos });
                            i += 2;
                        }
                        _ => {
                            return Err(ParseError::at(pos, "expected #t or #f"));
                        }
                    }
                } else {
                    return Err(ParseError::at(pos, "unexpected # at end of input"));
                }
            }
            _ => {
                // Number or symbol
                let start = i;
                while i < bytes.len() && !is_delimiter(bytes[i]) {
                    i += 1;
                }
                let s = std::str::from_utf8(&bytes[start..i])
                    .map_err(|_| ParseError::at(start, "invalid UTF-8"))?;

                // Try to classify as number
                if is_number(s) {
                    tokens.push(Located { value: Token::Number(s.to_string()), pos: start });
                } else {
                    tokens.push(Located { value: Token::Symbol(s.to_string()), pos: start });
                }
            }
        }
    }

    Ok(tokens)
}

fn is_delimiter(b: u8) -> bool {
    b.is_ascii_whitespace()
        || b == b'(' || b == b')'
        || b == b'[' || b == b']'
        || b == b';' || b == b'"'
}

fn is_number(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }
    // Handle +inf.0, -inf.0
    if s == "+inf.0" || s == "-inf.0" {
        return true;
    }
    let s = if s.starts_with('-') || s.starts_with('+') { &s[1..] } else { s };
    if s.is_empty() {
        return false;
    }
    let mut has_dot = false;
    for b in s.bytes() {
        if b == b'.' {
            if has_dot { return false; }
            has_dot = true;
        } else if !b.is_ascii_digit() {
            return false;
        }
    }
    true
}

// ═══════════════════════════════════════════════════════════════════════════
// S-expression tree (untyped)
// ═══════════════════════════════════════════════════════════════════════════

/// Raw S-expression — the output of the parser before semantic analysis.
#[derive(Debug, Clone, PartialEq)]
pub enum Sexp {
    Atom(Atom),
    List(Vec<Sexp>),
    /// Square-bracket list — used for let-bindings, cond clauses, for/fold accumulators
    Bracket(Vec<Sexp>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    Symbol(String),
    Number(String),
    Str(String),
    Bool(bool),
    Colon,
}

impl fmt::Display for Sexp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Sexp::Atom(a) => write!(f, "{}", a),
            Sexp::List(items) => {
                write!(f, "(")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 { write!(f, " ")?; }
                    write!(f, "{}", item)?;
                }
                write!(f, ")")
            }
            Sexp::Bracket(items) => {
                write!(f, "[")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 { write!(f, " ")?; }
                    write!(f, "{}", item)?;
                }
                write!(f, "]")
            }
        }
    }
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Atom::Symbol(s) => write!(f, "{}", s),
            Atom::Number(n) => write!(f, "{}", n),
            Atom::Str(s) => write!(f, "\"{}\"", s),
            Atom::Bool(b) => write!(f, "{}", if *b { "#t" } else { "#f" }),
            Atom::Colon => write!(f, ":"),
        }
    }
}

/// Parse tokens into an S-expression tree.
pub fn parse_sexp(tokens: &[Located<Token>]) -> Result<Sexp, ParseError> {
    let mut pos = 0;
    let result = parse_one(tokens, &mut pos)?;
    if pos < tokens.len() {
        return Err(ParseError::at(
            tokens[pos].pos,
            format!("unexpected token after top-level form: {}", tokens[pos].value),
        ));
    }
    Ok(result)
}

fn parse_one(tokens: &[Located<Token>], pos: &mut usize) -> Result<Sexp, ParseError> {
    if *pos >= tokens.len() {
        return Err(ParseError::eof("unexpected end of input"));
    }

    let tok = &tokens[*pos];
    match &tok.value {
        Token::LParen => {
            let start_pos = tok.pos;
            *pos += 1;
            let mut items = Vec::new();
            loop {
                if *pos >= tokens.len() {
                    return Err(ParseError::at(start_pos, "unbalanced '(' — missing closing ')'"));
                }
                if tokens[*pos].value == Token::RParen {
                    *pos += 1;
                    return Ok(Sexp::List(items));
                }
                items.push(parse_one(tokens, pos)?);
            }
        }
        Token::LBracket => {
            let start_pos = tok.pos;
            *pos += 1;
            let mut items = Vec::new();
            loop {
                if *pos >= tokens.len() {
                    return Err(ParseError::at(start_pos, "unbalanced '[' — missing closing ']'"));
                }
                if tokens[*pos].value == Token::RBracket {
                    *pos += 1;
                    return Ok(Sexp::Bracket(items));
                }
                items.push(parse_one(tokens, pos)?);
            }
        }
        Token::RParen => {
            Err(ParseError::at(tok.pos, "unexpected ')'"))
        }
        Token::RBracket => {
            Err(ParseError::at(tok.pos, "unexpected ']'"))
        }
        Token::Symbol(s) => {
            *pos += 1;
            Ok(Sexp::Atom(Atom::Symbol(s.clone())))
        }
        Token::Number(n) => {
            *pos += 1;
            Ok(Sexp::Atom(Atom::Number(n.clone())))
        }
        Token::Str(s) => {
            *pos += 1;
            Ok(Sexp::Atom(Atom::Str(s.clone())))
        }
        Token::Bool(b) => {
            *pos += 1;
            Ok(Sexp::Atom(Atom::Bool(*b)))
        }
        Token::Colon => {
            *pos += 1;
            Ok(Sexp::Atom(Atom::Colon))
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Typed AST
// ═══════════════════════════════════════════════════════════════════════════

/// A complete plan definition parsed from sexpr.
#[derive(Debug, Clone)]
pub struct PlanAst {
    /// Plan name (from the define signature)
    pub name: String,
    /// Typed parameters
    pub params: Vec<Param>,
    /// Return type expression (as string, e.g. "Number", "List(Number)")
    pub return_type: String,
    /// Default bindings: param name → expression
    pub bindings: Vec<(String, Expr)>,
    /// Body expressions (the algorithm)
    pub body: Vec<Expr>,
}

/// A typed parameter.
#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub type_expr: String,
}

/// Expression AST node.
#[derive(Debug, Clone)]
pub enum Expr {
    /// Numeric literal
    Num(String),
    /// String literal
    Str(String),
    /// Boolean literal
    Bool(bool),
    /// Variable/parameter reference
    Var(String),
    /// Sequential let: (let ([name expr] ...) body ...)
    Let {
        bindings: Vec<(String, Box<Expr>)>,
        body: Vec<Expr>,
    },
    /// Fold: (for/fold ([acc init] ...) ([var seq]) body)
    ForFold {
        accumulators: Vec<(String, Box<Expr>)>,
        var: String,
        seq: Box<Expr>,
        body: Vec<Expr>,
    },
    /// Iterate for side effects: (for/each ([var seq]) body ...)
    ForEach {
        var: String,
        seq: Box<Expr>,
        body: Vec<Expr>,
    },
    /// Multi-branch: (cond [test then] ... [else val])
    Cond {
        clauses: Vec<(Expr, Expr)>,
        else_expr: Option<Box<Expr>>,
    },
    /// Conditional side effect: (when test body ...)
    When {
        test: Box<Expr>,
        body: Vec<Expr>,
    },
    /// Integer range: (range start end)
    Range {
        start: Box<Expr>,
        end: Box<Expr>,
    },
    /// Indexed read: (ref coll i)
    Ref {
        coll: Box<Expr>,
        index: Box<Expr>,
    },
    /// Indexed write: (set! coll i val)
    SetBang {
        coll: Box<Expr>,
        index: Box<Expr>,
        val: Box<Expr>,
    },
    /// Create mutable collection: (make n init)
    Make {
        size: Box<Expr>,
        init: Box<Expr>,
    },
    /// List literal: (list expr ...)
    ListLit(Vec<Expr>),
    /// Op call: (op-name arg ... :keyword value ...)
    Call {
        op: String,
        args: Vec<Expr>,
        /// Named keyword arguments (:keyword value pairs)
        keywords: Vec<(String, Expr)>,
    },
}

// ═══════════════════════════════════════════════════════════════════════════
// Semantic analysis: Sexp → PlanAst
// ═══════════════════════════════════════════════════════════════════════════

/// Parse a source string into a PlanAst.
pub fn parse_plan_sexpr(src: &str) -> Result<PlanAst, ParseError> {
    let tokens = tokenize(src)?;
    let sexp = parse_sexp(&tokens)?;
    analyze_define(&sexp)
}

/// Analyze a top-level (define ...) form.
fn analyze_define(sexp: &Sexp) -> Result<PlanAst, ParseError> {
    let items = match sexp {
        Sexp::List(items) => items,
        _ => return Err(ParseError::at(0, "expected (define ...) at top level")),
    };

    if items.is_empty() {
        return Err(ParseError::at(0, "empty top-level form"));
    }

    // First element must be 'define'
    match &items[0] {
        Sexp::Atom(Atom::Symbol(s)) if s == "define" => {}
        _ => return Err(ParseError::at(0, "expected 'define' as first form")),
    }

    if items.len() < 3 {
        return Err(ParseError::at(0, "define requires signature and body"));
    }

    // Parse signature: (name (param : Type) ...)
    let (name, params) = parse_signature(&items[1])?;

    // Parse return type: : Type
    // items[2] should be Colon, items[3] should be the type
    let (return_type, body_start) = parse_return_type(&items, 2)?;

    // Separate bind forms from body expressions
    let mut bindings = Vec::new();
    let mut body = Vec::new();
    let mut in_bindings = true;

    for item in &items[body_start..] {
        if in_bindings {
            if let Some(bind) = try_parse_bind(item)? {
                bindings.push(bind);
                continue;
            }
            in_bindings = false;
        }
        body.push(analyze_expr(item)?);
    }

    if body.is_empty() {
        return Err(ParseError::at(0, "define body is empty — need at least one expression"));
    }

    // Check for recursion: body must not reference the plan name as a call
    check_no_recursion(&name, &body)?;

    Ok(PlanAst {
        name,
        params,
        return_type,
        bindings,
        body,
    })
}

/// Parse the signature: (name (param : Type) ...)
fn parse_signature(sexp: &Sexp) -> Result<(String, Vec<Param>), ParseError> {
    let items = match sexp {
        Sexp::List(items) => items,
        _ => return Err(ParseError::at(0, "expected (name (param : Type) ...) signature")),
    };

    if items.is_empty() {
        return Err(ParseError::at(0, "empty signature"));
    }

    let name = match &items[0] {
        Sexp::Atom(Atom::Symbol(s)) => s.clone(),
        _ => return Err(ParseError::at(0, "expected plan name as first element of signature")),
    };

    let mut params = Vec::new();
    for item in &items[1..] {
        let param_items = match item {
            Sexp::List(items) => items,
            _ => return Err(ParseError::at(0, format!("expected (param : Type), got {}", item))),
        };

        if param_items.len() != 3 {
            return Err(ParseError::at(0, format!(
                "parameter must be (name : Type), got {} elements", param_items.len()
            )));
        }

        let param_name = match &param_items[0] {
            Sexp::Atom(Atom::Symbol(s)) => s.clone(),
            _ => return Err(ParseError::at(0, "expected parameter name")),
        };

        // param_items[1] must be colon
        match &param_items[1] {
            Sexp::Atom(Atom::Colon) => {}
            _ => return Err(ParseError::at(0, format!(
                "expected ':' after parameter name '{}', got {}", param_name, param_items[1]
            ))),
        }

        let type_expr = sexp_to_type_string(&param_items[2])?;

        params.push(Param { name: param_name, type_expr });
    }

    Ok((name, params))
}

/// Convert a Sexp to a type string.
/// Handles: Symbol("Number"), or call-like List(Symbol("List"), Symbol("Number")) → "List(Number)"
fn sexp_to_type_string(sexp: &Sexp) -> Result<String, ParseError> {
    match sexp {
        Sexp::Atom(Atom::Symbol(s)) => Ok(s.clone()),
        Sexp::List(items) if !items.is_empty() => {
            // Constructor type: (List Number) → List(Number)
            let head = match &items[0] {
                Sexp::Atom(Atom::Symbol(s)) => s.clone(),
                _ => return Err(ParseError::at(0, "expected type constructor name")),
            };
            let args: Result<Vec<String>, _> = items[1..].iter()
                .map(|s| sexp_to_type_string(s))
                .collect();
            let args = args?;
            Ok(format!("{}({})", head, args.join(", ")))
        }
        _ => Err(ParseError::at(0, format!("expected type expression, got {}", sexp))),
    }
}

/// Parse return type annotation: : TypeExpr
/// Returns (type_string, index_of_first_body_element)
fn parse_return_type(items: &[Sexp], start: usize) -> Result<(String, usize), ParseError> {
    if start >= items.len() {
        return Err(ParseError::at(0, "expected ':' and return type after signature"));
    }

    match &items[start] {
        Sexp::Atom(Atom::Colon) => {
            if start + 1 >= items.len() {
                return Err(ParseError::at(0, "expected return type after ':'"));
            }
            let type_str = sexp_to_type_string(&items[start + 1])?;
            Ok((type_str, start + 2))
        }
        _ => {
            // No return type annotation — default to "Any"
            Ok(("Any".to_string(), start))
        }
    }
}

/// Try to parse a (bind name expr) form. Returns None if not a bind.
fn try_parse_bind(sexp: &Sexp) -> Result<Option<(String, Expr)>, ParseError> {
    let items = match sexp {
        Sexp::List(items) if items.len() >= 3 => items,
        _ => return Ok(None),
    };

    match &items[0] {
        Sexp::Atom(Atom::Symbol(s)) if s == "bind" => {}
        _ => return Ok(None),
    }

    let name = match &items[1] {
        Sexp::Atom(Atom::Symbol(s)) => s.clone(),
        _ => return Err(ParseError::at(0, "bind: expected parameter name")),
    };

    let expr = analyze_expr(&items[2])?;

    Ok(Some((name, expr)))
}

/// Analyze an expression from Sexp to Expr.
fn analyze_expr(sexp: &Sexp) -> Result<Expr, ParseError> {
    match sexp {
        // Atoms
        Sexp::Atom(Atom::Number(n)) => Ok(Expr::Num(n.clone())),
        Sexp::Atom(Atom::Str(s)) => Ok(Expr::Str(s.clone())),
        Sexp::Atom(Atom::Bool(b)) => Ok(Expr::Bool(*b)),
        Sexp::Atom(Atom::Symbol(s)) => Ok(Expr::Var(s.clone())),
        Sexp::Atom(Atom::Colon) => Err(ParseError::at(0, "unexpected ':' in expression")),

        // Bracket forms are not valid as standalone expressions
        Sexp::Bracket(_) => Err(ParseError::at(0, "unexpected [...] in expression position")),

        // List forms
        Sexp::List(items) => {
            if items.is_empty() {
                return Err(ParseError::at(0, "empty expression ()"));
            }

            let head = match &items[0] {
                Sexp::Atom(Atom::Symbol(s)) => s.as_str(),
                _ => {
                    // Could be a nested expression used as a function — treat as call
                    return Err(ParseError::at(0, format!(
                        "expected operator/form name, got {}", items[0]
                    )));
                }
            };

            match head {
                "let" => analyze_let(items),
                "for/fold" => analyze_for_fold(items),
                "for/each" => analyze_for_each(items),
                "cond" => analyze_cond(items),
                "when" => analyze_when(items),
                "range" => {
                    // If range has keyword args, treat as op call
                    let has_keywords = items[1..].iter().any(|s| {
                        matches!(s, Sexp::Atom(Atom::Symbol(sym)) if sym.starts_with(':'))
                    });
                    if has_keywords {
                        analyze_call("range", items)
                    } else {
                        analyze_range(items)
                    }
                }
                "ref" => analyze_ref(items),
                "set!" => analyze_set_bang(items),
                "make" => analyze_make(items),
                "list" => analyze_list_lit(items),
                _ => analyze_call(head, items),
            }
        }
    }
}

/// (let ([name expr] ...) body ...)
fn analyze_let(items: &[Sexp]) -> Result<Expr, ParseError> {
    if items.len() < 3 {
        return Err(ParseError::at(0, "let requires bindings and body"));
    }

    let binding_list = match &items[1] {
        Sexp::List(bs) => bs,
        _ => return Err(ParseError::at(0, "let: expected ([name expr] ...) bindings")),
    };

    let mut bindings = Vec::new();
    for b in binding_list {
        let pair = match b {
            Sexp::Bracket(items) if items.len() == 2 => items,
            Sexp::List(items) if items.len() == 2 => items,
            _ => return Err(ParseError::at(0, "let: each binding must be [name expr]")),
        };
        let name = match &pair[0] {
            Sexp::Atom(Atom::Symbol(s)) => s.clone(),
            _ => return Err(ParseError::at(0, "let: binding name must be a symbol")),
        };
        let expr = analyze_expr(&pair[1])?;
        bindings.push((name, Box::new(expr)));
    }

    let body: Result<Vec<Expr>, _> = items[2..].iter().map(analyze_expr).collect();

    Ok(Expr::Let { bindings, body: body? })
}

/// (for/fold ([acc init] ...) ([var seq]) body)
fn analyze_for_fold(items: &[Sexp]) -> Result<Expr, ParseError> {
    if items.len() < 4 {
        return Err(ParseError::at(0, "for/fold requires accumulators, iteration var, and body"));
    }

    // Accumulators: ([acc init] ...)
    let acc_list = match &items[1] {
        Sexp::List(bs) => bs,
        _ => return Err(ParseError::at(0, "for/fold: expected ([acc init] ...) accumulators")),
    };

    let mut accumulators = Vec::new();
    for a in acc_list {
        let pair = match a {
            Sexp::Bracket(items) if items.len() == 2 => items,
            Sexp::List(items) if items.len() == 2 => items,
            _ => return Err(ParseError::at(0, "for/fold: each accumulator must be [name init]")),
        };
        let name = match &pair[0] {
            Sexp::Atom(Atom::Symbol(s)) => s.clone(),
            _ => return Err(ParseError::at(0, "for/fold: accumulator name must be a symbol")),
        };
        let init = analyze_expr(&pair[1])?;
        accumulators.push((name, Box::new(init)));
    }

    // Iteration variable: ([var seq])
    let iter_list = match &items[2] {
        Sexp::List(bs) if bs.len() == 1 => bs,
        _ => return Err(ParseError::at(0, "for/fold: expected ([var seq]) iteration clause")),
    };

    let iter_pair = match &iter_list[0] {
        Sexp::Bracket(items) if items.len() == 2 => items,
        Sexp::List(items) if items.len() == 2 => items,
        _ => return Err(ParseError::at(0, "for/fold: iteration clause must be [var seq]")),
    };

    let var = match &iter_pair[0] {
        Sexp::Atom(Atom::Symbol(s)) => s.clone(),
        _ => return Err(ParseError::at(0, "for/fold: iteration variable must be a symbol")),
    };
    let seq = analyze_expr(&iter_pair[1])?;

    let body: Result<Vec<Expr>, _> = items[3..].iter().map(analyze_expr).collect();

    Ok(Expr::ForFold {
        accumulators,
        var,
        seq: Box::new(seq),
        body: body?,
    })
}

/// (for/each ([var seq]) body ...)
fn analyze_for_each(items: &[Sexp]) -> Result<Expr, ParseError> {
    if items.len() < 3 {
        return Err(ParseError::at(0, "for/each requires iteration var and body"));
    }

    // Iteration variable: ([var seq])
    let iter_list = match &items[1] {
        Sexp::List(bs) if bs.len() == 1 => bs,
        _ => return Err(ParseError::at(0, "for/each: expected ([var seq]) iteration clause")),
    };

    let iter_pair = match &iter_list[0] {
        Sexp::Bracket(items) if items.len() == 2 => items,
        Sexp::List(items) if items.len() == 2 => items,
        _ => return Err(ParseError::at(0, "for/each: iteration clause must be [var seq]")),
    };

    let var = match &iter_pair[0] {
        Sexp::Atom(Atom::Symbol(s)) => s.clone(),
        _ => return Err(ParseError::at(0, "for/each: variable must be a symbol")),
    };
    let seq = analyze_expr(&iter_pair[1])?;

    let body: Result<Vec<Expr>, _> = items[2..].iter().map(analyze_expr).collect();

    Ok(Expr::ForEach {
        var,
        seq: Box::new(seq),
        body: body?,
    })
}

/// (cond [test then] ... [else val])
fn analyze_cond(items: &[Sexp]) -> Result<Expr, ParseError> {
    if items.len() < 2 {
        return Err(ParseError::at(0, "cond requires at least one clause"));
    }

    let mut clauses = Vec::new();
    let mut else_expr = None;

    for item in &items[1..] {
        let clause = match item {
            Sexp::Bracket(items) => items,
            Sexp::List(items) => items,
            _ => return Err(ParseError::at(0, "cond: each clause must be [test then] or [else val]")),
        };

        if clause.len() != 2 {
            return Err(ParseError::at(0, "cond: each clause must have exactly 2 elements"));
        }

        // Check for else clause
        if let Sexp::Atom(Atom::Symbol(s)) = &clause[0] {
            if s == "else" {
                else_expr = Some(Box::new(analyze_expr(&clause[1])?));
                continue;
            }
        }

        let test = analyze_expr(&clause[0])?;
        let then = analyze_expr(&clause[1])?;
        clauses.push((test, then));
    }

    Ok(Expr::Cond { clauses, else_expr })
}

/// (when test body ...)
fn analyze_when(items: &[Sexp]) -> Result<Expr, ParseError> {
    if items.len() < 3 {
        return Err(ParseError::at(0, "when requires test and body"));
    }

    let test = analyze_expr(&items[1])?;
    let body: Result<Vec<Expr>, _> = items[2..].iter().map(analyze_expr).collect();

    Ok(Expr::When {
        test: Box::new(test),
        body: body?,
    })
}

/// (range start end)
fn analyze_range(items: &[Sexp]) -> Result<Expr, ParseError> {
    if items.len() != 3 {
        return Err(ParseError::at(0, "range requires exactly 2 arguments: start and end"));
    }
    let start = analyze_expr(&items[1])?;
    let end = analyze_expr(&items[2])?;
    Ok(Expr::Range { start: Box::new(start), end: Box::new(end) })
}

/// (ref coll i)
fn analyze_ref(items: &[Sexp]) -> Result<Expr, ParseError> {
    if items.len() != 3 {
        return Err(ParseError::at(0, "ref requires exactly 2 arguments: collection and index"));
    }
    let coll = analyze_expr(&items[1])?;
    let index = analyze_expr(&items[2])?;
    Ok(Expr::Ref { coll: Box::new(coll), index: Box::new(index) })
}

/// (set! coll i val)
fn analyze_set_bang(items: &[Sexp]) -> Result<Expr, ParseError> {
    if items.len() != 4 {
        return Err(ParseError::at(0, "set! requires exactly 3 arguments: collection, index, value"));
    }
    let coll = analyze_expr(&items[1])?;
    let index = analyze_expr(&items[2])?;
    let val = analyze_expr(&items[3])?;
    Ok(Expr::SetBang {
        coll: Box::new(coll),
        index: Box::new(index),
        val: Box::new(val),
    })
}

/// (make n init)
fn analyze_make(items: &[Sexp]) -> Result<Expr, ParseError> {
    if items.len() != 3 {
        return Err(ParseError::at(0, "make requires exactly 2 arguments: size and init"));
    }
    let size = analyze_expr(&items[1])?;
    let init = analyze_expr(&items[2])?;
    Ok(Expr::Make { size: Box::new(size), init: Box::new(init) })
}

/// (list expr ...)
fn analyze_list_lit(items: &[Sexp]) -> Result<Expr, ParseError> {
    let elems: Result<Vec<Expr>, _> = items[1..].iter().map(analyze_expr).collect();
    Ok(Expr::ListLit(elems?))
}

/// (op-name arg ...)
fn analyze_call(op: &str, items: &[Sexp]) -> Result<Expr, ParseError> {
    // Parse arguments, separating positional args from :keyword value pairs.
    // Positional args come first, then keyword args.
    // :each is special — it's a keyword with no value (a modifier flag).
    let tail = &items[1..];
    let mut args = Vec::new();
    let mut keywords: Vec<(String, Expr)> = Vec::new();
    let mut seen_keywords = std::collections::HashSet::new();
    let mut i = 0;

    while i < tail.len() {
        // Check if this is a keyword symbol (starts with ':')
        if let Sexp::Atom(Atom::Symbol(s)) = &tail[i] {
            if let Some(kw_name) = s.strip_prefix(':') {
                // Validate no duplicate keywords
                if !seen_keywords.insert(kw_name.to_string()) {
                    return Err(ParseError::at(0, format!(
                        "duplicate keyword :{} in call to {}", kw_name, op
                    )));
                }

                // :each is a flag — no value follows
                if kw_name == "each" {
                    keywords.push(("each".to_string(), Expr::Bool(true)));
                    i += 1;
                    continue;
                }

                // Other keywords require a value
                if i + 1 >= tail.len() {
                    return Err(ParseError::at(0, format!(
                        "keyword :{} requires a value in call to {}", kw_name, op
                    )));
                }

                let val = analyze_expr(&tail[i + 1])?;
                keywords.push((kw_name.to_string(), val));
                i += 2;
                continue;
            }
        }

        // Positional arg — but not allowed after keywords
        if !keywords.is_empty() {
            return Err(ParseError::at(0, format!(
                "positional argument after keyword argument in call to {}", op
            )));
        }

        args.push(analyze_expr(&tail[i])?);
        i += 1;
    }

    Ok(Expr::Call { op: op.to_string(), args, keywords })
}

/// Check that the body does not contain recursive calls to the plan name.
fn check_no_recursion(name: &str, exprs: &[Expr]) -> Result<(), ParseError> {
    for expr in exprs {
        check_expr_no_recursion(name, expr)?;
    }
    Ok(())
}

fn check_expr_no_recursion(name: &str, expr: &Expr) -> Result<(), ParseError> {
    match expr {
        Expr::Call { op, args, keywords } => {
            if op == name && (!args.is_empty() || !keywords.is_empty()) {
                return Err(ParseError::at(0, format!(
                    "recursion not allowed: '{}' cannot call itself", name
                )));
            }
            for arg in args {
                check_expr_no_recursion(name, arg)?;
            }
            for (_, val) in keywords {
                check_expr_no_recursion(name, val)?;
            }
        }
        Expr::Let { bindings, body } => {
            for (_, e) in bindings {
                check_expr_no_recursion(name, e)?;
            }
            check_no_recursion(name, body)?;
        }
        Expr::ForFold { accumulators, seq, body, .. } => {
            for (_, e) in accumulators {
                check_expr_no_recursion(name, e)?;
            }
            check_expr_no_recursion(name, seq)?;
            check_no_recursion(name, body)?;
        }
        Expr::ForEach { seq, body, .. } => {
            check_expr_no_recursion(name, seq)?;
            check_no_recursion(name, body)?;
        }
        Expr::Cond { clauses, else_expr } => {
            for (test, then) in clauses {
                check_expr_no_recursion(name, test)?;
                check_expr_no_recursion(name, then)?;
            }
            if let Some(e) = else_expr {
                check_expr_no_recursion(name, e)?;
            }
        }
        Expr::When { test, body } => {
            check_expr_no_recursion(name, test)?;
            check_no_recursion(name, body)?;
        }
        Expr::Range { start, end } => {
            check_expr_no_recursion(name, start)?;
            check_expr_no_recursion(name, end)?;
        }
        Expr::Ref { coll, index } => {
            check_expr_no_recursion(name, coll)?;
            check_expr_no_recursion(name, index)?;
        }
        Expr::SetBang { coll, index, val } => {
            check_expr_no_recursion(name, coll)?;
            check_expr_no_recursion(name, index)?;
            check_expr_no_recursion(name, val)?;
        }
        Expr::Make { size, init } => {
            check_expr_no_recursion(name, size)?;
            check_expr_no_recursion(name, init)?;
        }
        Expr::ListLit(elems) => {
            for e in elems {
                check_expr_no_recursion(name, e)?;
            }
        }
        // Atoms — no recursion possible
        Expr::Num(_) | Expr::Str(_) | Expr::Bool(_) | Expr::Var(_) => {}
    }
    Ok(())
}

// ═══════════════════════════════════════════════════════════════════════════
// Lowering: PlanAst → PlanDef + RawSteps
// ═══════════════════════════════════════════════════════════════════════════

use crate::plan::{PlanDef, PlanInput, RawStep, StepArgs, StepParam};

/// Lower a PlanAst into a PlanDef that the existing compiler understands.
pub fn lower_to_plan(ast: &PlanAst) -> Result<PlanDef, ParseError> {
    // Convert params to PlanInputs
    let inputs: Vec<PlanInput> = ast.params.iter()
        .map(|p| PlanInput::typed(&p.name, &p.type_expr))
        .collect();

    // Convert bindings: evaluate each bind expr to a Racket literal string
    let mut bindings = HashMap::new();
    for (name, expr) in &ast.bindings {
        bindings.insert(name.clone(), expr_to_racket_literal(expr));
    }

    // Lower body to steps
    let mut ctx = LowerCtx::new(&ast.params);
    for (i, expr) in ast.body.iter().enumerate() {
        let is_last = i == ast.body.len() - 1;
        lower_expr(expr, &mut ctx, is_last)?;
    }

    Ok(PlanDef {
        name: ast.name.clone(),
        inputs,
        output: Some(vec![ast.return_type.clone()]),
        steps: ctx.steps,
        bindings,
    })
}

/// Context for the lowering pass.
struct LowerCtx {
    /// Top-level steps accumulated so far
    steps: Vec<RawStep>,
    /// Track which variables are mutable vectors (created by `make`).
    /// Everything else (params of List type, etc.) is treated as a list.
    vectors: std::collections::HashSet<String>,
    /// Prefix for step references: "$step" at top level, "$body" inside bodies
    ref_prefix: String,
    /// Map from variable name → reference string ($step-N, $body-N, $param, etc.)
    env: HashMap<String, String>,
    /// Current nesting depth (0 = top-level steps, >0 = inside body)
    depth: usize,
}

impl LowerCtx {
    fn new(params: &[Param]) -> Self {
        let mut env = HashMap::new();
        for p in params {
            env.insert(p.name.clone(), format!("${}", p.name));
        }
        Self {
            steps: Vec::new(),
            vectors: std::collections::HashSet::new(),
            ref_prefix: "$step".to_string(),
            env, depth: 0,
        }
    }

    fn push_step(&mut self, step: RawStep) -> String {
        self.steps.push(step);
        let n = self.steps.len();
        format!("{}-{}", self.ref_prefix, n)
    }

    /// Create a child context for a body (for_each, fold, when_do).
    /// Inherits the env but uses $body-N references for new steps.
    fn child(&self) -> Self {
        Self {
            steps: Vec::new(), ref_prefix: "$body".to_string(),
            vectors: self.vectors.clone(),
            env: self.env.clone(), depth: self.depth + 1,
        }
    }
}

/// Lower an expression, appending steps to ctx.
/// Returns the reference string for the result (e.g. "$step-3", "$body-2", "42").
fn lower_expr(expr: &Expr, ctx: &mut LowerCtx, _is_tail: bool) -> Result<String, ParseError> {
    match expr {
        Expr::Num(n) => Ok(n.clone()),
        Expr::Str(s) => Ok(format!("\"{}\"", s)),
        Expr::Bool(b) => Ok(if *b { "#t".to_string() } else { "#f".to_string() }),
        Expr::Var(name) => {
            if let Some(r) = ctx.env.get(name) {
                Ok(r.clone())
            } else if let Some(bare) = name.strip_prefix('$') {
                // $var references: look up bare name, pass through as $var
                if ctx.env.contains_key(bare) || bare.starts_with("step-") || bare.starts_with("body-") {
                    Ok(name.clone())
                } else {
                    Err(ParseError::at(0, format!("undefined variable: '{}'", name)))
                }
            } else {
                Err(ParseError::at(0, format!("undefined variable: '{}'", name)))
            }
        }

        Expr::Let { bindings, body } => {
            // Sequential let: each binding becomes a step, and we record the
            // variable → $step-N mapping
            for (name, init_expr) in bindings {
                let val = lower_expr(init_expr, ctx, false)?;
                // If the value is already a step reference or a simple value,
                // we need to wrap it in a step so it gets a $step-N binding
                let step_ref = emit_value_step(&val, ctx);
                // Track if this binding is a mutable vector (from `make`)
                if matches!(init_expr.as_ref(), Expr::Make { .. }) {
                    ctx.vectors.insert(name.clone());
                }
                ctx.env.insert(name.clone(), step_ref);
            }
            // Lower body expressions; return the last one's reference
            let mut last_ref = String::new();
            for (i, expr) in body.iter().enumerate() {
                let is_last = i == body.len() - 1;
                last_ref = lower_expr(expr, ctx, is_last)?;
            }
            Ok(last_ref)
        }

        Expr::Range { start, end } => {
            let s = lower_expr(start, ctx, false)?;
            let e = lower_expr(end, ctx, false)?;
            let mut params = HashMap::new();
            params.insert("start".to_string(), StepParam::Value(s));
            params.insert("end".to_string(), StepParam::Value(e));
            let step = RawStep { op: "range".to_string(), args: StepArgs::Map(params) };
            Ok(ctx.push_step(step))
        }

        Expr::Make { size, init } => {
            let sz = lower_expr(size, ctx, false)?;
            let ini = lower_expr(init, ctx, false)?;
            let mut params = HashMap::new();
            params.insert("size".to_string(), StepParam::Value(sz));
            params.insert("init".to_string(), StepParam::Value(ini));
            let step = RawStep { op: "make_vector".to_string(), args: StepArgs::Map(params) };
            Ok(ctx.push_step(step))
        }

        Expr::Ref { coll, index } => {
            let c = lower_expr(coll, ctx, false)?;
            let i = lower_expr(index, ctx, false)?;
            // Determine if this is a vector-ref or list-ref based on the collection
            let is_vector = match coll.as_ref() {
                Expr::Var(name) => ctx.vectors.contains(name),
                _ => false,
            };
            let op = if is_vector { "vector_ref" } else { "list_ref" };
            let mut params = HashMap::new();
            params.insert(if is_vector { "v" } else { "x" }.to_string(), StepParam::Value(c));
            params.insert(if is_vector { "i" } else { "y" }.to_string(), StepParam::Value(i));
            let step = RawStep { op: op.to_string(), args: StepArgs::Map(params) };
            Ok(ctx.push_step(step))
        }

        Expr::SetBang { coll, index, val } => {
            let c = lower_expr(coll, ctx, false)?;
            let i = lower_expr(index, ctx, false)?;
            let v = lower_expr(val, ctx, false)?;
            let mut params = HashMap::new();
            params.insert("v".to_string(), StepParam::Value(c));
            params.insert("i".to_string(), StepParam::Value(i));
            params.insert("val".to_string(), StepParam::Value(v));
            let step = RawStep { op: "vector_set".to_string(), args: StepArgs::Map(params) };
            Ok(ctx.push_step(step))
        }

        Expr::ListLit(elems) => {
            let parts: Result<Vec<String>, _> = elems.iter()
                .map(|e| lower_expr(e, ctx, false))
                .collect();
            let val = parts?.join(" ");
            let step = RawStep { op: "list_new".to_string(), args: StepArgs::Scalar(val) };
            Ok(ctx.push_step(step))
        }

        Expr::Call { op, args, keywords } => {
            lower_call(op, args, keywords, ctx)
        }

        Expr::ForFold { accumulators, var, seq, body } => {
            lower_for_fold(accumulators, var, seq, body, ctx)
        }

        Expr::ForEach { var, seq, body } => {
            lower_for_each(var, seq, body, ctx)
        }

        Expr::Cond { clauses, else_expr } => {
            lower_cond(clauses, else_expr.as_deref(), ctx)
        }

        Expr::When { test, body } => {
            lower_when(test, body, ctx)
        }
    }
}

/// Lower an expression to a raw string value (no Racket quoting).
/// Used for keyword argument values where the plan compiler expects unquoted strings.
fn lower_expr_raw(expr: &Expr, ctx: &mut LowerCtx) -> Result<String, ParseError> {
    match expr {
        Expr::Str(s) => Ok(s.clone()), // Raw string, no quotes
        other => lower_expr(other, ctx, false),
    }
}

/// Emit a step that just passes through a value, returning its $step-N ref.
/// If the value is already a $step-N reference, return it as-is (no extra step).
fn emit_value_step(val: &str, _ctx: &mut LowerCtx) -> String {
    if val.starts_with("$step-") || val.starts_with("$body-") {
        return val.to_string();
    }
    // For simple values, we need to wrap them. Use a no-op identity step.
    // Actually, for the plan compiler, we can just track the mapping directly.
    // The $step-N reference is what matters — if the init expression already
    // produced a step, we use that. Otherwise we need to emit one.
    //
    // For literals and $param refs, we don't need a step — we can reference
    // them directly. The env mapping handles this.
    val.to_string()
}

/// Lower an op call to a step.
fn lower_call(op: &str, args: &[Expr], keywords: &[(String, Expr)], ctx: &mut LowerCtx) -> Result<String, ParseError> {
    // Map common Scheme operators to Cadmus op names
    let cadmus_op = match op {
        "+" => "add",
        "-" => "subtract",
        "*" => "multiply",
        "/" => "divide",
        "=" => "equal",
        "<" => "less_than",
        ">" => "greater_than",
        "<=" => "less_than_or_equal",
        ">=" => "greater_than_or_equal",
        "modulo" => "modulo",
        "remainder" => "remainder",
        "not" => "not",
        "and" => "and_logic",
        "or" => "or_logic",
        "list-ref" => "list_ref",
        "string-length" => "string_length",
        "string-ref" => "string_ref",
        "vector->list" => "vector_to_list",
        other => other,
    };

    // Lower positional arguments (raw — no Racket quoting for plan IR values)
    let lowered_args: Vec<String> = args.iter()
        .map(|a| lower_expr_raw(a, ctx))
        .collect::<Result<_, _>>()?;

    // Check for :each modifier (keyword with no real value)
    let has_each = keywords.iter().any(|(k, _)| k == "each");
    // Collect non-each keywords
    let real_keywords: Vec<&(String, Expr)> = keywords.iter()
        .filter(|(k, _)| k != "each")
        .collect();

    // Build the step
    let step = if !real_keywords.is_empty() {
        // Keyword args → StepArgs::Map
        let mut params = HashMap::new();

        // Add positional args with x, y, z names
        let param_names = ["x", "y", "z", "w"];
        for (i, arg) in lowered_args.iter().enumerate() {
            let key = if i < param_names.len() {
                param_names[i].to_string()
            } else {
                format!("arg{}", i)
            };
            params.insert(key, StepParam::Value(arg.clone()));
        }

        // Add keyword args
        for (kw, val_expr) in &real_keywords {
            let val = lower_expr_raw(val_expr, ctx)?;
            params.insert(kw.to_string(), StepParam::Value(val));
        }

        RawStep { op: cadmus_op.to_string(), args: StepArgs::Map(params) }
    } else if has_each {
        // :each only → StepArgs::Scalar("each")
        RawStep { op: cadmus_op.to_string(), args: StepArgs::Scalar("each".to_string()) }
    } else {
        // Pure positional args (original behavior)
        match lowered_args.len() {
            0 => RawStep { op: cadmus_op.to_string(), args: StepArgs::None },
            1 => RawStep { op: cadmus_op.to_string(), args: StepArgs::Scalar(lowered_args[0].clone()) },
            _ => {
                let mut params = HashMap::new();
                let param_names = ["x", "y", "z", "w"];
                for (i, arg) in lowered_args.iter().enumerate() {
                    let key = if i < param_names.len() {
                        param_names[i].to_string()
                    } else {
                        format!("arg{}", i)
                    };
                    params.insert(key, StepParam::Value(arg.clone()));
                }
                RawStep { op: cadmus_op.to_string(), args: StepArgs::Map(params) }
            }
        }
    };

    Ok(ctx.push_step(step))
}

/// Lower for/fold to a fold step with sub-steps.
fn lower_for_fold(
    accumulators: &[(String, Box<Expr>)],
    var: &str,
    seq: &Expr,
    body: &[Expr],
    ctx: &mut LowerCtx,
) -> Result<String, ParseError> {
    // Lower the sequence expression
    let seq_ref = lower_expr(seq, ctx, false)?;

    // For now, support single accumulator (the common case)
    if accumulators.len() != 1 {
        return Err(ParseError::at(0, "for/fold currently supports exactly one accumulator"));
    }
    let (acc_name, acc_init) = &accumulators[0];
    let init_val = lower_expr(acc_init, ctx, false)?;

    // Lower body into sub-steps
    let mut body_ctx = ctx.child();
    // Add the iteration variable and accumulator
    body_ctx.env.insert(var.to_string(), format!("${}", var));
    body_ctx.env.insert(acc_name.clone(), format!("${}", acc_name));

    for (i, expr) in body.iter().enumerate() {
        let _is_last = i == body.len() - 1;
        lower_expr(expr, &mut body_ctx, _is_last)?;
    }

    // Build the fold step
    let mut params = HashMap::new();
    params.insert("acc".to_string(), StepParam::Value(acc_name.clone()));
    params.insert("init".to_string(), StepParam::Value(init_val));
    params.insert("var".to_string(), StepParam::Value(var.to_string()));
    params.insert("over".to_string(), StepParam::Value(seq_ref));

    if !body_ctx.steps.is_empty() {
        params.insert("body".to_string(), StepParam::Steps(body_ctx.steps));
    }

    let step = RawStep { op: "fold".to_string(), args: StepArgs::Map(params) };
    Ok(ctx.push_step(step))
}

/// Lower for/each to a for_each step with sub-steps.
fn lower_for_each(
    var: &str,
    seq: &Expr,
    body: &[Expr],
    ctx: &mut LowerCtx,
) -> Result<String, ParseError> {
    // Lower the sequence expression
    let seq_ref = lower_expr(seq, ctx, false)?;

    // Lower body into sub-steps
    let mut body_ctx = ctx.child();
    body_ctx.env.insert(var.to_string(), format!("${}", var));

    for (i, expr) in body.iter().enumerate() {
        let _is_last = i == body.len() - 1;
        lower_expr(expr, &mut body_ctx, _is_last)?;
    }

    let mut params = HashMap::new();
    params.insert("var".to_string(), StepParam::Value(var.to_string()));
    params.insert("over".to_string(), StepParam::Value(seq_ref));

    if !body_ctx.steps.is_empty() {
        params.insert("body".to_string(), StepParam::Steps(body_ctx.steps));
    }

    let step = RawStep { op: "for_each".to_string(), args: StepArgs::Map(params) };
    Ok(ctx.push_step(step))
}

/// Lower cond to a cond step with clause params.
fn lower_cond(
    clauses: &[(Expr, Expr)],
    else_expr: Option<&Expr>,
    ctx: &mut LowerCtx,
) -> Result<String, ParseError> {
    // Build YAML-compatible clause structure
    let mut yaml_clauses = Vec::new();

    for (test, then) in clauses {
        let test_val = lower_expr_to_yaml(test, ctx)?;
        let then_val = lower_expr_to_yaml(then, ctx)?;

        let mut clause = serde_yaml::Mapping::new();
        clause.insert(
            serde_yaml::Value::String("test".to_string()),
            test_val,
        );
        clause.insert(
            serde_yaml::Value::String("then".to_string()),
            then_val,
        );
        yaml_clauses.push(serde_yaml::Value::Mapping(clause));
    }

    if let Some(else_e) = else_expr {
        let else_val = lower_expr_to_yaml(else_e, ctx)?;
        let mut clause = serde_yaml::Mapping::new();
        clause.insert(
            serde_yaml::Value::String("else".to_string()),
            else_val,
        );
        yaml_clauses.push(serde_yaml::Value::Mapping(clause));
    }

    let mut params = HashMap::new();
    params.insert("clauses".to_string(), StepParam::Clauses(yaml_clauses));

    let step = RawStep { op: "cond".to_string(), args: StepArgs::Map(params) };
    Ok(ctx.push_step(step))
}

/// Lower when to a when_do step with sub-steps.
fn lower_when(
    test: &Expr,
    body: &[Expr],
    ctx: &mut LowerCtx,
) -> Result<String, ParseError> {
    let test_ref = lower_expr(test, ctx, false)?;

    // Lower body into sub-steps
    let mut body_ctx = ctx.child();

    for (i, expr) in body.iter().enumerate() {
        let _is_last = i == body.len() - 1;
        lower_expr(expr, &mut body_ctx, _is_last)?;
    }

    let mut params = HashMap::new();
    params.insert("test".to_string(), StepParam::Value(test_ref));

    if !body_ctx.steps.is_empty() {
        params.insert("body".to_string(), StepParam::Steps(body_ctx.steps));
    }

    let step = RawStep { op: "when_do".to_string(), args: StepArgs::Map(params) };
    Ok(ctx.push_step(step))
}

/// Convert an Expr to a serde_yaml::Value for use in cond clauses.
fn lower_expr_to_yaml(expr: &Expr, ctx: &mut LowerCtx) -> Result<serde_yaml::Value, ParseError> {
    match expr {
        Expr::Num(n) => Ok(serde_yaml::Value::String(n.clone())),
        Expr::Str(s) => Ok(serde_yaml::Value::String(format!("\"{}\"", s))),
        Expr::Bool(b) => Ok(serde_yaml::Value::String(if *b { "#t".to_string() } else { "#f".to_string() })),
        Expr::Var(name) => {
            if let Some(r) = ctx.env.get(name) {
                Ok(serde_yaml::Value::String(r.clone()))
            } else {
                Err(ParseError::at(0, format!("undefined variable in cond: '{}'", name)))
            }
        }
        Expr::Call { op, args, .. } => {
            // For cond clause expressions, emit as inline YAML mapping
            let cadmus_op = match op.as_str() {
                "+" => "add",
                "-" => "subtract",
                "*" => "multiply",
                "/" => "divide",
                "=" => "equal",
                "<" => "less_than",
                ">" => "greater_than",
                "<=" => "less_than_or_equal",
                ">=" => "greater_than_or_equal",
                other => other,
            };

            let lowered_args: Result<Vec<serde_yaml::Value>, _> = args.iter()
                .map(|a| lower_expr_to_yaml(a, ctx))
                .collect();
            let lowered_args = lowered_args?;

            let mut mapping = serde_yaml::Mapping::new();
            let param_names = ["x", "y", "z", "w"];
            for (i, arg) in lowered_args.iter().enumerate() {
                let key = if i < param_names.len() {
                    param_names[i].to_string()
                } else {
                    format!("arg{}", i)
                };
                mapping.insert(
                    serde_yaml::Value::String(key),
                    arg.clone(),
                );
            }

            let mut outer = serde_yaml::Mapping::new();
            outer.insert(
                serde_yaml::Value::String(cadmus_op.to_string()),
                serde_yaml::Value::Mapping(mapping),
            );
            Ok(serde_yaml::Value::Mapping(outer))
        }
        _ => {
            // For complex expressions in cond, lower to a step and reference it
            let ref_str = lower_expr(expr, ctx, false)?;
            Ok(serde_yaml::Value::String(ref_str))
        }
    }
}

/// Convert an Expr to a Racket literal string (for bind values).
fn expr_to_racket_literal(expr: &Expr) -> String {
    match expr {
        Expr::Num(n) => n.clone(),
        Expr::Str(s) => format!("\"{}\"", s),
        Expr::Bool(b) => if *b { "#t".to_string() } else { "#f".to_string() },
        Expr::Var(name) => name.clone(),
        Expr::ListLit(elems) => {
            let parts: Vec<String> = elems.iter().map(expr_to_racket_literal).collect();
            format!("(list {})", parts.join(" "))
        }
        Expr::Call { op, args, .. } => {
            let parts: Vec<String> = args.iter().map(expr_to_racket_literal).collect();
            format!("({} {})", op, parts.join(" "))
        }
        _ => format!("{:?}", expr), // fallback — shouldn't happen for bind values
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Public convenience: parse + lower in one step
// ═══════════════════════════════════════════════════════════════════════════

/// Parse a sexpr source string and lower it to a PlanDef.
pub fn parse_sexpr_to_plan(src: &str) -> Result<PlanDef, ParseError> {
    let ast = parse_plan_sexpr(src)?;
    lower_to_plan(&ast)
}

/// Infer the type string for a PlanInput when serializing to sexpr.
/// Uses the explicit type_hint if present, otherwise infers from the input name
/// to match what the plan compiler's `infer_input_type()` would produce.
/// Returns the type in sexpr format: (Dir Bytes) not Dir(Bytes).
fn infer_type_for_serialization(input: &crate::plan::PlanInput) -> String {
    if let Some(hint) = &input.type_hint {
        return type_str_to_sexpr(hint);
    }
    // Mirror the name-based inference from plan.rs::infer_input_type()
    let name = input.name.to_lowercase();
    match name.as_str() {
        "x" | "y" | "a" | "b" | "n" | "m" | "left" | "right" => "Number".to_string(),
        "pathref" | "target" => "Path".to_string(),
        "url" => "URL".to_string(),
        "repo" => "Repo".to_string(),
        "textdir" | "text_dir" => "(Dir (File Text))".to_string(),
        "file" | "logfile" | "archive" => "(File Bytes)".to_string(),
        _ if name.contains("dir") || name == "path" || name == "source" || name == "dest" => "(Dir Bytes)".to_string(),
        _ if name.contains("url") => "URL".to_string(),
        _ if name.contains("pattern") || name.contains("keyword") || name.contains("query") || name.contains("search") => "Pattern".to_string(),
        _ if name.contains("size") => "Size".to_string(),
        _ if name.contains("repository") => "Repo".to_string(),
        _ => "Bytes".to_string(),
    }
}

/// Convert a Cadmus type string like "Dir(Bytes)" to sexpr format "(Dir Bytes)".
fn type_str_to_sexpr(s: &str) -> String {
    // Simple types with no parens pass through
    if !s.contains('(') {
        return s.to_string();
    }
    // Parse as TypeExpr and re-emit in sexpr format
    if let Ok(te) = crate::type_expr::TypeExpr::parse(s) {
        return type_expr_to_sexpr(&te);
    }
    // Fallback: return as-is
    s.to_string()
}

/// Convert a TypeExpr to sexpr format string.
fn type_expr_to_sexpr(te: &crate::type_expr::TypeExpr) -> String {
    use crate::type_expr::TypeExpr;
    match te {
        TypeExpr::Primitive(name) | TypeExpr::Var(name) => name.clone(),
        TypeExpr::Constructor(name, args) if args.is_empty() => name.clone(),
        TypeExpr::Constructor(name, args) => {
            let inner: Vec<String> = args.iter().map(type_expr_to_sexpr).collect();
            format!("({} {})", name, inner.join(" "))
        }
    }
}

/// Serialize a PlanDef to sexpr string for display.
///
/// Handles simple pipeline plans (the NL layer output). Complex algorithm
/// plans with nested body/clauses are loaded from .sexp files directly.
pub fn plan_to_sexpr(plan: &PlanDef) -> String {
    let mut out = String::new();

    // Signature: (define (name (param : Type) ...) : ReturnType
    out.push_str("(define (");
    out.push_str(&plan.name);
    for input in &plan.inputs {
        out.push_str(" (");
        out.push_str(&input.name);
        out.push_str(" : ");
        out.push_str(&infer_type_for_serialization(input));
        out.push(')');
    }
    out.push(')');

    // Return type (if declared)
    if let Some(output) = &plan.output {
        if let Some(first) = output.first() {
            out.push_str(" : ");
            out.push_str(&type_str_to_sexpr(first));
        }
    }

    // Bindings
    let mut sorted_bindings: Vec<_> = plan.bindings.iter().collect();
    sorted_bindings.sort_by_key(|(k, _)| k.as_str());
    for (name, value) in &sorted_bindings {
        out.push_str("\n  (bind ");
        out.push_str(name);
        out.push(' ');
        // If value looks like a Racket expression (starts with '('), emit as-is
        if value.starts_with('(') || value.parse::<f64>().is_ok() {
            out.push_str(value);
        } else {
            out.push('"');
            out.push_str(value);
            out.push('"');
        }
        out.push(')');
    }

    // Steps
    for step in &plan.steps {
        // Self-referential step: op matches plan name with $var keyword args.
        // Emit (bind param value) lines + bare (op) to avoid recursion-check.
        if step.op == plan.name {
            if let StepArgs::Map(ref m) = step.args {
                let all_var_refs = m.values().all(|p| {
                    matches!(p, StepParam::Value(v) if v.starts_with('$'))
                });
                if all_var_refs && !m.is_empty() {
                    // Already emitted as bindings above or input defaults;
                    // just emit the bare op call.
                    out.push_str("\n  (");
                    out.push_str(&step.op);
                    out.push(')');
                    continue;
                }
            }
            if let StepArgs::None = step.args {
                out.push_str("\n  (");
                out.push_str(&step.op);
                out.push(')');
                continue;
            }
        }
        out.push_str("\n  ");
        format_step(step, &mut out);
    }

    out.push(')');
    out.push('\n');
    out
}

/// Format a single step as sexpr.
fn format_step(step: &RawStep, out: &mut String) {
    match &step.args {
        StepArgs::None => {
            // (op_name)
            out.push('(');
            out.push_str(&step.op);
            out.push(')');
        }
        StepArgs::Scalar(s) if s == "each" => {
            // (op_name :each)
            out.push('(');
            out.push_str(&step.op);
            out.push_str(" :each)");
        }
        StepArgs::Scalar(s) => {
            // (op_name "value")
            out.push('(');
            out.push_str(&step.op);
            out.push_str(" \"");
            out.push_str(s);
            out.push_str("\")");
        }
        StepArgs::Map(m) => {
            out.push('(');
            out.push_str(&step.op);
            let mut sorted: Vec<_> = m.iter().collect();
            sorted.sort_by_key(|(k, _)| k.as_str());
            for (key, param) in sorted {
                match param {
                    StepParam::Value(v) => {
                        out.push_str(" :");
                        out.push_str(key);
                        out.push(' ');
                        // Emit numbers and $refs unquoted, strings quoted
                        if v.starts_with('$') || v.parse::<f64>().is_ok()
                            || v == "#t" || v == "#f"
                        {
                            out.push_str(v);
                        } else {
                            out.push('"');
                            out.push_str(v);
                            out.push('"');
                        }
                    }
                    StepParam::Steps(steps) => {
                        // Sub-step body — emit as nested forms
                        out.push_str(" :");
                        out.push_str(key);
                        out.push_str(" (");
                        for (i, sub) in steps.iter().enumerate() {
                            if i > 0 { out.push(' '); }
                            format_step(sub, out);
                        }
                        out.push(')');
                    }
                    StepParam::Inline(inner) => {
                        out.push_str(" :");
                        out.push_str(key);
                        out.push(' ');
                        format_step(inner, out);
                    }
                    StepParam::Clauses(clauses) => {
                        out.push_str(" :clauses (");
                        for (i, c) in clauses.iter().enumerate() {
                            if i > 0 { out.push(' '); }
                            out.push_str(&format!("{}", serde_yaml::to_string(c).unwrap_or_default()).trim());
                        }
                        out.push(')');
                    }
                }
            }
            out.push(')');
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Tests
// ═══════════════════════════════════════════════════════════════════════════

#[cfg(test)]
mod tests {
    use super::*;

    // -----------------------------------------------------------------------
    // Tokenizer tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_tokenize_simple() {
        let tokens = tokenize("(+ 1 2)").unwrap();
        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[0].value, Token::LParen);
        assert_eq!(tokens[1].value, Token::Symbol("+".to_string()));
        assert_eq!(tokens[2].value, Token::Number("1".to_string()));
        assert_eq!(tokens[3].value, Token::Number("2".to_string()));
        assert_eq!(tokens[4].value, Token::RParen);
    }

    #[test]
    fn test_tokenize_brackets() {
        let tokens = tokenize("[x 10]").unwrap();
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0].value, Token::LBracket);
        assert_eq!(tokens[3].value, Token::RBracket);
    }

    #[test]
    fn test_tokenize_string() {
        let tokens = tokenize("\"hello world\"").unwrap();
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].value, Token::Str("hello world".to_string()));
    }

    #[test]
    fn test_tokenize_booleans() {
        let tokens = tokenize("#t #f").unwrap();
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].value, Token::Bool(true));
        assert_eq!(tokens[1].value, Token::Bool(false));
    }

    #[test]
    fn test_tokenize_colon() {
        let tokens = tokenize("(n : Number)").unwrap();
        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[2].value, Token::Colon);
    }

    #[test]
    fn test_tokenize_comments() {
        let tokens = tokenize(";; this is a comment\n(+ 1 2)").unwrap();
        assert_eq!(tokens.len(), 5); // comment stripped
    }

    #[test]
    fn test_tokenize_negative_number() {
        let tokens = tokenize("-42 +inf.0 3.14").unwrap();
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[0].value, Token::Number("-42".to_string()));
        assert_eq!(tokens[1].value, Token::Number("+inf.0".to_string()));
        assert_eq!(tokens[2].value, Token::Number("3.14".to_string()));
    }

    #[test]
    fn test_tokenize_unterminated_string() {
        let err = tokenize("\"hello").unwrap_err();
        assert!(err.message.contains("unterminated"));
    }

    #[test]
    fn test_tokenize_operators() {
        let tokens = tokenize("+ - * / = < > <= >=").unwrap();
        assert_eq!(tokens.len(), 9);
        for t in &tokens {
            assert!(matches!(t.value, Token::Symbol(_)));
        }
    }

    #[test]
    fn test_tokenize_set_bang() {
        let tokens = tokenize("set!").unwrap();
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].value, Token::Symbol("set!".to_string()));
    }

    // -----------------------------------------------------------------------
    // S-expression parser tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_parse_sexp_atom() {
        let tokens = tokenize("42").unwrap();
        let sexp = parse_sexp(&tokens).unwrap();
        assert_eq!(sexp, Sexp::Atom(Atom::Number("42".to_string())));
    }

    #[test]
    fn test_parse_sexp_list() {
        let tokens = tokenize("(+ 1 2)").unwrap();
        let sexp = parse_sexp(&tokens).unwrap();
        assert_eq!(sexp, Sexp::List(vec![
            Sexp::Atom(Atom::Symbol("+".to_string())),
            Sexp::Atom(Atom::Number("1".to_string())),
            Sexp::Atom(Atom::Number("2".to_string())),
        ]));
    }

    #[test]
    fn test_parse_sexp_nested() {
        let tokens = tokenize("(+ (* 2 3) 4)").unwrap();
        let sexp = parse_sexp(&tokens).unwrap();
        match sexp {
            Sexp::List(items) => {
                assert_eq!(items.len(), 3);
                assert!(matches!(&items[1], Sexp::List(_)));
            }
            _ => panic!("expected list"),
        }
    }

    #[test]
    fn test_parse_sexp_brackets() {
        let tokens = tokenize("([x 1] [y 2])").unwrap();
        let sexp = parse_sexp(&tokens).unwrap();
        match sexp {
            Sexp::List(items) => {
                assert_eq!(items.len(), 2);
                assert!(matches!(&items[0], Sexp::Bracket(_)));
            }
            _ => panic!("expected list"),
        }
    }

    #[test]
    fn test_parse_sexp_unbalanced_paren() {
        let tokens = tokenize("(+ 1 2").unwrap();
        let err = parse_sexp(&tokens).unwrap_err();
        assert!(err.message.contains("unbalanced"));
    }

    #[test]
    fn test_parse_sexp_unexpected_rparen() {
        let tokens = tokenize(")").unwrap();
        let err = parse_sexp(&tokens).unwrap_err();
        assert!(err.message.contains("unexpected ')'"));
    }

    #[test]
    fn test_parse_sexp_extra_tokens() {
        let tokens = tokenize("(+ 1 2) 3").unwrap();
        let err = parse_sexp(&tokens).unwrap_err();
        assert!(err.message.contains("unexpected token after"));
    }

    // -----------------------------------------------------------------------
    // PlanAst analysis tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_analyze_simple_define() {
        let src = r#"
            (define (double (x : Number)) : Number
              (* x 2))
        "#;
        let ast = parse_plan_sexpr(src).unwrap();
        assert_eq!(ast.name, "double");
        assert_eq!(ast.params.len(), 1);
        assert_eq!(ast.params[0].name, "x");
        assert_eq!(ast.params[0].type_expr, "Number");
        assert_eq!(ast.return_type, "Number");
        assert_eq!(ast.body.len(), 1);
    }

    #[test]
    fn test_analyze_with_bindings() {
        let src = r#"
            (define (factorial (n : Number)) : Number
              (bind n 10)
              (* n 2))
        "#;
        let ast = parse_plan_sexpr(src).unwrap();
        assert_eq!(ast.bindings.len(), 1);
        assert_eq!(ast.bindings[0].0, "n");
        assert_eq!(ast.body.len(), 1);
    }

    #[test]
    fn test_analyze_list_type() {
        let src = r#"
            (define (lis (lst : (List Number))) : Number
              (length lst))
        "#;
        let ast = parse_plan_sexpr(src).unwrap();
        assert_eq!(ast.params[0].type_expr, "List(Number)");
    }

    #[test]
    fn test_analyze_let() {
        let src = r#"
            (define (test (n : Number)) : Number
              (let ([a (+ n 1)]
                    [b (+ n 2)])
                (+ a b)))
        "#;
        let ast = parse_plan_sexpr(src).unwrap();
        match &ast.body[0] {
            Expr::Let { bindings, body } => {
                assert_eq!(bindings.len(), 2);
                assert_eq!(bindings[0].0, "a");
                assert_eq!(bindings[1].0, "b");
                assert_eq!(body.len(), 1);
            }
            _ => panic!("expected Let"),
        }
    }

    #[test]
    fn test_analyze_for_fold() {
        let src = r#"
            (define (sum_range (n : Number)) : Number
              (for/fold ([acc 0]) ([i (range 1 n)])
                (+ acc i)))
        "#;
        let ast = parse_plan_sexpr(src).unwrap();
        match &ast.body[0] {
            Expr::ForFold { accumulators, var, .. } => {
                assert_eq!(accumulators.len(), 1);
                assert_eq!(accumulators[0].0, "acc");
                assert_eq!(var, "i");
            }
            _ => panic!("expected ForFold"),
        }
    }

    #[test]
    fn test_analyze_for_each() {
        let src = r#"
            (define (fill (n : Number)) : Number
              (for/each ([i (range 0 n)])
                (set! arr i 0)))
        "#;
        let ast = parse_plan_sexpr(src).unwrap();
        match &ast.body[0] {
            Expr::ForEach { var, .. } => {
                assert_eq!(var, "i");
            }
            _ => panic!("expected ForEach"),
        }
    }

    #[test]
    fn test_analyze_cond() {
        let src = r#"
            (define (abs (x : Number)) : Number
              (cond
                [(< x 0) (- 0 x)]
                [else x]))
        "#;
        let ast = parse_plan_sexpr(src).unwrap();
        match &ast.body[0] {
            Expr::Cond { clauses, else_expr } => {
                assert_eq!(clauses.len(), 1);
                assert!(else_expr.is_some());
            }
            _ => panic!("expected Cond"),
        }
    }

    #[test]
    fn test_analyze_when() {
        let src = r#"
            (define (maybe_set (n : Number)) : Number
              (when (> n 0)
                (set! arr 0 n)))
        "#;
        let ast = parse_plan_sexpr(src).unwrap();
        assert!(matches!(&ast.body[0], Expr::When { .. }));
    }

    #[test]
    fn test_analyze_nested_let_for_when() {
        let src = r#"
            (define (complex (n : Number)) : Number
              (let ([dp (make n 1)])
                (for/each ([i (range 1 n)])
                  (when (> i 0)
                    (let ([cur (ref dp i)])
                      (set! dp i (+ cur 1)))))
                (ref dp 0)))
        "#;
        let ast = parse_plan_sexpr(src).unwrap();
        // Should parse without error — nested let > for/each > when > let > set!
        match &ast.body[0] {
            Expr::Let { body, .. } => {
                assert!(body.len() >= 2); // for/each + ref
            }
            _ => panic!("expected Let"),
        }
    }

    // -----------------------------------------------------------------------
    // Recursion rejection
    // -----------------------------------------------------------------------

    #[test]
    fn test_reject_recursion() {
        let src = r#"
            (define (bad (n : Number)) : Number
              (bad (- n 1)))
        "#;
        let err = parse_plan_sexpr(src).unwrap_err();
        assert!(err.message.contains("recursion not allowed"));
    }

    #[test]
    fn test_reject_recursion_in_let() {
        let src = r#"
            (define (bad (n : Number)) : Number
              (let ([x (bad n)])
                x))
        "#;
        let err = parse_plan_sexpr(src).unwrap_err();
        assert!(err.message.contains("recursion not allowed"));
    }

    // -----------------------------------------------------------------------
    // Error cases
    // -----------------------------------------------------------------------

    #[test]
    fn test_reject_empty_define() {
        let src = "(define (empty (n : Number)) : Number)";
        let err = parse_plan_sexpr(src).unwrap_err();
        assert!(err.message.contains("empty"));
    }

    #[test]
    fn test_reject_non_define_toplevel() {
        let src = "(let ([x 1]) x)";
        let err = parse_plan_sexpr(src).unwrap_err();
        assert!(err.message.contains("define"));
    }

    #[test]
    fn test_reject_empty_form() {
        let src = "()";
        let err = parse_plan_sexpr(src).unwrap_err();
        assert!(err.message.contains("empty"));
    }

    // -----------------------------------------------------------------------
    // Bind expression tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_bind_list_literal() {
        let src = r#"
            (define (test (lst : (List Number))) : Number
              (bind lst (list 1 2 3))
              (length lst))
        "#;
        let ast = parse_plan_sexpr(src).unwrap();
        assert_eq!(ast.bindings.len(), 1);
        match &ast.bindings[0].1 {
            Expr::ListLit(elems) => assert_eq!(elems.len(), 3),
            _ => panic!("expected ListLit"),
        }
    }

    #[test]
    fn test_bind_number() {
        let src = r#"
            (define (test (n : Number)) : Number
              (bind n 42)
              n)
        "#;
        let ast = parse_plan_sexpr(src).unwrap();
        assert_eq!(ast.bindings.len(), 1);
        match &ast.bindings[0].1 {
            Expr::Num(n) => assert_eq!(n, "42"),
            _ => panic!("expected Num"),
        }
    }

    // -----------------------------------------------------------------------
    // Lowering tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_lower_simple_call() {
        let src = r#"
            (define (double (x : Number)) : Number
              (bind x 5)
              (* x 2))
        "#;
        let plan = parse_sexpr_to_plan(src).unwrap();
        assert_eq!(plan.name, "double");
        assert_eq!(plan.inputs.len(), 1);
        assert_eq!(plan.bindings.get("x"), Some(&"5".to_string()));
        assert!(!plan.steps.is_empty());
    }

    #[test]
    fn test_lower_let_bindings() {
        let src = r#"
            (define (test (n : Number)) : Number
              (bind n 10)
              (let ([a (+ n 1)]
                    [b (+ n 2)])
                (* a b)))
        "#;
        let plan = parse_sexpr_to_plan(src).unwrap();
        // Should have steps for: add(n,1), add(n,2), multiply(a,b)
        assert!(plan.steps.len() >= 3);
    }

    #[test]
    fn test_lower_undefined_variable() {
        let src = r#"
            (define (test (n : Number)) : Number
              (+ n undefined_var))
        "#;
        let err = parse_sexpr_to_plan(src).unwrap_err();
        assert!(err.message.contains("undefined variable"));
    }

    #[test]
    fn test_lower_for_fold() {
        let src = r#"
            (define (factorial (n : Number)) : Number
              (bind n 10)
              (for/fold ([acc 1]) ([i (range 1 (+ n 1))])
                (* acc i)))
        "#;
        let plan = parse_sexpr_to_plan(src).unwrap();
        // Should have: add step, range step, fold step
        assert!(plan.steps.len() >= 2);
        // Last step should be fold
        let last = plan.steps.last().unwrap();
        assert_eq!(last.op, "fold");
    }

    #[test]
    fn test_lower_bind_list() {
        let src = r#"
            (define (test (lst : (List Number))) : Number
              (bind lst (list 10 9 2 5))
              (length lst))
        "#;
        let plan = parse_sexpr_to_plan(src).unwrap();
        assert_eq!(plan.bindings.get("lst"), Some(&"(list 10 9 2 5)".to_string()));
    }

    #[test]
    fn test_lower_for_each_with_set() {
        let src = r#"
            (define (fill (n : Number)) : Number
              (bind n 5)
              (let ([arr (make n 0)])
                (for/each ([i (range 0 n)])
                  (set! arr i i))
                (ref arr 0)))
        "#;
        let plan = parse_sexpr_to_plan(src).unwrap();
        // Should have make_vector, range, for_each, vector_ref steps
        assert!(plan.steps.len() >= 3);
    }

    // -----------------------------------------------------------------------
    // Keyword argument tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_keyword_single_value() {
        // (filter :pattern "*.pdf") → StepArgs::Map { pattern: "*.pdf" }
        let src = r#"
            (define (test (path : Dir)) : Dir
              (filter :pattern "*.pdf"))
        "#;
        let plan = parse_sexpr_to_plan(src).unwrap();
        assert_eq!(plan.steps.len(), 1);
        assert_eq!(plan.steps[0].op, "filter");
        match &plan.steps[0].args {
            StepArgs::Map(m) => {
                assert_eq!(m.get("pattern"), Some(&StepParam::Value("*.pdf".to_string())));
            }
            other => panic!("expected Map, got {:?}", other),
        }
    }

    #[test]
    fn test_keyword_multiple_values() {
        // (awk_extract :program "{print $1}") → StepArgs::Map
        let src = r#"
            (define (test (logfile : File)) : File
              (awk_extract :program "{print $1, $3}"))
        "#;
        let plan = parse_sexpr_to_plan(src).unwrap();
        assert_eq!(plan.steps[0].op, "awk_extract");
        match &plan.steps[0].args {
            StepArgs::Map(m) => {
                assert_eq!(m.get("program"), Some(&StepParam::Value("{print $1, $3}".to_string())));
            }
            other => panic!("expected Map, got {:?}", other),
        }
    }

    #[test]
    fn test_keyword_each_modifier() {
        // (extract_archive :each) → StepArgs::Scalar("each")
        let src = r#"
            (define (test (path : Dir)) : Dir
              (extract_archive :each))
        "#;
        let plan = parse_sexpr_to_plan(src).unwrap();
        assert_eq!(plan.steps[0].op, "extract_archive");
        assert_eq!(plan.steps[0].args, StepArgs::Scalar("each".to_string()));
    }

    #[test]
    fn test_keyword_scalar_shorthand() {
        // (sort_by :name) → StepArgs::Map { name: true }
        // Actually, :name is a keyword with no value... but it's not :each.
        // For (sort_by :name), the YAML equivalent is `sort_by: name`.
        // This should be StepArgs::Scalar("name") — a single keyword that
        // acts as a bare scalar value, not a key-value pair.
        //
        // Wait — :name has no value. It's like :each but for a different purpose.
        // In YAML: `sort_by: name` is Scalar("name").
        // In sexpr: (sort_by :name) should also be Scalar("name").
        //
        // Let's handle this: if there's exactly one keyword with Bool(true)
        // value (i.e., a flag keyword) and it's not "each", treat it as
        // a scalar argument.
        //
        // Actually, the cleaner approach: (sort_by "name") works as positional.
        // But :name is more natural. Let me test the positional form first.
        let src = r#"
            (define (test (path : Dir)) : Dir
              (sort_by "name"))
        "#;
        let plan = parse_sexpr_to_plan(src).unwrap();
        assert_eq!(plan.steps[0].op, "sort_by");
        assert_eq!(plan.steps[0].args, StepArgs::Scalar("name".to_string()));
    }

    #[test]
    fn test_keyword_no_value_error() {
        // (filter :pattern) → error: keyword requires a value
        let src = r#"
            (define (test (path : Dir)) : Dir
              (filter :pattern))
        "#;
        let err = parse_sexpr_to_plan(src).unwrap_err();
        assert!(err.message.contains("requires a value"), "got: {}", err.message);
    }

    #[test]
    fn test_keyword_duplicate_error() {
        // (filter :pattern "a" :pattern "b") → error: duplicate keyword
        let src = r#"
            (define (test (path : Dir)) : Dir
              (filter :pattern "a" :pattern "b"))
        "#;
        let err = parse_sexpr_to_plan(src).unwrap_err();
        assert!(err.message.contains("duplicate keyword"), "got: {}", err.message);
    }

    #[test]
    fn test_keyword_positional_after_keyword_error() {
        // (filter :pattern "a" extra) → error: positional after keyword
        let src = r#"
            (define (test (path : Dir)) : Dir
              (filter :pattern "a" extra))
        "#;
        let err = parse_sexpr_to_plan(src).unwrap_err();
        assert!(err.message.contains("positional argument after keyword"), "got: {}", err.message);
    }

    #[test]
    fn test_keyword_pipeline_plan() {
        // Full pipeline: (list_dir) (find_matching :pattern "*.pdf") (sort_by "name")
        let src = r#"
            (define (find-pdfs (path : Dir) (keyword : Pattern)) : Dir
              (list_dir)
              (find_matching :pattern "*.pdf")
              (sort_by "name"))
        "#;
        let plan = parse_sexpr_to_plan(src).unwrap();
        assert_eq!(plan.steps.len(), 3);
        assert_eq!(plan.steps[0].op, "list_dir");
        assert_eq!(plan.steps[0].args, StepArgs::None);
        assert_eq!(plan.steps[1].op, "find_matching");
        match &plan.steps[1].args {
            StepArgs::Map(m) => {
                assert_eq!(m.get("pattern"), Some(&StepParam::Value("*.pdf".to_string())));
            }
            other => panic!("expected Map, got {:?}", other),
        }
        assert_eq!(plan.steps[2].op, "sort_by");
        assert_eq!(plan.steps[2].args, StepArgs::Scalar("name".to_string()));
    }

    #[test]
    fn test_keyword_repack_pipeline() {
        // Repack comics pipeline with :each
        let src = r#"
            (define (repack-comics (path : Dir)) : Dir
              (list_dir)
              (find_matching :pattern "*.cbz")
              (sort_by "name")
              (extract_archive :each)
              (pack_archive :output "combined.cbz"))
        "#;
        let plan = parse_sexpr_to_plan(src).unwrap();
        assert_eq!(plan.steps.len(), 5);
        assert_eq!(plan.steps[3].op, "extract_archive");
        assert_eq!(plan.steps[3].args, StepArgs::Scalar("each".to_string()));
        assert_eq!(plan.steps[4].op, "pack_archive");
        match &plan.steps[4].args {
            StepArgs::Map(m) => {
                assert_eq!(m.get("output"), Some(&StepParam::Value("combined.cbz".to_string())));
            }
            other => panic!("expected Map, got {:?}", other),
        }
    }

    #[test]
    fn test_keyword_two_keywords() {
        // (head :count "100") → StepArgs::Map { count: "100" }
        let src = r#"
            (define (test (logfile : File)) : File
              (head :count "100"))
        "#;
        let plan = parse_sexpr_to_plan(src).unwrap();
        match &plan.steps[0].args {
            StepArgs::Map(m) => {
                assert_eq!(m.get("count"), Some(&StepParam::Value("100".to_string())));
            }
            other => panic!("expected Map, got {:?}", other),
        }
    }

    #[test]
    fn test_tokenize_keyword_symbol() {
        // :pattern should tokenize as Symbol(":pattern"), not Colon + Symbol("pattern")
        let tokens = tokenize(":pattern").unwrap();
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].value, Token::Symbol(":pattern".to_string()));
    }

    #[test]
    fn test_tokenize_standalone_colon() {
        // : followed by space should be Colon token
        let tokens = tokenize(": Number").unwrap();
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].value, Token::Colon);
        assert_eq!(tokens[1].value, Token::Symbol("Number".to_string()));
    }

    // -----------------------------------------------------------------------
    // plan_to_sexpr serializer tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_plan_to_sexpr_simple_pipeline() {
        use crate::plan::{PlanDef, PlanInput, RawStep, StepArgs};
        let plan = PlanDef {
            name: "find-pdfs".to_string(),
            inputs: vec![
                PlanInput::typed("path", "Dir"),
                PlanInput::typed("keyword", "Pattern"),
            ],
            output: None,
            steps: vec![
                RawStep { op: "list_dir".to_string(), args: StepArgs::None },
                RawStep { op: "find_matching".to_string(), args: StepArgs::Map({
                    let mut m = HashMap::new();
                    m.insert("pattern".to_string(), StepParam::Value("*.pdf".to_string()));
                    m
                })},
                RawStep { op: "sort_by".to_string(), args: StepArgs::Scalar("name".to_string()) },
            ],
            bindings: HashMap::new(),
        };
        let sexpr = plan_to_sexpr(&plan);
        assert!(sexpr.contains("(define (find-pdfs"), "got: {}", sexpr);
        assert!(sexpr.contains("(path : Dir)"), "got: {}", sexpr);
        assert!(sexpr.contains("(list_dir)"), "got: {}", sexpr);
        assert!(sexpr.contains(":pattern \"*.pdf\""), "got: {}", sexpr);
        assert!(sexpr.contains("(sort_by \"name\")"), "got: {}", sexpr);
    }

    #[test]
    fn test_plan_to_sexpr_roundtrip() {
        use crate::plan::{PlanDef, PlanInput, RawStep, StepArgs};
        let plan = PlanDef {
            name: "test-plan".to_string(),
            inputs: vec![PlanInput::typed("path", "Dir")],
            output: None,
            steps: vec![
                RawStep { op: "walk_tree".to_string(), args: StepArgs::None },
                RawStep { op: "filter".to_string(), args: StepArgs::Map({
                    let mut m = HashMap::new();
                    m.insert("pattern".to_string(), StepParam::Value("*.rs".to_string()));
                    m
                })},
                RawStep { op: "sort_by".to_string(), args: StepArgs::Scalar("name".to_string()) },
            ],
            bindings: HashMap::new(),
        };
        let sexpr = plan_to_sexpr(&plan);
        let parsed = parse_sexpr_to_plan(&sexpr).unwrap();
        assert_eq!(parsed.name, "test-plan");
        assert_eq!(parsed.steps.len(), 3);
        assert_eq!(parsed.steps[0].op, "walk_tree");
        assert_eq!(parsed.steps[1].op, "filter");
        assert_eq!(parsed.steps[2].op, "sort_by");
    }

    #[test]
    fn test_plan_to_sexpr_with_each() {
        use crate::plan::{PlanDef, PlanInput, RawStep, StepArgs};
        let plan = PlanDef {
            name: "repack".to_string(),
            inputs: vec![PlanInput::typed("path", "Dir")],
            output: None,
            steps: vec![
                RawStep { op: "list_dir".to_string(), args: StepArgs::None },
                RawStep { op: "extract_archive".to_string(), args: StepArgs::Scalar("each".to_string()) },
            ],
            bindings: HashMap::new(),
        };
        let sexpr = plan_to_sexpr(&plan);
        assert!(sexpr.contains("(extract_archive :each)"), "got: {}", sexpr);
        // Round-trip
        let parsed = parse_sexpr_to_plan(&sexpr).unwrap();
        assert_eq!(parsed.steps[1].args, StepArgs::Scalar("each".to_string()));
    }

    #[test]
    fn test_plan_to_sexpr_with_bindings() {
        use crate::plan::{PlanDef, PlanInput, RawStep, StepArgs};
        let mut bindings = HashMap::new();
        bindings.insert("n".to_string(), "10".to_string());
        let plan = PlanDef {
            name: "test".to_string(),
            inputs: vec![PlanInput::typed("n", "Number")],
            output: None,
            steps: vec![
                RawStep { op: "add".to_string(), args: StepArgs::Map({
                    let mut m = HashMap::new();
                    m.insert("x".to_string(), StepParam::Value("$n".to_string()));
                    m.insert("y".to_string(), StepParam::Value("1".to_string()));
                    m
                })},
            ],
            bindings,
        };
        let sexpr = plan_to_sexpr(&plan);
        assert!(sexpr.contains("(bind n 10)"), "got: {}", sexpr);
        assert!(sexpr.contains(":x $n"), "got: {}", sexpr);
        assert!(sexpr.contains(":y 1"), "got: {}", sexpr);
    }

    #[test]
    fn test_plan_to_sexpr_empty_steps() {
        use crate::plan::{PlanDef, PlanInput};
        let plan = PlanDef {
            name: "empty".to_string(),
            inputs: vec![PlanInput::bare("path")],
            output: None,
            steps: vec![],
            bindings: HashMap::new(),
        };
        let sexpr = plan_to_sexpr(&plan);
        assert!(sexpr.contains("(define (empty"), "got: {}", sexpr);
        // Should not crash, just produce a define with no body
    }

    #[test]
    fn test_plan_to_sexpr_with_output_type() {
        use crate::plan::{PlanDef, PlanInput, RawStep, StepArgs};
        let plan = PlanDef {
            name: "count-files".to_string(),
            inputs: vec![PlanInput::typed("path", "Dir")],
            output: Some(vec!["Count".to_string()]),
            steps: vec![
                RawStep { op: "walk_tree".to_string(), args: StepArgs::None },
                RawStep { op: "count".to_string(), args: StepArgs::None },
            ],
            bindings: HashMap::new(),
        };
        let sexpr = plan_to_sexpr(&plan);
        assert!(sexpr.contains(": Count"), "got: {}", sexpr);
    }
}
