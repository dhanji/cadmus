use std::collections::HashMap;
use std::fmt;

// ---------------------------------------------------------------------------
// TypeExpr — compositional type grammar
// ---------------------------------------------------------------------------

/// A compositional type expression for Cadmus.
///
/// Unlike flat `TypeId` strings, `TypeExpr` supports constructors with
/// arguments (e.g., `File(Archive(Bytes, Zip))`), type variables for
/// polymorphic operations, and structural unification.
///
/// Existing strategies (Comparison, Coding) continue using `TypeId`.
/// `TypeExpr` is opt-in for strategies that need richer type structure.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeExpr {
    /// A primitive type with no arguments: `Bytes`, `Path`, `Name`, etc.
    Primitive(String),
    /// A type constructor applied to arguments: `File(Image)`, `Seq(Entry(Name, Bytes))`, etc.
    Constructor(String, Vec<TypeExpr>),
    /// A type variable for polymorphic signatures: `a`, `b`, etc.
    Var(String),
}

// ---------------------------------------------------------------------------
// Convenience constructors
// ---------------------------------------------------------------------------

impl TypeExpr {
    /// Create a primitive type.
    pub fn prim(name: impl Into<String>) -> Self {
        TypeExpr::Primitive(name.into())
    }

    /// Create a type constructor with arguments.
    pub fn cons(name: impl Into<String>, args: Vec<TypeExpr>) -> Self {
        TypeExpr::Constructor(name.into(), args)
    }

    /// Create a type variable.
    pub fn var(name: impl Into<String>) -> Self {
        TypeExpr::Var(name.into())
    }

    /// Sugar: `Seq(elem)`
    pub fn seq(elem: TypeExpr) -> Self {
        TypeExpr::Constructor("Seq".to_string(), vec![elem])
    }

    /// Sugar: `File(content)`
    pub fn file(content: TypeExpr) -> Self {
        TypeExpr::Constructor("File".to_string(), vec![content])
    }

    /// Sugar: `Dir(entry)`
    pub fn dir(entry: TypeExpr) -> Self {
        TypeExpr::Constructor("Dir".to_string(), vec![entry])
    }

    /// Sugar: `Entry(key, val)`
    pub fn entry(key: TypeExpr, val: TypeExpr) -> Self {
        TypeExpr::Constructor("Entry".to_string(), vec![key, val])
    }

    /// Sugar: `Archive(content, format)`
    pub fn archive(content: TypeExpr, format: TypeExpr) -> Self {
        TypeExpr::Constructor("Archive".to_string(), vec![content, format])
    }

    /// Sugar: `Tree(node)`
    pub fn tree(node: TypeExpr) -> Self {
        TypeExpr::Constructor("Tree".to_string(), vec![node])
    }

    /// Sugar: `Match(pattern, val)`
    pub fn match_type(pattern: TypeExpr, val: TypeExpr) -> Self {
        TypeExpr::Constructor("Match".to_string(), vec![pattern, val])
    }

    /// Sugar: `Option(inner)` — represents an optional value.
    pub fn option(inner: TypeExpr) -> Self {
        TypeExpr::Constructor("Option".to_string(), vec![inner])
    }

    /// Returns true if this type expression contains any type variables.
    pub fn has_vars(&self) -> bool {
        match self {
            TypeExpr::Primitive(_) => false,
            TypeExpr::Var(_) => true,
            TypeExpr::Constructor(_, args) => args.iter().any(|a| a.has_vars()),
        }
    }

    /// Collect all type variable names in this expression.
    pub fn free_vars(&self) -> Vec<String> {
        let mut vars = Vec::new();
        self.collect_vars(&mut vars);
        vars.sort();
        vars.dedup();
        vars
    }

    fn collect_vars(&self, out: &mut Vec<String>) {
        match self {
            TypeExpr::Primitive(_) => {}
            TypeExpr::Var(name) => out.push(name.clone()),
            TypeExpr::Constructor(_, args) => {
                for a in args {
                    a.collect_vars(out);
                }
            }
        }
    }

    /// Apply a substitution to this type expression, replacing variables
    /// with their bound values.
    pub fn apply_subst(&self, subst: &Substitution) -> TypeExpr {
        match self {
            TypeExpr::Primitive(_) => self.clone(),
            TypeExpr::Var(name) => {
                if let Some(bound) = subst.get(name) {
                    // Recursively apply in case the bound value also has vars
                    bound.apply_subst(subst)
                } else {
                    self.clone()
                }
            }
            TypeExpr::Constructor(name, args) => {
                TypeExpr::Constructor(
                    name.clone(),
                    args.iter().map(|a| a.apply_subst(subst)).collect(),
                )
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Display — human-readable rendering
// ---------------------------------------------------------------------------

impl fmt::Display for TypeExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeExpr::Primitive(name) => write!(f, "{}", name),
            TypeExpr::Var(name) => write!(f, "{}", name),
            TypeExpr::Constructor(name, args) => {
                write!(f, "{}(", name)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Parsing — from string shorthand
// ---------------------------------------------------------------------------

/// Parse error for TypeExpr string parsing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub message: String,
    pub position: usize,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "parse error at position {}: {}", self.position, self.message)
    }
}

impl std::error::Error for ParseError {}

impl TypeExpr {
    /// Parse a type expression from a string.
    ///
    /// Grammar:
    /// ```text
    /// type_expr = identifier [ '(' type_expr (',' type_expr)* ')' ]
    /// identifier = [a-zA-Z_][a-zA-Z0-9_]*
    /// ```
    ///
    /// Identifiers starting with a lowercase letter are treated as type variables.
    /// Identifiers starting with an uppercase letter are primitives or constructors
    /// (constructors if followed by parenthesized arguments).
    ///
    /// Examples:
    /// - `"Bytes"` → `Primitive("Bytes")`
    /// - `"a"` → `Var("a")`
    /// - `"File(Image)"` → `Constructor("File", [Primitive("Image")])`
    /// - `"Seq(Entry(Name, File(Bytes)))"`
    pub fn parse(input: &str) -> Result<TypeExpr, ParseError> {
        let input = input.trim();
        if input.is_empty() {
            return Err(ParseError {
                message: "empty input".to_string(),
                position: 0,
            });
        }
        let (expr, rest) = parse_type_expr(input, 0)?;
        let rest = rest.trim();
        if !rest.is_empty() {
            return Err(ParseError {
                message: format!("unexpected trailing characters: '{}'", rest),
                position: input.len() - rest.len(),
            });
        }
        Ok(expr)
    }
}

/// Parse a single type expression, returning the parsed expr and remaining input.
fn parse_type_expr(input: &str, offset: usize) -> Result<(TypeExpr, &str), ParseError> {
    let input = skip_whitespace(input);
    if input.is_empty() {
        return Err(ParseError {
            message: "unexpected end of input".to_string(),
            position: offset,
        });
    }

    // Parse identifier
    let (name, rest) = parse_identifier(input, offset)?;
    let rest = skip_whitespace(rest);

    // Check for constructor arguments
    if rest.starts_with('(') {
        let rest = &rest[1..]; // skip '('
        let mut args = Vec::new();
        let mut current = skip_whitespace(rest);

        // Handle empty parens
        if current.starts_with(')') {
            return Ok((TypeExpr::Constructor(name, args), &current[1..]));
        }

        loop {
            let arg_offset = offset + (input.len() - current.len());
            let (arg, after_arg) = parse_type_expr(current, arg_offset)?;
            args.push(arg);
            let after_arg = skip_whitespace(after_arg);

            if after_arg.starts_with(')') {
                return Ok((TypeExpr::Constructor(name, args), &after_arg[1..]));
            } else if after_arg.starts_with(',') {
                current = skip_whitespace(&after_arg[1..]);
            } else if after_arg.is_empty() {
                return Err(ParseError {
                    message: "unclosed parenthesis".to_string(),
                    position: offset + input.len(),
                });
            } else {
                return Err(ParseError {
                    message: format!("expected ',' or ')' but found '{}'", &after_arg[..1]),
                    position: offset + (input.len() - after_arg.len()),
                });
            }
        }
    } else {
        // No parens: variable or primitive based on case
        let first_char = name.chars().next().unwrap();
        if first_char.is_lowercase() {
            Ok((TypeExpr::Var(name), rest))
        } else {
            Ok((TypeExpr::Primitive(name), rest))
        }
    }
}

fn parse_identifier(input: &str, offset: usize) -> Result<(String, &str), ParseError> {
    let mut chars = input.chars();
    let first = chars.next().ok_or_else(|| ParseError {
        message: "expected identifier".to_string(),
        position: offset,
    })?;

    if !first.is_alphabetic() && first != '_' {
        return Err(ParseError {
            message: format!("expected identifier, found '{}'", first),
            position: offset,
        });
    }

    let mut len = first.len_utf8();
    for c in chars {
        if c.is_alphanumeric() || c == '_' {
            len += c.len_utf8();
        } else {
            break;
        }
    }

    Ok((input[..len].to_string(), &input[len..]))
}

fn skip_whitespace(input: &str) -> &str {
    input.trim_start()
}

// ---------------------------------------------------------------------------
// Substitution — mapping from type variable names to types
// ---------------------------------------------------------------------------

/// A substitution: a mapping from type variable names to their bound types.
/// Produced by unification.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Substitution {
    bindings: HashMap<String, TypeExpr>,
}

impl Substitution {
    /// Create an empty substitution.
    pub fn empty() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }

    /// Get the binding for a variable, if any.
    pub fn get(&self, var: &str) -> Option<&TypeExpr> {
        self.bindings.get(var)
    }

    /// Insert a binding. Returns Err if the variable is already bound to a
    /// different type.
    pub fn bind(&mut self, var: String, ty: TypeExpr) -> Result<(), UnifyError> {
        if let Some(existing) = self.bindings.get(&var) {
            if existing != &ty {
                // Try to unify the existing binding with the new one
                let mut sub = self.clone();
                let unified = unify_inner(existing.clone(), ty, &mut sub)?;
                self.bindings = sub.bindings;
                self.bindings.insert(var, unified);
                return Ok(());
            }
        }
        self.bindings.insert(var, ty);
        Ok(())
    }

    /// Number of bindings.
    pub fn len(&self) -> usize {
        self.bindings.len()
    }

    /// Whether the substitution is empty.
    pub fn is_empty(&self) -> bool {
        self.bindings.is_empty()
    }

    /// Iterate over all bindings.
    pub fn iter(&self) -> impl Iterator<Item = (&String, &TypeExpr)> {
        self.bindings.iter()
    }

    /// Compose this substitution with another: apply `other` first, then `self`.
    pub fn compose(&self, other: &Substitution) -> Substitution {
        let mut result = Substitution::empty();
        // Apply self to all of other's bindings
        for (var, ty) in &other.bindings {
            result.bindings.insert(var.clone(), ty.apply_subst(self));
        }
        // Add self's bindings that aren't in other
        for (var, ty) in &self.bindings {
            result.bindings.entry(var.clone()).or_insert_with(|| ty.clone());
        }
        result
    }
}

impl fmt::Display for Substitution {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{")?;
        let mut entries: Vec<_> = self.bindings.iter().collect();
        entries.sort_by_key(|(k, _)| (*k).clone());
        for (i, (var, ty)) in entries.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{} → {}", var, ty)?;
        }
        write!(f, "}}")
    }
}

// ---------------------------------------------------------------------------
// Unification
// ---------------------------------------------------------------------------

/// Error from unification.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnifyError {
    /// Two types have different constructors and cannot unify.
    ConstructorMismatch {
        left: String,
        right: String,
    },
    /// Two constructors have different arities.
    ArityMismatch {
        constructor: String,
        left_arity: usize,
        right_arity: usize,
    },
    /// Occurs check failed: a variable would need to contain itself.
    OccursCheck {
        var: String,
        in_type: TypeExpr,
    },
    /// A primitive and constructor cannot unify.
    KindMismatch {
        left: TypeExpr,
        right: TypeExpr,
    },
}

impl fmt::Display for UnifyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnifyError::ConstructorMismatch { left, right } => {
                write!(f, "constructor mismatch: {} vs {}", left, right)
            }
            UnifyError::ArityMismatch { constructor, left_arity, right_arity } => {
                write!(f, "arity mismatch for {}: {} vs {}", constructor, left_arity, right_arity)
            }
            UnifyError::OccursCheck { var, in_type } => {
                write!(f, "occurs check: {} appears in {}", var, in_type)
            }
            UnifyError::KindMismatch { left, right } => {
                write!(f, "kind mismatch: {} vs {}", left, right)
            }
        }
    }
}

impl std::error::Error for UnifyError {}

/// Unify two type expressions, producing a substitution that makes them equal.
///
/// Returns `Ok(substitution)` if unification succeeds, or `Err(UnifyError)` if
/// the types cannot be unified.
pub fn unify(left: &TypeExpr, right: &TypeExpr) -> Result<Substitution, UnifyError> {
    let mut subst = Substitution::empty();
    let _result = unify_inner(left.clone(), right.clone(), &mut subst)?;
    Ok(subst)
}

fn unify_inner(left: TypeExpr, right: TypeExpr, subst: &mut Substitution) -> Result<TypeExpr, UnifyError> {
    // Apply current substitution first
    let left = left.apply_subst(subst);
    let right = right.apply_subst(subst);

    match (&left, &right) {
        // Identical types unify trivially
        _ if left == right => Ok(left),

        // Variable on the left: bind it
        (TypeExpr::Var(name), _) => {
            occurs_check(name, &right)?;
            subst.bind(name.clone(), right.clone())?;
            Ok(right)
        }

        // Variable on the right: bind it
        (_, TypeExpr::Var(name)) => {
            occurs_check(name, &left)?;
            subst.bind(name.clone(), left.clone())?;
            Ok(left)
        }

        // Two primitives: must be equal (already handled by left == right above)
        (TypeExpr::Primitive(a), TypeExpr::Primitive(b)) => {
            Err(UnifyError::ConstructorMismatch {
                left: a.clone(),
                right: b.clone(),
            })
        }

        // Two constructors: names must match, then unify arguments pairwise
        (TypeExpr::Constructor(name_l, args_l), TypeExpr::Constructor(name_r, args_r)) => {
            if name_l != name_r {
                return Err(UnifyError::ConstructorMismatch {
                    left: name_l.clone(),
                    right: name_r.clone(),
                });
            }
            if args_l.len() != args_r.len() {
                return Err(UnifyError::ArityMismatch {
                    constructor: name_l.clone(),
                    left_arity: args_l.len(),
                    right_arity: args_r.len(),
                });
            }
            let mut unified_args = Vec::with_capacity(args_l.len());
            for (al, ar) in args_l.iter().zip(args_r.iter()) {
                let unified = unify_inner(al.clone(), ar.clone(), subst)?;
                unified_args.push(unified);
            }
            Ok(TypeExpr::Constructor(name_l.clone(), unified_args))
        }

        // Primitive vs Constructor (or vice versa): kind mismatch
        _ => Err(UnifyError::KindMismatch {
            left: left.clone(),
            right: right.clone(),
        }),
    }
}

/// Occurs check: ensure `var` does not appear in `ty` (prevents infinite types).
fn occurs_check(var: &str, ty: &TypeExpr) -> Result<(), UnifyError> {
    match ty {
        TypeExpr::Var(name) if name == var => {
            Err(UnifyError::OccursCheck {
                var: var.to_string(),
                in_type: ty.clone(),
            })
        }
        TypeExpr::Var(_) | TypeExpr::Primitive(_) => Ok(()),
        TypeExpr::Constructor(_, args) => {
            for arg in args {
                occurs_check(var, arg)?;
            }
            Ok(())
        }
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // --- Display ---

    #[test]
    fn test_display_primitive() {
        assert_eq!(TypeExpr::prim("Bytes").to_string(), "Bytes");
    }

    #[test]
    fn test_display_var() {
        assert_eq!(TypeExpr::var("a").to_string(), "a");
    }

    #[test]
    fn test_display_constructor() {
        let ty = TypeExpr::file(TypeExpr::prim("Image"));
        assert_eq!(ty.to_string(), "File(Image)");
    }

    #[test]
    fn test_display_nested() {
        let ty = TypeExpr::seq(TypeExpr::entry(
            TypeExpr::prim("Name"),
            TypeExpr::file(TypeExpr::prim("Bytes")),
        ));
        assert_eq!(ty.to_string(), "Seq(Entry(Name, File(Bytes)))");
    }

    // --- Parsing ---

    #[test]
    fn test_parse_primitive() {
        let ty = TypeExpr::parse("Bytes").unwrap();
        assert_eq!(ty, TypeExpr::prim("Bytes"));
    }

    #[test]
    fn test_parse_var() {
        let ty = TypeExpr::parse("a").unwrap();
        assert_eq!(ty, TypeExpr::var("a"));
    }

    #[test]
    fn test_parse_constructor_single_arg() {
        let ty = TypeExpr::parse("File(Image)").unwrap();
        assert_eq!(ty, TypeExpr::file(TypeExpr::prim("Image")));
    }

    #[test]
    fn test_parse_constructor_multi_arg() {
        let ty = TypeExpr::parse("Archive(Bytes, Zip)").unwrap();
        assert_eq!(ty, TypeExpr::archive(TypeExpr::prim("Bytes"), TypeExpr::prim("Zip")));
    }

    #[test]
    fn test_parse_nested() {
        let ty = TypeExpr::parse("File(Archive(Bytes, Zip))").unwrap();
        assert_eq!(
            ty,
            TypeExpr::file(TypeExpr::archive(TypeExpr::prim("Bytes"), TypeExpr::prim("Zip")))
        );
    }

    #[test]
    fn test_parse_deeply_nested() {
        let ty = TypeExpr::parse("Seq(Entry(Name, File(Bytes)))").unwrap();
        assert_eq!(
            ty,
            TypeExpr::seq(TypeExpr::entry(
                TypeExpr::prim("Name"),
                TypeExpr::file(TypeExpr::prim("Bytes")),
            ))
        );
    }

    #[test]
    fn test_parse_roundtrip() {
        let input = "File(Archive(Bytes, Zip))";
        let ty = TypeExpr::parse(input).unwrap();
        assert_eq!(ty.to_string(), input);
    }

    #[test]
    fn test_parse_with_whitespace() {
        let ty = TypeExpr::parse("  File( Archive( Bytes , Zip ) )  ").unwrap();
        assert_eq!(
            ty,
            TypeExpr::file(TypeExpr::archive(TypeExpr::prim("Bytes"), TypeExpr::prim("Zip")))
        );
    }

    #[test]
    fn test_parse_var_in_constructor() {
        let ty = TypeExpr::parse("Seq(a)").unwrap();
        assert_eq!(ty, TypeExpr::seq(TypeExpr::var("a")));
    }

    // --- Parse errors ---

    #[test]
    fn test_parse_empty() {
        let err = TypeExpr::parse("").unwrap_err();
        assert_eq!(err.message, "empty input");
    }

    #[test]
    fn test_parse_unclosed_paren() {
        let err = TypeExpr::parse("File(").unwrap_err();
        assert!(err.message.contains("unexpected end of input") || err.message.contains("unclosed"),
            "got: {}", err.message);
    }

    #[test]
    fn test_parse_trailing_chars() {
        let err = TypeExpr::parse("File(Bytes) extra").unwrap_err();
        assert!(err.message.contains("trailing"), "got: {}", err.message);
    }

    #[test]
    fn test_parse_invalid_start() {
        let err = TypeExpr::parse("123").unwrap_err();
        assert!(err.message.contains("expected identifier"), "got: {}", err.message);
    }

    // --- Unification ---

    #[test]
    fn test_unify_identical_primitives() {
        let subst = unify(&TypeExpr::prim("Bytes"), &TypeExpr::prim("Bytes")).unwrap();
        assert!(subst.is_empty());
    }

    #[test]
    fn test_unify_different_primitives_fails() {
        let err = unify(&TypeExpr::prim("Bytes"), &TypeExpr::prim("Text")).unwrap_err();
        assert!(matches!(err, UnifyError::ConstructorMismatch { .. }));
    }

    #[test]
    fn test_unify_var_with_primitive() {
        let subst = unify(&TypeExpr::var("a"), &TypeExpr::prim("Bytes")).unwrap();
        assert_eq!(subst.get("a"), Some(&TypeExpr::prim("Bytes")));
    }

    #[test]
    fn test_unify_primitive_with_var() {
        let subst = unify(&TypeExpr::prim("Bytes"), &TypeExpr::var("a")).unwrap();
        assert_eq!(subst.get("a"), Some(&TypeExpr::prim("Bytes")));
    }

    #[test]
    fn test_unify_var_with_constructor() {
        let ty = TypeExpr::file(TypeExpr::seq(TypeExpr::prim("Bytes")));
        let subst = unify(&TypeExpr::var("a"), &ty).unwrap();
        assert_eq!(subst.get("a"), Some(&ty));
    }

    #[test]
    fn test_unify_constructors_same_name() {
        let left = TypeExpr::file(TypeExpr::prim("Image"));
        let right = TypeExpr::file(TypeExpr::prim("Image"));
        let subst = unify(&left, &right).unwrap();
        assert!(subst.is_empty());
    }

    #[test]
    fn test_unify_constructors_different_names_fails() {
        let left = TypeExpr::file(TypeExpr::prim("Image"));
        let right = TypeExpr::dir(TypeExpr::prim("Image"));
        let err = unify(&left, &right).unwrap_err();
        assert!(matches!(err, UnifyError::ConstructorMismatch { .. }));
    }

    #[test]
    fn test_unify_constructors_different_arity_fails() {
        let left = TypeExpr::cons("Foo", vec![TypeExpr::prim("A")]);
        let right = TypeExpr::cons("Foo", vec![TypeExpr::prim("A"), TypeExpr::prim("B")]);
        let err = unify(&left, &right).unwrap_err();
        assert!(matches!(err, UnifyError::ArityMismatch { .. }));
    }

    #[test]
    fn test_unify_nested_with_var() {
        // File(Archive(Var(a), Zip)) with File(Archive(Bytes, Zip)) → {a → Bytes}
        let left = TypeExpr::file(TypeExpr::archive(TypeExpr::var("a"), TypeExpr::prim("Zip")));
        let right = TypeExpr::file(TypeExpr::archive(TypeExpr::prim("Bytes"), TypeExpr::prim("Zip")));
        let subst = unify(&left, &right).unwrap();
        assert_eq!(subst.len(), 1);
        assert_eq!(subst.get("a"), Some(&TypeExpr::prim("Bytes")));
    }

    #[test]
    fn test_unify_multiple_vars() {
        // Entry(a, b) with Entry(Name, File(Bytes)) → {a → Name, b → File(Bytes)}
        let left = TypeExpr::entry(TypeExpr::var("a"), TypeExpr::var("b"));
        let right = TypeExpr::entry(TypeExpr::prim("Name"), TypeExpr::file(TypeExpr::prim("Bytes")));
        let subst = unify(&left, &right).unwrap();
        assert_eq!(subst.len(), 2);
        assert_eq!(subst.get("a"), Some(&TypeExpr::prim("Name")));
        assert_eq!(subst.get("b"), Some(&TypeExpr::file(TypeExpr::prim("Bytes"))));
    }

    #[test]
    fn test_unify_same_var_consistent() {
        // Entry(a, a) with Entry(Bytes, Bytes) → {a → Bytes}
        let left = TypeExpr::entry(TypeExpr::var("a"), TypeExpr::var("a"));
        let right = TypeExpr::entry(TypeExpr::prim("Bytes"), TypeExpr::prim("Bytes"));
        let subst = unify(&left, &right).unwrap();
        assert_eq!(subst.get("a"), Some(&TypeExpr::prim("Bytes")));
    }

    #[test]
    fn test_unify_same_var_inconsistent_fails() {
        // Entry(a, a) with Entry(Bytes, Text) → fails (a can't be both)
        let left = TypeExpr::entry(TypeExpr::var("a"), TypeExpr::var("a"));
        let right = TypeExpr::entry(TypeExpr::prim("Bytes"), TypeExpr::prim("Text"));
        let err = unify(&left, &right).unwrap_err();
        assert!(matches!(err, UnifyError::ConstructorMismatch { .. }));
    }

    #[test]
    fn test_occurs_check_direct() {
        // Var(a) with Seq(Var(a)) → occurs check failure
        let err = unify(&TypeExpr::var("a"), &TypeExpr::seq(TypeExpr::var("a"))).unwrap_err();
        assert!(matches!(err, UnifyError::OccursCheck { .. }));
    }

    #[test]
    fn test_occurs_check_nested() {
        // Var(a) with File(Seq(Var(a))) → occurs check failure
        let err = unify(
            &TypeExpr::var("a"),
            &TypeExpr::file(TypeExpr::seq(TypeExpr::var("a"))),
        ).unwrap_err();
        assert!(matches!(err, UnifyError::OccursCheck { .. }));
    }

    #[test]
    fn test_unify_primitive_vs_constructor_fails() {
        let err = unify(&TypeExpr::prim("Bytes"), &TypeExpr::file(TypeExpr::prim("Bytes"))).unwrap_err();
        assert!(matches!(err, UnifyError::KindMismatch { .. }));
    }

    // --- Substitution ---

    #[test]
    fn test_apply_subst() {
        let ty = TypeExpr::file(TypeExpr::var("a"));
        let mut subst = Substitution::empty();
        subst.bindings.insert("a".to_string(), TypeExpr::prim("Image"));
        let result = ty.apply_subst(&subst);
        assert_eq!(result, TypeExpr::file(TypeExpr::prim("Image")));
    }

    #[test]
    fn test_apply_subst_nested() {
        let ty = TypeExpr::seq(TypeExpr::entry(TypeExpr::var("k"), TypeExpr::var("v")));
        let mut subst = Substitution::empty();
        subst.bindings.insert("k".to_string(), TypeExpr::prim("Name"));
        subst.bindings.insert("v".to_string(), TypeExpr::file(TypeExpr::prim("Bytes")));
        let result = ty.apply_subst(&subst);
        assert_eq!(
            result,
            TypeExpr::seq(TypeExpr::entry(
                TypeExpr::prim("Name"),
                TypeExpr::file(TypeExpr::prim("Bytes")),
            ))
        );
    }

    #[test]
    fn test_apply_subst_no_vars() {
        let ty = TypeExpr::prim("Bytes");
        let subst = Substitution::empty();
        assert_eq!(ty.apply_subst(&subst), ty);
    }

    // --- free_vars / has_vars ---

    #[test]
    fn test_has_vars() {
        assert!(!TypeExpr::prim("Bytes").has_vars());
        assert!(TypeExpr::var("a").has_vars());
        assert!(TypeExpr::file(TypeExpr::var("a")).has_vars());
        assert!(!TypeExpr::file(TypeExpr::prim("Bytes")).has_vars());
    }

    #[test]
    fn test_free_vars() {
        let ty = TypeExpr::entry(TypeExpr::var("a"), TypeExpr::file(TypeExpr::var("b")));
        let vars = ty.free_vars();
        assert_eq!(vars, vec!["a".to_string(), "b".to_string()]);
    }

    #[test]
    fn test_free_vars_dedup() {
        let ty = TypeExpr::entry(TypeExpr::var("a"), TypeExpr::var("a"));
        let vars = ty.free_vars();
        assert_eq!(vars, vec!["a".to_string()]);
    }

    // --- Substitution display ---

    #[test]
    fn test_substitution_display() {
        let mut subst = Substitution::empty();
        subst.bindings.insert("a".to_string(), TypeExpr::prim("Bytes"));
        let s = subst.to_string();
        assert_eq!(s, "{a → Bytes}");
    }

    // --- Compose ---

    #[test]
    fn test_substitution_compose() {
        let mut s1 = Substitution::empty();
        s1.bindings.insert("a".to_string(), TypeExpr::var("b"));

        let mut s2 = Substitution::empty();
        s2.bindings.insert("b".to_string(), TypeExpr::prim("Bytes"));

        // Composing s2 with s1: apply s2 to s1's bindings, then add s2's own
        let composed = s2.compose(&s1);
        // s1 maps a→b, applying s2 to that gives a→Bytes
        assert_eq!(composed.get("a"), Some(&TypeExpr::prim("Bytes")));
        // s2 maps b→Bytes
        assert_eq!(composed.get("b"), Some(&TypeExpr::prim("Bytes")));
    }

    // --- Option(a) first-class constructor ---

    #[test]
    fn test_option_convenience_constructor() {
        let ty = TypeExpr::option(TypeExpr::prim("Bytes"));
        assert_eq!(ty, TypeExpr::Constructor("Option".to_string(), vec![TypeExpr::prim("Bytes")]));
    }

    #[test]
    fn test_option_display() {
        let ty = TypeExpr::option(TypeExpr::prim("Bytes"));
        assert_eq!(ty.to_string(), "Option(Bytes)");
    }

    #[test]
    fn test_option_parse_roundtrip() {
        let ty = TypeExpr::parse("Option(Bytes)").unwrap();
        assert_eq!(ty, TypeExpr::option(TypeExpr::prim("Bytes")));
        assert_eq!(ty.to_string(), "Option(Bytes)");
    }

    #[test]
    fn test_option_parse_with_var() {
        let ty = TypeExpr::parse("Option(a)").unwrap();
        assert_eq!(ty, TypeExpr::option(TypeExpr::var("a")));
    }

    #[test]
    fn test_option_nested() {
        let ty = TypeExpr::parse("Option(Option(Bytes))").unwrap();
        assert_eq!(ty, TypeExpr::option(TypeExpr::option(TypeExpr::prim("Bytes"))));
        assert_eq!(ty.to_string(), "Option(Option(Bytes))");
    }

    #[test]
    fn test_option_unify_with_concrete() {
        // Option(a) unifies with Option(Bytes) → {a → Bytes}
        let left = TypeExpr::option(TypeExpr::var("a"));
        let right = TypeExpr::option(TypeExpr::prim("Bytes"));
        let subst = unify(&left, &right).unwrap();
        assert_eq!(subst.get("a"), Some(&TypeExpr::prim("Bytes")));
    }

    #[test]
    fn test_option_unify_mismatch() {
        // Option(Bytes) vs Seq(Bytes) → constructor mismatch
        let left = TypeExpr::option(TypeExpr::prim("Bytes"));
        let right = TypeExpr::seq(TypeExpr::prim("Bytes"));
        let err = unify(&left, &right).unwrap_err();
        assert!(matches!(err, UnifyError::ConstructorMismatch { .. }));
    }

    #[test]
    fn test_option_in_complex_type() {
        // Seq(Entry(Name, Option(File(Bytes)))) round-trips
        let ty = TypeExpr::parse("Seq(Entry(Name, Option(File(Bytes))))").unwrap();
        assert_eq!(ty.to_string(), "Seq(Entry(Name, Option(File(Bytes))))");
    }
}
