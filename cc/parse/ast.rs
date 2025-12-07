//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Abstract Syntax Tree for pcc C99 compiler
// Based on sparse's AST representation
//

use crate::diag::Position;
use crate::types::TypeId;

// ============================================================================
// Operators
// ============================================================================

/// Unary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    /// Negation: -x
    Neg,
    /// Logical not: !x
    Not,
    /// Bitwise not: ~x
    BitNot,
    /// Address-of: &x
    AddrOf,
    /// Dereference: *x
    Deref,
    /// Pre-increment: ++x
    PreInc,
    /// Pre-decrement: --x
    PreDec,
}

/// Binary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    // Comparison
    Lt,
    Gt,
    Le,
    Ge,
    Eq,
    Ne,

    // Logical
    LogAnd,
    LogOr,

    // Bitwise
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
}

impl BinaryOp {
    /// Check if this is a comparison operator (result type differs from operand type)
    pub fn is_comparison(&self) -> bool {
        matches!(
            self,
            BinaryOp::Lt | BinaryOp::Gt | BinaryOp::Le | BinaryOp::Ge | BinaryOp::Eq | BinaryOp::Ne
        )
    }
}

/// Assignment operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignOp {
    /// Simple assignment: =
    Assign,
    /// Add-assign: +=
    AddAssign,
    /// Sub-assign: -=
    SubAssign,
    /// Mul-assign: *=
    MulAssign,
    /// Div-assign: /=
    DivAssign,
    /// Mod-assign: %=
    ModAssign,
    /// And-assign: &=
    AndAssign,
    /// Or-assign: |=
    OrAssign,
    /// Xor-assign: ^=
    XorAssign,
    /// Left-shift-assign: <<=
    ShlAssign,
    /// Right-shift-assign: >>=
    ShrAssign,
}

// ============================================================================
// Expressions
// ============================================================================

/// An expression with type annotation (like sparse's expr->ctype)
///
/// Following sparse's design, every expression carries its computed type.
/// The type is filled in during type evaluation (after parsing, before linearization).
#[derive(Debug, Clone)]
pub struct Expr {
    /// The expression kind/variant
    pub kind: ExprKind,
    /// The computed type of this expression (like sparse's expr->ctype)
    /// None before type evaluation, Some after (interned TypeId)
    pub typ: Option<TypeId>,
    /// Source position for debug info
    pub pos: Position,
}

impl Expr {
    /// Create a new untyped expression with position
    pub fn new(kind: ExprKind, pos: Position) -> Self {
        Self {
            kind,
            typ: None,
            pos,
        }
    }

    /// Create an expression with a known type ID and position
    pub fn typed(kind: ExprKind, typ: TypeId, pos: Position) -> Self {
        Self {
            kind,
            typ: Some(typ),
            pos,
        }
    }

    /// Create a new untyped expression with default position (for tests)
    #[cfg(test)]
    pub fn new_unpositioned(kind: ExprKind) -> Self {
        Self {
            kind,
            typ: None,
            pos: Position::default(),
        }
    }

    /// Create an expression with a known type ID but default position (for tests)
    #[cfg(test)]
    pub fn typed_unpositioned(kind: ExprKind, typ: TypeId) -> Self {
        Self {
            kind,
            typ: Some(typ),
            pos: Position::default(),
        }
    }
}

/// Expression kinds (variants)
#[derive(Debug, Clone)]
pub enum ExprKind {
    /// Integer literal
    IntLit(i64),

    /// Floating-point literal
    FloatLit(f64),

    /// Character literal
    CharLit(char),

    /// String literal
    StringLit(String),

    /// Identifier (variable reference)
    Ident {
        name: String,
    },

    /// Unary operation
    Unary {
        op: UnaryOp,
        operand: Box<Expr>,
    },

    /// Binary operation
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },

    /// Assignment operation
    Assign {
        op: AssignOp,
        target: Box<Expr>,
        value: Box<Expr>,
    },

    /// Postfix increment/decrement
    PostInc(Box<Expr>),
    PostDec(Box<Expr>),

    /// Conditional (ternary) expression: cond ? then : else
    Conditional {
        cond: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },

    /// Function call
    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
    },

    /// Member access: expr.member
    Member {
        expr: Box<Expr>,
        member: String,
    },

    /// Pointer member access: expr->member
    Arrow {
        expr: Box<Expr>,
        member: String,
    },

    /// Array subscript: array[index]
    Index {
        array: Box<Expr>,
        index: Box<Expr>,
    },

    /// Type cast: (type)expr
    Cast {
        cast_type: TypeId,
        expr: Box<Expr>,
    },

    /// sizeof type: sizeof(int)
    SizeofType(TypeId),

    /// sizeof expression: sizeof expr
    SizeofExpr(Box<Expr>),

    /// Comma expression: expr1, expr2
    Comma(Vec<Expr>),

    /// Initializer list: {1, 2, 3} or {.x = 1, [0] = 2}
    InitList {
        elements: Vec<InitElement>,
    },

    // =========================================================================
    // Variadic function support (va_* builtins)
    // =========================================================================
    /// __builtin_va_start(ap, last_param)
    /// Initializes a va_list for use with variadic arguments
    VaStart {
        /// The va_list to initialize (lvalue)
        ap: Box<Expr>,
        /// Name of the last named parameter before ...
        last_param: String,
    },

    /// __builtin_va_arg(ap, type)
    /// Retrieves the next argument from a va_list
    VaArg {
        /// The va_list to read from
        ap: Box<Expr>,
        /// The type of argument to retrieve (interned TypeId)
        arg_type: TypeId,
    },

    /// __builtin_va_end(ap)
    /// Cleans up a va_list (often a no-op)
    VaEnd {
        /// The va_list to clean up
        ap: Box<Expr>,
    },

    /// __builtin_va_copy(dest, src)
    /// Copies a va_list
    VaCopy {
        /// Destination va_list
        dest: Box<Expr>,
        /// Source va_list
        src: Box<Expr>,
    },

    // =========================================================================
    // Byte-swapping builtins
    // =========================================================================
    /// __builtin_bswap16(x)
    /// Returns x with the order of bytes reversed (16-bit)
    Bswap16 {
        /// The value to byte-swap
        arg: Box<Expr>,
    },

    /// __builtin_bswap32(x)
    /// Returns x with the order of bytes reversed (32-bit)
    Bswap32 {
        /// The value to byte-swap
        arg: Box<Expr>,
    },

    /// __builtin_bswap64(x)
    /// Returns x with the order of bytes reversed (64-bit)
    Bswap64 {
        /// The value to byte-swap
        arg: Box<Expr>,
    },

    /// __builtin_alloca(size)
    /// Allocates size bytes on the stack, returns pointer
    Alloca {
        /// The size to allocate in bytes
        size: Box<Expr>,
    },
}

// ============================================================================
// Initializer List Support (C99)
// ============================================================================

/// A designator for struct field or array index in an initializer
#[derive(Debug, Clone)]
pub enum Designator {
    /// Field designator: .field_name
    Field(String),
    /// Index designator: [constant_expr] - evaluated at parse time
    Index(i64),
}

/// A single element in an initializer list
#[derive(Debug, Clone)]
pub struct InitElement {
    /// Optional designator chain: .x, [0], .x[1].y, etc.
    /// Empty for positional initialization
    pub designators: Vec<Designator>,
    /// The initializer value (can be another InitList for nested)
    pub value: Box<Expr>,
}

// Test-only helper constructors for AST nodes
#[cfg(test)]
use crate::types::TypeTable;

#[cfg(test)]
impl Expr {
    /// Create an integer literal (typed as int) - no position (for tests/internal use)
    pub fn int(value: i64, types: &TypeTable) -> Self {
        Expr::typed_unpositioned(ExprKind::IntLit(value), types.int_id)
    }

    /// Create a variable reference (untyped - needs type evaluation) - no position
    pub fn var(name: &str) -> Self {
        Expr::new_unpositioned(ExprKind::Ident {
            name: name.to_string(),
        })
    }

    /// Create a variable reference with a known type - no position
    pub fn var_typed(name: &str, typ: TypeId) -> Self {
        Expr::typed_unpositioned(
            ExprKind::Ident {
                name: name.to_string(),
            },
            typ,
        )
    }

    /// Create a binary expression (using TypeTable for type inference)
    pub fn binary(op: BinaryOp, left: Expr, right: Expr, types: &TypeTable) -> Self {
        // Derive type from operands - comparisons return int, arithmetic uses left type
        let result_type = match op {
            BinaryOp::Lt
            | BinaryOp::Gt
            | BinaryOp::Le
            | BinaryOp::Ge
            | BinaryOp::Eq
            | BinaryOp::Ne
            | BinaryOp::LogAnd
            | BinaryOp::LogOr => types.int_id,
            _ => left.typ.unwrap_or(types.int_id),
        };
        let pos = left.pos;
        Expr::typed(
            ExprKind::Binary {
                op,
                left: Box::new(left),
                right: Box::new(right),
            },
            result_type,
            pos,
        )
    }

    /// Create a unary expression
    pub fn unary(op: UnaryOp, operand: Expr, types: &TypeTable) -> Self {
        let result_type = match op {
            UnaryOp::Not => types.int_id,
            _ => operand.typ.unwrap_or(types.int_id),
        };
        let pos = operand.pos;
        Expr::typed(
            ExprKind::Unary {
                op,
                operand: Box::new(operand),
            },
            result_type,
            pos,
        )
    }

    /// Create an assignment (result type is target type)
    pub fn assign(target: Expr, value: Expr, types: &TypeTable) -> Self {
        let result_type = target.typ.unwrap_or(types.int_id);
        let pos = target.pos;
        Expr::typed(
            ExprKind::Assign {
                op: AssignOp::Assign,
                target: Box::new(target),
                value: Box::new(value),
            },
            result_type,
            pos,
        )
    }

    /// Create a function call (returns int by default - proper type needs evaluation)
    pub fn call(func: Expr, args: Vec<Expr>, types: &TypeTable) -> Self {
        let pos = func.pos;
        Expr::typed(
            ExprKind::Call {
                func: Box::new(func),
                args,
            },
            types.int_id,
            pos,
        )
    }
}

// ============================================================================
// Statements
// ============================================================================

/// A statement in the AST
#[derive(Debug, Clone)]
pub enum Stmt {
    /// Empty statement: ;
    Empty,

    /// Expression statement: expr;
    Expr(Expr),

    /// Compound statement (block): { ... }
    Block(Vec<BlockItem>),

    /// If statement: if (cond) then_stmt [else else_stmt]
    If {
        cond: Expr,
        then_stmt: Box<Stmt>,
        else_stmt: Option<Box<Stmt>>,
    },

    /// While loop: while (cond) body
    While { cond: Expr, body: Box<Stmt> },

    /// Do-while loop: do body while (cond);
    DoWhile { body: Box<Stmt>, cond: Expr },

    /// For loop: for (init; cond; post) body
    For {
        init: Option<ForInit>,
        cond: Option<Expr>,
        post: Option<Expr>,
        body: Box<Stmt>,
    },

    /// Return statement: return [expr];
    Return(Option<Expr>),

    /// Break statement
    Break,

    /// Continue statement
    Continue,

    /// Goto statement: goto label;
    Goto(String),

    /// Labeled statement: label: stmt
    Label { name: String, stmt: Box<Stmt> },

    /// Switch statement: switch (expr) { cases }
    Switch { expr: Expr, body: Box<Stmt> },

    /// Case label: case expr: (within switch body)
    Case(Expr),

    /// Default label (within switch body)
    Default,
}

/// Initializer for a for loop (can be declaration or expression)
#[derive(Debug, Clone)]
pub enum ForInit {
    /// Declaration: for (int i = 0; ...)
    Declaration(Declaration),
    /// Expression: for (i = 0; ...)
    Expression(Expr),
}

/// An item in a compound statement (block)
#[derive(Debug, Clone)]
pub enum BlockItem {
    Declaration(Declaration),
    Statement(Stmt),
}

// ============================================================================
// Declarations
// ============================================================================

/// A declaration
#[derive(Debug, Clone)]
pub struct Declaration {
    /// List of declarators
    pub declarators: Vec<InitDeclarator>,
}

/// A single declarator with optional initializer
#[derive(Debug, Clone)]
pub struct InitDeclarator {
    /// The name being declared
    pub name: String,
    /// The complete type (after applying declarator modifiers) - interned TypeId
    pub typ: TypeId,
    /// Optional initializer
    pub init: Option<Expr>,
}

#[cfg(test)]
impl Declaration {
    /// Create a simple declaration with one variable
    pub fn simple(name: &str, typ: TypeId, init: Option<Expr>) -> Self {
        Declaration {
            declarators: vec![InitDeclarator {
                name: name.to_string(),
                typ,
                init,
            }],
        }
    }
}

// ============================================================================
// Function Definition
// ============================================================================

/// A function parameter
#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: Option<String>,
    /// Parameter type (interned TypeId)
    pub typ: TypeId,
}

/// A function definition
#[derive(Debug, Clone)]
pub struct FunctionDef {
    /// Return type (interned TypeId)
    pub return_type: TypeId,
    /// Function name
    pub name: String,
    /// Parameters
    pub params: Vec<Parameter>,
    /// Function body
    pub body: Stmt,
    /// Source position of function definition (for debug info)
    pub pos: Position,
}

// ============================================================================
// Translation Unit
// ============================================================================

/// An external declaration (top-level item)
#[derive(Debug, Clone)]
pub enum ExternalDecl {
    /// Function definition
    FunctionDef(FunctionDef),
    /// Variable/type declaration
    Declaration(Declaration),
}

/// A translation unit (entire source file)
#[derive(Debug, Clone)]
pub struct TranslationUnit {
    pub items: Vec<ExternalDecl>,
}

impl TranslationUnit {
    pub fn new() -> Self {
        TranslationUnit { items: Vec::new() }
    }

    pub fn add(&mut self, item: ExternalDecl) {
        self.items.push(item);
    }
}

impl Default for TranslationUnit {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::TypeKind;

    #[test]
    fn test_int_literal() {
        let types = TypeTable::new();
        let expr = Expr::int(42, &types);
        match expr.kind {
            ExprKind::IntLit(v) => assert_eq!(v, 42),
            _ => panic!("Expected IntLit"),
        }
    }

    #[test]
    fn test_binary_expr() {
        let types = TypeTable::new();
        // 1 + 2
        let expr = Expr::binary(
            BinaryOp::Add,
            Expr::int(1, &types),
            Expr::int(2, &types),
            &types,
        );

        match expr.kind {
            ExprKind::Binary { op, left, right } => {
                assert_eq!(op, BinaryOp::Add);
                match left.kind {
                    ExprKind::IntLit(v) => assert_eq!(v, 1),
                    _ => panic!("Expected IntLit"),
                }
                match right.kind {
                    ExprKind::IntLit(v) => assert_eq!(v, 2),
                    _ => panic!("Expected IntLit"),
                }
            }
            _ => panic!("Expected Binary"),
        }
    }

    #[test]
    fn test_nested_binary() {
        let types = TypeTable::new();
        // 1 + 2 * 3 (represented as 1 + (2 * 3))
        let mul = Expr::binary(
            BinaryOp::Mul,
            Expr::int(2, &types),
            Expr::int(3, &types),
            &types,
        );
        let add = Expr::binary(BinaryOp::Add, Expr::int(1, &types), mul, &types);

        match add.kind {
            ExprKind::Binary { op, left, right } => {
                assert_eq!(op, BinaryOp::Add);
                match left.kind {
                    ExprKind::IntLit(v) => assert_eq!(v, 1),
                    _ => panic!("Expected IntLit"),
                }
                match right.kind {
                    ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::Mul),
                    _ => panic!("Expected Binary"),
                }
            }
            _ => panic!("Expected Binary"),
        }
    }

    #[test]
    fn test_unary_expr() {
        let types = TypeTable::new();
        // -x
        let expr = Expr::unary(UnaryOp::Neg, Expr::var("x"), &types);

        match expr.kind {
            ExprKind::Unary { op, operand } => {
                assert_eq!(op, UnaryOp::Neg);
                match operand.kind {
                    ExprKind::Ident { name, .. } => assert_eq!(name, "x"),
                    _ => panic!("Expected Ident"),
                }
            }
            _ => panic!("Expected Unary"),
        }
    }

    #[test]
    fn test_assignment() {
        let types = TypeTable::new();
        // x = 5
        let expr = Expr::assign(Expr::var("x"), Expr::int(5, &types), &types);

        match expr.kind {
            ExprKind::Assign { op, target, value } => {
                assert_eq!(op, AssignOp::Assign);
                match target.kind {
                    ExprKind::Ident { name, .. } => assert_eq!(name, "x"),
                    _ => panic!("Expected Ident"),
                }
                match value.kind {
                    ExprKind::IntLit(v) => assert_eq!(v, 5),
                    _ => panic!("Expected IntLit"),
                }
            }
            _ => panic!("Expected Assign"),
        }
    }

    #[test]
    fn test_function_call() {
        let types = TypeTable::new();
        // foo(1, 2)
        let expr = Expr::call(
            Expr::var("foo"),
            vec![Expr::int(1, &types), Expr::int(2, &types)],
            &types,
        );

        match expr.kind {
            ExprKind::Call { func, args } => {
                match func.kind {
                    ExprKind::Ident { name, .. } => assert_eq!(name, "foo"),
                    _ => panic!("Expected Ident"),
                }
                assert_eq!(args.len(), 2);
            }
            _ => panic!("Expected Call"),
        }
    }

    #[test]
    fn test_if_stmt() {
        let types = TypeTable::new();
        // if (x) return 1;
        let stmt = Stmt::If {
            cond: Expr::var("x"),
            then_stmt: Box::new(Stmt::Return(Some(Expr::int(1, &types)))),
            else_stmt: None,
        };

        match stmt {
            Stmt::If {
                cond,
                then_stmt,
                else_stmt,
            } => {
                match cond.kind {
                    ExprKind::Ident { name, .. } => assert_eq!(name, "x"),
                    _ => panic!("Expected Ident"),
                }
                match *then_stmt {
                    Stmt::Return(Some(ref e)) => match e.kind {
                        ExprKind::IntLit(1) => {}
                        _ => panic!("Expected IntLit(1)"),
                    },
                    _ => panic!("Expected Return"),
                }
                assert!(else_stmt.is_none());
            }
            _ => panic!("Expected If"),
        }
    }

    #[test]
    fn test_while_stmt() {
        // while (x) x--;
        let stmt = Stmt::While {
            cond: Expr::var("x"),
            body: Box::new(Stmt::Expr(Expr::new_unpositioned(ExprKind::PostDec(
                Box::new(Expr::var("x")),
            )))),
        };

        match stmt {
            Stmt::While { cond, body } => {
                match cond.kind {
                    ExprKind::Ident { name, .. } => assert_eq!(name, "x"),
                    _ => panic!("Expected Ident"),
                }
                match *body {
                    Stmt::Expr(ref e) => match e.kind {
                        ExprKind::PostDec(_) => {}
                        _ => panic!("Expected PostDec"),
                    },
                    _ => panic!("Expected Expr stmt"),
                }
            }
            _ => panic!("Expected While"),
        }
    }

    #[test]
    fn test_declaration() {
        let types = TypeTable::new();
        // int x = 5;
        let decl = Declaration::simple("x", types.int_id, Some(Expr::int(5, &types)));

        assert_eq!(decl.declarators.len(), 1);
        assert_eq!(decl.declarators[0].name, "x");
        assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Int);
        assert!(decl.declarators[0].init.is_some());
    }

    #[test]
    fn test_translation_unit() {
        let types = TypeTable::new();
        let mut tu = TranslationUnit::new();

        // Add a declaration
        let decl = Declaration::simple("x", types.int_id, None);
        tu.add(ExternalDecl::Declaration(decl));

        assert_eq!(tu.items.len(), 1);
    }

    #[test]
    fn test_for_loop() {
        let types = TypeTable::new();
        // for (int i = 0; i < 10; i++) {}
        let init = ForInit::Declaration(Declaration::simple(
            "i",
            types.int_id,
            Some(Expr::int(0, &types)),
        ));
        let cond = Expr::binary(BinaryOp::Lt, Expr::var("i"), Expr::int(10, &types), &types);
        let post = Expr::new_unpositioned(ExprKind::PostInc(Box::new(Expr::var("i"))));

        let stmt = Stmt::For {
            init: Some(init),
            cond: Some(cond),
            post: Some(post),
            body: Box::new(Stmt::Block(vec![])),
        };

        match stmt {
            Stmt::For {
                init,
                cond,
                post,
                body,
            } => {
                assert!(init.is_some());
                assert!(cond.is_some());
                assert!(post.is_some());
                match *body {
                    Stmt::Block(items) => assert!(items.is_empty()),
                    _ => panic!("Expected Block"),
                }
            }
            _ => panic!("Expected For"),
        }
    }
}
