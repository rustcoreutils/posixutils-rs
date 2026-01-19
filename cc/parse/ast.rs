//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Abstract Syntax Tree for pcc C99 compiler
// Type-annotated AST for C99 with expressions, statements, and declarations
//

use crate::diag::Position;
use crate::strings::StringId;
use crate::symbol::SymbolId;
use crate::types::{TypeId, TypeModifiers};

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

/// An expression with type annotation
///
/// Every expression carries its computed type, filled in during type evaluation
/// (after parsing, before linearization).
#[derive(Debug, Clone)]
pub struct Expr {
    /// The expression kind/variant
    pub kind: ExprKind,
    /// The computed type of this expression
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

    /// Wide string literal (L"...")
    WideStringLit(String),

    /// Identifier (variable reference)
    /// Symbol is None only for builtins like __func__ that need special handling
    Ident(SymbolId),

    /// Builtin identifier (__func__, __FUNCTION__, __PRETTY_FUNCTION__)
    /// These are handled specially by the linearizer
    FuncName,

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
        member: StringId,
    },

    /// Pointer member access: expr->member
    Arrow {
        expr: Box<Expr>,
        member: StringId,
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

    /// Compound literal: (type){ init-list }
    /// C99 6.5.2.5: Creates unnamed object with automatic/static storage
    CompoundLiteral {
        typ: TypeId,
        elements: Vec<InitElement>,
    },

    /// sizeof type: sizeof(int)
    SizeofType(TypeId),

    /// sizeof expression: sizeof expr
    SizeofExpr(Box<Expr>),

    /// _Alignof type: _Alignof(int) - C11
    AlignofType(TypeId),

    /// _Alignof expression: _Alignof expr - C11
    AlignofExpr(Box<Expr>),

    /// Comma expression: expr1, expr2
    Comma(Vec<Expr>),

    /// Initializer list: {1, 2, 3} or {.x = 1, [0] = 2}
    InitList {
        elements: Vec<InitElement>,
    },

    // =========================================================================
    // GNU Statement Expressions
    // =========================================================================
    /// Statement expression: ({ stmt; stmt; expr; })
    /// GNU extension that allows a compound statement to be used as an expression.
    /// The value is the result of the last expression statement.
    StmtExpr {
        /// The statements in the block (all but the last expression)
        stmts: Vec<BlockItem>,
        /// The result expression (value of the statement expression)
        result: Box<Expr>,
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
        last_param: StringId,
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

    // =========================================================================
    // Count trailing zeros builtins
    // =========================================================================
    /// __builtin_ctz(x)
    /// Returns the number of trailing 0-bits in x (32-bit unsigned int)
    /// Result is undefined if x is 0
    Ctz {
        /// The value to count trailing zeros in
        arg: Box<Expr>,
    },

    /// __builtin_ctzl(x)
    /// Returns the number of trailing 0-bits in x (unsigned long)
    /// Result is undefined if x is 0
    Ctzl {
        /// The value to count trailing zeros in
        arg: Box<Expr>,
    },

    /// __builtin_ctzll(x)
    /// Returns the number of trailing 0-bits in x (unsigned long long)
    /// Result is undefined if x is 0
    Ctzll {
        /// The value to count trailing zeros in
        arg: Box<Expr>,
    },

    // =========================================================================
    // Count leading zeros builtins
    // =========================================================================
    /// __builtin_clz(x)
    /// Returns the number of leading 0-bits in x (32-bit unsigned int)
    /// Result is undefined if x is 0
    Clz {
        /// The value to count leading zeros in
        arg: Box<Expr>,
    },

    /// __builtin_clzl(x)
    /// Returns the number of leading 0-bits in x (unsigned long)
    /// Result is undefined if x is 0
    Clzl {
        /// The value to count leading zeros in
        arg: Box<Expr>,
    },

    /// __builtin_clzll(x)
    /// Returns the number of leading 0-bits in x (unsigned long long)
    /// Result is undefined if x is 0
    Clzll {
        /// The value to count leading zeros in
        arg: Box<Expr>,
    },

    // =========================================================================
    // Population count builtins
    // =========================================================================
    /// __builtin_popcount(x)
    /// Returns the number of 1-bits in x (32-bit unsigned int)
    Popcount {
        /// The value to count set bits in
        arg: Box<Expr>,
    },

    /// __builtin_popcountl(x)
    /// Returns the number of 1-bits in x (unsigned long)
    Popcountl {
        /// The value to count set bits in
        arg: Box<Expr>,
    },

    /// __builtin_popcountll(x)
    /// Returns the number of 1-bits in x (unsigned long long)
    Popcountll {
        /// The value to count set bits in
        arg: Box<Expr>,
    },

    /// __builtin_alloca(size)
    /// Allocates size bytes on the stack, returns pointer
    Alloca {
        /// The size to allocate in bytes
        size: Box<Expr>,
    },

    // =========================================================================
    // Memory builtins - generate calls to C library functions
    // =========================================================================
    /// __builtin_memset(dest, c, n) - calls memset
    Memset {
        dest: Box<Expr>,
        c: Box<Expr>,
        n: Box<Expr>,
    },

    /// __builtin_memcpy(dest, src, n) - calls memcpy
    Memcpy {
        dest: Box<Expr>,
        src: Box<Expr>,
        n: Box<Expr>,
    },

    /// __builtin_memmove(dest, src, n) - calls memmove
    Memmove {
        dest: Box<Expr>,
        src: Box<Expr>,
        n: Box<Expr>,
    },

    // =========================================================================
    // Floating-point builtins
    // =========================================================================
    /// __builtin_fabs(x) - absolute value of double
    Fabs {
        arg: Box<Expr>,
    },

    /// __builtin_fabsf(x) - absolute value of float
    Fabsf {
        arg: Box<Expr>,
    },

    /// __builtin_fabsl(x) - absolute value of long double
    Fabsl {
        arg: Box<Expr>,
    },

    /// __builtin_signbit(x) - test sign bit of double, returns non-zero if negative
    Signbit {
        arg: Box<Expr>,
    },

    /// __builtin_signbitf(x) - test sign bit of float, returns non-zero if negative
    Signbitf {
        arg: Box<Expr>,
    },

    /// __builtin_signbitl(x) - test sign bit of long double, returns non-zero if negative
    Signbitl {
        arg: Box<Expr>,
    },

    // =========================================================================
    // Optimization hints
    // =========================================================================
    /// __builtin_unreachable()
    /// Indicates that this code path is never reached.
    /// If control flow reaches this point, behavior is undefined.
    /// Used for optimization hints and silencing warnings.
    Unreachable,

    /// __builtin_frame_address(level)
    /// Returns the frame pointer address at the given level.
    /// Level 0 is the current frame, 1 is the caller's frame, etc.
    FrameAddress {
        level: Box<Expr>,
    },

    /// __builtin_return_address(level)
    /// Returns the return address at the given level.
    /// Level 0 is the current function's return address.
    ReturnAddress {
        level: Box<Expr>,
    },

    // =========================================================================
    // setjmp/longjmp (non-local jumps)
    // =========================================================================
    /// setjmp(env)
    /// Saves the current execution context in env.
    /// Returns 0 on direct call, non-zero when returning via longjmp.
    Setjmp {
        /// The jmp_buf to save the context to
        env: Box<Expr>,
    },

    /// longjmp(env, val)
    /// Restores the execution context saved in env.
    /// Causes the corresponding setjmp to return val (or 1 if val == 0).
    /// This function never returns (noreturn).
    Longjmp {
        /// The jmp_buf containing the saved context
        env: Box<Expr>,
        /// The value to return from setjmp (1 if 0 is passed)
        val: Box<Expr>,
    },

    // =========================================================================
    // offsetof (structure member offset)
    // =========================================================================
    /// __builtin_offsetof(type, member-designator)
    /// Returns the offset in bytes of a member within a structure.
    /// The member-designator can be a chain like .x.y or .arr[0].field
    OffsetOf {
        /// The struct/union type
        type_id: TypeId,
        /// The member path (.field or [index] components)
        path: Vec<OffsetOfPath>,
    },

    // =========================================================================
    // Atomic builtins (Clang __c11_atomic_* for C11 stdatomic.h)
    // =========================================================================
    /// __c11_atomic_init(ptr, val)
    /// Initialize an atomic variable (no memory ordering)
    C11AtomicInit {
        /// Pointer to the atomic variable
        ptr: Box<Expr>,
        /// Initial value
        val: Box<Expr>,
    },

    /// __c11_atomic_load(ptr, order)
    /// Atomically loads and returns *ptr with given memory ordering
    C11AtomicLoad {
        /// Pointer to the atomic variable
        ptr: Box<Expr>,
        /// Memory ordering constant (0-5)
        order: Box<Expr>,
    },

    /// __c11_atomic_store(ptr, val, order)
    /// Atomically stores val to *ptr with given memory ordering
    C11AtomicStore {
        /// Pointer to the atomic variable
        ptr: Box<Expr>,
        /// Value to store
        val: Box<Expr>,
        /// Memory ordering constant (0-5)
        order: Box<Expr>,
    },

    /// __c11_atomic_exchange(ptr, val, order)
    /// Atomically replaces *ptr with val and returns the old value
    C11AtomicExchange {
        /// Pointer to the atomic variable
        ptr: Box<Expr>,
        /// Value to store
        val: Box<Expr>,
        /// Memory ordering constant (0-5)
        order: Box<Expr>,
    },

    /// __c11_atomic_compare_exchange_strong(ptr, expected, desired, succ_order, fail_order)
    /// Atomically compares *ptr with *expected; if equal, stores desired to *ptr,
    /// otherwise stores *ptr to *expected. Returns true if exchange happened.
    C11AtomicCompareExchangeStrong {
        /// Pointer to the atomic variable
        ptr: Box<Expr>,
        /// Pointer to expected value (updated on failure)
        expected: Box<Expr>,
        /// Desired value to store on success
        desired: Box<Expr>,
        /// Memory ordering on success
        succ_order: Box<Expr>,
    },

    /// __c11_atomic_compare_exchange_weak(ptr, expected, desired, succ_order, fail_order)
    /// Like strong version but may spuriously fail (implemented as strong)
    C11AtomicCompareExchangeWeak {
        /// Pointer to the atomic variable
        ptr: Box<Expr>,
        /// Pointer to expected value (updated on failure)
        expected: Box<Expr>,
        /// Desired value to store on success
        desired: Box<Expr>,
        /// Memory ordering on success
        succ_order: Box<Expr>,
    },

    /// __c11_atomic_fetch_add(ptr, val, order)
    /// Atomically adds val to *ptr and returns the old value
    C11AtomicFetchAdd {
        /// Pointer to the atomic variable
        ptr: Box<Expr>,
        /// Value to add
        val: Box<Expr>,
        /// Memory ordering constant (0-5)
        order: Box<Expr>,
    },

    /// __c11_atomic_fetch_sub(ptr, val, order)
    /// Atomically subtracts val from *ptr and returns the old value
    C11AtomicFetchSub {
        /// Pointer to the atomic variable
        ptr: Box<Expr>,
        /// Value to subtract
        val: Box<Expr>,
        /// Memory ordering constant (0-5)
        order: Box<Expr>,
    },

    /// __c11_atomic_fetch_and(ptr, val, order)
    /// Atomically ANDs val with *ptr and returns the old value
    C11AtomicFetchAnd {
        /// Pointer to the atomic variable
        ptr: Box<Expr>,
        /// Value to AND
        val: Box<Expr>,
        /// Memory ordering constant (0-5)
        order: Box<Expr>,
    },

    /// __c11_atomic_fetch_or(ptr, val, order)
    /// Atomically ORs val with *ptr and returns the old value
    C11AtomicFetchOr {
        /// Pointer to the atomic variable
        ptr: Box<Expr>,
        /// Value to OR
        val: Box<Expr>,
        /// Memory ordering constant (0-5)
        order: Box<Expr>,
    },

    /// __c11_atomic_fetch_xor(ptr, val, order)
    /// Atomically XORs val with *ptr and returns the old value
    C11AtomicFetchXor {
        /// Pointer to the atomic variable
        ptr: Box<Expr>,
        /// Value to XOR
        val: Box<Expr>,
        /// Memory ordering constant (0-5)
        order: Box<Expr>,
    },

    /// __c11_atomic_thread_fence(order)
    /// Synchronization fence with given memory ordering
    C11AtomicThreadFence {
        /// Memory ordering constant (0-5)
        order: Box<Expr>,
    },

    /// __c11_atomic_signal_fence(order)
    /// Signal handler fence (compiler barrier only)
    C11AtomicSignalFence {
        /// Memory ordering constant (0-5)
        order: Box<Expr>,
    },
}

// ============================================================================
// OffsetOf Support (__builtin_offsetof)
// ============================================================================

/// A path element in __builtin_offsetof(type, path)
/// Supports member.path[index].field style paths
#[derive(Debug, Clone)]
pub enum OffsetOfPath {
    /// Field access: .field_name
    Field(StringId),
    /// Array index: [constant_expr] - evaluated at parse time
    Index(i64),
}

// ============================================================================
// Initializer List Support (C99)
// ============================================================================

/// A designator for struct field or array index in an initializer
#[derive(Debug, Clone)]
pub enum Designator {
    /// Field designator: .field_name
    Field(StringId),
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

    /// Create a variable reference with resolved symbol (untyped) - no position
    pub fn var(symbol: SymbolId) -> Self {
        Expr::new_unpositioned(ExprKind::Ident(symbol))
    }

    /// Create a variable reference with resolved symbol and type - no position
    pub fn var_typed(symbol: SymbolId, typ: TypeId) -> Self {
        Expr::typed_unpositioned(ExprKind::Ident(symbol), typ)
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
// Inline Assembly Support (GCC Extended Asm)
// ============================================================================

/// An operand in an inline assembly statement
/// Format: [name] "constraint" (expr)
#[derive(Debug, Clone)]
pub struct AsmOperand {
    /// Optional symbolic name for the operand (e.g., [result])
    pub name: Option<StringId>,
    /// Constraint string (e.g., "=r", "+m", "r")
    pub constraint: String,
    /// The C expression (lvalue for outputs, rvalue for inputs)
    pub expr: Expr,
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
    Goto(StringId),

    /// Labeled statement: label: stmt
    Label { name: StringId, stmt: Box<Stmt> },

    /// Switch statement: switch (expr) { cases }
    Switch { expr: Expr, body: Box<Stmt> },

    /// Case label: case expr: (within switch body)
    Case(Expr),

    /// Default label (within switch body)
    Default,

    /// Inline assembly statement (GCC extended asm)
    /// Format: asm [volatile] [goto] ( "template" : outputs : inputs : clobbers [: goto_labels] );
    Asm {
        /// The assembly template string with %0, %1, etc. placeholders
        template: String,
        /// Output operands: [name] "=constraint" (lvalue)
        outputs: Vec<AsmOperand>,
        /// Input operands: [name] "constraint" (rvalue)
        inputs: Vec<AsmOperand>,
        /// Clobber list: registers and special values ("memory", "cc")
        clobbers: Vec<String>,
        /// Goto labels for asm goto (4th colon): labels the asm can jump to
        goto_labels: Vec<StringId>,
    },
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
    /// Resolved symbol ID (name available via symbols.get(symbol).name)
    pub symbol: SymbolId,
    /// The complete type (after applying declarator modifiers) - interned TypeId
    pub typ: TypeId,
    /// Storage class modifiers (extern, static, _Thread_local, etc.)
    /// These are NOT part of the type but affect code generation
    pub storage_class: TypeModifiers,
    /// Optional initializer
    pub init: Option<Expr>,
    /// For VLAs: runtime size expressions for each variable dimension
    /// Empty for fixed-size arrays or non-array types.
    /// For int arr[n][m], contains [n_expr, m_expr] (outer to inner order).
    pub vla_sizes: Vec<Expr>,
    /// Explicit alignment from _Alignas specifier (None = use natural alignment)
    pub explicit_align: Option<u32>,
}

#[cfg(test)]
impl Declaration {
    /// Create a simple declaration with one variable
    pub fn simple(symbol: SymbolId, typ: TypeId, init: Option<Expr>) -> Self {
        Declaration {
            declarators: vec![InitDeclarator {
                symbol,
                typ,
                storage_class: TypeModifiers::empty(),
                init,
                vla_sizes: vec![],
                explicit_align: None,
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
    /// Resolved symbol ID (None for unnamed params like in `void foo(int)`)
    /// Name available via symbols.get(symbol).name when Some
    pub symbol: Option<SymbolId>,
    /// Parameter type (interned TypeId)
    pub typ: TypeId,
}

/// A function definition
#[derive(Debug, Clone)]
pub struct FunctionDef {
    /// Return type (interned TypeId)
    pub return_type: TypeId,
    /// Function name
    pub name: StringId,
    /// Parameters
    pub params: Vec<Parameter>,
    /// Function body
    pub body: Stmt,
    /// Source position of function definition (for debug info)
    pub pos: Position,
    /// Whether function has static linkage
    pub is_static: bool,
    /// Whether function is inline
    pub is_inline: bool,
    /// Calling convention override (from __attribute__((sysv_abi)) etc.)
    pub calling_conv: crate::abi::CallingConv,
}

// ============================================================================
// Translation Unit
// ============================================================================

/// An external declaration (top-level item)
#[derive(Debug, Clone)]
#[allow(clippy::large_enum_variant)]
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
    use crate::strings::StringTable;
    use crate::symbol::{Symbol, SymbolTable};
    use crate::target::Target;
    use crate::types::TypeKind;

    #[test]
    fn test_int_literal() {
        let types = TypeTable::new(&Target::host());
        let expr = Expr::int(42, &types);
        match expr.kind {
            ExprKind::IntLit(v) => assert_eq!(v, 42),
            _ => panic!("Expected IntLit"),
        }
    }

    #[test]
    fn test_binary_expr() {
        let types = TypeTable::new(&Target::host());
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
        let types = TypeTable::new(&Target::host());
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
        let mut strings = StringTable::new();
        let types = TypeTable::new(&Target::host());
        let mut symbols = SymbolTable::new();
        let x_name = strings.intern("x");
        let x_sym = symbols
            .declare(Symbol::variable(x_name, types.int_id, 0))
            .unwrap();
        // -x
        let expr = Expr::unary(UnaryOp::Neg, Expr::var(x_sym), &types);

        match expr.kind {
            ExprKind::Unary { op, operand } => {
                assert_eq!(op, UnaryOp::Neg);
                match operand.kind {
                    ExprKind::Ident(sym_id) => assert_eq!(sym_id, x_sym),
                    _ => panic!("Expected Ident"),
                }
            }
            _ => panic!("Expected Unary"),
        }
    }

    #[test]
    fn test_assignment() {
        let mut strings = StringTable::new();
        let types = TypeTable::new(&Target::host());
        let mut symbols = SymbolTable::new();
        let x_name = strings.intern("x");
        let x_sym = symbols
            .declare(Symbol::variable(x_name, types.int_id, 0))
            .unwrap();
        // x = 5
        let expr = Expr::assign(Expr::var(x_sym), Expr::int(5, &types), &types);

        match expr.kind {
            ExprKind::Assign { op, target, value } => {
                assert_eq!(op, AssignOp::Assign);
                match target.kind {
                    ExprKind::Ident(sym_id) => assert_eq!(sym_id, x_sym),
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
        let mut strings = StringTable::new();
        let types = TypeTable::new(&Target::host());
        let mut symbols = SymbolTable::new();
        let foo_name = strings.intern("foo");
        let foo_sym = symbols
            .declare(Symbol::variable(foo_name, types.int_id, 0))
            .unwrap();
        // foo(1, 2)
        let expr = Expr::call(
            Expr::var(foo_sym),
            vec![Expr::int(1, &types), Expr::int(2, &types)],
            &types,
        );

        match expr.kind {
            ExprKind::Call { func, args } => {
                match func.kind {
                    ExprKind::Ident(sym_id) => assert_eq!(sym_id, foo_sym),
                    _ => panic!("Expected Ident"),
                }
                assert_eq!(args.len(), 2);
            }
            _ => panic!("Expected Call"),
        }
    }

    #[test]
    fn test_if_stmt() {
        let mut strings = StringTable::new();
        let types = TypeTable::new(&Target::host());
        let mut symbols = SymbolTable::new();
        let x_name = strings.intern("x");
        let x_sym = symbols
            .declare(Symbol::variable(x_name, types.int_id, 0))
            .unwrap();
        // if (x) return 1;
        let stmt = Stmt::If {
            cond: Expr::var(x_sym),
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
                    ExprKind::Ident(sym_id) => assert_eq!(sym_id, x_sym),
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
        let mut strings = StringTable::new();
        let types = TypeTable::new(&Target::host());
        let mut symbols = SymbolTable::new();
        let x_name = strings.intern("x");
        let x_sym = symbols
            .declare(Symbol::variable(x_name, types.int_id, 0))
            .unwrap();
        // while (x) x--;
        let stmt = Stmt::While {
            cond: Expr::var(x_sym),
            body: Box::new(Stmt::Expr(Expr::new_unpositioned(ExprKind::PostDec(
                Box::new(Expr::var(x_sym)),
            )))),
        };

        match stmt {
            Stmt::While { cond, body } => {
                match cond.kind {
                    ExprKind::Ident(sym_id) => assert_eq!(sym_id, x_sym),
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
        let mut strings = StringTable::new();
        let types = TypeTable::new(&Target::host());
        let mut symbols = SymbolTable::new();
        let x_name = strings.intern("x");
        let x_sym = symbols
            .declare(Symbol::variable(x_name, types.int_id, 0))
            .unwrap();
        // int x = 5;
        let decl = Declaration::simple(x_sym, types.int_id, Some(Expr::int(5, &types)));

        assert_eq!(decl.declarators.len(), 1);
        assert_eq!(decl.declarators[0].symbol, x_sym);
        assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Int);
        assert!(decl.declarators[0].init.is_some());
    }

    #[test]
    fn test_translation_unit() {
        let mut strings = StringTable::new();
        let types = TypeTable::new(&Target::host());
        let mut symbols = SymbolTable::new();
        let x_name = strings.intern("x");
        let x_sym = symbols
            .declare(Symbol::variable(x_name, types.int_id, 0))
            .unwrap();
        let mut tu = TranslationUnit::new();

        // Add a declaration
        let decl = Declaration::simple(x_sym, types.int_id, None);
        tu.add(ExternalDecl::Declaration(decl));

        assert_eq!(tu.items.len(), 1);
    }

    #[test]
    fn test_for_loop() {
        let mut strings = StringTable::new();
        let types = TypeTable::new(&Target::host());
        let mut symbols = SymbolTable::new();
        let i_name = strings.intern("i");
        let i_sym = symbols
            .declare(Symbol::variable(i_name, types.int_id, 0))
            .unwrap();
        // for (int i = 0; i < 10; i++) {}
        let init = ForInit::Declaration(Declaration::simple(
            i_sym,
            types.int_id,
            Some(Expr::int(0, &types)),
        ));
        let cond = Expr::binary(
            BinaryOp::Lt,
            Expr::var(i_sym),
            Expr::int(10, &types),
            &types,
        );
        let post = Expr::new_unpositioned(ExprKind::PostInc(Box::new(Expr::var(i_sym))));

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

    #[test]
    fn test_fabs_builtins() {
        let types = TypeTable::new(&Target::host());

        // Test Fabs (double)
        let arg = Expr::typed_unpositioned(ExprKind::FloatLit(1.5), types.double_id);
        let fabs = Expr::new_unpositioned(ExprKind::Fabs { arg: Box::new(arg) });
        match fabs.kind {
            ExprKind::Fabs { arg } => {
                assert!(matches!(arg.kind, ExprKind::FloatLit(_)));
            }
            _ => panic!("Expected Fabs"),
        }

        // Test Fabsf (float)
        let arg = Expr::typed_unpositioned(ExprKind::FloatLit(2.5), types.float_id);
        let fabsf = Expr::new_unpositioned(ExprKind::Fabsf { arg: Box::new(arg) });
        match fabsf.kind {
            ExprKind::Fabsf { arg } => {
                assert!(matches!(arg.kind, ExprKind::FloatLit(_)));
            }
            _ => panic!("Expected Fabsf"),
        }

        // Test Fabsl (long double)
        let arg = Expr::typed_unpositioned(ExprKind::FloatLit(3.5), types.longdouble_id);
        let fabsl = Expr::new_unpositioned(ExprKind::Fabsl { arg: Box::new(arg) });
        match fabsl.kind {
            ExprKind::Fabsl { arg } => {
                assert!(matches!(arg.kind, ExprKind::FloatLit(_)));
            }
            _ => panic!("Expected Fabsl"),
        }
    }

    #[test]
    fn test_c11_atomic_init() {
        let types = TypeTable::new(&Target::host());
        let ptr = Expr::int(0x1000, &types); // dummy pointer
        let val = Expr::int(42, &types);

        let init = Expr::new_unpositioned(ExprKind::C11AtomicInit {
            ptr: Box::new(ptr),
            val: Box::new(val),
        });

        match init.kind {
            ExprKind::C11AtomicInit { ptr, val } => {
                assert!(matches!(ptr.kind, ExprKind::IntLit(0x1000)));
                assert!(matches!(val.kind, ExprKind::IntLit(42)));
            }
            _ => panic!("Expected C11AtomicInit"),
        }
    }

    #[test]
    fn test_c11_atomic_load_store() {
        let types = TypeTable::new(&Target::host());
        let ptr = Expr::int(0x1000, &types);
        let val = Expr::int(42, &types);
        let order = Expr::int(5, &types); // seq_cst

        // Test AtomicLoad
        let load = Expr::new_unpositioned(ExprKind::C11AtomicLoad {
            ptr: Box::new(ptr.clone()),
            order: Box::new(order.clone()),
        });
        match load.kind {
            ExprKind::C11AtomicLoad { ptr, order } => {
                assert!(matches!(ptr.kind, ExprKind::IntLit(0x1000)));
                assert!(matches!(order.kind, ExprKind::IntLit(5)));
            }
            _ => panic!("Expected C11AtomicLoad"),
        }

        // Test AtomicStore
        let store = Expr::new_unpositioned(ExprKind::C11AtomicStore {
            ptr: Box::new(ptr),
            val: Box::new(val),
            order: Box::new(order),
        });
        match store.kind {
            ExprKind::C11AtomicStore { ptr, val, order } => {
                assert!(matches!(ptr.kind, ExprKind::IntLit(0x1000)));
                assert!(matches!(val.kind, ExprKind::IntLit(42)));
                assert!(matches!(order.kind, ExprKind::IntLit(5)));
            }
            _ => panic!("Expected C11AtomicStore"),
        }
    }

    #[test]
    fn test_c11_atomic_exchange() {
        let types = TypeTable::new(&Target::host());
        let ptr = Expr::int(0x1000, &types);
        let val = Expr::int(42, &types);
        let order = Expr::int(5, &types);

        let xchg = Expr::new_unpositioned(ExprKind::C11AtomicExchange {
            ptr: Box::new(ptr),
            val: Box::new(val),
            order: Box::new(order),
        });

        match xchg.kind {
            ExprKind::C11AtomicExchange { ptr, val, order } => {
                assert!(matches!(ptr.kind, ExprKind::IntLit(0x1000)));
                assert!(matches!(val.kind, ExprKind::IntLit(42)));
                assert!(matches!(order.kind, ExprKind::IntLit(5)));
            }
            _ => panic!("Expected C11AtomicExchange"),
        }
    }

    #[test]
    fn test_c11_atomic_compare_exchange() {
        let types = TypeTable::new(&Target::host());
        let ptr = Expr::int(0x1000, &types);
        let expected = Expr::int(0x2000, &types);
        let desired = Expr::int(42, &types);
        let succ_order = Expr::int(5, &types);

        // Test strong variant
        let cas_strong = Expr::new_unpositioned(ExprKind::C11AtomicCompareExchangeStrong {
            ptr: Box::new(ptr.clone()),
            expected: Box::new(expected.clone()),
            desired: Box::new(desired.clone()),
            succ_order: Box::new(succ_order.clone()),
        });
        match cas_strong.kind {
            ExprKind::C11AtomicCompareExchangeStrong {
                ptr,
                expected,
                desired,
                succ_order,
            } => {
                assert!(matches!(ptr.kind, ExprKind::IntLit(0x1000)));
                assert!(matches!(expected.kind, ExprKind::IntLit(0x2000)));
                assert!(matches!(desired.kind, ExprKind::IntLit(42)));
                assert!(matches!(succ_order.kind, ExprKind::IntLit(5)));
            }
            _ => panic!("Expected C11AtomicCompareExchangeStrong"),
        }

        // Test weak variant
        let cas_weak = Expr::new_unpositioned(ExprKind::C11AtomicCompareExchangeWeak {
            ptr: Box::new(ptr),
            expected: Box::new(expected),
            desired: Box::new(desired),
            succ_order: Box::new(succ_order),
        });
        match cas_weak.kind {
            ExprKind::C11AtomicCompareExchangeWeak {
                ptr,
                expected,
                desired,
                succ_order,
            } => {
                assert!(matches!(ptr.kind, ExprKind::IntLit(0x1000)));
                assert!(matches!(expected.kind, ExprKind::IntLit(0x2000)));
                assert!(matches!(desired.kind, ExprKind::IntLit(42)));
                assert!(matches!(succ_order.kind, ExprKind::IntLit(5)));
            }
            _ => panic!("Expected C11AtomicCompareExchangeWeak"),
        }
    }

    #[test]
    fn test_c11_atomic_fetch_ops() {
        let types = TypeTable::new(&Target::host());
        let ptr = Expr::int(0x1000, &types);
        let val = Expr::int(10, &types);
        let order = Expr::int(5, &types);

        // Test FetchAdd
        let fetch_add = Expr::new_unpositioned(ExprKind::C11AtomicFetchAdd {
            ptr: Box::new(ptr.clone()),
            val: Box::new(val.clone()),
            order: Box::new(order.clone()),
        });
        assert!(matches!(fetch_add.kind, ExprKind::C11AtomicFetchAdd { .. }));

        // Test FetchSub
        let fetch_sub = Expr::new_unpositioned(ExprKind::C11AtomicFetchSub {
            ptr: Box::new(ptr.clone()),
            val: Box::new(val.clone()),
            order: Box::new(order.clone()),
        });
        assert!(matches!(fetch_sub.kind, ExprKind::C11AtomicFetchSub { .. }));

        // Test FetchAnd
        let fetch_and = Expr::new_unpositioned(ExprKind::C11AtomicFetchAnd {
            ptr: Box::new(ptr.clone()),
            val: Box::new(val.clone()),
            order: Box::new(order.clone()),
        });
        assert!(matches!(fetch_and.kind, ExprKind::C11AtomicFetchAnd { .. }));

        // Test FetchOr
        let fetch_or = Expr::new_unpositioned(ExprKind::C11AtomicFetchOr {
            ptr: Box::new(ptr.clone()),
            val: Box::new(val.clone()),
            order: Box::new(order.clone()),
        });
        assert!(matches!(fetch_or.kind, ExprKind::C11AtomicFetchOr { .. }));

        // Test FetchXor
        let fetch_xor = Expr::new_unpositioned(ExprKind::C11AtomicFetchXor {
            ptr: Box::new(ptr),
            val: Box::new(val),
            order: Box::new(order),
        });
        assert!(matches!(fetch_xor.kind, ExprKind::C11AtomicFetchXor { .. }));
    }

    #[test]
    fn test_c11_atomic_fences() {
        let types = TypeTable::new(&Target::host());
        let order = Expr::int(5, &types); // seq_cst

        // Test thread fence
        let thread_fence = Expr::new_unpositioned(ExprKind::C11AtomicThreadFence {
            order: Box::new(order.clone()),
        });
        match thread_fence.kind {
            ExprKind::C11AtomicThreadFence { order } => {
                assert!(matches!(order.kind, ExprKind::IntLit(5)));
            }
            _ => panic!("Expected C11AtomicThreadFence"),
        }

        // Test signal fence
        let signal_fence = Expr::new_unpositioned(ExprKind::C11AtomicSignalFence {
            order: Box::new(order),
        });
        match signal_fence.kind {
            ExprKind::C11AtomicSignalFence { order } => {
                assert!(matches!(order.kind, ExprKind::IntLit(5)));
            }
            _ => panic!("Expected C11AtomicSignalFence"),
        }
    }
}
