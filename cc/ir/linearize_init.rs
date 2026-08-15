//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT

//! Initializer and global declaration linearization

use super::linearize::*;
use super::Initializer;
use crate::diag::error;
use crate::float::FloatVal;
use crate::parse::ast::{BinaryOp, Declaration, Designator, Expr, ExprKind, InitElement, UnaryOp};
use crate::strings::StringId;
use crate::token::lexer::Position;
use crate::types::{MemberInfo, TypeId, TypeKind, TypeModifiers, TypeTable};
use std::collections::{BTreeMap, HashMap};

/// Determine whether a declared object type is `const`-qualified for the
/// purpose of section selection.
///
/// For scalars/pointers/structs the top-level CONST modifier is decisive.
/// For arrays, C semantics put the qualifier on the element type (e.g.
/// `const int a[10]` declares an array of const int), but the array as
/// a whole must still be treated as read-only. We therefore look through
/// nested array types until we hit a non-array type.
pub(crate) fn is_const_object_type(types: &TypeTable, typ: TypeId) -> bool {
    let mut cur = typ;
    loop {
        if types.modifiers(cur).contains(TypeModifiers::CONST) {
            return true;
        }
        if types.kind(cur) == TypeKind::Array {
            if let Some(base) = types.base_type(cur) {
                cur = base;
                continue;
            }
        }
        return false;
    }
}

/// Name an expression the way a C programmer would, for a diagnostic.
///
/// The alternative these messages used was `{:?}` on the AST node, which
/// prints interned ids, `FloatVal` internals and a `Position` struct for every
/// subexpression -- pages of compiler internals for a one-line mistake.
fn describe_expr(kind: &ExprKind) -> &'static str {
    match kind {
        ExprKind::Call { .. } => "a function call",
        ExprKind::Assign { .. } => "an assignment",
        ExprKind::Unary {
            op: UnaryOp::PreInc | UnaryOp::PreDec,
            ..
        }
        | ExprKind::PostInc(_)
        | ExprKind::PostDec(_) => "an increment or decrement",
        ExprKind::Unary {
            op: UnaryOp::Deref, ..
        } => "a pointer dereference",
        ExprKind::Index { .. } => "an array subscript",
        ExprKind::Member { .. } => "a member access",
        ExprKind::Ident(_) => "the value of a variable",
        ExprKind::Comma { .. } => "a comma expression",
        ExprKind::Binary { .. } => "this arithmetic",
        ExprKind::Conditional { .. } => "this conditional",
        _ => "this expression",
    }
}

impl<'a> super::linearize::Linearizer<'a> {
    // ========================================================================
    // Global declarations
    // ========================================================================

    pub(crate) fn linearize_global_decl(&mut self, decl: &Declaration) {
        for declarator in &decl.declarators {
            // Use storage_class from declarator (extern, static, _Thread_local, etc.)
            // These are NOT stored in the type system
            let storage_class = declarator.storage_class;
            let name = self.symbol_name(declarator.symbol);

            // Skip typedef declarations - they don't define storage
            if storage_class.contains(TypeModifiers::TYPEDEF) {
                continue;
            }

            // Function declarations without bodies are external functions
            // Track them in extern_symbols so codegen uses GOT access
            if self.types.kind(declarator.typ) == TypeKind::Function {
                // Check if not defined in this module (forward refs will be cleaned up later)
                if !self.module.functions.iter().any(|f| f.name == name) {
                    self.module.extern_symbols.insert(name);
                }
                continue;
            }

            // Skip extern declarations - they don't define storage
            // But track them so codegen can use GOT access on macOS
            // Only add to extern_symbols if not already defined (handles both cases:
            // extern int x; int x = 1;  - x is defined, not extern
            // int x = 1; extern int x;  - x is defined, not extern)
            if storage_class.contains(TypeModifiers::EXTERN) {
                // Check if this symbol is already defined in globals
                if !self.module.globals.iter().any(|g| g.name == name) {
                    self.module.extern_symbols.insert(name.clone());
                    // Track extern thread-local symbols separately for TLS access
                    if storage_class.contains(TypeModifiers::THREAD_LOCAL) {
                        self.module.extern_tls_symbols.insert(name);
                    }
                }
                continue;
            }

            let init = declarator.init.as_ref().map_or(Initializer::None, |e| {
                self.ast_init_to_ir(e, declarator.typ)
            });

            // Track file-scope static variables for inline semantic checks
            if storage_class.contains(TypeModifiers::STATIC) {
                self.file_scope_statics.insert(name.clone());
            }

            // If this symbol was previously declared extern, remove it from extern_symbols
            // (we now have the actual definition)
            self.module.extern_symbols.remove(&name);

            // Check for thread-local storage
            let is_static = storage_class.contains(TypeModifiers::STATIC);
            // Const-qualified at the object level. For arrays, the element type
            // carries the qualifier (e.g., `const int a[10]`), so look through
            // arrays to their element type.
            let is_const = is_const_object_type(self.types, declarator.typ);
            if storage_class.contains(TypeModifiers::THREAD_LOCAL) {
                self.module.add_global_tls_aligned(
                    &name,
                    declarator.typ,
                    init,
                    declarator.explicit_align,
                    is_static,
                    is_const,
                );
            } else {
                self.module.add_global_aligned(
                    &name,
                    declarator.typ,
                    init,
                    declarator.explicit_align,
                    is_static,
                    is_const,
                );
            }
            self.module
                .set_symbol_attrs(&name, declarator.symbol_attrs.clone());
        }
    }

    /// Convert an AST initializer expression to an IR Initializer
    ///
    /// This handles:
    /// - Scalar initializers (int, float, char literals)
    /// - String literals (for char arrays or char pointers)
    /// - Array initializers with designated and positional elements
    /// - Struct initializers with designated and positional fields
    /// - Address-of expressions (&symbol)
    /// - Nested initializers
    /// - Compound literals (C99 6.5.2.5)
    pub(crate) fn ast_init_to_ir(&mut self, expr: &Expr, typ: TypeId) -> Initializer {
        // An object of complex type needs both halves, whatever shape the
        // initializer takes, so it is handled before the by-expression arms
        // below -- several of which would otherwise match and keep only the
        // real part.
        if self.types.is_complex(typ) {
            if let Some(init) = self.complex_initializer(expr, typ) {
                return init;
            }
        }

        // An arithmetic object is initialized with the *object's* encoding,
        // whatever the constant's own type is (C17 6.7.9p11: the initializer
        // is converted as in assignment). `int c = 1.0 + 2.0;` stores 3, not
        // the IEEE bits of 3.0, and `double d = 1 + 2;` stores 3.0, not the
        // integer 3. Deciding that here, from the type, keeps every expression
        // arm below from having to decide it again -- and differently.
        if self.types.is_integer(typ) || self.types.is_float(typ) {
            if let Some(init) = self.fold_scalar_init(expr, typ) {
                return init;
            }
        }

        match &expr.kind {
            ExprKind::IntLit(v) => Initializer::Int(*v as i128),
            ExprKind::Int128Lit(v) => Initializer::Int(*v),
            ExprKind::FloatLit(v) => Initializer::Float(*v),
            ExprKind::CharLit(c) => Initializer::Int(*c as u8 as i8 as i128),

            // String literal - for arrays, store as String; for pointers, create label reference
            ExprKind::StringLit(s) => {
                let type_kind = self.types.kind(typ);
                if type_kind == TypeKind::Array {
                    // char array - embed the string directly
                    Initializer::String(s.clone())
                } else {
                    // Pointer - create a string constant and reference it
                    Initializer::SymAddr(self.module.add_string(s.clone()))
                }
            }

            // Wide string literal - for arrays, store as WideString; for pointers, create label reference
            ExprKind::WideStringLit(s) => {
                let type_kind = self.types.kind(typ);
                if type_kind == TypeKind::Array {
                    // wchar_t array - embed the wide string directly
                    Initializer::WideString(s.clone())
                } else {
                    // Pointer - create a wide string constant and reference it
                    Initializer::SymAddr(self.module.add_wide_string(s.clone()))
                }
            }

            // char16_t / char32_t string literals, same shape as the wide
            // case: embedded for an array, interned and referenced otherwise.
            ExprKind::Utf16StringLit(units) => {
                if self.types.kind(typ) == TypeKind::Array {
                    Initializer::Utf16String(units.clone())
                } else {
                    let label = self.module.add_utf16_string(units.clone());
                    Initializer::SymAddr(label)
                }
            }

            ExprKind::Utf32StringLit(units) => {
                if self.types.kind(typ) == TypeKind::Array {
                    Initializer::Utf32String(units.clone())
                } else {
                    let label = self.module.add_utf32_string(units.clone());
                    Initializer::SymAddr(label)
                }
            }

            // Negative literal (fast path for simple cases)
            ExprKind::Unary {
                op: UnaryOp::Neg,
                operand,
            } => match &operand.kind {
                ExprKind::IntLit(v) => Initializer::Int(-(*v as i128)),
                ExprKind::Int128Lit(v) => Initializer::Int(v.wrapping_neg()),
                ExprKind::FloatLit(v) => Initializer::Float(v.negated()),
                // For more complex expressions like -(1+2), try constant evaluation
                _ => {
                    // An arithmetic object folded above, at its own type; what
                    // is left is a negation initializing something else.
                    if let Some(val) = self.eval_const_expr(expr) {
                        Initializer::Int(val)
                    } else {
                        // Returning `Initializer::None` here would put the
                        // object in .bss and make it silently zero -- which is
                        // what `-(1.0 + 2.0)` used to do.
                        self.reject_initializer(expr);
                        Initializer::None
                    }
                }
            },

            // Address-of expression
            ExprKind::Unary {
                op: UnaryOp::AddrOf,
                operand,
            } => {
                // Try to compute the address as symbol + offset
                if let Some((name, offset)) = self.static_address_of(operand) {
                    if offset == 0 {
                        Initializer::SymAddr(name)
                    } else {
                        Initializer::SymAddrOffset(name, offset)
                    }
                } else if let Some(val) = self.eval_const_expr(expr) {
                    // Not every address-of is a relocation: the address of a
                    // member of a null pointer is an integer constant, and a
                    // pointer object may be initialized with one.
                    Initializer::Int(val)
                } else {
                    // Returning `Initializer::None` here would put the object
                    // in .bss and make the pointer null, which is what made
                    // `&(struct P){1, 2}` segfault rather than fail to build.
                    self.reject_initializer(expr);
                    Initializer::None
                }
            }

            // Cast expression - evaluate the inner expression
            ExprKind::Cast { expr: inner, .. } => self.ast_init_to_ir(inner, typ),

            // Initializer list for arrays/structs
            ExprKind::InitList { elements } => self.ast_init_list_to_ir(elements, typ),

            // Compound literal in initializer context (C99 6.5.2.5)
            ExprKind::CompoundLiteral {
                typ: cl_type,
                elements,
            } => {
                // Check if compound literal type matches target type
                if *cl_type == typ {
                    // Direct value - treat like InitList
                    self.ast_init_list_to_ir(elements, typ)
                } else if self.types.kind(typ) == TypeKind::Pointer {
                    // Pointer initialization - create anonymous static global
                    // and return its address
                    let anon_name = format!(".CL{}", self.compound_literal_counter);
                    self.compound_literal_counter += 1;

                    // Create the anonymous global
                    let init = self.ast_init_list_to_ir(elements, *cl_type);
                    self.module.add_global(&anon_name, *cl_type, init);

                    // Return address of the anonymous global
                    Initializer::SymAddr(anon_name)
                } else {
                    // Type mismatch - use the compound literal's own type
                    self.ast_init_list_to_ir(elements, *cl_type)
                }
            }

            // Identifier - for constant addresses (function pointers, array decay, etc.)
            // or enum constants
            ExprKind::Ident(symbol_id) => {
                let type_kind = self.types.kind(typ);
                // For pointer types, this is likely a function address or array decay
                if type_kind == TypeKind::Pointer {
                    let name_str = self.symbol_name(*symbol_id);
                    // Check if this is a static local variable
                    // Static locals have mangled names like "func_name.var_name.N"
                    let key = format!("{}.{}", self.current_func_name, name_str);
                    if let Some(static_info) = self.static_locals.get(&key) {
                        Initializer::SymAddr(static_info.global_name.clone())
                    } else {
                        Initializer::SymAddr(name_str)
                    }
                } else {
                    // Check if it's an enum constant
                    let sym = self.symbols.get(*symbol_id);
                    if let Some(val) = sym.enum_value {
                        Initializer::Int(val as i128)
                    } else {
                        Initializer::None
                    }
                }
            }

            // Binary add/sub with pointer operand → SymAddrOffset
            ExprKind::Binary {
                op: op @ (BinaryOp::Add | BinaryOp::Sub),
                left,
                right,
            } => {
                // Try pointer/array + int or int + pointer/array → symbol address with offset
                let is_ptr_or_array =
                    |t: TypeId| matches!(self.types.kind(t), TypeKind::Pointer | TypeKind::Array);
                let (ptr_expr, int_expr, is_sub) = if left.typ.is_some_and(is_ptr_or_array) {
                    (left.as_ref(), right.as_ref(), *op == BinaryOp::Sub)
                } else if right.typ.is_some_and(is_ptr_or_array) && *op == BinaryOp::Add {
                    (right.as_ref(), left.as_ref(), false)
                } else {
                    // Neither operand is a pointer, so this is ordinary
                    // arithmetic that happens to use `+` or `-`. An arithmetic
                    // object folded above at its own type; anything else that
                    // reaches here can only be an integer constant.
                    if let Some(val) = self.eval_const_expr(expr) {
                        return Initializer::Int(val);
                    }
                    self.reject_initializer(expr);
                    return Initializer::None;
                };

                // Evaluate the pointer side as a static address
                if let Some((name, base_off)) = self.static_address_of(ptr_expr) {
                    // Evaluate the integer side as a constant
                    if let Some(int_val) = self.eval_const_expr(int_expr) {
                        // Get the pointee size for pointer arithmetic scaling
                        let pointee_size = ptr_expr
                            .typ
                            .and_then(|t| self.types.base_type(t))
                            .map(|t| self.types.size_bytes(t) as i64)
                            .unwrap_or(1);
                        let byte_offset = if is_sub {
                            base_off - int_val as i64 * pointee_size
                        } else {
                            base_off + int_val as i64 * pointee_size
                        };
                        if byte_offset == 0 {
                            Initializer::SymAddr(name)
                        } else {
                            Initializer::SymAddrOffset(name, byte_offset)
                        }
                    } else if let Some(val) = self.eval_const_expr(expr) {
                        Initializer::Int(val)
                    } else {
                        error(
                            self.current_pos.unwrap_or_default(),
                            "non-constant offset in pointer arithmetic initializer",
                        );
                        Initializer::None
                    }
                } else if let Some(val) = self.eval_const_expr(expr) {
                    Initializer::Int(val)
                } else {
                    error(
                        self.current_pos.unwrap_or_default(),
                        "non-constant pointer expression in global initializer",
                    );
                    Initializer::None
                }
            }

            // Compile-time ternary: cond ? then_expr : else_expr
            // Used in CPython's _Py_LATIN1_CHR() macro for static initializers
            ExprKind::Conditional {
                cond,
                then_expr,
                else_expr,
            } => {
                if let Some(cond_val) = self.eval_const_expr(cond) {
                    if cond_val != 0 {
                        return self.ast_init_to_ir(then_expr, typ);
                    } else {
                        return self.ast_init_to_ir(else_expr, typ);
                    }
                }
                // If condition isn't constant, fall through to error
                error(
                    self.current_pos.unwrap_or_default(),
                    &format!(
                        "non-constant condition in global initializer ternary: {:?}",
                        cond.kind
                    ),
                );
                Initializer::None
            }

            // Other constant expressions
            // Try to evaluate as integer or float constant expression
            _ => {
                // An arithmetic object folded above, at its own type.
                if let Some(val) = self.eval_const_expr(expr) {
                    Initializer::Int(val)
                } else if let Some((name, offset)) = self.eval_static_address(expr) {
                    // Try as a static address (e.g., &global.field->subfield chains)
                    if offset != 0 {
                        Initializer::SymAddrOffset(name, offset)
                    } else {
                        Initializer::SymAddr(name)
                    }
                } else {
                    // Hard error for non-empty expressions we can't evaluate
                    self.reject_initializer(expr);
                    Initializer::None
                }
            }
        }
    }

    /// The static address of `expr`, as a symbol name and byte offset.
    ///
    /// Wraps `eval_static_address` to also cover a string literal, which has a
    /// perfectly good static address but only once it has been interned and
    /// given a label -- and interning needs `&mut self`, which the `&self`
    /// evaluator cannot do. Without this `const char *p = "hello" + 1;` was
    /// rejected, while `arr + 1` on a static array was accepted.
    fn static_address_of(&mut self, expr: &Expr) -> Option<(String, i64)> {
        // A compound literal at file scope has static storage duration
        // (C99 6.5.2.5p5), so it is an object with an address -- but it only
        // acquires one when it is given a name here.
        if let ExprKind::CompoundLiteral { typ, elements } = &expr.kind {
            let name = format!(".CL{}", self.compound_literal_counter);
            self.compound_literal_counter += 1;
            let typ = *typ;
            let init = self.ast_init_list_to_ir(elements, typ);
            self.module.add_global(&name, typ, init);
            return Some((name, 0));
        }
        if let ExprKind::StringLit(lit) = &expr.kind {
            return Some((self.module.add_string(lit.clone()), 0));
        }
        self.eval_static_address(expr)
    }

    /// Fold a constant expression of complex type into its two halves.
    ///
    /// Returns `None` for anything that is not a constant, so callers can fall
    /// through to their existing diagnostics.
    ///
    /// A complex constant is two real ones, and each half is carried the way
    /// [`Self::eval_const_float_expr`] carries a real constant: a literal at
    /// its full declared width, arithmetic folded through `f64`. Halving the
    /// precision of a `long double _Complex` literal here, while the real path
    /// keeps it, would make the two disagree about the same written value.
    fn eval_const_complex(&self, expr: &Expr) -> Option<(FloatVal, FloatVal)> {
        match &expr.kind {
            // `I` itself is `__builtin_complex(0.0, 1.0)`.
            ExprKind::BuiltinComplex { real, imag } => Some((
                self.eval_const_float_expr(real)?,
                self.eval_const_float_expr(imag)?,
            )),

            // A real constant is a complex one with a zero imaginary part.
            ExprKind::FloatLit(_) | ExprKind::IntLit(_) | ExprKind::CharLit(_) => {
                Some((self.eval_const_float_expr(expr)?, FloatVal::ZERO))
            }

            ExprKind::Cast { expr: inner, .. } => self.eval_const_complex(inner),

            ExprKind::Unary {
                op: UnaryOp::Neg,
                operand,
            } => {
                let (re, im) = self.eval_const_complex(operand)?;
                Some((re.negated(), im.negated()))
            }

            ExprKind::Binary { op, left, right } => {
                let (a, b) = self.eval_const_complex(left)?;
                let (c, d) = self.eval_const_complex(right)?;
                let (a, b, c, d) = (a.to_f64(), b.to_f64(), c.to_f64(), d.to_f64());
                let (re, im) = match op {
                    BinaryOp::Add => (a + c, b + d),
                    BinaryOp::Sub => (a - c, b - d),
                    BinaryOp::Mul => (a * c - b * d, a * d + b * c),
                    BinaryOp::Div => {
                        let den = c * c + d * d;
                        if den == 0.0 {
                            return None;
                        }
                        ((a * c + b * d) / den, (b * c - a * d) / den)
                    }
                    _ => return None,
                };
                Some((FloatVal::from_f64(re), FloatVal::from_f64(im)))
            }

            _ => None,
        }
    }

    /// Build the initializer for an object of complex type.
    ///
    /// A complex value is two reals laid out end to end, which
    /// `Initializer::Struct` already describes, so no new variant is needed and
    /// `emit_float_initializer` handles every base width.
    fn complex_initializer(&mut self, expr: &Expr, typ: TypeId) -> Option<Initializer> {
        // `double _Complex z = {1.0};` -- a *scalar* initializer that happens
        // to be braced, because a complex type is a scalar type (C11 6.2.5p21).
        // Only the first element initializes the object; gcc warns "excess
        // elements in scalar initializer" for any others and ignores them, so
        // `{1.0, 2.0}` is 1.0 + 0.0i rather than 1.0 + 2.0i.
        let (re, im) = if let ExprKind::InitList { elements } = &expr.kind {
            let first = elements.first()?;
            self.eval_const_complex(&first.value)?
        } else {
            self.eval_const_complex(expr)?
        };

        let base = self.types.complex_base(typ);
        let base_bytes = self.types.size_bytes(base);
        // Narrowing to the base width happens at emission, which knows the
        // field size; `FloatVal` just carries the value.
        Some(Initializer::Struct {
            total_size: base_bytes * 2,
            fields: vec![
                (0, base_bytes, Initializer::Float(re)),
                (base_bytes, base_bytes, Initializer::Float(im)),
            ],
        })
    }

    /// Report an initializer that is not a constant expression we can fold.
    ///
    /// Named rather than inlined because three arms need it and they used to
    /// disagree: two printed a raw Rust `{:?}` dump of the AST -- internal
    /// representation in a user-facing message -- and the third said nothing
    /// at all, which silently zeroed the object.
    fn reject_initializer(&self, expr: &Expr) {
        error(
            self.expr_pos(expr),
            &format!(
                "{} is not a constant expression, so it cannot initialize an object with static storage duration",
                describe_expr(&expr.kind)
            ),
        );
    }

    /// Fold a constant expression into an initializer for an *arithmetic*
    /// object of type `typ`, converting as an assignment would.
    ///
    /// Returns None when the expression is not a constant this compiler can
    /// fold, leaving the caller's own diagnostics to run.
    fn fold_scalar_init(&mut self, expr: &Expr, typ: TypeId) -> Option<Initializer> {
        if self.types.is_float(typ) {
            let wrap = |val| {
                if self.types.kind(typ) == TypeKind::Float128 {
                    Initializer::Float128(val)
                } else {
                    Initializer::Float(val)
                }
            };
            if let Some(val) = self.eval_const_float_expr(expr) {
                return Some(wrap(val));
            }
            // An integer constant initializing a floating object converts
            // exactly, however wide it is: `long double x = 1;`.
            let val = self.eval_const_expr(expr)?;
            return Some(wrap(FloatVal::from_parts(val < 0, val.unsigned_abs(), 0)));
        }

        // Converting to `_Bool` is not a truncation: every non-zero value
        // becomes 1, so 0.5 is `true` where `(int)0.5` is 0 (C17 6.3.1.2).
        let is_bool = self.types.kind(typ) == TypeKind::Bool;

        if let Some(val) = self.eval_const_expr(expr) {
            return Some(Initializer::Int(if is_bool {
                i128::from(val != 0)
            } else {
                val
            }));
        }
        // C17 6.3.1.4: converting a floating constant to an integer type
        // discards the fractional part.
        let val = self.eval_const_float_expr(expr)?;
        Some(Initializer::Int(if is_bool {
            i128::from(!val.is_zero())
        } else {
            val.to_f64() as i128
        }))
    }

    /// The position to report for `expr`.
    ///
    /// `linearize_global_decl` has no statement to set `current_pos` from, so
    /// it stays `None` at file scope and every such diagnostic used to come
    /// out as `file:0`. The expression carries its own position; prefer it.
    fn expr_pos(&self, expr: &Expr) -> Position {
        if expr.pos != Position::default() {
            expr.pos
        } else {
            self.current_pos.unwrap_or_default()
        }
    }

    /// Count the number of scalar fields needed to fill an aggregate type
    /// (for brace elision per C99 6.7.8p17-20).
    pub(crate) fn count_scalar_fields(&self, typ: TypeId) -> usize {
        match self.types.kind(typ) {
            TypeKind::Array => {
                let elem_type = self.types.base_type(typ).unwrap_or(self.types.int_id);
                let count = self.types.get(typ).array_size.unwrap_or(0);
                count * self.count_scalar_fields(elem_type)
            }
            TypeKind::Struct => {
                if let Some(composite) = self.types.get(typ).composite.as_ref() {
                    composite
                        .members
                        .iter()
                        // Skip unnamed bitfield padding
                        .filter(|m| m.name != StringId::EMPTY || m.bit_width.is_none())
                        .map(|m| self.count_scalar_fields(m.typ))
                        .sum()
                } else {
                    1
                }
            }
            TypeKind::Union => {
                // Union only initializes first named member
                if let Some(composite) = self.types.get(typ).composite.as_ref() {
                    composite
                        .members
                        .iter()
                        .find(|m| m.name != StringId::EMPTY)
                        .map(|m| self.count_scalar_fields(m.typ))
                        .unwrap_or(1)
                } else {
                    1
                }
            }
            _ => 1,
        }
    }

    /// Check if brace elision applies: the element is a positional scalar targeting
    /// an aggregate member, and is NOT a string literal initializing a char array
    /// (C99 6.7.8p14: string literals are a special case for char arrays).
    pub(crate) fn is_brace_elision_candidate(
        &self,
        element: &InitElement,
        target_type: TypeId,
    ) -> bool {
        if !element.designators.is_empty() {
            return false;
        }
        let target_is_aggregate = matches!(
            self.types.kind(target_type),
            TypeKind::Array | TypeKind::Struct | TypeKind::Union
        );
        if !target_is_aggregate {
            return false;
        }
        // String/wide string literals can directly initialize char/wchar_t arrays
        // without brace elision (C99 6.7.8p14)
        if matches!(
            element.value.kind,
            ExprKind::InitList { .. }
                | ExprKind::StringLit(_)
                | ExprKind::WideStringLit(_)
                | ExprKind::Utf16StringLit(_)
                | ExprKind::Utf32StringLit(_)
        ) {
            return false;
        }
        // An element that is already an expression of the target's own type
        // initializes the whole aggregate by itself (C17 6.7.9p13). Eliding
        // braces around it consumes `count_scalar_fields` *elements* instead
        // of one, so `struct P a[2] = {p, p};` put both structs into a[0] and
        // left a[1] uninitialized -- then assigned a struct where a scalar
        // field was expected, producing garbage.
        if let Some(elem_typ) = element.value.typ {
            let elem_kind = self.types.kind(elem_typ);
            if matches!(elem_kind, TypeKind::Struct | TypeKind::Union)
                && elem_kind == self.types.kind(target_type)
                && self.types.size_bits(elem_typ) == self.types.size_bits(target_type)
            {
                return false;
            }
        }
        true
    }

    /// Consume elements from `elements[elem_idx..]` via brace elision to fill
    /// an aggregate `target_type`. Returns the collected sub-elements.
    /// Advances `elem_idx` past the consumed elements.
    pub(crate) fn consume_brace_elision(
        &self,
        elements: &[InitElement],
        elem_idx: &mut usize,
        target_type: TypeId,
    ) -> Vec<InitElement> {
        let n = self.count_scalar_fields(target_type);
        let mut sub_elements = Vec::new();
        let mut consumed = 0;
        while consumed < n && *elem_idx < elements.len() {
            let e = &elements[*elem_idx];
            // Stop at designated elements (they apply to the current aggregate level)
            if consumed > 0 && !e.designators.is_empty() {
                break;
            }
            sub_elements.push(InitElement {
                designators: vec![],
                value: e.value.clone(),
            });
            *elem_idx += 1;
            consumed += 1;
        }
        sub_elements
    }

    /// Group array init elements by index, handling designators, brace elision,
    /// and nested InitList flattening. Shared between static and runtime paths.
    pub(crate) fn group_array_init_elements(
        &self,
        elements: &[InitElement],
        elem_type: TypeId,
    ) -> ArrayInitGroups {
        let mut element_lists: HashMap<i64, Vec<InitElement>> = HashMap::new();
        let mut element_indices: Vec<i64> = Vec::new();
        let mut current_idx: i64 = 0;
        let mut elem_idx = 0;

        while elem_idx < elements.len() {
            let element = &elements[elem_idx];
            let mut index = None;
            let mut index_pos = None;
            for (pos, designator) in element.designators.iter().enumerate() {
                if let Designator::Index(idx) = designator {
                    index = Some(*idx);
                    index_pos = Some(pos);
                    break;
                }
            }

            let element_index = if let Some(idx) = index {
                current_idx = idx + 1;
                idx
            } else {
                let idx = current_idx;
                current_idx += 1;
                idx
            };

            let remaining_designators = match index_pos {
                Some(pos) => element.designators[pos + 1..].to_vec(),
                None => element.designators.clone(),
            };

            // Brace elision (C99 6.7.8p20): positional scalar for aggregate element
            if remaining_designators.is_empty()
                && self.is_brace_elision_candidate(element, elem_type)
            {
                let sub_elements = self.consume_brace_elision(elements, &mut elem_idx, elem_type);
                let entry = element_lists.entry(element_index).or_insert_with(|| {
                    element_indices.push(element_index);
                    Vec::new()
                });
                entry.extend(sub_elements);
                continue;
            }

            let entry = element_lists.entry(element_index).or_insert_with(|| {
                element_indices.push(element_index);
                Vec::new()
            });

            if remaining_designators.is_empty() {
                if let ExprKind::InitList {
                    elements: nested_elements,
                } = &element.value.kind
                {
                    entry.extend(nested_elements.clone());
                    elem_idx += 1;
                    continue;
                }
            }

            entry.push(InitElement {
                designators: remaining_designators,
                value: element.value.clone(),
            });
            elem_idx += 1;
        }

        element_indices.sort();
        ArrayInitGroups {
            element_lists,
            indices: element_indices,
        }
    }

    /// Walk struct/union init elements and produce field visits.
    /// Handles positional iteration, anonymous struct continuation, designators,
    /// and brace elision. Shared between static and runtime init paths.
    pub(crate) fn walk_struct_init_fields(
        &self,
        resolved_typ: TypeId,
        members: &[crate::types::StructMember],
        is_union: bool,
        elements: &[InitElement],
    ) -> Vec<StructFieldVisit> {
        let mut visits = Vec::new();
        let mut current_field_idx = 0;
        let mut anon_cont: Option<AnonContinuation> = None;
        let mut elem_idx = 0;

        while elem_idx < elements.len() {
            let element = &elements[elem_idx];
            if element.designators.is_empty() {
                // Positional: check anonymous struct continuation, then next member
                let mut member = None;
                if anon_cont.is_some() {
                    member = self.get_anon_continuation_member(
                        &mut anon_cont,
                        members,
                        &mut current_field_idx,
                    );
                }
                if member.is_none() {
                    member = self.next_positional_member(members, is_union, &mut current_field_idx);
                }
                let Some(member) = member else {
                    elem_idx += 1;
                    continue;
                };
                let field_size = (self.types.size_bits(member.typ) / 8) as usize;

                // Brace elision: scalar for aggregate member
                let kind = if self.is_brace_elision_candidate(element, member.typ) {
                    let sub_elements =
                        self.consume_brace_elision(elements, &mut elem_idx, member.typ);
                    StructFieldVisitKind::BraceElision(sub_elements)
                } else {
                    elem_idx += 1;
                    StructFieldVisitKind::Expr(element.value.clone())
                };
                visits.push(StructFieldVisit {
                    offset: member.offset,
                    typ: member.typ,
                    field_size,
                    kind,
                    bit_offset: member.bit_offset,
                    bit_width: member.bit_width,
                    storage_unit_size: member.storage_unit_size,
                });
                continue;
            }

            // Designated path
            let resolved = self.resolve_designator_chain(resolved_typ, 0, &element.designators);
            let Some(ResolvedDesignator {
                offset,
                typ: field_type,
                bit_offset,
                bit_width,
                storage_unit_size,
            }) = resolved
            else {
                elem_idx += 1;
                continue;
            };
            if let Some(Designator::Field(name)) = element.designators.first() {
                if let Some(result) = self.member_index_for_designator(members, *name) {
                    match result {
                        MemberDesignatorResult::Direct(next_idx) => {
                            current_field_idx = next_idx;
                            anon_cont = None;
                        }
                        MemberDesignatorResult::Anonymous { outer_idx, levels } => {
                            current_field_idx = outer_idx;
                            anon_cont = Some(AnonContinuation { outer_idx, levels });
                        }
                    }
                }
            }
            let field_size = (self.types.size_bits(field_type) / 8) as usize;
            visits.push(StructFieldVisit {
                offset,
                typ: field_type,
                field_size,
                kind: StructFieldVisitKind::Expr(element.value.clone()),
                bit_offset,
                bit_width,
                storage_unit_size,
            });
            elem_idx += 1;
        }

        visits
    }

    /// Convert an AST initializer list to an IR Initializer
    pub(crate) fn ast_init_list_to_ir(
        &mut self,
        elements: &[InitElement],
        typ: TypeId,
    ) -> Initializer {
        let type_kind = self.types.kind(typ);
        let total_size = (self.types.size_bits(typ) / 8) as usize;

        match type_kind {
            TypeKind::Array => {
                let elem_type = self.types.base_type(typ).unwrap_or(self.types.int_id);

                // `char b[] = {"hi"}` -- C17 6.7.9p14 lets the string literal
                // initializing a character array be enclosed in braces, and it
                // still initializes *this* array. Treated as an ordinary
                // element list it became one element, so the characters were
                // never copied in and the array held whatever a truncated
                // pointer left behind. The same look-through already exists
                // one level down for `char names[3][4] = {"Sun", "Mon"}`.
                if self.types.is_integer(elem_type) {
                    if let [only] = elements {
                        if only.designators.is_empty()
                            && matches!(
                                only.value.kind,
                                ExprKind::StringLit(_)
                                    | ExprKind::WideStringLit(_)
                                    | ExprKind::Utf16StringLit(_)
                                    | ExprKind::Utf32StringLit(_)
                            )
                        {
                            return self.ast_init_to_ir(&only.value, typ);
                        }
                    }
                }

                let elem_size = (self.types.size_bits(elem_type) / 8) as usize;
                let elem_is_aggregate = matches!(
                    self.types.kind(elem_type),
                    TypeKind::Array | TypeKind::Struct | TypeKind::Union
                );

                let groups = self.group_array_init_elements(elements, elem_type);
                let mut init_elements = Vec::new();
                for element_index in groups.indices {
                    let Some(list) = groups.element_lists.get(&element_index) else {
                        continue;
                    };
                    let offset = (element_index as usize) * elem_size;
                    // When a string literal initializes a char/wchar_t array element
                    // (e.g., char names[3][4] = {"Sun", "Mon", "Tue"}), handle it
                    // directly with the ARRAY type. Otherwise ast_init_list_to_ir
                    // recurses and passes elem_type=char, causing the string to be
                    // stored as a pointer instead of inline char data.
                    let is_string_for_char_array = elem_is_aggregate
                        && list.len() == 1
                        && matches!(
                            list[0].value.kind,
                            ExprKind::StringLit(_)
                                | ExprKind::WideStringLit(_)
                                | ExprKind::Utf16StringLit(_)
                                | ExprKind::Utf32StringLit(_)
                        )
                        && self.types.kind(elem_type) == TypeKind::Array;
                    let elem_init = if is_string_for_char_array {
                        self.ast_init_to_ir(&list[0].value, elem_type)
                    } else if elem_is_aggregate {
                        self.ast_init_list_to_ir(list, elem_type)
                    } else if let Some(last) = list.last() {
                        self.ast_init_to_ir(&last.value, elem_type)
                    } else {
                        Initializer::None
                    };
                    init_elements.push((offset, elem_init));
                }

                init_elements.sort_by_key(|(offset, _)| *offset);

                Initializer::Array {
                    elem_size,
                    total_size,
                    elements: init_elements,
                }
            }

            TypeKind::Struct | TypeKind::Union => {
                let resolved_typ = self.resolve_struct_type(typ);
                let resolved_size = (self.types.size_bits(resolved_typ) / 8) as usize;
                if let Some(composite) = self.types.get(resolved_typ).composite.as_ref() {
                    let members: Vec<_> = composite.members.clone();
                    let is_union = self.types.kind(resolved_typ) == TypeKind::Union;

                    let visits =
                        self.walk_struct_init_fields(resolved_typ, &members, is_union, elements);

                    // Convert field visits to RawFieldInit by evaluating expressions
                    let mut raw_fields: Vec<RawFieldInit> = Vec::new();
                    for visit in visits {
                        let field_init = match visit.kind {
                            StructFieldVisitKind::BraceElision(sub_elements) => {
                                self.ast_init_list_to_ir(&sub_elements, visit.typ)
                            }
                            StructFieldVisitKind::Expr(expr) => {
                                self.ast_init_to_ir(&expr, visit.typ)
                            }
                        };
                        raw_fields.push(RawFieldInit {
                            offset: visit.offset,
                            field_size: visit.field_size,
                            init: field_init,
                            bit_offset: visit.bit_offset,
                            bit_width: visit.bit_width,
                        });
                    }

                    // Sort by the bit each field starts at, so that designated
                    // initializers emit in address order however they were
                    // written -- the emitter fills the gaps between fields and
                    // so requires them sorted and non-overlapping.
                    raw_fields.sort_by_key(|f| f.offset * 8 + f.bit_offset.unwrap_or(0) as usize);

                    // Initializing the same object twice: the later one wins
                    // (C17 6.7.9p19). Two *distinct* bitfields are different
                    // objects even when they share a byte, so both survive.
                    let mut idx = 0;
                    while idx + 1 < raw_fields.len() {
                        let (a, b) = (&raw_fields[idx], &raw_fields[idx + 1]);
                        let distinct_bitfields = a.bit_width.is_some()
                            && b.bit_width.is_some()
                            && (a.offset, a.bit_offset) != (b.offset, b.bit_offset);
                        let a_span = a.byte_span();
                        let b_span = b.byte_span();

                        if !distinct_bitfields
                            && a_span.start < b_span.end
                            && b_span.start < a_span.end
                        {
                            raw_fields.remove(idx);
                        } else {
                            idx += 1;
                        }
                    }

                    // Merge bitfields byte by byte rather than one storage unit
                    // at a time. A unit is `sizeof(T)` wide and aligned, so it
                    // routinely spans bytes that belong to other members --
                    // `unsigned a:1` after a `char` sits at bit 8 of a unit
                    // based at byte 0, which also holds the `char` and whatever
                    // follows. Emitting whole units here would blank them; the
                    // units of two fields with different declared types can
                    // also be different sizes at the same byte offset, leaving
                    // no single width to emit.
                    let mut bitfield_bytes: BTreeMap<usize, u8> = BTreeMap::new();
                    let mut init_fields: Vec<(usize, usize, Initializer)> = Vec::new();

                    for field in &raw_fields {
                        let (Some(bit_off), Some(bit_width)) = (field.bit_offset, field.bit_width)
                        else {
                            init_fields.push((field.offset, field.field_size, field.init.clone()));
                            continue;
                        };
                        let Initializer::Int(value) = field.init else {
                            continue;
                        };
                        if bit_width == 0 {
                            continue;
                        }

                        // A field never crosses its own window, so the shift
                        // stays inside the 128-bit carrier even at width 64.
                        let mask = (1u128 << bit_width) - 1;
                        let placed = ((value as u128) & mask) << bit_off;

                        // Only the bytes the field's own bits reach. Its window
                        // is wider and generally starts earlier -- `unsigned a:1`
                        // after a `char` sits at bit 8 of a window based at byte
                        // 0 -- and writing the whole window here would blank the
                        // members sharing it.
                        for byte in
                            (bit_off / 8) as usize..=((bit_off + bit_width - 1) / 8) as usize
                        {
                            let bits = ((placed >> (byte * 8)) & 0xff) as u8;
                            *bitfield_bytes.entry(field.offset + byte).or_default() |= bits;
                        }
                    }

                    init_fields.extend(
                        bitfield_bytes
                            .into_iter()
                            .map(|(offset, bits)| (offset, 1, Initializer::Int(bits as i128))),
                    );
                    init_fields.sort_by_key(|(offset, _, _)| *offset);

                    Initializer::Struct {
                        total_size: resolved_size,
                        fields: init_fields,
                    }
                } else {
                    Initializer::None
                }
            }

            _ => {
                if let Some(element) = elements.first() {
                    self.ast_init_to_ir(&element.value, typ)
                } else {
                    Initializer::None
                }
            }
        }
    }

    pub(crate) fn resolve_designator_chain(
        &self,
        base_type: TypeId,
        base_offset: usize,
        designators: &[Designator],
    ) -> Option<ResolvedDesignator> {
        let mut offset = base_offset;
        let mut typ = base_type;
        let mut bit_offset = None;
        let mut bit_width = None;
        let mut storage_unit_size = None;

        for (idx, designator) in designators.iter().enumerate() {
            match designator {
                Designator::Field(name) => {
                    let mut resolved = typ;
                    if self.types.kind(resolved) == TypeKind::Array {
                        resolved = self.types.base_type(resolved)?;
                    }
                    resolved = self.resolve_struct_type(resolved);
                    let member = self.types.find_member(resolved, *name)?;
                    offset += member.offset;
                    typ = member.typ;
                    if idx + 1 == designators.len() {
                        bit_offset = member.bit_offset;
                        bit_width = member.bit_width;
                        storage_unit_size = member.storage_unit_size;
                    } else {
                        bit_offset = None;
                        bit_width = None;
                        storage_unit_size = None;
                    }
                }
                Designator::Index(index) => {
                    if self.types.kind(typ) != TypeKind::Array {
                        return None;
                    }
                    let elem_type = self.types.base_type(typ)?;
                    let elem_size = self.types.size_bits(elem_type) / 8;
                    offset += (*index as usize) * (elem_size as usize);
                    typ = elem_type;
                    bit_offset = None;
                    bit_width = None;
                    storage_unit_size = None;
                }
            }
        }

        Some(ResolvedDesignator {
            offset,
            typ,
            bit_offset,
            bit_width,
            storage_unit_size,
        })
    }

    pub(crate) fn next_positional_member(
        &self,
        members: &[crate::types::StructMember],
        is_union: bool,
        current_field_idx: &mut usize,
    ) -> Option<MemberInfo> {
        if is_union {
            if *current_field_idx > 0 {
                return None;
            }
            let member = members.iter().find(|m| m.name != StringId::EMPTY)?;
            *current_field_idx = members.len();
            return Some(MemberInfo {
                offset: member.offset,
                typ: member.typ,
                bit_offset: member.bit_offset,
                bit_width: member.bit_width,
                storage_unit_size: member.storage_unit_size,
            });
        }

        while *current_field_idx < members.len() {
            let member = &members[*current_field_idx];
            if member.name == StringId::EMPTY && member.bit_width.is_some() {
                *current_field_idx += 1;
                continue;
            }
            if member.name != StringId::EMPTY || member.bit_width.is_none() {
                *current_field_idx += 1;
                return Some(MemberInfo {
                    offset: member.offset,
                    typ: member.typ,
                    bit_offset: member.bit_offset,
                    bit_width: member.bit_width,
                    storage_unit_size: member.storage_unit_size,
                });
            }
            *current_field_idx += 1;
        }

        None
    }

    /// Get the next positional member from an anonymous struct continuation.
    /// Walks the stack of anonymous struct levels from innermost to outermost.
    /// If the innermost level is exhausted, pops it and tries the next outer level.
    /// When all levels are exhausted, clears the continuation and returns None.
    pub(crate) fn get_anon_continuation_member(
        &self,
        cont: &mut Option<AnonContinuation>,
        _outer_members: &[crate::types::StructMember],
        current_field_idx: &mut usize,
    ) -> Option<MemberInfo> {
        let c = cont.as_mut()?;

        loop {
            let Some(level) = c.levels.last() else {
                // All levels exhausted
                *current_field_idx = c.outer_idx + 1;
                *cont = None;
                return None;
            };
            let anon_type_id = level.anon_type;
            let base_offset = level.base_offset;
            let mut idx = level.inner_next_idx;

            let anon_type = self.types.get(anon_type_id);
            let Some(composite) = anon_type.composite.as_ref() else {
                c.levels.pop();
                continue;
            };
            let members = composite.members.clone();

            // Scan members at this level
            let mut found_member = None;
            let mut descend_into = None;

            while idx < members.len() {
                let inner = &members[idx];
                // Skip unnamed bitfield padding
                if inner.name == StringId::EMPTY && inner.bit_width.is_some() {
                    idx += 1;
                    continue;
                }
                // Nested anonymous aggregate — descend into it
                if inner.name == StringId::EMPTY && inner.bit_width.is_none() {
                    let inner_type = self.types.get(inner.typ);
                    let is_nested_anon =
                        matches!(inner_type.kind, TypeKind::Struct | TypeKind::Union)
                            && inner_type
                                .composite
                                .as_ref()
                                .is_some_and(|comp| comp.tag.is_none());
                    if is_nested_anon {
                        descend_into = Some((inner.typ, base_offset + inner.offset, idx + 1));
                        break;
                    }
                }
                // Found a valid named member
                found_member = Some((idx + 1, inner.clone()));
                break;
            }

            if let Some((next_idx, inner)) = found_member {
                // Update the current level's index
                c.levels.last_mut().unwrap().inner_next_idx = next_idx;
                return Some(MemberInfo {
                    offset: base_offset + inner.offset,
                    typ: inner.typ,
                    bit_offset: inner.bit_offset,
                    bit_width: inner.bit_width,
                    storage_unit_size: inner.storage_unit_size,
                });
            }

            if let Some((nested_type, nested_offset, next_idx)) = descend_into {
                // Advance past the anon struct at this level, then descend
                c.levels.last_mut().unwrap().inner_next_idx = next_idx;
                c.levels.push(AnonLevel {
                    anon_type: nested_type,
                    base_offset: nested_offset,
                    inner_next_idx: 0,
                });
                continue;
            }

            // This level is exhausted — pop it
            c.levels.pop();
        }
    }

    pub(crate) fn member_index_for_designator(
        &self,
        members: &[crate::types::StructMember],
        name: StringId,
    ) -> Option<MemberDesignatorResult> {
        for (idx, member) in members.iter().enumerate() {
            if member.name == name {
                return Some(MemberDesignatorResult::Direct(idx + 1));
            }
            if member.name == StringId::EMPTY {
                let member_type = self.types.get(member.typ);
                let is_anon_aggregate =
                    matches!(member_type.kind, TypeKind::Struct | TypeKind::Union)
                        && member_type
                            .composite
                            .as_ref()
                            .is_some_and(|composite| composite.tag.is_none());
                if is_anon_aggregate {
                    // Recursively search for the field, building the nesting path
                    let mut path = Vec::new();
                    if self.find_anon_field_path(member.typ, member.offset, name, &mut path) {
                        return Some(MemberDesignatorResult::Anonymous {
                            outer_idx: idx,
                            levels: path,
                        });
                    }
                }
            }
        }

        None
    }

    /// Recursively search for `name` inside an anonymous aggregate, building
    /// the path of `AnonLevel`s needed for positional continuation.
    /// Returns true if the field was found.
    pub(crate) fn find_anon_field_path(
        &self,
        anon_type: TypeId,
        base_offset: usize,
        name: StringId,
        path: &mut Vec<AnonLevel>,
    ) -> bool {
        let typ = self.types.get(anon_type);
        let Some(composite) = typ.composite.as_ref() else {
            return false;
        };
        for (inner_idx, inner_member) in composite.members.iter().enumerate() {
            if inner_member.name == name {
                // Found it directly at this level
                path.push(AnonLevel {
                    anon_type,
                    base_offset,
                    inner_next_idx: inner_idx + 1,
                });
                return true;
            }
            // Check if this is a nested anonymous aggregate
            if inner_member.name == StringId::EMPTY {
                let inner_type = self.types.get(inner_member.typ);
                let is_nested_anon = matches!(inner_type.kind, TypeKind::Struct | TypeKind::Union)
                    && inner_type
                        .composite
                        .as_ref()
                        .is_some_and(|c| c.tag.is_none());
                if is_nested_anon {
                    // Push this level pointing PAST the nested anon struct.
                    // The inner level handles continuation within the nested anon;
                    // when it's exhausted, this level continues from the next member.
                    path.push(AnonLevel {
                        anon_type,
                        base_offset,
                        inner_next_idx: inner_idx + 1,
                    });
                    if self.find_anon_field_path(
                        inner_member.typ,
                        base_offset + inner_member.offset,
                        name,
                        path,
                    ) {
                        return true;
                    }
                    path.pop(); // not found in this branch
                }
            }
        }
        false
    }
}
