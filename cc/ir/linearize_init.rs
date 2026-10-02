//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT

//! Initializer and global declaration linearization

use super::linearize::*;
use super::{Initializer, SymbolAlias};
use crate::constexpr;
use crate::constexpr::ConstScope;
use crate::diag::error;
use crate::float::FloatVal;
use crate::parse::ast::{BinaryOp, Declaration, Designator, Expr, ExprKind, InitElement, UnaryOp};
use crate::strings::StringId;
use crate::token::lexer::Position;
use crate::types::{MemberInfo, Type, TypeId, TypeKind, TypeModifiers, TypeTable};
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

/// `-Wno-overflow` silences the warning for a floating constant that
/// converts to an integer type outside its range, as it does in gcc.
const OVERFLOW_WARNING: &str = "overflow";

/// The bytes a bit-field's own bits occupy, as `(byte offset from the field's
/// own offset, the bits `value` puts there, the mask of the bits the field
/// owns)`.
///
/// Only the bytes the field's bits reach. Its access span is wider and
/// generally starts earlier -- `unsigned a:1` after a `char` sits at bit 8 of
/// a span based at byte 0 -- and writing the whole span would blank the
/// members sharing it.
fn bitfield_carrier_bytes(
    bit_offset: u32,
    bit_width: u32,
    value: i128,
) -> impl Iterator<Item = (usize, u8, u8)> {
    // A field with no bits, or one no carrier could hold, occupies no byte.
    // Otherwise the mask comes of shifting `u128::MAX` down rather than
    // `1 << width` up, for the reason `bitfield_value_mask` records: the
    // latter overflows at the carrier's own width.
    let fits = bit_width > 0 && u64::from(bit_offset) + u64::from(bit_width) <= 128;
    let (shift, width_mask) = if fits {
        (bit_offset, u128::MAX >> (128 - bit_width))
    } else {
        (0, 0)
    };
    let placed = ((value as u128) & width_mask) << shift;
    let owned = width_mask << shift;
    let bytes = if fits {
        (bit_offset / 8) as usize..((bit_offset + bit_width - 1) / 8) as usize + 1
    } else {
        0..0
    };
    bytes.map(move |byte| {
        let shift = byte * 8;
        (
            byte,
            ((placed >> shift) & 0xff) as u8,
            ((owned >> shift) & 0xff) as u8,
        )
    })
}

/// The bytes a bit-field member's own bits occupy, measured from the first
/// byte of the struct that declares it. `None` for anything but a bit-field.
fn bitfield_byte_span(member: &crate::types::StructMember) -> Option<std::ops::Range<usize>> {
    Some(crate::types::own_bit_bytes(
        member.offset,
        member.bit_offset?,
        member.bit_width?,
    ))
}

/// Give the carrier byte at `offset` of a struct initializer the bits `bits`
/// in place of the ones `mask` marks, leaving the neighbouring bit-fields
/// that share the byte as they are.
///
/// False when the byte is not a carrier byte the lowering wrote -- an entry
/// for an ordinary member covering it, or something other than an integer --
/// which leaves the caller to discard the earlier initializer whole.
fn replace_carrier_bits(
    fields: &mut Vec<(usize, usize, Initializer)>,
    offset: usize,
    bits: u8,
    mask: u8,
) -> bool {
    match fields
        .iter()
        .position(|(off, size, _)| *off <= offset && offset < *off + *size)
    {
        Some(index) => {
            if (fields[index].0, fields[index].1) != (offset, 1) {
                return false;
            }
            let Initializer::Int(held) = fields[index].2 else {
                return false;
            };
            fields[index].2 = Initializer::Int(i128::from((held as u8 & !mask) | bits));
            true
        }
        None => {
            // The byte holds no bit-field the earlier initializer gave a
            // value to, so the bits around this field's are zero.
            fields.push((offset, 1, Initializer::Int(i128::from(bits))));
            fields.sort_by_key(|(off, _, _)| *off);
            true
        }
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
        ExprKind::Conditional { .. } | ExprKind::CondElvis { .. } => "this conditional",
        _ => "this expression",
    }
}

impl<'a> super::linearize::Linearizer<'a> {
    // Global declarations

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

            // A second name for a definition: neither storage of its own nor
            // a reference to storage elsewhere, so none of the paths below.
            if declarator.symbol_attrs.alias.is_some() {
                if declarator.init.is_some() {
                    crate::diag::error_args(
                        declarator.pos,
                        "'{0}' defined both normally and as 'alias' attribute",
                        &[crate::arch::lir::undecorated(&name)],
                    );
                    continue;
                }
                let kind = if self.types.kind(declarator.typ) == TypeKind::Function {
                    AliasKind::Function
                } else {
                    AliasKind::Object
                };
                if storage_class.contains(TypeModifiers::THREAD_LOCAL) {
                    // Accessed as a thread-local, whatever it names.
                    self.module.extern_tls_symbols.insert(name.clone());
                }
                self.declare_alias(
                    &name,
                    &declarator.symbol_attrs,
                    storage_class.contains(TypeModifiers::STATIC),
                    kind,
                    declarator.pos,
                );
                continue;
            }

            // Function declarations without bodies are external functions
            // Track them in extern_symbols so codegen uses GOT access
            if self.types.kind(declarator.typ) == TypeKind::Function {
                // Check if not defined in this module (forward refs will be cleaned up later)
                if !self.module.functions.iter().any(|f| f.name == name) {
                    self.module
                        .set_declared_symbol_attrs(&name, declarator.symbol_attrs.clone());
                    if declarator.fn_effect != crate::parse::ast::MemEffect::Unknown {
                        self.module
                            .declared_fn_effects
                            .insert(name.clone(), declarator.fn_effect);
                    }
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
                    self.module
                        .set_declared_symbol_attrs(&name, declarator.symbol_attrs.clone());
                    self.module.extern_symbols.insert(name.clone());
                    self.module.extern_object_align.insert(
                        name.clone(),
                        declarator
                            .explicit_align
                            .unwrap_or(self.types.alignment(declarator.typ) as u32),
                    );
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

            // A definition here outranks anything recorded for the declaration.
            self.module.declared_symbol_attrs.remove(&name);
            self.module.extern_symbols.remove(&name);
            self.module.extern_object_align.remove(&name);

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
            ExprKind::CharLit(c) => Initializer::Int(*c as i128),

            // GNU `&&label` in a static initializer -- `static void *t[] =
            // {&&a, &&b};`, which is how an interpreter builds its dispatch
            // table. The label is a real assembler symbol, so this is the same
            // shape as a string-literal reference.
            //
            // Every initializer that reaches here has static storage duration,
            // so the function can no longer be copied: see
            // `Function::saves_label_in_static`.
            ExprKind::LabelAddr(name) => match self.take_label_address(*name, expr.pos) {
                Some(sym) => {
                    if let Some(func) = &mut self.current_func {
                        func.saves_label_in_static = true;
                    }
                    Initializer::SymAddr(sym)
                }
                None => Initializer::Int(0),
            },

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

            // Prefixed string literals: embedded for an array, interned and
            // referenced otherwise.
            ExprKind::Utf16StringLit(units) => {
                if self.types.kind(typ) == TypeKind::Array {
                    Initializer::Utf16String(units.clone())
                } else {
                    let label = self.module.add_utf16_string(units.clone());
                    Initializer::SymAddr(label)
                }
            }

            // `wchar_t` is 4 bytes on every target, so a wide literal is laid
            // out exactly as a `char32_t` one.
            ExprKind::WideStringLit(units) | ExprKind::Utf32StringLit(units) => {
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
                    if let Some(val) = self.eval_const_init_expr(expr) {
                        Initializer::Int(val)
                    } else {
                        // Returning `Initializer::None` here would put the
                        // object in .bss and make it silently zero.
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
                if let Some((name, offset)) = self.eval_static_address(operand) {
                    if offset == 0 {
                        Initializer::SymAddr(name)
                    } else {
                        Initializer::SymAddrOffset(name, offset)
                    }
                } else if let Some(val) = self.eval_const_init_expr(expr) {
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
                    let init = self.new_static_object_init(elements, *cl_type);
                    self.module.add_global(&anon_name, *cl_type, init);

                    // Return address of the anonymous global
                    Initializer::SymAddr(anon_name)
                } else {
                    // Type mismatch - use the compound literal's own type
                    self.ast_init_list_to_ir(elements, *cl_type)
                }
            }

            // Identifier: an address constant when it names an array or a
            // function, which decay to their address (C17 6.3.2.1p3-4), or an
            // enum constant.
            //
            // The name's own type decides, not the type it initializes.
            // Asking the target made `long l = (long)f;` a diagnostic, when
            // it is as much a relocation as `long (*p)(void) = f;`, and made
            // `int *q = p;` for a pointer object `p` initialize `q` with the
            // address of `p` rather than reject a value read.
            ExprKind::Ident(symbol_id) => {
                if self.types.decays(self.expr_type(expr)) {
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
                        Initializer::Int(val)
                    } else {
                        // Not an enum constant, and `fold_scalar_init` has
                        // already tried the `const`-object folding gcc does
                        // here -- so this reads the value of an object that
                        // has none to read, which 6.7.9p4 does not permit to
                        // initialize one with static storage duration.
                        // Returning `Initializer::None` put the object in .bss
                        // and said nothing, so `int v; int w = v;` silently
                        // became zero.
                        self.reject_initializer(expr);
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
                // C17 6.6 does not list it, but the difference of two addresses
                // *into the same object* is a link-time constant and gcc folds
                // it: `&a[2] - &a[0]` and `(char *)&s.b - (char *)&s.a` are
                // ordinary idioms, and both were rejected here. Across two
                // objects the distance is not known until they are laid out, so
                // that stays a diagnostic -- which is what requiring one symbol
                // on both sides enforces, matching gcc on `&a[0] - &b[0]`.
                if *op == BinaryOp::Sub {
                    if let Some(diff) = self.static_address_difference(left, right) {
                        return Initializer::Int(diff);
                    }
                }

                // Try pointer/array + int or int + pointer/array → symbol address with offset
                let is_ptr_or_array =
                    |t: TypeId| matches!(self.types.kind(t), TypeKind::Pointer | TypeKind::Array);
                // Which side is an *address*, not which side has a pointer
                // type: `(unsigned long)&_text - 0x10000000L` is a relocation
                // at an integer type, and asking the type alone rejected every
                // one of them. The scale below still comes from the type, so
                // an integer-typed address counts bytes and a pointer counts
                // elements, which is what each means.
                let _ = is_ptr_or_array;
                let (ptr_expr, int_expr, is_sub) = if self.is_address_valued(left) {
                    (left.as_ref(), right.as_ref(), *op == BinaryOp::Sub)
                } else if self.is_address_valued(right) && *op == BinaryOp::Add {
                    (right.as_ref(), left.as_ref(), false)
                } else {
                    // Neither operand names a symbol, so this is ordinary
                    // arithmetic that happens to use `+` or `-` -- except that
                    // one of its leaves may still be the difference of two
                    // addresses into one object, which is a constant although
                    // neither address is.
                    if let Some(val) = self.eval_const_with_address_difference(expr) {
                        return Initializer::Int(val);
                    }
                    self.reject_initializer(expr);
                    return Initializer::None;
                };

                // Evaluate the pointer side as a static address
                if let Some((name, base_off)) = self.eval_static_address(ptr_expr) {
                    // Evaluate the integer side as a constant
                    if let Some(int_val) = self.eval_const_init_expr(int_expr) {
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
                    } else if let Some(val) = self.eval_const_init_expr(expr) {
                        Initializer::Int(val)
                    } else {
                        error(
                            self.expr_pos(expr),
                            "non-constant offset in pointer arithmetic initializer",
                        );
                        Initializer::None
                    }
                } else if let Some(val) = self.eval_const_init_expr(expr) {
                    Initializer::Int(val)
                } else {
                    error(
                        self.expr_pos(expr),
                        "non-constant pointer expression in global initializer",
                    );
                    Initializer::None
                }
            }

            // `a ?: b` in a static initializer. The condition is also the
            // value of the true arm, so folding it needs no temporary.
            ExprKind::CondElvis { cond, else_expr } => {
                match self.const_condition(cond) {
                    Some(true) => return self.ast_init_to_ir(cond, typ),
                    Some(false) => return self.ast_init_to_ir(else_expr, typ),
                    None => self.reject_initializer(cond),
                }
                Initializer::None
            }

            // `cond ? a : b`, as CPython's _Py_LATIN1_CHR() writes it.
            ExprKind::Conditional {
                cond,
                then_expr,
                else_expr,
            } => {
                match self.const_condition(cond) {
                    Some(true) => return self.ast_init_to_ir(then_expr, typ),
                    Some(false) => return self.ast_init_to_ir(else_expr, typ),
                    None => self.reject_initializer(cond),
                }
                Initializer::None
            }

            // Other constant expressions
            // Try to evaluate as integer or float constant expression
            _ => {
                // An arithmetic object folded above, at its own type.
                if let Some(val) = self.eval_const_init_expr(expr) {
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

    /// The constant difference of two addresses into one object, in whatever
    /// units the subtraction is written in.
    ///
    /// `ptr - ptr` counts elements (6.5.6p9) and so divides by the pointee
    /// size; a subtraction of two addresses already cast to an integer type
    /// counts bytes and does not. Both sides must resolve to the *same* symbol,
    /// which is what keeps the cross-object form a diagnostic.
    /// A constant whose leaves may include the difference of two addresses
    /// into the same object.
    ///
    /// `(int)((void *)&v.p[1] - (void *)&v) + 1U` is an address constant by
    /// C17 6.6p9, and its value is known at translation time although neither
    /// address is. The ordinary constant evaluator stops at the subtraction,
    /// because an address is not a constant to it; this walks the arithmetic
    /// around one so the whole expression folds.
    pub(crate) fn eval_const_with_address_difference(&mut self, expr: &Expr) -> Option<i128> {
        if let Some(v) = self.eval_const_init_expr(expr) {
            return Some(v);
        }
        match &expr.kind {
            ExprKind::Cast { expr: inner, .. } => self.eval_const_with_address_difference(inner),
            ExprKind::Binary { op, left, right } => {
                if *op == BinaryOp::Sub {
                    if let Some(diff) = self.static_address_difference(left, right) {
                        return Some(diff);
                    }
                }
                let l = self.eval_const_with_address_difference(left)?;
                let r = self.eval_const_with_address_difference(right)?;
                match op {
                    BinaryOp::Add => l.checked_add(r),
                    BinaryOp::Sub => l.checked_sub(r),
                    BinaryOp::Mul => l.checked_mul(r),
                    BinaryOp::Div if r != 0 => l.checked_div(r),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn static_address_difference(&mut self, left: &Expr, right: &Expr) -> Option<i128> {
        let (lname, loff) = self.static_address_operand(left)?;
        let (rname, roff) = self.static_address_operand(right)?;
        if lname != rname {
            return None;
        }
        let bytes = loff - roff;

        // Scaled only when this really is a pointer difference. A cast to an
        // integer type makes it ordinary arithmetic on two byte addresses.
        let is_ptr = |t: Option<TypeId>| {
            t.is_some_and(|t| matches!(self.types.kind(t), TypeKind::Pointer | TypeKind::Array))
        };
        if !is_ptr(left.typ) || !is_ptr(right.typ) {
            return Some(bytes as i128);
        }
        // `void *` has no element size, and C makes the difference of two
        // `void *` a byte count -- which gcc accepts and glibc relies on.
        // Filtering a zero size out and giving up left
        // `(void *)&v.p[0] - (void *)&v` unfoldable.
        let elem = left
            .typ
            .and_then(|t| self.types.base_type(t))
            .map(|t| self.types.size_bytes(t) as i64)
            .unwrap_or(1)
            .max(1);
        Some((bytes / elem) as i128)
    }

    /// An operand of [`Self::static_address_difference`], resolved to the
    /// symbol it addresses and a byte offset into it.
    ///
    /// Deliberately narrower than [`Self::eval_static_address`], which answers
    /// for a bare `Ident` because its caller has already seen the `&`. Reaching
    /// it directly from a subtraction would fold `int v, w; long d = v - w;` to
    /// zero -- reading two variables, which is no kind of constant. So only an
    /// expression that is *shaped* like an address qualifies: an `&`, an array
    /// designator decaying to its first element, pointer arithmetic over one of
    /// those, or any of them behind casts.
    pub(crate) fn static_address_operand(&mut self, expr: &Expr) -> Option<(String, i64)> {
        let is_ptr_or_array = expr
            .typ
            .is_some_and(|t| matches!(self.types.kind(t), TypeKind::Pointer | TypeKind::Array));
        match &expr.kind {
            // A cast changes the units the difference is counted in, not the
            // address itself.
            ExprKind::Cast { expr: inner, .. } => self.static_address_operand(inner),
            ExprKind::Unary {
                op: UnaryOp::AddrOf,
                operand,
            } => self.eval_static_address(operand),
            ExprKind::Ident(_) | ExprKind::Index { .. } | ExprKind::Member { .. }
                if is_ptr_or_array =>
            {
                self.eval_static_address(expr)
            }
            ExprKind::Binary { .. } if is_ptr_or_array => self.eval_static_address(expr),
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
        let value = if let ExprKind::InitList { elements } = &expr.kind {
            &elements.first()?.value
        } else {
            expr
        };
        // Converted as in assignment (C17 6.7.9p11), from the initializer's
        // own type to the object's.
        let base = self.types.complex_base(typ);
        let (re, im) =
            constexpr::eval_complex_as(self, ConstScope::StaticInitializer, value, base)?;

        let base_bytes = self.types.size_bytes(base);
        // A GNU complex integer's halves are integers. Emitting them as
        // floats laid an eight-byte floating image over a four-byte half, so
        // `_Complex int b = 8;` wrote past its own object and zeroed whatever
        // the frame had put next to it.
        let half = |v: FloatVal| {
            if self.types.is_integer(base) {
                // Already an integer of `base`'s range: converting it is exact.
                constexpr::float_to_integer(self.types, v, base).map(Initializer::Int)
            } else {
                Some(Initializer::Float(v))
            }
        };
        let (re_init, im_init) = (half(re)?, half(im)?);
        Some(Initializer::Struct {
            total_size: base_bytes * 2,
            fields: vec![(0, base_bytes, re_init), (base_bytes, base_bytes, im_init)],
        })
    }

    /// Which way a constant controlling expression goes, or None if it is not
    /// a constant this compiler can fold.
    ///
    /// A conditional in a static initializer is folded rather than emitted, so
    /// its condition has to be decided here. Asking `eval_const_init_expr`
    /// alone made that test i128-only, which rejected
    /// `static double d = 1.5 ?: 2.5;` and `static const char *g = "a" ?: "b";`
    /// -- neither of which is an integer, and both of which C17 6.6 makes
    /// perfectly constant.
    fn const_condition(&self, cond: &Expr) -> Option<bool> {
        if let Some(truth) = constexpr::eval_truth(self, ConstScope::StaticInitializer, cond) {
            return Some(truth);
        }
        // An address constant. A string literal has storage of its own and the
        // address of an object or a function is never null, so all of these are
        // true without needing a value.
        match &cond.kind {
            ExprKind::StringLit(_)
            | ExprKind::WideStringLit(_)
            | ExprKind::Utf16StringLit(_)
            | ExprKind::Utf32StringLit(_) => Some(true),
            ExprKind::Unary {
                op: UnaryOp::AddrOf,
                ..
            } => Some(true),
            // A bare identifier is an address constant only when it decays --
            // an array or a function. Anything else is an object's *value*,
            // which is not a constant expression at file scope.
            ExprKind::Ident(_) => self.types.decays(cond.typ?).then_some(true),
            _ => None,
        }
    }

    /// Report an initializer that is not a constant expression we can fold.
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
            // Converted as in assignment (C17 6.7.9p11), from the
            // initializer's own type: `double d = 0.1f;` holds 0.1f widened,
            // not 0.1, and a `double` signalling NaN initializing a `long
            // double` is quieted as the conversion at run time quiets it. An
            // integer constant is exact however wide it is, and rounds once:
            // `long double x = 1;`. A complex constant gives its real part.
            let val = constexpr::eval_as_float(self, ConstScope::StaticInitializer, expr, typ)?;
            return Some(wrap(val));
        }

        // Converting to `_Bool` is not a truncation: every non-zero value
        // becomes 1, so 0.5 is `true` where `(int)0.5` is 0 (C17 6.3.1.2).
        let is_bool = self.types.kind(typ) == TypeKind::Bool;

        if let Some(val) = self.eval_const_init_expr(expr) {
            return Some(Initializer::Int(if is_bool {
                i128::from(val != 0)
            } else {
                val
            }));
        }
        // C17 6.3.1.4: converting a floating constant to an integer type
        // discards the fractional part; a complex one converts its real part.
        let v = match constexpr::eval_as_integer(self, ConstScope::StaticInitializer, expr, typ)? {
            constexpr::IntConversion::InRange(v) => return Some(Initializer::Int(v)),
            constexpr::IntConversion::Saturated(v) => v,
        };
        // Out of range, where C gives no value and a static object must still
        // have one: gcc's, with gcc's warning.
        if crate::diag::warning_group_enabled(OVERFLOW_WARNING) {
            let from = expr
                .typ
                .map_or_else(String::new, |t| self.types.format_type(t, None));
            crate::diag::warning(
                self.expr_pos(expr),
                &format!(
                    "overflow in conversion from '{from}' to '{}' changes value",
                    self.types.format_type(typ, None)
                ),
            );
        }
        Some(Initializer::Int(v))
    }

    /// The position to report for `expr`.
    ///
    /// `linearize_global_decl` has no statement to set `current_pos` from, so
    /// at file scope it stays `None` and a diagnostic reads `file:0`. The
    /// expression carries its own position; prefer it.
    fn expr_pos(&self, expr: &Expr) -> Position {
        if expr.pos != Position::default() {
            expr.pos
        } else {
            self.current_pos.unwrap_or_default()
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
        crate::parse::ast::is_brace_elision_candidate(self.types, element, target_type)
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
        let span =
            crate::parse::ast::brace_elision_span(self.types, elements, *elem_idx, target_type);
        *elem_idx = span.end;
        elements[span]
            .iter()
            .map(|e| InitElement {
                designators: vec![],
                value: e.value.clone(),
            })
            .collect()
    }

    /// Group array init elements by index, handling designators, brace elision,
    /// and nested InitList flattening. Shared between static and runtime paths.
    ///
    /// `array_typ` is the array being initialized, not its element type: the
    /// bound is needed as well as the element type, because an initializer
    /// past the last element is excess (C17 6.7.9p2) and must be *discarded*.
    /// Grouping it anyway gave it an offset beyond the object and both lowering
    /// paths then wrote there -- `int a[2] = {1, 2, 3};` stored the 3 over
    /// whatever the frame put after `a`, and the file-scope form emitted a
    /// third `.long` under a two-element symbol. c17 already warns about the
    /// excess element; this is where it stops mattering.
    pub(crate) fn group_array_init_elements(
        &self,
        elements: &[InitElement],
        array_typ: TypeId,
    ) -> ArrayInitGroups {
        let elem_type = self.types.base_type(array_typ).unwrap_or(self.types.int_id);

        // An absent or zero bound is an array sized *by* this initializer -- an
        // incomplete type `int a[] = {1, 2, 3}`, a flexible array member, or a
        // GNU zero-length array -- so nothing in the list can be excess.
        let last_index = self
            .types
            .array_size(array_typ)
            .filter(|&n| n > 0)
            .map(|n| n as i64 - 1);
        let in_bounds = |idx: i64| last_index.is_none_or(|last| (0..=last).contains(&idx));

        let mut element_lists: HashMap<i64, Vec<InitElement>> = HashMap::new();
        let mut element_indices: Vec<i64> = Vec::new();
        let mut current_idx: i64 = 0;
        let mut elem_idx = 0;

        while elem_idx < elements.len() {
            let element = &elements[elem_idx];
            let (element_index, span_end, index_pos) =
                crate::parse::ast::array_slot(&element.designators, &mut current_idx);

            let remaining_designators = match index_pos {
                Some(pos) => element.designators[pos + 1..].to_vec(),
                None => element.designators.clone(),
            };

            // Brace elision (C99 6.7.8p20): positional scalar for aggregate element
            if remaining_designators.is_empty()
                && self.is_brace_elision_candidate(element, elem_type)
            {
                // Consumed either way: the elements belong to this slot, and
                // leaving them in the list would make the next iteration read
                // them as initializers for the enclosing array.
                let sub_elements = self.consume_brace_elision(elements, &mut elem_idx, elem_type);
                if !in_bounds(element_index) {
                    continue;
                }
                let entry = element_lists.entry(element_index).or_insert_with(|| {
                    element_indices.push(element_index);
                    Vec::new()
                });
                entry.extend(sub_elements);
                continue;
            }

            // A GNU range `[lo ... hi] = v` initializes every element it
            // covers with the same value, so it becomes one entry per element.
            // Expanding here keeps both lowering paths -- the static data
            // image and the runtime stores -- unchanged, and matches how c17
            // already lowers a bulk initializer element by element.
            //
            // A range is clamped rather than dropped whole, so that
            // `[0 ... 4] = 1` on a three-element array still initializes the
            // three elements the array has. Clamping the high endpoint also
            // keeps the loop off the excess indices rather than walking one
            // iteration per discarded element, which matters for an endpoint
            // far past the array.
            let span_end = last_index.map_or(span_end, |last| span_end.min(last));
            if in_bounds(element_index) {
                for target_index in element_index..=span_end {
                    let entry = element_lists.entry(target_index).or_insert_with(|| {
                        element_indices.push(target_index);
                        Vec::new()
                    });

                    if remaining_designators.is_empty() {
                        if let ExprKind::InitList {
                            elements: nested_elements,
                        } = &element.value.kind
                        {
                            entry.extend(nested_elements.clone());
                            continue;
                        }
                    }

                    entry.push(InitElement {
                        designators: remaining_designators.clone(),
                        value: element.value.clone(),
                    });
                }
            }
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
                let mut member_index = None;
                if anon_cont.is_some() {
                    // A continuation is filling the anonymous aggregate at
                    // `outer_idx`, so that is the member this element lands in.
                    member_index = anon_cont.as_ref().map(|cont| cont.outer_idx);
                    member = self.get_anon_continuation_member(
                        &mut anon_cont,
                        members,
                        &mut current_field_idx,
                    );
                    if member.is_none() {
                        member_index = None;
                    }
                }
                if member.is_none() {
                    let picked =
                        self.next_positional_member(members, is_union, &mut current_field_idx);
                    member_index = picked.as_ref().map(|(_, index)| *index);
                    member = picked.map(|(member, _)| member);
                }
                let Some(member) = member else {
                    elem_idx += 1;
                    continue;
                };
                let field_size = self.types.size_bytes(member.typ);

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
                    access_bytes: member.access_bytes,
                    member_index,
                    unions: UnionMembers::default(),
                    quals: member.quals,
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
                access_bytes,
                unions,
                quals,
            }) = resolved
            else {
                elem_idx += 1;
                continue;
            };
            let mut member_index = None;
            if let Some(Designator::Field(name)) = element.designators.first() {
                if let Some(result) = self.member_index_for_designator(members, *name) {
                    match result {
                        MemberDesignatorResult::Direct(next_idx) => {
                            member_index = Some(next_idx - 1);
                            current_field_idx = next_idx;
                            anon_cont = None;
                        }
                        MemberDesignatorResult::Anonymous { outer_idx, levels } => {
                            member_index = Some(outer_idx);
                            current_field_idx = outer_idx;
                            anon_cont = Some(AnonContinuation { outer_idx, levels });
                        }
                    }
                }
            }
            let field_size = self.types.size_bytes(field_type);
            visits.push(StructFieldVisit {
                offset,
                typ: field_type,
                field_size,
                kind: StructFieldVisitKind::Expr(element.value.clone()),
                bit_offset,
                bit_width,
                access_bytes,
                member_index,
                unions,
                quals,
            });
            elem_idx += 1;
        }

        visits
    }

    /// Drop every visit that initializes a flexible array member where gcc
    /// does not allow one to be, reporting each at its initializer.
    ///
    /// Initializing a flexible array member at all is a GNU extension (C17
    /// 6.7.2.1p18 gives it no storage); see [`fam_init_violation`] for where
    /// gcc draws the line. A dropped visit leaves the object's bytes as if
    /// the member had not been named, so nothing further is reported.
    pub(crate) fn admit_fam_visits(
        &self,
        visits: &mut Vec<StructFieldVisit>,
        members: &[crate::types::StructMember],
        storage: InitStorage,
    ) {
        visits.retain(|visit| {
            let Some(message) = self
                .fam_init_form(visit, members)
                .and_then(|form| fam_init_violation(form, storage))
            else {
                return true;
            };
            error(self.visit_pos(visit), &gettextrs::gettext(message));
            false
        });
    }

    /// How `visit` initializes a flexible array member, or `None` if it
    /// initializes some other member.
    fn fam_init_form(
        &self,
        visit: &StructFieldVisit,
        members: &[crate::types::StructMember],
    ) -> Option<FamInit> {
        let member = members.get(visit.member_index?)?;
        if !self.types.is_flexible_array_member(member) {
            return None;
        }
        let form = match &visit.kind {
            // A designator into the member, `.s[1] = c`, initializes elements.
            StructFieldVisitKind::Expr(expr) if self.types.unsized_array_levels(visit.typ) > 0 => {
                match &expr.kind {
                    ExprKind::InitList { elements } if elements.is_empty() => FamInit::Empty,
                    ExprKind::InitList { elements } => match elements.as_slice() {
                        [only] if only.designators.is_empty() && only.value.is_string_literal() => {
                            FamInit::String
                        }
                        _ => FamInit::Elements,
                    },
                    _ if expr.is_string_literal() => FamInit::String,
                    _ => FamInit::Elements,
                }
            }
            _ => FamInit::Elements,
        };
        Some(form)
    }

    /// The position of the initializer a visit stands for.
    fn visit_pos(&self, visit: &StructFieldVisit) -> Position {
        match &visit.kind {
            StructFieldVisitKind::Expr(expr) => self.expr_pos(expr),
            StructFieldVisitKind::BraceElision(elements) => elements
                .first()
                .map_or(self.current_pos.unwrap_or_default(), |e| {
                    self.expr_pos(&e.value)
                }),
        }
    }

    /// The member each union inside the subobject a visit initializes comes
    /// to hold, by byte offset within the object being initialized.
    ///
    /// Answered from the initializer list rather than from the lowered
    /// `Initializer`, so that the static and the automatic path get the same
    /// answer from the same walk. Both need it to resolve a later initializer
    /// naming something inside a union against this one: only where the union
    /// still holds the same member does the rest of that member survive.
    pub(crate) fn held_union_members(
        &self,
        typ: TypeId,
        kind: &StructFieldVisitKind,
        base: usize,
    ) -> UnionMembers {
        let mut held = UnionMembers::default();
        // The walk costs a second pass over the initializer, so it is only
        // made where there is a union to find.
        if self.type_holds_union(typ) {
            self.record_held_unions_of(typ, kind, base, &mut held);
        }
        held
    }

    /// Whether an object of type `typ` has a union anywhere inside it, itself
    /// included. Pointers are not looked through, so this terminates on the
    /// self-referential types C allows.
    fn type_holds_union(&self, typ: TypeId) -> bool {
        let typ = self.resolve_struct_type(typ);
        match self.types.kind(typ) {
            TypeKind::Union => true,
            TypeKind::Struct => self
                .types
                .get(typ)
                .composite
                .as_ref()
                .is_some_and(|composite| {
                    composite
                        .members
                        .iter()
                        .any(|member| self.type_holds_union(member.typ))
                }),
            TypeKind::Array => self
                .types
                .base_type(typ)
                .is_some_and(|elem| self.type_holds_union(elem)),
            _ => false,
        }
    }

    fn record_held_unions(
        &self,
        typ: TypeId,
        elements: &[InitElement],
        base: usize,
        held: &mut UnionMembers,
    ) {
        let typ = self.resolve_struct_type(typ);
        match self.types.kind(typ) {
            TypeKind::Struct | TypeKind::Union => {
                let Some(composite) = self.types.get(typ).composite.as_ref() else {
                    return;
                };
                let members = composite.members.clone();
                let is_union = self.types.kind(typ) == TypeKind::Union;
                let visits = self.walk_struct_init_fields(typ, &members, is_union, elements);
                if is_union {
                    // Each element of a union's initializer list replaces what
                    // the one before it put there, so the union ends up
                    // holding whichever member the last of them named.
                    if let Some(index) = visits.last().and_then(|visit| visit.member_index) {
                        held.record(base, typ, index);
                    }
                }
                for visit in &visits {
                    self.record_held_unions_of(visit.typ, &visit.kind, base + visit.offset, held);
                }
            }
            TypeKind::Array => {
                let Some(elem_type) = self.types.base_type(typ) else {
                    return;
                };
                let elem_size = self.types.size_bytes(elem_type);
                let groups = self.group_array_init_elements(elements, typ);
                for index in groups.indices {
                    let Some(list) = groups.element_lists.get(&index) else {
                        continue;
                    };
                    self.record_held_unions(
                        elem_type,
                        list,
                        base + index as usize * elem_size,
                        held,
                    );
                }
            }
            _ => {}
        }
    }

    fn record_held_unions_of(
        &self,
        typ: TypeId,
        kind: &StructFieldVisitKind,
        base: usize,
        held: &mut UnionMembers,
    ) {
        match kind {
            StructFieldVisitKind::BraceElision(elements) => {
                self.record_held_unions(typ, elements, base, held);
            }
            StructFieldVisitKind::Expr(expr) => {
                if let ExprKind::InitList { elements } = &expr.kind {
                    self.record_held_unions(typ, elements, base, held);
                } else {
                    self.record_value_unions(typ, base, held);
                }
            }
        }
    }

    /// Every union inside an object of type `typ` at `base` holds the bytes
    /// of the one value that initialized the object whole. See
    /// [`Held::Value`].
    fn record_value_unions(&self, typ: TypeId, base: usize, held: &mut UnionMembers) {
        if self.type_holds_union(typ) {
            held.record_value(base..base + self.types.size_bytes(typ));
        }
    }

    /// Where the byte range `offset..offset + size` sits inside an object of
    /// type `typ`, with `offset` measured from that object's first byte.
    ///
    /// Two initializers in one list can describe overlapping storage, and
    /// C17 6.7.9p19 resolves that by *subobject*, not by bytes: given
    /// `{ .t = {1, 2}, .t.y = 9 }` the second names a member of the first and
    /// replaces only it, while given `{ .u.i = 1, .u.s.b = 9 }` the second
    /// names a different member of a union and so replaces the whole of what
    /// the first wrote. The spans are identical in shape -- one inside the
    /// other -- so only the type can tell the two cases apart.
    ///
    /// `unions` says which member each union on the way holds; a union it has
    /// nothing to say about is answered [`SubobjectPlace::ThroughUnion`],
    /// which discards its contents.
    pub(crate) fn classify_subobject(
        &self,
        typ: TypeId,
        offset: usize,
        size: usize,
        unions: UnionFold<'_>,
    ) -> SubobjectPlace {
        self.classify_range(typ, offset, size, false, unions)
    }

    /// The same question for the bytes a *bit-field's* own bits occupy.
    ///
    /// A bit-field is not addressable storage, so it is never a subobject of
    /// its own and the answer wanted is the struct that declares it, whose
    /// initializer holds the carrier the field shares with its neighbours.
    /// An exact byte match is no reason to stop early: `struct { unsigned
    /// char a : 4, b : 4; }` is one byte wide, and replacing it whole would
    /// take `b` with it.
    pub(crate) fn classify_bitfield_carrier(
        &self,
        typ: TypeId,
        offset: usize,
        size: usize,
        unions: UnionFold<'_>,
    ) -> SubobjectPlace {
        self.classify_range(typ, offset, size, true, unions)
    }

    fn classify_range(
        &self,
        typ: TypeId,
        offset: usize,
        size: usize,
        bitfield: bool,
        unions: UnionFold<'_>,
    ) -> SubobjectPlace {
        let mut typ = self.resolve_struct_type(typ);
        let mut base = 0usize;
        let mut unions = unions;

        loop {
            let type_size = self.types.size_bytes(typ);
            if !bitfield && offset == base && size == type_size {
                return SubobjectPlace::Member;
            }
            if size == 0 || offset < base || offset + size > base + type_size {
                return SubobjectPlace::NotASubobject;
            }

            match self.types.kind(typ) {
                // Reached a union without having named it exactly, so the
                // range lies inside one of its members. Which member the union
                // holds is not decidable from the span -- every member starts
                // at the same byte -- so unless both initializers agree on it,
                // giving this range an initializer discards what the union
                // held before.
                TypeKind::Union => {
                    let Some(member) = self.agreed_union_member(typ, base, offset, size, unions)
                    else {
                        return SubobjectPlace::ThroughUnion {
                            offset: base,
                            size: type_size,
                        };
                    };
                    base += member.0;
                    unions = unions.inside(member.0);
                    typ = self.resolve_struct_type(member.1);
                }
                TypeKind::Struct => {
                    let Some(composite) = self.types.get(typ).composite.as_ref() else {
                        return SubobjectPlace::NotASubobject;
                    };
                    // A bit-field is not addressable storage of its own, so a
                    // range inside its carrier is not a subobject.
                    let found = composite.members.iter().find(|member| {
                        member.bit_width.is_none() && {
                            let member_start = base + member.offset;
                            let member_end = member_start + self.types.size_bytes(member.typ);
                            offset >= member_start && offset + size <= member_end
                        }
                    });
                    let Some(member) = found else {
                        // No ordinary member holds these bytes. For a
                        // bit-field's bits that is expected, and this is the
                        // struct that declares it.
                        let declares = bitfield
                            && composite.members.iter().any(|member| {
                                bitfield_byte_span(member)
                                    .is_some_and(|span| self.holds(base, &span, offset, size))
                            });
                        return if declares {
                            SubobjectPlace::BitfieldCarrier {
                                offset: base,
                                size: type_size,
                            }
                        } else {
                            SubobjectPlace::NotASubobject
                        };
                    };
                    base += member.offset;
                    unions = unions.inside(member.offset);
                    typ = self.resolve_struct_type(member.typ);
                }
                TypeKind::Array => {
                    let Some(elem_type) = self.types.base_type(typ) else {
                        return SubobjectPlace::NotASubobject;
                    };
                    let elem_size = self.types.size_bytes(elem_type);
                    if elem_size == 0 {
                        return SubobjectPlace::NotASubobject;
                    }
                    let elem_start = base + ((offset - base) / elem_size) * elem_size;
                    if offset + size > elem_start + elem_size {
                        return SubobjectPlace::NotASubobject;
                    }
                    unions = unions.inside(elem_start - base);
                    base = elem_start;
                    typ = self.resolve_struct_type(elem_type);
                }
                // A scalar with something strictly inside it: only a union or
                // a bit-field carrier can produce that, and neither is a
                // subobject relation.
                _ => return SubobjectPlace::NotASubobject,
            }
        }
    }

    /// Whether the member span `span`, measured from a struct beginning at
    /// `base`, holds all of `offset..offset + size`.
    fn holds(
        &self,
        base: usize,
        span: &std::ops::Range<usize>,
        offset: usize,
        size: usize,
    ) -> bool {
        offset >= base + span.start && offset + size <= base + span.end
    }

    /// The member a union of type `typ`, beginning at `base`, is agreed to
    /// hold -- as `(its offset, its type)` -- when that member holds all of
    /// `offset..offset + size`.
    fn agreed_union_member(
        &self,
        typ: TypeId,
        base: usize,
        offset: usize,
        size: usize,
        unions: UnionFold<'_>,
    ) -> Option<(usize, TypeId)> {
        let index = unions.agreed(typ)?;
        let member = self
            .types
            .get(typ)
            .composite
            .as_ref()?
            .members
            .get(index)
            .filter(|member| member.bit_width.is_none())?;
        let span = member.offset..member.offset + self.types.size_bytes(member.typ);
        self.holds(base, &span, offset, size)
            .then_some((member.offset, member.typ))
    }

    /// An initializer that writes nothing, shaped for `typ` so that
    /// [`Self::subobject_init_mut`] can place entries into it.
    fn empty_aggregate_init(&self, typ: TypeId) -> Option<Initializer> {
        match self.types.kind(typ) {
            TypeKind::Struct | TypeKind::Union => Some(Initializer::Struct {
                total_size: self.types.size_bytes(typ),
                fields: Vec::new(),
            }),
            TypeKind::Array => {
                let elem_type = self.types.base_type(typ)?;
                Some(Initializer::Array {
                    elem_size: self.types.size_bytes(elem_type),
                    total_size: self.types.size_bytes(typ),
                    elements: Vec::new(),
                })
            }
            _ => None,
        }
    }

    /// Fold `new_init` into `init`, the initializer for an object of type
    /// `typ`, so that it initializes the subobject at `offset..offset + size`
    /// and leaves every other subobject as it was.
    ///
    /// The caller has already established with [`Self::classify_subobject`]
    /// that the range *is* such a subobject. Returns false when the existing
    /// initializer's shape cannot express the replacement -- a string literal
    /// standing for a character array, say -- in which case the caller falls
    /// back to discarding the earlier initializer whole.
    pub(crate) fn overlay_subobject(
        &self,
        typ: TypeId,
        init: &mut Initializer,
        offset: usize,
        size: usize,
        new_init: &Initializer,
        unions: UnionFold<'_>,
    ) -> bool {
        match self.subobject_init_mut(typ, init, offset, size, unions) {
            Some(slot) => {
                *slot = new_init.clone();
                true
            }
            None => false,
        }
    }

    /// The initializer for the subobject at `offset..offset + size` of an
    /// object of type `typ` initialized by `init`, to be read or replaced in
    /// place. A subobject the initializer left out gains an empty one, so
    /// that what is written there leaves the rest of its parent alone.
    ///
    /// A string literal is taken apart into its elements first, so that one
    /// of them can be replaced.
    ///
    /// `None` when the existing initializer's shape cannot express the
    /// subobject -- an entry for a bit-field carrier rather than for a
    /// member -- in which case the
    /// caller discards the earlier initializer whole. It may by then have
    /// gained an empty initializer for a member on the way down, which is
    /// harmless precisely because the whole of it is discarded.
    fn subobject_init_mut<'i>(
        &self,
        typ: TypeId,
        init: &'i mut Initializer,
        offset: usize,
        size: usize,
        unions: UnionFold<'_>,
    ) -> Option<&'i mut Initializer> {
        let typ = self.resolve_struct_type(typ);
        if offset == 0 && size == self.types.size_bytes(typ) {
            return Some(init);
        }

        match self.types.kind(typ) {
            TypeKind::Struct | TypeKind::Union => {
                let (slot_offset, slot_type) = if self.types.kind(typ) == TypeKind::Union {
                    // Every member of a union begins at the same byte, so the
                    // range cannot say which one it is in. Only the member the
                    // two initializers agree the union holds will do.
                    self.agreed_union_member(typ, 0, offset, size, unions)?
                } else {
                    self.types
                        .get(typ)
                        .composite
                        .as_ref()?
                        .members
                        .iter()
                        .find(|member| {
                            member.bit_width.is_none() && {
                                let span = member.offset
                                    ..member.offset + self.types.size_bytes(member.typ);
                                self.holds(0, &span, offset, size)
                            }
                        })
                        .map(|member| (member.offset, member.typ))?
                };
                let slot_size = self.types.size_bytes(slot_type);
                let Initializer::Struct { fields, .. } = init else {
                    return None;
                };
                let slot = Self::entry_init_mut(fields, slot_offset, slot_size, || {
                    self.empty_aggregate_init(slot_type)
                })?;
                self.subobject_init_mut(
                    slot_type,
                    slot,
                    offset - slot_offset,
                    size,
                    unions.inside(slot_offset),
                )
            }
            TypeKind::Array => {
                let elem_type = self.types.base_type(typ)?;
                let elem_size = self.types.size_bytes(elem_type);
                if elem_size == 0 {
                    return None;
                }
                let elem_offset = (offset / elem_size) * elem_size;
                if offset + size > elem_offset + elem_size {
                    return None;
                }
                if let Some(exploded) = init.string_as_array(elem_size, self.types.size_bytes(typ))
                {
                    *init = exploded;
                }
                let Initializer::Array { elements, .. } = init else {
                    return None;
                };
                // An array's entries carry no width, so give each one the
                // element width it implicitly has and borrow the struct
                // path's bookkeeping.
                let slot = Self::element_init_mut(elements, elem_offset, || {
                    self.empty_aggregate_init(elem_type)
                })?;
                self.subobject_init_mut(
                    elem_type,
                    slot,
                    offset - elem_offset,
                    size,
                    unions.inside(elem_offset),
                )
            }
            _ => None,
        }
    }

    /// The entry in a struct initializer's field list for the member at
    /// `slot_offset` of `slot_size` bytes, added as an empty initializer if
    /// the list has none.
    ///
    /// `None` when an entry overlaps the member without being exactly it -- a
    /// bit-field carrier byte, or an entry spanning several members -- which
    /// is not something a subobject can be placed into.
    fn entry_init_mut(
        fields: &mut Vec<(usize, usize, Initializer)>,
        slot_offset: usize,
        slot_size: usize,
        empty: impl FnOnce() -> Option<Initializer>,
    ) -> Option<&mut Initializer> {
        let slot_end = slot_offset + slot_size;
        let index = match fields
            .iter()
            .position(|(off, sz, _)| *off < slot_end && slot_offset < *off + *sz)
        {
            Some(index) => {
                if (fields[index].0, fields[index].1) != (slot_offset, slot_size) {
                    return None;
                }
                index
            }
            None => {
                // A scalar member has no aggregate to make empty; whatever is
                // put here is about to be replaced outright, since nothing
                // lies inside a scalar to descend to.
                fields.push((slot_offset, slot_size, empty().unwrap_or_default()));
                fields.sort_by_key(|(off, _, _)| *off);
                fields.iter().position(|(off, _, _)| *off == slot_offset)?
            }
        };
        Some(&mut fields[index].2)
    }

    /// The same for an array initializer's element list, whose entries are
    /// one element wide by construction.
    fn element_init_mut(
        elements: &mut Vec<(usize, Initializer)>,
        elem_offset: usize,
        empty: impl FnOnce() -> Option<Initializer>,
    ) -> Option<&mut Initializer> {
        let index = match elements.iter().position(|(off, _)| *off == elem_offset) {
            Some(index) => index,
            None => {
                elements.push((elem_offset, empty().unwrap_or_default()));
                elements.sort_by_key(|(off, _)| *off);
                elements.iter().position(|(off, _)| *off == elem_offset)?
            }
        };
        Some(&mut elements[index].1)
    }

    /// Apply C17 6.7.9p19 to the initializers one struct or union
    /// initializer list produced, in the order the list wrote them.
    ///
    /// An initializer for a subobject overrides any previously listed
    /// initializer *for that subobject*, and leaves initializers for other
    /// subobjects alone. So a later entry is folded into an earlier one it is
    /// a member of, replaces an earlier one it contains, and -- when the two
    /// are related only through a union or a bit-field carrier, where no
    /// structural fold exists -- discards it.
    ///
    /// "Later" means later in the list. The entries arrive in that order and
    /// the sort into address order, which the emitter needs, runs afterwards.
    pub(crate) fn merge_raw_field_inits(&self, raw: Vec<RawFieldInit>) -> Vec<RawFieldInit> {
        let mut merged: Vec<RawFieldInit> = Vec::with_capacity(raw.len());

        for later in raw {
            let later_span = later.byte_span();
            let mut folded = false;
            let mut idx = 0;

            while idx < merged.len() {
                let earlier = &merged[idx];
                let earlier_span = earlier.byte_span();
                if earlier_span.start >= later_span.end || later_span.start >= earlier_span.end {
                    idx += 1;
                    continue;
                }
                // Two *distinct* bit-fields are different objects even when
                // they share a carrier byte, so both survive.
                if earlier.bit_width.is_some()
                    && later.bit_width.is_some()
                    && (earlier.offset, earlier.bit_offset) != (later.offset, later.bit_offset)
                {
                    idx += 1;
                    continue;
                }
                // A bit-field never supersedes an initializer for an object
                // containing it, however the two spans compare: it is narrower
                // than the storage they share -- `struct { unsigned char a : 4,
                // b : 4; }` is one byte -- so what it does not name stands, and
                // it is folded into the carrier instead.
                let inside = earlier_span.start <= later_span.start
                    && later_span.end <= earlier_span.end
                    && earlier.bit_width.is_none();
                if !(inside && later.bit_width.is_some())
                    && later_span.start <= earlier_span.start
                    && earlier_span.end <= later_span.end
                {
                    merged.remove(idx);
                    continue;
                }
                let foldable = !folded && inside && self.fold_field_init(&mut merged[idx], &later);
                if foldable {
                    folded = true;
                    idx += 1;
                    continue;
                }
                merged.remove(idx);
            }

            if !folded {
                merged.push(later);
            }
        }

        merged
    }

    /// Fold `later`, whose bytes lie inside `earlier`'s, into `earlier`.
    /// Returns false when no structural fold exists, which leaves the caller
    /// to discard `earlier`.
    fn fold_field_init(&self, earlier: &mut RawFieldInit, later: &RawFieldInit) -> bool {
        // `unions` borrows what the earlier entry holds, which the fold then
        // updates, so it reads from a copy.
        let held = earlier.held.clone();
        let unions = UnionFold::new(&held, &later.named, earlier.offset);
        let span = later.byte_span();
        let inner_offset = span.start - earlier.offset;
        let inner_size = span.end - span.start;

        let place = if later.bit_width.is_some() {
            self.classify_bitfield_carrier(earlier.typ, inner_offset, inner_size, unions)
        } else {
            self.classify_subobject(earlier.typ, inner_offset, inner_size, unions)
        };
        let folded = match place {
            SubobjectPlace::Member => self.overlay_subobject(
                earlier.typ,
                &mut earlier.init,
                inner_offset,
                inner_size,
                &later.init,
                unions,
            ),
            // Not storage of its own: only the bits the field names change,
            // inside the carrier its declaring struct's initializer wrote.
            SubobjectPlace::BitfieldCarrier { offset, size } => {
                self.fold_bitfield_init(earlier, later, offset, size, unions)
            }
            // The union stops holding what it held: everything it contained
            // goes, and it comes to hold just this one initializer.
            SubobjectPlace::ThroughUnion { offset, size } => {
                let Some(fields) = self.union_replacement_fields(earlier, later, offset) else {
                    return false;
                };
                let replacement = Initializer::Struct {
                    total_size: size,
                    fields,
                };
                let replaced = self.overlay_subobject(
                    earlier.typ,
                    &mut earlier.init,
                    offset,
                    size,
                    &replacement,
                    unions,
                );
                if replaced {
                    let start = earlier.offset + offset;
                    earlier.held.clear_range(start..start + size);
                }
                replaced
            }
            SubobjectPlace::NotASubobject => false,
        };

        if folded {
            // What the later initializer says about the unions it wrote or
            // passed through is the last word on them.
            earlier.held.absorb(&later.held);
            earlier.held.absorb(&later.named);
        }
        folded
    }

    /// The entries a union comes to hold once `later` replaces its contents:
    /// the initializer itself, or the bytes of a bit-field's carrier, placed
    /// at their offsets within the union beginning at `offset` of `earlier`.
    fn union_replacement_fields(
        &self,
        earlier: &RawFieldInit,
        later: &RawFieldInit,
        offset: usize,
    ) -> Option<Vec<(usize, usize, Initializer)>> {
        let union_start = earlier.offset + offset;
        let (Some(bit_offset), Some(bit_width)) = (later.bit_offset, later.bit_width) else {
            let inner = later.offset.checked_sub(union_start)?;
            return Some(vec![(inner, later.field_size, later.init.clone())]);
        };
        let Initializer::Int(value) = later.init else {
            return None;
        };
        bitfield_carrier_bytes(bit_offset, bit_width, value)
            .map(|(byte, bits, _)| {
                let inner = (later.offset + byte).checked_sub(union_start)?;
                Some((inner, 1, Initializer::Int(i128::from(bits))))
            })
            .collect()
    }

    /// Replace the bits `later` names inside the carrier bytes the struct
    /// declaring it -- at `offset..offset + size` of `earlier`'s object --
    /// already has an initializer for.
    ///
    /// The carrier is whole bytes by the time it reaches here: a struct's
    /// bit-fields are merged into one byte apiece as the last step of lowering
    /// it, downstream of the `Initializer` tree, so there is no bit-field left
    /// in the tree to replace -- only the bits of it that a byte holds.
    fn fold_bitfield_init(
        &self,
        earlier: &mut RawFieldInit,
        later: &RawFieldInit,
        offset: usize,
        size: usize,
        unions: UnionFold<'_>,
    ) -> bool {
        let (Some(bit_offset), Some(bit_width)) = (later.bit_offset, later.bit_width) else {
            return false;
        };
        let Initializer::Int(value) = later.init else {
            return false;
        };
        let Some(carrier) =
            self.subobject_init_mut(earlier.typ, &mut earlier.init, offset, size, unions)
        else {
            return false;
        };
        let Initializer::Struct { fields, .. } = carrier else {
            return false;
        };
        // Where the field's own storage begins within the declaring struct.
        let Some(base) = later.offset.checked_sub(earlier.offset + offset) else {
            return false;
        };
        for (byte, bits, mask) in bitfield_carrier_bytes(bit_offset, bit_width, value) {
            if !replace_carrier_bits(fields, base + byte, bits, mask) {
                return false;
            }
        }
        true
    }

    /// Convert an AST initializer list to an IR Initializer
    /// Lower the initializer of a new object with static storage duration --
    /// a compound literal -- met while lowering another one: it is at its own
    /// top level, not at the enclosing initializer's.
    pub(crate) fn new_static_object_init(
        &mut self,
        elements: &[InitElement],
        typ: TypeId,
    ) -> Initializer {
        let outer = std::mem::take(&mut self.static_init_nesting);
        let init = self.ast_init_list_to_ir(elements, typ);
        self.static_init_nesting = outer;
        init
    }

    /// Mark the levels below as inside a subobject -- an array element if
    /// `array` -- returning the nesting to restore once they are lowered.
    fn enter_static_subobject(&mut self, array: bool) -> StaticInitNesting {
        let outer = self.static_init_nesting;
        self.static_init_nesting = StaticInitNesting {
            nested: true,
            in_array: outer.in_array || array,
        };
        outer
    }

    pub(crate) fn ast_init_list_to_ir(
        &mut self,
        elements: &[InitElement],
        typ: TypeId,
    ) -> Initializer {
        let type_kind = self.types.kind(typ);
        let total_size = self.types.size_bytes(typ);

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

                let elem_size = self.types.size_bytes(elem_type);
                let elem_is_aggregate = matches!(
                    self.types.kind(elem_type),
                    TypeKind::Array | TypeKind::Struct | TypeKind::Union
                );

                let groups = self.group_array_init_elements(elements, typ);
                let mut init_elements = Vec::new();
                let outer = self.enter_static_subobject(true);
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
                self.static_init_nesting = outer;

                init_elements.sort_by_key(|(offset, _)| *offset);

                Initializer::Array {
                    elem_size,
                    total_size,
                    elements: init_elements,
                }
            }

            TypeKind::Struct | TypeKind::Union => {
                let resolved_typ = self.resolve_struct_type(typ);
                let resolved_size = self.types.size_bytes(resolved_typ);
                if let Some(composite) = self.types.get(resolved_typ).composite.as_ref() {
                    let members: Vec<_> = composite.members.clone();
                    let is_union = self.types.kind(resolved_typ) == TypeKind::Union;

                    let mut visits =
                        self.walk_struct_init_fields(resolved_typ, &members, is_union, elements);
                    let storage = InitStorage::Static(self.static_init_nesting);
                    self.admit_fam_visits(&mut visits, &members, storage);

                    // Convert field visits to RawFieldInit by evaluating expressions
                    let mut raw_fields: Vec<RawFieldInit> = Vec::new();
                    let outer = self.enter_static_subobject(false);
                    for visit in visits {
                        let held = self.held_union_members(visit.typ, &visit.kind, visit.offset);
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
                            typ: visit.typ,
                            init: field_init,
                            bit_offset: visit.bit_offset,
                            bit_width: visit.bit_width,
                            held,
                            named: visit.unions,
                        });
                    }
                    self.static_init_nesting = outer;

                    // Initializing the same object twice: the later one wins
                    // (C17 6.7.9p19), and one that names a *subobject* of an
                    // earlier one replaces only that subobject. Resolved in
                    // the order the list wrote them, before the sort below
                    // reorders them by address.
                    let mut raw_fields = self.merge_raw_field_inits(raw_fields);

                    // Sort by the bit each field starts at, so that designated
                    // initializers emit in address order however they were
                    // written -- the emitter fills the gaps between fields and
                    // so requires them sorted and non-overlapping.
                    raw_fields.sort_by_key(|f| f.offset * 8 + f.bit_offset.unwrap_or(0) as usize);

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
                        for (byte, bits, _) in bitfield_carrier_bytes(bit_off, bit_width, value) {
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
        let mut access_bytes = None;
        let mut unions = UnionMembers::default();
        // Qualifiers picked up below `base_type`: `.m.x` inside a `volatile`
        // member `m` reaches a volatile `x`.
        let mut quals = TypeModifiers::empty();

        for (idx, designator) in designators.iter().enumerate() {
            match designator {
                Designator::Field(name) => {
                    let mut resolved = typ;
                    if self.types.kind(resolved) == TypeKind::Array {
                        resolved = self.types.base_type(resolved)?;
                    }
                    resolved = self.resolve_struct_type(resolved);
                    // Naming a member of a union says which member the
                    // initializer is for, and nothing downstream can recover
                    // that: every member of a union begins at `offset`.
                    if self.types.kind(resolved) == TypeKind::Union {
                        if let Some(index) = self.designated_member_index(resolved, *name) {
                            unions.record(offset, resolved, index);
                        }
                    }
                    let member = self.types.find_member(resolved, *name)?;
                    if idx > 0 {
                        quals |= self.types.qualifiers(typ);
                    }
                    quals |= member.quals;
                    offset += member.offset;
                    typ = member.typ;
                    if idx + 1 == designators.len() {
                        bit_offset = member.bit_offset;
                        bit_width = member.bit_width;
                        access_bytes = member.access_bytes;
                    } else {
                        bit_offset = None;
                        bit_width = None;
                        access_bytes = None;
                    }
                }
                // A range inside a *chain* -- `.m[0 ... 3] = v` -- names many
                // offsets, and this resolves to one. Refused rather than
                // silently collapsed to the low endpoint, which would drop
                // every element but the first. A range in a nested list,
                // `.m = {[0 ... 3] = v}`, is the common spelling and goes
                // through `group_array_init_elements` instead.
                Designator::IndexRange(..) => return None,
                Designator::Index(index) => {
                    if self.types.kind(typ) != TypeKind::Array {
                        return None;
                    }
                    let elem_type = self.types.base_type(typ)?;
                    let elem_size = self.types.size_bytes(elem_type);
                    offset += (*index as usize) * elem_size;
                    typ = elem_type;
                    bit_offset = None;
                    bit_width = None;
                    access_bytes = None;
                }
            }
        }

        Some(ResolvedDesignator {
            offset,
            typ,
            bit_offset,
            bit_width,
            access_bytes,
            unions,
            quals: quals & Type::MEMBER_QUALIFIERS,
        })
    }

    /// The index, in `composite_typ`'s member list, of the member a `.name`
    /// designator names -- or of the anonymous member that contains it.
    fn designated_member_index(&self, composite_typ: TypeId, name: StringId) -> Option<usize> {
        let members = &self.types.get(composite_typ).composite.as_ref()?.members;
        match self.member_index_for_designator(members, name)? {
            MemberDesignatorResult::Direct(next_idx) => Some(next_idx - 1),
            MemberDesignatorResult::Anonymous { outer_idx, .. } => Some(outer_idx),
        }
    }

    /// The member the next positional element initializes, and its index in
    /// `members`.
    ///
    /// The index is what tells two members of a union apart: they share a byte
    /// offset, so nothing downstream can recover which one an initializer
    /// chose.
    pub(crate) fn next_positional_member(
        &self,
        members: &[crate::types::StructMember],
        is_union: bool,
        current_field_idx: &mut usize,
    ) -> Option<(MemberInfo, usize)> {
        if is_union {
            if *current_field_idx > 0 {
                return None;
            }
            // C17 6.7.9p17 initializes a union's first member, and 6.7.2.1p13
            // makes the members of an anonymous structure members of the
            // union itself -- so an anonymous aggregate *is* that first
            // member. Requiring a name skipped it and initialized whatever
            // came after:
            //
            //   union { struct { int a, b; }; long q; } u = {{1,2}};
            //
            // wrote `{1,2}` into `q` and left `b` zero, and a union whose
            // members are *all* anonymous found none at all and stayed zero
            // entirely. Unnamed bit-field padding is not a member and is
            // still skipped.
            let (index, member) = members
                .iter()
                .enumerate()
                .find(|(_, m)| m.is_initializable())?;
            *current_field_idx = members.len();
            return Some((
                MemberInfo {
                    offset: member.offset,
                    typ: member.typ,
                    bit_offset: member.bit_offset,
                    bit_width: member.bit_width,
                    access_bytes: member.access_bytes,
                    quals: TypeModifiers::empty(),
                },
                index,
            ));
        }

        while *current_field_idx < members.len() {
            let index = *current_field_idx;
            let member = &members[index];
            *current_field_idx += 1;
            if member.is_initializable() {
                return Some((
                    MemberInfo {
                        offset: member.offset,
                        typ: member.typ,
                        bit_offset: member.bit_offset,
                        bit_width: member.bit_width,
                        access_bytes: member.access_bytes,
                        quals: TypeModifiers::empty(),
                    },
                    index,
                ));
            }
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
                if !inner.is_initializable() {
                    idx += 1;
                    continue;
                }
                // Nested anonymous aggregate — descend into it
                if self.types.is_anonymous_aggregate(inner) {
                    descend_into = Some((inner.typ, base_offset + inner.offset, idx + 1));
                    break;
                }
                // Found a valid named member
                found_member = Some((idx + 1, inner.clone()));
                break;
            }

            if let Some((next_idx, inner)) = found_member {
                // Update the current level's index
                c.levels.last_mut().unwrap().inner_next_idx = next_idx;
                // Every level is an anonymous aggregate the member lives
                // inside, so each one's qualifiers reach it, exactly as
                // `TypeTable::find_member` gathers them for a lookup by name.
                let quals = c.levels.iter().fold(TypeModifiers::empty(), |q, l| {
                    q | (self.types.qualifiers(l.anon_type) & Type::MEMBER_QUALIFIERS)
                });
                return Some(MemberInfo {
                    offset: base_offset + inner.offset,
                    typ: inner.typ,
                    bit_offset: inner.bit_offset,
                    bit_width: inner.bit_width,
                    access_bytes: inner.access_bytes,
                    quals,
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
            if self.types.is_anonymous_aggregate(member) {
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
            if self.types.is_anonymous_aggregate(inner_member) {
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
        false
    }
}

/// Whether an alias, or what it names, is code or data.
///
/// gcc refuses to make one name for the other: a function symbol's value is
/// an address in `.text`, and treating it as an object -- or calling an
/// object -- is never what the program meant.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum AliasKind {
    Function,
    Object,
}

/// An `__attribute__((alias))` declaration, awaiting the end of the unit.
#[derive(Debug, Clone)]
pub(crate) struct DeclaredAlias {
    /// The alias, as emitted.
    name: String,
    /// The target, as written in the attribute.
    target: String,
    kind: AliasKind,
    is_static: bool,
    weak: bool,
    visibility: Option<String>,
    pos: Position,
}

/// Why an alias's target is not something it can name.
enum AliasFault {
    /// Nothing in this unit defines it.
    Undefined,
    /// Defined only as an inline definition, which emits no symbol here.
    External,
}

impl<'a> super::linearize::Linearizer<'a> {
    /// Record `name` as `__attribute__((alias))` for the target in `attrs`.
    ///
    /// Every declaration of the alias may repeat the attribute; the first is
    /// the one recorded.
    pub(crate) fn declare_alias(
        &mut self,
        name: &str,
        attrs: &crate::parse::ast::SymbolAttrs,
        is_static: bool,
        kind: AliasKind,
        pos: Position,
    ) {
        let Some(target) = attrs.alias.clone() else {
            return;
        };
        if self.declared_aliases.iter().any(|a| a.name == name) {
            return;
        }
        self.declared_aliases.push(DeclaredAlias {
            name: name.to_string(),
            target,
            kind,
            is_static,
            weak: attrs.weak,
            visibility: attrs.visibility.clone(),
            pos,
        });
    }

    /// Check every alias against the finished unit and hand the good ones to
    /// the module.
    ///
    /// Done last because C lets the target be defined after the alias, and
    /// because an ordinary redeclaration of the alias -- `extern int b;` after
    /// `extern int b __attribute__((alias("a")));` -- records it as an
    /// external reference, which it is not.
    pub(crate) fn resolve_aliases(&mut self) {
        let declared = std::mem::take(&mut self.declared_aliases);
        for a in &declared {
            self.module.extern_symbols.remove(&a.name);
            self.module.declared_symbol_attrs.remove(&a.name);
            self.module.extern_object_align.remove(&a.name);
        }
        // Mach-O has no symbol aliases -- clang refuses the attribute on
        // Darwin for the same reason -- so there is nothing correct to emit.
        if self.target.os == crate::target::Os::MacOS {
            for a in &declared {
                error(
                    a.pos,
                    &gettextrs::gettext("aliases are not supported on darwin"),
                );
            }
            return;
        }
        for a in &declared {
            if let Some(alias) = self.resolve_alias(&declared, a) {
                self.module.aliases.push(alias);
            }
        }
    }

    /// Check one alias, diagnosing it if it cannot be made.
    fn resolve_alias(&self, declared: &[DeclaredAlias], a: &DeclaredAlias) -> Option<SymbolAlias> {
        let shown = crate::arch::lir::undecorated(&a.name);
        // An inline definition emits no symbol, so it and an alias of the
        // same name coexist: the body is there to inline, and the alias is
        // what an out-of-line call reaches (gcc.c-torture `20011119-1`).
        let defined_here = self
            .module
            .functions
            .iter()
            .any(|f| f.name == a.name && f.emit)
            || self.module.globals.iter().any(|g| g.name == a.name);
        if defined_here {
            crate::diag::error_args(
                a.pos,
                "'{0}' defined both normally and as 'alias' attribute",
                &[shown],
            );
            return None;
        }
        let (target, kind) = match self.alias_target(declared, &a.target) {
            Ok(found) => found,
            Err(AliasFault::Undefined) => {
                crate::diag::error_args(
                    a.pos,
                    "'{0}' aliased to undefined symbol '{1}'",
                    &[shown, &a.target],
                );
                return None;
            }
            Err(AliasFault::External) => {
                crate::diag::error_args(
                    a.pos,
                    "'{0}' aliased to external symbol '{1}'",
                    &[shown, &a.target],
                );
                return None;
            }
        };
        if kind != a.kind {
            crate::diag::error_args(
                a.pos,
                "'{0}' alias between function and variable is not supported",
                &[shown],
            );
            return None;
        }
        Some(SymbolAlias {
            name: a.name.clone(),
            target,
            is_static: a.is_static,
            weak: a.weak,
            visibility: a.visibility.clone(),
        })
    }

    /// The emitted name `target` refers to, and what kind of symbol it
    /// finally names.
    ///
    /// The target is matched by its assembler name, as gcc matches it. It may
    /// itself be an alias: the `.set` names that alias, and the kind is read
    /// off the definition at the end of the chain.
    fn alias_target(
        &self,
        declared: &[DeclaredAlias],
        target: &str,
    ) -> Result<(String, AliasKind), AliasFault> {
        let undecorated = crate::arch::lir::undecorated;
        let mut emitted: Option<String> = None;
        let mut cur = target;
        // One hop per alias at most; anything longer is a cycle, which
        // defines nothing.
        for _ in 0..=declared.len() {
            if let Some(f) = self
                .module
                .functions
                .iter()
                .find(|f| undecorated(&f.name) == cur)
            {
                if !f.emit {
                    return Err(AliasFault::External);
                }
                return Ok((
                    emitted.unwrap_or_else(|| f.name.clone()),
                    AliasKind::Function,
                ));
            }
            if let Some(g) = self
                .module
                .globals
                .iter()
                .find(|g| undecorated(&g.name) == cur)
            {
                return Ok((emitted.unwrap_or_else(|| g.name.clone()), AliasKind::Object));
            }
            let Some(next) = declared.iter().find(|d| undecorated(&d.name) == cur) else {
                return Err(AliasFault::Undefined);
            };
            emitted.get_or_insert_with(|| next.name.clone());
            cur = &next.target;
        }
        Err(AliasFault::Undefined)
    }
}

/// What initializes a flexible array member, as far as where it may appear
/// depends on it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FamInit {
    /// `{}`: nothing, which gcc discards wherever the object is static.
    Empty,
    /// A string literal, braced or not.
    String,
    /// Elements: a braced list, brace-elided values, or a designator into it.
    Elements,
}

/// The diagnostic gcc gives for initializing a flexible array member with
/// `form` in an object of `storage`, or `None` where gcc accepts it.
///
/// An automatic object cannot be given one at all, not even `{}`. In a static
/// object a list of elements is allowed only at the object's own top level,
/// and a string literal anywhere but inside an array element -- each element
/// would have a different size.
fn fam_init_violation(form: FamInit, storage: InitStorage) -> Option<&'static str> {
    let nesting = match storage {
        InitStorage::Automatic => {
            return Some("non-static initialization of a flexible array member")
        }
        InitStorage::Static(nesting) => nesting,
    };
    let rejected = match form {
        FamInit::Empty => false,
        FamInit::String => nesting.in_array,
        FamInit::Elements => nesting.nested,
    };
    rejected.then_some("initialization of flexible array member in a nested context")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::ast::Expr;
    use crate::symbol::SymbolTable;
    use crate::target::Target;
    use crate::types::Type;

    /// A positional initializer element holding an `int` constant.
    fn positional(value: i64, types: &TypeTable) -> InitElement {
        InitElement {
            designators: vec![],
            value: Box::new(Expr::int(value, types)),
        }
    }

    /// The same, addressed by a designator.
    fn designated(designator: Designator, value: i64, types: &TypeTable) -> InitElement {
        InitElement {
            designators: vec![designator],
            value: Box::new(Expr::int(value, types)),
        }
    }

    /// The indices `group_array_init_elements` keeps for `elements` when they
    /// initialize an `int` array of `size` elements (`None`: a bound derived
    /// from the initializer itself, as for `int a[] = {1, 2, 3}`).
    fn kept_indices(
        size: Option<usize>,
        build: impl Fn(&TypeTable) -> Vec<InitElement>,
    ) -> Vec<i64> {
        let target = Target::host();
        let mut types = TypeTable::new(&target);
        let elements = build(&types);
        let array = types.intern(Type {
            kind: TypeKind::Array,
            base: Some(types.int_id),
            array_size: size,
            ..Default::default()
        });
        let symbols = SymbolTable::new();
        let strings = crate::strings::StringTable::new();
        let lin = Linearizer::new(&symbols, &types, &strings, &target);
        let groups = lin.group_array_init_elements(&elements, array);
        assert_eq!(groups.indices.len(), groups.element_lists.len());
        groups.indices
    }

    #[test]
    fn excess_array_elements_are_discarded() {
        // `int a[2] = {1, 2, 3};` -- the third element has nowhere to go.
        let indices = kept_indices(Some(2), |types| {
            (1..=3).map(|v| positional(v, types)).collect()
        });
        assert_eq!(indices, vec![0, 1]);
    }

    #[test]
    fn a_positional_element_past_a_designator_can_be_excess() {
        // `int a[3] = {[2] = 3, 1};` -- C17 6.7.9p17 resumes at index 3, so
        // the `1` is excess although the list is shorter than the array.
        let indices = kept_indices(Some(3), |types| {
            vec![
                designated(Designator::Index(2), 3, types),
                positional(1, types),
            ]
        });
        assert_eq!(indices, vec![2]);
    }

    #[test]
    fn a_range_designator_is_clamped_to_the_array() {
        // `int a[3] = {[0 ... 4] = 7};` initializes the three elements it has.
        let indices = kept_indices(Some(3), |types| {
            vec![designated(Designator::IndexRange(0, 4), 7, types)]
        });
        assert_eq!(indices, vec![0, 1, 2]);
    }

    #[test]
    fn an_initializer_derived_bound_has_no_excess() {
        // `int a[] = {1, 2, 3};` and a flexible array member are sized by the
        // initializer, so every element belongs to the object.
        let indices = kept_indices(None, |types| {
            (1..=3).map(|v| positional(v, types)).collect()
        });
        assert_eq!(indices, vec![0, 1, 2]);
        let indices = kept_indices(Some(0), |types| {
            (1..=3).map(|v| positional(v, types)).collect()
        });
        assert_eq!(indices, vec![0, 1, 2]);
    }

    /// The static image of `int a[2] = {1, 2, 3};` is two elements wide, not
    /// three: the excess element used to become a third `.long` under a
    /// two-element symbol, which the next symbol in the section absorbed.
    #[test]
    fn a_static_array_image_holds_no_excess_element() {
        let target = Target::host();
        let mut types = TypeTable::new(&target);
        let elements: Vec<_> = (1..=3).map(|v| positional(v, &types)).collect();
        let array = types.intern(Type::array(types.int_id, 2));
        let symbols = SymbolTable::new();
        let strings = crate::strings::StringTable::new();
        let mut lin = Linearizer::new(&symbols, &types, &strings, &target);
        let init = lin.ast_init_list_to_ir(&elements, array);
        let Initializer::Array {
            elem_size,
            total_size,
            elements,
        } = init
        else {
            panic!("an array initializer lowers to Initializer::Array, got {init:?}");
        };
        assert_eq!(elem_size, 4);
        assert_eq!(total_size, 8);
        assert_eq!(
            elements
                .iter()
                .map(|(offset, init)| (*offset, init.clone()))
                .collect::<Vec<_>>(),
            vec![(0, Initializer::Int(1)), (4, Initializer::Int(2))],
        );
    }

    #[test]
    fn a_flexible_array_member_initializer_is_placed_as_gcc_places_it() {
        use FamInit::{Elements, Empty, String};
        let top = InitStorage::Static(StaticInitNesting::default());
        let member = InitStorage::Static(StaticInitNesting {
            nested: true,
            in_array: false,
        });
        let element = InitStorage::Static(StaticInitNesting {
            nested: true,
            in_array: true,
        });
        let nested = Some("initialization of flexible array member in a nested context");
        for form in [Empty, String, Elements] {
            assert_eq!(fam_init_violation(form, top), None, "{form:?} at top level");
        }
        assert_eq!(fam_init_violation(Empty, member), None);
        assert_eq!(fam_init_violation(String, member), None);
        assert_eq!(fam_init_violation(Elements, member), nested);
        assert_eq!(fam_init_violation(Empty, element), None);
        assert_eq!(fam_init_violation(String, element), nested);
        assert_eq!(fam_init_violation(Elements, element), nested);
        for form in [Empty, String, Elements] {
            assert_eq!(
                fam_init_violation(form, InitStorage::Automatic),
                Some("non-static initialization of a flexible array member"),
            );
        }
    }

    /// `struct V { int n; char s[]; }` initialized `{1, "x"}`: the string is
    /// laid out after `n` at the object's top level, and dropped -- leaving
    /// `n` alone -- in an array element, where it is rejected.
    #[test]
    fn a_rejected_flexible_array_member_initializer_lays_out_nothing() {
        let target = Target::host();
        let mut types = TypeTable::new(&target);
        let mut strings = crate::strings::StringTable::new();
        let (n, s) = (strings.intern("n"), strings.intern("s"));
        let chars = types.intern(Type {
            kind: TypeKind::Array,
            base: Some(types.char_id),
            array_size: None,
            ..Default::default()
        });
        let v = types.intern(Type::struct_type(composite(
            vec![member(n, types.int_id, 0), member(s, chars, 4)],
            4,
            4,
        )));
        let v_array = types.intern(Type::array(v, 1));
        let list = |elements: Vec<InitElement>| InitElement {
            designators: vec![],
            value: Box::new(Expr::new_unpositioned(ExprKind::InitList { elements })),
        };
        let fields = vec![
            positional(1, &types),
            InitElement {
                designators: vec![],
                value: Box::new(Expr::new_unpositioned(ExprKind::StringLit("x".into()))),
            },
        ];
        let symbols = SymbolTable::new();
        let mut lin = Linearizer::new(&symbols, &types, &strings, &target);
        let offsets = |init: &Initializer| match init {
            Initializer::Struct { fields, .. } => fields.iter().map(|f| f.0).collect::<Vec<_>>(),
            other => panic!("a struct initializer lowers to Initializer::Struct, got {other:?}"),
        };

        let top = lin.ast_init_list_to_ir(&fields, v);
        assert_eq!(offsets(&top), vec![0, 4]);

        let init = lin.ast_init_list_to_ir(&[list(fields)], v_array);
        let Initializer::Array { elements, .. } = init else {
            panic!("an array initializer lowers to Initializer::Array, got {init:?}");
        };
        assert_eq!(elements.len(), 1);
        assert_eq!(offsets(&elements[0].1), vec![0]);
        assert_eq!(lin.static_init_nesting, StaticInitNesting::default());
    }

    // Resolving two initializers that describe overlapping storage
    // (C17 6.7.9p19).

    /// Types for the override tests:
    ///
    /// ```c
    /// struct T { int x, y; };
    /// struct S { struct T t; int z; };
    /// struct A { int a[3]; int z; };
    /// struct W { union { int i; struct { char a, b, c, d; } s; } u; };
    /// struct B { unsigned a : 4, b : 4; };
    /// struct C { struct B t; int z; };
    /// struct N { union { struct T p; int i; } u; };
    /// ```
    struct OverrideTypes {
        target: Target,
        types: TypeTable,
        strings: crate::strings::StringTable,
        symbols: SymbolTable,
        t: TypeId,
        s: TypeId,
        a: TypeId,
        w: TypeId,
        v: TypeId,
        b: TypeId,
        c: TypeId,
        n: TypeId,
        /// The `union { struct T p; int i; }` inside `struct N`.
        n_union: TypeId,
        int_array: TypeId,
    }

    fn member(name: StringId, typ: TypeId, offset: usize) -> crate::types::StructMember {
        crate::types::StructMember {
            name,
            typ,
            offset,
            bit_offset: None,
            bit_width: None,
            access_bytes: None,
            align: crate::types::MemberAlign::NATURAL,
        }
    }

    /// A bit-field of `width` bits at `bit_offset` of a four-byte access span
    /// based at byte 0.
    fn bitfield(
        name: StringId,
        typ: TypeId,
        bit_offset: u32,
        width: u32,
    ) -> crate::types::StructMember {
        crate::types::StructMember {
            name,
            typ,
            offset: 0,
            bit_offset: Some(bit_offset),
            bit_width: Some(width),
            access_bytes: Some(4),
            align: crate::types::MemberAlign::NATURAL,
        }
    }

    fn composite(
        members: Vec<crate::types::StructMember>,
        size: usize,
        align: usize,
    ) -> crate::types::CompositeType {
        crate::types::CompositeType {
            members,
            size,
            align,
            member_align: align,
            is_complete: true,
            ..crate::types::CompositeType::incomplete(None)
        }
    }

    impl OverrideTypes {
        fn new() -> Self {
            let target = Target::host();
            let mut types = TypeTable::new(&target);
            let mut strings = crate::strings::StringTable::new();
            let name = |strings: &mut crate::strings::StringTable, s: &str| strings.intern(s);

            let int = types.int_id;
            let ch = types.char_id;

            let (x, y) = (name(&mut strings, "x"), name(&mut strings, "y"));
            let t = types.intern(Type::struct_type(composite(
                vec![member(x, int, 0), member(y, int, 4)],
                8,
                4,
            )));

            let (t_name, z) = (name(&mut strings, "t"), name(&mut strings, "z"));
            let s = types.intern(Type::struct_type(composite(
                vec![member(t_name, t, 0), member(z, int, 8)],
                12,
                4,
            )));

            let int_array = types.intern(Type::array(int, 3));
            let a_name = name(&mut strings, "a");
            let a = types.intern(Type::struct_type(composite(
                vec![member(a_name, int_array, 0), member(z, int, 12)],
                16,
                4,
            )));

            let (b, c, d) = (
                name(&mut strings, "b"),
                name(&mut strings, "c"),
                name(&mut strings, "d"),
            );
            let chars = types.intern(Type::struct_type(composite(
                vec![
                    member(a_name, ch, 0),
                    member(b, ch, 1),
                    member(c, ch, 2),
                    member(d, ch, 3),
                ],
                4,
                1,
            )));
            let (i, s_name) = (name(&mut strings, "i"), name(&mut strings, "s"));
            let union_u = types.intern(Type::union_type(composite(
                vec![member(i, int, 0), member(s_name, chars, 0)],
                4,
                4,
            )));
            let u_name = name(&mut strings, "u");
            let w = types.intern(Type::struct_type(composite(
                vec![member(u_name, union_u, 0)],
                4,
                4,
            )));

            // `struct V { int k; union { int i; struct { char a, b, c, d; } s; } u; }`
            // -- the union has a sibling, so resetting it must leave `k` be.
            let k = name(&mut strings, "k");
            let v = types.intern(Type::struct_type(composite(
                vec![member(k, int, 0), member(u_name, union_u, 4)],
                8,
                4,
            )));

            // `struct B { unsigned a : 4, b : 4; }` and the `struct C { struct
            // B t; int z; }` that holds it: two bit-fields in one carrier byte,
            // with an ordinary member beside them.
            let uint = types.uint_id;
            let bits = types.intern(Type::struct_type(composite(
                vec![bitfield(a_name, uint, 0, 4), bitfield(b, uint, 4, 4)],
                4,
                4,
            )));
            let bits_holder = types.intern(Type::struct_type(composite(
                vec![member(t_name, bits, 0), member(z, int, 4)],
                8,
                4,
            )));

            // `struct N { union { struct T p; int i; } u; }` -- a union whose
            // members differ in size, so that an override naming a subobject
            // of the one it holds has somewhere to fold into.
            let p_name = name(&mut strings, "p");
            let n_union = types.intern(Type::union_type(composite(
                vec![member(p_name, t, 0), member(i, int, 0)],
                8,
                4,
            )));
            let n = types.intern(Type::struct_type(composite(
                vec![member(u_name, n_union, 0)],
                8,
                4,
            )));

            Self {
                target,
                types,
                strings,
                symbols: SymbolTable::new(),
                t,
                s,
                a,
                w,
                v,
                b: bits,
                c: bits_holder,
                n,
                n_union,
                int_array,
            }
        }

        fn linearizer(&self) -> Linearizer<'_> {
            Linearizer::new(&self.symbols, &self.types, &self.strings, &self.target)
        }
    }

    /// One entry of a struct initializer list, as `walk_struct_init_fields`
    /// hands it over: the subobject it names and the value for it.
    fn raw(offset: usize, typ: TypeId, size: usize, init: Initializer) -> RawFieldInit {
        RawFieldInit {
            offset,
            field_size: size,
            typ,
            init,
            bit_offset: None,
            bit_width: None,
            held: UnionMembers::default(),
            named: UnionMembers::default(),
        }
    }

    /// The same for a bit-field: `offset` is its access span's first byte and
    /// the value goes at `bit_offset..bit_offset + width` of that span.
    fn raw_bits(
        offset: usize,
        typ: TypeId,
        bit_offset: u32,
        width: u32,
        value: i128,
    ) -> RawFieldInit {
        RawFieldInit {
            bit_offset: Some(bit_offset),
            bit_width: Some(width),
            ..raw(offset, typ, 4, Initializer::Int(value))
        }
    }

    /// One `(byte offset, union type, member index)` recorded for a union.
    fn union_member(offset: usize, typ: TypeId, index: usize) -> UnionMembers {
        let mut members = UnionMembers::default();
        members.record(offset, typ, index);
        members
    }

    fn struct_init(total_size: usize, fields: &[(usize, usize, i128)]) -> Initializer {
        Initializer::Struct {
            total_size,
            fields: fields
                .iter()
                .map(|(off, size, value)| (*off, *size, Initializer::Int(*value)))
                .collect(),
        }
    }

    /// `(offset, size, initializer)` for each entry the merge kept, in the
    /// order it kept them.
    fn kept(merged: &[RawFieldInit]) -> Vec<(usize, usize, Initializer)> {
        merged
            .iter()
            .map(|f| (f.offset, f.field_size, f.init.clone()))
            .collect()
    }

    #[test]
    fn a_range_is_a_member_when_struct_members_and_array_elements_reach_it() {
        let fixture = OverrideTypes::new();
        let lin = fixture.linearizer();

        // The whole object, the member `t`, and `t.y` inside it.
        assert_eq!(
            lin.classify_subobject(fixture.s, 0, 12, UnionFold::default()),
            SubobjectPlace::Member
        );
        assert_eq!(
            lin.classify_subobject(fixture.s, 0, 8, UnionFold::default()),
            SubobjectPlace::Member
        );
        assert_eq!(
            lin.classify_subobject(fixture.s, 4, 4, UnionFold::default()),
            SubobjectPlace::Member
        );
        // `a[1]` of `struct A`.
        assert_eq!(
            lin.classify_subobject(fixture.a, 4, 4, UnionFold::default()),
            SubobjectPlace::Member
        );
        // The four bytes straddling `t.y` and `z` are no object at all.
        assert_eq!(
            lin.classify_subobject(fixture.s, 6, 4, UnionFold::default()),
            SubobjectPlace::NotASubobject
        );
    }

    #[test]
    fn a_range_inside_a_union_member_is_reached_through_the_union() {
        let fixture = OverrideTypes::new();
        let lin = fixture.linearizer();

        // `u.s.b` -- one byte, reached only by choosing a union member.
        assert_eq!(
            lin.classify_subobject(fixture.w, 1, 1, UnionFold::default()),
            SubobjectPlace::ThroughUnion { offset: 0, size: 4 }
        );
        // The union named exactly is an ordinary member of `struct W`.
        assert_eq!(
            lin.classify_subobject(fixture.w, 0, 4, UnionFold::default()),
            SubobjectPlace::Member
        );
    }

    /// When both initializers name the same member of a union, the range is
    /// an ordinary subobject reached through it and the rest of that member
    /// stands. When they name different ones, it is not.
    #[test]
    fn a_union_is_descended_into_only_where_both_sides_name_the_same_member() {
        let fixture = OverrideTypes::new();
        let lin = fixture.linearizer();

        // `n.u.p.y` of `struct N`, with the union holding `p`.
        let holds_p = union_member(0, fixture.n_union, 0);
        let names_p = union_member(0, fixture.n_union, 0);
        let names_i = union_member(0, fixture.n_union, 1);
        assert_eq!(
            lin.classify_subobject(fixture.n, 4, 4, UnionFold::new(&holds_p, &names_p, 0)),
            SubobjectPlace::Member
        );
        assert_eq!(
            lin.classify_subobject(fixture.n, 4, 4, UnionFold::new(&holds_p, &names_i, 0)),
            SubobjectPlace::ThroughUnion { offset: 0, size: 8 }
        );
        // And with nothing said about it at all.
        assert_eq!(
            lin.classify_subobject(fixture.n, 4, 4, UnionFold::default()),
            SubobjectPlace::ThroughUnion { offset: 0, size: 8 }
        );
    }

    /// `struct S s = { .t = {1, 2}, .t.y = 9, .z = 7 };` -- the override
    /// names `t.y`, so `t.x` keeps the 1 it was given.
    #[test]
    fn a_contained_override_replaces_only_the_subobject_it_names() {
        let fixture = OverrideTypes::new();
        let lin = fixture.linearizer();
        let int = fixture.types.int_id;

        let merged = lin.merge_raw_field_inits(vec![
            raw(0, fixture.t, 8, struct_init(8, &[(0, 4, 1), (4, 4, 2)])),
            raw(4, int, 4, Initializer::Int(9)),
            raw(8, int, 4, Initializer::Int(7)),
        ]);

        assert_eq!(
            kept(&merged),
            vec![
                (0, 8, struct_init(8, &[(0, 4, 1), (4, 4, 9)])),
                (8, 4, Initializer::Int(7)),
            ]
        );
    }

    /// The same one level further down: `{ .a = {1,2,3}, .a[1] = 9 }` keeps
    /// elements 0 and 2.
    #[test]
    fn a_contained_override_replaces_only_the_array_element_it_names() {
        let fixture = OverrideTypes::new();
        let lin = fixture.linearizer();
        let int = fixture.types.int_id;

        let array = Initializer::Array {
            elem_size: 4,
            total_size: 12,
            elements: vec![
                (0, Initializer::Int(1)),
                (4, Initializer::Int(2)),
                (8, Initializer::Int(3)),
            ],
        };
        let merged = lin.merge_raw_field_inits(vec![
            raw(0, fixture.int_array, 12, array),
            raw(4, int, 4, Initializer::Int(9)),
        ]);

        assert_eq!(
            kept(&merged),
            vec![(
                0,
                12,
                Initializer::Array {
                    elem_size: 4,
                    total_size: 12,
                    elements: vec![
                        (0, Initializer::Int(1)),
                        (4, Initializer::Int(9)),
                        (8, Initializer::Int(3)),
                    ],
                }
            )]
        );
    }

    /// An initializer for a whole subobject replaces every earlier one for a
    /// part of it.
    #[test]
    fn a_containing_override_replaces_the_earlier_entry_whole() {
        let fixture = OverrideTypes::new();
        let lin = fixture.linearizer();
        let int = fixture.types.int_id;

        let merged = lin.merge_raw_field_inits(vec![
            raw(4, int, 4, Initializer::Int(9)),
            raw(0, fixture.t, 8, struct_init(8, &[(0, 4, 1), (4, 4, 2)])),
        ]);

        assert_eq!(
            kept(&merged),
            vec![(0, 8, struct_init(8, &[(0, 4, 1), (4, 4, 2)]))]
        );
    }

    /// Initializers for different members stand side by side.
    #[test]
    fn disjoint_entries_are_all_kept() {
        let fixture = OverrideTypes::new();
        let lin = fixture.linearizer();
        let int = fixture.types.int_id;

        let merged = lin.merge_raw_field_inits(vec![
            raw(0, int, 4, Initializer::Int(1)),
            raw(4, int, 4, Initializer::Int(2)),
            raw(8, int, 4, Initializer::Int(7)),
        ]);

        assert_eq!(
            kept(&merged),
            vec![
                (0, 4, Initializer::Int(1)),
                (4, 4, Initializer::Int(2)),
                (8, 4, Initializer::Int(7)),
            ]
        );
    }

    /// `{ .u.i = 0x01020304, .u.s.b = 9 }` -- a union holds one member at a
    /// time, so naming a second discards what the first wrote rather than
    /// overlaying it.
    #[test]
    fn initializing_a_second_union_member_discards_the_first() {
        let fixture = OverrideTypes::new();
        let lin = fixture.linearizer();
        let (int, ch) = (fixture.types.int_id, fixture.types.char_id);

        let merged = lin.merge_raw_field_inits(vec![
            raw(0, int, 4, Initializer::Int(0x01020304)),
            raw(1, ch, 1, Initializer::Int(9)),
        ]);

        assert_eq!(kept(&merged), vec![(1, 1, Initializer::Int(9))]);
    }

    /// An initializer for a whole struct, then one byte of a different member
    /// of a union inside it: only that union is reset, and the struct's other
    /// members keep what they were given.
    ///
    /// `struct V v = { .k = 5, .u.i = 0x01020304 }` followed by `.u.s.b = 9`.
    #[test]
    fn an_override_through_a_nested_union_resets_only_that_union() {
        let fixture = OverrideTypes::new();
        let lin = fixture.linearizer();
        let ch = fixture.types.char_id;

        let merged = lin.merge_raw_field_inits(vec![
            raw(
                0,
                fixture.v,
                8,
                struct_init(8, &[(0, 4, 5), (4, 4, 0x01020304)]),
            ),
            raw(5, ch, 1, Initializer::Int(9)),
        ]);

        assert_eq!(
            kept(&merged),
            vec![(
                0,
                8,
                Initializer::Struct {
                    total_size: 8,
                    fields: vec![
                        (0, 4, Initializer::Int(5)),
                        (4, 4, struct_init(4, &[(1, 1, 9)])),
                    ],
                }
            )]
        );
    }

    /// A struct whose only member is a union is the union, byte for byte, so
    /// resetting the union replaces the whole entry.
    #[test]
    fn an_override_through_a_union_filling_its_struct_replaces_the_entry() {
        let fixture = OverrideTypes::new();
        let lin = fixture.linearizer();
        let ch = fixture.types.char_id;

        let merged = lin.merge_raw_field_inits(vec![
            raw(0, fixture.w, 4, struct_init(4, &[(0, 4, 0x01020304)])),
            raw(1, ch, 1, Initializer::Int(9)),
        ]);

        assert_eq!(kept(&merged), vec![(0, 4, struct_init(4, &[(1, 1, 9)]))]);
    }

    /// "Later wins" is later in the list, not at a higher address:
    /// `{ .z = 7, .t.y = 9, .t = {1, 2} }` ends with `t` holding `{1, 2}`.
    #[test]
    fn the_override_rule_is_applied_in_source_order() {
        let fixture = OverrideTypes::new();
        let lin = fixture.linearizer();
        let int = fixture.types.int_id;

        let merged = lin.merge_raw_field_inits(vec![
            raw(8, int, 4, Initializer::Int(7)),
            raw(4, int, 4, Initializer::Int(9)),
            raw(0, fixture.t, 8, struct_init(8, &[(0, 4, 1), (4, 4, 2)])),
        ]);

        assert_eq!(
            kept(&merged),
            vec![
                (8, 4, Initializer::Int(7)),
                (0, 8, struct_init(8, &[(0, 4, 1), (4, 4, 2)])),
            ]
        );

        // And the other order, where the narrower one is written last.
        let merged = lin.merge_raw_field_inits(vec![
            raw(0, fixture.t, 8, struct_init(8, &[(0, 4, 1), (4, 4, 2)])),
            raw(8, int, 4, Initializer::Int(7)),
            raw(4, int, 4, Initializer::Int(9)),
        ]);

        assert_eq!(
            kept(&merged),
            vec![
                (0, 8, struct_init(8, &[(0, 4, 1), (4, 4, 9)])),
                (8, 4, Initializer::Int(7)),
            ]
        );
    }

    /// A member the earlier initializer left out gains an entry of its own
    /// rather than costing the whole earlier initializer: `{ .t = {1}, .t.y
    /// = 9 }` keeps `t.x`.
    #[test]
    fn an_override_of_an_uninitialized_member_is_added_to_the_earlier_entry() {
        let fixture = OverrideTypes::new();
        let lin = fixture.linearizer();
        let int = fixture.types.int_id;

        let merged = lin.merge_raw_field_inits(vec![
            raw(0, fixture.t, 8, struct_init(8, &[(0, 4, 1)])),
            raw(4, int, 4, Initializer::Int(9)),
        ]);

        assert_eq!(
            kept(&merged),
            vec![(0, 8, struct_init(8, &[(0, 4, 1), (4, 4, 9)]))]
        );
    }

    /// A bit-field's bytes are its carrier's, not its own, so the answer for
    /// them is the struct that declares it -- and they are no subobject at
    /// all when asked for as an object.
    #[test]
    fn a_bitfields_bytes_name_the_struct_that_declares_it() {
        let fixture = OverrideTypes::new();
        let lin = fixture.linearizer();

        // `t.a` of `struct C`: byte 0, inside `t` at 0..4.
        assert_eq!(
            lin.classify_bitfield_carrier(fixture.c, 0, 1, UnionFold::default()),
            SubobjectPlace::BitfieldCarrier { offset: 0, size: 4 }
        );
        // Asked for `struct B` itself, the carrier byte is the whole of the
        // declaring struct and still must not be replaced whole.
        assert_eq!(
            lin.classify_bitfield_carrier(fixture.b, 0, 1, UnionFold::default()),
            SubobjectPlace::BitfieldCarrier { offset: 0, size: 4 }
        );
        assert_eq!(
            lin.classify_subobject(fixture.c, 0, 1, UnionFold::default()),
            SubobjectPlace::NotASubobject
        );
        // Padding inside a struct with bit-fields is still nothing at all.
        assert_eq!(
            lin.classify_bitfield_carrier(fixture.c, 1, 1, UnionFold::default()),
            SubobjectPlace::NotASubobject
        );
    }

    /// `struct C c = { .t = {1, 2}, .t.a = 3 };` -- the override names one
    /// bit-field, so the one sharing its carrier keeps its value.
    #[test]
    fn a_designated_override_of_a_bitfield_replaces_only_its_bits() {
        let fixture = OverrideTypes::new();
        let lin = fixture.linearizer();
        let uint = fixture.types.uint_id;

        // `{1, 2}` has already been lowered to the carrier byte it produces.
        let merged = lin.merge_raw_field_inits(vec![
            raw(0, fixture.b, 4, struct_init(4, &[(0, 1, 0x21)])),
            raw_bits(0, uint, 0, 4, 3),
        ]);
        assert_eq!(kept(&merged), vec![(0, 4, struct_init(4, &[(0, 1, 0x23)]))]);

        // And the other bit-field of the pair.
        let merged = lin.merge_raw_field_inits(vec![
            raw(0, fixture.b, 4, struct_init(4, &[(0, 1, 0x21)])),
            raw_bits(0, uint, 4, 4, 5),
        ]);
        assert_eq!(kept(&merged), vec![(0, 4, struct_init(4, &[(0, 1, 0x51)]))]);
    }

    /// A bit-field never supersedes an initializer for an object containing
    /// it, but a whole-struct initializer written after one does.
    #[test]
    fn a_whole_struct_initializer_supersedes_an_earlier_bitfield() {
        let fixture = OverrideTypes::new();
        let lin = fixture.linearizer();
        let uint = fixture.types.uint_id;

        let merged = lin.merge_raw_field_inits(vec![
            raw_bits(0, uint, 4, 4, 5),
            raw(0, fixture.b, 4, struct_init(4, &[(0, 1, 0x21)])),
        ]);
        assert_eq!(kept(&merged), vec![(0, 4, struct_init(4, &[(0, 1, 0x21)]))]);
    }

    /// `struct N n = { .u = {1, 2}, .u.p.y = 9 };` -- the union still holds
    /// `p`, and the override names a member *of* `p`, so `p.x` keeps its 1.
    #[test]
    fn an_override_inside_the_held_union_member_folds_into_it() {
        let fixture = OverrideTypes::new();
        let lin = fixture.linearizer();
        let int = fixture.types.int_id;

        let held = union_member(0, fixture.n_union, 0);
        let named = union_member(0, fixture.n_union, 0);
        let merged = lin.merge_raw_field_inits(vec![
            RawFieldInit {
                held,
                ..raw(
                    0,
                    fixture.n_union,
                    8,
                    Initializer::Struct {
                        total_size: 8,
                        fields: vec![(0, 8, struct_init(8, &[(0, 4, 1), (4, 4, 2)]))],
                    },
                )
            },
            RawFieldInit {
                named,
                ..raw(4, int, 4, Initializer::Int(9))
            },
        ]);

        assert_eq!(
            kept(&merged),
            vec![(
                0,
                8,
                Initializer::Struct {
                    total_size: 8,
                    fields: vec![(0, 8, struct_init(8, &[(0, 4, 1), (4, 4, 9)]))],
                }
            )]
        );
    }

    /// The companion case that makes the rule a rule: naming a *different*
    /// member discards what the union held, however the two spans overlap.
    #[test]
    fn an_override_naming_a_different_union_member_still_resets_it() {
        let fixture = OverrideTypes::new();
        let lin = fixture.linearizer();
        let int = fixture.types.int_id;

        let merged = lin.merge_raw_field_inits(vec![
            RawFieldInit {
                held: union_member(0, fixture.n_union, 0),
                ..raw(
                    0,
                    fixture.n_union,
                    8,
                    Initializer::Struct {
                        total_size: 8,
                        fields: vec![(0, 8, struct_init(8, &[(0, 4, 1), (4, 4, 2)]))],
                    },
                )
            },
            RawFieldInit {
                // `.u.i`, the union's other member.
                named: union_member(0, fixture.n_union, 1),
                ..raw(0, int, 4, Initializer::Int(7))
            },
        ]);

        assert_eq!(kept(&merged), vec![(0, 8, struct_init(8, &[(0, 4, 7)]))]);
    }

    /// With nothing said about the union, the merge cannot tell "the same
    /// member, deeper" from "a different member" and takes the reset, which
    /// is the answer that discards rather than invents.
    #[test]
    fn an_override_through_a_union_nothing_names_resets_it() {
        let fixture = OverrideTypes::new();
        let lin = fixture.linearizer();
        let int = fixture.types.int_id;

        let merged = lin.merge_raw_field_inits(vec![
            raw(
                0,
                fixture.n_union,
                8,
                Initializer::Struct {
                    total_size: 8,
                    fields: vec![(0, 8, struct_init(8, &[(0, 4, 1), (4, 4, 2)]))],
                },
            ),
            raw(4, int, 4, Initializer::Int(9)),
        ]);

        assert_eq!(kept(&merged), vec![(0, 8, struct_init(8, &[(4, 4, 9)]))]);
    }
}
