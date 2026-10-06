//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Builtins gcc makes for its own lowering, which code may also call
//
// `__builtin_cexpi` is what gcc folds `cexp(I * x)` and `sincos` pairs into;
// `__builtin_clear_padding` is what it lowers C++ atomics' compare-exchange
// with. Each has gcc's argument rules and diagnostics here. The stack-pointer
// pair, `__builtin_stack_save` and `__builtin_stack_restore`, has an ordinary
// prototype and is parsed with `alloca` (`parse_memory_builtin`).
//

use super::ast::{Expr, ExprKind};
use super::library_builtin::ProtoType;
use super::parser::{ParseResult, Parser};
use crate::diag;
use crate::kw;
use crate::strings::StringId;
use crate::token::lexer::Position;
use crate::types::{TypeId, TypeKind, TypeModifiers};

/// Each `__builtin_cexpi`: the real type of its argument, the complex type
/// of its result, and the library function that computes it.
#[rustfmt::skip]
pub(super) const CEXPI_BUILTINS: &[(StringId, ProtoType, ProtoType, StringId)] = {
    use ProtoType::*;
    &[
        (kw::BUILTIN_CEXPI,  Double,     ComplexDouble,     kw::CEXP),
        (kw::BUILTIN_CEXPIF, Float,      ComplexFloat,      kw::CEXPF),
        (kw::BUILTIN_CEXPIL, LongDouble, ComplexLongDouble, kw::CEXPL),
    ]
};

/// The builtin's name, as gcc's diagnostics give it.
const NAME: &str = "__builtin_clear_padding";

impl Parser<'_> {
    /// `__builtin_cexpi(x)`, which is `cos x + i sin x`: a call to `cexp` of
    /// `+0 + xi`.
    ///
    /// gcc calls `sincos` where the C library has it, and `cexp` where it
    /// does not. `cexp` is the definition, so its answer is exactly what
    /// `cexp(I * x)` gives -- glibc computes it as `sincos` scaled by
    /// `exp(+0)`, which is exactly 1 -- and it exists on every target,
    /// macOS included, which has no `sincos`. The argument is checked and
    /// converted as gcc's prototype `double _Complex (double)` says.
    pub(super) fn parse_cexpi(
        &mut self,
        name: StringId,
        real: ProtoType,
        complex: ProtoType,
        cexp: StringId,
        pos: Position,
    ) -> ParseResult<Expr> {
        let complex_type = complex.id(self.types);
        let real_type = real.id(self.types);
        let args = self.parse_prototyped_builtin(name, complex, &[real], false)?;
        let Some(x) = args.and_then(|args| args.into_iter().next()) else {
            return Ok(self.diagnosed_call(complex_type, pos));
        };
        let int_zero = Self::typed_expr(ExprKind::IntLit(0), self.types.int_id, pos);
        let zero = self.convert_operand(int_zero, real_type);
        let z = Self::typed_expr(
            ExprKind::BuiltinComplex {
                real: Box::new(zero),
                imag: Box::new(x),
            },
            complex_type,
            pos,
        );
        Ok(self.libm_call(cexp, complex_type, &[complex_type], vec![z], pos))
    }

    /// `__builtin_clear_padding(ptr)`: gcc's `void (...)` checked by its own
    /// rules -- one argument, a pointer to a complete object type that is
    /// not `const`, with no flexible array member. A pointer to a variable
    /// length array is complete; a pointer to a function is accepted, and
    /// clears nothing.
    pub(super) fn parse_clear_padding(&mut self, pos: Position) -> ParseResult<Expr> {
        let name = kw::BUILTIN_CLEAR_PADDING;
        let void = self.types.void_id;
        let Some(ptr) = self
            .parse_generic_builtin_args(name, 1, 1)?
            .and_then(|args| args.into_iter().next())
        else {
            return Ok(self.diagnosed_call(void, pos));
        };
        let Some(pointee) = self.clear_padding_pointee(&ptr, pos) else {
            return Ok(self.diagnosed_call(void, pos));
        };
        Ok(Self::typed_expr(
            ExprKind::ClearPadding {
                ptr: Box::new(ptr),
                pointee,
            },
            void,
            pos,
        ))
    }

    /// The object type `ptr` points at, or `None` once gcc's complaint about
    /// it has been made.
    fn clear_padding_pointee(&mut self, ptr: &Expr, pos: Position) -> Option<TypeId> {
        // An argument whose type is unknown was reported already.
        let typ = ptr.typ?;
        let decayed = self.decayed_type(typ);
        let pointee = (self.types.kind(decayed) == TypeKind::Pointer)
            .then(|| self.types.base_type(decayed))
            .flatten();
        let Some(pointee) = pointee else {
            diag::error_args(
                ptr.pos,
                "argument 1 in call to function '{0}' does not have pointer type",
                &[NAME],
            );
            return None;
        };
        if self.type_name_is_incomplete(pointee) {
            diag::error_args(
                ptr.pos,
                "argument 1 in call to function '{0}' points to incomplete type",
                &[NAME],
            );
            return None;
        }
        if self.is_const_object(pointee) {
            let shown = self.types.format_type(decayed, Some(self.idents));
            diag::error_args(
                ptr.pos,
                "argument 1 in call to function '{0}' has pointer to 'const' type ('{1}')",
                &[NAME, &shown],
            );
            return None;
        }
        let mut object = pointee;
        while self.types.kind(object) == TypeKind::Array {
            object = self.types.base_type(object)?;
        }
        if let Err(member) = crate::ir::padding::value_bits(self.types, object) {
            let member = self.idents.get_opt(member).unwrap_or("");
            diag::error_args(
                pos,
                "flexible array member '{0}' does not have well defined padding bits for '{1}'",
                &[member, NAME],
            );
            return None;
        }
        Some(pointee)
    }

    /// Whether an object of type `typ` is `const`: an array is when its
    /// elements are.
    fn is_const_object(&self, mut typ: TypeId) -> bool {
        loop {
            if self.types.modifiers(typ).contains(TypeModifiers::CONST) {
                return true;
            }
            match self.types.kind(typ) {
                TypeKind::Array => match self.types.base_type(typ) {
                    Some(elem) => typ = elem,
                    None => return false,
                },
                _ => return false,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::test_parser::parse_expr_for;
    use crate::parse::ast::{CalleeBinding, ExprKind};
    use crate::target::{Arch, Os, Target};
    use crate::types::TypeKind;

    fn x86() -> Target {
        Target::new(Arch::X86_64, Os::Linux)
    }

    #[test]
    fn stack_save_and_restore_are_their_nodes() {
        let (e, types, _, _) = parse_expr_for("__builtin_stack_save()", &x86()).unwrap();
        assert!(matches!(e.kind, ExprKind::StackSave));
        assert_eq!(e.typ, Some(types.void_ptr_id));

        let (e, types, _, _) =
            parse_expr_for("__builtin_stack_restore(__builtin_stack_save())", &x86()).unwrap();
        let ExprKind::StackRestore { ptr } = &e.kind else {
            panic!("{:?}", e.kind);
        };
        assert!(matches!(ptr.kind, ExprKind::StackSave));
        assert_eq!(e.typ, Some(types.void_id));
    }

    #[test]
    fn cexpi_calls_cexp_of_a_pure_imaginary_argument() {
        for (call, callee) in [
            ("__builtin_cexpi(1)", "cexp"),
            ("__builtin_cexpif(1)", "cexpf"),
            ("__builtin_cexpil(1)", "cexpl"),
        ] {
            let (e, types, strings, symbols) = parse_expr_for(call, &x86()).unwrap();
            let ExprKind::Call {
                func,
                args,
                binding,
                ..
            } = &e.kind
            else {
                panic!("{call}: {:?}", e.kind);
            };
            assert_eq!(*binding, CalleeBinding::Library);
            let ExprKind::Ident(sym) = func.kind else {
                panic!("{call}");
            };
            assert_eq!(strings.get(symbols.get(sym).name), callee);
            let ret = e.typ.unwrap();
            assert!(types.is_complex(ret), "{call}");
            let [z] = args.as_slice() else {
                panic!("{call}");
            };
            assert_eq!(z.typ, Some(ret));
            let ExprKind::BuiltinComplex { real, imag } = &z.kind else {
                panic!("{call}: {:?}", z.kind);
            };
            // The real part is +0 and the argument, converted, is the
            // imaginary part.
            let half = types.complex_base(ret);
            assert_eq!((real.typ, imag.typ), (Some(half), Some(half)), "{call}");
        }
    }

    /// The library's `cpow`, declared with its prototype, so a real
    /// argument is converted to the complex parameter as in any call.
    #[test]
    fn cpow_calls_the_library_through_its_prototype() {
        for (call, callee, complex) in [
            ("__builtin_cpow(1.0, 75)", "cpow", TypeKind::Double),
            ("__builtin_cpowf(1.0f, 2)", "cpowf", TypeKind::Float),
            ("__builtin_cpowl(1.0L, 2)", "cpowl", TypeKind::LongDouble),
        ] {
            let (e, types, strings, symbols) = parse_expr_for(call, &x86()).unwrap();
            let ExprKind::Call { func, binding, .. } = &e.kind else {
                panic!("{call}: {:?}", e.kind);
            };
            assert_eq!(*binding, CalleeBinding::Library);
            let ExprKind::Ident(sym) = func.kind else {
                panic!("{call}");
            };
            let sym = symbols.get(sym);
            assert_eq!(strings.get(sym.name), callee);
            let ret = e.typ.unwrap();
            assert!(types.is_complex(ret), "{call}");
            assert_eq!(types.kind(types.complex_base(ret)), complex, "{call}");
            let params = types.get(sym.typ).params.clone().unwrap();
            assert_eq!(params, vec![ret, ret], "{call}");
        }
    }

    #[test]
    fn clear_padding_records_the_pointee_and_yields_void() {
        let (e, types, _, _) = parse_expr_for(
            "__builtin_clear_padding((struct S { char a; int b; } *)0)",
            &x86(),
        )
        .unwrap();
        let ExprKind::ClearPadding { pointee, .. } = e.kind else {
            panic!("{:?}", e.kind);
        };
        assert_eq!(types.kind(pointee), TypeKind::Struct);
        assert_eq!(types.size_bytes(pointee), 8);
        assert_eq!(e.typ, Some(types.void_id));
    }
}
