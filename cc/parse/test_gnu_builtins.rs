//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Parser tests for gcc builtins glibc reaches: the machine-instruction
// builtins (`ast::MachineOp`), the `_FloatN` math builtins and
// `__builtin_tgmath`.
//

use super::ast::{ExprKind, MachineOp};
use super::test_parser::parse_expr_for;
use crate::target::{Arch, Os, Target};

fn x86_linux() -> Target {
    Target::new(Arch::X86_64, Os::Linux)
}

/// Each machine builtin parses to its instruction, typed as gcc declares
/// it; a wrong argument is refused as a call through that prototype is.
#[test]
fn test_machine_builtins_parse_with_gccs_prototypes() {
    let target = x86_linux();
    let check = |src: &str, op: MachineOp, ty: &str| {
        let (e, types, _, _) = parse_expr_for(src, &target).unwrap();
        assert!(
            matches!(&e.kind, ExprKind::MachineBuiltin { op: o, .. } if *o == op),
            "{src}: {:?}",
            e.kind
        );
        assert_eq!(types.format_type(e.typ.unwrap(), None), ty, "{src}");
    };
    check(
        "__builtin_ia32_rdtsc()",
        MachineOp::Rdtsc,
        "unsigned long long",
    );
    check(
        "__builtin_ia32_rdtscp((unsigned *)0)",
        MachineOp::Rdtscp,
        "unsigned long long",
    );
    check(
        "__builtin_ia32_cvtpd2ps((double __attribute__((vector_size(16)))){1, 2})",
        MachineOp::Cvtpd2ps,
        "__vector(4) float",
    );
    check(
        "__builtin_thread_pointer()",
        MachineOp::ThreadPointer,
        "void *",
    );
    for bad in [
        "__builtin_ia32_rdtsc(1)",
        "__builtin_ia32_rdtscp()",
        "__builtin_ia32_cvtpd2ps(1.0)",
        "__builtin_thread_pointer(0)",
    ] {
        let before = crate::diag::error_count();
        let parsed = parse_expr_for(bad, &target);
        assert!(
            parsed.is_err() || crate::diag::error_count() > before,
            "{bad} accepted"
        );
    }
}

/// The `_FloatN` forms of the functions computed in place are computed so
/// at their own type, as are `roundeven`'s; the rest are calls of the
/// function of the builtin's own name, at its prototype's types.
#[test]
fn test_float_n_math_builtins_parse() {
    use super::ast::InlineLibraryFn;
    use crate::float::IntegralRounding;
    let target = x86_linux();
    let in_place = |src: &str, want: InlineLibraryFn, ty: &str| {
        let (e, types, _, _) = parse_expr_for(src, &target).unwrap();
        assert!(
            matches!(&e.kind, ExprKind::InlineLibraryCall { func, .. } if *func == want),
            "{src}: {:?}",
            e.kind
        );
        assert_eq!(types.format_type(e.typ.unwrap(), None), ty, "{src}");
    };
    let even = InlineLibraryFn::RoundToIntegral(IntegralRounding::RoundEven);
    in_place("__builtin_roundeven(2.5)", even, "double");
    in_place("__builtin_roundevenf32(2.5f32)", even, "_Float32");
    in_place(
        "__builtin_floorf32x(2.5)",
        InlineLibraryFn::RoundToIntegral(IntegralRounding::Floor),
        "_Float32x",
    );
    in_place(
        "__builtin_fminf64(1.0, 2.0)",
        InlineLibraryFn::FMin,
        "_Float64",
    );
    in_place(
        "__builtin_fmaf32(1, 2, 3)",
        InlineLibraryFn::Fma,
        "_Float32",
    );
    let called = |src: &str, ty: &str| {
        let (e, types, _, _) = parse_expr_for(src, &target).unwrap();
        assert!(
            matches!(e.kind, ExprKind::Call { .. }),
            "{src}: {:?}",
            e.kind
        );
        assert_eq!(types.format_type(e.typ.unwrap(), None), ty, "{src}");
    };
    called("__builtin_roundevenl(2.5L)", "long double");
    called("__builtin_floorf128(2.5f128)", "__float128");
    called("__builtin_lroundf32(2.5f32)", "long");
    called("__builtin_llrintf64x(2.5)", "long long");
    called("__builtin_scalblnf(1.0f, 3L)", "float");
    called("__builtin_ilogbf32x(8.0)", "int");
    called(
        "__builtin_modff64((_Float64)2.5, (_Float64 *)0)",
        "_Float64",
    );
    let before = crate::diag::error_count();
    let _ = parse_expr_for("__builtin_modff32(2.5f32, 1.5)", &target);
    assert!(crate::diag::error_count() > before, "a modff32 of a double");
}

/// `__builtin_tgmath` is the call of the function its arguments pick, and
/// nothing else: an integer argument picks the `double` one.
#[test]
fn test_tgmath_is_the_picked_call() {
    let decls = "float f(float); double g(double);\n";
    assert_eq!(tgmath_pick(decls, "double", "f, g, 1"), "g");
}

/// gcc 13's rules beyond the exact match: an integer is `_Float32x` beside
/// a `_FloatNx` argument, and where every function returns one real
/// floating type (a narrowing macro), a type no function takes reaches the
/// first function listed whose parameter type holds its values.
#[test]
fn test_tgmath_narrowing_and_integer_rules() {
    let narrow = "float fd(double, double); float fl(long double, long double);\n";
    assert_eq!(tgmath_pick(narrow, "float", "fd, fl, 1.0f, 1.0f"), "fd");
    assert_eq!(
        tgmath_pick(narrow, "float", "fd, fl, 1, (_Float32x)1"),
        "fd"
    );
    assert_eq!(
        tgmath_pick(narrow, "float", "fd, fl, 1.0f, (_Float64x)1"),
        "fl"
    );
    let f32x = "_Float32x a(_Float64x); _Float32x b(_Float64); _Float32x c(_Float128);\n";
    assert_eq!(tgmath_pick(f32x, "_Float32x", "a, b, c, 1.0"), "a");
    assert_eq!(tgmath_pick(f32x, "_Float32x", "a, b, c, 1.0f64"), "b");
    assert_eq!(tgmath_pick(f32x, "_Float32x", "a, b, c, 1.0f128"), "c");
    let plain = "double d(double, double); _Float32x x(_Float32x, _Float32x);\n";
    assert_eq!(tgmath_pick(plain, "double", "d, x, 1, 1.0"), "d");
    assert_eq!(tgmath_pick(plain, "double", "d, x, 1, (_Float32x)1"), "x");
}

/// The name of the function `__builtin_tgmath(args)` calls, after `decls`,
/// in a function returning `ret`.
fn tgmath_pick(decls: &str, ret: &str, args: &str) -> String {
    use super::ast::{BlockItem, ExternalDecl, Stmt};
    use super::test_parser::parse_tu_for;
    let src = format!("{decls}{ret} h(void) {{ return __builtin_tgmath({args}); }}\n");
    let (tu, _, strings, symbols) = parse_tu_for(&src, &x86_linux()).unwrap();
    let body = tu
        .items
        .iter()
        .find_map(|d| match d {
            ExternalDecl::FunctionDef(fd) => Some(&fd.body),
            _ => None,
        })
        .expect("h is defined");
    let Stmt::Block(items) = body else {
        panic!("{body:?}")
    };
    let Some(BlockItem::Statement(stmt)) = items.first() else {
        panic!("{items:?}")
    };
    let Stmt::Return(Some(e)) = &**stmt else {
        panic!("{stmt:?}")
    };
    let ExprKind::Call { func, .. } = &e.kind else {
        panic!("{:?}", e.kind)
    };
    let ExprKind::Ident(sym) = func.kind else {
        panic!("{:?}", func.kind)
    };
    strings.get(symbols.get(sym).name).to_string()
}

/// The `__builtin_ia32_*` names are x86-64's alone; the thread pointer is
/// every target's.
#[test]
fn test_machine_builtins_belong_to_their_target() {
    let a64 = Target::new(Arch::Aarch64, Os::Linux);
    let (e, ..) = parse_expr_for("__builtin_thread_pointer()", &a64).unwrap();
    assert!(matches!(
        e.kind,
        ExprKind::MachineBuiltin {
            op: MachineOp::ThreadPointer,
            ..
        }
    ));
    assert!(!crate::builtins::available_on("__builtin_ia32_rdtsc", &a64));
    assert!(crate::builtins::available_on(
        "__builtin_ia32_rdtsc",
        &x86_linux()
    ));
}

/// aarch64's floating-point register builtins parse to their reads and
/// writes, typed as gcc 13 declares them, and exist on aarch64 alone.
#[test]
fn test_aarch64_fp_register_builtins() {
    use super::ast::FpSysReg::{Fpcr, Fpsr};
    let a64 = Target::new(Arch::Aarch64, Os::Linux);
    for (src, op, ty) in [
        (
            "__builtin_aarch64_get_fpcr()",
            MachineOp::FpRegRead {
                reg: Fpcr,
                wide: false,
            },
            "unsigned int",
        ),
        (
            "__builtin_aarch64_get_fpsr64()",
            MachineOp::FpRegRead {
                reg: Fpsr,
                wide: true,
            },
            "unsigned long long",
        ),
        (
            "__builtin_aarch64_set_fpsr(1)",
            MachineOp::FpRegWrite {
                reg: Fpsr,
                wide: false,
            },
            "void",
        ),
        (
            "__builtin_aarch64_set_fpcr64(1)",
            MachineOp::FpRegWrite {
                reg: Fpcr,
                wide: true,
            },
            "void",
        ),
    ] {
        let (e, types, _, _) = parse_expr_for(src, &a64).unwrap();
        assert!(
            matches!(&e.kind, ExprKind::MachineBuiltin { op: o, .. } if *o == op),
            "{src}: {:?}",
            e.kind
        );
        assert_eq!(types.format_type(e.typ.unwrap(), None), ty, "{src}");
    }
    for name in ["__builtin_aarch64_get_fpcr", "__builtin_aarch64_set_fpsr64"] {
        assert!(crate::builtins::available_on(name, &a64), "{name}");
        assert!(!crate::builtins::available_on(name, &x86_linux()), "{name}");
    }
}
