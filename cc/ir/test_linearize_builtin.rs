//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Linearizer tests for builtins and variable arguments: library calls that
// become opcodes, `va_list`, `va_arg` and `__builtin_va_arg_pack`.
//

use super::test_linearize::{
    insns_of, linearize_no_ssa, linearize_source, linearize_source_under, make_simple_func,
    test_pos, TestContext,
};
use super::*;
use crate::float::FpFormat;
use crate::ir::NanCompare;
use crate::parse::ast::{
    BlockItem, Declaration, ExprKind, ExternalDecl, FunctionDef, InitDeclarator, ParamStyle,
    Parameter, Stmt,
};
use crate::target::{Arch, Os, Target};
use crate::types::Type;

// Frame/Return address builtin tests

#[test]
fn test_frame_address_emits_opcode() {
    // Test: return __builtin_frame_address(2);
    // Should emit FrameAddress opcode
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let void_ptr = ctx.types.void_ptr_id;

    let frame_addr_expr = Expr::typed_unpositioned(ExprKind::FrameAddress { level: 2 }, void_ptr);

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: void_ptr,
        name: test_id,
        params: vec![],
        body: Stmt::Return(Some(frame_addr_expr)),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
        param_style: ParamStyle::Prototype,
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };
    let module = ctx.linearize(&tu);

    let func = &module.functions[0];
    // The level travels as an immediate, not as a source pseudo.
    let insn = func
        .blocks
        .iter()
        .flat_map(|bb| bb.insns.iter())
        .find(|insn| insn.op == Opcode::FrameAddress)
        .expect("__builtin_frame_address should emit FrameAddress opcode");
    assert_eq!(insn.frame_level(), 2);
    assert!(insn.src.is_empty());
}

#[test]
fn test_return_address_emits_opcode() {
    // Test: return __builtin_return_address(2);
    // Should emit ReturnAddress opcode
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let void_ptr = ctx.types.void_ptr_id;

    let return_addr_expr = Expr::typed_unpositioned(ExprKind::ReturnAddress { level: 2 }, void_ptr);

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: void_ptr,
        name: test_id,
        params: vec![],
        body: Stmt::Return(Some(return_addr_expr)),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
        param_style: ParamStyle::Prototype,
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };
    let module = ctx.linearize(&tu);

    let func = &module.functions[0];
    let insn = func
        .blocks
        .iter()
        .flat_map(|bb| bb.insns.iter())
        .find(|insn| insn.op == Opcode::ReturnAddress)
        .expect("__builtin_return_address should emit ReturnAddress opcode");
    assert_eq!(insn.frame_level(), 2);
    assert!(insn.src.is_empty());
}

// va_list parameter handling tests
// ============================================================================
// va_list parameter handling tests

#[test]
fn test_valist_parameter_stored_as_pointer() {
    // Test: va_list parameter is stored as 8-byte pointer (not 24-byte struct)
    // because va_list decays to pointer at call site due to being an array type.
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");

    // Create va_list type
    let valist_type = ctx
        .types
        .intern(Type::basic(crate::types::TypeKind::VaList));

    // Create symbol for parameter va
    let va_sym = ctx.var("va", valist_type);

    // Function: void test(va_list va) { }
    let func = FunctionDef {
        attrs: Default::default(),
        return_type: ctx.types.void_id,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(va_sym),
            typ: valist_type,
            vm_dims: vec![],
            discarded_dims: vec![],
        }],
        body: Stmt::Block(vec![]),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
        param_style: ParamStyle::Prototype,
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    // Observed before SSA conversion: the width of the parameter's store is the linearizer's job; SSA then promotes it away.
    let module = linearize_no_ssa(&tu, &ctx.types, &ctx.strings, &ctx.symbols);
    let ir = format!("{}", module.display(&ctx.types));

    // The va_list parameter should be stored with .64 (pointer size),
    // not .192 (full va_list struct size of 24 bytes = 192 bits)
    assert!(
        ir.contains("store.64"),
        "va_list parameter should be stored as 64-bit pointer, got: {}",
        ir
    );
    assert!(
        !ir.contains("store.192"),
        "va_list parameter should NOT be stored as 192-bit struct, got: {}",
        ir
    );
}

#[test]
fn test_valist_local_not_indirect() {
    // Test: va_list local declaration (not parameter) should be direct storage
    // Only va_list parameters are indirect (holding a pointer).
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");

    // Create va_list type
    let valist_type = ctx
        .types
        .intern(Type::basic(crate::types::TypeKind::VaList));

    // Create symbol for local va_list
    let lva_sym = ctx.var("lva", valist_type);

    // Function: void test(void) { va_list lva; }
    let lva_decl = Declaration {
        declarators: vec![InitDeclarator {
            fn_effect: Default::default(),
            cleanup: None,
            symbol_attrs: Default::default(),
            pos: Position::default(),
            symbol: lva_sym,
            typ: valist_type,
            storage_class: crate::types::TypeModifiers::empty(),
            init: None,
            vla_sizes: vec![],
            explicit_align: None,
        }],
    };

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: ctx.types.void_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![BlockItem::Declaration(lva_decl)]),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
        param_style: ParamStyle::Prototype,
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    // Use linearize_no_ssa so mem2reg doesn't drop the unused local before
    // we get to inspect it (this test checks the linearizer's storage shape,
    // not what later passes do with an unused declaration).
    let module = linearize_no_ssa(&tu, &ctx.types, &ctx.strings, &ctx.symbols);

    // Check that the function has a local for lva (may have suffix like ".0")
    let test_func = module.functions.iter().find(|f| f.name == "test");
    assert!(test_func.is_some(), "Should have function 'test'");

    let func = test_func.unwrap();
    let has_lva_local = func.locals.keys().any(|k| k.starts_with("lva"));
    assert!(
        has_lva_local,
        "Should have local starting with 'lva'. Locals: {:?}",
        func.locals.keys().collect::<Vec<_>>()
    );
}

#[test]
fn test_valist_expression_decay() {
    // Test: va_list used in expression context should decay to pointer
    // This is similar to array decay - va_list is __va_list_tag[1]
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");

    // Create va_list and pointer types
    let valist_type = ctx
        .types
        .intern(Type::basic(crate::types::TypeKind::VaList));
    let uchar_ptr_type = ctx.types.pointer_to(ctx.types.uchar_id);

    // Create symbols
    let va_sym = ctx.var("va", valist_type);
    let ptr_sym = ctx.var("ptr", uchar_ptr_type);

    // va_list va (parameter)
    // unsigned char *ptr = (unsigned char*)va;
    let cast_expr = Expr::typed_unpositioned(
        ExprKind::Cast {
            cast_type: uchar_ptr_type,
            expr: Box::new(Expr::var_typed(va_sym, valist_type)),
        },
        uchar_ptr_type,
    );

    let ptr_decl = Declaration {
        declarators: vec![InitDeclarator {
            fn_effect: Default::default(),
            cleanup: None,
            symbol_attrs: Default::default(),
            pos: Position::default(),
            symbol: ptr_sym,
            typ: uchar_ptr_type,
            storage_class: crate::types::TypeModifiers::empty(),
            init: Some(cast_expr),
            vla_sizes: vec![],
            explicit_align: None,
        }],
    };

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: ctx.types.void_id,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(va_sym),
            typ: valist_type,
            vm_dims: vec![],
            discarded_dims: vec![],
        }],
        body: Stmt::Block(vec![BlockItem::Declaration(ptr_decl)]),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
        param_style: ParamStyle::Prototype,
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    // Observed before SSA conversion: the load of the va pointer is the linearizer's job; SSA then promotes it away.
    let module = linearize_no_ssa(&tu, &ctx.types, &ctx.strings, &ctx.symbols);
    let ir = format!("{}", module.display(&ctx.types));

    // The cast from va_list to pointer should involve loading the va pointer
    // (since va is a parameter and is_indirect), not taking symaddr of va_list struct
    assert!(
        ir.contains("load.64"),
        "Cast of va_list parameter should load the pointer value, got: {}",
        ir
    );
}

/// `__builtin_va_arg_pack()` is not an argument: it stands for the caller's
/// whole argument list, which is unknown until the enclosing function is
/// inlined. The linearizer therefore lifts it off the argument list and
/// records it on the call, rather than emitting an operand for it.
#[test]
fn test_va_arg_pack_becomes_a_flag_not_an_argument() {
    use crate::ir::Opcode;

    let mut ctx = TestContext::new();
    let int_t = ctx.int_type();
    let name = ctx.str("fwd");
    let target = ctx.str("target");

    // fwd(): target(1, __builtin_va_arg_pack());
    let call = Expr {
        kind: ExprKind::Call {
            func: Box::new(Expr {
                kind: ExprKind::Ident(ctx.var("target", int_t)),
                typ: Some(int_t),
                pos: test_pos(),
                bitfield_bits: None,
            }),
            args: vec![
                Expr {
                    kind: ExprKind::IntLit(1),
                    typ: Some(int_t),
                    pos: test_pos(),
                    bitfield_bits: None,
                },
                Expr {
                    kind: ExprKind::VaArgPack,
                    typ: Some(ctx.types.void_id),
                    pos: test_pos(),
                    bitfield_bits: None,
                },
            ],
            binding: Default::default(),
            known: None,
        },
        typ: Some(int_t),
        pos: test_pos(),
        bitfield_bits: None,
    };
    let _ = target;

    let body = Stmt::Block(vec![BlockItem::Statement(Box::new(Stmt::Expr(call)))]);
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(make_simple_func(
            name, body, &ctx.types,
        ))],
    };
    let module = ctx.linearize(&tu);

    let calls: Vec<_> = module.functions[0]
        .blocks
        .iter()
        .flat_map(|b| &b.insns)
        .filter(|i| i.op == Opcode::Call)
        .collect();
    assert_eq!(calls.len(), 1, "expected one call");

    // The pack is recorded, and contributed no operand: only the `1` is there.
    assert!(
        calls[0].extra().ends_with_va_arg_pack,
        "the call should carry the pack"
    );
    assert_eq!(
        calls[0].src.len(),
        1,
        "the pack must not become an argument: {:?}",
        calls[0].src
    );
    assert_eq!(calls[0].extra().arg_types.len(), calls[0].src.len());
}

/// `sqrt` is the `Sqrt` opcode, naming its library function, with a call to
/// that function behind an ordered `x < 0` so that a domain error still sets
/// `errno`.
#[test]
fn test_sqrt_keeps_a_call_for_errno() {
    let src = "double sqrt(double);\ndouble f(double x) { return sqrt(x); }\n";
    let module = linearize_source(src, &Target::new(Arch::X86_64, Os::Linux));
    let insns = insns_of(&module, "f");
    let sqrt: Vec<_> = insns.iter().filter(|i| i.op == Opcode::Sqrt).collect();
    assert_eq!(sqrt.len(), 1);
    assert_eq!(sqrt[0].extra().func_name.as_deref(), Some("sqrt"));
    let calls: Vec<_> = insns.iter().filter(|i| i.op == Opcode::Call).collect();
    assert_eq!(calls.len(), 1);
    assert_eq!(calls[0].extra().func_name.as_deref(), Some("sqrt"));
    assert!(insns.iter().any(|i| i.op == Opcode::FCmpOLt));
    assert!(insns.iter().any(|i| i.op == Opcode::Phi));
}

/// A rounding is its opcode, naming its library function, with no call on
/// any path -- `floor` of a `float` at `float`, by `floorf`, widened after.
#[test]
fn test_rounding_is_one_opcode() {
    use crate::float::IntegralRounding::{Floor, NearbyInt};
    let src = "double floor(double); double nearbyint(double);\n\
               double f(float x, double y) { return floor(x) + nearbyint(y); }\n";
    let module = linearize_source(src, &Target::new(Arch::X86_64, Os::Linux));
    let insns = insns_of(&module, "f");
    assert!(!insns.iter().any(|i| i.op == Opcode::Call));
    let floor = insns
        .iter()
        .find(|i| i.op == Opcode::RoundToIntegral(Floor))
        .expect("floor");
    assert_eq!(floor.extra().func_name.as_deref(), Some("floorf"));
    assert_eq!(floor.size, 32, "computed at float");
    let nearby = insns
        .iter()
        .find(|i| i.op == Opcode::RoundToIntegral(NearbyInt))
        .expect("nearbyint");
    assert_eq!(nearby.extra().func_name.as_deref(), Some("nearbyint"));
    assert_eq!(nearby.size, 64);
}

/// `fma` is one three-operand opcode, and `fmin` and `fmax` two-operand
/// ones, each naming its function; nothing is called.
#[test]
fn test_min_max_fma_are_opcodes() {
    let src = "double fma(double, double, double); float fminf(float, float);\n\
               double fmax(double, double);\n\
               double f(double x, float y) { return fma(x, x, x) + fminf(y, y) + fmax(x, 1.0); }\n";
    let module = linearize_source(src, &Target::new(Arch::Aarch64, Os::Linux));
    let insns = insns_of(&module, "f");
    assert!(!insns.iter().any(|i| i.op == Opcode::Call));
    for (op, name, srcs, size) in [
        (Opcode::Fma, "fma", 3, 64),
        (Opcode::FMin, "fminf", 2, 32),
        (Opcode::FMax, "fmax", 2, 64),
    ] {
        let insn = insns.iter().find(|i| i.op == op).expect(name);
        assert_eq!(insn.extra().func_name.as_deref(), Some(name));
        assert_eq!((insn.src.len(), insn.size), (srcs, size), "{name}");
    }
}

/// `memcpy`, `memset` and `memmove`, bare or reserved, are their block
/// memory opcodes, naming the library function the IR calls when it does
/// not expand them.
#[test]
fn test_memory_builtins_are_their_opcodes() {
    let src = "void f(char *d, char *s, int c, unsigned long n) {\n\
                   memcpy(d, s, n); __builtin_memset(d, c, n); memmove(d, s, n); }\n";
    let module = linearize_source(src, &Target::new(Arch::X86_64, Os::Linux));
    let ops: Vec<_> = insns_of(&module, "f")
        .iter()
        .filter(|i| matches!(i.op, Opcode::Memcpy | Opcode::Memset | Opcode::Memmove))
        .map(|i| (i.op, i.extra().func_name.as_deref(), i.src.len()))
        .collect();
    assert_eq!(
        ops,
        [
            (Opcode::Memcpy, Some("memcpy"), 3),
            (Opcode::Memset, Some("memset"), 3),
            (Opcode::Memmove, Some("memmove"), 3),
        ]
    );
}

/// `mempcpy` is a `Memcpy` that calls `memcpy`, and its value the
/// destination advanced by the length; `bcopy` is a `Memmove` that calls
/// `memmove`, with its source and destination put back in `memmove`'s
/// order. An asm label on `memcpy` reaches the one `mempcpy` makes.
#[test]
fn test_mempcpy_and_bcopy_are_memcpy_and_memmove() {
    let src = "void *memcpy(void *, const void *, unsigned long) __asm(\"my_memcpy\");\n\
               void *f(char *d, char *s, unsigned long n) {\n\
                   bcopy(s, d, n); return mempcpy(d, s, n); }\n";
    let module = linearize_source(src, &Target::new(Arch::X86_64, Os::Linux));
    let insns = insns_of(&module, "f");
    let blocks: Vec<_> = insns
        .iter()
        .filter(|i| matches!(i.op, Opcode::Memcpy | Opcode::Memmove))
        .collect();
    let [bcopy, mempcpy] = blocks.as_slice() else {
        panic!("two block moves: {blocks:?}");
    };
    let my_memcpy = crate::arch::lir::verbatim("my_memcpy");
    assert_eq!(
        (bcopy.op, bcopy.library_callee()),
        (Opcode::Memmove, "memmove")
    );
    assert_eq!(
        (mempcpy.op, mempcpy.library_callee()),
        (Opcode::Memcpy, my_memcpy.as_str())
    );
    // Both write `d` from `s`: each operand loaded from the same parameter.
    let origin = |p| {
        let def = insns.iter().find(|i| i.target == Some(p)).unwrap();
        (def.op, def.src.clone())
    };
    for k in 0..2 {
        assert_eq!(origin(bcopy.src[k]), origin(mempcpy.src[k]), "operand {k}");
    }
    assert_ne!(origin(bcopy.src[0]), origin(bcopy.src[1]));
    let end = insns
        .iter()
        .find(|i| i.op == Opcode::Add && i.src == [mempcpy.src[0], mempcpy.src[2]])
        .expect("mempcpy's value is d + n");
    assert_eq!(end.size, 64);
}

/// Under `-fno-math-errno` the opcode stands alone; on a target without the
/// instruction (binary128 on aarch64) the call alone does everything.
#[test]
fn test_sqrt_without_errno_or_without_an_instruction() {
    let policy = crate::parse::LibraryCallPolicy {
        optimizing: true,
        math_errno: false,
    };
    let src = "double sqrt(double);\ndouble f(double x) { return sqrt(x); }\n";
    let (module, _) = linearize_source_under(src, &Target::new(Arch::X86_64, Os::Linux), policy);
    let ops: Vec<Opcode> = insns_of(&module, "f").iter().map(|i| i.op).collect();
    assert!(ops.contains(&Opcode::Sqrt));
    assert!(!ops.contains(&Opcode::Call) && !ops.contains(&Opcode::FCmpOLt));

    let src = "long double sqrtl(long double);\n\
               long double f(long double x) { return sqrtl(x); }\n";
    let module = linearize_source(src, &Target::new(Arch::Aarch64, Os::Linux));
    let insns = insns_of(&module, "f");
    assert!(!insns
        .iter()
        .any(|i| i.op == Opcode::Sqrt || i.op == Opcode::FCmpOLt));
    let calls: Vec<_> = insns.iter().filter(|i| i.op == Opcode::Call).collect();
    assert_eq!(calls.len(), 1);
    assert_eq!(calls[0].extra().func_name.as_deref(), Some("sqrtl"));
}

/// `abs` and its siblings become the branch-free `(x ^ s) - s` sequence
/// rather than a call -- including when the translation unit defines the
/// function itself, which is undefined behaviour and does not displace it.
#[test]
fn test_int_abs_is_linearized_without_a_call() {
    let src = "long labs(long);\n\
               int abs(int v) { return 42; }\n\
               long f(int a, long b) { return abs(a) + labs(b) + __builtin_llabs(b); }\n";
    let module = linearize_source(src, &Target::host());
    let f = module.functions.iter().find(|f| f.name == "f").unwrap();
    let insns: Vec<&Instruction> = f.blocks.iter().flat_map(|bb| bb.insns.iter()).collect();
    assert!(
        !insns.iter().any(|i| i.op == Opcode::Call),
        "f still calls a library function"
    );
    let count = |op| insns.iter().filter(|i| i.op == op).count();
    assert_eq!(count(Opcode::Asr), 3);
    assert_eq!(count(Opcode::Xor), 3);
    let widths: Vec<u32> = insns
        .iter()
        .filter(|i| i.op == Opcode::Asr)
        .map(|i| i.size)
        .collect();
    assert_eq!(widths, vec![32, 64, 64]);
}

/// `conj` of a call calls once: it lowers as the conjugate `~z` of one
/// evaluated operand, where it used to read `__real__` and `__imag__` of two
/// copies of the argument expression, each with its own call.
#[test]
fn test_conj_evaluates_its_argument_once() {
    let src = "double _Complex g(void);\n\
               double _Complex conj(double _Complex);\n\
               double _Complex f(void) { return conj(g()); }\n\
               double _Complex h(void) { return __builtin_conj(g()); }\n";
    let module = linearize_source(src, &Target::host());
    for name in ["f", "h"] {
        let func = module.functions.iter().find(|f| f.name == name).unwrap();
        let calls = func
            .blocks
            .iter()
            .flat_map(|bb| bb.insns.iter())
            .filter(|i| i.op == Opcode::Call)
            .count();
        assert_eq!(calls, 1, "{name}");
    }
}

/// `va_arg` of a complex type writes a local of its own, exactly as it does
/// for a struct: a complex value travels by address at every size, so the
/// result has to *be* storage. A bare register pseudo -- what a scalar gets --
/// had the backend write the value into it and every consumer then
/// dereference that value as the address of the two halves.
#[test]
fn test_va_arg_of_complex_writes_a_local() {
    let src = "struct P { double a, b; };\n\
               void use(void *);\n\
               void f(int n, ...) {\n\
                   __builtin_va_list ap;\n\
                   __builtin_va_start(ap, n);\n\
                   float _Complex fc = __builtin_va_arg(ap, float _Complex);\n\
                   double _Complex dc = __builtin_va_arg(ap, double _Complex);\n\
                   long double _Complex lc = __builtin_va_arg(ap, long double _Complex);\n\
                   int _Complex ic = __builtin_va_arg(ap, int _Complex);\n\
                   _Float16 _Complex hc = __builtin_va_arg(ap, _Float16 _Complex);\n\
                   struct P p = __builtin_va_arg(ap, struct P);\n\
                   double d = __builtin_va_arg(ap, double);\n\
                   use(&fc); use(&dc); use(&lc); use(&ic); use(&hc); use(&p); use(&d);\n\
                   __builtin_va_end(ap);\n\
               }\n";
    for target in [
        Target::new(crate::target::Arch::X86_64, crate::target::Os::Linux),
        Target::new(crate::target::Arch::Aarch64, crate::target::Os::Linux),
        Target::new(crate::target::Arch::Aarch64, crate::target::Os::MacOS),
    ] {
        let module = linearize_source(src, &target);
        let f = module.functions.iter().find(|f| f.name == "f").unwrap();
        let names_storage: Vec<bool> = f
            .blocks
            .iter()
            .flat_map(|bb| bb.insns.iter())
            .filter(|i| i.op == Opcode::VaArg)
            .map(|i| {
                matches!(
                    f.get_pseudo(i.target.unwrap()).map(|p| &p.kind),
                    Some(crate::ir::PseudoKind::Sym(_))
                )
            })
            .collect();
        // Five complex types and a struct name a local; the double does not.
        assert_eq!(
            names_storage,
            [true, true, true, true, true, true, false],
            "{target:?}"
        );
    }
}

// Floating comparisons: signaling or quiet

/// The float comparisons `f` computes, in order.
fn float_cmps(module: &Module, name: &str) -> Vec<Opcode> {
    insns_of(module, name)
        .iter()
        .map(|i| i.op)
        .filter(|op| op.is_float_comparison())
        .collect()
}

/// C17 F.9.3: `<`, `<=`, `>` and `>=` are IEEE 754's signaling predicates,
/// and raise invalid for a quiet NaN; `==`, `!=` and the 7.12.14 macros are
/// the quiet ones. So is every comparison the compiler makes up for itself:
/// `isfinite` and `fpclassify` raise nothing for a NaN either, and neither
/// does the boolean test of a float.
#[test]
fn test_relational_operators_signal_and_everything_else_is_quiet() {
    use Opcode::*;
    let cases: &[(&str, &[Opcode])] = &[
        ("a < b", &[FCmpsOLt]),
        ("a <= b", &[FCmpsOLe]),
        ("a > b", &[FCmpsOGt]),
        ("a >= b", &[FCmpsOGe]),
        ("!(a < b)", &[FCmpsOLt]),
        ("a < 1", &[FCmpsOLt]),
        ("a == b", &[FCmpOEq]),
        ("a != b", &[FCmpONe]),
        ("!a", &[FCmpOEq]),
        ("__builtin_isless(a, b)", &[FCmpOLt]),
        ("__builtin_islessequal(a, b)", &[FCmpOLe]),
        ("__builtin_isgreater(a, b)", &[FCmpOGt]),
        ("__builtin_isgreaterequal(a, b)", &[FCmpOGe]),
        ("__builtin_islessgreater(a, b)", &[FCmpOLt, FCmpOGt]),
        ("__builtin_isunordered(a, b)", &[FCmpONe, FCmpONe]),
        ("__builtin_isfinite(a)", &[FCmpOLt, FCmpOGt]),
    ];
    // Every format with a quiet relational of its own. aarch64's binary128
    // `long double` has none, and builds `isless` differently: see
    // `test_quiet_binary128_relational_is_guarded_by_an_unordered_test`.
    let x86 = Target::new(Arch::X86_64, Os::Linux);
    let a64 = Target::new(Arch::Aarch64, Os::Linux);
    let formats = [
        (&x86, "float"),
        (&x86, "double"),
        (&x86, "long double"),
        (&a64, "float"),
        (&a64, "double"),
    ];
    for (target, t) in formats {
        for (expr, want) in cases {
            let src = format!("int f({t} a, {t} b) {{ return {expr}; }}");
            let module = linearize_source(&src, target);
            assert_eq!(
                float_cmps(&module, "f"),
                *want,
                "{:?} {t}: {expr}",
                target.arch
            );
        }
    }
    let src = "int f(double a) { return __builtin_fpclassify(0, 1, 2, 3, 4, a); }";
    for target in [&x86, &a64] {
        let module = linearize_source(src, target);
        let cmps = float_cmps(&module, "f");
        assert!(!cmps.is_empty());
        assert!(
            cmps.iter()
                .all(|op| op.float_cmp().unwrap().nan() == NanCompare::Quiet),
            "{:?}: {cmps:?}",
            target.arch
        );
    }
}

/// binary128 has no quiet relational of its own -- libgcc's ordering helpers
/// all signal -- so `isless` on aarch64's `long double` is the signaling
/// comparison behind an unordered test, as gcc builds it. `<` needs no
/// guard: it is meant to signal.
#[test]
fn test_quiet_binary128_relational_is_guarded_by_an_unordered_test() {
    let target = Target::new(Arch::Aarch64, Os::Linux);
    for t in ["long double", "__float128"] {
        let src = format!(
            "int q({t} a, {t} b) {{ return __builtin_isless(a, b); }}\n\
             int s({t} a, {t} b) {{ return a < b; }}\n"
        );
        let module = linearize_source(&src, &target);
        let q = insns_of(&module, "q");
        let ops: Vec<Opcode> = q.iter().map(|i| i.op).collect();
        assert_eq!(
            float_cmps(&module, "q"),
            [Opcode::FCmpONe, Opcode::FCmpONe, Opcode::FCmpsOLt],
            "{t}"
        );
        assert!(ops.contains(&Opcode::Cbr), "{t}: {ops:?}");
        assert_eq!(float_cmps(&module, "s"), [Opcode::FCmpsOLt], "{t}");
        assert!(
            !insns_of(&module, "s").iter().any(|i| i.op == Opcode::Cbr),
            "{t}"
        );
    }
    // The hardware formats keep the one quiet compare.
    let module = linearize_source(
        "int q(double a, double b) { return __builtin_isless(a, b); }",
        &target,
    );
    assert_eq!(float_cmps(&module, "q"), [Opcode::FCmpOLt]);
}

/// A conditional expression evaluates one arm (C17 6.5.15p4), so an arm is
/// computed unconditionally only when evaluating it cannot be observed. A
/// float relational can: it raises invalid for a NaN, which
/// `isnan(a) ? 0 : a < b` exists to avoid. Equality cannot.
#[test]
fn test_a_relational_arm_is_not_evaluated_unconditionally() {
    let src = "int r(int c, double a, double b) { return c ? a < b : 0; }\n\
               int e(int c, double a, double b) { return c ? a == b : 0; }\n\
               int i(int c, int a, int b) { return c ? a < b : 0; }\n";
    let module = linearize_source(src, &Target::host());
    let branches = |name| insns_of(&module, name).iter().any(|i| i.op == Opcode::Cbr);
    assert!(branches("r"), "a signaling compare stays behind its branch");
    assert!(!branches("e"), "a quiet compare is speculated");
    assert!(!branches("i"), "an integer compare is speculated");
}

// `__builtin_issignaling`: a test of the representation

/// Run `f`, the linearized `int f(T x) { return __builtin_issignaling(x); }`,
/// with `x` holding the bytes `image`: what `f`'s integer loads from the
/// temporary that `x` is stored to read back. Everything else it computes is
/// straight-line integer arithmetic, evaluated by the optimizer's own
/// folding.
fn run_issignaling(module: &Module, image: [u8; 16]) -> i128 {
    let f = module.functions.iter().find(|f| f.name == "f").unwrap();
    let mut vals: std::collections::HashMap<PseudoId, i128> = Default::default();
    let mut temp = None;
    for insn in f.blocks.iter().flat_map(|bb| bb.insns.iter()) {
        let value = |vals: &std::collections::HashMap<PseudoId, i128>, id: PseudoId| {
            vals.get(&id)
                .copied()
                .or_else(|| match f.get_pseudo(id)?.kind {
                    PseudoKind::Val(v) => Some(v),
                    _ => None,
                })
        };
        match insn.op {
            Opcode::SymAddr => {
                let sym = f.get_pseudo(insn.src[0]).map(|p| &p.kind);
                if matches!(sym, Some(PseudoKind::Sym(n)) if n.starts_with("__snan")) {
                    temp = insn.target;
                }
            }
            Opcode::Load if Some(insn.src[0]) == temp => {
                let at = insn.offset as usize;
                let bytes = insn.size as usize / 8;
                let mut word = [0u8; 16];
                word[..bytes].copy_from_slice(&image[at..at + bytes]);
                vals.insert(insn.target.unwrap(), u128::from_le_bytes(word) as i128);
            }
            Opcode::Ret => return value(&vals, insn.src[0]).expect("a computed result"),
            op if crate::ir::constfold::is_int_foldable(op) || op == Opcode::Copy => {
                let ops: Vec<i128> = insn.src.iter().filter_map(|&s| value(&vals, s)).collect();
                if ops.len() != insn.src.len() {
                    continue; // the floating operand, on its way to the store
                }
                let v = if op == Opcode::Copy {
                    ops[0]
                } else {
                    crate::ir::constfold::eval_int(insn, &ops).expect("folds")
                };
                vals.insert(insn.target.unwrap(), v);
            }
            _ => {}
        }
    }
    panic!("f returned nothing")
}

/// The interesting values of `fmt`, each with whether it is a signalling
/// NaN, as images of the format's bits.
fn issignaling_cases(fmt: FpFormat) -> Vec<(String, u128, bool)> {
    use crate::float::{FloatVal, NanKind};
    let mut values = vec![
        (
            "sNaN",
            FloatVal::nan_with_payload(fmt, 0, NanKind::Signalling),
            true,
        ),
        (
            "sNaN 0x123",
            FloatVal::nan_with_payload(fmt, 0x123, NanKind::Signalling),
            true,
        ),
        (
            "sNaN 1",
            FloatVal::nan_with_payload(fmt, 1, NanKind::Signalling),
            true,
        ),
        (
            "qNaN",
            FloatVal::nan_with_payload(fmt, 0, NanKind::Quiet),
            false,
        ),
        (
            "qNaN 0x234",
            FloatVal::nan_with_payload(fmt, 0x234, NanKind::Quiet),
            false,
        ),
        ("inf", FloatVal::infinity(false), false),
        ("zero", FloatVal::ZERO, false),
        ("1.5", FloatVal::from_f64(1.5), false),
        (
            "min subnormal",
            FloatVal::from_parts(false, 1, -16000),
            false,
        ),
    ];
    // Every sign: issignaling ignores it.
    let negated: Vec<_> = values
        .iter()
        .map(|&(n, v, s)| (n, v.negated(), s))
        .collect();
    values.extend(negated);
    let mut cases: Vec<(String, u128, bool)> = values
        .into_iter()
        .map(|(n, v, s)| (format!("{n} (sign {})", v.sign_bit()), v.to_bits(fmt), s))
        .collect();
    let top = |bits: u32| 1u128 << (bits - 1);
    match fmt {
        // The x87 encodings no IEEE format has, as gcc and glibc count them:
        // a nonzero exponent with the integer bit clear signals.
        FpFormat::X87Extended => {
            let x87 = |sign_exp: u128, sig: u128| (sign_exp << 64) | sig;
            for (name, bits, s) in [
                ("pseudo-infinity", x87(0x7fff, 0), true),
                ("pseudo-NaN", x87(0x7fff, 42), true),
                (
                    "pseudo-NaN, quiet bit",
                    x87(0xffff, 0x4000_0000_0000_0042),
                    true,
                ),
                ("unnormal", x87(0x42, 0), true),
                ("unnormal 42", x87(0x8042, 42), true),
                ("pseudo-denormal", x87(0, 0x8000_0000_0000_0000), false),
                ("denormal", x87(0x8000, 42), false),
                ("indefinite", x87(0xffff, 0xc000_0000_0000_0000), false),
                ("normal", x87(0x42, 0x8000_0000_0000_0042), false),
            ] {
                cases.push((name.to_string(), bits, s));
            }
        }
        // A payload in the low word alone: the half the test folds into the
        // high one.
        FpFormat::Binary128 => {
            let exp = 0x7fffu128 << 112;
            cases.push(("sNaN, low word".to_string(), exp | 1, true));
            cases.push(("sNaN, bit 63".to_string(), exp | top(64), true));
            cases.push(("qNaN, low word".to_string(), exp | top(112) | 1, false));
        }
        _ => {}
    }
    cases
}

/// `__builtin_issignaling` reads every format's representation correctly, on
/// each target: signalling NaNs of either sign and every payload position
/// are 1; quiet NaNs, infinities, zeros, numbers are 0; and on x87 the
/// encodings the hardware no longer accepts are 1, as in gcc. No floating
/// comparison or conversion is made, since either would quiet or raise.
#[test]
fn test_issignaling_tests_the_representation() {
    let x86 = Target::new(Arch::X86_64, Os::Linux);
    let a64 = Target::new(Arch::Aarch64, Os::Linux);
    let apple = Target::new(Arch::Aarch64, Os::MacOS);
    let cases = [
        (&x86, "float", FpFormat::Binary32),
        (&x86, "double", FpFormat::Binary64),
        (&x86, "long double", FpFormat::X87Extended),
        (&x86, "_Float16", FpFormat::Binary16),
        (&x86, "_Float128", FpFormat::Binary128),
        (&x86, "_Float64x", FpFormat::X87Extended),
        (&a64, "float", FpFormat::Binary32),
        (&a64, "double", FpFormat::Binary64),
        (&a64, "long double", FpFormat::Binary128),
        (&a64, "_Float16", FpFormat::Binary16),
        (&a64, "_Float32x", FpFormat::Binary64),
        (&apple, "long double", FpFormat::Binary64),
    ];
    for (target, t, fmt) in cases {
        let src = format!("int f({t} x) {{ return __builtin_issignaling(x); }}");
        let module = linearize_source(&src, target);
        let insns = insns_of(&module, "f");
        assert!(
            !insns.iter().any(|i| i.op.is_float_comparison()
                || matches!(i.op, Opcode::FCvtF | Opcode::Call)),
            "{:?} {t}: a floating operation",
            target.arch
        );
        for (name, bits, want) in issignaling_cases(fmt) {
            let got = run_issignaling(&module, bits.to_le_bytes());
            assert_eq!(
                got,
                i128::from(want),
                "{:?} {t}: {name} {bits:#x}",
                target.arch
            );
        }
    }
}

/// A constant operand folds in the linearizer, at every level: no store, no
/// load, just the answer -- with the constant quieted wherever the program
/// would have quieted it.
#[test]
fn test_issignaling_of_a_constant_folds() {
    let target = Target::new(Arch::X86_64, Os::Linux);
    for (arg, want) in [
        ("__builtin_nans(\"\")", 1),
        ("-__builtin_nansl(\"\")", 1),
        ("__builtin_nansf128(\"\")", 1),
        ("__builtin_nan(\"\")", 0),
        ("(float)__builtin_nans(\"\")", 0),
        ("__builtin_nans(\"\") + 1.0", 0),
        ("2.5f", 0),
    ] {
        let src = format!("int f(void) {{ return __builtin_issignaling({arg}); }}");
        let module = linearize_source(&src, &target);
        let insns = insns_of(&module, "f");
        assert!(
            !insns
                .iter()
                .any(|i| matches!(i.op, Opcode::Store | Opcode::Load)),
            "{arg}: not folded"
        );
        assert_eq!(run_issignaling(&module, [0; 16]), want, "{arg}");
    }
}

/// `__builtin_stack_save` is a `StackSave`, and `__builtin_stack_restore`
/// a `StackRestore` of the value it gave, on both targets.
#[test]
fn test_stack_save_and_restore_are_their_opcodes() {
    let src = "void g(int n) { void *m = __builtin_stack_save();\n\
               __builtin_stack_restore(m); }\n";
    for arch in [Arch::X86_64, Arch::Aarch64] {
        let module = linearize_source(src, &Target::new(arch, Os::Linux));
        let insns = insns_of(&module, "g");
        let save = insns
            .iter()
            .find(|i| i.op == Opcode::StackSave)
            .expect("a StackSave");
        let restore = insns
            .iter()
            .find(|i| i.op == Opcode::StackRestore)
            .expect("a StackRestore");
        assert_eq!(save.size, 64, "{arch:?}");
        assert_eq!(restore.src.len(), 1, "{arch:?}");
        // `m` is promoted: the restore reads the save's own result, through
        // the copy that was the assignment.
        let mut mark = restore.src[0];
        while let Some(copy) = insns
            .iter()
            .find(|i| i.op == Opcode::Copy && i.target == Some(mark))
        {
            mark = copy.src[0];
        }
        assert_eq!(Some(mark), save.target, "{arch:?}");
    }
}

// __builtin_object_size

/// What became of `f`'s `__builtin_object_size`, compiled optimizing or not:
/// `Err(type)` deferred as an `ObjectSize` placeholder of that type, or
/// `Ok(constants)` answered while parsing, the answer among the constants
/// `f` uses.
fn object_size(
    src: &str,
    optimizing: bool,
) -> Result<Vec<i128>, crate::parse::ast::ObjectSizeType> {
    let policy = crate::parse::LibraryCallPolicy {
        optimizing,
        math_errno: true,
    };
    let (module, _) = linearize_source_under(src, &Target::new(Arch::X86_64, Os::Linux), policy);
    let f = module.functions.iter().find(|f| f.name == "f").unwrap();
    let insns = insns_of(&module, "f");
    if let Some(insn) = insns.iter().find(|i| matches!(i.op, Opcode::ObjectSize(_))) {
        let Opcode::ObjectSize(otype) = insn.op else {
            unreachable!()
        };
        return Err(otype);
    }
    Ok(insns
        .iter()
        .flat_map(|i| &i.src)
        .filter_map(|&s| f.const_val(s))
        .collect())
}

/// A pointer whose object only propagation finds is deferred when
/// optimizing, with its type, and answered as an unknown object at `-O0`,
/// as gcc does: `(size_t)-1` for a maximum, 0 for a minimum.
#[test]
fn test_object_size_of_a_pointer_variable_is_deferred_when_optimizing() {
    use crate::parse::ast::ObjectSizeType;
    for (otype, unknown) in [(0, -1), (1, -1), (2, 0), (3, 0)] {
        let src = format!(
            "char buf[32];\n\
             unsigned long f(void) {{ char *p = buf + 4; return __builtin_object_size(p, {otype}); }}\n"
        );
        assert_eq!(
            object_size(&src, true),
            Err(ObjectSizeType::from_bits(otype))
        );
        let answered = object_size(&src, false).expect("answered at -O0");
        assert!(answered.contains(&unknown), "type {otype}: {answered:?}");
    }
}

/// What the expression itself names is answered while parsing, at every
/// level; only the rest waits.
#[test]
fn test_object_size_of_a_named_object_is_answered_at_once() {
    let src = "char buf[32];\n\
               unsigned long f(void) { return __builtin_object_size(buf + 4, 0); }\n";
    for optimizing in [false, true] {
        let answered = object_size(src, optimizing).expect("answered while parsing");
        assert!(answered.contains(&28), "{answered:?}");
    }
}

/// The builtin does not evaluate its argument: one with a side effect, or
/// a `volatile` read, is an unknown object at once, as in gcc.
#[test]
fn test_object_size_of_a_side_effect_is_unknown_at_once() {
    for body in [
        "char *p = buf; return __builtin_object_size(p++, 0);",
        "char *p; return __builtin_object_size(p = buf, 0);",
        "return __builtin_object_size(g(), 0);",
        "char *volatile p = buf; return __builtin_object_size(p, 0);",
    ] {
        let src = format!("char buf[32];\nchar *g(void);\nunsigned long f(void) {{ {body} }}\n");
        let answered = object_size(&src, true).expect("answered at once");
        assert!(answered.contains(&-1), "{body}: {answered:?}");
    }
}

// __builtin_constant_p

/// Whether `f`'s `__builtin_constant_p` was deferred to the optimizer as a
/// `ConstantP` placeholder, rather than answered while linearizing.
fn constant_p_deferred(src: &str) -> bool {
    let module = linearize_source(src, &Target::new(Arch::X86_64, Os::Linux));
    insns_of(&module, "f")
        .iter()
        .any(|i| i.op == Opcode::ConstantP)
}

/// The operand of a deferred `__builtin_constant_p` is computed only to be
/// asked about and is then deleted unrun, so a read that may trap, a
/// division and a floating operation are all deferred: gcc answers each 1
/// where propagation proves it constant (`"hi"[0]` in gcc.c-torture bcp-1).
#[test]
fn test_constant_p_defers_an_operand_that_may_trap() {
    for body in [
        "return __builtin_constant_p(\"hi\"[0]);",
        "return __builtin_constant_p(*\"hi\");",
        "int x = 6, y = 2; return __builtin_constant_p(x / y);",
        "int x = 6, y = 4; return __builtin_constant_p(x % y);",
        "int a[2] = { 1, 2 }; return __builtin_constant_p(a[1]);",
        "struct { int m; } s = { 1 }, *p = &s; return __builtin_constant_p(p->m);",
        "double d = 2.0; return __builtin_constant_p(d * 3.0);",
        "double d = 2.5; return __builtin_constant_p((int)d);",
    ] {
        let src = format!("int f(void) {{ {body} }}\n");
        assert!(constant_p_deferred(&src), "{body}");
    }
}

/// An operand with a side effect is never evaluated, so it is answered 0 at
/// once -- as is a `volatile` read, the observable kind of read.
#[test]
fn test_constant_p_answers_a_side_effect_at_once() {
    for body in [
        "int x = 1; return __builtin_constant_p(x++);",
        "int x = 1; return __builtin_constant_p(x = 2);",
        "return __builtin_constant_p(g());",
        "volatile int v = 1; return __builtin_constant_p(v);",
        "volatile int a[2] = { 1, 2 }; return __builtin_constant_p(a[1]);",
        "volatile int v = 1, *p = &v; return __builtin_constant_p(*p);",
    ] {
        let src = format!("int g(void);\nint f(void) {{ {body} }}\n");
        assert!(!constant_p_deferred(&src), "{body}");
    }
}

/// The trapping operand a deferred `__builtin_constant_p` admits is not
/// admitted where an expression is evaluated speculatively: a conditional
/// with a division in an arm still branches.
#[test]
fn test_a_trapping_arm_is_still_not_speculated() {
    let src = "int f(int c, int x, int y) { return c ? x / y : 0; }\n";
    let module = linearize_source(src, &Target::new(Arch::X86_64, Os::Linux));
    assert!(!insns_of(&module, "f")
        .iter()
        .any(|i| i.op == Opcode::Select));
}
