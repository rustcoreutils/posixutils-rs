//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Linearizer tests for expressions: unary operators, pointer arithmetic,
// floating point, conversions, `_Float16`, `_Alignof`, bit-fields and casts.
//

use super::test_linearize::{
    has_op, linearize_no_ssa, linearize_source, linearize_source_with_types, test_pos, TestContext,
};
use super::*;
use crate::parse::ast::{
    BinaryOp, BlockItem, ExprKind, ExternalDecl, FunctionDef, ParamStyle, Parameter, Stmt, UnaryOp,
};
use crate::strings::StringTable;
use crate::target::{Arch, Os, Target};
use crate::types::{CompositeType, MemberAlign, StructMember, Type, TypeTable};

// Unary operation tests

#[test]
fn test_unary_logical_not() {
    // Test: return !x;
    // Verifies logical not produces comparison to zero
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let x_sym = ctx.var("x", int_type);

    let not_expr = Expr::typed_unpositioned(
        ExprKind::Unary {
            op: UnaryOp::Not,
            operand: Box::new(Expr::var_typed(x_sym, int_type)),
        },
        int_type,
    );

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: int_type,
            vm_dims: vec![],
            discarded_dims: vec![],
        }],
        body: Stmt::Return(Some(not_expr)),
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
    let ir = format!("{}", module.display(&ctx.types));

    // Logical not should produce seteq (comparison to zero)
    assert!(
        has_op(&module, &[Opcode::SetEq]),
        "Logical NOT should produce seteq instruction: {}",
        ir
    );
}

#[test]
fn test_unary_bitwise_not() {
    // Test: return ~x;
    // Verifies bitwise not produces not instruction
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let x_sym = ctx.var("x", int_type);

    let not_expr = Expr::typed_unpositioned(
        ExprKind::Unary {
            op: UnaryOp::BitNot,
            operand: Box::new(Expr::var_typed(x_sym, int_type)),
        },
        int_type,
    );

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: int_type,
            vm_dims: vec![],
            discarded_dims: vec![],
        }],
        body: Stmt::Return(Some(not_expr)),
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
    let ir = format!("{}", module.display(&ctx.types));

    // Bitwise not should produce not instruction
    assert!(
        has_op(&module, &[Opcode::Not]),
        "Bitwise NOT should produce not instruction: {}",
        ir
    );
}

#[test]
fn test_unary_negate() {
    // Test: return -x;
    // Verifies unary negation produces neg instruction
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let x_sym = ctx.var("x", int_type);

    let neg_expr = Expr::typed_unpositioned(
        ExprKind::Unary {
            op: UnaryOp::Neg,
            operand: Box::new(Expr::var_typed(x_sym, int_type)),
        },
        int_type,
    );

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: int_type,
            vm_dims: vec![],
            discarded_dims: vec![],
        }],
        body: Stmt::Return(Some(neg_expr)),
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
    let ir = format!("{}", module.display(&ctx.types));

    // Negation should produce neg instruction
    assert!(
        has_op(&module, &[Opcode::Neg]),
        "Unary negation should produce neg instruction: {}",
        ir
    );
}

#[test]
fn test_pre_increment() {
    // Test: return ++x;
    // Verifies pre-increment adds 1 and returns new value
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let x_sym = ctx.var("x", int_type);

    let inc_expr = Expr::typed_unpositioned(
        ExprKind::Unary {
            op: UnaryOp::PreInc,
            operand: Box::new(Expr::var_typed(x_sym, int_type)),
        },
        int_type,
    );

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: int_type,
            vm_dims: vec![],
            discarded_dims: vec![],
        }],
        body: Stmt::Return(Some(inc_expr)),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
        param_style: ParamStyle::Prototype,
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    // Observed before SSA conversion: the store of the incremented value is the linearizer's job; SSA then promotes it away.
    let module = linearize_no_ssa(&tu, &ctx.types, &ctx.strings, &ctx.symbols);
    let ir = format!("{}", module.display(&ctx.types));

    // Pre-increment should produce add instruction
    assert!(
        has_op(&module, &[Opcode::Add]),
        "Pre-increment should produce add instruction: {}",
        ir
    );

    // Should store the incremented value
    assert!(
        has_op(&module, &[Opcode::Store]),
        "Pre-increment should store new value: {}",
        ir
    );
}

// Pointer arithmetic tests

#[test]
fn test_pointer_add_int() {
    // Test: return p + 5;
    // Verifies pointer arithmetic scales by element size
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let int_ptr_type = ctx.ptr(int_type);
    let p_sym = ctx.var("p", int_ptr_type);

    // p + 5
    let add_expr = Expr::binary(
        BinaryOp::Add,
        Expr::var_typed(p_sym, int_ptr_type),
        Expr::int(5, &ctx.types),
        &ctx.types,
    );

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: int_ptr_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(p_sym),
            typ: int_ptr_type,
            vm_dims: vec![],
            discarded_dims: vec![],
        }],
        body: Stmt::Return(Some(add_expr)),
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
    let ir = format!("{}", module.display(&ctx.types));

    // Pointer addition should have multiplication for scaling
    assert!(
        has_op(&module, &[Opcode::Mul]),
        "Pointer add should scale by element size (mul): {}",
        ir
    );

    // Should have add instruction
    assert!(
        has_op(&module, &[Opcode::Add]),
        "Pointer add should have add instruction: {}",
        ir
    );
}

#[test]
fn test_pointer_difference() {
    // Test: return p - q;
    // Verifies pointer difference divides by element size
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let long_type = ctx.types.long_id;
    let int_ptr_type = ctx.ptr(int_type);
    let p_sym = ctx.var("p", int_ptr_type);
    let q_sym = ctx.var("q", int_ptr_type);

    // p - q (result is ptrdiff_t, which is long)
    let diff_expr = Expr::typed_unpositioned(
        ExprKind::Binary {
            op: BinaryOp::Sub,
            left: Box::new(Expr::var_typed(p_sym, int_ptr_type)),
            right: Box::new(Expr::var_typed(q_sym, int_ptr_type)),
        },
        long_type,
    );

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: long_type,
        name: test_id,
        params: vec![
            Parameter {
                symbol: Some(p_sym),
                typ: int_ptr_type,
                vm_dims: vec![],
                discarded_dims: vec![],
            },
            Parameter {
                symbol: Some(q_sym),
                typ: int_ptr_type,
                vm_dims: vec![],
                discarded_dims: vec![],
            },
        ],
        body: Stmt::Return(Some(diff_expr)),
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
    let ir = format!("{}", module.display(&ctx.types));

    // Pointer difference should have subtraction
    assert!(
        has_op(&module, &[Opcode::Sub]),
        "Pointer difference should have sub instruction: {}",
        ir
    );

    // Should have division for scaling (divs for signed division)
    assert!(
        has_op(&module, &[Opcode::DivS, Opcode::DivU]),
        "Pointer difference should divide by element size: {}",
        ir
    );
}

/// Arithmetic on a function pointer steps by gcc's `sizeof` of a function
/// type, 1: the difference divides by 1 and the addend is scaled by 1. A
/// function designator is a pointer operand too, once it decays. The
/// difference used to divide by the function type's size of 0.
#[test]
fn test_function_pointer_arithmetic_steps_by_one() {
    let src = "int f(int);\n\
        long d(int (*a)(void), int (*b)(void)) { return a - b; }\n\
        long e(void) { return f - f; }\n\
        int (*s(int (*p)(void), long n))(void) { return p + n; }\n\
        int (*t(long n))(int) { return n + f; }\n";
    let module = linearize_source(src, &Target::host());
    for (name, op) in [
        ("d", Opcode::DivS),
        ("e", Opcode::DivS),
        ("s", Opcode::Mul),
        ("t", Opcode::Mul),
    ] {
        let func = module.functions.iter().find(|f| f.name == name).unwrap();
        let insn = func
            .blocks
            .iter()
            .flat_map(|bb| bb.insns.iter())
            .find(|i| i.op == op)
            .unwrap_or_else(|| panic!("{name}: no {op:?}"));
        assert!(
            matches!(
                func.get_pseudo(insn.src[1]).map(|p| &p.kind),
                Some(crate::ir::PseudoKind::Val(1))
            ),
            "{name}: the step is not 1"
        );
    }
}

// Floating-point operation tests

#[test]
fn test_float_add() {
    // Test: double a, b; return a + b;
    // Verifies float addition produces fadd instruction
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let double_type = ctx.types.double_id;
    let a_sym = ctx.var("a", double_type);
    let b_sym = ctx.var("b", double_type);

    let add_expr = Expr::binary(
        BinaryOp::Add,
        Expr::var_typed(a_sym, double_type),
        Expr::var_typed(b_sym, double_type),
        &ctx.types,
    );

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: double_type,
        name: test_id,
        params: vec![
            Parameter {
                symbol: Some(a_sym),
                typ: double_type,
                vm_dims: vec![],
                discarded_dims: vec![],
            },
            Parameter {
                symbol: Some(b_sym),
                typ: double_type,
                vm_dims: vec![],
                discarded_dims: vec![],
            },
        ],
        body: Stmt::Return(Some(add_expr)),
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
    let ir = format!("{}", module.display(&ctx.types));

    // Float addition should produce fadd instruction
    assert!(
        has_op(&module, &[Opcode::FAdd]),
        "Float addition should produce fadd instruction: {}",
        ir
    );
}

#[test]
fn test_float_comparison() {
    // Test: double a, b; return a < b;
    // Verifies float comparison produces fcmp instruction
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let double_type = ctx.types.double_id;
    let a_sym = ctx.var("a", double_type);
    let b_sym = ctx.var("b", double_type);

    let cmp_expr = Expr::binary(
        BinaryOp::Lt,
        Expr::var_typed(a_sym, double_type),
        Expr::var_typed(b_sym, double_type),
        &ctx.types,
    );

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: int_type,
        name: test_id,
        params: vec![
            Parameter {
                symbol: Some(a_sym),
                typ: double_type,
                vm_dims: vec![],
                discarded_dims: vec![],
            },
            Parameter {
                symbol: Some(b_sym),
                typ: double_type,
                vm_dims: vec![],
                discarded_dims: vec![],
            },
        ],
        body: Stmt::Return(Some(cmp_expr)),
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
    let ir = format!("{}", module.display(&ctx.types));

    // Float comparison should produce fcmp instruction
    assert!(
        has_op(
            &module,
            &[
                Opcode::FCmpOEq,
                Opcode::FCmpONe,
                Opcode::FCmpOLt,
                Opcode::FCmpOLe,
                Opcode::FCmpOGt,
                Opcode::FCmpOGe
            ]
        ),
        "Float comparison should produce fcmp instruction: {}",
        ir
    );
}

#[test]
fn test_float_to_int_cast() {
    // Test: double x; return (int)x;
    // Verifies float-to-int cast produces fcvts instruction
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let double_type = ctx.types.double_id;
    let x_sym = ctx.var("x", double_type);

    let cast_expr = Expr::typed_unpositioned(
        ExprKind::Cast {
            cast_type: int_type,
            expr: Box::new(Expr::var_typed(x_sym, double_type)),
        },
        int_type,
    );

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: double_type,
            vm_dims: vec![],
            discarded_dims: vec![],
        }],
        body: Stmt::Return(Some(cast_expr)),
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
    let ir = format!("{}", module.display(&ctx.types));

    // Float-to-int cast should produce fcvts instruction
    assert!(
        has_op(&module, &[Opcode::FCvtS]),
        "Float-to-int cast should produce fcvts instruction: {}",
        ir
    );
}

#[test]
fn test_int_to_float_cast() {
    // Test: int x; return (double)x;
    // Verifies int-to-float cast produces scvtf instruction
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let double_type = ctx.types.double_id;
    let x_sym = ctx.var("x", int_type);

    let cast_expr = Expr::typed_unpositioned(
        ExprKind::Cast {
            cast_type: double_type,
            expr: Box::new(Expr::var_typed(x_sym, int_type)),
        },
        double_type,
    );

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: double_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: int_type,
            vm_dims: vec![],
            discarded_dims: vec![],
        }],
        body: Stmt::Return(Some(cast_expr)),
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
    let ir = format!("{}", module.display(&ctx.types));

    // Int-to-float cast should produce scvtf instruction
    assert!(
        has_op(&module, &[Opcode::SCvtF]),
        "Int-to-float cast should produce scvtf instruction: {}",
        ir
    );
}

// Struct/union dereference tests

#[test]
fn test_struct_deref_returns_address() {
    // Test: struct S *p; return *p;
    // Dereferencing a pointer to struct should return the address (for struct copy)
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let s_tag = ctx.str("S");

    // Define struct S { int x; }
    let struct_type = Type::struct_type(CompositeType {
        tag: Some(s_tag),
        members: vec![StructMember {
            name: ctx.str("x"),
            typ: ctx.types.int_id,
            offset: 0,
            bit_width: None,
            bit_offset: None,
            access_bytes: None,
            align: MemberAlign::NATURAL,
        }],
        enum_constants: vec![],
        size: 4,
        align: 4,
        member_align: 4,
        is_complete: true,
        transparent: false,
        anon_id: None,
        tag_type: None,
    });
    let struct_type_id = ctx.types.intern(struct_type);
    let struct_ptr_type_id = ctx.types.intern(Type::pointer(struct_type_id));
    let p_sym = ctx.var("p", struct_ptr_type_id);

    // Function: struct S test(struct S *p) { return *p; }
    let deref_p = Expr::typed_unpositioned(
        ExprKind::Unary {
            op: UnaryOp::Deref,
            operand: Box::new(Expr::var_typed(p_sym, struct_ptr_type_id)),
        },
        struct_type_id,
    );

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: struct_type_id,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(p_sym),
            typ: struct_ptr_type_id,
            vm_dims: vec![],
            discarded_dims: vec![],
        }],
        body: Stmt::Return(Some(deref_p)),
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
    let ir = format!("{}", module.display(&ctx.types));

    // The struct dereference should NOT generate a load instruction
    // Instead, it returns the address for struct copying
    // The return statement will handle the struct return ABI
    // We should see memcpy or struct_store, not a simple load followed by ret
    assert!(
        !ir.contains("load i32"),
        "Struct dereference should not generate scalar load. IR:\n{}",
        ir
    );
}

// Tests for src_typ field on conversion instructions

#[test]
fn test_int_to_float_cast_has_src_typ() {
    // Test: int x; return (double)x;
    // Verifies src_typ is set on conversion instruction
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let double_type = ctx.types.double_id;
    let x_sym = ctx.var("x", int_type);

    let cast_expr = Expr::typed_unpositioned(
        ExprKind::Cast {
            cast_type: double_type,
            expr: Box::new(Expr::var_typed(x_sym, int_type)),
        },
        double_type,
    );

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: double_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: int_type,
            vm_dims: vec![],
            discarded_dims: vec![],
        }],
        body: Stmt::Return(Some(cast_expr)),
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

    // Find the conversion instruction and verify src_typ is set
    let func = &module.functions[0];
    let has_src_typ = func.blocks.iter().any(|bb| {
        bb.insns
            .iter()
            .any(|insn| matches!(insn.op, Opcode::SCvtF | Opcode::UCvtF) && insn.src_typ.is_some())
    });
    assert!(
        has_src_typ,
        "Int-to-float conversion should have src_typ set"
    );
}

#[test]
fn test_float_to_int_cast_has_src_typ() {
    // Test: double x; return (int)x;
    // Verifies src_typ is set on conversion instruction
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let double_type = ctx.types.double_id;
    let x_sym = ctx.var("x", double_type);

    let cast_expr = Expr::typed_unpositioned(
        ExprKind::Cast {
            cast_type: int_type,
            expr: Box::new(Expr::var_typed(x_sym, double_type)),
        },
        int_type,
    );

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: double_type,
            vm_dims: vec![],
            discarded_dims: vec![],
        }],
        body: Stmt::Return(Some(cast_expr)),
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

    // Find the conversion instruction and verify src_typ is set
    let func = &module.functions[0];
    let has_src_typ = func.blocks.iter().any(|bb| {
        bb.insns
            .iter()
            .any(|insn| matches!(insn.op, Opcode::FCvtS | Opcode::FCvtU) && insn.src_typ.is_some())
    });
    assert!(
        has_src_typ,
        "Float-to-int conversion should have src_typ set"
    );
}

#[test]
fn test_integer_extension_has_src_typ() {
    // Test: char x; return (int)x;
    // Verifies src_typ is set on sign-extend instruction
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let char_type = ctx.types.char_id;
    let x_sym = ctx.var("x", char_type);

    let cast_expr = Expr::typed_unpositioned(
        ExprKind::Cast {
            cast_type: int_type,
            expr: Box::new(Expr::var_typed(x_sym, char_type)),
        },
        int_type,
    );

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: char_type,
            vm_dims: vec![],
            discarded_dims: vec![],
        }],
        body: Stmt::Return(Some(cast_expr)),
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

    // Find the extension instruction and verify src_typ is set
    let func = &module.functions[0];
    let has_src_typ = func.blocks.iter().any(|bb| {
        bb.insns
            .iter()
            .any(|insn| matches!(insn.op, Opcode::Sext | Opcode::Zext) && insn.src_typ.is_some())
    });
    assert!(has_src_typ, "Integer extension should have src_typ set");
}

// Float16 (_Float16) conversion tests

#[test]
fn test_float16_to_float_conversion() {
    // Test: _Float16 x; return (float)x;
    // Should call __extendhfsf2 runtime library function
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let float_type = ctx.types.float_id;
    let float16_type = ctx.types.float16_id;
    let x_sym = ctx.var("x", float16_type);

    let cast_expr = Expr::typed_unpositioned(
        ExprKind::Cast {
            cast_type: float_type,
            expr: Box::new(Expr::var_typed(x_sym, float16_type)),
        },
        float_type,
    );

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: float_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: float16_type,
            vm_dims: vec![],
            discarded_dims: vec![],
        }],
        body: Stmt::Return(Some(cast_expr)),
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

    // Float16→float now emits FCvtF with src_typ=Float16 (mapping pass lowers to rtlib)
    let func = &module.functions[0];
    let has_fcvtf = func.blocks.iter().any(|bb| {
        bb.insns.iter().any(|insn| {
            insn.op == Opcode::FCvtF
                && insn
                    .src_typ
                    .is_some_and(|t| ctx.types.kind(t) == TypeKind::Float16)
        })
    });
    assert!(
        has_fcvtf,
        "Float16 to float conversion should emit FCvtF with Float16 src_typ"
    );
}

#[test]
fn test_float_to_float16_conversion() {
    // Test: float x; return (_Float16)x;
    // Should call __truncsfhf2 runtime library function
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let float_type = ctx.types.float_id;
    let float16_type = ctx.types.float16_id;
    let x_sym = ctx.var("x", float_type);

    let cast_expr = Expr::typed_unpositioned(
        ExprKind::Cast {
            cast_type: float16_type,
            expr: Box::new(Expr::var_typed(x_sym, float_type)),
        },
        float16_type,
    );

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: float16_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: float_type,
            vm_dims: vec![],
            discarded_dims: vec![],
        }],
        body: Stmt::Return(Some(cast_expr)),
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

    // float→Float16 now emits FCvtF with Float16 dst type (mapping pass lowers to rtlib)
    let func = &module.functions[0];
    let has_fcvtf = func.blocks.iter().any(|bb| {
        bb.insns.iter().any(|insn| {
            insn.op == Opcode::FCvtF
                && insn
                    .typ
                    .is_some_and(|t| ctx.types.kind(t) == TypeKind::Float16)
        })
    });
    assert!(
        has_fcvtf,
        "Float to Float16 conversion should emit FCvtF with Float16 dst type"
    );
}

#[test]
fn test_float16_to_int_conversion() {
    // Test: _Float16 x; return (int)x;
    // Should call __fixhfsi runtime library function
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let float16_type = ctx.types.float16_id;
    let x_sym = ctx.var("x", float16_type);

    let cast_expr = Expr::typed_unpositioned(
        ExprKind::Cast {
            cast_type: int_type,
            expr: Box::new(Expr::var_typed(x_sym, float16_type)),
        },
        int_type,
    );

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: float16_type,
            vm_dims: vec![],
            discarded_dims: vec![],
        }],
        body: Stmt::Return(Some(cast_expr)),
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

    // Float16→int now emits FCvtS with Float16 src_typ (mapping pass lowers to rtlib)
    let func = &module.functions[0];
    let has_fcvts = func.blocks.iter().any(|bb| {
        bb.insns.iter().any(|insn| {
            insn.op == Opcode::FCvtS
                && insn
                    .src_typ
                    .is_some_and(|t| ctx.types.kind(t) == TypeKind::Float16)
        })
    });
    assert!(
        has_fcvts,
        "Float16 to int conversion should emit FCvtS with Float16 src_typ"
    );
}

#[test]
fn test_int_to_float16_conversion() {
    // Test: int x; return (_Float16)x;
    // Should call __floatsihf runtime library function
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let float16_type = ctx.types.float16_id;
    let x_sym = ctx.var("x", int_type);

    let cast_expr = Expr::typed_unpositioned(
        ExprKind::Cast {
            cast_type: float16_type,
            expr: Box::new(Expr::var_typed(x_sym, int_type)),
        },
        float16_type,
    );

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: float16_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: int_type,
            vm_dims: vec![],
            discarded_dims: vec![],
        }],
        body: Stmt::Return(Some(cast_expr)),
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

    // int→Float16 now emits SCvtF with Float16 dst type (mapping pass lowers to rtlib)
    let func = &module.functions[0];
    let has_scvtf = func.blocks.iter().any(|bb| {
        bb.insns.iter().any(|insn| {
            insn.op == Opcode::SCvtF
                && insn
                    .typ
                    .is_some_and(|t| ctx.types.kind(t) == TypeKind::Float16)
        })
    });
    assert!(
        has_scvtf,
        "Int to Float16 conversion should emit SCvtF with Float16 dst type"
    );
}

// C11 _Alignof tests

#[test]
fn test_alignof_type_emits_setval() {
    // Test: return _Alignof(int);
    // Should emit a SetVal instruction (constant folded at linearize time)
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();

    let alignof_expr =
        Expr::typed_unpositioned(ExprKind::AlignofType(int_type), ctx.types.ulong_id);

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: ctx.types.ulong_id,
        name: test_id,
        params: vec![],
        body: Stmt::Return(Some(alignof_expr)),
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

    // _Alignof should emit SetVal for the constant
    let func = &module.functions[0];
    let has_setval = func
        .blocks
        .iter()
        .any(|bb| bb.insns.iter().any(|insn| insn.op == Opcode::SetVal));
    assert!(has_setval, "_Alignof(int) should emit SetVal for constant");
}

#[test]
fn test_alignof_expr_emits_setval() {
    // Test: int x; return _Alignof(x);
    // Should emit a SetVal instruction (constant folded at linearize time)
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let x_sym = ctx.var("x", int_type);

    let alignof_expr = Expr::typed_unpositioned(
        ExprKind::AlignofExpr(Box::new(Expr::var_typed(x_sym, int_type))),
        ctx.types.ulong_id,
    );

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: ctx.types.ulong_id,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: int_type,
            vm_dims: vec![],
            discarded_dims: vec![],
        }],
        body: Stmt::Return(Some(alignof_expr)),
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

    // _Alignof(x) should emit SetVal for the constant
    let func = &module.functions[0];
    let has_setval = func
        .blocks
        .iter()
        .any(|bb| bb.insns.iter().any(|insn| insn.op == Opcode::SetVal));
    assert!(
        has_setval,
        "_Alignof(x) where x is int should emit SetVal for constant"
    );
}

// Phase 1: Foundation Helper Tests

#[test]
fn test_bitfield_storage_type() {
    let strings = StringTable::new();
    let types = TypeTable::new(&Target::host());
    let symbols = SymbolTable::new();
    let target = Target::host();
    let linearizer = Linearizer::new(&symbols, &types, &strings, &target);

    assert_eq!(linearizer.bitfield_storage_type(1), types.uchar_id);
    assert_eq!(linearizer.bitfield_storage_type(2), types.ushort_id);
    assert_eq!(linearizer.bitfield_storage_type(4), types.uint_id);
    assert_eq!(linearizer.bitfield_storage_type(8), types.ulong_id);
    // A 16-byte unit carries an `__int128` bit-field wider than 64 bits. Its
    // *kind* is what matters downstream: `identify_int128_pseudos` routes on
    // `TypeKind::Int128`, and that is what puts the value in a 16-byte stack
    // slot instead of a GP register the backend cannot address as a pair.
    assert_eq!(linearizer.bitfield_storage_type(16), types.uint128_id);
    assert_eq!(types.kind(types.uint128_id), crate::types::TypeKind::Int128);
    // Fallback for unexpected sizes
    assert_eq!(linearizer.bitfield_storage_type(3), types.uint_id);
}

/// The 128-bit mask is exact at both ends, including the full width where the
/// `(1 << n) - 1` spelling collapses to zero.
#[test]
fn test_bitfield_value_mask_128_covers_the_wide_carrier() {
    use crate::ir::linearize_emit::bitfield_value_mask_128;

    assert_eq!(bitfield_value_mask_128(0), 0);
    assert_eq!(bitfield_value_mask_128(1), 0x1);
    assert_eq!(bitfield_value_mask_128(64), u64::MAX as u128);
    assert_eq!(
        bitfield_value_mask_128(65),
        (u64::MAX as u128) | (1u128 << 64)
    );
    assert_eq!(bitfield_value_mask_128(127), u128::MAX >> 1);
    assert_eq!(bitfield_value_mask_128(128), u128::MAX);

    // Agrees with the 64-bit twin everywhere the twin is defined, so the two
    // cannot drift apart.
    for w in 1..=64u32 {
        assert_eq!(
            bitfield_value_mask_128(w),
            crate::ir::linearize_emit::bitfield_value_mask(w) as u128,
            "masks disagree at width {w}"
        );
    }
    // Every width sets exactly that many bits.
    for w in 0..=128u32 {
        assert_eq!(bitfield_value_mask_128(w).count_ones(), w);
    }
}

/// `emit_bitfield_load` extends only for a signed field, and decides that by
/// asking `is_unsigned`. A `_Bool` field must not be extended: the extension
/// is `shl 31; sar 31` at one bit wide, which turns the stored 1 into -1.
///
/// Asserted at the type table rather than on emitted IR because that is where
/// the decision is made -- and because the same predicate answers for every
/// other consumer that picks an extension, an opcode or an ABI class.
#[test]
fn test_bool_is_an_unsigned_type_for_bitfield_extension() {
    let types = TypeTable::new(&Target::host());

    // The signedness that governs code generation.
    assert!(types.is_unsigned(types.bool_id), "_Bool is unsigned");
    assert!(types.is_unsigned(types.uchar_id));
    assert!(!types.is_unsigned(types.schar_id));
    assert!(!types.is_unsigned(types.int_id));

    // The storage unit a one-byte `_Bool` field is read through is unsigned
    // either way, so the extension decision rests entirely on the member type.
    assert_eq!(
        types.size_bits(types.bool_id),
        8,
        "a _Bool bitfield is backed by a one-byte unit"
    );
}

/// `(1 << width) - 1` overflows at the one width that matters most: Rust masks
/// a shift amount to the operand's width, so `1u64 << 64` is `1` and the mask
/// comes out `0`, so `struct { unsigned long long a:64; }` reads back as zero.
/// The boundary cases are asserted individually rather than through a loop
/// that could share the same mistake.
#[test]
fn test_bitfield_value_mask_covers_the_full_carrier() {
    use crate::ir::linearize_emit::bitfield_value_mask;

    assert_eq!(bitfield_value_mask(1), 0x1);
    assert_eq!(bitfield_value_mask(3), 0x7);
    assert_eq!(bitfield_value_mask(8), 0xff);
    assert_eq!(bitfield_value_mask(31), 0x7fff_ffff);
    assert_eq!(bitfield_value_mask(32), 0xffff_ffff);
    assert_eq!(bitfield_value_mask(33), 0x1_ffff_ffff);
    assert_eq!(bitfield_value_mask(63), 0x7fff_ffff_ffff_ffff);
    assert_eq!(bitfield_value_mask(64), u64::MAX);

    // Every width is a contiguous run of low bits of exactly that length.
    for w in 1..=64u32 {
        let m = bitfield_value_mask(w);
        assert_eq!(m.count_ones(), w, "width {w} masks {} bits", m.count_ones());
        assert_eq!(m.trailing_ones(), w, "width {w} is not low-contiguous");
    }
}

/// `Expr::vm_index_base` finds the object whose recorded extents give the
/// stride for a variably-modified type, and counts the index steps that
/// separate them.
///
/// A dereference is an index step: 6.5.2.1p2 defines `E1[E2]` as
/// `(*((E1)+(E2)))`, so `*p` and `p[0]` are the same expression at the same
/// depth. Counting only `Index` carried the extents in and dropped them out,
/// which is why `p[0][i][j]` indexed correctly while `(*p)[i][j]` used a
/// stride of zero and `sizeof(*p)` answered 0.
#[test]
fn test_vm_index_base_counts_a_deref_as_an_index_step() {
    let mut ctx = TestContext::new();
    let int_t = ctx.int_type();
    let sym = ctx.var("a", int_t);

    let ident = || Expr {
        kind: ExprKind::Ident(sym),
        typ: Some(int_t),
        pos: test_pos(),

        bitfield_bits: None,
    };
    let zero = || {
        Box::new(Expr {
            kind: ExprKind::IntLit(0),
            typ: Some(int_t),
            pos: test_pos(),
            bitfield_bits: None,
        })
    };
    let index = |base: Expr| Expr {
        kind: ExprKind::Index {
            array: Box::new(base),
            index: zero(),
        },
        typ: Some(int_t),
        pos: test_pos(),
        bitfield_bits: None,
    };
    let deref = |base: Expr| Expr {
        kind: ExprKind::Unary {
            op: UnaryOp::Deref,
            operand: Box::new(base),
        },
        typ: Some(int_t),
        pos: test_pos(),
        bitfield_bits: None,
    };

    // The object itself is depth 0.
    assert_eq!(ident().vm_index_base(), Some((sym, 0)));

    // `a[0]` and `*a` are the same depth, and so are `a[0][0]`, `(*a)[0]`,
    // `*(a[0])` and `**a`.
    assert_eq!(index(ident()).vm_index_base(), Some((sym, 1)));
    assert_eq!(deref(ident()).vm_index_base(), Some((sym, 1)));
    for at_two in [
        index(index(ident())),
        index(deref(ident())),
        deref(index(ident())),
        deref(deref(ident())),
    ] {
        assert_eq!(at_two.vm_index_base(), Some((sym, 2)));
    }

    // Anything else under the chain has no recorded extents, so the caller
    // must fall back to the compile-time size rather than guess.
    let not_an_object = Expr {
        kind: ExprKind::Unary {
            op: UnaryOp::Neg,
            operand: Box::new(ident()),
        },
        typ: Some(int_t),
        pos: test_pos(),
        bitfield_bits: None,
    };
    assert_eq!(not_an_object.vm_index_base(), None);
    assert_eq!(index(not_an_object).vm_index_base(), None);

    // `&` steps back out: `&a` is one step above `a`, and `&*a` is `a`.
    let addr_of = |base: Expr| Expr {
        kind: ExprKind::Unary {
            op: UnaryOp::AddrOf,
            operand: Box::new(base),
        },
        typ: Some(int_t),
        pos: test_pos(),
        bitfield_bits: None,
    };
    assert_eq!(addr_of(ident()).vm_index_base(), Some((sym, -1)));
    assert_eq!(addr_of(deref(ident())).vm_index_base(), Some((sym, 0)));
    assert_eq!(
        index(addr_of(index(ident()))).vm_index_base(),
        Some((sym, 1))
    );

    // Adding to a pointer does not change what it points at, so `p + 2` sits
    // at the same depth as `p` -- from either side, and for `-` as well.
    // Without this, `(p + 2) - p` found no extents and divided by a
    // compile-time size of zero, which traps.
    let two = || {
        Box::new(Expr {
            kind: ExprKind::IntLit(2),
            typ: Some(int_t),
            pos: test_pos(),
            bitfield_bits: None,
        })
    };
    let arith = |op, left: Expr, swap: bool| Expr {
        kind: ExprKind::Binary {
            op,
            left: if swap { two() } else { Box::new(left.clone()) },
            right: if swap { Box::new(left) } else { two() },
        },
        typ: Some(int_t),
        pos: test_pos(),
        bitfield_bits: None,
    };
    assert_eq!(
        arith(BinaryOp::Add, ident(), false).vm_index_base(),
        Some((sym, 0))
    );
    assert_eq!(
        arith(BinaryOp::Add, ident(), true).vm_index_base(),
        Some((sym, 0))
    );
    assert_eq!(
        arith(BinaryOp::Sub, ident(), false).vm_index_base(),
        Some((sym, 0))
    );
    // And the steps compose: `(*p + 2)[0]` is two steps from `p`.
    assert_eq!(
        index(arith(BinaryOp::Add, deref(ident()), false)).vm_index_base(),
        Some((sym, 2))
    );
    // An arithmetic operator that is not `+`/`-` reaches no object.
    assert_eq!(arith(BinaryOp::Mul, ident(), false).vm_index_base(), None);
}

/// A branch condition has to be compared against zero the way its type says,
/// not fed to `cbr` as a bit pattern. For a `double` that means `FCmpONe`
/// against `0.0`, so `-0.0` is false; feeding the raw value tested the bit
/// pattern, and `-0.0`'s is not zero.
#[test]
fn test_float_condition_compares_against_zero() {
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let dbl_type = ctx.types.double_id;
    let d_sym = ctx.var("d", dbl_type);

    // int test(double d) { if (d) return 1; return 0; }
    let body = Stmt::Block(vec![
        BlockItem::Statement(Box::new(Stmt::If {
            cond: Expr::var_typed(d_sym, dbl_type),
            then_stmt: Box::new(Stmt::Return(Some(Expr::int(1, &ctx.types)))),
            else_stmt: None,
        })),
        BlockItem::Statement(Box::new(Stmt::Return(Some(Expr::int(0, &ctx.types))))),
    ]);
    let func = FunctionDef {
        attrs: Default::default(),
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(d_sym),
            typ: dbl_type,
            vm_dims: vec![],
            discarded_dims: vec![],
        }],
        body,
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
        param_style: ParamStyle::Prototype,
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };
    let module = linearize_no_ssa(&tu, &ctx.types, &ctx.strings, &ctx.symbols);
    let f = &module.functions[0];

    let has_fcmp = f
        .blocks
        .iter()
        .flat_map(|b| b.insns.iter())
        .any(|i| i.op == Opcode::FCmpONe);
    assert!(
        has_fcmp,
        "a floating-point condition must be compared against 0.0:\n{}",
        module.display(&ctx.types)
    );

    // The value handed to `cbr` must be that comparison, not the double.
    let cbr = f
        .blocks
        .iter()
        .flat_map(|b| b.insns.iter())
        .find(|i| i.op == Opcode::Cbr)
        .expect("no conditional branch");
    let cond_def = f
        .blocks
        .iter()
        .flat_map(|b| b.insns.iter())
        .find(|i| i.target == Some(cbr.src[0]))
        .expect("condition has no definition");
    assert_eq!(
        cond_def.op,
        Opcode::FCmpONe,
        "the branch must test the comparison, not the raw value:\n{}",
        module.display(&ctx.types)
    );
}

/// Equality on complex operands compares both halves. The complex arm of
/// `linearize_binary` keyed off the *result* type, which for a comparison is
/// `int`, so `a == b` fell into the scalar path and answered from whatever
/// the low half held.
#[test]
fn test_complex_equality_compares_both_halves() {
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let complex_double = ctx.types.complex_double_id;
    let a_sym = ctx.var("a", complex_double);
    let b_sym = ctx.var("b", complex_double);

    // int test(double _Complex a, double _Complex b) { return a == b; }
    let cmp = Expr::typed_unpositioned(
        ExprKind::Binary {
            op: BinaryOp::Eq,
            left: Box::new(Expr::var_typed(a_sym, complex_double)),
            right: Box::new(Expr::var_typed(b_sym, complex_double)),
        },
        int_type,
    );
    let func = FunctionDef {
        attrs: Default::default(),
        return_type: int_type,
        name: test_id,
        params: vec![
            Parameter {
                symbol: Some(a_sym),
                typ: complex_double,
                vm_dims: vec![],
                discarded_dims: vec![],
            },
            Parameter {
                symbol: Some(b_sym),
                typ: complex_double,
                vm_dims: vec![],
                discarded_dims: vec![],
            },
        ],
        body: Stmt::Return(Some(cmp)),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
        param_style: ParamStyle::Prototype,
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };
    let module = linearize_no_ssa(&tu, &ctx.types, &ctx.strings, &ctx.symbols);
    let f = &module.functions[0];
    let ops: Vec<Opcode> = f
        .blocks
        .iter()
        .flat_map(|b| b.insns.iter())
        .map(|i| i.op)
        .collect();

    assert_eq!(
        ops.iter().filter(|o| **o == Opcode::FCmpOEq).count(),
        2,
        "both halves must be compared:\n{}",
        module.display(&ctx.types)
    );
    assert!(
        ops.contains(&Opcode::And),
        "the two half-comparisons must be combined:\n{}",
        module.display(&ctx.types)
    );
    assert!(
        !ops.contains(&Opcode::SetEq),
        "a complex comparison must not become an integer compare:\n{}",
        module.display(&ctx.types)
    );
}

/// A function or an array converts from the pointer it decays to (C17
/// 6.3.2.1p3-4), at the full width of an address. Typed as the designator, a
/// function had no width: `(long)h` sign-extended from it, which x86 lowered
/// as a 32-bit move, and `(_Bool)h` compared at width 0. As a static
/// initializer, `(long)h` is a relocation, not the value of a variable.
#[test]
fn test_function_and_array_convert_from_their_address() {
    let src = "long h(void);\nint arr[4];\n\
               long fn_long(void) { return (long)h; }\n\
               unsigned long fn_ulong(void) { return (unsigned long)h; }\n\
               long arr_long(void) { return (long)arr; }\n\
               _Bool fn_bool(void) { return (_Bool)h; }\n\
               _Bool fn_bool_implicit(void) { return h; }\n\
               int fn_int(void) { return (int)h; }\n\
               long file_long = (long)h;\n\
               long file_arr = (long)arr;\n";
    for target in [
        Target::new(Arch::X86_64, Os::Linux),
        Target::new(Arch::Aarch64, Os::Linux),
    ] {
        let module = linearize_source(src, &target);
        let insns = |f: &str| -> Vec<Instruction> {
            let func = module.functions.iter().find(|x| x.name == f).unwrap();
            func.blocks
                .iter()
                .flat_map(|bb| bb.insns.iter().cloned())
                .collect()
        };
        for f in ["fn_long", "fn_ulong", "arr_long"] {
            let body = insns(f);
            assert!(
                !body
                    .iter()
                    .any(|i| matches!(i.op, Opcode::Sext | Opcode::Zext | Opcode::Trunc)),
                "{f}: an address converts to a 64-bit integer unchanged\n{body:?}"
            );
        }
        for f in ["fn_bool", "fn_bool_implicit"] {
            let body = insns(f);
            let test = body
                .iter()
                .find(|i| i.op == Opcode::SetNe)
                .unwrap_or_else(|| panic!("{f}: no comparison\n{body:?}"));
            assert_eq!(test.operand_width(), 64, "{f}: {test:?}");
        }
        let body = insns("fn_int");
        let trunc = body
            .iter()
            .find(|i| i.op == Opcode::Trunc)
            .unwrap_or_else(|| panic!("fn_int: no truncation\n{body:?}"));
        assert_eq!((trunc.src_size, trunc.size), (64, 32), "{trunc:?}");
        for (global, sym) in [("file_long", "h"), ("file_arr", "arr")] {
            let def = module.globals.iter().find(|g| g.name == global).unwrap();
            assert!(
                matches!(&def.init, Initializer::SymAddr(s) if s == sym),
                "{global}: {:?}",
                def.init
            );
        }
    }
}

/// A member more than 2 GiB into a struct is folded into the address, so no
/// load or store leaves the linearizer with an offset a 32-bit displacement
/// cannot hold -- through a pointer, and through a global.
#[test]
fn test_far_member_offset_is_folded_into_the_address() {
    let src = "struct far { char pad[3000000000UL]; int y; long z; };\n\
               int get(struct far *p) { return p->y; }\n\
               void put(struct far *p, long v) { p->z = v; }\n";
    let module = linearize_source(src, &Target::host());
    for name in ["get", "put"] {
        let func = module.functions.iter().find(|f| f.name == name).unwrap();
        let accesses: Vec<&Instruction> = func
            .blocks
            .iter()
            .flat_map(|bb| bb.insns.iter())
            .filter(|i| matches!(i.op, Opcode::Load | Opcode::Store))
            .collect();
        assert!(!accesses.is_empty());
        for insn in accesses {
            assert!(
                i32::try_from(insn.offset).is_ok(),
                "{name}: {:?} kept offset {}",
                insn.op,
                insn.offset
            );
        }
        assert!(crate::ir::validate::validate_function(func).is_ok());
    }
}

/// A register-sized struct travels through the IR as its value, so a `?:`
/// between two of them selects values, and the assignment consuming the
/// result must not read through it as though it were an address. It did:
/// `rvalue_addr` passed any non-`Sym` pseudo through as a pointer, and `t = c
/// ? u : v` on an eight-byte struct dereferenced the struct's own bits.
#[test]
fn test_register_sized_struct_through_conditional_is_not_dereferenced() {
    let src = "struct S { int a, b; };\n\
               struct S t, u, v; int c;\n\
               void f(void) { t = c ? u : v; }\n";
    let module = linearize_source(src, &Target::host());
    let func = module.functions.iter().find(|f| f.name == "f").expect("f");
    let insns: Vec<&Instruction> = func.blocks.iter().flat_map(|bb| bb.insns.iter()).collect();
    let merged: std::collections::HashSet<PseudoId> = insns
        .iter()
        .filter(|i| matches!(i.op, Opcode::Select | Opcode::Phi))
        .filter_map(|i| i.target)
        .collect();
    assert!(!merged.is_empty(), "expected the arms to be merged");
    for insn in insns.iter().filter(|i| i.op == Opcode::Load) {
        assert!(
            !merged.contains(&insn.src[0]),
            "a load reads through the merged struct value {:?}",
            insn.src[0]
        );
    }
}

/// A cast to `void` converts nothing: `(void)x` of a floating `x` was a
/// float-to-integer conversion, which raises `FE_INVALID` for a NaN.
#[test]
fn test_void_cast_of_a_float_converts_nothing() {
    let src = "void f(float a, double b, long double c) { (void)a; (void)b; (void)c; }\n";
    let module = linearize_source(src, &Target::host());
    let f = module.functions.iter().find(|f| f.name == "f").unwrap();
    let converts = f
        .blocks
        .iter()
        .flat_map(|bb| bb.insns.iter())
        .filter(|i| matches!(i.op, Opcode::FCvtS | Opcode::FCvtU))
        .count();
    assert_eq!(converts, 0);
}

/// A conversion to `_Bool` is a comparison against zero, and a comparison
/// carries its operand type so the backend sizes it from the operands.
///
/// Carrying `_Bool` sized the compare at 8 bits, which became a 32-bit `cmpl`
/// -- `(_Bool)0x100000000L` then read only the low half. The instruction now
/// carries `long`, as every comparison built in `linearize_emit` does.
#[test]
fn test_bool_conversion_compares_at_the_operand_width() {
    let src = "void f(long x) { _Bool b = x; (void)b; }\n";
    let module = linearize_source(src, &Target::host());
    let f = module.functions.iter().find(|f| f.name == "f").unwrap();
    let cmp = f
        .blocks
        .iter()
        .flat_map(|bb| bb.insns.iter())
        .find(|i| i.op == Opcode::SetNe)
        .expect("a _Bool conversion compares against zero");
    assert_eq!(
        cmp.operand_width(),
        64,
        "the compare reads its operand at its own width, not _Bool's"
    );
    assert_eq!(cmp.size, 8, "and produces the _Bool");
}

/// An explicit cast to `_Bool` from a floating type takes the same rule, and
/// not the float-to-integer truncation beside it: `(_Bool)0.5` is 1.
#[test]
fn test_cast_to_bool_from_a_float_does_not_truncate() {
    let src = "void f(double x) { _Bool b = (_Bool)x; (void)b; }\n";
    let module = linearize_source(src, &Target::host());
    let f = module.functions.iter().find(|f| f.name == "f").unwrap();
    let insns: Vec<_> = f.blocks.iter().flat_map(|bb| bb.insns.iter()).collect();
    assert!(
        insns.iter().any(|i| i.op == Opcode::FCmpONe),
        "the cast compares against zero"
    );
    assert!(
        !insns
            .iter()
            .any(|i| matches!(i.op, Opcode::FCvtS | Opcode::FCvtU)),
        "and does not convert the float to an integer"
    );
}

/// `__imag__` of a real operand evaluates it, even though its value is a
/// zero known in advance.
///
/// The zero was returned without linearizing the operand at all, so every
/// effect in it was dropped -- `__imag__ (x += 5.0)` left `x` alone. The
/// store the assignment owes must still be emitted.
#[test]
fn test_imag_of_a_real_operand_emits_its_side_effects() {
    let src = "void f(double x) { (void)(__imag__ (x += 5.0)); }\n";
    let module = linearize_source(src, &Target::host());
    let f = module.functions.iter().find(|f| f.name == "f").unwrap();
    let adds = f
        .blocks
        .iter()
        .flat_map(|bb| bb.insns.iter())
        .filter(|i| i.op == Opcode::FAdd)
        .count();
    assert_eq!(adds, 1, "the assignment inside __imag__ still happens");
}

/// A cast to `void` converts nothing, from a complex operand as well as a
/// real one.
///
/// The `void` check sat after the arm that converts a complex operand to a
/// real type, and `void` is not complex, so `(void)z` took that arm and
/// emitted a conversion -- a `cvttsd2si` at -O0, which raises `FE_INVALID`
/// for a NaN real part. `test_void_cast_of_a_float_converts_nothing` covers
/// the real operand; this is the complex one.
#[test]
fn test_void_cast_of_a_complex_converts_nothing() {
    let src = "void f(double _Complex z, float _Complex w) { (void)z; (void)w; }\n";
    let module = linearize_source(src, &Target::host());
    let f = module.functions.iter().find(|f| f.name == "f").unwrap();
    let converts = f
        .blocks
        .iter()
        .flat_map(|bb| bb.insns.iter())
        .filter(|i| matches!(i.op, Opcode::FCvtS | Opcode::FCvtU | Opcode::FCvtF))
        .count();
    assert_eq!(converts, 0);
}

/// Every bit count -- `ctz`, `clz` (`clrsb`'s too), `popcount` -- is an `int`
/// result that records the 32- or 64-bit operand it reads in `src_size`, so
/// a fold that replaces one with its count builds an `int`. `ctzl` and
/// `clzl` recorded the operand width as the result's.
#[test]
fn test_bit_counts_record_their_operand() {
    let src = "int f(unsigned long l, unsigned u, long s) {\n\
               return __builtin_ctzl(l) + __builtin_clz(u) + __builtin_clzll(l)\n\
               + __builtin_popcountl(l) + __builtin_ctz(u) + __builtin_clrsbl(s);\n}\n";
    let (module, types) = linearize_source_with_types(src, &Target::host());
    let f = module.functions.iter().find(|f| f.name == "f").unwrap();
    let counts: Vec<&Instruction> = f
        .blocks
        .iter()
        .flat_map(|bb| bb.insns.iter())
        .filter(|i| i.op.is_bit_count())
        .collect();
    assert_eq!(counts.len(), 6);
    for insn in counts {
        let operand = if matches!(insn.op, Opcode::Ctz32 | Opcode::Clz32) {
            32
        } else {
            64
        };
        assert_eq!(insn.typ, Some(types.int_id), "{:?}", insn.op);
        assert_eq!(insn.size, 32, "{:?}: the count is an int", insn.op);
        assert_eq!(insn.operand_width(), operand, "{:?}", insn.op);
        assert!(insn.src_typ.is_some(), "{:?}", insn.op);
    }
}

/// A conversion into or out of a complex type is never a scalar conversion
/// instruction, at any site that converts as if by assignment.
///
/// A complex value travels by the address of its two halves, so the scalar
/// conversion of one is meaningless: `return 1;` from a `double _Complex`
/// function sign-extended the 1 to 128 bits and returned it where the caller
/// reads an address, and `double d = z;` converted the 128-bit load of `z`
/// to `double` as though it were an integer.
#[test]
fn test_complex_conversions_are_never_scalar() {
    let src = "double _Complex r1(void) { return 1; }\n\
               float _Complex r2(double d) { return d; }\n\
               long double _Complex r3(void) { return 2.5f; }\n\
               int r4(double _Complex z) { return z; }\n\
               _Bool r5(float _Complex z) { return z; }\n\
               double i1(double _Complex z) { double d = z; return d; }\n\
               double a1(double _Complex z) { double d; d = z; return d; }\n\
               double c1(double _Complex z) { double d = 1; d += z; return d; }\n\
               double take(double);\n\
               double p1(double _Complex z) { return take(z); }\n\
               double _Complex takec(double _Complex);\n\
               double _Complex p2(void) { return takec(3); }\n\
               int l1(double _Complex z) {\n\
               double a[2] = { z, 1 }; struct { int i : 4; } s = { z };\n\
               return a[0] + s.i; }\n";
    let (module, types) = linearize_source_with_types(src, &Target::host());
    for f in &module.functions {
        for insn in f.blocks.iter().flat_map(|bb| bb.insns.iter()) {
            if !insn.op.is_conversion() {
                continue;
            }
            for t in [insn.typ, insn.src_typ].into_iter().flatten() {
                assert!(
                    !types.is_complex(t),
                    "{}: scalar conversion of a complex value: {:?}",
                    f.name,
                    insn.op
                );
            }
        }
    }

    // And a real returned as a complex is returned as the address of the
    // temporary that holds both halves.
    let r1 = module.functions.iter().find(|f| f.name == "r1").unwrap();
    let insns: Vec<&Instruction> = r1.blocks.iter().flat_map(|bb| bb.insns.iter()).collect();
    let ret = insns.iter().find(|i| i.op == Opcode::Ret).unwrap();
    let returned = ret.src[0];
    assert!(
        insns
            .iter()
            .any(|i| i.op == Opcode::SymAddr && i.target == Some(returned)),
        "r1 must return the address of its complex temporary"
    );
}

/// Two vectors of integer lanes differing in signedness compare unsigned,
/// whichever side is unsigned, as gcc does; arithmetic keeps the left
/// operand's lane type. Asked of both targets, whatever the host: SSE2
/// compares such words lane by lane, NEON with one unsigned `cmhi`.
#[test]
fn test_vector_mixed_signedness_compares_unsigned() {
    use crate::ir::SimdOp;
    let src = "typedef short v8hi __attribute__((vector_size(16)));\n\
        typedef unsigned short v8hu __attribute__((vector_size(16)));\n\
        v8hi lt(v8hi a, v8hu b) { return a < b; }\n\
        v8hi gt(v8hu b, v8hi a) { return b > a; }\n\
        v8hi div(v8hi a, v8hu b) { return a / b; }\n";
    for arch in [Arch::X86_64, Arch::Aarch64] {
        let module = linearize_source(src, &Target::new(arch, Os::Linux));
        let ops = |name: &str| -> Vec<Opcode> {
            let func = module.functions.iter().find(|f| f.name == name).unwrap();
            func.blocks
                .iter()
                .flat_map(|bb| bb.insns.iter().map(|i| i.op))
                .collect()
        };
        for (name, unsigned, signed) in [
            ("lt", Opcode::SetB, Opcode::SetLt),
            ("gt", Opcode::SetA, Opcode::SetGt),
        ] {
            let ops = ops(name);
            let signed_ops = [
                signed,
                Opcode::Simd(SimdOp::CmpGt),
                Opcode::Simd(SimdOp::CmpGe),
            ];
            assert!(
                !ops.iter().any(|o| signed_ops.contains(o)),
                "{arch:?} {name}: signed"
            );
            let want = match arch {
                Arch::X86_64 => unsigned,
                Arch::Aarch64 => Opcode::Simd(SimdOp::CmpGtU),
            };
            assert!(ops.contains(&want), "{arch:?} {name}: no {want:?}: {ops:?}");
        }
        let div = ops("div");
        assert!(
            div.contains(&Opcode::DivS) && !div.contains(&Opcode::DivU),
            "{arch:?}"
        );
    }
}
