//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Linearizer tests for assignment: compound assignment through an lvalue,
// one model for `E1 op= E2` (C17 6.5.16.2p3), and `_Atomic` assignment.
//

use super::test_linearize::{has_op, test_pos, TestContext};
use super::*;
use crate::ir::linearize_emit::{compound_assign_arith_type, compound_assign_opcode};
use crate::parse::ast::{
    AssignOp, BlockItem, ExprKind, ExternalDecl, FunctionDef, ParamStyle, Parameter, Stmt, UnaryOp,
};
use crate::symbol::Symbol;
use crate::target::Target;
use crate::types::{CompositeType, MemberAlign, StructMember, Type, TypeModifiers, TypeTable};

// Compound assignment lvalue tests

#[test]
fn test_compound_assignment_deref() {
    // Test: *p += 1;
    // Regression: assignment operators work with dereferenced pointers
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let int_ptr_type = ctx.ptr(int_type);

    // Create symbol for parameter p
    let p_sym = ctx.var("p", int_ptr_type);

    // Function: void test(int *p) { *p += 1; }
    let deref_expr = Expr::typed_unpositioned(
        ExprKind::Unary {
            op: UnaryOp::Deref,
            operand: Box::new(Expr::var_typed(p_sym, int_ptr_type)),
        },
        int_type,
    );

    let assign_expr = Expr::typed_unpositioned(
        ExprKind::Assign {
            op: AssignOp::AddAssign,
            target: Box::new(deref_expr),
            value: Box::new(Expr::int(1, &ctx.types)),
        },
        int_type,
    );

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: ctx.types.void_id,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(p_sym),
            typ: int_ptr_type,
            vm_dims: vec![],
            discarded_dims: vec![],
        }],
        body: Stmt::Expr(assign_expr),
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

    // The IR should have load and store for the dereferenced pointer
    assert!(
        has_op(&module, &[Opcode::Load]),
        "Compound assignment to *p should load: {}",
        ir
    );
    assert!(
        has_op(&module, &[Opcode::Store]),
        "Compound assignment to *p should store: {}",
        ir
    );
    assert!(
        has_op(&module, &[Opcode::Add]),
        "Compound assignment += should have add: {}",
        ir
    );
}

#[test]
fn test_compound_assignment_index() {
    // Test: arr[i] += 1;
    // Regression: assignment operators work with array subscripts
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let arr_ptr_type = ctx.ptr(int_type);

    // Create symbols for parameters
    let arr_sym = ctx.var("arr", arr_ptr_type);
    let i_sym = ctx.var("i", int_type);

    // Function: void test(int *arr, int i) { arr[i] += 1; }
    let index_expr = Expr::typed_unpositioned(
        ExprKind::Index {
            array: Box::new(Expr::var_typed(arr_sym, arr_ptr_type)),
            index: Box::new(Expr::var_typed(i_sym, int_type)),
        },
        int_type,
    );

    let assign_expr = Expr::typed_unpositioned(
        ExprKind::Assign {
            op: AssignOp::AddAssign,
            target: Box::new(index_expr),
            value: Box::new(Expr::int(1, &ctx.types)),
        },
        int_type,
    );

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: ctx.types.void_id,
        name: test_id,
        params: vec![
            Parameter {
                symbol: Some(arr_sym),
                typ: arr_ptr_type,
                vm_dims: vec![],
                discarded_dims: vec![],
            },
            Parameter {
                symbol: Some(i_sym),
                typ: int_type,
                vm_dims: vec![],
                discarded_dims: vec![],
            },
        ],
        body: Stmt::Expr(assign_expr),
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

    // The IR should have load and store for the array element
    // Also should have index calculation (mul for offset)
    assert!(
        has_op(&module, &[Opcode::Load]),
        "Compound assignment to arr[i] should load: {}",
        ir
    );
    assert!(
        has_op(&module, &[Opcode::Store]),
        "Compound assignment to arr[i] should store: {}",
        ir
    );
}

// `_Atomic` through ordinary operators (audit #X1)

/// Build `void test(T x) { x <op>= 1; }` with T either `_Atomic int` or
/// plain `int`, and linearize it.
///
/// The operand is a parameter because the linearizer registers scalar
/// parameters as ordinary locals, which is the same lvalue shape a local
/// declaration produces -- and it keeps the AST small.
fn compound_module(op: AssignOp, atomic: bool) -> (TestContext, Module) {
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");

    // `_Atomic int` -- the qualifier is a modifier on the type. The
    // non-atomic build of the same function is the control: it shows how many
    // plain memory operations the shape costs when nothing is atomic.
    let atomic_int = if atomic {
        let mut t = ctx.types.get(ctx.types.int_id).clone();
        t.modifiers |= TypeModifiers::ATOMIC;
        ctx.types.intern(t)
    } else {
        ctx.types.int_id
    };
    let x_sym = ctx.var("x", atomic_int);

    let assign = Expr::typed_unpositioned(
        ExprKind::Assign {
            op,
            target: Box::new(Expr::var_typed(x_sym, atomic_int)),
            value: Box::new(Expr::typed_unpositioned(
                ExprKind::IntLit(1),
                ctx.types.int_id,
            )),
        },
        atomic_int,
    );

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: ctx.types.void_id,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: atomic_int,
            vm_dims: vec![],
            discarded_dims: vec![],
        }],
        body: Stmt::Block(vec![BlockItem::Statement(Box::new(Stmt::Expr(assign)))]),
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
    (ctx, module)
}

fn count_op(module: &Module, op: Opcode) -> usize {
    module.functions[0]
        .blocks
        .iter()
        .map(|bb| bb.insns.iter().filter(|i| i.op == op).count())
        .sum()
}

/// `x += 1` must become one atomic read-modify-write, and must not leave the
/// plain load/store pair behind that made it a silent data race.
#[test]
fn test_atomic_compound_assign_uses_fetch_add() {
    let (_ctx, module) = compound_module(AssignOp::AddAssign, true);

    assert_eq!(
        count_op(&module, Opcode::AtomicFetchAdd),
        1,
        "expected exactly one AtomicFetchAdd"
    );
    // One instruction replaces the load / add / store the plain shape would
    // need, so neither a plain Load nor a plain Store of the object may
    // survive. Asserted directly rather than by differencing against the
    // non-atomic control: the control's object is promotable, so SSA leaves
    // it with no memory operations at all to difference against.
    assert_eq!(
        count_op(&module, Opcode::Load),
        0,
        "an atomic compound assignment must not load the object separately"
    );
    assert_eq!(
        count_op(&module, Opcode::Store),
        1,
        "the one plain Store left is the parameter prologue; the object's own \
         store must have become atomic"
    );
    // The control is what makes the above mean something: the same function
    // without `_Atomic` gets no atomic operation at all.
    let (_c, control) = compound_module(AssignOp::AddAssign, false);
    assert_eq!(
        count_op(&control, Opcode::AtomicFetchAdd),
        0,
        "the non-atomic control must not become atomic"
    );

    // The operation is sequentially consistent and carries the object's width.
    let insn = module.functions[0]
        .blocks
        .iter()
        .flat_map(|bb| bb.insns.iter())
        .find(|i| i.op == Opcode::AtomicFetchAdd)
        .unwrap();
    assert_eq!(insn.extra().memory_order, MemoryOrder::SeqCst);
    assert_eq!(insn.size, 32);
    assert_eq!(insn.src.len(), 3, "expected [addr, value, order]");
}

/// `x -= 1` maps to the native subtract form rather than the CAS loop.
#[test]
fn test_atomic_compound_assign_uses_fetch_sub() {
    let (_ctx, module) = compound_module(AssignOp::SubAssign, true);
    assert_eq!(count_op(&module, Opcode::AtomicFetchSub), 1);
    assert_eq!(count_op(&module, Opcode::AtomicCas), 0);
}

/// The bitwise operators have native atomic forms too.
#[test]
fn test_atomic_compound_assign_bitops_are_native() {
    for (op, want) in [
        (AssignOp::AndAssign, Opcode::AtomicFetchAnd),
        (AssignOp::OrAssign, Opcode::AtomicFetchOr),
        (AssignOp::XorAssign, Opcode::AtomicFetchXor),
    ] {
        let (_ctx, module) = compound_module(op, true);
        assert_eq!(count_op(&module, want), 1, "{:?} should use {:?}", op, want);
        assert_eq!(count_op(&module, Opcode::AtomicCas), 0);
    }
}

/// Multiplication has no native atomic form, so it becomes a
/// compare-and-swap retry loop: a seeding atomic load, one CAS, and a
/// conditional branch back to the block containing the CAS.
#[test]
fn test_atomic_compound_assign_without_native_op_uses_a_cas_loop() {
    let (_ctx, module) = compound_module(AssignOp::MulAssign, true);

    assert_eq!(count_op(&module, Opcode::AtomicCas), 1, "one CAS");
    assert_eq!(count_op(&module, Opcode::AtomicLoad), 1, "one seeding load");
    assert_eq!(
        count_op(&module, Opcode::AtomicFetchAdd),
        0,
        "must not use a native fetch-op"
    );

    let func = &module.functions[0];
    assert!(
        func.blocks.len() >= 3,
        "a CAS loop needs entry, loop and exit blocks, got {}",
        func.blocks.len()
    );

    // The block holding the CAS must end in a conditional branch whose false
    // edge returns to itself -- otherwise the loop cannot retry.
    let cas_bb = func
        .blocks
        .iter()
        .find(|bb| bb.insns.iter().any(|i| i.op == Opcode::AtomicCas))
        .expect("no block contains the CAS");
    let terminator = cas_bb.insns.last().expect("empty block");
    assert_eq!(
        terminator.op,
        Opcode::Cbr,
        "the CAS block must end in a conditional branch"
    );
}

/// `x = 1` is a single atomic store, not a plain one.
#[test]
fn test_atomic_plain_assign_uses_atomic_store() {
    let (_ctx, module) = compound_module(AssignOp::Assign, true);
    assert_eq!(count_op(&module, Opcode::AtomicStore), 1);

    assert_eq!(
        count_op(&module, Opcode::Store),
        1,
        "the one plain Store left is the parameter prologue; the object's own \
         store must have become atomic"
    );
    let (_c, control) = compound_module(AssignOp::Assign, false);
    assert_eq!(
        count_op(&control, Opcode::AtomicStore),
        0,
        "the non-atomic control must not become atomic"
    );
}

/// An `_Atomic` aggregate of lock-free size lowers to a single atomic store
/// at the aggregate's own width, through an unsigned integer surrogate.
///
/// The IR is where this is worth asserting: an ordinary struct copy reads back
/// what it wrote exactly as the atomic store does, so running the program
/// cannot tell them apart. `insn.size` is what every backend uses, so a
/// wrong width here is a read or write of the neighbouring bytes.
#[test]
fn test_atomic_aggregate_assign_uses_atomic_store() {
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let s_tag = ctx.str("S");
    let a_name = ctx.str("a");
    let int_id = ctx.types.int_id;

    // struct S { int a; } -- four bytes, a lock-free width.
    let struct_type = ctx.types.intern(Type::struct_type(CompositeType {
        tag: Some(s_tag),
        members: vec![StructMember {
            name: a_name,
            typ: int_id,
            offset: 0,
            bit_offset: None,
            bit_width: None,
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
        forward_of: None,
    }));
    let atomic_struct = {
        let mut t = ctx.types.get(struct_type).clone();
        t.modifiers |= TypeModifiers::ATOMIC;
        ctx.types.intern(t)
    };

    // void test(_Atomic struct S g, struct S v) { g = v; }
    let g_sym = ctx.var("g", atomic_struct);
    let v_sym = ctx.var("v", struct_type);
    let assign = Expr::typed_unpositioned(
        ExprKind::Assign {
            op: AssignOp::Assign,
            target: Box::new(Expr::var_typed(g_sym, atomic_struct)),
            value: Box::new(Expr::var_typed(v_sym, struct_type)),
        },
        atomic_struct,
    );
    let func = FunctionDef {
        attrs: Default::default(),
        return_type: ctx.types.void_id,
        name: test_id,
        params: vec![
            Parameter {
                symbol: Some(g_sym),
                typ: atomic_struct,
                vm_dims: vec![],
                discarded_dims: vec![],
            },
            Parameter {
                symbol: Some(v_sym),
                typ: struct_type,
                vm_dims: vec![],
                discarded_dims: vec![],
            },
        ],
        body: Stmt::Block(vec![BlockItem::Statement(Box::new(Stmt::Expr(assign)))]),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
        param_style: ParamStyle::Prototype,
    };
    let module = ctx.linearize(&TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    });

    assert_eq!(
        count_op(&module, Opcode::AtomicStore),
        1,
        "an _Atomic aggregate assignment is one atomic store"
    );

    let store = module.functions[0]
        .blocks
        .iter()
        .flat_map(|bb| bb.insns.iter())
        .find(|i| i.op == Opcode::AtomicStore)
        .expect("no AtomicStore");
    assert_eq!(
        store.size, 32,
        "the access must be the aggregate's own width, not widened"
    );
    let typ = store.typ.expect("AtomicStore carries no type");
    assert!(
        ctx.types.is_integer(typ),
        "the aggregate travels through an integer surrogate; got {:?}",
        ctx.types.kind(typ)
    );
    assert_eq!(
        ctx.types.size_bits(typ),
        32,
        "the surrogate must be the same width as the aggregate"
    );
}

// One model for `E1 op= E2` (C17 6.5.16.2p3)
//
// The ordinary and the `_Atomic` lowerings each used to carry their own copy
// of these rules, and the copies disagreed: the atomic one converted the right
// operand down to the target and computed there, so `_Atomic unsigned char c =
// 50; c /= -5;` divided 50 by 251 and stored 0. Both now go through
// `compound_assign_value`, and these tests pin the decisions it makes.

/// Build `void test(T x) { x <op>= <value>; }` with `T` the chosen type made
/// `_Atomic` and the right operand a plain `int` literal, and linearize it.
///
/// `target` is a selector rather than a `TypeId` because the table the id
/// belongs to is built by `TestContext::new`.
fn atomic_typed_module(
    op: AssignOp,
    target: fn(&TypeTable) -> TypeId,
    value: i64,
) -> (TestContext, Module) {
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_id = ctx.types.int_id;

    let base = target(&ctx.types);
    let atomic_typ = {
        let mut t = ctx.types.get(base).clone();
        t.modifiers |= TypeModifiers::ATOMIC;
        ctx.types.intern(t)
    };
    let x_sym = ctx.var("x", atomic_typ);

    let assign = Expr::typed_unpositioned(
        ExprKind::Assign {
            op,
            target: Box::new(Expr::var_typed(x_sym, atomic_typ)),
            value: Box::new(Expr::typed_unpositioned(ExprKind::IntLit(value), int_id)),
        },
        atomic_typ,
    );
    let func = FunctionDef {
        attrs: Default::default(),
        return_type: ctx.types.void_id,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: atomic_typ,
            vm_dims: vec![],
            discarded_dims: vec![],
        }],
        body: Stmt::Block(vec![BlockItem::Statement(Box::new(Stmt::Expr(assign)))]),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
        param_style: ParamStyle::Prototype,
    };
    let module = ctx.linearize(&TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    });
    (ctx, module)
}

/// The first instruction with this opcode, for asserting on its type and width.
fn first_op(module: &Module, op: Opcode) -> &Instruction {
    module.functions[0]
        .blocks
        .iter()
        .flat_map(|bb| bb.insns.iter())
        .find(|i| i.op == op)
        .unwrap_or_else(|| panic!("no {:?} in the module", op))
}

/// The usual arithmetic conversions decide the type, and the type decides the
/// opcode -- so a narrow unsigned target divided by an `int` is a *signed*
/// 32-bit divide.
#[test]
fn test_compound_assign_divides_at_the_operands_common_type() {
    let types = TypeTable::new(&Target::host());

    let ca = CompoundAssign::new(AssignOp::DivAssign, types.uchar_id, types.int_id);
    let arith = compound_assign_arith_type(&types, &ca);
    assert_eq!(arith, types.int_id, "unsigned char / int is done at int");
    assert_eq!(
        compound_assign_opcode(&types, AssignOp::DivAssign, arith),
        Opcode::DivS
    );

    // Asking the *target's* type instead is the defect this replaced: it makes
    // the same expression an unsigned divide, and `50 /= -5` stores 0.
    assert_eq!(
        compound_assign_opcode(&types, AssignOp::DivAssign, types.uchar_id),
        Opcode::DivU
    );
}

/// The congruent operators are decided the same way, even though their result
/// is the same either width.
#[test]
fn test_compound_assign_add_also_computes_at_the_common_type() {
    let types = TypeTable::new(&Target::host());
    let ca = CompoundAssign::new(AssignOp::AddAssign, types.uchar_id, types.int_id);
    assert_eq!(compound_assign_arith_type(&types, &ca), types.int_id);
    assert_eq!(
        compound_assign_opcode(&types, AssignOp::AddAssign, types.int_id),
        Opcode::Add
    );
    // A floating target picks the floating form of the same operator.
    let fca = CompoundAssign::new(AssignOp::AddAssign, types.float_id, types.int_id);
    let farith = compound_assign_arith_type(&types, &fca);
    assert_eq!(farith, types.float_id);
    assert_eq!(
        compound_assign_opcode(&types, AssignOp::AddAssign, farith),
        Opcode::FAdd
    );
}

/// A shift promotes its **left** operand and nothing else (C17 6.5.7p3), so
/// the right operand's type has no say in the width it is done at.
#[test]
fn test_compound_assign_shift_takes_the_promoted_left_operand() {
    let types = TypeTable::new(&Target::host());

    let ca = CompoundAssign::new(AssignOp::ShrAssign, types.schar_id, types.longlong_id);
    let arith = compound_assign_arith_type(&types, &ca);
    assert_eq!(
        arith, types.int_id,
        "the promoted left operand decides, not the common type"
    );
    assert_ne!(
        arith,
        types.common_type(types.schar_id, types.longlong_id),
        "the shift must not follow the usual arithmetic conversions"
    );

    // And the promotion is what makes the shift arithmetic: `unsigned char`
    // promotes to `int`, so `u >>= 1` on 200 is 100 and not a logical shift of
    // the byte.
    let uca = CompoundAssign::new(AssignOp::ShrAssign, types.uchar_id, types.int_id);
    let uarith = compound_assign_arith_type(&types, &uca);
    assert_eq!(uarith, types.int_id);
    assert_eq!(
        compound_assign_opcode(&types, AssignOp::ShrAssign, uarith),
        Opcode::Asr
    );
    assert_eq!(
        compound_assign_opcode(&types, AssignOp::ShrAssign, types.uchar_id),
        Opcode::Lsr,
        "computing at the target's own width would shift the wrong way"
    );
}

/// `_Bool` promotes to `int` like any narrow integer; what is special about it
/// is the conversion *back*, which is a test against zero.
#[test]
fn test_compound_assign_bool_computes_at_int() {
    let types = TypeTable::new(&Target::host());
    let ca = CompoundAssign::new(AssignOp::SubAssign, types.bool_id, types.int_id);
    assert_eq!(compound_assign_arith_type(&types, &ca), types.int_id);
}

/// Pointer arithmetic is the other exception: the addend arrives already
/// scaled to a byte count and the addition happens at pointer width.
#[test]
fn test_compound_assign_pointer_arithmetic_is_done_at_pointer_width() {
    let types = TypeTable::new(&Target::host());
    let ca = CompoundAssign {
        is_ptr_arith: true,
        ..CompoundAssign::new(AssignOp::AddAssign, types.char_ptr_id, types.long_id)
    };
    assert_eq!(compound_assign_arith_type(&types, &ca), types.long_id);
    assert_eq!(
        compound_assign_opcode(&types, AssignOp::AddAssign, types.long_id),
        Opcode::Add
    );
}

/// `_Atomic unsigned char c; c /= -5;` divides at `int`, in the CAS loop --
/// the same arithmetic the ordinary lowering does.
#[test]
fn test_atomic_compound_divide_computes_at_the_common_type() {
    let (_ctx, module) = atomic_typed_module(AssignOp::DivAssign, |t| t.uchar_id, -5);

    let div = first_op(&module, Opcode::DivS);
    assert_eq!(
        div.size, 32,
        "the divide happens at the common type's width, not the object's"
    );
    assert_eq!(
        count_op(&module, Opcode::DivU),
        0,
        "narrowing the right operand first would make this an unsigned divide"
    );
    assert_eq!(
        count_op(&module, Opcode::AtomicCas),
        1,
        "divide has no native atomic form"
    );
}

/// The same for a shift: promoted left operand, 32-bit arithmetic shift.
#[test]
fn test_atomic_compound_shift_promotes_its_left_operand() {
    let (_ctx, module) = atomic_typed_module(AssignOp::ShrAssign, |t| t.uchar_id, 1);

    let shift = first_op(&module, Opcode::Asr);
    assert_eq!(shift.size, 32, "the left operand is promoted to int first");
    assert_eq!(
        count_op(&module, Opcode::Lsr),
        0,
        "an 8-bit logical shift would be the target's width, not the promoted one"
    );
    assert_eq!(count_op(&module, Opcode::AtomicCas), 1);
}

/// A narrow congruent operator keeps its native fetch-and-op.
///
/// The standard computes `c += 100` at `int` and converts back, but add is
/// congruent modulo 2^8, so the hardware's 8-bit add agrees with it -- and a
/// single instruction beats a retry loop.
#[test]
fn test_atomic_narrow_add_keeps_its_native_fetch_op() {
    let (_ctx, module) = atomic_typed_module(AssignOp::AddAssign, |t| t.uchar_id, 100);

    assert_eq!(count_op(&module, Opcode::AtomicFetchAdd), 1);
    assert_eq!(
        count_op(&module, Opcode::AtomicCas),
        0,
        "no retry loop needed"
    );
    assert_eq!(
        first_op(&module, Opcode::AtomicFetchAdd).size,
        8,
        "the atomic operates at the object's own width"
    );
}

/// `_Atomic _Bool` cannot: converting to `_Bool` is a test against zero, not
/// the truncation congruence permits, so the value stored has to be computed
/// before the exchange.
#[test]
fn test_atomic_bool_compound_assign_cannot_use_a_native_fetch_op() {
    let (_ctx, module) = atomic_typed_module(AssignOp::SubAssign, |t| t.bool_id, 1);

    assert_eq!(
        count_op(&module, Opcode::AtomicFetchSub),
        0,
        "a native fetch-and-sub would store the raw 255"
    );
    assert_eq!(count_op(&module, Opcode::AtomicCas), 1);
    assert!(
        count_op(&module, Opcode::SetNe) >= 1,
        "the CAS loop must convert the result to _Bool before storing it"
    );
}

/// C99 6.7.4p3 asks what an identifier resolves to, not how it is spelled.
///
/// `static int counter; inline int next(int counter) { return counter++; }`
/// reads the parameter. The check once looked the place's `Sym` up by name in
/// the set of file-scope statics, and a parameter's frame slot carries the
/// parameter's name, so the read was refused as a reference to the static.
#[test]
fn test_inline_static_reference_is_decided_by_the_binding() {
    let mut ctx = TestContext::new();
    let int_type = ctx.int_type();
    let global = ctx.var("counter", int_type);
    ctx.symbols.enter_scope();
    let param = {
        let name = ctx.str("counter");
        let sym = Symbol::parameter(name, int_type, ctx.symbols.depth());
        ctx.symbols.declare(sym).unwrap()
    };
    let target = Target::host();
    let mut lin = Linearizer::new(&ctx.symbols, &ctx.types, &ctx.strings, &target);
    lin.file_scope_statics.insert("counter".to_string());
    lin.locals
        .insert(param, LocalVarInfo::frame(PseudoId(1), int_type));

    // Outside an inline definition nothing is refused.
    assert_eq!(lin.inline_static_reference(global), None);

    lin.current_func_is_inline_definition = true;
    assert_eq!(
        lin.inline_static_reference(global).as_deref(),
        Some("counter"),
        "the file-scope static itself"
    );
    assert_eq!(
        lin.inline_static_reference(param),
        None,
        "a parameter spelled like the static is the function's own object"
    );
}
