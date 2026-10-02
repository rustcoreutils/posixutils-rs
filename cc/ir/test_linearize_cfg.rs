//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Linearizer tests for control flow: if, switch, loops, goto and labels,
// constant conditions, unreachable code, VLA scope exits, and the
// consistency of the recorded CFG.
//

use super::test_linearize::{
    has_op, linearize_source, linearize_source_trapping, make_simple_func, test_pos, TestContext,
};
use super::*;
use crate::parse::ast::{
    AssignOp, BinaryOp, BlockItem, ExprKind, ExternalDecl, FunctionDef, Label, ParamStyle,
    Parameter, Stmt,
};
use crate::strings::StringTable;
use crate::target::Target;
use crate::types::{CompositeType, MemberAlign, StructMember, Type, TypeTable};

// Nested if-else CFG edge linking

#[test]
fn test_nested_if_cfg_linking() {
    // Test: if (outer) { if (inner) { x = 1; } else { x = 2; } } else { x = 3; }
    // Verifies that after a nested if-else in the then branch, the inner merge block
    // is correctly linked to the outer merge block in the resulting control-flow graph.
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();

    // Create symbols for parameters
    let outer_sym = ctx.var("outer", int_type);
    let inner_sym = ctx.var("inner", int_type);
    let x_sym = ctx.var("x", int_type);

    // Build: if (inner) { x = 1; } else { x = 2; }
    let inner_then = Stmt::Expr(Expr::typed_unpositioned(
        ExprKind::Assign {
            op: AssignOp::Assign,
            target: Box::new(Expr::var_typed(x_sym, int_type)),
            value: Box::new(Expr::int(1, &ctx.types)),
        },
        int_type,
    ));
    let inner_else = Stmt::Expr(Expr::typed_unpositioned(
        ExprKind::Assign {
            op: AssignOp::Assign,
            target: Box::new(Expr::var_typed(x_sym, int_type)),
            value: Box::new(Expr::int(2, &ctx.types)),
        },
        int_type,
    ));
    let inner_if = Stmt::If {
        cond: Expr::var_typed(inner_sym, int_type),
        then_stmt: Box::new(inner_then),
        else_stmt: Some(Box::new(inner_else)),
    };

    // Build: if (outer) { <inner_if> } else { x = 3; }
    let outer_else = Stmt::Expr(Expr::typed_unpositioned(
        ExprKind::Assign {
            op: AssignOp::Assign,
            target: Box::new(Expr::var_typed(x_sym, int_type)),
            value: Box::new(Expr::int(3, &ctx.types)),
        },
        int_type,
    ));
    let outer_if = Stmt::If {
        cond: Expr::var_typed(outer_sym, int_type),
        then_stmt: Box::new(inner_if),
        else_stmt: Some(Box::new(outer_else)),
    };

    // Function: void test(int outer, int inner, int x) { <outer_if> }
    let func = FunctionDef {
        attrs: Default::default(),
        return_type: ctx.types.void_id,
        name: test_id,
        params: vec![
            Parameter {
                symbol: Some(outer_sym),
                typ: int_type,
                vm_dims: vec![],
                discarded_dims: vec![],
            },
            Parameter {
                symbol: Some(inner_sym),
                typ: int_type,
                vm_dims: vec![],
                discarded_dims: vec![],
            },
            Parameter {
                symbol: Some(x_sym),
                typ: int_type,
                vm_dims: vec![],
                discarded_dims: vec![],
            },
        ],
        body: outer_if,
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

    // The function should have at least 7 blocks:
    // entry, outer_then, outer_else, inner_then, inner_else, inner_merge, outer_merge
    assert!(
        func.blocks.len() >= 7,
        "Nested if-else should produce at least 7 blocks, got {}: {:?}",
        func.blocks.len(),
        func.blocks.iter().map(|b| b.id).collect::<Vec<_>>()
    );

    // The outer merge block (last block) should have 2 parents:
    // one from the inner merge block (via outer then path) and one from outer else.
    let outer_merge = func.blocks.last().unwrap();
    assert!(
        outer_merge.parents.len() >= 2,
        "Outer merge block should have at least 2 parents, got {}: {:?}",
        outer_merge.parents.len(),
        outer_merge.parents
    );
}

// Switch statement linearization tests

#[test]
fn test_switch_basic() {
    // Test: switch(x) { case 1: return 10; case 2: return 20; default: return 0; }
    // Verifies switch instruction is generated and multiple blocks for cases
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();

    // Create symbol for parameter x
    let x_sym = ctx.var("x", int_type);

    // Build switch body: { case 1: return 10; case 2: return 20; default: return 0; }
    // A label carries the statement it prefixes (C17 6.8.1), so each arm is
    // one `BlockItem` rather than a marker followed by a sibling.
    let switch_body = Stmt::Block(vec![
        BlockItem::Statement(Box::new(Stmt::Labeled {
            labels: vec![Label::Case(Expr::int(1, &ctx.types), None)],
            stmt: Box::new(Stmt::Return(Some(Expr::int(10, &ctx.types)))),
        })),
        BlockItem::Statement(Box::new(Stmt::Labeled {
            labels: vec![Label::Case(Expr::int(2, &ctx.types), None)],
            stmt: Box::new(Stmt::Return(Some(Expr::int(20, &ctx.types)))),
        })),
        BlockItem::Statement(Box::new(Stmt::Labeled {
            labels: vec![Label::Default(test_pos())],
            stmt: Box::new(Stmt::Return(Some(Expr::int(0, &ctx.types)))),
        })),
    ]);

    let switch_stmt = Stmt::Switch {
        expr: Expr::var_typed(x_sym, int_type),
        body: Box::new(switch_body),
    };

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
        body: switch_stmt,
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
    let func = &module.functions[0];

    // Switch should generate switch instruction
    assert!(
        has_op(&module, &[Opcode::Switch]),
        "Switch statement should produce switch instruction: {}",
        ir
    );

    // Should have at least 4 blocks: entry + 3 cases (case 1, case 2, default)
    assert!(
        func.blocks.len() >= 4,
        "Switch should produce at least 4 blocks, got {}: {}",
        func.blocks.len(),
        ir
    );
}

#[test]
fn test_switch_with_break() {
    // Test: switch(x) { case 1: x = 10; break; default: x = 0; }
    // Verifies break targets the end of switch, not some outer loop
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();

    // Create symbol for parameter x
    let x_sym = ctx.var("x", int_type);

    let switch_body = Stmt::Block(vec![
        BlockItem::Statement(Box::new(Stmt::Labeled {
            labels: vec![Label::Case(Expr::int(1, &ctx.types), None)],
            stmt: Box::new(Stmt::Expr(Expr::typed_unpositioned(
                ExprKind::Assign {
                    op: AssignOp::Assign,
                    target: Box::new(Expr::var_typed(x_sym, int_type)),
                    value: Box::new(Expr::int(10, &ctx.types)),
                },
                int_type,
            ))),
        })),
        BlockItem::Statement(Box::new(Stmt::Break(test_pos()))),
        BlockItem::Statement(Box::new(Stmt::Labeled {
            labels: vec![Label::Default(test_pos())],
            stmt: Box::new(Stmt::Empty),
        })),
        BlockItem::Statement(Box::new(Stmt::Expr(Expr::typed_unpositioned(
            ExprKind::Assign {
                op: AssignOp::Assign,
                target: Box::new(Expr::var_typed(x_sym, int_type)),
                value: Box::new(Expr::int(0, &ctx.types)),
            },
            int_type,
        )))),
    ]);

    let switch_stmt = Stmt::Switch {
        expr: Expr::var_typed(x_sym, int_type),
        body: Box::new(switch_body),
    };

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: ctx.types.void_id,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: int_type,
            vm_dims: vec![],
            discarded_dims: vec![],
        }],
        body: switch_stmt,
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

    // Should have switch and branch instructions
    assert!(
        has_op(&module, &[Opcode::Switch]),
        "Switch statement should produce switch instruction: {}",
        ir
    );
    assert!(
        has_op(&module, &[Opcode::Br]),
        "Break should produce branch instruction: {}",
        ir
    );
}

// Do-while loop linearization tests

#[test]
fn test_do_while_basic() {
    // Test: do { x = x + 1; } while (x < 10);
    // Verifies body executes before condition check
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();

    // Create symbol for parameter x
    let x_sym = ctx.var("x", int_type);

    // Body: x = x + 1
    let body = Stmt::Expr(Expr::typed_unpositioned(
        ExprKind::Assign {
            op: AssignOp::Assign,
            target: Box::new(Expr::var_typed(x_sym, int_type)),
            value: Box::new(Expr::binary(
                BinaryOp::Add,
                Expr::var_typed(x_sym, int_type),
                Expr::int(1, &ctx.types),
                &ctx.types,
            )),
        },
        int_type,
    ));

    // Condition: x < 10
    let cond = Expr::binary(
        BinaryOp::Lt,
        Expr::var_typed(x_sym, int_type),
        Expr::int(10, &ctx.types),
        &ctx.types,
    );

    let do_while = Stmt::DoWhile {
        body: Box::new(body),
        cond,
    };

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: ctx.types.void_id,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: int_type,
            vm_dims: vec![],
            discarded_dims: vec![],
        }],
        body: do_while,
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
    let func = &module.functions[0];

    // Do-while should have at least 3 blocks: entry/body, condition, exit
    assert!(
        func.blocks.len() >= 3,
        "Do-while should produce at least 3 blocks, got {}: {}",
        func.blocks.len(),
        ir
    );

    // Should have conditional branch for the while condition
    assert!(
        has_op(&module, &[Opcode::Cbr]),
        "Do-while should produce conditional branch: {}",
        ir
    );

    // Should have comparison for x < 10
    assert!(
        has_op(&module, &[Opcode::SetLt]),
        "Do-while condition should have comparison: {}",
        ir
    );
}

#[test]
fn test_do_while_with_break() {
    // Test: do { x = 1; if (cond) break; } while (1);
    // Verifies break exits the do-while loop
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let x_sym = ctx.var("x", int_type);
    let cond_sym = ctx.var("cond", int_type);

    // Body: { x = 1; if (cond) break; }
    let assign = Stmt::Expr(Expr::typed_unpositioned(
        ExprKind::Assign {
            op: AssignOp::Assign,
            target: Box::new(Expr::var_typed(x_sym, int_type)),
            value: Box::new(Expr::int(1, &ctx.types)),
        },
        int_type,
    ));
    let if_break = Stmt::If {
        cond: Expr::var_typed(cond_sym, int_type),
        then_stmt: Box::new(Stmt::Break(test_pos())),
        else_stmt: None,
    };
    let body = Stmt::Block(vec![
        BlockItem::Statement(Box::new(assign)),
        BlockItem::Statement(Box::new(if_break)),
    ]);

    let do_while = Stmt::DoWhile {
        body: Box::new(body),
        cond: Expr::int(1, &ctx.types),
    };

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: ctx.types.void_id,
        name: test_id,
        params: vec![
            Parameter {
                symbol: Some(x_sym),
                typ: int_type,
                vm_dims: vec![],
                discarded_dims: vec![],
            },
            Parameter {
                symbol: Some(cond_sym),
                typ: int_type,
                vm_dims: vec![],
                discarded_dims: vec![],
            },
        ],
        body: do_while,
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

    // Break should generate unconditional branch
    assert!(
        has_op(&module, &[Opcode::Br]),
        "Break in do-while should produce branch: {}",
        ir
    );

    // Should have conditional branch for the if
    assert!(
        has_op(&module, &[Opcode::Cbr]),
        "If statement should produce conditional branch: {}",
        ir
    );
}

// Goto and label linearization tests

#[test]
fn test_goto_forward() {
    // Test: goto end; x = 1; end: x = 2; return x;
    // Verifies forward goto creates proper branch and unreachable code is handled
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let end_id = ctx.str("end");
    let int_type = ctx.int_type();
    let x_sym = ctx.var("x", int_type);

    // Block: { goto end; x = 1; end: x = 2; return x; }
    let body = Stmt::Block(vec![
        BlockItem::Statement(Box::new(Stmt::Goto {
            name: end_id,
            pos: test_pos(),
        })),
        BlockItem::Statement(Box::new(Stmt::Expr(Expr::typed_unpositioned(
            ExprKind::Assign {
                op: AssignOp::Assign,
                target: Box::new(Expr::var_typed(x_sym, int_type)),
                value: Box::new(Expr::int(1, &ctx.types)),
            },
            int_type,
        )))),
        BlockItem::Statement(Box::new(Stmt::Labeled {
            labels: vec![Label::Named {
                name: end_id,
                pos: test_pos(),
            }],
            stmt: Box::new(Stmt::Expr(Expr::typed_unpositioned(
                ExprKind::Assign {
                    op: AssignOp::Assign,
                    target: Box::new(Expr::var_typed(x_sym, int_type)),
                    value: Box::new(Expr::int(2, &ctx.types)),
                },
                int_type,
            ))),
        })),
        BlockItem::Statement(Box::new(Stmt::Return(Some(Expr::var_typed(
            x_sym, int_type,
        ))))),
    ]);

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

    let module = ctx.linearize(&tu);
    let ir = format!("{}", module.display(&ctx.types));

    // Goto should produce unconditional branch
    assert!(
        has_op(&module, &[Opcode::Br]),
        "Goto should produce branch instruction: {}",
        ir
    );
}

#[test]
fn test_goto_backward() {
    // Test: loop: x = x + 1; if (x < 10) goto loop; return x;
    // Verifies backward goto creates loop-like CFG
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let loop_id = ctx.str("loop");
    let int_type = ctx.int_type();
    let x_sym = ctx.var("x", int_type);

    // Build: loop: x = x + 1
    let increment = Stmt::Expr(Expr::typed_unpositioned(
        ExprKind::Assign {
            op: AssignOp::Assign,
            target: Box::new(Expr::var_typed(x_sym, int_type)),
            value: Box::new(Expr::binary(
                BinaryOp::Add,
                Expr::var_typed(x_sym, int_type),
                Expr::int(1, &ctx.types),
                &ctx.types,
            )),
        },
        int_type,
    ));

    // Build: if (x < 10) goto loop;
    let cond = Expr::binary(
        BinaryOp::Lt,
        Expr::var_typed(x_sym, int_type),
        Expr::int(10, &ctx.types),
        &ctx.types,
    );
    let if_goto = Stmt::If {
        cond,
        then_stmt: Box::new(Stmt::Goto {
            name: loop_id,
            pos: test_pos(),
        }),
        else_stmt: None,
    };

    let body = Stmt::Block(vec![
        BlockItem::Statement(Box::new(Stmt::Labeled {
            labels: vec![Label::Named {
                name: loop_id,
                pos: test_pos(),
            }],
            stmt: Box::new(increment),
        })),
        BlockItem::Statement(Box::new(if_goto)),
        BlockItem::Statement(Box::new(Stmt::Return(Some(Expr::var_typed(
            x_sym, int_type,
        ))))),
    ]);

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

    let module = ctx.linearize(&tu);
    let ir = format!("{}", module.display(&ctx.types));

    // Should have conditional branch for the if
    assert!(
        has_op(&module, &[Opcode::Cbr]),
        "Backward goto pattern should have conditional branch: {}",
        ir
    );

    // Should have unconditional branch for the goto
    assert!(
        has_op(&module, &[Opcode::Br]),
        "Goto should produce unconditional branch: {}",
        ir
    );
}

// Nested loop break/continue tests

#[test]
fn test_nested_loop_break() {
    // Test: while(1) { while(1) { break; } x = 1; break; }
    // Verifies inner break only exits inner loop
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let x_sym = ctx.var("x", int_type);

    // Inner loop: while(1) { break; }
    let inner_loop = Stmt::While {
        cond: Expr::int(1, &ctx.types),
        body: Box::new(Stmt::Break(test_pos())),
    };

    // x = 1
    let assign = Stmt::Expr(Expr::typed_unpositioned(
        ExprKind::Assign {
            op: AssignOp::Assign,
            target: Box::new(Expr::var_typed(x_sym, int_type)),
            value: Box::new(Expr::int(1, &ctx.types)),
        },
        int_type,
    ));

    // Outer loop body: { inner_loop; x = 1; break; }
    let outer_body = Stmt::Block(vec![
        BlockItem::Statement(Box::new(inner_loop)),
        BlockItem::Statement(Box::new(assign)),
        BlockItem::Statement(Box::new(Stmt::Break(test_pos()))),
    ]);

    let outer_loop = Stmt::While {
        cond: Expr::int(1, &ctx.types),
        body: Box::new(outer_body),
    };

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: ctx.types.void_id,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: int_type,
            vm_dims: vec![],
            discarded_dims: vec![],
        }],
        body: outer_loop,
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

    // Should have setval for constant 1 (proves inner break doesn't skip x = 1 assignment)
    // After SSA conversion, the store becomes a setval + nop/copy
    assert!(
        has_op(&module, &[Opcode::SetVal]),
        "Inner break should not skip x = 1 assignment (setval for const 1): {}",
        ir
    );

    // Should have multiple branch instructions (breaks)
    let br_count = ir.matches("br ").count();
    assert!(
        br_count >= 2,
        "Should have at least 2 unconditional branches for breaks, got {}: {}",
        br_count,
        ir
    );
}

#[test]
fn test_nested_loop_continue() {
    // Test: while(cond1) { while(cond2) { continue; } x = 1; }
    // Verifies inner continue goes to inner loop condition, not outer
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let cond1_sym = ctx.var("cond1", int_type);
    let cond2_sym = ctx.var("cond2", int_type);
    let x_sym = ctx.var("x", int_type);

    // Inner loop: while(cond2) { continue; }
    let inner_loop = Stmt::While {
        cond: Expr::var_typed(cond2_sym, int_type),
        body: Box::new(Stmt::Continue(test_pos())),
    };

    // x = 1
    let assign = Stmt::Expr(Expr::typed_unpositioned(
        ExprKind::Assign {
            op: AssignOp::Assign,
            target: Box::new(Expr::var_typed(x_sym, int_type)),
            value: Box::new(Expr::int(1, &ctx.types)),
        },
        int_type,
    ));

    // Outer loop body: { inner_loop; x = 1; }
    let outer_body = Stmt::Block(vec![
        BlockItem::Statement(Box::new(inner_loop)),
        BlockItem::Statement(Box::new(assign)),
    ]);

    let outer_loop = Stmt::While {
        cond: Expr::var_typed(cond1_sym, int_type),
        body: Box::new(outer_body),
    };

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: ctx.types.void_id,
        name: test_id,
        params: vec![
            Parameter {
                symbol: Some(cond1_sym),
                typ: int_type,
                vm_dims: vec![],
                discarded_dims: vec![],
            },
            Parameter {
                symbol: Some(cond2_sym),
                typ: int_type,
                vm_dims: vec![],
                discarded_dims: vec![],
            },
            Parameter {
                symbol: Some(x_sym),
                typ: int_type,
                vm_dims: vec![],
                discarded_dims: vec![],
            },
        ],
        body: outer_loop,
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
    let func = &module.functions[0];

    // Should have at least 6 blocks for nested loops
    assert!(
        func.blocks.len() >= 6,
        "Nested loops should produce at least 6 blocks, got {}: {}",
        func.blocks.len(),
        ir
    );

    // Should have setval for constant 1 (proves inner continue doesn't skip x = 1 assignment)
    // After SSA conversion, the store becomes a setval + nop/copy
    assert!(
        has_op(&module, &[Opcode::SetVal]),
        "Inner continue should not skip x = 1 assignment (setval for const 1): {}",
        ir
    );
}

/// The functions `f` in `module` calls, by name.
fn calls_in(module: &Module, f: &str) -> Vec<String> {
    let func = module.functions.iter().find(|x| x.name == f).unwrap();
    func.blocks
        .iter()
        .flat_map(|bb| bb.insns.iter())
        .filter(|i| i.op == Opcode::Call)
        .filter_map(|i| i.extra().func_name.clone())
        .collect()
}

/// Whether `f` in `module` has any conditional branch or switch left.
fn still_branches(module: &Module, f: &str) -> bool {
    let func = module.functions.iter().find(|x| x.name == f).unwrap();
    func.blocks
        .iter()
        .flat_map(|bb| bb.insns.iter())
        .any(|i| matches!(i.op, Opcode::Cbr | Opcode::Switch))
}

/// Each construct that branches on a constant emits only the arm it takes,
/// with no conditional branch left behind: `if`, the loops, `?:`, the
/// short circuits, `switch`, and a floating condition decided exactly (a NaN is
/// unequal to itself; -0.0 equals 0.0).
#[test]
fn test_constant_condition_emits_only_the_taken_arm() {
    let src = "void dead(void); void live(void); int g(void);\n\
               void f_if(void) { if (0) dead(); else live(); }\n\
               void f_while(void) { while (0) dead(); live(); }\n\
               void f_do(void) { do live(); while (0); }\n\
               void f_for(void) { for (; 0;) dead(); live(); }\n\
               void f_and(void) { if (0 && g()) dead(); else live(); }\n\
               void f_or(void) { if (1 || g()) live(); else dead(); }\n\
               void f_and_rest(void) { if (1 && 0) dead(); live(); }\n\
               int f_value(void) { return 0 && (dead(), 1); }\n\
               void f_nan(void) { if (__builtin_nan(\"\") != __builtin_nan(\"\")) live(); else dead(); }\n\
               void f_zero(void) { if (-0.0 == 0.0) live(); else dead(); }\n\
               void f_ternary(void) { 0.5 ? live() : dead(); }\n\
               void f_return(void) { live(); return; dead(); }\n\
               void f_break(void) { for (;;) { live(); break; dead(); } }\n\
               void f_switch(void) { switch (1) { case 0: dead(); break; case 1: live(); } }\n";
    let module = linearize_source(src, &Target::host());
    for f in [
        "f_if",
        "f_while",
        "f_do",
        "f_for",
        "f_and",
        "f_or",
        "f_and_rest",
        "f_value",
        "f_nan",
        "f_zero",
        "f_ternary",
        "f_return",
        "f_break",
        "f_switch",
    ] {
        let calls = calls_in(&module, f);
        assert!(!calls.iter().any(|c| c == "dead"), "{f}: {calls:?}");
        assert!(
            !still_branches(&module, f),
            "{f} still branches on a constant"
        );
    }
    for f in [
        "f_if", "f_while", "f_do", "f_for", "f_and", "f_or", "f_nan", "f_zero",
    ] {
        assert!(calls_in(&module, f).iter().any(|c| c == "live"), "{f}");
    }
}

/// Under `-fno-trapping-math` a floating comparison no value can change the
/// answer of decides its branch, as gcc's front end folds it at `-O0`; the
/// unknown side is still evaluated. With trapping math, the default, it is
/// a comparison like any other: a NaN operand would raise `FE_INVALID`.
#[test]
fn test_decided_float_comparison_folds_only_without_trapping_math() {
    let src = "void dead(void); void live(void); double g(void);\n\
               void f_gt(double x) { if (x > __builtin_inf()) dead(); }\n\
               void f_lt(float y) { if (-__builtin_inff() > y) dead(); }\n\
               void f_huge(double x) { if (x > 1e308 * 10) dead(); }\n\
               void f_nan(double x) { if (x != __builtin_nan(\"\")) live(); else dead(); }\n\
               void f_isgreater(double x) { if (__builtin_isgreater(x, __builtin_inf())) dead(); }\n\
               void f_call(void) { if (g() > __builtin_inf()) dead(); }\n\
               void f_le(double x) { if (x <= __builtin_inf()) live(); }\n";
    let policy = Default::default();
    let (module, _) = linearize_source_trapping(src, &Target::host(), policy, false);
    for f in ["f_gt", "f_lt", "f_huge", "f_nan", "f_isgreater", "f_call"] {
        assert!(!calls_in(&module, f).iter().any(|c| c == "dead"), "{f}");
        assert!(!still_branches(&module, f), "{f}");
    }
    assert!(
        calls_in(&module, "f_call").iter().any(|c| c == "g"),
        "g() still runs"
    );
    // False for a NaN and true otherwise, so undecided.
    assert!(still_branches(&module, "f_le"));

    let (module, _) = linearize_source_trapping(src, &Target::host(), policy, true);
    for f in ["f_gt", "f_lt", "f_huge", "f_nan", "f_isgreater", "f_call"] {
        assert!(still_branches(&module, f), "{f} folded under trapping math");
    }
}

/// Only a constant expression decides a branch. A `const` object is not one,
/// and neither is a condition whose first operand must run.
#[test]
fn test_constant_condition_needs_a_constant_expression() {
    let src = "void maybe(void); int g(void);\n\
               void f_const(void) { const int k = 0; if (k) maybe(); }\n\
               void f_call(void) { if (g() && 0) maybe(); }\n";
    let module = linearize_source(src, &Target::host());
    for f in ["f_const", "f_call"] {
        assert!(still_branches(&module, f), "{f} must still branch");
        assert!(calls_in(&module, f).iter().any(|c| c == "maybe"), "{f}");
    }
}

/// A block inside a dead arm that a label, a `case` or a `default` reaches
/// is kept, and only the code in front of the label is dropped
/// (gcc.c-torture medce-1).
#[test]
fn test_constant_condition_keeps_what_a_label_reaches() {
    let src = "void dead(void); void live(void);\n\
               void f_case(int x) { switch (x) { case 0: if (0) { dead(); case 1: live(); } } }\n\
               void f_goto(int c) { if (c) goto in; if (0) { dead(); in: live(); } }\n\
               void f_default(int x) { switch (x) { case 0: while (0) { dead(); default: live(); } } }\n";
    let module = linearize_source(src, &Target::host());
    for f in ["f_case", "f_goto", "f_default"] {
        let calls = calls_in(&module, f);
        assert_eq!(calls, ["live"], "{f}");
    }
}

/// Test that ternary conditional expressions with pointer dereference use
/// short-circuit evaluation (control flow + phi) instead of Select instruction.
#[test]
fn test_conditional_short_circuit_arrow() {
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let x_id = ctx.str("x");
    let int_type = ctx.types.int_id;

    // Create a struct with an int field: struct { int x; }
    let members = vec![StructMember {
        name: x_id,
        typ: int_type,
        offset: 0,
        bit_offset: None,
        bit_width: None,
        access_bytes: None,
        align: MemberAlign::NATURAL,
    }];
    let struct_type = ctx.types.intern(Type::struct_type(CompositeType {
        tag: None,
        members,
        enum_constants: vec![],
        size: 4,
        align: 4,
        member_align: 4,
        is_complete: true,
        transparent: false,
        anon_id: None,
        forward_of: None,
    }));
    let struct_ptr_type = ctx.types.intern(Type::pointer(struct_type));

    // Create symbol for the parameter
    let entry_sym = ctx.var("entry", struct_ptr_type);

    // Create: entry == NULL ? 0 : entry->x
    let entry_expr = Expr::typed_unpositioned(ExprKind::Ident(entry_sym), struct_ptr_type);
    let null_expr = Expr::typed_unpositioned(ExprKind::IntLit(0), int_type);
    let zero_expr = Expr::typed_unpositioned(ExprKind::IntLit(0), int_type);
    let arrow_expr = Expr::typed_unpositioned(
        ExprKind::Arrow {
            expr: Box::new(entry_expr.clone()),
            member: x_id,
        },
        int_type,
    );
    let cond_eq = Expr::typed_unpositioned(
        ExprKind::Binary {
            left: Box::new(entry_expr),
            op: BinaryOp::Eq,
            right: Box::new(null_expr),
        },
        int_type,
    );
    let conditional = Expr::typed_unpositioned(
        ExprKind::Conditional {
            cond: Box::new(cond_eq),
            then_expr: Box::new(zero_expr),
            else_expr: Box::new(arrow_expr),
        },
        int_type,
    );

    // Create function: int test(struct S *entry) { return entry == NULL ? 0 : entry->x; }
    let func = FunctionDef {
        attrs: Default::default(),
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(entry_sym),
            typ: struct_ptr_type,
            vm_dims: vec![],
            discarded_dims: vec![],
        }],
        body: Stmt::Block(vec![BlockItem::Statement(Box::new(Stmt::Return(Some(
            conditional,
        ))))]),
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

    // Verify that we use control flow (cbr + phi) instead of select instruction.
    // Arrow expressions can cause UB if the pointer is NULL, so we must use
    // short-circuit evaluation to avoid dereferencing NULL.

    // Should have conditional branch (cbr) for proper short-circuit evaluation
    assert!(
        has_op(&module, &[Opcode::Cbr]),
        "Expected conditional branch (cbr) for short-circuit evaluation: {}",
        ir
    );

    // Should have phi instruction to merge results from both branches
    assert!(
        has_op(&module, &[Opcode::Phi]),
        "Expected phi instruction for merging conditional results: {}",
        ir
    );

    // Should NOT have select instruction (would mean eager evaluation of both branches)
    assert!(
        !has_op(&module, &[Opcode::Select]),
        "Should NOT use select instruction with pointer dereference (causes UB): {}",
        ir
    );
}

/// Every CFG edge the linearizer builds is recorded once, in both lists.
///
/// Fifty `if (x) goto end;` all reach one label, so the label collects fifty
/// predecessors. Whether an edge is already present is answered by a set of
/// the edges linked so far rather than by scanning the label's `parents` --
/// the scan made a label thousands of `goto`s jump to quadratic in them. The
/// lists must still hold each edge exactly once, and `parents` and `children`
/// must describe the same edges.
#[test]
fn test_cfg_edges_are_recorded_once_in_both_lists() {
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let end_id = ctx.str("end");
    let int_type = ctx.int_type();
    let x_sym = ctx.var("x", int_type);

    let mut items: Vec<BlockItem> = (0..50)
        .map(|_| {
            BlockItem::Statement(Box::new(Stmt::If {
                cond: Expr::var_typed(x_sym, int_type),
                then_stmt: Box::new(Stmt::Goto {
                    name: end_id,
                    pos: test_pos(),
                }),
                else_stmt: None,
            }))
        })
        .collect();
    items.push(BlockItem::Statement(Box::new(Stmt::Labeled {
        labels: vec![Label::Named {
            name: end_id,
            pos: test_pos(),
        }],
        stmt: Box::new(Stmt::Return(Some(Expr::var_typed(x_sym, int_type)))),
    })));
    let mut func = make_simple_func(test_id, Stmt::Block(items), &ctx.types);
    func.params = vec![Parameter {
        symbol: Some(x_sym),
        typ: int_type,
        vm_dims: vec![],
        discarded_dims: vec![],
    }];
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };
    let module = ctx.linearize(&tu);
    let func = &module.functions[0];

    let mut widest = 0;
    for bb in &func.blocks {
        let mut parents = bb.parents.clone();
        parents.sort();
        parents.dedup();
        assert_eq!(
            parents.len(),
            bb.parents.len(),
            "{:?} repeats a parent",
            bb.id
        );
        let mut children = bb.children.clone();
        children.sort();
        children.dedup();
        assert_eq!(
            children.len(),
            bb.children.len(),
            "{:?} repeats a child",
            bb.id
        );
        for p in &bb.parents {
            let pb = func.get_block(*p).expect("a parent is a block");
            assert!(
                pb.children.contains(&bb.id),
                "{:?} -> {:?} missing a child edge",
                p,
                bb.id
            );
        }
        for c in &bb.children {
            let cb = func.get_block(*c).expect("a child is a block");
            assert!(
                cb.parents.contains(&bb.id),
                "{:?} -> {:?} missing a parent edge",
                bb.id,
                c
            );
        }
        widest = widest.max(bb.parents.len());
    }
    assert!(
        widest >= 50,
        "the label should collect every goto, widest was {widest}"
    );
}

/// Linearize `src`, returning the module and the labels the last function
/// defined, as the linearizer recorded them.
fn linearize_source_labels(src: &str) -> (Module, std::collections::HashSet<String>) {
    let target = Target::host();
    let mut strings = StringTable::new();
    let mut tokenizer = crate::token::lexer::Tokenizer::new(src.as_bytes(), 0, &mut strings);
    let tokens = tokenizer.tokenize();
    let mut symbols = crate::symbol::SymbolTable::new();
    let mut types = TypeTable::new(&target);
    let tu = {
        let mut parser =
            crate::parse::Parser::new(&tokens, &strings, &mut symbols, &mut types, Vec::new());
        parser.parse_translation_unit().expect("parse")
    };
    let mut linearizer = Linearizer::new(&symbols, &types, &strings, &target);
    let module = linearizer.linearize(&tu);
    (module, linearizer.defined_labels.clone())
}

/// A label is defined wherever it sits in a `switch` body -- before a case
/// label, after `default:`, in an arm of an `if` after a case label. The
/// switch-body walk placed labels through a copy of its own that never
/// recorded them, so `&&lbl` naming one was reported as undefined.
#[test]
fn test_labels_inside_switch_body_are_defined() {
    let src = "void *p;\n\
               void f(int a) {\n\
                 switch (a) {\n\
                 a0: case 0: p = &&a1; break;\n\
                 case 1: a1: p = &&dflt; break;\n\
                 default: dflt: if (a) { arm: p = &&a0; } else other: p = &&arm;\n\
                 }\n\
               }\n";
    let (_, defined) = linearize_source_labels(src);
    for name in ["a0", "a1", "dflt", "arm", "other"] {
        assert!(
            defined.contains(name),
            "label '{name}' not recorded as defined"
        );
    }
}

/// A backward `goto` to a label inside a `switch` body leaves the scope of a
/// VLA declared after the label, so it restores the stack. Neither half was
/// seen: a VLA under a case label did not make the function a VLA function,
/// and the switch-body walk never recorded the label's depth -- so the loop
/// grew the stack every time round.
#[test]
fn test_backward_goto_to_switch_label_restores_vla_stack() {
    let src = "void g(volatile char *);\n\
               void f(int k, int n) {\n\
                 switch (k) {\n\
                 case 1: L: { char v[n]; g(v); if (k++ < 9) goto L; }\n\
                 }\n\
               }\n";
    let module = linearize_source(src, &Target::host());
    let func = module.functions.iter().find(|f| f.name == "f").expect("f");
    let label = func
        .blocks
        .iter()
        .find(|bb| bb.label.as_deref() == Some("L"))
        .expect("block for L")
        .id;
    // Two edges reach `L`: the fall-in from `case 1:`, before the VLA, and
    // the `goto`, which has to release it on the way.
    let restoring_jump = func.blocks.iter().any(|bb| {
        bb.insns
            .iter()
            .any(|i| i.op == Opcode::Br && i.bb_true == Some(label))
            && bb.insns.iter().any(|i| i.op == Opcode::StackRestore)
    });
    assert!(restoring_jump, "the backward goto restores no stack");
}

/// A constant condition drops the arm it does not take -- unless that arm
/// defines a label a computed `goto` can reach (compile/pr17913). Dropping it
/// left the label's block empty and unterminated. The arm without one is
/// still dropped, for `?:` and for `?:`'s GNU two-operand form alike.
#[test]
fn test_constant_conditional_keeps_an_arm_that_defines_a_label() {
    let src = "int puts(const char *);\n\
               int f(int k) {\n\
                   void *p = k ? &&a : &&b;\n\
                   int v = 1 ? 3 : ({ a: puts(\"x\"); });\n\
                   int w = 5 ?: ({ b: 6; });\n\
                   int u = 1 ? 4 : puts(\"dead\");\n\
                   if (k) return v + w + u;\n\
                   goto *p;\n\
               }\n";
    let module = linearize_source(src, &Target::host());
    let f = module.functions.iter().find(|f| f.name == "f").unwrap();
    for name in ["a", "b"] {
        let block = f
            .blocks
            .iter()
            .find(|bb| bb.label.as_deref() == Some(name))
            .unwrap_or_else(|| panic!("no block for label {name}"));
        assert!(!block.insns.is_empty(), "label {name} was dropped");
    }
    // `puts("x")` is kept with its label; `puts("dead")` is still folded away.
    let calls = f
        .blocks
        .iter()
        .flat_map(|bb| bb.insns.iter())
        .filter(|i| i.op == Opcode::Call)
        .count();
    assert_eq!(calls, 1);
}

/// A construct that builds control flow of its own, reached where control
/// cannot arrive.
///
/// `current_bb` is `None` after a `goto` and before a `switch`'s first
/// `case`, and `emit` quietly drops what it is handed there. A diamond
/// cannot be dropped that way -- it has to hang three blocks off something --
/// and it took `self.current_bb.unwrap()`, so `sqrt`, whose errno check is a
/// diamond, panicked the compiler outright on the dead statement after a
/// `goto`.
#[test]
fn test_two_way_in_unreachable_code_does_not_panic() {
    let src = "\
double sqrt(double);
double d;
void f(void) { goto skip; d = sqrt(d); skip: return; }
";
    let module = linearize_source(src, &Target::host());
    let f = module.functions.iter().find(|f| f.name == "f").unwrap();
    // The unreachable statement is lowered into a block of its own, which
    // nothing branches to; what matters is that lowering finished at all.
    assert!(!f.blocks.is_empty());
}

/// The same, before a `switch`'s first `case`, which leaves `current_bb`
/// `None` for the same reason (C17 6.8.4.2 gives such a statement no edge).
#[test]
fn test_two_way_before_the_first_case_does_not_panic() {
    let src = "\
double sqrt(double);
double d;
void f(int x) { switch (x) { d = sqrt(d); case 1: d = 1.0; } }
";
    let module = linearize_source(src, &Target::host());
    let f = module.functions.iter().find(|f| f.name == "f").unwrap();
    assert!(!f.blocks.is_empty());
}

// CFG consistency

/// A terminator control never returns from ends its block: what the source
/// says after `longjmp` or `__builtin_unreachable` is lowered into a fresh
/// block, never after the terminator in the same one.
///
/// Both left the rest of the statement in the terminated block, which the
/// verifier -- now run on every compile -- rejects, and each back end emitted
/// after the jump.
#[test]
fn a_non_returning_terminator_ends_its_block() {
    let target = Target::host();
    let src = "typedef long jmp_buf[8];\n\
               void longjmp(jmp_buf, int);\n\
               jmp_buf env;\n\
               int g;\n\
               void jump(void) { longjmp(env, 1); g = 2; }\n\
               void never(int x) { if (x) { __builtin_unreachable(); g = 3; } g = 4; }\n";
    let module = linearize_source(src, &target);
    for f in &module.functions {
        assert_eq!(cfg_inconsistency(f), None, "{}", f.name);
    }
}

/// The validator's report on `func`, or `None` when it is consistent.
///
/// The CFG invariants live in `validate` (I8, I9), which every compile runs;
/// this only adapts its answer to the tests that ask.
fn cfg_inconsistency(func: &Function) -> Option<String> {
    crate::ir::validate::validate_function(func)
        .err()
        .map(|errs| format!("{errs:?}"))
}

/// A `for` post-expression that splits the block still links the back edge from
/// the block the branch was emitted into.
///
/// `&&`, `||` and `?:` leave `current_bb` on their merge block, so
/// `link_bb(post_bb, cond_bb)` recorded an edge out of a block that no longer
/// holds the terminator -- the loop's back edge went missing from the CFG while
/// a merge block gained an unrecorded one.
///
/// Stated on the CFG rather than on the program's answer because the defect
/// makes the compiled loop non-terminating, which a runtime test cannot
/// observe without hanging.
#[test]
fn for_post_expression_splitting_the_block_keeps_the_back_edge() {
    let target = Target::host();
    let cases = [
        ("and", "for (int i = 0; i < n; (void)(n && 1), i++) s += i;"),
        ("or", "for (int i = 0; i < n; (void)(n || 0), i++) s += i;"),
        (
            "ternary",
            "for (int i = 0; i < n; (void)(n ? 1 : 2), i++) s += i;",
        ),
        (
            "and_in_cond_and_post",
            "for (int i = 0; i < n && n; (void)(n && 1), i++) s += i;",
        ),
        (
            "nested_and",
            "for (int i = 0; i < n; (void)(n && (i || 1)), i++) s += i;",
        ),
    ];

    for (tag, loop_src) in cases {
        // Plain, and again inside a switch body -- a separate copy of the
        // lowering that carried the same defect.
        let plain = format!("int f(int n) {{ int s = 0; {loop_src} return s; }}");
        let in_switch = format!(
            "int f(int n) {{ switch (n) {{ case 5: {{ int s = 0; {loop_src} return s; }} \
             default: return 0; }} }}"
        );

        for (where_, src) in [("plain", &plain), ("in_switch", &in_switch)] {
            let module = linearize_source(src, &target);
            let func = module
                .functions
                .iter()
                .find(|f| f.name == "f")
                .expect("function f");
            assert!(
                cfg_inconsistency(func).is_none(),
                "{tag} / {where_}: {}\nsource: {src}",
                cfg_inconsistency(func).unwrap()
            );
        }
    }
}

/// The check above is only as good as its ability to see a broken edge, so
/// assert it rejects one.
#[test]
fn cfg_inconsistency_sees_a_misrecorded_edge() {
    let target = Target::host();
    let module = linearize_source(
        "int f(int n) { int s = 0; for (int i = 0; i < n; i++) s += i; return s; }",
        &target,
    );
    let mut func = module
        .functions
        .iter()
        .find(|f| f.name == "f")
        .expect("function f")
        .clone();
    assert!(cfg_inconsistency(&func).is_none(), "baseline is consistent");

    // Record a successor the terminator does not name -- exactly the shape the
    // `for` defect produced.
    let victim = func.blocks.len() - 1;
    let bogus = func.blocks[0].id;
    func.blocks[victim].children.push(bogus);
    assert!(
        cfg_inconsistency(&func).is_some(),
        "an unnamed successor must be reported"
    );
}

/// The same audit over every other lowering that can end a block with a
/// terminator after evaluating an expression: `while`, `do`/`while`, `switch`,
/// `if`, `?:`, `goto`, `break`/`continue`, and the loops inside a `switch`
/// body.
///
/// Each source puts a short-circuit operator or a `?:` -- the things that split
/// the block and move `current_bb` to a merge block -- where the construct
/// evaluates an expression, so an edge linked from the block the construct
/// started in rather than the one it ended in shows up as an inconsistency.
#[test]
fn short_circuit_operands_keep_every_lowering_cfg_consistent() {
    let target = Target::host();
    let cases = [
        ("while_cond", "while (n && s < 3) s++;"),
        ("do_while_cond", "do { s++; } while (n && s < 3);"),
        ("if_cond", "if (n && s) s = 1; else s = 2;"),
        ("ternary", "s = n && 1 ? (n || 2) : (n ? 3 : 4);"),
        ("for_cond", "for (int i = 0; i < n && n; i++) s += i;"),
        ("for_init", "for (int i = (n && 1); i < n; i++) s += i;"),
        (
            "switch_selector",
            "switch (n && 1) { case 1: s = 1; break; }",
        ),
        (
            "switch_in_loop",
            "while (s < 3) { switch (n && 1) { case 1: s++; break; default: s += 2; } }",
        ),
        ("break_after_split", "while (1) { if (n && 1) break; s++; }"),
        (
            "continue_after_split",
            "for (int i = 0; i < n; i++) { if (n || 0) continue; s++; }",
        ),
        (
            "goto_after_split",
            "if (n && 1) goto done; s = 7; done: s++;",
        ),
        (
            "while_in_switch",
            "switch (n) { case 5: while (n && s < 3) s++; break; default: s = 1; }",
        ),
        (
            "do_while_in_switch",
            "switch (n) { case 5: do { s++; } while (n && s < 3); break; default: s = 1; }",
        ),
        (
            "nested_for_in_switch",
            "switch (n) { case 5: for (int i = 0; i < n; (void)(n && 1), i++) \
             for (int j = 0; j < n; (void)(n || 0), j++) s++; break; default: s = 1; }",
        ),
        (
            "duffs_device",
            "switch (n % 2) { case 0: do { s++; case 1: s += 2; } while (n && --n > 0); }",
        ),
    ];

    for (tag, body) in cases {
        let src = format!("int f(int n) {{ int s = 0; {body} return s; }}");
        let module = linearize_source(&src, &target);
        let func = module
            .functions
            .iter()
            .find(|f| f.name == "f")
            .expect("function f");
        assert!(
            cfg_inconsistency(func).is_none(),
            "{tag}: {}\nsource: {src}",
            cfg_inconsistency(func).unwrap()
        );
    }
}

/// A short-circuit operator with a constant left operand emits no branch.
///
/// `emit_logical_and`/`emit_logical_or` are *triangles*, not diamonds: only one
/// arm block exists, the other phi predecessor is the left operand's own block,
/// and that block's phi value is emitted before the branch. They also branch
/// through `branch_on(Controlling, ..)`, whose `Constant` case deliberately
/// emits a plain `Br` and elides the merge edge entirely.
///
/// So they must not be folded into the generic two-way/diamond builder, which
/// takes a `PseudoId` condition and always emits a `Cbr`: `1 && g()` would
/// regain a dead conditional branch and a second, empty arm block. This test is
/// the guard on that -- it fails if the short-circuit lowerings are ever routed
/// through the diamond helper.
#[test]
fn a_constant_short_circuit_operand_emits_no_branch() {
    let target = Target::host();

    let count_cbr = |src: &str| -> usize {
        let module = linearize_source(src, &target);
        let func = module
            .functions
            .iter()
            .find(|f| f.name == "f")
            .expect("function f");
        func.blocks
            .iter()
            .flat_map(|b| b.insns.iter())
            .filter(|i| i.op == Opcode::Cbr)
            .count()
    };

    // A constant controlling operand is decided at compile time.
    for src in [
        "int g(void); int f(void) { return 1 && g(); }",
        "int g(void); int f(void) { return 0 || g(); }",
    ] {
        assert_eq!(
            count_cbr(src),
            0,
            "a constant short-circuit operand needs no branch: {src}"
        );
    }

    // The control: a runtime operand does branch, so the check above is not
    // passing because nothing ever emits a Cbr.
    for src in [
        "int g(void); int f(int x) { return x && g(); }",
        "int g(void); int f(int x) { return x || g(); }",
    ] {
        assert_eq!(
            count_cbr(src),
            1,
            "a runtime short-circuit operand branches exactly once: {src}"
        );
    }
}

/// Every lowering that builds a two-armed conditional keeps the CFG
/// consistent, in all three places its blocks can be built: where control
/// reaches them, where it cannot -- before a `switch`'s first `case`, and
/// after a `goto` -- and where an arm jumps out from under it.
///
/// These are the shapes that went through `self.current_bb.unwrap()` and so
/// crashed the compiler outright on the last two. They now share
/// `emit_diamond`, or read the block back through
/// `current_or_unreachable_bb`, which starts a block nothing branches to; the
/// point of auditing the CFG rather than only that lowering finished is that
/// such a block is *removed* again by `Function::remove_unreachable_blocks`, and a
/// mislinked edge into or out of it would outlive it.
#[test]
fn conditional_lowerings_keep_the_cfg_consistent() {
    let target = Target::host();

    // (tag, declarations, statement). The statement is placed reachable, then
    // before a `switch`'s first `case`, then after a `goto`.
    let shapes = [
        ("ternary", "int g(void);", "y = g() ? g() : g();"),
        ("logical_and", "int g(void);", "y = g() && g();"),
        ("logical_or", "int g(void);", "y = g() || g();"),
        ("elvis", "int g(void);", "y = g() ?: g();"),
        (
            "nested_ternary",
            "int g(void);",
            "y = g() ? (g() ? g() : g()) : g();",
        ),
        (
            "and_in_ternary",
            "int g(void);",
            "y = g() ? (g() && g()) : g();",
        ),
        (
            "sqrt_errno",
            "double sqrt(double); double d;",
            "d = sqrt(d);",
        ),
        (
            "complex_ternary",
            "int g(void); _Complex double h(void);",
            "(void)(g() ? h() : h());",
        ),
        (
            "complex_elvis",
            "_Complex double h(void);",
            "(void)(h() ?: h());",
        ),
        (
            "complex_int_div",
            "_Complex int ci(void);",
            "(void)(ci() / ci());",
        ),
        (
            "atomic_nand",
            "_Atomic int a;",
            "y = __atomic_fetch_nand(&a, 1, 5);",
        ),
        // An arm that jumps away leaves *that arm* without a block, which is a
        // different read from the condition's.
        (
            "goto_out_of_then_arm",
            "int g(void);",
            "y = x ? ({ goto L; g(); }) : g();",
        ),
        (
            "goto_out_of_else_arm",
            "int g(void);",
            "y = x ? g() : ({ goto L; g(); });",
        ),
    ];

    for (tag, decls, stmt) in shapes {
        let bodies = [
            ("reachable", stmt.to_string()),
            (
                "before_first_case",
                format!("switch (x) {{ {stmt} case 1: y = 1; }}"),
            ),
            ("after_goto", format!("goto L; {stmt}")),
        ];

        for (where_, body) in bodies {
            let src = format!("{decls}\nint f(int x) {{ int y = 0; {body} L: return y; }}\n");
            let module = linearize_source(&src, &target);
            let func = module
                .functions
                .iter()
                .find(|f| f.name == "f")
                .expect("function f");
            assert!(
                cfg_inconsistency(func).is_none(),
                "{tag} / {where_}: {}\nsource: {src}",
                cfg_inconsistency(func).unwrap()
            );
        }
    }
}

// VLA scope exit

/// Every `stacksave` a function emits is matched by a `stackrestore` on each
/// path that leaves the scope it opened.
///
/// A VLA's storage is released by restoring the stack pointer the scope saved,
/// so the two have to balance -- and on *every* exit, which for an `asm goto`
/// means one per edge. The declaration scope and the VLA scope are tracked
/// separately, and the second was opened at only three of the places that open
/// the first, so a VLA declared in a `for`-init clause or a statement
/// expression was never released.
///
/// The consequence is currently masked: c17 always keeps a frame pointer
/// (`-fomit-frame-pointer` is accepted and ignored) and the backend resets
/// `%rsp` from it, so no program leaks stack today. That masking is a frame
/// choice rather than a guarantee, and the imbalance also poisons the forward
/// `goto` machinery, which reads `vla_marks.len()` as a depth -- an unclosed
/// scope makes every later label's recorded depth too high and the restore is
/// skipped. This is stated on the IR because that is where it is true.
#[test]
fn a_vla_scope_releases_the_stack_on_every_exit() {
    let target = Target::host();

    let counts = |src: &str| -> (usize, usize) {
        let module = linearize_source(src, &target);
        let func = module
            .functions
            .iter()
            .find(|f| f.name == "f")
            .expect("function f");
        let n = |op| {
            func.blocks
                .iter()
                .flat_map(|b| b.insns.iter())
                .filter(|i| i.op == op)
                .count()
        };
        (n(Opcode::StackSave), n(Opcode::StackRestore))
    };

    // (tag, source, expected saves, expected restores)
    let cases: &[(&str, &str, usize, usize)] = &[
        // The control: a plain block already balances.
        (
            "block",
            "void s(int*); int f(int n){ for(int k=0;k<3;k++){ int a[n]; a[0]=k; s(a); } return 0; }",
            1,
            1,
        ),
        // A `for`-init clause is a declaration scope like any other.
        (
            "for_init",
            "void s(int*); int f(int n){ for(int k=0;k<3;k++) for(int a[n];0;){ s(a); } return 0; }",
            1,
            1,
        ),
        // The same `for`, inside a switch arm.
        (
            "for_init_in_switch",
            "void s(int*); int f(int n,int x){ switch(x){ case 1: \
             for(int k=0;k<3;k++) for(int a[n];0;){ s(a); } return 0; } return 1; }",
            1,
            1,
        ),
        // A statement expression is a sixth scope entry.
        (
            "stmt_expr",
            "void s(int*); int f(int n){ for(int k=0;k<3;k++) \
             (void)({ int a[n]; a[0]=k; s(a); 0; }); return 0; }",
            1,
            1,
        ),
        // Leaving the scope by `break` unwinds it.
        (
            "break_out_of_for_init",
            "void s(int*); int f(int n){ for(int k=0;k<3;k++) \
             for(int a[n];;){ a[0]=k; s(a); break; } return 0; }",
            1,
            1,
        ),
        // And by a forward `goto`, which is also what the depth bookkeeping
        // needs to stay right for every label after it.
        (
            "goto_out_of_for_init",
            "void s(int*); int f(int n){ for(int k=0;k<3;k++) \
             for(int a[n];;){ a[0]=k; s(a); goto L; } L: return 0; }",
            1,
            1,
        ),
        // A computed `goto` leaves a scope exactly as a plain one does.
        (
            "computed_goto",
            "void s(int*); int f(int n){ void*p=&&L; for(int k=0;k<3;k++){ int a[n]; \
             a[0]=k; s(a); goto *p; } L: return 0; }",
            1,
            1,
        ),
        // `asm goto` has two exits, so it needs a restore on each: the
        // fall-through and the label edge. One restore here means the jump
        // leaves the scope without releasing it.
        (
            "asm_goto",
            "void s(int*); int f(int n){ for(int k=0;k<3;k++){ int a[n]; a[0]=k; s(a); \
             __asm__ goto(\"\" :::: L); } L: return 0; }",
            1,
            2,
        ),
    ];

    for (tag, src, want_save, want_restore) in cases {
        let (saves, restores) = counts(src);
        assert_eq!(
            (saves, restores),
            (*want_save, *want_restore),
            "{tag}: expected {want_save} stacksave / {want_restore} stackrestore, \
             got {saves} / {restores}\nsource: {src}"
        );
    }
}

/// The counts of `stacksave`/`stackrestore` in `f`, for the VLA scope tests.
fn vla_stack_ops(src: &str) -> (usize, usize) {
    let target = Target::host();
    let module = linearize_source(src, &target);
    let func = module
        .functions
        .iter()
        .find(|f| f.name == "f")
        .expect("function f");
    let n = |op| {
        func.blocks
            .iter()
            .flat_map(|b| b.insns.iter())
            .filter(|i| i.op == op)
            .count()
    };
    (n(Opcode::StackSave), n(Opcode::StackRestore))
}

/// Entering a declaration scope and entering a VLA scope are one operation,
/// so nesting the first nests the second: each scope releases exactly what
/// was allocated after it was entered, innermost first.
#[test]
fn nested_scopes_each_release_only_their_own_vlas() {
    let src = "void s(int*); int f(int n){ for(int k=0;k<2;k++){ int a[n]; \
               { int b[n]; s(b); } s(a); } return 0; }";
    assert_eq!(
        vla_stack_ops(src),
        (2, 2),
        "each of the two scopes captures and releases once"
    );

    // And in the right order: the inner scope puts the stack back to what it
    // captured on entry, then the outer one to what *it* captured. Restoring
    // the outer mark first would free the inner array while it is still in
    // scope.
    let target = Target::host();
    let module = linearize_source(src, &target);
    let func = module
        .functions
        .iter()
        .find(|f| f.name == "f")
        .expect("function f");
    let mut saves = Vec::new();
    let mut restores = Vec::new();
    for insn in func.blocks.iter().flat_map(|b| b.insns.iter()) {
        match insn.op {
            Opcode::StackSave => saves.push(insn.target.expect("stacksave target")),
            Opcode::StackRestore => restores.push(insn.src[0]),
            _ => {}
        }
    }
    assert_eq!(
        restores,
        vec![saves[1], saves[0]],
        "scopes must be released innermost first"
    );
}

/// A VLA declared in a `for` init clause is allocated once, ahead of the
/// loop, and its scope *encloses* the loop's exit -- so neither `break` nor
/// `continue` may release it. `continue` especially: the storage is live on
/// the next iteration, and freeing it there would hand the loop a dangling
/// array. Only the loop's own scope, which ends after the exit block, puts
/// the stack back.
///
/// This is what fixes the order in which [`Linearizer::push_vla_mark`] reads
/// the break and continue depths: before the loop pushes its targets, so
/// `unwind_vla_marks` sees the mark as taken *outside* the construct being
/// left and leaves it alone.
#[test]
fn a_for_init_vla_outlives_break_and_continue() {
    assert_eq!(
        vla_stack_ops(
            "void s(int*); int f(int n,int x){ for(int a[n];x;){ s(a); \
             if(x==1) continue; if(x==2) break; } return 0; }"
        ),
        (1, 1),
        "the loop's scope is the only release; break and continue land inside it"
    );
}

/// The mirror image: a VLA declared in the loop *body* is allocated afresh
/// every iteration, so every way out of the body has to release it -- the
/// fall-through through the body scope's end, and the `break` that jumps
/// past it.
#[test]
fn a_loop_body_vla_is_released_on_break_as_well_as_fallthrough() {
    assert_eq!(
        vla_stack_ops(
            "void s(int*); int f(int n,int x){ for(;x;){ int a[n]; s(a); \
             if(x==2) break; } return 0; }"
        ),
        (1, 2),
        "one release on the break edge, one on the way out of the body"
    );
}

/// A `switch` body's braces are a declaration scope like any other: a VLA
/// declared directly in it is released when the switch ends, not left for
/// the enclosing loop to accumulate.
#[test]
fn a_switch_body_block_is_a_scope() {
    assert_eq!(
        vla_stack_ops(
            "void s(int*); int f(int n,int x){ for(int k=0;k<2;k++) \
             switch(x){ default: { int a[n]; s(a); } } return 0; }"
        ),
        (1, 1),
        "the switch body's block releases what it declared"
    );
}

/// Where each `switch` in `f` sends each of its labels, named by the first
/// function the label's block calls: per switch, in block order, the
/// `(low endpoint, callee)` of every case and then `("default", callee)`.
fn switch_arms(src: &str) -> Vec<Vec<(String, String)>> {
    let module = linearize_source(src, &Target::host());
    let func = module.functions.iter().find(|f| f.name == "f").unwrap();
    let callee = |bb: BasicBlockId| {
        func.get_block(bb)
            .unwrap()
            .insns
            .iter()
            .find(|i| i.op == Opcode::Call)
            .and_then(|i| i.extra().func_name.clone())
            .unwrap_or_else(|| panic!("{bb:?} calls nothing"))
    };
    func.blocks
        .iter()
        .flat_map(|bb| bb.insns.iter())
        .filter(|i| i.op == Opcode::Switch)
        .map(|i| {
            let extra = i.extra();
            let mut arms: Vec<(String, String)> = extra
                .switch_cases
                .iter()
                .map(|&(lo, _, bb)| (lo.to_string(), callee(bb)))
                .collect();
            if let Some(bb) = extra.switch_default {
                arms.push(("default".to_string(), callee(bb)));
            }
            arms
        })
        .collect()
}

/// The `(label, callee)` pairs `pairs` spells.
fn arms(pairs: &[(&str, &str)]) -> Vec<(String, String)> {
    pairs
        .iter()
        .map(|&(l, c)| (l.to_string(), c.to_string()))
        .collect()
}

/// A `case` belongs to the innermost enclosing `switch`: the inner switch's
/// labels shadow the outer's equal ones, and an outer label written after
/// the inner switch -- in the same block -- goes back to the outer one.
#[test]
fn nested_switch_labels_belong_to_the_innermost_switch() {
    let got = switch_arms(
        "void o1(void); void o2(void); void od(void); void i1(void); \
         void i2(void); void id(void); \
         void f(int x, int y){ switch(x){ case 1: o1(); \
         { switch(y){ case 1: i1(); break; default: id(); case 2: i2(); } \
         case 2: o2(); } default: od(); } }",
    );
    assert_eq!(
        got,
        vec![
            arms(&[("1", "o1"), ("2", "o2"), ("default", "od")]),
            arms(&[("1", "i1"), ("2", "i2"), ("default", "id")]),
        ]
    );
}

/// Labels inside a loop, an `if`-`else` and a `for` within the body all belong
/// to the switch, as in Duff's device.
#[test]
fn case_labels_inside_nested_statements_reach_the_switch() {
    let got = switch_arms(
        "void c0(void); void c1(void); void c2(void); void c3(void); void d(void); \
         void f(int x, int y){ switch(x){ case 0: c0(); \
         while (y--) { case 1: c1(); if (y) { case 2: c2(); } else case 3: c3(); } \
         for (;;) { default: d(); break; } } }",
    );
    assert_eq!(
        got,
        vec![arms(&[
            ("0", "c0"),
            ("1", "c1"),
            ("2", "c2"),
            ("3", "c3"),
            ("default", "d")
        ])]
    );
}

/// `default` may stand anywhere among the case labels.
#[test]
fn default_label_first_middle_or_last() {
    let decls = "void a(void); void b(void); void d(void); ";
    for body in [
        "default: d(); case 1: a(); case 2: b();",
        "case 1: a(); default: d(); case 2: b();",
        "case 1: a(); case 2: b(); default: d();",
    ] {
        let got = switch_arms(&format!("{decls} void f(int x){{ switch(x){{ {body} }} }}"));
        assert_eq!(
            got,
            vec![arms(&[("1", "a"), ("2", "b"), ("default", "d")])],
            "{body}"
        );
    }
}

/// A backward `goto` to a label ahead of a VLA declaration leaves that
/// declaration's scope, so it restores the stack as it stood at the label --
/// otherwise the loop the jump makes grows the stack every time round. The
/// label's depth is recorded per block, which is also what lets a computed
/// `goto` ask the same question of every candidate at once.
#[test]
fn a_backward_goto_past_a_vla_declaration_releases_it() {
    assert_eq!(
        vla_stack_ops(
            "void s(int*); int f(int n,int x){ lab: { int a[n]; s(a); \
             if(x--) goto lab; } return 0; }"
        ),
        (1, 2),
        "the jump back to `lab` puts the stack where the label found it, and \
         the path that falls out of the block releases it too"
    );
}
