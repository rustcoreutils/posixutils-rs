//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Additional unit tests for linearize.rs
// These tests cover bug fixes and regression prevention
//

use super::*;
use crate::parse::ast::{AssignOp, ExprKind, ExternalDecl, FunctionDef, Parameter, UnaryOp};
use crate::strings::StringTable;

/// Create a default position for test code
fn test_pos() -> Position {
    Position {
        stream: 0,
        line: 1,
        col: 1,
        newline: false,
        whitespace: false,
        noexpand: false,
    }
}

fn test_linearize(tu: &TranslationUnit, types: &TypeTable, strings: &StringTable) -> Module {
    let symbols = SymbolTable::new();
    let target = Target::host();
    linearize(tu, &symbols, types, strings, &target)
}

fn make_simple_func(name: StringId, body: Stmt, types: &TypeTable) -> FunctionDef {
    FunctionDef {
        return_type: types.int_id,
        name,
        params: vec![],
        body,
        pos: test_pos(),
    }
}

// ============================================================================
// Bug fix regression tests
// ============================================================================

#[test]
fn test_parameter_stored_to_local() {
    // Test that scalar parameters are stored to local storage for SSA correctness.
    // This ensures that if a parameter is reassigned inside a branch, phi nodes
    // can be properly inserted at merge points.
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let x_id = strings.intern("x");
    let int_type = types.int_id;

    // Function: int test(int x) { return x; }
    let func = FunctionDef {
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            name: Some(x_id),
            typ: int_type,
        }],
        body: Stmt::Return(Some(Expr::var_typed(x_id, int_type))),
        pos: test_pos(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);

    // The parameter should be stored to a local variable
    // Look for store instruction in the entry block
    assert!(
        ir.contains("store"),
        "Parameter should be stored to local for SSA: {}",
        ir
    );
}

#[test]
fn test_function_with_many_params() {
    // Test that functions with many parameters (> 6 integer args)
    // are correctly handled, including stack-passed arguments.
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let int_type = types.int_id;

    // Create 8 parameters: a, b, c, d, e, f, g, h
    let param_names: Vec<StringId> = (b'a'..=b'h')
        .map(|c| strings.intern(&(c as char).to_string()))
        .collect();

    let params: Vec<Parameter> = param_names
        .iter()
        .map(|&name| Parameter {
            name: Some(name),
            typ: int_type,
        })
        .collect();

    // Return a + h (first and last params)
    let func = FunctionDef {
        return_type: int_type,
        name: test_id,
        params,
        body: Stmt::Return(Some(Expr::binary(
            BinaryOp::Add,
            Expr::var_typed(param_names[0], int_type),
            Expr::var_typed(param_names[7], int_type),
            &types,
        ))),
        pos: test_pos(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);

    // The IR should contain references to both parameters
    assert!(ir.contains("%a"), "IR should have first param: {}", ir);
    assert!(ir.contains("%h"), "IR should have last param: {}", ir);
    // Should have add operation for a + h
    assert!(ir.contains("add"), "IR should have add for a + h: {}", ir);
}

// ============================================================================
// Compound assignment lvalue regression tests
// These test the fix for assignment operators with complex lvalues
// (Member, Arrow, Deref, Index) - linearize.rs:3730
// ============================================================================

#[test]
fn test_compound_assignment_deref() {
    // Test: *p += 1;
    // Regression: assignment operators work with dereferenced pointers
    let mut strings = StringTable::new();
    let mut types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let p_id = strings.intern("p");
    let int_type = types.int_id;
    let int_ptr_type = types.intern(crate::types::Type::pointer(int_type));

    // Function: void test(int *p) { *p += 1; }
    let deref_expr = Expr::typed_unpositioned(
        ExprKind::Unary {
            op: UnaryOp::Deref,
            operand: Box::new(Expr::var_typed(p_id, int_ptr_type)),
        },
        int_type,
    );

    let assign_expr = Expr::typed_unpositioned(
        ExprKind::Assign {
            op: AssignOp::AddAssign,
            target: Box::new(deref_expr),
            value: Box::new(Expr::int(1, &types)),
        },
        int_type,
    );

    let func = FunctionDef {
        return_type: types.void_id,
        name: test_id,
        params: vec![Parameter {
            name: Some(p_id),
            typ: int_ptr_type,
        }],
        body: Stmt::Expr(assign_expr),
        pos: test_pos(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);

    // The IR should have load and store for the dereferenced pointer
    assert!(
        ir.contains("load"),
        "Compound assignment to *p should load: {}",
        ir
    );
    assert!(
        ir.contains("store"),
        "Compound assignment to *p should store: {}",
        ir
    );
    assert!(
        ir.contains("add"),
        "Compound assignment += should have add: {}",
        ir
    );
}

#[test]
fn test_compound_assignment_index() {
    // Test: arr[i] += 1;
    // Regression: assignment operators work with array subscripts
    let mut strings = StringTable::new();
    let mut types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let arr_id = strings.intern("arr");
    let i_id = strings.intern("i");
    let int_type = types.int_id;
    let arr_ptr_type = types.intern(crate::types::Type::pointer(int_type));

    // Function: void test(int *arr, int i) { arr[i] += 1; }
    let index_expr = Expr::typed_unpositioned(
        ExprKind::Index {
            array: Box::new(Expr::var_typed(arr_id, arr_ptr_type)),
            index: Box::new(Expr::var_typed(i_id, int_type)),
        },
        int_type,
    );

    let assign_expr = Expr::typed_unpositioned(
        ExprKind::Assign {
            op: AssignOp::AddAssign,
            target: Box::new(index_expr),
            value: Box::new(Expr::int(1, &types)),
        },
        int_type,
    );

    let func = FunctionDef {
        return_type: types.void_id,
        name: test_id,
        params: vec![
            Parameter {
                name: Some(arr_id),
                typ: arr_ptr_type,
            },
            Parameter {
                name: Some(i_id),
                typ: int_type,
            },
        ],
        body: Stmt::Expr(assign_expr),
        pos: test_pos(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);

    // The IR should have load and store for the array element
    // Also should have index calculation (mul for offset)
    assert!(
        ir.contains("load"),
        "Compound assignment to arr[i] should load: {}",
        ir
    );
    assert!(
        ir.contains("store"),
        "Compound assignment to arr[i] should store: {}",
        ir
    );
}

// ============================================================================
// Array field initialization regression test
// Tests the fix for C99 6.7.8p14: scalar initializes first array element
// ============================================================================

#[test]
fn test_simple_array_element_store() {
    // Simpler test: verify we can store to a specific array element
    // This exercises the path that was fixed for array field initialization
    let mut strings = StringTable::new();
    let mut types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let arr_id = strings.intern("arr");
    let int_type = types.int_id;
    let arr_ptr_type = types.intern(crate::types::Type::pointer(int_type));

    // Function: void test(int *arr) { arr[0] = 42; }
    let index_expr = Expr::typed_unpositioned(
        ExprKind::Index {
            array: Box::new(Expr::var_typed(arr_id, arr_ptr_type)),
            index: Box::new(Expr::int(0, &types)),
        },
        int_type,
    );

    let assign_expr = Expr::typed_unpositioned(
        ExprKind::Assign {
            op: AssignOp::Assign,
            target: Box::new(index_expr),
            value: Box::new(Expr::int(42, &types)),
        },
        int_type,
    );

    let func = FunctionDef {
        return_type: types.void_id,
        name: test_id,
        params: vec![Parameter {
            name: Some(arr_id),
            typ: arr_ptr_type,
        }],
        body: Stmt::Expr(assign_expr),
        pos: test_pos(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);

    // Should have a store for the array element assignment
    assert!(
        ir.contains("store"),
        "Array element assignment should produce store: {}",
        ir
    );
}

// ============================================================================
// Nested if-else CFG edge linking regression test
// Tests the fix for current_bb changing during nested control flow
// ============================================================================

#[test]
fn test_nested_if_cfg_linking() {
    // Test: if (outer) { if (inner) { x = 1; } else { x = 2; } } else { x = 3; }
    // Verifies that after a nested if-else in the then branch, the inner merge block
    // is correctly linked to the outer merge block in the resulting control-flow graph.
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let outer_id = strings.intern("outer");
    let inner_id = strings.intern("inner");
    let x_id = strings.intern("x");
    let int_type = types.int_id;

    // Build: if (inner) { x = 1; } else { x = 2; }
    let inner_then = Stmt::Expr(Expr::typed_unpositioned(
        ExprKind::Assign {
            op: AssignOp::Assign,
            target: Box::new(Expr::var_typed(x_id, int_type)),
            value: Box::new(Expr::int(1, &types)),
        },
        int_type,
    ));
    let inner_else = Stmt::Expr(Expr::typed_unpositioned(
        ExprKind::Assign {
            op: AssignOp::Assign,
            target: Box::new(Expr::var_typed(x_id, int_type)),
            value: Box::new(Expr::int(2, &types)),
        },
        int_type,
    ));
    let inner_if = Stmt::If {
        cond: Expr::var_typed(inner_id, int_type),
        then_stmt: Box::new(inner_then),
        else_stmt: Some(Box::new(inner_else)),
    };

    // Build: if (outer) { <inner_if> } else { x = 3; }
    let outer_else = Stmt::Expr(Expr::typed_unpositioned(
        ExprKind::Assign {
            op: AssignOp::Assign,
            target: Box::new(Expr::var_typed(x_id, int_type)),
            value: Box::new(Expr::int(3, &types)),
        },
        int_type,
    ));
    let outer_if = Stmt::If {
        cond: Expr::var_typed(outer_id, int_type),
        then_stmt: Box::new(inner_if),
        else_stmt: Some(Box::new(outer_else)),
    };

    // Function: void test(int outer, int inner, int x) { <outer_if> }
    let func = FunctionDef {
        return_type: types.void_id,
        name: test_id,
        params: vec![
            Parameter {
                name: Some(outer_id),
                typ: int_type,
            },
            Parameter {
                name: Some(inner_id),
                typ: int_type,
            },
            Parameter {
                name: Some(x_id),
                typ: int_type,
            },
        ],
        body: outer_if,
        pos: test_pos(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
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
    // Without the fix, the outer merge would have incorrect parents.
    let outer_merge = func.blocks.last().unwrap();
    assert!(
        outer_merge.parents.len() >= 2,
        "Outer merge block should have at least 2 parents, got {}: {:?}",
        outer_merge.parents.len(),
        outer_merge.parents
    );
}

// ============================================================================
// Switch statement linearization tests
// ============================================================================

#[test]
fn test_switch_basic() {
    // Test: switch(x) { case 1: return 10; case 2: return 20; default: return 0; }
    // Verifies switch instruction is generated and multiple blocks for cases
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let x_id = strings.intern("x");
    let int_type = types.int_id;

    // Build switch body: { case 1: return 10; case 2: return 20; default: return 0; }
    let switch_body = Stmt::Block(vec![
        BlockItem::Statement(Stmt::Case(Expr::int(1, &types))),
        BlockItem::Statement(Stmt::Return(Some(Expr::int(10, &types)))),
        BlockItem::Statement(Stmt::Case(Expr::int(2, &types))),
        BlockItem::Statement(Stmt::Return(Some(Expr::int(20, &types)))),
        BlockItem::Statement(Stmt::Default),
        BlockItem::Statement(Stmt::Return(Some(Expr::int(0, &types)))),
    ]);

    let switch_stmt = Stmt::Switch {
        expr: Expr::var_typed(x_id, int_type),
        body: Box::new(switch_body),
    };

    let func = FunctionDef {
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            name: Some(x_id),
            typ: int_type,
        }],
        body: switch_stmt,
        pos: test_pos(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);
    let func = &module.functions[0];

    // Switch should generate switch instruction
    assert!(
        ir.contains("switch"),
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
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let x_id = strings.intern("x");
    let int_type = types.int_id;

    let switch_body = Stmt::Block(vec![
        BlockItem::Statement(Stmt::Case(Expr::int(1, &types))),
        BlockItem::Statement(Stmt::Expr(Expr::typed_unpositioned(
            ExprKind::Assign {
                op: AssignOp::Assign,
                target: Box::new(Expr::var_typed(x_id, int_type)),
                value: Box::new(Expr::int(10, &types)),
            },
            int_type,
        ))),
        BlockItem::Statement(Stmt::Break),
        BlockItem::Statement(Stmt::Default),
        BlockItem::Statement(Stmt::Expr(Expr::typed_unpositioned(
            ExprKind::Assign {
                op: AssignOp::Assign,
                target: Box::new(Expr::var_typed(x_id, int_type)),
                value: Box::new(Expr::int(0, &types)),
            },
            int_type,
        ))),
    ]);

    let switch_stmt = Stmt::Switch {
        expr: Expr::var_typed(x_id, int_type),
        body: Box::new(switch_body),
    };

    let func = FunctionDef {
        return_type: types.void_id,
        name: test_id,
        params: vec![Parameter {
            name: Some(x_id),
            typ: int_type,
        }],
        body: switch_stmt,
        pos: test_pos(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);

    // Should have switch and branch instructions
    assert!(
        ir.contains("switch"),
        "Switch statement should produce switch instruction: {}",
        ir
    );
    assert!(
        ir.contains("br"),
        "Break should produce branch instruction: {}",
        ir
    );
}

// ============================================================================
// Do-while loop linearization tests
// ============================================================================

#[test]
fn test_do_while_basic() {
    // Test: do { x = x + 1; } while (x < 10);
    // Verifies body executes before condition check
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let x_id = strings.intern("x");
    let int_type = types.int_id;

    // Body: x = x + 1
    let body = Stmt::Expr(Expr::typed_unpositioned(
        ExprKind::Assign {
            op: AssignOp::Assign,
            target: Box::new(Expr::var_typed(x_id, int_type)),
            value: Box::new(Expr::binary(
                BinaryOp::Add,
                Expr::var_typed(x_id, int_type),
                Expr::int(1, &types),
                &types,
            )),
        },
        int_type,
    ));

    // Condition: x < 10
    let cond = Expr::binary(
        BinaryOp::Lt,
        Expr::var_typed(x_id, int_type),
        Expr::int(10, &types),
        &types,
    );

    let do_while = Stmt::DoWhile {
        body: Box::new(body),
        cond,
    };

    let func = FunctionDef {
        return_type: types.void_id,
        name: test_id,
        params: vec![Parameter {
            name: Some(x_id),
            typ: int_type,
        }],
        body: do_while,
        pos: test_pos(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);
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
        ir.contains("cbr"),
        "Do-while should produce conditional branch: {}",
        ir
    );

    // Should have comparison for x < 10
    assert!(
        ir.contains("setlt"),
        "Do-while condition should have comparison: {}",
        ir
    );
}

#[test]
fn test_do_while_with_break() {
    // Test: do { x = 1; if (cond) break; } while (1);
    // Verifies break exits the do-while loop
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let x_id = strings.intern("x");
    let cond_id = strings.intern("cond");
    let int_type = types.int_id;

    // Body: { x = 1; if (cond) break; }
    let assign = Stmt::Expr(Expr::typed_unpositioned(
        ExprKind::Assign {
            op: AssignOp::Assign,
            target: Box::new(Expr::var_typed(x_id, int_type)),
            value: Box::new(Expr::int(1, &types)),
        },
        int_type,
    ));
    let if_break = Stmt::If {
        cond: Expr::var_typed(cond_id, int_type),
        then_stmt: Box::new(Stmt::Break),
        else_stmt: None,
    };
    let body = Stmt::Block(vec![
        BlockItem::Statement(assign),
        BlockItem::Statement(if_break),
    ]);

    let do_while = Stmt::DoWhile {
        body: Box::new(body),
        cond: Expr::int(1, &types),
    };

    let func = FunctionDef {
        return_type: types.void_id,
        name: test_id,
        params: vec![
            Parameter {
                name: Some(x_id),
                typ: int_type,
            },
            Parameter {
                name: Some(cond_id),
                typ: int_type,
            },
        ],
        body: do_while,
        pos: test_pos(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);

    // Break should generate unconditional branch
    assert!(
        ir.contains("br"),
        "Break in do-while should produce branch: {}",
        ir
    );

    // Should have conditional branch for the if
    assert!(
        ir.contains("cbr"),
        "If statement should produce conditional branch: {}",
        ir
    );
}

// ============================================================================
// Goto and label linearization tests
// ============================================================================

#[test]
fn test_goto_forward() {
    // Test: goto end; x = 1; end: x = 2; return x;
    // Verifies forward goto creates proper branch and unreachable code is handled
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let x_id = strings.intern("x");
    let end_id = strings.intern("end");
    let int_type = types.int_id;

    // Block: { goto end; x = 1; end: x = 2; return x; }
    let body = Stmt::Block(vec![
        BlockItem::Statement(Stmt::Goto(end_id)),
        BlockItem::Statement(Stmt::Expr(Expr::typed_unpositioned(
            ExprKind::Assign {
                op: AssignOp::Assign,
                target: Box::new(Expr::var_typed(x_id, int_type)),
                value: Box::new(Expr::int(1, &types)),
            },
            int_type,
        ))),
        BlockItem::Statement(Stmt::Label {
            name: end_id,
            stmt: Box::new(Stmt::Expr(Expr::typed_unpositioned(
                ExprKind::Assign {
                    op: AssignOp::Assign,
                    target: Box::new(Expr::var_typed(x_id, int_type)),
                    value: Box::new(Expr::int(2, &types)),
                },
                int_type,
            ))),
        }),
        BlockItem::Statement(Stmt::Return(Some(Expr::var_typed(x_id, int_type)))),
    ]);

    let func = FunctionDef {
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            name: Some(x_id),
            typ: int_type,
        }],
        body,
        pos: test_pos(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);

    // Goto should produce unconditional branch
    assert!(
        ir.contains("br"),
        "Goto should produce branch instruction: {}",
        ir
    );
}

#[test]
fn test_goto_backward() {
    // Test: loop: x = x + 1; if (x < 10) goto loop; return x;
    // Verifies backward goto creates loop-like CFG
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let x_id = strings.intern("x");
    let loop_id = strings.intern("loop");
    let int_type = types.int_id;

    // Build: loop: x = x + 1
    let increment = Stmt::Expr(Expr::typed_unpositioned(
        ExprKind::Assign {
            op: AssignOp::Assign,
            target: Box::new(Expr::var_typed(x_id, int_type)),
            value: Box::new(Expr::binary(
                BinaryOp::Add,
                Expr::var_typed(x_id, int_type),
                Expr::int(1, &types),
                &types,
            )),
        },
        int_type,
    ));

    // Build: if (x < 10) goto loop;
    let cond = Expr::binary(
        BinaryOp::Lt,
        Expr::var_typed(x_id, int_type),
        Expr::int(10, &types),
        &types,
    );
    let if_goto = Stmt::If {
        cond,
        then_stmt: Box::new(Stmt::Goto(loop_id)),
        else_stmt: None,
    };

    let body = Stmt::Block(vec![
        BlockItem::Statement(Stmt::Label {
            name: loop_id,
            stmt: Box::new(increment),
        }),
        BlockItem::Statement(if_goto),
        BlockItem::Statement(Stmt::Return(Some(Expr::var_typed(x_id, int_type)))),
    ]);

    let func = FunctionDef {
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            name: Some(x_id),
            typ: int_type,
        }],
        body,
        pos: test_pos(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);

    // Should have conditional branch for the if
    assert!(
        ir.contains("cbr"),
        "Backward goto pattern should have conditional branch: {}",
        ir
    );

    // Should have unconditional branch for the goto
    assert!(
        ir.contains("br "),
        "Goto should produce unconditional branch: {}",
        ir
    );
}

// ============================================================================
// Nested loop break/continue tests
// ============================================================================

#[test]
fn test_nested_loop_break() {
    // Test: while(1) { while(1) { break; } x = 1; break; }
    // Verifies inner break only exits inner loop
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let x_id = strings.intern("x");
    let int_type = types.int_id;

    // Inner loop: while(1) { break; }
    let inner_loop = Stmt::While {
        cond: Expr::int(1, &types),
        body: Box::new(Stmt::Break),
    };

    // x = 1
    let assign = Stmt::Expr(Expr::typed_unpositioned(
        ExprKind::Assign {
            op: AssignOp::Assign,
            target: Box::new(Expr::var_typed(x_id, int_type)),
            value: Box::new(Expr::int(1, &types)),
        },
        int_type,
    ));

    // Outer loop body: { inner_loop; x = 1; break; }
    let outer_body = Stmt::Block(vec![
        BlockItem::Statement(inner_loop),
        BlockItem::Statement(assign),
        BlockItem::Statement(Stmt::Break),
    ]);

    let outer_loop = Stmt::While {
        cond: Expr::int(1, &types),
        body: Box::new(outer_body),
    };

    let func = FunctionDef {
        return_type: types.void_id,
        name: test_id,
        params: vec![Parameter {
            name: Some(x_id),
            typ: int_type,
        }],
        body: outer_loop,
        pos: test_pos(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);

    // Should have setval for constant 1 (proves inner break doesn't skip x = 1 assignment)
    // After SSA conversion, the store becomes a setval + nop/copy
    assert!(
        ir.contains("setval"),
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
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let x_id = strings.intern("x");
    let cond1_id = strings.intern("cond1");
    let cond2_id = strings.intern("cond2");
    let int_type = types.int_id;

    // Inner loop: while(cond2) { continue; }
    let inner_loop = Stmt::While {
        cond: Expr::var_typed(cond2_id, int_type),
        body: Box::new(Stmt::Continue),
    };

    // x = 1
    let assign = Stmt::Expr(Expr::typed_unpositioned(
        ExprKind::Assign {
            op: AssignOp::Assign,
            target: Box::new(Expr::var_typed(x_id, int_type)),
            value: Box::new(Expr::int(1, &types)),
        },
        int_type,
    ));

    // Outer loop body: { inner_loop; x = 1; }
    let outer_body = Stmt::Block(vec![
        BlockItem::Statement(inner_loop),
        BlockItem::Statement(assign),
    ]);

    let outer_loop = Stmt::While {
        cond: Expr::var_typed(cond1_id, int_type),
        body: Box::new(outer_body),
    };

    let func = FunctionDef {
        return_type: types.void_id,
        name: test_id,
        params: vec![
            Parameter {
                name: Some(cond1_id),
                typ: int_type,
            },
            Parameter {
                name: Some(cond2_id),
                typ: int_type,
            },
            Parameter {
                name: Some(x_id),
                typ: int_type,
            },
        ],
        body: outer_loop,
        pos: test_pos(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);
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
        ir.contains("setval"),
        "Inner continue should not skip x = 1 assignment (setval for const 1): {}",
        ir
    );
}

// ============================================================================
// Unary operation tests
// ============================================================================

#[test]
fn test_unary_logical_not() {
    // Test: return !x;
    // Verifies logical not produces comparison to zero
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let x_id = strings.intern("x");
    let int_type = types.int_id;

    let not_expr = Expr::typed_unpositioned(
        ExprKind::Unary {
            op: UnaryOp::Not,
            operand: Box::new(Expr::var_typed(x_id, int_type)),
        },
        int_type,
    );

    let func = FunctionDef {
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            name: Some(x_id),
            typ: int_type,
        }],
        body: Stmt::Return(Some(not_expr)),
        pos: test_pos(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);

    // Logical not should produce seteq (comparison to zero)
    assert!(
        ir.contains("seteq"),
        "Logical NOT should produce seteq instruction: {}",
        ir
    );
}

#[test]
fn test_unary_bitwise_not() {
    // Test: return ~x;
    // Verifies bitwise not produces not instruction
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let x_id = strings.intern("x");
    let int_type = types.int_id;

    let not_expr = Expr::typed_unpositioned(
        ExprKind::Unary {
            op: UnaryOp::BitNot,
            operand: Box::new(Expr::var_typed(x_id, int_type)),
        },
        int_type,
    );

    let func = FunctionDef {
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            name: Some(x_id),
            typ: int_type,
        }],
        body: Stmt::Return(Some(not_expr)),
        pos: test_pos(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);

    // Bitwise not should produce not instruction
    assert!(
        ir.contains("not"),
        "Bitwise NOT should produce not instruction: {}",
        ir
    );
}

#[test]
fn test_unary_negate() {
    // Test: return -x;
    // Verifies unary negation produces neg instruction
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let x_id = strings.intern("x");
    let int_type = types.int_id;

    let neg_expr = Expr::typed_unpositioned(
        ExprKind::Unary {
            op: UnaryOp::Neg,
            operand: Box::new(Expr::var_typed(x_id, int_type)),
        },
        int_type,
    );

    let func = FunctionDef {
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            name: Some(x_id),
            typ: int_type,
        }],
        body: Stmt::Return(Some(neg_expr)),
        pos: test_pos(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);

    // Negation should produce neg instruction
    assert!(
        ir.contains("neg"),
        "Unary negation should produce neg instruction: {}",
        ir
    );
}

#[test]
fn test_pre_increment() {
    // Test: return ++x;
    // Verifies pre-increment adds 1 and returns new value
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let x_id = strings.intern("x");
    let int_type = types.int_id;

    let inc_expr = Expr::typed_unpositioned(
        ExprKind::Unary {
            op: UnaryOp::PreInc,
            operand: Box::new(Expr::var_typed(x_id, int_type)),
        },
        int_type,
    );

    let func = FunctionDef {
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            name: Some(x_id),
            typ: int_type,
        }],
        body: Stmt::Return(Some(inc_expr)),
        pos: test_pos(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);

    // Pre-increment should produce add instruction
    assert!(
        ir.contains("add"),
        "Pre-increment should produce add instruction: {}",
        ir
    );

    // Should store the incremented value
    assert!(
        ir.contains("store"),
        "Pre-increment should store new value: {}",
        ir
    );
}

// ============================================================================
// Pointer arithmetic tests
// ============================================================================

#[test]
fn test_pointer_add_int() {
    // Test: return p + 5;
    // Verifies pointer arithmetic scales by element size
    let mut strings = StringTable::new();
    let mut types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let p_id = strings.intern("p");
    let int_type = types.int_id;
    let int_ptr_type = types.intern(crate::types::Type::pointer(int_type));

    // p + 5
    let add_expr = Expr::binary(
        BinaryOp::Add,
        Expr::var_typed(p_id, int_ptr_type),
        Expr::int(5, &types),
        &types,
    );

    let func = FunctionDef {
        return_type: int_ptr_type,
        name: test_id,
        params: vec![Parameter {
            name: Some(p_id),
            typ: int_ptr_type,
        }],
        body: Stmt::Return(Some(add_expr)),
        pos: test_pos(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);

    // Pointer addition should have multiplication for scaling
    assert!(
        ir.contains("mul"),
        "Pointer add should scale by element size (mul): {}",
        ir
    );

    // Should have add instruction
    assert!(
        ir.contains("add"),
        "Pointer add should have add instruction: {}",
        ir
    );
}

#[test]
fn test_pointer_difference() {
    // Test: return p - q;
    // Verifies pointer difference divides by element size
    let mut strings = StringTable::new();
    let mut types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let p_id = strings.intern("p");
    let q_id = strings.intern("q");
    let int_type = types.int_id;
    let long_type = types.long_id;
    let int_ptr_type = types.intern(crate::types::Type::pointer(int_type));

    // p - q (result is ptrdiff_t, which is long)
    let diff_expr = Expr::typed_unpositioned(
        ExprKind::Binary {
            op: BinaryOp::Sub,
            left: Box::new(Expr::var_typed(p_id, int_ptr_type)),
            right: Box::new(Expr::var_typed(q_id, int_ptr_type)),
        },
        long_type,
    );

    let func = FunctionDef {
        return_type: long_type,
        name: test_id,
        params: vec![
            Parameter {
                name: Some(p_id),
                typ: int_ptr_type,
            },
            Parameter {
                name: Some(q_id),
                typ: int_ptr_type,
            },
        ],
        body: Stmt::Return(Some(diff_expr)),
        pos: test_pos(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);

    // Pointer difference should have subtraction
    assert!(
        ir.contains("sub"),
        "Pointer difference should have sub instruction: {}",
        ir
    );

    // Should have division for scaling (divs for signed division)
    assert!(
        ir.contains("div"),
        "Pointer difference should divide by element size: {}",
        ir
    );
}

// ============================================================================
// Floating-point operation tests
// ============================================================================

#[test]
fn test_float_add() {
    // Test: double a, b; return a + b;
    // Verifies float addition produces fadd instruction
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let a_id = strings.intern("a");
    let b_id = strings.intern("b");
    let double_type = types.double_id;

    let add_expr = Expr::binary(
        BinaryOp::Add,
        Expr::var_typed(a_id, double_type),
        Expr::var_typed(b_id, double_type),
        &types,
    );

    let func = FunctionDef {
        return_type: double_type,
        name: test_id,
        params: vec![
            Parameter {
                name: Some(a_id),
                typ: double_type,
            },
            Parameter {
                name: Some(b_id),
                typ: double_type,
            },
        ],
        body: Stmt::Return(Some(add_expr)),
        pos: test_pos(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);

    // Float addition should produce fadd instruction
    assert!(
        ir.contains("fadd"),
        "Float addition should produce fadd instruction: {}",
        ir
    );
}

#[test]
fn test_float_comparison() {
    // Test: double a, b; return a < b;
    // Verifies float comparison produces fcmp instruction
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let a_id = strings.intern("a");
    let b_id = strings.intern("b");
    let int_type = types.int_id;
    let double_type = types.double_id;

    let cmp_expr = Expr::binary(
        BinaryOp::Lt,
        Expr::var_typed(a_id, double_type),
        Expr::var_typed(b_id, double_type),
        &types,
    );

    let func = FunctionDef {
        return_type: int_type,
        name: test_id,
        params: vec![
            Parameter {
                name: Some(a_id),
                typ: double_type,
            },
            Parameter {
                name: Some(b_id),
                typ: double_type,
            },
        ],
        body: Stmt::Return(Some(cmp_expr)),
        pos: test_pos(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);

    // Float comparison should produce fcmp instruction
    assert!(
        ir.contains("fcmp"),
        "Float comparison should produce fcmp instruction: {}",
        ir
    );
}

#[test]
fn test_float_to_int_cast() {
    // Test: double x; return (int)x;
    // Verifies float-to-int cast produces fcvts instruction
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let x_id = strings.intern("x");
    let int_type = types.int_id;
    let double_type = types.double_id;

    let cast_expr = Expr::typed_unpositioned(
        ExprKind::Cast {
            cast_type: int_type,
            expr: Box::new(Expr::var_typed(x_id, double_type)),
        },
        int_type,
    );

    let func = FunctionDef {
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            name: Some(x_id),
            typ: double_type,
        }],
        body: Stmt::Return(Some(cast_expr)),
        pos: test_pos(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);

    // Float-to-int cast should produce fcvts instruction
    assert!(
        ir.contains("fcvts"),
        "Float-to-int cast should produce fcvts instruction: {}",
        ir
    );
}

#[test]
fn test_int_to_float_cast() {
    // Test: int x; return (double)x;
    // Verifies int-to-float cast produces scvtf instruction
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let x_id = strings.intern("x");
    let int_type = types.int_id;
    let double_type = types.double_id;

    let cast_expr = Expr::typed_unpositioned(
        ExprKind::Cast {
            cast_type: double_type,
            expr: Box::new(Expr::var_typed(x_id, int_type)),
        },
        double_type,
    );

    let func = FunctionDef {
        return_type: double_type,
        name: test_id,
        params: vec![Parameter {
            name: Some(x_id),
            typ: int_type,
        }],
        body: Stmt::Return(Some(cast_expr)),
        pos: test_pos(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);

    // Int-to-float cast should produce scvtf instruction
    assert!(
        ir.contains("scvtf"),
        "Int-to-float cast should produce scvtf instruction: {}",
        ir
    );
}

// ============================================================================
// Core linearization tests (moved from linearize.rs)
// ============================================================================

#[test]
fn test_linearize_empty_function() {
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let func = make_simple_func(test_id, Stmt::Block(vec![]), &types);
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    assert_eq!(module.functions.len(), 1);
    assert_eq!(module.functions[0].name, "test");
    assert!(!module.functions[0].blocks.is_empty());
}

#[test]
fn test_linearize_return() {
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let func = make_simple_func(test_id, Stmt::Return(Some(Expr::int(42, &types))), &types);
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);
    assert!(ir.contains("ret"));
}

#[test]
fn test_linearize_if() {
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let func = make_simple_func(
        test_id,
        Stmt::If {
            cond: Expr::int(1, &types),
            then_stmt: Box::new(Stmt::Return(Some(Expr::int(1, &types)))),
            else_stmt: Some(Box::new(Stmt::Return(Some(Expr::int(0, &types))))),
        },
        &types,
    );
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);
    assert!(ir.contains("cbr")); // Conditional branch
}

#[test]
fn test_linearize_while() {
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let func = make_simple_func(
        test_id,
        Stmt::While {
            cond: Expr::int(1, &types),
            body: Box::new(Stmt::Break),
        },
        &types,
    );
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    assert!(module.functions[0].blocks.len() >= 3); // cond, body, exit
}

#[test]
fn test_linearize_for() {
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let i_id = strings.intern("i");
    // for (int i = 0; i < 10; i++) { }
    let int_type = types.int_id;
    let i_var = Expr::var_typed(i_id, int_type);
    let func = make_simple_func(
        test_id,
        Stmt::For {
            init: Some(ForInit::Declaration(Declaration {
                declarators: vec![crate::parse::ast::InitDeclarator {
                    name: i_id,
                    typ: int_type,
                    init: Some(Expr::int(0, &types)),
                    vla_sizes: vec![],
                }],
            })),
            cond: Some(Expr::binary(
                BinaryOp::Lt,
                i_var.clone(),
                Expr::int(10, &types),
                &types,
            )),
            post: Some(Expr::typed(
                ExprKind::PostInc(Box::new(i_var)),
                int_type,
                test_pos(),
            )),
            body: Box::new(Stmt::Empty),
        },
        &types,
    );
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    assert!(module.functions[0].blocks.len() >= 4); // entry, cond, body, post, exit
}

#[test]
fn test_linearize_binary_expr() {
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    // return 1 + 2 * 3;
    let func = make_simple_func(
        test_id,
        Stmt::Return(Some(Expr::binary(
            BinaryOp::Add,
            Expr::int(1, &types),
            Expr::binary(
                BinaryOp::Mul,
                Expr::int(2, &types),
                Expr::int(3, &types),
                &types,
            ),
            &types,
        ))),
        &types,
    );
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);
    assert!(ir.contains("mul"));
    assert!(ir.contains("add"));
}

#[test]
fn test_linearize_function_with_params() {
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let add_id = strings.intern("add");
    let a_id = strings.intern("a");
    let b_id = strings.intern("b");
    let int_type = types.int_id;
    let func = FunctionDef {
        return_type: int_type,
        name: add_id,
        params: vec![
            Parameter {
                name: Some(a_id),
                typ: int_type,
            },
            Parameter {
                name: Some(b_id),
                typ: int_type,
            },
        ],
        body: Stmt::Return(Some(Expr::binary(
            BinaryOp::Add,
            Expr::var_typed(a_id, int_type),
            Expr::var_typed(b_id, int_type),
            &types,
        ))),
        pos: test_pos(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);
    assert!(ir.contains("add"));
    assert!(ir.contains("%a"));
    assert!(ir.contains("%b"));
}

#[test]
fn test_linearize_call() {
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let foo_id = strings.intern("foo");
    let func = make_simple_func(
        test_id,
        Stmt::Return(Some(Expr::call(
            Expr::var(foo_id),
            vec![Expr::int(1, &types), Expr::int(2, &types)],
            &types,
        ))),
        &types,
    );
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);
    assert!(ir.contains("call"));
    assert!(ir.contains("foo"));
}

#[test]
fn test_linearize_comparison() {
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let func = make_simple_func(
        test_id,
        Stmt::Return(Some(Expr::binary(
            BinaryOp::Lt,
            Expr::int(1, &types),
            Expr::int(2, &types),
            &types,
        ))),
        &types,
    );
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);
    assert!(ir.contains("setlt"));
}

#[test]
fn test_linearize_unsigned_comparison() {
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");

    // Create unsigned comparison: (unsigned)1 < (unsigned)2
    let uint_type = types.uint_id;
    let mut left = Expr::int(1, &types);
    left.typ = Some(uint_type);
    let mut right = Expr::int(2, &types);
    right.typ = Some(uint_type);
    let mut cmp = Expr::binary(BinaryOp::Lt, left, right, &types);
    cmp.typ = Some(types.int_id);

    let func = make_simple_func(test_id, Stmt::Return(Some(cmp)), &types);
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);
    // Should use unsigned comparison opcode (setb = set if below)
    assert!(
        ir.contains("setb"),
        "Expected 'setb' for unsigned comparison, got:\n{}",
        ir
    );
}

#[test]
fn test_display_module() {
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let main_id = strings.intern("main");
    let func = make_simple_func(main_id, Stmt::Return(Some(Expr::int(0, &types))), &types);
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);

    // Should have proper structure
    assert!(ir.contains("define"));
    assert!(ir.contains("main"));
    assert!(ir.contains(".L0:")); // Entry block label
    assert!(ir.contains("ret"));
}

#[test]
fn test_type_propagation_expr_type() {
    let strings = StringTable::new();
    let types = TypeTable::new(64);

    // Create an expression with a type annotation
    let mut expr = Expr::int(42, &types);
    // Simulate type evaluation having set the type
    expr.typ = Some(types.int_id);

    // Create linearizer and test that expr_type reads from the expression
    let symbols = SymbolTable::new();
    let target = Target::host();
    let linearizer = Linearizer::new(&symbols, &types, &strings, &target);
    let typ = linearizer.expr_type(&expr);
    assert_eq!(types.kind(typ), TypeKind::Int);

    // Test with unsigned type
    let mut unsigned_expr = Expr::int(42, &types);
    unsigned_expr.typ = Some(types.uint_id);
    let typ = linearizer.expr_type(&unsigned_expr);
    assert!(types.is_unsigned(typ));
}

#[test]
fn test_type_propagation_double_literal() {
    let strings = StringTable::new();
    let types = TypeTable::new(64);

    // Create a double literal
    let mut expr = Expr::new(ExprKind::FloatLit(3.14), test_pos());
    expr.typ = Some(types.double_id);

    let symbols = SymbolTable::new();
    let target = Target::host();
    let linearizer = Linearizer::new(&symbols, &types, &strings, &target);
    let typ = linearizer.expr_type(&expr);
    assert_eq!(types.kind(typ), TypeKind::Double);
}

// ========================================================================
// SSA Conversion Tests
// ========================================================================

/// Helper to linearize without SSA conversion (for comparing before/after)
fn linearize_no_ssa(tu: &TranslationUnit, types: &TypeTable, strings: &StringTable) -> Module {
    let symbols = SymbolTable::new();
    let target = Target::host();
    let mut linearizer = Linearizer::new_no_ssa(&symbols, types, strings, &target);
    linearizer.linearize(tu)
}

#[test]
fn test_local_var_emits_load_store() {
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let x_id = strings.intern("x");
    // int test() { int x = 1; return x; }
    let int_type = types.int_id;
    let func = FunctionDef {
        return_type: int_type,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(Declaration {
                declarators: vec![crate::parse::ast::InitDeclarator {
                    name: x_id,
                    typ: int_type,
                    init: Some(Expr::int(1, &types)),
                    vla_sizes: vec![],
                }],
            }),
            BlockItem::Statement(Stmt::Return(Some(Expr::var_typed(x_id, int_type)))),
        ]),
        pos: test_pos(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    // Without SSA, should have store and load
    let module = linearize_no_ssa(&tu, &types, &strings);
    let ir = format!("{}", module);
    assert!(
        ir.contains("store"),
        "Should have store instruction before SSA: {}",
        ir
    );
    assert!(
        ir.contains("load"),
        "Should have load instruction before SSA: {}",
        ir
    );
}

#[test]
fn test_ssa_converts_local_to_phi() {
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let cond_id = strings.intern("cond");
    let x_id = strings.intern("x");
    // int test(int cond) {
    //     int x = 1;
    //     if (cond) x = 2;
    //     return x;
    // }
    let int_type = types.int_id;

    let func = FunctionDef {
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            name: Some(cond_id),
            typ: int_type,
        }],
        body: Stmt::Block(vec![
            // int x = 1;
            BlockItem::Declaration(Declaration {
                declarators: vec![crate::parse::ast::InitDeclarator {
                    name: x_id,
                    typ: int_type,
                    init: Some(Expr::int(1, &types)),
                    vla_sizes: vec![],
                }],
            }),
            // if (cond) x = 2;
            BlockItem::Statement(Stmt::If {
                cond: Expr::var_typed(cond_id, int_type),
                then_stmt: Box::new(Stmt::Expr(Expr::assign(
                    Expr::var_typed(x_id, int_type),
                    Expr::int(2, &types),
                    &types,
                ))),
                else_stmt: None,
            }),
            // return x;
            BlockItem::Statement(Stmt::Return(Some(Expr::var_typed(x_id, int_type)))),
        ]),
        pos: test_pos(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    // With SSA, should have phi node at merge point
    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);

    // Should have a phi instruction
    assert!(
        ir.contains("phi"),
        "SSA should insert phi node at merge point: {}",
        ir
    );
}

#[test]
fn test_ssa_loop_variable() {
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let i_id = strings.intern("i");
    // int test() {
    //     int i = 0;
    //     while (i < 10) { i = i + 1; }
    //     return i;
    // }
    let int_type = types.int_id;

    let i_var = || Expr::var_typed(i_id, int_type);

    let func = FunctionDef {
        return_type: int_type,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            // int i = 0;
            BlockItem::Declaration(Declaration {
                declarators: vec![crate::parse::ast::InitDeclarator {
                    name: i_id,
                    typ: int_type,
                    init: Some(Expr::int(0, &types)),
                    vla_sizes: vec![],
                }],
            }),
            // while (i < 10) { i = i + 1; }
            BlockItem::Statement(Stmt::While {
                cond: Expr::binary(BinaryOp::Lt, i_var(), Expr::int(10, &types), &types),
                body: Box::new(Stmt::Expr(Expr::assign(
                    i_var(),
                    Expr::binary(BinaryOp::Add, i_var(), Expr::int(1, &types), &types),
                    &types,
                ))),
            }),
            // return i;
            BlockItem::Statement(Stmt::Return(Some(i_var()))),
        ]),
        pos: test_pos(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    // With SSA, should have phi node at loop header
    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);

    // Loop should have a phi at the condition block
    assert!(ir.contains("phi"), "Loop should have phi node: {}", ir);
}

#[test]
fn test_short_circuit_and() {
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let a_id = strings.intern("a");
    let b_id = strings.intern("b");
    // int test(int a, int b) {
    //     return a && b;
    // }
    // Short-circuit: if a is false, don't evaluate b
    let int_type = types.int_id;

    let func = FunctionDef {
        return_type: int_type,
        name: test_id,
        params: vec![
            Parameter {
                name: Some(a_id),
                typ: int_type,
            },
            Parameter {
                name: Some(b_id),
                typ: int_type,
            },
        ],
        body: Stmt::Return(Some(Expr::binary(
            BinaryOp::LogAnd,
            Expr::var_typed(a_id, int_type),
            Expr::var_typed(b_id, int_type),
            &types,
        ))),
        pos: test_pos(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);

    // Short-circuit AND should have:
    // 1. A conditional branch (cbr) to skip evaluation of b if a is false
    // 2. A phi node to merge the result
    assert!(
        ir.contains("cbr"),
        "Short-circuit AND should have conditional branch: {}",
        ir
    );
    assert!(
        ir.contains("phi"),
        "Short-circuit AND should have phi node: {}",
        ir
    );
}

#[test]
fn test_short_circuit_or() {
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let a_id = strings.intern("a");
    let b_id = strings.intern("b");
    // int test(int a, int b) {
    //     return a || b;
    // }
    // Short-circuit: if a is true, don't evaluate b
    let int_type = types.int_id;

    let func = FunctionDef {
        return_type: int_type,
        name: test_id,
        params: vec![
            Parameter {
                name: Some(a_id),
                typ: int_type,
            },
            Parameter {
                name: Some(b_id),
                typ: int_type,
            },
        ],
        body: Stmt::Return(Some(Expr::binary(
            BinaryOp::LogOr,
            Expr::var_typed(a_id, int_type),
            Expr::var_typed(b_id, int_type),
            &types,
        ))),
        pos: test_pos(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);

    // Short-circuit OR should have:
    // 1. A conditional branch (cbr) to skip evaluation of b if a is true
    // 2. A phi node to merge the result
    assert!(
        ir.contains("cbr"),
        "Short-circuit OR should have conditional branch: {}",
        ir
    );
    assert!(
        ir.contains("phi"),
        "Short-circuit OR should have phi node: {}",
        ir
    );
}

#[test]
fn test_ternary_pure_uses_select() {
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let a_id = strings.intern("a");
    let b_id = strings.intern("b");
    // int test(int a, int b) {
    //     return a > b ? a : b;  // pure ternary -> should use select
    // }
    let int_type = types.int_id;

    // Build ternary: (a > b) ? a : b
    let cond = Expr::binary(
        BinaryOp::Gt,
        Expr::var_typed(a_id, int_type),
        Expr::var_typed(b_id, int_type),
        &types,
    );
    let ternary = Expr::typed(
        ExprKind::Conditional {
            cond: Box::new(cond),
            then_expr: Box::new(Expr::var_typed(a_id, int_type)),
            else_expr: Box::new(Expr::var_typed(b_id, int_type)),
        },
        int_type,
        test_pos(),
    );

    let func = FunctionDef {
        return_type: int_type,
        name: test_id,
        params: vec![
            Parameter {
                name: Some(a_id),
                typ: int_type,
            },
            Parameter {
                name: Some(b_id),
                typ: int_type,
            },
        ],
        body: Stmt::Return(Some(ternary)),
        pos: test_pos(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);

    // Pure ternary should use select instruction (enables cmov/csel)
    // Note: IR displays as "sel" not "select"
    assert!(
        ir.contains("sel."),
        "Pure ternary should use select instruction: {}",
        ir
    );
    // Should NOT have phi (that's for impure ternary)
    assert!(
        !ir.contains("phi"),
        "Pure ternary should NOT use phi node: {}",
        ir
    );
}

#[test]
fn test_ternary_impure_uses_phi() {
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let a_id = strings.intern("a");
    let foo_id = strings.intern("foo");
    let bar_id = strings.intern("bar");
    // int test(int a) {
    //     return a ? foo() : bar();  // impure ternary -> should use phi
    // }
    let int_type = types.int_id;

    // Build ternary with function calls (impure)
    let foo_call = Expr::typed(
        ExprKind::Call {
            func: Box::new(Expr::var_typed(foo_id, int_type)),
            args: vec![],
        },
        int_type,
        test_pos(),
    );
    let bar_call = Expr::typed(
        ExprKind::Call {
            func: Box::new(Expr::var_typed(bar_id, int_type)),
            args: vec![],
        },
        int_type,
        test_pos(),
    );
    let ternary = Expr::typed(
        ExprKind::Conditional {
            cond: Box::new(Expr::var_typed(a_id, int_type)),
            then_expr: Box::new(foo_call),
            else_expr: Box::new(bar_call),
        },
        int_type,
        test_pos(),
    );

    let func = FunctionDef {
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            name: Some(a_id),
            typ: int_type,
        }],
        body: Stmt::Return(Some(ternary)),
        pos: test_pos(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);

    // Impure ternary should use phi (for proper short-circuit evaluation)
    assert!(
        ir.contains("phi"),
        "Impure ternary should use phi node: {}",
        ir
    );
    // Should have conditional branch
    assert!(
        ir.contains("cbr"),
        "Impure ternary should use conditional branch: {}",
        ir
    );
    // Should NOT use select (that's for pure ternary)
    assert!(
        !ir.contains("sel."),
        "Impure ternary should NOT use select instruction: {}",
        ir
    );
}

#[test]
fn test_ternary_with_assignment_uses_phi() {
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let a_id = strings.intern("a");
    let b_id = strings.intern("b");
    // int test(int a, int b) {
    //     return a ? (b = 1) : (b = 2);  // assignment is impure
    // }
    let int_type = types.int_id;

    // Build ternary with assignments (impure)
    let assign1 = Expr::typed(
        ExprKind::Assign {
            op: AssignOp::Assign,
            target: Box::new(Expr::var_typed(b_id, int_type)),
            value: Box::new(Expr::int(1, &types)),
        },
        int_type,
        test_pos(),
    );
    let assign2 = Expr::typed(
        ExprKind::Assign {
            op: AssignOp::Assign,
            target: Box::new(Expr::var_typed(b_id, int_type)),
            value: Box::new(Expr::int(2, &types)),
        },
        int_type,
        test_pos(),
    );
    let ternary = Expr::typed(
        ExprKind::Conditional {
            cond: Box::new(Expr::var_typed(a_id, int_type)),
            then_expr: Box::new(assign1),
            else_expr: Box::new(assign2),
        },
        int_type,
        test_pos(),
    );

    let func = FunctionDef {
        return_type: int_type,
        name: test_id,
        params: vec![
            Parameter {
                name: Some(a_id),
                typ: int_type,
            },
            Parameter {
                name: Some(b_id),
                typ: int_type,
            },
        ],
        body: Stmt::Return(Some(ternary)),
        pos: test_pos(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);

    // Assignment is impure, so should use phi
    assert!(
        ir.contains("phi"),
        "Ternary with assignment should use phi: {}",
        ir
    );
    assert!(
        !ir.contains("sel."),
        "Ternary with assignment should NOT use select: {}",
        ir
    );
}

#[test]
fn test_ternary_with_post_increment_uses_phi() {
    let mut strings = StringTable::new();
    let types = TypeTable::new(64);
    let test_id = strings.intern("test");
    let a_id = strings.intern("a");
    let b_id = strings.intern("b");
    // int test(int a, int b) {
    //     return a ? b++ : b--;  // post-inc/dec is impure
    // }
    let int_type = types.int_id;

    // Build ternary with post-inc/dec (impure)
    let post_inc = Expr::typed(
        ExprKind::PostInc(Box::new(Expr::var_typed(b_id, int_type))),
        int_type,
        test_pos(),
    );
    let post_dec = Expr::typed(
        ExprKind::PostDec(Box::new(Expr::var_typed(b_id, int_type))),
        int_type,
        test_pos(),
    );
    let ternary = Expr::typed(
        ExprKind::Conditional {
            cond: Box::new(Expr::var_typed(a_id, int_type)),
            then_expr: Box::new(post_inc),
            else_expr: Box::new(post_dec),
        },
        int_type,
        test_pos(),
    );

    let func = FunctionDef {
        return_type: int_type,
        name: test_id,
        params: vec![
            Parameter {
                name: Some(a_id),
                typ: int_type,
            },
            Parameter {
                name: Some(b_id),
                typ: int_type,
            },
        ],
        body: Stmt::Return(Some(ternary)),
        pos: test_pos(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module);

    // Post-increment/decrement is impure, so should use phi
    assert!(
        ir.contains("phi"),
        "Ternary with post-inc/dec should use phi: {}",
        ir
    );
    assert!(
        !ir.contains("sel."),
        "Ternary with post-inc/dec should NOT use select: {}",
        ir
    );
}
