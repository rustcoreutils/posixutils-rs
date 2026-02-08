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

// Allow approximate float constants in tests - we're testing literal parsing, not using PI
#![allow(clippy::approx_constant)]

use super::*;
use crate::parse::ast::{
    AssignOp, BlockItem, Declaration, Designator, ExprKind, ExternalDecl, FunctionDef,
    InitDeclarator, InitElement, Parameter, UnaryOp,
};
use crate::strings::StringTable;
use crate::symbol::Symbol;
use crate::target::Target;
use crate::types::{CompositeType, StructMember, Type};

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

/// Test context that provides StringTable, TypeTable, and SymbolTable for tests.
/// This makes it easy to create test symbols without boilerplate.
struct TestContext {
    strings: StringTable,
    types: TypeTable,
    symbols: SymbolTable,
}

impl TestContext {
    fn new() -> Self {
        Self {
            strings: StringTable::new(),
            types: TypeTable::new(&Target::host()),
            symbols: SymbolTable::new(),
        }
    }

    /// Intern a string and create a variable symbol for it, returning the SymbolId.
    fn var(&mut self, name: &str, typ: TypeId) -> SymbolId {
        let name_id = self.strings.intern(name);
        let sym = Symbol::variable(name_id, typ, self.symbols.depth());
        self.symbols.declare(sym).unwrap()
    }

    /// Intern a string and return the StringId (for function names etc.)
    fn str(&mut self, name: &str) -> StringId {
        self.strings.intern(name)
    }

    /// Get common int type
    fn int_type(&self) -> TypeId {
        self.types.int_id
    }

    /// Create a pointer type
    fn ptr(&self, pointee: TypeId) -> TypeId {
        self.types.pointer_to(pointee)
    }

    /// Linearize with this context
    fn linearize(&self, tu: &TranslationUnit) -> Module {
        let target = Target::host();
        linearize(
            tu,
            &self.symbols,
            &self.types,
            &self.strings,
            &target,
            false,
        )
    }
}

fn test_linearize(tu: &TranslationUnit, types: &TypeTable, strings: &StringTable) -> Module {
    let symbols = SymbolTable::new();
    let target = Target::host();
    linearize(tu, &symbols, types, strings, &target, false)
}

fn make_simple_func(name: StringId, body: Stmt, types: &TypeTable) -> FunctionDef {
    FunctionDef {
        return_type: types.int_id,
        name,
        params: vec![],
        body,
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
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
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();

    // Create symbol for parameter x
    let x_sym = ctx.var("x", int_type);

    // Function: int test(int x) { return x; }
    let func = FunctionDef {
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: int_type,
        }],
        body: Stmt::Return(Some(Expr::var_typed(x_sym, int_type))),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
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
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();

    // Create 8 parameters: a, b, c, d, e, f, g, h
    let param_syms: Vec<SymbolId> = (b'a'..=b'h')
        .map(|c| ctx.var(&(c as char).to_string(), int_type))
        .collect();

    let params: Vec<Parameter> = param_syms
        .iter()
        .map(|&sym| Parameter {
            symbol: Some(sym),
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
            Expr::var_typed(param_syms[0], int_type),
            Expr::var_typed(param_syms[7], int_type),
            &ctx.types,
        ))),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
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
        return_type: ctx.types.void_id,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(p_sym),
            typ: int_ptr_type,
        }],
        body: Stmt::Expr(assign_expr),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
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
        return_type: ctx.types.void_id,
        name: test_id,
        params: vec![
            Parameter {
                symbol: Some(arr_sym),
                typ: arr_ptr_type,
            },
            Parameter {
                symbol: Some(i_sym),
                typ: int_type,
            },
        ],
        body: Stmt::Expr(assign_expr),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
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
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let arr_ptr_type = ctx.ptr(int_type);

    // Create symbol for parameter arr
    let arr_sym = ctx.var("arr", arr_ptr_type);

    // Function: void test(int *arr) { arr[0] = 42; }
    let index_expr = Expr::typed_unpositioned(
        ExprKind::Index {
            array: Box::new(Expr::var_typed(arr_sym, arr_ptr_type)),
            index: Box::new(Expr::int(0, &ctx.types)),
        },
        int_type,
    );

    let assign_expr = Expr::typed_unpositioned(
        ExprKind::Assign {
            op: AssignOp::Assign,
            target: Box::new(index_expr),
            value: Box::new(Expr::int(42, &ctx.types)),
        },
        int_type,
    );

    let func = FunctionDef {
        return_type: ctx.types.void_id,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(arr_sym),
            typ: arr_ptr_type,
        }],
        body: Stmt::Expr(assign_expr),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
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
        return_type: ctx.types.void_id,
        name: test_id,
        params: vec![
            Parameter {
                symbol: Some(outer_sym),
                typ: int_type,
            },
            Parameter {
                symbol: Some(inner_sym),
                typ: int_type,
            },
            Parameter {
                symbol: Some(x_sym),
                typ: int_type,
            },
        ],
        body: outer_if,
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
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
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();

    // Create symbol for parameter x
    let x_sym = ctx.var("x", int_type);

    // Build switch body: { case 1: return 10; case 2: return 20; default: return 0; }
    let switch_body = Stmt::Block(vec![
        BlockItem::Statement(Stmt::Case(Expr::int(1, &ctx.types))),
        BlockItem::Statement(Stmt::Return(Some(Expr::int(10, &ctx.types)))),
        BlockItem::Statement(Stmt::Case(Expr::int(2, &ctx.types))),
        BlockItem::Statement(Stmt::Return(Some(Expr::int(20, &ctx.types)))),
        BlockItem::Statement(Stmt::Default),
        BlockItem::Statement(Stmt::Return(Some(Expr::int(0, &ctx.types)))),
    ]);

    let switch_stmt = Stmt::Switch {
        expr: Expr::var_typed(x_sym, int_type),
        body: Box::new(switch_body),
    };

    let func = FunctionDef {
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: int_type,
        }],
        body: switch_stmt,
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
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
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();

    // Create symbol for parameter x
    let x_sym = ctx.var("x", int_type);

    let switch_body = Stmt::Block(vec![
        BlockItem::Statement(Stmt::Case(Expr::int(1, &ctx.types))),
        BlockItem::Statement(Stmt::Expr(Expr::typed_unpositioned(
            ExprKind::Assign {
                op: AssignOp::Assign,
                target: Box::new(Expr::var_typed(x_sym, int_type)),
                value: Box::new(Expr::int(10, &ctx.types)),
            },
            int_type,
        ))),
        BlockItem::Statement(Stmt::Break),
        BlockItem::Statement(Stmt::Default),
        BlockItem::Statement(Stmt::Expr(Expr::typed_unpositioned(
            ExprKind::Assign {
                op: AssignOp::Assign,
                target: Box::new(Expr::var_typed(x_sym, int_type)),
                value: Box::new(Expr::int(0, &ctx.types)),
            },
            int_type,
        ))),
    ]);

    let switch_stmt = Stmt::Switch {
        expr: Expr::var_typed(x_sym, int_type),
        body: Box::new(switch_body),
    };

    let func = FunctionDef {
        return_type: ctx.types.void_id,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: int_type,
        }],
        body: switch_stmt,
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
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
        return_type: ctx.types.void_id,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: int_type,
        }],
        body: do_while,
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
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
        then_stmt: Box::new(Stmt::Break),
        else_stmt: None,
    };
    let body = Stmt::Block(vec![
        BlockItem::Statement(assign),
        BlockItem::Statement(if_break),
    ]);

    let do_while = Stmt::DoWhile {
        body: Box::new(body),
        cond: Expr::int(1, &ctx.types),
    };

    let func = FunctionDef {
        return_type: ctx.types.void_id,
        name: test_id,
        params: vec![
            Parameter {
                symbol: Some(x_sym),
                typ: int_type,
            },
            Parameter {
                symbol: Some(cond_sym),
                typ: int_type,
            },
        ],
        body: do_while,
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
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
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let end_id = ctx.str("end");
    let int_type = ctx.int_type();
    let x_sym = ctx.var("x", int_type);

    // Block: { goto end; x = 1; end: x = 2; return x; }
    let body = Stmt::Block(vec![
        BlockItem::Statement(Stmt::Goto(end_id)),
        BlockItem::Statement(Stmt::Expr(Expr::typed_unpositioned(
            ExprKind::Assign {
                op: AssignOp::Assign,
                target: Box::new(Expr::var_typed(x_sym, int_type)),
                value: Box::new(Expr::int(1, &ctx.types)),
            },
            int_type,
        ))),
        BlockItem::Statement(Stmt::Label {
            name: end_id,
            stmt: Box::new(Stmt::Expr(Expr::typed_unpositioned(
                ExprKind::Assign {
                    op: AssignOp::Assign,
                    target: Box::new(Expr::var_typed(x_sym, int_type)),
                    value: Box::new(Expr::int(2, &ctx.types)),
                },
                int_type,
            ))),
        }),
        BlockItem::Statement(Stmt::Return(Some(Expr::var_typed(x_sym, int_type)))),
    ]);

    let func = FunctionDef {
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: int_type,
        }],
        body,
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
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
        then_stmt: Box::new(Stmt::Goto(loop_id)),
        else_stmt: None,
    };

    let body = Stmt::Block(vec![
        BlockItem::Statement(Stmt::Label {
            name: loop_id,
            stmt: Box::new(increment),
        }),
        BlockItem::Statement(if_goto),
        BlockItem::Statement(Stmt::Return(Some(Expr::var_typed(x_sym, int_type)))),
    ]);

    let func = FunctionDef {
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: int_type,
        }],
        body,
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
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
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let x_sym = ctx.var("x", int_type);

    // Inner loop: while(1) { break; }
    let inner_loop = Stmt::While {
        cond: Expr::int(1, &ctx.types),
        body: Box::new(Stmt::Break),
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
        BlockItem::Statement(inner_loop),
        BlockItem::Statement(assign),
        BlockItem::Statement(Stmt::Break),
    ]);

    let outer_loop = Stmt::While {
        cond: Expr::int(1, &ctx.types),
        body: Box::new(outer_body),
    };

    let func = FunctionDef {
        return_type: ctx.types.void_id,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: int_type,
        }],
        body: outer_loop,
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
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
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let cond1_sym = ctx.var("cond1", int_type);
    let cond2_sym = ctx.var("cond2", int_type);
    let x_sym = ctx.var("x", int_type);

    // Inner loop: while(cond2) { continue; }
    let inner_loop = Stmt::While {
        cond: Expr::var_typed(cond2_sym, int_type),
        body: Box::new(Stmt::Continue),
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
        BlockItem::Statement(inner_loop),
        BlockItem::Statement(assign),
    ]);

    let outer_loop = Stmt::While {
        cond: Expr::var_typed(cond1_sym, int_type),
        body: Box::new(outer_body),
    };

    let func = FunctionDef {
        return_type: ctx.types.void_id,
        name: test_id,
        params: vec![
            Parameter {
                symbol: Some(cond1_sym),
                typ: int_type,
            },
            Parameter {
                symbol: Some(cond2_sym),
                typ: int_type,
            },
            Parameter {
                symbol: Some(x_sym),
                typ: int_type,
            },
        ],
        body: outer_loop,
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
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
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: int_type,
        }],
        body: Stmt::Return(Some(not_expr)),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
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
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: int_type,
        }],
        body: Stmt::Return(Some(not_expr)),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
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
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: int_type,
        }],
        body: Stmt::Return(Some(neg_expr)),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
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
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: int_type,
        }],
        body: Stmt::Return(Some(inc_expr)),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
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
        return_type: int_ptr_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(p_sym),
            typ: int_ptr_type,
        }],
        body: Stmt::Return(Some(add_expr)),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
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
        return_type: long_type,
        name: test_id,
        params: vec![
            Parameter {
                symbol: Some(p_sym),
                typ: int_ptr_type,
            },
            Parameter {
                symbol: Some(q_sym),
                typ: int_ptr_type,
            },
        ],
        body: Stmt::Return(Some(diff_expr)),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
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
        return_type: double_type,
        name: test_id,
        params: vec![
            Parameter {
                symbol: Some(a_sym),
                typ: double_type,
            },
            Parameter {
                symbol: Some(b_sym),
                typ: double_type,
            },
        ],
        body: Stmt::Return(Some(add_expr)),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
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
        return_type: int_type,
        name: test_id,
        params: vec![
            Parameter {
                symbol: Some(a_sym),
                typ: double_type,
            },
            Parameter {
                symbol: Some(b_sym),
                typ: double_type,
            },
        ],
        body: Stmt::Return(Some(cmp_expr)),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
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
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: double_type,
        }],
        body: Stmt::Return(Some(cast_expr)),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
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
        return_type: double_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: int_type,
        }],
        body: Stmt::Return(Some(cast_expr)),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
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
    let types = TypeTable::new(&Target::host());
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
    let types = TypeTable::new(&Target::host());
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
    let types = TypeTable::new(&Target::host());
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
    let types = TypeTable::new(&Target::host());
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
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let i_sym = ctx.var("i", int_type);
    // for (int i = 0; i < 10; i++) { }
    let i_var = Expr::var_typed(i_sym, int_type);
    let func = make_simple_func(
        test_id,
        Stmt::For {
            init: Some(ForInit::Declaration(Declaration {
                declarators: vec![crate::parse::ast::InitDeclarator {
                    symbol: i_sym,
                    typ: int_type,
                    storage_class: crate::types::TypeModifiers::empty(),
                    init: Some(Expr::int(0, &ctx.types)),
                    vla_sizes: vec![],
                    explicit_align: None,
                }],
            })),
            cond: Some(Expr::binary(
                BinaryOp::Lt,
                i_var.clone(),
                Expr::int(10, &ctx.types),
                &ctx.types,
            )),
            post: Some(Expr::typed(
                ExprKind::PostInc(Box::new(i_var)),
                int_type,
                test_pos(),
            )),
            body: Box::new(Stmt::Empty),
        },
        &ctx.types,
    );
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
    assert!(module.functions[0].blocks.len() >= 4); // entry, cond, body, post, exit
}

#[test]
fn test_linearize_binary_expr() {
    let mut strings = StringTable::new();
    let types = TypeTable::new(&Target::host());
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
    let mut ctx = TestContext::new();
    let add_id = ctx.str("add");
    let int_type = ctx.int_type();
    let a_sym = ctx.var("a", int_type);
    let b_sym = ctx.var("b", int_type);
    let func = FunctionDef {
        return_type: int_type,
        name: add_id,
        params: vec![
            Parameter {
                symbol: Some(a_sym),
                typ: int_type,
            },
            Parameter {
                symbol: Some(b_sym),
                typ: int_type,
            },
        ],
        body: Stmt::Return(Some(Expr::binary(
            BinaryOp::Add,
            Expr::var_typed(a_sym, int_type),
            Expr::var_typed(b_sym, int_type),
            &ctx.types,
        ))),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
    let ir = format!("{}", module);
    assert!(ir.contains("add"));
    assert!(ir.contains("%a"));
    assert!(ir.contains("%b"));
}

#[test]
fn test_linearize_call() {
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    // Create a function type for foo
    let func_type = ctx
        .types
        .intern(crate::types::Type::function(int_type, vec![], false, false));
    let foo_sym = ctx.var("foo", func_type);
    let func = make_simple_func(
        test_id,
        Stmt::Return(Some(Expr::call(
            Expr::var(foo_sym),
            vec![Expr::int(1, &ctx.types), Expr::int(2, &ctx.types)],
            &ctx.types,
        ))),
        &ctx.types,
    );
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
    let ir = format!("{}", module);
    assert!(ir.contains("call"));
    assert!(ir.contains("foo"));
}

#[test]
fn test_linearize_comparison() {
    let mut strings = StringTable::new();
    let types = TypeTable::new(&Target::host());
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
    let types = TypeTable::new(&Target::host());
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
    let types = TypeTable::new(&Target::host());
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
    let types = TypeTable::new(&Target::host());

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
    let types = TypeTable::new(&Target::host());

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
fn linearize_no_ssa(
    tu: &TranslationUnit,
    types: &TypeTable,
    strings: &StringTable,
    symbols: &SymbolTable,
) -> Module {
    let target = Target::host();
    let mut linearizer = Linearizer::new_no_ssa(symbols, types, strings, &target);
    linearizer.linearize(tu)
}

#[test]
fn test_local_var_emits_load_store() {
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let x_sym = ctx.var("x", int_type);
    // int test() { int x = 1; return x; }
    let func = FunctionDef {
        return_type: int_type,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(Declaration {
                declarators: vec![crate::parse::ast::InitDeclarator {
                    symbol: x_sym,
                    typ: int_type,
                    storage_class: crate::types::TypeModifiers::empty(),
                    init: Some(Expr::int(1, &ctx.types)),
                    vla_sizes: vec![],
                    explicit_align: None,
                }],
            }),
            BlockItem::Statement(Stmt::Return(Some(Expr::var_typed(x_sym, int_type)))),
        ]),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    // Without SSA, should have store and load
    let module = linearize_no_ssa(&tu, &ctx.types, &ctx.strings, &ctx.symbols);
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
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let cond_sym = ctx.var("cond", int_type);
    let x_sym = ctx.var("x", int_type);
    // int test(int cond) {
    //     int x = 1;
    //     if (cond) x = 2;
    //     return x;
    // }

    let func = FunctionDef {
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(cond_sym),
            typ: int_type,
        }],
        body: Stmt::Block(vec![
            // int x = 1;
            BlockItem::Declaration(Declaration {
                declarators: vec![crate::parse::ast::InitDeclarator {
                    symbol: x_sym,
                    typ: int_type,
                    storage_class: crate::types::TypeModifiers::empty(),
                    init: Some(Expr::int(1, &ctx.types)),
                    vla_sizes: vec![],
                    explicit_align: None,
                }],
            }),
            // if (cond) x = 2;
            BlockItem::Statement(Stmt::If {
                cond: Expr::var_typed(cond_sym, int_type),
                then_stmt: Box::new(Stmt::Expr(Expr::assign(
                    Expr::var_typed(x_sym, int_type),
                    Expr::int(2, &ctx.types),
                    &ctx.types,
                ))),
                else_stmt: None,
            }),
            // return x;
            BlockItem::Statement(Stmt::Return(Some(Expr::var_typed(x_sym, int_type)))),
        ]),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    // With SSA, should have phi node at merge point
    let module = ctx.linearize(&tu);
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
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let i_sym = ctx.var("i", int_type);
    // int test() {
    //     int i = 0;
    //     while (i < 10) { i = i + 1; }
    //     return i;
    // }

    let i_var = || Expr::var_typed(i_sym, int_type);

    let func = FunctionDef {
        return_type: int_type,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            // int i = 0;
            BlockItem::Declaration(Declaration {
                declarators: vec![crate::parse::ast::InitDeclarator {
                    symbol: i_sym,
                    typ: int_type,
                    storage_class: crate::types::TypeModifiers::empty(),
                    init: Some(Expr::int(0, &ctx.types)),
                    vla_sizes: vec![],
                    explicit_align: None,
                }],
            }),
            // while (i < 10) { i = i + 1; }
            BlockItem::Statement(Stmt::While {
                cond: Expr::binary(BinaryOp::Lt, i_var(), Expr::int(10, &ctx.types), &ctx.types),
                body: Box::new(Stmt::Expr(Expr::assign(
                    i_var(),
                    Expr::binary(BinaryOp::Add, i_var(), Expr::int(1, &ctx.types), &ctx.types),
                    &ctx.types,
                ))),
            }),
            // return i;
            BlockItem::Statement(Stmt::Return(Some(i_var()))),
        ]),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    // With SSA, should have phi node at loop header
    let module = ctx.linearize(&tu);
    let ir = format!("{}", module);

    // Loop should have a phi at the condition block
    assert!(ir.contains("phi"), "Loop should have phi node: {}", ir);
}

#[test]
fn test_short_circuit_and() {
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let a_sym = ctx.var("a", int_type);
    let b_sym = ctx.var("b", int_type);
    // int test(int a, int b) {
    //     return a && b;
    // }
    // Short-circuit: if a is false, don't evaluate b

    let func = FunctionDef {
        return_type: int_type,
        name: test_id,
        params: vec![
            Parameter {
                symbol: Some(a_sym),
                typ: int_type,
            },
            Parameter {
                symbol: Some(b_sym),
                typ: int_type,
            },
        ],
        body: Stmt::Return(Some(Expr::binary(
            BinaryOp::LogAnd,
            Expr::var_typed(a_sym, int_type),
            Expr::var_typed(b_sym, int_type),
            &ctx.types,
        ))),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
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
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let a_sym = ctx.var("a", int_type);
    let b_sym = ctx.var("b", int_type);
    // int test(int a, int b) {
    //     return a || b;
    // }
    // Short-circuit: if a is true, don't evaluate b

    let func = FunctionDef {
        return_type: int_type,
        name: test_id,
        params: vec![
            Parameter {
                symbol: Some(a_sym),
                typ: int_type,
            },
            Parameter {
                symbol: Some(b_sym),
                typ: int_type,
            },
        ],
        body: Stmt::Return(Some(Expr::binary(
            BinaryOp::LogOr,
            Expr::var_typed(a_sym, int_type),
            Expr::var_typed(b_sym, int_type),
            &ctx.types,
        ))),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
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
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let a_sym = ctx.var("a", int_type);
    let b_sym = ctx.var("b", int_type);
    // int test(int a, int b) {
    //     return a > b ? a : b;  // pure ternary -> should use select
    // }

    // Build ternary: (a > b) ? a : b
    let cond = Expr::binary(
        BinaryOp::Gt,
        Expr::var_typed(a_sym, int_type),
        Expr::var_typed(b_sym, int_type),
        &ctx.types,
    );
    let ternary = Expr::typed(
        ExprKind::Conditional {
            cond: Box::new(cond),
            then_expr: Box::new(Expr::var_typed(a_sym, int_type)),
            else_expr: Box::new(Expr::var_typed(b_sym, int_type)),
        },
        int_type,
        test_pos(),
    );

    let func = FunctionDef {
        return_type: int_type,
        name: test_id,
        params: vec![
            Parameter {
                symbol: Some(a_sym),
                typ: int_type,
            },
            Parameter {
                symbol: Some(b_sym),
                typ: int_type,
            },
        ],
        body: Stmt::Return(Some(ternary)),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
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
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let a_sym = ctx.var("a", int_type);
    let foo_sym = ctx.var("foo", int_type);
    let bar_sym = ctx.var("bar", int_type);
    // int test(int a) {
    //     return a ? foo() : bar();  // impure ternary -> should use phi
    // }

    // Build ternary with function calls (impure)
    let foo_call = Expr::typed(
        ExprKind::Call {
            func: Box::new(Expr::var_typed(foo_sym, int_type)),
            args: vec![],
        },
        int_type,
        test_pos(),
    );
    let bar_call = Expr::typed(
        ExprKind::Call {
            func: Box::new(Expr::var_typed(bar_sym, int_type)),
            args: vec![],
        },
        int_type,
        test_pos(),
    );
    let ternary = Expr::typed(
        ExprKind::Conditional {
            cond: Box::new(Expr::var_typed(a_sym, int_type)),
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
            symbol: Some(a_sym),
            typ: int_type,
        }],
        body: Stmt::Return(Some(ternary)),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
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
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let a_sym = ctx.var("a", int_type);
    let b_sym = ctx.var("b", int_type);
    // int test(int a, int b) {
    //     return a ? (b = 1) : (b = 2);  // assignment is impure
    // }

    // Build ternary with assignments (impure)
    let assign1 = Expr::typed(
        ExprKind::Assign {
            op: AssignOp::Assign,
            target: Box::new(Expr::var_typed(b_sym, int_type)),
            value: Box::new(Expr::int(1, &ctx.types)),
        },
        int_type,
        test_pos(),
    );
    let assign2 = Expr::typed(
        ExprKind::Assign {
            op: AssignOp::Assign,
            target: Box::new(Expr::var_typed(b_sym, int_type)),
            value: Box::new(Expr::int(2, &ctx.types)),
        },
        int_type,
        test_pos(),
    );
    let ternary = Expr::typed(
        ExprKind::Conditional {
            cond: Box::new(Expr::var_typed(a_sym, int_type)),
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
                symbol: Some(a_sym),
                typ: int_type,
            },
            Parameter {
                symbol: Some(b_sym),
                typ: int_type,
            },
        ],
        body: Stmt::Return(Some(ternary)),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
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
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let a_sym = ctx.var("a", int_type);
    let b_sym = ctx.var("b", int_type);
    // int test(int a, int b) {
    //     return a ? b++ : b--;  // post-inc/dec is impure
    // }

    // Build ternary with post-inc/dec (impure)
    let post_inc = Expr::typed(
        ExprKind::PostInc(Box::new(Expr::var_typed(b_sym, int_type))),
        int_type,
        test_pos(),
    );
    let post_dec = Expr::typed(
        ExprKind::PostDec(Box::new(Expr::var_typed(b_sym, int_type))),
        int_type,
        test_pos(),
    );
    let ternary = Expr::typed(
        ExprKind::Conditional {
            cond: Box::new(Expr::var_typed(a_sym, int_type)),
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
                symbol: Some(a_sym),
                typ: int_type,
            },
            Parameter {
                symbol: Some(b_sym),
                typ: int_type,
            },
        ],
        body: Stmt::Return(Some(ternary)),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
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

// ============================================================================
// String literal initialization tests
// ============================================================================

#[test]
fn test_string_literal_char_array_init() {
    // Test that `char arr[6] = "hello";` generates store instructions for each byte
    // plus null terminator (6 stores total: 'h', 'e', 'l', 'l', 'o', '\0')
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");

    // Create char[6] type
    let char_arr_type = ctx.types.intern(Type::array(ctx.types.char_id, 6));
    let arr_sym = ctx.var("arr", char_arr_type);

    // Function: int test() { char arr[6] = "hello"; return 0; }
    let func = FunctionDef {
        return_type: ctx.types.int_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(Declaration {
                declarators: vec![InitDeclarator {
                    symbol: arr_sym,
                    typ: char_arr_type,
                    storage_class: crate::types::TypeModifiers::empty(),
                    init: Some(Expr::typed(
                        ExprKind::StringLit("hello".to_string()),
                        ctx.types.char_ptr_id,
                        test_pos(),
                    )),
                    vla_sizes: vec![],
                    explicit_align: None,
                }],
            }),
            BlockItem::Statement(Stmt::Return(Some(Expr::int(0, &ctx.types)))),
        ]),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
    let ir = format!("{}", module);

    // Should have 6 store instructions (5 chars + null terminator)
    let store_count = ir.matches("store").count();
    assert!(
        store_count >= 6,
        "Expected at least 6 store instructions for 'hello' + null, got {}: {}",
        store_count,
        ir
    );
}

#[test]
fn test_string_literal_char_pointer_init() {
    // Test that `char *p = "hello";` generates a single store of the string address
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let p_sym = ctx.var("p", ctx.types.char_ptr_id);

    // Function: int test() { char *p = "hello"; return 0; }
    let func = FunctionDef {
        return_type: ctx.types.int_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(Declaration {
                declarators: vec![InitDeclarator {
                    symbol: p_sym,
                    typ: ctx.types.char_ptr_id,
                    storage_class: crate::types::TypeModifiers::empty(),
                    init: Some(Expr::typed(
                        ExprKind::StringLit("hello".to_string()),
                        ctx.types.char_ptr_id,
                        test_pos(),
                    )),
                    vla_sizes: vec![],
                    explicit_align: None,
                }],
            }),
            BlockItem::Statement(Stmt::Return(Some(Expr::int(0, &ctx.types)))),
        ]),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
    let ir = format!("{}", module);

    // Should have a store instruction for the pointer (storing the string address)
    assert!(
        ir.contains("store"),
        "Pointer init should have a store instruction: {}",
        ir
    );

    // The module should contain the string literal (strings are stored in module)
    assert!(
        !module.strings.is_empty(),
        "Module should contain string literal: {}",
        ir
    );
}

// ============================================================================
// Incomplete struct type resolution test (regression test for forward declarations)
// Tests the fix in resolve_struct_type() that resolves incomplete struct types
// to their complete definitions when processing initializers.
// ============================================================================

/// Helper to linearize with a custom symbol table (for testing struct resolution)
fn test_linearize_with_symbols(
    tu: &TranslationUnit,
    symbols: &SymbolTable,
    types: &TypeTable,
    strings: &StringTable,
) -> Module {
    let target = Target::host();
    linearize(tu, symbols, types, strings, &target, false)
}

#[test]
fn test_incomplete_struct_type_resolution() {
    // This test verifies that when a typedef refers to an incomplete struct,
    // the linearizer correctly resolves it to the complete struct definition
    // when processing struct initializers.
    //
    // Pattern being tested:
    //   typedef struct foo foo_t;  // incomplete at this point
    //   struct foo { int x; int y; };  // complete definition
    //   foo_t f = {1, 2};  // should use complete struct's size (8 bytes), not 0
    //
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let foo_tag = ctx.str("foo");
    let x_id = ctx.str("x");
    let y_id = ctx.str("y");
    let int_type = ctx.int_type();

    // Create the complete struct type: struct foo { int x; int y; }
    let complete_composite = CompositeType {
        tag: Some(foo_tag),
        members: vec![
            StructMember {
                name: x_id,
                typ: int_type,
                offset: 0,
                bit_offset: None,
                bit_width: None,
                storage_unit_size: None,
                explicit_align: None,
            },
            StructMember {
                name: y_id,
                typ: int_type,
                offset: 4, // Second int at offset 4 bytes
                bit_offset: None,
                bit_width: None,
                storage_unit_size: None,
                explicit_align: None,
            },
        ],
        enum_constants: vec![],
        size: 8,  // 2 ints = 8 bytes
        align: 4, // int alignment
        is_complete: true,
    };
    let complete_struct_type = ctx.types.intern(Type::struct_type(complete_composite));

    // Register the complete struct in the symbol table as a tag
    ctx.symbols
        .declare(Symbol::tag(foo_tag, complete_struct_type, 0))
        .expect("Failed to declare tag");

    // Verify the symbol table is correctly set up
    let looked_up = ctx.symbols.lookup_tag(foo_tag);
    assert!(
        looked_up.is_some(),
        "Symbol table should contain tag for 'foo'"
    );
    assert_eq!(
        looked_up.unwrap().typ,
        complete_struct_type,
        "Tag should point to complete struct type"
    );

    // Create an incomplete struct type with the same tag
    // This simulates what happens with: typedef struct foo foo_t; (before struct foo is defined)
    let incomplete_composite = CompositeType::incomplete(Some(foo_tag));
    let incomplete_struct_type = ctx.types.intern(Type::struct_type(incomplete_composite));

    // Create a symbol for the local variable
    let f_sym = ctx.var("f", incomplete_struct_type);

    // Verify the incomplete type has size 0 before resolution
    assert_eq!(
        ctx.types.size_bytes(incomplete_struct_type),
        0,
        "Incomplete struct should have size 0"
    );

    // Create an initializer list: {1, 2}
    let init_list = Expr::typed_unpositioned(
        ExprKind::InitList {
            elements: vec![
                InitElement {
                    designators: vec![],
                    value: Box::new(Expr::int(1, &ctx.types)),
                },
                InitElement {
                    designators: vec![],
                    value: Box::new(Expr::int(2, &ctx.types)),
                },
            ],
        },
        incomplete_struct_type,
    );

    // Create function: void test() { foo_t f = {1, 2}; }
    // Using the incomplete struct type for the declaration
    let func = FunctionDef {
        return_type: ctx.types.void_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![BlockItem::Declaration(Declaration {
            declarators: vec![InitDeclarator {
                symbol: f_sym,
                typ: incomplete_struct_type,
                storage_class: crate::types::TypeModifiers::empty(),
                init: Some(init_list),
                vla_sizes: vec![],
                explicit_align: None,
            }],
        })]),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    // Linearize with the symbol table that has the complete struct registered
    let module = test_linearize_with_symbols(&tu, &ctx.symbols, &ctx.types, &ctx.strings);
    let ir = format!("{}", module);

    // The IR should show stores to the struct fields at proper offsets
    // Without the fix, the initializer would have total_size=0 and generate no stores
    assert!(
        ir.contains("store"),
        "Struct initializer should generate store instructions. \
         This would fail if incomplete struct type was not resolved. IR:\n{}",
        ir
    );

    // Should have at least 2 stores (one for each field: x and y)
    let store_count = ir.matches("store").count();
    assert!(
        store_count >= 2,
        "Should have at least 2 stores for struct fields (x, y), got {}: {}",
        store_count,
        ir
    );
}

// ============================================================================
// Static local variable increment/decrement regression tests
// These test the fix for pre/post increment/decrement on static locals
// Bug: was storing to sentinel value (u32::MAX) instead of looking up global name
// ============================================================================

use crate::types::TypeModifiers;

#[test]
fn test_static_local_pre_increment() {
    // Test: static int counter = 0; return ++counter;
    // Regression: pre-increment on static locals should store to the global symbol,
    // not to the sentinel value (u32::MAX)
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");

    // Create static int type
    let static_int_type = ctx.types.intern(Type::with_modifiers(
        crate::types::TypeKind::Int,
        TypeModifiers::STATIC,
    ));
    let counter_sym = ctx.var("counter", static_int_type);

    // Create declaration: static int counter = 0;
    let decl = Declaration {
        declarators: vec![InitDeclarator {
            symbol: counter_sym,
            typ: static_int_type,
            storage_class: crate::types::TypeModifiers::empty(),
            init: Some(Expr::int(0, &ctx.types)),
            vla_sizes: vec![],
            explicit_align: None,
        }],
    };

    // Create pre-increment expression: ++counter
    let inc_expr = Expr::typed_unpositioned(
        ExprKind::Unary {
            op: UnaryOp::PreInc,
            operand: Box::new(Expr::var_typed(counter_sym, static_int_type)),
        },
        static_int_type,
    );

    // Function: int test() { static int counter = 0; return ++counter; }
    let func = FunctionDef {
        return_type: ctx.types.int_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(decl),
            BlockItem::Statement(Stmt::Return(Some(inc_expr))),
        ]),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };

    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
    let ir = format!("{}", module);

    // The IR should NOT contain the sentinel value %4294967295
    assert!(
        !ir.contains("%4294967295"),
        "Static local pre-increment should NOT use sentinel pseudo (u32::MAX). IR:\n{}",
        ir
    );

    // Should have a store instruction (storing back to the static variable)
    assert!(
        ir.contains("store"),
        "Static local pre-increment should generate store. IR:\n{}",
        ir
    );

    // Should have a global symbol reference (test.counter.0)
    assert!(
        ir.contains("test.counter"),
        "Static local should use global name 'test.counter'. IR:\n{}",
        ir
    );
}

#[test]
fn test_static_local_pre_decrement() {
    // Test: static int counter = 10; return --counter;
    // Regression: pre-decrement on static locals should store to the global symbol
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");

    let static_int_type = ctx.types.intern(Type::with_modifiers(
        crate::types::TypeKind::Int,
        TypeModifiers::STATIC,
    ));
    let counter_sym = ctx.var("counter", static_int_type);

    let decl = Declaration {
        declarators: vec![InitDeclarator {
            symbol: counter_sym,
            typ: static_int_type,
            storage_class: crate::types::TypeModifiers::empty(),
            init: Some(Expr::int(10, &ctx.types)),
            vla_sizes: vec![],
            explicit_align: None,
        }],
    };

    let dec_expr = Expr::typed_unpositioned(
        ExprKind::Unary {
            op: UnaryOp::PreDec,
            operand: Box::new(Expr::var_typed(counter_sym, static_int_type)),
        },
        static_int_type,
    );

    let func = FunctionDef {
        return_type: ctx.types.int_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(decl),
            BlockItem::Statement(Stmt::Return(Some(dec_expr))),
        ]),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };

    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
    let ir = format!("{}", module);

    assert!(
        !ir.contains("%4294967295"),
        "Static local pre-decrement should NOT use sentinel pseudo (u32::MAX). IR:\n{}",
        ir
    );
    assert!(
        ir.contains("store"),
        "Static local pre-decrement should generate store. IR:\n{}",
        ir
    );
    assert!(
        ir.contains("test.counter"),
        "Static local should use global name 'test.counter'. IR:\n{}",
        ir
    );
}

#[test]
fn test_static_local_post_increment() {
    // Test: static int counter = 0; return counter++;
    // Regression: post-increment on static locals should store to the global symbol
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");

    let static_int_type = ctx.types.intern(Type::with_modifiers(
        crate::types::TypeKind::Int,
        TypeModifiers::STATIC,
    ));
    let counter_sym = ctx.var("counter", static_int_type);

    let decl = Declaration {
        declarators: vec![InitDeclarator {
            symbol: counter_sym,
            typ: static_int_type,
            storage_class: crate::types::TypeModifiers::empty(),
            init: Some(Expr::int(0, &ctx.types)),
            vla_sizes: vec![],
            explicit_align: None,
        }],
    };

    // Post-increment is ExprKind::PostInc, not UnaryOp
    let inc_expr = Expr::typed_unpositioned(
        ExprKind::PostInc(Box::new(Expr::var_typed(counter_sym, static_int_type))),
        static_int_type,
    );

    let func = FunctionDef {
        return_type: ctx.types.int_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(decl),
            BlockItem::Statement(Stmt::Return(Some(inc_expr))),
        ]),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };

    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
    let ir = format!("{}", module);

    assert!(
        !ir.contains("%4294967295"),
        "Static local post-increment should NOT use sentinel pseudo (u32::MAX). IR:\n{}",
        ir
    );
    assert!(
        ir.contains("store"),
        "Static local post-increment should generate store. IR:\n{}",
        ir
    );
    assert!(
        ir.contains("test.counter"),
        "Static local should use global name 'test.counter'. IR:\n{}",
        ir
    );
}

#[test]
fn test_static_local_post_decrement() {
    // Test: static int counter = 10; return counter--;
    // Regression: post-decrement on static locals should store to the global symbol
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");

    let static_int_type = ctx.types.intern(Type::with_modifiers(
        crate::types::TypeKind::Int,
        TypeModifiers::STATIC,
    ));
    let counter_sym = ctx.var("counter", static_int_type);

    let decl = Declaration {
        declarators: vec![InitDeclarator {
            symbol: counter_sym,
            typ: static_int_type,
            storage_class: crate::types::TypeModifiers::empty(),
            init: Some(Expr::int(10, &ctx.types)),
            vla_sizes: vec![],
            explicit_align: None,
        }],
    };

    // Post-decrement is ExprKind::PostDec, not UnaryOp
    let dec_expr = Expr::typed_unpositioned(
        ExprKind::PostDec(Box::new(Expr::var_typed(counter_sym, static_int_type))),
        static_int_type,
    );

    let func = FunctionDef {
        return_type: ctx.types.int_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(decl),
            BlockItem::Statement(Stmt::Return(Some(dec_expr))),
        ]),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };

    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
    let ir = format!("{}", module);

    assert!(
        !ir.contains("%4294967295"),
        "Static local post-decrement should NOT use sentinel pseudo (u32::MAX). IR:\n{}",
        ir
    );
    assert!(
        ir.contains("store"),
        "Static local post-decrement should generate store. IR:\n{}",
        ir
    );
    assert!(
        ir.contains("test.counter"),
        "Static local should use global name 'test.counter'. IR:\n{}",
        ir
    );
}

#[test]
fn test_static_local_compound_assignment() {
    // Test: static int sum = 0; sum += 5; return sum;
    // Verifies compound assignment on static locals uses proper global symbol
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");

    let static_int_type = ctx.types.intern(Type::with_modifiers(
        crate::types::TypeKind::Int,
        TypeModifiers::STATIC,
    ));
    let sum_sym = ctx.var("sum", static_int_type);

    let decl = Declaration {
        declarators: vec![InitDeclarator {
            symbol: sum_sym,
            typ: static_int_type,
            storage_class: crate::types::TypeModifiers::empty(),
            init: Some(Expr::int(0, &ctx.types)),
            vla_sizes: vec![],
            explicit_align: None,
        }],
    };

    // sum += 5
    let compound_assign = Expr::typed_unpositioned(
        ExprKind::Assign {
            op: AssignOp::AddAssign,
            target: Box::new(Expr::var_typed(sum_sym, static_int_type)),
            value: Box::new(Expr::int(5, &ctx.types)),
        },
        static_int_type,
    );

    let func = FunctionDef {
        return_type: ctx.types.int_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(decl),
            BlockItem::Statement(Stmt::Expr(compound_assign)),
            BlockItem::Statement(Stmt::Return(Some(Expr::var_typed(
                sum_sym,
                static_int_type,
            )))),
        ]),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };

    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
    let ir = format!("{}", module);

    assert!(
        !ir.contains("%4294967295"),
        "Static local compound assignment should NOT use sentinel pseudo. IR:\n{}",
        ir
    );
    assert!(
        ir.contains("store"),
        "Static local compound assignment should generate store. IR:\n{}",
        ir
    );
    assert!(
        ir.contains("test.sum"),
        "Static local should use global name 'test.sum'. IR:\n{}",
        ir
    );
}

// ============================================================================
// Wide string literal tests
// ============================================================================

#[test]
fn test_wide_string_literal_expression() {
    // Test: return L"hello";
    // Wide string literal should create a .LWC label and emit wide string data
    let mut strings = StringTable::new();
    let mut types = TypeTable::new(&Target::host());
    let test_id = strings.intern("test");

    // wchar_t* is int* on this platform
    let wchar_ptr_type = types.intern(Type::pointer(types.int_id));

    // Function: wchar_t* test() { return L"hello"; }
    let func = FunctionDef {
        return_type: wchar_ptr_type,
        name: test_id,
        params: vec![],
        body: Stmt::Return(Some(Expr::typed_unpositioned(
            ExprKind::WideStringLit("hello".to_string()),
            wchar_ptr_type,
        ))),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };

    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);

    // Check that wide string was added to the module
    assert!(
        !module.wide_strings.is_empty(),
        "Wide string literal should be added to module.wide_strings"
    );

    // Check label format
    let (label, content) = &module.wide_strings[0];
    assert!(
        label.starts_with(".LWC"),
        "Wide string label should start with .LWC, got: {}",
        label
    );
    assert_eq!(content, "hello", "Wide string content should match");
}

#[test]
fn test_wide_string_literal_is_pure() {
    // Test that WideStringLit is considered a pure expression
    // This is important for ternary optimization
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let cond_sym = ctx.var("cond", int_type);

    let wchar_ptr_type = ctx.types.intern(Type::pointer(int_type));

    // Function: wchar_t* test(int cond) { return cond ? L"yes" : L"no"; }
    // Both branches are pure, so this should use select instead of phi
    let ternary = Expr::typed_unpositioned(
        ExprKind::Conditional {
            cond: Box::new(Expr::var_typed(cond_sym, int_type)),
            then_expr: Box::new(Expr::typed_unpositioned(
                ExprKind::WideStringLit("yes".to_string()),
                wchar_ptr_type,
            )),
            else_expr: Box::new(Expr::typed_unpositioned(
                ExprKind::WideStringLit("no".to_string()),
                wchar_ptr_type,
            )),
        },
        wchar_ptr_type,
    );

    let func = FunctionDef {
        return_type: wchar_ptr_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(cond_sym),
            typ: int_type,
        }],
        body: Stmt::Return(Some(ternary)),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };

    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
    let ir = format!("{}", module);

    // Wide strings are pure, so ternary should use select (sel in IR)
    assert!(
        ir.contains("sel."),
        "Ternary with pure wide string branches should use select. IR:\n{}",
        ir
    );
}

// ============================================================================
// __FUNCTION__ and __PRETTY_FUNCTION__ tests
// ============================================================================

#[test]
fn test_gcc_function_identifier() {
    // Test: __FUNCTION__ should behave like __func__
    // Returns the current function name as a string
    let mut ctx = TestContext::new();
    let my_func_id = ctx.str("my_func");

    // Function: const char* my_func() { return __FUNCTION__; }
    // Uses FuncName which handles __func__, __FUNCTION__, __PRETTY_FUNCTION__
    let func = FunctionDef {
        return_type: ctx.types.char_ptr_id,
        name: my_func_id,
        params: vec![],
        body: Stmt::Return(Some(Expr::typed_unpositioned(
            ExprKind::FuncName,
            ctx.types.char_ptr_id,
        ))),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };

    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);

    // Check that a string containing the function name was added
    let has_func_name = module
        .strings
        .iter()
        .any(|(_, content)| content == "my_func");
    assert!(
        has_func_name,
        "__FUNCTION__ should add function name as string literal. strings: {:?}",
        module.strings
    );
}

#[test]
fn test_gcc_pretty_function_identifier() {
    // Test: __PRETTY_FUNCTION__ should behave like __func__
    let mut ctx = TestContext::new();
    let another_func_id = ctx.str("another_func");

    // Function: const char* another_func() { return __PRETTY_FUNCTION__; }
    // Uses FuncName which handles __func__, __FUNCTION__, __PRETTY_FUNCTION__
    let func = FunctionDef {
        return_type: ctx.types.char_ptr_id,
        name: another_func_id,
        params: vec![],
        body: Stmt::Return(Some(Expr::typed_unpositioned(
            ExprKind::FuncName,
            ctx.types.char_ptr_id,
        ))),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };

    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);

    // Check that a string containing the function name was added
    let has_func_name = module
        .strings
        .iter()
        .any(|(_, content)| content == "another_func");
    assert!(
        has_func_name,
        "__PRETTY_FUNCTION__ should add function name as string literal. strings: {:?}",
        module.strings
    );
}

// ============================================================================
// Static local address in initializer tests
// ============================================================================

#[test]
fn test_static_local_address_in_initializer() {
    // Test: static int x = 0; static int *p = &x;
    // The address of a static local should use the mangled global name
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");

    let static_int_type = ctx.types.intern(Type::with_modifiers(
        crate::types::TypeKind::Int,
        TypeModifiers::STATIC,
    ));
    let x_sym = ctx.var("x", static_int_type);

    let static_int_ptr_type = ctx.types.intern(Type {
        kind: crate::types::TypeKind::Pointer,
        modifiers: TypeModifiers::STATIC,
        base: Some(ctx.types.int_id),
        array_size: None,
        params: None,
        variadic: false,
        noreturn: false,
        composite: None,
    });
    let p_sym = ctx.var("p", static_int_ptr_type);

    // static int x = 0;
    let x_decl = Declaration {
        declarators: vec![InitDeclarator {
            symbol: x_sym,
            typ: static_int_type,
            storage_class: crate::types::TypeModifiers::empty(),
            init: Some(Expr::int(0, &ctx.types)),
            vla_sizes: vec![],
            explicit_align: None,
        }],
    };

    // static int *p = &x;
    let addr_of_x = Expr::typed_unpositioned(
        ExprKind::Unary {
            op: UnaryOp::AddrOf,
            operand: Box::new(Expr::var_typed(x_sym, static_int_type)),
        },
        static_int_ptr_type,
    );

    let p_decl = Declaration {
        declarators: vec![InitDeclarator {
            symbol: p_sym,
            typ: static_int_ptr_type,
            storage_class: crate::types::TypeModifiers::empty(),
            init: Some(addr_of_x),
            vla_sizes: vec![],
            explicit_align: None,
        }],
    };

    // Function body with both declarations
    let func = FunctionDef {
        return_type: ctx.types.int_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(x_decl),
            BlockItem::Declaration(p_decl),
            BlockItem::Statement(Stmt::Return(Some(Expr::int(0, &ctx.types)))),
        ]),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };

    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);

    // Check that p's initializer references the mangled name of x
    let p_global = module.globals.iter().find(|g| g.name.contains("test.p"));
    assert!(
        p_global.is_some(),
        "Should have a global for static local 'p'. globals: {:?}",
        module.globals
    );

    // The initializer for p should be a SymAddr pointing to the mangled x name
    if let Some(global) = p_global {
        if let Initializer::SymAddr(sym_name) = &global.init {
            assert!(
                sym_name.contains("test.x"),
                "Address of static local x should use mangled name 'test.x', got: {}",
                sym_name
            );
        } else {
            panic!(
                "Static pointer initializer should be SymAddr, got: {:?}",
                global.init
            );
        }
    }
}

// ============================================================================
// Struct/union dereference tests
// ============================================================================

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
            storage_unit_size: None,
            explicit_align: None,
        }],
        enum_constants: vec![],
        size: 4,
        align: 4,
        is_complete: true,
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
        return_type: struct_type_id,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(p_sym),
            typ: struct_ptr_type_id,
        }],
        body: Stmt::Return(Some(deref_p)),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };

    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
    let ir = format!("{}", module);

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

// ============================================================================
// Tests for src_typ field on conversion instructions
// ============================================================================

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
        return_type: double_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: int_type,
        }],
        body: Stmt::Return(Some(cast_expr)),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
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
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: double_type,
        }],
        body: Stmt::Return(Some(cast_expr)),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
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
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: char_type,
        }],
        body: Stmt::Return(Some(cast_expr)),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
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

// ========================================================================
// Float16 (_Float16) conversion tests
// ========================================================================

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
        return_type: float_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: float16_type,
        }],
        body: Stmt::Return(Some(cast_expr)),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };
    let module = ctx.linearize(&tu);

    // Find call to __extendhfsf2
    let func = &module.functions[0];
    let has_rtlib_call = func.blocks.iter().any(|bb| {
        bb.insns.iter().any(|insn| {
            insn.op == Opcode::Call && insn.func_name.as_deref() == Some("__extendhfsf2")
        })
    });
    assert!(
        has_rtlib_call,
        "Float16 to float conversion should call __extendhfsf2"
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
        return_type: float16_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: float_type,
        }],
        body: Stmt::Return(Some(cast_expr)),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };
    let module = ctx.linearize(&tu);

    // Find call to __truncsfhf2
    let func = &module.functions[0];
    let has_rtlib_call = func.blocks.iter().any(|bb| {
        bb.insns.iter().any(|insn| {
            insn.op == Opcode::Call && insn.func_name.as_deref() == Some("__truncsfhf2")
        })
    });
    assert!(
        has_rtlib_call,
        "Float to Float16 conversion should call __truncsfhf2"
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
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: float16_type,
        }],
        body: Stmt::Return(Some(cast_expr)),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };
    let module = ctx.linearize(&tu);

    // Find call to __fixhfsi
    let func = &module.functions[0];
    let has_rtlib_call = func.blocks.iter().any(|bb| {
        bb.insns
            .iter()
            .any(|insn| insn.op == Opcode::Call && insn.func_name.as_deref() == Some("__fixhfsi"))
    });
    assert!(
        has_rtlib_call,
        "Float16 to int conversion should call __fixhfsi"
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
        return_type: float16_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: int_type,
        }],
        body: Stmt::Return(Some(cast_expr)),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };
    let module = ctx.linearize(&tu);

    // Find call to __floatsihf
    let func = &module.functions[0];
    let has_rtlib_call = func.blocks.iter().any(|bb| {
        bb.insns
            .iter()
            .any(|insn| insn.op == Opcode::Call && insn.func_name.as_deref() == Some("__floatsihf"))
    });
    assert!(
        has_rtlib_call,
        "Int to Float16 conversion should call __floatsihf"
    );
}

// ============================================================================
// C11 _Alignof tests
// ============================================================================

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
        return_type: ctx.types.ulong_id,
        name: test_id,
        params: vec![],
        body: Stmt::Return(Some(alignof_expr)),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
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
        return_type: ctx.types.ulong_id,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: int_type,
        }],
        body: Stmt::Return(Some(alignof_expr)),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
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

// ============================================================================
// Frame/Return address builtin tests
// ============================================================================

#[test]
fn test_frame_address_emits_opcode() {
    // Test: return __builtin_frame_address(0);
    // Should emit FrameAddress opcode
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let void_ptr = ctx.types.void_ptr_id;

    let level_expr = Expr::typed_unpositioned(ExprKind::IntLit(0), ctx.types.int_id);
    let frame_addr_expr = Expr::typed_unpositioned(
        ExprKind::FrameAddress {
            level: Box::new(level_expr),
        },
        void_ptr,
    );

    let func = FunctionDef {
        return_type: void_ptr,
        name: test_id,
        params: vec![],
        body: Stmt::Return(Some(frame_addr_expr)),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };
    let module = ctx.linearize(&tu);

    let func = &module.functions[0];
    let has_frame_addr = func
        .blocks
        .iter()
        .any(|bb| bb.insns.iter().any(|insn| insn.op == Opcode::FrameAddress));
    assert!(
        has_frame_addr,
        "__builtin_frame_address should emit FrameAddress opcode"
    );
}

#[test]
fn test_return_address_emits_opcode() {
    // Test: return __builtin_return_address(0);
    // Should emit ReturnAddress opcode
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let void_ptr = ctx.types.void_ptr_id;

    let level_expr = Expr::typed_unpositioned(ExprKind::IntLit(0), ctx.types.int_id);
    let return_addr_expr = Expr::typed_unpositioned(
        ExprKind::ReturnAddress {
            level: Box::new(level_expr),
        },
        void_ptr,
    );

    let func = FunctionDef {
        return_type: void_ptr,
        name: test_id,
        params: vec![],
        body: Stmt::Return(Some(return_addr_expr)),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };
    let module = ctx.linearize(&tu);

    let func = &module.functions[0];
    let has_return_addr = func
        .blocks
        .iter()
        .any(|bb| bb.insns.iter().any(|insn| insn.op == Opcode::ReturnAddress));
    assert!(
        has_return_addr,
        "__builtin_return_address should emit ReturnAddress opcode"
    );
}

// ============================================================================
// Mixed designated + positional initializer field tracking
// Regression test: positional fields after a designator must use the correct
// field index (one past the designated field), not the element's enumeration index.
// Bug: {.b = 20, 30, 40} stored 30 at offset 4 (b) instead of offset 8 (c).
// ============================================================================

#[test]
fn test_mixed_designated_positional_struct_init() {
    // Test: struct S { int a; int b; int c; int d; };
    //       struct S s = {.b = 20, 30, 40};
    // Expected stores: offset 4 = 20 (b), offset 8 = 30 (c), offset 12 = 40 (d)
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();

    // Create field name StringIds
    let a_id = ctx.str("a");
    let b_id = ctx.str("b");
    let c_id = ctx.str("c");
    let d_id = ctx.str("d");

    // Create struct S { int a; int b; int c; int d; }
    let struct_composite = CompositeType {
        tag: None,
        members: vec![
            StructMember {
                name: a_id,
                typ: int_type,
                offset: 0,
                bit_offset: None,
                bit_width: None,
                storage_unit_size: None,
                explicit_align: None,
            },
            StructMember {
                name: b_id,
                typ: int_type,
                offset: 4,
                bit_offset: None,
                bit_width: None,
                storage_unit_size: None,
                explicit_align: None,
            },
            StructMember {
                name: c_id,
                typ: int_type,
                offset: 8,
                bit_offset: None,
                bit_width: None,
                storage_unit_size: None,
                explicit_align: None,
            },
            StructMember {
                name: d_id,
                typ: int_type,
                offset: 12,
                bit_offset: None,
                bit_width: None,
                storage_unit_size: None,
                explicit_align: None,
            },
        ],
        enum_constants: vec![],
        size: 16,
        align: 4,
        is_complete: true,
    };
    let struct_type = ctx.types.intern(Type::struct_type(struct_composite));
    let s_sym = ctx.var("s", struct_type);

    // Create init list: {.b = 20, 30, 40}
    let init_list = Expr::typed_unpositioned(
        ExprKind::InitList {
            elements: vec![
                InitElement {
                    designators: vec![Designator::Field(b_id)],
                    value: Box::new(Expr::int(20, &ctx.types)),
                },
                InitElement {
                    designators: vec![],
                    value: Box::new(Expr::int(30, &ctx.types)),
                },
                InitElement {
                    designators: vec![],
                    value: Box::new(Expr::int(40, &ctx.types)),
                },
            ],
        },
        struct_type,
    );

    // Function: int test() { struct S s = {.b = 20, 30, 40}; return 0; }
    let func = FunctionDef {
        return_type: ctx.types.int_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(Declaration {
                declarators: vec![InitDeclarator {
                    symbol: s_sym,
                    typ: struct_type,
                    storage_class: crate::types::TypeModifiers::empty(),
                    init: Some(init_list),
                    vla_sizes: vec![],
                    explicit_align: None,
                }],
            }),
            BlockItem::Statement(Stmt::Return(Some(Expr::int(0, &ctx.types)))),
        ]),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
    let ir = format!("{}", module);

    // Should have 3 stores for the 3 initialized fields (b, c, d)
    let store_count = ir.matches("store").count();
    assert!(
        store_count >= 3,
        "Expected at least 3 stores for .b=20, c=30, d=40, got {}: {}",
        store_count,
        ir
    );

    // Verify stores go to distinct offsets (not the same offset twice)
    // The bug caused 20 and 30 to both be stored at offset +4
    // Extract all "+ N" store offsets from the IR
    let store_lines: Vec<&str> = ir.lines().filter(|l| l.contains("store")).collect();

    // Count unique offsets among store instructions
    let mut offsets: Vec<&str> = store_lines
        .iter()
        .filter_map(|line| line.find("+ ").map(|pos| &line[pos..]))
        .collect();
    offsets.sort();
    offsets.dedup();

    // With the fix, we should have 3 distinct offsets (4, 8, 12)
    // Without the fix, offset 4 appears twice and we'd only have 2 unique offsets
    assert!(
        offsets.len() >= 3,
        "Expected 3 distinct store offsets for fields b(+4), c(+8), d(+12), \
         got {} unique offsets {:?}. IR:\n{}",
        offsets.len(),
        offsets,
        ir
    );
}

#[test]
fn test_mixed_designated_positional_array_init() {
    // Test: int arr[5] = {[2] = 20, 30, 40};
    // Expected stores: index 2 = 20, index 3 = 30, index 4 = 40
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();

    let arr_type = ctx.types.intern(Type::array(int_type, 5));
    let arr_sym = ctx.var("arr", arr_type);

    // Create init list: {[2] = 20, 30, 40}
    let init_list = Expr::typed_unpositioned(
        ExprKind::InitList {
            elements: vec![
                InitElement {
                    designators: vec![Designator::Index(2)],
                    value: Box::new(Expr::int(20, &ctx.types)),
                },
                InitElement {
                    designators: vec![],
                    value: Box::new(Expr::int(30, &ctx.types)),
                },
                InitElement {
                    designators: vec![],
                    value: Box::new(Expr::int(40, &ctx.types)),
                },
            ],
        },
        arr_type,
    );

    // Function: int test() { int arr[5] = {[2] = 20, 30, 40}; return 0; }
    let func = FunctionDef {
        return_type: ctx.types.int_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(Declaration {
                declarators: vec![InitDeclarator {
                    symbol: arr_sym,
                    typ: arr_type,
                    storage_class: crate::types::TypeModifiers::empty(),
                    init: Some(init_list),
                    vla_sizes: vec![],
                    explicit_align: None,
                }],
            }),
            BlockItem::Statement(Stmt::Return(Some(Expr::int(0, &ctx.types)))),
        ]),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
    let ir = format!("{}", module);

    // Should have 3 stores for the 3 initialized elements
    let store_count = ir.matches("store").count();
    assert!(
        store_count >= 3,
        "Expected at least 3 stores for [2]=20, [3]=30, [4]=40, got {}: {}",
        store_count,
        ir
    );

    // Verify stores go to distinct offsets
    let store_lines: Vec<&str> = ir.lines().filter(|l| l.contains("store")).collect();
    let mut offsets: Vec<&str> = store_lines
        .iter()
        .filter_map(|line| line.find("+ ").map(|pos| &line[pos..]))
        .collect();
    offsets.sort();
    offsets.dedup();

    // With the fix: 3 distinct offsets (8, 12, 16 for indices 2, 3, 4)
    // Without the fix: offset 8 appears twice (indices 2 and "1" via enumerate)
    assert!(
        offsets.len() >= 3,
        "Expected 3 distinct store offsets for arr[2](+8), arr[3](+12), arr[4](+16), \
         got {} unique offsets {:?}. IR:\n{}",
        offsets.len(),
        offsets,
        ir
    );
}

#[test]
fn test_designator_chain_nested_struct_init() {
    // struct { struct { int x; int y; } pt; int z; } s = { .pt.x = 10, .pt.y = 20, .z = 30 };
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");

    let x_id = ctx.str("x");
    let y_id = ctx.str("y");
    let pt_members = vec![
        StructMember {
            name: x_id,
            typ: ctx.int_type(),
            offset: 0,
            bit_offset: None,
            bit_width: None,
            storage_unit_size: None,
            explicit_align: None,
        },
        StructMember {
            name: y_id,
            typ: ctx.int_type(),
            offset: 4,
            bit_offset: None,
            bit_width: None,
            storage_unit_size: None,
            explicit_align: None,
        },
    ];
    let pt_type = ctx.types.intern(Type::struct_type(CompositeType {
        tag: None,
        members: pt_members,
        enum_constants: vec![],
        size: 8,
        align: 4,
        is_complete: true,
    }));

    let pt_id = ctx.str("pt");
    let z_id = ctx.str("z");
    let outer_members = vec![
        StructMember {
            name: pt_id,
            typ: pt_type,
            offset: 0,
            bit_offset: None,
            bit_width: None,
            storage_unit_size: None,
            explicit_align: None,
        },
        StructMember {
            name: z_id,
            typ: ctx.int_type(),
            offset: 8,
            bit_offset: None,
            bit_width: None,
            storage_unit_size: None,
            explicit_align: None,
        },
    ];
    let outer_type = ctx.types.intern(Type::struct_type(CompositeType {
        tag: None,
        members: outer_members,
        enum_constants: vec![],
        size: 12,
        align: 4,
        is_complete: true,
    }));
    let outer_sym = ctx.var("s", outer_type);

    let init_list = Expr::typed_unpositioned(
        ExprKind::InitList {
            elements: vec![
                InitElement {
                    designators: vec![Designator::Field(pt_id), Designator::Field(x_id)],
                    value: Box::new(Expr::int(10, &ctx.types)),
                },
                InitElement {
                    designators: vec![Designator::Field(pt_id), Designator::Field(y_id)],
                    value: Box::new(Expr::int(20, &ctx.types)),
                },
                InitElement {
                    designators: vec![Designator::Field(z_id)],
                    value: Box::new(Expr::int(30, &ctx.types)),
                },
            ],
        },
        outer_type,
    );

    let func = FunctionDef {
        return_type: ctx.types.int_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(Declaration {
                declarators: vec![InitDeclarator {
                    symbol: outer_sym,
                    typ: outer_type,
                    storage_class: crate::types::TypeModifiers::empty(),
                    init: Some(init_list),
                    vla_sizes: vec![],
                    explicit_align: None,
                }],
            }),
            BlockItem::Statement(Stmt::Return(Some(Expr::int(0, &ctx.types)))),
        ]),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };

    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };
    let module = ctx.linearize(&tu);
    let ir = format!("{}", module);
    let store_count = ir.matches("store").count();
    assert!(
        store_count >= 3,
        "Expected stores for nested designators, got {}: {}",
        store_count,
        ir
    );
}

#[test]
fn test_designator_chain_array_member_init() {
    // struct { int arr[3]; } s = { .arr[1] = 42 };
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let arr_type = ctx.types.intern(Type::array(int_type, 3));
    let arr_id = ctx.str("arr");
    let members = vec![StructMember {
        name: arr_id,
        typ: arr_type,
        offset: 0,
        bit_offset: None,
        bit_width: None,
        storage_unit_size: None,
        explicit_align: None,
    }];
    let struct_type = ctx.types.intern(Type::struct_type(CompositeType {
        tag: None,
        members,
        enum_constants: vec![],
        size: 12,
        align: 4,
        is_complete: true,
    }));
    let s_sym = ctx.var("s", struct_type);

    let init_list = Expr::typed_unpositioned(
        ExprKind::InitList {
            elements: vec![InitElement {
                designators: vec![Designator::Field(arr_id), Designator::Index(1)],
                value: Box::new(Expr::int(42, &ctx.types)),
            }],
        },
        struct_type,
    );

    let func = FunctionDef {
        return_type: ctx.types.int_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(Declaration {
                declarators: vec![InitDeclarator {
                    symbol: s_sym,
                    typ: struct_type,
                    storage_class: crate::types::TypeModifiers::empty(),
                    init: Some(init_list),
                    vla_sizes: vec![],
                    explicit_align: None,
                }],
            }),
            BlockItem::Statement(Stmt::Return(Some(Expr::int(0, &ctx.types)))),
        ]),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };

    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };
    let module = ctx.linearize(&tu);
    let ir = format!("{}", module);
    let store_count = ir.matches("store").count();
    assert!(
        store_count >= 1,
        "Expected store for array member designator, got {}: {}",
        store_count,
        ir
    );
}

#[test]
fn test_repeated_designator_last_wins_array() {
    // int arr[2] = {[0] = 1, [0] = 2}; should store only last value
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let arr_type = ctx.types.intern(Type::array(int_type, 2));
    let arr_sym = ctx.var("arr", arr_type);

    let init_list = Expr::typed_unpositioned(
        ExprKind::InitList {
            elements: vec![
                InitElement {
                    designators: vec![Designator::Index(0)],
                    value: Box::new(Expr::int(1, &ctx.types)),
                },
                InitElement {
                    designators: vec![Designator::Index(0)],
                    value: Box::new(Expr::int(2, &ctx.types)),
                },
            ],
        },
        arr_type,
    );

    let func = FunctionDef {
        return_type: ctx.types.int_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(Declaration {
                declarators: vec![InitDeclarator {
                    symbol: arr_sym,
                    typ: arr_type,
                    storage_class: crate::types::TypeModifiers::empty(),
                    init: Some(init_list),
                    vla_sizes: vec![],
                    explicit_align: None,
                }],
            }),
            BlockItem::Statement(Stmt::Return(Some(Expr::int(0, &ctx.types)))),
        ]),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };

    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };
    let module = ctx.linearize(&tu);
    let ir = format!("{}", module);
    let store_count = ir.matches("store").count();
    assert!(
        store_count >= 1 && store_count <= 2,
        "Expected one store for repeated designator, got {}: {}",
        store_count,
        ir
    );
}

#[test]
fn test_skip_unnamed_bitfield_positional_init() {
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let a_id = ctx.str("a");
    let b_id = ctx.str("b");
    let members = vec![
        StructMember {
            name: a_id,
            typ: int_type,
            offset: 0,
            bit_offset: None,
            bit_width: None,
            storage_unit_size: None,
            explicit_align: None,
        },
        StructMember {
            name: StringId::EMPTY,
            typ: int_type,
            offset: 4,
            bit_offset: Some(0),
            bit_width: Some(8),
            storage_unit_size: Some(4),
            explicit_align: None,
        },
        StructMember {
            name: b_id,
            typ: int_type,
            offset: 8,
            bit_offset: None,
            bit_width: None,
            storage_unit_size: None,
            explicit_align: None,
        },
    ];
    let struct_type = ctx.types.intern(Type::struct_type(CompositeType {
        tag: None,
        members,
        enum_constants: vec![],
        size: 12,
        align: 4,
        is_complete: true,
    }));
    let s_sym = ctx.var("s", struct_type);

    let init_list = Expr::typed_unpositioned(
        ExprKind::InitList {
            elements: vec![
                InitElement {
                    designators: vec![],
                    value: Box::new(Expr::int(10, &ctx.types)),
                },
                InitElement {
                    designators: vec![],
                    value: Box::new(Expr::int(20, &ctx.types)),
                },
            ],
        },
        struct_type,
    );

    let func = FunctionDef {
        return_type: ctx.types.int_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(Declaration {
                declarators: vec![InitDeclarator {
                    symbol: s_sym,
                    typ: struct_type,
                    storage_class: crate::types::TypeModifiers::empty(),
                    init: Some(init_list),
                    vla_sizes: vec![],
                    explicit_align: None,
                }],
            }),
            BlockItem::Statement(Stmt::Return(Some(Expr::int(0, &ctx.types)))),
        ]),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };

    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };
    let module = ctx.linearize(&tu);
    let ir = format!("{}", module);
    let store_count = ir.matches("store").count();
    assert!(
        store_count >= 2,
        "Expected stores for named fields a/b, got {}: {}",
        store_count,
        ir
    );
}

#[test]
fn test_union_first_named_member_positional_init() {
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let a_id = ctx.str("a");
    let members = vec![
        StructMember {
            name: StringId::EMPTY,
            typ: int_type,
            offset: 0,
            bit_offset: Some(0),
            bit_width: Some(16),
            storage_unit_size: Some(4),
            explicit_align: None,
        },
        StructMember {
            name: a_id,
            typ: int_type,
            offset: 0,
            bit_offset: None,
            bit_width: None,
            storage_unit_size: None,
            explicit_align: None,
        },
    ];
    let union_type = ctx.types.intern(Type::union_type(CompositeType {
        tag: None,
        members,
        enum_constants: vec![],
        size: 4,
        align: 4,
        is_complete: true,
    }));
    let u_sym = ctx.var("u", union_type);

    let init_list = Expr::typed_unpositioned(
        ExprKind::InitList {
            elements: vec![InitElement {
                designators: vec![],
                value: Box::new(Expr::int(42, &ctx.types)),
            }],
        },
        union_type,
    );

    let func = FunctionDef {
        return_type: ctx.types.int_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(Declaration {
                declarators: vec![InitDeclarator {
                    symbol: u_sym,
                    typ: union_type,
                    storage_class: crate::types::TypeModifiers::empty(),
                    init: Some(init_list),
                    vla_sizes: vec![],
                    explicit_align: None,
                }],
            }),
            BlockItem::Statement(Stmt::Return(Some(Expr::int(0, &ctx.types)))),
        ]),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
    };

    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };
    let module = ctx.linearize(&tu);
    let ir = format!("{}", module);
    let store_count = ir.matches("store").count();
    assert!(
        store_count >= 1,
        "Expected store for union first named member, got {}: {}",
        store_count,
        ir
    );
}
