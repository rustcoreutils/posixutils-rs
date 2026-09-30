//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Additional unit tests for linearize.rs
//

// Allow approximate float constants in tests - we're testing literal parsing, not using PI
#![allow(clippy::approx_constant)]

use super::*;
use crate::parse::ast::{
    AsmOperand, AssignOp, BinaryOp, BlockItem, Declaration, Designator, ExprKind, ExternalDecl,
    ForInit, FunctionDef, InitDeclarator, InitElement, ParamStyle, Parameter, Stmt, UnaryOp,
};
use crate::strings::StringTable;
use crate::symbol::Symbol;
use crate::target::{Arch, Os, Target};
use crate::types::{CompositeType, StructMember, Type, TypeTable};

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
            true,
        )
    }
}

fn test_linearize(tu: &TranslationUnit, types: &TypeTable, strings: &StringTable) -> Module {
    let symbols = SymbolTable::new();
    let target = Target::host();
    linearize(tu, &symbols, types, strings, &target, false, true)
}

fn make_simple_func(name: StringId, body: Stmt, types: &TypeTable) -> FunctionDef {
    FunctionDef {
        attrs: Default::default(),
        return_type: types.int_id,
        name,
        params: vec![],
        body,
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
        param_style: ParamStyle::Prototype,
    }
}

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
        attrs: Default::default(),
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(x_sym),
            typ: int_type,
            vm_dims: vec![],
            discarded_dims: vec![],
        }],
        body: Stmt::Return(Some(Expr::var_typed(x_sym, int_type))),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
        param_style: ParamStyle::Prototype,
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    // Observed before SSA conversion: the store into the parameter's local is the linearizer's job; SSA then promotes it away.
    let module = linearize_no_ssa(&tu, &ctx.types, &ctx.strings, &ctx.symbols);
    let ir = format!("{}", module.display(&ctx.types));

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
            vm_dims: vec![],
            discarded_dims: vec![],
        })
        .collect();

    // Return a + h (first and last params)
    let func = FunctionDef {
        attrs: Default::default(),
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
        param_style: ParamStyle::Prototype,
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
    let ir = format!("{}", module.display(&ctx.types));

    // The IR should contain references to both parameters
    assert!(ir.contains("%a"), "IR should have first param: {}", ir);
    assert!(ir.contains("%h"), "IR should have last param: {}", ir);
    // Should have add operation for a + h
    assert!(ir.contains("add"), "IR should have add for a + h: {}", ir);
}

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

// C99 6.7.8p14: a scalar initializes the first array element.

#[test]
fn test_simple_array_element_store() {
    // Simpler test: verify we can store to a specific array element
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
        attrs: Default::default(),
        return_type: ctx.types.void_id,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(arr_sym),
            typ: arr_ptr_type,
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

    // Should have a store for the array element assignment
    assert!(
        ir.contains("store"),
        "Array element assignment should produce store: {}",
        ir
    );
}

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
        BlockItem::Statement(Box::new(Stmt::Case(
            Expr::int(1, &ctx.types),
            None,
            Box::new(Stmt::Return(Some(Expr::int(10, &ctx.types)))),
        ))),
        BlockItem::Statement(Box::new(Stmt::Case(
            Expr::int(2, &ctx.types),
            None,
            Box::new(Stmt::Return(Some(Expr::int(20, &ctx.types)))),
        ))),
        BlockItem::Statement(Box::new(Stmt::Default(
            test_pos(),
            Box::new(Stmt::Return(Some(Expr::int(0, &ctx.types)))),
        ))),
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
        BlockItem::Statement(Box::new(Stmt::Case(
            Expr::int(1, &ctx.types),
            None,
            Box::new(Stmt::Expr(Expr::typed_unpositioned(
                ExprKind::Assign {
                    op: AssignOp::Assign,
                    target: Box::new(Expr::var_typed(x_sym, int_type)),
                    value: Box::new(Expr::int(10, &ctx.types)),
                },
                int_type,
            ))),
        ))),
        BlockItem::Statement(Box::new(Stmt::Break(test_pos()))),
        BlockItem::Statement(Box::new(Stmt::Default(test_pos(), Box::new(Stmt::Empty)))),
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
        BlockItem::Statement(Box::new(Stmt::Label {
            name: end_id,
            stmt: Box::new(Stmt::Expr(Expr::typed_unpositioned(
                ExprKind::Assign {
                    op: AssignOp::Assign,
                    target: Box::new(Expr::var_typed(x_sym, int_type)),
                    value: Box::new(Expr::int(2, &ctx.types)),
                },
                int_type,
            ))),
            pos: test_pos(),
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
        then_stmt: Box::new(Stmt::Goto {
            name: loop_id,
            pos: test_pos(),
        }),
        else_stmt: None,
    };

    let body = Stmt::Block(vec![
        BlockItem::Statement(Box::new(Stmt::Label {
            name: loop_id,
            stmt: Box::new(increment),
            pos: test_pos(),
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
        ir.contains("setval"),
        "Inner continue should not skip x = 1 assignment (setval for const 1): {}",
        ir
    );
}

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
        ir.contains("scvtf"),
        "Int-to-float cast should produce scvtf instruction: {}",
        ir
    );
}

// Core linearization tests (moved from linearize.rs)

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
    let ir = format!("{}", module.display(&types));
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

    // A constant condition jumps straight to the arm it selects, and the
    // other arm is not emitted at all.
    let module = test_linearize(&tu, &types, &strings);
    let ir = format!("{}", module.display(&types));
    assert!(!ir.contains("cbr"), "{ir}");
    let rets = module.functions[0]
        .blocks
        .iter()
        .flat_map(|bb| bb.insns.iter())
        .filter(|i| i.op == Opcode::Ret)
        .count();
    assert_eq!(rets, 1, "only the taken arm's return is emitted\n{ir}");
}

/// The functions `f` in `module` calls, by name.
fn calls_in(module: &Module, f: &str) -> Vec<String> {
    let func = module.functions.iter().find(|x| x.name == f).unwrap();
    func.blocks
        .iter()
        .flat_map(|bb| bb.insns.iter())
        .filter(|i| i.op == Opcode::Call)
        .filter_map(|i| i.func_name.clone())
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

#[test]
fn test_linearize_while() {
    let mut strings = StringTable::new();
    let types = TypeTable::new(&Target::host());
    let test_id = strings.intern("test");
    let func = make_simple_func(
        test_id,
        Stmt::While {
            cond: Expr::int(1, &types),
            body: Box::new(Stmt::Break(test_pos())),
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
                    fn_effect: Default::default(),
                    symbol_attrs: Default::default(),
                    pos: Position::default(),
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
    let ir = format!("{}", module.display(&types));
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
        attrs: Default::default(),
        return_type: int_type,
        name: add_id,
        params: vec![
            Parameter {
                symbol: Some(a_sym),
                typ: int_type,
                vm_dims: vec![],
                discarded_dims: vec![],
            },
            Parameter {
                symbol: Some(b_sym),
                typ: int_type,
                vm_dims: vec![],
                discarded_dims: vec![],
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
        param_style: ParamStyle::Prototype,
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
    let ir = format!("{}", module.display(&ctx.types));
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
    let ir = format!("{}", module.display(&ctx.types));
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
    let ir = format!("{}", module.display(&types));
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
    let ir = format!("{}", module.display(&types));
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
    let ir = format!("{}", module.display(&types));

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
    let mut expr = Expr::new(ExprKind::FloatLit(FloatVal::from_f64(3.14)), test_pos());
    expr.typ = Some(types.double_id);

    let symbols = SymbolTable::new();
    let target = Target::host();
    let linearizer = Linearizer::new(&symbols, &types, &strings, &target);
    let typ = linearizer.expr_type(&expr);
    assert_eq!(types.kind(typ), TypeKind::Double);
}

// SSA Conversion Tests

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
        attrs: Default::default(),
        return_type: int_type,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(Declaration {
                declarators: vec![crate::parse::ast::InitDeclarator {
                    fn_effect: Default::default(),
                    symbol_attrs: Default::default(),
                    pos: Position::default(),
                    symbol: x_sym,
                    typ: int_type,
                    storage_class: crate::types::TypeModifiers::empty(),
                    init: Some(Expr::int(1, &ctx.types)),
                    vla_sizes: vec![],
                    explicit_align: None,
                }],
            }),
            BlockItem::Statement(Box::new(Stmt::Return(Some(Expr::var_typed(
                x_sym, int_type,
            ))))),
        ]),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
        param_style: ParamStyle::Prototype,
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    // Without SSA, should have store and load
    let module = linearize_no_ssa(&tu, &ctx.types, &ctx.strings, &ctx.symbols);
    let ir = format!("{}", module.display(&ctx.types));
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
        attrs: Default::default(),
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(cond_sym),
            typ: int_type,
            vm_dims: vec![],
            discarded_dims: vec![],
        }],
        body: Stmt::Block(vec![
            // int x = 1;
            BlockItem::Declaration(Declaration {
                declarators: vec![crate::parse::ast::InitDeclarator {
                    fn_effect: Default::default(),
                    symbol_attrs: Default::default(),
                    pos: Position::default(),
                    symbol: x_sym,
                    typ: int_type,
                    storage_class: crate::types::TypeModifiers::empty(),
                    init: Some(Expr::int(1, &ctx.types)),
                    vla_sizes: vec![],
                    explicit_align: None,
                }],
            }),
            // if (cond) x = 2;
            BlockItem::Statement(Box::new(Stmt::If {
                cond: Expr::var_typed(cond_sym, int_type),
                then_stmt: Box::new(Stmt::Expr(Expr::assign(
                    Expr::var_typed(x_sym, int_type),
                    Expr::int(2, &ctx.types),
                    &ctx.types,
                ))),
                else_stmt: None,
            })),
            // return x;
            BlockItem::Statement(Box::new(Stmt::Return(Some(Expr::var_typed(
                x_sym, int_type,
            ))))),
        ]),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
        param_style: ParamStyle::Prototype,
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    // With SSA, should have phi node at merge point
    let module = ctx.linearize(&tu);
    let ir = format!("{}", module.display(&ctx.types));

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
        attrs: Default::default(),
        return_type: int_type,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            // int i = 0;
            BlockItem::Declaration(Declaration {
                declarators: vec![crate::parse::ast::InitDeclarator {
                    fn_effect: Default::default(),
                    symbol_attrs: Default::default(),
                    pos: Position::default(),
                    symbol: i_sym,
                    typ: int_type,
                    storage_class: crate::types::TypeModifiers::empty(),
                    init: Some(Expr::int(0, &ctx.types)),
                    vla_sizes: vec![],
                    explicit_align: None,
                }],
            }),
            // while (i < 10) { i = i + 1; }
            BlockItem::Statement(Box::new(Stmt::While {
                cond: Expr::binary(BinaryOp::Lt, i_var(), Expr::int(10, &ctx.types), &ctx.types),
                body: Box::new(Stmt::Expr(Expr::assign(
                    i_var(),
                    Expr::binary(BinaryOp::Add, i_var(), Expr::int(1, &ctx.types), &ctx.types),
                    &ctx.types,
                ))),
            })),
            // return i;
            BlockItem::Statement(Box::new(Stmt::Return(Some(i_var())))),
        ]),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
        param_style: ParamStyle::Prototype,
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    // With SSA, should have phi node at loop header
    let module = ctx.linearize(&tu);
    let ir = format!("{}", module.display(&ctx.types));

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
        attrs: Default::default(),
        return_type: int_type,
        name: test_id,
        params: vec![
            Parameter {
                symbol: Some(a_sym),
                typ: int_type,
                vm_dims: vec![],
                discarded_dims: vec![],
            },
            Parameter {
                symbol: Some(b_sym),
                typ: int_type,
                vm_dims: vec![],
                discarded_dims: vec![],
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
        param_style: ParamStyle::Prototype,
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
    let ir = format!("{}", module.display(&ctx.types));

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
        attrs: Default::default(),
        return_type: int_type,
        name: test_id,
        params: vec![
            Parameter {
                symbol: Some(a_sym),
                typ: int_type,
                vm_dims: vec![],
                discarded_dims: vec![],
            },
            Parameter {
                symbol: Some(b_sym),
                typ: int_type,
                vm_dims: vec![],
                discarded_dims: vec![],
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
        param_style: ParamStyle::Prototype,
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = ctx.linearize(&tu);
    let ir = format!("{}", module.display(&ctx.types));

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
        attrs: Default::default(),
        return_type: int_type,
        name: test_id,
        params: vec![
            Parameter {
                symbol: Some(a_sym),
                typ: int_type,
                vm_dims: vec![],
                discarded_dims: vec![],
            },
            Parameter {
                symbol: Some(b_sym),
                typ: int_type,
                vm_dims: vec![],
                discarded_dims: vec![],
            },
        ],
        body: Stmt::Return(Some(ternary)),
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
            binding: Default::default(),
            known: None,
        },
        int_type,
        test_pos(),
    );
    let bar_call = Expr::typed(
        ExprKind::Call {
            func: Box::new(Expr::var_typed(bar_sym, int_type)),
            args: vec![],
            binding: Default::default(),
            known: None,
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
        attrs: Default::default(),
        return_type: int_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(a_sym),
            typ: int_type,
            vm_dims: vec![],
            discarded_dims: vec![],
        }],
        body: Stmt::Return(Some(ternary)),
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
        attrs: Default::default(),
        return_type: int_type,
        name: test_id,
        params: vec![
            Parameter {
                symbol: Some(a_sym),
                typ: int_type,
                vm_dims: vec![],
                discarded_dims: vec![],
            },
            Parameter {
                symbol: Some(b_sym),
                typ: int_type,
                vm_dims: vec![],
                discarded_dims: vec![],
            },
        ],
        body: Stmt::Return(Some(ternary)),
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
        attrs: Default::default(),
        return_type: int_type,
        name: test_id,
        params: vec![
            Parameter {
                symbol: Some(a_sym),
                typ: int_type,
                vm_dims: vec![],
                discarded_dims: vec![],
            },
            Parameter {
                symbol: Some(b_sym),
                typ: int_type,
                vm_dims: vec![],
                discarded_dims: vec![],
            },
        ],
        body: Stmt::Return(Some(ternary)),
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

// String literal initialization tests

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
        attrs: Default::default(),
        return_type: ctx.types.int_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(Declaration {
                declarators: vec![InitDeclarator {
                    fn_effect: Default::default(),
                    symbol_attrs: Default::default(),
                    pos: Position::default(),
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
            BlockItem::Statement(Box::new(Stmt::Return(Some(Expr::int(0, &ctx.types))))),
        ]),
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
        attrs: Default::default(),
        return_type: ctx.types.int_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(Declaration {
                declarators: vec![InitDeclarator {
                    fn_effect: Default::default(),
                    symbol_attrs: Default::default(),
                    pos: Position::default(),
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
            BlockItem::Statement(Box::new(Stmt::Return(Some(Expr::int(0, &ctx.types))))),
        ]),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
        param_style: ParamStyle::Prototype,
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    // Observed before SSA conversion: the store of the string address is the linearizer's job; SSA then promotes it away.
    let module = linearize_no_ssa(&tu, &ctx.types, &ctx.strings, &ctx.symbols);
    let ir = format!("{}", module.display(&ctx.types));

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

// Incomplete struct type resolution, through a forward declaration.

/// Helper to linearize with a custom symbol table (for testing struct resolution)
fn test_linearize_with_symbols(
    tu: &TranslationUnit,
    symbols: &SymbolTable,
    types: &TypeTable,
    strings: &StringTable,
) -> Module {
    let target = Target::host();
    linearize(tu, symbols, types, strings, &target, false, true)
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
                access_bytes: None,
                explicit_align: None,
            },
            StructMember {
                name: y_id,
                typ: int_type,
                offset: 4, // Second int at offset 4 bytes
                bit_offset: None,
                bit_width: None,
                access_bytes: None,
                explicit_align: None,
            },
        ],
        enum_constants: vec![],
        size: 8,  // 2 ints = 8 bytes
        align: 4, // int alignment
        member_align: 4,
        is_complete: true,
        transparent: false,
        anon_id: None,
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
        attrs: Default::default(),
        return_type: ctx.types.void_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![BlockItem::Declaration(Declaration {
            declarators: vec![InitDeclarator {
                fn_effect: Default::default(),
                symbol_attrs: Default::default(),
                pos: Position::default(),
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
        param_style: ParamStyle::Prototype,
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    // Linearize with the symbol table that has the complete struct registered
    let module = test_linearize_with_symbols(&tu, &ctx.symbols, &ctx.types, &ctx.strings);
    let ir = format!("{}", module.display(&ctx.types));

    // The IR should show stores to the struct fields at proper offsets
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

// Static local variable increment/decrement tests

use crate::types::TypeModifiers;

#[test]
fn test_static_local_pre_increment() {
    // Test: static int counter = 0; return ++counter;
    // Regression: pre-increment on static locals should store to the global symbol,
    // not to the sentinel value (u32::MAX)
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");

    // Create static int type
    // STATIC goes in storage_class, not type modifiers (matches parser behavior)
    let static_int_type = ctx.types.int_id;
    let counter_sym = ctx.var("counter", static_int_type);

    // Create declaration: static int counter = 0;
    let decl = Declaration {
        declarators: vec![InitDeclarator {
            fn_effect: Default::default(),
            symbol_attrs: Default::default(),
            pos: Position::default(),
            symbol: counter_sym,
            typ: static_int_type,
            storage_class: TypeModifiers::STATIC,
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
        attrs: Default::default(),
        return_type: ctx.types.int_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(decl),
            BlockItem::Statement(Box::new(Stmt::Return(Some(inc_expr)))),
        ]),
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

    // STATIC goes in storage_class, not type modifiers (matches parser behavior)
    let static_int_type = ctx.types.int_id;
    let counter_sym = ctx.var("counter", static_int_type);

    let decl = Declaration {
        declarators: vec![InitDeclarator {
            fn_effect: Default::default(),
            symbol_attrs: Default::default(),
            pos: Position::default(),
            symbol: counter_sym,
            typ: static_int_type,
            storage_class: TypeModifiers::STATIC,
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
        attrs: Default::default(),
        return_type: ctx.types.int_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(decl),
            BlockItem::Statement(Box::new(Stmt::Return(Some(dec_expr)))),
        ]),
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

    // STATIC goes in storage_class, not type modifiers (matches parser behavior)
    let static_int_type = ctx.types.int_id;
    let counter_sym = ctx.var("counter", static_int_type);

    let decl = Declaration {
        declarators: vec![InitDeclarator {
            fn_effect: Default::default(),
            symbol_attrs: Default::default(),
            pos: Position::default(),
            symbol: counter_sym,
            typ: static_int_type,
            storage_class: TypeModifiers::STATIC,
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
        attrs: Default::default(),
        return_type: ctx.types.int_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(decl),
            BlockItem::Statement(Box::new(Stmt::Return(Some(inc_expr)))),
        ]),
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

    // STATIC goes in storage_class, not type modifiers (matches parser behavior)
    let static_int_type = ctx.types.int_id;
    let counter_sym = ctx.var("counter", static_int_type);

    let decl = Declaration {
        declarators: vec![InitDeclarator {
            fn_effect: Default::default(),
            symbol_attrs: Default::default(),
            pos: Position::default(),
            symbol: counter_sym,
            typ: static_int_type,
            storage_class: TypeModifiers::STATIC,
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
        attrs: Default::default(),
        return_type: ctx.types.int_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(decl),
            BlockItem::Statement(Box::new(Stmt::Return(Some(dec_expr)))),
        ]),
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

    // STATIC goes in storage_class, not type modifiers (matches parser behavior)
    let static_int_type = ctx.types.int_id;
    let sum_sym = ctx.var("sum", static_int_type);

    let decl = Declaration {
        declarators: vec![InitDeclarator {
            fn_effect: Default::default(),
            symbol_attrs: Default::default(),
            pos: Position::default(),
            symbol: sum_sym,
            typ: static_int_type,
            storage_class: TypeModifiers::STATIC,
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
        attrs: Default::default(),
        return_type: ctx.types.int_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(decl),
            BlockItem::Statement(Box::new(Stmt::Expr(compound_assign))),
            BlockItem::Statement(Box::new(Stmt::Return(Some(Expr::var_typed(
                sum_sym,
                static_int_type,
            ))))),
        ]),
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

// Wide string literal tests

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
        attrs: Default::default(),
        return_type: wchar_ptr_type,
        name: test_id,
        params: vec![],
        body: Stmt::Return(Some(Expr::typed_unpositioned(
            ExprKind::WideStringLit("hello".chars().map(u32::from).collect()),
            wchar_ptr_type,
        ))),
        pos: test_pos(),
        is_static: false,
        is_inline: false,
        calling_conv: crate::abi::CallingConv::default(),
        param_style: ParamStyle::Prototype,
    };

    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };

    let module = test_linearize(&tu, &types, &strings);

    // A wide literal is interned with the other 4-byte-unit literals:
    // wchar_t is 4 bytes on every target, laid out as char32_t is.
    let (label, content) = &module.utf32_strings[0];
    assert!(
        label.starts_with(".LU32C"),
        "4-byte-unit literal label should start with .LU32C, got: {}",
        label
    );
    let hello: Vec<u32> = "hello".chars().map(u32::from).collect();
    assert_eq!(content, &hello, "Wide string content should match");
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
                ExprKind::WideStringLit("yes".chars().map(u32::from).collect()),
                wchar_ptr_type,
            )),
            else_expr: Box::new(Expr::typed_unpositioned(
                ExprKind::WideStringLit("no".chars().map(u32::from).collect()),
                wchar_ptr_type,
            )),
        },
        wchar_ptr_type,
    );

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: wchar_ptr_type,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(cond_sym),
            typ: int_type,
            vm_dims: vec![],
            discarded_dims: vec![],
        }],
        body: Stmt::Return(Some(ternary)),
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

    // Wide strings are pure, so ternary should use select (sel in IR)
    assert!(
        ir.contains("sel."),
        "Ternary with pure wide string branches should use select. IR:\n{}",
        ir
    );
}

// __FUNCTION__ and __PRETTY_FUNCTION__ tests

#[test]
fn test_gcc_function_identifier() {
    // Test: __FUNCTION__ should behave like __func__
    // Returns the current function name as a string
    let mut ctx = TestContext::new();
    let my_func_id = ctx.str("my_func");

    // Function: const char* my_func() { return __FUNCTION__; }
    // Uses FuncName which handles __func__, __FUNCTION__, __PRETTY_FUNCTION__
    let func = FunctionDef {
        attrs: Default::default(),
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
        param_style: ParamStyle::Prototype,
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
        attrs: Default::default(),
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
        param_style: ParamStyle::Prototype,
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

// Static local address in initializer tests

#[test]
fn test_static_local_address_in_initializer() {
    // Test: static int x = 0; static int *p = &x;
    // The address of a static local should use the mangled global name
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");

    // STATIC goes in storage_class, not type modifiers (matches parser behavior)
    let static_int_type = ctx.types.int_id;
    let x_sym = ctx.var("x", static_int_type);

    let static_int_ptr_type = ctx.types.intern(Type {
        kind: crate::types::TypeKind::Pointer,
        base: Some(ctx.types.int_id),
        ..Default::default()
    });
    let p_sym = ctx.var("p", static_int_ptr_type);

    // static int x = 0;
    let x_decl = Declaration {
        declarators: vec![InitDeclarator {
            fn_effect: Default::default(),
            symbol_attrs: Default::default(),
            pos: Position::default(),
            symbol: x_sym,
            typ: static_int_type,
            storage_class: TypeModifiers::STATIC,
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
            fn_effect: Default::default(),
            symbol_attrs: Default::default(),
            pos: Position::default(),
            symbol: p_sym,
            typ: static_int_ptr_type,
            storage_class: TypeModifiers::STATIC,
            init: Some(addr_of_x),
            vla_sizes: vec![],
            explicit_align: None,
        }],
    };

    // Function body with both declarations
    let func = FunctionDef {
        attrs: Default::default(),
        return_type: ctx.types.int_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(x_decl),
            BlockItem::Declaration(p_decl),
            BlockItem::Statement(Box::new(Stmt::Return(Some(Expr::int(0, &ctx.types))))),
        ]),
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
            explicit_align: None,
        }],
        enum_constants: vec![],
        size: 4,
        align: 4,
        member_align: 4,
        is_complete: true,
        transparent: false,
        anon_id: None,
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

// Mixed designated + positional initializer field tracking
// Regression test: positional fields after a designator must use the correct
// field index (one past the designated field), not the element's enumeration index.

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
                access_bytes: None,
                explicit_align: None,
            },
            StructMember {
                name: b_id,
                typ: int_type,
                offset: 4,
                bit_offset: None,
                bit_width: None,
                access_bytes: None,
                explicit_align: None,
            },
            StructMember {
                name: c_id,
                typ: int_type,
                offset: 8,
                bit_offset: None,
                bit_width: None,
                access_bytes: None,
                explicit_align: None,
            },
            StructMember {
                name: d_id,
                typ: int_type,
                offset: 12,
                bit_offset: None,
                bit_width: None,
                access_bytes: None,
                explicit_align: None,
            },
        ],
        enum_constants: vec![],
        size: 16,
        align: 4,
        member_align: 4,
        is_complete: true,
        transparent: false,
        anon_id: None,
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
        attrs: Default::default(),
        return_type: ctx.types.int_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(Declaration {
                declarators: vec![InitDeclarator {
                    fn_effect: Default::default(),
                    symbol_attrs: Default::default(),
                    pos: Position::default(),
                    symbol: s_sym,
                    typ: struct_type,
                    storage_class: crate::types::TypeModifiers::empty(),
                    init: Some(init_list),
                    vla_sizes: vec![],
                    explicit_align: None,
                }],
            }),
            BlockItem::Statement(Box::new(Stmt::Return(Some(Expr::int(0, &ctx.types))))),
        ]),
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

    // Should have 3 stores for the 3 initialized fields (b, c, d)
    let store_count = ir.matches("store").count();
    assert!(
        store_count >= 3,
        "Expected at least 3 stores for .b=20, c=30, d=40, got {}: {}",
        store_count,
        ir
    );

    // Verify stores go to distinct offsets (not the same offset twice)
    // Extract all "+ N" store offsets from the IR
    let store_lines: Vec<&str> = ir.lines().filter(|l| l.contains("store")).collect();

    // Count unique offsets among store instructions
    let mut offsets: Vec<&str> = store_lines
        .iter()
        .filter_map(|line| line.find("+ ").map(|pos| &line[pos..]))
        .collect();
    offsets.sort();
    offsets.dedup();

    // 3 distinct offsets (4, 8, 12)
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
        attrs: Default::default(),
        return_type: ctx.types.int_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(Declaration {
                declarators: vec![InitDeclarator {
                    fn_effect: Default::default(),
                    symbol_attrs: Default::default(),
                    pos: Position::default(),
                    symbol: arr_sym,
                    typ: arr_type,
                    storage_class: crate::types::TypeModifiers::empty(),
                    init: Some(init_list),
                    vla_sizes: vec![],
                    explicit_align: None,
                }],
            }),
            BlockItem::Statement(Box::new(Stmt::Return(Some(Expr::int(0, &ctx.types))))),
        ]),
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

    // 3 distinct offsets (8, 12, 16 for indices 2, 3, 4)
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
            access_bytes: None,
            explicit_align: None,
        },
        StructMember {
            name: y_id,
            typ: ctx.int_type(),
            offset: 4,
            bit_offset: None,
            bit_width: None,
            access_bytes: None,
            explicit_align: None,
        },
    ];
    let pt_type = ctx.types.intern(Type::struct_type(CompositeType {
        tag: None,
        members: pt_members,
        enum_constants: vec![],
        size: 8,
        align: 4,
        member_align: 4,
        is_complete: true,
        transparent: false,
        anon_id: None,
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
            access_bytes: None,
            explicit_align: None,
        },
        StructMember {
            name: z_id,
            typ: ctx.int_type(),
            offset: 8,
            bit_offset: None,
            bit_width: None,
            access_bytes: None,
            explicit_align: None,
        },
    ];
    let outer_type = ctx.types.intern(Type::struct_type(CompositeType {
        tag: None,
        members: outer_members,
        enum_constants: vec![],
        size: 12,
        align: 4,
        member_align: 4,
        is_complete: true,
        transparent: false,
        anon_id: None,
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
        attrs: Default::default(),
        return_type: ctx.types.int_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(Declaration {
                declarators: vec![InitDeclarator {
                    fn_effect: Default::default(),
                    symbol_attrs: Default::default(),
                    pos: Position::default(),
                    symbol: outer_sym,
                    typ: outer_type,
                    storage_class: crate::types::TypeModifiers::empty(),
                    init: Some(init_list),
                    vla_sizes: vec![],
                    explicit_align: None,
                }],
            }),
            BlockItem::Statement(Box::new(Stmt::Return(Some(Expr::int(0, &ctx.types))))),
        ]),
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
        access_bytes: None,
        explicit_align: None,
    }];
    let struct_type = ctx.types.intern(Type::struct_type(CompositeType {
        tag: None,
        members,
        enum_constants: vec![],
        size: 12,
        align: 4,
        member_align: 4,
        is_complete: true,
        transparent: false,
        anon_id: None,
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
        attrs: Default::default(),
        return_type: ctx.types.int_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(Declaration {
                declarators: vec![InitDeclarator {
                    fn_effect: Default::default(),
                    symbol_attrs: Default::default(),
                    pos: Position::default(),
                    symbol: s_sym,
                    typ: struct_type,
                    storage_class: crate::types::TypeModifiers::empty(),
                    init: Some(init_list),
                    vla_sizes: vec![],
                    explicit_align: None,
                }],
            }),
            BlockItem::Statement(Box::new(Stmt::Return(Some(Expr::int(0, &ctx.types))))),
        ]),
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
        attrs: Default::default(),
        return_type: ctx.types.int_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(Declaration {
                declarators: vec![InitDeclarator {
                    fn_effect: Default::default(),
                    symbol_attrs: Default::default(),
                    pos: Position::default(),
                    symbol: arr_sym,
                    typ: arr_type,
                    storage_class: crate::types::TypeModifiers::empty(),
                    init: Some(init_list),
                    vla_sizes: vec![],
                    explicit_align: None,
                }],
            }),
            BlockItem::Statement(Box::new(Stmt::Return(Some(Expr::int(0, &ctx.types))))),
        ]),
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
    let store_count = ir.matches("store").count();
    assert!(
        (1..=2).contains(&store_count),
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
            access_bytes: None,
            explicit_align: None,
        },
        StructMember {
            name: StringId::EMPTY,
            typ: int_type,
            offset: 4,
            bit_offset: Some(0),
            bit_width: Some(8),
            access_bytes: Some(4),
            explicit_align: None,
        },
        StructMember {
            name: b_id,
            typ: int_type,
            offset: 8,
            bit_offset: None,
            bit_width: None,
            access_bytes: None,
            explicit_align: None,
        },
    ];
    let struct_type = ctx.types.intern(Type::struct_type(CompositeType {
        tag: None,
        members,
        enum_constants: vec![],
        size: 12,
        align: 4,
        member_align: 4,
        is_complete: true,
        transparent: false,
        anon_id: None,
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
        attrs: Default::default(),
        return_type: ctx.types.int_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(Declaration {
                declarators: vec![InitDeclarator {
                    fn_effect: Default::default(),
                    symbol_attrs: Default::default(),
                    pos: Position::default(),
                    symbol: s_sym,
                    typ: struct_type,
                    storage_class: crate::types::TypeModifiers::empty(),
                    init: Some(init_list),
                    vla_sizes: vec![],
                    explicit_align: None,
                }],
            }),
            BlockItem::Statement(Box::new(Stmt::Return(Some(Expr::int(0, &ctx.types))))),
        ]),
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
            access_bytes: Some(4),
            explicit_align: None,
        },
        StructMember {
            name: a_id,
            typ: int_type,
            offset: 0,
            bit_offset: None,
            bit_width: None,
            access_bytes: None,
            explicit_align: None,
        },
    ];
    let union_type = ctx.types.intern(Type::union_type(CompositeType {
        tag: None,
        members,
        enum_constants: vec![],
        size: 4,
        align: 4,
        member_align: 4,
        is_complete: true,
        transparent: false,
        anon_id: None,
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
        attrs: Default::default(),
        return_type: ctx.types.int_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(Declaration {
                declarators: vec![InitDeclarator {
                    fn_effect: Default::default(),
                    symbol_attrs: Default::default(),
                    pos: Position::default(),
                    symbol: u_sym,
                    typ: union_type,
                    storage_class: crate::types::TypeModifiers::empty(),
                    init: Some(init_list),
                    vla_sizes: vec![],
                    explicit_align: None,
                }],
            }),
            BlockItem::Statement(Box::new(Stmt::Return(Some(Expr::int(0, &ctx.types))))),
        ]),
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
    let store_count = ir.matches("store").count();
    assert!(
        store_count >= 1,
        "Expected store for union first named member, got {}: {}",
        store_count,
        ir
    );
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

/// Test that multiple bitfields at the same offset are all initialized
/// when using designated initializers (static local case, which uses same path as globals).
#[test]
fn test_bitfield_designated_init_multiple_same_offset() {
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();

    // Create a struct with multiple bitfields sharing the same storage unit
    // Similar to CPython's PyASCIIObject state field
    let kind_id = ctx.str("kind");
    let compact_id = ctx.str("compact");
    let ascii_id = ctx.str("ascii");
    let static_alloc_id = ctx.str("statically_allocated");

    let members = vec![
        StructMember {
            name: kind_id,
            typ: int_type,
            offset: 0,
            bit_offset: Some(0),
            bit_width: Some(3),
            access_bytes: Some(1),
            explicit_align: None,
        },
        StructMember {
            name: compact_id,
            typ: int_type,
            offset: 0,
            bit_offset: Some(3),
            bit_width: Some(1),
            access_bytes: Some(1),
            explicit_align: None,
        },
        StructMember {
            name: ascii_id,
            typ: int_type,
            offset: 0,
            bit_offset: Some(4),
            bit_width: Some(1),
            access_bytes: Some(1),
            explicit_align: None,
        },
        StructMember {
            name: static_alloc_id,
            typ: int_type,
            offset: 0,
            bit_offset: Some(5),
            bit_width: Some(1),
            access_bytes: Some(1),
            explicit_align: None,
        },
    ];

    // STATIC goes in storage_class, not type modifiers (matches parser behavior)
    let struct_type = ctx.types.intern(Type {
        kind: crate::types::TypeKind::Struct,
        composite: Some(Box::new(CompositeType {
            tag: None,
            members,
            enum_constants: vec![],
            size: 1,
            align: 1,
            member_align: 1,
            is_complete: true,
            transparent: false,
            anon_id: None,
        })),
        ..Default::default()
    });

    let s_sym = ctx.var("s", struct_type);

    // Create init list: { .kind = 1, .compact = 1, .ascii = 1, .statically_allocated = 1 }
    let init_list = Expr::typed_unpositioned(
        ExprKind::InitList {
            elements: vec![
                InitElement {
                    designators: vec![Designator::Field(kind_id)],
                    value: Box::new(Expr::int(1, &ctx.types)),
                },
                InitElement {
                    designators: vec![Designator::Field(compact_id)],
                    value: Box::new(Expr::int(1, &ctx.types)),
                },
                InitElement {
                    designators: vec![Designator::Field(ascii_id)],
                    value: Box::new(Expr::int(1, &ctx.types)),
                },
                InitElement {
                    designators: vec![Designator::Field(static_alloc_id)],
                    value: Box::new(Expr::int(1, &ctx.types)),
                },
            ],
        },
        struct_type,
    );

    // Create a function with static local declaration
    let decl = Declaration {
        declarators: vec![InitDeclarator {
            fn_effect: Default::default(),
            symbol_attrs: Default::default(),
            pos: Position::default(),
            symbol: s_sym,
            typ: struct_type,
            storage_class: TypeModifiers::STATIC,
            init: Some(init_list),
            vla_sizes: vec![],
            explicit_align: None,
        }],
    };

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: ctx.types.int_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(decl),
            BlockItem::Statement(Box::new(Stmt::Return(Some(Expr::int(0, &ctx.types))))),
        ]),
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

    // Check that the static local initializer has the correct packed value
    // kind=1 (bits 0-2), compact=1 (bit 3), ascii=1 (bit 4), static_alloc=1 (bit 5)
    // Expected: 1 | (1 << 3) | (1 << 4) | (1 << 5) = 1 + 8 + 16 + 32 = 57
    let global_names: Vec<_> = module.globals.iter().map(|g| &g.name).collect();
    let global = module
        .globals
        .iter()
        .find(|g| g.name.contains("s"))
        .unwrap_or_else(|| panic!("Should have static local 's', found: {:?}", global_names));

    if let crate::ir::Initializer::Struct { fields, .. } = &global.init {
        // Should have one field (the packed bitfield value)
        assert_eq!(fields.len(), 1, "Should pack all bitfields into one field");
        let (offset, size, init) = &fields[0];
        assert_eq!(*offset, 0, "Packed field should be at offset 0");
        assert_eq!(*size, 1, "Storage unit size should be 1 byte");
        if let crate::ir::Initializer::Int(val) = init {
            // All four bitfields set to 1 should give: 1 + 8 + 16 + 32 = 57
            assert_eq!(
                *val, 57,
                "Packed value should be 57 (all four bitfields set)"
            );
        } else {
            panic!("Expected Int initializer, got {:?}", init);
        }
    } else {
        panic!("Expected Struct initializer, got {:?}", global.init);
    }
}

/// Test that multiple bitfields are correctly initialized with read-modify-write
/// when using designated initializers for local variables.
#[test]
fn test_bitfield_designated_init_local_var() {
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();

    // Create a struct with multiple bitfields
    let a_id = ctx.str("a");
    let b_id = ctx.str("b");
    let c_id = ctx.str("c");

    let members = vec![
        StructMember {
            name: a_id,
            typ: int_type,
            offset: 0,
            bit_offset: Some(0),
            bit_width: Some(4),
            access_bytes: Some(1),
            explicit_align: None,
        },
        StructMember {
            name: b_id,
            typ: int_type,
            offset: 0,
            bit_offset: Some(4),
            bit_width: Some(4),
            access_bytes: Some(1),
            explicit_align: None,
        },
        StructMember {
            name: c_id,
            typ: int_type,
            offset: 1,
            bit_offset: Some(0),
            bit_width: Some(8),
            access_bytes: Some(1),
            explicit_align: None,
        },
    ];

    let struct_type = ctx.types.intern(Type::struct_type(CompositeType {
        tag: None,
        members,
        enum_constants: vec![],
        size: 2,
        align: 1,
        member_align: 1,
        is_complete: true,
        transparent: false,
        anon_id: None,
    }));

    let s_sym = ctx.var("s", struct_type);

    // Create init list: { .a = 5, .b = 10, .c = 255 }
    let init_list = Expr::typed_unpositioned(
        ExprKind::InitList {
            elements: vec![
                InitElement {
                    designators: vec![Designator::Field(a_id)],
                    value: Box::new(Expr::int(5, &ctx.types)),
                },
                InitElement {
                    designators: vec![Designator::Field(b_id)],
                    value: Box::new(Expr::int(10, &ctx.types)),
                },
                InitElement {
                    designators: vec![Designator::Field(c_id)],
                    value: Box::new(Expr::int(255, &ctx.types)),
                },
            ],
        },
        struct_type,
    );

    let func = FunctionDef {
        attrs: Default::default(),
        return_type: ctx.types.int_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(Declaration {
                declarators: vec![InitDeclarator {
                    fn_effect: Default::default(),
                    symbol_attrs: Default::default(),
                    pos: Position::default(),
                    symbol: s_sym,
                    typ: struct_type,
                    storage_class: crate::types::TypeModifiers::empty(),
                    init: Some(init_list),
                    vla_sizes: vec![],
                    explicit_align: None,
                }],
            }),
            BlockItem::Statement(Box::new(Stmt::Return(Some(Expr::int(0, &ctx.types))))),
        ]),
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

    // For local variables, each bitfield should use read-modify-write pattern
    // So we should see multiple load and store pairs for the bitfield operations
    // Count the number of load and store operations
    let load_count = ir.matches("load.8").count();
    let store_count = ir.matches("store.8").count();

    // We have 3 bitfields: a, b at offset 0, c at offset 1
    // Each bitfield init should do read-modify-write, so we expect:
    // - At least 3 loads (one per bitfield)
    // - At least 3 stores (one per bitfield)
    // Plus the zero initialization stores
    assert!(
        load_count >= 3,
        "Expected at least 3 loads for bitfield read-modify-write, got {}: {}",
        load_count,
        ir
    );
    assert!(
        store_count >= 3,
        "Expected at least 3 stores for bitfield initialization, got {}: {}",
        store_count,
        ir
    );

    // Also verify the AND operations for masking (part of read-modify-write)
    // In IR format it appears as "and.8" (with type suffix)
    let and_count = ir.matches("and.8").count();
    assert!(
        and_count >= 3,
        "Expected at least 3 AND operations for bitfield masking, got {}: {}",
        and_count,
        ir
    );
}

/// Test that large struct (> 64 bits) copy from array element works correctly.
#[test]
fn test_large_struct_copy_from_array() {
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let ptr_type = ctx.types.void_ptr_id;

    // Create a struct with two pointers (128 bits on 64-bit systems)
    // struct pair { void *ptr; const char *str; };
    let ptr_id = ctx.str("ptr");
    let str_id = ctx.str("str");

    let members = vec![
        StructMember {
            name: ptr_id,
            typ: ptr_type,
            offset: 0,
            bit_offset: None,
            bit_width: None,
            access_bytes: None,
            explicit_align: None,
        },
        StructMember {
            name: str_id,
            typ: ptr_type,
            offset: 8,
            bit_offset: None,
            bit_width: None,
            access_bytes: None,
            explicit_align: None,
        },
    ];

    let struct_type = ctx.types.intern(Type {
        kind: crate::types::TypeKind::Struct,
        composite: Some(Box::new(CompositeType {
            tag: None,
            members,
            enum_constants: vec![],
            size: 16,
            align: 8,
            member_align: 8,
            is_complete: true,
            transparent: false,
            anon_id: None,
        })),
        ..Default::default()
    });

    // Create array type: struct pair[2]
    let array_type = ctx.types.intern(Type::array(struct_type, 2));

    // Create global array variable
    let arr_sym = ctx.var("arr", array_type);

    // Create local variable to copy into
    let item_sym = ctx.var("item", struct_type);

    // Create function: void test(void) { struct pair item = arr[0]; }
    let func = FunctionDef {
        attrs: Default::default(),
        return_type: ctx.types.void_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![
            BlockItem::Declaration(Declaration {
                declarators: vec![InitDeclarator {
                    fn_effect: Default::default(),
                    symbol_attrs: Default::default(),
                    pos: Position::default(),
                    symbol: item_sym,
                    typ: struct_type,
                    storage_class: crate::types::TypeModifiers::empty(),
                    init: Some(Expr::typed_unpositioned(
                        ExprKind::Index {
                            array: Box::new(Expr::typed_unpositioned(
                                ExprKind::Ident(arr_sym),
                                array_type,
                            )),
                            index: Box::new(Expr::int(0, &ctx.types)),
                        },
                        struct_type,
                    )),
                    vla_sizes: vec![],
                    explicit_align: None,
                }],
            }),
            BlockItem::Statement(Box::new(Stmt::Return(None))),
        ]),
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

    // For large struct copy (128 bits), we should see:
    // - Multiple 64-bit load/store pairs (at least 2 for a 128-bit struct)
    // The loads should NOT be dereferencing the struct's first field as a pointer
    // Instead they should be copying the struct content directly

    // Count 64-bit loads and stores
    let load64_count = ir.matches("load.64").count();
    let store64_count = ir.matches("store.64").count();

    // We need at least 2 loads and 2 stores for a 128-bit struct copy
    assert!(
        load64_count >= 2,
        "Expected at least 2 64-bit loads for struct copy, got {}: {}",
        load64_count,
        ir
    );
    assert!(
        store64_count >= 2,
        "Expected at least 2 64-bit stores for struct copy, got {}: {}",
        store64_count,
        ir
    );
}

/// Test that compound literals with designated initializers zero-initialize
/// fields that are not explicitly set.
/// This is required by C99 6.7.8p21: "If there are fewer initializers in a
/// brace-enclosed list than there are elements or members of an aggregate,
/// ... the remainder of the aggregate shall be initialized implicitly the same
/// as objects that have static storage duration."
#[test]
fn test_compound_literal_zero_init_lvalue() {
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let ptr_type = ctx.types.void_ptr_id;

    // Create a struct with three pointer fields (192 bits)
    // struct { void *a, *b, *c; };
    let a_id = ctx.str("a");
    let b_id = ctx.str("b");
    let c_id = ctx.str("c");

    let members = vec![
        StructMember {
            name: a_id,
            typ: ptr_type,
            offset: 0,
            bit_offset: None,
            bit_width: None,
            access_bytes: None,
            explicit_align: None,
        },
        StructMember {
            name: b_id,
            typ: ptr_type,
            offset: 8,
            bit_offset: None,
            bit_width: None,
            access_bytes: None,
            explicit_align: None,
        },
        StructMember {
            name: c_id,
            typ: ptr_type,
            offset: 16,
            bit_offset: None,
            bit_width: None,
            access_bytes: None,
            explicit_align: None,
        },
    ];

    let struct_type = ctx.types.intern(Type {
        kind: crate::types::TypeKind::Struct,
        composite: Some(Box::new(CompositeType {
            tag: None,
            members,
            enum_constants: vec![],
            size: 24,
            align: 8,
            member_align: 8,
            is_complete: true,
            transparent: false,
            anon_id: None,
        })),
        ..Default::default()
    });

    let struct_ptr_type = ctx.types.pointer_to(struct_type);

    // Create parameter: struct S *p
    let p_sym = ctx.var("p", struct_ptr_type);

    // Create compound literal expression: (struct S){.a = (void*)0x1234}
    // Only .a is set, .b and .c should be zero-initialized
    let compound_literal = Expr::typed_unpositioned(
        ExprKind::CompoundLiteral {
            typ: struct_type,
            elements: vec![InitElement {
                designators: vec![Designator::Field(a_id)],
                value: Box::new(Expr::typed_unpositioned(
                    ExprKind::Cast {
                        cast_type: ptr_type,
                        expr: Box::new(Expr::int(0x1234, &ctx.types)),
                    },
                    ptr_type,
                )),
            }],
        },
        struct_type,
    );

    // Create assignment: *p = compound_literal
    let assign = Expr::typed_unpositioned(
        ExprKind::Assign {
            op: AssignOp::Assign,
            target: Box::new(Expr::typed_unpositioned(
                ExprKind::Unary {
                    op: UnaryOp::Deref,
                    operand: Box::new(Expr::typed_unpositioned(
                        ExprKind::Ident(p_sym),
                        struct_ptr_type,
                    )),
                },
                struct_type,
            )),
            value: Box::new(compound_literal),
        },
        struct_type,
    );

    // Create function: void test(struct S *p) { *p = (struct S){.a = ...}; }
    let func = FunctionDef {
        attrs: Default::default(),
        return_type: ctx.types.void_id,
        name: test_id,
        params: vec![Parameter {
            symbol: Some(p_sym),
            typ: struct_ptr_type,
            vm_dims: vec![],
            discarded_dims: vec![],
        }],
        body: Stmt::Block(vec![
            BlockItem::Statement(Box::new(Stmt::Expr(assign))),
            BlockItem::Statement(Box::new(Stmt::Return(None))),
        ]),
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

    // The compound literal must be zero-initialized first, then the designated
    // fields are set. The code will:
    // 1. Zero all 3 fields (24 bytes at offsets 0, 8, 16)
    // 2. Store the designated value to .a (offset 0)
    // 3. Copy the compound literal to *p
    //
    // The zero-init may reuse the same register for all zero stores (optimization)
    // so we count stores to the compound literal at different offsets.

    // Verify that we have stores to the compound literal at offset +8 and +16
    // These are the fields (.b and .c) that are not explicitly initialized
    // and must be zero-initialized per C99 6.7.8p21
    assert!(
        ir.contains("+ 8") && ir.contains("+ 16"),
        "Expected stores to offsets +8 and +16 for zero-init of fields b and c: {}",
        ir
    );

    // Count store.64 operations - we need:
    // - 3 stores for zero-init (offsets 0, 8, 16)
    // - 1 store for designated init of .a (offset 0)
    // - 3 stores for copying to *p (offsets 0, 8, 16)
    // Plus some for parameter handling
    let store64_count = ir.matches("store.64").count();
    assert!(
        store64_count >= 7,
        "Expected at least 7 store.64 (3 zero-init + 1 designated + 3 copy), got {}: {}",
        store64_count,
        ir
    );
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
        explicit_align: None,
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
        ir.contains("cbr "),
        "Expected conditional branch (cbr) for short-circuit evaluation: {}",
        ir
    );

    // Should have phi instruction to merge results from both branches
    assert!(
        ir.contains("phi."),
        "Expected phi instruction for merging conditional results: {}",
        ir
    );

    // Should NOT have select instruction (would mean eager evaluation of both branches)
    assert!(
        !ir.contains("select."),
        "Should NOT use select instruction with pointer dereference (causes UB): {}",
        ir
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
    assert_eq!(insn.memory_order, MemoryOrder::SeqCst);
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
            explicit_align: None,
        }],
        enum_constants: vec![],
        size: 4,
        align: 4,
        member_align: 4,
        is_complete: true,
        transparent: false,
        anon_id: None,
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

/// A complex member of an automatic struct is stored as two halves.
///
/// A complex value travels by *address*, so storing it the way a scalar member
/// is stored wrote the address rather than the value. The IR shape that says
/// this is right is two stores at the base type's width, at the member's
/// offset and one base width above it -- not a single store at the complex
/// type's full width.
#[test]
fn test_complex_struct_member_init_stores_both_halves() {
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let s_tag = ctx.str("S");
    let complex_double = ctx.types.complex_double_id;

    // struct S { double _Complex z; };
    let struct_type = Type::struct_type(CompositeType {
        tag: Some(s_tag),
        members: vec![StructMember {
            name: ctx.str("z"),
            typ: complex_double,
            offset: 0,
            bit_width: None,
            bit_offset: None,
            access_bytes: None,
            explicit_align: None,
        }],
        enum_constants: vec![],
        size: 16,
        align: 8,
        member_align: 8,
        is_complete: true,
        transparent: false,
        anon_id: None,
    });
    let struct_type_id = ctx.types.intern(struct_type);
    let s_sym = ctx.var("s", struct_type_id);

    // void test(void) { struct S s = { 1.0 }; }
    //
    // A *real* initializer is the case that proves the zero imaginary half
    // gets written.
    let init = Expr::typed_unpositioned(
        ExprKind::InitList {
            elements: vec![InitElement {
                designators: vec![],
                value: Box::new(Expr::typed_unpositioned(
                    ExprKind::FloatLit(crate::float::FloatVal::from_f64(1.0)),
                    ctx.types.double_id,
                )),
            }],
        },
        struct_type_id,
    );
    let func = FunctionDef {
        attrs: Default::default(),
        return_type: ctx.types.void_id,
        name: test_id,
        params: vec![],
        body: Stmt::Block(vec![BlockItem::Declaration(Declaration {
            declarators: vec![InitDeclarator {
                fn_effect: Default::default(),
                symbol_attrs: Default::default(),
                pos: Position::default(),
                symbol: s_sym,
                typ: struct_type_id,
                storage_class: crate::types::TypeModifiers::empty(),
                init: Some(init),
                vla_sizes: vec![],
                explicit_align: None,
            }],
        })]),
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
    let stores: Vec<_> = func
        .blocks
        .iter()
        .flat_map(|b| b.insns.iter())
        .filter(|i| i.op == Opcode::Store)
        .filter(|i| i.size == 64)
        .map(|i| i.offset)
        .collect();

    // Both halves, at the base type's width. The zeroing pass emits its own
    // stores, so this asserts the offsets are present rather than counting.
    assert!(
        stores.contains(&0) && stores.contains(&8),
        "expected 64-bit stores at offsets 0 and 8, got {:?}\n{}",
        stores,
        module.display(&ctx.types)
    );
    assert!(
        !func
            .blocks
            .iter()
            .flat_map(|b| b.insns.iter())
            .any(|i| i.op == Opcode::Store && i.size == 128),
        "a complex member must not be stored as one 128-bit value: {}",
        module.display(&ctx.types)
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
        calls[0].ends_with_va_arg_pack,
        "the call should carry the pack"
    );
    assert_eq!(
        calls[0].src.len(),
        1,
        "the pack must not become an argument: {:?}",
        calls[0].src
    );
    assert_eq!(calls[0].arg_types.len(), calls[0].src.len());
}

/// A declaration inside a function body with `extern` declares no object: it
/// refers to one with external linkage (C17 6.2.2p4). Falling through to the
/// automatic-storage path gave it a stack slot *and* an entry in the
/// linearizer's local map, and the local map is what every later reference
/// consults -- so the read went to the frame instead of to the global.
#[test]
fn test_block_scope_extern_declares_no_local() {
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let g_sym = ctx.var("g", int_type);

    // int test(void) { extern int g; return g; }
    let body = Stmt::Block(vec![
        BlockItem::Declaration(Declaration {
            declarators: vec![crate::parse::ast::InitDeclarator {
                fn_effect: Default::default(),
                symbol_attrs: Default::default(),
                pos: Position::default(),
                symbol: g_sym,
                typ: int_type,
                storage_class: crate::types::TypeModifiers::EXTERN,
                init: None,
                vla_sizes: vec![],
                explicit_align: None,
            }],
        }),
        BlockItem::Statement(Box::new(Stmt::Return(Some(Expr::var_typed(
            g_sym, int_type,
        ))))),
    ]);
    let func = FunctionDef {
        attrs: Default::default(),
        return_type: int_type,
        name: test_id,
        params: vec![],
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

    assert!(
        f.locals.is_empty(),
        "an extern declaration must allocate no local: {:?}",
        f.locals.keys().collect::<Vec<_>>()
    );

    // The reference must reach a bare-named global symbol, not the mangled
    // `g.<id>` spelling a block-scope local would get.
    let syms: Vec<&str> = f
        .pseudos
        .iter()
        .filter_map(|p| match &p.kind {
            crate::ir::PseudoKind::Sym(name) => Some(name.as_str()),
            _ => None,
        })
        .collect();
    assert!(
        syms.contains(&"g"),
        "the reference should name the global `g`, got {:?}",
        syms
    );
    assert!(
        !syms.iter().any(|s| s.starts_with("g.")),
        "no mangled local should be created, got {:?}",
        syms
    );
    assert!(
        module.extern_symbols.contains("g"),
        "the name must be recorded as external so codegen reaches it through \
         the GOT on macOS, got {:?}",
        module.extern_symbols
    ); // And its alignment, which is all a backend knows about an object
       // defined elsewhere -- aarch64 folds `:lo12:` into an access only when it
       // covers the access size.
    assert_eq!(
        module.extern_object_align.get("g"),
        Some(&4),
        "an extern object's declared alignment must be recorded"
    );
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

/// A function's `aligned` attribute reaches the IR function the backends emit.
#[test]
fn test_function_alignment_reaches_the_ir() {
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let mut func = make_simple_func(
        test_id,
        Stmt::Return(Some(Expr::int(0, &ctx.types))),
        &ctx.types,
    );
    func.attrs.align = Some(64);
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };
    let module = ctx.linearize(&tu);
    assert_eq!(module.functions[0].align, Some(64));
}

/// An `asm goto` output is written back on every path out of the statement.
///
/// The label edge gets a block of its own that stores the output and then
/// branches to the label; the asm names that block, not the label. Before,
/// the store sat only on the fall-through, so a jump reached the label with
/// the output never written.
#[test]
fn test_asm_goto_output_written_back_on_the_label_edge() {
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let out_id = ctx.str("out");
    let int_type = ctx.int_type();
    let x_sym = ctx.var("x", int_type);

    // { asm goto("" : "=r"(x) : : : out); return 0; out: return x; }
    let body = Stmt::Block(vec![
        BlockItem::Statement(Box::new(Stmt::Asm {
            template: String::new(),
            outputs: vec![AsmOperand {
                name: None,
                constraint: "=r".to_string(),
                expr: Expr::var_typed(x_sym, int_type),
            }],
            inputs: vec![],
            clobbers: vec![],
            goto_labels: vec![out_id],
        })),
        BlockItem::Statement(Box::new(Stmt::Return(Some(Expr::int(0, &ctx.types))))),
        BlockItem::Statement(Box::new(Stmt::Label {
            name: out_id,
            stmt: Box::new(Stmt::Return(Some(Expr::var_typed(x_sym, int_type)))),
            pos: test_pos(),
        })),
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
    let func = &module.functions[0];

    let asm = func
        .blocks
        .iter()
        .flat_map(|bb| bb.insns.iter())
        .find(|insn| insn.op == Opcode::Asm)
        .expect("an asm instruction");
    let out_pseudo = asm.asm_data.as_ref().unwrap().outputs[0].pseudo;
    let (edge, _) = asm.asm_data.as_ref().unwrap().goto_labels[0];
    let edge = func.get_block(edge).expect("the label edge block");
    assert!(
        edge.insns
            .iter()
            .any(|i| i.op == Opcode::Store && i.src.contains(&out_pseudo)),
        "the label edge must store the output: {:?}",
        edge.insns
    );
    assert!(
        matches!(edge.insns.last().map(|i| i.op), Some(Opcode::Br)),
        "the label edge must end by branching to the label"
    );
}

/// A memory operand naming an object is handed over as the object's `Sym`,
/// not as an address computed into a pseudo.
///
/// Addressed in place it needs no register; as an address value it
/// competed for one and, once spilled, the backend substituted the spill
/// slot as the operand. The address arithmetic the lvalue produced is left
/// for DCE, which removes it once nothing reads it.
#[test]
fn test_asm_memory_operand_names_its_object() {
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.int_type();
    let x_sym = ctx.var("x", int_type);

    // { asm("" : "=m"(x) : "m"(x)); return x; }
    let body = Stmt::Block(vec![
        BlockItem::Statement(Box::new(Stmt::Asm {
            template: String::new(),
            outputs: vec![AsmOperand {
                name: None,
                constraint: "=m".to_string(),
                expr: Expr::var_typed(x_sym, int_type),
            }],
            inputs: vec![AsmOperand {
                name: None,
                constraint: "m".to_string(),
                expr: Expr::var_typed(x_sym, int_type),
            }],
            clobbers: vec![],
            goto_labels: vec![],
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
    let func = &module.functions[0];

    let asm = func
        .blocks
        .iter()
        .flat_map(|bb| bb.insns.iter())
        .find(|insn| insn.op == Opcode::Asm)
        .expect("an asm instruction");
    let data = asm.asm_data.as_ref().unwrap();
    for c in data.outputs.iter().chain(data.inputs.iter()) {
        assert!(
            matches!(
                func.get_pseudo(c.pseudo).map(|p| &p.kind),
                Some(crate::ir::PseudoKind::Sym(_))
            ),
            "{c:?} should name the object"
        );
        assert_eq!(c.offset, 0);
    }
    let mut func = func.clone();
    crate::ir::dce::run(&mut func);
    assert!(
        !func
            .blocks
            .iter()
            .flat_map(|bb| bb.insns.iter())
            .any(|i| i.op == Opcode::SymAddr),
        "nothing reads the operand's address arithmetic, so DCE removes it"
    );
}

/// The address arithmetic behind a memory operand that names an object is
/// not the linearizer's to delete: `"=m"(*(q = &arr[2]))` also stores that
/// address into `q`, before the asm. Deleting it left the store reading an
/// undefined register, so `q == &arr[2]` was false.
#[test]
fn test_asm_memory_operand_keeps_an_address_something_else_reads() {
    let src = "int arr[4]; int *q;\n\
               void f(void) { __asm__ volatile(\"\" : \"=m\"(*(q = &arr[2]))); }\n";
    let module = linearize_source(src, &Target::host());
    let func = module.functions.iter().find(|f| f.name == "f").expect("f");
    let insns: Vec<&Instruction> = func.blocks.iter().flat_map(|bb| bb.insns.iter()).collect();
    // Every pseudo a live instruction reads must be defined by a live one.
    let defined: std::collections::HashSet<PseudoId> = insns
        .iter()
        .filter(|i| i.op != Opcode::Nop)
        .filter_map(|i| i.target)
        .chain(
            func.pseudos
                .iter()
                .filter(|p| {
                    !matches!(
                        p.kind,
                        crate::ir::PseudoKind::Reg(_) | crate::ir::PseudoKind::Phi(_)
                    )
                })
                .map(|p| p.id),
        )
        .collect();
    for insn in insns.iter().filter(|i| i.op == Opcode::Store) {
        for s in &insn.src {
            assert!(
                defined.contains(s),
                "{:?} reads {s:?}, which nothing defines",
                insn.op
            );
        }
    }
}

/// Parse `src` and linearize it for `target`.
fn linearize_source(src: &str, target: &Target) -> Module {
    linearize_source_with_types(src, target).0
}

/// [`linearize_source`], also handing back the type table the module's type
/// ids index.
fn linearize_source_with_types(src: &str, target: &Target) -> (Module, TypeTable) {
    linearize_source_under(src, target, Default::default())
}

/// [`linearize_source_with_types`], with library builtins evaluated as
/// `policy` says.
fn linearize_source_under(
    src: &str,
    target: &Target,
    policy: crate::parse::LibraryCallPolicy,
) -> (Module, TypeTable) {
    linearize_source_trapping(src, target, policy, true)
}

/// [`linearize_source_under`], with `-f[no-]trapping-math` as given.
fn linearize_source_trapping(
    src: &str,
    target: &Target,
    policy: crate::parse::LibraryCallPolicy,
    trapping_math: bool,
) -> (Module, TypeTable) {
    let mut strings = StringTable::new();
    let mut tokenizer = crate::token::lexer::Tokenizer::new(src.as_bytes(), 0, &mut strings);
    let tokens = tokenizer.tokenize();
    let mut symbols = crate::symbol::SymbolTable::new();
    let mut types = TypeTable::new(target);
    let tu = {
        let mut parser =
            crate::parse::Parser::new(&tokens, &strings, &mut symbols, &mut types, Vec::new());
        parser.set_library_call_policy(policy);
        parser.parse_translation_unit().expect("parse")
    };
    let module = linearize(
        &tu,
        &symbols,
        &types,
        &strings,
        target,
        false,
        trapping_math,
    );
    (module, types)
}

/// A call records each argument at the type the callee receives it as, which
/// is what the ABI places it by: the parameter's type under a prototype --
/// narrower included -- and the default argument promotions without one
/// (C17 6.5.2.2p6-7). Apple arm64 stacks a `char` parameter in one byte, so
/// `p('a')` recorded as `int` took four; and an unprototyped call passed a
/// `float` where a gcc-compiled K&R callee reads a `double`.
#[test]
fn test_call_argument_types_follow_the_callee() {
    use crate::target::{Arch, Os};
    let src = "void p(char, short, int);\n\
               int u();\n\
               void f(long l, char c, float x) { p(l, 300, l); u(c, x); }\n";
    let target = Target::new(Arch::Aarch64, Os::MacOS);
    let (module, types) = linearize_source_with_types(src, &target);
    let f = module.functions.iter().find(|f| f.name == "f").unwrap();
    let calls: Vec<&Instruction> = f
        .blocks
        .iter()
        .flat_map(|bb| bb.insns.iter())
        .filter(|i| i.op == Opcode::Call)
        .collect();
    assert_eq!(calls.len(), 2);
    let kinds = |call: &Instruction| -> Vec<TypeKind> {
        call.arg_types.iter().map(|&t| types.kind(t)).collect()
    };
    assert_eq!(
        kinds(calls[0]),
        [TypeKind::Char, TypeKind::Short, TypeKind::Int],
        "a prototyped call passes each argument as its parameter's type"
    );
    assert_eq!(
        kinds(calls[1]),
        [TypeKind::Int, TypeKind::Double],
        "an unprototyped call passes the promoted types"
    );
}

/// An identifier-list definition receives each parameter as its default
/// argument promotion -- what every caller passes, having no prototype to
/// convert to -- and converts it to the declared type on entry (C17
/// 6.9.1p10). It read a `float` parameter out of the register a caller had
/// put a `double` in.
#[test]
fn test_identifier_list_parameters_arrive_promoted() {
    use crate::target::{Arch, Os};
    let src = "int kr(f, c, s, l) float f; char c; short s; long l;\n\
               { return (int)f + c + s + (int)l; }\n\
               int pr(float f, char c) { return (int)f + c; }\n";
    let (module, types) = linearize_source_with_types(src, &Target::new(Arch::Aarch64, Os::Linux));
    let params = |name: &str| -> Vec<TypeKind> {
        let f = module.functions.iter().find(|f| f.name == name).unwrap();
        f.params.iter().map(|(_, t)| types.kind(*t)).collect()
    };
    assert_eq!(
        params("kr"),
        [
            TypeKind::Double,
            TypeKind::Int,
            TypeKind::Int,
            TypeKind::Long
        ]
    );
    assert_eq!(params("pr"), [TypeKind::Float, TypeKind::Char]);
    let kr = module.functions.iter().find(|f| f.name == "kr").unwrap();
    assert!(
        kr.blocks
            .iter()
            .flat_map(|bb| bb.insns.iter())
            .any(|i| i.op == Opcode::FCvtF),
        "the double must be converted to the float parameter on entry"
    );
}

/// Every instruction of the function `name` in `module`.
fn insns_of<'m>(module: &'m Module, name: &str) -> Vec<&'m Instruction> {
    let f = module.functions.iter().find(|f| f.name == name).unwrap();
    f.blocks.iter().flat_map(|bb| bb.insns.iter()).collect()
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
    assert_eq!(sqrt[0].func_name.as_deref(), Some("sqrt"));
    let calls: Vec<_> = insns.iter().filter(|i| i.op == Opcode::Call).collect();
    assert_eq!(calls.len(), 1);
    assert_eq!(calls[0].func_name.as_deref(), Some("sqrt"));
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
    assert_eq!(floor.func_name.as_deref(), Some("floorf"));
    assert_eq!(floor.size, 32, "computed at float");
    let nearby = insns
        .iter()
        .find(|i| i.op == Opcode::RoundToIntegral(NearbyInt))
        .expect("nearbyint");
    assert_eq!(nearby.func_name.as_deref(), Some("nearbyint"));
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
        assert_eq!(insn.func_name.as_deref(), Some(name));
        assert_eq!((insn.src.len(), insn.size), (srcs, size), "{name}");
    }
}

/// The translation unit's own `floor` and `memcpy`, defined below the call,
/// are called -- `fabs`, which gcc folds as it parses, stays computed in
/// place.
#[test]
fn test_own_definition_below_the_call_is_called() {
    let src = "double floor(double); double fabs(double);\n\
               double f(double x, char *d, char *s) {\n\
                   memcpy(d, s, 8); return floor(x) + fabs(x); }\n\
               double floor(double x) { return x; }\n\
               double fabs(double x) { return x; }\n\
               void *memcpy(void *d, const void *s, unsigned long n) { return d; }\n";
    let module = linearize_source(src, &Target::new(Arch::X86_64, Os::Linux));
    let insns = insns_of(&module, "f");
    let calls: Vec<_> = insns
        .iter()
        .filter(|i| i.op == Opcode::Call)
        .map(|i| i.func_name.as_deref())
        .collect();
    assert_eq!(calls, [Some("memcpy"), Some("floor")]);
    assert!(!insns
        .iter()
        .any(|i| i.op.is_libm() || i.op == Opcode::Memcpy));
    assert!(insns.iter().any(|i| i.op == Opcode::Fabs));
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
        .map(|i| (i.op, i.func_name.as_deref(), i.src.len()))
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
    assert_eq!(calls[0].func_name.as_deref(), Some("sqrtl"));
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

/// AAPCS64 B.4 passes a composite over sixteen bytes as a pointer to a copy
/// the caller makes; System V puts the bytes themselves in the argument area.
/// So only the aarch64 lowering copies the argument into a frame temporary
/// before the call -- and passes that temporary's address, not the global's.
#[test]
fn test_large_composite_argument_copied_only_where_the_abi_passes_a_reference() {
    use crate::target::{Arch, Os};
    let src = "struct big { long a, b, c; };\n\
               struct big g;\n\
               long f(struct big s);\n\
               long t(void) { return f(g); }\n";
    let has_copy = |target: Target| {
        let module = linearize_source(src, &target);
        let t = module.functions.iter().find(|f| f.name == "t").unwrap();
        t.locals.keys().any(|name| name.starts_with("__argcopy_"))
    };
    assert!(has_copy(Target::new(Arch::Aarch64, Os::Linux)));
    assert!(has_copy(Target::new(Arch::Aarch64, Os::MacOS)));
    assert!(!has_copy(Target::new(Arch::X86_64, Os::Linux)));
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
    items.push(BlockItem::Statement(Box::new(Stmt::Label {
        name: end_id,
        stmt: Box::new(Stmt::Return(Some(Expr::var_typed(x_sym, int_type)))),
        pos: test_pos(),
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

/// Inlining `__builtin_va_arg_pack_len()` replaces the pseudo standing for
/// it with a constant. The inliner did that with `pseudos.retain` and never
/// rebuilt `pseudo_idx`, so every caller pseudo after the removed placeholder
/// was looked up at its neighbour's position -- `get_pseudo` answered a
/// different pseudo's kind until `mem2reg` happened to rebuild the index.
#[test]
fn test_inlining_va_arg_pack_len_keeps_the_pseudo_index() {
    let src = "extern inline __attribute__((always_inline, gnu_inline))\n\
               int count(int a, ...) { int k = a * 3; return __builtin_va_arg_pack_len() + k; }\n\
               int caller(int x) { int y = x + 1; return count(y, 1, 2, 3) + count(x) * 7 + y; }\n";
    let mut module = linearize_source(src, &Target::host());
    let opt = crate::opt::Optimization::from_flag("2").unwrap();
    crate::ir::inline::run(&mut module, opt);
    let caller = module
        .functions
        .iter()
        .find(|f| f.name == "caller")
        .expect("caller");
    assert!(
        caller
            .blocks
            .iter()
            .flat_map(|b| b.insns.iter())
            .all(|i| i.op != Opcode::Call),
        "count should have been inlined"
    );
    let mut errors = Vec::new();
    crate::ir::validate::check_pseudo_index(caller, &mut errors);
    assert!(errors.is_empty(), "{errors:?}");
}

/// Collapsing the inner diamond of `a && b && c` is what makes the outer one
/// recognizable, so `ifconv` has to revisit the blocks that branch to a
/// collapsed predecessor. It does so from a worklist now, not by rescanning
/// the function after every collapse; this pins that the whole chain still
/// folds to straight-line code, and that many sequential diamonds all do.
#[test]
fn test_ifconv_collapses_nested_and_sequential_diamonds() {
    let chain = |src: &str, name: &str| -> usize {
        let mut module = linearize_source(src, &Target::host());
        let func = module
            .functions
            .iter_mut()
            .find(|f| f.name == name)
            .expect("function");
        assert!(crate::ir::ifconv::run(func));
        let mut errors = Vec::new();
        crate::ir::validate::check_pseudo_index(func, &mut errors);
        assert!(errors.is_empty(), "{errors:?}");
        func.blocks
            .iter()
            .flat_map(|b| b.insns.iter())
            .filter(|i| i.op == Opcode::Cbr)
            .count()
    };
    assert_eq!(
        chain(
            "int f(int a, int b, int c, int d) { return a > 0 && b > 0 && c > 0 && d > 0; }",
            "f"
        ),
        0
    );
    let many: String = (0..200)
        .map(|i| format!("  if (x > {i}) s += {};\n", i % 7))
        .collect();
    let src = format!("int g(int x) {{\n  int s = 0;\n{many}  return s;\n}}\n");
    assert_eq!(chain(&src, "g"), 0);
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

/// A complex result lives in a fixed frame slot, never an `alloca`.
///
/// Each complex operation needs its result in memory, and that memory was an
/// `alloca`: it grew the stack on every evaluation and was released only at
/// return, so a complex expression in a loop exhausted the stack.
#[test]
fn test_complex_temporaries_are_frame_slots() {
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let complex_double = ctx.types.complex_double_id;
    let a_sym = ctx.var("a", complex_double);
    let b_sym = ctx.var("b", complex_double);

    // double _Complex test(double _Complex a, double _Complex b)
    // { return -(a * b + a); }
    let a = || Box::new(Expr::var_typed(a_sym, complex_double));
    let mul = Expr::typed_unpositioned(
        ExprKind::Binary {
            op: BinaryOp::Mul,
            left: a(),
            right: Box::new(Expr::var_typed(b_sym, complex_double)),
        },
        complex_double,
    );
    let add = Expr::typed_unpositioned(
        ExprKind::Binary {
            op: BinaryOp::Add,
            left: Box::new(mul),
            right: a(),
        },
        complex_double,
    );
    let neg = Expr::typed_unpositioned(
        ExprKind::Unary {
            op: UnaryOp::Neg,
            operand: Box::new(add),
        },
        complex_double,
    );
    let param = |symbol| Parameter {
        symbol: Some(symbol),
        typ: complex_double,
        vm_dims: vec![],
        discarded_dims: vec![],
    };
    let func = FunctionDef {
        attrs: Default::default(),
        return_type: complex_double,
        name: test_id,
        params: vec![param(a_sym), param(b_sym)],
        body: Stmt::Return(Some(neg)),
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

    assert!(
        f.blocks
            .iter()
            .flat_map(|b| b.insns.iter())
            .all(|i| i.op != Opcode::Alloca),
        "a complex temporary must not be an alloca:\n{}",
        module.display(&ctx.types)
    );
    // The multiply, the add and the negation each own a slot.
    assert_eq!(
        f.locals.keys().filter(|n| n.starts_with("__ctmp_")).count(),
        3,
        "each complex result needs its own frame slot:\n{}",
        module.display(&ctx.types)
    );
}

/// Every opcode a backend lowers to a library call carries that function's
/// assembler name, resolved through the program's declarations: an asm-label
/// rename of `memcpy` reaches `__builtin_memcpy` and a structure copy alike,
/// and a function the program did not rename keeps its own name.
#[test]
fn test_library_callee_honours_asm_label() {
    let src = "typedef unsigned long size_t;\n\
               void *memcpy(void *, const void *, size_t) __asm(\"my_memcpy\");\n\
               void *memset(void *, int, size_t) __asm(\"my_memset\");\n\
               struct big { long a[32]; };\n\
               void t(struct big *d, struct big *s, char *p, size_t n) {\n\
                   __builtin_memcpy(p, p + 1, n);\n\
                   __builtin_memset(p, 0, n);\n\
                   __builtin_memmove(p, p + 1, n);\n\
                   *d = *s;\n\
               }\n";
    let module = linearize_source(src, &Target::host());
    let t = module.functions.iter().find(|f| f.name == "t").unwrap();
    let callees: Vec<(Opcode, &str)> = t
        .blocks
        .iter()
        .flat_map(|b| b.insns.iter())
        .filter(|i| matches!(i.op, Opcode::Memcpy | Opcode::Memset | Opcode::Memmove))
        .map(|i| (i.op, i.library_callee()))
        .collect();
    let my_memcpy = crate::arch::lir::verbatim("my_memcpy");
    let my_memset = crate::arch::lir::verbatim("my_memset");
    assert_eq!(
        callees,
        vec![
            (Opcode::Memcpy, my_memcpy.as_str()),
            (Opcode::Memset, my_memset.as_str()),
            (Opcode::Memmove, "memmove"),
            (Opcode::Memcpy, my_memcpy.as_str()),
        ]
    );
}

/// An `alias` declaration becomes a `SymbolAlias` -- not a definition, and
/// not an external reference, even when an ordinary redeclaration of the
/// same name follows it or the target is defined only afterwards. An alias
/// of an alias names the alias it was written against.
#[test]
fn test_alias_declarations_become_symbol_aliases() {
    let src = "extern int b[4] __attribute__((alias(\"a\")));\n\
               extern int b[4];\n\
               int a[4];\n\
               static int s;\n\
               static int t __attribute__((weak, alias(\"s\")));\n\
               int f(void) { return 0; }\n\
               int g(void) __attribute__((alias(\"f\"), visibility(\"hidden\")));\n\
               int h(void) __attribute__((alias(\"g\")));\n\
               int use(void) { return b[0] + g(); }\n";
    // An ELF target: Mach-O has no aliases, and a Darwin host would reject
    // every one of these before recording it.
    let target = Target::new(crate::target::Arch::X86_64, crate::target::Os::Linux);
    let module = linearize_source(src, &target);
    let alias = |name: &str, target: &str, is_static: bool, weak: bool, vis: Option<&str>| {
        crate::ir::SymbolAlias {
            name: name.to_string(),
            target: target.to_string(),
            is_static,
            weak,
            visibility: vis.map(str::to_string),
        }
    };
    assert_eq!(
        module.aliases,
        [
            alias("b", "a", false, false, None),
            alias("t", "s", true, true, None),
            alias("g", "f", false, false, Some("hidden")),
            alias("h", "g", false, false, None),
        ]
    );
    for name in ["b", "t", "g", "h"] {
        assert!(
            !module.extern_symbols.contains(name),
            "{name} is not extern"
        );
        assert!(
            !module.globals.iter().any(|g| g.name == name),
            "{name} has no storage"
        );
        assert!(
            !module.functions.iter().any(|f| f.name == name),
            "{name} has no body"
        );
    }
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

/// GNU `~` on a complex constant is the conjugate, and folds in a static
/// initializer: only the imaginary half is negated, so `~(3 + 4i)` is
/// `3 - 4i`, and `~~z` is `z` again.
#[test]
fn test_complex_conjugate_static_initializer() {
    let src = "static _Complex double a = ~(3.0 + 4.0i);\n\
               static _Complex double b = ~~(5.0 + 6.0i);\n\
               static _Complex double c = -~(1.0 + 0.0i);\n";
    let module = linearize_source(src, &Target::host());
    let halves = |name: &str| -> (f64, f64) {
        let g = module
            .globals
            .iter()
            .find(|g| g.name == name)
            .unwrap_or_else(|| panic!("no global {name}"));
        let crate::ir::Initializer::Struct { fields, .. } = &g.init else {
            panic!("{name}: expected a two-field initializer, got {:?}", g.init);
        };
        let value = |i: usize| match &fields[i].2 {
            crate::ir::Initializer::Float(v) => v.to_f64(),
            other => panic!("{name}[{i}]: expected a float half, got {other:?}"),
        };
        (value(0), value(1))
    };
    assert_eq!(halves("a"), (3.0, -4.0));
    assert_eq!(halves("b"), (5.0, 6.0));
    // `-~(1 + 0i)` is `-(1 - 0i)`: the conjugate's `-0.0` imaginary half is
    // negated back to `+0.0`, and the real half becomes `-1.0`.
    let (re, im) = halves("c");
    assert_eq!(re, -1.0);
    assert!(im == 0.0 && im.is_sign_positive(), "c imag is {im}");
}

/// A static floating initializer is converted from its own type to the
/// object's, as an assignment converts it (C17 6.7.9p11): rounded at the
/// initializer's format first, and a NaN quieted when the format changes
/// while keeping its sign and the high bits of its payload. Every encoding
/// is gcc's for the same declaration.
#[test]
fn test_static_float_initializer_converts_from_its_own_type() {
    use crate::float::FpFormat;
    let src = "static double widened = 0.1f;\n\
               static float narrowed = (float)__builtin_nans(\"0x40000000\");\n\
               static double quieted = __builtin_nansf(\"0x123\");\n\
               static double kept = -__builtin_nans(\"0x5\");\n\
               static double propagated = __builtin_nan(\"0x5\") + 1.0;\n";
    let module = linearize_source(src, &Target::host());
    let bits = |name: &str, fmt: FpFormat| -> u128 {
        let g = module
            .globals
            .iter()
            .find(|g| g.name == name)
            .unwrap_or_else(|| panic!("no global {name}"));
        match &g.init {
            crate::ir::Initializer::Float(v) => v.to_bits(fmt),
            other => panic!("{name}: expected a float initializer, got {other:?}"),
        }
    };
    assert_eq!(
        bits("widened", FpFormat::Binary64),
        u128::from(f64::from(0.1f32).to_bits())
    );
    assert_eq!(bits("narrowed", FpFormat::Binary32), 0x7fc0_0002);
    assert_eq!(bits("quieted", FpFormat::Binary64), 0x7ff8_0024_6000_0000);
    // No conversion: a negated signalling NaN stays signalling.
    assert_eq!(bits("kept", FpFormat::Binary64), 0xfff0_0000_0000_0005);
    assert_eq!(
        bits("propagated", FpFormat::Binary64),
        0x7ff8_0000_0000_0005
    );
}

/// The global `name`'s initializer.
fn global_init<'m>(module: &'m crate::ir::Module, name: &str) -> &'m crate::ir::Initializer {
    &module
        .globals
        .iter()
        .find(|g| g.name == name)
        .unwrap_or_else(|| panic!("no global {name}"))
        .init
}

/// A floating constant converts to an integer object exactly, truncating
/// toward zero from the value at the constant's own precision: each of these
/// came out rounded to `double` first. Out of range, where C gives no value,
/// the object gets gcc's saturated one.
#[test]
fn test_static_float_to_integer_initializer_is_exact() {
    let src = "static long long a = 0x1p62L + 1.0L;\n\
               static unsigned long long b = 0x1p63L + 3.0L;\n\
               static long long c = -0x1p62L - 5.0L;\n\
               static long long d = 0x1p53 + 1.0L;\n\
               static int e = 0.99999999999999999999;\n\
               static _Bool f = 0.5;\n\
               static int g = 3e9;\n\
               static unsigned h = -1.5;\n\
               static int i = __builtin_nan(\"\");\n";
    let module = linearize_source(src, &x86_64_linux());
    let int = |name: &str| match global_init(&module, name) {
        crate::ir::Initializer::Int(v) => *v,
        other => panic!("{name}: expected an integer, got {other:?}"),
    };
    assert_eq!(int("a"), (1i128 << 62) + 1);
    assert_eq!(int("b"), (1i128 << 63) + 3);
    assert_eq!(int("c"), -((1i128 << 62) + 5));
    assert_eq!(int("d"), (1i128 << 53) + 1);
    // A `double` literal is a `double`: this one rounds to 1.0 before it
    // is truncated.
    assert_eq!(int("e"), 1);
    assert_eq!(int("f"), 1);
    assert_eq!(int("g"), i128::from(i32::MAX));
    assert_eq!(int("h"), 0);
    assert_eq!(int("i"), 0);
}

/// A cast inside a static initializer converts: `(int)2.5` is 2 whatever
/// type is being initialized, and an integer subexpression of a floating one
/// is integer arithmetic.
#[test]
fn test_static_initializer_casts_and_integer_subexpressions_convert() {
    let src = "static double a = (int)2.5;\n\
               static double b = (1 / 2) + 0.5;\n\
               static long long c = (long long)(0x1p62L + 1.0L);\n\
               static int d = (int)1e300;\n";
    let module = linearize_source(src, &x86_64_linux());
    let float = |name: &str| match global_init(&module, name) {
        crate::ir::Initializer::Float(v) => v.to_f64(),
        other => panic!("{name}: expected a float, got {other:?}"),
    };
    assert_eq!(float("a"), 2.0);
    assert_eq!(float("b"), 0.5);
    assert!(
        matches!(global_init(&module, "c"), crate::ir::Initializer::Int(v) if *v == (1i128 << 62) + 1)
    );
    assert!(
        matches!(global_init(&module, "d"), crate::ir::Initializer::Int(v) if *v == i128::from(i32::MAX))
    );
}

/// The two halves of a complex static initializer.
fn complex_halves(
    module: &crate::ir::Module,
    name: &str,
) -> (crate::ir::Initializer, crate::ir::Initializer) {
    let crate::ir::Initializer::Struct { fields, .. } = global_init(module, name) else {
        panic!("{name}: expected a two-field initializer");
    };
    (fields[0].2.clone(), fields[1].2.clone())
}

/// Complex constant arithmetic is done in the base format of the
/// expression's own type, as the program does it at run time, on both
/// `long double` formats: the old fold went through `f64` and lost the
/// 2^-60 in every one of these.
#[test]
fn test_static_complex_long_double_keeps_its_precision() {
    use crate::float::FloatVal;
    let src = "static long double _Complex z = (1.0L + 0x1p-60L) + 2.0iL;\n\
               static long double _Complex w = (1.0L + 0x1p-60L) * (1.0L + 1.0iL);\n\
               static long double _Complex q = (2.0L + 0x1p-59L) / 2.0L;\n\
               static long double _Complex s = (1.0L + 1.0iL) - 0x1p-60L;\n\
               static long double _Complex r = (1.0L + 0x1p-60L + 1.0iL) / (1.0L + 1.0iL);\n";
    let one = FloatVal::from_i128(1);
    let one_plus = FloatVal::from_parts(false, (1u128 << 60) + 1, -60);
    let one_minus = FloatVal::from_parts(false, (1u128 << 60) - 1, -60);
    let half_tiny = FloatVal::from_parts(false, 1, -61);
    for target in [
        Target::new(crate::target::Arch::X86_64, crate::target::Os::Linux),
        Target::new(crate::target::Arch::Aarch64, crate::target::Os::Linux),
    ] {
        let module = linearize_source(src, &target);
        let halves = |name: &str| match complex_halves(&module, name) {
            (crate::ir::Initializer::Float(re), crate::ir::Initializer::Float(im)) => (re, im),
            other => panic!("{name}: expected float halves, got {other:?}"),
        };
        assert_eq!(halves("z"), (one_plus, FloatVal::from_i128(2)), "z");
        assert_eq!(halves("w"), (one_plus, one_plus), "w");
        assert_eq!(halves("q"), (one_plus, FloatVal::ZERO), "q");
        assert_eq!(halves("s"), (one_minus, one), "s");
        let (re, im) = halves("r");
        assert_eq!(
            re,
            one.add(half_tiny, crate::float::FpFormat::Binary128),
            "r re"
        );
        assert_eq!(im, half_tiny.negated(), "r im");
    }
}

/// A GNU complex integer folds as integers, by the algorithm the run-time
/// lowering uses: Smith's method, truncating at every step, which is why
/// `(-9 + 38i) / (5 + 6i)` is `6 + 1i` in gcc and here, where the exact
/// quotient is `3 + 4i`. The old fold went through `f64` and the textbook
/// formula, and answered `3 + 4i`.
#[test]
fn test_static_complex_integer_folds_as_integers() {
    let src = "static _Complex int p = (1 + 2i) * (1 + 2i);\n\
               static _Complex int q = (-9 + 38i) / (5 + 6i);\n\
               static _Complex unsigned u = (4000000000u + 0i) / (2u + 0i);\n\
               static _Complex int t = (_Complex int)(2.5 + 3.5i);\n";
    let module = linearize_source(src, &Target::host());
    let halves = |name: &str| match complex_halves(&module, name) {
        (crate::ir::Initializer::Int(re), crate::ir::Initializer::Int(im)) => (re, im),
        other => panic!("{name}: expected integer halves, got {other:?}"),
    };
    assert_eq!(halves("p"), (-3, 4));
    assert_eq!(halves("q"), (6, 1));
    assert_eq!(halves("u"), (2_000_000_000, 0));
    assert_eq!(halves("t"), (2, 3));
}

/// Complex constants in the scalar shapes a static initializer takes, each
/// value gcc's on both targets: `==`/`!=` against the common complex type,
/// `__real__`/`__imag__`, `!`, `&&`/`||`, a condition, and conversion to a
/// real, an integer or `_Bool` type.
#[test]
fn test_static_initializer_complex_scalar_shapes() {
    let src = "int f = (_Complex float)(0.5) == 0.5;\n\
               int f2 = (1.0 + 2.0i) != (1.0 + 2.0i);\n\
               int f3 = (1.0f + 2.0fi) == (1.0L + 2.0iL);\n\
               int f4 = 3 == (3 + 0i);\n\
               int f5 = (0.1f + 0i) == 0.1;\n\
               int f6 = __builtin_complex(__builtin_nan(\"\"), 0.0) == __builtin_complex(__builtin_nan(\"\"), 0.0);\n\
               double r1 = __real__ (1.5 + 2.5i);\n\
               double r2 = __imag__ (1.5 + 2.5i);\n\
               int r3 = __real__ (3 + 4i);\n\
               int r4 = __imag__ (3 + 4i);\n\
               double r5 = __imag__ 2.5;\n\
               int n1 = !(0.0 + 0.0i);\n\
               int n2 = !(0.0 + 1.0i);\n\
               int n3 = !0.5;\n\
               int l1 = (0.0 + 1.0i) && 1;\n\
               int l2 = (0.0 + 0.0i) || 0.5;\n\
               int c1 = (0.0 + 1.0i) ? 7 : 8;\n\
               int c2 = (0.0 + 0.0i) ? 7 : 8;\n\
               double k1 = (double)(1.5 + 2.5i);\n\
               int k2 = (int)(3.75 + 2.5i);\n\
               _Bool k3 = (_Bool)(0.0 + 1.0i);\n\
               _Bool k4 = (_Bool)(0.0 + 0.0i);\n\
               double k5 = 1.25 + 2.0i;\n\
               int k6 = 3.75 + 2.5i;\n\
               long long k9 = (long long)(0x1p62L + 1.0L + 1.0iL);\n\
               int k10 = (int)(5 + 6i);\n\
               double k11 = (double)(5 + 6i);\n\
               double q1 = (1 ? 2.5 : 3) * 2;\n\
               double q2 = __real__ ((0.0 + 1.0i) ? (4.0 + 5.0i) : 0);\n";
    for target in [
        Target::new(crate::target::Arch::X86_64, crate::target::Os::Linux),
        Target::new(crate::target::Arch::Aarch64, crate::target::Os::Linux),
    ] {
        let module = linearize_source(src, &target);
        let int = |name: &str| match global_init(&module, name) {
            crate::ir::Initializer::Int(v) => *v,
            other => panic!("{name}: expected an integer, got {other:?}"),
        };
        let float = |name: &str| match global_init(&module, name) {
            crate::ir::Initializer::Float(v) => v.to_f64(),
            other => panic!("{name}: expected a float, got {other:?}"),
        };
        for (name, want) in [
            ("f", 1),
            ("f2", 0),
            ("f3", 1),
            ("f4", 1),
            ("f5", 0),
            ("f6", 0),
            ("r3", 3),
            ("r4", 4),
            ("n1", 1),
            ("n2", 0),
            ("n3", 0),
            ("l1", 1),
            ("l2", 1),
            ("c1", 7),
            ("c2", 8),
            ("k2", 3),
            ("k3", 1),
            ("k4", 0),
            ("k6", 3),
            ("k9", (1 << 62) + 1),
            ("k10", 5),
        ] {
            assert_eq!(int(name), want, "{name}");
        }
        for (name, want) in [
            ("r1", 1.5),
            ("r2", 2.5),
            ("r5", 0.0),
            ("k1", 1.5),
            ("k5", 1.25),
            ("k11", 5.0),
            ("q1", 5.0),
            ("q2", 4.0),
        ] {
            assert_eq!(float(name), want, "{name}");
        }
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

/// A complex `*` calls `__mul?c3` by the ABI's classification of the complex
/// type, as any other call returning it would be: on x86-64 a
/// `_Float128 _Complex` is MEMORY class, so `__multc3` is handed a hidden
/// pointer ahead of its four halves and the function itself returns through
/// one; `long double _Complex` is COMPLEX_X87 and comes back in st(0)/st(1),
/// with no hidden pointer. On aarch64 `_Float128` is `long double` and its
/// complex an HFA, so neither has one.
#[test]
fn test_complex_multiply_routine_follows_the_return_class() {
    use crate::target::{Arch, Os};
    let cases = [
        (Arch::X86_64, "_Float128", "__multc3", true),
        (Arch::X86_64, "long double", "__mulxc3", false),
        (Arch::Aarch64, "_Float128", "__multc3", false),
    ];
    for (arch, base, routine, sret) in cases {
        let src = format!(
            "{base} _Complex m({base} _Complex x, {base} _Complex y) {{ return x * y; }}\n"
        );
        let module = linearize_source(&src, &Target::new(arch, Os::Linux));
        let m = module.functions.iter().find(|f| f.name == "m").unwrap();
        let has_sret = m
            .pseudos
            .iter()
            .any(|p| p.kind == PseudoKind::Arg(0) && p.name.as_deref() == Some("__sret"));
        assert_eq!(
            has_sret, sret,
            "{base} on {arch:?}: the function's own return"
        );
        let call = m
            .blocks
            .iter()
            .flat_map(|b| &b.insns)
            .find(|i| i.op == Opcode::Call && i.func_name.as_deref() == Some(routine))
            .unwrap_or_else(|| panic!("{base} on {arch:?}: no {routine} call"));
        assert_eq!(
            call.returns_via_sret(),
            sret,
            "{base} on {arch:?}: {routine}"
        );
        assert_eq!(
            call.src.len(),
            4 + usize::from(sret),
            "{base} on {arch:?}: {routine} takes the four halves, after any hidden pointer"
        );
    }
}

/// A known library call keeps its tag in the IR under whatever name the
/// program gave it: `strstr` renamed by an asm label is called as
/// `my_strstr` and is still `strstr` to the optimizer. A call through a
/// pointer to it is not tagged.
#[test]
fn test_known_call_keeps_its_tag_under_an_asm_label() {
    let src = "char *strstr(const char *, const char *) __asm__(\"my_strstr\");\n\
               char *f(const char *s) { return strstr(s, \"o\"); }\n\
               char *g(const char *s) { return (&strstr)(s, \"o\"); }\n";
    let module = linearize_source(src, &Target::new(Arch::X86_64, Os::Linux));
    let call_in = |name: &str| {
        insns_of(&module, name)
            .into_iter()
            .find(|i| i.op == Opcode::Call)
            .unwrap_or_else(|| panic!("{name}: no call"))
    };
    let f = call_in("f");
    let label = crate::arch::lir::verbatim("my_strstr");
    assert_eq!(f.func_name.as_deref(), Some(label.as_str()));
    assert_eq!(f.known, Some(crate::parse::ast::LibFn::Strstr));
    assert_eq!(call_in("g").known, None);
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

/// A construct that builds control flow of its own, reached where control
/// cannot arrive.
///
/// `current_bb` is `None` after a `goto` and before a `switch`'s first
/// `case`, and `emit` quietly drops what it is handed there. `emit_two_way`
/// cannot be dropped that way -- it has to hang three blocks off something --
/// and it took `self.current_bb.unwrap()`, so `sqrt`, whose errno check is a
/// two-way, panicked the compiler outright on the dead statement after a
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
        cmp.size, 64,
        "the compare is sized by its operand, not _Bool"
    );
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

/// x86-64 Linux, whose x87 `long double` holds `0x1p62L + 1.0L` exactly --
/// a test about that names the target rather than taking the host's, since
/// on an arm64 Mac `long double` is `double`.
fn x86_64_linux() -> Target {
    Target::new(crate::target::Arch::X86_64, crate::target::Os::Linux)
}

// CFG consistency

/// Every block's recorded successors are exactly the blocks its terminator
/// names, and `parents` is the inverse of `children`.
///
/// Returns a description of the first inconsistency, or `None`.
fn cfg_inconsistency(func: &Function) -> Option<String> {
    use std::collections::HashSet;

    for bb in &func.blocks {
        let children: HashSet<BasicBlockId> = bb.children.iter().copied().collect();
        if children.len() != bb.children.len() {
            return Some(format!("{}: duplicate edge in children", bb.id));
        }

        let named = match bb.insns.last() {
            Some(last) if last.op.is_terminator() => crate::ir::propagate::terminator_targets(last),
            // A block with no terminator falls through to nothing the CFG can
            // name; `children` must then be empty too.
            _ => HashSet::new(),
        };

        if named != children {
            return Some(format!(
                "{}: terminator names {:?} but children are {:?}",
                bb.id,
                sorted_ids(&named),
                sorted_ids(&children),
            ));
        }
    }

    // `parents` is the inverse of `children`.
    let mut expected: std::collections::HashMap<BasicBlockId, HashSet<BasicBlockId>> =
        std::collections::HashMap::new();
    for bb in &func.blocks {
        for child in &bb.children {
            expected.entry(*child).or_default().insert(bb.id);
        }
    }
    for bb in &func.blocks {
        let have: HashSet<BasicBlockId> = bb.parents.iter().copied().collect();
        let want = expected.remove(&bb.id).unwrap_or_default();
        if have != want {
            return Some(format!(
                "{}: parents are {:?} but {:?} name it as a successor",
                bb.id,
                sorted_ids(&have),
                sorted_ids(&want),
            ));
        }
    }

    None
}

fn sorted_ids(s: &std::collections::HashSet<BasicBlockId>) -> Vec<u32> {
    let mut v: Vec<u32> = s.iter().map(|b| b.0).collect();
    v.sort_unstable();
    v
}

/// A `for` post-expression that splits the block still links the back edge from
/// the block the branch was emitted into.
///
/// `&&`, `||` and `?:` leave `current_bb` on their merge block, so
/// `link_bb(post_bb, cond_bb)` recorded an edge out of a block that no longer
/// holds the terminator -- the loop's back edge went missing from the CFG while
/// a merge block gained an unrecorded one. Both `for` arms had it: the one in
/// `linearize_for` and its copy in the switch-body walker.
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
/// `if`, `?:`, `goto`, `break`/`continue` and the loop lowerings' copies in the
/// switch-body walker.
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

/// Every access the linearizer emits to a `volatile` object carries the
/// marker, including the one that has no variable to ask.
///
/// `LocalVar::is_volatile` and `GlobalFacts::is_volatile` answer for a named
/// object, and for `*p` with a `volatile int *p` there is none: `p` is an
/// ordinary pointer. So DCE saw no reason to keep the read and deleted every
/// discarded `volatile` access from `-O1` up.
#[test]
fn test_volatile_accesses_carry_the_marker() {
    let target = Target::host();
    let src = "volatile int g;\n\
               volatile int *vp;\n\
               int plain;\n\
               int *pp;\n\
               volatile int arr[4];\n\
               struct T { volatile int a; };\n\
               struct T t;\n\
               void read_named(void) { g; }\n\
               void read_via_ptr(void) { *vp; }\n\
               void write_named(void) { g = 1; }\n\
               void write_via_ptr(void) { *vp = 1; }\n\
               void read_element(void) { arr[2]; }\n\
               void read_member(void) { t.a; }\n\
               void read_plain(void) { plain; }\n\
               void read_plain_ptr(void) { *pp; }\n";
    let module = linearize_source(src, &target);

    let accesses = |name: &str| -> Vec<(Opcode, bool)> {
        module
            .functions
            .iter()
            .find(|f| f.name == name)
            .unwrap_or_else(|| panic!("function {name}"))
            .blocks
            .iter()
            .flat_map(|bb| bb.insns.iter())
            .filter(|i| matches!(i.op, Opcode::Load | Opcode::Store))
            .map(|i| (i.op, i.is_volatile_access()))
            .collect()
    };

    // A named volatile object: one marked access each way.
    assert_eq!(accesses("read_named"), vec![(Opcode::Load, true)]);
    assert_eq!(accesses("write_named"), vec![(Opcode::Store, true)]);

    // Through a pointer *to* volatile, the qualifier is on the pointee, so
    // reading `vp` itself is plain and the access through it is volatile.
    assert_eq!(
        accesses("read_via_ptr"),
        vec![(Opcode::Load, false), (Opcode::Load, true)]
    );
    assert_eq!(
        accesses("write_via_ptr"),
        vec![(Opcode::Load, false), (Opcode::Store, true)]
    );

    // The qualifier reaches through an array's element type and a member's
    // own type.
    assert_eq!(accesses("read_element"), vec![(Opcode::Load, true)]);
    assert_eq!(accesses("read_member"), vec![(Opcode::Load, true)]);

    // And nothing unqualified is marked -- the marker that says "keep this"
    // is worth nothing if it is on every access.
    assert_eq!(accesses("read_plain"), vec![(Opcode::Load, false)]);
    assert_eq!(
        accesses("read_plain_ptr"),
        vec![(Opcode::Load, false), (Opcode::Load, false)]
    );
}

/// A member of a `volatile` object is itself volatile (C17 6.5.2.3p3/p4), so
/// every access to one carries the marker.
///
/// The reverse direction -- a `volatile` member of a plain object -- always
/// worked, because there the member's own declared type carries the qualifier.
/// This is the other one: the qualifier is on the *object*, and
/// `find_member` answers with the member's declared type, which cannot show it.
/// So the load was unmarked and DCE deleted it from `-O1` up.
#[test]
fn test_a_member_of_a_volatile_object_carries_the_marker() {
    let target = Target::host();
    let src = "struct S { int a; int b; };\n\
               struct N { struct S in; };\n\
               typedef volatile struct S VS;\n\
               volatile struct S vs;\n\
               volatile struct S *vp;\n\
               volatile struct S vsa[4];\n\
               volatile struct N vn;\n\
               VS vt;\n\
               struct S plain;\n\
               struct S *pp;\n\
               void read_direct(void) { vs.a; }\n\
               void read_arrow(void) { vp->a; }\n\
               void read_element(void) { vsa[2].a; }\n\
               void read_nested(void) { vn.in.a; }\n\
               void read_typedef(void) { vt.a; }\n\
               void write_direct(void) { vs.a = 1; }\n\
               void read_plain(void) { plain.a; }\n\
               void read_plain_arrow(void) { pp->a; }\n";
    let module = linearize_source(src, &target);

    let accesses = |name: &str| -> Vec<(Opcode, bool)> {
        module
            .functions
            .iter()
            .find(|f| f.name == name)
            .unwrap_or_else(|| panic!("function {name}"))
            .blocks
            .iter()
            .flat_map(|bb| bb.insns.iter())
            .filter(|i| matches!(i.op, Opcode::Load | Opcode::Store))
            .map(|i| (i.op, i.is_volatile_access()))
            .collect()
    };

    // Every spelling of "the object is volatile": directly, through a pointer
    // to volatile, through an array's element type, through a nested member
    // whose own type is qualified by the object above it, and through a
    // typedef that carries the qualifier.
    assert_eq!(accesses("read_direct"), vec![(Opcode::Load, true)]);
    assert_eq!(accesses("read_element"), vec![(Opcode::Load, true)]);
    assert_eq!(accesses("read_nested"), vec![(Opcode::Load, true)]);
    assert_eq!(accesses("read_typedef"), vec![(Opcode::Load, true)]);
    assert_eq!(accesses("write_direct"), vec![(Opcode::Store, true)]);
    // `volatile struct S *vp` qualifies the pointee, so reading `vp` itself is
    // an ordinary load and the access through it is the volatile one.
    assert_eq!(
        accesses("read_arrow"),
        vec![(Opcode::Load, false), (Opcode::Load, true)]
    );

    // The control: an unqualified object's member is not marked, or the marker
    // would mean nothing.
    assert_eq!(accesses("read_plain"), vec![(Opcode::Load, false)]);
    assert_eq!(
        accesses("read_plain_arrow"),
        vec![(Opcode::Load, false), (Opcode::Load, false)]
    );
}

/// A `volatile` bit-field access is marked although the access is of the
/// carrier.
///
/// `emit_bitfield_load`/`_store` read and write a storage unit whose type is
/// an unqualified integer, so no marker can be derived from the instruction's
/// own type. `mark_volatile_access` preserves one the emitter sets, and this is
/// the case it exists for.
#[test]
fn test_a_volatile_bitfield_access_carries_the_marker() {
    let target = Target::host();
    let src = "struct B { volatile unsigned f : 3; unsigned g : 5; };\n\
               struct B b;\n\
               volatile struct B vb;\n\
               void read_field(void) { b.f; }\n\
               void read_object(void) { vb.g; }\n\
               void write_object(void) { vb.g = 1; }\n\
               void read_plain(void) { b.g; }\n\
               void write_plain(void) { b.g = 1; }\n";
    let module = linearize_source(src, &target);

    let accesses = |name: &str| -> Vec<(Opcode, bool)> {
        module
            .functions
            .iter()
            .find(|f| f.name == name)
            .unwrap_or_else(|| panic!("function {name}"))
            .blocks
            .iter()
            .flat_map(|bb| bb.insns.iter())
            .filter(|i| matches!(i.op, Opcode::Load | Opcode::Store))
            .map(|i| (i.op, i.is_volatile_access()))
            .collect()
    };

    // Both spellings: the field declared `volatile`, and an ordinary field of
    // a `volatile` object.
    assert_eq!(accesses("read_field"), vec![(Opcode::Load, true)]);
    assert_eq!(accesses("read_object"), vec![(Opcode::Load, true)]);
    // A bit-field store is a read-modify-write of the carrier, and both halves
    // of it are observable.
    assert_eq!(
        accesses("write_object"),
        vec![(Opcode::Load, true), (Opcode::Store, true)]
    );

    // The controls.
    assert_eq!(accesses("read_plain"), vec![(Opcode::Load, false)]);
    assert_eq!(
        accesses("write_plain"),
        vec![(Opcode::Load, false), (Opcode::Store, false)]
    );
}

/// A conditional may not speculate a `volatile` member, in either spelling.
///
/// `Select` is the branchless form, and reaching it means both arms were
/// evaluated. C17 6.5.15p4 evaluates only one of them, and 5.1.2.3 makes each
/// volatile read an observable event -- so the arms may only collapse when
/// both are pure. `is_pure_expr` asked whether the *base* was pure, which a
/// named object always is.
#[test]
fn test_a_volatile_member_is_not_speculated() {
    let target = Target::host();
    let src = "struct V { volatile unsigned status; unsigned other; };\n\
               struct P { unsigned one; unsigned other; };\n\
               struct V v;\n\
               volatile struct P vp;\n\
               struct P p;\n\
               unsigned member_is_volatile(int c) { return c ? v.status : v.other; }\n\
               unsigned object_is_volatile(int c) { return c ? vp.one : vp.other; }\n\
               unsigned all_plain(int c) { return c ? p.one : p.other; }\n";
    let module = linearize_source(src, &target);

    let selects = |name: &str| -> usize {
        module
            .functions
            .iter()
            .find(|f| f.name == name)
            .unwrap_or_else(|| panic!("function {name}"))
            .blocks
            .iter()
            .flat_map(|bb| bb.insns.iter())
            .filter(|i| i.op == Opcode::Select)
            .count()
    };

    assert_eq!(
        selects("member_is_volatile"),
        0,
        "a volatile member may not be read on the path that did not select it"
    );
    assert_eq!(
        selects("object_is_volatile"),
        0,
        "a member of a volatile object is volatile (C17 6.5.2.3p3)"
    );
    assert_eq!(
        selects("all_plain"),
        1,
        "two ordinary member reads are pure and may still collapse"
    );
}

/// A `volatile` access keeps the object it reaches in memory: promotion would
/// rewrite the access into a register `Copy`, and the access must happen.
///
/// The variable need not itself be volatile. `ssa` tests
/// `LocalVar::is_volatile`, which answers no here -- the qualifier is on the
/// access alone.
#[test]
fn test_volatile_access_to_a_plain_local_blocks_promotion() {
    let target = Target::host();
    let src = "int f(void) { int a = 1; return *(volatile int *)&a; }\n";
    let module = linearize_source(src, &target);
    let mut func = module
        .functions
        .iter()
        .find(|f| f.name == "f")
        .expect("f")
        .clone();

    let volatile_loads = |func: &Function| -> usize {
        func.blocks
            .iter()
            .flat_map(|bb| bb.insns.iter())
            .filter(|i| i.is_volatile_access())
            .count()
    };
    assert_eq!(volatile_loads(&func), 1, "the cast qualifies the access");

    let types = TypeTable::new(&target);
    crate::ir::ssa::ssa_convert(&mut func, &types);
    assert_eq!(
        volatile_loads(&func),
        1,
        "SSA promotion turned a volatile access into a register copy"
    );
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
/// such a block is *removed* again by `dce::remove_unreachable_blocks`, and a
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
