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
