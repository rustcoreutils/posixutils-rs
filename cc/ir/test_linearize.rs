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
    AssignOp, BinaryOp, BlockItem, Declaration, ExprKind, ExternalDecl, ForInit, FunctionDef,
    InitDeclarator, InitElement, ParamStyle, Parameter, Stmt,
};
use crate::strings::StringTable;
use crate::symbol::Symbol;
use crate::target::Target;
use crate::types::{CompositeType, MemberAlign, StructMember, Type, TypeTable};

/// Create a default position for test code
pub(super) fn test_pos() -> Position {
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
pub(super) struct TestContext {
    pub(super) strings: StringTable,
    pub(super) types: TypeTable,
    pub(super) symbols: SymbolTable,
}

impl TestContext {
    pub(super) fn new() -> Self {
        Self {
            strings: StringTable::new(),
            types: TypeTable::new(&Target::host()),
            symbols: SymbolTable::new(),
        }
    }

    /// Intern a string and create a variable symbol for it, returning the SymbolId.
    pub(super) fn var(&mut self, name: &str, typ: TypeId) -> SymbolId {
        let name_id = self.strings.intern(name);
        let sym = Symbol::variable(name_id, typ, self.symbols.depth());
        self.symbols.declare(sym).unwrap()
    }

    /// Intern a string and return the StringId (for function names etc.)
    pub(super) fn str(&mut self, name: &str) -> StringId {
        self.strings.intern(name)
    }

    pub(super) fn int_type(&self) -> TypeId {
        self.types.int_id
    }

    /// Create a pointer type
    pub(super) fn ptr(&self, pointee: TypeId) -> TypeId {
        self.types.pointer_to(pointee)
    }

    /// Linearize with this context
    pub(super) fn linearize(&self, tu: &TranslationUnit) -> Module {
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

/// Whether any instruction of `module` is one of `ops`: the structural form
/// of asking a dump whether an opcode's name appears in it, which a longer
/// name or an operand containing it would also answer.
pub(super) fn has_op(module: &Module, ops: &[Opcode]) -> bool {
    module
        .functions
        .iter()
        .flat_map(|f| &f.blocks)
        .flat_map(|b| &b.insns)
        .any(|i| ops.contains(&i.op))
}

pub(super) fn test_linearize(
    tu: &TranslationUnit,
    types: &TypeTable,
    strings: &StringTable,
) -> Module {
    let symbols = SymbolTable::new();
    let target = Target::host();
    linearize(tu, &symbols, types, strings, &target, false, true)
}

pub(super) fn make_simple_func(name: StringId, body: Stmt, types: &TypeTable) -> FunctionDef {
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
        has_op(&module, &[Opcode::Store]),
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
    assert!(
        has_op(&module, &[Opcode::Add]),
        "IR should have add for a + h: {}",
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
    assert!(has_op(&module, &[Opcode::Ret]));
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
    assert!(!has_op(&module, &[Opcode::Cbr]), "{ir}");
    let rets = module.functions[0]
        .blocks
        .iter()
        .flat_map(|bb| bb.insns.iter())
        .filter(|i| i.op == Opcode::Ret)
        .count();
    assert_eq!(rets, 1, "only the taken arm's return is emitted\n{ir}");
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
    assert!(has_op(&module, &[Opcode::Mul]));
    assert!(has_op(&module, &[Opcode::Add]));
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
    assert!(has_op(&module, &[Opcode::Add]));
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
    assert!(has_op(&module, &[Opcode::Call]));
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
    assert!(has_op(&module, &[Opcode::SetLt]));
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
        has_op(&module, &[Opcode::SetB]),
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
    assert!(has_op(&module, &[Opcode::Ret]));
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
pub(super) fn linearize_no_ssa(
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
        has_op(&module, &[Opcode::Store]),
        "Should have store instruction before SSA: {}",
        ir
    );
    assert!(
        has_op(&module, &[Opcode::Load]),
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
        has_op(&module, &[Opcode::Phi]),
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
    assert!(
        has_op(&module, &[Opcode::Phi]),
        "Loop should have phi node: {}",
        ir
    );
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
        has_op(&module, &[Opcode::Cbr]),
        "Short-circuit AND should have conditional branch: {}",
        ir
    );
    assert!(
        has_op(&module, &[Opcode::Phi]),
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
        has_op(&module, &[Opcode::Cbr]),
        "Short-circuit OR should have conditional branch: {}",
        ir
    );
    assert!(
        has_op(&module, &[Opcode::Phi]),
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
        has_op(&module, &[Opcode::Select]),
        "Pure ternary should use select instruction: {}",
        ir
    );
    // Should NOT have phi (that's for impure ternary)
    assert!(
        !has_op(&module, &[Opcode::Phi]),
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
        has_op(&module, &[Opcode::Phi]),
        "Impure ternary should use phi node: {}",
        ir
    );
    // Should have conditional branch
    assert!(
        has_op(&module, &[Opcode::Cbr]),
        "Impure ternary should use conditional branch: {}",
        ir
    );
    // Should NOT use select (that's for pure ternary)
    assert!(
        !has_op(&module, &[Opcode::Select]),
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
        has_op(&module, &[Opcode::Phi]),
        "Ternary with assignment should use phi: {}",
        ir
    );
    assert!(
        !has_op(&module, &[Opcode::Select]),
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
        has_op(&module, &[Opcode::Phi]),
        "Ternary with post-inc/dec should use phi: {}",
        ir
    );
    assert!(
        !has_op(&module, &[Opcode::Select]),
        "Ternary with post-inc/dec should NOT use select: {}",
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
                align: MemberAlign::NATURAL,
            },
            StructMember {
                name: y_id,
                typ: int_type,
                offset: 4, // Second int at offset 4 bytes
                bit_offset: None,
                bit_width: None,
                access_bytes: None,
                align: MemberAlign::NATURAL,
            },
        ],
        enum_constants: vec![],
        size: 8,  // 2 ints = 8 bytes
        align: 4, // int alignment
        member_align: 4,
        is_complete: true,
        transparent: false,
        anon_id: None,
        forward_of: None,
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
        has_op(&module, &[Opcode::Store]),
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

/// Parse `src` and linearize it for `target`.
pub(crate) fn linearize_source(src: &str, target: &Target) -> Module {
    linearize_source_with_types(src, target).0
}

/// [`linearize_source`], also handing back the type table the module's type
/// ids index.
pub(crate) fn linearize_source_with_types(src: &str, target: &Target) -> (Module, TypeTable) {
    linearize_source_under(src, target, Default::default())
}

/// [`linearize_source_with_types`], with library builtins evaluated as
/// `policy` says.
pub(super) fn linearize_source_under(
    src: &str,
    target: &Target,
    policy: crate::parse::LibraryCallPolicy,
) -> (Module, TypeTable) {
    linearize_source_trapping(src, target, policy, true)
}

/// [`linearize_source_under`], with `-f[no-]trapping-math` as given.
pub(super) fn linearize_source_trapping(
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

/// Every instruction of the function `name` in `module`.
pub(super) fn insns_of<'m>(module: &'m Module, name: &str) -> Vec<&'m Instruction> {
    let f = module.functions.iter().find(|f| f.name == name).unwrap();
    f.blocks.iter().flat_map(|bb| bb.insns.iter()).collect()
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

/// x86-64 Linux, whose x87 `long double` holds `0x1p62L + 1.0L` exactly --
/// a test about that names the target rather than taking the host's, since
/// on an arm64 Mac `long double` is `double`.
pub(super) fn x86_64_linux() -> Target {
    Target::new(crate::target::Arch::X86_64, crate::target::Os::Linux)
}
