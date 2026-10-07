//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Linearizer tests for initializers: arrays, strings and wide strings,
// designators, bit-fields, compound literals, and static initializers.
//

use super::test_linearize::{
    has_op, insns_of, linearize_no_ssa, linearize_source, test_linearize, test_pos, x86_64_linux,
    TestContext,
};
use super::*;
use crate::parse::ast::{
    AssignOp, BlockItem, Declaration, Designator, ExprKind, ExternalDecl, FunctionDef,
    InitDeclarator, InitElement, ParamStyle, Parameter, Stmt, UnaryOp,
};
use crate::strings::StringTable;
use crate::target::{Arch, Os, Target};
use crate::types::{CompositeType, MemberAlign, StructMember, Type, TypeModifiers, TypeTable};

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
        storage_class: TypeModifiers::empty(),
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
        has_op(&module, &[Opcode::Store]),
        "Array element assignment should produce store: {}",
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
                    cleanup: None,
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
        storage_class: TypeModifiers::empty(),
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
                    cleanup: None,
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
        storage_class: TypeModifiers::empty(),
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
        has_op(&module, &[Opcode::Store]),
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
        storage_class: TypeModifiers::empty(),
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
        storage_class: TypeModifiers::empty(),
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
        has_op(&module, &[Opcode::Select]),
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
        storage_class: TypeModifiers::empty(),
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
        storage_class: TypeModifiers::empty(),
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
            cleanup: None,
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
            cleanup: None,
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
        storage_class: TypeModifiers::empty(),
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
                align: MemberAlign::NATURAL,
            },
            StructMember {
                name: b_id,
                typ: int_type,
                offset: 4,
                bit_offset: None,
                bit_width: None,
                access_bytes: None,
                align: MemberAlign::NATURAL,
            },
            StructMember {
                name: c_id,
                typ: int_type,
                offset: 8,
                bit_offset: None,
                bit_width: None,
                access_bytes: None,
                align: MemberAlign::NATURAL,
            },
            StructMember {
                name: d_id,
                typ: int_type,
                offset: 12,
                bit_offset: None,
                bit_width: None,
                access_bytes: None,
                align: MemberAlign::NATURAL,
            },
        ],
        enum_constants: vec![],
        size: 16,
        align: 4,
        member_align: 4,
        is_complete: true,
        transparent: false,
        reverse_order: false,
        anon_id: None,
        tag_type: None,
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
                    cleanup: None,
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
        storage_class: TypeModifiers::empty(),
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
                    cleanup: None,
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
        storage_class: TypeModifiers::empty(),
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
            align: MemberAlign::NATURAL,
        },
        StructMember {
            name: y_id,
            typ: ctx.int_type(),
            offset: 4,
            bit_offset: None,
            bit_width: None,
            access_bytes: None,
            align: MemberAlign::NATURAL,
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
        reverse_order: false,
        anon_id: None,
        tag_type: None,
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
            align: MemberAlign::NATURAL,
        },
        StructMember {
            name: z_id,
            typ: ctx.int_type(),
            offset: 8,
            bit_offset: None,
            bit_width: None,
            access_bytes: None,
            align: MemberAlign::NATURAL,
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
        reverse_order: false,
        anon_id: None,
        tag_type: None,
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
                    cleanup: None,
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
        storage_class: TypeModifiers::empty(),
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
        align: MemberAlign::NATURAL,
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
        reverse_order: false,
        anon_id: None,
        tag_type: None,
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
                    cleanup: None,
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
        storage_class: TypeModifiers::empty(),
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
                    cleanup: None,
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
        storage_class: TypeModifiers::empty(),
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
            align: MemberAlign::NATURAL,
        },
        StructMember {
            name: StringId::EMPTY,
            typ: int_type,
            offset: 4,
            bit_offset: Some(0),
            bit_width: Some(8),
            access_bytes: Some(4),
            align: MemberAlign::NATURAL,
        },
        StructMember {
            name: b_id,
            typ: int_type,
            offset: 8,
            bit_offset: None,
            bit_width: None,
            access_bytes: None,
            align: MemberAlign::NATURAL,
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
        reverse_order: false,
        anon_id: None,
        tag_type: None,
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
                    cleanup: None,
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
        storage_class: TypeModifiers::empty(),
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
            align: MemberAlign::NATURAL,
        },
        StructMember {
            name: a_id,
            typ: int_type,
            offset: 0,
            bit_offset: None,
            bit_width: None,
            access_bytes: None,
            align: MemberAlign::NATURAL,
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
        reverse_order: false,
        anon_id: None,
        tag_type: None,
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
                    cleanup: None,
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
        storage_class: TypeModifiers::empty(),
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
            align: MemberAlign::NATURAL,
        },
        StructMember {
            name: compact_id,
            typ: int_type,
            offset: 0,
            bit_offset: Some(3),
            bit_width: Some(1),
            access_bytes: Some(1),
            align: MemberAlign::NATURAL,
        },
        StructMember {
            name: ascii_id,
            typ: int_type,
            offset: 0,
            bit_offset: Some(4),
            bit_width: Some(1),
            access_bytes: Some(1),
            align: MemberAlign::NATURAL,
        },
        StructMember {
            name: static_alloc_id,
            typ: int_type,
            offset: 0,
            bit_offset: Some(5),
            bit_width: Some(1),
            access_bytes: Some(1),
            align: MemberAlign::NATURAL,
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
            reverse_order: false,
            anon_id: None,
            tag_type: None,
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
            cleanup: None,
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
        storage_class: TypeModifiers::empty(),
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
            align: MemberAlign::NATURAL,
        },
        StructMember {
            name: b_id,
            typ: int_type,
            offset: 0,
            bit_offset: Some(4),
            bit_width: Some(4),
            access_bytes: Some(1),
            align: MemberAlign::NATURAL,
        },
        StructMember {
            name: c_id,
            typ: int_type,
            offset: 1,
            bit_offset: Some(0),
            bit_width: Some(8),
            access_bytes: Some(1),
            align: MemberAlign::NATURAL,
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
        reverse_order: false,
        anon_id: None,
        tag_type: None,
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
                    cleanup: None,
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
        storage_class: TypeModifiers::empty(),
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
            align: MemberAlign::NATURAL,
        },
        StructMember {
            name: b_id,
            typ: ptr_type,
            offset: 8,
            bit_offset: None,
            bit_width: None,
            access_bytes: None,
            align: MemberAlign::NATURAL,
        },
        StructMember {
            name: c_id,
            typ: ptr_type,
            offset: 16,
            bit_offset: None,
            bit_width: None,
            access_bytes: None,
            align: MemberAlign::NATURAL,
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
            reverse_order: false,
            anon_id: None,
            tag_type: None,
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
        storage_class: TypeModifiers::empty(),
        calling_conv: crate::abi::CallingConv::default(),
        param_style: ParamStyle::Prototype,
    };

    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };
    let mut module = ctx.linearize(&tu);

    // The linearizer asks for the zero-init as one `Memset` of the whole
    // literal rather than emitting the stores itself. That is what gives it
    // the same bound as every other block memory operation -- the ladder it
    // used to hand-roll here had none, so `char buf[N] = {0}` unrolled for any
    // N at all.
    let linearized = format!("{}", module.display(&ctx.types));
    assert!(
        linearized.contains("memset"),
        "expected the compound literal's zero-init to be asked for as a memset: {linearized}"
    );

    // `memexpand` turns it back into stores, at every optimization level --
    // `-O0` included, see `opt::optimize_module` -- so run it here and hold
    // the stores to the same account as before.
    for f in &mut module.functions {
        crate::ir::memexpand::run(f, &ctx.types);
    }
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
            align: MemberAlign::NATURAL,
        }],
        enum_constants: vec![],
        size: 16,
        align: 8,
        member_align: 8,
        is_complete: true,
        transparent: false,
        reverse_order: false,
        anon_id: None,
        tag_type: None,
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
                cleanup: None,
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
        storage_class: TypeModifiers::empty(),
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

/// The constant `id` holds in `f`, if it is one -- through the narrowing a
/// conversion to the destination's own type leaves.
fn const_of(module: &Module, name: &str, id: PseudoId) -> Option<i128> {
    let f = module.functions.iter().find(|f| f.name == name).unwrap();
    crate::ir::facts::ConstMap::new(f).get(id)
}

/// Every `Memset` in `f`, as `(fill byte, length)`.
fn memsets_of(module: &Module, name: &str) -> Vec<(Option<i128>, Option<i128>)> {
    insns_of(module, name)
        .iter()
        .filter(|i| i.op == Opcode::Memset)
        .map(|i| {
            (
                const_of(module, name, i.src[1]),
                const_of(module, name, i.src[2]),
            )
        })
        .collect()
}

/// Every `Store` in `f`, as `(offset, width in bits, stored constant)`. A
/// store's `src` is `(address, value)`.
fn stores_of(module: &Module, name: &str) -> Vec<(i64, u32, Option<i128>)> {
    insns_of(module, name)
        .iter()
        .filter(|i| i.op == Opcode::Store)
        .map(|i| (i.offset, i.size, const_of(module, name, i.src[1])))
        .collect()
}

/// Zero-initializing an aggregate is one `Memset` of the whole object, whose
/// length `memexpand` then weighs against `INLINE_LIMIT_BYTES`.
///
/// `emit_aggregate_zero` hand-rolled the same 8/4/2/1 descent
/// `memexpand::block_chunks` already produces, but with **no** upper bound, so
/// `char buf[N] = {0}` emitted one store per chunk for any N: 8 KB cost 2081
/// instructions in the function body and 1 MB did not finish compiling in 25
/// minutes. Asking for the opcode instead makes the bound the shared one and
/// leaves the linearizer with no ladder of its own.
#[test]
fn test_aggregate_zero_is_one_memset_of_the_whole_object() {
    let target = Target::new(Arch::X86_64, Os::Linux);
    // Each declares an object and hands it to `sink` so nothing is dead. The
    // only store left is the one element `{0}` names explicitly; every other
    // byte is the memset's, whatever the object's size.
    for (decl, bytes, explicit) in [
        ("char buf[200] = {0}; sink(buf);", 200, (0, 8, Some(0))),
        ("char buf[7] = {0}; sink(buf);", 7, (0, 8, Some(0))),
        (
            "struct S { int a; char b; } s = {0}; sink(&s);",
            8,
            (0, 32, Some(0)),
        ),
    ] {
        let src = format!("void sink(void *);\nvoid f(void) {{ {decl} }}\n");
        let module = linearize_source(&src, &target);
        assert_eq!(
            memsets_of(&module, "f"),
            vec![(Some(0), Some(bytes))],
            "{decl}: one memset of the whole object and nothing else"
        );
        assert_eq!(
            stores_of(&module, "f"),
            vec![explicit],
            "{decl}: the linearizer emits no chunk ladder of its own"
        );
    }
}

/// `char b[N] = "str"` zero-fills the elements the literal does not reach,
/// and only those.
///
/// C17 6.7.9p21 initializes them as a static object would be. The `InitList`
/// arm of a local declaration calls `emit_aggregate_zero` first; the bare
/// string arm did not, so only the literal's own bytes and one terminator were
/// written. The braced form reaches the array through an initializer list,
/// which is already zeroed whole -- zeroing again there would double the
/// stores at `-O0`, where no `dse` runs to remove them.
#[test]
fn test_a_string_initializer_zero_fills_only_its_tail() {
    let target = Target::new(Arch::X86_64, Os::Linux);

    // Three bytes written -- 'h', 'i', and the terminator -- then five left.
    let bare = linearize_source(
        "void sink(void *);\nvoid f(void) { char b[8] = \"hi\"; sink(b); }\n",
        &target,
    );
    assert_eq!(
        stores_of(&bare, "f"),
        vec![(0, 8, Some(0x68)), (1, 8, Some(0x69)), (2, 8, Some(0))]
    );
    assert_eq!(memsets_of(&bare, "f"), vec![(Some(0), Some(5))]);

    // The braced form is preceded by the whole-object zero, so its tail is
    // already done: one memset of 8, not one of 8 and another of 5.
    let braced = linearize_source(
        "void sink(void *);\nvoid f(void) { char b[8] = {\"hi\"}; sink(b); }\n",
        &target,
    );
    assert_eq!(memsets_of(&braced, "f"), vec![(Some(0), Some(8))]);

    // Exactly as long as the literal: C17 6.7.9p14 drops the terminator, and
    // there is then no tail either.
    let exact = linearize_source(
        "void sink(void *);\nvoid f(void) { char b[2] = \"hi\"; sink(b); }\n",
        &target,
    );
    assert_eq!(
        stores_of(&exact, "f"),
        vec![(0, 8, Some(0x68)), (1, 8, Some(0x69))]
    );
    assert_eq!(memsets_of(&exact, "f"), vec![]);
}

/// A string literal initializing a *nested* array element steps the
/// destination by the element's own width and stops at its capacity.
///
/// This path was a third hand-rolled copy of `store_string_units`. It
/// recognized all four literal kinds and then handled only the narrow one, so
/// a wide element was dropped and left zero; it stepped the destination by raw
/// bytes where a wide element is 2 or 4 bytes wide; and it had no capacity
/// clamp at all, so `char s[1][3] = {"hello"}` stored five bytes into a
/// three-byte object.
#[test]
fn test_a_nested_string_element_keeps_its_stride_and_capacity() {
    let target = Target::new(Arch::X86_64, Os::Linux);

    // `unsigned short` is `char16_t`: two bytes of stride, and the literal
    // reaches the array at all.
    let wide = linearize_source(
        "void sink(void *);\n\
         void f(void) { unsigned short u[2][3] = {u\"ab\", u\"cd\"}; sink(u); }\n",
        &target,
    );
    assert_eq!(
        stores_of(&wide, "f"),
        vec![
            (0, 16, Some(0x61)),
            (2, 16, Some(0x62)),
            (4, 16, Some(0)),
            (6, 16, Some(0x63)),
            (8, 16, Some(0x64)),
            (10, 16, Some(0)),
        ]
    );

    // Five units into a three-byte row: three stored, none past the row, and
    // the terminator dropped with them.
    let over = linearize_source(
        "void sink(void *);\nvoid f(void) { char s[1][3] = {\"hello\"}; sink(s); }\n",
        &target,
    );
    assert_eq!(
        stores_of(&over, "f"),
        vec![(0, 8, Some(0x68)), (1, 8, Some(0x65)), (2, 8, Some(0x6c))]
    );
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

/// A union a whole value initialized holds that value's bytes, so a later
/// designator through *any* member keeps them; one a list initialized agrees
/// only with the member the list named. Recording a value forgets what was
/// said before inside it, and a later member record inside wins.
#[test]
fn test_a_value_initialized_union_agrees_with_any_member() {
    let types = TypeTable::new(&Target::host());
    let u = types.int_id; // stands in for the union type; only identity matters
    let mut value = UnionMembers::default();
    value.record(0, u, 1);
    value.record_value(0..8);
    let mut named = UnionMembers::default();
    named.record(0, u, 0);
    assert_eq!(UnionFold::new(&value, &named, 0).agreed(u), Some(0));

    let mut listed = UnionMembers::default();
    listed.record(0, u, 1);
    assert_eq!(UnionFold::new(&listed, &named, 0).agreed(u), None);
    named.record(0, u, 1);
    assert_eq!(UnionFold::new(&listed, &named, 0).agreed(u), Some(1));

    // Discarding part of the value's bytes leaves nothing to agree with.
    value.clear_range(0..4);
    let mut named0 = UnionMembers::default();
    named0.record(0, u, 0);
    assert_eq!(UnionFold::new(&value, &named0, 0).agreed(u), None);
}

/// An address constant converted to `_Bool` -- implicitly, by a cast, or
/// through a pointer cast -- is true, never the address's low byte; and the
/// fold is the value of a wider integer the cast initializes.
#[test]
fn test_address_constant_to_bool_is_true() {
    let src = "int arr[4];\nlong h(void);\n\
               _Bool a = arr, b = h, c = \"x\", d = (_Bool)arr, e = (_Bool)(char *)arr;\n\
               _Bool z = (_Bool)(void *)0;\n\
               int n = (_Bool)arr;\n";
    let module = linearize_source(src, &Target::host());
    for name in ["a", "b", "c", "d", "e", "n"] {
        assert!(
            matches!(global_init(&module, name), Initializer::Int(1)),
            "{name}: {:?}",
            global_init(&module, name)
        );
    }
    assert!(global_init(&module, "z").is_all_zero());
}

/// An element or member of a `const` object folds to its value in a static
/// initializer, not to its address.
#[test]
fn test_const_subobject_folds_to_its_value() {
    let src = "struct P { int x; double d; };\n\
               const int a[2] = {1, 2};\n\
               const struct P p = {4, 2.5};\n\
               int w = a[1];\nlong x = p.x;\ndouble d = p.d;\n";
    let module = linearize_source(src, &Target::host());
    assert!(matches!(global_init(&module, "w"), Initializer::Int(2)));
    assert!(matches!(global_init(&module, "x"), Initializer::Int(4)));
    assert!(
        matches!(global_init(&module, "d"), Initializer::Float(v) if v.to_f64() == 2.5),
        "{:?}",
        global_init(&module, "d")
    );
}

/// An object's definition carries the attributes of every declaration of it,
/// before or after: CPython's `PyAPI_DATA` puts `visibility("default")` on
/// the `extern` declaration and none on the definition.
#[test]
fn test_object_definition_inherits_declaration_attrs() {
    let module = linearize_source(
        "extern __attribute__((visibility(\"default\"))) int a;\nint a = 1;\n\
         int b = 2;\nextern int b __attribute__((visibility(\"protected\")));\n\
         extern __attribute__((weak)) int c;\nextern __attribute__((visibility(\"hidden\"))) int c;\nint c;\n",
        &Target::host(),
    );
    let attrs = |name: &str| {
        module
            .globals
            .iter()
            .find(|g| g.name == name)
            .map(|g| g.symbol_attrs.clone())
            .expect("defined")
    };
    assert_eq!(attrs("a").visibility.as_deref(), Some("default"));
    assert_eq!(attrs("b").visibility.as_deref(), Some("protected"));
    assert!(attrs("c").weak);
    assert_eq!(attrs("c").visibility.as_deref(), Some("hidden"));
    assert!(module.declared_symbol_attrs.is_empty());
}
