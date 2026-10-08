//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Linearizer tests for storage and memory access: static locals, frame
// slots, volatile accesses, and reading an object by address or value.
//

use super::test_linearize::{
    linearize_no_ssa, linearize_source, make_simple_func, test_pos, TestContext,
};
use super::*;
use crate::parse::ast::{
    AssignOp, BinaryOp, BlockItem, Declaration, ExprKind, ExternalDecl, FunctionDef,
    InitDeclarator, ParamStyle, Parameter, Stmt, UnaryOp,
};
use crate::strings::StringTable;
use crate::target::{Arch, Os, Target};
use crate::types::{
    ArrayExtent, CompositeType, MemberAlign, StructMember, Type, TypeModifiers, TypeTable,
};

// Static local variables

/// Linearize, without SSA, `int test(void) { static int counter = 7; <e>;
/// return 0; }`, where `build` makes `e` from an expression naming
/// `counter`. Answers the module and its dump.
fn linearize_static_local_stmt(build: impl FnOnce(&TestContext, Expr) -> Expr) -> (Module, String) {
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.types.int_id;
    let counter_sym = ctx.var("counter", int_type);

    // STATIC goes in storage_class, not type modifiers (matches parser behavior)
    let decl = Declaration {
        declarators: vec![InitDeclarator {
            fn_effect: Default::default(),
            cleanup: None,
            symbol_attrs: Default::default(),
            pos: Position::default(),
            symbol: counter_sym,
            typ: int_type,
            storage_class: TypeModifiers::STATIC,
            init: Some(Expr::int(7, &ctx.types)),
            vla_sizes: vec![],
            explicit_align: None,
        }],
    };
    let stmt = build(&ctx, Expr::var_typed(counter_sym, int_type));
    let body = Stmt::Block(vec![
        BlockItem::Declaration(decl),
        BlockItem::Statement(Box::new(Stmt::Expr(stmt))),
        BlockItem::Statement(Box::new(Stmt::Return(Some(Expr::int(0, &ctx.types))))),
    ]);
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(make_simple_func(
            test_id, body, &ctx.types,
        ))],
    };
    let module = linearize_no_ssa(&tu, &ctx.types, &ctx.strings, &ctx.symbols);
    let ir = format!("{}", module.display(&ctx.types));
    (module, ir)
}

/// The address operands of every load and store in `module`'s one function
/// that names the global `name`, in order, as `(opcode, address)`.
fn global_accesses(module: &Module, name: &str) -> Vec<(Opcode, PseudoId)> {
    let func = &module.functions[0];
    func.blocks
        .iter()
        .flat_map(|b| &b.insns)
        .filter(|i| matches!(i.op, Opcode::Load | Opcode::Store))
        .filter(|i| {
            matches!(
                func.get_pseudo(i.src[0]).map(|p| &p.kind),
                Some(PseudoKind::Sym(n)) if n == name
            )
        })
        .map(|i| (i.op, i.src[0]))
        .collect()
}

/// A read-modify-write of a static local loads the global and stores back
/// through the very same symbol: one place, resolved once.
fn assert_static_local_rmw((module, ir): (Module, String)) {
    let accesses = global_accesses(&module, "test.counter.0");
    match accesses.as_slice() {
        [(Opcode::Load, load), (Opcode::Store, store)] => {
            assert_eq!(
                load, store,
                "load and store must share one place. IR:\n{ir}"
            )
        }
        other => {
            panic!("expected one load then one store of test.counter.0, got {other:?}. IR:\n{ir}")
        }
    }
    assert!(
        func_has_no_local(&module, "counter"),
        "a static local must not get a frame slot. IR:\n{ir}"
    );
}

/// Whether no frame local of `module`'s one function is named after `name`.
fn func_has_no_local(module: &Module, name: &str) -> bool {
    let prefix = format!("{name}.");
    !module.functions[0]
        .locals
        .keys()
        .any(|k| k == name || k.starts_with(&prefix))
}

#[test]
fn test_static_local_pre_increment() {
    let rmw = linearize_static_local_stmt(|ctx, counter| {
        Expr::typed_unpositioned(
            ExprKind::Unary {
                op: UnaryOp::PreInc,
                operand: Box::new(counter),
            },
            ctx.types.int_id,
        )
    });
    assert_static_local_rmw(rmw);
}

#[test]
fn test_static_local_pre_decrement() {
    let rmw = linearize_static_local_stmt(|ctx, counter| {
        Expr::typed_unpositioned(
            ExprKind::Unary {
                op: UnaryOp::PreDec,
                operand: Box::new(counter),
            },
            ctx.types.int_id,
        )
    });
    assert_static_local_rmw(rmw);
}

#[test]
fn test_static_local_post_increment() {
    let rmw = linearize_static_local_stmt(|ctx, counter| {
        Expr::typed_unpositioned(ExprKind::PostInc(Box::new(counter)), ctx.types.int_id)
    });
    assert_static_local_rmw(rmw);
}

#[test]
fn test_static_local_post_decrement() {
    let rmw = linearize_static_local_stmt(|ctx, counter| {
        Expr::typed_unpositioned(ExprKind::PostDec(Box::new(counter)), ctx.types.int_id)
    });
    assert_static_local_rmw(rmw);
}

#[test]
fn test_static_local_compound_assignment() {
    let rmw = linearize_static_local_stmt(|ctx, counter| {
        Expr::typed_unpositioned(
            ExprKind::Assign {
                op: AssignOp::AddAssign,
                target: Box::new(counter),
                value: Box::new(Expr::int(5, &ctx.types)),
            },
            ctx.types.int_id,
        )
    });
    assert_static_local_rmw(rmw);
}

#[test]
fn test_static_local_assignment_stores_without_reading() {
    let (module, ir) = linearize_static_local_stmt(|ctx, counter| {
        Expr::typed_unpositioned(
            ExprKind::Assign {
                op: AssignOp::Assign,
                target: Box::new(counter),
                value: Box::new(Expr::int(5, &ctx.types)),
            },
            ctx.types.int_id,
        )
    });
    let accesses = global_accesses(&module, "test.counter.0");
    assert!(
        matches!(accesses.as_slice(), [(Opcode::Store, _)]),
        "expected exactly one store to test.counter.0, got {accesses:?}. IR:\n{ir}"
    );
    assert!(func_has_no_local(&module, "counter"), "IR:\n{ir}");
}

/// The symbol a type name's extents are recorded under names no object: it
/// gets no frame slot and no `Sym` of its own, only the hidden locals that
/// hold the extents.
#[test]
fn test_type_name_extents_get_no_storage() {
    // int (*test(int n, int *p))[] { return (int (*)[n])p; }
    let mut ctx = TestContext::new();
    let test_id = ctx.str("test");
    let int_type = ctx.types.int_id;
    let int_ptr = ctx.ptr(int_type);
    let vla_row = ctx
        .types
        .intern(Type::array_of(int_type, ArrayExtent::Variable));
    let row_ptr = ctx.types.intern(Type::pointer(vla_row));
    let n_sym = ctx.var("n", int_type);
    let p_sym = ctx.var("p", int_ptr);
    let tn_sym = ctx.var("tyname", row_ptr);

    let cast = Expr::typed_unpositioned(
        ExprKind::VmTypeName {
            symbol: tn_sym,
            dims: vec![Expr::var_typed(n_sym, int_type)],
            expr: Box::new(Expr::typed_unpositioned(
                ExprKind::Cast {
                    cast_type: row_ptr,
                    expr: Box::new(Expr::var_typed(p_sym, int_ptr)),
                },
                row_ptr,
            )),
        },
        row_ptr,
    );
    let param = |symbol, typ| Parameter {
        symbol: Some(symbol),
        typ,
        vm_dims: vec![],
        discarded_dims: vec![],
    };
    // A pure expression statement is dropped unevaluated, so the cast is
    // returned.
    let func = FunctionDef {
        return_type: row_ptr,
        params: vec![param(n_sym, int_type), param(p_sym, int_ptr)],
        ..make_simple_func(test_id, Stmt::Return(Some(cast)), &ctx.types)
    };
    let tu = TranslationUnit {
        items: vec![ExternalDecl::FunctionDef(func)],
    };
    let module = linearize_no_ssa(&tu, &ctx.types, &ctx.strings, &ctx.symbols);
    let ir = format!("{}", module.display(&ctx.types));
    let f = &module.functions[0];

    assert!(
        f.locals.keys().any(|k| k.starts_with("__vla_dim0_tyname.")),
        "the extent should be recorded. IR:\n{ir}"
    );
    assert!(
        func_has_no_local(&module, "tyname"),
        "a type name's symbol must not get a slot. IR:\n{ir}"
    );
    assert!(
        !f.pseudos
            .iter()
            .any(|p| matches!(&p.kind, PseudoKind::Sym(n) if n.starts_with("tyname"))),
        "a type name's symbol must not be named by any Sym. IR:\n{ir}"
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
            align: MemberAlign::NATURAL,
        },
        StructMember {
            name: str_id,
            typ: ptr_type,
            offset: 8,
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
            size: 16,
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
                    cleanup: None,
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
        storage_class: TypeModifiers::empty(),
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
                cleanup: None,
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
        storage_class: TypeModifiers::empty(),
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
        storage_class: TypeModifiers::empty(),
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

    let accesses = |name: &str| memory_accesses(&module, name);

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

    let accesses = |name: &str| memory_accesses(&module, name);

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

/// Every `Load` and `Store` in function `name`, with whether it carries the
/// volatile marker, in program order.
fn memory_accesses(module: &Module, name: &str) -> Vec<(Opcode, bool)> {
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
}

/// A member of an anonymous `volatile` structure is volatile.
///
/// C17 6.7.2.1p13 makes `a` a member of `s`, and it lives inside the anonymous
/// structure, which is volatile -- so `s.a` is a volatile access although
/// neither `s` nor `a` was declared so. `find_member` walked into the
/// anonymous member without collecting its qualifiers, and the read was
/// unmarked. The same holds for an initializer that reaches `a` by name or
/// positionally, since the walker reaches it through the same member.
#[test]
fn test_a_member_of_an_anonymous_volatile_member_carries_the_marker() {
    let target = Target::host();
    let src = "struct A { volatile struct { int a; }; int b; };\n\
               struct A s;\n\
               struct { const volatile union { int u; }; } cu;\n\
               void read_anon(void) { s.a; }\n\
               void read_union(void) { cu.u; }\n\
               void read_plain(void) { s.b; }\n\
               void init_named(void) { struct A l = { .a = 1 }; (void)l; }\n\
               void init_positional(void) { struct A l = { 1, 2 }; (void)l; }\n";
    let module = linearize_source(src, &target);
    assert_eq!(
        memory_accesses(&module, "read_anon"),
        vec![(Opcode::Load, true)]
    );
    assert_eq!(
        memory_accesses(&module, "read_union"),
        vec![(Opcode::Load, true)]
    );
    // The control: the sibling outside the anonymous member is ordinary.
    assert_eq!(
        memory_accesses(&module, "read_plain"),
        vec![(Opcode::Load, false)]
    );

    // `l` holds a volatile member, so its whole-object zero is volatile too,
    // and the store into `a` is marked whichever way the list reaches it.
    let stores = |name: &str| -> Vec<bool> {
        memory_accesses(&module, name)
            .into_iter()
            .filter(|(op, _)| *op == Opcode::Store)
            .map(|(_, v)| v)
            .collect()
    };
    assert!(
        stores("init_named").iter().all(|v| *v),
        "{:?}",
        stores("init_named")
    );
    // Zero, then `a` (inside the volatile anonymous member), then `b`
    // (outside it, and ordinary).
    assert_eq!(stores("init_positional"), vec![true, true, false]);
}

/// An initializer's stores into a `volatile` object are volatile accesses.
///
/// The stores are typed with each member's *declared* type, which does not
/// show a qualifier the object carries, so `volatile struct B vb = {1, 2}`
/// stored into `vb` unmarked -- correctness rested on the passes also asking
/// `LocalVar` about the named object. Every store is marked now, the
/// whole-object zero included, and through a nested member and a designator.
/// An aggregate member that is itself declared `volatile` inside an ordinary
/// object is the other spelling.
#[test]
fn test_an_initializer_of_a_volatile_object_marks_every_store() {
    let target = Target::host();
    let src = "struct B { int x, y; };\n\
               struct O { struct B in; int z; };\n\
               struct P { volatile struct B vb; int z; };\n\
               struct F { unsigned f : 3, g : 5; };\n\
               void whole(void) { volatile struct B vb = {1, 2}; }\n\
               void partial(void) { volatile struct B vb = {1}; }\n\
               void designated(void) { volatile struct B vb = { .y = 2 }; }\n\
               void nested(void) { volatile struct O vo = { {1, 2}, 3 }; }\n\
               void member(void) { struct P p = { {1, 2}, 3 }; (void)p; }\n\
               void bits(void) { volatile struct F vf = { 1, 2 }; }\n\
               void plain(void) { struct B pb = {1, 2}; (void)pb; }\n";
    let module = linearize_source(src, &target);
    for name in ["whole", "partial", "designated", "nested"] {
        let accesses = memory_accesses(&module, name);
        assert!(!accesses.is_empty(), "{name}: no stores at all");
        assert!(
            accesses.iter().all(|&(op, v)| op == Opcode::Store && v),
            "{name}: every store into a volatile object is volatile: {accesses:?}"
        );
    }
    // A bit-field is initialized by reading its carrier back and storing it:
    // both halves are accesses to the volatile object.
    let bits = memory_accesses(&module, "bits");
    assert!(bits.contains(&(Opcode::Load, true)), "{bits:?}");
    assert!(bits.iter().all(|&(_, v)| v), "{bits:?}");

    // `p` is not volatile, but `p.vb` is: its two stores are marked, and the
    // ordinary member beside it is not. The zero covers the volatile member,
    // so it is marked as a whole.
    let member = memory_accesses(&module, "member");
    assert!(
        member.contains(&(Opcode::Store, true)) && member.contains(&(Opcode::Store, false)),
        "{member:?}"
    );

    // The control: an ordinary object's initializer is not marked.
    let plain = memory_accesses(&module, "plain");
    assert!(
        !plain.is_empty() && plain.iter().all(|&(_, v)| !v),
        "{plain:?}"
    );
}

/// Every chunk of an aggregate copy out of, or into, a `volatile` object is a
/// volatile access.
///
/// A copy of a struct too large for one register moves integer chunks, whose
/// types say nothing about the aggregate's qualifier. So `struct S t = *p;`
/// through a `volatile struct S *p` read `*p` unmarked, and with `t` unused
/// DCE deleted every read from `-O1` up. The store side is the same
/// question for `*p = t`, and a plain copy is the control.
#[test]
fn test_an_aggregate_copy_of_a_volatile_object_marks_every_chunk() {
    let target = Target::host();
    let src = "struct S { int a, b, c; };\n\
               volatile struct S *vp;\n\
               struct S *pp;\n\
               void copy_out(void) { struct S t = *vp; (void)t; }\n\
               void copy_in(struct S t) { *vp = t; }\n\
               void copy_plain(void) { struct S t = *pp; (void)t; }\n";
    let module = linearize_source(src, &target);

    // Reading `vp` itself is an ordinary load; every chunk load after it is of
    // `*vp`, and the stores into `t` are not volatile.
    let out = memory_accesses(&module, "copy_out");
    let chunk_loads: Vec<bool> = out
        .iter()
        .skip(1)
        .filter(|(op, _)| *op == Opcode::Load)
        .map(|(_, v)| *v)
        .collect();
    assert!(
        !chunk_loads.is_empty() && chunk_loads.iter().all(|v| *v),
        "{out:?}"
    );
    assert!(out
        .iter()
        .filter(|(op, _)| *op == Opcode::Store)
        .all(|(_, v)| !v));

    let into = memory_accesses(&module, "copy_in");
    let chunk_stores: Vec<bool> = into
        .iter()
        .filter(|(op, _)| *op == Opcode::Store)
        .map(|(_, v)| *v)
        .collect();
    assert!(chunk_stores.iter().any(|v| *v), "{into:?}");

    let plain = memory_accesses(&module, "copy_plain");
    assert!(plain.iter().all(|(_, v)| !v), "{plain:?}");
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

    let accesses = |name: &str| memory_accesses(&module, name);

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

/// Parse `src` for `target` and answer, for each named file-scope object,
/// whether [`Linearizer::object_reads_as_address`] and
/// [`Linearizer::aggregate_travels_by_value`] say so of its type.
fn read_object_decisions(src: &str, target: &Target, names: &[&str]) -> Vec<(bool, bool)> {
    let mut strings = StringTable::new();
    let mut tokenizer = crate::token::lexer::Tokenizer::new(src.as_bytes(), 0, &mut strings);
    let tokens = tokenizer.tokenize();
    let mut symbols = crate::symbol::SymbolTable::new();
    let mut types = TypeTable::new(target);
    {
        let mut parser =
            crate::parse::Parser::new(&tokens, &strings, &mut symbols, &mut types, Vec::new());
        parser.parse_translation_unit().expect("parse");
    }
    let linearizer = Linearizer::new(&symbols, &types, &strings, target);
    names
        .iter()
        .map(|name| {
            let id = strings.lookup(name).expect("name interned");
            let typ = symbols
                .lookup(id, crate::symbol::Namespace::Ordinary)
                .expect("declared")
                .typ;
            (
                linearizer.object_reads_as_address(typ),
                linearizer.aggregate_travels_by_value(typ),
            )
        })
        .collect()
}

/// The one rule for reading an object as an rvalue: an array, a function
/// designator and an array-typed `va_list` yield their address; a struct or
/// union travels as its value only when it has between one and 64 bits, so a
/// zero-sized one is used by address exactly like a wide one.
#[test]
fn test_read_object_decides_address_or_value() {
    let src = "struct E {}; struct Z { int a[0]; };\n\
               struct S4 { int a; }; struct S16 { long a, b; };\n\
               union U8 { long l; char c; }; union U0 {};\n\
               int i; double d; int *p; int arr[3]; int fn(void);\n\
               __builtin_va_list ap;\n\
               struct E e; struct Z z; struct S4 s4; struct S16 s16;\n\
               union U8 u8; union U0 u0;\n\
               float _Complex fc; double _Complex dc; _Complex int ci;\n";
    let names = [
        "i", "d", "p", "arr", "fn", "ap", "e", "z", "s4", "s16", "u8", "u0", "fc", "dc", "ci",
    ];
    let expect = [
        (false, false), // i
        (false, false), // d
        (false, false), // p
        (true, false),  // arr
        (true, false),  // fn
        (true, false),  // ap: an array on x86-64
        (true, false),  // e: zero-sized
        (true, false),  // z: zero-sized
        (false, true),  // s4
        (true, false),  // s16
        (false, true),  // u8
        (true, false),  // u0: zero-sized
        (true, false),  // fc: complex, though it fits in a register
        (true, false),  // dc
        (true, false),  // ci
    ];
    for target in [
        Target::new(Arch::X86_64, Os::Linux),
        Target::new(Arch::Aarch64, Os::Linux),
    ] {
        let got = read_object_decisions(src, &target, &names);
        for ((name, want), have) in names.iter().zip(expect).zip(got) {
            // Only `ap` differs: AAPCS64's va_list is a struct.
            let want = if *name == "ap" && target.arch == Arch::Aarch64 {
                have
            } else {
                want
            };
            assert_eq!(have, want, "{name}: (reads as address, travels by value)");
        }
    }

    // Where `va_list` is itself a pointer it is read like one.
    let got = read_object_decisions(
        "__builtin_va_list ap;\n",
        &Target::new(Arch::Aarch64, Os::MacOS),
        &["ap"],
    );
    assert_eq!(got, [(false, false)], "a pointer va_list is loaded");
}

/// A statement expression's value is out of the block before the block's
/// objects die: when it is carried as an address -- a struct wider than a
/// register, a complex number, a vector -- nothing after `x`'s
/// `lifetime.end` reads `x`, whose slot the next object may share. An
/// eight-byte struct travels as its value and needs no copy.
#[test]
fn test_stmt_expr_value_is_read_before_its_block_ends() {
    let src = "struct S24 { long a, b, c; }; struct S8 { int a, b; };\n\
               typedef int v4 __attribute__((vector_size(16)));\n\
               struct S24 g(void) { return ({ struct S24 x = {1, 2, 3}; x; }); }\n\
               double _Complex h(void) { return ({ double _Complex x = 1.0; x; }); }\n\
               float _Complex k(void) { return ({ float _Complex x = 1.0f; x; }); }\n\
               v4 v(void) { return ({ v4 x = {1, 2, 3, 4}; x; }); }\n\
               struct S8 s(void) { return ({ struct S8 x = {1, 2}; x; }); }\n";
    for target in [
        Target::new(Arch::X86_64, Os::Linux),
        Target::new(Arch::Aarch64, Os::Linux),
    ] {
        let module = linearize_source(src, &target);
        for name in ["g", "h", "k", "v", "s"] {
            let func = module
                .functions
                .iter()
                .find(|f| f.name == name)
                .expect(name);
            let x = func
                .locals
                .iter()
                .find(|(n, _)| n.starts_with("x."))
                .map(|(_, l)| l.sym)
                .expect("x");
            let insns: Vec<&Instruction> =
                func.blocks.iter().flat_map(|bb| bb.insns.iter()).collect();
            let mut names_x = vec![x];
            for insn in &insns {
                if insn.op == Opcode::SymAddr && insn.src.first() == Some(&x) {
                    names_x.extend(insn.target);
                }
            }
            let end = insns
                .iter()
                .position(|i| i.op == Opcode::LifetimeEnd && i.extra().lifetime_of == Some(x))
                .unwrap_or_else(|| panic!("{name}: x's lifetime ends in the block"));
            for insn in &insns[end + 1..] {
                assert!(
                    !insn.src.iter().any(|p| names_x.contains(p)),
                    "{name} on {:?}: {insn:?} reads x after its lifetime ended",
                    target.arch
                );
            }
            let copied = func.locals.keys().any(|n| n.starts_with("__value_copy"));
            assert_eq!(copied, name != "s", "{name}: copied out of the block");
        }
    }
}

/// No access to a zero-sized object moves any bits: every load and store in
/// these functions is of a non-zero width. A zero-sized read through `*p`,
/// `s.m`, `a[i]`, a name or a compound literal was a zero-bit load whose
/// store the back end performed as a byte, over the following member.
#[test]
fn test_zero_sized_aggregate_is_never_loaded_or_stored() {
    let src = "struct E {};\n\
               struct W { char lo; struct E e; char hi; };\n\
               struct E g; struct E garr[2];\n\
               void f(struct W *w, struct E *p, int i, int c, struct E a) {\n\
                 struct E l = *p, arr[2];\n\
                 w->e = *p; w->e = l; w->e = arr[i]; w->e = g; w->e = garr[i];\n\
                 w->e = (struct E){}; w->e = c ? a : l; l = w->e; arr[i] = w->e;\n\
               }\n";
    let module = linearize_source(src, &Target::new(Arch::X86_64, Os::Linux));
    let func = module.functions.iter().find(|f| f.name == "f").expect("f");
    for insn in func.blocks.iter().flat_map(|bb| bb.insns.iter()) {
        if matches!(insn.op, Opcode::Load | Opcode::Store) {
            assert!(insn.size > 0, "a zero-width access: {insn:?}");
        }
    }
}

/// A compound literal is an anonymous frame local whether it is read as a
/// value or has its address taken: each gets its own `.compound_literal.N`
/// local, named after its `Sym` pseudo, whose kind names that same local.
#[test]
fn test_compound_literal_is_a_named_frame_local() {
    let src = "struct P { int x, y; };\n\
               int f(void) { struct P a = (struct P){ 1 }; struct P *q = &(struct P){ .y = 2 };\n\
               return a.x + q->y; }\n";
    let module = linearize_source(src, &Target::host());
    let f = module.functions.iter().find(|f| f.name == "f").unwrap();
    let literals: Vec<(&String, &crate::ir::LocalVar)> = f
        .locals
        .iter()
        .filter(|(name, _)| name.starts_with(".compound_literal."))
        .collect();
    assert_eq!(literals.len(), 2, "{:?}", f.locals.keys());
    for (name, local) in literals {
        assert_eq!(name.as_str(), format!(".compound_literal.{}", local.sym.0));
        assert!(local.decl_block.is_some(), "{name}");
        let pseudo = f.get_pseudo(local.sym).expect("registered");
        assert!(
            matches!(&pseudo.kind, PseudoKind::Sym(n) if n == name),
            "{name}: {:?}",
            pseudo.kind
        );
    }
}

/// A value of an incomplete type has no width to read or write it at
/// (C17 6.3.2.1p2). Each access is reported, and nothing zero-width is
/// emitted: a forward-declared `enum` read as a value used to reach a
/// conversion the IR validator refused as an internal compiler error.
#[test]
fn test_incomplete_object_access_is_reported() {
    for body in [
        "int f(void) { return ve; }",
        "void f(void) { ve = 1; }",
        "void f(void) { ve++; }",
        "void f(void) { ve *= 3; }",
        "int f(enum e *p) { return *p; }",
    ] {
        let src = format!("extern enum e ve;\n{body}\n");
        let before = crate::diag::error_count();
        let _ = linearize_source(&src, &Target::host());
        assert!(crate::diag::error_count() > before, "{body}: not reported");
    }
}
