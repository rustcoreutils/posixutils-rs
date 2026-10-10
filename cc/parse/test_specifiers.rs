//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Parser tests for the types of `__func__`, `_Complex` combinations, the
// named machine modes and `__auto_type`.
//

use super::ast::{BlockItem, Expr, ExprKind, ExternalDecl, Stmt};
use super::test_parser::{parse_tu, parse_tu_for, with_statement_expr};
use crate::symbol::{Namespace, SymbolTable};
use crate::target::{Arch, Os, Target};
use crate::types::{TypeId, TypeKind, TypeModifiers, TypeTable};

/// `__func__` is `const char[N]` for the enclosing function's name: the
/// helper's function is `t`, so N is 2.
#[test]
fn test_func_name_is_a_const_char_array() {
    for spelled in ["__func__", "__FUNCTION__", "__PRETTY_FUNCTION__"] {
        with_statement_expr("", spelled, |p, e| {
            let typ = e.typ.expect("typed");
            assert_eq!(p.types.kind(typ), TypeKind::Array, "{spelled}");
            assert_eq!(p.types.size_bytes(typ), 2, "{spelled}");
            let elem = p.types.base_type(typ).expect("element");
            assert!(p.types.modifiers(elem).contains(TypeModifiers::CONST));
        });
        with_statement_expr("", &format!("sizeof {spelled}"), |p, e| {
            assert_eq!(p.eval_const_expr(e), Some(2), "{spelled}");
        });
    }
}

/// `_Complex` with a base that is not arithmetic is reported, not dropped.
#[test]
fn test_complex_needs_an_arithmetic_base() {
    for src in [
        "typedef double ty; ty _Complex z;",
        "__typeof__(1.0f) _Complex z;",
        "_Complex _Bool b;",
        "_Complex void *p;",
        "struct S { int a; } _Complex v;",
    ] {
        let before = crate::diag::error_count();
        let _ = parse_tu(src);
        assert!(crate::diag::error_count() > before, "{src}: accepted");
    }
}

/// `byte` is one byte; `unwind_word` is eight.
#[test]
fn test_named_machine_modes() {
    let (_, types, strings, symbols) = parse_tu(
        "typedef int b __attribute__((mode(byte)));\n\
         typedef unsigned uw __attribute__((__mode__(__unwind_word__)));",
    )
    .unwrap();
    let size = |name: &str| {
        let id = strings.lookup(name).expect("interned");
        let typ = symbols
            .lookup(id, Namespace::Ordinary)
            .expect("declared")
            .typ;
        (types.size_bytes(typ), types.is_unsigned(typ))
    };
    assert_eq!(size("b"), (1, false));
    assert_eq!(size("uw"), (8, true));
}

/// The declarators of the last function in `src`'s body, in order: each
/// one's name, type and initializer.
fn body_declarators(src: &str) -> (Vec<(String, TypeId, Option<Expr>)>, TypeTable) {
    let target = Target::new(Arch::X86_64, Os::Linux);
    let (tu, types, strings, symbols): (_, _, _, SymbolTable) =
        parse_tu_for(src, &target).expect("parses");
    let Some(ExternalDecl::FunctionDef(f)) = tu.items.last() else {
        panic!("no function definition last in {src}");
    };
    let Stmt::Block(items) = &f.body else {
        panic!("function body is a block");
    };
    let mut out = Vec::new();
    for item in items {
        if let BlockItem::Declaration(decl) = item {
            for d in &decl.declarators {
                let name = strings.get(symbols.get(d.symbol).name).to_string();
                out.push((name, d.typ, d.init.clone()));
            }
        }
    }
    (out, types)
}

/// `__auto_type` gives the object its initializer's type after lvalue
/// conversion: an array and a function decay, top-level qualifiers drop, and
/// the qualifiers written beside `__auto_type` apply.
#[test]
fn test_auto_type_takes_the_lvalue_converted_initializer_type() {
    let (decls, mut types) = body_declarators(
        "int g(void);\n\
         void f(void) {\n\
             int arr[4];\n\
             const volatile int cv = 1;\n\
             __auto_type p = arr;\n\
             __auto_type fp = g;\n\
             __auto_type m = cv;\n\
             const __auto_type k = 7L;\n\
             __auto_type d = 0.5;\n\
         }",
    );
    let typ = |name: &str| {
        decls
            .iter()
            .find(|(n, _, _)| n == name)
            .unwrap_or_else(|| panic!("no declarator {name}"))
            .1
    };
    let int_ptr = types.pointer_to(types.int_id);
    assert_eq!(typ("p"), int_ptr, "an array decays to a pointer");

    assert_eq!(types.kind(typ("fp")), TypeKind::Pointer);
    let pointee = types.base_type(typ("fp")).expect("pointee");
    assert_eq!(types.kind(pointee), TypeKind::Function, "a function decays");

    assert_eq!(typ("m"), types.int_id, "const volatile drops");

    let k = typ("k");
    assert_eq!(types.unqualified(k), types.long_id);
    assert!(types.modifiers(k).contains(TypeModifiers::CONST));

    assert_eq!(typ("d"), types.double_id);
}

/// The initializer is the declarator's one initializer: a call in it is in
/// the tree once, so it is evaluated once.
#[test]
fn test_auto_type_initializer_is_parsed_once() {
    let (decls, types) = body_declarators(
        "short next(void);\n\
         void f(void) { __auto_type x = next(); }",
    );
    let [(name, typ, Some(init))] = decls.as_slice() else {
        panic!("one initialized declarator: {}", decls.len());
    };
    assert_eq!(name, "x");
    assert_eq!(types.kind(*typ), TypeKind::Short, "the call's type");
    assert!(
        matches!(&init.kind, ExprKind::Call { args, .. } if args.is_empty()),
        "the initializer is the call itself"
    );
}

/// The name is not in scope inside its own initializer: it names the outer
/// declaration, as in gcc, so `x` is a pointer to the file-scope `int`.
#[test]
fn test_auto_type_name_is_not_in_scope_in_its_initializer() {
    let (decls, types) = body_declarators("int x;\nvoid f(void) { __auto_type x = &x; }");
    let int_ptr = types.pointer_to(types.int_id);
    assert_eq!(decls[0].1, int_ptr);
}

/// gcc's misuses are refused, and `__auto_type` is a specifier only in a
/// declaration: not a parameter, member or type name.
#[test]
fn test_auto_type_misuse_is_refused() {
    for src in [
        "__auto_type x;",
        "void f(void) { __auto_type a = 1, b = 2; }",
        "void f(void) { __auto_type *p = 0; }",
        "void f(void) { __auto_type a[] = {1}; }",
        "__auto_type f(void);",
        "void f(void) { long __auto_type a = 1; }",
        "typedef __auto_type T = 1;",
        "void f(__auto_type x);",
        "struct S { __auto_type m; };",
        "int n = sizeof(__auto_type);",
        "__auto_type;",
        "struct B { int b : 3; } s; void f(void) { __auto_type a = s.b; }",
    ] {
        let before = crate::diag::error_count();
        let parsed = parse_tu(src);
        assert!(
            parsed.is_err() || crate::diag::error_count() > before,
            "{src}: accepted"
        );
    }
}

/// gcc's `__signed` and `__signed__` are keywords spelling `signed`, not
/// macros, so the parser meets them as written -- in a `.i` from gcc, or
/// wherever the preprocessor leaves them.
#[test]
fn test_gnu_signed_spellings_are_signed() {
    for spelled in ["__signed", "__signed__"] {
        let src = format!("{spelled} char c; {spelled} x; long {spelled} int l;");
        let (_, types, strings, symbols) = parse_tu(&src).unwrap();
        let type_of = |name: &str| {
            let id = strings.lookup(name).expect(name);
            symbols.lookup(id, Namespace::Ordinary).expect(name).typ
        };
        let c = type_of("c");
        assert_eq!(types.kind(c), TypeKind::Char, "{spelled}");
        assert!(
            types.modifiers(c).contains(TypeModifiers::SIGNED),
            "{spelled}"
        );
        assert_eq!(types.kind(type_of("x")), TypeKind::Int, "{spelled}");
        assert_eq!(types.kind(type_of("l")), TypeKind::Long, "{spelled}");
    }
    let before = crate::diag::error_count();
    let _ = parse_tu("__signed__ unsigned u;");
    assert!(crate::diag::error_count() > before, "signed and unsigned");
}

/// `__extension__` is a keyword, not a macro for nothing: the parser takes
/// it where gcc does -- before an external declaration, a block item, a
/// member declaration, a `for` declaration, and as a unary prefix.
#[test]
fn test_gnu_extension_keyword_positions() {
    let src = "__extension__ typedef long long ll;\n\
               __extension__ __extension__ static int z;\n\
               struct s { __extension__ union { int a; }; __extension__ int b; };\n\
               int f(void) {\n\
                 __extension__ int x = __extension__ 1;\n\
                 for (__extension__ int i = 0; i < 1; i++) x += i;\n\
                 __extension__ x++;\n\
                 return __extension__ ({ x; }) + (__extension__ sizeof(ll));\n\
               }\n";
    let before = crate::diag::error_count();
    let parsed = parse_tu(src);
    assert!(parsed.is_ok(), "{:?}", parsed.err().map(|e| e.message));
    assert_eq!(crate::diag::error_count(), before);
}

/// Every spelling of `volatile`, `inline` and `goto` qualifies an asm
/// statement, after any spelling of `asm`.
#[test]
fn test_asm_qualifier_spellings() {
    for asm in ["asm", "__asm", "__asm__"] {
        for quals in [
            "__inline volatile",
            "__inline__ __volatile__",
            "volatile __inline",
            "__volatile",
            "inline",
            "goto __inline",
        ] {
            let src = if quals.contains("goto") {
                format!("void f(void) {{ {asm} {quals}(\"\" ::: \"memory\" : l); l:; }}")
            } else {
                format!("void f(void) {{ {asm} {quals}(\"\" ::: \"memory\"); }}")
            };
            let parsed = parse_tu(&src);
            assert!(
                parsed.is_ok(),
                "{src}: {:?}",
                parsed.err().map(|e| e.message)
            );
        }
    }
}
