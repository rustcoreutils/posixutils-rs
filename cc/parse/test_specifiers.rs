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

/// `setjmp` and `_setjmp` are ordinary identifiers wherever they are not
/// called: glibc's `libc_hidden_proto (_setjmp)` is `extern __typeof
/// (_setjmp) _setjmp __attribute__((visibility("hidden")));`, and a program
/// may take their addresses. The parser wanted a `(` after either name.
#[test]
fn test_setjmp_names_outside_a_call() {
    let src = "typedef int jb[8];\n\
               extern int setjmp(jb);\n\
               extern int _setjmp(jb);\n\
               extern void longjmp(jb, int);\n\
               extern __typeof(_setjmp) _setjmp __attribute__((visibility(\"hidden\")));\n\
               extern __typeof(setjmp) setjmp;\n\
               int (*p)(jb) = &setjmp, (*q)(jb) = _setjmp;\n\
               void (*r)(jb, int) = longjmp;\n\
               jb env;\n\
               int f(void) { return setjmp(env) + _setjmp(env) + (p == q); }\n";
    let before = crate::diag::error_count();
    let parsed = parse_tu(src);
    assert!(parsed.is_ok(), "{:?}", parsed.err());
    assert_eq!(crate::diag::error_count(), before);
}

/// libgcc's own modes, which glibc's soft-fp declares its comparison results
/// and shift counts with (`__gcc_CMPtype`), so they must be libgcc's widths:
/// the shift count is a word everywhere, and the comparison result a word
/// on x86-64 but `SImode` on aarch64 -- gcc 13 and aarch64-linux-gnu-gcc 13
/// answer `sizeof` 8 and 8, 4 and 8. They were "not implemented" and kept
/// the declared `int`.
#[test]
fn test_libgcc_machine_modes() {
    let src = "typedef int cmp_t __attribute__((mode(__libgcc_cmp_return__)));\n\
               typedef unsigned sh_t __attribute__((__mode__(libgcc_shift_count)));";
    for (arch, cmp_bytes) in [(Arch::X86_64, 8), (Arch::Aarch64, 4)] {
        let target = Target::new(arch, Os::Linux);
        let before = crate::diag::warning_count();
        let (_, types, strings, symbols): (_, _, _, SymbolTable) =
            parse_tu_for(src, &target).expect("parses");
        assert_eq!(crate::diag::warning_count(), before, "{arch:?}: warned");
        let size = |name: &str| {
            let id = strings.lookup(name).expect("interned");
            let typ = symbols
                .lookup(id, Namespace::Ordinary)
                .expect("declared")
                .typ;
            (types.size_bytes(typ), types.is_unsigned(typ))
        };
        assert_eq!(size("cmp_t"), (cmp_bytes, false), "{arch:?}");
        assert_eq!(size("sh_t"), (8, true), "{arch:?}");
    }
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

/// Before a block item that is no declaration, `__extension__` is the unary
/// operator of an expression statement, as in gcc, which consumes all but
/// the last of a run to tell: no label follows one. c17 skipped the run and
/// took `__extension__ l:` as the label `l`.
#[test]
fn test_gnu_extension_before_a_label_is_an_expression() {
    for (src, want) in [
        (
            "int f(void) { int l = 0; __extension__ l: return l; }",
            "expected ';'",
        ),
        (
            "int f(void) { int l = 0; __extension__ __extension__ l: return l; }",
            "expected ';'",
        ),
        (
            "int f(int c) { switch (c) { __extension__ case 1: return 0; } return 1; }",
            "expected expression before 'case'",
        ),
        (
            "int f(int c) { switch (c) { __extension__ default: return 0; } return 1; }",
            "expected expression before 'default'",
        ),
    ] {
        let Err(err) = parse_tu(src) else {
            panic!("{src}: accepted");
        };
        assert!(err.message.contains(want), "{src}: {}", err.message);
    }
    // A label then `__extension__` is the label on an expression statement.
    let parsed = parse_tu("int f(int x) { l: __extension__ x++; return x; }");
    assert!(parsed.is_ok(), "{:?}", parsed.err().map(|e| e.message));
}

/// An attribute between a struct, union or enum tag and a body ends the
/// specifier, as in gcc: the attributes are the declaration's, so the body
/// has nothing to belong to. c17 laid the struct or union out with them.
/// Before the tag, after the body, and after a tag with no body, they stay.
#[test]
fn test_attribute_between_tag_and_body_is_refused() {
    const GCC: &str = "expected identifier or '(' before '{' token";
    for (src, want) in [
        ("struct T __attribute__((packed)) { int a; char b; };", GCC),
        ("union U __attribute__((aligned(16))) { int a; };", GCC),
        ("struct T __attribute__((unknown_thing)) { int a; };", GCC),
        // An enum's attributes were never read there; the declarator
        // refuses the brace.
        (
            "enum E __attribute__((packed)) { X };",
            "expected identifier",
        ),
    ] {
        let Err(err) = parse_tu(src) else {
            panic!("{src}: accepted");
        };
        assert!(err.message.contains(want), "{src}: {}", err.message);
    }
    let ok = "struct __attribute__((packed)) P { int a; char b; };\n\
              struct Q { int a; char b; } __attribute__((packed));\n\
              struct __attribute__((packed)) __attribute__((aligned(4))) { int a; } anon;\n\
              struct P __attribute__((aligned(32))) aligned_p;\n\
              int size_p = sizeof(struct P);";
    let parsed = parse_tu(ok);
    assert!(parsed.is_ok(), "{:?}", parsed.err().map(|e| e.message));
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
