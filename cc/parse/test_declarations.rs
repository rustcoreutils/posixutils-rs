//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Parser tests for linkage, redeclaration, tags and declarator constraints.
//

use super::ast::{
    brace_elision_span, is_brace_elision_candidate, Designator, ExprKind, ExternalDecl, InitElement,
};
use super::test_parser::{parse_tu, parse_tu_for};
use crate::symbol::{Linkage, Namespace};
use crate::target::{Arch, Os, Target};
use crate::types::{TypeId, TypeTable};

/// Parse `src`, requiring it to be rejected: a parse error, or a diagnostic.
fn assert_rejected(src: &str) {
    let before = crate::diag::error_count();
    let failed = parse_tu(src).is_err();
    assert!(
        failed || crate::diag::error_count() > before,
        "{src}: accepted"
    );
}

/// The linkage each declaration gives its identifier (C17 6.2.2).
#[test]
fn test_symbols_record_their_linkage() {
    let (_, _, strings, symbols) = parse_tu(
        "int e; static int i; extern int x; static int g(void); int g(void);\n\
         void h(void);",
    )
    .unwrap();
    let linkage = |name: &str| {
        let id = strings.lookup(name).expect("interned");
        symbols
            .lookup(id, Namespace::Ordinary)
            .expect("declared")
            .linkage
    };
    assert_eq!(linkage("e"), Linkage::External);
    assert_eq!(linkage("i"), Linkage::Internal);
    assert_eq!(linkage("x"), Linkage::External);
    // `int g(void);` after `static int g(void);` takes the visible linkage.
    assert_eq!(linkage("g"), Linkage::Internal);
    assert_eq!(linkage("h"), Linkage::External);
}

#[test]
fn test_redeclaration_rules() {
    for src in [
        "void f(void) { int x; int x; }",
        "void f(int a) { int a; }",
        "int i = 1; int i = 2;",
        "void f(void) {} void f(void) {}",
        "int i; static int i;",
        "static int j; int j;",
        "void f(void) { int y; extern int y; }",
        "void f(void) { extern int y; int y; }",
        "int x; void f(void) { extern long x; }",
        "void f(void) { static void g(void); }",
    ] {
        assert_rejected(src);
    }
    // Repeats that C allows parse cleanly.
    for src in [
        "int i; int i; int i = 3; extern int i;",
        "static int g(void); int g(void); static int g(void) { return 0; }",
        "void f(void) { extern int z; extern int z; }",
        "extern inline __attribute__((gnu_inline)) int k(void) { return 1; } int k(void) { return 2; }",
    ] {
        parse_tu(src).unwrap();
    }
}

/// A `static` declaration may take over a name declared GNU `extern inline`
/// -- gcc's one exception to C17 6.2.2p7 -- and the static definition is the
/// function. Every verdict is gcc 13's.
#[test]
fn test_static_takes_over_gnu_extern_inline() {
    const EI: &str = "extern inline __attribute__((gnu_inline))";
    for src in [
        format!("{EI} int k(void) {{ return 1; }} static int k(void) {{ return 2; }}"),
        format!("{EI} int k(void) {{ return 1; }} static inline int k(void) {{ return 2; }}"),
        format!("int k(void); {EI} int k(void) {{ return 1; }} static int k(void) {{ return 2; }}"),
        format!("extern int k(void); {EI} int k(void) {{ return 1; }} static int k(void);"),
        format!("{EI} int k(void) {{ return 1; }} int k(void); static int k(void) {{ return 2; }}"),
        format!("{EI} int k(void); static int k(void) {{ return 2; }}"),
        format!("{EI} int k(void); int k(void); static int k(void) {{ return 2; }}"),
        format!("{EI} int k(void) {{ return 1; }} static int k(void) {{ return 2; }} int k(void);"),
        format!("static int k(void); {EI} int k(void) {{ return 1; }}"),
    ] {
        parse_tu(&src).unwrap_or_else(|e| panic!("{src}: {e:?}"));
    }
    for src in [
        // The static one already defined it, then the name is internal.
        format!("static int k(void) {{ return 2; }} {EI} int k(void) {{ return 1; }}"),
        "int k(void); static int k(void) { return 2; }".to_string(),
        // A plain `inline` promises the external definition.
        format!(
            "{EI} int k(void); inline __attribute__((gnu_inline)) int k(void); static int k(void);"
        ),
        "inline __attribute__((gnu_inline)) int k(void) { return 1; } static int k(void);"
            .to_string(),
        // So does the real definition.
        format!("{EI} int k(void) {{ return 1; }} int k(void) {{ return 3; }} static int k(void);"),
        // Without GNU inline semantics, C17 6.2.2p7.
        "extern inline int k(void) { return 1; } static int k(void) { return 2; }".to_string(),
        // The static one is a declaration of the same function.
        format!("{EI} int k(void) {{ return 1; }} static long k(void) {{ return 2; }}"),
    ] {
        assert_rejected(&src);
    }
}

/// A function definition has the linkage its name has, not only the one its
/// own specifiers spell: after `static int f(void);`, `int f(void) {..}` and
/// a GNU `extern inline` body are both static (C17 6.2.2p4, p5).
#[test]
fn test_definition_takes_prior_internal_linkage() {
    for src in [
        "static int f(void); int f(void) { return 0; }",
        "static int f(void); extern inline __attribute__((gnu_inline)) int f(void) { return 0; }",
        "extern inline __attribute__((gnu_inline)) int f(void) { return 1; }\n\
         static int f(void) { return 0; }",
    ] {
        let (tu, _, strings, _) = parse_tu(src).unwrap();
        let f = strings.lookup("f").expect("interned");
        let defs: Vec<bool> = tu
            .items
            .iter()
            .filter_map(|item| match item {
                ExternalDecl::FunctionDef(def) if def.name == f => Some(def.is_static),
                _ => None,
            })
            .collect();
        assert_eq!(defs.last(), Some(&true), "{src}");
    }
}

#[test]
fn test_tag_rules() {
    for src in [
        "struct S { int x; }; struct S { long y; };",
        "enum E { A }; enum E { B };",
        "struct S; union S *p;",
        "struct T { int a; }; enum T e;",
    ] {
        assert_rejected(src);
    }
    parse_tu("struct S { int x; }; void f(void) { struct S { long y; } s; }").unwrap();
}

#[test]
fn test_declarator_and_storage_rules() {
    for src in [
        "typedef int A[3]; A f(void);",
        "typedef int F(void); F f(void);",
        "restrict int x;",
        "void (* restrict fp)(void);",
        "void f(restrict int x);",
        "_Thread_local void f(void);",
        "typedef _Thread_local int T;",
        "void f(void) { _Thread_local int d; }",
        "register int x;",
        "auto int x;",
        "void f(void) { for (typedef int T;;) break; }",
        "void f(int n) { typedef int T[n]; typedef int T[n]; }",
    ] {
        assert_rejected(src);
    }
    parse_tu("int * restrict p; typedef int *IP; restrict IP r; int (*fp(void))[3];").unwrap();
}

#[test]
fn test_star_array_only_in_prototypes() {
    assert_rejected("void g(int n, int a[*]) {}");
    parse_tu("void h(int n, int a[*]); void k(void (*cb)(int n, int a[*])) {}").unwrap();
}

/// Parse `src`, requiring it to be accepted: no parse error and no error
/// diagnostic.
fn assert_accepted(src: &str) {
    let before = crate::diag::error_count();
    assert!(parse_tu(src).is_ok(), "{src}: parse error");
    assert_eq!(crate::diag::error_count(), before, "{src}: rejected");
}

/// C17 6.7.6.2p4: `[*]` is refused in the parameter list a function body
/// follows -- the defined function's own, which for a function returning a
/// function pointer is the inner list, not the return type's.
#[test]
fn test_star_array_scope_is_the_defined_functions_own_list() {
    for src in [
        "void (*fp(int x))(int n, int a[*]) { return 0; }",
        "void (*fr(int n, int a[*]))(int m, int b[*]);",
        "void f(void (*g)(int n, int a[*])) {}",
    ] {
        assert_accepted(src);
    }
    for src in [
        "void (*fq(int n, int a[*]))(int) { return 0; }",
        "void (*fq(int n, int a[*]))(int m, int b[*]) { return 0; }",
        "void f(int n, int (*a)[*]) {}",
    ] {
        assert_rejected(src);
    }
}

/// C17 6.7.6.3p7: `static` and qualifiers in `[ ]` belong to the parameter's
/// own array type; a parenthesized name, `(a)`, is still that declarator.
#[test]
fn test_static_array_parameter_with_parenthesized_name() {
    for src in [
        "void f3(int (a)[static 3]) {}",
        "void f(int ((a))[static 3]) {}",
        "void f(int (a)[const 3]) {}",
        "void f(int (a)[static 3][4]) {}",
        "void f(int (a[static 3])) {}",
        "void f(int (*a[static 3])) {}",
    ] {
        assert_accepted(src);
    }
    for src in [
        "void f(int (*a)[static 3]) {}",
        "void f(int (a[3])[static 3]) {}",
        "void f(void) { int (a)[static 3]; }",
        // gcc's: an attribute in the parentheses makes them a declarator.
        "void f(int (__attribute__((unused)) a)[static 3]) {}",
        "void f(int ((__attribute__((unused)) a))[static 3]) {}",
    ] {
        assert_rejected(src);
    }
}

/// C17 6.9.2p2-p3: a file-scope array declared without an extent is judged at
/// the end of the translation unit -- a later declaration may complete it --
/// and one still incomplete there gets one element, as gcc gives it, with a
/// warning. Every declarator is given the final type, since any of them may
/// be the one whose storage is emitted.
#[test]
fn test_incomplete_array_definition_is_judged_at_end_of_unit() {
    use crate::parse::ast::ExternalDecl;
    use crate::types::TypeKind;
    let sizes = |src: &str| -> (u32, Vec<usize>) {
        let before = crate::diag::warning_count();
        let (tu, types, _, _) = parse_tu(src).unwrap();
        let mut sizes = Vec::new();
        for item in &tu.items {
            if let ExternalDecl::Declaration(d) = item {
                for decl in &d.declarators {
                    if types.kind(decl.typ) == TypeKind::Array {
                        sizes.push(types.size_bytes(decl.typ));
                    }
                }
            }
        }
        (crate::diag::warning_count() - before, sizes)
    };
    // Completed later: no warning, and every declarator has the full size.
    assert_eq!(
        sizes("static int a[]; static int a[3] = {1, 2, 3};"),
        (0, vec![12, 12])
    );
    assert_eq!(sizes("static int a[]; static int a[5];"), (0, vec![20, 20]));
    assert_eq!(sizes("int a[]; int a[3];"), (0, vec![12, 12]));
    assert_eq!(sizes("int a[3]; int a[];"), (0, vec![12, 12]));
    assert_eq!(sizes("static int a[]; extern int a[4];"), (0, vec![16, 16]));
    // Never completed: one element, and one warning however many times it
    // was declared.
    assert_eq!(sizes("static int a[];"), (1, vec![4]));
    assert_eq!(sizes("int a[];"), (1, vec![4]));
    assert_eq!(sizes("static int a[]; static int a[];"), (1, vec![4, 4]));
    assert_eq!(sizes("static int a[][2];"), (1, vec![8]));
    // A declaration that is not a definition reserves nothing.
    assert_eq!(sizes("extern int a[];").0, 0);
    // A block-scope `extern` names the same object, and its extent counts.
    assert_eq!(
        sizes("int *g(void) { extern int a[5]; return a; } int a[];"),
        (0, vec![20])
    );
    assert_eq!(
        sizes("int a[]; int *g(void) { extern int a[1]; return a; }"),
        (0, vec![4])
    );
    // Given after the definition, it is too late: the definition is still
    // one element, which a different extent contradicts, as in gcc.
    for src in [
        "int a[]; int *g(void) { extern int a[5]; return a; }",
        "static int a[]; int *g(void) { extern int a[5]; return a; }",
    ] {
        assert_rejected(src);
    }
}

/// The target the initializer tests parse for: one whose `wchar_t` is `int`,
/// so `int s[3]` suits `L"ab"` wherever the tests run.
fn wchar_is_int() -> Target {
    Target::new(Arch::X86_64, Os::Linux)
}

/// Parse `src`, requiring it to compile with no diagnostic at all.
fn assert_clean(src: &str) {
    let (errors, warnings) = (crate::diag::error_count(), crate::diag::warning_count());
    parse_tu_for(src, &wchar_is_int()).unwrap();
    assert_eq!(crate::diag::error_count(), errors, "{src}: error");
    assert_eq!(crate::diag::warning_count(), warnings, "{src}: warning");
}

/// The last declarator of `src`'s last file-scope declaration: its type and
/// the elements of its initializer list.
fn last_initializer(src: &str) -> (TypeTable, TypeId, Vec<InitElement>) {
    let (tu, types, _, _) = parse_tu_for(src, &wchar_is_int()).unwrap();
    let Some(ExternalDecl::Declaration(decl)) = tu.items.last() else {
        panic!("{src}: no declaration last");
    };
    let d = decl.declarators.last().unwrap();
    let Some(ExprKind::InitList { elements }) = d.init.as_ref().map(|e| &e.kind) else {
        panic!("{src}: no initializer list");
    };
    (types, d.typ, elements.clone())
}

/// C17 6.7.9p20: an elided slot takes as many elements as its subobjects
/// do, each one element or -- itself elided -- as many as its own take. A
/// string literal fills a whole character array and a braced list a whole
/// subaggregate, so neither counts as its scalars; the count used to.
#[test]
fn test_brace_elision_span_walks_subobjects() {
    let cases: &[(&str, &[usize])] = &[
        // Each `struct In` slot takes the string and the int.
        (
            "struct In { char s[4]; int n; }; \
             struct Out { struct In in; struct In j; } v[1] = {\"abc\", 7, {\"de\", 8}};",
            &[3],
        ),
        // A braced element ends a row of two structs after three elements.
        (
            "struct Pt { int x, y; }; struct Pt g[2][2] = {1, 2, {.y = 5}, 7};",
            &[3, 1],
        ),
        // Two strings fill `m`; `n` takes the third element.
        (
            "struct E { char m[2][4]; int n; } e[2] = {\"ab\", \"cd\", 5, \"ef\", \"gh\", 6};",
            &[3, 3],
        ),
        // A wide string fills a `wchar_t` array that suits it.
        (
            "struct W { int s[3]; int n; } w[2] = {L\"ab\", 3, L\"c\", 4};",
            &[2, 2],
        ),
        // A pointer member takes a string as one scalar.
        (
            "struct D { char *p; int n; } d[2] = {\"x\", 1, \"yz\", 2};",
            &[2, 2],
        ),
        // A designated element after the first ends the span, and starts
        // one of its own.
        (
            "struct G { char s[4]; int n; } g[3] = {\"ab\", [2] = \"cd\", 3};",
            &[1, 2],
        ),
    ];
    for (src, spans) in cases {
        let (types, typ, elements) = last_initializer(src);
        let slot = types.base_type(typ).unwrap();
        let mut at = 0;
        for want in *spans {
            assert!(
                is_brace_elision_candidate(&types, &elements[at], slot),
                "{src}: element {at} not elided"
            );
            let span = brace_elision_span(&types, &elements, at, slot);
            assert_eq!(span.len(), *want, "{src}: span at {at}");
            at = span.end;
        }
        assert_eq!(at, elements.len(), "{src}: elements left over");
    }
}

/// A string literal is an elided list's first element unless the slot is an
/// array of integers, which takes it whole (6.7.9p14-15) -- and, when it does
/// not suit, rejects it there as gcc does.
#[test]
fn test_string_literal_elides_into_non_character_aggregates() {
    let (types, typ, elements) =
        last_initializer("struct In { char s[4]; double d; }; struct In v[1] = {\"abc\", 2.5};");
    let slot = types.base_type(typ).unwrap();
    assert!(is_brace_elision_candidate(&types, &elements[0], slot));
    let char_array = types.composite(slot).unwrap().members[0].typ;
    assert!(!is_brace_elision_candidate(
        &types,
        &elements[0],
        char_array
    ));
}

/// The parser's walk follows elided braces, so each element is checked
/// against the subobject it initializes -- `{.y = 5}` against `struct Pt`,
/// not the row of them, and `2.5` against the `double`, not the pointer.
#[test]
fn test_initializer_walk_follows_brace_elision() {
    for src in [
        "struct In { char s[4]; double d; }; \
         struct Out { struct In in; int *p; } v = {\"abc\", 2.5, 0};",
        "struct Pt { int x, y; }; struct Pt grid[2][2] = {1, 2, {.y = 5}, 7};",
        "struct In { int s[3]; int n; }; struct In a[2] = {L\"ab\", 3, L\"c\", 4};",
        "struct In { char s[4]; int n; }; \
         struct Out { int a; struct In in; struct In j; } o = {.in = \"abc\", 7, \"de\", 8};",
        "struct Pt { int x, y; }; struct Pt pa[3] = {1, 2, [2] = 3, 4};",
        "union U { struct { char s[4]; int n; } in; int k; } u = {\"uv\", 4};",
        "struct S { char s[4]; int n; } s = {{\"abc\"}, 1};",
        "char c[4] = {\"abc\"};",
    ] {
        assert_clean(src);
    }
    for src in [
        // An array of integers the string does not suit takes it and fails.
        "struct S { short h[3]; int n; } s = {\"ab\", 1};",
        "struct S { _Bool b[3]; int n; } s = {\"ab\", 1};",
        // The walk still finds a real mismatch after a string.
        "struct In { char s[4]; double d; }; struct In v = {\"abc\", \"x\"};",
        "struct Pt { int x, y; }; struct Pt g[2][2] = {1, 2, {.z = 5}};",
    ] {
        assert_rejected(src);
    }
}

/// The excess-element count walks elided braces too: the union's one member
/// takes both elements, and a third `struct In` member does not exist.
#[test]
fn test_excess_initializers_count_by_subobject() {
    assert_clean("union U { struct { char s[4]; int n; } in; int k; } u = {\"uv\", 4};");
    for src in [
        "union U { struct { int x, y; } p; int n; } u = {1, 2, 3};",
        "struct In { char s[4]; int n; } x = {\"ab\", 1, 2};",
        "struct Pt { int x, y; }; struct Pt a[1] = {1, 2, 3};",
    ] {
        let warnings = crate::diag::warning_count();
        parse_tu(src).unwrap();
        assert!(crate::diag::warning_count() > warnings, "{src}: no warning");
    }
}

/// C17 6.7.9p17: a positional element after a designator chain lands in the
/// subobject after the one the chain named. The parser spells that landing
/// place out as the element's own designator chain, so everything after it
/// -- its checks, the array's size, the linearizer -- sees where it goes.
#[test]
fn test_designator_chain_continuation_is_spelled_out() {
    let field = |s: &str, strings: &crate::strings::StringTable| {
        Designator::Field(strings.lookup(s).expect("interned"))
    };
    let src = "struct Pt { int x, y; }; struct E3 { int k; struct Pt a[2]; int z; } \
               e = {.a[0].y = 1, 2, 3, 4};";
    let (tu, _, strings, _) = parse_tu_for(src, &wchar_is_int()).unwrap();
    let Some(ExternalDecl::Declaration(decl)) = tu.items.last() else {
        panic!("no declaration");
    };
    let Some(ExprKind::InitList { elements }) = decl.declarators[0].init.as_ref().map(|e| &e.kind)
    else {
        panic!("no initializer list");
    };
    let chains: Vec<_> = elements.iter().map(|e| e.designators.clone()).collect();
    assert_eq!(
        chains,
        vec![
            vec![
                field("a", &strings),
                Designator::Index(0),
                field("y", &strings)
            ],
            // `2` starts `a[1]` and elides into it, taking `3` with it...
            vec![field("a", &strings), Designator::Index(1)],
            vec![],
            // ...and `4` is the enclosing list's next member, after `a`.
            vec![],
        ]
    );
}

/// The continuation is checked against the subobject it lands in, counted
/// for excess there, and counted where it lands when it sizes an array.
#[test]
fn test_designator_chain_continuation_checks_and_sizes() {
    assert_clean(
        "struct Pt { int x, y; }; struct S { struct Pt a; double *p; } s = {.a.x = 1, 2, 0};",
    );
    assert_clean("struct Pt { int x, y; }; struct Pt pc[2][2] = {[1][0] = 5, 6};");
    for src in [
        "struct Pt { int x, y; }; struct E2 { struct Pt a; int t[2]; } e = {.a.x = 1, 2, 3, 4, 5};",
        "struct Pt { int x, y; }; struct Pt p[1] = {[0].x = 1, 2, 3};",
        "union U { int i; float f; } u = {.f = 1.0f, 2};",
    ] {
        let warnings = crate::diag::warning_count();
        parse_tu_for(src, &wchar_is_int()).unwrap();
        assert!(crate::diag::warning_count() > warnings, "{src}: no warning");
    }
    for (src, len) in [
        (
            "struct Pt { int x, y; }; struct Pt q[] = {[1].x = 1, 2, 3, 4};",
            3,
        ),
        (
            "struct Pt { int x, y; }; struct Pt q[] = {[3].y = 1, 2, 3};",
            5,
        ),
    ] {
        let (types, typ, _) = last_initializer(src);
        assert_eq!(types.array_size(typ), Some(len), "{src}");
    }
}

/// A continuation that lands on an anonymous member has no name to spell,
/// so its chain names the member by its position in the member list --
/// and a braced list there initializes that member, not a scalar inside it.
#[test]
fn test_continuation_into_an_anonymous_member_is_spelled_by_position() {
    let cases: &[(&str, &[&str])] = &[
        (
            "struct A { int k; struct { int p, q; }; int z; }; \
             struct B { struct A a; int w; } b = {.a.k = 1, {2, 3}, 4, 5};",
            &[
                "[Field(_), Field(_)]",
                "[Field(_), Member(1)]",
                "[Field(_), Field(_)]",
                "[]",
            ],
        ),
        (
            "struct A2 { int k; union { int p; float f; }; int z; }; \
             struct B2 { struct A2 a; int w; } b = {.a.k = 1, {2}, 4, 5};",
            &[
                "[Field(_), Field(_)]",
                "[Field(_), Member(1)]",
                "[Field(_), Field(_)]",
                "[]",
            ],
        ),
        // `.p` reaches through the anonymous member at position 1, and the
        // braced list lands on the one nested inside it, at its position 1.
        (
            "struct C { int k; struct { int p; struct { int r, s; }; }; int z; } \
             c = {.p = 1, {2, 3}, 4};",
            &["[Field(_)]", "[Member(1), Member(1)]", "[]"],
        ),
    ];
    for (src, want) in cases {
        let (_, _, elements) = last_initializer(src);
        let got: Vec<String> = elements
            .iter()
            .map(|e| {
                let names: Vec<String> = e
                    .designators
                    .iter()
                    .map(|d| match d {
                        Designator::Field(_) => "Field(_)".to_string(),
                        other => format!("{other:?}"),
                    })
                    .collect();
                format!("[{}]", names.join(", "))
            })
            .collect();
        assert_eq!(got, *want, "{src}");
    }
    for src in [
        "struct A { int k; struct { int p, q; }; int z; }; \
         struct B { struct A a; int w; } b = {.a.k = 1, {2, 3}, 4, 5};",
        "struct A { int k; struct { int p, q; }; int z; } a = {1, {2, 3}, 4};",
        "struct C { int k; struct { int p; struct { int r, s; }; }; int z; }; \
         struct D { struct C c; int w; } d = {.c.k = 1, {2, {3, 4}}, 5, 6};",
    ] {
        assert_clean(src);
    }
}

/// The type the file-scope identifier `name` has once `src` is parsed.
fn file_scope_type(src: &str, name: &str) -> (TypeTable, TypeId) {
    let (_, types, strings, symbols) = parse_tu_for(src, &wchar_is_int()).unwrap();
    let id = strings.lookup(name).expect("interned");
    let typ = symbols
        .lookup(id, Namespace::Ordinary)
        .expect("declared")
        .typ;
    (types, typ)
}

/// C17 6.2.7p3-4: after a later declaration of the same object, the
/// identifier has the composite type, wherever in the type the incomplete
/// array it completes sits. Only a top-level array used to be merged, so
/// `int (*q)[]; int (*q)[3];` left `sizeof *q` at 0.
#[test]
fn test_redeclaration_has_the_composite_type() {
    for src in [
        // A pointer to an array, completed by the later declaration and kept
        // complete by a later incomplete one.
        "int (*q)[]; int (*q)[3]; _Static_assert(sizeof *q == 12, \"\");",
        "int (*q)[3]; int (*q)[]; _Static_assert(sizeof *q == 12, \"\");",
        "extern int (*q)[]; extern int (*q)[3]; _Static_assert(sizeof *q == 12, \"\");",
        // Nested arrays: the outer extent completes, the inner one is kept.
        "extern int m[][4]; int m[2][4]; _Static_assert(sizeof m == 32, \"\");",
        "int m[2][4]; extern int m[][4]; _Static_assert(sizeof m == 32, \"\");",
        // A pointer to a pointer to an array.
        "int (**pp)[]; int (**pp)[2]; _Static_assert(sizeof **pp == 8, \"\");",
        // Qualifiers on the element type survive the composite.
        "const int (*c)[]; const int (*c)[2]; _Static_assert(sizeof *c == 8, \"\");",
        // A function returning a pointer to an incomplete array, completed
        // by a later declaration or by the definition.
        "int (*fp(void))[]; int (*fp(void))[6]; \
         _Static_assert(sizeof *fp() == 24, \"\");",
        "extern int (*fp(void))[]; int (*fp(void))[6] { return 0; } \
         _Static_assert(sizeof *fp() == 24, \"\");",
        "int (*fp(void))[6]; int (*fp(void))[] { return 0; } \
         _Static_assert(sizeof *fp() == 24, \"\");",
    ] {
        assert_clean(src);
    }
}

/// A function's composite type composes its parameters (C17 6.2.7p3), and
/// takes the prototype when only one declaration has one.
#[test]
fn test_function_redeclaration_composes_parameters() {
    let param = |src: &str| {
        let (types, f) = file_scope_type(src, "f");
        let params = types.get(f).params.clone().expect("a prototype");
        let pointee = types.base_type(params[0]).expect("a pointer");
        types.get(pointee).array_size
    };
    assert_eq!(param("int f(int (*a)[]); int f(int (*a)[5]);"), Some(5));
    assert_eq!(param("int f(int (*a)[5]); int f(int (*a)[]);"), Some(5));
    assert_eq!(
        param("int f(int (*a)[]); int f(int (*a)[5]) { return sizeof *a; }"),
        Some(5)
    );
    assert_eq!(
        param("int f(int (*a)[5]); int f(int (*a)[]) { return 0; }"),
        Some(5)
    );
    // A parameter that is itself a pointer to a function of a pointer to
    // an array.
    let (types, g) = file_scope_type(
        "void g(void (*)(int (*)[])); void g(void (*)(int (*)[7]));",
        "g",
    );
    let callback = types.get(g).params.as_ref().unwrap()[0];
    let callback_fn = types.base_type(callback).unwrap();
    let array_ptr = types.get(callback_fn).params.as_ref().unwrap()[0];
    let array = types.base_type(array_ptr).unwrap();
    assert_eq!(types.get(array).array_size, Some(7));

    // Prototyped and unprototyped, in either order: the prototype wins.
    for src in ["int f(); int f(int (*)[]);", "int f(int (*)[]); int f();"] {
        let (types, f) = file_scope_type(src, "f");
        assert_eq!(types.get(f).params.as_ref().map(Vec::len), Some(1), "{src}");
    }
    // A block-scope declaration without a prototype gets the file-scope one,
    // so a call through it is checked against it, as gcc does.
    assert_rejected("int h(int); void k(void) { int h(); h(1, 2); }");
}

/// A block-scope declaration with linkage composes with the visible
/// file-scope one (C17 6.2.7p4), and with a repeat in its own block; one
/// without linkage is a different object.
#[test]
fn test_block_scope_extern_has_the_composite_type() {
    for src in [
        "int (*q)[3]; void g(void) { extern int (*q)[]; \
         _Static_assert(sizeof *q == 12, \"\"); }",
        "extern int (*q)[]; void g(void) { extern int (*q)[3]; \
         _Static_assert(sizeof *q == 12, \"\"); }",
        "int (*fp(void))[6]; void g(void) { int (*fp(void))[]; \
         _Static_assert(sizeof *fp() == 24, \"\"); }",
        "void g(void) { extern int z[]; extern int z[3]; \
         _Static_assert(sizeof z == 12, \"\"); }",
        "void g(void) { extern int (*z)[]; extern int (*z)[3]; \
         _Static_assert(sizeof *z == 12, \"\"); }",
    ] {
        assert_clean(src);
    }
    // The inner `q` is its own object, which the outer declaration does not
    // complete.
    assert_rejected(
        "int (*q)[3]; void g(void) { int (*q)[]; _Static_assert(sizeof *q == 12, \"\"); }",
    );
}
