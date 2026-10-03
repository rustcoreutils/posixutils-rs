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

use super::test_parser::parse_tu;
use crate::symbol::{Linkage, Namespace};

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
