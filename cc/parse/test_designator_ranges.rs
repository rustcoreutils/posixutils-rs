//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// A GNU index range after a field designator, `.m[lo ... hi] = v`, is
// spelled out as one element per index; one first in its chain is kept.
//

use super::ast::{Designator, ExprKind, ExternalDecl, InitElement};
use super::test_parser::parse_tu;

/// The elements of the initializer of the last declaration in `src`.
fn elements(src: &str) -> Vec<InitElement> {
    let (tu, ..) = parse_tu(src).expect("parse");
    let Some(ExternalDecl::Declaration(decl)) = tu.items.into_iter().last() else {
        panic!("expected a declaration last");
    };
    let init = decl.declarators.into_iter().next().unwrap().init.unwrap();
    let ExprKind::InitList { elements } = init.kind else {
        panic!("expected an initializer list");
    };
    elements
}

#[test]
fn a_range_after_a_field_is_one_element_per_index() {
    let els = elements("struct S { int m[4]; int t; }; struct S s = { .m[1 ... 2] = -1, .t = 9 };");
    let chains: Vec<&[Designator]> = els.iter().map(|e| e.designators.as_slice()).collect();
    assert_eq!(chains.len(), 3, "{chains:?}");
    assert!(matches!(
        chains[0],
        [Designator::Field(_), Designator::Index(1)]
    ));
    assert!(matches!(
        chains[1],
        [Designator::Field(_), Designator::Index(2)]
    ));
    assert!(matches!(chains[2], [Designator::Field(_)]));
}

#[test]
fn two_ranges_in_one_chain_give_every_pair() {
    let els = elements(
        "struct In { int k[3]; }; struct S { struct In in[3]; };\n\
         struct S s = { .in[0 ... 1].k[1 ... 2] = 5 };",
    );
    let pairs: Vec<(i64, i64)> = els
        .iter()
        .map(|e| match e.designators.as_slice() {
            [Designator::Field(_), Designator::Index(a), Designator::Field(_), Designator::Index(b)] => {
                (*a, *b)
            }
            other => panic!("{other:?}"),
        })
        .collect();
    assert_eq!(pairs, [(0, 1), (0, 2), (1, 1), (1, 2)]);
}

#[test]
fn a_leading_range_is_kept_whole() {
    let els = elements("int a[8] = { [2 ... 5] = 1 };");
    assert_eq!(els.len(), 1);
    assert!(matches!(
        els[0].designators.as_slice(),
        [Designator::IndexRange(2, 5)]
    ));
}

#[test]
fn a_range_after_a_field_with_a_value_that_is_not_constant_is_refused() {
    let src = "struct S { int m[4]; }; int g; void f(void) { struct S s = { .m[0 ... 3] = g++ }; }";
    assert!(parse_tu(src).is_err());
    for value in ["f()", "v", "(g = 1)", "(struct H){ 0 }"] {
        let src = format!(
            "struct H {{ int x; }}; struct S {{ struct H m[4]; }}; int f(void); \
             volatile struct H v; int g;\n\
             void h(void) {{ struct S s = {{ .m[0 ... 3] = {value} }}; }}"
        );
        assert!(parse_tu(&src).is_err(), "{value}");
    }
}

/// A value that is not an integer constant is spelled out too when
/// evaluating it once per element cannot be told from evaluating it once:
/// a braced address constant (the kernel's `HLIST_HEAD_INIT`), or an
/// expression with no effect and no volatile access.
#[test]
fn a_range_after_a_field_with_a_value_free_of_effects_is_spelled_out() {
    let els = elements(
        "struct H { void *first; }; struct S { struct H q[4]; };\n\
         struct S s = { .q[0 ... 3] = { (void *)8 } };",
    );
    assert_eq!(els.len(), 4);
    assert!(els
        .iter()
        .all(|e| matches!(e.value.kind, ExprKind::InitList { .. })));
    let src = "struct S { double d[4]; int *p[2]; }; int g;\n\
               void h(int n) { int a[3]; struct S s = { .d[1 ... 3] = n * 0.5 + g, \
               .p[0 ... 1] = &a[n] }; (void)s; }";
    assert!(parse_tu(src).is_ok());
}
