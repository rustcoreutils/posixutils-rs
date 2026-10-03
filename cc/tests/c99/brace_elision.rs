//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Brace elision (C17 6.7.9p20) around string literals, braced elements and
// designators, and the positional elements that continue a designator chain
// (6.7.9p17), for objects of static and automatic storage.
//

use crate::common::{compile_and_run, compile_and_run_aarch64};

/// Run `code` at -O0 and -O2 on the host and, when the cross toolchain is
/// present, on linux-aarch64; each must exit 0.
fn runs_clean(name: &str, code: &str) {
    for level in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run(&format!("{name}{level}"), code, &[level.to_string()]),
            0,
            "{name} {level}"
        );
        if let Some(rc) = compile_and_run_aarch64(&format!("{name}_a64"), code, level) {
            assert_eq!(rc, 0, "{name} aarch64 {level}");
        }
    }
}

/// A string literal that fills only a nested structure's first character
/// array is the first element of that structure's elided list. Exempting
/// every string from brace elision gave it the whole `struct In`, so `2.5`
/// was checked against -- and rejected for -- the `int *` after it.
#[test]
fn brace_elision_string_starts_a_nested_struct() {
    let code = r#"
struct In { char s[4]; double d; };
struct Out { struct In in; int *p; } v = {"abc", 2.5, 0};
int main(void) {
    struct Out a = {"xyz", 3.5, 0};
    if (!(v.in.d == 2.5 && v.in.s[1] == 'b' && v.in.s[3] == 0 && v.p == 0)) return 1;
    if (!(a.in.d == 3.5 && a.in.s[2] == 'z' && a.in.s[3] == 0 && a.p == 0)) return 2;
    return 0;
}
"#;
    runs_clean("elide_string_nested", code);
}

/// The same rule where it compiled into the wrong thing: each `struct In`
/// takes a string and an int, and the braced `{"de", 8}` is the second
/// member's own list. Counting the slot's scalars (five) gave all three
/// elements to `in`.
#[test]
fn brace_elision_string_and_int_per_struct_member() {
    let code = r#"
struct In { char s[4]; int n; };
struct Out { struct In in; struct In j; } v = {"abc", 7, {"de", 8}};
int main(void) {
    struct Out a = {"abc", 7, {"de", 8}};
    if (!(v.in.n == 7 && v.j.n == 8 && v.in.s[2] == 'c' && v.j.s[1] == 'e')) return 1;
    if (!(a.in.n == 7 && a.j.n == 8 && a.in.s[2] == 'c' && a.j.s[1] == 'e')) return 2;
    return 0;
}
"#;
    runs_clean("elide_string_int", code);
}

/// A braced element inside an elided row of structures initializes one
/// structure: `{.y = 5}` is `grid[0][1]`, and `7` starts the next row. The
/// parser checked the designator against the row type, and the slot's
/// scalar count gave the row the `7` as well.
#[test]
fn brace_elision_braced_element_in_a_row_of_structs() {
    let code = r#"
struct Pt { int x, y; };
struct Pt grid[2][2] = {1, 2, {.y = 5}, 7};
static int check(struct Pt (*g)[2]) {
    return !(g[0][0].x == 1 && g[0][0].y == 2 && g[0][1].x == 0 && g[0][1].y == 5
             && g[1][0].x == 7 && g[1][0].y == 0 && g[1][1].x == 0 && g[1][1].y == 0);
}
int main(void) {
    struct Pt a[2][2] = {1, 2, {.y = 5}, 7};
    struct Pt b[2][2][2] = {1, 2, {.y = 5}, 7, 8, {9}, 10};
    if (check(grid)) return 1;
    if (check(a)) return 2;
    if (!(b[0][1][0].x == 7 && b[0][1][0].y == 8 && b[0][1][1].x == 9 && b[1][0][0].x == 10))
        return 3;
    return 0;
}
"#;
    runs_clean("elide_braced_row", code);
}

/// Wide string literals fill the wide arrays of an array of structures;
/// each structure then takes its int. The literal was given the whole
/// structure, which the static image could not hold as a constant.
#[test]
fn brace_elision_wide_strings_in_an_array_of_structs() {
    let code = r#"
#include <stddef.h>
typedef __typeof__(u"x"[0]) char16;
typedef __typeof__(U"x"[0]) char32;
struct W { wchar_t s[3]; int n; } w[2] = {L"ab", 3, L"c", 4};
struct A { char16 s[3]; int n; } u16[2] = {u"ab", 1, u"c", 2};
struct B { char32 s[3]; int n; } u32[2] = {U"ab", 5, U"c", 6};
int main(void) {
    struct W lw[2] = {L"ab", 3, L"c", 4};
    struct A l16[2] = {u"ab", 1, u"c", 2};
    if (!(w[0].n == 3 && w[1].n == 4 && w[0].s[1] == 'b' && w[1].s[0] == 'c' && w[1].s[1] == 0))
        return 1;
    if (!(lw[0].n == 3 && lw[1].n == 4 && lw[0].s[1] == 'b' && lw[1].s[0] == 'c')) return 2;
    if (!(u16[0].s[1] == 'b' && u16[0].n == 1 && u16[1].s[0] == 'c' && u16[1].n == 2)) return 3;
    if (!(l16[0].s[1] == 'b' && l16[0].n == 1 && l16[1].s[0] == 'c' && l16[1].n == 2)) return 4;
    if (!(u32[0].s[1] == 'b' && u32[0].n == 5 && u32[1].s[0] == 'c' && u32[1].n == 6)) return 5;
    return 0;
}
"#;
    runs_clean("elide_wide_strings", code);
}

/// Other slots a string literal can start: a pointer member (one scalar), a
/// two-dimensional character array (one string per row), an array of
/// structures inside a structure, and a union's structure member.
#[test]
fn brace_elision_string_literal_variants() {
    let code = r#"
struct D { const char *p; int n; } d[2] = {"x", 1, "yz", 2};
struct E { char m[2][4]; int n; } e[2] = {"ab", "cd", 5, "ef", "gh", 6};
struct F { struct { char s[3]; int k; } in[2]; int z; } f = {"a", 1, "b", 2, 3};
union U { struct { char s[4]; int n; } in; int k; } u = {"uv", 4};
struct S { char s[4]; int n; } s = {{"abc"}, 1};
static int check(struct D *d, struct E *e, struct F *f, union U *u, struct S *s) {
    if (!(d[0].p[0] == 'x' && d[0].n == 1 && d[1].p[1] == 'z' && d[1].n == 2)) return 1;
    if (!(e[0].m[1][1] == 'd' && e[0].n == 5 && e[1].m[0][0] == 'e' && e[1].m[1][1] == 'h'
          && e[1].n == 6)) return 2;
    if (!(f->in[0].s[0] == 'a' && f->in[0].k == 1 && f->in[1].s[0] == 'b' && f->in[1].k == 2
          && f->z == 3)) return 3;
    if (!(u->in.s[1] == 'v' && u->in.n == 4)) return 4;
    if (!(s->s[2] == 'c' && s->n == 1)) return 5;
    return 0;
}
int main(void) {
    struct D ld[2] = {"x", 1, "yz", 2};
    struct E le[2] = {"ab", "cd", 5, "ef", "gh", 6};
    struct F lf = {"a", 1, "b", 2, 3};
    union U lu = {"uv", 4};
    struct S ls = {{"abc"}, 1};
    int r = check(d, e, &f, &u, &s);
    return r ? r : check(ld, le, &lf, &lu, &ls) * 10;
}
"#;
    runs_clean("elide_string_variants", code);
}

/// A designated element elides braces into the subobject it names, as a
/// positional one would (`[2] = "cd", 3` fills the third structure), and a
/// designator after an elided span addresses the enclosing list.
#[test]
fn brace_elision_with_designators() {
    let code = r#"
struct Pt { int x, y; };
struct In { char s[4]; int n; };
struct O { int a; struct In in; struct In j; int z; };
struct In g[3] = {"ab", 1, [2] = "cd", 3};
struct O o = {.in = "abc", 7, "de", 8, 9};
struct Pt p[3] = {1, 2, [2] = 3, 4};
struct In h[] = {[1] = "ab", 1, "c", 2};
static int check(struct In *g, struct O *o, struct Pt *p, struct In *h) {
    if (!(g[0].s[1] == 'b' && g[0].n == 1 && g[1].s[0] == 0 && g[1].n == 0)) return 1;
    if (!(g[2].s[1] == 'd' && g[2].n == 3)) return 2;
    if (!(o->a == 0 && o->in.s[2] == 'c' && o->in.n == 7 && o->j.s[1] == 'e' && o->j.n == 8
          && o->z == 9)) return 3;
    if (!(p[0].x == 1 && p[0].y == 2 && p[1].x == 0 && p[2].x == 3 && p[2].y == 4)) return 4;
    if (!(h[1].s[1] == 'b' && h[1].n == 1 && h[2].s[0] == 'c' && h[2].n == 2)) return 5;
    return 0;
}
int main(void) {
    struct In lg[3] = {"ab", 1, [2] = "cd", 3};
    struct O lo = {.in = "abc", 7, "de", 8, 9};
    struct Pt lp[3] = {1, 2, [2] = 3, 4};
    struct In lh[] = {[1] = "ab", 1, "c", 2};
    if (sizeof h != 3 * sizeof h[0] || sizeof lh != 3 * sizeof lh[0]) return 9;
    int r = check(g, &o, p, h);
    return r ? r : check(lg, &lo, lp, lh) * 10;
}
"#;
    runs_clean("elide_designators", code);
}

/// C17 6.7.9p17: after a designator chain, positional elements continue with
/// the subobject after the one the chain named -- inside the chain's
/// aggregate first, and only then in the enclosing list. They went straight
/// back to the outermost list: `{.a.x = 1, 2, 3}` gave `a.y` nothing and
/// `t` both values.
#[test]
fn designator_chain_continues_inside_a_struct() {
    let code = r#"
struct Pt { int x, y; };
struct In { char s[4]; int n; };
struct E2 { struct Pt a; int t[2]; };
struct E3 { int k; struct Pt a[2]; int z; };
struct AB { struct Pt a; struct Pt b; int z; };
struct UZ { union { int i; float f; } u; int z; };
struct B2 { struct Pt b; int c; };
struct Deep { struct B2 a; int z; };
struct SA { struct In a; int z; };
union UP { struct Pt p; int n; };
struct W { struct E2 e; int k; };

struct E2 e2 = {.a.x = 1, 2, 3};
struct E3 e3 = {.a[0].y = 1, 2, 3, 4};
struct E3 e4 = {.a[1].x = 7, 8, 9};
struct AB ab = {1, 2, .b.y = 3, 4};
struct UZ uz = {.u.f = 1.5f, 2};
struct Deep dp = {.a.b.x = 1, 2, 3, 4};
struct SA sa = {.a.s = "pq", 1, 2};
union UP up = {.p.x = 1, 2};
struct W w = {{.a.x = 1, 2, 3}, 4};

static int check(struct E2 *e2, struct E3 *e3, struct E3 *e4, struct AB *ab, struct UZ *uz,
                 struct Deep *dp, struct SA *sa, union UP *up, struct W *w) {
    if (!(e2->a.x == 1 && e2->a.y == 2 && e2->t[0] == 3 && e2->t[1] == 0)) return 1;
    if (!(e3->k == 0 && e3->a[0].x == 0 && e3->a[0].y == 1 && e3->a[1].x == 2
          && e3->a[1].y == 3 && e3->z == 4)) return 2;
    if (!(e4->a[0].x == 0 && e4->a[1].x == 7 && e4->a[1].y == 8 && e4->z == 9)) return 3;
    if (!(ab->a.x == 1 && ab->a.y == 2 && ab->b.x == 0 && ab->b.y == 3 && ab->z == 4)) return 4;
    if (!(uz->u.f == 1.5f && uz->z == 2)) return 5;
    if (!(dp->a.b.x == 1 && dp->a.b.y == 2 && dp->a.c == 3 && dp->z == 4)) return 6;
    if (!(sa->a.s[1] == 'q' && sa->a.n == 1 && sa->z == 2)) return 7;
    if (!(up->p.x == 1 && up->p.y == 2)) return 8;
    if (!(w->e.a.x == 1 && w->e.a.y == 2 && w->e.t[0] == 3 && w->e.t[1] == 0 && w->k == 4))
        return 9;
    return 0;
}

int main(void) {
    struct E2 l2 = {.a.x = 1, 2, 3};
    struct E3 l3 = {.a[0].y = 1, 2, 3, 4};
    struct E3 l4 = {.a[1].x = 7, 8, 9};
    struct AB lab = {1, 2, .b.y = 3, 4};
    struct UZ luz = {.u.f = 1.5f, 2};
    struct Deep ldp = {.a.b.x = 1, 2, 3, 4};
    struct SA lsa = {.a.s = "pq", 1, 2};
    union UP lup = {.p.x = 1, 2};
    struct W lw = {{.a.x = 1, 2, 3}, 4};
    struct E2 *cl = &(struct E2){.a.x = 1, 2, 3};
    int r = check(&e2, &e3, &e4, &ab, &uz, &dp, &sa, &up, &w);
    if (r) return r;
    r = check(&l2, &l3, &l4, &lab, &luz, &ldp, &lsa, &lup, &lw);
    if (r) return 10 + r;
    return (cl->a.x == 1 && cl->a.y == 2 && cl->t[0] == 3) ? 0 : 30;
}
"#;
    runs_clean("desig_chain_struct", code);
}

/// The same rule through array designators: `[1][0] = 5, 6` elides into
/// `pc[1][0]`, `[0].x = 1, 2, 3` gives `p[0].y` the 2 and `p[1]` the 3, and
/// an array sized by its initializer counts the continuation where it lands.
#[test]
fn designator_chain_continues_inside_an_array() {
    let code = r#"
struct Pt { int x, y; };
struct Pt pc[2][2] = {[1][0] = 5, 6};
struct Pt pd[2][2] = {[1][0] = 5, 6, 7, 8};
struct Pt p0[2] = {[0].x = 1, 2, 3};
struct Pt q[] = {[1].x = 1, 2, 3, 4};
struct Pt q3[] = {[3].y = 1, 2, 3};
int m[2][3] = {[0][1] = 1, 2, 3};

static int check(struct Pt (*pc)[2], struct Pt (*pd)[2], struct Pt *p0, struct Pt *q,
                 struct Pt *q3, int (*m)[3]) {
    if (!(pc[0][0].x == 0 && pc[1][0].x == 5 && pc[1][0].y == 6 && pc[1][1].x == 0)) return 1;
    if (!(pd[1][0].x == 5 && pd[1][0].y == 6 && pd[1][1].x == 7 && pd[1][1].y == 8)) return 2;
    if (!(p0[0].x == 1 && p0[0].y == 2 && p0[1].x == 3 && p0[1].y == 0)) return 3;
    if (!(q[0].x == 0 && q[1].x == 1 && q[1].y == 2 && q[2].x == 3 && q[2].y == 4)) return 4;
    if (!(q3[3].y == 1 && q3[4].x == 2 && q3[4].y == 3)) return 5;
    if (!(m[0][0] == 0 && m[0][1] == 1 && m[0][2] == 2 && m[1][0] == 3)) return 6;
    return 0;
}

int main(void) {
    struct Pt lpc[2][2] = {[1][0] = 5, 6};
    struct Pt lpd[2][2] = {[1][0] = 5, 6, 7, 8};
    struct Pt lp0[2] = {[0].x = 1, 2, 3};
    struct Pt lq[] = {[1].x = 1, 2, 3, 4};
    struct Pt lq3[] = {[3].y = 1, 2, 3};
    int lm[2][3] = {[0][1] = 1, 2, 3};
    if (sizeof q != 3 * sizeof q[0] || sizeof lq != 3 * sizeof lq[0]) return 20;
    if (sizeof q3 != 5 * sizeof q3[0] || sizeof lq3 != 5 * sizeof lq3[0]) return 21;
    int r = check(pc, pd, p0, q, q3, m);
    return r ? r : check(lpc, lpd, lp0, lq, lq3, lm) * 10;
}
"#;
    runs_clean("desig_chain_array", code);
}

/// Chain continuation through an anonymous member (whose members are named
/// one by one), a GNU range (continued in its last element alone, as gcc
/// does), and chains that size an array or mix array and member designators.
#[test]
fn designator_chain_continues_through_anonymous_members_and_ranges() {
    let code = r#"
struct Pt { int x, y; };
struct A3 { int k; struct { int p, q; }; int z; };
struct N { struct A3 a; int w; };
struct T { struct Pt p[2]; int z; };
struct Pt g[][2] = {[1][0] = 5, 6};
struct Pt r[3] = {[0 ... 1].x = 1, 2, 3};
struct N n1 = {.a.k = 1, 2, 3, 4, 5};
struct N n2 = {.a.p = 1, 2, 3, 4};
struct A3 a3 = {.p = 1, 2, 3};
int mm[][3] = {[1][2] = 1, 2, 3};
struct T t[2] = {[0].p[1].y = 1, 2, 3, [1].p[0] = 4, 5, 6};

static int check_n(struct N *n1, struct N *n2) {
    if (!(n1->a.k == 1 && n1->a.p == 2 && n1->a.q == 3 && n1->a.z == 4 && n1->w == 5)) return 1;
    if (!(n2->a.k == 0 && n2->a.p == 1 && n2->a.q == 2 && n2->a.z == 3 && n2->w == 4)) return 2;
    return 0;
}

static int check_t(struct T *t) {
    if (!(t[0].p[0].x == 0 && t[0].p[1].y == 1 && t[0].z == 2)) return 3;
    if (!(t[1].p[0].x == 4 && t[1].p[0].y == 5 && t[1].p[1].x == 6 && t[1].z == 0)) return 4;
    return 0;
}

int main(void) {
    struct Pt lg[][2] = {[1][0] = 5, 6};
    struct Pt lr[3] = {[0 ... 1].x = 1, 2, 3};
    struct N ln1 = {.a.k = 1, 2, 3, 4, 5};
    struct N ln2 = {.a.p = 1, 2, 3, 4};
    struct T lt[2] = {[0].p[1].y = 1, 2, 3, [1].p[0] = 4, 5, 6};
    if (sizeof g != 2 * sizeof g[0] || sizeof lg != 2 * sizeof lg[0]) return 10;
    if (!(g[1][0].x == 5 && g[1][0].y == 6 && lg[1][0].x == 5 && lg[1][0].y == 6)) return 11;
    if (!(r[0].x == 1 && r[0].y == 0 && r[1].x == 1 && r[1].y == 2 && r[2].x == 3)) return 12;
    if (!(lr[0].x == 1 && lr[0].y == 0 && lr[1].x == 1 && lr[1].y == 2 && lr[2].x == 3)) return 13;
    if (!(a3.k == 0 && a3.p == 1 && a3.q == 2 && a3.z == 3)) return 14;
    if (!(sizeof mm == 3 * sizeof mm[0] && mm[1][2] == 1 && mm[2][0] == 2 && mm[2][1] == 3))
        return 15;
    int r1 = check_n(&n1, &n2);
    if (!r1) r1 = check_t(t);
    if (r1) return r1;
    r1 = check_n(&ln1, &ln2);
    if (!r1) r1 = check_t(lt);
    return r1 ? 20 + r1 : 0;
}
"#;
    runs_clean("desig_chain_anon_range", code);
}

/// A continuation landing on an anonymous member -- a braced list for the
/// whole member, or values eliding into it -- after a chain, with no chain,
/// nested two anonymous levels deep, and in a compound literal. The braced
/// list was taken for a scalar inside the member and its values dropped.
#[test]
fn designator_chain_continues_into_an_anonymous_member() {
    let code = r#"
struct A { int k; struct { int p, q; }; int z; };
struct B { struct A a; int w; };
struct A2 { int k; union { int p; float f; }; int z; };
struct B2 { struct A2 a; int w; };
struct C { int k; struct { int p; struct { int r, s; }; }; int z; };
struct D { struct C c; int w; };

struct B b = {.a.k = 1, {2, 3}, 4, 5};
struct B2 b2 = {.a.k = 1, {2}, 4, 5};
struct A a = {1, {2, 3}, 4};
struct D d1 = {.c.k = 1, 2, {3, 4}, 5, 6};
struct D d2 = {.c.k = 1, {2, {3, 4}}, 5, 6};
struct C c2 = {.p = 1, {2, 3}, 4};

static int check(struct B *b, struct B2 *b2, struct A *a, struct D *d1, struct D *d2,
                 struct C *c2) {
    if (!(b->a.k == 1 && b->a.p == 2 && b->a.q == 3 && b->a.z == 4 && b->w == 5)) return 1;
    if (!(b2->a.k == 1 && b2->a.p == 2 && b2->a.z == 4 && b2->w == 5)) return 2;
    if (!(a->k == 1 && a->p == 2 && a->q == 3 && a->z == 4)) return 3;
    if (!(d1->c.k == 1 && d1->c.p == 2 && d1->c.r == 3 && d1->c.s == 4 && d1->c.z == 5
          && d1->w == 6)) return 4;
    if (!(d2->c.k == 1 && d2->c.p == 2 && d2->c.r == 3 && d2->c.s == 4 && d2->c.z == 5
          && d2->w == 6)) return 5;
    if (!(c2->k == 0 && c2->p == 1 && c2->r == 2 && c2->s == 3 && c2->z == 4)) return 6;
    return 0;
}

int main(void) {
    struct B lb = {.a.k = 1, {2, 3}, 4, 5};
    struct B2 lb2 = {.a.k = 1, {2}, 4, 5};
    struct A la = {1, {2, 3}, 4};
    struct D ld1 = {.c.k = 1, 2, {3, 4}, 5, 6};
    struct D ld2 = {.c.k = 1, {2, {3, 4}}, 5, 6};
    struct C lc2 = {.p = 1, {2, 3}, 4};
    struct B *cl = &(struct B){.a.k = 1, {2, 3}, 4, 5};
    int r = check(&b, &b2, &a, &d1, &d2, &c2);
    if (r) return r;
    r = check(&lb, &lb2, &la, &ld1, &ld2, &lc2);
    if (r) return 10 + r;
    return (cl->a.p == 2 && cl->a.q == 3 && cl->a.z == 4 && cl->w == 5) ? 0 : 30;
}
"#;
    runs_clean("desig_chain_anon_member", code);
}
