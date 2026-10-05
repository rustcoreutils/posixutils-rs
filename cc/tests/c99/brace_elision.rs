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

/// One program, one section per original test; each section carries the
/// original doc comment and its exit-code range in its header. Consolidates:
/// `elide_string_nested`, `elide_string_int`, `elide_braced_row`,
/// `elide_wide_strings`, `elide_string_variants`, `elide_designators`,
/// `desig_chain_struct`.
#[test]
fn c99_brace_elision_mega() {
    let code = r#"
#include <stddef.h>

// ==========================================================================
// elide_string_nested  (exit codes 1-2: 0 + its own code)
//
// (was #[test] brace_elision_string_starts_a_nested_struct)
//
// A string literal that fills only a nested structure's first character
// array is the first element of that structure's elided list. Exempting
// every string from brace elision gave it the whole `struct In`, so `2.5`
// was checked against -- and rejected for -- the `int *` after it.
// ==========================================================================
struct bsn_In { char s[4]; double d; };
struct bsn_Out { struct bsn_In in; int *p; } bsn_v = {"abc", 2.5, 0};
static int t_elide_string_nested(void) {
    struct bsn_Out a = {"xyz", 3.5, 0};
    if (!(bsn_v.in.d == 2.5 && bsn_v.in.s[1] == 'b' && bsn_v.in.s[3] == 0 && bsn_v.p == 0)) return 1;
    if (!(a.in.d == 3.5 && a.in.s[2] == 'z' && a.in.s[3] == 0 && a.p == 0)) return 2;
    return 0;
}

// ==========================================================================
// elide_string_int  (exit codes 3-4: 2 + its own code)
//
// (was #[test] brace_elision_string_and_int_per_struct_member)
//
// The same rule where it compiled into the wrong thing: each `struct In`
// takes a string and an int, and the braced `{"de", 8}` is the second
// member's own list. Counting the slot's scalars (five) gave all three
// elements to `in`.
// ==========================================================================
struct bsi_In { char s[4]; int n; };
struct bsi_Out { struct bsi_In in; struct bsi_In j; } bsi_v = {"abc", 7, {"de", 8}};
static int t_elide_string_int(void) {
    struct bsi_Out a = {"abc", 7, {"de", 8}};
    if (!(bsi_v.in.n == 7 && bsi_v.j.n == 8 && bsi_v.in.s[2] == 'c' && bsi_v.j.s[1] == 'e')) return 1;
    if (!(a.in.n == 7 && a.j.n == 8 && a.in.s[2] == 'c' && a.j.s[1] == 'e')) return 2;
    return 0;
}

// ==========================================================================
// elide_braced_row  (exit codes 5-7: 4 + its own code)
//
// (was #[test] brace_elision_braced_element_in_a_row_of_structs)
//
// A braced element inside an elided row of structures initializes one
// structure: `{.y = 5}` is `grid[0][1]`, and `7` starts the next row. The
// parser checked the designator against the row type, and the slot's
// scalar count gave the row the `7` as well.
// ==========================================================================
struct bbr_Pt { int x, y; };
struct bbr_Pt bbr_grid[2][2] = {1, 2, {.y = 5}, 7};
static int bbr_check(struct bbr_Pt (*g)[2]) {
    return !(g[0][0].x == 1 && g[0][0].y == 2 && g[0][1].x == 0 && g[0][1].y == 5
             && g[1][0].x == 7 && g[1][0].y == 0 && g[1][1].x == 0 && g[1][1].y == 0);
}
static int t_elide_braced_row(void) {
    struct bbr_Pt a[2][2] = {1, 2, {.y = 5}, 7};
    struct bbr_Pt b[2][2][2] = {1, 2, {.y = 5}, 7, 8, {9}, 10};
    if (bbr_check(bbr_grid)) return 1;
    if (bbr_check(a)) return 2;
    if (!(b[0][1][0].x == 7 && b[0][1][0].y == 8 && b[0][1][1].x == 9 && b[1][0][0].x == 10))
        return 3;
    return 0;
}

// ==========================================================================
// elide_wide_strings  (exit codes 8-12: 7 + its own code)
//
// (was #[test] brace_elision_wide_strings_in_an_array_of_structs)
//
// Wide string literals fill the wide arrays of an array of structures;
// each structure then takes its int. The literal was given the whole
// structure, which the static image could not hold as a constant.
// ==========================================================================
typedef __typeof__(u"x"[0]) bws_char16;
typedef __typeof__(U"x"[0]) bws_char32;
struct bws_W { wchar_t s[3]; int n; } bws_w[2] = {L"ab", 3, L"c", 4};
struct bws_A { bws_char16 s[3]; int n; } bws_u16[2] = {u"ab", 1, u"c", 2};
struct bws_B { bws_char32 s[3]; int n; } bws_u32[2] = {U"ab", 5, U"c", 6};
static int t_elide_wide_strings(void) {
    struct bws_W lw[2] = {L"ab", 3, L"c", 4};
    struct bws_A l16[2] = {u"ab", 1, u"c", 2};
    if (!(bws_w[0].n == 3 && bws_w[1].n == 4 && bws_w[0].s[1] == 'b' && bws_w[1].s[0] == 'c' && bws_w[1].s[1] == 0))
        return 1;
    if (!(lw[0].n == 3 && lw[1].n == 4 && lw[0].s[1] == 'b' && lw[1].s[0] == 'c')) return 2;
    if (!(bws_u16[0].s[1] == 'b' && bws_u16[0].n == 1 && bws_u16[1].s[0] == 'c' && bws_u16[1].n == 2)) return 3;
    if (!(l16[0].s[1] == 'b' && l16[0].n == 1 && l16[1].s[0] == 'c' && l16[1].n == 2)) return 4;
    if (!(bws_u32[0].s[1] == 'b' && bws_u32[0].n == 5 && bws_u32[1].s[0] == 'c' && bws_u32[1].n == 6)) return 5;
    return 0;
}

// ==========================================================================
// elide_string_variants  (exit codes 13-62: 12 + its own code)
//
// (was #[test] brace_elision_string_literal_variants)
//
// Other slots a string literal can start: a pointer member (one scalar), a
// two-dimensional character array (one string per row), an array of
// structures inside a structure, and a union's structure member.
// ==========================================================================
struct bsv_D { const char *p; int n; } bsv_d[2] = {"x", 1, "yz", 2};
struct bsv_E { char m[2][4]; int n; } bsv_e[2] = {"ab", "cd", 5, "ef", "gh", 6};
struct bsv_F { struct { char bsv_s[3]; int k; } in[2]; int z; } bsv_f = {"a", 1, "b", 2, 3};
union bsv_U { struct { char bsv_s[4]; int n; } in; int k; } bsv_u = {"uv", 4};
struct bsv_S { char bsv_s[4]; int n; } bsv_s = {{"abc"}, 1};
static int bsv_check(struct bsv_D *bsv_d, struct bsv_E *bsv_e, struct bsv_F *bsv_f, union bsv_U *bsv_u, struct bsv_S *bsv_s) {
    if (!(bsv_d[0].p[0] == 'x' && bsv_d[0].n == 1 && bsv_d[1].p[1] == 'z' && bsv_d[1].n == 2)) return 1;
    if (!(bsv_e[0].m[1][1] == 'd' && bsv_e[0].n == 5 && bsv_e[1].m[0][0] == 'e' && bsv_e[1].m[1][1] == 'h'
          && bsv_e[1].n == 6)) return 2;
    if (!(bsv_f->in[0].bsv_s[0] == 'a' && bsv_f->in[0].k == 1 && bsv_f->in[1].bsv_s[0] == 'b' && bsv_f->in[1].k == 2
          && bsv_f->z == 3)) return 3;
    if (!(bsv_u->in.bsv_s[1] == 'v' && bsv_u->in.n == 4)) return 4;
    if (!(bsv_s->bsv_s[2] == 'c' && bsv_s->n == 1)) return 5;
    return 0;
}
static int t_elide_string_variants(void) {
    struct bsv_D ld[2] = {"x", 1, "yz", 2};
    struct bsv_E le[2] = {"ab", "cd", 5, "ef", "gh", 6};
    struct bsv_F lf = {"a", 1, "b", 2, 3};
    union bsv_U lu = {"uv", 4};
    struct bsv_S ls = {{"abc"}, 1};
    int r = bsv_check(bsv_d, bsv_e, &bsv_f, &bsv_u, &bsv_s);
    return r ? r : bsv_check(ld, le, &lf, &lu, &ls) * 10;
}

// ==========================================================================
// elide_designators  (exit codes 63-112: 62 + its own code)
//
// (was #[test] brace_elision_with_designators)
//
// A designated element elides braces into the subobject it names, as a
// positional one would (`[2] = "cd", 3` fills the third structure), and a
// designator after an elided span addresses the enclosing list.
// ==========================================================================
struct bde_Pt { int x, y; };
struct bde_In { char s[4]; int n; };
struct bde_O { int a; struct bde_In in; struct bde_In j; int z; };
struct bde_In bde_g[3] = {"ab", 1, [2] = "cd", 3};
struct bde_O bde_o = {.in = "abc", 7, "de", 8, 9};
struct bde_Pt bde_p[3] = {1, 2, [2] = 3, 4};
struct bde_In bde_h[] = {[1] = "ab", 1, "c", 2};
static int bde_check(struct bde_In *bde_g, struct bde_O *bde_o, struct bde_Pt *bde_p, struct bde_In *bde_h) {
    if (!(bde_g[0].s[1] == 'b' && bde_g[0].n == 1 && bde_g[1].s[0] == 0 && bde_g[1].n == 0)) return 1;
    if (!(bde_g[2].s[1] == 'd' && bde_g[2].n == 3)) return 2;
    if (!(bde_o->a == 0 && bde_o->in.s[2] == 'c' && bde_o->in.n == 7 && bde_o->j.s[1] == 'e' && bde_o->j.n == 8
          && bde_o->z == 9)) return 3;
    if (!(bde_p[0].x == 1 && bde_p[0].y == 2 && bde_p[1].x == 0 && bde_p[2].x == 3 && bde_p[2].y == 4)) return 4;
    if (!(bde_h[1].s[1] == 'b' && bde_h[1].n == 1 && bde_h[2].s[0] == 'c' && bde_h[2].n == 2)) return 5;
    return 0;
}
static int t_elide_designators(void) {
    struct bde_In lg[3] = {"ab", 1, [2] = "cd", 3};
    struct bde_O lo = {.in = "abc", 7, "de", 8, 9};
    struct bde_Pt lp[3] = {1, 2, [2] = 3, 4};
    struct bde_In lh[] = {[1] = "ab", 1, "c", 2};
    if (sizeof bde_h != 3 * sizeof bde_h[0] || sizeof lh != 3 * sizeof lh[0]) return 9;
    int r = bde_check(bde_g, &bde_o, bde_p, bde_h);
    return r ? r : bde_check(lg, &lo, lp, lh) * 10;
}

// ==========================================================================
// desig_chain_struct  (exit codes 113-142: 112 + its own code)
//
// (was #[test] designator_chain_continues_inside_a_struct)
//
// C17 6.7.9p17: after a designator chain, positional elements continue with
// the subobject after the one the chain named -- inside the chain's
// aggregate first, and only then in the enclosing list. They went straight
// back to the outermost list: `{.a.x = 1, 2, 3}` gave `a.y` nothing and
// `t` both values.
// ==========================================================================
struct dcs_Pt { int x, y; };
struct dcs_In { char s[4]; int n; };
struct dcs_E2 { struct dcs_Pt a; int t[2]; };
struct dcs_E3 { int k; struct dcs_Pt a[2]; int z; };
struct dcs_AB { struct dcs_Pt a; struct dcs_Pt b; int z; };
struct dcs_UZ { union { int i; float f; } u; int z; };
struct dcs_B2 { struct dcs_Pt b; int c; };
struct dcs_Deep { struct dcs_B2 a; int z; };
struct dcs_SA { struct dcs_In a; int z; };
union dcs_UP { struct dcs_Pt p; int n; };
struct dcs_W { struct dcs_E2 e; int k; };

struct dcs_E2 dcs_e2 = {.a.x = 1, 2, 3};
struct dcs_E3 dcs_e3 = {.a[0].y = 1, 2, 3, 4};
struct dcs_E3 dcs_e4 = {.a[1].x = 7, 8, 9};
struct dcs_AB dcs_ab = {1, 2, .b.y = 3, 4};
struct dcs_UZ dcs_uz = {.u.f = 1.5f, 2};
struct dcs_Deep dcs_dp = {.a.b.x = 1, 2, 3, 4};
struct dcs_SA dcs_sa = {.a.s = "pq", 1, 2};
union dcs_UP dcs_up = {.p.x = 1, 2};
struct dcs_W dcs_w = {{.a.x = 1, 2, 3}, 4};

static int dcs_check(struct dcs_E2 *dcs_e2, struct dcs_E3 *dcs_e3, struct dcs_E3 *dcs_e4, struct dcs_AB *dcs_ab, struct dcs_UZ *dcs_uz,
                 struct dcs_Deep *dcs_dp, struct dcs_SA *dcs_sa, union dcs_UP *dcs_up, struct dcs_W *dcs_w) {
    if (!(dcs_e2->a.x == 1 && dcs_e2->a.y == 2 && dcs_e2->t[0] == 3 && dcs_e2->t[1] == 0)) return 1;
    if (!(dcs_e3->k == 0 && dcs_e3->a[0].x == 0 && dcs_e3->a[0].y == 1 && dcs_e3->a[1].x == 2
          && dcs_e3->a[1].y == 3 && dcs_e3->z == 4)) return 2;
    if (!(dcs_e4->a[0].x == 0 && dcs_e4->a[1].x == 7 && dcs_e4->a[1].y == 8 && dcs_e4->z == 9)) return 3;
    if (!(dcs_ab->a.x == 1 && dcs_ab->a.y == 2 && dcs_ab->b.x == 0 && dcs_ab->b.y == 3 && dcs_ab->z == 4)) return 4;
    if (!(dcs_uz->u.f == 1.5f && dcs_uz->z == 2)) return 5;
    if (!(dcs_dp->a.b.x == 1 && dcs_dp->a.b.y == 2 && dcs_dp->a.c == 3 && dcs_dp->z == 4)) return 6;
    if (!(dcs_sa->a.s[1] == 'q' && dcs_sa->a.n == 1 && dcs_sa->z == 2)) return 7;
    if (!(dcs_up->p.x == 1 && dcs_up->p.y == 2)) return 8;
    if (!(dcs_w->e.a.x == 1 && dcs_w->e.a.y == 2 && dcs_w->e.t[0] == 3 && dcs_w->e.t[1] == 0 && dcs_w->k == 4))
        return 9;
    return 0;
}

static int t_desig_chain_struct(void) {
    struct dcs_E2 l2 = {.a.x = 1, 2, 3};
    struct dcs_E3 l3 = {.a[0].y = 1, 2, 3, 4};
    struct dcs_E3 l4 = {.a[1].x = 7, 8, 9};
    struct dcs_AB lab = {1, 2, .b.y = 3, 4};
    struct dcs_UZ luz = {.u.f = 1.5f, 2};
    struct dcs_Deep ldp = {.a.b.x = 1, 2, 3, 4};
    struct dcs_SA lsa = {.a.s = "pq", 1, 2};
    union dcs_UP lup = {.p.x = 1, 2};
    struct dcs_W lw = {{.a.x = 1, 2, 3}, 4};
    struct dcs_E2 *cl = &(struct dcs_E2){.a.x = 1, 2, 3};
    int r = dcs_check(&dcs_e2, &dcs_e3, &dcs_e4, &dcs_ab, &dcs_uz, &dcs_dp, &dcs_sa, &dcs_up, &dcs_w);
    if (r) return r;
    r = dcs_check(&l2, &l3, &l4, &lab, &luz, &ldp, &lsa, &lup, &lw);
    if (r) return 10 + r;
    return (cl->a.x == 1 && cl->a.y == 2 && cl->t[0] == 3) ? 0 : 30;
}

int main(void) {
    int r;
    if ((r = t_elide_string_nested()) != 0) return 0 + r;
    if ((r = t_elide_string_int()) != 0) return 2 + r;
    if ((r = t_elide_braced_row()) != 0) return 4 + r;
    if ((r = t_elide_wide_strings()) != 0) return 7 + r;
    if ((r = t_elide_string_variants()) != 0) return 12 + r;
    if ((r = t_elide_designators()) != 0) return 62 + r;
    if ((r = t_desig_chain_struct()) != 0) return 112 + r;
    return 0;
}
"#;
    runs_clean("c99_brace_elision_mega", code);
}

/// One program, one section per original test; each section carries the
/// original doc comment and its exit-code range in its header. Consolidates:
/// `desig_chain_array`, `desig_chain_anon_range`, `desig_chain_anon_member`.
#[test]
fn c99_designator_chain_mega() {
    let code = r#"
// ==========================================================================
// desig_chain_array  (exit codes 1-60: 0 + its own code)
//
// (was #[test] designator_chain_continues_inside_an_array)
//
// The same rule through array designators: `[1][0] = 5, 6` elides into
// `pc[1][0]`, `[0].x = 1, 2, 3` gives `p[0].y` the 2 and `p[1]` the 3, and
// an array sized by its initializer counts the continuation where it lands.
// ==========================================================================
struct dca_Pt { int x, y; };
struct dca_Pt dca_pc[2][2] = {[1][0] = 5, 6};
struct dca_Pt dca_pd[2][2] = {[1][0] = 5, 6, 7, 8};
struct dca_Pt dca_p0[2] = {[0].x = 1, 2, 3};
struct dca_Pt dca_q[] = {[1].x = 1, 2, 3, 4};
struct dca_Pt dca_q3[] = {[3].y = 1, 2, 3};
int dca_m[2][3] = {[0][1] = 1, 2, 3};

static int dca_check(struct dca_Pt (*dca_pc)[2], struct dca_Pt (*dca_pd)[2], struct dca_Pt *dca_p0, struct dca_Pt *dca_q,
                 struct dca_Pt *dca_q3, int (*dca_m)[3]) {
    if (!(dca_pc[0][0].x == 0 && dca_pc[1][0].x == 5 && dca_pc[1][0].y == 6 && dca_pc[1][1].x == 0)) return 1;
    if (!(dca_pd[1][0].x == 5 && dca_pd[1][0].y == 6 && dca_pd[1][1].x == 7 && dca_pd[1][1].y == 8)) return 2;
    if (!(dca_p0[0].x == 1 && dca_p0[0].y == 2 && dca_p0[1].x == 3 && dca_p0[1].y == 0)) return 3;
    if (!(dca_q[0].x == 0 && dca_q[1].x == 1 && dca_q[1].y == 2 && dca_q[2].x == 3 && dca_q[2].y == 4)) return 4;
    if (!(dca_q3[3].y == 1 && dca_q3[4].x == 2 && dca_q3[4].y == 3)) return 5;
    if (!(dca_m[0][0] == 0 && dca_m[0][1] == 1 && dca_m[0][2] == 2 && dca_m[1][0] == 3)) return 6;
    return 0;
}

static int t_desig_chain_array(void) {
    struct dca_Pt lpc[2][2] = {[1][0] = 5, 6};
    struct dca_Pt lpd[2][2] = {[1][0] = 5, 6, 7, 8};
    struct dca_Pt lp0[2] = {[0].x = 1, 2, 3};
    struct dca_Pt lq[] = {[1].x = 1, 2, 3, 4};
    struct dca_Pt lq3[] = {[3].y = 1, 2, 3};
    int lm[2][3] = {[0][1] = 1, 2, 3};
    if (sizeof dca_q != 3 * sizeof dca_q[0] || sizeof lq != 3 * sizeof lq[0]) return 20;
    if (sizeof dca_q3 != 5 * sizeof dca_q3[0] || sizeof lq3 != 5 * sizeof lq3[0]) return 21;
    int r = dca_check(dca_pc, dca_pd, dca_p0, dca_q, dca_q3, dca_m);
    return r ? r : dca_check(lpc, lpd, lp0, lq, lq3, lm) * 10;
}

// ==========================================================================
// desig_chain_anon_range  (exit codes 61-84: 60 + its own code)
//
// (was #[test] designator_chain_continues_through_anonymous_members_and_ranges)
//
// Chain continuation through an anonymous member (whose members are named
// one by one), a GNU range (continued in its last element alone, as gcc
// does), and chains that size an array or mix array and member designators.
// ==========================================================================
struct dcr_Pt { int x, y; };
struct dcr_A3 { int k; struct { int p, q; }; int z; };
struct dcr_N { struct dcr_A3 a; int w; };
struct dcr_T { struct dcr_Pt p[2]; int z; };
struct dcr_Pt dcr_g[][2] = {[1][0] = 5, 6};
struct dcr_Pt dcr_r[3] = {[0 ... 1].x = 1, 2, 3};
struct dcr_N dcr_n1 = {.a.k = 1, 2, 3, 4, 5};
struct dcr_N dcr_n2 = {.a.p = 1, 2, 3, 4};
struct dcr_A3 dcr_a3 = {.p = 1, 2, 3};
int dcr_mm[][3] = {[1][2] = 1, 2, 3};
struct dcr_T dcr_t[2] = {[0].p[1].y = 1, 2, 3, [1].p[0] = 4, 5, 6};

static int dcr_check_n(struct dcr_N *dcr_n1, struct dcr_N *dcr_n2) {
    if (!(dcr_n1->a.k == 1 && dcr_n1->a.p == 2 && dcr_n1->a.q == 3 && dcr_n1->a.z == 4 && dcr_n1->w == 5)) return 1;
    if (!(dcr_n2->a.k == 0 && dcr_n2->a.p == 1 && dcr_n2->a.q == 2 && dcr_n2->a.z == 3 && dcr_n2->w == 4)) return 2;
    return 0;
}

static int dcr_check_t(struct dcr_T *dcr_t) {
    if (!(dcr_t[0].p[0].x == 0 && dcr_t[0].p[1].y == 1 && dcr_t[0].z == 2)) return 3;
    if (!(dcr_t[1].p[0].x == 4 && dcr_t[1].p[0].y == 5 && dcr_t[1].p[1].x == 6 && dcr_t[1].z == 0)) return 4;
    return 0;
}

static int t_desig_chain_anon_range(void) {
    struct dcr_Pt lg[][2] = {[1][0] = 5, 6};
    struct dcr_Pt lr[3] = {[0 ... 1].x = 1, 2, 3};
    struct dcr_N ln1 = {.a.k = 1, 2, 3, 4, 5};
    struct dcr_N ln2 = {.a.p = 1, 2, 3, 4};
    struct dcr_T lt[2] = {[0].p[1].y = 1, 2, 3, [1].p[0] = 4, 5, 6};
    if (sizeof dcr_g != 2 * sizeof dcr_g[0] || sizeof lg != 2 * sizeof lg[0]) return 10;
    if (!(dcr_g[1][0].x == 5 && dcr_g[1][0].y == 6 && lg[1][0].x == 5 && lg[1][0].y == 6)) return 11;
    if (!(dcr_r[0].x == 1 && dcr_r[0].y == 0 && dcr_r[1].x == 1 && dcr_r[1].y == 2 && dcr_r[2].x == 3)) return 12;
    if (!(lr[0].x == 1 && lr[0].y == 0 && lr[1].x == 1 && lr[1].y == 2 && lr[2].x == 3)) return 13;
    if (!(dcr_a3.k == 0 && dcr_a3.p == 1 && dcr_a3.q == 2 && dcr_a3.z == 3)) return 14;
    if (!(sizeof dcr_mm == 3 * sizeof dcr_mm[0] && dcr_mm[1][2] == 1 && dcr_mm[2][0] == 2 && dcr_mm[2][1] == 3))
        return 15;
    int r1 = dcr_check_n(&dcr_n1, &dcr_n2);
    if (!r1) r1 = dcr_check_t(dcr_t);
    if (r1) return r1;
    r1 = dcr_check_n(&ln1, &ln2);
    if (!r1) r1 = dcr_check_t(lt);
    return r1 ? 20 + r1 : 0;
}

// ==========================================================================
// desig_chain_anon_member  (exit codes 85-114: 84 + its own code)
//
// (was #[test] designator_chain_continues_into_an_anonymous_member)
//
// A continuation landing on an anonymous member -- a braced list for the
// whole member, or values eliding into it -- after a chain, with no chain,
// nested two anonymous levels deep, and in a compound literal. The braced
// list was taken for a scalar inside the member and its values dropped.
// ==========================================================================
struct dcm_A { int k; struct { int p, q; }; int z; };
struct dcm_B { struct dcm_A dcm_a; int w; };
struct dcm_A2 { int k; union { int p; float f; }; int z; };
struct dcm_B2 { struct dcm_A2 dcm_a; int w; };
struct dcm_C { int k; struct { int p; struct { int r, s; }; }; int z; };
struct dcm_D { struct dcm_C c; int w; };

struct dcm_B dcm_b = {.dcm_a.k = 1, {2, 3}, 4, 5};
struct dcm_B2 dcm_b2 = {.dcm_a.k = 1, {2}, 4, 5};
struct dcm_A dcm_a = {1, {2, 3}, 4};
struct dcm_D dcm_d1 = {.c.k = 1, 2, {3, 4}, 5, 6};
struct dcm_D dcm_d2 = {.c.k = 1, {2, {3, 4}}, 5, 6};
struct dcm_C dcm_c2 = {.p = 1, {2, 3}, 4};

static int dcm_check(struct dcm_B *dcm_b, struct dcm_B2 *dcm_b2, struct dcm_A *dcm_a, struct dcm_D *dcm_d1, struct dcm_D *dcm_d2,
                 struct dcm_C *dcm_c2) {
    if (!(dcm_b->dcm_a.k == 1 && dcm_b->dcm_a.p == 2 && dcm_b->dcm_a.q == 3 && dcm_b->dcm_a.z == 4 && dcm_b->w == 5)) return 1;
    if (!(dcm_b2->dcm_a.k == 1 && dcm_b2->dcm_a.p == 2 && dcm_b2->dcm_a.z == 4 && dcm_b2->w == 5)) return 2;
    if (!(dcm_a->k == 1 && dcm_a->p == 2 && dcm_a->q == 3 && dcm_a->z == 4)) return 3;
    if (!(dcm_d1->c.k == 1 && dcm_d1->c.p == 2 && dcm_d1->c.r == 3 && dcm_d1->c.s == 4 && dcm_d1->c.z == 5
          && dcm_d1->w == 6)) return 4;
    if (!(dcm_d2->c.k == 1 && dcm_d2->c.p == 2 && dcm_d2->c.r == 3 && dcm_d2->c.s == 4 && dcm_d2->c.z == 5
          && dcm_d2->w == 6)) return 5;
    if (!(dcm_c2->k == 0 && dcm_c2->p == 1 && dcm_c2->r == 2 && dcm_c2->s == 3 && dcm_c2->z == 4)) return 6;
    return 0;
}

static int t_desig_chain_anon_member(void) {
    struct dcm_B lb = {.dcm_a.k = 1, {2, 3}, 4, 5};
    struct dcm_B2 lb2 = {.dcm_a.k = 1, {2}, 4, 5};
    struct dcm_A la = {1, {2, 3}, 4};
    struct dcm_D ld1 = {.c.k = 1, 2, {3, 4}, 5, 6};
    struct dcm_D ld2 = {.c.k = 1, {2, {3, 4}}, 5, 6};
    struct dcm_C lc2 = {.p = 1, {2, 3}, 4};
    struct dcm_B *cl = &(struct dcm_B){.dcm_a.k = 1, {2, 3}, 4, 5};
    int r = dcm_check(&dcm_b, &dcm_b2, &dcm_a, &dcm_d1, &dcm_d2, &dcm_c2);
    if (r) return r;
    r = dcm_check(&lb, &lb2, &la, &ld1, &ld2, &lc2);
    if (r) return 10 + r;
    return (cl->dcm_a.p == 2 && cl->dcm_a.q == 3 && cl->dcm_a.z == 4 && cl->w == 5) ? 0 : 30;
}

int main(void) {
    int r;
    if ((r = t_desig_chain_array()) != 0) return 0 + r;
    if ((r = t_desig_chain_anon_range()) != 0) return 60 + r;
    if ((r = t_desig_chain_anon_member()) != 0) return 84 + r;
    return 0;
}
"#;
    runs_clean("c99_designator_chain_mega", code);
}
