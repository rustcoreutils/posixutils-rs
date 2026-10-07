//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C99 initializers: designated overrides, ranges and excess initializers
//

use crate::common::{compile_and_run, compile_and_run_aarch64, compile_and_run_optimized};

// ============================================================================
// Mega-test: designated overrides, ranges and excess initializers
// ============================================================================

// Original test documentation, in section order:
//
// ---- c99_designated_init_ranges ----
// GNU designated-initializer ranges: `[lo ... hi] = v`.
//
// The second-most-used extension c17 rejected — 357 files in the Linux tree,
// 82 in mesa, 5 in CPython. Both endpoints are inclusive; the positional
// cursor resumes past the *high* one, so `{[0 ... 2] = 1, 9}` puts 9 at index
// 3. A later range overwrites an earlier overlapping one.
//
// The rule lives in two independent places — the initializer grouping and the
// array-size inference used for `int a[] = {...}` — and both are exercised
// here, because the tree already records a bug born of exactly that split.
// ---- c99_excess_array_initializers_do_not_write_past_the_object ----
// An excess array initializer is discarded, not written past the object.
//
// C17 6.7.9p2 makes more initializers than elements a constraint violation;
// c17 already diagnoses it. The grouping pass never bounded its element
// cursor by the array size, so the extra value was still stored -- one element
// past the end, on top of whatever the frame put there. `int x[2] = {7, 8};`
// followed by `int a[2] = {1, 2, 3};` read back `x = {3, 8}`.
// ---- c99_a_nested_string_literal_element_is_stored_in_every_encoding ----
// A string literal initializing a nested array element, in every encoding.
//
// `is_string_for_char_array` accepts all four literal kinds, but the body that
// consumed them handled only the narrow one and silently `continue`d on the
// rest, so a wide element was dropped and left zero. The same loop stepped the
// destination by *bytes* while a wide element is 2 or 4 bytes wide, and it had
// no capacity clamp at all — a third hand-rolled copy of what
// `store_string_units` already does correctly for the non-nested form.
//
// The static twin of each case was already right, which is how the two paths
// could disagree: `static wchar_t sw[2][4] = {L"ab", L"cd"}` read back 97/99
// while the automatic form read back 0/0.
// ---- c99_an_overlong_string_literal_does_not_write_past_its_array ----
// A string literal too long for the array it initializes writes only as much
// as fits.
//
// C17 6.7.9p14 allows exactly the terminating NUL to be dropped, and nothing
// more. The nested-array path had no clamp, so `char s[1][3] = {"hello"}`
// stored five bytes into a three-byte object — two of them past the whole
// local, not merely into the next row. The guards on either side are what make
// that visible rather than layout-dependent.
// ---- c99_a_string_initializer_zero_fills_the_rest_of_its_array ----
// `char buf[N] = "str"` zero-fills the bytes the literal does not reach.
//
// C17 6.7.9p21: the members not initialized explicitly are initialized as a
// static object would be, i.e. to zero. The `InitList` arm of a local
// declaration calls `emit_aggregate_zero` first; the string arm did not, so
// only the literal's own bytes were written.
//
// On entry the backend zeroes the whole frame, which hides this the first time
// through — the declaration is inside a loop so the second pass sees what the
// first one left. All four encodings are affected.
// ---- c99_a_designated_override_replaces_only_the_subobject_it_names ----
// A later designated initializer replaces the subobject it names, not every
// object whose bytes it touches.
//
// C17 6.7.9p19: an initializer for a subobject overrides any previously
// listed initializer *for that subobject*, and initializers for other
// subobjects are unaffected. The static path merged its field initializers by
// byte span and dropped an earlier entry whole on any intersection, so
// `.t = {1,2}` followed by `.t.y = 9` lost the `1` as well as the `2` -- while
// the automatic path, which just stores in order and lets the later store land
// on the earlier one, kept it. The two disagreed on the same initializer.
//
// Every case here is checked in both storage durations, against the values
// gcc and clang produce.
// ---- c99_a_designated_override_is_resolved_in_source_order ----
// "Later wins" means later in the initializer list, not later in the object.
//
// The static path sorted its field initializers by address before resolving
// overlaps, so the rule was applied in the wrong order entirely: in
// `{ .z = 7, .t.y = 9, .t = {1,2} }` the `.t = {1,2}` is written last and must
// win, but after sorting it sat before `.t.y` and was the entry dropped.
// ---- c99_initializing_a_second_union_member_resets_the_union ----
// Initializing a second member of a union resets it; it does not overlay the
// first.
//
// This is the case where the two paths disagree the other way round. A union
// holds one member at a time, so `{ .u.i = 0x01020304, .u.s.b = 9 }` leaves
// the union holding `.u.s` with only `b` given a value and the rest zero --
// which is what the static path produced and what gcc and clang produce. The
// automatic path stored the `int` and then stored one byte over it, keeping
// the other three, so it read back `0x01020904`.
//
// It is here as a guard on the fix above: making the static path store in
// source order the way the automatic path does would adopt this bug, so the
// merge has to keep the union case distinct from the struct and array cases.
// ---- c99_a_designated_override_of_a_bitfield_keeps_its_neighbours ----
// A designated override of a bit-field replaces only that bit-field.
//
// The remaining half of the subobject rule. Bit-fields share a carrier, and
// the carrier's bytes are merged downstream of the `Initializer` tree, so the
// static path could not fold one override into an earlier initializer and fell
// back to dropping it whole -- losing `b` in `{ .t = {1,2}, .t.a = 3 }` --
// while the automatic path stored the carrier and then stored over part of it,
// keeping `b`. gcc keeps it. The two paths disagreeing is the defect; gcc's
// answer is which way to settle it.
// ---- c99_an_override_inside_the_held_union_member_keeps_the_rest ----
// An override naming a subobject of the union member already held keeps the
// rest of that member.
//
// `{ .u = {1,2}, .u.p.y = 9 }` initializes the union's first member and then
// overrides one of *its* members, so the union still holds `p` and `p.x` keeps
// the 1 it was given. c17 reset the union instead, because the `Initializer`
// tree records no discriminant and the merge could not tell "the same member,
// deeper" from "a different member" -- and resetting is right only for the
// second. Both storage durations agreed on the wrong answer, so nothing caught
// it.
//
// The companion case, where a *different* member is named and the union really
// is reset, is covered by
// `c99_initializing_a_second_union_member_resets_the_union`, which must keep
// passing: the two are what distinguish the rule.
// ---- c99_a_whole_struct_initializer_supersedes_an_earlier_bitfield ----
// An initializer for a whole struct supersedes an earlier one for a
// bit-field inside it, including the bit-fields it says nothing about.
//
// The other direction of the bit-field rule, and the one the automatic path
// had wrong: it stored the bit-field, then stored the struct's own
// bit-fields over it, and `c` -- which `{1, 2}` does not mention -- kept the
// 7. The static path dropped the earlier entry whole and was right. gcc and
// clang zero it.
//
// The objects here are deliberately wider than eight bytes, to keep the
// assertions clear of an unrelated x86-64 defect that widens a 32-bit store
// at offset 0 of an eight-byte local to 64 bits.
// ---- c99_a_bitfield_naming_a_second_union_member_resets_the_union ----
// A bit-field naming a second member of a union resets the union, as any
// other initializer for a second member does.
//
// A bit-field is stored by reading its carrier and writing it back, so the
// automatic path emitted no fill for it and three bytes of the `int` showed
// through the `struct` that replaced it. It is not that a bit-field clears
// nothing -- it clears nothing *of its own*, because its neighbours in the
// carrier are other objects -- but what the union it displaces requires.
// ---- c99_an_override_naming_another_union_member_resets_it_whatever_its_shape ----
// Naming a subobject of a union member the union does *not* hold resets it,
// even where the two members are the same size and the same shape.
//
// The guard on the fold above. Knowing which member is held comes from the
// initializer list, not from the lowered bytes, so `struct P` and `struct Q`
// being indistinguishable once lowered costs nothing: `.u = {1, 2}` gives
// `p` a value and `.u.q.d = 9` names `q`, so the union comes to hold `q`
// with only `d` given a value. Reading it back through the *other* member
// would be undefined; `q.c` is not.
//
/// Later designators override exactly the subobject they name, ranges and
/// excess initializers stay inside the object, and string elements are
/// stored and zero-filled -- each original ran at the matrix level and at
/// -O1 (`compile_and_run_optimized`), and so does this.
///
/// Consolidates (one C section each, a `t_<name>` function):
/// - `c99_designated_init_ranges`
/// - `c99_excess_array_initializers_do_not_write_past_the_object`
/// - `c99_a_nested_string_literal_element_is_stored_in_every_encoding`
/// - `c99_an_overlong_string_literal_does_not_write_past_its_array`
/// - `c99_a_string_initializer_zero_fills_the_rest_of_its_array`
/// - `c99_a_designated_override_replaces_only_the_subobject_it_names`
/// - `c99_a_designated_override_is_resolved_in_source_order`
/// - `c99_initializing_a_second_union_member_resets_the_union`
/// - `c99_a_designated_override_of_a_bitfield_keeps_its_neighbours`
/// - `c99_an_override_inside_the_held_union_member_keeps_the_rest`
/// - `c99_a_whole_struct_initializer_supersedes_an_earlier_bitfield`
/// - `c99_a_bitfield_naming_a_second_union_member_resets_the_union`
/// - `c99_an_override_naming_another_union_member_resets_it_whatever_its_shape`
///
/// Exit codes: see the map at the top of the program.
#[test]
fn c99_initializers_overrides_mega() {
    let code = r#"
/* Exit-code map: each section returns its original code, offset by
   the base listed in its banner.
       1.. 16  c99_designated_init_ranges
      17.. 28  c99_excess_array_initializers_do_not_write_past_the_object
      29.. 35  c99_a_nested_string_literal_element_is_stored_in_every_encoding
      36.. 41  c99_an_overlong_string_literal_does_not_write_past_its_array
      42.. 44  c99_a_string_initializer_zero_fills_the_rest_of_its_array
      45.. 48  c99_a_designated_override_replaces_only_the_subobject_it_names
      49.. 52  c99_a_designated_override_is_resolved_in_source_order
      53.. 56  c99_initializing_a_second_union_member_resets_the_union
      57.. 60  c99_a_designated_override_of_a_bitfield_keeps_its_neighbours
      61.. 62  c99_an_override_inside_the_held_union_member_keeps_the_rest
      63.. 64  c99_a_whole_struct_initializer_supersedes_an_earlier_bitfield
      65.. 66  c99_a_bitfield_naming_a_second_union_member_resets_the_union
      67.. 70  c99_an_override_naming_another_union_member_resets_it_whatever_its_shape
*/

/* ==== c99_designated_init_ranges: exit codes 1..16 (original code + 0) ==== */
/* Static storage: the data-image path. */
int dr_basic[8]      = {[0 ... 3] = 7};
int dr_two[8]        = {[0 ... 3] = 1, [4 ... 7] = 2};
int dr_overlap[8]    = {[0 ... 5] = 1, [3 ... 7] = 2};
int dr_then_pos[8]   = {[0 ... 2] = 1, 9};
int dr_single[4]     = {[1 ... 1] = 5};
int dr_inferred[]    = {[0 ... 3] = 1};
char dr_chars[8]     = {[0 ... 6] = 65};

struct dr_P { int x, y; };
struct dr_P dr_structs[4] = {[0 ... 2] = {1, 2}};

struct dr_V { int v[4]; };
struct dr_V dr_nested = {.v = {[1 ... 2] = 8}};

static __attribute__((noinline)) int t_c99_designated_init_ranges(void)
{
    if (dr_basic[0] != 7 || dr_basic[3] != 7) return 1;
    if (dr_basic[4] != 0 || dr_basic[7] != 0) return 2;

    if (dr_two[0] != 1 || dr_two[3] != 1 || dr_two[4] != 2 || dr_two[7] != 2) return 3;

    /* A later range wins over an earlier one where they overlap. */
    if (dr_overlap[2] != 1 || dr_overlap[3] != 2 || dr_overlap[7] != 2) return 4;

    /* The cursor resumes past the high endpoint. */
    if (dr_then_pos[2] != 1 || dr_then_pos[3] != 9 || dr_then_pos[4] != 0) return 5;

    if (dr_single[1] != 5 || dr_single[0] != 0 || dr_single[2] != 0) return 6;

    /* Array size inferred from the range's high endpoint. */
    if (sizeof dr_inferred / sizeof dr_inferred[0] != 4) return 7;
    if (dr_inferred[3] != 1) return 8;

    if (dr_chars[0] != 65 || dr_chars[6] != 65 || dr_chars[7] != 0) return 9;

    if (dr_structs[0].x != 1 || dr_structs[2].y != 2) return 10;
    if (dr_structs[3].x != 0 || dr_structs[3].y != 0) return 11;

    if (dr_nested.v[0] != 0 || dr_nested.v[1] != 8 || dr_nested.v[2] != 8 || dr_nested.v[3] != 0) return 12;

    /* Automatic storage: the runtime-store path, which must agree. */
    {
        int a[8] = {[0 ... 3] = 7};
        int b[8] = {[0 ... 2] = 1, 9};
        int c[8] = {[0 ... 5] = 1, [3 ... 7] = 2};
        if (a[0] != 7 || a[3] != 7 || a[4] != 0) return 13;
        if (b[2] != 1 || b[3] != 9 || b[4] != 0) return 14;
        if (c[2] != 1 || c[3] != 2 || c[7] != 2) return 15;

        struct dr_P p[4] = {[0 ... 2] = {1, 2}};
        if (p[2].y != 2 || p[3].y != 0) return 16;
    }

    return 0;
}

/* ==== c99_excess_array_initializers_do_not_write_past_the_object: exit codes 17..28 (original code + 16) ==== */
static __attribute__((noinline)) int t_c99_excess_array_initializers_do_not_write_past_the_object(void)
{
    int x[2] = {7, 8};
    int a[2] = {1, 2, 3};
    if (a[0] != 1 || a[1] != 2) return 1;
    if (x[0] != 7 || x[1] != 8) return 2;

    /* Several excess elements, and a designator that jumps back first.
       C17 6.7.9p17: a positional initializer after a designator resumes at
       the next subobject, so after `[0] = 1` the cursor is at index 1 and the
       9 overrides the earlier 2. Only the 10 and 11 are excess. Confirmed
       against clang, which warns -Winitializer-overrides on the 9. */
    short y[2] = {5, 6};
    short b[2] = {[1] = 2, [0] = 1, 9, 10, 11};
    if (b[0] != 1 || b[1] != 9) return 3;
    if (y[0] != 5 || y[1] != 6) return 4;

    /* The bound is on the index, not on the count. Here the array has three
       elements and the initializer list has two, so a count-based rule keeps
       the 1 -- but it resumes after `[2]`, i.e. at index 3, and is excess.
       clang gives {0,0,3}. */
    int guard_before[2] = {11, 12};
    int d[3] = {[2] = 3, 1};
    int guard_after[2] = {13, 14};
    if (d[0] != 0 || d[1] != 0 || d[2] != 3) return 10;
    if (guard_before[0] != 11 || guard_before[1] != 12) return 11;
    if (guard_after[0] != 13 || guard_after[1] != 14) return 12;

    /* A nested array: the excess belongs to the inner object. */
    int z[2] = {8, 9};
    int c[2][2] = {{1, 2, 3}, {4, 5}};
    if (c[0][0] != 1 || c[0][1] != 2) return 5;
    if (c[1][0] != 4 || c[1][1] != 5) return 6;
    if (z[0] != 8 || z[1] != 9) return 7;

    /* A char array from a string literal that does not fit: C17 6.7.9p14
       allows exactly the terminator to be dropped, nothing more. */
    char w[2] = {'a', 'b'};
    char s[3] = "hello";
    if (s[0] != 'h' || s[1] != 'e' || s[2] != 'l') return 8;
    if (w[0] != 'a' || w[1] != 'b') return 9;

    return 0;
}

/* ==== c99_a_nested_string_literal_element_is_stored_in_every_encoding: exit codes 29..35 (original code + 28) ==== */
#include <wchar.h>
/* <uchar.h> does not exist on macOS, and the test needs only the two types --
   the same substitution the universal-character-name test above makes. */
typedef unsigned short char16_t;
typedef unsigned int char32_t;

static __attribute__((noinline)) int t_c99_a_nested_string_literal_element_is_stored_in_every_encoding(void)
{
    /* Narrow, and the tail of a short element must be zero. */
    char n[2][4] = {"ab", "cd"};
    if (n[0][0] != 'a' || n[0][1] != 'b' || n[0][2] != 0 || n[0][3] != 0) return 1;
    if (n[1][0] != 'c' || n[1][1] != 'd' || n[1][2] != 0 || n[1][3] != 0) return 2;

    /* Wide: dropped entirely before the fix. */
    wchar_t w[2][4] = {L"ab", L"cd"};
    if ((int)w[0][0] != 'a' || (int)w[0][1] != 'b' || w[0][2] != 0) return 3;
    if ((int)w[1][0] != 'c' || (int)w[1][1] != 'd' || w[1][2] != 0) return 4;

    char16_t u[2][4] = {u"ab", u"cd"};
    if ((int)u[0][0] != 'a' || (int)u[1][0] != 'c' || u[0][2] != 0) return 5;

    char32_t U[2][4] = {U"ab", U"cd"};
    if ((int)U[0][0] != 'a' || (int)U[1][0] != 'c' || U[0][2] != 0) return 6;

    /* The static path was always correct; the two must now agree. */
    static wchar_t sw[2][4] = {L"ab", L"cd"};
    if ((int)sw[0][0] != 'a' || (int)sw[1][0] != 'c') return 7;

    return 0;
}

/* ==== c99_an_overlong_string_literal_does_not_write_past_its_array: exit codes 36..41 (original code + 35) ==== */
static __attribute__((noinline)) int t_c99_an_overlong_string_literal_does_not_write_past_its_array(void)
{
    unsigned char lo = 0xA5;
    char s[1][3] = {"hello"};
    unsigned char hi = 0x5A;
    if (s[0][0] != 'h' || s[0][1] != 'e' || s[0][2] != 'l') return 1;
    if (lo != 0xA5 || hi != 0x5A) return 2;

    /* Exactly the terminator dropped: this is legal and keeps all three. */
    unsigned char lo2 = 0xA5;
    char e[1][3] = {"abc"};
    unsigned char hi2 = 0x5A;
    if (e[0][0] != 'a' || e[0][1] != 'b' || e[0][2] != 'c') return 3;
    if (lo2 != 0xA5 || hi2 != 0x5A) return 4;

    /* Wide, where the stride is 4 bytes and a byte-stepped copy lands wrong. */
    unsigned char lo3 = 0xA5;
    __WCHAR_TYPE__ w[1][2] = {L"xyz"};
    unsigned char hi3 = 0x5A;
    if ((int)w[0][0] != 'x' || (int)w[0][1] != 'y') return 5;
    if (lo3 != 0xA5 || hi3 != 0x5A) return 6;

    return 0;
}

/* ==== c99_a_string_initializer_zero_fills_the_rest_of_its_array: exit codes 42..44 (original code + 41) ==== */
#include <wchar.h>

static __attribute__((noinline)) int t_c99_a_string_initializer_zero_fills_the_rest_of_its_array(void)
{
    for (int pass = 0; pass < 2; pass++) {
        char b[8] = "hi";
        if (b[2] != 0 || b[3] != 0 || b[7] != 0) return 1;
        b[3] = 'Z';
        b[7] = 'Z';
    }

    for (int pass = 0; pass < 2; pass++) {
        wchar_t w[4] = L"hi";
        if (w[2] != 0 || w[3] != 0) return 2;
        w[3] = 'Z';
    }

    /* The braced form went through the InitList arm and was already correct;
       both spellings must now agree. */
    for (int pass = 0; pass < 2; pass++) {
        char c[8] = {"hi"};
        if (c[3] != 0 || c[7] != 0) return 3;
        c[3] = 'Z';
    }

    return 0;
}

/* ==== c99_a_designated_override_replaces_only_the_subobject_it_names: exit codes 45..48 (original code + 44) ==== */
struct po_T { int x, y; };
struct po_S { struct po_T t; int z; };
struct po_A { int a[3]; int z; };

struct po_S po_g1 = { .t = {1, 2}, .t.y = 9, .z = 7 };
struct po_A po_g2 = { .a = {1, 2, 3}, .a[1] = 9, .z = 7 };

static __attribute__((noinline)) int t_c99_a_designated_override_replaces_only_the_subobject_it_names(void)
{
    struct po_S l1 = { .t = {1, 2}, .t.y = 9, .z = 7 };
    struct po_A l2 = { .a = {1, 2, 3}, .a[1] = 9, .z = 7 };

    /* The override names .t.y, so .t.x keeps the 1 it was given. */
    if (po_g1.t.x != 1 || po_g1.t.y != 9 || po_g1.z != 7) return 1;
    if (l1.t.x != 1 || l1.t.y != 9 || l1.z != 7) return 2;

    /* The same one level down: only element 1 is replaced. */
    if (po_g2.a[0] != 1 || po_g2.a[1] != 9 || po_g2.a[2] != 3 || po_g2.z != 7) return 3;
    if (l2.a[0] != 1 || l2.a[1] != 9 || l2.a[2] != 3 || l2.z != 7) return 4;

    return 0;
}

/* ==== c99_a_designated_override_is_resolved_in_source_order: exit codes 49..52 (original code + 48) ==== */
struct sr_T { int x, y; };
struct sr_S { struct sr_T t; int z; };

/* The whole-field initializer comes last and wins, even though it names a
   lower address than the override before it. */
struct sr_S sr_g = { .z = 7, .t.y = 9, .t = {1, 2} };

/* And the other order, where the narrower one wins. */
struct sr_S sr_h = { .t = {1, 2}, .z = 7, .t.y = 9 };

static __attribute__((noinline)) int t_c99_a_designated_override_is_resolved_in_source_order(void)
{
    struct sr_S lg = { .z = 7, .t.y = 9, .t = {1, 2} };
    struct sr_S lh = { .t = {1, 2}, .z = 7, .t.y = 9 };

    if (sr_g.t.x != 1 || sr_g.t.y != 2 || sr_g.z != 7) return 1;
    if (lg.t.x != 1 || lg.t.y != 2 || lg.z != 7) return 2;
    if (sr_h.t.x != 1 || sr_h.t.y != 9 || sr_h.z != 7) return 3;
    if (lh.t.x != 1 || lh.t.y != 9 || lh.z != 7) return 4;

    return 0;
}

/* ==== c99_initializing_a_second_union_member_resets_the_union: exit codes 53..56 (original code + 52) ==== */
struct ur_U { union { int i; struct { char a, b, c, d; } s; } u; };
struct ur_U ur_g = { .u.i = 0x01020304, .u.s.b = 9 };

static __attribute__((noinline)) int t_c99_initializing_a_second_union_member_resets_the_union(void)
{
    struct ur_U l = { .u.i = 0x01020304, .u.s.b = 9 };
    if (ur_g.u.i != 0x900) return 1;
    if (l.u.i != 0x900) return 2;
    if (ur_g.u.s.a != 0 || ur_g.u.s.b != 9 || ur_g.u.s.c != 0 || ur_g.u.s.d != 0) return 3;
    if (l.u.s.a != 0 || l.u.s.b != 9 || l.u.s.c != 0 || l.u.s.d != 0) return 4;
    return 0;
}

/* ==== c99_a_designated_override_of_a_bitfield_keeps_its_neighbours: exit codes 57..60 (original code + 56) ==== */
struct bo_B { unsigned a : 4, b : 4; };
struct bo_S { struct bo_B t; int z; };

struct bo_S bo_g = { .t = {1, 2}, .t.a = 3, .z = 7 };
struct bo_S bo_g2 = { .t = {1, 2}, .t.b = 5 };

static __attribute__((noinline)) int t_c99_a_designated_override_of_a_bitfield_keeps_its_neighbours(void)
{
    struct bo_S l = { .t = {1, 2}, .t.a = 3, .z = 7 };
    struct bo_S l2 = { .t = {1, 2}, .t.b = 5 };

    if (bo_g.t.a != 3 || bo_g.t.b != 2 || bo_g.z != 7) return 1;
    if (l.t.a != 3 || l.t.b != 2 || l.z != 7) return 2;
    if (bo_g2.t.a != 1 || bo_g2.t.b != 5) return 3;
    if (l2.t.a != 1 || l2.t.b != 5) return 4;

    return 0;
}

/* ==== c99_an_override_inside_the_held_union_member_keeps_the_rest: exit codes 61..62 (original code + 60) ==== */
struct hu_P { int x, y; };
struct hu_N { union { struct hu_P p; int i; } u; };

struct hu_N hu_g = { .u = {1, 2}, .u.p.y = 9 };

static __attribute__((noinline)) int t_c99_an_override_inside_the_held_union_member_keeps_the_rest(void)
{
    struct hu_N l = { .u = {1, 2}, .u.p.y = 9 };
    if (hu_g.u.p.x != 1 || hu_g.u.p.y != 9) return 1;
    if (l.u.p.x != 1 || l.u.p.y != 9) return 2;
    return 0;
}

/* ==== c99_a_whole_struct_initializer_supersedes_an_earlier_bitfield: exit codes 63..64 (original code + 62) ==== */
struct ws_B { unsigned a : 4, b : 4, c : 4; };
struct ws_S { struct ws_B t; int z; long pad; };

struct ws_S ws_g = { .t.c = 7, .t = {1, 2} };

static __attribute__((noinline)) int t_c99_a_whole_struct_initializer_supersedes_an_earlier_bitfield(void)
{
    struct ws_S l = { .t.c = 7, .t = {1, 2} };

    if (ws_g.t.a != 1 || ws_g.t.b != 2 || ws_g.t.c != 0) return 1;
    if (l.t.a != 1 || l.t.b != 2 || l.t.c != 0) return 2;

    return 0;
}

/* ==== c99_a_bitfield_naming_a_second_union_member_resets_the_union: exit codes 65..66 (original code + 64) ==== */
struct bu_U { union { int i; struct { unsigned a : 4, b : 4; } s; } u; long pad; };

struct bu_U bu_g = { .u.i = 0x01020304, .u.s.a = 3 };

static __attribute__((noinline)) int t_c99_a_bitfield_naming_a_second_union_member_resets_the_union(void)
{
    struct bu_U l = { .u.i = 0x01020304, .u.s.a = 3 };

    if (bu_g.u.i != 3 || bu_g.u.s.a != 3 || bu_g.u.s.b != 0) return 1;
    if (l.u.i != 3 || l.u.s.a != 3 || l.u.s.b != 0) return 2;

    return 0;
}

/* ==== c99_an_override_naming_another_union_member_resets_it_whatever_its_shape: exit codes 67..70 (original code + 66) ==== */
struct om_P { int x, y; };
struct om_Q { int c, d; };
struct om_N { union { struct om_P p; struct om_Q q; } u; long pad; };

struct om_N om_g = { .u = {1, 2}, .u.q.d = 9 };

/* And the fold, in the same union, when the member named is the held one. */
struct om_N om_h = { .u = {1, 2}, .u.p.y = 9 };

static __attribute__((noinline)) int t_c99_an_override_naming_another_union_member_resets_it_whatever_its_shape(void)
{
    struct om_N l = { .u = {1, 2}, .u.q.d = 9 };
    struct om_N m = { .u = {1, 2}, .u.p.y = 9 };

    if (om_g.u.q.c != 0 || om_g.u.q.d != 9) return 1;
    if (l.u.q.c != 0 || l.u.q.d != 9) return 2;
    if (om_h.u.p.x != 1 || om_h.u.p.y != 9) return 3;
    if (m.u.p.x != 1 || m.u.p.y != 9) return 4;

    return 0;
}

int main(void)
{
    int r;
    if ((r = t_c99_designated_init_ranges()) != 0) return 0 + r;
    if ((r = t_c99_excess_array_initializers_do_not_write_past_the_object()) != 0) return 16 + r;
    if ((r = t_c99_a_nested_string_literal_element_is_stored_in_every_encoding()) != 0) return 28 + r;
    if ((r = t_c99_an_overlong_string_literal_does_not_write_past_its_array()) != 0) return 35 + r;
    if ((r = t_c99_a_string_initializer_zero_fills_the_rest_of_its_array()) != 0) return 41 + r;
    if ((r = t_c99_a_designated_override_replaces_only_the_subobject_it_names()) != 0) return 44 + r;
    if ((r = t_c99_a_designated_override_is_resolved_in_source_order()) != 0) return 48 + r;
    if ((r = t_c99_initializing_a_second_union_member_resets_the_union()) != 0) return 52 + r;
    if ((r = t_c99_a_designated_override_of_a_bitfield_keeps_its_neighbours()) != 0) return 56 + r;
    if ((r = t_c99_an_override_inside_the_held_union_member_keeps_the_rest()) != 0) return 60 + r;
    if ((r = t_c99_a_whole_struct_initializer_supersedes_an_earlier_bitfield()) != 0) return 62 + r;
    if ((r = t_c99_a_bitfield_naming_a_second_union_member_resets_the_union()) != 0) return 64 + r;
    if ((r = t_c99_an_override_naming_another_union_member_resets_it_whatever_its_shape()) != 0) return 66 + r;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("c99_initializers_overrides_mega", code, &[]),
        0
    );
    assert_eq!(
        compile_and_run_optimized("c99_initializers_overrides_mega_opt", code),
        0
    );
}

// Kept separate: it pins the static layout of adjacent globals (the object
// after the array must not absorb an excess element), a TU-wide property.
/// The static form of the same defect: the emitted object is exactly as wide
/// as the array declares.
///
/// `int garr[2] = {1, 2, 3};` emitted three `.long`s under an eight-byte
/// object, so the next symbol in the section absorbed the third.
#[test]
fn c99_excess_static_array_initializers_do_not_widen_the_object() {
    let code = r#"
int garr[2] = {1, 2, 3};
int after = 42;
short garr2[2] = {[1] = 2, [0] = 1, 9, 10};
short after2 = 7;
/* Bounded by index, not by count -- see the automatic case. */
int garr3[3] = {[2] = 3, 1};
int after3 = 5;

int main(void)
{
    if (garr[0] != 1 || garr[1] != 2) return 1;
    if (after != 42) return 2;
    if (garr2[0] != 1 || garr2[1] != 9) return 3;
    if (after2 != 7) return 4;
    if (garr3[0] != 0 || garr3[1] != 0 || garr3[2] != 3) return 5;
    if (after3 != 5) return 6;
    return 0;
}
"#;
    assert_eq!(compile_and_run("excess_static_array_init", code, &[]), 0);
    assert_eq!(
        compile_and_run_optimized("excess_static_array_init_opt", code),
        0
    );
}

/// The GNU index range after a field designator: `.m[lo ... hi] = v`, as
/// binutils' i386-dis.c initializes its decoder state
/// (`.op_index[0 ... MAX_OPERANDS - 1] = -1`). Each index in the range gets
/// `v`, and what the range does not name keeps its own initializer; the next
/// positional initializer continues after `hi`.
#[test]
fn c99_index_range_after_a_field_designator() {
    let code = r#"
struct In { int k[3]; char c; };
struct S { int a; int m[6]; struct In in[3]; long t; };

static struct S g = { .m[1 ... 3] = -1, 8, .in[0 ... 1].k[1 ... 2] = 5, .t = 9 };

__attribute__((noinline)) static int check(const struct S *s)
{
    static const int want_m[6] = { 0, -1, -1, -1, 8, 0 };
    for (int i = 0; i < 6; i++)
        if (s->m[i] != want_m[i]) return 1 + i;
    for (int j = 0; j < 3; j++)
        for (int i = 0; i < 3; i++)
            if (s->in[j].k[i] != (j < 2 && i >= 1 ? 5 : 0)) return 10 + 3 * j + i;
    if (s->t != 9 || s->a != 0) return 20;
    return 0;
}

int main(void)
{
    int r;
    if ((r = check(&g)) != 0) return r;
    struct S l = { .m[1 ... 3] = -1, 8, .in[0 ... 1].k[1 ... 2] = 5, .t = 9 };
    if ((r = check(&l)) != 0) return 30 + r;
    /* A later designator overrides one element of the range. */
    struct S o = { .m[0 ... 5] = 4, .m[2] = 1 };
    if (o.m[0] != 4 || o.m[2] != 1 || o.m[5] != 4) return 60;
    return 0;
}
"#;
    assert_eq!(compile_and_run("index_range_after_field", code, &[]), 0);
    assert_eq!(
        compile_and_run("index_range_after_field_o2", code, &["-O2".to_string()]),
        0
    );
}

// Kept separate: the only test run at exactly -O0 and -O2 (no matrix level).
/// A later designator reaching inside something a whole *value* initialized
/// keeps the rest of that value, through a union exactly as through a
/// struct.
///
/// Which member a union value last had stored into it is a fact about the
/// run, so c17 treats its bytes as a value and replaces only what the later
/// designator names. A union initialized that way was reset instead, while
/// the same shape through a struct kept its value. gcc discards the whole
/// earlier initializer in both cases; see DECISIONS.md.
#[test]
fn c99_an_override_inside_a_value_initialized_union_keeps_the_value() {
    let code = r#"
struct P { int a, b; };
union U { struct P s; long l; };
struct O { union U u; int z; };
struct Q { struct P t; int z; };
int main(void) {
    union U v = { .s = { 1, 2 } };
    struct P p = { 5, 6 };
    struct O o = { .u = v, .u.s.b = 9 };
    struct Q q = { .t = p, .t.b = 9 };
    if (o.u.s.a != 1 || o.u.s.b != 9) return 1;
    if (q.t.a != 5 || q.t.b != 9) return 2;
    return 0;
}
"#;
    for level in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run(
                &format!("c99_union_value_override{level}"),
                code,
                &[level.to_string()]
            ),
            0,
            "{level}"
        );
    }
}

// Kept separate: it relies on noinline functions and the frame of one
// specific function, and runs on aarch64 as well.
/// Two shapes a deleted branch once miscompiled, pinned now that they are
/// right: a designated initializer of an eight-byte struct written out of
/// member order (a store widened over the second member), and the tail of a
/// short string in a longer array, which must read as zero however dirty the
/// frame was before.
#[test]
fn c99_out_of_order_designators_and_string_tails_are_exact() {
    let code = r#"
struct T { int a, b; };
__attribute__((noinline)) struct T mk(int x, int y) { struct T s = { .b = y, .a = x }; return s; }
__attribute__((noinline)) int tail(void) {
    char junk[64];
    __builtin_memset(junk, 'X', sizeof junk);
    __asm__ volatile("" :: "r"(junk) : "memory");
    char s[13] = "ab";
    for (int i = 2; i < 13; i++) if (s[i] != 0) return 100 + i;
    return 0;
}
int main(void) {
    struct T t = mk(3, 4);
    if (t.a != 3 || t.b != 4) return 1;
    for (int k = 0; k < 3; k++) { int r = tail(); if (r) return r; }
    return 0;
}
"#;
    for level in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run(&format!("c99_frame_a{level}"), code, &[level.to_string()]),
            0,
            "{level}"
        );
        if let Some(rc) = compile_and_run_aarch64("c99_frame_a_a64", code, level) {
            assert_eq!(rc, 0, "aarch64 {level}");
        }
    }
}
