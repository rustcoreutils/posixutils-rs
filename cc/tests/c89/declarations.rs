//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C89/C99 Declarations Mega-Test
//
// Covers: declaration syntax, variable decls, function decls (incl. K&R),
// structs (forward decl, flexible array, zero-width bitfield), unions,
// enums, typedefs, initializers
//

use crate::common::{compile_and_run, compile_and_run_everywhere, compile_and_run_optimized};

// ============================================================================
// Mega-test: Declarations (Section 7 of C99)
// ============================================================================

/// C89 declarations and bit-field layout, as one program; each section keeps its
/// original test name and doc comment, and the exit-code table is at the top.
///
/// Consolidates: c89_declarations_mega, c89_bitfield_increment_leaves_neighbours_alone,
/// c89_zero_width_bitfield_forces_a_boundary,
/// c89_bitfield_as_wide_as_its_carrier_round_trips and the matrix-level run of
/// c89_int128_bitfield_wider_than_64_round_trips.
#[test]
fn c89_declarations_mega() {
    let code = r#"
/*
 * Exit codes: each section's own failure codes, offset by its base.
 *     1- 79  c89_declarations_mega
 *    81- 99  c89_bitfield_increment_leaves_neighbours_alone
 *   101-149  c89_zero_width_bitfield_forces_a_boundary
 *   151-163  c89_bitfield_as_wide_as_its_carrier_round_trips
 *   171-183  c89_int128_bitfield_wider_than_64_round_trips
 */

/* ---- c89_declarations_mega (exit codes 1-79) ----
 */
#include <stdlib.h>
#include <string.h>

// === K&R function definitions ===
int knr_add(a, b)
    int a;
    int b;
{
    return a + b;
}

// K&R with pointer and multiple params per declaration
int knr_multi(a, b, c)
    int a, b;
    char *c;
{
    return a + b + c[0];
}

// === Forward declaration ===
struct ForwardNode;
struct ForwardNode {
    int val;
    struct ForwardNode *next;
};

// === Function returning function pointer ===
typedef int (*binop_t)(int, int);
int op_add(int a, int b) { return a + b; }
int op_mul(int a, int b) { return a * b; }
binop_t get_op(int which) {
    return which == 0 ? op_add : op_mul;
}

// === Flexible array member ===
struct FlexBuf {
    int len;
    char data[];
};

// === Pointer to array ===
void fill_via_ptr(int (*p)[3]) {
    (*p)[0] = 100;
    (*p)[1] = 200;
    (*p)[2] = 300;
}

// === Extern incomplete array (defined elsewhere) ===
int extern_arr[] = {10, 20, 30, 40, 50};

static int t_c89_declarations_mega(void) {
    // ========== DECLARATION SYNTAX (returns 1-9) ==========
    {
        // Declaration specifiers + declarators
        int x;
        x = 42;
        if (x != 42) return 1;

        // Multiple declarators per declaration
        int a = 1, b = 2, c = 3;
        if (a + b + c != 6) return 2;

        // Abstract declarators (in sizeof, casts)
        if (sizeof(int *) != sizeof(void *)) return 3;
        int val = (int)3.14;
        if (val != 3) return 4;

        // Declarator with initializer
        int d = 100;
        if (d != 100) return 5;
    }

    // ========== VARIABLE DECLARATIONS (returns 10-19) ==========
    {
        // Pointer variable
        int x = 42;
        int *p = &x;
        if (*p != 42) return 10;

        // Array variable
        int arr[5] = {1, 2, 3, 4, 5};
        if (arr[4] != 5) return 11;

        // Array without size (incomplete, sized by initializer)
        int arr2[] = {10, 20, 30};
        if (sizeof(arr2) != 3 * sizeof(int)) return 12;

        // Pointer to array
        int a3[3] = {0, 0, 0};
        int (*pa)[3] = &a3;
        fill_via_ptr(pa);
        if ((*pa)[0] != 100 || (*pa)[2] != 300) return 13;

        // Array of pointers
        int v1 = 10, v2 = 20, v3 = 30;
        int *ptrs[3] = {&v1, &v2, &v3};
        if (*ptrs[1] != 20) return 14;

        // Extern incomplete array
        if (extern_arr[0] != 10 || extern_arr[4] != 50) return 15;
    }

    // ========== FUNCTION DECLARATIONS (returns 20-29) ==========
    {
        // K&R function definition
        if (knr_add(3, 4) != 7) return 20;

        // K&R with multiple params per line and pointer
        if (knr_multi(10, 20, "A") != 95) return 21;  // 10+20+'A'(65)

        // Function returning function pointer
        binop_t op = get_op(0);
        if (op(3, 4) != 7) return 22;
        op = get_op(1);
        if (op(3, 4) != 12) return 23;

        // Pointer to function
        int (*fp)(int, int) = op_add;
        if (fp(5, 6) != 11) return 24;

        // Variadic (tested via printf-style, just verify call works)
        // Already well-tested in c99/features.rs, just confirm basic call
    }

    // ========== STRUCT DECLARATIONS (returns 30-39) ==========
    {
        // Struct with tag
        struct Point { int x; int y; };
        struct Point p = {10, 20};
        if (p.x != 10 || p.y != 20) return 30;

        // Anonymous struct
        struct { int a; int b; } anon = {5, 6};
        if (anon.a != 5) return 31;

        // Nested struct
        struct Rect { struct Point tl; struct Point br; };
        struct Rect r = {{0, 0}, {10, 20}};
        if (r.br.y != 20) return 32;

        // Forward declaration + self-referential (linked list)
        struct ForwardNode n1 = {10, 0};
        struct ForwardNode n2 = {20, &n1};
        if (n2.next->val != 10) return 33;

        // Bit-fields
        struct Bits {
            unsigned int a : 3;
            unsigned int b : 5;
        };
        struct Bits bits = {0};
        bits.a = 7;
        bits.b = 31;
        if (bits.a != 7 || bits.b != 31) return 34;

        // Zero-width bit-field (forces alignment to next storage unit)
        struct Aligned {
            unsigned int a : 4;
            unsigned int : 0;
            unsigned int b : 4;
        };
        struct Aligned al = {0};
        al.a = 5;
        al.b = 9;
        if (al.a != 5 || al.b != 9) return 35;

        // Flexible array member
        struct FlexBuf *fb = malloc(sizeof(struct FlexBuf) + 6);
        fb->len = 5;
        memcpy(fb->data, "hello", 6);
        if (fb->len != 5) return 36;
        if (strcmp(fb->data, "hello") != 0) return 37;
        // sizeof excludes flexible member
        if (sizeof(struct FlexBuf) != sizeof(int)) return 38;
        free(fb);
    }

    // ========== UNION DECLARATIONS (returns 40-49) ==========
    {
        // Union with tag
        union Data { int i; float f; char c; };
        union Data d;
        d.i = 42;
        if (d.i != 42) return 40;
        d.c = 'X';
        if (d.c != 'X') return 41;

        // Anonymous union
        struct { union { int ival; float fval; }; } mixed;
        mixed.ival = 99;
        if (mixed.ival != 99) return 42;
    }

    // ========== ENUM DECLARATIONS (returns 50-59) ==========
    {
        // Enum with tag and explicit values
        enum Color { RED = 0, GREEN = 1, BLUE = 2 };
        enum Color c = GREEN;
        if (c != 1) return 50;

        // Anonymous enum
        enum { X_VAL = 10, Y_VAL = 20 };
        if (X_VAL != 10 || Y_VAL != 20) return 51;

        // Implicit sequential values
        enum Seq { FIRST, SECOND, THIRD };
        if (FIRST != 0 || SECOND != 1 || THIRD != 2) return 52;

        // Mixed explicit/implicit
        enum Mixed { A = 5, B, C, D = 20, E };
        if (B != 6 || C != 7 || E != 21) return 53;
    }

    // ========== TYPEDEF (returns 60-69) ==========
    {
        // Simple typedef
        typedef int MyInt;
        MyInt x = 42;
        if (x != 42) return 60;

        // Pointer typedef
        typedef int *IntPtr;
        int val = 99;
        IntPtr p = &val;
        if (*p != 99) return 61;

        // Function pointer typedef
        typedef int (*BinOp)(int, int);
        BinOp add = op_add;
        if (add(3, 4) != 7) return 62;

        // Array typedef
        typedef int Arr5[5];
        Arr5 arr = {10, 20, 30, 40, 50};
        if (arr[4] != 50) return 63;

        // Struct typedef
        typedef struct { int x; int y; } Vec2;
        Vec2 v = {3, 4};
        if (v.x != 3 || v.y != 4) return 64;

        // Typedef redeclaration (same type, allowed in C)
        typedef int MyInt;
        MyInt y = 100;
        if (y != 100) return 65;
    }

    // ========== INITIALIZERS (returns 70-79) ==========
    {
        // Scalar initializer
        int x = 42;
        if (x != 42) return 70;

        // Brace-enclosed, size inferred
        int arr[] = {1, 2, 3};
        if (sizeof(arr) != 3 * sizeof(int)) return 71;

        // Nested brace initializers
        struct { int a[2]; int b; } nested = {{10, 20}, 30};
        if (nested.a[1] != 20 || nested.b != 30) return 72;

        // String literal initializer for char array
        char str[6] = "hello";
        if (str[4] != 'o' || str[5] != '\0') return 73;

        // Partial initialization (rest zero)
        int partial[5] = {1, 2};
        if (partial[2] != 0 || partial[4] != 0) return 74;

        // Designated initializers: struct field
        struct { int x; int y; int z; } ds = {.y = 20, .z = 30};
        if (ds.x != 0 || ds.y != 20 || ds.z != 30) return 75;

        // Designated initializers: array index
        int da[5] = {[1] = 10, [3] = 30};
        if (da[0] != 0 || da[1] != 10 || da[3] != 30) return 76;

        // Mixed designated/positional
        struct { int a; int b; int c; int d; } mix = {1, .c = 30, 40};
        if (mix.a != 1 || mix.b != 0 || mix.c != 30 || mix.d != 40) return 77;

        // Out-of-order designated
        struct { int a; int b; int c; } oo = {.c = 3, .a = 1, .b = 2};
        if (oo.a != 1 || oo.b != 2 || oo.c != 3) return 78;

        // Compound literal in initializer
        int *cp = (int[]){100, 200, 300};
        if (cp[1] != 200) return 79;
    }

    return 0;
}


/* ---- c89_bitfield_increment_leaves_neighbours_alone (exit codes 81-99) ----
 *
 *  `++` and `--` on a bitfield must change only that field.
 *
 *  They did not: the increment path stored its result with a plain store of
 *  the whole storage unit, so the value landed at bit 0 and every neighbour
 *  sharing the unit was wiped. On `struct A{unsigned a:3;int b:5;unsigned
 *  c:1;signed d:4;}` holding `{7,5,1,-8}`, `a.b++` gave `6 0 0 0` where gcc
 *  gives `7 6 1 -8` -- literally the value 6 written at offset 0.
 *
 *  Assignment and compound assignment were always correct, because they went
 *  through `emit_bitfield_store`; the increment path simply did not. All four
 *  now share `emit_member_store`.
 */
#include <stdio.h>
#include <string.h>

struct bi_A { unsigned a:3; int b:5; unsigned c:1; signed d:4; };
struct bi_U { unsigned x:4; unsigned y:4; };

static int bi_check(struct bi_A got, unsigned a, int b, unsigned c, int d) {
    return got.a == a && got.b == b && got.c == c && got.d == d;
}

static struct bi_A bi_by_value(struct bi_A v) { v.b++; return v; }

static int t_c89_bitfield_increment_leaves_neighbours_alone(void) {
    struct bi_A base = {7, 5, 1, -8};
    struct bi_A t;

    t = base; t.b++;   if (!bi_check(t, 7, 6, 1, -8)) return 1;
    t = base; ++t.b;   if (!bi_check(t, 7, 6, 1, -8)) return 2;
    t = base; t.b--;   if (!bi_check(t, 7, 4, 1, -8)) return 3;
    t = base; --t.b;   if (!bi_check(t, 7, 4, 1, -8)) return 4;

    /* Each field in turn, so a wrong bit offset cannot pass by luck. */
    t = base; t.a++;   if (!bi_check(t, 0, 5, 1, -8)) return 5;   /* 7+1 wraps 3 bits */
    t = base; t.c++;   if (!bi_check(t, 7, 5, 0, -8)) return 6;   /* 1+1 wraps 1 bit  */
    t = base; t.d++;   if (!bi_check(t, 7, 5, 1, -7)) return 7;

    /* The value of the expression: postfix is the old value, prefix the new. */
    t = base; if (t.b++ != 5) return 8;
    t = base; if (++t.b != 6) return 9;

    /* Signed wrap at the field's width, not the storage unit's. */
    t = base; t.b = 15; t.b++;  if (t.b != -16) return 10;
    t = base; t.b = -16; t.b--; if (t.b != 15) return 11;

    /* Every access path, since each had its own store. */
    struct bi_A arr[2] = {{7,5,1,-8}, {7,5,1,-8}};
    arr[1].b++;
    if (!bi_check(arr[1], 7, 6, 1, -8)) return 12;
    if (!bi_check(arr[0], 7, 5, 1, -8)) return 13;

    t = base;
    struct bi_A *p = &t; p->b++;
    if (!bi_check(t, 7, 6, 1, -8)) return 14;

    t = base; t = bi_by_value(t);
    if (!bi_check(t, 7, 6, 1, -8)) return 15;

    /* Two fields in one unit, so a full-unit write is unmissable. */
    struct bi_U u = {5, 6}; u.x++;
    if (u.x != 6 || u.y != 6) return 16;

    /* Compound and plain assignment, which always worked -- pinned so a fix
       to one path cannot regress the other. */
    t = base; t.b += 1;      if (!bi_check(t, 7, 6, 1, -8)) return 17;
    t = base; t.b = t.b + 1; if (!bi_check(t, 7, 6, 1, -8)) return 18;
    t = base; t.b = 6;       if (!bi_check(t, 7, 6, 1, -8)) return 19;

    return 0;
}


/* ---- c89_zero_width_bitfield_forces_a_boundary (exit codes 101-149) ----
 *
 *  A zero-width bitfield forces the next member to the next boundary of its
 *  declared type's storage unit (C17 6.7.2.1p12).
 *
 *  The layout code flushed an open bitfield storage unit but never aligned
 *  the offset, so after a plain member -- where no unit is open -- `int :0;`
 *  did nothing at all: `struct { char c; int :0; char d; }` was 2 bytes
 *  rather than 5, disagreeing with every gcc-compiled object.
 *
 *  Every expectation here came from gcc on this source.
 */
#include <stddef.h>

struct zw_A { char c; int :0; char d; };            /* after a plain member    */
struct zw_B { int a:3; int :0; int b:5; };          /* after a same-type field */
struct zw_C { char a:3; int :0; char b:5; };        /* different-type field    */
struct zw_D { int a:3; int :0; int :0; int b:5; };  /* two in a row            */
struct zw_E { char c; int :0; };                    /* at the end              */
struct zw_F { char c; short :0; char d; };          /* a narrower unit         */
struct zw_G { int :0; char c; };                    /* first, before anything  */
struct zw_H { int a:3; int :4; int :0; int b:5; };  /* beside an unnamed field */
struct zw_P { char c; int :0; char d; } __attribute__((packed));
struct zw_Z { char c; char :0; char d; };           /* asks for alignment 1    */
union  zw_U { char c; int :0; };                    /* in a union              */
union  zw_V { char c; long :0; };
union  zw_W { char c; int :0; } __attribute__((packed));

static int t_c89_zero_width_bitfield_forces_a_boundary(void) {
    /* The boundary itself is target-independent: every one of these offsets
       is the same under gcc on both targets. */
    if (offsetof(struct zw_A, c) != 0) return 2;
    if (offsetof(struct zw_A, d) != 4) return 3;
    if (offsetof(struct zw_F, d) != 2) return 9;
    if (offsetof(struct zw_G, c) != 0) return 11;
    if (offsetof(struct zw_P, d) != 4) return 45;

    /* So are the shapes whose alignment an int bitfield already set. */
    if (sizeof(struct zw_B) != 8 || _Alignof(struct zw_B) != 4) return 4;
    if (sizeof(struct zw_D) != 8 || _Alignof(struct zw_D) != 4) return 6;
    if (sizeof(struct zw_H) != 8 || _Alignof(struct zw_H) != 4) return 12;
    /* zw_A `char :0` asks for alignment 1, so it changes nothing anywhere. */
    if (sizeof(struct zw_Z) != 2 || _Alignof(struct zw_Z) != 1) return 46;

    /* What the two ABIs disagree about is whether a zero-width bitfield
       raises the *aggregate's* alignment. C17 6.7.2.1p12 leaves it to them:
       the x86-64 psABI says no, so zw_A is 5 bytes with alignment 1; AAPCS64
       says it contributes its declared type's alignment, making zw_A 8 with
       alignment 4 -- and, unlike an ordinary member's, that survives packing.
       Every number below is gcc's on the target it is written for. */
#ifdef __aarch64__
    if (sizeof(struct zw_A) != 8 || _Alignof(struct zw_A) != 4) return 40;
    if (sizeof(struct zw_C) != 8 || _Alignof(struct zw_C) != 4) return 41;
    if (sizeof(struct zw_E) != 4 || _Alignof(struct zw_E) != 4) return 42;
    if (sizeof(struct zw_F) != 4 || _Alignof(struct zw_F) != 2) return 43;
    if (sizeof(struct zw_G) != 4 || _Alignof(struct zw_G) != 4) return 10;
    if (sizeof(struct zw_P) != 8 || _Alignof(struct zw_P) != 4) return 44;
    /* zw_A union takes the alignment too, and with it the trailing padding. */
    if (sizeof(union zw_U) != 4 || _Alignof(union zw_U) != 4) return 47;
    if (sizeof(union zw_V) != 8 || _Alignof(union zw_V) != 8) return 48;
    if (sizeof(union zw_W) != 4 || _Alignof(union zw_W) != 4) return 49;
#else
    if (sizeof(struct zw_A) != 5 || _Alignof(struct zw_A) != 1) return 40;
    if (sizeof(struct zw_C) != 5 || _Alignof(struct zw_C) != 1) return 41;
    if (sizeof(struct zw_E) != 4 || _Alignof(struct zw_E) != 1) return 42;
    if (sizeof(struct zw_F) != 3 || _Alignof(struct zw_F) != 1) return 43;
    if (sizeof(struct zw_G) != 1 || _Alignof(struct zw_G) != 1) return 10;
    if (sizeof(struct zw_P) != 5 || _Alignof(struct zw_P) != 1) return 44;
    /* zw_A zero-width bitfield occupies no storage, so it cannot widen a union:
       the union is as wide as its widest real member. */
    if (sizeof(union zw_U) != 1 || _Alignof(union zw_U) != 1) return 47;
    if (sizeof(union zw_V) != 1 || _Alignof(union zw_V) != 1) return 48;
    if (sizeof(union zw_W) != 1 || _Alignof(union zw_W) != 1) return 49;
#endif

    /* The surrounding fields still read and write correctly. */
    struct zw_B b;
    b.a = 5;
    b.b = -7;
    if (b.a != -3) return 15;
    if (b.b != -7) return 16;

    struct zw_A a;
    a.c = 'x';
    a.d = 'y';
    if (a.c != 'x') return 17;
    if (a.d != 'y') return 18;

    struct zw_C c;
    c.a = 1;
    c.b = 2;
    if (c.a != 1) return 19;
    if (c.b != 2) return 20;

    return 0;
}


/* ---- c89_bitfield_as_wide_as_its_carrier_round_trips (exit codes 151-163) ----
 *
 *  A bit-field as wide as its own carrier read back as zero (#C98).
 *
 *  `(1 << width) - 1` is the natural way to spell the mask and is wrong at
 *  exactly one width: Rust masks the shift amount to the operand's width, so
 *  `1u64 << 64` is `1` and the mask is `0`. `struct { unsigned long long a:64; }`
 *  therefore ANDed every read with zero, at `-O0` and `-O2` alike, on both
 *  targets. Width 63 was correct throughout, which is why nothing caught it.
 *
 *  Widths on both sides of the boundary are exercised, and a bit-field with
 *  ordinary members either side of it, because a mask fix that overshot would
 *  corrupt the neighbours rather than the field.
 */
struct fc_U64 { unsigned long long a:64; };
struct fc_S64 { signed long long a:64; };
struct fc_W63 { unsigned long long a:63; };
struct fc_Two { unsigned long long a:64; unsigned long long b:64; };
struct fc_Pad { unsigned char pre; unsigned long long a:64; unsigned char post; };
struct fc_U32 { unsigned int a:32; };
struct fc_S32 { signed int a:32; };

static int t_c89_bitfield_as_wide_as_its_carrier_round_trips(void) {
    struct fc_U64 u; u.a = 0xFFFFFFFFFFFFFFFFULL;
    if (u.a != 0xFFFFFFFFFFFFFFFFULL) return 1;
    u.a = 0x0123456789ABCDEFULL;
    if (u.a != 0x0123456789ABCDEFULL) return 2;

    /* A full-width signed field keeps its sign without any extension step. */
    struct fc_S64 s; s.a = -3;
    if (s.a != -3) return 3;
    s.a = 0x7FFFFFFFFFFFFFFFLL;
    if (s.a != 0x7FFFFFFFFFFFFFFFLL) return 4;

    /* One bit narrower always worked; it must still. */
    struct fc_W63 w; w.a = 0x7FFFFFFFFFFFFFFFULL;
    if (w.a != 0x7FFFFFFFFFFFFFFFULL) return 5;

    /* fc_Two full-width fields do not share a unit, so neither may disturb the
       other. */
    struct fc_Two t; t.a = 0xAAAAAAAAAAAAAAAAULL; t.b = 0x5555555555555555ULL;
    if (t.a != 0xAAAAAAAAAAAAAAAAULL) return 6;
    if (t.b != 0x5555555555555555ULL) return 7;

    /* An over-wide mask would write through the neighbours. */
    struct fc_Pad p; p.pre = 0x11; p.post = 0x22; p.a = 0xDEADBEEFCAFEBABEULL;
    if (p.pre != 0x11) return 8;
    if (p.post != 0x22) return 9;
    if (p.a != 0xDEADBEEFCAFEBABEULL) return 10;
    p.a = 0;
    if (p.pre != 0x11 || p.post != 0x22) return 11;

    /* The same boundary one carrier down. */
    struct fc_U32 u32; u32.a = 0xFFFFFFFFU;
    if (u32.a != 0xFFFFFFFFU) return 12;
    struct fc_S32 s32; s32.a = -1;
    if (s32.a != -1) return 13;

    return 0;
}


/* ---- c89_int128_bitfield_wider_than_64_round_trips (exit codes 171-183) ----
 *
 *  A `__int128` bit-field wider than 64 bits round-trips, at both -O levels.
 *
 *  These were refused outright until the carrier existed: the value mask was a
 *  `u64` and `bitfield_storage_type` had no 16-byte arm. The subtle half is
 *  that the carrier's *kind* must be `Int128`, because that is what routes the
 *  pseudos to a 16-byte stack slot -- an earlier attempt typed them otherwise,
 *  and the backend panicked in `int128_lo_mem_loc` with a value the allocator
 *  had placed in a single GP register.
 *
 *  Widths sit either side of 64 so a mask computed in the wrong width shows
 *  up, and ordinary members bracket the field so an over-wide mask corrupts a
 *  neighbour rather than passing silently.
 */
struct wi_W65  { __int128 a:65; };
struct wi_W100 { __int128 a:100; };
struct wi_W128 { __int128 a:128; };
struct wi_U100 { unsigned __int128 a:100; };
struct wi_Pad  { int pre; __int128 a:100; int post; };
struct wi_Two  { __int128 a:100; __int128 b:100; };
struct wi_Mix  { __int128 a:100; unsigned b:3; };

static int t_c89_int128_bitfield_wider_than_64_round_trips(void) {
    /* Sign must reach past bit 64, which is what a 64-bit carrier could not do. */
    struct wi_W65 x; x.a = -1;
    if (x.a != -1 || !(x.a < 0)) return 1;

    struct wi_W100 y; y.a = -1;
    if (y.a != -1 || !(y.a < 0)) return 2;
    y.a = 5;
    if (y.a != 5) return 3;

    /* Full width: the (1 << n) - 1 spelling collapses to zero here. */
    struct wi_W128 z; z.a = -1;
    if (z.a != -1) return 4;

    /* A bit above the 64-bit half must survive unsigned. */
    struct wi_U100 u; u.a = (unsigned __int128)1 << 99;
    if (u.a != ((unsigned __int128)1 << 99)) return 5;
    u.a = (unsigned __int128)1 << 64;
    if (u.a != ((unsigned __int128)1 << 64)) return 6;

    /* An over-wide mask would write through the neighbours. */
    struct wi_Pad p; p.pre = 0x11111111; p.post = 0x22222222; p.a = -1;
    if (p.pre != 0x11111111) return 7;
    if (p.post != 0x22222222) return 8;
    if (p.a != -1) return 9;

    /* wi_Two wide fields do not share a unit, so neither may disturb the other. */
    struct wi_Two t; t.a = -1; t.b = 5;
    if (t.a != -1) return 10;
    if (t.b != 5) return 11;

    /* A narrow field beside a wide one keeps its own carrier. */
    struct wi_Mix m; m.a = -1; m.b = 5;
    if (m.a != -1) return 12;
    if (m.b != 5) return 13;

    return 0;
}

int main(void)
{
    int r;
    if ((r = t_c89_declarations_mega()) != 0)
        return 0 + r;
    if ((r = t_c89_bitfield_increment_leaves_neighbours_alone()) != 0)
        return 80 + r;
    if ((r = t_c89_zero_width_bitfield_forces_a_boundary()) != 0)
        return 100 + r;
    if ((r = t_c89_bitfield_as_wide_as_its_carrier_round_trips()) != 0)
        return 150 + r;
    if ((r = t_c89_int128_bitfield_wider_than_64_round_trips()) != 0)
        return 170 + r;
    return 0;
}
"#;
    assert_eq!(compile_and_run("c89_declarations_mega", code, &[]), 0);
}

/// Declarators, init-declarator lists, specifier order and attribute placement,
/// as one program; each section keeps its original test name and doc comment,
/// and the exit-code table is at the top. A failure in
/// c89_function_declarators_share_an_init_declarator_list (codes 51-63) means a
/// function declarator must be usable anywhere in an init-declarator list.
///
/// Consolidates: c89_parenthesized_declarators_nest,
/// c89_abstract_declarators_in_type_names,
/// c89_function_declarators_share_an_init_declarator_list,
/// declarations_grouped_declarators_may_be_listed,
/// declarations_pointer_run_belongs_to_its_own_declarator,
/// declarations_specifiers_may_follow_a_struct_definition,
/// declarations_attribute_placements_gcc_accepts and
/// declarations_aligned_attribute_may_reduce_alignment.
#[test]
fn c89_declarators_mega() {
    let code = r#"
/*
 * Exit codes: each section's own failure codes, offset by its base.
 *     1- 11  c89_parenthesized_declarators_nest
 *    21- 43  c89_abstract_declarators_in_type_names
 *    51- 63  c89_function_declarators_share_an_init_declarator_list
 *    71- 76  declarations_grouped_declarators_may_be_listed
 *    81- 82  declarations_pointer_run_belongs_to_its_own_declarator
 *    91- 93  declarations_specifiers_may_follow_a_struct_definition
 *   101-102  declarations_attribute_placements_gcc_accepts
 *   111-114  declarations_aligned_attribute_may_reduce_alignment
 */

/* ---- c89_parenthesized_declarators_nest (exit codes 1-11) ----
 *
 *  C17 6.7.6 makes `direct-declarator` recursive through `( declarator )`, and
 *  5.2.4.1 asks for 63 levels of it. Only one level parsed: the predicate
 *  deciding whether a `(` opened a grouped declarator or a parameter list
 *  looked for `*` or an identifier, and a nested `(` is neither -- so
 *  `int ((q));` was rejected outright as "expected identifier", and so was
 *  every conforming program that spelled a declarator that way.
 *
 *  The same predicate had a second copy that accepted *any* identifier after
 *  `(`, without the typedef test, which read the function-type parameter
 *  `int (fn)(int)` as a declarator named `fn` wrapped in parentheses. Both
 *  copies are now one predicate.
 *
 *  Every expectation here came from gcc on this source.
 */
int ((pd_q)) = 7;
int ((*pd_p));
int (((*pd_h)));
static int pd_arr3[3] = {10, 20, 30};
void ((pd_g))(void);
static int (*((pd_kimpl))(void))[3] { return &pd_arr3; }
int (*(*pd_k)(void))[3];
typedef int (((pd_int_alias)));
pd_int_alias pd_alias_obj = 42;

static int pd_gcalls = 0;
void ((pd_g))(void) { pd_gcalls++; }

/* A parameter whose type is a function type, spelled without a name.
   6.7.6.3p8 adjusts it to a pointer to function. */
static int pd_apply(int (fn)(int), int v) { return fn(v); }
static int pd_twice(int v) { return v * 2; }

static int t_c89_parenthesized_declarators_nest(void) {
    pd_p = &pd_q;
    pd_h = &pd_q;
    if (pd_q != 7) return 1;
    if (*pd_p != 7) return 2;
    if (*pd_h != 7) return 3;

    /* Redundant parentheses must not perturb the type. */
    if (sizeof(pd_q) != sizeof(int)) return 4;
    if (sizeof(pd_p) != sizeof(int *)) return 5;

    pd_g();
    if (pd_gcalls != 1) return 6;

    pd_k = pd_kimpl;
    if ((*pd_k())[0] != 10) return 7;
    if ((*pd_k())[2] != 30) return 8;

    if (pd_alias_obj != 42) return 9;
    if (pd_apply(pd_twice, 21) != 42) return 10;

    int (((((((((((deep))))))))))) = 5;
    if (deep != 5) return 11;

    return 0;
}


/* ---- c89_abstract_declarators_in_type_names (exit codes 21-43) ----
 *
 *  A type-name is a specifier-qualifier list plus an *abstract* declarator
 *  (C17 6.7.7) -- the ordinary declarator grammar with the identifier left
 *  out. Type-names had their own parser, which recognised exactly one abstract
 *  shape, `(*)(params)`, and gave up on everything else. So a pointer to an
 *  array, a pointer to a function pointer, and an array of function pointers
 *  were not spellable in a `sizeof`, a cast, or a `_Generic` association:
 *  `sizeof(int (*)[3])` and `(int(*)[3])0` were parse errors, though the same
 *  types declared as objects had always worked.
 *
 *  They now go through `parse_declarator`, the one every other declarator
 *  uses, so the two spellings cannot disagree again.
 *
 *  Every expectation here came from gcc on this source.
 */
#include <stddef.h>

static int ad_a3[3] = {1, 2, 3};
static int (*ad_pa)[3] = &ad_a3;
static int ad_fret(void) { return 9; }

/* A type-name in a constant expression: array bound, enum value, member. */
static char ad_buf[sizeof(int (*)[3])];
static int ad_tbl[sizeof(int (*[4])(void)) / sizeof(void *)];
enum { ad_E1 = sizeof(int (**)(void)), ad_E2 = sizeof(int[3][4]) };
struct ad_Pad { char pad[sizeof(int (*)(const char *))]; };

static int t_c89_abstract_declarators_in_type_names(void) {
    /* sizeof over shapes the old type-name parser could not spell */
    if (sizeof(int (*)[3]) != sizeof(void *)) return 1;
    if (sizeof(int (**)(void)) != sizeof(void *)) return 2;
    if (sizeof(int (*[4])(void)) != 4 * sizeof(void *)) return 3;
    if (sizeof(int ((*))(void)) != sizeof(void *)) return 4;
    if (sizeof(int[3][4]) != 12 * sizeof(int)) return 5;
    if (sizeof(char[10]) != 10) return 6;
    /* a function type as a type-name, parameters and all */
    if (sizeof(int (*)(const char *)) != sizeof(void *)) return 7;

    /* casts through those same shapes */
    int (*q)[3] = (int (*)[3])&ad_a3;
    if ((*q)[2] != 3) return 8;
    if (q != ad_pa) return 9;

    int (*fp)(void) = (int (*)(void))ad_fret;
    if (fp() != 9) return 10;
    int (**fpp)(void) = &fp;
    if ((*fpp)() != 9) return 11;

    /* An incomplete array type-name still takes its size from the
       initializer: parse_declarator spells "no size" as absent, where the
       parser it replaced spelled it zero. */
    int *ci = (int[]){10, 20, 30};
    if (ci[2] != 30) return 12;
    if (sizeof((int[]){1, 2, 3, 4}) != 4 * sizeof(int)) return 13;

    struct ad_S { int x, y; };
    struct ad_S *cs = &(struct ad_S){4, 5};
    if (cs->y != 5) return 14;

    /* _Generic associations are type-names too, and end at a ':' -- which
       the old abstract-declarator whitelist did not include. */
    if (_Generic((int (*)[3])0, int (*)[3]: 1, default: 0) != 1) return 15;
    if (_Generic(1, int: 1, double: 0, default: 0) != 1) return 16;

    /* __builtin_types_compatible_p takes two type-names. */
    if (!__builtin_types_compatible_p(int (*)[3], int (*)[3])) return 17;
    if (__builtin_types_compatible_p(int (*)[3], int (*)[4])) return 18;

    /* And they must fold in constant-expression contexts, not merely parse. */
    if (sizeof ad_buf != sizeof(void *)) return 19;
    if (sizeof ad_tbl / sizeof ad_tbl[0] != 4) return 20;
    if (ad_E1 != (int)sizeof(void *)) return 21;
    if (ad_E2 != (int)(12 * sizeof(int))) return 22;
    if (sizeof(struct ad_Pad) != sizeof(void *)) return 23;

    return 0;
}


/* ---- c89_function_declarators_share_an_init_declarator_list (exit codes 51-63) ----
 *
 *  A function declarator is an ordinary member of an init-declarator list.
 *
 *  C17 6.7 is *declaration-specifiers init-declarator-list ;* and says nothing
 *  that would exclude a function declarator from the list. c17 rejected every
 *  list whose **first** declarator was a function -- `int f(int), g(int);` gave
 *  "expected ';', found ','" -- because the function-declaration path demanded
 *  a semicolon the moment it had a prototype. An object first was fine, so
 *  `int x, f(int);` already worked, which is why nothing caught it.
 *
 *  Found by building sparse, which opens `dissect.c` with four
 *  pointer-returning function declarators sharing one specifier.
 *
 *  The `*` binds to its own declarator, so this checks derivations differ
 *  across the list rather than only that the line is accepted.
 */
struct fd_S { int v; };

int fd_f(int a), fd_g(int b);
struct fd_S *fd_mk(int v), **fd_mk2(int v);
int fd_h(int), fd_obj = 7, fd_arr[3];
typedef int fd_F(int), fd_G(long);
/* A `*` on the first declarator must not reach the second. */
int *fd_pf(int), fd_plain(int);

int fd_f(int a){ return a * 2; }
int fd_g(int b){ return b + 1; }
int fd_h(int c){ return c - 1; }
int fd_plain(int c){ return c + 100; }
static int fd_cell;
int *fd_pf(int v){ fd_cell = v; return &fd_cell; }

static struct fd_S fd_storage;
static struct fd_S *fd_pstorage;
struct fd_S *fd_mk(int v){ fd_storage.v = v; return &fd_storage; }
struct fd_S **fd_mk2(int v){ fd_pstorage = fd_mk(v); return &fd_pstorage; }

static fd_F *fd_fp = fd_f;
static fd_G *fd_gp;
static int fd_gimpl(long x){ return (int)(x * 10); }

static int t_c89_function_declarators_share_an_init_declarator_list(void) {
    if (fd_f(21) != 42) return 1;
    if (fd_g(1) != 2) return 2;
    if (fd_h(5) != 4) return 3;
    if (fd_obj != 7) return 4;
    if (sizeof fd_arr / sizeof fd_arr[0] != 3) return 5;
    if (fd_mk(9)->v != 9) return 6;
    if ((*fd_mk2(8))->v != 8) return 7;

    /* Each typedef in the list keeps its own signature. */
    if (fd_fp(3) != 6) return 8;
    fd_gp = fd_gimpl;
    if (fd_gp(4) != 40) return 9;

    /* `int *fd_pf(int), fd_plain(int);` -- fd_pf returns int *, fd_plain returns int. */
    if (*fd_pf(11) != 11) return 10;
    if (fd_plain(1) != 101) return 11;

    /* Taking each through a correctly-typed pointer proves the derivation
       rather than just the value. */
    int *(*ppf)(int) = fd_pf;
    int (*pplain)(int) = fd_plain;
    if (*ppf(12) != 12) return 12;
    if (pplain(2) != 102) return 13;
    return 0;
}


/* ---- declarations_grouped_declarators_may_be_listed (exit codes 71-76) ----
 *
 *  `int (*b)(), (*c)();` is two declarators, not two declarations.
 *
 *  At file scope the parenthesized form went down a path of its own that
 *  parsed exactly one declarator and then demanded the semicolon, so the comma
 *  was a syntax error -- although the identical line inside a function has
 *  always worked, because block scope runs one list walker for every
 *  declarator it sees. The two paths now share that walker.
 */
int gd_one(void) { return 1; }
int gd_two(void) { return 2; }

int (*gd_b)(void), (*gd_c)(void);
int (*gd_arr1)[4], (*gd_arr2)[8];
int gd_plain, (*gd_mixed)(void), gd_tail;
typedef int (gd_fa)(void), (gd_fb)(int);
static gd_fa *gd_sp = gd_one;

static int t_declarations_grouped_declarators_may_be_listed(void) {
    int (*p)(void), (*q)(void);           /* the block-scope spelling */
    gd_b = gd_one; gd_c = gd_two;
    if (gd_b() != 1 || gd_c() != 2) return 1;
    p = gd_two; q = gd_one;
    if (p() != 2 || q() != 1) return 2;
    if (sizeof(*gd_arr1) != 4 * sizeof(int)) return 3;
    if (sizeof(*gd_arr2) != 8 * sizeof(int)) return 4;
    gd_plain = 5; gd_tail = 6; gd_mixed = gd_two;
    if (gd_plain + gd_tail != 11 || gd_mixed() != 2) return 5;
    if (gd_sp() != 1) return 6;
    return 0;
}


/* ---- declarations_pointer_run_belongs_to_its_own_declarator (exit codes 81-82) ----
 *
 *  The `*` written before a parenthesized declarator belongs to that
 *  declarator alone: in `int *(*d)(void), (*e)(void);` the second declarator
 *  is built from `int`, not from `int *`.
 */
int pr_one(void) { return 1; }
static int pr_value = 7;
int *pr_ptr(void) { return &pr_value; }

int *(*pr_d)(void), (*pr_e)(void);

static int t_declarations_pointer_run_belongs_to_its_own_declarator(void) {
    pr_d = pr_ptr;
    pr_e = pr_one;
    if (*pr_d() != 7) return 1;
    if (pr_e() != 1) return 2;
    return 0;
}


/* ---- declarations_specifiers_may_follow_a_struct_definition (exit codes 91-93) ----
 *
 *  C17 6.7p1 lets the specifiers of a declaration appear in any order, so a
 *  storage class or a function specifier may follow a `struct` definition.
 *
 *  The list that accepted them there was five storage classes long, which made
 *  `struct S { int x; } extern __thread a;` stop at `__thread` and read it as
 *  the declarator's name -- a silent misparse rather than a diagnostic.
 */
struct sf_A { int x; };
__thread struct sf_A sf_ta;                  /* the definition `extern` refers to */
struct sf_A extern __thread sf_ta;           /* specifiers in any order, C17 6.7p1 */
struct sf_B { int x; } static sf_sb;
union sf_C { int x; } static const sf_uc;
struct sf_D { int x; } _Alignas(16) sf_da;

static int t_declarations_specifiers_may_follow_a_struct_definition(void) {
    if (_Alignof(sf_da) != 16) return 1;
    sf_sb.x = 3;
    if (sf_sb.x != 3) return 2;
    sf_ta.x = 4;
    if (sf_ta.x != 4) return 3;
    (void)sf_uc;
    return 0;
}


/* ---- declarations_attribute_placements_gcc_accepts (exit codes 101-102) ----
 *
 *  An attribute may sit between two `*`s, at the head of a parameter
 *  declaration, and after a comma in a declarator list.
 *
 *  The first was worse than a parse error at block scope:
 *  `expect_declarator_name` does not treat `__attribute__` as reserved, so it
 *  became the declared object's *name*. The second came of two predicates
 *  disagreeing about whether a declaration starts here -- one asked
 *  `TYPE_KEYWORD`, the other `DECL_START`, which includes the attribute
 *  keyword.
 */
int *__attribute__((__aligned__(16))) *ap_pp;
void ap_bar(int (__attribute__((__mode__(__SI__))) int foo));
int ap_a, __attribute__((unused)) ap_bb;
__attribute__((noreturn)) void ap_d0(void), ap_d1(void);

static int t_declarations_attribute_placements_gcc_accepts(void) {
    int *__attribute__((aligned(16))) *q;
    int local, __attribute__((unused)) other;
    q = ap_pp;
    (void)q;
    local = 1;
    other = 2;
    return local + other == 3 ? 0 : 1;
}

void ap_d0(void) { for (;;) {} }
void ap_d1(void) { for (;;) {} }


/* ---- declarations_aligned_attribute_may_reduce_alignment (exit codes 111-114) ----
 *
 *  C11 6.7.5p5 forbids the `_Alignas` **keyword** from reducing alignment.
 *  The GNU `aligned` attribute shares the same slot and is not constrained:
 *  gcc accepts `__attribute__((aligned(2))) int` on a typedef, a variable and
 *  a struct alike, and rejects `_Alignas(2) int`. The check knew which was
 *  written and never asked.
 */
typedef __attribute__((aligned(2))) int ar_small_int;
__attribute__((aligned(2))) int ar_reduced;
struct ar_raised { int x; } __attribute__((aligned(16)));

static int t_declarations_aligned_attribute_may_reduce_alignment(void) {
    if (_Alignof(ar_small_int) != 2) return 1;
    if (_Alignof(ar_reduced) != 2) return 2;
    /* On a struct, `aligned` only raises: reducing one needs `packed`, and
       gcc answers 4 here too. Raising is the usual case and still works. */
    if (_Alignof(struct ar_raised) != 16) return 3;
    {
        typedef __attribute__((aligned(32))) int wide_int;
        if (_Alignof(wide_int) != 32) return 4;
    }
    return 0;
}

int main(void)
{
    int r;
    if ((r = t_c89_parenthesized_declarators_nest()) != 0)
        return 0 + r;
    if ((r = t_c89_abstract_declarators_in_type_names()) != 0)
        return 20 + r;
    if ((r = t_c89_function_declarators_share_an_init_declarator_list()) != 0)
        return 50 + r;
    if ((r = t_declarations_grouped_declarators_may_be_listed()) != 0)
        return 70 + r;
    if ((r = t_declarations_pointer_run_belongs_to_its_own_declarator()) != 0)
        return 80 + r;
    if ((r = t_declarations_specifiers_may_follow_a_struct_definition()) != 0)
        return 90 + r;
    if ((r = t_declarations_attribute_placements_gcc_accepts()) != 0)
        return r > 0 && r < 2 ? 100 + r : 102;
    if ((r = t_declarations_aligned_attribute_may_reduce_alignment()) != 0)
        return 110 + r;
    return 0;
}
"#;
    assert_eq!(compile_and_run("c89_declarators_mega", code, &[]), 0);
}

/// A `__int128` bit-field wider than 64 bits round-trips, at both -O levels.
///
/// These were refused outright until the carrier existed: the value mask was a
/// `u64` and `bitfield_storage_type` had no 16-byte arm. The subtle half is
/// that the carrier's *kind* must be `Int128`, because that is what routes the
/// pseudos to a 16-byte stack slot -- an earlier attempt typed them otherwise,
/// and the backend panicked in `int128_lo_mem_loc` with a value the allocator
/// had placed in a single GP register.
///
/// Widths sit either side of 64 so a mask computed in the wrong width shows
/// up, and ordinary members bracket the field so an over-wide mask corrupts a
/// neighbour rather than passing silently.
#[test]
fn c89_int128_bitfield_wider_than_64_round_trips() {
    let code = r#"
struct W65  { __int128 a:65; };
struct W100 { __int128 a:100; };
struct W128 { __int128 a:128; };
struct U100 { unsigned __int128 a:100; };
struct Pad  { int pre; __int128 a:100; int post; };
struct Two  { __int128 a:100; __int128 b:100; };
struct Mix  { __int128 a:100; unsigned b:3; };

int main(void) {
    /* Sign must reach past bit 64, which is what a 64-bit carrier could not do. */
    struct W65 x; x.a = -1;
    if (x.a != -1 || !(x.a < 0)) return 1;

    struct W100 y; y.a = -1;
    if (y.a != -1 || !(y.a < 0)) return 2;
    y.a = 5;
    if (y.a != 5) return 3;

    /* Full width: the (1 << n) - 1 spelling collapses to zero here. */
    struct W128 z; z.a = -1;
    if (z.a != -1) return 4;

    /* A bit above the 64-bit half must survive unsigned. */
    struct U100 u; u.a = (unsigned __int128)1 << 99;
    if (u.a != ((unsigned __int128)1 << 99)) return 5;
    u.a = (unsigned __int128)1 << 64;
    if (u.a != ((unsigned __int128)1 << 64)) return 6;

    /* An over-wide mask would write through the neighbours. */
    struct Pad p; p.pre = 0x11111111; p.post = 0x22222222; p.a = -1;
    if (p.pre != 0x11111111) return 7;
    if (p.post != 0x22222222) return 8;
    if (p.a != -1) return 9;

    /* Two wide fields do not share a unit, so neither may disturb the other. */
    struct Two t; t.a = -1; t.b = 5;
    if (t.a != -1) return 10;
    if (t.b != 5) return 11;

    /* A narrow field beside a wide one keeps its own carrier. */
    struct Mix m; m.a = -1; m.b = 5;
    if (m.a != -1) return 12;
    if (m.b != 5) return 13;

    return 0;
}
"#;
    // The matrix-level run is a section of c89_declarations_mega.
    assert_eq!(
        compile_and_run_optimized("c89_int128_bitfield_wide_o2", code),
        0
    );
}

// ============================================================================
// Where a GNU attribute may be written
// ============================================================================

/// Forward, qualified, inner and hidden tags, on every level and target; each
/// section keeps its original test name and doc comment, and the exit-code table
/// is at the top.
///
/// Consolidates: c89_forward_enum_is_completed_by_its_definition,
/// c89_qualified_forward_tag_is_completed_by_its_definition,
/// c89_inner_tag_definition_does_not_complete_the_outer_tag,
/// c89_typedef_names_its_own_tag_under_an_inner_one and
/// c89_bare_tag_declaration_hides_the_outer_tag.
#[test]
fn c89_tag_scope_everywhere_mega() {
    let code = r#"
/*
 * Exit codes: each section's own failure codes, offset by its base.
 *     1-  4  c89_forward_enum_is_completed_by_its_definition
 *    11- 17  c89_qualified_forward_tag_is_completed_by_its_definition
 *    21- 23  c89_inner_tag_definition_does_not_complete_the_outer_tag
 *    31- 34  c89_typedef_names_its_own_tag_under_an_inner_one
 *    41- 43  c89_bare_tag_declaration_hides_the_outer_tag
 */

/* ---- c89_forward_enum_is_completed_by_its_definition (exit codes 1-4) ----
 *
 *  A tag named before its enum is defined -- `typedef enum foo E;` ahead of
 *  `enum foo { ... }`, a GNU forward enum gcc accepts -- is the complete
 *  type once the definition is seen: a typedef, a member and a pointee
 *  declared through the forward reference all take the enum's size and
 *  signedness. The forward reference used to stay a 0-byte incomplete type,
 *  so `sizeof (E)` was rejected and a member of type `E` was stored in no
 *  bytes at all (gcc.c-torture/execute/930408-1).
 */
typedef enum fe_foo fe_E;
enum fe_foo *fe_fp;
enum fe_foo { fe_e0, fe_e1, fe_e2 = 0x12345678 };
enum fe_big;
typedef enum fe_big fe_B;
enum fe_big { fe_HUGE = 0x80000000u };
struct { fe_E eval; int after; } fe_s;
struct { fe_B b; } fe_t;
static int t_c89_forward_enum_is_completed_by_its_definition(void) {
    enum fe_foo v = fe_e1;
    fe_fp = &v;
    if (sizeof(fe_E) != sizeof(int) || sizeof fe_s != 2 * sizeof(int)) return 1;
    fe_s.eval = fe_e2;
    fe_s.after = 7;
    if (fe_s.eval != fe_e2 || fe_s.after != 7 || *fe_fp != fe_e1) return 2;
    fe_t.b = fe_HUGE;
    if (fe_t.b < 0 || sizeof(fe_B) != 4) return 3;
    switch (fe_s.eval) { case fe_e2: break; default: return 4; }
    return 0;
}


/* ---- c89_qualified_forward_tag_is_completed_by_its_definition (exit codes 11-17) ----
 *
 *  A *qualified* reference to a tag before its definition -- `const enum E`,
 *  `volatile enum E`, `const struct S` -- is the same type as the tag once
 *  the definition is seen, qualifiers and all. Each such reference is its
 *  own `TypeId` (a qualified copy of the tag's type), and the definition
 *  used to complete only the tag's own entry: the copies stayed incomplete,
 *  so `sizeof` of a `const enum` typedef was rejected, a large enumerator
 *  would have been truncated through it, and `void show(const enum col *)`
 *  declared before the enum and defined after it was "conflicting types".
 */
enum qf_big;
typedef const enum qf_big qf_CB;
typedef volatile enum qf_big qf_VB;
enum qf_pos;
typedef const enum qf_pos qf_CP;
enum qf_col;
void qf_show(const enum qf_col *);
struct qf_holder { const enum qf_col *p; volatile enum qf_col *q; };
struct qf_S;
typedef const struct qf_S qf_CS;
enum qf_big { qf_NEG = -1, qf_LARGE = 0x7fffffffffLL };
enum qf_pos { qf_P0, qf_PHIGH = 0x80000000u };
enum qf_col { qf_RED, qf_GREEN = 5 };
struct qf_S { long a, b; };
static int qf_seen;
void qf_show(const enum qf_col *c) { qf_seen = *c; }
static int t_c89_qualified_forward_tag_is_completed_by_its_definition(void) {
    qf_CB cb = qf_LARGE;
    qf_VB vb = qf_LARGE;
    qf_CP cp = qf_PHIGH;
    qf_CS cs = { 1, 2 };
    enum qf_col c = qf_GREEN, *pc = &c;
    const enum qf_col *qc = pc;
    struct qf_holder h = { qc, &c };
    if (sizeof(qf_CB) != sizeof(long long) || sizeof(qf_VB) != sizeof(long long)) return 1;
    if (cb != qf_LARGE || vb != qf_LARGE || cb >> 32 != 0x7f) return 2;
    vb = qf_NEG;
    if (vb >= 0) return 3;
    if (sizeof(qf_CP) != 4 || cp <= 0 || cp != 0x80000000u) return 4;
    if (sizeof(qf_CS) != 2 * sizeof(long) || cs.b != 2) return 5;
    qf_show(h.p);
    if (qf_seen != qf_GREEN) return 6;
    *h.q = qf_RED;
    qf_show(qc);
    if (qf_seen != qf_RED || sizeof *h.p != sizeof(int)) return 7;
    return 0;
}


/* ---- c89_inner_tag_definition_does_not_complete_the_outer_tag (exit codes 21-23) ----
 *
 *  A tag defined in an inner scope is a new type that hides the outer one
 *  (C17 6.7.2.3p5), even while the outer tag is still incomplete: the
 *  definition inside `inner` must not complete the file-scope `struct S` or
 *  `enum E`, which get their own definitions later. The inner definition used
 *  to complete the outer forward declaration, so `g->y` named no member. A
 *  qualified redeclaration on either side of the enum's definition --
 *  `extern const enum E ce;` -- is one object, not "conflicting types".
 */
struct it_S;
const struct it_S *it_g;
enum it_E;
extern const enum it_E it_ce;
static int it_inner(void) {
    struct it_S { int a; } s = { 3 };
    enum it_E { it_IA = 7 } e = it_IA;
    return s.a + (int)sizeof s + e;
}
struct it_S { long x, y; };
enum it_E { it_EA, it_EB = 3000000000u };
extern const enum it_E it_ce;
const enum it_E it_ce = it_EB;
static int t_c89_inner_tag_definition_does_not_complete_the_outer_tag(void) {
    static struct it_S t = { 1, 2 };
    it_g = &t;
    if (it_inner() != 3 + (int)sizeof(int) + 7) return 1;
    if (sizeof *it_g != 2 * sizeof(long) || it_g->y != 2) return 2;
    if (sizeof it_ce != 4 || it_ce <= 0 || it_ce != 3000000000u) return 3;
    return 0;
}


/* ---- c89_typedef_names_its_own_tag_under_an_inner_one (exit codes 31-34) ----
 *
 *  A typedef name or `typeof` names the tag visible where *it* was declared,
 *  not whatever an inner scope has since given that tag's name to.
 *
 *  The type behind `TS` used to be found again by looking its tag up by name
 *  at the point of use, so in a block that defines its own `struct S`, `TS x;`
 *  declared `x` with the inner type: `sizeof x` was the inner one's, and
 *  `v.a` had "no member named 'a'".
 */
struct tn_S { int a; };
typedef struct tn_S tn_TS;
typedef const struct tn_S tn_CTS;
struct tn_S *tn_gp;
static int tn_f1(void) { struct tn_S { char c[50]; }; tn_TS x; return sizeof x; }
static int tn_f2(void) { struct tn_S { char c[50]; }; tn_CTS x = { 0 }; return sizeof x; }
static int tn_f3(void) { struct tn_S { char c[50]; }; tn_TS *p = 0; return sizeof *p; }
static int tn_f4(void) { struct tn_S { char c[50]; }; return sizeof *tn_gp; }
static int tn_f5(void) { struct tn_S { char c[50]; }; __typeof__(*tn_gp) y; return sizeof y; }
static int tn_f6(void) { struct tn_S { char c[50]; }; struct tn_W { tn_TS m; int k; } w; return sizeof w; }
static int tn_f7(void) { struct tn_S { char c[50]; }; tn_TS arr[2]; return sizeof arr; }
static int tn_f8(void) { struct tn_S { char c[50]; }; tn_TS v = { 7 }; return v.a; }
static int t_c89_typedef_names_its_own_tag_under_an_inner_one(void) {
    if (tn_f1() != sizeof(int) || tn_f2() != sizeof(int) || tn_f3() != sizeof(int)) return 1;
    if (tn_f4() != sizeof(int) || tn_f5() != sizeof(int)) return 2;
    if (tn_f6() != 2 * sizeof(int) || tn_f7() != 2 * sizeof(int)) return 3;
    if (tn_f8() != 7) return 4;
    return 0;
}


/* ---- c89_bare_tag_declaration_hides_the_outer_tag (exit codes 41-43) ----
 *
 *  C17 6.7.2.3p7: `struct S;` alone declares a new incomplete type in its
 *  scope, hiding an outer `struct S`, and a definition later in that scope
 *  completes the new one. It used to name the outer tag, so a pointer
 *  declared between the two pointed at the outer type: `sizeof *p` was 4 for
 *  a 50-byte struct. With anything else written ahead of it (`const`,
 *  `static`) it is an empty declaration that redeclares nothing.
 */
struct bt_S { int a; };
static int bt_hidden(void) {
    struct bt_S;
    struct bt_S *p = 0;
    struct bt_S { char c[50]; };
    return sizeof *p;
}
static int bt_qualified(void) {
    const struct bt_S;
    struct bt_S *p = 0;
    return sizeof *p;
}
static int bt_nested(void) {
    struct bt_S;
    {
        struct bt_S { char c[9]; } inner;
        (void)inner;
    }
    struct bt_S { char c[3]; } x;
    return sizeof x;
}
static int t_c89_bare_tag_declaration_hides_the_outer_tag(void) {
    if (bt_hidden() != 50) return 1;
    if (bt_qualified() != sizeof(int)) return 2;
    if (bt_nested() != 3) return 3;
    return 0;
}

int main(void)
{
    int r;
    if ((r = t_c89_forward_enum_is_completed_by_its_definition()) != 0)
        return 0 + r;
    if ((r = t_c89_qualified_forward_tag_is_completed_by_its_definition()) != 0)
        return 10 + r;
    if ((r = t_c89_inner_tag_definition_does_not_complete_the_outer_tag()) != 0)
        return 20 + r;
    if ((r = t_c89_typedef_names_its_own_tag_under_an_inner_one()) != 0)
        return 30 + r;
    if ((r = t_c89_bare_tag_declaration_hides_the_outer_tag()) != 0)
        return 40 + r;
    return 0;
}
"#;
    compile_and_run_everywhere("c89_tag_scope_everywhere_mega", code);
}
