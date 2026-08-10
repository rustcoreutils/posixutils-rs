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

use crate::common::compile_and_run;

// ============================================================================
// Mega-test: Declarations (Section 7 of C99)
// ============================================================================

#[test]
fn c89_declarations_mega() {
    let code = r#"
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

int main(void) {
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
"#;
    assert_eq!(compile_and_run("c89_declarations_mega", code, &[]), 0);
}

/// `++` and `--` on a bitfield must change only that field.
///
/// They did not: the increment path stored its result with a plain store of
/// the whole storage unit, so the value landed at bit 0 and every neighbour
/// sharing the unit was wiped. On `struct A{unsigned a:3;int b:5;unsigned
/// c:1;signed d:4;}` holding `{7,5,1,-8}`, `a.b++` gave `6 0 0 0` where gcc
/// gives `7 6 1 -8` -- literally the value 6 written at offset 0.
///
/// Assignment and compound assignment were always correct, because they went
/// through `emit_bitfield_store`; the increment path simply did not. All four
/// now share `emit_member_store`.
#[test]
fn c89_bitfield_increment_leaves_neighbours_alone() {
    let code = r#"
#include <stdio.h>
#include <string.h>

struct A { unsigned a:3; int b:5; unsigned c:1; signed d:4; };
struct U { unsigned x:4; unsigned y:4; };

static int check(struct A got, unsigned a, int b, unsigned c, int d) {
    return got.a == a && got.b == b && got.c == c && got.d == d;
}

static struct A by_value(struct A v) { v.b++; return v; }

int main(void) {
    struct A base = {7, 5, 1, -8};
    struct A t;

    t = base; t.b++;   if (!check(t, 7, 6, 1, -8)) return 1;
    t = base; ++t.b;   if (!check(t, 7, 6, 1, -8)) return 2;
    t = base; t.b--;   if (!check(t, 7, 4, 1, -8)) return 3;
    t = base; --t.b;   if (!check(t, 7, 4, 1, -8)) return 4;

    /* Each field in turn, so a wrong bit offset cannot pass by luck. */
    t = base; t.a++;   if (!check(t, 0, 5, 1, -8)) return 5;   /* 7+1 wraps 3 bits */
    t = base; t.c++;   if (!check(t, 7, 5, 0, -8)) return 6;   /* 1+1 wraps 1 bit  */
    t = base; t.d++;   if (!check(t, 7, 5, 1, -7)) return 7;

    /* The value of the expression: postfix is the old value, prefix the new. */
    t = base; if (t.b++ != 5) return 8;
    t = base; if (++t.b != 6) return 9;

    /* Signed wrap at the field's width, not the storage unit's. */
    t = base; t.b = 15; t.b++;  if (t.b != -16) return 10;
    t = base; t.b = -16; t.b--; if (t.b != 15) return 11;

    /* Every access path, since each had its own store. */
    struct A arr[2] = {{7,5,1,-8}, {7,5,1,-8}};
    arr[1].b++;
    if (!check(arr[1], 7, 6, 1, -8)) return 12;
    if (!check(arr[0], 7, 5, 1, -8)) return 13;

    t = base;
    struct A *p = &t; p->b++;
    if (!check(t, 7, 6, 1, -8)) return 14;

    t = base; t = by_value(t);
    if (!check(t, 7, 6, 1, -8)) return 15;

    /* Two fields in one unit, so a full-unit write is unmissable. */
    struct U u = {5, 6}; u.x++;
    if (u.x != 6 || u.y != 6) return 16;

    /* Compound and plain assignment, which always worked -- pinned so a fix
       to one path cannot regress the other. */
    t = base; t.b += 1;      if (!check(t, 7, 6, 1, -8)) return 17;
    t = base; t.b = t.b + 1; if (!check(t, 7, 6, 1, -8)) return 18;
    t = base; t.b = 6;       if (!check(t, 7, 6, 1, -8)) return 19;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("c89_bitfield_increment_leaves_neighbours_alone", code, &[]),
        0
    );
}
