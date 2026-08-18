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

/// A zero-width bitfield forces the next member to the next boundary of its
/// declared type's storage unit (C17 6.7.2.1p12).
///
/// The layout code flushed an open bitfield storage unit but never aligned
/// the offset, so after a plain member -- where no unit is open -- `int :0;`
/// did nothing at all: `struct { char c; int :0; char d; }` was 2 bytes
/// rather than 5, disagreeing with every gcc-compiled object.
///
/// Every expectation here came from gcc on this source.
#[test]
fn c89_zero_width_bitfield_forces_a_boundary() {
    let code = r#"
#include <stddef.h>

struct A { char c; int :0; char d; };            /* after a plain member    */
struct B { int a:3; int :0; int b:5; };          /* after a same-type field */
struct C { char a:3; int :0; char b:5; };        /* different-type field    */
struct D { int a:3; int :0; int :0; int b:5; };  /* two in a row            */
struct E { char c; int :0; };                    /* at the end              */
struct F { char c; short :0; char d; };          /* a narrower unit         */
struct G { int :0; char c; };                    /* first, before anything  */
struct H { int a:3; int :4; int :0; int b:5; };  /* beside an unnamed field */
struct P { char c; int :0; char d; } __attribute__((packed));
struct Z { char c; char :0; char d; };           /* asks for alignment 1    */
union  U { char c; int :0; };                    /* in a union              */
union  V { char c; long :0; };
union  W { char c; int :0; } __attribute__((packed));

int main(void) {
    /* The boundary itself is target-independent: every one of these offsets
       is the same under gcc on both targets. */
    if (offsetof(struct A, c) != 0) return 2;
    if (offsetof(struct A, d) != 4) return 3;
    if (offsetof(struct F, d) != 2) return 9;
    if (offsetof(struct G, c) != 0) return 11;
    if (offsetof(struct P, d) != 4) return 45;

    /* So are the shapes whose alignment an int bitfield already set. */
    if (sizeof(struct B) != 8 || _Alignof(struct B) != 4) return 4;
    if (sizeof(struct D) != 8 || _Alignof(struct D) != 4) return 6;
    if (sizeof(struct H) != 8 || _Alignof(struct H) != 4) return 12;
    /* A `char :0` asks for alignment 1, so it changes nothing anywhere. */
    if (sizeof(struct Z) != 2 || _Alignof(struct Z) != 1) return 46;

    /* What the two ABIs disagree about is whether a zero-width bitfield
       raises the *aggregate's* alignment. C17 6.7.2.1p12 leaves it to them:
       the x86-64 psABI says no, so A is 5 bytes with alignment 1; AAPCS64
       says it contributes its declared type's alignment, making A 8 with
       alignment 4 -- and, unlike an ordinary member's, that survives packing.
       Every number below is gcc's on the target it is written for. */
#ifdef __aarch64__
    if (sizeof(struct A) != 8 || _Alignof(struct A) != 4) return 40;
    if (sizeof(struct C) != 8 || _Alignof(struct C) != 4) return 41;
    if (sizeof(struct E) != 4 || _Alignof(struct E) != 4) return 42;
    if (sizeof(struct F) != 4 || _Alignof(struct F) != 2) return 43;
    if (sizeof(struct G) != 4 || _Alignof(struct G) != 4) return 10;
    if (sizeof(struct P) != 8 || _Alignof(struct P) != 4) return 44;
    /* A union takes the alignment too, and with it the trailing padding. */
    if (sizeof(union U) != 4 || _Alignof(union U) != 4) return 47;
    if (sizeof(union V) != 8 || _Alignof(union V) != 8) return 48;
    if (sizeof(union W) != 4 || _Alignof(union W) != 4) return 49;
#else
    if (sizeof(struct A) != 5 || _Alignof(struct A) != 1) return 40;
    if (sizeof(struct C) != 5 || _Alignof(struct C) != 1) return 41;
    if (sizeof(struct E) != 4 || _Alignof(struct E) != 1) return 42;
    if (sizeof(struct F) != 3 || _Alignof(struct F) != 1) return 43;
    if (sizeof(struct G) != 1 || _Alignof(struct G) != 1) return 10;
    if (sizeof(struct P) != 5 || _Alignof(struct P) != 1) return 44;
    /* A zero-width bitfield occupies no storage, so it cannot widen a union:
       the union is as wide as its widest real member. */
    if (sizeof(union U) != 1 || _Alignof(union U) != 1) return 47;
    if (sizeof(union V) != 1 || _Alignof(union V) != 1) return 48;
    if (sizeof(union W) != 1 || _Alignof(union W) != 1) return 49;
#endif

    /* The surrounding fields still read and write correctly. */
    struct B b;
    b.a = 5;
    b.b = -7;
    if (b.a != -3) return 15;
    if (b.b != -7) return 16;

    struct A a;
    a.c = 'x';
    a.d = 'y';
    if (a.c != 'x') return 17;
    if (a.d != 'y') return 18;

    struct C c;
    c.a = 1;
    c.b = 2;
    if (c.a != 1) return 19;
    if (c.b != 2) return 20;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("c89_zero_width_bitfield_forces_a_boundary", code, &[]),
        0
    );
}

/// C17 6.7.6 makes `direct-declarator` recursive through `( declarator )`, and
/// 5.2.4.1 asks for 63 levels of it. Only one level parsed: the predicate
/// deciding whether a `(` opened a grouped declarator or a parameter list
/// looked for `*` or an identifier, and a nested `(` is neither -- so
/// `int ((q));` was rejected outright as "expected identifier", and so was
/// every conforming program that spelled a declarator that way.
///
/// The same predicate had a second copy that accepted *any* identifier after
/// `(`, without the typedef test, which read the function-type parameter
/// `int (fn)(int)` as a declarator named `fn` wrapped in parentheses. Both
/// copies are now one predicate.
///
/// Every expectation here came from gcc on this source.
#[test]
fn c89_parenthesized_declarators_nest() {
    let code = r#"
int ((q)) = 7;
int ((*p));
int (((*h)));
static int arr3[3] = {10, 20, 30};
void ((g))(void);
static int (*((kimpl))(void))[3] { return &arr3; }
int (*(*k)(void))[3];
typedef int (((int_alias)));
int_alias alias_obj = 42;

static int gcalls = 0;
void ((g))(void) { gcalls++; }

/* A parameter whose type is a function type, spelled without a name.
   6.7.6.3p8 adjusts it to a pointer to function. */
static int apply(int (fn)(int), int v) { return fn(v); }
static int twice(int v) { return v * 2; }

int main(void) {
    p = &q;
    h = &q;
    if (q != 7) return 1;
    if (*p != 7) return 2;
    if (*h != 7) return 3;

    /* Redundant parentheses must not perturb the type. */
    if (sizeof(q) != sizeof(int)) return 4;
    if (sizeof(p) != sizeof(int *)) return 5;

    g();
    if (gcalls != 1) return 6;

    k = kimpl;
    if ((*k())[0] != 10) return 7;
    if ((*k())[2] != 30) return 8;

    if (alias_obj != 42) return 9;
    if (apply(twice, 21) != 42) return 10;

    int (((((((((((deep))))))))))) = 5;
    if (deep != 5) return 11;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("c89_parenthesized_declarators_nest", code, &[]),
        0
    );
}

/// A type-name is a specifier-qualifier list plus an *abstract* declarator
/// (C17 6.7.7) -- the ordinary declarator grammar with the identifier left
/// out. Type-names had their own parser, which recognised exactly one abstract
/// shape, `(*)(params)`, and gave up on everything else. So a pointer to an
/// array, a pointer to a function pointer, and an array of function pointers
/// were not spellable in a `sizeof`, a cast, or a `_Generic` association:
/// `sizeof(int (*)[3])` and `(int(*)[3])0` were parse errors, though the same
/// types declared as objects had always worked.
///
/// They now go through `parse_declarator`, the one every other declarator
/// uses, so the two spellings cannot disagree again.
///
/// Every expectation here came from gcc on this source.
#[test]
fn c89_abstract_declarators_in_type_names() {
    let code = r#"
#include <stddef.h>

static int a3[3] = {1, 2, 3};
static int (*pa)[3] = &a3;
static int fret(void) { return 9; }

/* A type-name in a constant expression: array bound, enum value, member. */
static char buf[sizeof(int (*)[3])];
static int tbl[sizeof(int (*[4])(void)) / sizeof(void *)];
enum { E1 = sizeof(int (**)(void)), E2 = sizeof(int[3][4]) };
struct Pad { char pad[sizeof(int (*)(const char *))]; };

int main(void) {
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
    int (*q)[3] = (int (*)[3])&a3;
    if ((*q)[2] != 3) return 8;
    if (q != pa) return 9;

    int (*fp)(void) = (int (*)(void))fret;
    if (fp() != 9) return 10;
    int (**fpp)(void) = &fp;
    if ((*fpp)() != 9) return 11;

    /* An incomplete array type-name still takes its size from the
       initializer: parse_declarator spells "no size" as absent, where the
       parser it replaced spelled it zero. */
    int *ci = (int[]){10, 20, 30};
    if (ci[2] != 30) return 12;
    if (sizeof((int[]){1, 2, 3, 4}) != 4 * sizeof(int)) return 13;

    struct S { int x, y; };
    struct S *cs = &(struct S){4, 5};
    if (cs->y != 5) return 14;

    /* _Generic associations are type-names too, and end at a ':' -- which
       the old abstract-declarator whitelist did not include. */
    if (_Generic((int (*)[3])0, int (*)[3]: 1, default: 0) != 1) return 15;
    if (_Generic(1, int: 1, double: 0, default: 0) != 1) return 16;

    /* __builtin_types_compatible_p takes two type-names. */
    if (!__builtin_types_compatible_p(int (*)[3], int (*)[3])) return 17;
    if (__builtin_types_compatible_p(int (*)[3], int (*)[4])) return 18;

    /* And they must fold in constant-expression contexts, not merely parse. */
    if (sizeof buf != sizeof(void *)) return 19;
    if (sizeof tbl / sizeof tbl[0] != 4) return 20;
    if (E1 != (int)sizeof(void *)) return 21;
    if (E2 != (int)(12 * sizeof(int))) return 22;
    if (sizeof(struct Pad) != sizeof(void *)) return 23;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("c89_abstract_declarators_in_type_names", code, &[]),
        0
    );
}
