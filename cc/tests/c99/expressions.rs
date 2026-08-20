//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C99 Expressions Mega-Test
//
// Covers gaps: casts (pointer↔integer, between pointer types, void cast),
// constant expressions (address constants, compile-time eval),
// implicit conversions (integer promotions, default arg promotions,
// array/function-to-pointer decay, lvalue conversion)
//

use crate::common::{compile_and_run, compile_and_run_optimized};

// ============================================================================
// Mega-test: C99 expressions (Section 5)
// ============================================================================

#[test]
fn c99_expressions_mega() {
    let code = r#"
#include <stddef.h>
#include <stdarg.h>
#include <string.h>

// Helpers for function-to-pointer decay and default arg promotions
int add(int a, int b) { return a + b; }
int mul(int a, int b) { return a * b; }

// Variadic to test default argument promotions
int sum_promoted(int count, ...) {
    va_list ap;
    va_start(ap, count);
    int total = 0;
    for (int i = 0; i < count; i++) {
        total += va_arg(ap, int);  // char/short promoted to int
    }
    va_end(ap);
    return total;
}

double sum_double_promoted(int count, ...) {
    va_list ap;
    va_start(ap, count);
    double total = 0.0;
    for (int i = 0; i < count; i++) {
        total += va_arg(ap, double);  // float promoted to double
    }
    va_end(ap);
    return total;
}

// Global for address constant tests
int global_var = 42;
int global_arr[5] = {10, 20, 30, 40, 50};
int *global_ptr = &global_var;
int *global_arr_ptr = &global_arr[2];

int main(void) {
    // ========== CAST EXPRESSIONS (returns 1-19) ==========
    {
        // Cast between pointer types
        int x = 0x12345678;
        int *ip = &x;
        char *cp = (char *)ip;
        if (*cp != 0x78 && *cp != 0x12) return 1;  // endian-dependent, just check no crash

        // Cast pointer to integer and back (round-trip)
        int val = 99;
        int *p1 = &val;
        long addr = (long)p1;
        int *p2 = (int *)addr;
        if (*p2 != 99) return 2;

        // Cast to void (discard result)
        (void)42;
        (void)p1;

        // Cast between integer types (widening/narrowing)
        unsigned char uc = 200;
        int widened = (int)uc;
        if (widened != 200) return 3;

        int big = 300;
        unsigned char narrowed = (unsigned char)big;
        if (narrowed != 44) return 4;  // 300 % 256 = 44

        // Cast between float types
        double d = 3.14;
        float f = (float)d;
        if (f < 3.13f || f > 3.15f) return 5;

        float f2 = 1.5f;
        double d2 = (double)f2;
        if (d2 < 1.49 || d2 > 1.51) return 6;

        // Cast float ↔ int
        double d3 = 42.9;
        int truncated = (int)d3;
        if (truncated != 42) return 7;

        int i = 100;
        double d4 = (double)i;
        if (d4 < 99.9 || d4 > 100.1) return 8;

        // Cast NULL to different pointer types
        int *np1 = (int *)0;
        char *np2 = (char *)0;
        void *np3 = (void *)0;
        if (np1 != 0 || np2 != 0 || np3 != 0) return 9;

        // Pointer to void and back
        int arr[3] = {10, 20, 30};
        void *vp = (void *)arr;
        int *ip2 = (int *)vp;
        if (ip2[1] != 20) return 10;
    }

    // ========== CONSTANT EXPRESSIONS (returns 20-29) ==========
    {
        // Integer constant expressions (used in array sizes)
        int arr[2 + 3];  // size = 5
        arr[4] = 42;
        if (arr[4] != 42) return 20;

        // Arithmetic constant expression
        int arr2[10 * 2 / 5];  // size = 4
        arr2[3] = 99;
        if (arr2[3] != 99) return 21;

        // Address constants (global pointers initialized at compile time)
        if (*global_ptr != 42) return 22;
        if (*global_arr_ptr != 30) return 23;

        // Null pointer constant
        int *null_p = 0;
        if (null_p != (void *)0) return 24;
        int *null_p2 = (void *)0;
        if (null_p2 != 0) return 25;

        // Compile-time sizeof in constant expression
        char buf[sizeof(int) * 2];  // size = 8
        if (sizeof(buf) != 8) return 26;

        // Conditional constant expression
        int arr3[1 > 0 ? 10 : 5];  // size = 10
        arr3[9] = 77;
        if (arr3[9] != 77) return 27;
    }

    // ========== IMPLICIT CONVERSIONS (returns 30-49) ==========
    {
        // Integer promotions: char/short → int in arithmetic
        char c1 = 100, c2 = 100;
        int sum = c1 + c2;  // promoted to int before addition
        if (sum != 200) return 30;

        short s1 = 30000, s2 = 30000;
        int ssum = s1 + s2;  // promoted, no overflow
        if (ssum != 60000) return 31;

        // Usual arithmetic conversions: int + long → long
        int i = 2;
        long l = 3000000000L;
        long result = i + l;
        if (result != 3000000002L) return 32;

        // int + unsigned → unsigned
        unsigned int u = 4000000000U;
        int neg = -1;
        unsigned int uresult = u + neg;
        if (uresult != 3999999999U) return 33;

        // Default argument promotions (variadic): char → int
        char ca = 10, cb = 20;
        int promoted_sum = sum_promoted(2, ca, cb);
        if (promoted_sum != 30) return 34;

        // Default argument promotions (variadic): float → double
        float fa = 1.5f, fb = 2.5f;
        double promoted_fsum = sum_double_promoted(2, fa, fb);
        if (promoted_fsum < 3.9 || promoted_fsum > 4.1) return 35;

        // Array-to-pointer decay
        int arr[5] = {1, 2, 3, 4, 5};
        int *p = arr;  // array decays to pointer
        if (p[0] != 1 || p[4] != 5) return 36;

        // Array decay in function argument
        if (strlen("hello") != 5) return 37;  // "hello" decays to char*

        // Function-to-pointer decay
        int (*fp)(int, int) = add;  // function name decays to pointer
        if (fp(3, 4) != 7) return 38;

        // Function pointer via explicit address-of (same result)
        int (*fp2)(int, int) = &mul;
        if (fp2(5, 6) != 30) return 39;

        // Lvalue conversion (reading value from lvalue)
        int lv = 42;
        int rv = lv;  // lvalue-to-rvalue conversion
        if (rv != 42) return 40;

        // Lvalue conversion through pointer dereference
        int *plv = &lv;
        int rv2 = *plv;  // deref yields lvalue, then converted to rvalue
        if (rv2 != 42) return 41;
    }

    // ========== COMPOUND LITERALS (returns 50-59) ==========
    {
        // Array compound literal
        int *p = (int[]){10, 20, 30};
        if (p[0] != 10 || p[2] != 30) return 50;

        // Struct compound literal
        struct Point { int x; int y; };
        struct Point pt = (struct Point){5, 10};
        if (pt.x != 5 || pt.y != 10) return 51;

        // Compound literal as function argument (address-of)
        struct Point *pp = &(struct Point){100, 200};
        if (pp->x != 100 || pp->y != 200) return 52;

        // Compound literal is modifiable
        int *mp = (int[]){1, 2, 3};
        mp[1] = 99;
        if (mp[1] != 99) return 53;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("c99_expressions_mega", code, &[]), 0);
}

/// A struct rvalue used as a designator gets a real address.
///
/// `linearize_lvalue` fell through to evaluating the expression for anything
/// that is not a designator, and for a struct-returning call that yields the
/// *result local* -- the value, not its address. A consumer that loads or
/// stores folds the member offset in and reads the right bytes, which is why
/// `mk().scalar_member` always worked. A consumer that does arithmetic --
/// indexing an array member, taking an address -- dereferenced the value's own
/// bits and crashed, for every element type.
///
/// C17 6.5.2.3p5 gives the temporary lifetime to the end of the full
/// expression; the materialized local is function-scope, so that holds.
#[test]
fn c99_struct_rvalue_designators() {
    let code = r#"
struct A { int v[2]; };
struct D { double v[2]; };
struct I { int x; };
struct B { long pad; struct I inner; };
struct S { int a; long b; };

__attribute__((noinline)) static struct A mka(void) { struct A a = {{7, 9}}; return a; }
__attribute__((noinline)) static struct D mkd(void) { struct D d = {{1.5, 2.5}}; return d; }
__attribute__((noinline)) static struct B mkb(void) { struct B b = {1, {42}}; return b; }
__attribute__((noinline)) static struct S mks(void) { struct S s = {3, 4}; return s; }
__attribute__((noinline)) static int  take(const int *p) { return p[1]; }
__attribute__((noinline)) static long sum(struct S s) { return s.a + s.b; }

int main(void)
{
    /* Indexing an array member: the case that crashed. */
    if (mka().v[0] != 7) return 1;
    if (mka().v[1] != 9) return 2;
    if (mkd().v[1] != 2.5) return 3;

    /* Taking an address, and letting the member decay to a pointer. */
    if (*&mka().v[1] != 9) return 4;
    if (take(mka().v) != 9) return 5;

    /* A member at a non-zero offset: offset zero worked by accident. */
    if (mkb().inner.x != 42) return 6;

    /* Scalar members and whole-struct arguments, which already worked. */
    if (mks().a != 3 || mks().b != 4) return 7;
    if (sum(mks()) != 7) return 8;

    /* Assigned through a local first -- the control. */
    struct A a = mka();
    if (a.v[0] != 7 || a.v[1] != 9) return 9;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("c99_struct_rvalue_designators", code, &[]),
        0
    );
    assert_eq!(
        compile_and_run_optimized("c99_struct_rvalue_designators_opt", code),
        0
    );
}

// ============================================================================
// The integer promotions run before the usual arithmetic conversions
// ============================================================================

/// C17 6.3.1.8p1 performs the integer promotions on both operands *before*
/// comparing their ranks, so every sub-`int` operand pair yields `int` and the
/// arithmetic that follows is signed. Skipping them let two `unsigned char`s
/// reach the "either operand is unsigned" fallback and divide unsigned:
///
/// ```text
///   unsigned char a = 1, b = 2;
///   (a - b) / 2     gave 2147483647, where gcc gives 0
///   (a - b) >> 1    gave 2147483647, where gcc gives -1
///   (a - b) < 0     gave 0,          where gcc gives 1
/// ```
///
/// `_Bool` and plain `char` answered correctly only because the signedness
/// predicate wrongly called them signed, so the two defects cancelled; they are
/// covered here because fixing that predicate uncancels them. Each row must
/// equal the `int` answer, whatever the operand type.
#[test]
fn c99_integer_promotions_precede_the_usual_arithmetic_conversions() {
    let code = r#"
int main(void)
{
    /* ===== unsigned char: the operand type that was visibly wrong (1-9) ===== */
    {
        unsigned char a = 1, b = 2;
        if ((a - b) / 2 != 0) return 1;
        if ((a - b) >> 1 != -1) return 2;
        if (((a - b) < 0) != 1) return 3;
        if ((a - b) != -1) return 4;
        if ((a - b) % 2 != -1) return 5;
    }

    /* ===== unsigned short: same fallback, same defect (10-19) ===== */
    {
        unsigned short a = 1, b = 2;
        if ((a - b) / 2 != 0) return 10;
        if ((a - b) >> 1 != -1) return 11;
        if (((a - b) < 0) != 1) return 12;
        if ((a - b) != -1) return 13;
    }

    /* ===== _Bool: correct today only by the cancelling defect (20-29) ===== */
    {
        _Bool a = 0, b = 1;
        if ((a - b) / 2 != 0) return 20;
        if ((a - b) >> 1 != -1) return 21;
        if (((a - b) < 0) != 1) return 22;
        if ((a - b) != -1) return 23;
    }

    /* ===== plain char: likewise (30-39) ===== */
    {
        char a = 1, b = 2;
        if ((a - b) / 2 != 0) return 30;
        if ((a - b) >> 1 != -1) return 31;
        if (((a - b) < 0) != 1) return 32;
        if ((a - b) != -1) return 33;
    }

    /* ===== signed char and short: the controls that always worked (40-49) ===== */
    {
        signed char a = 1, b = 2;
        if ((a - b) / 2 != 0) return 40;
        if (((a - b) < 0) != 1) return 41;
        short c = 1, d = 2;
        if ((c - d) / 2 != 0) return 42;
        if (((c - d) < 0) != 1) return 43;
    }

    /* ===== a genuinely unsigned result must stay unsigned (50-59) ===== */
    {
        unsigned int a = 1, b = 2;
        if ((a - b) / 2 != 2147483647u) return 50;
        if ((a - b) < 0) return 51;
        unsigned char c = 1;
        unsigned int d = 2;
        if ((c - d) / 2 != 2147483647u) return 52;   /* uchar promotes, uint wins */
        unsigned long e = 2;
        if ((c - e) / 2 == 0) return 53;             /* ulong wins */
    }

    /* ===== promotion applies to the unary operators too (60-69) ===== */
    {
        unsigned char a = 1;
        if (-a != -1) return 60;
        if (~a != -2) return 61;
        unsigned short b = 1;
        if (-b != -1) return 62;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("c99_integer_promotions", code, &[]), 0);
    assert_eq!(
        compile_and_run_optimized("c99_integer_promotions_opt", code),
        0
    );
}

/// Constant folding of floating comparisons and unsigned comparisons (#C114).
///
/// Two wrong answers from one place: the parser's constant folder, which
/// evaluates array sizes, enumerators, `case` labels and `_Static_assert`.
///
/// It truncated a floating literal to `i128` before folding, so `1.5 > 1.0`
/// became `1 > 1` and was **false** -- `enum E { X = 1.5 > 1.0 }` was 0 and
/// `int a[1.5 > 1.0 ? 4 : 8]` took the wrong branch. And it compared signed
/// whatever the operand types, so `(unsigned)-1 > 0` was **false** too.
/// Neither is exotic; both silently produce a different program.
///
/// The runtime rows are the control: the same expressions were always right
/// when they reached code generation, which is what made the folder's
/// disagreement invisible.
#[test]
fn c99_constant_folding_of_float_and_unsigned_comparisons() {
    let code = r#"
/* Floating comparisons, in every context the parser folds. */
enum FloatCmp { GT = 1.5 > 1.0, LT = 1.0 < 1.5, EQ = 2.0 == 2.0,
                NE = 2.0 != 3.0, GE = 1.0 >= 1.0, LE = 2.0 <= 1.0,
                MIXED = 1 < 1.5, LD = 1.5L > 1.0L, DIVCMP = 1.0/4.0 < 0.5 };
int chosen[1.5 > 1.0 ? 4 : 8];

/* A cast from floating arithmetic truncates the floating value, so the
   arithmetic under it has to be done in floating point. */
enum FloatCast { SUM = (int)(1.5 + 1.5), DIV = (int)(7.0/2.0),
                 NEG = (int)(-3.7), LIT = (int)3.9 };
int scaled[(int)(2.5 * 2)];

/* Unsigned comparisons; C promotes to the common type before comparing. */
enum UnsignedCmp { UMAX = (unsigned)-1 > 0, ULMAX = 0xFFFFFFFFFFFFFFFFULL > 1,
                   UEQ = 1u == 1, SNEG = -1 > 0, SLE = -1 <= 0 };
int usized[((unsigned)-1 > 0) ? 4 : 8];

_Static_assert(1.5 > 1.0, "floating comparison folds");
_Static_assert(1.5 + 1.5 == 3.0, "floating arithmetic folds");
_Static_assert((unsigned)-1 > 0, "unsigned comparison folds");

int main(void) {
    if (GT != 1 || LT != 1 || EQ != 1 || NE != 1) return 1;
    if (GE != 1 || LE != 0) return 2;
    if (MIXED != 1 || LD != 1 || DIVCMP != 1) return 3;
    if (sizeof chosen / sizeof chosen[0] != 4) return 4;

    if (SUM != 3 || DIV != 3 || NEG != -3 || LIT != 3) return 5;
    if (sizeof scaled / sizeof scaled[0] != 5) return 6;

    if (UMAX != 1 || ULMAX != 1 || UEQ != 1) return 7;
    if (SNEG != 0 || SLE != 1) return 8;
    if (sizeof usized / sizeof usized[0] != 4) return 9;

    /* The control: at run time these were always right, which is why the
       folder disagreeing with them went unnoticed. */
    double a = 1.5, b = 1.0;
    unsigned u = (unsigned)-1;
    if (!(a > b)) return 10;
    if (!(u > 0)) return 11;
    if ((int)(a + a) != 3) return 12;

    return 0;
}
"#;
    assert_eq!(compile_and_run("const_fold_float_unsigned", code, &[]), 0);
}

// ============================================================================
// The folder converts to the common type before operating (6.3.1.8)
// ============================================================================

/// A folded comparison, division and remainder take their signedness from the
/// *common type*, not from "either operand is unsigned" and not from the left
/// operand.
///
/// Both shortcuts get the integer promotions wrong. `(unsigned char)200 > -1`
/// is a signed comparison because both operands promote to `int` (6.3.1.1p2),
/// and `-1L < 1u` is signed on LP64 because `long` represents every
/// `unsigned int`; asking whether *either* operand was unsigned made both
/// false. `-1 / 2u` is unsigned because the common type is `unsigned int`;
/// asking the left operand alone made it signed and folded 0 where the answer
/// is 2147483647.
///
/// Every one of these is a constant expression, so a wrong fold is a wrong
/// program, not a wrong diagnostic.
#[test]
fn c99_const_fold_uses_the_common_type() {
    let code = r#"
/* Promotions make these signed, and true. */
_Static_assert(-1L < 1u, "long vs unsigned int is signed on LP64");
_Static_assert((unsigned char)200 > -1, "unsigned char promotes to int");
_Static_assert((unsigned short)200 > -1, "unsigned short promotes to int");

/* And these unsigned, and true: no promotion changes the signedness. */
_Static_assert((unsigned)-1 > 0, "unsigned int stays unsigned");
_Static_assert(-1 > 1u, "int vs unsigned int is unsigned");

/* Naming the common type is only half of it -- C *converts* both operands to
   it. Comparing the raw values as u128 made this true, because an i128 -1 is
   2^128-1 rather than the 4294967295 that -1 converts to. */
_Static_assert(!(-1 > 4294967295u), "the left operand converts, not widens");
_Static_assert(-1 == 4294967295u, "and converts to exactly that");

int main(void)
{
    enum { UDIV = -1 / 2u };        /* common type unsigned: 2147483647 */
    enum { UMOD = -1 % 2u };        /* common type unsigned: 1 */
    enum { SDIV = (unsigned char)200 / -1 };  /* both promote to int: -200 */
    enum { SMOD = (unsigned char)200 % -1 };  /* -> 0 */
    enum { LDIV = -1L / 2u };       /* common type long, signed: 0 */

    if (UDIV != 2147483647) return 1;
    if (UMOD != 1) return 2;
    if (SDIV != -200) return 3;
    if (SMOD != 0) return 4;
    if (LDIV != 0) return 5;

    /* A shift follows the LEFT operand alone (6.5.7p3): the operands are
       promoted separately and the result is the left operand's type. Making
       the shift use the common type would break this. */
    enum { SHR = -8 >> 1u };        /* arithmetic: -4, not a huge unsigned */
    if (SHR != -4) return 6;
    if (sizeof(1 << 1L) != sizeof(int)) return 7;

    /* The control: at run time these were always right. */
    long l = -1; unsigned u = 1;
    unsigned char uc = 200; int m1 = -1;
    if (!(l < u)) return 8;
    if (!(uc > m1)) return 9;
    if (m1 / 2u != 2147483647u) return 10;

    return 0;
}
"#;
    assert_eq!(compile_and_run("const_fold_common_type", code, &[]), 0);
    assert_eq!(
        compile_and_run_optimized("const_fold_common_type_opt", code),
        0
    );
}

/// `sizeof` a variably-length array is not an integer constant expression.
///
/// `SizeofType` grew a runtime guard; `SizeofExpr` did not, so `sizeof a` for
/// `int a[n]` folded to 0 -- `size_bits` reports 0 for an array with no extent
/// -- and `case sizeof a:` was accepted with the wrong value, matching
/// `switch (0)`. A `TypeId` for `int[n]` is indistinguishable from `int[]`, so
/// the question has to be asked of the array levels.
#[test]
fn c99_sizeof_a_vla_is_not_a_constant_expression() {
    // `case sizeof a:` must be rejected, not silently folded to 0.
    let rejected = r#"
int g(int n)
{
    int a[n];
    switch (n) {
    case sizeof a: return 1;
    default: return 0;
    }
}
int main(void) { return g(0); }
"#;
    crate::common::compile_expect_error("sizeof_vla_case_label", rejected, "constant");

    // `sizeof` a *fixed* array is still a constant expression.
    let accepted = r#"
int main(void)
{
    int a[4];
    switch (sizeof a) {
    case sizeof a: return 0;
    default: return 1;
    }
}
"#;
    assert_eq!(compile_and_run("sizeof_fixed_array_case", accepted, &[]), 0);
}

/// A floating comparison in a constant expression reaches the integer folder
/// for its integer-valued operands.
///
/// `eval_const_f64` had arms only for literals, casts, negation and the four
/// arithmetic operators, so `_Static_assert(sizeof(int) < 4.5, "")` was
/// rejected as "not a constant expression" although both operands are
/// constant. And `CharLit` -- an `i64` whose signedness the lexer has already
/// resolved -- was read through `u32`, so `'\x80'` folded to 4294967168.0
/// instead of -128.0 where `char` is signed.
#[test]
fn c99_float_comparison_folds_integer_operands() {
    let code = r#"
_Static_assert(sizeof(int) < 4.5, "sizeof reaches the float comparison");
_Static_assert(sizeof(int) > 3.5, "and compares as a number, not a truncation");
_Static_assert(_Alignof(int) < 4.5, "_Alignof too");
_Static_assert((1 ? 2 : 3) < 2.5, "a conditional too");

int main(void)
{
    /* `char` is signed on both targets c17 supports, so '\x80' is -128. */
    if ('\x80' >= 0) return 1;          /* the integer folder always knew */
    return 0;
}
"#;
    assert_eq!(compile_and_run("float_cmp_integer_operands", code, &[]), 0);
}
