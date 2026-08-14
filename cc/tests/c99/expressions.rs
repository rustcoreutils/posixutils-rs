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
