//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C99 Features Mega-Test
//
// Consolidates: VLA, inline, varargs, array_param_qualifiers tests
//

use crate::common::{compile_and_run, compile_and_run_everywhere};

// ============================================================================
// Mega-test: C99 features (VLA, inline, varargs, array params)
// ============================================================================

#[test]
fn c99_features_mega() {
    let code = r#"
#include <stdarg.h>

// Inline function
static inline int add_inline(int a, int b) {
    return a + b;
}

static inline int square_inline(int x) {
    return x * x;
}

// VLA helper functions
int test_vla_basic(int n) {
    int arr[n];
    arr[0] = 1;
    arr[n-1] = 2;
    return arr[0] + arr[n-1];
}

int test_vla_computed(int n) {
    int arr[n];
    for (int i = 0; i < n; i++) {
        arr[i] = i * 10;
    }
    int sum = 0;
    for (int i = 0; i < n; i++) {
        sum += arr[i];
    }
    return sum;
}

int test_vla_sizeof(int n) {
    int arr[n];
    return sizeof(arr);
}

// VLA function parameter syntax
void fill_vla(int n, int arr[n]) {
    for (int i = 0; i < n; i++) arr[i] = i * 2;
}

int sum_vla(int n, int arr[n]) {
    int sum = 0;
    for (int i = 0; i < n; i++) sum += arr[i];
    return sum;
}

// Array parameter qualifiers
void read_array_const(int n, const int arr[n]) {
    // arr is read-only
    int sum = 0;
    for (int i = 0; i < n; i++) sum += arr[i];
}

void write_array_restrict(int n, int arr[restrict n]) {
    for (int i = 0; i < n; i++) arr[i] = i;
}

void process_static(int arr[static 5]) {
    // arr is guaranteed to have at least 5 elements
    for (int i = 0; i < 5; i++) arr[i] *= 2;
}

// Varargs functions
int sum_varargs(int count, ...) {
    va_list args;
    va_start(args, count);
    int sum = 0;
    for (int i = 0; i < count; i++) {
        sum += va_arg(args, int);
    }
    va_end(args);
    return sum;
}

int max_varargs(int count, ...) {
    va_list args;
    va_start(args, count);
    int max = va_arg(args, int);
    for (int i = 1; i < count; i++) {
        int val = va_arg(args, int);
        if (val > max) max = val;
    }
    va_end(args);
    return max;
}

// va_copy test
int sum_twice(int count, ...) {
    va_list args, args_copy;
    va_start(args, count);
    va_copy(args_copy, args);

    int sum1 = 0;
    for (int i = 0; i < count; i++) {
        sum1 += va_arg(args, int);
    }

    int sum2 = 0;
    for (int i = 0; i < count; i++) {
        sum2 += va_arg(args_copy, int);
    }

    va_end(args);
    va_end(args_copy);
    return sum1 + sum2;
}

// va_arg with string pointers (tests 64-bit loads)
// This test ensures va_arg correctly loads 64-bit pointers, not 32-bit
#include <string.h>

static int process_strings(va_list *p_va, int count) {
    int total_len = 0;
    for (int i = 0; i < count; i++) {
        const char *str = va_arg(*p_va, const char *);
        if (str == (const char*)0) return -1;
        total_len += strlen(str);
    }
    return total_len;
}

int test_va_arg_strings(int count, ...) {
    va_list va;
    va_start(va, count);
    int result = process_strings(&va, count);
    va_end(va);
    return result;
}

// va_list cast to pointer test (C99 6.3.2.1 - array decay)
// va_list is defined as __va_list_tag[1] and should decay to a pointer
int test_va_cast(int count, ...) {
    va_list args;
    va_start(args, count);
    
    // Cast va_list to pointer - this tests array decay of va_list
    unsigned char* ptr = (unsigned char*)args;
    
    // Verify we got a valid pointer (non-null)
    if (ptr == (unsigned char*)0) {
        va_end(args);
        return -1;
    }
    
    // Read a few bytes to verify memory access works
    unsigned char first_byte = ptr[0];
    (void)first_byte;  // Suppress unused warning
    
    // Now consume the arguments normally to verify va_list still works
    int sum = 0;
    for (int i = 0; i < count; i++) {
        sum += va_arg(args, int);
    }
    
    va_end(args);
    return sum;
}

int main(void) {
    // ========== INLINE SECTION (returns 1-9) ==========
    {
        // Basic inline function
        if (add_inline(10, 32) != 42) return 1;
        if (add_inline(0, 0) != 0) return 2;
        if (add_inline(-5, 47) != 42) return 3;

        // Another inline function
        if (square_inline(6) != 36) return 4;
        if (square_inline(0) != 0) return 5;

        // Inline in expression
        if (add_inline(square_inline(3), 33) != 42) return 6;  // 9 + 33
    }

    // ========== VLA SECTION (returns 10-39) ==========
    {
        // Basic VLA
        if (test_vla_basic(5) != 3) return 10;
        if (test_vla_basic(10) != 3) return 11;

        // Computed VLA
        if (test_vla_computed(5) != 100) return 12;   // 0+10+20+30+40
        if (test_vla_computed(10) != 450) return 13;  // 0+10+...+90

        // VLA sizeof
        if (test_vla_sizeof(5) != 20) return 14;   // 5 * 4
        if (test_vla_sizeof(10) != 40) return 15;  // 10 * 4

        // VLA in nested scope
        int n = 5;
        int total = 0;
        {
            int arr[n];
            for (int i = 0; i < n; i++) arr[i] = i * 2;
            for (int i = 0; i < n; i++) total += arr[i];
        }
        if (total != 20) return 16;  // 0+2+4+6+8

        // VLA function parameter
        int arr[5];
        fill_vla(5, arr);
        if (sum_vla(5, arr) != 20) return 17;  // 0+2+4+6+8

        // VLA with expression size
        int a = 3, b = 4;
        int arr2[a + b];  // size 7
        for (int i = 0; i < 7; i++) arr2[i] = i;
        int sum = 0;
        for (int i = 0; i < 7; i++) sum += arr2[i];
        if (sum != 21) return 18;  // 0+1+2+3+4+5+6

        // 2D VLA
        int rows = 3, cols = 4;
        int matrix[rows][cols];
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                matrix[i][j] = i * 10 + j;
            }
        }
        if (matrix[0][0] != 0) return 19;
        if (matrix[2][3] != 23) return 20;
        if (sizeof(matrix) != 48) return 21;  // 3*4*4

        // Different VLA element types
        char carr[n];
        if (sizeof(carr) != 5) return 22;

        short sarr[n];
        if (sizeof(sarr) != 10) return 23;

        long larr[n];
        if (sizeof(larr) != 40) return 24;
    }

    // ========== ARRAY PARAMETER QUALIFIERS (returns 40-59) ==========
    {
        // const array parameter
        int arr[5] = {1, 2, 3, 4, 5};
        read_array_const(5, arr);
        // arr should be unchanged
        if (arr[0] != 1) return 40;
        if (arr[4] != 5) return 41;

        // restrict array parameter
        int arr2[5];
        write_array_restrict(5, arr2);
        if (arr2[0] != 0) return 42;
        if (arr2[4] != 4) return 43;

        // static array size
        int arr3[10] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
        process_static(arr3);
        if (arr3[0] != 2) return 44;   // 1 * 2
        if (arr3[4] != 10) return 45;  // 5 * 2
        if (arr3[5] != 6) return 46;   // unchanged
    }

    // ========== VARARGS SECTION (returns 60-79) ==========
    {
        // Basic varargs sum
        if (sum_varargs(3, 10, 20, 12) != 42) return 60;
        if (sum_varargs(5, 1, 2, 3, 4, 5) != 15) return 61;
        if (sum_varargs(1, 42) != 42) return 62;

        // Max varargs
        if (max_varargs(5, 10, 42, 5, 30, 20) != 42) return 63;
        if (max_varargs(3, -5, -1, -10) != -1) return 64;
        if (max_varargs(1, 100) != 100) return 65;

        // va_copy
        if (sum_twice(3, 10, 20, 12) != 84) return 66;  // 42 + 42

        // Varargs with different counts
        if (sum_varargs(0) != 0) return 67;  // No args
        if (sum_varargs(10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10) != 55) return 68;

        // va_list cast to pointer (tests array decay)
        if (test_va_cast(3, 10, 20, 12) != 42) return 69;
        if (test_va_cast(1, 42) != 42) return 70;

        // va_arg with string pointers (tests 64-bit loads and stack spill)
        // Tests: 1) va_arg correctly loads 64-bit pointers
        //        2) va_list passed through pointer works correctly
        //        3) stack spill offset is calculated correctly
        if (test_va_arg_strings(1, "hello") != 5) return 71;
        if (test_va_arg_strings(2, "hi", "there") != 7) return 72;  // 2 + 5
        if (test_va_arg_strings(4, "a", "bb", "ccc", "dddd") != 10) return 73;  // 1+2+3+4
    }

    // ========== C99 FOR LOOP DECLARATIONS (returns 80-89) ==========
    {
        // for loop with declaration
        int sum = 0;
        for (int i = 0; i < 5; i++) {
            sum += i;
        }
        if (sum != 10) return 80;  // 0+1+2+3+4

        // Multiple declarations in for
        int result = 0;
        for (int i = 0, j = 10; i < 5; i++, j--) {
            result += i * j;
        }
        // i=0,j=10: 0; i=1,j=9: 9; i=2,j=8: 16; i=3,j=7: 21; i=4,j=6: 24
        // Total: 0+9+16+21+24 = 70
        if (result != 70) return 81;

        // Nested for with separate scopes
        sum = 0;
        for (int i = 0; i < 3; i++) {
            for (int i = 0; i < 2; i++) {  // Inner i shadows outer
                sum++;
            }
        }
        if (sum != 6) return 82;  // 3 * 2
    }

    // ========== MIXED DECLARATIONS AND CODE (returns 90-99) ==========
    {
        int x = 10;
        x++;
        int y = 20;  // Declaration after statement (C99)
        if (x + y != 31) return 90;

        for (int i = 0; i < 3; i++) {
            int temp = i * 2;  // Declaration in block
            x += temp;
        }
        // x = 11 + 0 + 2 + 4 = 17
        if (x != 17) return 91;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("c99_features_mega", code, &[]), 0);
}

/// One program, one section per original test; each section carries the
/// original doc comment and its exit-code range in its header. Consolidates:
/// `scope_shadowing_deep`, `hex_float_long_significand`,
/// `param_shadows_typedef`, `extreme_float_literals`,
/// `compound_literal_postfix`, `array_from_compound_literal`,
/// `nested_case_labels`, `label_at_block_end`, `static_address_constants`,
/// `deep_nesting`.
#[test]
fn c99_features_misc_mega() {
    let code = r#"
#include <float.h>

// ==========================================================================
// scope_shadowing_deep  (exit codes 1-21: 0 + its own code)
//
// (was #[test] c99_scope_shadowing_deep)
//
// ==========================================================================
static int t_scope_shadowing_deep(void) {
    int x = 1;
    {
        int x = 2;
        {
            int x = 3;
            {
                int x = 4;
                {
                    int x = 5;
                    if (x != 5) return 1;
                }
                if (x != 4) return 2;
            }
            if (x != 3) return 3;
        }
        if (x != 2) return 4;
    }
    if (x != 1) return 5;

    // For-loop scoping with nested for-loop and x shadowing
    int sum = 0;
    for (int i = 0; i < 3; i++) {
        int x = i * 10;
        sum += x;
        for (int i = 0; i < 2; i++) {
            int x = i + 100;
            sum += x;
        }
    }
    // outer x: 0+10+20 = 30
    // inner x: 3*(100+101) = 603
    // total: 633
    if (sum != 633) return 10;

    // Shadowing in for-init
    {
        int val = 99;
        for (int val = 0; val < 1; val++) {
            if (val != 0) return 20;
        }
        if (val != 99) return 21;
    }

    return 0;
}

// ==========================================================================
// hex_float_long_significand  (exit codes 22-31: 21 + its own code)
//
// (was #[test] c99_hex_float_long_significand)
//
// C99 6.4.4.2 hex floats name a binary value directly, so a long significand
// must survive the parse intact.
//
// The significand was accumulated in a `u64` and divided by
// `1u64 << (4 * digits)`. Sixteen fraction digits make that a shift of 64,
// which wraps to a shift of 0 in a release build, so
// `0x1.0000000000000002p+0` evaluated to **3.0** -- wrong by a factor of
// three, with no diagnostic, in a feature the conformance matrix listed as
// passing. Values are checked against arithmetic that cannot use the same
// parser, so a repeat of the bug cannot satisfy both sides.
// ==========================================================================
static int t_hex_float_long_significand(void) {
    /* Just above 1.0: the case that used to yield 3.0. */
    double a = 0x1.0000000000000002p+0;
    if (a < 1.0 || a > 1.0000000001) return 1;

    /* Exactly representable, so the comparison is not a near-miss. */
    if (0x1.8p+0 != 1.5) return 2;
    if (0x2p+3 != 16.0) return 3;
    if (0x.8p+1 != 1.0) return 4;
    if (0x8.p-3 != 1.0) return 5;

    /* Wider than the mantissa: 2^124 - 1 rounds to 2^124. */
    double wide = 0xfffffffffffffffffffffffffffffffp0;
    double two124 = 1.0;
    for (int i = 0; i < 124; i++) two124 *= 2.0;
    if (wide != two124) return 6;

    /* The extremes of the exponent range, reached without passing through
       an intermediate infinity or zero. */
    if (0x1p-1074 <= 0.0) return 7;          /* smallest subnormal */
    if (0x1p-1080 != 0.0) return 8;          /* underflows */
    if (0x1p+2000 <= DBL_MAX) return 9;      /* overflows to infinity */

    /* A 16-digit fraction at float precision too. */
    float f = 0x1.000002p+0f;
    if (f <= 1.0f) return 10;

    return 0;
}

// ==========================================================================
// param_shadows_typedef  (exit codes 32-37: 31 + its own code)
//
// (was #[test] c99_parameter_shadows_a_file_scope_typedef)
//
// A parameter must shadow a file-scope typedef of the same name
// (C17 6.2.1p4), so `(name)` inside the function is a parenthesized
// expression and not a type name.
//
// The parameter symbol has to be registered as the *innermost* binding.
// Registering it as the outermost instead left the typedef winning, and
// `((PyObject*)((string)))` in CPython then failed to parse -- the compiler
// read `(string)` as a type name and wanted a cast operand after it.
// ==========================================================================
typedef int *pst_string;
typedef long pst_counter;

static int pst_deref(int *pst_string) {
    /* `string` is the parameter here, so this is a cast of an expression. */
    return *((int *)((pst_string)));
}

static long pst_total(long pst_counter, long n) {
    long s = 0;
    for (long i = 0; i < n; i++)
        s += (pst_counter);
    return s;
}

/* The typedef is visible again once the parameter is out of scope. */
static pst_string pst_pick(pst_string a, pst_string b, int which) {
    return which ? a : b;
}

static int t_param_shadows_typedef(void) {
    int v = 42;
    if (pst_deref(&v) != 42) return 1;
    if (pst_total(3, 4) != 12) return 2;

    int x = 7, y = 9;
    pst_string p = &x, q = &y;
    if (*pst_pick(p, q, 1) != 7) return 3;
    if (*pst_pick(p, q, 0) != 9) return 4;

    /* At file scope the typedef still names a type. */
    pst_string r = &x;
    if (*r != 7) return 5;
    pst_counter c = 5;
    if (c != 5) return 6;

    return 0;
}

// ==========================================================================
// extreme_float_literals  (exit codes 38-47: 37 + its own code)
//
// (was #[test] c99_extreme_float_literals)
//
// A zero significand is zero at any exponent, and a subnormal literal is
// rounded to nearest rather than truncated.
//
// The decimal converter short-circuits an exponent far outside every target
// format, on the reasoning that the value can only be an infinity or a zero.
// That reasoning holds only for a non-zero significand: `0e6000` came out as
// an infinity. Separately, the subnormal encoding path shifted the
// significand down and dropped what fell off, which is directed rounding
// toward zero where every other path rounds to nearest.
// ==========================================================================
double efl_zero_big = 0e6000;
double efl_zero_small = 0.0e-9999;
long double efl_zero_ld = 0.000e5001;

static int t_extreme_float_literals(void)
{
    if (efl_zero_big != 0.0) return 1;
    if (efl_zero_small != 0.0) return 2;
    if (efl_zero_ld != 0.0L) return 3;
    if (0e6000 != 0.0) return 4;

    /* A non-zero significand still saturates as before. */
    if (1e6000 <= DBL_MAX) return 5;
    if (1e-6000 != 0.0) return 6;

    /* The smallest subnormal double, and one that must round up to it
       rather than truncate to zero. */
    if (4.9406564584124654e-324 == 0.0) return 7;
    if (3e-324 == 0.0) return 8;

#if LDBL_MIN_EXP < DBL_MIN_EXP
    /* Subnormal long doubles survive at all, and stay ordered. Only where
       long double has range of its own: on a target whose long double *is*
       double -- Apple's arm64 among them -- these underflow to zero, which
       is the right answer there and not what this is testing. */
    long double a = 3.6451995318824746025e-4951L;
    long double b = 7.2903990637649492050e-4951L;
    if (a == 0.0L) return 9;
    if (!(a < b)) return 10;
#endif

    return 0;
}

// ==========================================================================
// compound_literal_postfix  (exit codes 48-50: 47 + its own code)
//
// (was #[test] c99_compound_literal_is_a_postfix_expression)
// Under the heading: A compound literal is a postfix expression (C99 6.5.2.5)
//
// `sizeof (struct s){1, 2}` is `sizeof` of a *literal*, not of the type.
//
// `parse_sizeof` and `parse_alignof` each consumed the `(`, committed to the
// type name, and never looked for the `{` -- so the braces were left for
// whatever was parsing the enclosing construct, and the error named that
// instead. Recognition now lives in one place that every production reaches.
// ==========================================================================
struct clp_s { int a; int b; };
char clp_x[((sizeof (struct clp_s){ 1, 2 }) == sizeof (struct clp_s)) ? 1 : -1];
char clp_y[(_Alignof (struct clp_s){ 1, 2 }) == _Alignof (struct clp_s) ? 1 : -1];
char clp_z[sizeof (int[3]){ 1, 2, 3 } == 3 * sizeof (int) ? 1 : -1];

static int t_compound_literal_postfix(void) {
    /* The postfix suffixes apply to the literal, as to any postfix
       expression. */
    if ((int[3]){ 7, 8, 9 }[1] != 8) return 1;
    if (sizeof (int[3]){ 1, 2, 3 }[0] != sizeof(int)) return 2;
    if ((struct clp_s){ 4, 5 }.b != 5) return 3;
    (void)clp_x; (void)clp_y; (void)clp_z;
    return 0;
}

// ==========================================================================
// array_from_compound_literal  (exit codes 51-52: 50 + its own code)
//
// (was #[test] c99_array_initialized_from_a_compound_literal)
//
// An array may be initialized from a compound literal of its own type. The
// lowering already handled it; only the initializer check stood in the way,
// having been written when a string literal was the one non-braced
// initializer an array could take.
// ==========================================================================
static const unsigned short afc_array[] = (const unsigned short []){ 0x0D2B, 0x0D2C };

static int t_array_from_compound_literal(void) {
    if (afc_array[0] != 0x0D2B || afc_array[1] != 0x0D2C) return 1;
    if (sizeof afc_array != 2 * sizeof(unsigned short)) return 2;
    return 0;
}

// ==========================================================================
// nested_case_labels  (exit codes 53-62: 52 + its own code)
//
// (was #[test] c99_case_labels_inside_a_nested_statement)
// Under the heading: A labeled statement is one statement (C17 6.8.1)
//
// `case` and `default` carry the statement they label.
//
// Held as flat sibling markers they worked inside a compound statement and
// nowhere else: in `switch (c) case 1: if (d) case 2: case 3: f();` the `if`
// took the bare `case 2:` as its whole then-branch, and `case 3: f();` fell
// out of the switch entirely -- reported as "case label not within a switch
// statement".
// ==========================================================================
int ncl_duff(int c, int d) {
    int t = 0;
    switch (c)
        case 1:
            if (d)
                case 2:
                case 3:
                    t = 10;
    return t;
}

/* Duff's device: labels inside a loop inside the switch. */
int ncl_copy(int *dst, const int *src, int n) {
    int moved = 0;
    int count = (n + 3) / 4;
    switch (n % 4) {
    case 0: do { *dst++ = *src++; moved++;
    case 3:      *dst++ = *src++; moved++;
    case 2:      *dst++ = *src++; moved++;
    case 1:      *dst++ = *src++; moved++;
            } while (--count > 0);
    }
    return moved;
}

static int t_nested_case_labels(void) {
    if (ncl_duff(1, 1) != 10) return 1;
    if (ncl_duff(1, 0) != 0) return 2;
    if (ncl_duff(2, 0) != 10) return 3;
    if (ncl_duff(3, 0) != 10) return 4;
    if (ncl_duff(9, 0) != 0) return 5;

    {
        int src[7] = { 1, 2, 3, 4, 5, 6, 7 }, dst[8] = { 0 };
        if (ncl_copy(dst, src, 7) != 7) return 6;
        if (dst[0] != 1 || dst[6] != 7) return 7;
    }

    /* Ordinary fall-through must be unchanged. */
    {
        int t = 0, c = 1;
        switch (c) { case 1: t = 1; case 2: t += 2; break; default: t = 99; }
        if (t != 3) return 8;
    }
    /* A declaration after a label still belongs to the enclosing block. */
    {
        int c = 1;
        switch (c) { case 1: ; int v = 5; if (v != 5) return 9; v++; if (v != 6) return 10; }
    }
    return 0;
}

// ==========================================================================
// label_at_block_end  (exit codes 63-65: 62 + its own code)
//
// (was #[test] c99_label_at_the_end_of_a_block)
//
// A label at the end of a compound statement. C17 requires a statement after
// it; gcc and clang accept it without, and C23 made it legal, so c17 accepts
// it with a warning rather than failing on the `}`.
// ==========================================================================
int lbe_g;
int lbe_f(int n) { if (n) goto done; lbe_g = 1; done: }
int lbe_h(int n) { switch (n) { case 1: lbe_g = 2; break; default: } return lbe_g; }

static int t_label_at_block_end(void) {
    lbe_f(0);
    if (lbe_g != 1) return 1;
    lbe_f(1);
    if (lbe_g != 1) return 2;
    if (lbe_h(1) != 2) return 3;
    return 0;
}

// ==========================================================================
// static_address_constants  (exit codes 66-71: 65 + its own code)
//
// (was #[test] c99_address_constants_in_static_initializers)
// Under the heading: Address constants in a static initializer (C17 6.6p9)
//
// A relocation with an addend is a constant expression even where the type
// system sees plain integer arithmetic.
//
// The initializer folder asked which operand had a *pointer type* rather than
// which named a symbol, so `(unsigned long)&_text - 0x10000000L - 1` -- how a
// kernel or a linker script's C half is written -- was "not a constant
// expression". Two smaller holes went with it: identical string literals were
// two objects, so their difference was a difference between different
// symbols; and a `void *` difference was discarded for having no element
// size, although C counts it in bytes.
// ==========================================================================
int sac_literal_diff = (&"Foobar"[1] - &"Foobar"[0]);
struct sac_s { char p[2]; };
static struct sac_s sac_v;
const int sac_o0 = (int)((void *)&sac_v.p[0] - (void *)&sac_v) + 0U;
const int sac_o1 = (int)((void *)&sac_v.p[1] - (void *)&sac_v) + 1U;
int sac_x[60];
char *sac_y = ((char *)&(sac_x[2 * 8 + 2]) - 8);
static unsigned long sac_addend = (unsigned long)&sac_x - 0x1000L - 1;

static int t_static_address_constants(void) {
    if (sac_literal_diff != 1) return 1;
    if (sac_o0 != 0) return 2;
    if (sac_o1 != 2) return 3;
    if (sac_y != (char *)&sac_x[18] - 8) return 4;
    if (sac_addend != (unsigned long)&sac_x - 0x1001L) return 5;
    /* Two spellings of one literal are one object. */
    if ("Foobar" != "Foobar") return 6;
    return 0;
}

// ==========================================================================
// deep_nesting  (exit codes 72-72: 71 + its own code)
//
// (was #[test] c99_deeply_nested_constructs_compile)
// Under the heading: Translation limits
// The deeply nested `dn_deep` and the 600 case labels of `dn_labels` are
// generated by `deep_nesting_src()` and spliced in at DEEP_NESTING.
//
// The front end descends recursively through the source, so a deeply nested
// expression or a long run of `case` labels costs stack. Running out of it
// was a Rust panic about a stack overflow rather than anything a user could
// act on; the compile now runs on a thread whose stack is ours to choose.
//
// C17 5.2.4.1 asks for 63 levels of each. These go well past that, because
// generated source does.
// ==========================================================================
/* DEEP_NESTING */
static int t_deep_nesting(void) { return dn_deep() == 1 && dn_labels(3) == 1 ? 0 : 1; }

int main(void) {
    int r;
    if ((r = t_scope_shadowing_deep()) != 0) return 0 + r;
    if ((r = t_hex_float_long_significand()) != 0) return 21 + r;
    if ((r = t_param_shadows_typedef()) != 0) return 31 + r;
    if ((r = t_extreme_float_literals()) != 0) return 37 + r;
    if ((r = t_compound_literal_postfix()) != 0) return 47 + r;
    if ((r = t_array_from_compound_literal()) != 0) return 50 + r;
    if ((r = t_nested_case_labels()) != 0) return 52 + r;
    if ((r = t_label_at_block_end()) != 0) return 62 + r;
    if ((r = t_static_address_constants()) != 0) return 65 + r;
    if ((r = t_deep_nesting()) != 0) return 71 + r;
    return 0;
}
"#
    .replace("/* DEEP_NESTING */", &deep_nesting_src());
    assert_eq!(compile_and_run("c99_features_misc_mega", &code, &[]), 0);
}

/// The `deep_nesting` section's functions (was `c99_deeply_nested_constructs_compile`):
/// a 400-deep parenthesized expression in `dn_deep` and 600 `case` labels in
/// `dn_labels`. C17 5.2.4.1 asks for 63 levels of each; these go well past
/// that, because generated source does.
fn deep_nesting_src() -> String {
    let mut code = String::from("int dn_deep(void) { return ");
    let depth = 400;
    for _ in 0..depth {
        code.push('(');
    }
    code.push('1');
    for _ in 0..depth {
        code.push(')');
    }
    code.push_str("; }\n");

    code.push_str("int dn_labels(int c) { int t = 0; switch (c) {\n");
    for i in 0..600 {
        code.push_str(&format!("case {i}:\n"));
    }
    code.push_str("t = 1; break; default: t = 2; }\nreturn t; }\n");
    code
}

/// One program, one section per original test; each section carries the
/// original doc comment and its exit-code range in its header. Consolidates:
/// `vm_array_params`, `ptr_to_vm_array`, `address_of_vla`, `deref_vm_pointer`,
/// `vm_pointer_arith`, `switch_body_block_scope`, `stmt_expr_scope`,
/// `vla_scope_release`.
#[test]
fn c99_features_vla_mega() {
    let code = r#"
#include <stddef.h>
#include <stdlib.h>

// ==========================================================================
// vm_array_params  (exit codes 1-28: 0 + its own code)
//
// (was #[test] c99_variably_modified_array_parameters)
//
// A variably-modified array *parameter* indexed with a row stride of zero,
// so every row aliased row 0 on reads and on writes alike, at 2D and 3D.
//
// `cc/parse/parser.rs` dropped a parameter declarator's dimension
// expressions -- alone among the declarator paths -- and the one place that
// computed a run-time stride only handled the outermost dimension of a bare
// identifier. Locals were affected too: a 3D VLA's inner stride and
// `sizeof` of any sub-array were both 0.
//
// Every expectation here was taken from gcc on the same source.
// ==========================================================================
static int vap_sum2(int n, int m, int a[n][m]) {
    int s = 0;
    for (int i = 0; i < n; i++)
        for (int j = 0; j < m; j++)
            s += a[i][j];
    return s;
}

/* The pointer-to-array spelling of the same parameter. */
static int vap_sum2p(int m, int (*a)[m], int n) {
    int s = 0;
    for (int i = 0; i < n; i++)
        for (int j = 0; j < m; j++)
            s += a[i][j];
    return s;
}

/* Neither dimension is named in the body: the extents must still be
   evaluated on entry, which is what a throwaway parameter scope broke. */
static long vap_row_stride(int n, int m, int a[n][m]) {
    (void)n;
    return (long)(&a[1][0] - &a[0][0]);
}

static size_t vap_row_size(int n, int m, int a[n][m]) {
    (void)n;
    (void)m;
    return sizeof(a[0]);
}

static void vap_write2(int n, int m, int a[n][m]) {
    (void)n;
    a[1][1] = 99;
    a[0][2] += 100;
    a[1][2]++;
}

static int vap_sum3(int n, int m, int k, int a[n][m][k]) {
    int s = 0;
    for (int i = 0; i < n; i++)
        for (int j = 0; j < m; j++)
            for (int t = 0; t < k; t++)
                s += a[i][j][t];
    return s;
}

/* A dimension that is an expression over earlier parameters. */
static int vap_sum_expr(int n, int m, int a[n][m + 1]) {
    int s = 0;
    for (int i = 0; i < n; i++)
        for (int j = 0; j < m + 1; j++)
            s += a[i][j];
    return s;
}

/* A constant inner extent mixed with a variable one. */
static int vap_sum_mixed(int n, int a[n][3]) {
    int s = 0;
    for (int i = 0; i < n; i++)
        for (int j = 0; j < 3; j++)
            s += a[i][j];
    return s;
}

static int t_vm_array_params(void) {
    int a[2][3] = { { 1, 2, 3 }, { 4, 5, 6 } };

    /* ===== parameters (returns 1-19) ===== */
    if (vap_sum2(2, 3, a) != 21) return 1;
    if (vap_sum2p(3, a, 2) != 21) return 2;
    if (vap_row_stride(2, 3, a) != 3) return 3;
    if (vap_row_size(2, 3, a) != 3 * sizeof(int)) return 4;
    if (vap_sum_expr(2, 2, a) != 21) return 5;
    if (vap_sum_mixed(2, a) != 21) return 6;

    int b[3][4];
    for (int i = 0; i < 3; i++)
        for (int j = 0; j < 4; j++)
            b[i][j] = i * 4 + j;
    vap_write2(3, 4, b);
    if (b[1][1] != 99) return 7;
    if (b[0][1] != 1) return 8;      /* the row that used to be clobbered */
    if (b[0][2] != 102) return 9;
    if (b[1][2] != 7) return 10;

    int c[2][2][2] = { { { 1, 2 }, { 3, 4 } }, { { 5, 6 }, { 7, 8 } } };
    if (vap_sum3(2, 2, 2, c) != 36) return 11;

    /* A genuine VLA argument, not just a fixed array passed to a
       variably-modified parameter. */
    int n = 3, m = 2;
    int v[n][m];
    for (int i = 0; i < n; i++)
        for (int j = 0; j < m; j++)
            v[i][j] = i * 10 + j;
    if (vap_sum2(n, m, v) != 63) return 12;

    /* ===== locals (returns 20-39) ===== */
    int k = 4;
    int d[n][m][k];
    for (int i = 0; i < n; i++)
        for (int j = 0; j < m; j++)
            for (int t = 0; t < k; t++)
                d[i][j][t] = i * 100 + j * 10 + t;

    int s3 = 0;
    for (int i = 0; i < n; i++)
        for (int j = 0; j < m; j++)
            for (int t = 0; t < k; t++)
                s3 += d[i][j][t];
    if (s3 != 2556) return 20;
    if (d[2][1][3] != 213) return 21;

    /* Strides at every depth, not just the outermost. */
    if (&d[1][0][0] - &d[0][0][0] != m * k) return 22;
    if (&d[0][1][0] - &d[0][0][0] != k) return 23;

    /* sizeof of a sub-array, which reported 0. */
    if (sizeof(d) != (size_t)n * m * k * sizeof(int)) return 24;
    if (sizeof(d[0]) != (size_t)m * k * sizeof(int)) return 25;
    if (sizeof(d[0][0]) != (size_t)k * sizeof(int)) return 26;

    /* A constant extent between two variable ones. */
    int e[n][3][k];
    for (int i = 0; i < n; i++)
        for (int j = 0; j < 3; j++)
            for (int t = 0; t < k; t++)
                e[i][j][t] = i + j + t;
    if (e[2][2][3] != 7) return 27;
    if (sizeof(e[0]) != (size_t)3 * k * sizeof(int)) return 28;

    return 0;
}

// ==========================================================================
// ptr_to_vm_array  (exit codes 29-33: 28 + its own code)
//
// (was #[test] c99_pointer_to_variably_modified_array)
//
// A pointer to a variably-modified array is a pointer, not a VLA.
//
// `int (*p)[n]` records run-time extents like a VLA declarator does, and the
// declaration path keyed on that alone: it rejected the initializer in
// `int (*p)[n] = a;` as "variable length arrays cannot have initializers",
// and then walked an array type that was not there and panicked with
// "VLA must have at least one dimension".
//
// What the extents are actually for here is the row stride: one index step
// off `p` has to advance by `n` elements, the same arithmetic a variably
// modified *parameter* -- which is exactly this pointer type after
// adjustment -- already needed.
// ==========================================================================
static int pvm_fill(int n)
{
    int a[3][n];
    /* Declared separately from its initialization, and with one. */
    int (*p)[n];
    int (*q)[n] = a;

    p = a;
    for (int i = 0; i < 3; i++)
        for (int j = 0; j < n; j++)
            p[i][j] = i * 10 + j;

    int total = 0;
    for (int i = 0; i < 3; i++)
        for (int j = 0; j < n; j++)
            total += q[i][j];

    /* The rows really are n elements apart. */
    if (&p[1][0] - &p[0][0] != n) return -1;
    if (&q[2][0] - &q[0][0] != 2 * n) return -2;
    return total;
}

static int t_ptr_to_vm_array(void)
{
    /* n = 5: rows 0,10..14 and 20..24 -> 0+1+2+3+4 + 50+10 + 100+10 */
    if (pvm_fill(5) != 180) return 1;
    if (pvm_fill(1) != 30) return 2;

    /* A constant extent on the pointee still behaves. */
    int n = 4;
    int m[4][4];
    int (*r)[n] = m;
    r[3][3] = 9;
    if (m[3][3] != 9) return 3;

    /* Malloc'd storage, the idiom this type exists for. */
    int rows = 3, cols = 6;
    int (*d)[cols] = malloc(sizeof(int) * rows * cols);
    if (!d) return 4;
    d[2][5] = 77;
    if (d[2][5] != 77) return 5;
    free(d);

    return 0;
}

// ==========================================================================
// address_of_vla  (exit codes 34-46: 33 + its own code)
//
// (was #[test] c99_address_of_a_vla_is_the_array_address)
//
// A VLA's local slot holds a *pointer* to the storage, not the storage, so
// `&a` has to yield the pointer's value -- the same address the array decays
// to (C99 6.5.3.2p3, and 6.3.2.1p3 for the decay). c17 took the slot's
// address instead, so `&a` differed from `a` for every VLA and
// `int (*p)[n] = &a` pointed at the pointer.
// ==========================================================================
static int t_address_of_vla(void) {
    int n = 4, m = 5;

    /* One dimension. */
    int b[n];
    for (int i = 0; i < n; i++) b[i] = i + 100;
    if ((void *)&b != (void *)b) return 1;
    if ((void *)&b[0] != (void *)b) return 2;
    int (*pb)[n] = &b;
    if ((*pb)[2] != 102) return 3;

    /* Two dimensions. */
    int a[n][m];
    for (int i = 0; i < n; i++)
        for (int j = 0; j < m; j++) a[i][j] = i * m + j;
    if ((void *)&a != (void *)a) return 4;
    if ((void *)&a[0] != (void *)a) return 5;
    int (*pa)[n][m] = &a;
    if ((void *)pa != (void *)a) return 6;
    if (pa[0][3][4] != 19) return 7;

    /* Through a variably modified typedef (6.7.7). */
    typedef int T[n][m];
    T *pt = &a;
    if ((void *)pt != (void *)a) return 8;
    if (pt[0][1][0] != 5) return 9;

    /* The row type still decays the ordinary way. */
    int (*q)[m] = a;
    if (q[3][4] != 19) return 10;

    /* A VLA in an inner scope, so the slot is reused. */
    {
        int c[n];
        for (int i = 0; i < n; i++) c[i] = i * 7;
        if ((void *)&c != (void *)c) return 11;
        int (*pc)[n] = &c;
        if ((*pc)[3] != 21) return 12;
    }

    /* And a VLA whose extent is itself an expression. */
    int d[n * 2 + 1];
    for (int i = 0; i < n * 2 + 1; i++) d[i] = i;
    if ((void *)&d != (void *)d) return 13;

    return 0;
}

// ==========================================================================
// deref_vm_pointer  (exit codes 47-62: 46 + its own code)
//
// (was #[test] c99_deref_of_a_pointer_to_a_vm_array)
//
// Dereferencing a pointer to a variably-modified array has to keep the
// array's run-time extents: `(*p)[i]` steps a whole row, and `sizeof(*p)` is
// the run-time size (C99 6.5.3.4p2 -- "if the type is variable length, the
// size is computed at execution time").
//
// The extents were carried only on the way *in*: `p[0][i][j]` indexed
// correctly while `(*p)[i][j]` -- the same address, spelled with a deref --
// used a stride of zero, and every `sizeof(*p)` answered 0.
// ==========================================================================
static int t_deref_vm_pointer(void) {
    int n = 4, m = 5;
    unsigned long isz = sizeof(int);

    int a[n][m];
    for (int i = 0; i < n; i++)
        for (int j = 0; j < m; j++) a[i][j] = i * m + j;
    int b[n];
    for (int i = 0; i < n; i++) b[i] = i + 100;

    /* Pointer to a one-dimensional VM array. */
    int (*pb)[n] = &b;
    if ((*pb)[2] != 102) return 1;
    if (sizeof(*pb) != (unsigned long)n * isz) return 2;

    /* Pointer to a two-dimensional VM array: the deref must step rows. */
    int (*pa)[n][m] = &a;
    if ((*pa)[0][0] != 0) return 3;
    if ((*pa)[1][0] != 5) return 4;
    if ((*pa)[3][4] != 19) return 5;
    if (sizeof(*pa) != (unsigned long)(n * m) * isz) return 6;
    if (sizeof((*pa)[0]) != (unsigned long)m * isz) return 7;

    /* The same through a variably modified typedef (6.7.7). */
    typedef int T[n][m];
    T *pt = &a;
    if ((*pt)[1][0] != 5) return 8;
    if ((*pt)[3][4] != 19) return 9;
    if (sizeof(*pt) != (unsigned long)(n * m) * isz) return 10;

    /* Indexing without the deref must keep working. */
    if (pa[0][3][4] != 19) return 11;
    int (*q)[m] = a;
    if (q[3][4] != 19) return 12;
    if (sizeof(*q) != (unsigned long)m * isz) return 13;

    /* Writing through the deref lands in the original array. */
    (*pa)[2][1] = 777;
    if (a[2][1] != 777) return 14;

    /* Pointer arithmetic steps whole arrays. */
    if ((char *)(pa + 1) - (char *)pa != (long)(n * m) * (long)isz) return 15;
    if ((char *)(pb + 1) - (char *)pb != (long)n * (long)isz) return 16;

    return 0;
}

// ==========================================================================
// vm_pointer_arith  (exit codes 63-78: 62 + its own code)
//
// (was #[test] c99_vm_pointer_arithmetic_every_spelling)
//
// Every way of stepping a pointer to a variably-modified array has to use
// the run-time stride, not just the two that were fixed first.
//
// `++p` and `p + 1` took it; `p++`, `p += 1` and `p - q` did not. So the
// pointer silently did not move for two of the five spellings, and a
// difference whose left operand was not a bare identifier divided by a
// compile-time size of zero and trapped.
// ==========================================================================
static int t_vm_pointer_arith(void) {
    int n = 3, m = 5;
    long row = (long)m * (long)sizeof(int);
    int a[n][m];
    int (*p)[m] = a;
    int (*r)[m];

    /* All five spellings step exactly one row. */
    r = p; r++;        if ((char *)r - (char *)p != row) return 1;
    r = p; ++r;        if ((char *)r - (char *)p != row) return 2;
    r = p; r += 1;     if ((char *)r - (char *)p != row) return 3;
    r = p; r = r + 1;  if ((char *)r - (char *)p != row) return 4;
    r = p; r = 1 + r;  if ((char *)r - (char *)p != row) return 5;

    /* And backwards. */
    r = p + 2; r--;    if ((char *)r - (char *)p != row) return 6;
    r = p + 2; --r;    if ((char *)r - (char *)p != row) return 7;
    r = p + 2; r -= 1; if ((char *)r - (char *)p != row) return 8;
    r = p + 2; r = r - 1; if ((char *)r - (char *)p != row) return 9;

    /* Difference, including operands that are not bare identifiers. */
    r = p + 2;
    if (r - p != 2) return 10;
    if ((p + 2) - p != 2) return 11;
    if (r - (p + 1) != 1) return 12;
    if ((p + 2) - (p + 1) != 1) return 13;

    /* The post forms yield the old value and still move. */
    r = p;
    if ((char *)(r++) - (char *)p != 0) return 14;
    if ((char *)r - (char *)p != row) return 15;

    /* A two-dimensional VM pointee steps the whole array. */
    int (*w)[n][m] = &a;
    if ((char *)(w + 1) - (char *)w != (long)(n * m) * (long)sizeof(int)) return 16;

    return 0;
}

// ==========================================================================
// switch_body_block_scope  (exit codes 79-81: 78 + its own code)
//
// (was #[test] c99_a_switch_body_block_is_a_declaration_scope)
//
// A block inside a `switch` body is a declaration scope there too: its
// ordinary declarations do not outlive it, and a VLA declared in it is
// released when it ends.
// ==========================================================================
static int t_switch_body_block_scope(void) {
    int v = 1;
    int x = 2;
    switch (x) {
    default: {
        int v = 10;        /* shadows the outer v only inside these braces */
        if (v != 10) return 1;
        break;
    }
    }
    if (v != 1) return 2;  /* the inner declaration must not have escaped */

    /* And a VLA declared there is released when the block ends. */
    void *first = 0;
    for (int k = 0; k < 8; k++)
        switch (x) {
        default: {
            int a[x + 5];
            a[0] = k;
            if (!first) first = (void *)a;
            else if (first != (void *)a) return 3;
        }
        }
    return 0;
}

// ==========================================================================
// stmt_expr_scope  (exit codes 82-85: 81 + its own code)
//
// (was #[test] c99_a_statement_expression_is_a_declaration_scope)
//
// A declaration in a statement expression does not outlive it.
//
// The statement expression had no declaration scope at all, so its locals
// were inserted into the enclosing one and stayed there -- an inner `x`
// went on shadowing the outer one after the `})`.
// ==========================================================================
static int t_stmt_expr_scope(void) {
    int x = 1;
    int y = ({ int x = 41; x + 1; });
    if (y != 42) return 1;
    if (x != 1) return 2;   /* the inner x must be gone */
    {
        typedef int T;
        int z = ({ typedef long T; (int)sizeof(T); });
        if (z != (int)sizeof(long)) return 3;
        if ((int)sizeof(T) != (int)sizeof(int)) return 4;
    }
    return 0;
}

// ==========================================================================
// vla_scope_release  (exit codes 86-91: 85 + its own code)
//
// (was #[test] c99_a_vla_scope_is_released_on_every_exit)
// Placed last on purpose: gcc itself does not release a VLA left by a
// computed goto, so under gcc this section (alone or here) fails with
// its own code 6; every other section is validated against gcc first.
//
// Every way out of a scope that declares a VLA puts the stack pointer back.
//
// Observed without waiting for an exhaustion that a frame pointer hides:
// the same declaration reached on the same path allocates at the same
// address every time round *if and only if* the previous iteration released
// it. A scope that never releases marches the address up the stack, and the
// first mismatch is one iteration later.
//
// The declaration scope and the VLA scope used to be opened by hand at
// separate call sites, and three of the sites that opened the first never
// opened the second: a `for` init clause, the copy of the `for` lowering
// inside the switch-body walk, and a statement expression. A computed
// `goto` left no scope at all.
// ==========================================================================
/* A VLA in a `for` init clause: allocated once per execution of the inner
   `for` statement, released when that statement ends. */
static int vsr_for_init(int n) {
    void *first = 0;
    for (int k = 0; k < 8; k++)
        for (int a[n]; ; ) {
            a[0] = k;
            if (!first) first = (void *)a;
            else if (first != (void *)a) return 1;
            break;                      /* leaves by `break` */
        }
    return 0;
}

/* The same shape, inside a switch arm. */
static int vsr_for_init_in_switch(int n, int x) {
    void *first = 0;
    switch (x) {
    case 1:
        for (int k = 0; k < 8; k++)
            for (int a[n]; ; ) {
                a[0] = k;
                if (!first) first = (void *)a;
                else if (first != (void *)a) return 2;
                break;
            }
        return 0;
    }
    return 3;
}

/* A statement expression is a block, so it is a scope. */
static int vsr_stmt_expr(int n) {
    void *first = 0;
    int bad = 0;
    for (int k = 0; k < 8; k++)
        (void)({
            int a[n];
            a[0] = k;
            if (!first) first = (void *)a;
            else if (first != (void *)a) bad = 4;
            0;
        });
    return bad;
}

/* Leaving by a forward `goto`, which also has to leave the label bookkeeping
   straight for every label after it. */
static int vsr_goto_out(int n) {
    void *first = 0;
    for (int k = 0; k < 8; k++) {
        for (int a[n]; ; ) {
            a[0] = k;
            if (!first) first = (void *)a;
            else if (first != (void *)a) return 5;
            goto next;
        }
    next:
        ;
    }
    return 0;
}

/* And by a computed `goto`, which leaves a scope exactly as a plain one
   does. The jump is the loop, so every iteration goes through it. */
static int vsr_computed_goto(int n) {
    void *first = 0;
    int k = 0;
    void *back = &&top;
top:
    {
        int a[n];
        a[0] = k;
        if (!first) first = (void *)a;
        else if (first != (void *)a) return 6;
        k++;
        if (k < 8) goto *back;
    }
    return 0;
}

static int t_vla_scope_release(void) {
    int n = 7;
    int rc;
    if ((rc = vsr_for_init(n)) != 0) return rc;
    if ((rc = vsr_for_init_in_switch(n, 1)) != 0) return rc;
    if ((rc = vsr_stmt_expr(n)) != 0) return rc;
    if ((rc = vsr_goto_out(n)) != 0) return rc;
    if ((rc = vsr_computed_goto(n)) != 0) return rc;
    return 0;
}

int main(void) {
    int r;
    if ((r = t_vm_array_params()) != 0) return 0 + r;
    if ((r = t_ptr_to_vm_array()) != 0) return 28 + r;
    if ((r = t_address_of_vla()) != 0) return 33 + r;
    if ((r = t_deref_vm_pointer()) != 0) return 46 + r;
    if ((r = t_vm_pointer_arith()) != 0) return 62 + r;
    if ((r = t_switch_body_block_scope()) != 0) return 78 + r;
    if ((r = t_stmt_expr_scope()) != 0) return 81 + r;
    if ((r = t_vla_scope_release()) != 0) return 85 + r;
    return 0;
}
"#;
    assert_eq!(compile_and_run("c99_features_vla_mega", code, &[]), 0);
}

/// One program, one section per original test; each section carries the
/// original doc comment and its exit-code range in its header. Consolidates:
/// `labels_innermost_switch`, `fam_top_level_init`, `fam_elided_values`,
/// `array_extents`.
#[test]
fn c99_features_everywhere_mega() {
    let code = r#"
#include <string.h>

// ==========================================================================
// labels_innermost_switch  (exit codes 1-35: 0 + its own code)
//
// (was #[test] c99_labels_belong_to_the_innermost_switch)
//
// Every `case` and `default` label belongs to the innermost enclosing
// `switch`, wherever it sits in that switch's body: inside a `do` loop
// (Duff's device), in either arm of an `if`, in a `for` body, and after a
// nested switch in the same block -- whose own equal labels shadow the outer
// ones. `default` stands first, in the middle and last.
// ==========================================================================
/* Duff's device: case labels inside a do-while inside the switch. */
static int lis_duff_sum(const int *a, int count) {
    int s = 0, n = (count + 3) / 4;
    if (count == 0) return 0;
    switch (count % 4) {
    case 0: do { s += *a++;
    case 3:      s += *a++;
    case 2:      s += *a++;
    case 1:      s += *a++;
            } while (--n > 0);
    }
    return s;
}

/* Nested switches: the inner switch's labels shadow the outer's equal ones,
   and an outer label after the inner switch, in the same block, is the
   outer's again. `default` stands first, in the middle and last. */
static int lis_nested(int x, int y) {
    int r = 0;
    switch (x) {
    default: r += 1000;
    case 1:
        r += 1;
        {
            switch (y) {
            case 1: r += 10; break;
            default: r += 30;
            case 2: r += 20; break;
            }
    case 2:
            r += 2;
        }
        break;
    case 3:
        switch (y) { case 3: r += 300; break; case 1: r += 100; default: r += 400; }
        break;
    }
    return r;
}

/* Case labels in both arms of an if-else, and in a `for`, inside the switch. */
static int lis_in_if(int x, int c) {
    int r = 0, i = 0;
    switch (x) {
    case 0:
        if (c) {
    case 1:
            r += 1;
        } else {
    case 2:
            r += 2;
        }
        r += 10;
        break;
    case 3:
        for (; i < 2; i++) {
            r += 100;
    default:
            r += 1000;
        }
    }
    return r;
}

static int t_labels_innermost_switch(void) {
    int a[9] = {1, 2, 3, 4, 5, 6, 7, 8, 9};
    for (int k = 0; k <= 9; k++) {
        int want = k * (k + 1) / 2;
        if (lis_duff_sum(a, k) != want) return 1 + k;
    }
    if (lis_nested(1, 1) != 13) return 20;
    if (lis_nested(1, 2) != 23) return 21;
    if (lis_nested(1, 7) != 53) return 22;
    if (lis_nested(2, 1) != 2) return 23;
    if (lis_nested(3, 3) != 300) return 24;
    if (lis_nested(3, 1) != 500) return 25;
    if (lis_nested(3, 9) != 400) return 26;
    if (lis_nested(9, 1) != 1013) return 27;
    if (lis_in_if(0, 1) != 11) return 30;
    if (lis_in_if(0, 0) != 12) return 31;
    if (lis_in_if(1, 0) != 11) return 32;
    if (lis_in_if(2, 1) != 12) return 33;
    if (lis_in_if(3, 0) != 2200) return 34;
    if (lis_in_if(9, 0) != 2100) return 35;
    return 0;
}

// ==========================================================================
// fam_top_level_init  (exit codes 36-37: 35 + its own code)
//
// (was #[test] c99_flexible_array_member_initializer_in_an_array_is_rejected)
// The accepted top-level form from that test; its two rejected forms
// are compile-only and live in cc/test_asm/c99_features.rs.
//
// A flexible array member may be initialized at the top level of a static
// object, as a GNU extension gcc accepts, but not inside an element of an
// array of such structures: each element would be a different size, which
// no array can hold. gcc rejects that with "initialization of flexible array
// member in a nested context", once per element; c17 accepted it and laid
// the elements out as if the member were empty.
// ==========================================================================
struct fti_V { int n; const char s[]; };
static const struct fti_V fti_top = { 3, "abc" };          /* GNU extension: accepted */
struct fti_W { int n; int a[]; };
static struct fti_W fti_w = { 2, { 7, 8 } };
static int t_fam_top_level_init(void) {
    if (fti_top.n != 3 || strcmp(fti_top.s, "abc") != 0) return 1;
    if (fti_w.a[0] != 7 || fti_w.a[1] != 8) return 2;
    return 0;
}

// ==========================================================================
// fam_elided_values  (exit codes 38-40: 37 + its own code)
//
// (was #[test] c99_flexible_array_member_takes_every_elided_value)
//
// A brace-elided initializer for a flexible array member takes every value
// left in the list, as gcc lays it out; the member used to get only the
// first, and the rest were dropped as excess.
// ==========================================================================
struct fev_W { int n; int a[]; };
static struct fev_W fev_w = { 1, 2, 3 };
struct fev_W2 { int n; struct { int x, y; } a[]; };
static struct fev_W2 fev_w2 = { 1, 2, 3, 4, 5 };
static int t_fam_elided_values(void) {
    if (fev_w.n != 1 || fev_w.a[0] != 2 || fev_w.a[1] != 3) return 1;
    if (fev_w2.n != 1 || fev_w2.a[0].x != 2 || fev_w2.a[0].y != 3) return 2;
    if (fev_w2.a[1].x != 4 || fev_w2.a[1].y != 5) return 3;
    return 0;
}

// ==========================================================================
// array_extents  (exit codes 41-50: 40 + its own code)
//
// A variable length array and an array of unknown size are different
// types: `int[n]` is complete and measured at run time, `int[]` is
// incomplete until a declaration or an initializer sizes it. They once
// interned to one type, so these must still measure what they did.
// ==========================================================================
extern int ae_a[];
int ae_a[5];
static int ae_rows(int n, int (*q)[n]) { return (int)(sizeof *q / sizeof (*q)[0]); }
static int t_array_extents(int n) {
    int (*p)[n] = 0;
    __typeof__(*p) row;
    typedef int T[n];
    T t;
    int grid[n][3];
    int (*g)[3] = grid;
    int (*v)[n] = (int (*)[n])grid;
    int init[] = { 1, 2, 3 };
    if (sizeof *p != n * sizeof(int)) return 1;
    if (sizeof row != n * sizeof(int)) return 2;
    if (sizeof t != n * sizeof(int)) return 3;
    if (sizeof(__typeof__(*p)) != n * sizeof(int)) return 4;
    if (sizeof grid != n * 3 * sizeof(int) || sizeof grid[0] != 3 * sizeof(int)) return 5;
    if ((char *)(g + 1) - (char *)g != 3 * sizeof(int)) return 6;
    if ((char *)(v + 1) - (char *)v != n * sizeof(int)) return 7;
    if (sizeof ae_a != 5 * sizeof(int) || sizeof init != 3 * sizeof(int)) return 8;
    if (ae_rows(n, v) != n) return 9;
    if (sizeof(int[n][n]) != n * n * sizeof(int)) return 10;
    return 0;
}

int main(void) {
    int r;
    if ((r = t_labels_innermost_switch()) != 0) return 0 + r;
    if ((r = t_fam_top_level_init()) != 0) return 35 + r;
    if ((r = t_fam_elided_values()) != 0) return 37 + r;
    if ((r = t_array_extents(7)) != 0) return 40 + r;
    return 0;
}
"#;
    compile_and_run_everywhere("c99_features_everywhere_mega", code);
}
