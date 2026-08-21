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

use crate::common::compile_and_run;

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

#[test]
fn c99_scope_shadowing_deep() {
    let code = r#"
int main(void) {
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
"#;
    assert_eq!(compile_and_run("scope_shadowing_deep", code, &[]), 0);
}

/// C99 6.4.4.2 hex floats name a binary value directly, so a long significand
/// must survive the parse intact.
///
/// The significand was accumulated in a `u64` and divided by
/// `1u64 << (4 * digits)`. Sixteen fraction digits make that a shift of 64,
/// which wraps to a shift of 0 in a release build, so
/// `0x1.0000000000000002p+0` evaluated to **3.0** -- wrong by a factor of
/// three, with no diagnostic, in a feature the conformance matrix listed as
/// passing. Values are checked against arithmetic that cannot use the same
/// parser, so a repeat of the bug cannot satisfy both sides.
#[test]
fn c99_hex_float_long_significand() {
    let code = r#"
#include <float.h>

int main(void) {
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
"#;
    assert_eq!(
        compile_and_run("c99_hex_float_long_significand", code, &[]),
        0
    );
}

/// A variably-modified array *parameter* indexed with a row stride of zero,
/// so every row aliased row 0 on reads and on writes alike, at 2D and 3D.
///
/// `cc/parse/parser.rs` dropped a parameter declarator's dimension
/// expressions -- alone among the declarator paths -- and the one place that
/// computed a run-time stride only handled the outermost dimension of a bare
/// identifier. Locals were affected too: a 3D VLA's inner stride and
/// `sizeof` of any sub-array were both 0.
///
/// Every expectation here was taken from gcc on the same source.
#[test]
fn c99_variably_modified_array_parameters() {
    let code = r#"
#include <stddef.h>

static int sum2(int n, int m, int a[n][m]) {
    int s = 0;
    for (int i = 0; i < n; i++)
        for (int j = 0; j < m; j++)
            s += a[i][j];
    return s;
}

/* The pointer-to-array spelling of the same parameter. */
static int sum2p(int m, int (*a)[m], int n) {
    int s = 0;
    for (int i = 0; i < n; i++)
        for (int j = 0; j < m; j++)
            s += a[i][j];
    return s;
}

/* Neither dimension is named in the body: the extents must still be
   evaluated on entry, which is what a throwaway parameter scope broke. */
static long row_stride(int n, int m, int a[n][m]) {
    (void)n;
    return (long)(&a[1][0] - &a[0][0]);
}

static size_t row_size(int n, int m, int a[n][m]) {
    (void)n;
    (void)m;
    return sizeof(a[0]);
}

static void write2(int n, int m, int a[n][m]) {
    (void)n;
    a[1][1] = 99;
    a[0][2] += 100;
    a[1][2]++;
}

static int sum3(int n, int m, int k, int a[n][m][k]) {
    int s = 0;
    for (int i = 0; i < n; i++)
        for (int j = 0; j < m; j++)
            for (int t = 0; t < k; t++)
                s += a[i][j][t];
    return s;
}

/* A dimension that is an expression over earlier parameters. */
static int sum_expr(int n, int m, int a[n][m + 1]) {
    int s = 0;
    for (int i = 0; i < n; i++)
        for (int j = 0; j < m + 1; j++)
            s += a[i][j];
    return s;
}

/* A constant inner extent mixed with a variable one. */
static int sum_mixed(int n, int a[n][3]) {
    int s = 0;
    for (int i = 0; i < n; i++)
        for (int j = 0; j < 3; j++)
            s += a[i][j];
    return s;
}

int main(void) {
    int a[2][3] = { { 1, 2, 3 }, { 4, 5, 6 } };

    /* ===== parameters (returns 1-19) ===== */
    if (sum2(2, 3, a) != 21) return 1;
    if (sum2p(3, a, 2) != 21) return 2;
    if (row_stride(2, 3, a) != 3) return 3;
    if (row_size(2, 3, a) != 3 * sizeof(int)) return 4;
    if (sum_expr(2, 2, a) != 21) return 5;
    if (sum_mixed(2, a) != 21) return 6;

    int b[3][4];
    for (int i = 0; i < 3; i++)
        for (int j = 0; j < 4; j++)
            b[i][j] = i * 4 + j;
    write2(3, 4, b);
    if (b[1][1] != 99) return 7;
    if (b[0][1] != 1) return 8;      /* the row that used to be clobbered */
    if (b[0][2] != 102) return 9;
    if (b[1][2] != 7) return 10;

    int c[2][2][2] = { { { 1, 2 }, { 3, 4 } }, { { 5, 6 }, { 7, 8 } } };
    if (sum3(2, 2, 2, c) != 36) return 11;

    /* A genuine VLA argument, not just a fixed array passed to a
       variably-modified parameter. */
    int n = 3, m = 2;
    int v[n][m];
    for (int i = 0; i < n; i++)
        for (int j = 0; j < m; j++)
            v[i][j] = i * 10 + j;
    if (sum2(n, m, v) != 63) return 12;

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
"#;
    assert_eq!(
        compile_and_run("c99_variably_modified_array_parameters", code, &[]),
        0
    );
}

/// A parameter must shadow a file-scope typedef of the same name
/// (C17 6.2.1p4), so `(name)` inside the function is a parenthesized
/// expression and not a type name.
///
/// The parameter symbol has to be registered as the *innermost* binding.
/// Registering it as the outermost instead left the typedef winning, and
/// `((PyObject*)((string)))` in CPython then failed to parse -- the compiler
/// read `(string)` as a type name and wanted a cast operand after it.
#[test]
fn c99_parameter_shadows_a_file_scope_typedef() {
    let code = r#"
typedef int *string;
typedef long counter;

static int deref(int *string) {
    /* `string` is the parameter here, so this is a cast of an expression. */
    return *((int *)((string)));
}

static long total(long counter, long n) {
    long s = 0;
    for (long i = 0; i < n; i++)
        s += (counter);
    return s;
}

/* The typedef is visible again once the parameter is out of scope. */
static string pick(string a, string b, int which) {
    return which ? a : b;
}

int main(void) {
    int v = 42;
    if (deref(&v) != 42) return 1;
    if (total(3, 4) != 12) return 2;

    int x = 7, y = 9;
    string p = &x, q = &y;
    if (*pick(p, q, 1) != 7) return 3;
    if (*pick(p, q, 0) != 9) return 4;

    /* At file scope the typedef still names a type. */
    string r = &x;
    if (*r != 7) return 5;
    counter c = 5;
    if (c != 5) return 6;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("c99_parameter_shadows_a_file_scope_typedef", code, &[]),
        0
    );
}

/// A pointer to a variably-modified array is a pointer, not a VLA.
///
/// `int (*p)[n]` records run-time extents like a VLA declarator does, and the
/// declaration path keyed on that alone: it rejected the initializer in
/// `int (*p)[n] = a;` as "variable length arrays cannot have initializers",
/// and then walked an array type that was not there and panicked with
/// "VLA must have at least one dimension".
///
/// What the extents are actually for here is the row stride: one index step
/// off `p` has to advance by `n` elements, the same arithmetic a variably
/// modified *parameter* -- which is exactly this pointer type after
/// adjustment -- already needed.
#[test]
fn c99_pointer_to_variably_modified_array() {
    let code = r#"
#include <stdlib.h>

static int fill(int n)
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

int main(void)
{
    /* n = 5: rows 0,10..14 and 20..24 -> 0+1+2+3+4 + 50+10 + 100+10 */
    if (fill(5) != 180) return 1;
    if (fill(1) != 30) return 2;

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
"#;
    assert_eq!(
        compile_and_run("c99_pointer_to_variably_modified_array", code, &[]),
        0
    );
}

/// A zero significand is zero at any exponent, and a subnormal literal is
/// rounded to nearest rather than truncated.
///
/// The decimal converter short-circuits an exponent far outside every target
/// format, on the reasoning that the value can only be an infinity or a zero.
/// That reasoning holds only for a non-zero significand: `0e6000` came out as
/// an infinity. Separately, the subnormal encoding path shifted the
/// significand down and dropped what fell off, which is directed rounding
/// toward zero where every other path rounds to nearest.
#[test]
fn c99_extreme_float_literals() {
    let code = r#"
#include <float.h>

double zero_big = 0e6000;
double zero_small = 0.0e-9999;
long double zero_ld = 0.000e5001;

int main(void)
{
    if (zero_big != 0.0) return 1;
    if (zero_small != 0.0) return 2;
    if (zero_ld != 0.0L) return 3;
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
"#;
    assert_eq!(compile_and_run("c99_extreme_float_literals", code, &[]), 0);
}

/// A VLA's local slot holds a *pointer* to the storage, not the storage, so
/// `&a` has to yield the pointer's value -- the same address the array decays
/// to (C99 6.5.3.2p3, and 6.3.2.1p3 for the decay). c17 took the slot's
/// address instead, so `&a` differed from `a` for every VLA and
/// `int (*p)[n] = &a` pointed at the pointer.
#[test]
fn c99_address_of_a_vla_is_the_array_address() {
    let code = r#"
int main(void) {
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
"#;
    assert_eq!(
        compile_and_run("c99_address_of_a_vla", code, &[]),
        0,
        "&vla must be the array's address"
    );
}

/// Dereferencing a pointer to a variably-modified array has to keep the
/// array's run-time extents: `(*p)[i]` steps a whole row, and `sizeof(*p)` is
/// the run-time size (C99 6.5.3.4p2 -- "if the type is variable length, the
/// size is computed at execution time").
///
/// The extents were carried only on the way *in*: `p[0][i][j]` indexed
/// correctly while `(*p)[i][j]` -- the same address, spelled with a deref --
/// used a stride of zero, and every `sizeof(*p)` answered 0.
#[test]
fn c99_deref_of_a_pointer_to_a_vm_array() {
    let code = r#"
int main(void) {
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
"#;
    assert_eq!(
        compile_and_run("c99_deref_vm_pointer", code, &[]),
        0,
        "deref of a pointer to a variably-modified array"
    );
}
