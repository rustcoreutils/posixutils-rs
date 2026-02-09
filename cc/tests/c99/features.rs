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
