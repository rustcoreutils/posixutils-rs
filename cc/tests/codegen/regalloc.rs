//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Register Allocation Mega-Test
//
// Consolidates: loop_regalloc, shift_regalloc, many_args tests
//

use crate::common::compile_and_run;

// ============================================================================
// Mega-test: Register allocation
// ============================================================================

#[test]
fn codegen_regalloc_mega() {
    let code = r#"
// Lookup tables for high register pressure test
int lookup1[256];
int lookup2[256];
int extra_bits[256];

void init_tables(void) {
    for (int i = 0; i < 256; i++) {
        lookup1[i] = i;
        lookup2[i] = i * 2;
        extra_bits[i] = i & 0x0F;
    }
}

int test_alloca_size(int n) {
    int *arr = __builtin_alloca(n * sizeof(int));
    for (int i = 0; i < n; i++) {
        arr[i] = i * 10;
    }
    int sum = 0;
    for (int i = 0; i < n; i++) {
        sum += arr[i];
    }
    return sum;
}

// Process data with high register pressure (zlib-like)
int process_data(unsigned char *data, int len, int threshold) {
    int result = 0;
    int count = 0;

    while (count < len) {
        int a = data[count] & 0xff;
        int b = (data[count + 1] & 0xff) << 8;
        int c = data[count + 2];

        if (c == 0) {
            result += lookup1[a];
        } else {
            int code = lookup2[c];
            result += code + extra_bits[c];
            int dist_code = (b >> 8) & 0x1F;
            result += dist_code;
        }
        count += 3;
    }
    return result;
}

// Many mixed args function
double many_args(int a, double b, int c, double d, int e, double f,
                 int g, double h, int i, double j, int k, double l,
                 int m, double n, int o, double p, int q, double r) {
    double sum = (double)a + b + (double)c + d + (double)e + f +
                 (double)g + h + (double)i + j + (double)k + l +
                 (double)m + n + (double)o + p + (double)q + r;
    return sum;
}

long many_ints(int a, int b, int c, int d, int e, int f,
               int g, int h, int i, int j, int k, int l) {
    return (long)a + b + c + d + e + f + g + h + i + j + k + l;
}

double many_doubles(double a, double b, double c, double d,
                    double e, double f, double g, double h,
                    double i, double j) {
    return a + b + c + d + e + f + g + h + i + j;
}

int main(void) {
    // ========== LOOP REGALLOC (returns 1-49) ==========

    // Basic loop-carried value
    {
        int sum = 0;
        int multiplier = 3;
        for (int i = 0; i < 5; i++) {
            sum += multiplier;
        }
        if (sum != 15) return 1;

        sum = 0;
        multiplier = 7;
        for (int i = 0; i < 10; i++) {
            sum += multiplier;
        }
        if (sum != 70) return 2;
    }

    // Multiple loop-carried values
    {
        int a = 1, b = 2, c = 3;
        int sum = 0;
        for (int i = 0; i < 4; i++) {
            sum += a + b + c;
        }
        if (sum != 24) return 3;
    }

    // Loop with nested conditionals
    {
        int count_above = 0;
        int count_below = 0;
        int threshold = 5;
        for (int i = 0; i < 10; i++) {
            if (i >= threshold) {
                count_above++;
            } else {
                count_below++;
            }
        }
        if (count_above * 100 + count_below != 505) return 4;
    }

    // High register pressure (zlib-like)
    {
        init_tables();

        unsigned char data1[30];
        for (int i = 0; i < 30; i += 3) {
            data1[i] = i;
            data1[i + 1] = i + 1;
            data1[i + 2] = 0;
        }
        int r1 = process_data(data1, 30, 128);
        if (r1 != 135) return 5;

        unsigned char data2[30];
        for (int i = 0; i < 30; i += 3) {
            data2[i] = i;
            data2[i + 1] = 0x10;
            data2[i + 2] = 1;
        }
        int r2 = process_data(data2, 30, 128);
        if (r2 != 190) return 6;
    }

    // Do-while loop
    {
        int arr[10] = {1, 2, 3, 2, 5, 2, 7, 8, 2, 10};
        int count = 0;
        int i = 0;
        int len = 10;
        int target = 2;
        if (len > 0) do {
            if (arr[i] == target) {
                count++;
            }
            i++;
        } while (i < len);
        if (count != 4) return 7;
    }

    // Bit manipulation in loop
    {
        unsigned int data[4] = {0x12, 0x34, 0x56, 0x78};
        unsigned int buffer = 0;
        int bits_in_buffer = 0;
        int output_count = 0;

        for (int i = 0; i < 4; i++) {
            unsigned int value = data[i] & 0xFF;
            int bits_to_add = 8;
            buffer |= (value << bits_in_buffer);
            bits_in_buffer += bits_to_add;
            while (bits_in_buffer >= 16) {
                output_count++;
                buffer >>= 16;
                bits_in_buffer -= 16;
            }
        }
        if (bits_in_buffer > 0) output_count++;
        if (output_count != 2) return 8;
    }

    // Nested loops
    {
        int sum = 0;
        int fill_value = 10;
        for (int i = 0; i < 3; i++) {
            int row_sum = 0;
            for (int j = 0; j < 4; j++) {
                row_sum += fill_value + i;
            }
            sum += row_sum;
        }
        if (sum != 132) return 9;
    }

    // ========== MANY ARGS (returns 50-69) ==========

    // Mixed int/FP args (18 args)
    {
        double result = many_args(1, 0.5, 2, 1.5, 3, 2.5,
                                  4, 3.5, 5, 4.5, 6, 5.5,
                                  7, 6.5, 8, 7.5, 9, 8.5);
        if (result < 85.4 || result > 85.6) return 50;
    }

    // Many int args (12 args, 6 on stack for x86_64)
    {
        long result = many_ints(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12);
        if (result != 78) return 51;
    }

    // Many FP args (10 args, 2 on stack for x86_64)
    {
        double result = many_doubles(1.0, 2.0, 3.0, 4.0, 5.0,
                                      6.0, 7.0, 8.0, 9.0, 10.0);
        if (result < 54.9 || result > 55.1) return 52;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("codegen_regalloc_mega", code, &[]), 0);
}

// ============================================================================
// x86-64 shift register allocation tests (architecture-specific)
// ============================================================================

#[cfg(target_arch = "x86_64")]
#[test]
fn codegen_shift_regalloc_x86_64() {
    let code = r#"
// d is in RCX, shift count needs RCX - must spill d first
int shift_left(int a, int b, int c, int d, int shift) {
    return d << shift;
}

int shift_right(int a, int b, int c, int d, int shift) {
    return d >> shift;
}

int use_before_after(int a, int b, int c, int d, int shift) {
    int before = d + 1;
    int shifted = d << shift;
    int after = d * 2;
    return before + shifted + after;
}

int multi_shift(int a, int b, int c, int d, int s1, int s2) {
    int left = d << s1;
    int right = d >> s2;
    return left + right;
}

long shift_all(int a, int b, int c, int d, int e, int f, int shift) {
    return ((long)a << shift) + ((long)b << shift) + ((long)c << shift) +
           ((long)d << shift) + ((long)e << shift) + ((long)f << shift);
}

unsigned int shift_unsigned(int a, int b, int c, unsigned int d, int shift) {
    return d >> shift;
}

int shift_expr(int a, int b, int c, int d, int x, int y) {
    return d << (x + y);
}

int shift_loop(int a, int b, int c, int d, int n) {
    int result = d;
    for (int i = 0; i < n; i++) {
        result = result << 1;
    }
    return result;
}

int main(void) {
    // ========== SHIFT 4TH PARAM (returns 1-19) ==========

    // Left shift with 4th param
    if (shift_left(1, 2, 3, 8, 2) != 32) return 1;
    if (shift_left(0, 0, 0, 1, 4) != 16) return 2;
    if (shift_left(0, 0, 0, 255, 0) != 255) return 3;

    // Right shift with 4th param
    if (shift_right(1, 2, 3, 32, 2) != 8) return 4;
    if (shift_right(0, 0, 0, 16, 4) != 1) return 5;
    if (shift_right(0, 0, 0, -16, 2) != -4) return 6;

    // Use before and after shift
    if (use_before_after(0, 0, 0, 5, 2) != 36) return 7;
    if (use_before_after(0, 0, 0, 1, 3) != 12) return 8;

    // Multiple shifts
    if (multi_shift(0, 0, 0, 8, 2, 1) != 36) return 9;
    if (multi_shift(0, 0, 0, 16, 1, 2) != 36) return 10;

    // All register args with shifts
    if (shift_all(1, 1, 1, 1, 1, 1, 2) != 24) return 11;
    if (shift_all(1, 2, 3, 4, 5, 6, 1) != 42) return 12;

    // Unsigned right shift
    unsigned int big = 0x80000000u;
    if (shift_unsigned(0, 0, 0, big, 1) != 0x40000000u) return 13;
    if (shift_unsigned(0, 0, 0, 0xFFFFFFFFu, 4) != 0x0FFFFFFFu) return 14;

    // Shift with expression count
    if (shift_expr(0, 0, 0, 1, 1, 2) != 8) return 15;
    if (shift_expr(0, 0, 0, 4, 0, 1) != 8) return 16;

    // Shift in loop
    if (shift_loop(0, 0, 0, 1, 4) != 16) return 17;
    if (shift_loop(0, 0, 0, 3, 3) != 24) return 18;

    return 0;
}
"#;
    assert_eq!(compile_and_run("codegen_shift_x86_64", code, &[]), 0);
}
