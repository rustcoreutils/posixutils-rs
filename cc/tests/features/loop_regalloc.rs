//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for register allocation in loops
//
// These tests verify that loop-carried values (values defined before a loop
// and used within it) are correctly preserved across loop iterations.
//
// The bug this tests for: When there are no function calls in a loop body,
// the register allocator would use caller-saved registers for loop-carried
// values. Under high register pressure, these values could be clobbered
// across iterations. The fix adds an `in_loop` flag to LiveInterval that
// forces callee-saved registers for such values.
//

use crate::common::compile_and_run;

/// Basic loop-carried value test
/// Value defined before loop, used inside loop
#[test]
fn loop_carried_basic() {
    let code = r#"
int test(int n, int multiplier) {
    int sum = 0;
    for (int i = 0; i < n; i++) {
        sum += multiplier;  // multiplier must persist across iterations
    }
    return sum;
}

int main(void) {
    // 5 iterations * 3 = 15
    if (test(5, 3) != 15) return 1;
    // 10 iterations * 7 = 70
    if (test(10, 7) != 70) return 2;
    // 0 iterations = 0
    if (test(0, 100) != 0) return 3;
    return 0;
}
"#;
    assert_eq!(compile_and_run("loop_basic", code), 0);
}

/// Multiple loop-carried values
/// Tests that multiple values can persist across iterations
#[test]
fn loop_carried_multiple_values() {
    let code = r#"
int test(int n, int a, int b, int c) {
    int sum = 0;
    for (int i = 0; i < n; i++) {
        // All three values must persist
        sum += a + b + c;
    }
    return sum;
}

int main(void) {
    // 4 iterations * (1+2+3) = 24
    if (test(4, 1, 2, 3) != 24) return 1;
    // 3 iterations * (10+20+30) = 180
    if (test(3, 10, 20, 30) != 180) return 2;
    return 0;
}
"#;
    assert_eq!(compile_and_run("loop_multiple", code), 0);
}

/// Loop with nested conditionals (no function calls)
/// This pattern is similar to what caused the zlib bug
#[test]
fn loop_nested_conditionals() {
    let code = r#"
int test(int n, int threshold) {
    int count_above = 0;
    int count_below = 0;
    for (int i = 0; i < n; i++) {
        if (i >= threshold) {
            count_above++;
        } else {
            count_below++;
        }
    }
    return count_above * 100 + count_below;
}

int main(void) {
    // n=10, threshold=5: above=5, below=5 => 505
    if (test(10, 5) != 505) return 1;
    // n=10, threshold=3: above=7, below=3 => 703
    if (test(10, 3) != 703) return 2;
    // n=10, threshold=10: above=0, below=10 => 10
    if (test(10, 10) != 10) return 3;
    return 0;
}
"#;
    assert_eq!(compile_and_run("loop_nested_cond", code), 0);
}

/// Complex loop with high register pressure (zlib-like)
/// This test mimics the structure that caused the zlib compress_block bug:
/// - Multiple values competing for registers
/// - Nested conditionals inside loop body
/// - Array accesses with computed indices
/// - Bit manipulation operations
/// - NO external function calls (key condition for the bug)
#[test]
fn loop_high_register_pressure() {
    let code = r#"
// Lookup tables to create register pressure
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

// This function mimics zlib's compress_block structure
// It processes data in 3-byte chunks with conditional paths
int process_data(unsigned char *data, int len, int threshold) {
    int result = 0;
    int count = 0;

    while (count < len) {
        // Read 3 bytes (like zlib reads dist low, dist high, lc)
        int a = data[count] & 0xff;
        int b = (data[count + 1] & 0xff) << 8;
        int c = data[count + 2];

        // Conditional based on third byte (like dist == 0 check)
        if (c == 0) {
            // Literal path
            result += lookup1[a];
        } else {
            // Match path with multiple lookups
            int code = lookup2[c];
            result += code + extra_bits[c];
            // Use b in computation (distance code)
            int dist_code = (b >> 8) & 0x1F;
            result += dist_code;
        }
        count += 3;
    }
    return result;
}

int main(void) {
    init_tables();

    // Test data with mix of c==0 and c!=0
    unsigned char data1[30];
    for (int i = 0; i < 30; i += 3) {
        data1[i] = i;          // a
        data1[i + 1] = i + 1;  // b low
        data1[i + 2] = 0;      // c = 0 (literal path)
    }
    int r1 = process_data(data1, 30, 128);
    // Expected: sum of lookup1[0, 3, 6, 9, 12, 15, 18, 21, 24, 27]
    // = 0 + 3 + 6 + 9 + 12 + 15 + 18 + 21 + 24 + 27 = 135
    if (r1 != 135) return 1;

    // Test data with c != 0 (match path)
    unsigned char data2[30];
    for (int i = 0; i < 30; i += 3) {
        data2[i] = i;          // a
        data2[i + 1] = 0x10;   // b = 0x10 << 8 = 0x1000
        data2[i + 2] = 1;      // c = 1 (match path)
    }
    int r2 = process_data(data2, 30, 128);
    // For each iteration:
    // lookup2[1] = 2, extra_bits[1] = 1
    // dist_code = (0x1000 >> 8) & 0x1F = 0x10 = 16
    // Total per iteration: 2 + 1 + 16 = 19
    // 10 iterations * 19 = 190
    if (r2 != 190) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("loop_pressure", code), 0);
}

/// Do-while loop (like zlib's compress_block uses)
#[test]
fn loop_do_while() {
    let code = r#"
int test(int *arr, int len, int target) {
    int count = 0;
    int i = 0;
    if (len > 0) do {
        if (arr[i] == target) {
            count++;
        }
        i++;
    } while (i < len);
    return count;
}

int main(void) {
    int arr[10] = {1, 2, 3, 2, 5, 2, 7, 8, 2, 10};
    // Count occurrences of 2
    if (test(arr, 10, 2) != 4) return 1;
    // Count occurrences of 5
    if (test(arr, 10, 5) != 1) return 2;
    // Count occurrences of 99 (not found)
    if (test(arr, 10, 99) != 0) return 3;
    return 0;
}
"#;
    assert_eq!(compile_and_run("loop_do_while", code), 0);
}

/// Loop with bit manipulation (common in compression code)
#[test]
fn loop_bit_manipulation() {
    let code = r#"
int encode_bits(unsigned int *data, int len) {
    unsigned int buffer = 0;
    int bits_in_buffer = 0;
    int output_count = 0;

    for (int i = 0; i < len; i++) {
        unsigned int value = data[i] & 0xFF;
        int bits_to_add = 8;

        // Add bits to buffer
        buffer |= (value << bits_in_buffer);
        bits_in_buffer += bits_to_add;

        // Flush if buffer has 16+ bits
        while (bits_in_buffer >= 16) {
            output_count++;
            buffer >>= 16;
            bits_in_buffer -= 16;
        }
    }

    // Count remaining partial buffer
    if (bits_in_buffer > 0) {
        output_count++;
    }

    return output_count;
}

int main(void) {
    unsigned int data1[4] = {0x12, 0x34, 0x56, 0x78};
    // 4 bytes = 32 bits, should produce 2 outputs (16-bit each)
    if (encode_bits(data1, 4) != 2) return 1;

    unsigned int data2[3] = {0xAA, 0xBB, 0xCC};
    // 3 bytes = 24 bits, should produce 2 outputs (16 + 8 remaining)
    if (encode_bits(data2, 3) != 2) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("loop_bits", code), 0);
}

/// Nested loops with multiple loop-carried values
#[test]
fn loop_nested_loops() {
    let code = r#"
int matrix_sum(int rows, int cols, int fill_value) {
    int sum = 0;
    for (int i = 0; i < rows; i++) {
        int row_sum = 0;
        for (int j = 0; j < cols; j++) {
            // fill_value and i must persist through inner loop
            row_sum += fill_value + i;
        }
        sum += row_sum;
    }
    return sum;
}

int main(void) {
    // 3x4 matrix, fill=10
    // row0: 4 * (10+0) = 40
    // row1: 4 * (10+1) = 44
    // row2: 4 * (10+2) = 48
    // total = 132
    if (matrix_sum(3, 4, 10) != 132) return 1;

    // 2x3 matrix, fill=5
    // row0: 3 * (5+0) = 15
    // row1: 3 * (5+1) = 18
    // total = 33
    if (matrix_sum(2, 3, 5) != 33) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("loop_nested", code), 0);
}
