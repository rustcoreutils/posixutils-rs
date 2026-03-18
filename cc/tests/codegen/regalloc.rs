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

// ============================================================================
// Stack slot reuse tests
// ============================================================================

#[test]
fn regalloc_stack_slot_reuse() {
    let code = r#"
int switch_reuse(int x) {
    switch (x) {
    case 0: { int a=10,b=20,c=30,d=40; return a+b+c+d; }
    case 1: { int e=1,f=2,g=3,h=4; return e*f*g*h; }
    case 2: { int p=100,q=200; return p-q; }
    case 3: { int a=5,b=6,c=7,d=8; return a*b+c*d; }
    case 4: { int x1=11,x2=22,x3=33,x4=44; return x1+x2+x3+x4; }
    case 5: { int a=3,b=4; return a*b*a*b; }
    case 6: { int m=1,n=2,o=3,p=4; return (m+n)*(o+p); }
    case 7: { int a=50,b=25,c=10; return a-b+c; }
    case 8: { int a=7,b=8,c=9,d=10; return a+b+c+d; }
    case 9: { int a=2,b=3,c=4,d=5; return a*b+c*d; }
    case 10: { int a=100,b=50,c=25,d=12; return a-b-c-d; }
    case 11: { int a=6,b=7; return a*b; }
    default: return -1;
    }
}

int main(void) {
    if (switch_reuse(0) != 100) return 1;
    if (switch_reuse(1) != 24) return 2;
    if (switch_reuse(2) != -100) return 3;
    if (switch_reuse(3) != 86) return 4;
    if (switch_reuse(4) != 110) return 5;
    if (switch_reuse(5) != 144) return 6;
    if (switch_reuse(6) != 21) return 7;
    if (switch_reuse(7) != 35) return 8;
    if (switch_reuse(8) != 34) return 9;
    if (switch_reuse(9) != 26) return 10;
    if (switch_reuse(10) != 13) return 11;
    if (switch_reuse(11) != 42) return 12;
    if (switch_reuse(99) != -1) return 13;
    return 0;
}
"#;
    assert_eq!(compile_and_run("regalloc_stack_slot_reuse", code, &[]), 0);
}

#[test]
fn regalloc_no_reuse_overlapping() {
    let code = r#"
int overlapping(int x) {
    int a = x + 1;
    int b = x + 2;
    int c = a + b;
    return c;
}

int main(void) {
    if (overlapping(10) != 23) return 1;
    if (overlapping(0) != 3) return 2;
    if (overlapping(-5) != -7) return 3;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("regalloc_no_reuse_overlapping", code, &[]),
        0
    );
}

// ============================================================================
// Sym (named local) stack coloring tests
// ============================================================================

#[test]
fn regalloc_sym_stack_coloring() {
    let code = r#"
int switch_locals(int x) {
    switch (x) {
    case 0:  { int a=1,b=2,c=3,d=4; return a+b+c+d; }
    case 1:  { int a=10,b=20,c=30,d=40; return a+b+c+d; }
    case 2:  { int a=5,b=6,c=7,d=8; return a*b+c*d; }
    case 3:  { int a=100,b=50,c=25,d=12; return a-b-c-d; }
    case 4:  { int a=3,b=4,c=5,d=6; return (a+b)*(c+d); }
    case 5:  { int a=7,b=8,c=9,d=10; return a*d+b*c; }
    case 6:  { int a=2,b=3,c=4,d=5; return a*b*c*d; }
    case 7:  { int a=50,b=25,c=10,d=5; return a/d+b/d+c/d; }
    case 8:  { int a=11,b=22,c=33,d=44; return a+b+c+d; }
    case 9:  { int a=1,b=1,c=1,d=1; return a+b+c+d; }
    case 10: { int a=99,b=1,c=0,d=0; return a+b+c+d; }
    case 11: { int a=6,b=7,c=8,d=9; return a*b-c*d; }
    case 12: { int a=15,b=3,c=20,d=4; return a/b+c/d; }
    case 13: { int a=1,b=2,c=3,d=4; return d*d+c*c+b*b+a*a; }
    case 14: { int a=10,b=20,c=30,d=40; return d-c-b-a; }
    case 15: { double a=1.5,b=2.5; return (int)(a+b); }
    case 16: { long long a=1000000000LL,b=2000000000LL; return (int)((a+b)/100000000LL); }
    default: return -1;
    }
}

int main(void) {
    if (switch_locals(0) != 10) return 1;
    if (switch_locals(1) != 100) return 2;
    if (switch_locals(2) != 86) return 3;
    if (switch_locals(3) != 13) return 4;
    if (switch_locals(4) != 77) return 5;
    if (switch_locals(5) != 142) return 6;
    if (switch_locals(6) != 120) return 7;
    if (switch_locals(7) != 17) return 8;
    if (switch_locals(8) != 110) return 9;
    if (switch_locals(9) != 4) return 10;
    if (switch_locals(10) != 100) return 11;
    if (switch_locals(11) != -30) return 12;
    if (switch_locals(12) != 10) return 13;
    if (switch_locals(13) != 30) return 14;
    if (switch_locals(14) != -20) return 15;
    if (switch_locals(15) != 4) return 16;
    if (switch_locals(16) != 30) return 17;
    if (switch_locals(99) != -1) return 18;
    return 0;
}
"#;
    assert_eq!(compile_and_run("regalloc_sym_stack_coloring", code, &[]), 0);
}

#[test]
fn regalloc_sym_no_reuse_addr_taken() {
    let code = r#"
int test_addr_taken(int val) {
    int x = val;
    int *p = &x;
    *p += 10;
    return x;
}

int test_addr_taken_across_call(int val) {
    int x = val;
    int *p = &x;
    // Call a function to ensure p's target survives
    int y = test_addr_taken(*p);
    return x + y;
}

int main(void) {
    if (test_addr_taken(5) != 15) return 1;
    if (test_addr_taken(0) != 10) return 2;
    if (test_addr_taken(-10) != 0) return 3;
    if (test_addr_taken_across_call(5) != 20) return 4;
    if (test_addr_taken_across_call(0) != 10) return 5;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("regalloc_sym_no_reuse_addr_taken", code, &[]),
        0
    );
}

#[test]
fn regalloc_switch_in_loop() {
    let code = r#"
int side_effect(int x) { return x + 1; }

int main(void) {
    int total = 0;
    for (int iter = 0; iter < 3; iter++) {
        switch (iter) {
        case 0: {
            int a = 10, b = 20, c = 30;
            int tmp = side_effect(a) + b + c;
            total += tmp;
            break;
        }
        case 1: {
            int x = 5, y = 15, z = 25;
            int tmp = side_effect(x) * y + z;
            total += tmp;
            break;
        }
        case 2: {
            int p = 100, q = 50;
            int tmp = side_effect(p) - q;
            total += tmp;
            break;
        }
        }
    }
    // case 0: 11+20+30=61, case 1: 6*15+25=115, case 2: 101-50=51
    // total: 61+115+51=227
    if (total != 227) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("regalloc_switch_in_loop", code, &[]), 0);
}

#[test]
fn regalloc_write_only_locals() {
    let code = r#"
int side_effect(int x) { return x + 1; }

int main(void) {
    int a = 42;
    int b = 10;
    int c = side_effect(b);
    if (c != 11) return 1;

    int d = 99;
    int e = 20;
    int f = side_effect(e);
    if (f != 21) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("regalloc_write_only_locals", code, &[]), 0);
}

#[test]
fn regalloc_cross_block_liveness() {
    let code = r#"
int main(void) {
    int x = 42;
    int result;
    if (x > 0) {
        result = x + 10;
    } else {
        result = x - 10;
    }
    if (result != 52) return 1;

    int a = 5, b = 10;
    int sum;
    if (a < b) {
        int temp = a;
        a = b;
        b = temp;
        sum = a + b;
    } else {
        sum = a - b;
    }
    if (sum != 15) return 2;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("regalloc_cross_block_liveness", code, &[]),
        0
    );
}

#[test]
fn regalloc_nested_loops() {
    let code = r#"
int main(void) {
    int outer_sum = 0;
    for (int i = 0; i < 4; i++) {
        int inner_sum = 0;
        for (int j = 0; j < 3; j++) {
            inner_sum += i + j;
        }
        outer_sum += inner_sum;
    }
    // i=0: 0+1+2=3, i=1: 1+2+3=6, i=2: 2+3+4=9, i=3: 3+4+5=12
    // total: 3+6+9+12=30
    if (outer_sum != 30) return 1;

    // Nested with loop-carried at both levels
    int total = 1;
    for (int i = 0; i < 3; i++) {
        int factor = 1;
        for (int j = 0; j <= i; j++) {
            factor *= 2;
        }
        total *= factor;
    }
    // i=0: factor=2, total=2
    // i=1: factor=4, total=8
    // i=2: factor=8, total=64
    if (total != 64) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("regalloc_nested_loops", code, &[]), 0);
}

#[test]
fn regalloc_sym_array_no_reuse() {
    let code = r#"
int test_local_array(int n) {
    int arr[4];
    arr[0] = n;
    arr[1] = n * 2;
    arr[2] = n * 3;
    arr[3] = n * 4;
    return arr[0] + arr[1] + arr[2] + arr[3];
}

int main(void) {
    if (test_local_array(1) != 10) return 1;
    if (test_local_array(5) != 50) return 2;
    if (test_local_array(0) != 0) return 3;
    if (test_local_array(-3) != -30) return 4;
    return 0;
}
"#;
    assert_eq!(compile_and_run("regalloc_sym_array_no_reuse", code, &[]), 0);
}
