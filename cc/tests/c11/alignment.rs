//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C11 Alignment Mega-Test
//
// Tests _Alignas, __attribute__((aligned(N))), and stdalign.h
//

use crate::common::{compile_and_run, compile_and_run_optimized};

#[test]
fn c11_alignment_mega() {
    let code = r#"
#include <stdint.h>
#include <stddef.h>

// ============================================================================
// Section 1: _Alignas on globals
// ============================================================================

_Alignas(16) int g_aligned16;
_Alignas(32) int g_aligned32;
_Alignas(64) char g_aligned64;

int test_global_alignment(void) {
    if ((uintptr_t)&g_aligned16 % 16 != 0) return 1;
    if ((uintptr_t)&g_aligned32 % 32 != 0) return 2;
    if ((uintptr_t)&g_aligned64 % 64 != 0) return 3;
    return 0;
}

// ============================================================================
// Section 2: _Alignas on locals
// ============================================================================

int test_local_alignment_16(void) {
    _Alignas(16) int local16;
    local16 = 42;
    if ((uintptr_t)&local16 % 16 != 0) return 10;
    if (local16 != 42) return 11;
    return 0;
}

// ============================================================================
// Section 3: _Alignas on struct members
// ============================================================================

struct AlignedMembers {
    char a;
    _Alignas(16) int b;
    char c;
};

int test_struct_member_alignment(void) {
    // b should be at offset 16 (aligned to 16)
    if (offsetof(struct AlignedMembers, b) != 16) return 30;
    // struct alignment should be 16 (largest member alignment)
    if (_Alignof(struct AlignedMembers) != 16) return 31;
    // struct size should be padded to multiple of 16
    if (sizeof(struct AlignedMembers) % 16 != 0) return 32;
    return 0;
}

// ============================================================================
// Section 4: _Alignas(type) form
// ============================================================================

int test_alignas_type(void) {
    _Alignas(double) char buf[8];
    if ((uintptr_t)&buf % _Alignof(double) != 0) return 40;
    return 0;
}

// ============================================================================
// Section 5: _Alignas(0) has no effect
// ============================================================================

int test_alignas_zero(void) {
    _Alignas(0) int normal_int;
    normal_int = 99;
    // Should compile and work normally
    if (normal_int != 99) return 50;
    return 0;
}

// ============================================================================
// Section 6: Multiple _Alignas — strictest wins
// ============================================================================

int test_multiple_alignas(void) {
    _Alignas(8) _Alignas(16) int multi;
    multi = 77;
    if ((uintptr_t)&multi % 16 != 0) return 60;
    if (multi != 77) return 61;
    return 0;
}

// ============================================================================
// Section 7: __attribute__((aligned(N))) on locals
// ============================================================================

int test_attr_aligned_local(void) {
    int __attribute__((aligned(16))) attr16;
    attr16 = 123;
    if ((uintptr_t)&attr16 % 16 != 0) return 70;
    if (attr16 != 123) return 71;
    return 0;
}

// ============================================================================
// Section 8: __attribute__((aligned(N))) on globals
// ============================================================================

int __attribute__((aligned(32))) g_attr_aligned32;

int test_attr_aligned_global(void) {
    g_attr_aligned32 = 456;
    if ((uintptr_t)&g_attr_aligned32 % 32 != 0) return 80;
    if (g_attr_aligned32 != 456) return 81;
    return 0;
}

// ============================================================================
// Section 9: __attribute__((aligned(N))) on struct members
// ============================================================================

struct AttrAlignedMembers {
    char a;
    int __attribute__((aligned(16))) b;
    char c;
};

int test_attr_aligned_struct_member(void) {
    if (offsetof(struct AttrAlignedMembers, b) != 16) return 90;
    if (_Alignof(struct AttrAlignedMembers) != 16) return 91;
    return 0;
}

// ============================================================================
// Section 10: __attribute__((aligned(N))) on struct tag
// ============================================================================

struct __attribute__((aligned(32))) AlignedStruct {
    int x;
    int y;
};

int test_attr_aligned_struct_tag(void) {
    if (_Alignof(struct AlignedStruct) != 32) return 100;
    if (sizeof(struct AlignedStruct) != 32) return 101;
    return 0;
}

// ============================================================================
// Section 11: __attribute__((aligned)) with no argument (defaults to 16)
// ============================================================================

int test_attr_aligned_default(void) {
    int __attribute__((aligned)) def_aligned;
    def_aligned = 789;
    if ((uintptr_t)&def_aligned % 16 != 0) return 110;
    if (def_aligned != 789) return 111;
    return 0;
}

// ============================================================================
// Section 12: __attribute__((aligned(N))) on typedef
// ============================================================================

typedef int __attribute__((aligned(16))) aligned_int_t;

aligned_int_t g_typedef_aligned;

int test_attr_aligned_typedef(void) {
    if (_Alignof(aligned_int_t) != 16) return 120;
    if ((uintptr_t)&g_typedef_aligned % 16 != 0) return 121;

    aligned_int_t local_typedef;
    local_typedef = 321;
    if ((uintptr_t)&local_typedef % 16 != 0) return 122;
    if (local_typedef != 321) return 123;
    return 0;
}

// ============================================================================
// Section 13: stdalign.h macros
// ============================================================================

#include <stdalign.h>

int test_stdalign(void) {
    alignas(16) int stdalign_var;
    stdalign_var = 555;
    if ((uintptr_t)&stdalign_var % 16 != 0) return 130;
    if (stdalign_var != 555) return 131;
    if (alignof(int) != _Alignof(int)) return 132;
    return 0;
}

// ============================================================================
// Section 14: Callee-saved pressure test
// Ensure 16-byte alignment works even with many callee-saved registers
// ============================================================================

// Use volatile to force the compiler to actually allocate these
volatile int sink;

int test_callee_saved_pressure(void) {
    _Alignas(16) int aligned_var;
    // Force many variables to be live across a function call
    // to exhaust callee-saved registers
    int a = 1, b = 2, c = 3, d = 4, e = 5, f = 6;
    int g = 7, h = 8;
    aligned_var = a + b + c + d + e + f + g + h;
    // Force variables to survive across an external call
    sink = aligned_var;
    // Check that aligned_var is actually 16-byte aligned
    if ((uintptr_t)&aligned_var % 16 != 0) return 140;
    if (aligned_var != 36) return 141;
    return 0;
}

// ============================================================================
// Section 15: _Alignas(32) on local — exercises dynamic alignment (> 16)
// ============================================================================

int test_local_alignment_32(void) {
    _Alignas(32) int local32;
    local32 = 999;
    if ((uintptr_t)&local32 % 32 != 0) return 150;
    if (local32 != 999) return 151;
    return 0;
}

// ============================================================================
// Section 16: _Alignas(64) on local — exercises dynamic alignment
// ============================================================================

int test_local_alignment_64(void) {
    _Alignas(64) char buf[64];
    buf[0] = 'A';
    buf[63] = 'Z';
    if ((uintptr_t)buf % 64 != 0) return 160;
    if (buf[0] != 'A') return 161;
    if (buf[63] != 'Z') return 162;
    return 0;
}

// ============================================================================
// Section 17: Trailing attribute on struct
// ============================================================================

struct TrailingAligned {
    int x;
} __attribute__((aligned(32)));

int test_trailing_struct_aligned(void) {
    if (_Alignof(struct TrailingAligned) != 32) return 170;
    if (sizeof(struct TrailingAligned) != 32) return 171;
    return 0;
}

// ============================================================================
// Section 18: Combined _Alignas + __attribute__((aligned)) on same declaration
// Strictest (largest) wins across both mechanisms
// ============================================================================

int test_combined_alignas_and_attr(void) {
    _Alignas(16) int __attribute__((aligned(32))) combined;
    combined = 888;
    if ((uintptr_t)&combined % 32 != 0) return 180;
    if (combined != 888) return 181;

    // Reverse order — _Alignas larger
    _Alignas(32) int __attribute__((aligned(16))) combined2;
    combined2 = 777;
    if ((uintptr_t)&combined2 % 32 != 0) return 182;
    if (combined2 != 777) return 183;
    return 0;
}

// ============================================================================
// Section 19: Aligned typedef used as struct member (chained propagation)
// ============================================================================

typedef int __attribute__((aligned(16))) member_aligned_int_t;

struct TypedefMemberStruct {
    char a;
    member_aligned_int_t b;
    char c;
};

int test_typedef_as_struct_member(void) {
    // b should be at offset 16 (typedef alignment propagates to member)
    if (offsetof(struct TypedefMemberStruct, b) != 16) return 190;
    // struct alignment should be 16 (from typedef member)
    if (_Alignof(struct TypedefMemberStruct) != 16) return 191;
    // struct size padded to multiple of 16
    if (sizeof(struct TypedefMemberStruct) % 16 != 0) return 192;
    return 0;
}

// ============================================================================
// Section 20: Non-power-of-2 __attribute__((aligned(3))) silently ignored
// GCC errors on this; pcc silently ignores (skips non-power-of-2)
// The variable should get natural alignment, not 3
// ============================================================================

int test_nonpow2_aligned_ignored(void) {
    int __attribute__((aligned(3))) nonpow2;
    nonpow2 = 12345;
    // Should still be usable — alignment request was silently dropped
    if (nonpow2 != 12345) return 200;
    // Alignment should be natural (4 for int), not 3
    if (_Alignof(int) != 4) return 201;
    return 0;
}

// ============================================================================
// main
// ============================================================================

int main(void) {
    int r;
    if ((r = test_global_alignment()) != 0) return r;
    if ((r = test_local_alignment_16()) != 0) return r;
    if ((r = test_struct_member_alignment()) != 0) return r;
    if ((r = test_alignas_type()) != 0) return r;
    if ((r = test_alignas_zero()) != 0) return r;
    if ((r = test_multiple_alignas()) != 0) return r;
    if ((r = test_attr_aligned_local()) != 0) return r;
    if ((r = test_attr_aligned_global()) != 0) return r;
    if ((r = test_attr_aligned_struct_member()) != 0) return r;
    if ((r = test_attr_aligned_struct_tag()) != 0) return r;
    if ((r = test_attr_aligned_default()) != 0) return r;
    if ((r = test_attr_aligned_typedef()) != 0) return r;
    if ((r = test_stdalign()) != 0) return r;
    if ((r = test_callee_saved_pressure()) != 0) return r;
    if ((r = test_local_alignment_32()) != 0) return r;
    if ((r = test_local_alignment_64()) != 0) return r;
    if ((r = test_trailing_struct_aligned()) != 0) return r;
    if ((r = test_combined_alignas_and_attr()) != 0) return r;
    if ((r = test_typedef_as_struct_member()) != 0) return r;
    if ((r = test_nonpow2_aligned_ignored()) != 0) return r;
    return 0;
}
"#;
    assert_eq!(compile_and_run("c11_alignment_mega", code, &[]), 0);
}

#[test]
fn c11_alignment_mega_optimized() {
    let code = r#"
#include <stdint.h>
#include <stddef.h>

// Test alignment at -O2 to verify it works with optimizations

_Alignas(16) int g_opt_aligned16;
_Alignas(32) int g_opt_aligned32;

typedef int __attribute__((aligned(16))) opt_aligned_int_t;

struct __attribute__((aligned(32))) OptAlignedStruct {
    int x;
    int y;
};

int main(void) {
    // Global alignment
    if ((uintptr_t)&g_opt_aligned16 % 16 != 0) return 1;
    if ((uintptr_t)&g_opt_aligned32 % 32 != 0) return 2;

    // Local 16-byte alignment
    _Alignas(16) int local16;
    local16 = 42;
    if ((uintptr_t)&local16 % 16 != 0) return 3;
    if (local16 != 42) return 4;

    // Local 32-byte alignment (dynamic)
    _Alignas(32) int local32;
    local32 = 99;
    if ((uintptr_t)&local32 % 32 != 0) return 5;
    if (local32 != 99) return 6;

    // Typedef alignment
    if (_Alignof(opt_aligned_int_t) != 16) return 7;

    // Struct tag alignment
    if (_Alignof(struct OptAlignedStruct) != 32) return 8;
    if (sizeof(struct OptAlignedStruct) != 32) return 9;

    // __attribute__((aligned(N))) on local
    int __attribute__((aligned(16))) attr_local;
    attr_local = 777;
    if ((uintptr_t)&attr_local % 16 != 0) return 10;
    if (attr_local != 777) return 11;

    return 0;
}
"#;
    assert_eq!(compile_and_run_optimized("c11_alignment_opt", code), 0);
}

/// aarch64: test over-aligned locals (exercises x19 aligned base register path)
#[cfg(target_arch = "aarch64")]
#[test]
fn c11_alignment_aarch64_over_aligned() {
    let code = r#"
#include <stdint.h>

int main(void) {
    // 32-byte alignment — forces over-allocation + x19 aligned base
    _Alignas(32) int local32;
    local32 = 111;
    if ((uintptr_t)&local32 % 32 != 0) return 1;
    if (local32 != 111) return 2;

    // 64-byte alignment
    _Alignas(64) char buf[64];
    buf[0] = 'X';
    buf[63] = 'Y';
    if ((uintptr_t)buf % 64 != 0) return 3;
    if (buf[0] != 'X') return 4;
    if (buf[63] != 'Y') return 5;

    // 128-byte alignment
    _Alignas(128) int local128;
    local128 = 222;
    if ((uintptr_t)&local128 % 128 != 0) return 6;
    if (local128 != 222) return 7;

    // Mix of normal and over-aligned locals in the same function
    int normal = 333;
    _Alignas(32) int aligned = 444;
    if (normal != 333) return 8;
    if (aligned != 444) return 9;
    if ((uintptr_t)&aligned % 32 != 0) return 10;

    return 0;
}
"#;
    assert_eq!(compile_and_run("c11_alignment_aarch64_over", code, &[]), 0);
}
