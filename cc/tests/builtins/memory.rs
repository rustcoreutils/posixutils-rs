//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Memory Builtins Mega-Test
//
// Consolidates: alloca, offsetof tests
//

use crate::common::compile_and_run;

// ============================================================================
// Mega-test: Memory builtins (alloca, offsetof)
// ============================================================================

#[test]
fn builtins_memory_mega() {
    let code = r#"
#include <stddef.h>

struct TestStruct {
    char a;     // offset 0
    int b;      // offset 4 (padded)
    char c;     // offset 8
    double d;   // offset 16 (padded)
    short e;    // offset 24
};

struct Packed {
    char a;
    char b;
    char c;
};

struct Nested {
    int x;
    struct {
        int y;
        int z;
    } inner;
    int w;
};

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

int main(void) {
    // ========== ALLOCA SECTION (returns 1-39) ==========
    {
        // Basic allocation
        char *buf = __builtin_alloca(64);
        buf[0] = 'A';
        buf[1] = 'B';
        buf[2] = 'C';
        buf[3] = '\0';
        if (buf[0] != 'A') return 1;
        if (buf[1] != 'B') return 2;
        if (buf[2] != 'C') return 3;

        // Multiple allocations don't interfere
        int *arr1 = __builtin_alloca(sizeof(int) * 4);
        int *arr2 = __builtin_alloca(sizeof(int) * 4);
        arr1[0] = 100; arr1[1] = 200; arr1[2] = 300; arr1[3] = 400;
        arr2[0] = 1000; arr2[1] = 2000; arr2[2] = 3000; arr2[3] = 4000;
        if (arr1[0] != 100) return 4;
        if (arr1[3] != 400) return 5;
        if (arr2[0] != 1000) return 6;
        if (arr2[3] != 4000) return 7;

        // Alignment (16-byte)
        void *p1 = __builtin_alloca(1);
        void *p2 = __builtin_alloca(7);
        void *p3 = __builtin_alloca(16);
        void *p4 = __builtin_alloca(17);
        if (((long)p1 & 0xF) != 0) return 8;
        if (((long)p2 & 0xF) != 0) return 9;
        if (((long)p3 & 0xF) != 0) return 10;
        if (((long)p4 & 0xF) != 0) return 11;

        // Computed size
        if (test_alloca_size(5) != 100) return 12;   // 0+10+20+30+40
        if (test_alloca_size(10) != 450) return 13;  // 0+10+...+90

        // Allocation in loop
        int total = 0;
        for (int i = 0; i < 5; i++) {
            int *p = __builtin_alloca(sizeof(int) * 4);
            p[0] = i;
            p[1] = i * 2;
            p[2] = i * 3;
            p[3] = i * 4;
            total += p[0] + p[1] + p[2] + p[3];
        }
        if (total != 100) return 14;
    }

    // ========== OFFSETOF SECTION (returns 40-69) ==========
    {
        // Basic offsetof
        if (offsetof(struct TestStruct, a) != 0) return 40;
        if (offsetof(struct TestStruct, b) != 4) return 41;
        if (offsetof(struct TestStruct, c) != 8) return 42;
        if (offsetof(struct TestStruct, d) != 16) return 43;
        if (offsetof(struct TestStruct, e) != 24) return 44;

        // Packed struct (no padding between chars)
        if (offsetof(struct Packed, a) != 0) return 45;
        if (offsetof(struct Packed, b) != 1) return 46;
        if (offsetof(struct Packed, c) != 2) return 47;

        // Nested struct
        if (offsetof(struct Nested, x) != 0) return 48;
        if (offsetof(struct Nested, inner) != 4) return 49;
        if (offsetof(struct Nested, inner.y) != 4) return 50;
        if (offsetof(struct Nested, inner.z) != 8) return 51;
        if (offsetof(struct Nested, w) != 12) return 52;

        // __builtin_offsetof (GCC extension)
        if (__builtin_offsetof(struct TestStruct, a) != 0) return 53;
        if (__builtin_offsetof(struct TestStruct, b) != 4) return 54;

        // sizeof struct members via offsetof pattern
        long size_b = offsetof(struct TestStruct, c) - offsetof(struct TestStruct, b);
        if (size_b != 4) return 55;

        // Array in struct
        struct WithArray { int x; int arr[5]; int y; };
        if (offsetof(struct WithArray, arr) != 4) return 56;
        if (offsetof(struct WithArray, y) != 24) return 57;  // 4 + 5*4

        // Compile-time constant usage
        char buf[offsetof(struct TestStruct, d)];  // char buf[16]
        if (sizeof(buf) != 16) return 58;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("builtins_memory_mega", code, &[]), 0);
}

/// `__builtin_object_size` reports the real size of a statically known object.
///
/// Every expectation was taken from gcc on the same source. Before this was
/// implemented the builtin answered `(size_t)-1` -- "unknown" -- for all of
/// them, which is what `_FORTIFY_SOURCE` reads as "nothing to check".
///
/// The type argument matters twice over: bit 0 selects the closest surrounding
/// subobject over the whole object (`s.a` is 10 bytes of its own but 36 bytes
/// to the end of `s`), and bit 1 asks for a minimum instead of a maximum,
/// which only changes the answer when the object is *not* known -- 0 rather
/// than `(size_t)-1`.
#[test]
fn builtins_object_size_of_known_objects() {
    let code = r#"
struct S { char a[10]; int b; char c[20]; };   /* sizeof == 36 */
static char g_arr[64];
static struct S g_s;
static char *opaque(char *p) { return p; }

int main(void)
{
    char local[32];
    struct S ls;

    /* type 0: to the end of the whole object */
    if (__builtin_object_size(g_arr, 0) != 64) return 1;
    if (__builtin_object_size(g_arr + 8, 0) != 56) return 2;
    if (__builtin_object_size(local, 0) != 32) return 3;
    if (__builtin_object_size(g_s.a, 0) != 36) return 4;
    if (__builtin_object_size(g_s.c, 0) != 20) return 5;
    if (__builtin_object_size(&ls.a[2], 0) != 34) return 6;
    if (__builtin_object_size(&g_s, 0) != 36) return 7;

    /* type 1: to the end of the closest surrounding subobject */
    if (__builtin_object_size(g_s.a, 1) != 10) return 8;
    if (__builtin_object_size(g_arr, 1) != 64) return 9;

    /* a known object has the same minimum and maximum size */
    if (__builtin_object_size(g_arr, 2) != 64) return 10;
    if (__builtin_object_size(g_s.a, 3) != 10) return 11;

    /* unknown: -1 for a maximum, 0 for a minimum */
    if (__builtin_object_size(opaque(local), 0) != (unsigned long)-1) return 12;
    if (__builtin_object_size(opaque(local), 1) != (unsigned long)-1) return 13;
    if (__builtin_object_size(opaque(local), 2) != 0) return 14;
    if (__builtin_object_size(opaque(local), 3) != 0) return 15;

    /* a string literal is its bytes plus the terminator */
    if (__builtin_object_size("hello", 0) != 6) return 16;

    return 0;
}
"#;
    assert_eq!(compile_and_run("builtins_object_size", code, &[]), 0);
}
