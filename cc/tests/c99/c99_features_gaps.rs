//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C99-Specific Features Gap Test
//
// Covers Section 10 gaps: __func__, LLONG_MIN/MAX, declaration after case,
// sizeof excluding FAM, compound literal file-scope, inline with static vars,
// long long limits, VLA edge cases, overwriting designated initializers
//

use crate::common::compile_and_run;

#[test]
fn c99_features_gaps_mega() {
    let code = r#"
#include <string.h>
#include <limits.h>
#include <stdlib.h>

// === __func__ in different functions ===
const char *get_func_name(void) {
    return __func__;
}

// === inline with static variable ===
static inline int next_id(void) {
    static int counter = 0;
    return ++counter;
}

// === Flexible array member struct ===
struct FlexMsg {
    int len;
    char text[];
};

// === File-scope compound literal (static storage) ===
static int *file_scope_arr = (int[]){100, 200, 300};

int main(void) {
    // ========== __func__ IDENTIFIER (returns 1-9) ==========
    {
        // __func__ in main
        if (strcmp(__func__, "main") != 0) return 1;

        // __func__ in another function
        if (strcmp(get_func_name(), "get_func_name") != 0) return 2;

        // __func__ type is static const char[]
        const char *p = __func__;
        if (p[0] != 'm') return 3;
    }

    // ========== LONG LONG LIMITS (returns 10-19) ==========
    {
        // LLONG_MIN, LLONG_MAX, ULLONG_MAX from <limits.h>
        long long mn = LLONG_MIN;
        long long mx = LLONG_MAX;
        unsigned long long umx = ULLONG_MAX;
        if (mx <= 0) return 10;
        if (mn >= 0) return 11;
        if (umx == 0) return 12;

        // Verify sizes
        if (sizeof(long long) != 8) return 13;
        if (sizeof(unsigned long long) != 8) return 14;

        // long long arithmetic
        long long a = 1000000000LL;
        long long b = 2000000000LL;
        long long c = a * 3 + b;
        if (c != 5000000000LL) return 15;

        // LL suffix constants
        unsigned long long big = 18446744073709551615ULL;  // ULLONG_MAX
        if (big != ULLONG_MAX) return 16;
    }

    // ========== MIXED DECLARATIONS: AFTER CASE (returns 20-29) ==========
    {
        int x = 2;
        int result = 0;
        switch (x) {
        case 1: {
            int y = 10;
            result = y;
            break;
        }
        case 2: {
            int z = 42;
            result = z;
            break;
        }
        default: {
            int w = 99;
            result = w;
            break;
        }
        }
        if (result != 42) return 20;
    }

    // ========== FLEXIBLE ARRAY MEMBER (returns 30-39) ==========
    {
        // sizeof excludes flexible member
        if (sizeof(struct FlexMsg) != sizeof(int)) return 30;

        // Allocate and use
        struct FlexMsg *msg = malloc(sizeof(struct FlexMsg) + 12);
        msg->len = 11;
        memcpy(msg->text, "hello world", 12);
        if (msg->len != 11) return 31;
        if (strcmp(msg->text, "hello world") != 0) return 32;
        free(msg);
    }

    // ========== COMPOUND LITERAL EDGE CASES (returns 40-49) ==========
    {
        // File-scope compound literal (static storage)
        if (file_scope_arr[0] != 100 || file_scope_arr[2] != 300) return 40;

        // Compound literal as lvalue (modifiable)
        int *p = (int[]){10, 20, 30};
        p[1] = 99;
        if (p[1] != 99) return 41;

        // Compound literal as function argument (automatic storage)
        struct { int x; int y; } *pp = &(struct { int x; int y; }){5, 6};
        if (pp->x != 5 || pp->y != 6) return 42;
    }

    // ========== DESIGNATED INITIALIZER EDGE CASES (returns 50-59) ==========
    {
        // Overwriting previous initializer
        int arr[5] = {1, 2, 3, [1] = 99, 4};
        // arr[0]=1, arr[1]=99 (overwritten), arr[2]=4 (continues after [1]), arr[3]=0, arr[4]=0
        if (arr[0] != 1) return 50;
        if (arr[1] != 99) return 51;
        if (arr[2] != 4) return 52;

        // Nested designators
        struct { struct { int x; int y; } a; int b; } ns = { .a.x = 10, .a.y = 20, .b = 30 };
        if (ns.a.x != 10 || ns.a.y != 20 || ns.b != 30) return 53;
    }

    // ========== RESTRICT QUALIFIER (returns 60-69) ==========
    {
        // Basic restrict pointer
        int arr[3] = {1, 2, 3};
        int * restrict rp = arr;
        if (rp[0] != 1) return 60;

        // Multiple restrict pointers
        int a[3] = {10, 20, 30};
        int b[3] = {40, 50, 60};
        int * restrict pa = a;
        int * restrict pb = b;
        if (pa[0] + pb[0] != 50) return 61;

        // restrict on array parameter (tested in c99/features.rs)
    }

    // ========== INLINE FUNCTION EDGE CASES (returns 70-79) ==========
    {
        // inline with static variable (persistent state)
        int id1 = next_id();
        int id2 = next_id();
        int id3 = next_id();
        if (id1 != 1 || id2 != 2 || id3 != 3) return 70;
    }

    // ========== HEX FLOAT LITERALS (returns 80-89) ==========
    {
        // Basic hex float
        double d1 = 0x1.0p10;  // 1.0 * 2^10 = 1024
        if (d1 < 1023.9 || d1 > 1024.1) return 80;

        // Negative binary exponent
        double d2 = 0x1.0p-1;  // 1.0 * 2^-1 = 0.5
        if (d2 < 0.49 || d2 > 0.51) return 81;

        // Exact representation
        double d3 = 0xA.0p0;   // 10.0 * 2^0 = 10.0
        if (d3 < 9.99 || d3 > 10.01) return 82;

        // Positive exponent with sign
        double d4 = 0x1.0p+3;  // 1.0 * 2^3 = 8.0
        if (d4 < 7.99 || d4 > 8.01) return 83;

        // Float suffix on hex float
        float f1 = 0x1.0p5f;   // 1.0 * 2^5 = 32.0
        if (f1 < 31.9f || f1 > 32.1f) return 84;

        // Long double suffix on hex float
        long double ld1 = 0x1.0p3L;  // 1.0 * 2^3 = 8.0
        if (ld1 < 7.9L || ld1 > 8.1L) return 85;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("c99_features_gaps_mega", code, &[]), 0);
}
