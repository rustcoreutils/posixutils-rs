//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C99 Initializers Mega-Test
//
// Consolidates: designated initializers, compound literals tests
//

use crate::common::compile_and_run;

// ============================================================================
// Mega-test: C99 initializers (designated init, compound literals)
// ============================================================================

#[test]
fn c99_initializers_mega() {
    let code = r#"
struct Point { int x; int y; int z; };
struct Rect { struct Point tl; struct Point br; };

int sum_point(struct Point p) {
    return p.x + p.y + p.z;
}

int main(void) {
    // ========== DESIGNATED ARRAY INIT (returns 1-19) ==========
    {
        // Basic designated init
        int arr[5] = {[1] = 10, [3] = 30};
        if (arr[0] != 0) return 1;
        if (arr[1] != 10) return 2;
        if (arr[2] != 0) return 3;
        if (arr[3] != 30) return 4;
        if (arr[4] != 0) return 5;

        // Out of order designation
        int arr3[5] = {[4] = 4, [2] = 2, [0] = 0};
        if (arr3[0] != 0) return 11;
        if (arr3[1] != 0) return 12;
        if (arr3[2] != 2) return 13;
        if (arr3[3] != 0) return 14;
        if (arr3[4] != 4) return 15;
    }

    // ========== DESIGNATED STRUCT INIT (returns 20-39) ==========
    {
        // Basic designated struct init
        struct Point p1 = {.x = 10, .y = 20, .z = 12};
        if (p1.x != 10) return 20;
        if (p1.y != 20) return 21;
        if (p1.z != 12) return 22;

        // Out of order
        struct Point p2 = {.z = 30, .x = 10, .y = 2};
        if (sum_point(p2) != 42) return 23;

        // Partial designated (others are 0)
        struct Point p3 = {.y = 42};
        if (p3.x != 0) return 24;
        if (p3.y != 42) return 25;
        if (p3.z != 0) return 26;

        // Nested struct designated init
        struct Rect r = {
            .tl = {.x = 0, .y = 0},
            .br = {.x = 100, .y = 50}
        };
        if (r.tl.x != 0) return 27;
        if (r.br.x != 100) return 28;
        if (r.br.y != 50) return 29;
    }

    // ========== COMPOUND LITERALS (returns 40-69) ==========
    {
        // Basic compound literal
        struct Point p = (struct Point){10, 20, 12};
        if (sum_point(p) != 42) return 40;

        // Compound literal with designated init
        struct Point p2 = (struct Point){.y = 30, .x = 10, .z = 2};
        if (sum_point(p2) != 42) return 41;

        // Array compound literal
        int *arr = (int[]){1, 2, 3, 4, 5};
        if (arr[0] != 1) return 42;
        if (arr[2] != 3) return 43;
        if (arr[4] != 5) return 44;

        // Compound literal as function argument
        int result = sum_point((struct Point){5, 7, 30});
        if (result != 42) return 45;

        // Address of compound literal
        struct Point *ptr = &(struct Point){100, 200, 300};
        if (ptr->x != 100) return 46;
        if (ptr->y != 200) return 47;
        if (ptr->z != 300) return 48;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("c99_initializers_mega", code, &[]), 0);
}
