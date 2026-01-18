//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C89 Global Variables Mega-Test
//
// Consolidates: ALL globals/mod.rs tests
//

use crate::common::compile_and_run;

// ============================================================================
// Mega-test: C89 global variables
// ============================================================================

#[test]
fn c89_globals_mega() {
    let code = r#"
// Basic global variables
int global_counter = 42;
int global_uninit;
static int static_var = 10;
static int static_uninit;

// Multiple globals
int g_a = 10;
int g_b = 20;
int g_c = 12;

// Different types
char g_char = 1;
short g_short = 2;
int g_int = 3;
long g_long = 4;

// Float/double globals
double g_double = 42.5;
float g_float = 42.9f;

// Pointer globals
int g_value = 42;
int *g_ptr;

// Array globals
int g_arr[5] = {10, 20, 3, 4, 5};
int g_arr_partial[5] = {1, 2, 3};
int g_arr_desig[5] = {[1] = 10, [3] = 30};

// Struct globals
struct Point { int x; int y; int z; };
struct Point g_point = {10, 32};
struct Point g_point_desig = {.y = 20, .x = 10, .z = 12};

// String globals
char g_str[] = "ABC";

// Constant expression globals
int g_const_a = 2 + 3;
int g_const_b = 10 * 4 + 2;
int g_const_c = (1 << 4) | 10;
int g_const_d = 100 / 5 % 7;
int g_const_e = -10 + 52;

// Enum in global
enum { G_X = 5, G_Y = G_X + 3, G_Z = G_Y * 2 };
int g_enum_arr[G_X + G_Y];
int g_enum_val = G_Z - G_Y;

// Tentative definitions
int tentative;
int tentative = 42;

int tentative2;
int tentative2;
int tentative2 = 10;

int tentative_only;

int main(void) {
    // ========== BASIC GLOBALS (returns 1-19) ==========
    {
        // Initialized global
        if (global_counter != 42) return 1;

        // Uninitialized global (should be 0)
        if (global_uninit != 0) return 2;

        // Static global
        if (static_var != 10) return 3;

        // Static uninitialized (should be 0)
        if (static_uninit != 0) return 4;

        // Multiple globals
        if (g_a + g_b + g_c != 42) return 5;

        // Different types
        if (g_char + g_short + g_int + g_long != 10) return 6;

        // Global modification
        int saved = global_counter;
        global_counter = 100;
        if (global_counter != 100) return 7;
        global_counter = saved;

        // Global increment
        int counter = 40;
        global_counter = counter;
        global_counter = global_counter + 1;
        global_counter = global_counter + 1;
        if (global_counter != 42) return 8;
    }

    // ========== FLOAT/DOUBLE GLOBALS (returns 20-29) ==========
    {
        if ((int)g_double != 42) return 20;
        if ((int)g_float != 42) return 21;
    }

    // ========== POINTER GLOBALS (returns 30-39) ==========
    {
        // Null pointer check
        g_ptr = 0;
        if (g_ptr) return 30;

        // Pointer to global
        g_ptr = &g_value;
        if (*g_ptr != 42) return 31;
    }

    // ========== ARRAY GLOBALS (returns 40-59) ==========
    {
        // Full init
        if (g_arr[0] + g_arr[1] + g_arr[2] != 33) return 40;

        // Partial init (rest should be 0)
        if (g_arr_partial[0] + g_arr_partial[1] + g_arr_partial[2]
            + g_arr_partial[3] + g_arr_partial[4] != 6) return 41;

        // Designated init
        if (g_arr_desig[0] + g_arr_desig[1] + g_arr_desig[2]
            + g_arr_desig[3] + g_arr_desig[4] != 40) return 42;
    }

    // ========== STRUCT GLOBALS (returns 60-69) ==========
    {
        // Basic struct init
        if (g_point.x + g_point.y != 42) return 60;

        // Designated struct init
        if (g_point_desig.x + g_point_desig.y + g_point_desig.z != 42) return 61;

        // Partial struct init (z should be 0)
        if (g_point.z != 0) return 62;
    }

    // ========== STRING GLOBALS (returns 70-79) ==========
    {
        // 'A'=65, 'B'=66, 'C'=67 -> 198
        if (g_str[0] + g_str[1] + g_str[2] != 198) return 70;
    }

    // ========== CONSTANT EXPRESSION GLOBALS (returns 80-89) ==========
    {
        if (g_const_a != 5) return 80;
        if (g_const_b != 42) return 81;
        if (g_const_c != 26) return 82;
        if (g_const_d != 6) return 83;
        if (g_const_e != 42) return 84;
    }

    // ========== ENUM CONSTANT GLOBALS (returns 90-99) ==========
    {
        int arr_size = sizeof(g_enum_arr) / sizeof(g_enum_arr[0]);
        if (arr_size != 13) return 90;  // G_X + G_Y = 5 + 8 = 13
        if (g_enum_val != 8) return 91;  // G_Z - G_Y = 16 - 8 = 8
    }

    // ========== TENTATIVE DEFINITIONS (returns 100-109) ==========
    {
        if (tentative != 42) return 100;
        if (tentative2 != 10) return 101;
        if (tentative_only != 0) return 102;  // Should be zero
    }

    // ========== STATIC LOCAL (returns 110-119) ==========
    {
        static int local_arr[3] = {10, 20, 12};
        if (local_arr[0] + local_arr[1] + local_arr[2] != 42) return 110;

        static struct Point local_point = {10, 32};
        if (local_point.x + local_point.y != 42) return 111;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("c89_globals_mega", code, &[]), 0);
}
