//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C89 Storage Classes Mega-Test
//
// Consolidates: storage.rs + static_local.rs tests
//

use crate::common::compile_and_run;

// ============================================================================
// Mega-test: C89 storage classes (auto, static, register, extern)
// ============================================================================

#[test]
fn c89_storage_mega() {
    let code = r#"
// Extern declaration
extern int external_var;
int external_var = 42;

// Static file-scope
static int file_static = 100;

// Static function counter
int call_counter(void) {
    static int count = 0;
    count++;
    return count;
}

// Static with initialization
int get_static_array_sum(void) {
    static int arr[3] = {10, 20, 12};
    return arr[0] + arr[1] + arr[2];
}

// Multiple static locals in same function
int multi_static(int op) {
    static int a = 10;
    static int b = 0;

    if (op == 0) {
        a += 1;
        return a;
    } else {
        b += 5;
        return b;
    }
}

// Static struct
int get_static_point_sum(void) {
    static struct { int x; int y; } point = {10, 32};
    return point.x + point.y;
}

// Const storage
const int const_global = 42;

// Register hint (may be ignored by compiler)
int sum_with_register(int n) {
    register int sum = 0;
    register int i;
    for (i = 1; i <= n; i++) {
        sum += i;
    }
    return sum;
}

int main(void) {
    // ========== AUTO (DEFAULT) STORAGE (returns 1-19) ==========
    {
        // auto is the default for local variables
        auto int a = 10;
        if (a != 10) return 1;

        // Multiple auto declarations
        auto int x = 1, y = 2, z = 3;
        if (x + y + z != 6) return 2;

        // Auto is function-scoped
        {
            auto int inner = 42;
            if (inner != 42) return 3;
        }
        // inner not accessible here

        // Auto array
        auto int arr[3] = {1, 2, 3};
        if (arr[0] + arr[1] + arr[2] != 6) return 4;
    }

    // ========== STATIC LOCAL STORAGE (returns 20-49) ==========
    {
        // Static local persists across calls
        int c1 = call_counter();
        int c2 = call_counter();
        int c3 = call_counter();
        if (c1 != 1) return 20;
        if (c2 != 2) return 21;
        if (c3 != 3) return 22;

        // Static array initialization
        if (get_static_array_sum() != 42) return 23;

        // Multiple static locals
        int m1 = multi_static(0);  // a = 11
        int m2 = multi_static(1);  // b = 5
        int m3 = multi_static(0);  // a = 12
        int m4 = multi_static(1);  // b = 10
        if (m1 != 11) return 24;
        if (m2 != 5) return 25;
        if (m3 != 12) return 26;
        if (m4 != 10) return 27;

        // Static struct
        if (get_static_point_sum() != 42) return 28;

        // Static in block scope
        {
            static int block_static = 50;
            if (block_static != 50) return 29;
            block_static = 60;
        }
        // block_static persists but is not accessible here
    }

    // ========== STATIC FILE SCOPE (returns 50-59) ==========
    {
        // Access file-scope static
        if (file_static != 100) return 50;

        // Modify file-scope static
        file_static = 200;
        if (file_static != 200) return 51;
        file_static = 100;  // Reset
    }

    // ========== EXTERN STORAGE (returns 60-69) ==========
    {
        // Access extern variable
        if (external_var != 42) return 60;

        // Modify extern variable
        external_var = 100;
        if (external_var != 100) return 61;
        external_var = 42;  // Reset
    }

    // ========== REGISTER STORAGE (returns 70-79) ==========
    {
        // Register hint (compiler may ignore)
        register int r = 42;
        if (r != 42) return 70;

        // Register in loop
        if (sum_with_register(5) != 15) return 71;  // 1+2+3+4+5

        // Note: cannot take address of register variable
        // &r would be invalid
    }

    // ========== CONST STORAGE (returns 80-89) ==========
    {
        // Const global
        if (const_global != 42) return 80;

        // Const local
        const int local_const = 100;
        if (local_const != 100) return 81;

        // Const pointer to non-const
        int x = 10;
        int * const cp = &x;
        *cp = 20;
        if (x != 20) return 82;

        // Pointer to const
        const int ci = 30;
        const int *pci = &ci;
        if (*pci != 30) return 83;

        // Const array
        const int carr[3] = {1, 2, 3};
        if (carr[0] + carr[1] + carr[2] != 6) return 84;
    }

    // ========== VOLATILE STORAGE (returns 90-99) ==========
    {
        // Volatile prevents optimization
        volatile int v = 42;
        if (v != 42) return 90;

        v = 100;
        if (v != 100) return 91;

        // Volatile pointer
        volatile int *vp = &v;
        *vp = 200;
        if (v != 200) return 92;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("c89_storage_mega", code, &[]), 0);
}
