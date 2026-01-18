//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C11 Core Features Mega-Test
//
// Consolidates: _Static_assert, _Alignof, _Noreturn, _Thread_local tests
//

use crate::common::compile_and_run;

// ============================================================================
// Mega-test: C11 core features
// ============================================================================

#[test]
fn c11_core_mega() {
    let code = r#"
// File-scope static assertions
_Static_assert(1, "always true");
_Static_assert(sizeof(int) >= 4, "int at least 4 bytes");
_Static_assert(sizeof(char) == 1, "char is 1 byte");

// C23 style without message (C2x feature supported early)
static_assert(1);
static_assert(sizeof(long) == 8);

// Struct with static assert inside
struct WithAssert {
    int x;
    _Static_assert(sizeof(int) == 4, "int size check");
    double y;
    _Static_assert(1, "always passes");
};

// Thread-local variables
_Thread_local int tls_var = 42;
__thread int tls_var2 = 100;
static _Thread_local int tls_static = 200;
_Thread_local int tls_uninit;
__thread long tls_uninit_long;

// Helper functions for frame/return address
void *get_frame(void) {
    return __builtin_frame_address(0);
}

void *get_return_addr(void) {
    return __builtin_return_address(0);
}

int main(void) {
    // ========== _ALIGNOF SECTION (returns 1-19) ==========
    {
        // Basic type alignments
        if (_Alignof(char) != 1) return 1;
        if (_Alignof(short) != 2) return 2;
        if (_Alignof(int) != 4) return 3;
        if (_Alignof(long) != 8) return 4;
        if (_Alignof(void*) != 8) return 5;
        if (_Alignof(double) != 8) return 6;

        // Alternative spellings
        if (alignof(int) != 4) return 7;
        if (__alignof__(int) != 4) return 8;
        if (__alignof(int) != 4) return 9;

        // _Alignof on expression
        int x;
        if (_Alignof(x) != 4) return 10;

        // Struct alignment
        struct S { char c; int i; double d; };
        if (_Alignof(struct S) != 8) return 11;

        // Array alignment
        int arr[10];
        if (_Alignof(arr) != 4) return 12;

        // Struct with only chars
        struct CharOnly { char a; char b; };
        if (_Alignof(struct CharOnly) != 1) return 13;
    }

    // ========== _STATIC_ASSERT SECTION (returns 20-29) ==========
    {
        // Block-scope static assertion
        _Static_assert(sizeof(void*) == 8, "64-bit pointers");
        static_assert(1 + 1 == 2);

        // Static assert in struct
        struct WithAssert s;
        s.x = 42;
        s.y = 3.14;
        if (s.x != 42) return 20;

        // Complex constant expressions in static assert
        _Static_assert((1 << 10) == 1024, "bit shift");
        _Static_assert(100 / 25 == 4, "division");
    }

    // ========== _THREAD_LOCAL SECTION (returns 30-49) ==========
    {
        // Basic TLS read
        if (tls_var != 42) return 30;
        if (tls_var2 != 100) return 31;
        if (tls_static != 200) return 32;

        // TLS write
        tls_var = 10;
        if (tls_var != 10) return 33;
        tls_var2 = 20;
        if (tls_var2 != 20) return 34;

        // Uninitialized TLS (should be zero)
        if (tls_uninit != 0) return 35;
        if (tls_uninit_long != 0) return 36;

        // Restore values
        tls_var = 42;
        tls_var2 = 100;
    }

    // ========== FRAME/RETURN ADDRESS SECTION (returns 50-59) ==========
    {
        // Frame pointer should be non-null
        void *fp = __builtin_frame_address(0);
        if (fp == 0) return 50;

        // Return address should be non-null
        void *ra = __builtin_return_address(0);
        if (ra == 0) return 51;

        // Frame pointer and return address should be different
        if (fp == ra) return 52;

        // From helper functions
        void *helper_fp = get_frame();
        if (helper_fp == 0) return 53;

        void *helper_ra = get_return_addr();
        if (helper_ra == 0) return 54;

        // Main's frame should be valid
        void *main_fp = __builtin_frame_address(0);
        if (main_fp == 0) return 55;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("c11_core_mega", code, &[]), 0);
}
