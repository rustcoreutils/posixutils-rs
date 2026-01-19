//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// setjmp/longjmp Mega-Test
//
// Consolidates: ALL setjmp/longjmp tests
//

use crate::common::compile_and_run;

// ============================================================================
// Mega-test: setjmp/longjmp
// ============================================================================

#[test]
fn misc_setjmp_mega() {
    let code = r#"
typedef int jmp_buf[64];
extern int setjmp(jmp_buf);
extern void longjmp(jmp_buf, int);
extern int _setjmp(jmp_buf);
extern void _longjmp(jmp_buf, int);

jmp_buf env;
int count = 0;

void do_longjmp(int val) {
    longjmp(env, val);
}

void level3(int val) {
    longjmp(env, val);
}

void level2(int val) {
    level3(val);
}

void level1(int val) {
    level2(val);
}

int main(void) {
    // ========== BASIC SETJMP/LONGJMP (returns 1-9) ==========
    {
        // Basic setjmp/longjmp
        int val = setjmp(env);
        if (val == 0) {
            longjmp(env, 42);
            return 1;  // Should never reach here
        }
        if (val != 42) return 2;
    }

    // ========== MULTIPLE RETURNS (returns 10-19) ==========
    {
        count = 0;
        int val = setjmp(env);
        count++;
        if (count < 4) {
            longjmp(env, count * 10);
            return 10;
        }
        if (count != 4) return 11;
        if (val != 30) return 12;  // From count=3 jump
    }

    // ========== LONGJMP WITH 0 (returns 20-29) ==========
    {
        // Per C standard, longjmp(env, 0) causes setjmp to return 1
        int val = setjmp(env);
        if (val == 0) {
            longjmp(env, 0);
            return 20;
        }
        if (val != 1) return 21;
    }

    // ========== LONGJMP FROM FUNCTION (returns 30-39) ==========
    {
        int val = setjmp(env);
        if (val == 0) {
            do_longjmp(77);
            return 30;
        }
        if (val != 77) return 31;
    }

    // ========== LONGJMP FROM NESTED CALLS (returns 40-49) ==========
    {
        int val = setjmp(env);
        if (val == 0) {
            level1(123);
            return 40;
        }
        if (val != 123) return 41;
    }

    // ========== VOLATILE PRESERVATION (returns 50-59) ==========
    {
        volatile int x = 5;
        int val = setjmp(env);
        if (val == 0) {
            x = 10;
            longjmp(env, 1);
            return 50;
        }
        if (x != 10) return 51;
    }

    // ========== _SETJMP VARIANT (returns 60-69) ==========
    {
        int val = _setjmp(env);
        if (val == 0) {
            _longjmp(env, 55);
            return 60;
        }
        if (val != 55) return 61;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("misc_setjmp_mega", code, &[]), 0);
}
