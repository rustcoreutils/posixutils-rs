//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Statement Expressions Mega-Test (GNU extension)
//
// Consolidates: ALL statement expression tests ({ stmt; stmt; expr; })
//

use crate::common::compile_and_run;

// ============================================================================
// Mega-test: GNU statement expressions
// ============================================================================

#[test]
fn misc_stmt_expr_mega() {
    let code = r#"
int add(int a, int b) {
    return a + b;
}

int main(void) {
    // ========== BASIC STMT EXPR (returns 1-9) ==========
    {
        // Basic statement expression
        int x = ({ int a = 5; a + 3; });
        if (x != 8) return 1;
    }

    // ========== MULTIPLE DECLARATIONS (returns 10-19) ==========
    {
        int result = ({
            int a = 1;
            int b = 2;
            int c = 3;
            a + b + c;
        });
        if (result != 6) return 10;
    }

    // ========== NESTED STMT EXPR (returns 20-29) ==========
    {
        int x = ({
            int a = ({ int inner = 10; inner * 2; });
            a + 5;
        });
        if (x != 25) return 20;
    }

    // ========== AS FUNCTION ARG (returns 30-39) ==========
    {
        int result = add(({ int x = 3; x; }), ({ int y = 4; y; }));
        if (result != 7) return 30;
    }

    // ========== WITH CONTROL FLOW (returns 40-49) ==========
    {
        int x = ({
            int sum = 0;
            int i;
            for (i = 1; i <= 10; i++) {
                sum = sum + i;
            }
            sum;
        });
        // 1 + 2 + ... + 10 = 55
        if (x != 55) return 40;
    }

    // ========== WITH IF-ELSE (returns 50-59) ==========
    {
        int x = ({
            int a = 10;
            int result;
            if (a > 5) {
                result = a * 2;
            } else {
                result = a;
            }
            result;
        });
        if (x != 20) return 50;
    }

    // ========== WITH WHILE LOOP (returns 60-69) ==========
    {
        int x = ({
            int val = 1;
            int i = 0;
            while (i < 5) {
                val = val * 2;
                i++;
            }
            val;
        });
        // 1 * 2^5 = 32
        if (x != 32) return 60;
    }

    // ========== COMPLEX NESTED (returns 70-79) ==========
    {
        int x = ({
            int outer = ({
                int inner1 = ({ 1 + 2; });
                int inner2 = ({ 3 + 4; });
                inner1 + inner2;
            });
            outer * 2;
        });
        // (3 + 7) * 2 = 20
        if (x != 20) return 70;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("misc_stmt_expr_mega", code, &[]), 0);
}
