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

use crate::common::{compile_and_run, compile_and_run_aarch64_with};

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

/// A statement expression whose value is complex. The value was stored,
/// reloaded and then used as the address of the complex number -- a
/// segfault on both targets at every level.
#[test]
fn misc_stmt_expr_complex_value() {
    let code = r#"
/* A statement expression whose value is complex, used as a value. */
static double _Complex mk(double r) { return r + 2.0i; }
int main(void)
{
    double _Complex d = ({ double _Complex v = 1.0; v; });
    if (__real__ d != 1.0 || __imag__ d != 0.0) return 1;
    float _Complex f = ({ float _Complex w = mk(3.0); w * 2; });
    if (__real__ f != 6.0f || __imag__ f != 4.0f) return 2;
    long double _Complex l = ({ long double _Complex x = 5.0L; x; }) + 1;
    if (__real__ l != 6.0L) return 3;
    return 0;
}
"#;
    for level in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run("stmt_expr_complex", code, &[level.to_string()]),
            0,
            "{level}"
        );
        if let Some(rc) = compile_and_run_aarch64_with("stmt_expr_complex_a64", code, &[level], &[])
        {
            assert_eq!(rc, 0, "aarch64 {level}");
        }
    }
}

/// The value of a statement expression outlives its block, whose objects are
/// dead once it ends: a value carried as an address (a struct wider than a
/// register, a vector, a complex number) pointed into the block, and a second
/// statement expression reused the slot -- `pair24` was handed `y` twice.
/// The eight-byte struct, which travels as its value, is the other side of
/// that line. Also the comma operator's complex value, which was read as the
/// number's bits and dereferenced.
#[test]
fn misc_stmt_expr_value_outlives_block() {
    let code = r#"
/* The value of a statement expression outlives the block: an object
   declared in it is dead once it ends, and the next one may reuse its
   storage. Struct both sides of eight bytes, vector, complex, and the
   comma operator's complex value. */
typedef int v4si __attribute__((vector_size(16)));
struct S8 { int a, b; };
struct S24 { long a, b, c; };
static int one = 1;
static int pair8(struct S8 a, struct S8 b) { return a.b == 2 && b.b == 4; }
static int pair24(struct S24 a, struct S24 b) { return a.c == 3 && b.c == 6; }
static int cpair(double _Complex a, double _Complex b)
{
    return __real__ a == 1.0 && __imag__ a == 2.0 && __real__ b == 3.0;
}
int main(void)
{
    if (!pair8(({ struct S8 x = {1, 2}; x; }), ({ struct S8 y = {3, 4}; y; })))
        return 1;
    if (!pair24(({ struct S24 x = {1, 2, 3}; x; }),
                ({ struct S24 y = {4, 5, 6}; y; })))
        return 2;
    v4si v = ({ v4si x = {1, 2, 3, 4}; x; }) + ({ v4si y = {10, 20, 30, 40}; y; });
    if (v[0] != 11 || v[3] != 44)
        return 3;
    if (!cpair(({ double _Complex x = 1.0 + 2.0i; x; }),
               ({ double _Complex y = 3.0; y; })))
        return 4;
    double _Complex s = ({ double _Complex x = 1.0; x; }) + ({ double _Complex y = 2.0; y; });
    if (__real__ s != 3.0 || __imag__ s != 0.0)
        return 5;
    _Complex int ci = ({ _Complex int q = 3; q; });
    if (__real__ ci != 3 || __imag__ ci != 0)
        return 6;
    double _Complex w = 7.0 + 1.0i;
    double _Complex c = (++one, w) + 1;
    if (__real__ c != 8.0 || __imag__ c != 1.0 || one != 2)
        return 7;
    return 0;
}
"#;
    for level in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run("stmt_expr_outlives", code, &[level.to_string()]),
            0,
            "{level}"
        );
        if let Some(rc) =
            compile_and_run_aarch64_with("stmt_expr_outlives_a64", code, &[level], &[])
        {
            assert_eq!(rc, 0, "aarch64 {level}");
        }
    }
}
