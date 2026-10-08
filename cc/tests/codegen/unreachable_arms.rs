//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `__builtin_unreachable ()` runs only where the program reaches it.
//
// gnulib's `assume (R)` is `(R) ? (void) 0 : __builtin_unreachable ()`.
// Turning that conditional into a select evaluates the builtin -- a trap --
// whichever way `R` goes, so every function using `assume` died with SIGILL
// (gnulib's test-verify in sed, findutils and diffutils; tar's `--label`).
//

use crate::common::{compile_and_run, compile_and_run_aarch64_with};

const UNREACHABLE_ARMS: &str = r#"
#define assume(R) ((R) ? (void) 0 : __builtin_unreachable ())

typedef struct { unsigned int context : 4; unsigned int halt : 1; } state;

static int f(int a) { return a; }

__attribute__((noinline)) static void expressions(state *s)
{
    assume (f (1));
    assume (s->halt);
}

__attribute__((noinline)) static int optimization(int x)
{
    assume (x >= 4);
    return x > 1 ? x + 3 : 2 * x + 10;
}

__attribute__((noinline)) static int value(int x)
{
    return x ? x : (__builtin_unreachable (), 0);
}

__attribute__((noinline)) static int elvis(int x)
{
    return x ?: (__builtin_unreachable (), 0);
}

int main(void)
{
    state s = { 0, 1 };
    expressions(&s);
    if (optimization(5) != 8) return 1;
    if (value(7) != 7) return 2;
    if (elvis(9) != 9) return 3;
    return 0;
}
"#;

#[test]
fn codegen_an_unreachable_conditional_arm_runs_only_when_taken() {
    for level in ["-O0", "-O1", "-O2"] {
        assert_eq!(
            compile_and_run("unreachable_arms", UNREACHABLE_ARMS, &[level.to_string()]),
            0,
            "{level}"
        );
        if let Some(rc) =
            compile_and_run_aarch64_with("unreachable_arms_a64", UNREACHABLE_ARMS, &[level], &[])
        {
            assert_eq!(rc, 0, "aarch64 {level}");
        }
    }
}
