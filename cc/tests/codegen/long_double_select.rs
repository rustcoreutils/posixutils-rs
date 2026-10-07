//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// A conditional choosing between two `long double` values.
//
// The optimizer turns such a diamond into a select. On x86-64 a select of
// an x87 value went down the XMM path and came out as `movt`, which is no
// instruction: bash's `seq` loadable (`if (ret == -0.0) ret = 0.0;`) failed
// to assemble.
//

use crate::common::compile_and_run_everywhere;

const SELECTS: &str = r#"
#include <string.h>

__attribute__((noinline)) static long double unneg(long double ret)
{
    if (ret == -0.0)
        ret = 0.0;
    return ret;
}

__attribute__((noinline)) static long double pick(int c, long double a, long double b)
{
    return c ? a : b;
}

__attribute__((noinline)) static long double clamp(long double x, long double lo)
{
    return x < lo ? lo : x;
}

static int negative_zero(long double x)
{
    return x == 0 && __builtin_signbit(x);
}

int main(void)
{
    volatile long double mz = -0.0L, v = -7.25L, third = 1.0L / 3;
    if (negative_zero(unneg(mz))) return 1;
    if (unneg(mz) != 0) return 2;
    if (unneg(v) != -7.25L) return 3;
    if (pick(1, third, v) != third) return 4;
    if (pick(0, third, v) != v) return 5;
    /* every bit of the extended significand survives */
    long double t = pick(1, third, v), w = third;
    if (memcmp(&t, &w, 10) != 0) return 6;
    if (clamp(v, 2.5L) != 2.5L) return 7;
    if (clamp(third, -1.0L) != third) return 8;
    return 0;
}
"#;

#[test]
fn codegen_long_double_select_assembles_and_runs() {
    compile_and_run_everywhere("long_double_select", SELECTS);
}
