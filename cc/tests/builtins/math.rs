//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Math Builtins Mega-Test
//
// Consolidates: nan, nans, flt_rounds tests
//

use crate::common::compile_and_run;

// ============================================================================
// Mega-test: Math builtins
// ============================================================================

#[test]
fn builtins_math_mega() {
    let code = r#"
int main(void) {
    // ========== __BUILTIN_NAN (returns 1-9) ==========
    {
        // Quiet NaN for double
        double d = __builtin_nan("");
        volatile double vd = d;

        // Quiet NaN for float
        float f = __builtin_nanf("");
        volatile float vf = f;

        // Quiet NaN for long double
        long double ld = __builtin_nanl("");
        volatile long double vld = ld;

        // The values exist and don't crash
    }

    // ========== __BUILTIN_NANS (signaling NaN) (returns 10-19) ==========
    {
        // Signaling NaN variants
        double d = __builtin_nans("");
        volatile double vd = d;

        float f = __builtin_nansf("");
        volatile float vf = f;

        long double ld = __builtin_nansl("");
        volatile long double vld = ld;
    }

    // ========== __BUILTIN_FLT_ROUNDS (returns 20-29) ==========
    {
        // Returns current rounding mode
        // 1 = round to nearest (IEEE 754 default)
        int mode = __builtin_flt_rounds();
        if (mode != 1) return 20;
    }

    // ========== __BUILTIN_HUGE_VAL (returns 30-39) ==========
    {
        // Positive infinity
        double inf = __builtin_huge_val();
        volatile double vinf = inf;

        float finf = __builtin_huge_valf();
        volatile float vfinf = finf;

        long double ldinf = __builtin_huge_vall();
        volatile long double vldinf = ldinf;

        // inf > any finite number
        if (!(inf > 1000000.0)) return 30;
        if (!(finf > 1000000.0f)) return 31;
    }

    // ========== __BUILTIN_INF (returns 40-49) ==========
    {
        // Same as huge_val on most systems
        double inf = __builtin_inf();
        volatile double vinf = inf;

        float finf = __builtin_inff();
        volatile float vfinf = finf;

        long double ldinf = __builtin_infl();
        volatile long double vldinf = ldinf;

        if (!(inf > 1000000.0)) return 40;
    }

    // ========== FLOATING POINT BUILTINS (returns 60-79) ==========
    {
        // __builtin_fabs
        double fa = __builtin_fabs(-3.14);
        if (fa < 3.0 || fa > 3.2) return 62;

        fa = __builtin_fabs(3.14);
        if (fa < 3.0 || fa > 3.2) return 63;

        float ff = __builtin_fabsf(-2.5f);
        if (ff < 2.4f || ff > 2.6f) return 64;
    }

    // ========== __BUILTIN_SIGNBIT (returns 80-99) ==========
    {
        // signbit returns non-zero for negative, zero for non-negative
        // Test double (signbit)
        double neg_d = -1.0;
        double pos_d = 1.0;
        double zero_d = 0.0;
        double neg_zero_d = -0.0;

        if (!__builtin_signbit(neg_d)) return 80;
        if (__builtin_signbit(pos_d)) return 81;
        if (__builtin_signbit(zero_d)) return 82;
        if (!__builtin_signbit(neg_zero_d)) return 83;  // -0.0 has sign bit set

        // Test float (signbitf)
        float neg_f = -2.5f;
        float pos_f = 2.5f;
        float zero_f = 0.0f;
        float neg_zero_f = -0.0f;

        if (!__builtin_signbitf(neg_f)) return 84;
        if (__builtin_signbitf(pos_f)) return 85;
        if (__builtin_signbitf(zero_f)) return 86;
        if (!__builtin_signbitf(neg_zero_f)) return 87;

        // Test with infinity
        double neg_inf = -__builtin_inf();
        double pos_inf = __builtin_inf();
        if (!__builtin_signbit(neg_inf)) return 88;
        if (__builtin_signbit(pos_inf)) return 89;

        // Test result is usable as int
        int sb = __builtin_signbit(-5.0);
        if (sb == 0) return 90;
        sb = __builtin_signbit(5.0);
        if (sb != 0) return 91;
    }

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("builtins_math_mega", code, &["-lm".to_string()]),
        0
    );
}
