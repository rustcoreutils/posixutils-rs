//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// <tgmath.h> dispatch (C17 7.25p3) and the type of `I` (7.3.1p4).
//
// An integer argument counts as `double`: `pow(i, f)` for an `int i` and a
// `float f` is `pow`, where the usual arithmetic conversions alone give
// `float` and called `powf`, rounding `i`. A real argument to a complex-only
// macro is the complex type of its own precision. `I` is `float _Complex`,
// and a `double _Complex` one widened every float complex expression.
//

use crate::common::compile_and_run;

/// `<tgmath.h>` dispatch on integer arguments and the type of `I`, in one
/// unit under `<tgmath.h>` (which includes `<complex.h>`).
///
/// Consolidates: `tgmath_integer_arguments_count_as_double`, `complex_i_is_float_complex`.
/// The original tests' documentation follows, one block per section.
//
// ---- tgmath_integer_arguments_count_as_double (exit codes 1..50) ----
// (no doc comment)
//
// ---- complex_i_is_float_complex (exit codes 51..54) ----
// (no doc comment)
#[test]
fn tgmath_dispatch_and_complex_i_mega() {
    let code = r#"
/* tgmath: compile_and_run -lm
   Exit codes: section k's own failure code plus its base.
     1.. 50  tgmath_integer_arguments_count_as_double
    51.. 54  complex_i_is_float_complex
*/
#include <tgmath.h>
#include <complex.h>

/* ==== tgmath_integer_arguments_count_as_double (codes 1..50) ==== */
#define T(x) _Generic((x), float: 2, double: 3, long double: 4, \
    float _Complex: 5, double _Complex: 6, long double _Complex: 7, default: 99)
int i = 16777217; float fl = 2.0f; long double ld = 2.0L;
float _Complex fc = 1.0f;
static int t_tgmath_integer_arguments_count_as_double(void) {
    int got[] = {
        T(pow(i, fl)), T(pow(fl, i)), T(atan2(i, fl)), T(fmax(fl, i)),
        T(fma(i, fl, fl)), T(nextafter(fl, i)), T(pow(i, fc)), T(pow(fl, fl)),
        T(cimag(fl)), T(creal(I)), T(conj(fl)), T(pow(ld, i)),
        T(sqrt(i)), T(fabs(fc)), T(carg(i)), T(remquo(fl, i, &i)),
    };
    int want[] = { 3, 3, 3, 3, 3, 3, 6, 2, 2, 2, 5, 4, 3, 2, 3, 3 };
    for (unsigned k = 0; k < sizeof want / sizeof want[0]; k++)
        if (got[k] != want[k]) return 1 + k;
    /* The integer reached the function unrounded. */
    if (fmax(fl, 16777217) != 16777217.0) return 50;
    return 0;
}
#undef T

/* ==== complex_i_is_float_complex (codes 51..54) ==== */
#define T(x) _Generic((x), float _Complex: 5, double _Complex: 6, default: 99)
static int t_complex_i_is_float_complex(void) {
    if (sizeof(I) != sizeof(float _Complex)) return 1;
    if (T(I) != 5 || T(1.0f * I) != 5 || T(1 * I) != 5 || T(_Complex_I) != 5) return 2;
    if (T(1.0 * I) != 6) return 3;
    float _Complex z = 1.0f + 2.0f * I;
    if (crealf(z) != 1.0f || cimagf(z) != 2.0f) return 4;
    return 0;
}

int main(void)
{
    int rc;
    if ((rc = t_tgmath_integer_arguments_count_as_double()) != 0) return 0 + rc;
    if ((rc = t_complex_i_is_float_complex()) != 0) return 50 + rc;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("tgmath_dispatch_and_complex_i", code, &["-lm".to_string()]),
        0
    );
}

/// Which C23 macros `<tgmath.h>` defines follows glibc's feature tests under
/// either of their names: `_C2X` up to glibc 2.39, `_C23` from 2.40, which
/// kept no alias. Asking only the old names lost `exp10`, `roundeven`,
/// `fmaximum` and the rest on every newer glibc.
///
/// A stand-in `<math.h>` defines each spelling in turn, so the test needs no
/// particular glibc on the host.
#[test]
fn tgmath_c23_macros_follow_both_glibc_spellings() {
    let src = "#include <tgmath.h>\n\
               #if !defined exp10 || !defined roundeven || !defined nextdown \\\n\
               \x20   || !defined fmaximum\n\
               #error \"missing\"\n\
               #endif\n";
    for c23 in ["C2X", "C23"] {
        let dir = crate::common::work_dir("tgmath_c23_names");
        let math = format!(
            "#define __GLIBC_USE(F) __GLIBC_USE_ ## F\n\
             #define __GLIBC_USE_IEC_60559_BFP_EXT_{c23} 1\n\
             #define __GLIBC_USE_IEC_60559_FUNCS_EXT_{c23} 1\n\
             #define __GLIBC_USE_ISO{c23} 1\n"
        );
        std::fs::write(dir.path().join("math.h"), math).unwrap();
        std::fs::write(dir.path().join("complex.h"), "").unwrap();
        let include = format!("-I{}", dir.path().display());
        let run = crate::common::preprocess_text("tgmath_c23_names", src, &[&include]);
        assert!(run.success, "glibc's {c23} names:\n{}", run.stderr);
    }
}
