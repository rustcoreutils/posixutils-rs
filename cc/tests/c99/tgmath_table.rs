//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Every <tgmath.h> macro against every argument type, as gcc 13 and glibc
// 2.39 have them: which function each call reaches, or that it is refused,
// its result type and its value, on x86-64 and aarch64, with and without
// _GNU_SOURCE.
//
// The tables are gcc's own answers. Each row is the generic arguments' types
// and the function gcc's <tgmath.h> calls for them: `R` is the macro's own
// family and `C` its complex one, followed by the suffix (`Rf32` is
// `sqrtf32` for `sqrt`, `C` is `csqrt`), `!` is "no matching function for
// type-generic call", and `x|a` gives x86-64's answer and aarch64's where
// they differ. Without _GNU_SOURCE glibc declares no _FloatN function and
// none of the extensions, so there a row whose function is a _FloatN one is
// refused, `log10` reaches no `clog10`, and only C17's macros and `scalb`
// exist -- also checked against gcc, row by row.
//

use crate::common::{compile_and_run, compile_and_run_aarch64_with, run_c17, AARCH64_TARGET_ARGS};

/// The macros gcc treats alike, and their table.
const CLASSES: &[(&str, &str)] = &[
    (
        "acos asin atan acosh asinh atanh cos sin tan cosh sinh tanh exp log sqrt fabs log10",
        concat!(
            "f=Rf d=R ld=Rl i=R cf=Cf cd=C cld=Cl ci=C f16=! f32=Rf32 f64=Rf64 ",
            "f128=Rf128 f32x=Rf32x f64x=Rf64x cf16=! cf32=Cf32 cf64=Cf64 cf128=Cf128 ",
            "cf32x=Cf32x cf64x=Cf64x ",
        ),
    ),
    (
        "cbrt ceil erf erfc exp2 expm1 floor ilogb lgamma llrint llround log1p log2 logb lrint lround nearbyint rint round tgamma trunc exp10 nextdown nextup roundeven llogb frexp ldexp scalbn scalbln fromfp ufromfp fromfpx ufromfpx",
        concat!(
            "f=Rf d=R ld=Rl i=R cf=! cd=! cld=! ci=! f16=! f32=Rf32 f64=Rf64 f128=Rf128 ",
            "f32x=Rf32x f64x=Rf64x cf16=! cf32=! cf64=! cf128=! cf32x=! cf64x=! ",
        ),
    ),
    (
        "carg cimag conj cproj creal",
        concat!(
            "f=Rf d=R ld=Rl i=R cf=Rf cd=R cld=Rl ci=R f16=! f32=Rf32 f64=Rf64 ",
            "f128=Rf128 f32x=Rf32x f64x=Rf64x cf16=! cf32=Rf32 cf64=Rf64 cf128=Rf128 ",
            "cf32x=Rf32x cf64x=Rf64x ",
        ),
    ),
    (
        "fsqrt",
        concat!(
            "f=R d=R ld=Rl i=R cf=! cd=! cld=! ci=! f16=R f32=R f64=R f128=!|Rl f32x=R ",
            "f64x=Rl cf16=! cf32=! cf64=! cf128=! cf32x=! cf64x=! ",
        ),
    ),
    (
        "dsqrt",
        concat!(
            "f=Rl d=Rl ld=Rl i=Rl cf=Rl cd=Rl cld=Rl ci=Rl f16=Rl f32=Rl f64=Rl f128=Rl ",
            "f32x=Rl f64x=Rl cf16=Rl cf32=Rl cf64=Rl cf128=Rl cf32x=Rl cf64x=Rl ",
        ),
    ),
    (
        "f32sqrt",
        concat!(
            "f=Rf64 d=Rf64 ld=Rf128 i=Rf64 cf=! cd=! cld=! ci=! f16=Rf64 f32=Rf64 ",
            "f64=Rf64 f128=Rf128 f32x=Rf32x f64x=Rf64x cf16=! cf32=! cf64=! cf128=! ",
            "cf32x=! cf64x=! ",
        ),
    ),
    (
        "f64sqrt",
        concat!(
            "f=Rf128 d=Rf128 ld=Rf128 i=Rf128 cf=! cd=! cld=! ci=! f16=Rf128 f32=Rf128 ",
            "f64=Rf128 f128=Rf128 f32x=Rf128 f64x=Rf64x cf16=! cf32=! cf64=! cf128=! ",
            "cf32x=! cf64x=! ",
        ),
    ),
    (
        "f32xsqrt",
        concat!(
            "f=Rf64x d=Rf64x ld=Rf64x i=Rf64x cf=! cd=! cld=! ci=! f16=Rf64x f32=Rf64x ",
            "f64=Rf64 f128=Rf128 f32x=Rf64x f64x=Rf64x cf16=! cf32=! cf64=! cf128=! ",
            "cf32x=! cf64x=! ",
        ),
    ),
    (
        "f64xsqrt",
        concat!(
            "f=Rf128 d=Rf128 ld=Rf128 i=Rf128 cf=Rf128 cd=Rf128 cld=Rf128 ci=Rf128 ",
            "f16=Rf128 f32=Rf128 f64=Rf128 f128=Rf128 f32x=Rf128 f64x=Rf128 cf16=Rf128 ",
            "cf32=Rf128 cf64=Rf128 cf128=Rf128 cf32x=Rf128 cf64x=Rf128 ",
        ),
    ),
    (
        "nexttoward",
        concat!(
            "f=Rf d=R ld=Rl i=R cf=! cd=! cld=! ci=! f16=! f32=! f64=! f128=! f32x=! ",
            "f64x=! cf16=! cf32=! cf64=! cf128=! cf32x=! cf64x=! ",
        ),
    ),
    (
        "atan2 copysign fdim fmax fmin fmod hypot nextafter remainder fmaxmag fminmag fmaximum fminimum fmaximum_num fminimum_num fmaximum_mag fminimum_mag fmaximum_mag_num fminimum_mag_num remquo",
        concat!(
            "f,f=Rf d,d=R ld,ld=Rl i,i=R cf,cf=! cd,cd=! cld,cld=! ci,ci=! f16,f16=! ",
            "f32,f32=Rf32 f64,f64=Rf64 f128,f128=Rf128 f32x,f32x=Rf32x f64x,f64x=Rf64x ",
            "cf16,cf16=! cf32,cf32=! cf64,cf64=! cf128,cf128=! cf32x,cf32x=! ",
            "cf64x,cf64x=! f,d=R f,ld=Rl f,i=R f,cf=! f,ci=! f,f32=Rf32 f,f64=Rf64 ",
            "f,f128=Rf128 f,f32x=Rf32x f,f64x=Rf64x d,ld=Rl d,i=R d,cf=! d,ci=! d,f32=R ",
            "d,f64=Rf64 d,f128=Rf128 d,f32x=R d,f64x=Rf64x ld,i=Rl ld,cf=! ld,ci=! ",
            "ld,f32=Rl ld,f64=Rl ld,f128=Rf128 ld,f32x=Rl ld,f64x=Rl i,cf=! i,ci=! ",
            "i,f32=R i,f64=Rf64 i,f128=Rf128 i,f32x=Rf32x i,f64x=Rf64x cf,ci=! cf,f32=! ",
            "cf,f64=! cf,f128=! cf,f32x=! cf,f64x=! ci,f32=! ci,f64=! ci,f128=! ",
            "ci,f32x=! ci,f64x=! f32,f64=Rf64 f32,f128=Rf128 f32,f32x=Rf32x ",
            "f32,f64x=Rf64x f64,f128=Rf128 f64,f32x=Rf64 f64,f64x=Rf64x f128,f32x=Rf128 ",
            "f128,f64x=Rf128 f32x,f64x=Rf64x ",
        ),
    ),
    (
        "scalb",
        concat!(
            "f,f=Rf d,d=R ld,ld=Rl i,i=R cf,cf=! cd,cd=! cld,cld=! ci,ci=! f16,f16=! ",
            "f32,f32=! f64,f64=! f128,f128=! f32x,f32x=! f64x,f64x=! cf16,cf16=! ",
            "cf32,cf32=! cf64,cf64=! cf128,cf128=! cf32x,cf32x=! cf64x,cf64x=! f,d=R ",
            "f,ld=Rl f,i=R f,cf=! f,ci=! f,f32=! f,f64=! f,f128=! f,f32x=! f,f64x=! ",
            "d,ld=Rl d,i=R d,cf=! d,ci=! d,f32=R d,f64=! d,f128=! d,f32x=R d,f64x=! ",
            "ld,i=Rl ld,cf=! ld,ci=! ld,f32=Rl ld,f64=Rl ld,f128=! ld,f32x=Rl ld,f64x=Rl ",
            "i,cf=! i,ci=! i,f32=R i,f64=! i,f128=! i,f32x=! i,f64x=! cf,ci=! cf,f32=! ",
            "cf,f64=! cf,f128=! cf,f32x=! cf,f64x=! ci,f32=! ci,f64=! ci,f128=! ",
            "ci,f32x=! ci,f64x=! f32,f64=! f32,f128=! f32,f32x=! f32,f64x=! f64,f128=! ",
            "f64,f32x=! f64,f64x=! f128,f32x=! f128,f64x=! f32x,f64x=! ",
        ),
    ),
    (
        "pow",
        concat!(
            "f,f=Rf d,d=R ld,ld=Rl i,i=R cf,cf=Cf cd,cd=C cld,cld=Cl ci,ci=C f16,f16=! ",
            "f32,f32=Rf32 f64,f64=Rf64 f128,f128=Rf128 f32x,f32x=Rf32x f64x,f64x=Rf64x ",
            "cf16,cf16=! cf32,cf32=Cf32 cf64,cf64=Cf64 cf128,cf128=Cf128 ",
            "cf32x,cf32x=Cf32x cf64x,cf64x=Cf64x f,d=R f,ld=Rl f,i=R f,cf=Cf f,ci=C ",
            "f,f32=Rf32 f,f64=Rf64 f,f128=Rf128 f,f32x=Rf32x f,f64x=Rf64x d,ld=Rl d,i=R ",
            "d,cf=C d,ci=C d,f32=R d,f64=Rf64 d,f128=Rf128 d,f32x=R d,f64x=Rf64x ld,i=Rl ",
            "ld,cf=Cl ld,ci=Cl ld,f32=Rl ld,f64=Rl ld,f128=Rf128 ld,f32x=Rl ld,f64x=Rl ",
            "i,cf=C i,ci=C i,f32=R i,f64=Rf64 i,f128=Rf128 i,f32x=Rf32x i,f64x=Rf64x ",
            "cf,ci=C cf,f32=Cf32 cf,f64=Cf64 cf,f128=Cf128 cf,f32x=Cf32x cf,f64x=Cf64x ",
            "ci,f32=C ci,f64=Cf64 ci,f128=Cf128 ci,f32x=Cf32x ci,f64x=Cf64x f32,f64=Rf64 ",
            "f32,f128=Rf128 f32,f32x=Rf32x f32,f64x=Rf64x f64,f128=Rf128 f64,f32x=Rf64 ",
            "f64,f64x=Rf64x f128,f32x=Rf128 f128,f64x=Rf128 f32x,f64x=Rf64x ",
        ),
    ),
    (
        "fadd fsub fmul fdiv",
        concat!(
            "f,f=R d,d=R ld,ld=Rl i,i=R cf,cf=! cd,cd=! cld,cld=! ci,ci=! f16,f16=R ",
            "f32,f32=R f64,f64=R f128,f128=!|Rl f32x,f32x=R f64x,f64x=Rl cf16,cf16=! ",
            "cf32,cf32=! cf64,cf64=! cf128,cf128=! cf32x,cf32x=! cf64x,cf64x=! f,d=R ",
            "f,ld=Rl f,i=R f,cf=! f,ci=! f,f32=R f,f64=R f,f128=!|Rl f,f32x=R f,f64x=Rl ",
            "d,ld=Rl d,i=R d,cf=! d,ci=! d,f32=R d,f64=R d,f128=!|Rl d,f32x=R d,f64x=Rl ",
            "ld,i=Rl ld,cf=! ld,ci=! ld,f32=Rl ld,f64=Rl ld,f128=!|Rl ld,f32x=Rl ",
            "ld,f64x=Rl i,cf=! i,ci=! i,f32=R i,f64=R i,f128=!|Rl i,f32x=R i,f64x=Rl ",
            "cf,ci=! cf,f32=! cf,f64=! cf,f128=! cf,f32x=! cf,f64x=! ci,f32=! ci,f64=! ",
            "ci,f128=! ci,f32x=! ci,f64x=! f32,f64=R f32,f128=!|Rl f32,f32x=R ",
            "f32,f64x=Rl f64,f128=!|Rl f64,f32x=R f64,f64x=Rl f128,f32x=!|Rl ",
            "f128,f64x=!|Rl f32x,f64x=Rl ",
        ),
    ),
    (
        "dadd dsub dmul ddiv",
        concat!(
            "f,f=Rl d,d=Rl ld,ld=Rl i,i=Rl cf,cf=Rl cd,cd=Rl cld,cld=Rl ci,ci=Rl ",
            "f16,f16=Rl f32,f32=Rl f64,f64=Rl f128,f128=Rl f32x,f32x=Rl f64x,f64x=Rl ",
            "cf16,cf16=Rl cf32,cf32=Rl cf64,cf64=Rl cf128,cf128=Rl cf32x,cf32x=Rl ",
            "cf64x,cf64x=Rl f,d=Rl f,ld=Rl f,i=Rl f,cf=Rl f,ci=Rl f,f32=Rl f,f64=Rl ",
            "f,f128=Rl f,f32x=Rl f,f64x=Rl d,ld=Rl d,i=Rl d,cf=Rl d,ci=Rl d,f32=Rl ",
            "d,f64=Rl d,f128=Rl d,f32x=Rl d,f64x=Rl ld,i=Rl ld,cf=Rl ld,ci=Rl ld,f32=Rl ",
            "ld,f64=Rl ld,f128=Rl ld,f32x=Rl ld,f64x=Rl i,cf=Rl i,ci=Rl i,f32=Rl ",
            "i,f64=Rl i,f128=Rl i,f32x=Rl i,f64x=Rl cf,ci=Rl cf,f32=Rl cf,f64=Rl ",
            "cf,f128=Rl cf,f32x=Rl cf,f64x=Rl ci,f32=Rl ci,f64=Rl ci,f128=Rl ci,f32x=Rl ",
            "ci,f64x=Rl f32,f64=Rl f32,f128=Rl f32,f32x=Rl f32,f64x=Rl f64,f128=Rl ",
            "f64,f32x=Rl f64,f64x=Rl f128,f32x=Rl f128,f64x=Rl f32x,f64x=Rl ",
        ),
    ),
    (
        "f32add f32sub f32mul f32div",
        concat!(
            "f,f=Rf64 d,d=Rf64 ld,ld=Rf128 i,i=Rf64 cf,cf=! cd,cd=! cld,cld=! ci,ci=! ",
            "f16,f16=Rf64 f32,f32=Rf64 f64,f64=Rf64 f128,f128=Rf128 f32x,f32x=Rf32x ",
            "f64x,f64x=Rf64x cf16,cf16=! cf32,cf32=! cf64,cf64=! cf128,cf128=! ",
            "cf32x,cf32x=! cf64x,cf64x=! f,d=Rf64 f,ld=Rf128 f,i=Rf64 f,cf=! f,ci=! ",
            "f,f32=Rf64 f,f64=Rf64 f,f128=Rf128 f,f32x=Rf32x f,f64x=Rf64x d,ld=Rf128 ",
            "d,i=Rf64 d,cf=! d,ci=! d,f32=Rf64 d,f64=Rf64 d,f128=Rf128 d,f32x=Rf64 ",
            "d,f64x=Rf64x ld,i=Rf128 ld,cf=! ld,ci=! ld,f32=Rf128 ld,f64=Rf128 ",
            "ld,f128=Rf128 ld,f32x=Rf128 ld,f64x=Rf128 i,cf=! i,ci=! i,f32=Rf64 ",
            "i,f64=Rf64 i,f128=Rf128 i,f32x=Rf32x i,f64x=Rf64x cf,ci=! cf,f32=! cf,f64=! ",
            "cf,f128=! cf,f32x=! cf,f64x=! ci,f32=! ci,f64=! ci,f128=! ci,f32x=! ",
            "ci,f64x=! f32,f64=Rf64 f32,f128=Rf128 f32,f32x=Rf32x f32,f64x=Rf64x ",
            "f64,f128=Rf128 f64,f32x=Rf64 f64,f64x=Rf64x f128,f32x=Rf128 f128,f64x=Rf128 ",
            "f32x,f64x=Rf64x ",
        ),
    ),
    (
        "f64add f64sub f64mul f64div",
        concat!(
            "f,f=Rf128 d,d=Rf128 ld,ld=Rf128 i,i=Rf128 cf,cf=! cd,cd=! cld,cld=! ci,ci=! ",
            "f16,f16=Rf128 f32,f32=Rf128 f64,f64=Rf128 f128,f128=Rf128 f32x,f32x=Rf128 ",
            "f64x,f64x=Rf64x cf16,cf16=! cf32,cf32=! cf64,cf64=! cf128,cf128=! ",
            "cf32x,cf32x=! cf64x,cf64x=! f,d=Rf128 f,ld=Rf128 f,i=Rf128 f,cf=! f,ci=! ",
            "f,f32=Rf128 f,f64=Rf128 f,f128=Rf128 f,f32x=Rf128 f,f64x=Rf64x d,ld=Rf128 ",
            "d,i=Rf128 d,cf=! d,ci=! d,f32=Rf128 d,f64=Rf128 d,f128=Rf128 d,f32x=Rf128 ",
            "d,f64x=Rf64x ld,i=Rf128 ld,cf=! ld,ci=! ld,f32=Rf128 ld,f64=Rf128 ",
            "ld,f128=Rf128 ld,f32x=Rf128 ld,f64x=Rf128 i,cf=! i,ci=! i,f32=Rf128 ",
            "i,f64=Rf128 i,f128=Rf128 i,f32x=Rf128 i,f64x=Rf64x cf,ci=! cf,f32=! ",
            "cf,f64=! cf,f128=! cf,f32x=! cf,f64x=! ci,f32=! ci,f64=! ci,f128=! ",
            "ci,f32x=! ci,f64x=! f32,f64=Rf128 f32,f128=Rf128 f32,f32x=Rf128 ",
            "f32,f64x=Rf64x f64,f128=Rf128 f64,f32x=Rf128 f64,f64x=Rf64x f128,f32x=Rf128 ",
            "f128,f64x=Rf128 f32x,f64x=Rf64x ",
        ),
    ),
    (
        "f32xadd f32xsub f32xmul f32xdiv",
        concat!(
            "f,f=Rf64x d,d=Rf64x ld,ld=Rf64x i,i=Rf64x cf,cf=! cd,cd=! cld,cld=! ci,ci=! ",
            "f16,f16=Rf64x f32,f32=Rf64x f64,f64=Rf64 f128,f128=Rf128 f32x,f32x=Rf64x ",
            "f64x,f64x=Rf64x cf16,cf16=! cf32,cf32=! cf64,cf64=! cf128,cf128=! ",
            "cf32x,cf32x=! cf64x,cf64x=! f,d=Rf64x f,ld=Rf64x f,i=Rf64x f,cf=! f,ci=! ",
            "f,f32=Rf64x f,f64=Rf64 f,f128=Rf128 f,f32x=Rf64x f,f64x=Rf64x d,ld=Rf64x ",
            "d,i=Rf64x d,cf=! d,ci=! d,f32=Rf64x d,f64=Rf64 d,f128=Rf128 d,f32x=Rf64x ",
            "d,f64x=Rf64x ld,i=Rf64x ld,cf=! ld,ci=! ld,f32=Rf64x ld,f64=Rf64x ",
            "ld,f128=Rf128 ld,f32x=Rf64x ld,f64x=Rf64x i,cf=! i,ci=! i,f32=Rf64x ",
            "i,f64=Rf64 i,f128=Rf128 i,f32x=Rf64x i,f64x=Rf64x cf,ci=! cf,f32=! cf,f64=! ",
            "cf,f128=! cf,f32x=! cf,f64x=! ci,f32=! ci,f64=! ci,f128=! ci,f32x=! ",
            "ci,f64x=! f32,f64=Rf64 f32,f128=Rf128 f32,f32x=Rf64x f32,f64x=Rf64x ",
            "f64,f128=Rf128 f64,f32x=Rf64 f64,f64x=Rf64x f128,f32x=Rf128 f128,f64x=Rf128 ",
            "f32x,f64x=Rf64x ",
        ),
    ),
    (
        "f64xadd f64xsub f64xmul f64xdiv",
        concat!(
            "f,f=Rf128 d,d=Rf128 ld,ld=Rf128 i,i=Rf128 cf,cf=Rf128 cd,cd=Rf128 ",
            "cld,cld=Rf128 ci,ci=Rf128 f16,f16=Rf128 f32,f32=Rf128 f64,f64=Rf128 ",
            "f128,f128=Rf128 f32x,f32x=Rf128 f64x,f64x=Rf128 cf16,cf16=Rf128 ",
            "cf32,cf32=Rf128 cf64,cf64=Rf128 cf128,cf128=Rf128 cf32x,cf32x=Rf128 ",
            "cf64x,cf64x=Rf128 f,d=Rf128 f,ld=Rf128 f,i=Rf128 f,cf=Rf128 f,ci=Rf128 ",
            "f,f32=Rf128 f,f64=Rf128 f,f128=Rf128 f,f32x=Rf128 f,f64x=Rf128 d,ld=Rf128 ",
            "d,i=Rf128 d,cf=Rf128 d,ci=Rf128 d,f32=Rf128 d,f64=Rf128 d,f128=Rf128 ",
            "d,f32x=Rf128 d,f64x=Rf128 ld,i=Rf128 ld,cf=Rf128 ld,ci=Rf128 ld,f32=Rf128 ",
            "ld,f64=Rf128 ld,f128=Rf128 ld,f32x=Rf128 ld,f64x=Rf128 i,cf=Rf128 ",
            "i,ci=Rf128 i,f32=Rf128 i,f64=Rf128 i,f128=Rf128 i,f32x=Rf128 i,f64x=Rf128 ",
            "cf,ci=Rf128 cf,f32=Rf128 cf,f64=Rf128 cf,f128=Rf128 cf,f32x=Rf128 ",
            "cf,f64x=Rf128 ci,f32=Rf128 ci,f64=Rf128 ci,f128=Rf128 ci,f32x=Rf128 ",
            "ci,f64x=Rf128 f32,f64=Rf128 f32,f128=Rf128 f32,f32x=Rf128 f32,f64x=Rf128 ",
            "f64,f128=Rf128 f64,f32x=Rf128 f64,f64x=Rf128 f128,f32x=Rf128 ",
            "f128,f64x=Rf128 f32x,f64x=Rf128 ",
        ),
    ),
    (
        "fma",
        concat!(
            "f,f,f=Rf d,d,d=R ld,ld,ld=Rl i,i,i=R cf,cf,cf=! cd,cd,cd=! cld,cld,cld=! ",
            "ci,ci,ci=! f16,f16,f16=! f32,f32,f32=Rf32 f64,f64,f64=Rf64 ",
            "f128,f128,f128=Rf128 f32x,f32x,f32x=Rf32x f64x,f64x,f64x=Rf64x ",
            "cf16,cf16,cf16=! cf32,cf32,cf32=! cf64,cf64,cf64=! cf128,cf128,cf128=! ",
            "cf32x,cf32x,cf32x=! cf64x,cf64x,cf64x=! f,d,d=R f,ld,ld=Rl f,i,i=R ",
            "f,cf,cf=! f,ci,ci=! f,f32,f32=Rf32 f,f64,f64=Rf64 f,f128,f128=Rf128 ",
            "f,f32x,f32x=Rf32x f,f64x,f64x=Rf64x d,ld,ld=Rl d,i,i=R d,cf,cf=! d,ci,ci=! ",
            "d,f32,f32=R d,f64,f64=Rf64 d,f128,f128=Rf128 d,f32x,f32x=R ",
            "d,f64x,f64x=Rf64x ld,i,i=Rl ld,cf,cf=! ld,ci,ci=! ld,f32,f32=Rl ",
            "ld,f64,f64=Rl ld,f128,f128=Rf128 ld,f32x,f32x=Rl ld,f64x,f64x=Rl i,cf,cf=! ",
            "i,ci,ci=! i,f32,f32=R i,f64,f64=Rf64 i,f128,f128=Rf128 i,f32x,f32x=Rf32x ",
            "i,f64x,f64x=Rf64x cf,ci,ci=! cf,f32,f32=! cf,f64,f64=! cf,f128,f128=! ",
            "cf,f32x,f32x=! cf,f64x,f64x=! ci,f32,f32=! ci,f64,f64=! ci,f128,f128=! ",
            "ci,f32x,f32x=! ci,f64x,f64x=! f32,f64,f64=Rf64 f32,f128,f128=Rf128 ",
            "f32,f32x,f32x=Rf32x f32,f64x,f64x=Rf64x f64,f128,f128=Rf128 ",
            "f64,f32x,f32x=Rf64 f64,f64x,f64x=Rf64x f128,f32x,f32x=Rf128 ",
            "f128,f64x,f64x=Rf128 f32x,f64x,f64x=Rf64x ",
        ),
    ),
    (
        "ffma",
        concat!(
            "f,f,f=R d,d,d=R ld,ld,ld=Rl i,i,i=R cf,cf,cf=! cd,cd,cd=! cld,cld,cld=! ",
            "ci,ci,ci=! f16,f16,f16=R f32,f32,f32=R f64,f64,f64=R f128,f128,f128=!|Rl ",
            "f32x,f32x,f32x=R f64x,f64x,f64x=Rl cf16,cf16,cf16=! cf32,cf32,cf32=! ",
            "cf64,cf64,cf64=! cf128,cf128,cf128=! cf32x,cf32x,cf32x=! ",
            "cf64x,cf64x,cf64x=! f,d,d=R f,ld,ld=Rl f,i,i=R f,cf,cf=! f,ci,ci=! ",
            "f,f32,f32=R f,f64,f64=R f,f128,f128=!|Rl f,f32x,f32x=R f,f64x,f64x=Rl ",
            "d,ld,ld=Rl d,i,i=R d,cf,cf=! d,ci,ci=! d,f32,f32=R d,f64,f64=R ",
            "d,f128,f128=!|Rl d,f32x,f32x=R d,f64x,f64x=Rl ld,i,i=Rl ld,cf,cf=! ",
            "ld,ci,ci=! ld,f32,f32=Rl ld,f64,f64=Rl ld,f128,f128=!|Rl ld,f32x,f32x=Rl ",
            "ld,f64x,f64x=Rl i,cf,cf=! i,ci,ci=! i,f32,f32=R i,f64,f64=R ",
            "i,f128,f128=!|Rl i,f32x,f32x=R i,f64x,f64x=Rl cf,ci,ci=! cf,f32,f32=! ",
            "cf,f64,f64=! cf,f128,f128=! cf,f32x,f32x=! cf,f64x,f64x=! ci,f32,f32=! ",
            "ci,f64,f64=! ci,f128,f128=! ci,f32x,f32x=! ci,f64x,f64x=! f32,f64,f64=R ",
            "f32,f128,f128=!|Rl f32,f32x,f32x=R f32,f64x,f64x=Rl f64,f128,f128=!|Rl ",
            "f64,f32x,f32x=R f64,f64x,f64x=Rl f128,f32x,f32x=!|Rl f128,f64x,f64x=!|Rl ",
            "f32x,f64x,f64x=Rl ",
        ),
    ),
    (
        "dfma",
        concat!(
            "f,f,f=Rl d,d,d=Rl ld,ld,ld=Rl i,i,i=Rl cf,cf,cf=Rl cd,cd,cd=Rl ",
            "cld,cld,cld=Rl ci,ci,ci=Rl f16,f16,f16=Rl f32,f32,f32=Rl f64,f64,f64=Rl ",
            "f128,f128,f128=Rl f32x,f32x,f32x=Rl f64x,f64x,f64x=Rl cf16,cf16,cf16=Rl ",
            "cf32,cf32,cf32=Rl cf64,cf64,cf64=Rl cf128,cf128,cf128=Rl ",
            "cf32x,cf32x,cf32x=Rl cf64x,cf64x,cf64x=Rl f,d,d=Rl f,ld,ld=Rl f,i,i=Rl ",
            "f,cf,cf=Rl f,ci,ci=Rl f,f32,f32=Rl f,f64,f64=Rl f,f128,f128=Rl ",
            "f,f32x,f32x=Rl f,f64x,f64x=Rl d,ld,ld=Rl d,i,i=Rl d,cf,cf=Rl d,ci,ci=Rl ",
            "d,f32,f32=Rl d,f64,f64=Rl d,f128,f128=Rl d,f32x,f32x=Rl d,f64x,f64x=Rl ",
            "ld,i,i=Rl ld,cf,cf=Rl ld,ci,ci=Rl ld,f32,f32=Rl ld,f64,f64=Rl ",
            "ld,f128,f128=Rl ld,f32x,f32x=Rl ld,f64x,f64x=Rl i,cf,cf=Rl i,ci,ci=Rl ",
            "i,f32,f32=Rl i,f64,f64=Rl i,f128,f128=Rl i,f32x,f32x=Rl i,f64x,f64x=Rl ",
            "cf,ci,ci=Rl cf,f32,f32=Rl cf,f64,f64=Rl cf,f128,f128=Rl cf,f32x,f32x=Rl ",
            "cf,f64x,f64x=Rl ci,f32,f32=Rl ci,f64,f64=Rl ci,f128,f128=Rl ci,f32x,f32x=Rl ",
            "ci,f64x,f64x=Rl f32,f64,f64=Rl f32,f128,f128=Rl f32,f32x,f32x=Rl ",
            "f32,f64x,f64x=Rl f64,f128,f128=Rl f64,f32x,f32x=Rl f64,f64x,f64x=Rl ",
            "f128,f32x,f32x=Rl f128,f64x,f64x=Rl f32x,f64x,f64x=Rl ",
        ),
    ),
    (
        "f32fma",
        concat!(
            "f,f,f=Rf64 d,d,d=Rf64 ld,ld,ld=Rf128 i,i,i=Rf64 cf,cf,cf=! cd,cd,cd=! ",
            "cld,cld,cld=! ci,ci,ci=! f16,f16,f16=Rf64 f32,f32,f32=Rf64 f64,f64,f64=Rf64 ",
            "f128,f128,f128=Rf128 f32x,f32x,f32x=Rf32x f64x,f64x,f64x=Rf64x ",
            "cf16,cf16,cf16=! cf32,cf32,cf32=! cf64,cf64,cf64=! cf128,cf128,cf128=! ",
            "cf32x,cf32x,cf32x=! cf64x,cf64x,cf64x=! f,d,d=Rf64 f,ld,ld=Rf128 f,i,i=Rf64 ",
            "f,cf,cf=! f,ci,ci=! f,f32,f32=Rf64 f,f64,f64=Rf64 f,f128,f128=Rf128 ",
            "f,f32x,f32x=Rf32x f,f64x,f64x=Rf64x d,ld,ld=Rf128 d,i,i=Rf64 d,cf,cf=! ",
            "d,ci,ci=! d,f32,f32=Rf64 d,f64,f64=Rf64 d,f128,f128=Rf128 d,f32x,f32x=Rf64 ",
            "d,f64x,f64x=Rf64x ld,i,i=Rf128 ld,cf,cf=! ld,ci,ci=! ld,f32,f32=Rf128 ",
            "ld,f64,f64=Rf128 ld,f128,f128=Rf128 ld,f32x,f32x=Rf128 ld,f64x,f64x=Rf128 ",
            "i,cf,cf=! i,ci,ci=! i,f32,f32=Rf64 i,f64,f64=Rf64 i,f128,f128=Rf128 ",
            "i,f32x,f32x=Rf32x i,f64x,f64x=Rf64x cf,ci,ci=! cf,f32,f32=! cf,f64,f64=! ",
            "cf,f128,f128=! cf,f32x,f32x=! cf,f64x,f64x=! ci,f32,f32=! ci,f64,f64=! ",
            "ci,f128,f128=! ci,f32x,f32x=! ci,f64x,f64x=! f32,f64,f64=Rf64 ",
            "f32,f128,f128=Rf128 f32,f32x,f32x=Rf32x f32,f64x,f64x=Rf64x ",
            "f64,f128,f128=Rf128 f64,f32x,f32x=Rf64 f64,f64x,f64x=Rf64x ",
            "f128,f32x,f32x=Rf128 f128,f64x,f64x=Rf128 f32x,f64x,f64x=Rf64x ",
        ),
    ),
    (
        "f64fma",
        concat!(
            "f,f,f=Rf128 d,d,d=Rf128 ld,ld,ld=Rf128 i,i,i=Rf128 cf,cf,cf=! cd,cd,cd=! ",
            "cld,cld,cld=! ci,ci,ci=! f16,f16,f16=Rf128 f32,f32,f32=Rf128 ",
            "f64,f64,f64=Rf128 f128,f128,f128=Rf128 f32x,f32x,f32x=Rf128 ",
            "f64x,f64x,f64x=Rf64x cf16,cf16,cf16=! cf32,cf32,cf32=! cf64,cf64,cf64=! ",
            "cf128,cf128,cf128=! cf32x,cf32x,cf32x=! cf64x,cf64x,cf64x=! f,d,d=Rf128 ",
            "f,ld,ld=Rf128 f,i,i=Rf128 f,cf,cf=! f,ci,ci=! f,f32,f32=Rf128 ",
            "f,f64,f64=Rf128 f,f128,f128=Rf128 f,f32x,f32x=Rf128 f,f64x,f64x=Rf64x ",
            "d,ld,ld=Rf128 d,i,i=Rf128 d,cf,cf=! d,ci,ci=! d,f32,f32=Rf128 ",
            "d,f64,f64=Rf128 d,f128,f128=Rf128 d,f32x,f32x=Rf128 d,f64x,f64x=Rf64x ",
            "ld,i,i=Rf128 ld,cf,cf=! ld,ci,ci=! ld,f32,f32=Rf128 ld,f64,f64=Rf128 ",
            "ld,f128,f128=Rf128 ld,f32x,f32x=Rf128 ld,f64x,f64x=Rf128 i,cf,cf=! ",
            "i,ci,ci=! i,f32,f32=Rf128 i,f64,f64=Rf128 i,f128,f128=Rf128 ",
            "i,f32x,f32x=Rf128 i,f64x,f64x=Rf64x cf,ci,ci=! cf,f32,f32=! cf,f64,f64=! ",
            "cf,f128,f128=! cf,f32x,f32x=! cf,f64x,f64x=! ci,f32,f32=! ci,f64,f64=! ",
            "ci,f128,f128=! ci,f32x,f32x=! ci,f64x,f64x=! f32,f64,f64=Rf128 ",
            "f32,f128,f128=Rf128 f32,f32x,f32x=Rf128 f32,f64x,f64x=Rf64x ",
            "f64,f128,f128=Rf128 f64,f32x,f32x=Rf128 f64,f64x,f64x=Rf64x ",
            "f128,f32x,f32x=Rf128 f128,f64x,f64x=Rf128 f32x,f64x,f64x=Rf64x ",
        ),
    ),
    (
        "f32xfma",
        concat!(
            "f,f,f=Rf64x d,d,d=Rf64x ld,ld,ld=Rf64x i,i,i=Rf64x cf,cf,cf=! cd,cd,cd=! ",
            "cld,cld,cld=! ci,ci,ci=! f16,f16,f16=Rf64x f32,f32,f32=Rf64x ",
            "f64,f64,f64=Rf64 f128,f128,f128=Rf128 f32x,f32x,f32x=Rf64x ",
            "f64x,f64x,f64x=Rf64x cf16,cf16,cf16=! cf32,cf32,cf32=! cf64,cf64,cf64=! ",
            "cf128,cf128,cf128=! cf32x,cf32x,cf32x=! cf64x,cf64x,cf64x=! f,d,d=Rf64x ",
            "f,ld,ld=Rf64x f,i,i=Rf64x f,cf,cf=! f,ci,ci=! f,f32,f32=Rf64x ",
            "f,f64,f64=Rf64 f,f128,f128=Rf128 f,f32x,f32x=Rf64x f,f64x,f64x=Rf64x ",
            "d,ld,ld=Rf64x d,i,i=Rf64x d,cf,cf=! d,ci,ci=! d,f32,f32=Rf64x ",
            "d,f64,f64=Rf64 d,f128,f128=Rf128 d,f32x,f32x=Rf64x d,f64x,f64x=Rf64x ",
            "ld,i,i=Rf64x ld,cf,cf=! ld,ci,ci=! ld,f32,f32=Rf64x ld,f64,f64=Rf64x ",
            "ld,f128,f128=Rf128 ld,f32x,f32x=Rf64x ld,f64x,f64x=Rf64x i,cf,cf=! ",
            "i,ci,ci=! i,f32,f32=Rf64x i,f64,f64=Rf64 i,f128,f128=Rf128 ",
            "i,f32x,f32x=Rf64x i,f64x,f64x=Rf64x cf,ci,ci=! cf,f32,f32=! cf,f64,f64=! ",
            "cf,f128,f128=! cf,f32x,f32x=! cf,f64x,f64x=! ci,f32,f32=! ci,f64,f64=! ",
            "ci,f128,f128=! ci,f32x,f32x=! ci,f64x,f64x=! f32,f64,f64=Rf64 ",
            "f32,f128,f128=Rf128 f32,f32x,f32x=Rf64x f32,f64x,f64x=Rf64x ",
            "f64,f128,f128=Rf128 f64,f32x,f32x=Rf64 f64,f64x,f64x=Rf64x ",
            "f128,f32x,f32x=Rf128 f128,f64x,f64x=Rf128 f32x,f64x,f64x=Rf64x ",
        ),
    ),
    (
        "f64xfma",
        concat!(
            "f,f,f=Rf128 d,d,d=Rf128 ld,ld,ld=Rf128 i,i,i=Rf128 cf,cf,cf=Rf128 ",
            "cd,cd,cd=Rf128 cld,cld,cld=Rf128 ci,ci,ci=Rf128 f16,f16,f16=Rf128 ",
            "f32,f32,f32=Rf128 f64,f64,f64=Rf128 f128,f128,f128=Rf128 ",
            "f32x,f32x,f32x=Rf128 f64x,f64x,f64x=Rf128 cf16,cf16,cf16=Rf128 ",
            "cf32,cf32,cf32=Rf128 cf64,cf64,cf64=Rf128 cf128,cf128,cf128=Rf128 ",
            "cf32x,cf32x,cf32x=Rf128 cf64x,cf64x,cf64x=Rf128 f,d,d=Rf128 f,ld,ld=Rf128 ",
            "f,i,i=Rf128 f,cf,cf=Rf128 f,ci,ci=Rf128 f,f32,f32=Rf128 f,f64,f64=Rf128 ",
            "f,f128,f128=Rf128 f,f32x,f32x=Rf128 f,f64x,f64x=Rf128 d,ld,ld=Rf128 ",
            "d,i,i=Rf128 d,cf,cf=Rf128 d,ci,ci=Rf128 d,f32,f32=Rf128 d,f64,f64=Rf128 ",
            "d,f128,f128=Rf128 d,f32x,f32x=Rf128 d,f64x,f64x=Rf128 ld,i,i=Rf128 ",
            "ld,cf,cf=Rf128 ld,ci,ci=Rf128 ld,f32,f32=Rf128 ld,f64,f64=Rf128 ",
            "ld,f128,f128=Rf128 ld,f32x,f32x=Rf128 ld,f64x,f64x=Rf128 i,cf,cf=Rf128 ",
            "i,ci,ci=Rf128 i,f32,f32=Rf128 i,f64,f64=Rf128 i,f128,f128=Rf128 ",
            "i,f32x,f32x=Rf128 i,f64x,f64x=Rf128 cf,ci,ci=Rf128 cf,f32,f32=Rf128 ",
            "cf,f64,f64=Rf128 cf,f128,f128=Rf128 cf,f32x,f32x=Rf128 cf,f64x,f64x=Rf128 ",
            "ci,f32,f32=Rf128 ci,f64,f64=Rf128 ci,f128,f128=Rf128 ci,f32x,f32x=Rf128 ",
            "ci,f64x,f64x=Rf128 f32,f64,f64=Rf128 f32,f128,f128=Rf128 ",
            "f32,f32x,f32x=Rf128 f32,f64x,f64x=Rf128 f64,f128,f128=Rf128 ",
            "f64,f32x,f32x=Rf128 f64,f64x,f64x=Rf128 f128,f32x,f32x=Rf128 ",
            "f128,f64x,f64x=Rf128 f32x,f64x,f64x=Rf128 ",
        ),
    ),
];

/// Each abbreviation of a table row: the C type, and a value for it.
const TYPES: &[(&str, &str, &str)] = &[
    ("f", "float", "0.5f"),
    ("d", "double", "0.5"),
    ("ld", "long double", "0.5L"),
    ("i", "int", "1"),
    ("cf", "float _Complex", "0.5f + 0.25fi"),
    ("cd", "double _Complex", "0.5 + 0.25i"),
    ("cld", "long double _Complex", "0.5L + 0.25Li"),
    ("ci", "_Complex int", "1 + 2i"),
    ("f16", "_Float16", "0.5f16"),
    ("f32", "_Float32", "0.5f32"),
    ("f64", "_Float64", "0.5f64"),
    ("f128", "_Float128", "0.5f128"),
    ("f32x", "_Float32x", "0.5f32x"),
    ("f64x", "_Float64x", "0.5f64x"),
    ("cf16", "_Complex _Float16", "0.5f16 + 0.25f16i"),
    ("cf32", "_Complex _Float32", "0.5f32 + 0.25f32i"),
    ("cf64", "_Complex _Float64", "0.5f64 + 0.25f64i"),
    ("cf128", "_Complex _Float128", "0.5f128 + 0.25f128i"),
    ("cf32x", "_Complex _Float32x", "0.5f32x + 0.25f32xi"),
    ("cf64x", "_Complex _Float64x", "0.5f64x + 0.25f64xi"),
];

/// The macros glibc's <tgmath.h> defines without _GNU_SOURCE: C17's, and
/// `scalb` (`__USE_MISC`, on by default).
const DEFAULT_MACROS: &str = "acos asin atan acosh asinh atanh cos sin tan cosh sinh tanh exp log \
    sqrt fabs log10 pow atan2 cbrt ceil copysign erf erfc exp2 expm1 fdim floor fma fmax fmin fmod \
    frexp hypot ilogb ldexp lgamma llrint llround log1p log2 logb lrint lround nearbyint nextafter \
    nexttoward remainder remquo rint round scalbln scalbn tgamma trunc carg cimag conj cproj creal \
    scalb";

/// The `_Float16` narrowing macros: glibc has no `_Float16` functions on
/// either target, so its <tgmath.h> defines none of these.
const ABSENT_MACROS: &str = "f16add f16sub f16mul f16div f16sqrt f16fma";

#[derive(Clone, Copy, PartialEq)]
enum Arch {
    X86_64,
    Aarch64,
}

/// One macro call of a table row.
struct Case {
    /// `sqrt(g_f32)`.
    call: String,
    /// The function gcc calls, `None` where it refuses the call.
    function: Option<String>,
    /// The call of that function with the same arguments.
    direct: String,
}

/// The complex family a real-or-complex macro reaches for a complex argument.
fn complex_family(m: &str) -> String {
    match m {
        "fabs" => "cabs".to_string(),
        _ => format!("c{m}"),
    }
}

/// The non-generic arguments after a macro's generic ones.
fn extra_args(m: &str) -> &'static str {
    match m {
        "frexp" | "remquo" => ", &q",
        "ldexp" | "scalbn" => ", 3",
        "scalbln" => ", 3L",
        "nexttoward" => ", g_ld",
        "fromfp" | "ufromfp" | "fromfpx" | "ufromfpx" => ", 0, 32",
        _ => "",
    }
}

/// Every case of every class for `arch`, with or without _GNU_SOURCE.
fn cases(arch: Arch, gnu: bool) -> Vec<Case> {
    let default_macros: Vec<&str> = DEFAULT_MACROS.split_whitespace().collect();
    let mut out = Vec::new();
    for (macros, table) in CLASSES {
        for m in macros.split_whitespace() {
            if !gnu && !default_macros.contains(&m) {
                continue;
            }
            for row in table.split_whitespace() {
                let (types, pick) = row.split_once('=').expect("row is types=pick");
                let pick = match pick.split_once('|') {
                    Some((x86, a64)) => match arch {
                        Arch::X86_64 => x86,
                        Arch::Aarch64 => a64,
                    },
                    None => pick,
                };
                let args: Vec<String> = types.split(',').map(|t| format!("g_{t}")).collect();
                let args = format!("{}{}", args.join(", "), extra_args(m));
                let function = match pick.split_at(1) {
                    ("!", _) => None,
                    ("R", suffix) => Some(format!("{m}{suffix}")),
                    ("C", suffix) => Some(format!("{}{suffix}", complex_family(m))),
                    _ => panic!("bad pick {pick} for {m}"),
                };
                // glibc without _GNU_SOURCE: no _FloatN functions, no clog10.
                let function = function.filter(|f| {
                    gnu || !(f.contains("f16")
                        || f.contains("f32")
                        || f.contains("f64")
                        || f.contains("f128")
                        || f.starts_with("clog10"))
                });
                let direct = function
                    .as_ref()
                    .map(|f| format!("{f}({args})"))
                    .unwrap_or_default();
                out.push(Case {
                    call: format!("{m}({args})"),
                    function,
                    direct,
                });
            }
        }
    }
    out
}

/// The declarations every program starts with: a global of each type, and
/// a check that the macros glibc defines in this mode are the ones defined.
fn prologue(gnu: bool) -> String {
    let mut s = String::new();
    if gnu {
        s += "#define _GNU_SOURCE\n";
    }
    s += "#include <tgmath.h>\n#include <stdio.h>\nvoid sink(void *);\nint q;\n";
    for (abbr, typ, value) in TYPES {
        s += &format!("{typ} g_{abbr} = {value};\n");
    }
    let default_macros: Vec<&str> = DEFAULT_MACROS.split_whitespace().collect();
    for (macros, _) in CLASSES {
        for m in macros.split_whitespace() {
            let defined = gnu || default_macros.contains(&m);
            let test = if defined { "ifndef" } else { "ifdef" };
            s += &format!("#{test} {m}\n#error \"{m}\"\n#endif\n");
        }
    }
    for m in ABSENT_MACROS.split_whitespace() {
        s += &format!("#ifdef {m}\n#error \"{m}\"\n#endif\n");
    }
    s
}

/// The c17 options for `arch`, or `None` where its headers are missing.
fn target_args(arch: Arch) -> Option<Vec<&'static str>> {
    let host = if cfg!(target_arch = "x86_64") {
        Arch::X86_64
    } else {
        Arch::Aarch64
    };
    if arch == host {
        return Some(Vec::new());
    }
    if arch == Arch::Aarch64 && std::path::Path::new("/usr/aarch64-linux-gnu/include").is_dir() {
        return Some(AARCH64_TARGET_ARGS.to_vec());
    }
    eprintln!("SKIP <tgmath.h> table for the other target: no headers");
    None
}

/// Compile `src` to assembly for `arch` and return it, or the diagnostics.
fn compile(src: &str, args: &[&str]) -> Result<String, String> {
    let dir = crate::common::work_dir("tgmath_table");
    let c = dir.path().join("t.c");
    let s = dir.path().join("t.s");
    std::fs::write(&c, src).expect("write source");
    let mut argv = args.to_vec();
    argv.extend_from_slice(&["-O0", "-fno-builtin", "-S", "-o"]);
    argv.push(s.to_str().unwrap());
    argv.push(c.to_str().unwrap());
    let run = run_c17(&argv);
    if run.success {
        Ok(std::fs::read_to_string(&s).expect("read asm"))
    } else {
        Err(run.stderr)
    }
}

/// The functions each `f_<n>` of `asm` calls, other than `sink`.
fn calls_by_function(asm: &str) -> std::collections::HashMap<usize, Vec<String>> {
    let mut out = std::collections::HashMap::new();
    let mut current: Option<usize> = None;
    for line in asm.lines() {
        let line = line.trim();
        // A function's own labels are local (`.L`); any other ends it.
        if let Some(label) = line.strip_suffix(':').filter(|l| !l.starts_with('.')) {
            current = label.strip_prefix("f_").and_then(|n| n.parse().ok());
            if let Some(n) = current {
                out.insert(n, Vec::new());
            }
            continue;
        }
        let Some(n) = current else { continue };
        let mut words = line.split_whitespace();
        if let (Some("call" | "bl"), Some(target)) = (words.next(), words.next()) {
            let target = target.split('@').next().unwrap();
            if target != "sink" && !target.starts_with("__") {
                out.get_mut(&n).unwrap().push(target.to_string());
            }
        }
    }
    out
}

/// The function each call reaches, in gcc's words where it refuses one,
/// for both targets and both modes.
#[test]
#[cfg(target_os = "linux")]
fn tgmath_picks_gccs_function_for_every_macro_and_type() {
    for arch in [Arch::X86_64, Arch::Aarch64] {
        let Some(args) = target_args(arch) else {
            continue;
        };
        for gnu in [true, false] {
            let all = cases(arch, gnu);
            // The calls gcc accepts: each `f_<n>` calls gcc's function.
            let mut src = prologue(gnu);
            for (n, case) in all.iter().enumerate() {
                if case.function.is_some() {
                    let e = &case.call;
                    src += &format!("void f_{n}(void) {{ __typeof__({e}) r = {e}; sink(&r); }}\n");
                }
            }
            let asm = compile(&src, &args)
                .unwrap_or_else(|e| panic!("accepted calls were refused (gnu={gnu}):\n{e}"));
            let calls = calls_by_function(&asm);
            for (n, case) in all.iter().enumerate() {
                if let Some(f) = &case.function {
                    assert_eq!(
                        calls.get(&n).map(Vec::as_slice),
                        Some(std::slice::from_ref(f)),
                        "{} (gnu={gnu}) calls the wrong function",
                        case.call
                    );
                }
            }
            // The calls gcc refuses: each line is refused, in its words.
            let mut src = prologue(gnu);
            let refused: Vec<&Case> = all.iter().filter(|c| c.function.is_none()).collect();
            for (n, case) in refused.iter().enumerate() {
                src += &format!(
                    "#line {}\nvoid f_{n}(void) {{ (void){}; }}\n",
                    n + 1_000_000,
                    case.call
                );
            }
            let err = compile(&src, &args).expect_err("refused calls were accepted");
            for (n, case) in refused.iter().enumerate() {
                let line = format!(":{}:", n + 1_000_000);
                assert!(
                    err.lines().any(|l| l.contains(&line)
                        && l.ends_with("error: no matching function for type-generic call")),
                    "{} (gnu={gnu}) was not refused as gcc refuses it:\n{err}",
                    case.call
                );
            }
        }
    }
}

/// The program that checks, for every call gcc accepts, that the macro's
/// result has the type and the value of a direct call of gcc's function.
fn value_program(arch: Arch, gnu: bool) -> String {
    let mut src = prologue(gnu);
    src += "int main(void) {\n    int bad = 0;\n";
    for case in cases(arch, gnu) {
        if case.function.is_none() {
            continue;
        }
        let (call, direct) = (&case.call, &case.direct);
        src += &format!(
            "    {{ __typeof__({direct}) want = {direct}, got = {call};\n\
             \x20     if (!_Generic({call}, __typeof__(want): 1, default: 0)\n\
             \x20         || !(got == want || (got != got && want != want))) {{\n\
             \x20         printf(\"FAIL %s\\n\", \"{call}\"); bad = 1; }} }}\n"
        );
    }
    src += "    return bad;\n}\n";
    src
}

/// The value and type of every accepted call, run on the host and, where the
/// cross toolchain is installed, on aarch64 under qemu.
#[test]
#[cfg(target_os = "linux")]
fn tgmath_values_are_the_picked_functions() {
    let host = if cfg!(target_arch = "x86_64") {
        Arch::X86_64
    } else {
        Arch::Aarch64
    };
    for gnu in [true, false] {
        let src = value_program(host, gnu);
        assert_eq!(
            compile_and_run("tgmath_values", &src, &["-lm".to_string()]),
            0,
            "gnu={gnu}"
        );
        if host == Arch::X86_64 {
            let src = value_program(Arch::Aarch64, gnu);
            if let Some(code) =
                compile_and_run_aarch64_with("tgmath_values", &src, &["-O0"], &["-lm"])
            {
                assert_eq!(code, 0, "aarch64, gnu={gnu}");
            }
        }
    }
}
