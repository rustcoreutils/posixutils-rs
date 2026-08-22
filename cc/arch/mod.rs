//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Architecture-specific predefined macros and code generators
//

pub const DEFAULT_LIR_BUFFER_CAPACITY: usize = 5000;

pub mod aarch64;
pub mod asm_constraints;
pub mod codegen;
pub mod dwarf;
pub mod lir;
pub mod mapping;
pub mod regalloc;
pub mod x86_64;

// Re-export inline asm support traits and functions
pub use codegen::{substitute_asm_operands, AsmOperandFormatter, AsmOperandSlot};

use crate::target::{Arch, Os, Target};

/// Get architecture-specific predefined macros as (name, value) pairs
pub fn get_arch_macros(target: &Target) -> Vec<(&'static str, Option<&'static str>)> {
    // long is 64-bit on LP64 (Unix), 32-bit on LLP64 (Windows)
    let long_size = if target.long_width == 64 { "8" } else { "4" };

    // Signedness: __CHAR_UNSIGNED__ is defined only if char is unsigned
    let char_unsigned: Option<&'static str> = if target.char_signed { None } else { Some("1") };

    let mut macros = vec![
        // Common architecture macros based on type sizes
        ("__CHAR_BIT__", Some("8")),
        ("__SIZEOF_POINTER__", Some("8")),
        // Type sizes
        ("__SIZEOF_SHORT__", Some("2")),
        ("__SIZEOF_INT__", Some("4")),
        ("__SIZEOF_LONG__", Some(long_size)),
        ("__SIZEOF_LONG_LONG__", Some("8")),
        ("__SIZEOF_FLOAT__", Some("4")),
        ("__SIZEOF_DOUBLE__", Some("8")),
    ];

    // __CHAR_UNSIGNED__: only define when char is unsigned.
    // An empty-body #define still satisfies #ifdef, so we must
    // not add the macro at all when char is signed.
    if let Some(val) = char_unsigned {
        macros.push(("__CHAR_UNSIGNED__", Some(val)));
    }

    // LP64 macros only on LP64 targets (Unix), not on LLP64 (Windows)
    if target.long_width == 64 {
        macros.push(("__LP64__", Some("1")));
        macros.push(("_LP64", Some("1")));
    }

    // Architecture-specific
    match target.arch {
        Arch::X86_64 => {
            macros.extend(x86_64::get_macros());
        }
        Arch::Aarch64 => {
            macros.extend(aarch64::get_macros());
        }
    }

    macros
}

/// Get type limit macros (for <limits.h> compatibility)
pub fn get_limit_macros(target: &Target) -> Vec<(&'static str, &'static str)> {
    // long is 64-bit on LP64 (Unix), 32-bit on LLP64 (Windows)
    let (long_max, long_width) = if target.long_width == 64 {
        ("9223372036854775807L", "64")
    } else {
        ("2147483647L", "32")
    };

    vec![
        // Character limits
        ("__SCHAR_MAX__", "127"),
        ("__SHRT_MAX__", "32767"),
        ("__INT_MAX__", "2147483647"),
        ("__LONG_MAX__", long_max),
        ("__LONG_LONG_MAX__", "9223372036854775807LL"),
        // Width macros
        ("__SCHAR_WIDTH__", "8"),
        ("__SHRT_WIDTH__", "16"),
        ("__INT_WIDTH__", "32"),
        ("__LONG_WIDTH__", long_width),
        ("__LLONG_WIDTH__", "64"),
        // Size type (always 64-bit on our targets)
        ("__SIZE_MAX__", "18446744073709551615UL"),
        ("__SIZE_WIDTH__", "64"),
        ("__PTRDIFF_MAX__", "9223372036854775807L"),
        ("__PTRDIFF_WIDTH__", "64"),
        ("__INTPTR_MAX__", "9223372036854775807L"),
        ("__INTPTR_WIDTH__", "64"),
        ("__UINTPTR_MAX__", "18446744073709551615UL"),
        // POSIX ssize_t limits (same as LONG_MAX/LONG_MIN on LP64)
        ("SSIZE_MAX", long_max),
    ]
}

/// Get type definition macros (for <stdint.h> and <stddef.h> compatibility)
/// These define the underlying C type for various abstract types
pub fn get_type_macros(target: &Target) -> Vec<(&'static str, &'static str)> {
    // The 64-bit exact/least/fast types are distinct types even at the same
    // width, so getting this wrong makes our <stdint.h> and the host's
    // disagree about int64_t — which is exactly what surfaced when
    // <stdint.h> stopped being delegated. Every target is LP64, so the choice
    // is the platform's convention, not the width: Darwin says `long long`.
    let (s64, u64_) = if target.int64_is_long_long() {
        ("long long int", "long long unsigned int")
    } else {
        ("long int", "long unsigned int")
    };
    let wint = wint_type(target);
    vec![
        // Fixed-width integer types
        ("__INT8_TYPE__", "signed char"),
        ("__INT16_TYPE__", "short"),
        ("__INT32_TYPE__", "int"),
        ("__INT64_TYPE__", s64),
        ("__UINT8_TYPE__", "unsigned char"),
        ("__UINT16_TYPE__", "unsigned short"),
        ("__UINT32_TYPE__", "unsigned int"),
        ("__UINT64_TYPE__", u64_),
        // Least-width integer types (same as fixed-width for common targets)
        ("__INT_LEAST8_TYPE__", "signed char"),
        ("__INT_LEAST16_TYPE__", "short"),
        ("__INT_LEAST32_TYPE__", "int"),
        ("__INT_LEAST64_TYPE__", s64),
        ("__UINT_LEAST8_TYPE__", "unsigned char"),
        ("__UINT_LEAST16_TYPE__", "unsigned short"),
        ("__UINT_LEAST32_TYPE__", "unsigned int"),
        ("__UINT_LEAST64_TYPE__", u64_),
        // Fast integer types (same as fixed-width for common targets)
        ("__INT_FAST8_TYPE__", "signed char"),
        ("__INT_FAST16_TYPE__", "short"),
        ("__INT_FAST32_TYPE__", "int"),
        ("__INT_FAST64_TYPE__", s64),
        ("__UINT_FAST8_TYPE__", "unsigned char"),
        ("__UINT_FAST16_TYPE__", "unsigned short"),
        ("__UINT_FAST32_TYPE__", "unsigned int"),
        ("__UINT_FAST64_TYPE__", u64_),
        // Pointer-width types (always 64-bit on our targets)
        ("__SIZE_TYPE__", "long unsigned int"),
        ("__PTRDIFF_TYPE__", "long int"),
        ("__INTPTR_TYPE__", "long int"),
        ("__UINTPTR_TYPE__", "long unsigned int"),
        ("__INTMAX_TYPE__", "long int"),
        ("__UINTMAX_TYPE__", "long unsigned int"),
        // Character types.
        ("__WCHAR_TYPE__", "int"),
        ("__WINT_TYPE__", wint),
        ("__CHAR16_TYPE__", "unsigned short"),
        ("__CHAR32_TYPE__", "unsigned int"),
        // sig_atomic_t
        ("__SIG_ATOMIC_TYPE__", "int"),
    ]
}

/// Get fixed-width integer limit macros (for <stdint.h> compatibility)
pub fn get_stdint_limit_macros(target: &Target) -> Vec<(&'static str, &'static str)> {
    // The bounds follow `wint_t`'s own signedness; see `wint_type`.
    let (wint_min, wint_max) = if wint_type(target) == "unsigned int" {
        ("0U", "0xffffffffU")
    } else {
        ("(-2147483647 - 1)", "2147483647")
    };
    vec![
        // Signed fixed-width limits
        ("__INT8_MAX__", "127"),
        ("__INT16_MAX__", "32767"),
        ("__INT32_MAX__", "2147483647"),
        ("__INT64_MAX__", "9223372036854775807LL"),
        // Unsigned fixed-width limits
        ("__UINT8_MAX__", "255"),
        ("__UINT16_MAX__", "65535"),
        ("__UINT32_MAX__", "4294967295U"),
        ("__UINT64_MAX__", "18446744073709551615ULL"),
        // Least-width limits (same as fixed-width)
        ("__INT_LEAST8_MAX__", "127"),
        ("__INT_LEAST16_MAX__", "32767"),
        ("__INT_LEAST32_MAX__", "2147483647"),
        ("__INT_LEAST64_MAX__", "9223372036854775807LL"),
        ("__UINT_LEAST8_MAX__", "255"),
        ("__UINT_LEAST16_MAX__", "65535"),
        ("__UINT_LEAST32_MAX__", "4294967295U"),
        ("__UINT_LEAST64_MAX__", "18446744073709551615ULL"),
        // Fast limits (same as fixed-width)
        ("__INT_FAST8_MAX__", "127"),
        ("__INT_FAST16_MAX__", "32767"),
        ("__INT_FAST32_MAX__", "2147483647"),
        ("__INT_FAST64_MAX__", "9223372036854775807LL"),
        ("__UINT_FAST8_MAX__", "255"),
        ("__UINT_FAST16_MAX__", "65535"),
        ("__UINT_FAST32_MAX__", "4294967295U"),
        ("__UINT_FAST64_MAX__", "18446744073709551615ULL"),
        // Width macros for fixed-width types
        ("__INT8_WIDTH__", "8"),
        ("__INT16_WIDTH__", "16"),
        ("__INT32_WIDTH__", "32"),
        ("__INT64_WIDTH__", "64"),
        ("__INT_LEAST8_WIDTH__", "8"),
        ("__INT_LEAST16_WIDTH__", "16"),
        ("__INT_LEAST32_WIDTH__", "32"),
        ("__INT_LEAST64_WIDTH__", "64"),
        ("__INT_FAST8_WIDTH__", "8"),
        ("__INT_FAST16_WIDTH__", "16"),
        ("__INT_FAST32_WIDTH__", "32"),
        ("__INT_FAST64_WIDTH__", "64"),
        // intmax_t limits (64-bit on all our targets)
        ("__INTMAX_MAX__", "9223372036854775807L"),
        ("__UINTMAX_MAX__", "18446744073709551615UL"),
        ("__INTMAX_WIDTH__", "64"),
        ("__UINTMAX_WIDTH__", "64"),
        // wchar_t and wint_t limits
        ("__WCHAR_MAX__", "2147483647"),
        ("__WCHAR_WIDTH__", "32"),
        ("__WINT_MAX__", wint_max),
        ("__WINT_MIN__", wint_min),
        ("__WINT_WIDTH__", "32"),
        // sig_atomic_t limits
        ("__SIG_ATOMIC_MAX__", "2147483647"),
        ("__SIG_ATOMIC_WIDTH__", "32"),
    ]
}

/// Get integer constant suffix macros (for <stdint.h> compatibility)
pub fn get_suffix_macros(target: &Target) -> Vec<(&'static str, &'static str)> {
    // Must agree with get_type_macros: an INT64_C() constant has to come out
    // the same type as int64_t.
    let (c64, uc64) = if target.int64_is_long_long() {
        ("LL", "ULL")
    } else {
        ("L", "UL")
    };
    vec![
        // Fixed-width suffixes
        ("__INT8_C_SUFFIX__", ""),
        ("__INT16_C_SUFFIX__", ""),
        ("__INT32_C_SUFFIX__", ""),
        ("__INT64_C_SUFFIX__", c64),
        ("__UINT8_C_SUFFIX__", ""),
        ("__UINT16_C_SUFFIX__", ""),
        ("__UINT32_C_SUFFIX__", "U"),
        ("__UINT64_C_SUFFIX__", uc64),
        // intmax_t suffixes (64-bit uses L suffix on all our targets)
        ("__INTMAX_C_SUFFIX__", "L"),
        ("__UINTMAX_C_SUFFIX__", "UL"),
    ]
}

/// Get format specifier macros (for <inttypes.h> compatibility)
pub fn get_format_macros(_target: &Target) -> Vec<(&'static str, &'static str)> {
    vec![
        // Signed format specifiers
        ("__INT8_FMTd__", "\"hhd\""),
        ("__INT8_FMTi__", "\"hhi\""),
        ("__INT16_FMTd__", "\"hd\""),
        ("__INT16_FMTi__", "\"hi\""),
        ("__INT32_FMTd__", "\"d\""),
        ("__INT32_FMTi__", "\"i\""),
        ("__INT64_FMTd__", "\"lld\""),
        ("__INT64_FMTi__", "\"lli\""),
        // Unsigned format specifiers
        ("__UINT8_FMTo__", "\"hho\""),
        ("__UINT8_FMTu__", "\"hhu\""),
        ("__UINT8_FMTx__", "\"hhx\""),
        ("__UINT8_FMTX__", "\"hhX\""),
        ("__UINT16_FMTo__", "\"ho\""),
        ("__UINT16_FMTu__", "\"hu\""),
        ("__UINT16_FMTx__", "\"hx\""),
        ("__UINT16_FMTX__", "\"hX\""),
        ("__UINT32_FMTo__", "\"o\""),
        ("__UINT32_FMTu__", "\"u\""),
        ("__UINT32_FMTx__", "\"x\""),
        ("__UINT32_FMTX__", "\"X\""),
        ("__UINT64_FMTo__", "\"llo\""),
        ("__UINT64_FMTu__", "\"llu\""),
        ("__UINT64_FMTx__", "\"llx\""),
        ("__UINT64_FMTX__", "\"llX\""),
        // Least-width format specifiers (same as fixed-width)
        ("__INT_LEAST8_FMTd__", "\"hhd\""),
        ("__INT_LEAST8_FMTi__", "\"hhi\""),
        ("__INT_LEAST16_FMTd__", "\"hd\""),
        ("__INT_LEAST16_FMTi__", "\"hi\""),
        ("__INT_LEAST32_FMTd__", "\"d\""),
        ("__INT_LEAST32_FMTi__", "\"i\""),
        ("__INT_LEAST64_FMTd__", "\"lld\""),
        ("__INT_LEAST64_FMTi__", "\"lli\""),
        ("__UINT_LEAST8_FMTo__", "\"hho\""),
        ("__UINT_LEAST8_FMTu__", "\"hhu\""),
        ("__UINT_LEAST8_FMTx__", "\"hhx\""),
        ("__UINT_LEAST8_FMTX__", "\"hhX\""),
        ("__UINT_LEAST16_FMTo__", "\"ho\""),
        ("__UINT_LEAST16_FMTu__", "\"hu\""),
        ("__UINT_LEAST16_FMTx__", "\"hx\""),
        ("__UINT_LEAST16_FMTX__", "\"hX\""),
        ("__UINT_LEAST32_FMTo__", "\"o\""),
        ("__UINT_LEAST32_FMTu__", "\"u\""),
        ("__UINT_LEAST32_FMTx__", "\"x\""),
        ("__UINT_LEAST32_FMTX__", "\"X\""),
        ("__UINT_LEAST64_FMTo__", "\"llo\""),
        ("__UINT_LEAST64_FMTu__", "\"llu\""),
        ("__UINT_LEAST64_FMTx__", "\"llx\""),
        ("__UINT_LEAST64_FMTX__", "\"llX\""),
        // Fast format specifiers (same as fixed-width)
        ("__INT_FAST8_FMTd__", "\"hhd\""),
        ("__INT_FAST8_FMTi__", "\"hhi\""),
        ("__INT_FAST16_FMTd__", "\"hd\""),
        ("__INT_FAST16_FMTi__", "\"hi\""),
        ("__INT_FAST32_FMTd__", "\"d\""),
        ("__INT_FAST32_FMTi__", "\"i\""),
        ("__INT_FAST64_FMTd__", "\"lld\""),
        ("__INT_FAST64_FMTi__", "\"lli\""),
        ("__UINT_FAST8_FMTo__", "\"hho\""),
        ("__UINT_FAST8_FMTu__", "\"hhu\""),
        ("__UINT_FAST8_FMTx__", "\"hhx\""),
        ("__UINT_FAST8_FMTX__", "\"hhX\""),
        ("__UINT_FAST16_FMTo__", "\"ho\""),
        ("__UINT_FAST16_FMTu__", "\"hu\""),
        ("__UINT_FAST16_FMTx__", "\"hx\""),
        ("__UINT_FAST16_FMTX__", "\"hX\""),
        ("__UINT_FAST32_FMTo__", "\"o\""),
        ("__UINT_FAST32_FMTu__", "\"u\""),
        ("__UINT_FAST32_FMTx__", "\"x\""),
        ("__UINT_FAST32_FMTX__", "\"X\""),
        ("__UINT_FAST64_FMTo__", "\"llo\""),
        ("__UINT_FAST64_FMTu__", "\"llu\""),
        ("__UINT_FAST64_FMTx__", "\"llx\""),
        ("__UINT_FAST64_FMTX__", "\"llX\""),
        // intmax_t format specifiers (64-bit uses l format on all our targets)
        ("__INTMAX_FMTd__", "\"ld\""),
        ("__INTMAX_FMTi__", "\"li\""),
        ("__UINTMAX_FMTo__", "\"lo\""),
        ("__UINTMAX_FMTu__", "\"lu\""),
        ("__UINTMAX_FMTx__", "\"lx\""),
        ("__UINTMAX_FMTX__", "\"lX\""),
        // intptr_t format specifiers (64-bit uses l format)
        ("__INTPTR_FMTd__", "\"ld\""),
        ("__INTPTR_FMTi__", "\"li\""),
        ("__UINTPTR_FMTo__", "\"lo\""),
        ("__UINTPTR_FMTu__", "\"lu\""),
        ("__UINTPTR_FMTx__", "\"lx\""),
        ("__UINTPTR_FMTX__", "\"lX\""),
        // ptrdiff_t format specifiers
        ("__PTRDIFF_FMTd__", "\"ld\""),
        ("__PTRDIFF_FMTi__", "\"li\""),
        // size_t format specifiers
        ("__SIZE_FMTo__", "\"lo\""),
        ("__SIZE_FMTu__", "\"lu\""),
        ("__SIZE_FMTx__", "\"lx\""),
        ("__SIZE_FMTX__", "\"lX\""),
    ]
}

pub fn get_additional_sizeof_macros(target: &Target) -> Vec<(&'static str, &'static str)> {
    // 16 bytes for the x87 80-bit format and for IEEE binary128 alike; Apple's
    // aarch64 `long double` is a `double` and occupies 8.
    let sizeof_long_double = if target.arch == Arch::Aarch64 && target.os == Os::MacOS {
        "8"
    } else {
        "16"
    };
    vec![
        // size_t and ptrdiff_t sizes (always 8 bytes on 64-bit)
        ("__SIZEOF_SIZE_T__", "8"),
        ("__SIZEOF_PTRDIFF_T__", "8"),
        // wchar_t and wint_t sizes
        ("__SIZEOF_WCHAR_T__", "4"),
        ("__SIZEOF_WINT_T__", "4"),
        ("__SIZEOF_LONG_DOUBLE__", sizeof_long_double),
    ]
}

/// Get miscellaneous macros
pub fn get_misc_macros(_target: &Target) -> Vec<(&'static str, &'static str)> {
    vec![
        // Pointer width in bits (always 64 on our targets)
        ("__POINTER_WIDTH__", "64"),
        // Alignment
        ("__BIGGEST_ALIGNMENT__", "16"),
        ("__BOOL_WIDTH__", "8"),
        // Byte order (all our supported architectures are little-endian)
        ("__ORDER_LITTLE_ENDIAN__", "1234"),
        ("__ORDER_BIG_ENDIAN__", "4321"),
        ("__ORDER_PDP_ENDIAN__", "3412"),
        ("__BYTE_ORDER__", "__ORDER_LITTLE_ENDIAN__"),
        ("__LITTLE_ENDIAN__", "1"),
        // Floating point base
        ("__FLT_RADIX__", "2"),
        ("__FINITE_MATH_ONLY__", "0"),
    ]
}

/// Get floating-point limit macros (IEEE 754)
pub fn get_float_limit_macros(target: &Target) -> Vec<(&'static str, &'static str)> {
    // `long double` is three different types across our targets, and these
    // macros are the only description of it a program gets. Hardcoding the
    // x86_64 values everywhere told macOS/aarch64 code that LDBL_EPSILON was
    // 1.08e-19 when the type is really a 64-bit double, so `1.0L + LDBL_EPSILON`
    // compared equal to 1.0L and <float.h> was simply lying.
    let ldbl = LongDoubleLimits::for_target(target);
    let mut macros: Vec<(&'static str, &'static str)> = vec![
        // Float16 (16-bit IEEE 754 binary16, half precision)
        (
            "__FLT16_MIN__",
            "6.10351562500000000000000000000000000e-5F16",
        ),
        (
            "__FLT16_MAX__",
            "6.55040000000000000000000000000000000e+4F16",
        ),
        (
            "__FLT16_EPSILON__",
            "9.76562500000000000000000000000000000e-4F16",
        ),
        (
            "__FLT16_DENORM_MIN__",
            "5.96046447753906250000000000000000000e-8F16",
        ),
        ("__FLT16_MANT_DIG__", "11"),
        ("__FLT16_DIG__", "3"),
        ("__FLT16_MIN_EXP__", "(-13)"),
        ("__FLT16_MAX_EXP__", "16"),
        ("__FLT16_MIN_10_EXP__", "(-4)"),
        ("__FLT16_MAX_10_EXP__", "4"),
        ("__FLT16_HAS_DENORM__", "1"),
        ("__FLT16_HAS_INFINITY__", "1"),
        ("__FLT16_HAS_QUIET_NAN__", "1"),
        ("__FLT16_DECIMAL_DIG__", "5"),
        ("__SIZEOF_FLOAT16__", "2"),
        // Float (32-bit IEEE 754)
        ("__FLT_MIN__", "1.17549435082228750796873653722224568e-38F"),
        ("__FLT_MAX__", "3.40282346638528859811704183484516925e+38F"),
        (
            "__FLT_EPSILON__",
            "1.19209289550781250000000000000000000e-7F",
        ),
        (
            "__FLT_DENORM_MIN__",
            "1.40129846432481707092372958328991613e-45F",
        ),
        ("__FLT_MANT_DIG__", "24"),
        ("__FLT_DIG__", "6"),
        ("__FLT_MIN_EXP__", "(-125)"),
        ("__FLT_MAX_EXP__", "128"),
        ("__FLT_MIN_10_EXP__", "(-37)"),
        ("__FLT_MAX_10_EXP__", "38"),
        ("__FLT_HAS_DENORM__", "1"),
        ("__FLT_HAS_INFINITY__", "1"),
        ("__FLT_HAS_QUIET_NAN__", "1"),
        // Double (64-bit IEEE 754)
        ("__DBL_MIN__", "2.22507385850720138309023271733240406e-308"),
        ("__DBL_MAX__", "1.79769313486231570814527423731704357e+308"),
        (
            "__DBL_EPSILON__",
            "2.22044604925031308084726333618164062e-16",
        ),
        (
            "__DBL_DENORM_MIN__",
            "4.94065645841246544176568792868221372e-324",
        ),
        ("__DBL_MANT_DIG__", "53"),
        ("__DBL_DIG__", "15"),
        ("__DBL_MIN_EXP__", "(-1021)"),
        ("__DBL_MAX_EXP__", "1024"),
        ("__DBL_MIN_10_EXP__", "(-307)"),
        ("__DBL_MAX_10_EXP__", "308"),
        ("__DBL_HAS_DENORM__", "1"),
        ("__DBL_HAS_INFINITY__", "1"),
        ("__DBL_HAS_QUIET_NAN__", "1"),
        // Long double — see `LongDoubleLimits`.
        ("__LDBL_MIN__", ldbl.min),
        ("__LDBL_MAX__", ldbl.max),
        ("__LDBL_EPSILON__", ldbl.epsilon),
        ("__LDBL_DENORM_MIN__", ldbl.denorm_min),
        ("__LDBL_MANT_DIG__", ldbl.mant_dig),
        ("__LDBL_DIG__", ldbl.dig),
        ("__LDBL_MIN_EXP__", ldbl.min_exp),
        ("__LDBL_MAX_EXP__", ldbl.max_exp),
        ("__LDBL_MIN_10_EXP__", ldbl.min_10_exp),
        ("__LDBL_MAX_10_EXP__", ldbl.max_10_exp),
        ("__LDBL_HAS_DENORM__", "1"),
        ("__LDBL_HAS_INFINITY__", "1"),
        ("__LDBL_HAS_QUIET_NAN__", "1"),
        // Decimal digits for exact conversion
        ("__FLT_DECIMAL_DIG__", "9"),
        ("__DBL_DECIMAL_DIG__", "17"),
        ("__LDBL_DECIMAL_DIG__", ldbl.decimal_dig),
        ("__DECIMAL_DIG__", ldbl.decimal_dig),
    ];

    // __float128 / _Float128 (IEEE 754 binary128, quad precision).
    //
    // Spelled in hex so the values reach binary128 exactly, and suffixed `q`
    // so they carry that type rather than being rounded to whatever
    // `long double` happens to be. Same on every target that has the type --
    // unlike `long double`, binary128 does not vary -- but only on the targets
    // that have it at all: describing a type the runtime cannot support would
    // have <float.h> advertise an FLT128_* family that fails at link time.
    if has_float128(target) {
        macros.extend([
            ("__FLT128_MIN__", "0x1p-16382q"),
            ("__FLT128_MAX__", "0x1.ffffffffffffffffffffffffffffp+16383q"),
            ("__FLT128_EPSILON__", "0x1p-112q"),
            ("__FLT128_DENORM_MIN__", "0x1p-16494q"),
            ("__FLT128_MANT_DIG__", "113"),
            ("__FLT128_DIG__", "33"),
            ("__FLT128_MIN_EXP__", "(-16381)"),
            ("__FLT128_MAX_EXP__", "16384"),
            ("__FLT128_MIN_10_EXP__", "(-4931)"),
            ("__FLT128_MAX_10_EXP__", "4932"),
            ("__FLT128_HAS_DENORM__", "1"),
            ("__FLT128_HAS_INFINITY__", "1"),
            ("__FLT128_HAS_QUIET_NAN__", "1"),
            ("__FLT128_DECIMAL_DIG__", "36"),
            ("__SIZEOF_FLOAT128__", "16"),
        ]);
    }

    macros
}

/// The type behind `wint_t`, which is not the same on every target.
///
/// `wint_t` has to hold every `wchar_t` value *plus* `WEOF`, and the two
/// platforms solve that differently. glibc makes it `unsigned int`, so `WEOF`
/// -- `(wint_t)-1` -- is `0xffffffff`, a value no `wchar_t` reaches. Darwin
/// makes it `int`, following `__darwin_ct_rune_t`, and spends the negative
/// half of the range instead.
///
/// It has to be the platform's choice rather than ours: the C library's own
/// headers typedef `wint_t` from their own definition, and `__mbstate_t` holds
/// one. Predefining the other signedness puts the compiler and the library in
/// disagreement about a type they share -- which is how this was wrong in the
/// first place, as plain `int` on both.
fn wint_type(target: &Target) -> &'static str {
    match target.os {
        Os::MacOS => "int",
        Os::Linux | Os::FreeBSD => "unsigned int",
    }
}

/// Whether `__float128` exists on this target; see `TypeTable::has_float128`,
/// which must agree with this.
pub fn has_float128(target: &Target) -> bool {
    target.os != Os::MacOS
}

/// The <float.h> description of `long double`, which is a different type on
/// each of our targets:
///
/// - **x86_64**: the x87 80-bit extended format, padded to 16 bytes.
/// - **aarch64 Linux/FreeBSD**: IEEE 754 binary128 (true quad precision).
/// - **aarch64 macOS**: Apple makes `long double` an alias for `double`.
struct LongDoubleLimits {
    min: &'static str,
    max: &'static str,
    epsilon: &'static str,
    denorm_min: &'static str,
    mant_dig: &'static str,
    dig: &'static str,
    min_exp: &'static str,
    max_exp: &'static str,
    min_10_exp: &'static str,
    max_10_exp: &'static str,
    decimal_dig: &'static str,
}

impl LongDoubleLimits {
    // The float-valued limits are spelled as hex literals, which reach the
    // target format exactly. As decimal they were rounded through `f64` when
    // parsed back, so `__LDBL_MAX__` expanded to a literal that had already
    // become infinity and `__LDBL_MIN__` to one that had become zero.

    fn for_target(target: &Target) -> Self {
        match (target.arch, target.os) {
            // Apple aarch64: long double *is* double.
            (Arch::Aarch64, Os::MacOS) => Self {
                min: "0x1p-1022L",
                max: "0x1.fffffffffffffp+1023L",
                epsilon: "0x1p-52L",
                denorm_min: "0x1p-1074L",
                mant_dig: "53",
                dig: "15",
                min_exp: "(-1021)",
                max_exp: "1024",
                min_10_exp: "(-307)",
                max_10_exp: "308",
                decimal_dig: "17",
            },
            // aarch64 elsewhere: IEEE binary128.
            (Arch::Aarch64, _) => Self {
                min: "0x1p-16382L",
                max: "0x1.ffffffffffffffffffffffffffffp+16383L",
                epsilon: "0x1p-112L",
                denorm_min: "0x1p-16494L",
                mant_dig: "113",
                dig: "33",
                min_exp: "(-16381)",
                max_exp: "16384",
                min_10_exp: "(-4931)",
                max_10_exp: "4932",
                decimal_dig: "36",
            },
            // x86_64: x87 80-bit extended.
            _ => Self {
                min: "0x1p-16382L",
                max: "0x1.fffffffffffffffep+16383L",
                epsilon: "0x1p-63L",
                denorm_min: "0x1p-16445L",
                mant_dig: "64",
                dig: "18",
                min_exp: "(-16381)",
                max_exp: "16384",
                min_10_exp: "(-4931)",
                max_10_exp: "4932",
                decimal_dig: "21",
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::target::Os;

    fn macro_value(macros: &[(&'static str, &'static str)], name: &str) -> String {
        macros
            .iter()
            .find(|(n, _)| *n == name)
            .unwrap_or_else(|| panic!("{} is not defined", name))
            .1
            .to_string()
    }

    /// `long double` is a different type on each target, and these macros are
    /// the only description of it a program gets. They were hardcoded to the
    /// x86_64 values everywhere, so `<float.h>` told macOS/aarch64 code that
    /// LDBL_EPSILON was 1.08e-19 for a type that is really a 64-bit double.
    #[test]
    fn long_double_limits_describe_the_target() {
        for (arch, os, mant_dig, sizeof) in [
            (Arch::X86_64, Os::Linux, "64", "16"),   // x87 80-bit extended
            (Arch::X86_64, Os::MacOS, "64", "16"),   // likewise
            (Arch::Aarch64, Os::Linux, "113", "16"), // IEEE binary128
            (Arch::Aarch64, Os::MacOS, "53", "8"),   // Apple: long double == double
        ] {
            let target = Target::new(arch, os);
            let floats = get_float_limit_macros(&target);
            assert_eq!(
                macro_value(&floats, "__LDBL_MANT_DIG__"),
                mant_dig,
                "__LDBL_MANT_DIG__ on {:?}/{:?}",
                arch,
                os
            );
            assert_eq!(
                macro_value(
                    &get_additional_sizeof_macros(&target),
                    "__SIZEOF_LONG_DOUBLE__"
                ),
                sizeof,
                "__SIZEOF_LONG_DOUBLE__ on {:?}/{:?}",
                arch,
                os
            );
            // The epsilon has to belong to the same format as the mantissa.
            // Spelled in hex it is exactly 2^(1 - MANT_DIG), so this pins the
            // value rather than approximating it by a decimal exponent.
            let eps = macro_value(&floats, "__LDBL_EPSILON__");
            let expected = format!("0x1p-{}L", mant_dig.parse::<i32>().unwrap() - 1);
            assert_eq!(
                eps, expected,
                "__LDBL_EPSILON__ does not match a {}-bit mantissa on {:?}/{:?}",
                mant_dig, arch, os
            );
        }
    }

    /// Our `<stdint.h>` has to name the same type the host's headers do.
    /// Every target is LP64, so the width does not settle it: Linux says
    /// `long`, Darwin says `long long`, and they are distinct types.
    #[test]
    fn int64_spelling_follows_the_platform() {
        for (os, ty, suffix) in [
            (Os::Linux, "long int", "L"),
            (Os::FreeBSD, "long int", "L"),
            (Os::MacOS, "long long int", "LL"),
        ] {
            let target = Target::new(Arch::X86_64, os);
            assert_eq!(
                macro_value(&get_type_macros(&target), "__INT64_TYPE__"),
                ty,
                "__INT64_TYPE__ on {:?}",
                os
            );
            // INT64_C() must produce that same type.
            assert_eq!(
                macro_value(&get_suffix_macros(&target), "__INT64_C_SUFFIX__"),
                suffix,
                "__INT64_C_SUFFIX__ on {:?}",
                os
            );
        }
    }
}
