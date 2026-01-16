//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Runtime library function names for soft-float and complex operations.
//
// This module maps C type operations to their corresponding runtime library
// function names. Both libgcc and compiler-rt provide the same function names
// for these operations, so the rtlib choice only affects which library gets
// linked, not the generated code.
//
// Naming convention for floating-point suffixes:
// - sf = single float (32-bit)
// - df = double float (64-bit)
// - xf = x87 80-bit extended format (x86-64)
// - tf = 128-bit IEEE quad format (AArch64/Linux)
//
// For complex operations:
// - sc3 = single complex (float _Complex)
// - dc3 = double complex (double _Complex)
// - xc3 = x87 extended complex (long double _Complex on x86-64)
// - tc3 = quad complex (long double _Complex on AArch64/Linux)
//

use crate::target::{Arch, Os, Target};
use crate::types::TypeKind;

/// Runtime library function name provider.
///
/// Provides function names for soft-float and complex arithmetic operations
/// based on the target architecture and operating system.
pub struct RtlibNames<'a> {
    target: &'a Target,
}

impl<'a> RtlibNames<'a> {
    /// Create a new RtlibNames for the given target
    pub fn new(target: &'a Target) -> Self {
        Self { target }
    }

    /// Returns true if long double is the same as double on this platform.
    /// On macOS aarch64 (Apple Silicon), long double is 64-bit (same as double).
    pub fn longdouble_is_double(&self) -> bool {
        self.target.arch == Arch::Aarch64 && self.target.os == Os::MacOS
    }

    // ========================================================================
    // Float16 (_Float16) operations
    // ========================================================================

    /// Get function name for Float16 conversion.
    ///
    /// Suffix convention:
    /// - hf = half float (16-bit)
    /// - sf = single float (32-bit)
    /// - df = double float (64-bit)
    pub fn float16_convert(&self, from: &str, to: &str) -> Option<&'static str> {
        match (from, to) {
            // half <-> float
            ("hf", "sf") => Some("__extendhfsf2"),
            ("sf", "hf") => Some("__truncsfhf2"),

            // half <-> double
            ("hf", "df") => Some("__extendhfdf2"),
            ("df", "hf") => Some("__truncdfhf2"),

            // half <-> signed int32
            ("hf", "si") => Some("__fixhfsi"),
            ("si", "hf") => Some("__floatsihf"),

            // half <-> signed int64
            ("hf", "di") => Some("__fixhfdi"),
            ("di", "hf") => Some("__floatdihf"),

            // half <-> unsigned int32
            ("hf", "usi") => Some("__fixunshfsi"),
            ("usi", "hf") => Some("__floatunsihf"),

            // half <-> unsigned int64
            ("hf", "udi") => Some("__fixunshfdi"),
            ("udi", "hf") => Some("__floatundihf"),

            _ => None,
        }
    }

    // ========================================================================
    // Complex operations
    // ========================================================================

    /// Get function name for complex multiplication
    ///
    /// Complex multiply: result = __mulXc3(a_real, a_imag, b_real, b_imag)
    pub fn complex_mul(&self, base_kind: TypeKind) -> &'static str {
        match base_kind {
            TypeKind::Float => "__mulsc3",
            TypeKind::Double => "__muldc3",
            TypeKind::LongDouble => {
                if self.longdouble_is_double() {
                    "__muldc3" // macOS aarch64: long double == double
                } else {
                    match self.target.arch {
                        Arch::X86_64 => "__mulxc3",  // x87 80-bit
                        Arch::Aarch64 => "__multc3", // IEEE quad 128-bit
                    }
                }
            }
            _ => "__muldc3", // fallback
        }
    }

    /// Get function name for complex division
    ///
    /// Complex divide: result = __divXc3(a_real, a_imag, b_real, b_imag)
    /// Uses Smith's method for robust overflow handling.
    pub fn complex_div(&self, base_kind: TypeKind) -> &'static str {
        match base_kind {
            TypeKind::Float => "__divsc3",
            TypeKind::Double => "__divdc3",
            TypeKind::LongDouble => {
                if self.longdouble_is_double() {
                    "__divdc3"
                } else {
                    match self.target.arch {
                        Arch::X86_64 => "__divxc3",
                        Arch::Aarch64 => "__divtc3",
                    }
                }
            }
            _ => "__divdc3", // fallback
        }
    }

    // ========================================================================
    // Long double operations
    // ========================================================================

    /// Get function name for long double binary operation.
    /// Returns None if native FP instructions should be used.
    ///
    /// Note: x86-64 uses native x87 FPU instructions for long double,
    /// so rtlib is NOT used. Only AArch64/Linux needs rtlib for 128-bit IEEE quad.
    pub fn longdouble_binop(&self, op: &str) -> Option<&'static str> {
        if self.longdouble_is_double() {
            return None; // macOS aarch64: long double == double
        }
        // x86-64 uses native x87 FPU - no soft-float rtlib available
        if self.target.arch == Arch::X86_64 {
            return None;
        }
        // AArch64/Linux: use tf (128-bit IEEE quad) functions
        match op {
            "add" => Some("__addtf3"),
            "sub" => Some("__subtf3"),
            "mul" => Some("__multf3"),
            "div" => Some("__divtf3"),
            _ => None,
        }
    }

    /// Get function name for long double negation.
    /// Returns None if native FP instructions should be used.
    pub fn longdouble_neg(&self) -> Option<&'static str> {
        if self.longdouble_is_double() {
            return None; // macOS aarch64: long double == double
        }
        // x86-64 uses native x87 FPU
        if self.target.arch == Arch::X86_64 {
            return None;
        }
        // AArch64/Linux: use tf function
        Some("__negtf2")
    }

    /// Get function name for long double comparison.
    /// Returns None if native FP instructions should be used.
    ///
    /// The comparison functions return:
    /// - __lttf2/__letf2: < 0 if a < b (or a <= b), >= 0 otherwise
    /// - __gttf2/__getf2: > 0 if a > b (or a >= b), <= 0 otherwise
    /// - __eqtf2: 0 if a == b, non-zero otherwise
    /// - __netf2: 0 if a == b, non-zero otherwise (same as __eqtf2)
    pub fn longdouble_cmp(&self, cmp_kind: &str) -> Option<&'static str> {
        if self.longdouble_is_double() {
            return None; // macOS aarch64: long double == double
        }
        // x86-64 uses native x87 FPU
        if self.target.arch == Arch::X86_64 {
            return None;
        }
        // AArch64/Linux: use tf comparison functions
        match cmp_kind {
            "lt" => Some("__lttf2"),
            "le" => Some("__letf2"),
            "gt" => Some("__gttf2"),
            "ge" => Some("__getf2"),
            "eq" => Some("__eqtf2"),
            "ne" => Some("__netf2"),
            _ => None,
        }
    }

    // ========================================================================
    // Long double conversions
    // ========================================================================

    /// Get function name for long double conversion.
    /// Returns None if native FP instructions should be used.
    ///
    /// Note: x86-64 uses native x87 FPU instructions for long double,
    /// so rtlib is NOT used. Only AArch64/Linux needs rtlib for 128-bit IEEE quad.
    ///
    /// Suffix convention:
    /// - sf = single float (32-bit)
    /// - df = double float (64-bit)
    /// - tf = 128-bit IEEE quad (AArch64/Linux long double)
    /// - si = signed 32-bit integer
    /// - di = signed 64-bit integer
    /// - usi = unsigned 32-bit integer
    /// - udi = unsigned 64-bit integer
    pub fn longdouble_convert(&self, from: &str, to: &str) -> Option<&'static str> {
        if self.longdouble_is_double() {
            return None; // macOS aarch64: long double == double
        }
        // x86-64 uses native x87 FPU - no soft-float rtlib available
        if self.target.arch == Arch::X86_64 {
            return None;
        }
        // AArch64/Linux: use tf conversion functions
        match (from, to) {
            // float <-> long double
            ("sf", "tf") => Some("__extendsftf2"),
            ("tf", "sf") => Some("__trunctfsf2"),

            // double <-> long double
            ("df", "tf") => Some("__extenddftf2"),
            ("tf", "df") => Some("__trunctfdf2"),

            // signed int32 <-> long double
            ("si", "tf") => Some("__floatsitf"),
            ("tf", "si") => Some("__fixtfsi"),

            // signed int64 <-> long double
            ("di", "tf") => Some("__floatditf"),
            ("tf", "di") => Some("__fixtfdi"),

            // unsigned int32 <-> long double
            ("usi", "tf") => Some("__floatunsitf"),
            ("tf", "usi") => Some("__fixunstfsi"),

            // unsigned int64 <-> long double
            ("udi", "tf") => Some("__floatunditf"),
            ("tf", "udi") => Some("__fixunstfdi"),

            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_complex_mul_float() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let rtlib = RtlibNames::new(&target);
        assert_eq!(rtlib.complex_mul(TypeKind::Float), "__mulsc3");
    }

    #[test]
    fn test_complex_mul_double() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let rtlib = RtlibNames::new(&target);
        assert_eq!(rtlib.complex_mul(TypeKind::Double), "__muldc3");
    }

    #[test]
    fn test_complex_mul_longdouble_x86_64() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let rtlib = RtlibNames::new(&target);
        assert_eq!(rtlib.complex_mul(TypeKind::LongDouble), "__mulxc3");
    }

    #[test]
    fn test_complex_mul_longdouble_aarch64_linux() {
        let target = Target::new(Arch::Aarch64, Os::Linux);
        let rtlib = RtlibNames::new(&target);
        assert_eq!(rtlib.complex_mul(TypeKind::LongDouble), "__multc3");
    }

    #[test]
    fn test_complex_mul_longdouble_aarch64_macos() {
        // On macOS aarch64, long double == double
        let target = Target::new(Arch::Aarch64, Os::MacOS);
        let rtlib = RtlibNames::new(&target);
        assert_eq!(rtlib.complex_mul(TypeKind::LongDouble), "__muldc3");
    }

    #[test]
    fn test_longdouble_is_double() {
        let macos_arm = Target::new(Arch::Aarch64, Os::MacOS);
        let linux_arm = Target::new(Arch::Aarch64, Os::Linux);
        let linux_x86 = Target::new(Arch::X86_64, Os::Linux);

        assert!(RtlibNames::new(&macos_arm).longdouble_is_double());
        assert!(!RtlibNames::new(&linux_arm).longdouble_is_double());
        assert!(!RtlibNames::new(&linux_x86).longdouble_is_double());
    }

    #[test]
    fn test_longdouble_binop_x86_64() {
        // x86-64 uses native x87 FPU - no soft-float rtlib available
        let target = Target::new(Arch::X86_64, Os::Linux);
        let rtlib = RtlibNames::new(&target);
        assert_eq!(rtlib.longdouble_binop("add"), None);
        assert_eq!(rtlib.longdouble_binop("sub"), None);
        assert_eq!(rtlib.longdouble_binop("mul"), None);
        assert_eq!(rtlib.longdouble_binop("div"), None);
    }

    #[test]
    fn test_longdouble_binop_aarch64_linux() {
        let target = Target::new(Arch::Aarch64, Os::Linux);
        let rtlib = RtlibNames::new(&target);
        assert_eq!(rtlib.longdouble_binop("add"), Some("__addtf3"));
        assert_eq!(rtlib.longdouble_binop("sub"), Some("__subtf3"));
        assert_eq!(rtlib.longdouble_binop("mul"), Some("__multf3"));
        assert_eq!(rtlib.longdouble_binop("div"), Some("__divtf3"));
    }

    #[test]
    fn test_longdouble_binop_aarch64_macos() {
        // On macOS aarch64, long double ops use native double instructions
        let target = Target::new(Arch::Aarch64, Os::MacOS);
        let rtlib = RtlibNames::new(&target);
        assert_eq!(rtlib.longdouble_binop("add"), None);
        assert_eq!(rtlib.longdouble_binop("mul"), None);
    }

    #[test]
    fn test_longdouble_cmp() {
        let x86 = Target::new(Arch::X86_64, Os::Linux);
        let arm_linux = Target::new(Arch::Aarch64, Os::Linux);
        let arm_macos = Target::new(Arch::Aarch64, Os::MacOS);

        // x86-64 uses native x87 FPU - no soft-float rtlib
        assert_eq!(RtlibNames::new(&x86).longdouble_cmp("lt"), None);
        assert_eq!(RtlibNames::new(&x86).longdouble_cmp("eq"), None);

        // AArch64/Linux uses tf comparison functions
        assert_eq!(
            RtlibNames::new(&arm_linux).longdouble_cmp("lt"),
            Some("__lttf2")
        );
        assert_eq!(
            RtlibNames::new(&arm_linux).longdouble_cmp("le"),
            Some("__letf2")
        );
        assert_eq!(
            RtlibNames::new(&arm_linux).longdouble_cmp("gt"),
            Some("__gttf2")
        );
        assert_eq!(
            RtlibNames::new(&arm_linux).longdouble_cmp("ge"),
            Some("__getf2")
        );
        assert_eq!(
            RtlibNames::new(&arm_linux).longdouble_cmp("eq"),
            Some("__eqtf2")
        );
        assert_eq!(
            RtlibNames::new(&arm_linux).longdouble_cmp("ne"),
            Some("__netf2")
        );

        // macOS aarch64: long double == double, no rtlib needed
        assert_eq!(RtlibNames::new(&arm_macos).longdouble_cmp("lt"), None);
    }

    #[test]
    fn test_longdouble_neg() {
        let x86 = Target::new(Arch::X86_64, Os::Linux);
        let arm_linux = Target::new(Arch::Aarch64, Os::Linux);
        let arm_macos = Target::new(Arch::Aarch64, Os::MacOS);

        // x86-64 uses native x87 FPU - no soft-float rtlib
        assert_eq!(RtlibNames::new(&x86).longdouble_neg(), None);

        // AArch64/Linux uses tf negation function
        assert_eq!(
            RtlibNames::new(&arm_linux).longdouble_neg(),
            Some("__negtf2")
        );

        // macOS aarch64: long double == double, no rtlib needed
        assert_eq!(RtlibNames::new(&arm_macos).longdouble_neg(), None);
    }

    #[test]
    fn test_longdouble_convert_x86_64() {
        // x86-64 uses native x87 FPU - no soft-float rtlib available
        let target = Target::new(Arch::X86_64, Os::Linux);
        let rtlib = RtlibNames::new(&target);

        // All conversions return None - use native x87
        assert_eq!(rtlib.longdouble_convert("sf", "xf"), None);
        assert_eq!(rtlib.longdouble_convert("xf", "sf"), None);
        assert_eq!(rtlib.longdouble_convert("df", "xf"), None);
        assert_eq!(rtlib.longdouble_convert("xf", "df"), None);
        assert_eq!(rtlib.longdouble_convert("si", "xf"), None);
        assert_eq!(rtlib.longdouble_convert("xf", "si"), None);
    }

    #[test]
    fn test_longdouble_convert_aarch64_linux() {
        let target = Target::new(Arch::Aarch64, Os::Linux);
        let rtlib = RtlibNames::new(&target);

        // float <-> long double
        assert_eq!(rtlib.longdouble_convert("sf", "tf"), Some("__extendsftf2"));
        assert_eq!(rtlib.longdouble_convert("tf", "sf"), Some("__trunctfsf2"));

        // double <-> long double
        assert_eq!(rtlib.longdouble_convert("df", "tf"), Some("__extenddftf2"));
        assert_eq!(rtlib.longdouble_convert("tf", "df"), Some("__trunctfdf2"));

        // signed int32 <-> long double
        assert_eq!(rtlib.longdouble_convert("si", "tf"), Some("__floatsitf"));
        assert_eq!(rtlib.longdouble_convert("tf", "si"), Some("__fixtfsi"));

        // signed int64 <-> long double
        assert_eq!(rtlib.longdouble_convert("di", "tf"), Some("__floatditf"));
        assert_eq!(rtlib.longdouble_convert("tf", "di"), Some("__fixtfdi"));

        // unsigned int32 <-> long double
        assert_eq!(rtlib.longdouble_convert("usi", "tf"), Some("__floatunsitf"));
        assert_eq!(rtlib.longdouble_convert("tf", "usi"), Some("__fixunstfsi"));

        // unsigned int64 <-> long double
        assert_eq!(rtlib.longdouble_convert("udi", "tf"), Some("__floatunditf"));
        assert_eq!(rtlib.longdouble_convert("tf", "udi"), Some("__fixunstfdi"));
    }

    #[test]
    fn test_longdouble_convert_aarch64_macos() {
        // macOS aarch64: long double == double, no rtlib needed
        let target = Target::new(Arch::Aarch64, Os::MacOS);
        let rtlib = RtlibNames::new(&target);

        assert_eq!(rtlib.longdouble_convert("sf", "tf"), None);
        assert_eq!(rtlib.longdouble_convert("df", "tf"), None);
        assert_eq!(rtlib.longdouble_convert("si", "tf"), None);
    }

    // ========================================================================
    // Float16 (_Float16) rtlib tests
    // ========================================================================

    #[test]
    fn test_float16_convert() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let rtlib = RtlibNames::new(&target);

        // half <-> float
        assert_eq!(rtlib.float16_convert("hf", "sf"), Some("__extendhfsf2"));
        assert_eq!(rtlib.float16_convert("sf", "hf"), Some("__truncsfhf2"));

        // half <-> double
        assert_eq!(rtlib.float16_convert("hf", "df"), Some("__extendhfdf2"));
        assert_eq!(rtlib.float16_convert("df", "hf"), Some("__truncdfhf2"));

        // half <-> signed int32
        assert_eq!(rtlib.float16_convert("hf", "si"), Some("__fixhfsi"));
        assert_eq!(rtlib.float16_convert("si", "hf"), Some("__floatsihf"));

        // half <-> signed int64
        assert_eq!(rtlib.float16_convert("hf", "di"), Some("__fixhfdi"));
        assert_eq!(rtlib.float16_convert("di", "hf"), Some("__floatdihf"));

        // half <-> unsigned int32
        assert_eq!(rtlib.float16_convert("hf", "usi"), Some("__fixunshfsi"));
        assert_eq!(rtlib.float16_convert("usi", "hf"), Some("__floatunsihf"));

        // half <-> unsigned int64
        assert_eq!(rtlib.float16_convert("hf", "udi"), Some("__fixunshfdi"));
        assert_eq!(rtlib.float16_convert("udi", "hf"), Some("__floatundihf"));

        // Invalid conversion
        assert_eq!(rtlib.float16_convert("hf", "xf"), None);
    }
}
