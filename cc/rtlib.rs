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

/// ABI used for Float16 parameters/returns in rtlib functions.
///
/// Different runtime libraries use different ABIs for passing Float16 values:
/// - compiler-rt (used by clang): Float16 passed as 16-bit integer in GP registers
/// - libgcc (used by GCC): Float16 passed in XMM registers (SSE ABI)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Float16Abi {
    /// Float16 passed/returned as 16-bit integer in GP registers (RDI/RAX on x86-64)
    /// Used by compiler-rt (LLVM/clang runtime library)
    Integer,
    /// Float16 passed/returned in XMM registers using SSE ABI
    /// Used by libgcc (GCC runtime library)
    Sse,
}

/// Runtime library function name provider.
///
/// Provides function names and ABI information for soft-float and complex
/// arithmetic operations. The rtlib determines calling conventions for
/// operations that require runtime library support.
///
/// Currently, the rtlib is inferred from the target:
/// - macOS: compiler-rt (LLVM runtime)
/// - Linux: libgcc (GCC runtime)
///
/// Future: explicit rtlib selection could be added via command-line flag.
pub struct RtlibNames<'a> {
    target: &'a Target,
}

impl<'a> RtlibNames<'a> {
    /// Create a new RtlibNames for the given target
    pub fn new(target: &'a Target) -> Self {
        Self { target }
    }

    /// Returns the ABI used by this rtlib for Float16 parameters/returns.
    ///
    /// This is an rtlib attribute - different runtime libraries have different
    /// calling conventions for Float16:
    /// - compiler-rt: INTEGER ABI (Float16 as 16-bit int in GP registers)
    /// - libgcc: SSE ABI (Float16 in XMM registers)
    ///
    /// Currently inferred from target OS (macOS → compiler-rt, Linux → libgcc).
    pub fn float16_abi(&self) -> Float16Abi {
        if self.target.arch != Arch::X86_64 {
            // AArch64 uses native FP16 instructions, not rtlib
            return Float16Abi::Sse;
        }
        match self.target.os {
            Os::MacOS => Float16Abi::Integer, // compiler-rt
            Os::Linux => Float16Abi::Sse,     // libgcc
            Os::FreeBSD => Float16Abi::Sse,   // libgcc
        }
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

            // half <-> x87 extended precision (80-bit, x86_64)
            ("hf", "xf") => Some("__extendhfxf2"),
            ("xf", "hf") => Some("__truncxfhf2"),

            // half <-> quad precision (128-bit, aarch64)
            ("hf", "tf") => Some("__extendhftf2"),
            ("tf", "hf") => Some("__trunctfhf2"),

            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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

        // half <-> x87 extended precision (x86_64)
        assert_eq!(rtlib.float16_convert("hf", "xf"), Some("__extendhfxf2"));
        assert_eq!(rtlib.float16_convert("xf", "hf"), Some("__truncxfhf2"));

        // half <-> quad precision (aarch64)
        assert_eq!(rtlib.float16_convert("hf", "tf"), Some("__extendhftf2"));
        assert_eq!(rtlib.float16_convert("tf", "hf"), Some("__trunctfhf2"));

        // Invalid conversion
        assert_eq!(rtlib.float16_convert("hf", "invalid"), None);
    }

    // ========================================================================
}
