//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Architecture-specific predefined macros and code generators
//

/// Default capacity for LIR instruction buffers (reduces reallocation overhead)
pub const DEFAULT_LIR_BUFFER_CAPACITY: usize = 5000;

pub mod aarch64;
pub mod codegen;
pub mod lir;
pub mod x86_64;

use crate::target::{Arch, Target};

/// Get architecture-specific predefined macros as (name, value) pairs
pub fn get_arch_macros(target: &Target) -> Vec<(&'static str, Option<&'static str>)> {
    let mut macros = Vec::new();

    // Common architecture macros based on type sizes
    macros.push(("__CHAR_BIT__", Some("8")));

    // Pointer size
    if target.pointer_width == 64 {
        macros.push(("__LP64__", Some("1")));
        macros.push(("_LP64", Some("1")));
        macros.push(("__SIZEOF_POINTER__", Some("8")));
    } else {
        macros.push(("__ILP32__", Some("1")));
        macros.push(("__SIZEOF_POINTER__", Some("4")));
    }

    // Type sizes
    macros.push(("__SIZEOF_SHORT__", Some("2")));
    macros.push(("__SIZEOF_INT__", Some("4")));

    if target.long_width == 64 {
        macros.push(("__SIZEOF_LONG__", Some("8")));
    } else {
        macros.push(("__SIZEOF_LONG__", Some("4")));
    }

    macros.push(("__SIZEOF_LONG_LONG__", Some("8")));
    macros.push(("__SIZEOF_FLOAT__", Some("4")));
    macros.push(("__SIZEOF_DOUBLE__", Some("8")));

    // Signedness
    if target.char_signed {
        macros.push(("__CHAR_UNSIGNED__", None)); // Not defined
    } else {
        macros.push(("__CHAR_UNSIGNED__", Some("1")));
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
    let mut macros = Vec::new();

    // Character limits
    macros.push(("__SCHAR_MAX__", "127"));
    macros.push(("__SHRT_MAX__", "32767"));
    macros.push(("__INT_MAX__", "2147483647"));

    if target.long_width == 64 {
        macros.push(("__LONG_MAX__", "9223372036854775807L"));
    } else {
        macros.push(("__LONG_MAX__", "2147483647L"));
    }

    macros.push(("__LONG_LONG_MAX__", "9223372036854775807LL"));

    // Width macros
    macros.push(("__SCHAR_WIDTH__", "8"));
    macros.push(("__SHRT_WIDTH__", "16"));
    macros.push(("__INT_WIDTH__", "32"));

    if target.long_width == 64 {
        macros.push(("__LONG_WIDTH__", "64"));
    } else {
        macros.push(("__LONG_WIDTH__", "32"));
    }

    macros.push(("__LLONG_WIDTH__", "64"));

    // Size type
    if target.pointer_width == 64 {
        macros.push(("__SIZE_MAX__", "18446744073709551615UL"));
        macros.push(("__SIZE_WIDTH__", "64"));
        macros.push(("__PTRDIFF_MAX__", "9223372036854775807L"));
        macros.push(("__PTRDIFF_WIDTH__", "64"));
        macros.push(("__INTPTR_MAX__", "9223372036854775807L"));
        macros.push(("__INTPTR_WIDTH__", "64"));
        macros.push(("__UINTPTR_MAX__", "18446744073709551615UL"));
    } else {
        macros.push(("__SIZE_MAX__", "4294967295U"));
        macros.push(("__SIZE_WIDTH__", "32"));
        macros.push(("__PTRDIFF_MAX__", "2147483647"));
        macros.push(("__PTRDIFF_WIDTH__", "32"));
        macros.push(("__INTPTR_MAX__", "2147483647"));
        macros.push(("__INTPTR_WIDTH__", "32"));
        macros.push(("__UINTPTR_MAX__", "4294967295U"));
    }

    macros
}
