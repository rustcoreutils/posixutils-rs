//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Linux-specific predefined macros
//

/// Get Linux-specific predefined macros
pub fn get_macros() -> Vec<(&'static str, Option<&'static str>)> {
    vec![
        // Linux identification
        ("__linux__", Some("1")),
        ("__linux", Some("1")),
        ("linux", Some("1")),
        ("__gnu_linux__", Some("1")),
        // ELF binary format
        ("__ELF__", Some("1")),
        // GNU C library (glibc) compatibility
        ("__GLIBC__", Some("2")),
        ("__GLIBC_MINOR__", Some("17")), // Conservative baseline
        // Thread model
        ("_REENTRANT", Some("1")),
        // Feature test macros
        ("_GNU_SOURCE", Some("1")),
        ("_DEFAULT_SOURCE", Some("1")),
        ("_XOPEN_SOURCE", Some("700")),
        ("_XOPEN_SOURCE_EXTENDED", Some("1")),
    ]
}

use crate::target::{Arch, Target};

/// Get standard include paths for Linux
pub fn get_include_paths(target: &Target) -> Vec<&'static str> {
    let mut paths = vec!["/usr/local/include"];

    // Add architecture-specific multiarch path (Debian/Ubuntu convention)
    // This must come BEFORE /usr/include so bits/*.h files are found
    match target.arch {
        Arch::X86_64 => paths.push("/usr/include/x86_64-linux-gnu"),
        Arch::Aarch64 => paths.push("/usr/include/aarch64-linux-gnu"),
    }

    paths.push("/usr/include");
    paths
}
