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
        // Feature test macros.
        //
        // These are predefined unconditionally, before any user -D/-U, so a
        // translation unit cannot get a strictly-POSIX view of the system
        // headers by leaving them out. POSIX only *encourages* restricting
        // visibility here (88196-88203), so this is a deliberate divergence
        // rather than a violation -- it matches what a GCC install on a glibc
        // system effectively provides, and unsetting them breaks a great deal
        // of code that assumes GNU extensions are visible. Recorded as #P12.
        ("_GNU_SOURCE", Some("1")),
        ("_DEFAULT_SOURCE", Some("1")),
        ("_XOPEN_SOURCE", Some("800")),
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
