//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// FreeBSD-specific predefined macros
//

/// Get FreeBSD-specific predefined macros
pub fn get_macros() -> Vec<(&'static str, Option<&'static str>)> {
    vec![
        // FreeBSD identification
        ("__FreeBSD__", Some("13")), // Conservative version
        ("__FreeBSD_kernel__", Some("1")),
        // ELF binary format
        ("__ELF__", Some("1")),
        // BSD compatibility
        ("BSD", Some("199506")),
        ("__BSD_VISIBLE", Some("1")),
        // POSIX threads
        ("_REENTRANT", Some("1")),
    ]
}

/// Get standard include paths for FreeBSD
pub fn get_include_paths() -> Vec<&'static str> {
    vec!["/usr/local/include", "/usr/include"]
}
