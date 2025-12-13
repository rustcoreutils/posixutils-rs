//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// macOS-specific predefined macros
//

/// Get macOS-specific predefined macros
pub fn get_macros() -> Vec<(&'static str, Option<&'static str>)> {
    vec![
        // Darwin/macOS identification
        ("__APPLE__", Some("1")),
        ("__MACH__", Some("1")),
        ("__DARWIN__", Some("1")),
        // Mach-O binary format
        ("__MACH_O__", Some("1")),
        // BSD compatibility
        ("__FreeBSD__", None), // Not defined
        ("__NetBSD__", None),  // Not defined
        // Apple extensions
        ("__APPLE_CC__", Some("1")),
        // POSIX threads
        ("_REENTRANT", Some("1")),
        // Version - match current macOS version (26.0.0 = macOS Tahoe)
        (
            "__ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__",
            Some("260000"),
        ),
        ("__ENVIRONMENT_OS_VERSION_MIN_REQUIRED__", Some("260000")),
    ]
}

/// Get standard include paths for macOS
pub fn get_include_paths() -> Vec<&'static str> {
    vec![
        "/usr/local/include",
        "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include",
        "/usr/include",
    ]
}
