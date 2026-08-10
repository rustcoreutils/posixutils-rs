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

use crate::target::{Arch, Target};
use std::path::Path;

/// glibc version to assume when `features.h` cannot be found or parsed.
///
/// 2.17 is the oldest release still in wide use as a compatibility baseline;
/// guessing low is safe because `__GLIBC_PREREQ` tests then take the
/// conservative branch.
const FALLBACK_GLIBC: (&str, &str) = ("2", "17");

/// Read the real `__GLIBC__` / `__GLIBC_MINOR__` out of the `features.h` we
/// will actually compile against.
///
/// This is deliberately not a query of the *running* libc: what matters is the
/// header set on the include path, because that is what redefines these macros
/// mid-translation-unit. Predefining a value that disagrees with `features.h`
/// meant every `__GLIBC_PREREQ` test performed *before* a system header was
/// included got the wrong answer (audit #C3).
fn detect_glibc_version(target: &Target) -> (String, String) {
    for dir in get_include_paths(target) {
        let Ok(text) = std::fs::read_to_string(Path::new(dir).join("features.h")) else {
            continue;
        };

        let mut major = None;
        let mut minor = None;
        for line in text.lines() {
            // Lines look like `#define\t__GLIBC__\t2` — split on whitespace.
            let mut fields = line.split_whitespace();
            if fields.next() != Some("#define") {
                continue;
            }
            let (Some(name), Some(value)) = (fields.next(), fields.next()) else {
                continue;
            };
            // Only accept a plain integer; anything else is a macro we do not
            // understand and must not misreport.
            if !value.bytes().all(|b| b.is_ascii_digit()) {
                continue;
            }
            match name {
                "__GLIBC__" => major = Some(value.to_string()),
                "__GLIBC_MINOR__" => minor = Some(value.to_string()),
                _ => {}
            }
        }

        if let (Some(major), Some(minor)) = (major, minor) {
            return (major, minor);
        }
    }

    (FALLBACK_GLIBC.0.to_string(), FALLBACK_GLIBC.1.to_string())
}

/// Get Linux-specific predefined macros
pub fn get_macros(target: &Target) -> Vec<(&'static str, Option<String>)> {
    let (glibc_major, glibc_minor) = detect_glibc_version(target);

    vec![
        // Linux identification
        ("__linux__", Some("1".into())),
        ("__linux", Some("1".into())),
        ("linux", Some("1".into())),
        ("__gnu_linux__", Some("1".into())),
        // ELF binary format
        ("__ELF__", Some("1".into())),
        // GNU C library (glibc) compatibility, read from the host's features.h
        ("__GLIBC__", Some(glibc_major)),
        ("__GLIBC_MINOR__", Some(glibc_minor)),
        // Thread model
        ("_REENTRANT", Some("1".into())),
        // Feature test macros.
        //
        // These are predefined unconditionally, before any user -D/-U, so a
        // translation unit cannot get a strictly-POSIX view of the system
        // headers by leaving them out. POSIX only *encourages* restricting
        // visibility here (88196-88203), so this is a deliberate divergence
        // rather than a violation -- it matches what a GCC install on a glibc
        // system effectively provides, and unsetting them breaks a great deal
        // of code that assumes GNU extensions are visible. Recorded as #P12.
        ("_GNU_SOURCE", Some("1".into())),
        ("_DEFAULT_SOURCE", Some("1".into())),
        ("_XOPEN_SOURCE", Some("800".into())),
        ("_XOPEN_SOURCE_EXTENDED", Some("1".into())),
    ]
}

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
