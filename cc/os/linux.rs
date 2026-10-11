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

/// Get Linux-specific predefined macros
pub fn get_macros() -> Vec<(&'static str, Option<String>)> {
    vec![
        // Linux identification
        // ELF adds no prefix to a C identifier.
        ("__USER_LABEL_PREFIX__", Some("".into())),
        ("__linux__", Some("1".into())),
        ("__linux", Some("1".into())),
        ("linux", Some("1".into())),
        ("__gnu_linux__", Some("1".into())),
        // ELF binary format
        ("__ELF__", Some("1".into())),
        // No `__GLIBC__` / `__GLIBC_MINOR__`: they are glibc's, defined by
        // <features.h>, and gcc does not predefine them either. binutils'
        // config.h refuses to be read after a system header, which it detects
        // by `__GLIBC__` being defined.
        // No feature-test macros (`_GNU_SOURCE`, `_POSIX_C_SOURCE`, ...) and
        // no `_REENTRANT`: they are the program's to define (POSIX.1-2024
        // 2.2.1), and gcc predefines none of them. A program that names none
        // gets glibc's defaults from <features.h>, `_DEFAULT_SOURCE` among
        // them. `-pthread` defines `_REENTRANT`, as gcc's does.
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
