//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// OS-specific predefined macros
//

pub mod freebsd;
pub mod linux;
pub mod macos;

use crate::target::{Os, Target};

/// Get OS-specific predefined macros
pub fn get_os_macros(target: &Target) -> Vec<(&'static str, Option<&'static str>)> {
    let mut macros = vec![
        // POSIX compliance
        ("__STDC_HOSTED__", Some("1")),
        ("_POSIX_SOURCE", Some("1")),
        // POSIX.1-2024. Every delegated system header gates its 2024
        // prototypes behind this, so a c17-branded compiler that left it at
        // the 2008 value exposed a 16-year-old interface by default.
        ("_POSIX_C_SOURCE", Some("202405L")),
        // Unix-like
        ("__unix__", Some("1")),
        ("__unix", Some("1")),
        ("unix", Some("1")),
    ];

    match target.os {
        Os::Linux => {
            macros.extend(linux::get_macros());
        }
        Os::MacOS => {
            macros.extend(macos::get_macros());
        }
        Os::FreeBSD => {
            macros.extend(freebsd::get_macros());
        }
    }

    macros
}

/// Get standard include paths for the OS
pub fn get_include_paths(target: &Target) -> Vec<&'static str> {
    match target.os {
        Os::Linux => linux::get_include_paths(target),
        Os::MacOS => macos::get_include_paths(),
        Os::FreeBSD => freebsd::get_include_paths(),
    }
}
