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
pub fn get_os_macros(target: &Target) -> Vec<(&'static str, Option<String>)> {
    let mut macros = vec![
        // POSIX compliance
        ("__STDC_HOSTED__", Some("1".into())),
        ("_POSIX_SOURCE", Some("1".into())),
        // POSIX.1-2024. Every delegated system header gates its 2024
        // prototypes behind this, so a c17-branded compiler that left it at
        // the 2008 value exposed a 16-year-old interface by default.
        ("_POSIX_C_SOURCE", Some("202405L".into())),
        // Unix-like
        ("__unix__", Some("1".into())),
        ("__unix", Some("1".into())),
        ("unix", Some("1".into())),
    ];

    match target.os {
        Os::Linux => {
            macros.extend(linux::get_macros(target));
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

/// The system include directories for `target`, under `sysroot` if one was
/// given.
///
/// Owned `String`s rather than `&'static str`: a sysroot has to be joined on,
/// and there is nothing to borrow it from. Without that, `--target` could only
/// ever name the *host's* directories, so cross-compiling anything that
/// included a system header failed on `bits/libc-header-start.h`.
pub fn get_include_paths(target: &Target, sysroot: Option<&str>) -> Vec<String> {
    let paths = match target.os {
        Os::Linux => linux::get_include_paths(target),
        Os::MacOS => macos::get_include_paths(),
        Os::FreeBSD => freebsd::get_include_paths(),
    };
    match sysroot {
        // `/usr/include` under `/x` is `/x/usr/include`. `Path::join` would
        // discard the prefix, an absolute path replacing the base entirely.
        Some(root) => {
            let root = root.trim_end_matches('/');
            paths.iter().map(|p| format!("{}{}", root, p)).collect()
        }
        None => paths.iter().map(|p| p.to_string()).collect(),
    }
}
