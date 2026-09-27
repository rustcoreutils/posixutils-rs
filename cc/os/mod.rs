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
    ];

    // The `unix` family names the ELF Unixes. Neither clang nor gcc defines
    // it for Darwin, which code tells apart by `__APPLE__` instead.
    let unix = ["__unix__", "__unix", "unix"].map(|name| (name, Some("1".to_string())));

    match target.os {
        Os::Linux => {
            macros.extend(unix);
            macros.extend(linux::get_macros(target));
        }
        Os::MacOS => {
            macros.extend(macos::get_macros());
        }
        Os::FreeBSD => {
            macros.extend(unix);
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::target::Arch;

    fn defines(target: &Target, name: &str) -> bool {
        get_os_macros(target).iter().any(|(n, _)| *n == name)
    }

    /// Each OS predefines its own identity and no other's. The macOS list
    /// carried `__FreeBSD__` and `__NetBSD__` as "not defined" entries, which
    /// defined both, empty, so `#ifdef __FreeBSD__` was true on Apple. And
    /// neither clang nor gcc defines the `unix` family for Darwin: portable
    /// code tests `__unix__ || __APPLE__` for exactly that reason.
    #[test]
    fn each_os_names_only_itself() {
        for arch in [Arch::X86_64, Arch::Aarch64] {
            for (os, own, foreign) in [
                (
                    Os::Linux,
                    &[
                        "__linux__",
                        "__linux",
                        "linux",
                        "__gnu_linux__",
                        "unix",
                        "__unix",
                        "__unix__",
                    ][..],
                    &["__APPLE__", "__MACH__", "__FreeBSD__", "__NetBSD__"][..],
                ),
                (
                    Os::MacOS,
                    &["__APPLE__", "__MACH__"][..],
                    &[
                        "__linux__",
                        "linux",
                        "__FreeBSD__",
                        "__NetBSD__",
                        "unix",
                        "__unix",
                        "__unix__",
                        "__ELF__",
                    ][..],
                ),
                (
                    Os::FreeBSD,
                    &["__FreeBSD__", "unix", "__unix", "__unix__"][..],
                    &["__APPLE__", "__MACH__", "__linux__", "linux", "__NetBSD__"][..],
                ),
            ] {
                let target = Target::new(arch, os);
                for name in own {
                    assert!(defines(&target, name), "{name} missing on {arch}-{os}");
                }
                for name in foreign {
                    assert!(!defines(&target, name), "{name} defined on {arch}-{os}");
                }
            }
        }
    }
}
