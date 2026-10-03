//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Each target's predefined macros against its system compiler's: Darwin's
// against Apple clang, FreeBSD's against its base-system clang. Probed by
// cross-targeting `-dM` of an empty input, so they run on any host.
//

use crate::common::run_c17;

/// `c17 --target <triple> -E -dM` of an empty translation unit.
fn predefines(triple: &str) -> String {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_predef_")
        .tempdir()
        .expect("tempdir");
    let empty = dir.path().join("empty.c");
    std::fs::write(&empty, "").unwrap();
    let r = run_c17(&["--target", triple, "-E", "-dM", empty.to_str().unwrap()]);
    assert!(r.success, "{triple}: {}", r.stderr);
    r.stdout
}

fn defined(macros: &str, name: &str) -> Option<String> {
    macros.lines().find_map(|l| {
        let rest = l.strip_prefix("#define ")?;
        let (n, v) = rest.split_once(' ').unwrap_or((rest, ""));
        (n == name).then(|| v.to_string())
    })
}

/// Apple clang's: `__APPLE__`, `__MACH__`, `__APPLE_CC__` 6000, and none of
/// the `__DARWIN__` / `__MACH_O__` that no Darwin compiler defines.
#[test]
fn darwin_predefines_match_clang() {
    for triple in ["aarch64-apple-darwin", "x86_64-apple-darwin"] {
        let m = predefines(triple);
        assert_eq!(defined(&m, "__APPLE__").as_deref(), Some("1"), "{triple}");
        assert_eq!(defined(&m, "__MACH__").as_deref(), Some("1"), "{triple}");
        assert_eq!(
            defined(&m, "__APPLE_CC__").as_deref(),
            Some("6000"),
            "{triple}"
        );
        assert_eq!(defined(&m, "__DARWIN__"), None, "{triple}");
        assert_eq!(defined(&m, "__MACH_O__"), None, "{triple}");
        assert_eq!(
            defined(&m, "__WINT_TYPE__").as_deref(),
            Some("int"),
            "{triple}"
        );
    }
}

/// FreeBSD's `wint_t` is its `__ct_rune_t`, an `int`; `__FreeBSD_kernel__`
/// names GNU/kFreeBSD, and `__BSD_VISIBLE` is `<sys/cdefs.h>`'s to compute.
#[test]
fn freebsd_predefines_match_its_compiler() {
    for triple in ["x86_64-unknown-freebsd", "aarch64-unknown-freebsd"] {
        let m = predefines(triple);
        assert!(defined(&m, "__FreeBSD__").is_some(), "{triple}");
        assert_eq!(
            defined(&m, "__WINT_TYPE__").as_deref(),
            Some("int"),
            "{triple}"
        );
        assert_eq!(defined(&m, "__FreeBSD_kernel__"), None, "{triple}");
        assert_eq!(defined(&m, "__BSD_VISIBLE"), None, "{triple}");
    }
}

/// An operating system c17 does not support is an error, not Linux.
#[test]
fn unknown_target_os_is_rejected() {
    for triple in [
        "x86_64-pc-windows-msvc",
        "x86_64-w64-mingw32",
        "aarch64-unknown-none",
        "x86_64-unknown-netbsd",
    ] {
        let r = run_c17(&["--target", triple, "-E", "-dM", "-"]);
        assert!(!r.success, "{triple} accepted");
        assert!(r.stderr.contains("unsupported target"), "{}", r.stderr);
    }
    // Linux, under the spellings the test suites use.
    for triple in ["x86_64-unknown-linux-gnu", "aarch64-linux-gnu"] {
        assert!(
            defined(&predefines(triple), "__linux__").is_some(),
            "{triple}"
        );
    }
}
