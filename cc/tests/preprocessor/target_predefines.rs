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
/// the `__DARWIN__` / `__MACH_O__` that no Darwin compiler defines, nor
/// `_FORTIFY_SOURCE`.
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
        // Nor `_FORTIFY_SOURCE`, which the SDK's headers decide on and
        // which c17 had to pin at 0 while their `__builtin___*_chk`
        // calls were not all known.
        assert_eq!(defined(&m, "_FORTIFY_SOURCE"), None, "{triple}");
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

/// The feature-test macros are the program's to define (POSIX.1-2024 2.2.1):
/// gcc predefines none of them, nor does Apple clang or FreeBSD's clang, and
/// glibc's <features.h> computes `_DEFAULT_SOURCE` itself when a program
/// names nothing. `-pthread` alone adds `_REENTRANT`, as in gcc.
#[test]
fn feature_test_macros_are_not_predefined() {
    const FEATURE_MACROS: [&str; 8] = [
        "_GNU_SOURCE",
        "_DEFAULT_SOURCE",
        "_XOPEN_SOURCE",
        "_XOPEN_SOURCE_EXTENDED",
        "_REENTRANT",
        "_POSIX_C_SOURCE",
        "_POSIX_SOURCE",
        "_DARWIN_C_SOURCE",
    ];
    for triple in [
        "x86_64-unknown-linux-gnu",
        "aarch64-linux-gnu",
        "x86_64-apple-darwin",
        "aarch64-apple-darwin",
        "x86_64-unknown-freebsd",
        "aarch64-unknown-freebsd",
    ] {
        let m = predefines(triple);
        for name in FEATURE_MACROS {
            assert_eq!(defined(&m, name), None, "{name} on {triple}");
        }
    }

    // The host, spelled as gcc users spell the probe.
    let r = run_c17(&["-dM", "-E", "-x", "c", "/dev/null"]);
    assert!(r.success, "{}", r.stderr);
    for name in FEATURE_MACROS {
        assert_eq!(defined(&r.stdout, name), None, "{name}");
    }
    let r = run_c17(&["-pthread", "-dM", "-E", "-x", "c", "/dev/null"]);
    assert!(r.success, "{}", r.stderr);
    assert_eq!(defined(&r.stdout, "_REENTRANT").as_deref(), Some("1"));
    for name in FEATURE_MACROS.iter().filter(|n| **n != "_REENTRANT") {
        assert_eq!(defined(&r.stdout, name), None, "{name} with -pthread");
    }
}

/// gcc's x86-64 named address spaces say they exist on every x86-64 target,
/// and no other target has them: glibc's `THREAD_SELF` and the Linux
/// kernel's per-CPU accessors choose their `__seg_fs`/`__seg_gs` forms by
/// these.
#[test]
fn named_address_space_macros_are_x86_64s() {
    for triple in [
        "x86_64-unknown-linux-gnu",
        "x86_64-apple-darwin",
        "x86_64-unknown-freebsd",
    ] {
        let m = predefines(triple);
        for name in ["__SEG_FS", "__SEG_GS"] {
            assert_eq!(
                defined(&m, name).as_deref(),
                Some("1"),
                "{name} on {triple}"
            );
        }
    }
    for triple in ["aarch64-linux-gnu", "aarch64-apple-darwin"] {
        let m = predefines(triple);
        for name in ["__SEG_FS", "__SEG_GS"] {
            assert_eq!(defined(&m, name), None, "{name} on {triple}");
        }
    }
}

/// Inline asm can write a condition flag to an output (`"=@ccz"`) on both
/// targets, and says so as gcc does: the Linux kernel's `CC_SET`/`CC_OUT`
/// use flag outputs exactly when this is defined.
#[test]
fn asm_flag_outputs_macro_on_every_target() {
    for triple in [
        "x86_64-unknown-linux-gnu",
        "x86_64-apple-darwin",
        "aarch64-linux-gnu",
        "aarch64-apple-darwin",
    ] {
        let m = predefines(triple);
        assert_eq!(
            defined(&m, "__GCC_ASM_FLAG_OUTPUTS__").as_deref(),
            Some("1"),
            "{triple}"
        );
    }
}
