//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// gcc driver options that belong to the link, which c17 hands to the host
// driver that does its linking.
//

use crate::common::run_c17;
use std::path::PathBuf;

/// A scratch directory holding `m.c`, a program that does nothing.
fn scratch() -> (plib::tmp::TempDir, PathBuf) {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_link_options_")
        .tempdir()
        .expect("tempdir");
    let src = dir.path().join("m.c");
    std::fs::write(&src, "int main(void) { return 0; }\n").expect("write");
    (dir, src)
}

/// `-specs=FILE` reaches the link. Debian's default LDFLAGS carry
/// `-specs=.../debian-package-notes.specs` and libxcrypt's CFLAGS dpkg's
/// `no-pie-*.specs`; clap read the option as the short cluster `-s -p ...`
/// and every configure run stopped at "C compiler cannot create
/// executables". Spec files are gcc's driver language, so they are the
/// host driver's to read: a `*link:` spec that writes a map file proves it
/// was.
#[cfg(target_os = "linux")]
#[test]
fn link_options_specs_reach_the_host_driver() {
    let (dir, src) = scratch();
    let map = dir.path().join("out.map");
    let specs = dir.path().join("map.specs");
    std::fs::write(&specs, format!("*link:\n+ -Map={}\n", map.display())).unwrap();
    let exe = dir.path().join("m");
    let r = run_c17(&[
        &format!("-specs={}", specs.display()),
        "-o",
        exe.to_str().unwrap(),
        src.to_str().unwrap(),
    ]);
    assert!(r.success, "{}", r.stderr);
    assert!(map.exists(), "the link spec was not applied");

    // A compile alone has no link for it to reach, and is quiet about it.
    let obj = dir.path().join("m.o");
    let r = run_c17(&[
        &format!("-specs={}", specs.display()),
        "-c",
        "-o",
        obj.to_str().unwrap(),
        src.to_str().unwrap(),
    ]);
    assert!(r.success, "{}", r.stderr);
    assert!(r.stderr.is_empty(), "{}", r.stderr);
}

/// A spec file that cannot be read stops the run, compile or link, as it
/// stops gcc's.
#[test]
fn link_options_unreadable_specs_are_fatal() {
    let (dir, src) = scratch();
    let obj = dir.path().join("m.o");
    let r = run_c17(&[
        "-specs=no-such-c17.specs",
        "-c",
        "-o",
        obj.to_str().unwrap(),
        src.to_str().unwrap(),
    ]);
    assert!(!r.success);
    assert!(
        r.stderr
            .starts_with("c17: fatal error: cannot read spec file 'no-such-c17.specs': "),
        "{}",
        r.stderr
    );
}

/// `-nostartfiles` and `-nostdlib` reach the link. glibc's configure links a
/// `.S` file that defines its own `_start` with both to learn whether the
/// toolchain supports IFUNC relocations, and c17 stopped at clap's
/// `unexpected argument '-n'`, so glibc concluded it did not. Without the
/// options the host's start files would define `_start` a second time.
#[cfg(target_os = "linux")]
#[test]
fn link_options_nostartfiles_nostdlib_reach_the_link() {
    let (dir, _) = scratch();
    let src = dir.path().join("conftest.S");
    std::fs::write(
        &src,
        ".type foo,%gnu_indirect_function\nfoo:\n.globl _start\n_start:\n\
         .globl __start\n__start:\n.data\n#ifdef _LP64\n.quad foo\n#else\n\
         .long foo\n#endif\n",
    )
    .unwrap();
    let exe = dir.path().join("conftest");
    let r = run_c17(&[
        "-nostartfiles",
        "-nostdlib",
        "-o",
        exe.to_str().unwrap(),
        src.to_str().unwrap(),
    ]);
    assert!(r.success, "{}", r.stderr);
    assert!(exe.exists());
}
