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

/// `-nostdlib -nostartfiles` reach the link: a program that is its own
/// `_start` links with neither the startup files nor the C library, and
/// runs. glibc's configure probes the linker this way (STT_GNU_IFUNC,
/// protected data, TLS descriptors) and links `ld.so` and `libc.so` so; with
/// the options refused, its build stopped at "--enable-multi-arch support
/// requires assembler and linker support".
#[cfg(target_os = "linux")]
#[test]
fn link_options_nostdlib_links_a_bare_start() {
    let (dir, _) = scratch();
    let start = dir.path().join("start.s");
    let body = if cfg!(target_arch = "x86_64") {
        "\tmovl $60, %eax\n\tmovl $7, %edi\n\tsyscall\n"
    } else {
        "\tmov x8, #93\n\tmov x0, #7\n\tsvc #0\n"
    };
    std::fs::write(&start, format!("\t.text\n\t.globl _start\n_start:\n{body}")).unwrap();
    let exe = dir.path().join("bare");
    for flags in [
        &["-nostdlib"][..],
        &["-nostartfiles", "-nostdlib"],
        &["-nostartfiles", "-nodefaultlibs"],
        &["-nostartfiles", "-nolibc", "-static"],
    ] {
        let _ = std::fs::remove_file(&exe);
        let mut args = flags.to_vec();
        args.extend(["-o", exe.to_str().unwrap(), start.to_str().unwrap()]);
        let r = run_c17(&args);
        assert!(r.success, "{flags:?}: {}", r.stderr);
        let status = std::process::Command::new(&exe).status().expect("run");
        assert_eq!(status.code(), Some(7), "{flags:?}");
    }
}

/// `-nodefaultlibs` and `-nolibc` leave the C library out of the link and
/// `-nostartfiles` the startup files, as gcc's driver does: a program that
/// calls `puts` no longer links without `-lc`, and one linked without the
/// startup files has no `_start`.
#[cfg(target_os = "linux")]
#[test]
fn link_options_nodefaultlibs_and_nolibc_drop_the_c_library() {
    let (dir, _) = scratch();
    let src = dir.path().join("p.c");
    std::fs::write(
        &src,
        "int puts(const char *);\nint main(void) { return puts(\"x\") < 0; }\n",
    )
    .unwrap();
    let exe = dir.path().join("p");
    let link = |flags: &[&str]| {
        let mut args = flags.to_vec();
        args.extend(["-o", exe.to_str().unwrap(), src.to_str().unwrap()]);
        run_c17(&args)
    };
    for flag in ["-nodefaultlibs", "-nolibc"] {
        let r = link(&[flag]);
        assert!(!r.success, "{flag}: linked without the C library");
        assert!(r.stderr.contains("puts"), "{flag}: {}", r.stderr);
        let r = link(&[flag, "-lc"]);
        assert!(r.success, "{flag} -lc: {}", r.stderr);
    }
    let r = link(&["-nostartfiles"]);
    assert!(r.success, "{}", r.stderr);
    let r = std::process::Command::new("nm")
        .arg(&exe)
        .output()
        .expect("nm");
    let syms = String::from_utf8_lossy(&r.stdout);
    assert!(!syms.contains(" T _start\n"), "{syms}");
}

/// The libgcc options reach the link, and the link-only options are quiet
/// about a compile, as gcc's are: gcc-14's top-level configure puts
/// `-static-libstdc++ -static-libgcc` in every stage-1 LDFLAGS, glibc links
/// each DSO with `-static-libgcc`, and clap took `-shared-libgcc` for the
/// cluster `-s -h` and printed its help instead of linking.
#[test]
fn link_options_libgcc_options_are_taken() {
    let (dir, src) = scratch();
    let exe = dir.path().join("m");
    let obj = dir.path().join("m.o");
    for flag in [
        "-static-libgcc",
        "-static-libstdc++",
        "-shared-libgcc",
        "-nostdlib",
        "-nostartfiles",
        "-nodefaultlibs",
        "-nolibc",
    ] {
        let r = run_c17(&[
            flag,
            "-c",
            "-o",
            obj.to_str().unwrap(),
            src.to_str().unwrap(),
        ]);
        assert!(r.success, "{flag}: {}", r.stderr);
        assert!(
            r.stderr.is_empty() && r.stdout.is_empty(),
            "{flag}: {}{}",
            r.stdout,
            r.stderr
        );
        // Linked where the host driver is gcc, which knows them all.
        if cfg!(target_os = "linux") && flag.contains("lib") && flag.starts_with("-s") {
            let r = run_c17(&[flag, "-o", exe.to_str().unwrap(), src.to_str().unwrap()]);
            assert!(r.success, "{flag}: {}", r.stderr);
            let status = std::process::Command::new(&exe).status().expect("run");
            assert_eq!(status.code(), Some(0), "{flag}");
        }
    }
}
