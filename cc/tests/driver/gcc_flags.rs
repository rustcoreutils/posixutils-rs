//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// gcc driver flags real build systems pass, each of which used to stop c17
// before it compiled anything: `-pedantic`, `-ansi`, `-x LANG`, the `-g`
// levels and formats, and the `-m` flags that choose or tune for a CPU.
// Also the `__PIC__`/`__PIE__` macros, which describe the code generated.
//

use crate::common::{preprocess_text, run_c17};
use std::path::PathBuf;

/// A scratch directory holding `name` with `content`.
fn scratch(name: &str, content: &str) -> (plib::tmp::TempDir, PathBuf) {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_gcc_flags_")
        .tempdir()
        .expect("tempdir");
    let path = dir.path().join(name);
    std::fs::write(&path, content).expect("write");
    (dir, path)
}

const MAIN: &str = "int main(void) { return 0; }\n";

/// Compile `src` (written as `name`) to an object with `flags`.
fn compile_with(name: &str, src: &str, flags: &[&str]) -> crate::common::C17Run {
    let (dir, path) = scratch(name, src);
    let obj = dir.path().join("out.o");
    let mut args: Vec<&str> = flags.to_vec();
    args.extend(["-c", "-o", obj.to_str().unwrap(), path.to_str().unwrap()]);
    run_c17(&args)
}

#[test]
fn gcc_flags_pedantic_and_ansi_are_accepted() {
    for flags in [
        &["-pedantic"][..],
        &["-pedantic-errors"],
        &["-pedantic", "-pedantic-errors"],
        &["-Wpedantic"],
    ] {
        let r = compile_with("ped.c", MAIN, flags);
        assert!(r.success, "{flags:?}: {}", r.stderr);
    }
    // `-ansi` is `-std=c90`, reported as any older revision is.
    let r = compile_with("ansi.c", MAIN, &["-ansi"]);
    assert!(r.success, "{}", r.stderr);
    assert!(r.stderr.contains("'-std=c90' ignored"), "{}", r.stderr);
}

#[test]
fn gcc_flags_x_selects_the_language() {
    // A header compiled as C.
    let r = compile_with("hdr.h", MAIN, &["-x", "c"]);
    assert!(r.success, "{}", r.stderr);
    let r = compile_with("hdr.h", MAIN, &["-xc"]);
    assert!(r.success, "{}", r.stderr);

    // Assembly that needs the preprocessor, under a suffix that says neither.
    let r = compile_with(
        "code.asm",
        "#define NAME c17_x_sym\n.globl NAME\nNAME:\n    ret\n",
        &["-x", "assembler-with-cpp"],
    );
    assert!(r.success, "{}", r.stderr);

    // `-x none` goes back to suffixes, so a later `.h` is not compiled.
    let (dir, path) = scratch("later.h", MAIN);
    let c = dir.path().join("first.c");
    std::fs::write(&c, MAIN).unwrap();
    let obj = dir.path().join("o.o");
    let r = run_c17(&[
        "-x",
        "c",
        c.to_str().unwrap(),
        "-x",
        "none",
        path.to_str().unwrap(),
        "-c",
        "-o",
        obj.to_str().unwrap(),
    ]);
    assert!(r.stderr.contains("unrecognized file type"), "{}", r.stderr);

    // A language c17 does not compile is an error, not a guess.
    let r = compile_with("c.c", MAIN, &["-x", "c++"]);
    assert!(!r.success);
    assert!(
        r.stderr.contains("language not recognized: c++"),
        "{}",
        r.stderr
    );
}

/// The debug levels, on an ELF and a Mach-O target: the section is
/// `.debug_info` in ELF and `__DWARF,__debug_info` in Mach-O, which the
/// test once took for "no debug information" on macOS.
#[test]
fn gcc_flags_debug_levels() {
    let (dir, path) = scratch("dbg.c", "int f(int x) { return x + 1; }\n");
    let asm = dir.path().join("dbg.s");
    for (target, section) in [
        ("--target=x86_64-unknown-linux-gnu", ".section .debug_info"),
        (
            "--target=aarch64-apple-darwin",
            ".section __DWARF,__debug_info",
        ),
    ] {
        for (flags, want_debug) in [
            (&["-g3"][..], true),
            (&["-ggdb"], true),
            (&["-gdwarf-4"], true),
            (&["-g2", "-gsplit-dwarf"], true),
            (&["-g", "-g0"], false),
            (&["-g0", "-g1"], true),
        ] {
            let mut args = vec![target];
            args.extend(flags);
            args.extend(["-S", "-o", asm.to_str().unwrap(), path.to_str().unwrap()]);
            let r = run_c17(&args);
            assert!(r.success, "{target} {flags:?}: {}", r.stderr);
            let text = std::fs::read_to_string(&asm).unwrap();
            assert_eq!(text.contains(section), want_debug, "{target} {flags:?}");
        }
    }
}

#[test]
fn gcc_flags_cpu_selection_is_accepted() {
    let mut flags = vec!["-march=native", "-mtune=generic", "-mcpu=native"];
    if cfg!(target_arch = "x86_64") {
        flags.extend([
            "-m64",
            "-march=x86-64",
            "-march=x86-64-v2",
            "-msse2",
            "-mfpmath=sse",
        ]);
    }
    for flag in flags {
        let r = compile_with("cpu.c", MAIN, &[flag]);
        assert!(r.success, "{flag}: {}", r.stderr);
    }
    // An instruction-set extension or an ABI change is still refused.
    for flag in ["-mavx2", "-mno-red-zone", "-mgeneral-regs-only"] {
        let r = compile_with("cpu.c", MAIN, &[flag]);
        assert!(!r.success, "{flag} accepted");
        assert!(
            r.stderr.contains("unsupported machine flag"),
            "{}",
            r.stderr
        );
    }
}

/// `__PIC__`/`__pic__` whenever the code is position independent, and
/// `__PIE__`/`__pie__` when it is for a PIE -- the macros a `.S` file or an
/// inline asm statement tests before choosing a GOT access.
#[test]
fn gcc_flags_pic_macros_follow_the_code() {
    let probe = "PIC=__PIC__ pic=__pic__ PIE=__PIE__ pie=__pie__\n";
    let defined = |flags: &[&str]| {
        let r = preprocess_text("pic_probe", probe, flags);
        assert!(r.success, "{}", r.stderr);
        r.stdout
    };
    let out = defined(&["-fPIC", "-fno-pie"]);
    assert!(out.contains("PIC=2 pic=2 PIE=__PIE__"), "{out}");
    let out = defined(&["-fpie"]);
    assert!(out.contains("PIC=2 pic=2 PIE=2 pie=2"), "{out}");
    if cfg!(target_os = "linux") {
        // PIE is the Linux default, as it is for gcc there.
        let out = defined(&[]);
        assert!(out.contains("PIC=2 pic=2 PIE=2 pie=2"), "{out}");
        let out = defined(&["-fno-pie"]);
        assert!(out.contains("PIC=__PIC__"), "{out}");
    }
    if cfg!(target_os = "macos") {
        let out = defined(&["-fno-pie"]);
        assert!(out.contains("PIC=2"), "Mach-O is always PIC: {out}");
    }
}
