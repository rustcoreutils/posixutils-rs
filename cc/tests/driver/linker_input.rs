//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Operands c17 does not compile. As in gcc, one whose suffix names no
// language is a linker input, handed to the linker in its command-line
// place: an object under another name, a linker script, a version script.
// One whose suffix names a language gcc compiles and c17 does not (C++,
// Fortran, ...) is an error, never something for the linker to choke on.
//

use crate::common::run_c17;
use std::path::{Path, PathBuf};

const UNUSED: &str = "linker input file unused because linking not done";

/// A scratch directory that removes itself.
fn workdir() -> plib::tmp::TempDir {
    plib::tmp::Builder::new()
        .prefix("c17_linker_input_")
        .tempdir()
        .expect("tempdir")
}

fn write(dir: &Path, name: &str, content: &str) -> PathBuf {
    let path = dir.join(name);
    std::fs::write(&path, content).expect("write");
    path
}

fn s(p: &Path) -> &str {
    p.to_str().unwrap()
}

/// `int f(void)` returning 8, compiled by c17 to `name` under `dir`.
fn object_named(dir: &Path, name: &str) -> PathBuf {
    let src = write(dir, "f.c", "int f(void) { return 8; }\n");
    let obj = dir.join(name);
    let r = run_c17(&["-c", "-o", s(&obj), s(&src)]);
    assert!(r.success, "{}", r.stderr);
    obj
}

const MAIN: &str = "int f(void);\nint main(void) { return f() + 1; }\n";

fn run_exe(exe: &Path) -> Option<i32> {
    std::process::Command::new(exe)
        .status()
        .expect("run")
        .code()
}

/// An object under a suffix that names nothing reaches the linker as-is, in
/// its place, with no warning, as in gcc.
#[test]
fn linker_input_unknown_suffix_is_linked() {
    let dir = workdir();
    let obj = object_named(dir.path(), "f.weird");
    let main = write(dir.path(), "m.c", MAIN);
    let exe = dir.path().join("m");
    let r = run_c17(&["-o", s(&exe), s(&main), s(&obj)]);
    assert!(r.success, "{}", r.stderr);
    assert!(r.stderr.is_empty(), "{}", r.stderr);
    assert_eq!(run_exe(&exe), Some(9));
}

/// `-x none` after `-x c` goes back to suffixes, so a later unknown suffix
/// is a linker input again, not C.
#[test]
fn linker_input_after_x_none() {
    let dir = workdir();
    let obj = object_named(dir.path(), "f.bin");
    let main = write(dir.path(), "m.txt", MAIN);
    let exe = dir.path().join("m");
    let r = run_c17(&["-x", "c", s(&main), "-x", "none", s(&obj), "-o", s(&exe)]);
    assert!(r.success, "{}", r.stderr);
    assert_eq!(run_exe(&exe), Some(9));
}

/// A GNU ld linker script operand is read by the linker: here it pulls in
/// the object that defines `f`, which the command line never names.
#[cfg(target_os = "linux")]
#[test]
fn linker_input_linker_script_is_honoured() {
    let dir = workdir();
    let obj = object_named(dir.path(), "f.o");
    let script = write(dir.path(), "extra.ld", &format!("INPUT({})\n", s(&obj)));
    let main = write(dir.path(), "m.c", MAIN);
    let exe = dir.path().join("m");
    let r = run_c17(&["-o", s(&exe), s(&main), s(&script)]);
    assert!(r.success, "{}", r.stderr);
    assert_eq!(run_exe(&exe), Some(9));
}

/// With nothing linked, every linker input is reported unused, in gcc's
/// words, and the run still succeeds. gcc's driver says so under `-w` and
/// `-Werror` alike.
#[test]
fn linker_input_unused_without_linking() {
    let dir = workdir();
    let main = write(dir.path(), "m.c", MAIN);
    let weird = write(dir.path(), "x.weird", "");
    let obj = object_named(dir.path(), "g.o");
    let out = dir.path().join("out");
    for mode in ["-c", "-S", "-E"] {
        for extra in [&[][..], &["-w"], &["-Werror"]] {
            let mut args = vec![mode, "-o", s(&out)];
            args.extend(extra);
            args.extend([s(&main), s(&weird), s(&obj)]);
            let r = run_c17(&args);
            assert!(r.success, "{mode} {extra:?}: {}", r.stderr);
            for input in [&weird, &obj] {
                let want = format!("c17: warning: {}: {UNUSED}", s(input));
                assert!(r.stderr.contains(&want), "{mode} {extra:?}: {}", r.stderr);
            }
            assert!(!r.stderr.contains("unrecognized"), "{}", r.stderr);
        }
    }
}

/// A linker input that does not exist: the linker's error when linking,
/// gcc's "not found" error (after the unused warning) when not.
#[test]
fn linker_input_missing_is_an_error() {
    let dir = workdir();
    let main = write(dir.path(), "m.c", "int main(void) { return 0; }\n");
    let missing = dir.path().join("gone.weird");
    let exe = dir.path().join("m");
    let r = run_c17(&["-o", s(&exe), s(&main), s(&missing)]);
    assert!(!r.success, "{}", r.stderr);
    assert!(r.stderr.contains("gone.weird"), "{}", r.stderr);
    assert!(!exe.exists());

    let obj = dir.path().join("m.o");
    let r = run_c17(&["-c", "-o", s(&obj), s(&main), s(&missing)]);
    assert!(!r.success, "{}", r.stderr);
    assert!(
        r.stderr
            .contains(&format!("warning: {}: {UNUSED}", s(&missing))),
        "{}",
        r.stderr
    );
    assert!(
        r.stderr.contains(&format!(
            "error: {}: linker input file not found",
            s(&missing)
        )),
        "{}",
        r.stderr
    );
}

/// A suffix that names a language gcc compiles and c17 does not is an
/// error naming the language; the file never reaches the linker, and the
/// other operands are still compiled.
#[test]
fn linker_input_foreign_language_is_an_error() {
    let dir = workdir();
    let main = write(dir.path(), "m.c", "int main(void) { return 0; }\n");
    let obj = dir.path().join("m.o");
    let exe = dir.path().join("m");
    for (name, lang) in [
        ("t.cpp", "C++"),
        ("t.cc", "C++"),
        ("t.C", "C++"),
        ("t.ii", "C++"),
        ("t.hpp", "C++"),
        ("t.m", "Objective-C"),
        ("t.mm", "Objective-C++"),
        ("t.f90", "Fortran"),
        ("t.F", "Fortran"),
        ("t.go", "Go"),
        ("t.d", "D"),
        ("t.adb", "Ada"),
        ("t.mod", "Modula-2"),
    ] {
        let foreign = write(dir.path(), name, "int x;\n");
        let want = format!("c17: error: {}: c17 does not compile {lang}", s(&foreign));
        let r = run_c17(&["-o", s(&exe), s(&main), s(&foreign)]);
        assert!(!r.success, "{name}: {}", r.stderr);
        assert!(r.stderr.contains(&want), "{name}: {}", r.stderr);
        assert!(!exe.exists(), "{name}");

        let r = run_c17(&["-c", "-o", s(&obj), s(&main), s(&foreign)]);
        assert!(!r.success, "{name}: {}", r.stderr);
        assert!(r.stderr.contains(&want), "{name}: {}", r.stderr);
        assert!(obj.exists(), "{name}: the C operand was not compiled");
        std::fs::remove_file(&obj).unwrap();
    }
}

/// `-M` links nothing either: its linker inputs are unused, not linked.
#[test]
fn linker_input_unused_under_m() {
    let dir = workdir();
    let main = write(dir.path(), "m.c", "int f(void);\n");
    let weird = write(dir.path(), "x.weird", "");
    for mode in ["-M", "-MM"] {
        let r = run_c17(&[mode, s(&main), s(&weird)]);
        assert!(r.success, "{mode}: {}", r.stderr);
        assert!(r.stdout.starts_with("m.o:"), "{mode}: {}", r.stdout);
        assert!(
            r.stderr
                .contains(&format!("warning: {}: {UNUSED}", s(&weird))),
            "{mode}: {}",
            r.stderr
        );
    }
}

/// A `.h` operand is a C header: gcc preprocesses it under `-E` and `-M`,
/// and otherwise writes a precompiled header, which c17 does not.
#[test]
fn linker_input_c_header() {
    let dir = workdir();
    let hdr = write(dir.path(), "t.h", "#define V 42\nint v = V;\n");
    let r = run_c17(&["-E", "-P", s(&hdr)]);
    assert!(r.success, "{}", r.stderr);
    assert!(r.stdout.contains("int v = 42;"), "{}", r.stdout);

    let r = run_c17(&["-M", s(&hdr)]);
    assert!(r.success, "{}", r.stderr);
    assert!(r.stdout.starts_with("t.o:"), "{}", r.stdout);

    let r = run_c17(&["-c", s(&hdr)]);
    assert!(!r.success, "{}", r.stderr);
    assert!(
        r.stderr.contains(&format!(
            "c17: error: {}: c17 does not write precompiled headers",
            s(&hdr)
        )),
        "{}",
        r.stderr
    );
}

/// `.sx` is gcc's other spelling of assembly to preprocess.
#[test]
fn linker_input_sx_is_preprocessed_assembly() {
    let dir = workdir();
    let asm = write(
        dir.path(),
        "a.sx",
        "#define NAME c17_sx_sym\n.globl NAME\nNAME:\n    ret\n",
    );
    let obj = dir.path().join("a.o");
    let r = run_c17(&["-c", "-o", s(&obj), s(&asm)]);
    assert!(r.success, "{}", r.stderr);
    assert!(obj.exists());
}
