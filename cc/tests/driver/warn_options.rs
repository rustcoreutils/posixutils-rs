//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `-W<name>` as a configure probe sees it: a name gcc 13 does not know fails
// the compile with gcc's text, so a flag check gets gcc's answer.
//

use crate::common::run_c17;
use std::path::{Path, PathBuf};

/// A scratch directory holding `name` with `content`.
fn scratch(name: &str, content: &str) -> (plib::tmp::TempDir, PathBuf) {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_warnopt_")
        .tempdir()
        .expect("tempdir");
    let path = dir.path().join(name);
    std::fs::write(&path, content).expect("write");
    (dir, path)
}

/// Compile `path` to an object in `dir` with `flags` ahead of the operand.
fn compile_in(dir: &Path, path: &Path, flags: &[&str]) -> crate::common::C17Run {
    let obj = dir.join("out.o");
    let mut args: Vec<&str> = flags.to_vec();
    args.extend(["-c", "-o", obj.to_str().unwrap(), path.to_str().unwrap()]);
    run_c17(&args)
}

const CLEAN: &str = "int main(void) { return 0; }\n";
/// A warning c17 gives by default: `-Wshift-count-overflow`.
const WARNS: &str = "int f(void) { int x = 1 << 40; return x; }\n";

fn note(option: &str) -> String {
    format!(
        "c17: note: unrecognized command-line option '{option}' \
         may have been intended to silence earlier diagnostics"
    )
}

/// `-pedantic-errors` is an option of its own, not a `-W` name: gcc refuses
/// `-Wpedantic-errors`, though c17 folds `-pedantic-errors` in among the
/// `-W` options internally.
#[test]
fn pedantic_errors_is_not_a_warning_name() {
    let (dir, path) = scratch("t.c", CLEAN);
    let r = compile_in(dir.path(), &path, &["-Wpedantic-errors"]);
    assert!(!r.success, "{}", r.stderr);
    assert_eq!(
        r.stderr,
        "c17: error: unrecognized command-line option '-Wpedantic-errors'\n"
    );
    let r = compile_in(dir.path(), &path, &["-pedantic-errors"]);
    assert!(r.success, "{}", r.stderr);
}

#[test]
fn unknown_warning_is_an_error() {
    let (dir, path) = scratch("t.c", CLEAN);
    let r = compile_in(dir.path(), &path, &["-Wfoo"]);
    assert!(!r.success, "{}", r.stderr);
    assert_eq!(
        r.stderr, "c17: error: unrecognized command-line option '-Wfoo'\n",
        "{}",
        r.stderr
    );
    assert!(!dir.path().join("out.o").exists(), "an object was written");

    // Every bad name is reported, the driver's before the compiler's.
    let r = compile_in(dir.path(), &path, &["-Werror=baz", "-Wfoo", "-Wbar"]);
    assert!(!r.success);
    assert_eq!(
        r.stderr,
        "c17: error: unrecognized command-line option '-Wfoo'\n\
         c17: error: unrecognized command-line option '-Wbar'\n",
        "{}",
        r.stderr
    );
}

#[test]
fn unknown_werror_names_are_errors() {
    let (dir, path) = scratch("t.c", CLEAN);
    for (flag, text) in [
        ("-Werror=foo", "'-Werror=foo': no option '-Wfoo'"),
        ("-Wno-error=foo", "'-Wno-error=foo': no option '-Wfoo'"),
        (
            "-Werror=no-format",
            "'-Werror=no-format': no option '-Wno-format'",
        ),
        ("-Werror=", "missing argument to '-Werror='"),
    ] {
        let r = compile_in(dir.path(), &path, &[flag]);
        assert!(!r.success, "{flag}: {}", r.stderr);
        assert_eq!(r.stderr, format!("c17: error: {text}\n"), "{flag}");
    }
    for flag in ["-Werror=format-security", "-Wno-error=maybe-uninitialized"] {
        let r = compile_in(dir.path(), &path, &[flag]);
        assert!(r.success && r.stderr.is_empty(), "{flag}: {}", r.stderr);
    }
}

#[test]
fn unknown_negation_is_silent_on_clean_code() {
    let (dir, path) = scratch("t.c", CLEAN);
    let r = compile_in(dir.path(), &path, &["-Wno-foo", "-Wno-bar=3"]);
    assert!(r.success && r.stderr.is_empty(), "{}", r.stderr);
}

#[test]
fn unknown_negation_is_noted_after_a_warning() {
    let (dir, path) = scratch("t.c", WARNS);
    let r = compile_in(dir.path(), &path, &["-Wno-foo", "-Wno-bar"]);
    assert!(r.success, "{}", r.stderr);
    let lines: Vec<&str> = r.stderr.lines().collect();
    assert!(lines[0].contains(": warning: "), "{}", r.stderr);
    // gcc lists them last first.
    assert_eq!(
        lines[1..],
        [note("-Wno-bar"), note("-Wno-foo")],
        "{}",
        r.stderr
    );

    // `-w` shows nothing, so there is nothing to explain.
    let r = compile_in(dir.path(), &path, &["-Wno-foo", "-w"]);
    assert!(r.success && r.stderr.is_empty(), "{}", r.stderr);

    // Under -Werror the note comes ahead of gcc's closing line (and the
    // driver's verdict on the operand follows both).
    let r = compile_in(dir.path(), &path, &["-Wno-foo", "-Werror"]);
    assert!(!r.success);
    let lines: Vec<&str> = r.stderr.lines().collect();
    assert_eq!(
        lines[1..3],
        [
            note("-Wno-foo").as_str(),
            "c17: all warnings being treated as errors"
        ],
        "{}",
        r.stderr
    );
}

/// What Debian trixie's dpkg-buildflags, meson's warning levels and the
/// common configure checks pass.
#[test]
fn common_build_flags_are_accepted() {
    let (dir, path) = scratch("t.c", CLEAN);
    for flags in [
        &["-Wformat", "-Werror=format-security"][..],
        &["-Werror=implicit-function-declaration", "-Wdate-time"],
        &["-Wall", "-Wextra", "-Wpedantic", "-W"],
        &["-Wformat=2", "-Wstrict-overflow=3", "-Wlarger-than=100"],
        &[
            "-Wnormalized=nfc",
            "-Wimplicit-fallthrough=3",
            "-Wshadow=local",
        ],
        &[
            "-Wno-unused-parameter",
            "-Wmissing-prototypes",
            "-Wstrict-prototypes",
        ],
        &["-Wl,-z,now", "-Wl,--as-needed"],
    ] {
        let r = compile_in(dir.path(), &path, flags);
        assert!(r.success && r.stderr.is_empty(), "{flags:?}: {}", r.stderr);
    }
}

#[test]
fn bad_option_values_are_errors() {
    let (dir, path) = scratch("t.c", CLEAN);
    for (flag, text) in [
        (
            "-Wformat=9",
            "argument to '-Wformat=' is not between 0 and 2",
        ),
        (
            "-Wformat=abc",
            "argument to '-Wformat=' should be a non-negative integer",
        ),
        (
            "-Wstrict-overflow=9",
            "argument to '-Wstrict-overflow=' is not between 0 and 5",
        ),
        (
            "-Wlarger-than=abc",
            "argument to '-Wlarger-than=' should be a non-negative integer \
             optionally followed by a size unit",
        ),
        ("-Wformat=", "missing argument to '-Wformat='"),
        (
            "-Wno-format=2",
            "unrecognized command-line option '-Wno-format=2'",
        ),
        ("-Wall=1", "unrecognized command-line option '-Wall=1'"),
    ] {
        let r = compile_in(dir.path(), &path, &[flag]);
        assert!(!r.success, "{flag}: {}", r.stderr);
        assert_eq!(r.stderr, format!("c17: error: {text}\n"), "{flag}");
    }

    let r = compile_in(dir.path(), &path, &["-Wnormalized=bad"]);
    assert!(!r.success);
    assert_eq!(
        r.stderr,
        "c17: error: argument 'bad' to '-Wnormalized' not recognized\n\
         c17: note: valid arguments to '-Wnormalized=' are: id nfc nfkc none\n"
    );
}

/// `-Wl,` belongs to the linker: never read as a warning name.
#[test]
fn linker_options_are_untouched() {
    let (dir, path) = scratch("t.c", CLEAN);
    let exe = dir.path().join("t");
    let r = run_c17(&[
        "-Wl,--no-such-c17-name-check",
        "-o",
        exe.to_str().unwrap(),
        path.to_str().unwrap(),
    ]);
    // The linker, not c17, rejects it.
    assert!(
        !r.stderr.contains("unrecognized command-line option"),
        "{}",
        r.stderr
    );
    let r = run_c17(&[
        "-Wl,-z,now",
        "-o",
        exe.to_str().unwrap(),
        path.to_str().unwrap(),
    ]);
    assert!(r.success, "{}", r.stderr);
}
