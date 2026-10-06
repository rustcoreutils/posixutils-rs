//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `-Werror` through the driver, as a configure probe sees it: the exit
// status, gcc 13's text, and which command-line warnings it reaches.
//

use crate::common::run_c17;
use std::path::{Path, PathBuf};

/// A scratch directory holding `name` with `content`.
fn scratch(name: &str, content: &str) -> (plib::tmp::TempDir, PathBuf) {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_werror_")
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

/// gcc's `-Wincompatible-pointer-types` probe, the one configure scripts use.
const INCOMPATIBLE: &str = "char c; int *g(void){return &c;}\n";
const INCOMPATIBLE_MSG: &str =
    "returning 'char *' from a function with return type 'int *' incompatible pointer type";

#[test]
fn werror_fails_the_compile() {
    let (dir, path) = scratch("ip.c", INCOMPATIBLE);

    let plain = compile_in(dir.path(), &path, &[]);
    assert!(plain.success, "{}", plain.stderr);
    assert!(
        plain
            .stderr
            .contains(&format!("warning: {INCOMPATIBLE_MSG}\n")),
        "{}",
        plain.stderr
    );

    std::fs::remove_file(dir.path().join("out.o")).unwrap();
    let strict = compile_in(dir.path(), &path, &["-Werror"]);
    assert!(!strict.success, "{}", strict.stderr);
    let lines: Vec<&str> = strict.stderr.lines().collect();
    assert!(
        lines[0].ends_with(&format!(": error: {INCOMPATIBLE_MSG} [-Werror]")),
        "{}",
        strict.stderr
    );
    // gcc's cc1 closes with this line; the driver's own failure line follows.
    assert_eq!(
        lines[1], "c17: all warnings being treated as errors",
        "{}",
        strict.stderr
    );
    assert!(!dir.path().join("out.o").exists(), "an object was written");

    let named = compile_in(dir.path(), &path, &["-Werror=overflow"]);
    assert!(named.success, "{}", named.stderr);
}

#[test]
fn werror_with_w_is_silent() {
    let (dir, path) = scratch("ip.c", INCOMPATIBLE);
    let r = compile_in(dir.path(), &path, &["-Werror", "-w"]);
    assert!(r.success && r.stderr.is_empty(), "{}", r.stderr);
}

/// A header found through `-isystem` is not held to `-Werror`.
#[test]
fn werror_skips_system_headers() {
    let (dir, path) = scratch("main.c", "#include <werror_sys.h>\nint z;\n");
    let inc = dir.path().join("inc");
    std::fs::create_dir(&inc).unwrap();
    std::fs::write(inc.join("werror_sys.h"), INCOMPATIBLE).unwrap();
    let r = compile_in(
        dir.path(),
        &path,
        &["-Werror", "-isystem", inc.to_str().unwrap()],
    );
    assert!(r.success && r.stderr.is_empty(), "{}", r.stderr);

    // The same header through `-I` is the user's, and fails.
    let r = compile_in(dir.path(), &path, &["-Werror", "-I", inc.to_str().unwrap()]);
    assert!(!r.success, "{}", r.stderr);
    assert!(r.stderr.contains("[-Werror]"), "{}", r.stderr);
}

/// gcc refuses an option it does not know. c17 warns and goes on, which
/// is no answer to a probe that adds `-Werror` to find out: there it fails,
/// as gcc does. `-w` hides it, as it hides every warning.
#[test]
fn werror_fails_an_unrecognized_option() {
    let (dir, path) = scratch("a.c", "int a;\n");
    let r = compile_in(dir.path(), &path, &["-fc17-no-such-flag"]);
    assert!(r.success, "{}", r.stderr);
    assert_eq!(
        r.stderr,
        "c17: warning: unrecognized option, ignored: -fc17-no-such-flag\n"
    );

    let r = compile_in(dir.path(), &path, &["-w", "-fc17-no-such-flag"]);
    assert!(r.success && r.stderr.is_empty(), "{}", r.stderr);

    std::fs::remove_file(dir.path().join("out.o")).unwrap();
    for flags in [
        &["-Werror", "-fc17-no-such-flag"][..],
        &["-fc17-no-such-flag", "-Werror"],
    ] {
        let r = compile_in(dir.path(), &path, flags);
        assert!(!r.success, "{flags:?}: {}", r.stderr);
        assert_eq!(
            r.stderr, "c17: error: unrecognized option, ignored: -fc17-no-such-flag [-Werror]\n",
            "{flags:?}"
        );
        assert!(!dir.path().join("out.o").exists(), "an object was written");
    }

    let r = compile_in(
        dir.path(),
        &path,
        &["-Werror", "-Wno-error", "-gc17-no-such"],
    );
    assert!(r.success, "{}", r.stderr);
    assert!(
        r.stderr.contains("warning: unrecognized option"),
        "{}",
        r.stderr
    );

    let r = compile_in(dir.path(), &path, &["-Werror", "-gc17-no-such"]);
    assert!(!r.success, "{}", r.stderr);
}

/// gcc refuses `-o` with `-c` and several sources outright; c17 warns.
/// Under `-Werror` that leniency goes too.
#[test]
fn werror_fails_one_output_for_several_objects() {
    let (dir, a) = scratch("a.c", "int a;\n");
    let b = dir.path().join("b.c");
    std::fs::write(&b, "int b;\n").unwrap();
    let obj = dir.path().join("x.o");
    let args = |extra: &[&'static str]| {
        let mut v: Vec<String> = extra.iter().map(|s| s.to_string()).collect();
        for s in ["-c", "-o", obj.to_str().unwrap(), a.to_str().unwrap()] {
            v.push(s.to_string());
        }
        v.push(b.to_str().unwrap().to_string());
        v
    };
    let run = |v: Vec<String>| run_c17(&v.iter().map(String::as_str).collect::<Vec<_>>());

    let r = run(args(&[]));
    assert!(r.success, "{}", r.stderr);
    assert!(
        r.stderr.contains("c17: warning: -o applies"),
        "{}",
        r.stderr
    );

    let r = run(args(&["-Werror"]));
    assert!(!r.success, "{}", r.stderr);
    assert!(
        r.stderr.contains(
            "c17: error: -o applies only to the last source operand with -c (2) [-Werror]"
        ),
        "{}",
        r.stderr
    );
}

/// The command-line warnings gcc's driver gives stay warnings under
/// `-Werror`, as gcc's do: `-Werror` is a compiler option, not a driver one.
#[test]
fn werror_leaves_driver_warnings_alone() {
    let (dir, path) = scratch("a.c", "int a;\n");
    let unknown = dir.path().join("foo.xyz");
    std::fs::write(&unknown, "").unwrap();
    let r = compile_in(dir.path(), &path, &["-Werror", unknown.to_str().unwrap()]);
    assert!(r.success, "{}", r.stderr);
    assert!(
        r.stderr
            .contains("linker input file unused because linking not done"),
        "{}",
        r.stderr
    );

    // gcc says nothing about `-std=c89`; c17's note that it is not honoured
    // is not a reason to fail.
    let r = compile_in(dir.path(), &path, &["-Werror", "-std=c89"]);
    assert!(r.success, "{}", r.stderr);
    assert!(
        r.stderr.contains("warning: '-std=c89' ignored"),
        "{}",
        r.stderr
    );
}
