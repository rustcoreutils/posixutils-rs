//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `-f<name>` through the driver: a distribution's or a build system's
// default flags compile, under `-Werror` too; an option whose effect c17
// does not provide says so; one gcc does not know is refused, as gcc 13
// refuses it.
//

use crate::common::{preprocess_text, run_c17};
use std::path::{Path, PathBuf};

/// A scratch directory holding `name` with `content`.
fn scratch(name: &str, content: &str) -> (plib::tmp::TempDir, PathBuf) {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_fopt_")
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

/// Debian trixie's `dpkg-buildflags --get CFLAGS`, and Ubuntu 24.04's, on
/// amd64: elsewhere they have no `-fcf-protection`, which only x86-64 has.
const DEBIAN: &[&str] = &[
    "-g",
    "-O2",
    "-Werror=implicit-function-declaration",
    "-ffile-prefix-map=/build/pkg=.",
    "-fstack-protector-strong",
    "-fstack-clash-protection",
    "-Wformat",
    "-Werror=format-security",
    "-fcf-protection",
];
const UBUNTU: &[&str] = &[
    "-g",
    "-O2",
    "-fno-omit-frame-pointer",
    "-mno-omit-leaf-frame-pointer",
    "-ffile-prefix-map=/build/pkg=.",
    "-flto=auto",
    "-ffat-lto-objects",
    "-fstack-protector-strong",
    "-fstack-clash-protection",
    "-Wformat",
    "-Werror=format-security",
    "-fcf-protection",
];

/// A distribution's default flags compile under plain `-Werror` in silence.
/// They once failed: every `-f` c17 did not know was an "unrecognized
/// option" that `-Werror` made an error, and then `-fstack-protector-strong`
/// warned on every compile, which a build comparing a recipe's stderr (GNU
/// make's own test suite) reads as a failure. The stack-clash protection
/// says nothing for a function whose stack needs no probe.
#[test]
fn distribution_flags_compile_under_werror() {
    let (dir, path) = scratch("t.c", CLEAN);
    for flags in [DEBIAN, UBUNTU] {
        // The Ubuntu set's `-mno-omit-leaf-frame-pointer` is x86-64's and
        // aarch64's alike.
        let mut args = flags.to_vec();
        if !cfg!(all(target_arch = "x86_64", target_os = "linux")) {
            args.retain(|&f| f != "-fcf-protection");
        }
        args.push("-Werror");
        let r = compile_in(dir.path(), &path, &args);
        assert!(r.success, "{flags:?}: {}", r.stderr);
        assert_eq!(r.stderr, "", "{flags:?}");
    }
}

/// The stack protector's levels predefine gcc's macros, the last level
/// named winning and `-fno-stack-protector` withdrawing them all.
#[test]
fn stack_protector_macros() {
    let (dir, path) = scratch("m.c", "");
    let out = dir.path().join("m.i");
    for (flags, want) in [
        (&[][..], ""),
        (&["-fstack-protector"][..], "#define __SSP__ 1\n"),
        (
            &["-fstack-protector-strong"][..],
            "#define __SSP_STRONG__ 3\n",
        ),
        (&["-fstack-protector-all"][..], "#define __SSP_ALL__ 2\n"),
        (
            &["-fstack-protector-explicit"][..],
            "#define __SSP_EXPLICIT__ 4\n",
        ),
        (&["-fstack-protector-all", "-fno-stack-protector"][..], ""),
        (
            &["-fstack-protector-all", "-fstack-protector"][..],
            "#define __SSP__ 1\n",
        ),
        (
            &["-fno-stack-protector", "-fstack-protector-strong"][..],
            "#define __SSP_STRONG__ 3\n",
        ),
    ] {
        let mut args = flags.to_vec();
        args.extend([
            "-dM",
            "-E",
            "-o",
            out.to_str().unwrap(),
            path.to_str().unwrap(),
        ]);
        let r = run_c17(&args);
        assert!(r.success && r.stderr.is_empty(), "{flags:?}: {}", r.stderr);
        let defs = std::fs::read_to_string(&out).expect("read -dM output");
        let ssp: String = defs
            .lines()
            .filter(|l| l.contains("__SSP"))
            .map(|l| format!("{l}\n"))
            .collect();
        assert_eq!(ssp, want, "{flags:?}");
    }
}

/// What meson and cmake pass by default is taken in silence, under
/// `-Werror` too: diagnostics colour, LTO, position independence.
#[test]
fn build_system_defaults_are_silent() {
    let (dir, path) = scratch("t.c", CLEAN);
    for flags in [
        &["-fdiagnostics-color=always"][..],
        &["-fno-diagnostics-color"],
        &["-flto=auto", "-fno-fat-lto-objects"],
        &["-flto", "-fuse-linker-plugin", "-ffat-lto-objects"],
        &["-fPIC", "-fvisibility=hidden"],
        &["-fmessage-length=0", "-fdiagnostics-show-option"],
        &["-fno-diagnostics-show-caret", "-fdiagnostics-format=text"],
        &[
            "-fno-strict-aliasing",
            "-fwrapv",
            "-fno-semantic-interposition",
        ],
    ] {
        let mut args = flags.to_vec();
        if !cfg!(all(target_arch = "x86_64", target_os = "linux")) {
            args.retain(|&f| f != "-fcf-protection");
        }
        args.push("-Werror");
        let r = compile_in(dir.path(), &path, &args);
        assert!(r.success, "{flags:?}: {}", r.stderr);
        assert!(r.stderr.is_empty(), "{flags:?}: {}", r.stderr);
    }
}

/// An option gcc does not know is refused with gcc's text, `-w` or not, and
/// nothing is compiled.
#[test]
fn unknown_option_is_an_error() {
    let (dir, path) = scratch("t.c", CLEAN);
    for flags in [&["-ffoo"][..], &["-w", "-ffoo"], &["-ffoo", "-Wno-error"]] {
        let r = compile_in(dir.path(), &path, flags);
        assert!(!r.success, "{flags:?}: {}", r.stderr);
        assert_eq!(
            r.stderr, "c17: error: unrecognized command-line option '-ffoo'\n",
            "{flags:?}"
        );
        assert!(!dir.path().join("out.o").exists(), "an object was written");
    }

    // Every one is reported, in order, with the bad `-W` names.
    let r = compile_in(dir.path(), &path, &["-ffoo", "-Wbar", "-fbaz=1"]);
    assert!(!r.success);
    assert_eq!(
        r.stderr,
        "c17: error: unrecognized command-line option '-ffoo'\n\
         c17: error: unrecognized command-line option '-Wbar'\n\
         c17: error: unrecognized command-line option '-fbaz=1'\n"
    );

    // A known option with a value gcc refuses.
    let r = compile_in(dir.path(), &path, &["-fdiagnostics-color=sometimes"]);
    assert!(!r.success);
    assert_eq!(
        r.stderr,
        "c17: error: unrecognized argument in option '-fdiagnostics-color=sometimes'\n\
         c17: note: valid arguments to '-fdiagnostics-color=' are: always auto never\n"
    );
}

/// An option whose effect c17 does not provide is taken with a warning,
/// once however often it is given, and not at all when a later `-fno-`
/// withdraws it.
#[test]
fn unsupported_option_warns() {
    let (dir, path) = scratch("t.c", CLEAN);
    let r = compile_in(dir.path(), &path, &["-fcommon", "-fcommon", "-Werror"]);
    assert!(r.success, "{}", r.stderr);
    assert_eq!(
        r.stderr,
        "c17: warning: '-fcommon' is not supported; ignored\n"
    );

    let r = compile_in(dir.path(), &path, &["-ftrapv", "-fno-trapv", "-fcommon"]);
    assert!(r.success, "{}", r.stderr);
    assert_eq!(
        r.stderr,
        "c17: warning: '-fcommon' is not supported; ignored\n"
    );

    // `-Werror=` names the group, and then it is an error.
    let r = compile_in(
        dir.path(),
        &path,
        &["-ftrapv", "-Werror=c17-unsupported-option"],
    );
    assert!(!r.success, "{}", r.stderr);
    assert_eq!(
        r.stderr,
        "c17: error: '-ftrapv' is not supported; ignored [-Werror=c17-unsupported-option]\n"
    );
}

/// c17 instruments nothing, so a sanitizer request is refused: taking it
/// with a warning left a build believing it was checked, and a probe that
/// adds `-Werror` believing the compiler had one. A later `-fno-sanitize=`
/// withdraws a request, as gcc's does, and the modifiers that only tune a
/// sanitizer are taken in silence.
#[test]
fn sanitizers_are_refused() {
    let (dir, path) = scratch("t.c", CLEAN);
    for (flags, refused) in [
        (&["-fsanitize=address"][..], &["-fsanitize=address"][..]),
        (&["-fsanitize=undefined", "-w"], &["-fsanitize=undefined"]),
        (
            &[
                "-fsanitize=kernel-address",
                "-fsanitize-address-use-after-scope",
            ],
            &["-fsanitize=kernel-address"],
        ),
        (
            &["-fsanitize-coverage=trace-pc"],
            &["-fsanitize-coverage=trace-pc"],
        ),
        (
            &["-fsanitize=address,undefined", "-fno-sanitize=undefined"],
            &["-fsanitize=address,undefined"],
        ),
        (
            &["-fsanitize=address", "-fsanitize=thread"],
            &["-fsanitize=address", "-fsanitize=thread"],
        ),
    ] {
        let r = compile_in(dir.path(), &path, flags);
        assert!(!r.success, "{flags:?}: {}", r.stderr);
        let expected: String = refused
            .iter()
            .map(|o| format!("c17: error: '{o}' is not supported\n"))
            .collect();
        assert_eq!(r.stderr, expected, "{flags:?}");
    }
    for flags in [
        &["-fno-sanitize=all"][..],
        &["-fsanitize=address", "-fno-sanitize=address"],
        &["-fsanitize=address,undefined", "-fno-sanitize=all"],
        &[
            "-fsanitize-coverage=trace-pc",
            "-fno-sanitize-coverage=trace-pc",
        ],
        &[
            "-fsanitize-address-use-after-scope",
            "-fno-sanitize-recover",
        ],
        &["-fsanitize-recover=all", "-fsanitize-trap=undefined"],
        &[
            "-fsanitize-undefined-trap-on-error",
            "-fno-sanitize-recover=all",
        ],
    ] {
        let r = compile_in(dir.path(), &path, flags);
        assert!(r.success && r.stderr.is_empty(), "{flags:?}: {}", r.stderr);
    }
    for option in ["-fsanitize=", "-fno-sanitize=", "-fsanitize-coverage="] {
        let r = compile_in(dir.path(), &path, &[option]);
        assert!(!r.success, "{option}");
        assert_eq!(
            r.stderr,
            format!("c17: error: missing argument to '{option}'\n")
        );
    }
}

/// A function whose frame reaches the guard page, or that allocates on the
/// stack at run time, is one gcc's `-fstack-clash-protection` would probe
/// and c17 does not: that function, and only that one, gets the warning.
#[test]
fn stack_clash_protection_names_unprobed_functions() {
    let src = "void use(char *);\n\
               void small(void) { char b[64]; use(b); }\n\
               void big(void) { char b[70000]; use(b); }\n\
               void dynamic(int n) { char b[n]; use(b); }\n";
    let (dir, path) = scratch("s.c", src);
    let asm = dir.path().join("s.s");
    for target in [None, Some("--target=aarch64-unknown-linux-gnu")] {
        let mut args = vec!["-fstack-clash-protection", "-Werror", "-S", "-o"];
        args.push(asm.to_str().unwrap());
        args.push(path.to_str().unwrap());
        args.extend(target);
        let r = run_c17(&args);
        assert!(r.success, "{target:?}: {}", r.stderr);
        let p = path.display();
        assert_eq!(
            r.stderr,
            format!(
                "{p}:3:1: warning: '-fstack-clash-protection' is not supported; \
                 the stack of 'big' is not probed\n\
                 {p}:4:1: warning: '-fstack-clash-protection' is not supported; \
                 the stack of 'dynamic' is not probed\n"
            ),
            "{target:?}"
        );

        for quiet in ["-w", "-fno-stack-clash-protection"] {
            let mut args = args.clone();
            args.push(quiet);
            let r = run_c17(&args);
            assert!(r.success && r.stderr.is_empty(), "{quiet}: {}", r.stderr);
        }
    }
}

/// `--param stack-clash-protection-guard-size=` and `-probe-interval=` move
/// where gcc probes, and so which functions c17 names. On x86-64 a frame is
/// probed once it reaches both the guard and the interval (each a log2 of
/// bytes, 12 by default); aarch64 takes only a guard of 12 or 16, with the
/// interval equal to it, and probes past the guard less the caller's 1 KiB
/// -- refusing anything else, in gcc's words, with or without the option.
/// The parameters were accepted and changed nothing.
#[test]
fn stack_clash_protection_params_move_the_threshold() {
    let src = "void use(char *);\n\
               void f(void) { char b[5000]; use(b); }\n";
    let (dir, path) = scratch("p.c", src);
    let asm = dir.path().join("p.s");
    let run = |extra: &[&str]| {
        let mut args = vec!["-fstack-clash-protection", "-S", "-o"];
        args.push(asm.to_str().unwrap());
        args.push(path.to_str().unwrap());
        args.extend(extra);
        run_c17(&args)
    };
    let warns = |extra: &[&str]| {
        let r = run(extra);
        assert!(r.success, "{extra:?}: {}", r.stderr);
        r.stderr.contains("the stack of 'f' is not probed")
    };
    // The x86-64 rows name their target: on an aarch64 host the default
    // threshold is aarch64's.
    const X64: &str = "--target=x86_64-unknown-linux-gnu";
    const A64: &str = "--target=aarch64-unknown-linux-gnu";
    for (extra, want) in [
        (&[X64][..], true),
        (
            &[X64, "--param", "stack-clash-protection-guard-size=13"][..],
            false,
        ),
        (
            &[X64, "--param=stack-clash-protection-probe-interval=14"][..],
            false,
        ),
        (
            &[X64, "--param", "stack-clash-protection-guard-size=12"][..],
            true,
        ),
        (&[A64][..], false),
        (
            &[A64, "--param", "stack-clash-protection-guard-size=12"][..],
            true,
        ),
    ] {
        assert_eq!(warns(extra), want, "{extra:?}");
    }
    for (extra, error) in [
        (
            &[A64, "--param", "stack-clash-protection-guard-size=13"][..],
            "only values 12 (4 KB) and 16 (64 KB) are supported for guard size.  \
             Given value 13 (8 KB) is out of range",
        ),
        (
            &[A64, "--param", "stack-clash-protection-probe-interval=12"][..],
            "stack clash guard size '16' must be equal to probing interval '12'",
        ),
    ] {
        let r = run(extra);
        assert!(
            !r.success && r.stderr.contains(error),
            "{extra:?}: {}",
            r.stderr
        );
        let mut without: Vec<&str> = extra.to_vec();
        without.push("-fno-stack-clash-protection");
        let r = run(&without);
        assert!(
            !r.success && r.stderr.contains(error),
            "{without:?}: {}",
            r.stderr
        );
    }
}

/// `__GCC_HAVE_DWARF2_CFI_ASM` is defined exactly when gcc 13 defines it:
/// whenever call frame information is written -- by default on both
/// targets, with `-fexceptions` or `-g` -- and not when every unwind table
/// is off and there is no debug information, where aarch64 also needs
/// `-fno-unwind-tables`. c17 never defined it. `-fno-dwarf2-cfi-asm`, which
/// has gcc write the tables itself, is refused: c17 cannot.
#[test]
fn dwarf2_cfi_asm_macro_matches_gcc() {
    let (_dir, path) = scratch("cfi.c", "int x;\n");
    let defined = |extra: &[&str]| {
        let mut args = vec!["-E", "-dM", path.to_str().unwrap()];
        args.extend(extra);
        let r = run_c17(&args);
        assert!(r.success, "{extra:?}: {}", r.stderr);
        r.stdout.contains("#define __GCC_HAVE_DWARF2_CFI_ASM 1")
    };
    // The x86-64 rows name their target: aarch64 keeps synchronous tables
    // after -fno-asynchronous-unwind-tables, and Darwin always keeps CFI.
    const X64: &str = "--target=x86_64-unknown-linux-gnu";
    const A64: &str = "--target=aarch64-unknown-linux-gnu";
    const NO_ASYNC: &str = "-fno-asynchronous-unwind-tables";
    const NO_SYNC: &str = "-fno-unwind-tables";
    for (extra, want) in [
        (&[X64][..], true),
        (&[X64, NO_ASYNC][..], false),
        (&[X64, NO_ASYNC, NO_SYNC][..], false),
        (&[X64, NO_ASYNC, NO_SYNC, "-g"][..], true),
        (&[X64, NO_ASYNC, NO_SYNC, "-g0"][..], false),
        (&[X64, NO_ASYNC, NO_SYNC, "-fexceptions"][..], true),
        (&[A64][..], true),
        (&[A64, NO_ASYNC][..], true),
        (&[A64, NO_ASYNC, NO_SYNC][..], false),
        (&[X64, "-fdwarf2-cfi-asm"][..], true),
    ] {
        assert_eq!(defined(extra), want, "{extra:?}");
    }
    let r = run_c17(&["-c", "-fno-dwarf2-cfi-asm", path.to_str().unwrap()]);
    assert!(
        !r.success && r.stderr.contains("'-fno-dwarf2-cfi-asm' is not supported"),
        "{}",
        r.stderr
    );
}

/// `-fuse-ld=` reaches the link, as gcc's does: a linker that does not
/// exist fails it. Linux only: the host compiler driver is gcc there.
#[cfg(target_os = "linux")]
#[test]
fn use_ld_reaches_the_link() {
    let (dir, path) = scratch("t.c", CLEAN);
    let exe = dir.path().join("t");
    let run = |ld: &str| run_c17(&[ld, "-o", exe.to_str().unwrap(), path.to_str().unwrap()]);
    let r = run("-fuse-ld=bfd");
    assert!(r.success, "{}", r.stderr);
    assert!(r.stderr.is_empty(), "{}", r.stderr);

    // gcc runs `ld.mold`; where there is none, the link fails.
    let has_mold = std::process::Command::new("ld.mold")
        .arg("--version")
        .output()
        .is_ok_and(|o| o.status.success());
    if !has_mold {
        let r = run("-fuse-ld=mold");
        assert!(!r.success, "-fuse-ld=mold did not reach the link");
    }
}

/// Run c17 in `dir` with `args`, feeding it `stdin`.
fn run_in_with_stdin(dir: &Path, args: &[&str], stdin: &str) -> std::process::Output {
    use std::io::Write;
    let mut child = std::process::Command::new(plib::testing::get_binary_path("c17"))
        .args(args)
        .current_dir(dir)
        .env("LC_ALL", "C")
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .expect("spawn c17");
    child
        .stdin
        .take()
        .expect("stdin")
        .write_all(stdin.as_bytes())
        .expect("write stdin");
    child.wait_with_output().expect("wait for c17")
}

/// The names in `dir`, sorted.
fn listing(dir: &Path) -> Vec<String> {
    let mut names: Vec<String> = std::fs::read_dir(dir)
        .unwrap()
        .map(|e| e.unwrap().file_name().to_string_lossy().into_owned())
        .collect();
    names.sort();
    names
}

/// `-fsyntax-only` checks the translation unit and writes nothing: no
/// object, no assembly, no link. libxcrypt's `compute-symver-floor` asks the
/// compiler `cc -fsyntax-only -xc -` whether a preprocessor condition holds,
/// to choose the versions of its compatibility symbols. Refused, every
/// condition read as false, and libcrypt.so.1 exported `crypt@GLIBC_2.0`
/// instead of the `crypt@GLIBC_2.2.5` every x86-64 binary links against.
#[test]
fn syntax_only_checks_and_writes_nothing() {
    let (dir, path) = scratch("t.c", CLEAN);
    let out = run_in_with_stdin(dir.path(), &["-fsyntax-only", path.to_str().unwrap()], "");
    let err = String::from_utf8_lossy(&out.stderr);
    assert!(out.status.success(), "{err}");
    assert!(err.is_empty(), "{err}");
    assert_eq!(listing(dir.path()), ["t.c"]);

    let probe = |cond: &str| {
        let src = format!(
            "#include <limits.h>\n#if !({cond})\n#error nope\n#endif\n\
             int avoid_empty_translation_unit;\n"
        );
        let out = run_in_with_stdin(dir.path(), &["-fsyntax-only", "-xc", "-"], &src);
        (
            out.status.success(),
            String::from_utf8_lossy(&out.stderr).into_owned(),
        )
    };
    let (ok, err) = probe("ULONG_MAX >= UINT_MAX");
    assert!(ok, "{err}");
    let (ok, err) = probe("ULONG_MAX < UINT_MAX");
    assert!(!ok, "a false condition passed");
    assert!(err.contains("nope"), "{err}");

    // A syntax error and a constraint violation fail, as they do compiling.
    for bad in [
        "int f(void) { return }\n",
        "int f(void) { return undeclared; }\n",
    ] {
        let out = run_in_with_stdin(dir.path(), &["-fsyntax-only", "-xc", "-"], bad);
        assert!(!out.status.success(), "{bad}");
    }
    assert_eq!(listing(dir.path()), ["t.c"]);
}

/// `-fgnu89-inline` predefines `__GNUC_GNU_INLINE__` in place of
/// `__GNUC_STDC_INLINE__`, as gcc does, and the last of it and
/// `-fno-gnu89-inline` decides. glibc's <sys/cdefs.h> picks its
/// `__extern_inline` spelling from these.
#[test]
fn fgnu89_inline_selects_the_inline_predefine() {
    let src = "#ifdef __GNUC_GNU_INLINE__\ngnu __GNUC_GNU_INLINE__\n#endif\n\
               #ifdef __GNUC_STDC_INLINE__\nstdc __GNUC_STDC_INLINE__\n#endif\n";
    let (_dir, path) = scratch("inl.c", src);
    let path = path.to_string_lossy();
    for (flags, want) in [
        (&[][..], "stdc 1"),
        (&["-fgnu89-inline"][..], "gnu 1"),
        (&["-fno-gnu89-inline"][..], "stdc 1"),
        (&["-fgnu89-inline", "-fno-gnu89-inline"][..], "stdc 1"),
        (&["-fno-gnu89-inline", "-fgnu89-inline"][..], "gnu 1"),
    ] {
        let mut args = vec!["-E", "-P"];
        args.extend(flags);
        args.push(&path);
        let r = run_c17(&args);
        assert!(r.success, "{flags:?}: {}", r.stderr);
        assert_eq!(r.stdout.trim(), want, "{flags:?}");
    }
}

/// The `-f` options gcc takes for other languages are taken for C with
/// gcc's warning, exit status 0: gcc-14's configure probes `-fno-rtti`
/// with `CC` and puts the answer into its C++ flags. `-w` silences the
/// warning and `-Werror` makes it an error, as with gcc.
#[test]
fn f_options_other_language_options_warn_as_gcc() {
    let (dir, path) = scratch("t.c", CLEAN);
    for (opt, langs) in [
        ("-fno-rtti", "C++/D/ObjC++"),
        ("-frtti", "C++/D/ObjC++"),
        ("-fno-threadsafe-statics", "C++/ObjC++"),
        ("-fvisibility-inlines-hidden", "C++/ObjC++"),
        ("-fno-enforce-eh-specs", "C++/ObjC++"),
        ("-fno-use-cxa-atexit", "C++/ObjC++"),
        ("-fno-objc-exceptions", "ObjC/ObjC++"),
    ] {
        let r = compile_in(dir.path(), &path, &[opt]);
        assert!(r.success, "{opt}: {}", r.stderr);
        assert_eq!(
            r.stderr,
            format!(
                "c17: warning: command-line option '{opt}' is valid for {langs} but not for C\n"
            ),
            "{opt}"
        );
        let r = compile_in(dir.path(), &path, &["-w", opt]);
        assert!(r.success && r.stderr.is_empty(), "-w {opt}: {}", r.stderr);
        let r = compile_in(dir.path(), &path, &["-Werror", opt]);
        assert!(!r.success, "-Werror {opt} passed");
        assert!(r.stderr.contains("[-Werror]"), "{}", r.stderr);
    }
    // gcc 13 takes `-fcheck-new` for C in silence.
    let r = compile_in(dir.path(), &path, &["-fcheck-new", "-fno-check-new"]);
    assert!(r.success && r.stderr.is_empty(), "{}", r.stderr);
}

/// `--param` names one of gcc 13's tuning parameters, which are taken in
/// silence; any other name, or a value gcc would refuse, is refused in gcc's
/// words -- `allow-store-data-races` among them, an `-f` option since gcc 10.
#[test]
fn param_names_are_gccs() {
    let (dir, path) = scratch("t.c", CLEAN);
    for flags in [
        &["--param=max-inline-insns-single=10"][..],
        &["--param", "max-inline-insns-single=10"],
        &["--param=openacc-kernels=decompose"],
        &["--param=align-threshold=65536", "--param=asan-stack=0"],
        &["--param", "ssp-buffer-size=4", "-Werror"],
    ] {
        let r = compile_in(dir.path(), &path, flags);
        assert!(r.success && r.stderr.is_empty(), "{flags:?}: {}", r.stderr);
    }
    for (flags, text) in [
        (
            &["--param=allow-store-data-races=0"][..],
            "error: unrecognized command-line option '--param=allow-store-data-races=0'",
        ),
        (
            &["--param", "foo=1"],
            "error: unrecognized command-line option '--param=foo=1'",
        ),
        (
            &["--param=max-inline-insns-single"],
            "error: unrecognized command-line option '--param=max-inline-insns-single'",
        ),
        (
            &["--param=max-inline-insns-single="],
            "error: missing argument to '--param=max-inline-insns-single='",
        ),
        (
            &["--param=max-inline-insns-single=-1"],
            "error: argument to '--param=max-inline-insns-single=' should be a non-negative integer",
        ),
        (
            &["--param=align-threshold=0"],
            "error: argument to '--param=align-threshold=' is not between 1 and 65536",
        ),
        (
            &["--param=openacc-kernels=foo"],
            "error: unrecognized argument in option '--param=openacc-kernels=foo'\n\
             c17: note: valid arguments to '--param=openacc-kernels=' are: decompose parloops",
        ),
    ] {
        let r = compile_in(dir.path(), &path, flags);
        assert!(!r.success, "{flags:?}");
        assert_eq!(r.stderr, format!("c17: {text}\n"), "{flags:?}");
    }
}

/// `-fexceptions` predefines gcc's `__EXCEPTIONS`, for C and for a `.S`
/// alike, the last of `-f[no-]exceptions` deciding; and it is taken in
/// silence, under `-Werror` too, as an option c17 implements.
#[test]
fn fexceptions_defines_exceptions_macro() {
    let (dir, path) = scratch("m.c", "");
    let out = dir.path().join("m.i");
    let src = "#ifdef __EXCEPTIONS\neh __EXCEPTIONS\n#else\nno eh\n#endif\n";
    for (flags, defined) in [
        (&[][..], false),
        (&["-fexceptions"][..], true),
        (&["-fexceptions", "-fno-exceptions"][..], false),
        (&["-fno-exceptions", "-fexceptions"][..], true),
    ] {
        let mut args = flags.to_vec();
        args.extend([
            "-Werror",
            "-dM",
            "-E",
            "-o",
            out.to_str().unwrap(),
            path.to_str().unwrap(),
        ]);
        let r = run_c17(&args);
        assert!(r.success && r.stderr.is_empty(), "{flags:?}: {}", r.stderr);
        let defs = std::fs::read_to_string(&out).expect("read -dM output");
        let eh: String = defs
            .lines()
            .filter(|l| l.contains("EXCEPTIONS"))
            .map(|l| format!("{l}\n"))
            .collect();
        let want = if defined {
            "#define __EXCEPTIONS 1\n"
        } else {
            ""
        };
        assert_eq!(eh, want, "{flags:?}");
        let mut asm_args = flags.to_vec();
        asm_args.extend(["-x", "assembler-with-cpp"]);
        let r = preprocess_text("eh", src, &asm_args);
        assert!(r.success, "{flags:?} .S: {}", r.stderr);
        let want = if defined { "eh 1" } else { "no eh" };
        assert!(r.stdout.contains(want), "{flags:?} .S: {}", r.stdout);
    }
}

/// `-frandom-seed=` and `-fmerge-all-constants` are taken in silence and
/// change nothing c17 writes. c17 names nothing at random -- gcc reads the
/// seed for names it makes up, and libbacktrace builds each object with
/// `-frandom-seed=$@` so they come out the same every time, which c17's
/// already do -- and merging constants is a permission c17 never uses:
/// glibc compiles everything with `-fmerge-all-constants`.
#[test]
fn f_options_random_seed_and_merge_all_constants_change_nothing() {
    let src = "static const int t[] = {1, 2, 3};\nconst char *s(void) { return \"x\"; }\n\
               int f(int i) { return t[i]; }\n";
    // Less the lines naming the scratch file, which differs per run.
    let asm = |flags: &[&str]| -> String {
        crate::common::asm_for_at("c17_seed_", src, flags)
            .lines()
            .filter(|l| !l.contains("c17_seed_"))
            .collect::<Vec<_>>()
            .join("\n")
    };
    let plain = asm(&["-O2"]);
    for flags in [
        &["-frandom-seed=x.lo"][..],
        &["-frandom-seed=0x1234"],
        &["-fmerge-all-constants"],
        &["-fmerge-all-constants", "-frandom-seed=y", "-Werror"],
    ] {
        let mut args = flags.to_vec();
        args.push("-O2");
        assert_eq!(asm(&args), plain, "{flags:?}");
    }
    let (dir, path) = scratch("t.c", CLEAN);
    let r = compile_in(
        dir.path(),
        &path,
        &["-frandom-seed=x", "-fmerge-all-constants"],
    );
    assert!(r.success && r.stderr.is_empty(), "{}", r.stderr);
    // `-fexcess-precision=16` rounds each `_Float16` operation to
    // `_Float16`, which is what c17 does; `standard` and `fast` would keep
    // the intermediate in `float` (gcc's code for them is the same), which
    // c17 does not. `float` and `double` have no excess precision on either
    // target under any of them.
    let r = compile_in(dir.path(), &path, &["-fexcess-precision=16", "-Werror"]);
    assert!(r.success && r.stderr.is_empty(), "{}", r.stderr);
    for style in ["standard", "fast"] {
        let flag = format!("-fexcess-precision={style}");
        let r = compile_in(dir.path(), &path, &[&flag]);
        assert!(r.success, "{flag}: {}", r.stderr);
        assert!(
            r.stderr
                .contains(&format!("'{flag}' is not supported; ignored")),
            "{flag}: {}",
            r.stderr
        );
    }
    let r = compile_in(dir.path(), &path, &["-fexcess-precision=double"]);
    assert!(!r.success);
    assert_eq!(
        r.stderr,
        "c17: error: unknown excess precision style 'double'\n\
         c17: note: valid arguments to '-fexcess-precision=' are: 16 fast standard\n"
    );
    // gcc's text for a seed left out.
    let r = compile_in(dir.path(), &path, &["-frandom-seed="]);
    assert!(!r.success);
    assert_eq!(
        r.stderr,
        "c17: error: missing argument to '-frandom-seed='\n"
    );
}
