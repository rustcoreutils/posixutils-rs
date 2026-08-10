//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `-std=` as a compatibility shim, and host glibc detection (#C3).
//
// c17 implements one language: C17 (ISO/IEC 9899:2018) plus the GNU extensions
// it has always provided. `-std=` cannot select a different one. It survives
// only because build systems pass it unconditionally -- CPython's configure
// adds `-std=c11` whenever the compiler identifies as GCC-like, without ever
// probing whether it is accepted -- so rejecting it would break real builds.
//
// These tests pin that in both directions: every accepted spelling produces the
// same predefined macros, and a typo is still an error.
//

use crate::common::{compile_expect_ok, preprocess_text, run_c17};

/// Every `-std=` spelling c17 accepts, C17 and older alike.
const ACCEPTED: &[&str] = &[
    "c17",
    "c18",
    "gnu17",
    "gnu18",
    "iso9899:2017",
    "c89",
    "c90",
    "gnu89",
    "gnu90",
    "c99",
    "gnu99",
    "c11",
    "gnu11",
    "iso9899:1990",
    "iso9899:1999",
    "iso9899:2011",
];

/// Expand a single macro under a `-std=` spelling and return the replacement.
///
/// The marker keeps the interesting line findable in `-E` output, which also
/// carries line markers and blank lines.
fn expand_under(name: &str, std_flag: Option<&str>, macro_name: &str) -> String {
    let src = format!("MARKER {} MARKER\n", macro_name);
    let opts: Vec<&str> = std_flag.into_iter().collect();
    let run = preprocess_text(name, &src, &opts);
    assert!(
        run.success,
        "preprocessing failed for {:?}: {}",
        std_flag, run.stderr
    );

    let line = run
        .stdout
        .lines()
        .find(|l| l.starts_with("MARKER"))
        .unwrap_or_else(|| panic!("no marker line in output:\n{}", run.stdout));

    line.trim_start_matches("MARKER")
        .trim_end_matches("MARKER")
        .trim()
        .to_string()
}

/// An unexpanded macro name means the macro is not defined.
fn is_undefined(value: &str, macro_name: &str) -> bool {
    value == macro_name
}

#[test]
fn c17_version_macro_is_c17_whatever_std_says() {
    // C17 (ISO/IEC 9899:2018) is a defect-report revision of C11 that adds no
    // features, and it is the language POSIX.2024 binds the `c17` utility to
    // (line 87830). It is what we compile, so it is what we report -- asking
    // for an older revision does not change the answer, it only means the
    // request could not be honoured.
    assert_eq!(
        expand_under("std_default", None, "__STDC_VERSION__"),
        "201710L"
    );

    for spec in ACCEPTED {
        let got = expand_under(
            "std_version",
            Some(&format!("-std={spec}")),
            "__STDC_VERSION__",
        );
        assert_eq!(got, "201710L", "-std={spec}");
    }
}

#[test]
fn c17_never_defines_strict_ansi() {
    // There is no strict mode to advertise. Claiming one would tell system
    // headers to hide the extensions this compiler does provide, which is
    // exactly backwards.
    assert!(is_undefined(
        &expand_under("strict_default", None, "__STRICT_ANSI__"),
        "__STRICT_ANSI__"
    ));

    for spec in ACCEPTED {
        let got = expand_under(
            "strict_ansi",
            Some(&format!("-std={spec}")),
            "__STRICT_ANSI__",
        );
        assert!(
            is_undefined(&got, "__STRICT_ANSI__"),
            "-std={spec} must not define __STRICT_ANSI__, got {got}"
        );
    }
}

/// The unreserved and reserved OS macros for the host this test runs on.
///
/// `linux`/`__linux__` do not exist on Darwin, so asserting on them there
/// tested the build machine rather than the compiler. `unix`/`__unix__` are
/// common to both.
fn host_os_macros() -> (&'static [&'static str], &'static [&'static str]) {
    if cfg!(target_os = "linux") {
        (&["unix", "linux"], &["__unix__", "__linux__"])
    } else {
        // macOS and the BSDs predefine the unix pair but not the linux one.
        (&["unix"], &["__unix__"])
    }
}

#[test]
fn c17_always_predefines_the_unreserved_os_macros() {
    // Predefining names outside the implementation's reserved namespace is a
    // GNU extension, and this compiler is always in that mode -- so `unix` and
    // `linux` are there no matter how `-std=` is spelled, alongside the
    // reserved `__unix__` / `__linux__`.
    let (unreserved, reserved) = host_os_macros();

    for macro_name in unreserved.iter().chain(reserved) {
        assert_eq!(
            expand_under("os_default", None, macro_name),
            "1",
            "{macro_name} should be predefined by default"
        );
        for spec in ["c17", "gnu17", "c11", "c89"] {
            let got = expand_under("os_macros", Some(&format!("-std={spec}")), macro_name);
            assert_eq!(got, "1", "-std={spec} should predefine `{macro_name}`");
        }
    }
}

#[test]
fn c17_always_defines_the_unicode_macros() {
    // __STDC_UTF_16__ / __STDC_UTF_32__ describe char16_t / char32_t. Those
    // types exist here unconditionally, so the macros do too.
    for macro_name in ["__STDC_UTF_16__", "__STDC_UTF_32__"] {
        assert_eq!(expand_under("utf_default", None, macro_name), "1");
        for spec in ["c89", "c99", "c11", "c17"] {
            let got = expand_under("utf", Some(&format!("-std={spec}")), macro_name);
            assert_eq!(got, "1", "-std={spec} should define {macro_name}");
        }
    }
}

/// Repeating `-std=` is last-wins, as in gcc.
///
/// Every occurrence used to be forwarded to a clap `Option<String>`, so a
/// second one was a fatal "cannot be used multiple times". Build systems
/// accumulate `-std=` routinely -- one from configure's CFLAGS, another from a
/// makefile -- and gcc accepts it.
///
/// Now that every accepted spelling compiles the same language, last-wins is
/// observable through *validation* rather than through a macro value: a bad
/// spelling matters only if it is the one that survives.
#[test]
fn c17_repeated_std_takes_the_last() {
    let run = run_c17(&["-std=c99", "-std=c11", "--print-targets"]);
    assert!(
        run.success,
        "repeated -std= must be accepted: {}",
        run.stderr
    );

    // A typo in the last position is still an error...
    let run = run_c17(&["-std=c11", "-std=c42", "--print-targets"]);
    assert!(!run.success, "the last -std= should win and be rejected");
    assert!(
        run.stderr.contains("c42"),
        "diagnostic should name the surviving spelling, got: {}",
        run.stderr
    );

    // ...and one that is overridden is not, so this cannot pass by always
    // rejecting.
    let run = run_c17(&["-std=c42", "-std=c11", "--print-targets"]);
    assert!(
        run.success,
        "an overridden bad -std= should not be reported: {}",
        run.stderr
    );
}

#[test]
fn c17_rejects_an_unknown_std() {
    // Accepting and discarding a dialect request is exactly how the version
    // macro came to disagree with the binary's own name. A typo must not pass
    // silently just because the value would have been ignored anyway.
    for spec in ["c42", "gnu42", "c++17", "nonsense"] {
        let run = run_c17(&[&format!("-std={spec}"), "--print-targets"]);
        assert!(!run.success, "-std={spec} should be rejected");
        assert!(
            run.stderr.contains("-std=") && run.stderr.contains(spec),
            "diagnostic should name the bad spelling, got: {}",
            run.stderr
        );
    }
}

#[test]
fn c17_accepts_every_documented_std_spelling() {
    // The negative test above cannot pass vacuously: these must all work.
    for spec in ACCEPTED {
        compile_expect_ok(
            &format!("std_ok_{}", spec.replace(':', "_")),
            "int main(void) { return 0; }\n",
        );

        let run = run_c17(&[&format!("-std={spec}"), "--print-targets"]);
        assert!(
            run.success,
            "-std={spec} should be accepted, got: {}",
            run.stderr
        );
    }
}

#[test]
#[cfg(target_os = "linux")]
fn c17_glibc_minor_matches_the_host() {
    // Audit #C3: this was hardcoded to 17 ("conservative baseline") while the
    // host reported 2.39, so any code testing __GLIBC_PREREQ *before* including
    // a system header got the wrong answer. Read the same features.h the
    // compilation will use, rather than guessing.
    let Ok(features) = std::fs::read_to_string("/usr/include/features.h") else {
        // Not a glibc host; nothing to compare against.
        return;
    };

    let header_value = |name: &str| -> Option<String> {
        features.lines().find_map(|line| {
            let mut fields = line.split_whitespace();
            (fields.next() == Some("#define") && fields.next() == Some(name))
                .then(|| fields.next().map(str::to_string))
                .flatten()
        })
    };

    let (Some(want_major), Some(want_minor)) =
        (header_value("__GLIBC__"), header_value("__GLIBC_MINOR__"))
    else {
        return;
    };

    assert_eq!(
        expand_under("glibc_major", None, "__GLIBC__"),
        want_major,
        "__GLIBC__ should match features.h"
    );
    assert_eq!(
        expand_under("glibc_minor", None, "__GLIBC_MINOR__"),
        want_minor,
        "__GLIBC_MINOR__ should match features.h, not a hardcoded baseline"
    );
}
