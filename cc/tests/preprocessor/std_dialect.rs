//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `-std=` dialect selection (audit #P1/#X2) and host glibc detection (#C3).
//
// Before this, `-std=` was accepted and thrown away, and `__STDC_VERSION__`
// was hardcoded to C11's 201112L no matter what -- so a binary named `c17`
// took the wrong branch of `#if __STDC_VERSION__ >= 201710L`.
//

use crate::common::{compile_expect_ok, preprocess_text, run_c17};

/// Expand a single macro under a dialect and return the replacement text.
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
fn c17_std_selects_the_version_macro() {
    // Values match ISO: C89 predates the macro, C99/C11/C17 as published.
    // C17 (ISO/IEC 9899:2018) is a defect-report revision of C11 that adds no
    // features, so 201710L is the only externally visible difference.
    for (flag, expected) in [
        ("-std=c89", None),
        ("-std=gnu89", None),
        ("-std=c90", None),
        ("-std=c99", Some("199901L")),
        ("-std=gnu99", Some("199901L")),
        ("-std=c11", Some("201112L")),
        ("-std=gnu11", Some("201112L")),
        ("-std=c17", Some("201710L")),
        ("-std=c18", Some("201710L")),
        ("-std=gnu17", Some("201710L")),
        ("-std=iso9899:1999", Some("199901L")),
        ("-std=iso9899:2017", Some("201710L")),
    ] {
        let got = expand_under("std_version", Some(flag), "__STDC_VERSION__");
        match expected {
            Some(want) => assert_eq!(got, want, "{flag}"),
            None => assert!(
                is_undefined(&got, "__STDC_VERSION__"),
                "{flag} should leave __STDC_VERSION__ undefined, got {got}"
            ),
        }
    }
}

#[test]
fn c17_default_dialect_is_c17() {
    // POSIX.2024 defines the `c17` utility's language as "section 6 of the ISO
    // C standard" (87830), which for POSIX.2024 is ISO/IEC 9899:2018. So C17
    // must be the default, not something you opt into.
    let got = expand_under("std_default", None, "__STDC_VERSION__");
    assert_eq!(got, "201710L");
}

#[test]
fn c17_strict_dialects_define_strict_ansi() {
    for flag in ["-std=c89", "-std=c99", "-std=c11", "-std=c17"] {
        let got = expand_under("strict_ansi", Some(flag), "__STRICT_ANSI__");
        assert_eq!(got, "1", "{flag} should define __STRICT_ANSI__");
    }

    for flag in ["-std=gnu89", "-std=gnu99", "-std=gnu11", "-std=gnu17"] {
        let got = expand_under("strict_ansi_gnu", Some(flag), "__STRICT_ANSI__");
        assert!(
            is_undefined(&got, "__STRICT_ANSI__"),
            "{flag} must not define __STRICT_ANSI__, got {got}"
        );
    }

    // The default is a GNU dialect.
    let got = expand_under("strict_ansi_default", None, "__STRICT_ANSI__");
    assert!(is_undefined(&got, "__STRICT_ANSI__"));
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
fn c17_strict_dialects_drop_unreserved_macros() {
    // A strict dialect may only predefine names in the implementation's
    // reserved namespace, so `unix`/`linux` go away while `__unix__` stays.
    // Otherwise a conforming program may not use them as identifiers.
    let (unreserved, reserved) = host_os_macros();

    for macro_name in unreserved {
        let strict = expand_under("unreserved_strict", Some("-std=c17"), macro_name);
        assert!(
            is_undefined(&strict, macro_name),
            "-std=c17 must not predefine `{macro_name}`, got {strict}"
        );
    }

    // The reserved spellings survive in every dialect.
    for macro_name in reserved {
        let strict = expand_under("reserved_strict", Some("-std=c17"), macro_name);
        assert_eq!(strict, "1", "{macro_name} should survive -std=c17");
    }
}

#[test]
fn c17_gnu_dialects_keep_unreserved_macros() {
    // Pinned in both directions so `c17_strict_dialects_drop_unreserved_macros`
    // cannot pass by simply never defining these.
    let (unreserved, _) = host_os_macros();
    for macro_name in unreserved {
        let gnu = expand_under("unreserved_gnu", Some("-std=gnu17"), macro_name);
        assert_eq!(gnu, "1", "-std=gnu17 should predefine `{macro_name}`");
    }
}

#[test]
fn c17_c11_only_macros_are_absent_before_c11() {
    // __STDC_UTF_16__/__STDC_UTF_32__ assert a property of char16_t/char32_t,
    // types C11 introduced. Claiming them under -std=c99 would be false.
    for macro_name in ["__STDC_UTF_16__", "__STDC_UTF_32__"] {
        for flag in ["-std=c89", "-std=c99"] {
            let got = expand_under("utf_pre_c11", Some(flag), macro_name);
            assert!(
                is_undefined(&got, macro_name),
                "{flag} must not define {macro_name}, got {got}"
            );
        }

        for flag in ["-std=c11", "-std=c17"] {
            let got = expand_under("utf_c11", Some(flag), macro_name);
            assert_eq!(got, "1", "{flag} should define {macro_name}");
        }
    }
}

/// Repeating `-std=` is last-wins, as in gcc.
///
/// Every occurrence used to be forwarded to a clap `Option<String>`, so a
/// second one was a fatal "cannot be used multiple times". Build systems
/// accumulate `-std=` routinely -- one from configure's CFLAGS, another from a
/// makefile -- and gcc accepts it. `-O` in the same rewriter already did this.
#[test]
fn c17_repeated_std_takes_the_last() {
    assert_eq!(
        expand_under("std_repeat", None, "__STDC_VERSION__"),
        "201710L"
    );

    let run = preprocess_text(
        "std_repeat_two",
        "MARKER __STDC_VERSION__ MARKER\n",
        &["-std=c99", "-std=c11"],
    );
    assert!(
        run.success,
        "repeated -std= must be accepted: {}",
        run.stderr
    );
    let line = run
        .stdout
        .lines()
        .find(|l| l.starts_with("MARKER"))
        .unwrap_or_else(|| panic!("no marker line:\n{}", run.stdout));
    assert!(
        line.contains("201112L"),
        "the last -std= should win, got: {line}"
    );

    // And the other way round, so this cannot pass by always picking one.
    let run = preprocess_text(
        "std_repeat_rev",
        "MARKER __STDC_VERSION__ MARKER\n",
        &["-std=c11", "-std=c99"],
    );
    assert!(run.success);
    let line = run
        .stdout
        .lines()
        .find(|l| l.starts_with("MARKER"))
        .unwrap();
    assert!(
        line.contains("199901L"),
        "the last -std= should win, got: {line}"
    );
}

#[test]
fn c17_rejects_an_unknown_std() {
    // Accepting and discarding a dialect request is exactly how the version
    // macro came to disagree with the binary's own name.
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
    for spec in [
        "c89",
        "c90",
        "gnu89",
        "gnu90",
        "c99",
        "gnu99",
        "c11",
        "gnu11",
        "c17",
        "c18",
        "gnu17",
        "gnu18",
        "iso9899:1990",
        "iso9899:1999",
        "iso9899:2011",
        "iso9899:2017",
    ] {
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
