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

/// The `-std=` spellings that name C17 itself. Exhaustive.
const C17_SPELLINGS: &[&str] = &[
    "c17",
    "c18",
    "gnu17",
    "gnu18",
    "iso9899:2017",
    "iso9899:2018",
];

/// The `-std=` spellings naming an older revision. Exhaustive.
///
/// Together with `C17_SPELLINGS` this is every spelling `classify_std`
/// accepts, so "every accepted spelling behaves identically" is a claim these
/// tests actually check rather than sample.
const OLDER_SPELLINGS: &[&str] = &[
    "c89",
    "c90",
    "c9x",
    "c99",
    "c1x",
    "c11",
    "gnu89",
    "gnu90",
    "gnu9x",
    "gnu99",
    "gnu1x",
    "gnu11",
    "iso9899:1990",
    "iso9899:199409",
    "iso9899:199x",
    "iso9899:1999",
    "iso9899:2011",
];

/// Every `-std=` spelling c17 accepts.
fn accepted() -> Vec<&'static str> {
    C17_SPELLINGS
        .iter()
        .chain(OLDER_SPELLINGS)
        .copied()
        .collect()
}

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

    for spec in accepted() {
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

    for spec in accepted() {
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
    for spec in accepted() {
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

/// Asking for an older revision is accepted but not honoured, and c17 says so.
///
/// Silence would be the same failure the audit filed against `-std=` in the
/// first place: a flag taken and thrown away, leaving the user to believe a
/// request was met. The compiler cannot honour it -- there is one language --
/// so the least it can do is not pretend otherwise.
#[test]
fn c17_warns_that_an_older_std_was_not_honoured() {
    for spec in OLDER_SPELLINGS {
        let run = run_c17(&[&format!("-std={spec}"), "--print-targets"]);
        assert!(run.success, "-std={spec} must still be accepted");
        assert!(
            run.stderr.contains("warning") && run.stderr.contains(spec),
            "-std={spec} should warn and name the spelling, got: {}",
            run.stderr
        );
    }
}

/// A C17 spelling asks for what we compile, so there is nothing to report.
#[test]
fn c17_does_not_warn_for_a_c17_std() {
    for spec in C17_SPELLINGS {
        let run = run_c17(&[&format!("-std={spec}"), "--print-targets"]);
        assert!(run.success, "-std={spec} must be accepted");
        assert!(
            !run.stderr.contains("warning"),
            "-std={spec} matches what c17 compiles and must be silent, got: {}",
            run.stderr
        );
    }

    // Nor when no dialect was requested at all.
    let run = run_c17(&["--print-targets"]);
    assert!(!run.stderr.contains("warning"), "got: {}", run.stderr);
}

/// The warning is suppressible, because build systems emit `-std=` per
/// translation unit: CPython alone would print it some 500 times.
#[test]
fn c17_dialect_warning_can_be_silenced() {
    for silencer in ["-w", "-Wno-c17-dialect"] {
        let run = run_c17(&["-std=c11", silencer, "--print-targets"]);
        assert!(run.success, "{silencer} should be accepted: {}", run.stderr);
        assert!(
            !run.stderr.contains("warning"),
            "{silencer} should silence the dialect warning, got: {}",
            run.stderr
        );
    }

    // An unrelated -Wno- must not silence it, or the flag name means nothing.
    let run = run_c17(&["-std=c11", "-Wno-unused", "--print-targets"]);
    assert!(
        run.stderr.contains("warning"),
        "-Wno-unused should leave the dialect warning alone, got: {}",
        run.stderr
    );
}

/// `-w` is a warning switch, not a dialect one: it must not hide a typo.
#[test]
fn c17_suppression_does_not_hide_an_unknown_std() {
    for silencer in ["-w", "-Wno-c17-dialect"] {
        let run = run_c17(&["-std=c42", silencer, "--print-targets"]);
        assert!(
            !run.success,
            "{silencer} must not turn an unknown -std= into a pass"
        );
        assert!(run.stderr.contains("c42"), "got: {}", run.stderr);
    }
}

// ---------------------------------------------------------------------------
// Capability macros must not outrun the capability (#C163).
//
// A predefined feature macro is a promise to the preprocessor, and guarded
// code takes it at its word: `#ifdef __GCC_HAVE_SYNC_COMPARE_AND_SWAP_4` opens
// a branch that calls `__sync_bool_compare_and_swap`, and the `#else` beside
// it is a portable fallback that would have compiled. Defining the macro
// without the builtin does not make the feature available -- it makes the
// fallback unreachable, so the file fails where it would otherwise have built.
//
// These tests pin the *relationship*, not the absence: if the builtins are
// ever implemented, the sync assertions below are meant to fail so the macro
// gets restored along with them.
// ---------------------------------------------------------------------------

/// The `__sync_*` family c17 does not implement, and the macros that guard it.
const SYNC_CAS_MACROS: &[&str] = &[
    "__GCC_HAVE_SYNC_COMPARE_AND_SWAP_1",
    "__GCC_HAVE_SYNC_COMPARE_AND_SWAP_2",
    "__GCC_HAVE_SYNC_COMPARE_AND_SWAP_4",
    "__GCC_HAVE_SYNC_COMPARE_AND_SWAP_8",
];

#[test]
fn c17_does_not_advertise_the_sync_builtins_it_lacks() {
    for macro_name in SYNC_CAS_MACROS {
        let got = expand_under("sync_cas", None, macro_name);
        assert!(
            is_undefined(&got, macro_name),
            "{macro_name} must not be defined while `__sync_*` is unimplemented, got {got}"
        );
    }
}

/// The other half of the promise: the builtin really is absent. If this starts
/// failing, implement-and-restore is the fix -- not deleting the test.
#[test]
fn c17_sync_builtins_are_genuinely_absent() {
    // If this stops failing to compile, the feature arrived: restore
    // __GCC_HAVE_SYNC_COMPARE_AND_SWAP_* alongside it rather than deleting
    // this test.
    crate::common::compile_expect_error(
        "sync_absent",
        "int main(void){ int x = 1;\n\
         return __sync_bool_compare_and_swap(&x, 1, 2) ? 0 : 1; }\n",
        "__sync_bool_compare_and_swap",
    );
}

/// A program guarded on the macro must reach its portable `#else` and run.
#[test]
fn c17_sync_guarded_code_takes_the_portable_branch() {
    let src = "#ifdef __GCC_HAVE_SYNC_COMPARE_AND_SWAP_4\n\
               int main(void){ int x = 1;\n\
               return __sync_bool_compare_and_swap(&x, 1, 2) ? 0 : 1; }\n\
               #else\n\
               int main(void){ return 0; }\n\
               #endif\n";
    assert_eq!(
        crate::common::compile_and_run("sync_guarded", src, &[]),
        0,
        "guarded code must fall through to the portable branch and run"
    );
}

/// aarch64 advertised NEON with no `<arm_neon.h>` behind it. Probed through
/// `--target`, so the assertion holds from any host.
#[test]
fn c17_does_not_advertise_neon_without_the_header() {
    for macro_name in ["__ARM_NEON", "__ARM_NEON__"] {
        let got = expand_under("neon", Some("--target=aarch64-linux-gnu"), macro_name);
        assert!(
            is_undefined(&got, macro_name),
            "{macro_name} must not be defined while <arm_neon.h> is not shipped, got {got}"
        );
    }

    // Same target, same reason, other family.
    for macro_name in SYNC_CAS_MACROS {
        let got = expand_under("neon_sync", Some("--target=aarch64-linux-gnu"), macro_name);
        assert!(
            is_undefined(&got, macro_name),
            "{macro_name} must not be defined on aarch64 either, got {got}"
        );
    }
}

/// Facts about the hardware, not promises about headers: these stay.
#[test]
fn c17_keeps_the_capability_macros_it_can_back() {
    // C11 atomics are complete, so the lock-free advertisement is honest.
    for macro_name in [
        "__GCC_ATOMIC_INT_LOCK_FREE",
        "__GCC_ATOMIC_POINTER_LOCK_FREE",
    ] {
        assert_eq!(expand_under("atomic_lockfree", None, macro_name), "2");
    }

    // __SSE2__ is knowingly still advertised without intrinsic headers -- the
    // drop is deferred because it is the one change that could flip a
    // configure decision. Pinned so a later sweep cannot take it silently;
    // see "GNU extensions" in cc/doc/TODO.md.
    #[cfg(target_arch = "x86_64")]
    for macro_name in ["__SSE__", "__SSE2__", "__MMX__"] {
        assert_eq!(
            expand_under("sse_baseline", None, macro_name),
            "1",
            "{macro_name} is deliberately kept; changing it is a decision, not a sweep"
        );
    }
}
