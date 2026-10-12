//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `-Wstack-usage=<size>`, checked against gcc 13: a function whose stack use,
// as `-fstack-usage` measures it, is larger than the limit draws "stack usage
// is N bytes", or "stack usage might be N bytes" when outgoing arguments
// move the stack pointer; one that allocates at run time draws "stack usage
// might be unbounded" whatever the limit. Each is reported at the function's
// name, and none needs `-fstack-usage`.
//

use crate::test_compile::{compile, compile_accepted, compile_rejected_with};

const SRC: &str = "\
int leaf(int a) { return a + 1; }
static int locals(int n) { volatile char buf[100]; buf[n] = 1; return buf[0]; }
extern int g(int, int, int, int, int, int, int, int, int);
int stacked(int a) { return g(a, 1, 2, 3, 4, 5, 6, 7, 8) + 1; }
int vla(int n) { char p[n]; p[0] = 1; return p[n - 1]; }
int main(void) { return locals(3); }
";

const X86_64: &str = "--target=x86_64-unknown-linux-gnu";
const AARCH64: &str = "--target=aarch64-unknown-linux-gnu";

/// The stack-usage warnings in `stderr`, without the tag.
fn warnings(stderr: &str) -> Vec<&str> {
    stderr
        .lines()
        .filter(|l| l.contains("stack usage"))
        .map(|l| l.trim_end_matches(" [-Wstack-usage=]"))
        .collect()
}

/// Each function over the limit is reported once, at its name, in gcc's
/// words; the frames are those `-fstack-usage` gives
/// (`tests/driver/stack_usage.rs`).
#[test]
fn stack_usage_warns_over_the_limit() {
    let c = compile_accepted("su", SRC, &[X86_64, "-O0", "-Wstack-usage=100"]);
    assert_eq!(
        warnings(&c),
        [
            "su.c:2:12: warning: stack usage is 128 bytes",
            "su.c:5:5: warning: stack usage might be unbounded",
        ],
        "{c}"
    );
    assert!(
        c.contains("su.c:2:12: warning: stack usage is 128 bytes [-Wstack-usage=]\n"),
        "{c}"
    );
    let c = compile_accepted("su", SRC, &[X86_64, "-O0", "-Wstack-usage=40"]);
    assert_eq!(
        warnings(&c),
        [
            "su.c:2:12: warning: stack usage is 128 bytes",
            "su.c:4:5: warning: stack usage might be 64 bytes",
            "su.c:5:5: warning: stack usage might be unbounded",
        ],
        "{c}"
    );
    let c = compile_accepted("su", SRC, &[AARCH64, "-O0", "-Wstack-usage=0"]);
    assert_eq!(
        warnings(&c),
        [
            "su.c:1:5: warning: stack usage is 16 bytes",
            "su.c:2:12: warning: stack usage is 128 bytes",
            "su.c:4:5: warning: stack usage might be 48 bytes",
            "su.c:5:5: warning: stack usage might be unbounded",
            "su.c:6:5: warning: stack usage is 16 bytes",
        ],
        "{c}"
    );
    // A limit at the usage itself is not exceeded.
    let c = compile_accepted("su", SRC, &[X86_64, "-O0", "-Wstack-usage=128"]);
    assert_eq!(
        warnings(&c),
        ["su.c:5:5: warning: stack usage might be unbounded"],
        "{c}"
    );
}

/// Without the option, after `-Wno-stack-usage`, under `-w`, or with a limit
/// too large to mean one, nothing is said; the last limit given is the one
/// used, and a unit counts as gcc's do.
#[test]
fn stack_usage_options_fold() {
    for flags in [
        &[][..],
        &["-Wstack-usage=10", "-Wno-stack-usage"],
        &["-Wstack-usage=10", "-w"],
        &["-Wstack-usage=99999999999999999999"],
        &["-Wstack-usage=9223372036854775807"],
    ] {
        let mut all = vec![X86_64, "-O0"];
        all.extend_from_slice(flags);
        let c = compile_accepted("su_quiet", SRC, &all);
        assert!(warnings(&c).is_empty(), "{flags:?}:\n{c}");
    }
    // The largest limit gcc honours still reports the unbounded.
    let c = compile_accepted(
        "su",
        SRC,
        &[X86_64, "-O0", "-Wstack-usage=9223372036854775806"],
    );
    assert_eq!(warnings(&c).len(), 1, "{c}");
    let c = compile_accepted(
        "su",
        SRC,
        &[X86_64, "-O0", "-Wno-stack-usage", "-Wstack-usage=1KiB"],
    );
    assert_eq!(warnings(&c).len(), 1, "{c}");
    let c = compile_accepted(
        "su",
        SRC,
        &[X86_64, "-O0", "-Wstack-usage=10", "-Wstack-usage=100"],
    );
    assert_eq!(warnings(&c).len(), 2, "{c}");
}

/// `-Werror` makes it an error tagged as gcc tags it, and
/// `-Werror=stack-usage=<size>` sets the limit and does the same;
/// `-Wno-error=stack-usage=` leaves it a warning.
#[test]
fn stack_usage_answers_to_werror() {
    for flags in [
        &["-Wstack-usage=100", "-Werror"][..],
        &["-Werror=stack-usage=100"],
    ] {
        let mut all = vec![X86_64, "-O0"];
        all.extend_from_slice(flags);
        let c = compile_rejected_with("su_werror", SRC, &all);
        assert!(
            c.contains(
                "su_werror.c:2:12: error: stack usage is 128 bytes [-Werror=stack-usage=]\n"
            ),
            "{flags:?}:\n{c}"
        );
        assert!(
            c.contains("su_werror.c:5:5: error: stack usage might be unbounded"),
            "{flags:?}:\n{c}"
        );
    }
    let c = compile(
        "su_no_error",
        SRC,
        &[
            X86_64,
            "-O0",
            "-Wstack-usage=100",
            "-Werror",
            "-Wno-error=stack-usage=",
        ],
    );
    assert!(c.success, "{}", c.stderr);
    assert_eq!(warnings(&c.stderr).len(), 2, "{}", c.stderr);
}
