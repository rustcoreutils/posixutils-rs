//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `-Wframe-larger-than=<size>`, checked against gcc 13: a function whose
// frame -- its locals, spill slots and variadic register save area -- is
// larger than the limit draws "the frame size of B bytes is larger than N
// bytes", an error under `-Werror`, on both targets.
//

use crate::test_compile::{compile, compile_accepted, compile_rejected_with};

const SRC: &str = "\
void use(char *);
void big(void) { char b[70000]; use(b); }
int small(int a) { return a + 1; }
";

const AARCH64: &str = "--target=aarch64-unknown-linux-gnu";

/// The frame size each warning names, by function, for the limit `flags`
/// give.
#[track_caller]
fn sizes(stderr: &str) -> Vec<(u64, u64)> {
    stderr
        .lines()
        .filter_map(|l| {
            let rest = l.split_once("the frame size of ")?.1;
            let (size, rest) = rest.split_once(" bytes is larger than ")?;
            let limit = rest.split_once(" bytes")?.0;
            Some((size.parse().ok()?, limit.parse().ok()?))
        })
        .collect()
}

/// The big frame is reported and the small one is not, at every
/// optimization level and on both targets; the size reported is at least the
/// array's.
#[test]
fn frame_larger_than_warns_for_the_big_frame() {
    for target in [&[][..], &[AARCH64]] {
        for opt in ["-O0", "-O2"] {
            let mut flags = vec![opt, "-Wframe-larger-than=1000"];
            flags.extend(target);
            let c = compile_accepted("frame_big", SRC, &flags);
            let found = sizes(&c);
            assert_eq!(found.len(), 1, "{flags:?}:\n{c}");
            let (size, limit) = found[0];
            assert!((70000..71000).contains(&size), "{flags:?}:\n{c}");
            assert_eq!(limit, 1000);
            assert!(c.contains("frame_big.c:2:"), "{flags:?}:\n{c}");
            assert!(
                c.contains(&format!(
                    "warning: the frame size of {size} bytes is larger than 1000 bytes \
                     [-Wframe-larger-than=]\n"
                )),
                "{c}"
            );
            // A limit at the size itself is not exceeded.
            let at = format!("-Wframe-larger-than={size}");
            let mut flags = vec![opt, at.as_str()];
            flags.extend(target);
            let c = compile_accepted("frame_at", SRC, &flags);
            assert!(sizes(&c).is_empty(), "{flags:?}:\n{c}");
        }
    }
}

/// Without the option, or after `-Wno-frame-larger-than`, nothing is said;
/// the last limit given is the one used, and a unit counts as gcc's do.
#[test]
fn frame_larger_than_options_fold() {
    for flags in [
        &[][..],
        &["-Wframe-larger-than=10", "-Wno-frame-larger-than"],
        &["-Wframe-larger-than=10", "-Wframe-larger-than=1MB"],
        &["-Wframe-larger-than=10", "-w"],
        &["-Wframe-larger-than=69KiB", "-Wframe-larger-than=100KB"],
    ] {
        let c = compile_accepted("frame_quiet", SRC, flags);
        assert!(sizes(&c).is_empty(), "{flags:?}:\n{c}");
    }
    let c = compile_accepted("frame_kib", SRC, &["-Wframe-larger-than=68KiB"]);
    assert_eq!(sizes(&c).len(), 1, "{c}");
    assert_eq!(sizes(&c)[0].1, 68 * 1024, "{c}");
    let c = compile_accepted(
        "frame_last",
        SRC,
        &["-Wno-frame-larger-than", "-Wframe-larger-than=8"],
    );
    assert_eq!(sizes(&c), [(70000, 8)], "{c}");
}

/// `-Werror` makes it an error tagged as gcc tags it, and
/// `-Werror=frame-larger-than=<size>` sets the limit and does the same;
/// `-Wno-error=frame-larger-than=` leaves it a warning.
#[test]
fn frame_larger_than_answers_to_werror() {
    for flags in [
        &["-Wframe-larger-than=1000", "-Werror"][..],
        &["-Werror=frame-larger-than=1000"],
    ] {
        let c = compile_rejected_with("frame_werror", SRC, flags);
        assert!(
            c.contains("bytes is larger than 1000 bytes [-Werror=frame-larger-than=]\n"),
            "{flags:?}:\n{c}"
        );
    }
    let c = compile(
        "frame_no_error",
        SRC,
        &[
            "-Wframe-larger-than=1000",
            "-Werror",
            "-Wno-error=frame-larger-than=",
        ],
    );
    assert!(c.success, "{}", c.stderr);
    assert_eq!(sizes(&c.stderr).len(), 1, "{}", c.stderr);
}
