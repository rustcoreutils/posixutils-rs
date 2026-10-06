//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
//! The `-f<name>` options c17 accepts, and what it says about the rest.
//!
//! gcc refuses an `-f` option it does not know, and so does c17, with gcc's
//! text, so a configure probe gets the answer it would get from gcc. A known
//! option is one of three things here:
//!
//! - **Implemented**: the driver parses it and c17 does what it asks.
//! - **Accepted**: taken in silence, because c17's output already means what
//!   the option asks for. Each entry says why ([`Why`]).
//! - **Unsupported**: gcc would do something observable that c17 does not.
//!   Taken, with the warning "'-fX' is not supported; ignored" -- in the group
//!   [`UNSUPPORTED_WARNING`], which `-w` and `-Wno-c17-unsupported-option`
//!   silence and which plain `-Werror` leaves a warning: distribution
//!   default flags pass these to every compile, and a build that adds
//!   `-Werror` asks about its own code, not about the compiler's.
//!
//! The table is not gcc's roster. It holds what real builds pass: Debian's
//! and Ubuntu's `dpkg-buildflags`, meson's and cmake's defaults, the build
//! files of the projects c17 builds (CPython, sparse, mesa, llama.cpp,
//! yosys), c17's own tests, and every `-f` option in the gcc torture
//! suite's `dg-options`. Each name is one gcc 13 accepts for C, except
//! c17's `-fno-cf-protection` and `-fpermissive` (gcc 13 gives C a warning
//! for the second).
//!
//! `-ffreestanding` is not here: the driver refuses it, there being no
//! freestanding environment to compile for.

/// Why an [`Effect::Accepted`] option needs nothing from c17.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Why {
    /// It enables, disables or tunes a gcc optimisation. c17 has no such
    /// pass, or has one whose output means the same program; neither way
    /// changes what the program does.
    Pass,
    /// It permits a transformation that c17 never makes, such as
    /// reassociating floating-point arithmetic, assuming that pointers
    /// to different types do not alias, or omitting a frame pointer.
    Permission,
    /// It asks for something c17 always does. Each entry gives the
    /// evidence.
    Default,
    /// It chooses how diagnostics look: colour, line wrapping, carets,
    /// the option tag. c17's text is plain either way.
    Presentation,
    /// It chooses which section code or data goes in. Every choice gives
    /// the same program on a hosted system, where `.bss` starts zeroed.
    Placement,
    /// It asks c17 to omit unwind tables. c17 always emits CFI; extra
    /// unwind tables change no program's behaviour.
    UnwindTables,
}

/// What c17 does with an `-f` option it accepts.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Effect {
    /// Parsed by the driver, which does what it asks.
    Implemented,
    /// Taken in silence: c17's output already is what it asks for.
    Accepted(Why),
    /// Taken with a warning: what it asks for is missing.
    Unsupported,
}

use Effect::{Accepted, Implemented, Unsupported};
use Why::*;

/// The options taken as `-f<name>`, exactly, sorted. A name with `=` here
/// stands for that one value (`-finput-charset=utf-8`); the stems that take
/// a range of values are in [`JOINED`].
const PLAIN: &[(&str, Effect)] = &[
    // The `-fpic` family: `target::PositionIndependence`.
    ("PIC", Implemented),
    ("PIE", Implemented),
    ("associative-math", Accepted(Permission)),
    // c17 emits `.cfi_startproc` and frame rules for every function.
    ("asynchronous-unwind-tables", Accepted(Default)),
    // Main driver: `-fcf-protection` and its `=` form.
    ("cf-protection", Implemented),
    // A tentative definition is an ordinary definition, not a common
    // symbol that merges with its namesakes in other units.
    ("common", Unsupported),
    ("conserve-stack", Accepted(Pass)),
    ("cx-limited-range", Accepted(Permission)),
    ("data-sections", Accepted(Placement)),
    ("diagnostics-color", Accepted(Presentation)),
    ("diagnostics-format=text", Accepted(Presentation)),
    ("diagnostics-show-option", Accepted(Presentation)),
    // gcc's dump files, which c17 does not write.
    ("dump-rtl-reload", Unsupported),
    ("dump-tree-optimized", Unsupported),
    ("dump-tree-ssa", Unsupported),
    // c17 emits no LSDA, so a `cleanup` variable is not cleaned up when an
    // exception or a thread cancellation unwinds through its frame.
    ("exceptions", Unsupported),
    ("expensive-optimizations", Accepted(Pass)),
    ("fast-math", Accepted(Permission)),
    // c17's objects hold machine code, which is what a fat LTO object
    // carries besides the IR, and every link accepts them.
    ("fat-lto-objects", Accepted(Pass)),
    // c17 keeps no excess precision: `float` and `double` live in SSE or
    // FP registers of their own width.
    ("float-store", Accepted(Default)),
    ("function-sections", Accepted(Placement)),
    // The GIMPLE front end.
    ("gimple", Unsupported),
    ("gnu89-inline", Implemented),
    // Hardening: the compares are not checked twice.
    ("harden-compares", Unsupported),
    ("hosted", Implemented),
    ("indirect-inlining", Accepted(Pass)),
    ("inline", Implemented),
    ("inline-functions", Accepted(Pass)),
    ("inline-small-functions", Accepted(Pass)),
    // c17 reads its source as UTF-8.
    ("input-charset=UTF-8", Accepted(Default)),
    ("input-charset=utf-8", Accepted(Default)),
    // No `__cyg_profile_func_enter` calls are made.
    ("instrument-functions", Unsupported),
    ("ipa-icf", Accepted(Pass)),
    ("ipa-modref", Accepted(Pass)),
    ("ipa-pta", Accepted(Pass)),
    ("ivopts", Accepted(Pass)),
    ("live-range-shrinkage", Accepted(Pass)),
    ("loop-parallelize-all", Accepted(Pass)),
    ("lto", Accepted(Pass)),
    ("math-errno", Implemented),
    ("modulo-sched", Accepted(Pass)),
    ("move-loop-invariants", Accepted(Pass)),
    ("no-PIC", Implemented),
    ("no-PIE", Implemented),
    ("no-associative-math", Accepted(Default)),
    ("no-asynchronous-unwind-tables", Accepted(UnwindTables)),
    ("no-builtin", Implemented),
    // c17's own spelling of `-fcf-protection=none`; gcc 13 has no such
    // option.
    ("no-cf-protection", Implemented),
    ("no-code-hoisting", Accepted(Pass)),
    // A tentative definition is an ordinary definition, as in gcc 10 and
    // later: `int x;` is a `.globl` object, never `.comm`.
    ("no-common", Accepted(Default)),
    // c17 calls `__muldc3` and `__divdc3`, Annex G's full range.
    ("no-cx-limited-range", Accepted(Default)),
    ("no-dce", Accepted(Pass)),
    ("no-diagnostics-color", Accepted(Presentation)),
    ("no-diagnostics-show-caret", Accepted(Presentation)),
    ("no-diagnostics-show-option", Accepted(Presentation)),
    ("no-early-inlining", Accepted(Pass)),
    ("no-exceptions", Accepted(Default)),
    ("no-fast-math", Accepted(Default)),
    ("no-fat-lto-objects", Accepted(Pass)),
    ("no-finite-math-only", Accepted(Default)),
    ("no-float-store", Accepted(Permission)),
    ("no-gcse", Accepted(Pass)),
    ("no-gnu89-inline", Implemented),
    ("no-guess-branch-probability", Accepted(Pass)),
    ("no-if-conversion", Accepted(Pass)),
    ("no-inline", Implemented),
    ("no-inline-functions", Accepted(Pass)),
    ("no-ipa-cp", Accepted(Pass)),
    ("no-ipa-pure-const", Accepted(Pass)),
    ("no-ipa-ra", Accepted(Pass)),
    ("no-ira-share-spill-slots", Accepted(Pass)),
    ("no-ivopts", Accepted(Pass)),
    ("no-lto", Accepted(Default)),
    ("no-math-errno", Implemented),
    ("no-move-loop-invariants", Accepted(Pass)),
    ("no-non-call-exceptions", Accepted(Default)),
    // Every prologue sets up %rbp, or x29, leaf function or not.
    ("no-omit-frame-pointer", Accepted(Default)),
    ("no-optimize-sibling-calls", Accepted(Pass)),
    ("no-optimize-strlen", Accepted(Pass)),
    ("no-peel-loops", Accepted(Pass)),
    ("no-pic", Implemented),
    ("no-pie", Implemented),
    // A call through the PLT reaches the same function.
    ("no-plt", Accepted(Pass)),
    ("no-printf-return-value", Accepted(Pass)),
    ("no-reorder-blocks-and-partition", Accepted(Pass)),
    ("no-schedule-insns", Accepted(Pass)),
    ("no-schedule-insns2", Accepted(Pass)),
    ("no-semantic-interposition", Accepted(Permission)),
    // An enum is `int`-sized.
    ("no-short-enums", Accepted(Default)),
    ("no-signaling-nans", Implemented),
    ("no-signed-char", Implemented),
    ("no-signed-zeros", Accepted(Permission)),
    // Withdraws `-fstack-clash-protection`; the driver takes both.
    ("no-stack-clash-protection", Implemented),
    ("no-stack-protector", Accepted(Default)),
    // c17 does not assume aliasing by type: a store through `float *`
    // reloads an `int` read through `int *`.
    ("no-strict-aliasing", Accepted(Default)),
    // gcc's spelling of `-fwrapv`; see `wrapv`.
    ("no-strict-overflow", Accepted(Default)),
    ("no-tracer", Accepted(Pass)),
    ("no-trapping-math", Implemented),
    // Signed overflow wraps; nothing traps it.
    ("no-trapv", Accepted(Default)),
    ("no-tree-bit-ccp", Accepted(Pass)),
    ("no-tree-ccp", Accepted(Pass)),
    ("no-tree-ch", Accepted(Pass)),
    ("no-tree-coalesce-vars", Accepted(Pass)),
    ("no-tree-copy-prop", Accepted(Pass)),
    ("no-tree-dce", Accepted(Pass)),
    ("no-tree-dominator-opts", Accepted(Pass)),
    ("no-tree-dse", Accepted(Pass)),
    ("no-tree-forwprop", Accepted(Pass)),
    ("no-tree-fre", Accepted(Pass)),
    ("no-tree-loop-distribute-patterns", Accepted(Pass)),
    ("no-tree-loop-im", Accepted(Pass)),
    ("no-tree-pre", Accepted(Pass)),
    ("no-tree-sra", Accepted(Pass)),
    ("no-tree-vectorize", Accepted(Pass)),
    ("no-tree-vrp", Accepted(Pass)),
    ("no-unroll-loops", Accepted(Pass)),
    ("no-unsigned-char", Implemented),
    ("no-unwind-tables", Accepted(UnwindTables)),
    ("no-vect-cost-model", Accepted(Pass)),
    ("no-wrapv", Accepted(Permission)),
    ("no-zero-initialized-in-bss", Accepted(Placement)),
    // The OpenMP pragmas are ignored and `_OPENMP` is not defined.
    ("non-call-exceptions", Unsupported),
    ("omit-frame-pointer", Accepted(Permission)),
    ("openmp", Unsupported),
    ("optimize-strlen", Accepted(Pass)),
    // Every struct keeps its natural layout.
    ("pack-struct", Unsupported),
    ("peel-loops", Accepted(Pass)),
    ("permissive", Implemented),
    ("pic", Implemented),
    ("pie", Implemented),
    ("profile-correction", Accepted(Pass)),
    // No instrumentation, so no `.gcda` profile is written.
    ("profile-generate", Unsupported),
    ("profile-use", Accepted(Pass)),
    // No optimization record is written.
    ("save-optimization-record", Unsupported),
    ("sched-stalled-insns", Accepted(Pass)),
    ("sched2-use-superblocks", Accepted(Pass)),
    ("schedule-insns", Accepted(Pass)),
    ("schedule-insns2", Accepted(Pass)),
    ("selective-scheduling2", Accepted(Pass)),
    // At -O1 and above c17 inlines a global function into its callers in
    // the same unit even under -fPIC, so an interposed definition is not
    // the one they call.
    ("semantic-interposition", Unsupported),
    ("signaling-nans", Implemented),
    ("signed-char", Implemented),
    ("signed-zeros", Accepted(Default)),
    // Hardening: no stack probes.
    ("stack-check", Unsupported),
    // The driver takes it, and c17 warns for each function whose stack
    // gcc would probe: one whose frame reaches the guard, or that
    // allocates on the stack dynamically. Other functions need no probe.
    ("stack-clash-protection", Implemented),
    // Hardening: no canary is placed or checked.
    ("stack-protector", Unsupported),
    ("stack-protector-all", Unsupported),
    ("stack-protector-explicit", Unsupported),
    ("stack-protector-strong", Unsupported),
    ("strict-aliasing", Accepted(Permission)),
    ("strict-overflow", Accepted(Permission)),
    ("tracer", Accepted(Pass)),
    ("trapping-math", Implemented),
    // Signed overflow wraps rather than aborting.
    ("trapv", Unsupported),
    ("tree-loop-distribution", Accepted(Pass)),
    ("tree-loop-vectorize", Accepted(Pass)),
    ("tree-slp-vectorize", Accepted(Pass)),
    ("tree-vectorize", Accepted(Pass)),
    ("unroll-all-loops", Accepted(Pass)),
    ("unroll-loops", Accepted(Pass)),
    ("unsigned-char", Implemented),
    ("unswitch-loops", Accepted(Pass)),
    // c17 emits CFI for every function.
    ("unwind-tables", Accepted(Default)),
    // Handed to the host compiler driver, which links.
    ("use-ld=bfd", Implemented),
    ("use-ld=gold", Implemented),
    ("use-ld=lld", Implemented),
    ("use-ld=mold", Implemented),
    // The linker plugin reads LTO IR, which c17's objects do not carry.
    ("use-linker-plugin", Accepted(Pass)),
    ("verbose-asm", Implemented),
    // Signed arithmetic wraps: c17 does not fold `x + 1 > x`, nor assume
    // anything else from signed overflow being undefined.
    ("wrapv", Accepted(Default)),
];

/// How the error for a bad [`Value::Choice`] is worded, which differs by
/// option in gcc.
#[derive(Clone, Copy, Debug)]
enum ChoiceText {
    /// "unknown <what> '<value>'".
    Unknown(&'static str),
    /// "unrecognized argument in option '-f<stem>=<value>'".
    Unrecognized,
}

/// The value a [`JOINED`] option takes after its `=`.
#[derive(Clone, Copy, Debug)]
enum Value {
    /// Checked where the driver parses the option.
    Driver,
    /// Any non-empty text.
    Any,
    /// A non-negative integer.
    Unsigned,
    /// A power of two from 1 to 16.
    PackAlign,
    /// `auto`, `jobserver` or a positive count: `-flto=`'s job count.
    Jobs,
    /// One of these words.
    Choice(ChoiceText, &'static [&'static str]),
}

/// The options that take a value, `-f<stem>=<value>`, sorted by stem, each
/// with its value and what c17 does with it. A value [`PLAIN`] names on its
/// own is classified there first.
const JOINED: &[(&str, Value, Effect)] = &[
    ("cf-protection", Value::Driver, Implemented),
    ("debug-prefix-map", Value::Driver, Implemented),
    (
        "diagnostics-color",
        Value::Choice(ChoiceText::Unrecognized, &["always", "auto", "never"]),
        Accepted(Presentation),
    ),
    ("file-prefix-map", Value::Driver, Implemented),
    // `off` is what c17 does; `on` and `fast` permit contracting `a * b + c`
    // into a fused multiply-add, which c17 never does.
    (
        "fp-contract",
        Value::Choice(
            ChoiceText::Unknown("floating point contraction style"),
            &["fast", "off", "on"],
        ),
        Accepted(Permission),
    ),
    // Any character set but UTF-8, which is in `PLAIN`: c17 reads UTF-8.
    ("input-charset", Value::Any, Unsupported),
    (
        "ira-algorithm",
        Value::Choice(ChoiceText::Unknown("IRA algorithm"), &["CB", "priority"]),
        Accepted(Pass),
    ),
    ("lto", Value::Jobs, Accepted(Pass)),
    (
        "lto-partition",
        Value::Choice(
            ChoiceText::Unknown("LTO partitioning model"),
            &["1to1", "balanced", "max", "none", "one"],
        ),
        Accepted(Pass),
    ),
    ("macro-prefix-map", Value::Driver, Implemented),
    ("message-length", Value::Unsigned, Accepted(Presentation)),
    // Every struct keeps its natural layout.
    ("pack-struct", Value::PackAlign, Unsupported),
    // No sanitizer instruments the code, and no `__SANITIZE_*__` macro is
    // defined.
    ("sanitize", Value::Any, Unsupported),
    ("tls-model", Value::Driver, Implemented),
    ("tree-parallelize-loops", Value::Unsigned, Accepted(Pass)),
    ("visibility", Value::Driver, Implemented),
];

/// What becomes of one `-f<name>` option.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Verdict {
    /// Accepted, with this effect.
    Known(Effect),
    /// Refused before compiling, with these lines (an error, then any
    /// notes), as gcc's driver refuses it.
    Error(Vec<String>),
}

/// The warning group of the "not supported" warnings.
pub const UNSUPPORTED_WARNING: &str = "c17-unsupported-option";

/// The entry for `name` in [`PLAIN`].
fn plain(name: &str) -> Option<Effect> {
    PLAIN
        .binary_search_by(|(n, _)| n.cmp(&name))
        .ok()
        .map(|i| PLAIN[i].1)
}

/// The entry for `stem` in [`JOINED`].
fn joined(stem: &str) -> Option<(Value, Effect)> {
    JOINED
        .binary_search_by(|(s, _, _)| s.cmp(&stem))
        .ok()
        .map(|i| (JOINED[i].1, JOINED[i].2))
}

/// The text gcc gives for an option it does not know.
fn unrecognized(name: &str) -> Verdict {
    Verdict::Error(vec![format!(
        "error: unrecognized command-line option '-f{name}'"
    )])
}

/// Classify the option `-f<name>`.
pub fn classify(name: &str) -> Verdict {
    if let Some(effect) = plain(name) {
        return Verdict::Known(effect);
    }
    // `-fno-builtin-<function>`: any function name.
    if name
        .strip_prefix("no-builtin-")
        .is_some_and(|f| !f.is_empty())
    {
        return Verdict::Known(Implemented);
    }
    match name.split_once('=') {
        Some((stem, value)) => match joined(stem) {
            Some((kind, effect)) => check_value(stem, kind, value, effect),
            None => unrecognized(name),
        },
        None => unrecognized(name),
    }
}

/// `-f<stem>=<value>` for a stem in [`JOINED`].
fn check_value(stem: &str, kind: Value, value: &str, effect: Effect) -> Verdict {
    if value.is_empty() {
        return Verdict::Error(vec![format!("error: missing argument to '-f{stem}='")]);
    }
    let digits = !value.is_empty() && value.bytes().all(|b| b.is_ascii_digit());
    let fault = match kind {
        Value::Driver | Value::Any => None,
        Value::Unsigned | Value::PackAlign if !digits => Some(format!(
            "error: argument to '-f{stem}=' should be a non-negative integer"
        )),
        Value::Unsigned => None,
        Value::PackAlign => {
            let n = value.parse::<u64>().unwrap_or(u64::MAX);
            (!(n.is_power_of_two() && n <= 16)).then(|| {
                format!("error: structure alignment must be a small power of two, not {value}")
            })
        }
        Value::Jobs => {
            let count = digits && value.parse::<u64>().map_or(true, |n| n > 0);
            (!(count || value == "auto" || value == "jobserver"))
                .then(|| format!("error: unrecognized argument to '-f{stem}=' option: '{value}'"))
        }
        Value::Choice(text, words) => {
            if words.contains(&value) {
                None
            } else {
                let first = match text {
                    ChoiceText::Unknown(what) => format!("error: unknown {what} '{value}'"),
                    ChoiceText::Unrecognized => {
                        format!("error: unrecognized argument in option '-f{stem}={value}'")
                    }
                };
                return Verdict::Error(vec![
                    first,
                    format!(
                        "note: valid arguments to '-f{stem}=' are: {}",
                        words.join(" ")
                    ),
                ]);
            }
        }
    };
    match fault {
        Some(line) => Verdict::Error(vec![line]),
        None => Verdict::Known(effect),
    }
}

/// The name of what an option asks for, so that a later `-fno-<family>`
/// can withdraw it: the stack protector's levels are one request, and an
/// option with a value is a request for that stem.
pub fn family(name: &str) -> &str {
    let stem = name.split_once('=').map_or(name, |(stem, _)| stem);
    if stem.starts_with("stack-protector") {
        "stack-protector"
    } else {
        stem
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn error(name: &str) -> String {
        match classify(name) {
            Verdict::Error(lines) => lines.join("\n"),
            other => panic!("-f{name}: {other:?}"),
        }
    }

    #[test]
    fn tables_are_sorted_and_unique() {
        assert!(PLAIN.windows(2).all(|w| w[0].0 < w[1].0));
        assert!(JOINED.windows(2).all(|w| w[0].0 < w[1].0));
    }

    /// The defaults of the build systems and distributions c17 is meant
    /// to be dropped into.
    #[test]
    fn corpus_defaults() {
        for (name, effect) in [
            // Debian trixie and Ubuntu 24.04 `dpkg-buildflags`.
            ("file-prefix-map=/build=.", Implemented),
            ("stack-protector-strong", Unsupported),
            ("stack-clash-protection", Implemented),
            ("cf-protection", Implemented),
            ("no-omit-frame-pointer", Accepted(Default)),
            ("lto=auto", Accepted(Pass)),
            ("fat-lto-objects", Accepted(Pass)),
            // meson and cmake.
            ("diagnostics-color=always", Accepted(Presentation)),
            ("no-diagnostics-color", Accepted(Presentation)),
            ("no-fat-lto-objects", Accepted(Pass)),
            ("PIC", Implemented),
            ("visibility=hidden", Implemented),
            ("sanitize=address,undefined", Unsupported),
        ] {
            assert_eq!(classify(name), Verdict::Known(effect), "-f{name}");
        }
    }

    #[test]
    fn values() {
        for name in [
            "lto",
            "lto=jobserver",
            "lto=8",
            "message-length=0",
            "fp-contract=off",
            "lto-partition=none",
            "tree-parallelize-loops=2",
            "input-charset=utf-8",
            "no-builtin-memcpy",
        ] {
            assert!(
                matches!(classify(name), Verdict::Known(_)),
                "-f{name}: {:?}",
                classify(name)
            );
        }
        assert_eq!(classify("pack-struct=16"), Verdict::Known(Unsupported));
        assert_eq!(
            classify("input-charset=latin1"),
            Verdict::Known(Unsupported)
        );
    }

    #[test]
    fn unknown_names_get_gcc_text() {
        for name in [
            "foo",
            "",
            "sanitize",
            "no-lto=1",
            "no-builtin-",
            "use-ld=bogus",
            "no-stack-protector-strong",
            "bogus=1",
        ] {
            assert_eq!(
                error(name),
                format!("error: unrecognized command-line option '-f{name}'")
            );
        }
    }

    #[test]
    fn bad_values_get_gcc_text() {
        assert_eq!(
            error("diagnostics-color=bogus"),
            "error: unrecognized argument in option '-fdiagnostics-color=bogus'\n\
             note: valid arguments to '-fdiagnostics-color=' are: always auto never"
        );
        assert_eq!(
            error("lto-partition=bogus"),
            "error: unknown LTO partitioning model 'bogus'\n\
             note: valid arguments to '-flto-partition=' are: 1to1 balanced max none one"
        );
        assert_eq!(
            error("lto=0"),
            "error: unrecognized argument to '-flto=' option: '0'"
        );
        assert_eq!(error("lto="), "error: missing argument to '-flto='");
        assert_eq!(
            error("message-length=x"),
            "error: argument to '-fmessage-length=' should be a non-negative integer"
        );
        for v in ["0", "3", "32"] {
            assert_eq!(
                error(&format!("pack-struct={v}")),
                format!("error: structure alignment must be a small power of two, not {v}")
            );
        }
    }

    #[test]
    fn families() {
        assert_eq!(family("stack-protector-strong"), "stack-protector");
        assert_eq!(family("stack-protector"), "stack-protector");
        assert_eq!(family("sanitize=address"), "sanitize");
        assert_eq!(family("pack-struct=4"), "pack-struct");
        assert_eq!(family("trapv"), "trapv");
    }
}
