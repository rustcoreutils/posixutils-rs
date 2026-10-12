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
//! A fourth kind is an option gcc knows only for another language:
//! **ForLanguages** takes it with gcc's own warning, "command-line option
//! '-fno-rtti' is valid for C++/D/ObjC++ but not for C". gcc's configure
//! probes `-fno-rtti` with the C compiler and gives the answer to its C++
//! flags, so refusing it is not what gcc does.
//!
//! The options that change what a unit means or how its functions are laid
//! out -- `-fshort-wchar`, `-ffreestanding`, `-falign-functions=`,
//! `-fpatchable-function-entry=` and the rest -- are folded by
//! [`CodeOptions`].

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
    /// It chooses how diagnostics look: colour, line wrapping, carets.
    /// c17's text is plain either way.
    Presentation,
    /// It chooses which section code or data goes in. Every choice gives
    /// the same program on a hosted system, where `.bss` starts zeroed.
    Placement,
    /// It tunes a feature of another language that C does not have, and
    /// gcc 13 takes it for C without a word.
    OtherLanguage,
    /// It tunes a sanitizer, and alone asks for none: c17 refuses every
    /// `-fsanitize=` request (see [`SANITIZERS`]), so there is nothing for
    /// it to tune.
    SanitizerModifier,
}

/// The stems that ask for instrumentation c17 does not provide, each a
/// comma-separated list the driver keeps, `-fno-<stem>=` withdrawing names
/// from it (`all` every one). What remains is refused: a sanitizer taken
/// with a warning left a build believing its code was checked, and a
/// configure probe believing the compiler had one.
pub const SANITIZERS: &[&str] = &["sanitize", "sanitize-coverage"];

/// What c17 does with an `-f` option it accepts.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Effect {
    /// Parsed by the driver, which does what it asks.
    Implemented,
    /// Taken in silence: c17's output already is what it asks for.
    Accepted(Why),
    /// Taken with a warning: what it asks for is missing.
    Unsupported,
    /// Taken with gcc's warning that it is valid only for these languages,
    /// spelled as gcc lists them.
    ForLanguages(&'static str),
}

use Effect::{Accepted, ForLanguages, Implemented, Unsupported};

/// gcc's language lists for the [`ForLanguages`] options.
const CXX_D_OBJCXX: Effect = ForLanguages("C++/D/ObjC++");
const CXX_OBJCXX: Effect = ForLanguages("C++/ObjC++");
const OBJC_OBJCXX: Effect = ForLanguages("ObjC/ObjC++");
use Why::*;

/// The options taken as `-f<name>`, exactly, sorted. A name with `=` here
/// stands for that one value (`-finput-charset=utf-8`); the stems that take
/// a range of values are in [`JOINED`].
const PLAIN: &[(&str, Effect)] = &[
    // The `-fpic` family: `target::PositionIndependence`.
    ("PIC", Implemented),
    ("PIE", Implemented),
    // `-falign-functions` and its value: `CodeOptions`.
    ("align-functions", Implemented),
    // Alignment of branch targets inside a function: a tuning hint.
    ("align-jumps", Accepted(Pass)),
    ("align-labels", Accepted(Pass)),
    ("align-loops", Accepted(Pass)),
    ("allow-store-data-races", Accepted(Permission)),
    ("associative-math", Accepted(Permission)),
    // The unwind tables: `target::UnwindTables`.
    ("asynchronous-unwind-tables", Implemented),
    // Main driver: `-fcf-protection` and its `=` form.
    ("cf-protection", Implemented),
    // C++'s `new` checks its result for null.
    ("check-new", Accepted(OtherLanguage)),
    // A tentative definition is a common symbol, merged with its
    // namesakes in other units; `-fno-` (the default), an ordinary one.
    ("common", Implemented),
    ("conserve-stack", Accepted(Pass)),
    ("cx-limited-range", Accepted(Permission)),
    ("data-sections", Implemented),
    ("delete-null-pointer-checks", Accepted(Permission)),
    ("diagnostics-color", Accepted(Presentation)),
    ("diagnostics-format=text", Accepted(Presentation)),
    // Main driver: whether a diagnostic names its option.
    ("diagnostics-show-option", Implemented),
    // gcc's dump files, which c17 does not write.
    ("dump-rtl-reload", Unsupported),
    ("dump-tree-optimized", Unsupported),
    ("dump-tree-ssa", Unsupported),
    // Call frame information as `.cfi_*` directives, the only way c17
    // writes it; `-fno-` is refused by the driver, c17 having no tables of
    // its own to write instead.
    ("dwarf2-cfi-asm", Accepted(Default)),
    // Obsolete: gcc 13 takes it and does nothing.
    ("eliminate-dwarf2-dups", Accepted(Pass)),
    // The unwind tables, and the landing pads that run a `cleanup`
    // variable's cleanup when its frame is unwound: `target::UnwindTables`.
    ("exceptions", Implemented),
    // Each `_Float16` operation is rounded to `_Float16`, as c17 does;
    // `float` and `double` have no excess precision on either target.
    ("excess-precision=16", Accepted(Default)),
    ("expensive-optimizations", Accepted(Pass)),
    ("fast-math", Accepted(Permission)),
    // c17's objects hold machine code, which is what a fat LTO object
    // carries besides the IR, and every link accepts them.
    ("fat-lto-objects", Accepted(Pass)),
    // c17 keeps no excess precision: `float` and `double` live in SSE or
    // FP registers of their own width.
    ("float-store", Accepted(Default)),
    ("freestanding", Implemented),
    // Each function, or object, in a section of its own: `.text.NAME`,
    // `.data.NAME` and the rest.
    ("function-sections", Implemented),
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
    ("jump-tables", Accepted(Permission)),
    ("live-range-shrinkage", Accepted(Pass)),
    ("loop-parallelize-all", Accepted(Pass)),
    ("lto", Accepted(Pass)),
    ("math-errno", Implemented),
    // Lets distinct constant objects share storage, which C does not: c17
    // gives every object its own.
    ("merge-all-constants", Accepted(Permission)),
    ("modulo-sched", Accepted(Pass)),
    ("move-loop-invariants", Accepted(Pass)),
    ("no-PIC", Implemented),
    ("no-PIE", Implemented),
    ("no-align-functions", Implemented),
    ("no-align-jumps", Accepted(Pass)),
    ("no-align-labels", Accepted(Pass)),
    ("no-align-loops", Accepted(Pass)),
    // c17 stores only what the program stores: no pass invents a store
    // on a path that had none.
    ("no-allow-store-data-races", Accepted(Default)),
    ("no-associative-math", Accepted(Default)),
    ("no-asynchronous-unwind-tables", Implemented),
    ("no-builtin", Implemented),
    // c17's own spelling of `-fcf-protection=none`; gcc 13 has no such
    // option.
    ("no-cf-protection", Implemented),
    ("no-check-new", Accepted(OtherLanguage)),
    ("no-code-hoisting", Accepted(Pass)),
    ("no-common", Implemented),
    // c17 calls `__muldc3` and `__divdc3`, Annex G's full range.
    ("no-cx-limited-range", Accepted(Default)),
    ("no-data-sections", Implemented),
    ("no-dce", Accepted(Pass)),
    // No pass assumes a pointer is non-null because it was dereferenced:
    // `*p; if (!p)` keeps its test.
    ("no-delete-null-pointer-checks", Accepted(Default)),
    ("no-diagnostics-color", Accepted(Presentation)),
    ("no-diagnostics-show-caret", Accepted(Presentation)),
    ("no-diagnostics-show-option", Implemented),
    ("no-dwarf2-cfi-asm", Implemented),
    ("no-early-inlining", Accepted(Pass)),
    ("no-enforce-eh-specs", CXX_OBJCXX),
    ("no-exceptions", Implemented),
    ("no-fast-math", Accepted(Default)),
    ("no-fat-lto-objects", Accepted(Pass)),
    ("no-finite-math-only", Accepted(Default)),
    ("no-float-store", Accepted(Permission)),
    ("no-function-sections", Implemented),
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
    // c17 lowers a `switch` to compares and branches, never a table of
    // addresses an indirect jump reads -- which a retpoline build forbids.
    ("no-jump-tables", Accepted(Default)),
    ("no-lto", Accepted(Default)),
    ("no-math-errno", Implemented),
    ("no-move-loop-invariants", Accepted(Pass)),
    ("no-non-call-exceptions", Accepted(Default)),
    ("no-objc-exceptions", OBJC_OBJCXX),
    // Every prologue sets up %rbp, or x29, leaf function or not.
    ("no-omit-frame-pointer", Accepted(Default)),
    ("no-optimize-sibling-calls", Accepted(Pass)),
    ("no-optimize-strlen", Accepted(Pass)),
    ("no-peel-loops", Accepted(Pass)),
    ("no-pic", Implemented),
    ("no-pie", Implemented),
    // A call to a function that may be defined elsewhere goes through its
    // GOT entry rather than a PLT stub.
    ("no-plt", Implemented),
    ("no-printf-return-value", Accepted(Pass)),
    ("no-reorder-blocks-and-partition", Accepted(Pass)),
    ("no-rounding-math", Implemented),
    ("no-rtti", CXX_D_OBJCXX),
    (
        "no-sanitize-address-use-after-scope",
        Accepted(SanitizerModifier),
    ),
    ("no-sanitize-recover", Accepted(SanitizerModifier)),
    ("no-schedule-insns", Accepted(Pass)),
    ("no-schedule-insns2", Accepted(Pass)),
    ("no-semantic-interposition", Implemented),
    // An enum is `int`-sized.
    ("no-short-enums", Accepted(Default)),
    ("no-short-wchar", Implemented),
    ("no-signaling-nans", Implemented),
    ("no-signed-char", Implemented),
    ("no-signed-zeros", Accepted(Permission)),
    // No stack probes: what c17 emits.
    ("no-stack-check", Accepted(Default)),
    // Withdraws `-fstack-clash-protection`; the driver takes both.
    ("no-stack-clash-protection", Implemented),
    // The stack protector's levels and their withdrawal:
    // `target::StackProtector`.
    ("no-stack-protector", Implemented),
    ("no-stack-usage", Implemented),
    // c17 does not assume aliasing by type: a store through `float *`
    // reloads an `int` read through `int *`.
    ("no-strict-aliasing", Accepted(Default)),
    ("no-strict-flex-arrays", Implemented),
    // gcc's spelling of `-fwrapv`; see `wrapv`.
    ("no-strict-overflow", Accepted(Default)),
    ("no-threadsafe-statics", CXX_OBJCXX),
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
    ("no-unwind-tables", Implemented),
    ("no-use-cxa-atexit", CXX_OBJCXX),
    ("no-vect-cost-model", Accepted(Pass)),
    ("no-wrapv", Accepted(Permission)),
    ("no-zero-initialized-in-bss", Accepted(Placement)),
    // Only a call has a landing pad: a trapping instruction's signal
    // handler cannot throw through the frame and run its cleanups.
    ("non-call-exceptions", Unsupported),
    ("omit-frame-pointer", Accepted(Permission)),
    // The OpenMP pragmas are ignored and `_OPENMP` is not defined.
    ("openmp", Unsupported),
    // c17 makes no sibling call: every call returns to its caller.
    ("optimize-sibling-calls", Accepted(Permission)),
    ("optimize-strlen", Accepted(Pass)),
    // Every struct keeps its natural layout.
    ("pack-struct", Unsupported),
    ("peel-loops", Accepted(Pass)),
    ("permissive", Implemented),
    ("pic", Implemented),
    ("pie", Implemented),
    ("plt", Implemented),
    ("profile-correction", Accepted(Pass)),
    // No instrumentation, so no `.gcda` profile is written.
    ("profile-generate", Unsupported),
    ("profile-use", Accepted(Pass)),
    // Main driver: only an exact floating-point result is folded.
    ("rounding-math", Implemented),
    ("rtti", CXX_D_OBJCXX),
    (
        "sanitize-address-use-after-scope",
        Accepted(SanitizerModifier),
    ),
    ("sanitize-recover", Accepted(SanitizerModifier)),
    ("sanitize-trap", Accepted(SanitizerModifier)),
    (
        "sanitize-undefined-trap-on-error",
        Accepted(SanitizerModifier),
    ),
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
    ("semantic-interposition", Implemented),
    ("short-wchar", Implemented),
    ("signaling-nans", Implemented),
    ("signed-char", Implemented),
    ("signed-zeros", Accepted(Default)),
    // Hardening: no stack probes.
    ("stack-check", Unsupported),
    ("stack-check=no", Accepted(Default)),
    // The driver takes it, and c17 warns for each function whose stack
    // gcc would probe: one whose frame reaches the guard, or that
    // allocates on the stack dynamically. Other functions need no probe.
    ("stack-clash-protection", Implemented),
    ("stack-protector", Implemented),
    ("stack-protector-all", Implemented),
    ("stack-protector-explicit", Implemented),
    ("stack-protector-strong", Implemented),
    // Main driver: each function's stack use, in a `.su` file.
    ("stack-usage", Implemented),
    ("strict-aliasing", Accepted(Permission)),
    ("strict-flex-arrays", Implemented),
    ("strict-overflow", Accepted(Permission)),
    // Main driver: check, write nothing. libxcrypt's symbol-version probe.
    ("syntax-only", Implemented),
    ("threadsafe-statics", CXX_OBJCXX),
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
    ("unwind-tables", Implemented),
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
    ("visibility-inlines-hidden", CXX_OBJCXX),
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
    /// An alignment, `N[:M[:N2[:M2]]]`, as `-falign-functions=` takes.
    Align,
    /// One of these words.
    Choice(ChoiceText, &'static [&'static str]),
}

/// The options that take a value, `-f<stem>=<value>`, sorted by stem, each
/// with its value and what c17 does with it. A value [`PLAIN`] names on its
/// own is classified there first.
const JOINED: &[(&str, Value, Effect)] = &[
    ("align-functions", Value::Driver, Implemented),
    ("align-jumps", Value::Align, Accepted(Pass)),
    ("align-labels", Value::Align, Accepted(Pass)),
    ("align-loops", Value::Align, Accepted(Pass)),
    ("cf-protection", Value::Driver, Implemented),
    ("debug-prefix-map", Value::Driver, Implemented),
    (
        "diagnostics-color",
        Value::Choice(ChoiceText::Unrecognized, &["always", "auto", "never"]),
        Accepted(Presentation),
    ),
    // `16` is in `PLAIN`. `standard` and `fast` -- the same code in gcc --
    // keep a `_Float16` operation's result in `float` until it is assigned
    // or converted, where c17 rounds each one to `_Float16`.
    (
        "excess-precision",
        Value::Choice(
            ChoiceText::Unknown("excess precision style"),
            &["16", "fast", "standard"],
        ),
        Unsupported,
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
    ("min-function-alignment", Value::Driver, Implemented),
    ("no-sanitize", Value::Driver, Implemented),
    ("no-sanitize-coverage", Value::Driver, Implemented),
    (
        "no-sanitize-recover",
        Value::Any,
        Accepted(SanitizerModifier),
    ),
    ("no-sanitize-trap", Value::Any, Accepted(SanitizerModifier)),
    // Every struct keeps its natural layout.
    ("pack-struct", Value::PackAlign, Unsupported),
    ("patchable-function-entry", Value::Driver, Implemented),
    // The seed gcc makes up names from, so that its objects come out the
    // same every time. c17 makes up no name at random: its output is a
    // function of its input alone.
    ("random-seed", Value::Any, Accepted(Default)),
    // Refused by the driver unless withdrawn; see `SANITIZERS`.
    ("sanitize", Value::Driver, Implemented),
    ("sanitize-coverage", Value::Driver, Implemented),
    ("sanitize-recover", Value::Any, Accepted(SanitizerModifier)),
    ("sanitize-trap", Value::Any, Accepted(SanitizerModifier)),
    // `no` is in `PLAIN`. `generic` and `specific` ask for stack probes,
    // which c17 does not emit; gcc only warns of a value it does not know.
    ("stack-check", Value::Any, Unsupported),
    ("strict-flex-arrays", Value::Driver, Implemented),
    ("tls-model", Value::Driver, Implemented),
    ("tree-parallelize-loops", Value::Unsigned, Accepted(Pass)),
    ("trivial-auto-var-init", Value::Driver, Implemented),
    ("visibility", Value::Driver, Implemented),
    ("zero-call-used-regs", Value::Driver, Implemented),
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
        Value::Align => crate::target::AlignSpec::parse(stem, value, crate::target::Arch::X86_64)
            .err()
            .map(|e| format!("error: {e}")),
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
/// can withdraw it: an option with a value is a request for that stem.
pub fn family(name: &str) -> &str {
    name.split_once('=').map_or(name, |(stem, _)| stem)
}

/// What the `-f` options that change a unit's meaning or its functions'
/// shape ask, folded in command-line order, the last of each winning:
/// `-fshort-wchar`, `-ffreestanding`, `-falign-functions`,
/// `-fmin-function-alignment=`, `-fpatchable-function-entry=`,
/// `-fzero-call-used-regs=`, `-ftrivial-auto-var-init=` and
/// `-fstrict-flex-arrays`.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct CodeOptions {
    /// `-fshort-wchar`: `wchar_t` is `unsigned short`.
    pub short_wchar: bool,
    /// `-ffreestanding`: `__STDC_HOSTED__` is 0, and no library function is
    /// a builtin (`-fno-builtin`).
    pub freestanding: bool,
    pub shape: crate::target::FunctionShape,
    pub auto_var_init: crate::target::AutoVarInit,
    /// `-fstrict-flex-arrays=N`: which trailing arrays are flexible -- any
    /// (0), `[1]`, `[0]` and `[]` (1), `[0]` and `[]` (2), `[]` only (3).
    pub strict_flex_arrays: u8,
}

impl CodeOptions {
    /// Whether `-f<name>` is one of these.
    pub fn owns(name: &str) -> bool {
        const PLAIN: &[&str] = &[
            "short-wchar",
            "no-short-wchar",
            "freestanding",
            "hosted",
            "align-functions",
            "no-align-functions",
            "strict-flex-arrays",
            "no-strict-flex-arrays",
        ];
        const STEMS: &[&str] = &[
            "align-functions=",
            "min-function-alignment=",
            "patchable-function-entry=",
            "zero-call-used-regs=",
            "trivial-auto-var-init=",
            "strict-flex-arrays=",
        ];
        PLAIN.contains(&name) || STEMS.iter().any(|s| name.starts_with(s))
    }

    /// Fold `names` -- each an `-f` option without its `-f`, in order --
    /// for `target`. `Err` is gcc's refusal, a line per message.
    pub fn from_names(
        names: &[String],
        target: &crate::target::Target,
    ) -> Result<Self, Vec<String>> {
        use crate::target::{AlignSpec, AutoVarInit, PatchableEntry, ZeroRegs};
        let mut o = Self::default();
        for name in names {
            let (stem, value) = name.split_once('=').unwrap_or((name, ""));
            let fail = |msg: String| Err(vec![format!("error: {msg}")]);
            match (stem, name.contains('=')) {
                ("short-wchar", false) => o.short_wchar = true,
                ("no-short-wchar", false) => o.short_wchar = false,
                ("freestanding", false) => o.freestanding = true,
                ("hosted", false) => o.freestanding = false,
                ("align-functions", _) => {
                    match AlignSpec::parse("align-functions", value, target.arch) {
                        Ok(spec) => o.shape.align = Some(spec),
                        Err(e) => return fail(e),
                    }
                }
                ("no-align-functions", false) => o.shape.align = Some(AlignSpec::none()),
                ("min-function-alignment", true) => match value.parse::<u32>() {
                    Ok(n) if n <= 65536 => o.shape.min_align = n,
                    Ok(_) => {
                        return fail(
                            "'-fmin-function-alignment' is not between 0 and 65536".to_string(),
                        )
                    }
                    Err(_) => {
                        return fail(
                            "argument to '-fmin-function-alignment=' should be a \
                             non-negative integer"
                                .to_string(),
                        )
                    }
                },
                ("patchable-function-entry", true) => {
                    let mut parts = value.splitn(2, ',').map(str::parse::<u32>);
                    let total = parts.next().and_then(Result::ok);
                    let before = parts.next().map_or(Some(0), Result::ok);
                    match (total, before) {
                        (Some(total), Some(before)) if before <= total => {
                            o.shape.patchable =
                                (total > 0).then_some(PatchableEntry { total, before });
                        }
                        _ => {
                            return fail(
                                "invalid arguments for '-fpatchable-function-entry'".to_string(),
                            )
                        }
                    }
                }
                ("zero-call-used-regs", true) => match ZeroRegs::from_name(value) {
                    Some(z) => o.shape.zero_regs = z,
                    None => {
                        return fail(format!(
                            "unrecognized argument to '-fzero-call-used-regs=': '{value}'"
                        ))
                    }
                },
                ("trivial-auto-var-init", true) => {
                    o.auto_var_init = match value {
                        "uninitialized" => AutoVarInit::Uninitialized,
                        "zero" => AutoVarInit::Zero,
                        "pattern" => AutoVarInit::Pattern,
                        _ => {
                            return Err(vec![
                                format!(
                                    "error: unrecognized automatic variable initialization \
                                     type '{value}'"
                                ),
                                "note: valid arguments to '-ftrivial-auto-var-init=' are: \
                                 pattern uninitialized zero"
                                    .to_string(),
                            ])
                        }
                    }
                }
                ("strict-flex-arrays", false) => o.strict_flex_arrays = 3,
                ("no-strict-flex-arrays", false) => o.strict_flex_arrays = 0,
                ("strict-flex-arrays", true) => match value.parse::<u8>() {
                    Ok(n @ 0..=3) => o.strict_flex_arrays = n,
                    Ok(_) => {
                        return fail(
                            "argument to '-fstrict-flex-arrays=' is not between 0 and 3"
                                .to_string(),
                        )
                    }
                    Err(_) => {
                        return fail(
                            "argument to '-fstrict-flex-arrays=' should be a non-negative \
                             integer"
                                .to_string(),
                        )
                    }
                },
                _ => unreachable!("-f{name} is not a CodeOptions option"),
            }
        }
        let mut errors = Vec::new();
        let elf = target.os != crate::target::Os::MacOS;
        if o.shape.patchable.is_some() && !elf {
            errors.push(
                "error: '-fpatchable-function-entry' is not supported for this target".to_string(),
            );
        }
        if o.shape.zero_regs.on && target.arch != crate::target::Arch::X86_64 {
            errors.push(
                "error: '-fzero-call-used-regs' is not supported for this target".to_string(),
            );
        }
        if errors.is_empty() {
            Ok(o)
        } else {
            Err(errors)
        }
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
            ("stack-protector-strong", Implemented),
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
            ("sanitize=address,undefined", Implemented),
            (
                "sanitize-address-use-after-scope",
                Accepted(SanitizerModifier),
            ),
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

    /// gcc 13's language lists, from its warning for each in C.
    #[test]
    fn other_language_options() {
        for (name, langs) in [
            ("rtti", "C++/D/ObjC++"),
            ("no-rtti", "C++/D/ObjC++"),
            ("threadsafe-statics", "C++/ObjC++"),
            ("no-threadsafe-statics", "C++/ObjC++"),
            ("visibility-inlines-hidden", "C++/ObjC++"),
            ("no-enforce-eh-specs", "C++/ObjC++"),
            ("no-use-cxa-atexit", "C++/ObjC++"),
            ("no-objc-exceptions", "ObjC/ObjC++"),
        ] {
            assert_eq!(
                classify(name),
                Verdict::Known(ForLanguages(langs)),
                "-f{name}"
            );
        }
        assert_eq!(
            classify("check-new"),
            Verdict::Known(Accepted(OtherLanguage))
        );
    }

    #[test]
    fn families() {
        assert_eq!(family("sanitize=address"), "sanitize");
        assert_eq!(family("no-sanitize=all"), "no-sanitize");
        assert_eq!(family("pack-struct=4"), "pack-struct");
        assert_eq!(family("trapv"), "trapv");
    }
}
