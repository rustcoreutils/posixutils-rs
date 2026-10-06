//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
//! The `-W<name>` warning names c17 accepts, and what it says about the rest.
//!
//! Configure scripts decide whether a compiler supports a warning flag by
//! passing it and looking at the exit status (autoconf-archive's
//! `AX_CHECK_COMPILE_FLAG`, meson's `has_argument`, cmake's
//! `check_c_compiler_flag`). Accepting every name in silence gives each of
//! them the wrong answer, so a name not in this table is refused as gcc 13
//! refuses it, with gcc's text.
//!
//! The table is not gcc's roster. It holds c17's own groups, and the gcc C
//! warning names real builds pass: Debian's `dpkg-buildflags`, the
//! autoconf-archive warning macros, meson's warning levels, the c17 test
//! suites and the gcc torture suite's `dg-options`, and the projects c17
//! builds. Every one is a name gcc 13 accepts for C.

/// What c17 does with a warning name it accepts.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Effect {
    /// A group c17 gives warnings in, or a switch it honours.
    Implemented,
    /// A gcc warning c17 does not give: accepted, with no effect.
    Accepted,
}

use Effect::{Accepted, Implemented};

/// The names taken as `-W<name>` and `-Wno-<name>`, sorted. A name with
/// `=` here is one gcc treats as a name of its own (`-Wshadow=local`), not
/// a stem with a value; those are in [`JOINED`].
const PLAIN: &[(&str, Effect)] = &[
    ("abi", Accepted),
    ("address", Accepted),
    ("address-of-packed-member", Accepted),
    ("aggregate-return", Accepted),
    ("all", Accepted),
    ("alloc-zero", Accepted),
    ("alloca", Accepted),
    ("arith-conversion", Accepted),
    ("array-bounds", Accepted),
    ("array-parameter", Accepted),
    ("attribute-alias", Accepted),
    ("attributes", Implemented),
    ("bad-function-cast", Accepted),
    ("bool-compare", Accepted),
    ("bool-operation", Accepted),
    ("builtin-declaration-mismatch", Accepted),
    ("builtin-macro-redefined", Accepted),
    ("c++-compat", Accepted),
    // c17's own: the "-std= ignored" driver warning.
    ("c17-dialect", Implemented),
    ("c90-c99-compat", Accepted),
    ("c99-c11-compat", Accepted),
    ("cast-align", Accepted),
    ("cast-align=strict", Accepted),
    ("cast-function-type", Accepted),
    ("cast-qual", Accepted),
    ("char-subscripts", Accepted),
    ("clobbered", Accepted),
    ("comment", Accepted),
    ("comments", Accepted),
    // gcc 14's, not 13's; the torture suite passes it (compile/pr106537-2).
    ("compare-distinct-pointer-types", Accepted),
    ("conversion", Accepted),
    ("cpp", Accepted),
    ("dangling-else", Accepted),
    ("dangling-pointer", Accepted),
    ("date-time", Accepted),
    ("declaration-after-statement", Accepted),
    ("deprecated", Accepted),
    ("deprecated-declarations", Accepted),
    ("designated-init", Accepted),
    ("disabled-optimization", Accepted),
    ("discarded-array-qualifiers", Accepted),
    ("discarded-qualifiers", Accepted),
    ("div-by-zero", Accepted),
    ("double-promotion", Accepted),
    ("duplicate-decl-specifier", Accepted),
    ("duplicated-branches", Accepted),
    ("duplicated-cond", Accepted),
    ("empty-body", Accepted),
    ("endif-labels", Accepted),
    ("enum-compare", Accepted),
    ("enum-conversion", Accepted),
    ("enum-int-mismatch", Accepted),
    ("error", Implemented),
    ("error-implicit-function-declaration", Accepted),
    ("expansion-to-defined", Accepted),
    ("extra", Accepted),
    ("float-conversion", Accepted),
    ("float-equal", Accepted),
    ("format", Accepted),
    ("format-contains-nul", Accepted),
    ("format-extra-args", Accepted),
    ("format-nonliteral", Accepted),
    ("format-overflow", Accepted),
    ("format-security", Accepted),
    ("format-signedness", Accepted),
    ("format-truncation", Accepted),
    ("format-y2k", Accepted),
    ("format-zero-length", Accepted),
    ("frame-address", Accepted),
    ("free-nonheap-object", Accepted),
    ("ignored-attributes", Accepted),
    ("ignored-qualifiers", Accepted),
    ("implicit", Accepted),
    ("implicit-fallthrough", Accepted),
    ("implicit-function-declaration", Accepted),
    ("implicit-int", Accepted),
    ("incompatible-pointer-types", Accepted),
    ("infinite-recursion", Accepted),
    ("init-self", Accepted),
    ("inline", Accepted),
    ("int-conversion", Accepted),
    ("int-in-bool-context", Accepted),
    ("int-to-pointer-cast", Accepted),
    ("invalid-memory-model", Implemented),
    ("jump-misses-init", Accepted),
    ("logical-not-parentheses", Accepted),
    ("logical-op", Accepted),
    ("long-long", Accepted),
    ("main", Accepted),
    ("maybe-uninitialized", Accepted),
    ("memset-elt-size", Accepted),
    ("memset-transposed-args", Accepted),
    ("misleading-indentation", Accepted),
    ("missing-attributes", Accepted),
    ("missing-braces", Accepted),
    ("missing-declarations", Accepted),
    ("missing-field-initializers", Accepted),
    ("missing-format-attribute", Accepted),
    ("missing-include-dirs", Accepted),
    ("missing-noreturn", Accepted),
    ("missing-parameter-type", Accepted),
    ("missing-prototypes", Accepted),
    ("multichar", Accepted),
    ("multistatement-macros", Accepted),
    ("narrowing", Accepted),
    ("nested-externs", Accepted),
    ("nonnull", Accepted),
    ("nonnull-compare", Accepted),
    ("normalized", Accepted),
    ("null-dereference", Accepted),
    ("old-style-declaration", Accepted),
    ("old-style-definition", Accepted),
    ("overflow", Implemented),
    ("overlength-strings", Accepted),
    ("override-init", Accepted),
    ("packed", Accepted),
    ("packed-not-aligned", Accepted),
    ("padded", Accepted),
    ("parentheses", Accepted),
    ("pedantic", Implemented),
    ("pointer-arith", Accepted),
    ("pointer-sign", Accepted),
    ("pointer-to-int-cast", Accepted),
    ("pragmas", Accepted),
    ("psabi", Accepted),
    ("redundant-decls", Accepted),
    ("restrict", Accepted),
    ("return-local-addr", Accepted),
    ("return-type", Accepted),
    ("scalar-storage-order", Implemented),
    ("sequence-point", Accepted),
    ("shadow", Accepted),
    ("shadow=compatible-local", Accepted),
    ("shadow=global", Accepted),
    ("shadow=local", Accepted),
    ("shift-count-negative", Implemented),
    ("shift-count-overflow", Implemented),
    ("shift-negative-value", Accepted),
    ("shift-overflow", Accepted),
    ("sign-compare", Accepted),
    ("sign-conversion", Accepted),
    ("sizeof-array-argument", Accepted),
    ("sizeof-array-div", Accepted),
    ("sizeof-pointer-div", Accepted),
    ("sizeof-pointer-memaccess", Accepted),
    ("stack-protector", Accepted),
    ("strict-aliasing", Accepted),
    ("strict-overflow", Accepted),
    ("strict-prototypes", Accepted),
    ("string-compare", Accepted),
    ("stringop-overflow", Accepted),
    ("stringop-overread", Accepted),
    ("stringop-truncation", Accepted),
    ("suggest-attribute=cold", Accepted),
    ("suggest-attribute=const", Accepted),
    ("suggest-attribute=format", Accepted),
    ("suggest-attribute=malloc", Accepted),
    ("suggest-attribute=noreturn", Accepted),
    ("suggest-attribute=pure", Accepted),
    ("switch", Accepted),
    ("switch-bool", Accepted),
    ("switch-default", Accepted),
    ("switch-enum", Accepted),
    ("switch-outside-range", Implemented),
    ("switch-unreachable", Accepted),
    ("system-headers", Accepted),
    ("tautological-compare", Accepted),
    ("traditional", Accepted),
    ("traditional-conversion", Accepted),
    ("trampolines", Accepted),
    ("trigraphs", Accepted),
    ("type-limits", Accepted),
    ("undef", Accepted),
    ("uninitialized", Accepted),
    ("unknown-pragmas", Accepted),
    ("unreachable-code", Accepted),
    ("unsafe-loop-optimizations", Accepted),
    ("unsuffixed-float-constants", Accepted),
    ("unused", Accepted),
    ("unused-but-set-parameter", Accepted),
    ("unused-but-set-variable", Accepted),
    ("unused-const-variable", Accepted),
    ("unused-function", Accepted),
    ("unused-label", Accepted),
    ("unused-local-typedefs", Accepted),
    ("unused-macros", Accepted),
    ("unused-parameter", Accepted),
    ("unused-result", Accepted),
    ("unused-value", Accepted),
    ("unused-variable", Accepted),
    ("use-after-free", Accepted),
    ("varargs", Implemented),
    ("variadic-macros", Accepted),
    ("vla", Accepted),
    ("vla-parameter", Accepted),
    ("volatile-register-var", Accepted),
    ("write-strings", Accepted),
    ("zero-length-bounds", Accepted),
];

/// The value a [`JOINED`] option takes after its `=`.
#[derive(Clone, Copy, Debug)]
enum Value {
    /// An integer from 0 to the bound.
    Level(u64),
    /// Any non-negative integer.
    Unsigned,
    /// A byte count, optionally with a unit (`64KiB`).
    Size,
    /// One of these words.
    Choice(&'static [&'static str]),
}

/// The options that take a value, `-W<stem>=<value>`, sorted by stem. gcc
/// rejects a `-Wno-` spelling with a value; [`Value::Size`] stems take a
/// bare `-Wno-<stem>`, and the others' bare forms are in [`PLAIN`].
const JOINED: &[(&str, Value)] = &[
    ("abi", Value::Unsigned),
    ("alloc-size-larger-than", Value::Size),
    ("alloca-larger-than", Value::Size),
    ("array-bounds", Value::Level(2)),
    ("attribute-alias", Value::Level(2)),
    ("dangling-pointer", Value::Level(2)),
    ("format", Value::Level(2)),
    ("format-overflow", Value::Level(2)),
    ("format-truncation", Value::Level(2)),
    ("frame-larger-than", Value::Size),
    ("implicit-fallthrough", Value::Level(5)),
    ("larger-than", Value::Size),
    ("normalized", Value::Choice(&["id", "nfc", "nfkc", "none"])),
    ("shift-overflow", Value::Level(2)),
    ("stack-usage", Value::Size),
    ("strict-aliasing", Value::Level(3)),
    ("strict-overflow", Value::Level(5)),
    ("stringop-overflow", Value::Level(4)),
    ("unused-const-variable", Value::Level(2)),
    ("use-after-free", Value::Level(3)),
    ("vla-larger-than", Value::Size),
];

/// The units gcc takes after a [`Value::Size`] count.
const SIZE_UNITS: &[&str] = &[
    "kB", "KB", "KiB", "kiB", "MB", "MiB", "GB", "GiB", "TB", "TiB", "PB", "PiB", "EB", "EiB",
];

/// What becomes of one `-W<name>` option.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Verdict {
    /// Accepted: one of c17's own, or a gcc name with no effect here.
    Known(Effect),
    /// Not one of the warning options: `-Wl,`, `-Wa,` and `-Wp,` pass
    /// options to another program.
    PassThrough,
    /// `-Wno-<name>` for a name not in the table. gcc accepts it in
    /// silence, and if the translation unit has diagnostics, notes that it
    /// may have been meant to silence one.
    UnknownNegation,
    /// Refused before compiling, with these lines (an error, then any
    /// notes), as gcc's driver refuses it.
    DriverError(Vec<String>),
    /// Refused as gcc's compiler proper refuses an unknown `-Werror=<name>`
    /// -- after the driver's errors, if there are any.
    CompilerError(String),
}

/// The entry for `name` in [`PLAIN`].
fn plain(name: &str) -> Option<Effect> {
    PLAIN
        .binary_search_by(|(n, _)| n.cmp(&name))
        .ok()
        .map(|i| PLAIN[i].1)
}

/// The entry for `stem` in [`JOINED`].
fn joined(stem: &str) -> Option<Value> {
    JOINED
        .binary_search_by(|(s, _)| s.cmp(&stem))
        .ok()
        .map(|i| JOINED[i].1)
}

/// The text gcc gives for an option it does not know.
fn unrecognized(name: &str) -> Verdict {
    Verdict::DriverError(vec![format!(
        "error: unrecognized command-line option '-W{name}'"
    )])
}

/// Classify the option `-W<name>`.
pub fn classify(name: &str) -> Verdict {
    if name.is_empty() || ["l,", "a,", "p,"].iter().any(|p| name.starts_with(p)) {
        // Bare `-W` is gcc's old spelling of `-Wextra`.
        return if name.is_empty() {
            Verdict::Known(Accepted)
        } else {
            Verdict::PassThrough
        };
    }
    for (prefix, option) in [("error=", "-Werror="), ("no-error=", "-Wno-error=")] {
        if let Some(target) = name.strip_prefix(prefix) {
            return classify_werror(option, target);
        }
    }
    if name == "no-error" {
        return Verdict::Known(Implemented);
    }
    if let Some(negated) = name.strip_prefix("no-") {
        return classify_negation(name, negated);
    }
    if let Some(effect) = plain(name) {
        return Verdict::Known(effect);
    }
    match name.split_once('=') {
        Some((stem, value)) => match joined(stem) {
            Some(kind) => check_value(stem, kind, value),
            None => unrecognized(name),
        },
        None => unrecognized(name),
    }
}

/// `-Wno-<negated>`.
fn classify_negation(name: &str, negated: &str) -> Verdict {
    if negated.is_empty() {
        return unrecognized(name);
    }
    if let Some(effect) = plain(negated) {
        return Verdict::Known(effect);
    }
    if let Some(Value::Size) = joined(negated) {
        return Verdict::Known(Accepted);
    }
    match negated.split_once('=') {
        // A value on a known stem: gcc knows the option, and it has no
        // negative form.
        Some((stem, _)) if joined(stem).is_some() => unrecognized(name),
        _ => Verdict::UnknownNegation,
    }
}

/// `-Werror=<target>` or `-Wno-error=<target>`: gcc asks only that
/// `-W<target>` names an option, not that a value on it is good.
fn classify_werror(option: &str, target: &str) -> Verdict {
    if target.is_empty() {
        return Verdict::DriverError(vec![format!("error: missing argument to '{option}'")]);
    }
    let known = match plain(target) {
        Some(effect) => Some(effect),
        None => target
            .split_once('=')
            .and_then(|(stem, _)| joined(stem))
            .map(|_| Accepted),
    };
    match known {
        Some(effect) => Verdict::Known(effect),
        None => {
            Verdict::CompilerError(format!("error: '{option}{target}': no option '-W{target}'"))
        }
    }
}

/// `-W<stem>=<value>` for a stem in [`JOINED`].
fn check_value(stem: &str, kind: Value, value: &str) -> Verdict {
    if value.is_empty() {
        return Verdict::DriverError(vec![format!("error: missing argument to '-W{stem}='")]);
    }
    let all_digits = |s: &str| !s.is_empty() && s.bytes().all(|b| b.is_ascii_digit());
    let fault = match kind {
        Value::Unsigned | Value::Level(_) if !all_digits(value) => Some(format!(
            "argument to '-W{stem}=' should be a non-negative integer"
        )),
        // Too long for a u64 is past every bound.
        Value::Level(max) if value.parse::<u64>().map_or(true, |n| n > max) => Some(format!(
            "argument to '-W{stem}=' is not between 0 and {max}"
        )),
        Value::Size => {
            let digits = value.trim_end_matches(|c: char| !c.is_ascii_digit());
            let unit = &value[digits.len()..];
            (!all_digits(digits) || !(unit.is_empty() || SIZE_UNITS.contains(&unit))).then(|| {
                format!(
                    "argument to '-W{stem}=' should be a non-negative integer \
                     optionally followed by a size unit"
                )
            })
        }
        Value::Choice(words) if !words.contains(&value) => {
            return Verdict::DriverError(vec![
                format!("error: argument '{value}' to '-W{stem}' not recognized"),
                format!(
                    "note: valid arguments to '-W{stem}=' are: {}",
                    words.join(" ")
                ),
            ]);
        }
        _ => None,
    };
    match fault {
        Some(text) => Verdict::DriverError(vec![format!("error: {text}")]),
        None => Verdict::Known(Accepted),
    }
}

/// The note gcc gives, once a translation unit has had a diagnostic, for
/// each `-Wno-<name>` it did not know.
pub fn unknown_negation_note(name: &str) -> String {
    format!(
        "note: unrecognized command-line option '-W{name}' \
         may have been intended to silence earlier diagnostics"
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn driver_error(name: &str) -> String {
        match classify(name) {
            Verdict::DriverError(lines) => lines.join("\n"),
            other => panic!("-W{name}: {other:?}"),
        }
    }

    #[test]
    fn tables_are_sorted_and_unique() {
        assert!(PLAIN.windows(2).all(|w| w[0].0 < w[1].0));
        assert!(JOINED.windows(2).all(|w| w[0].0 < w[1].0));
    }

    /// Every name the warning-emitting code asks about is one the table
    /// says c17 implements.
    #[test]
    fn implemented_groups_are_in_the_table() {
        for name in [
            "attributes",
            "c17-dialect",
            "invalid-memory-model",
            "overflow",
            "pedantic",
            "scalar-storage-order",
            "shift-count-negative",
            "shift-count-overflow",
            "switch-outside-range",
            "varargs",
            "error",
        ] {
            assert_eq!(classify(name), Verdict::Known(Implemented), "{name}");
            assert_eq!(
                classify(&format!("no-{name}")),
                Verdict::Known(Implemented),
                "{name}"
            );
        }
        assert_eq!(classify("no-error"), Verdict::Known(Implemented));
    }

    #[test]
    fn known_names() {
        for name in [
            "",
            "all",
            "extra",
            "format",
            "format=2",
            "format=02",
            "format-security",
            "no-unused-parameter",
            "strict-overflow=5",
            "larger-than=100",
            "larger-than=10KiB",
            "larger-than=18446744073709551616",
            "no-larger-than",
            "normalized=nfc",
            "shadow=local",
            "no-shadow=local",
            "no-suggest-attribute=format",
            "abi=99",
            "error=format-security",
            "error=format=9",
            "no-error=maybe-uninitialized",
        ] {
            assert_eq!(classify(name), Verdict::Known(Accepted), "-W{name}");
        }
    }

    #[test]
    fn pass_through() {
        for name in ["l,-z,now", "a,--noexecstack", "p,-MD,x.d"] {
            assert_eq!(classify(name), Verdict::PassThrough, "-W{name}");
        }
    }

    #[test]
    fn unknown_names_get_gcc_text() {
        for name in ["foo", "foo=3", "all=1", "larger-than", "shadow=bad", "no-"] {
            assert_eq!(
                driver_error(name),
                format!("error: unrecognized command-line option '-W{name}'")
            );
        }
        assert_eq!(classify("no-foo"), Verdict::UnknownNegation);
        assert_eq!(classify("no-foo=3"), Verdict::UnknownNegation);
        assert_eq!(
            driver_error("no-format=2"),
            "error: unrecognized command-line option '-Wno-format=2'"
        );
        assert_eq!(
            classify("error=foo"),
            Verdict::CompilerError("error: '-Werror=foo': no option '-Wfoo'".into())
        );
        assert_eq!(
            classify("no-error=no-unused"),
            Verdict::CompilerError("error: '-Wno-error=no-unused': no option '-Wno-unused'".into())
        );
        assert_eq!(
            driver_error("error="),
            "error: missing argument to '-Werror='"
        );
    }

    #[test]
    fn values_are_checked() {
        assert_eq!(
            driver_error("format=3"),
            "error: argument to '-Wformat=' is not between 0 and 2"
        );
        assert_eq!(
            driver_error("format=99999999999999999999"),
            "error: argument to '-Wformat=' is not between 0 and 2"
        );
        for v in ["-1", "+1", "abc"] {
            assert_eq!(
                driver_error(&format!("format={v}")),
                "error: argument to '-Wformat=' should be a non-negative integer"
            );
        }
        assert_eq!(
            driver_error("format="),
            "error: missing argument to '-Wformat='"
        );
        for v in ["10k", "1M", "10B", "none", "-1", "kB"] {
            assert_eq!(
                driver_error(&format!("larger-than={v}")),
                "error: argument to '-Wlarger-than=' should be a non-negative \
                 integer optionally followed by a size unit",
                "{v}"
            );
        }
        assert_eq!(
            driver_error("normalized=bad"),
            "error: argument 'bad' to '-Wnormalized' not recognized\n\
             note: valid arguments to '-Wnormalized=' are: id nfc nfkc none"
        );
    }
}
