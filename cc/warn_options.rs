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
    ("attribute-warning", Implemented),
    ("attributes", Implemented),
    ("bad-function-cast", Accepted),
    ("bool-compare", Accepted),
    ("bool-operation", Accepted),
    ("builtin-declaration-mismatch", Accepted),
    ("builtin-macro-redefined", Implemented),
    ("c++-compat", Accepted),
    // c17's own: the "-std= ignored" driver warning.
    ("c17-dialect", Implemented),
    // c17's own: "'-fX' is not supported"; see `f_options`.
    ("c17-unsupported-option", Implemented),
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
    ("cpp", Implemented),
    ("dangling-else", Accepted),
    ("dangling-pointer", Accepted),
    ("date-time", Accepted),
    ("declaration-after-statement", Accepted),
    ("deprecated", Accepted),
    ("deprecated-declarations", Accepted),
    ("designated-init", Accepted),
    ("disabled-optimization", Accepted),
    ("discarded-array-qualifiers", Accepted),
    ("discarded-qualifiers", Implemented),
    ("div-by-zero", Accepted),
    ("double-promotion", Accepted),
    ("duplicate-decl-specifier", Accepted),
    ("duplicated-branches", Accepted),
    ("duplicated-cond", Accepted),
    ("empty-body", Accepted),
    ("endif-labels", Implemented),
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
    ("format-diag", Accepted),
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
    ("implicit-function-declaration", Implemented),
    ("implicit-int", Implemented),
    ("incompatible-pointer-types", Implemented),
    ("infinite-recursion", Accepted),
    ("init-self", Accepted),
    ("inline", Accepted),
    ("int-conversion", Implemented),
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
    ("pointer-sign", Implemented),
    ("pointer-to-int-cast", Accepted),
    ("pragmas", Implemented),
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

/// The units gcc takes after a [`Value::Size`] count, with what each
/// multiplies it by.
const SIZE_UNITS: &[(&str, u64)] = &[
    ("kB", 1000),
    ("KB", 1000),
    ("KiB", 1 << 10),
    ("kiB", 1 << 10),
    ("MB", 1_000_000),
    ("MiB", 1 << 20),
    ("GB", 1_000_000_000),
    ("GiB", 1 << 30),
    ("TB", 1_000_000_000_000),
    ("TiB", 1 << 40),
    ("PB", 1_000_000_000_000_000),
    ("PiB", 1 << 50),
    ("EB", 1_000_000_000_000_000_000),
    ("EiB", 1 << 60),
];

/// The bytes a [`Value::Size`] value names -- digits, then optionally one
/// of [`SIZE_UNITS`] -- or `None` if it is not one. A count too large for
/// 64 bits is the largest there is, past any frame.
pub fn size_value(value: &str) -> Option<u64> {
    let digits = value.trim_end_matches(|c: char| !c.is_ascii_digit());
    if digits.is_empty() || !digits.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    let scale = match &value[digits.len()..] {
        "" => 1,
        unit => SIZE_UNITS.iter().find(|(u, _)| *u == unit)?.1,
    };
    Some(
        digits
            .parse::<u64>()
            .unwrap_or(u64::MAX)
            .saturating_mul(scale),
    )
}

/// The warning group a `-Werror=<target>` or `-Wno-error=<target>` names,
/// as gcc tags its warnings: an option with a value is its stem and `=`
/// (`-Werror=frame-larger-than=100` names `frame-larger-than=`), and any
/// other name is itself.
pub fn werror_group(target: &str) -> &str {
    match target.split_once('=') {
        Some((stem, _)) if plain(target).is_none() && joined(stem).is_some() => {
            &target[..=stem.len()]
        }
        _ => target,
    }
}

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
        Value::Size => size_value(value).is_none().then(|| {
            format!(
                "argument to '-W{stem}=' should be a non-negative integer \
                 optionally followed by a size unit"
            )
        }),
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

/// What gcc makes of the option a `#pragma GCC diagnostic` kind names.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PragmaOption {
    /// A warning option for C: the pragma applies to it.
    Warning,
    /// An option that is no warning (`-Werror`, `-fstack-protector`,
    /// `-O2`), or a string that is no option name.
    NotAWarning,
    /// A warning only C++ has: [`CXX_ONLY`].
    CxxOnly,
    /// No option gcc knows -- among them a `-Wno-` form, which gcc's pragma
    /// does not take.
    Unknown,
}

/// The C++ warnings a C header shared with C++ code may name, which gcc
/// knows and refuses for C. Not gcc's roster: the names such headers use.
const CXX_ONLY: &[&str] = &[
    "c++11-compat",
    "c++20-compat",
    "catch-value",
    "class-conversion",
    "class-memaccess",
    "comma-subscript",
    "conversion-null",
    "ctor-dtor-privacy",
    "dangling-reference",
    "delete-non-virtual-dtor",
    "deprecated-copy",
    "deprecated-copy-dtor",
    "deprecated-enum-enum-conversion",
    "deprecated-enum-float-conversion",
    "effc++",
    "extra-semi",
    "inaccessible-base",
    "init-list-lifetime",
    "invalid-offsetof",
    "literal-suffix",
    "mismatched-new-delete",
    "missing-template-keyword",
    "noexcept",
    "non-virtual-dtor",
    "old-style-cast",
    "overloaded-virtual",
    "pessimizing-move",
    "pmf-conversions",
    "range-loop-construct",
    "redundant-move",
    "register",
    "reorder",
    "sign-promo",
    "subobject-linkage",
    "suggest-override",
    "terminate",
    "useless-cast",
    "virtual-move-assign",
    "volatile",
    "zero-as-null-pointer-constant",
];

/// Classify the option `text` of `#pragma GCC diagnostic error`, `warning`
/// or `ignored`, as gcc does. A value after a warning's `=` is not checked
/// there, as it is on the command line.
pub fn pragma_option(text: &str) -> PragmaOption {
    let Some(name) = text.strip_prefix("-W") else {
        return match text.strip_prefix("-f") {
            Some(f)
                if !matches!(
                    crate::f_options::classify(f),
                    crate::f_options::Verdict::Known(_)
                ) =>
            {
                PragmaOption::Unknown
            }
            _ if text.is_empty() => PragmaOption::Unknown,
            _ => PragmaOption::NotAWarning,
        };
    };
    if name == "error"
        || name.starts_with("error=")
        || ["l,", "a,", "p,"].iter().any(|p| name.starts_with(p))
    {
        return PragmaOption::NotAWarning;
    }
    if name.is_empty() || plain(name).is_some() {
        return PragmaOption::Warning;
    }
    if CXX_ONLY.contains(&name) {
        return PragmaOption::CxxOnly;
    }
    match name.split_once('=') {
        Some((stem, _)) if joined(stem).is_some() => PragmaOption::Warning,
        _ => PragmaOption::Unknown,
    }
}

/// The warning option gcc would suggest for the unknown `text` (`-Wunusedx`
/// is "did you mean '-Wunused'?"), by gcc's spelling rule
/// (`spellcheck.cc`): the closest name by edit distance -- insertions,
/// deletions, substitutions and adjacent transpositions, a change of case
/// alone costing half -- within a cutoff of about a third of the longer
/// length. The names are this table's.
pub fn suggest(text: &str) -> Option<String> {
    let goal = text.strip_prefix('-')?;
    // gcc's candidates include each warning's negative form, so an exact
    // `-Wno-<name>` suggests nothing.
    let candidates = PLAIN
        .iter()
        .flat_map(|(n, _)| [format!("W{n}"), format!("Wno-{n}")])
        .chain(JOINED.iter().map(|(s, _)| format!("W{s}=")));
    let mut best: Option<(u32, String)> = None;
    for candidate in candidates {
        let d = edit_distance(goal, &candidate);
        if d == 0 {
            return None;
        }
        if d > edit_distance_cutoff(goal.len(), candidate.len()) {
            continue;
        }
        if best.as_ref().is_none_or(|(b, _)| d < *b) {
            best = Some((d, candidate));
        }
    }
    best.map(|(_, c)| format!("-{c}"))
}

/// gcc's edit distance unit: a change of case alone costs 1.
const BASE_COST: u32 = 2;

/// gcc's `get_edit_distance`: optimal string alignment distance, each edit
/// [`BASE_COST`], a substitution that changes only case 1.
fn edit_distance(s: &str, t: &str) -> u32 {
    let (s, t): (Vec<char>, Vec<char>) = (s.chars().collect(), t.chars().collect());
    let (n, m) = (s.len(), t.len());
    let mut d = vec![vec![0u32; m + 1]; n + 1];
    for (i, row) in d.iter_mut().enumerate() {
        row[0] = i as u32 * BASE_COST;
    }
    for (j, cell) in d[0].iter_mut().enumerate() {
        *cell = j as u32 * BASE_COST;
    }
    for i in 1..=n {
        for j in 1..=m {
            let sub = if s[i - 1] == t[j - 1] {
                0
            } else if s[i - 1].eq_ignore_ascii_case(&t[j - 1]) {
                1
            } else {
                BASE_COST
            };
            let mut best = (d[i - 1][j] + BASE_COST)
                .min(d[i][j - 1] + BASE_COST)
                .min(d[i - 1][j - 1] + sub);
            if i > 1 && j > 1 && s[i - 1] == t[j - 2] && s[i - 2] == t[j - 1] {
                best = best.min(d[i - 2][j - 2] + BASE_COST);
            }
            d[i][j] = best;
        }
    }
    d[n][m]
}

/// gcc's `get_edit_distance_cutoff`.
fn edit_distance_cutoff(goal_len: usize, candidate_len: usize) -> u32 {
    let max_len = goal_len.max(candidate_len) as u32;
    let min_len = goal_len.min(candidate_len) as u32;
    if max_len <= 1 {
        return 0;
    }
    if max_len - min_len <= 1 {
        return BASE_COST * (max_len / 3).max(1);
    }
    BASE_COST * (max_len + 2) / 3
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
        assert!(CXX_ONLY.windows(2).all(|w| w[0] < w[1]));
        // A C++ warning is no C one.
        assert!(CXX_ONLY.iter().all(|n| plain(n).is_none()));
    }

    /// `#pragma GCC diagnostic`'s option, as gcc 13 classifies it.
    #[test]
    fn pragma_options_as_gcc_classifies_them() {
        use PragmaOption::*;
        for (text, want) in [
            ("-Wunused", Warning),
            ("-W", Warning),
            ("-Wformat=9", Warning),
            ("-Wbogus", Unknown),
            ("-Wno-unused", Unknown),
            ("-Wbogus=3", Unknown),
            ("", Unknown),
            ("-fbogus", Unknown),
            ("-Werror", NotAWarning),
            ("-Werror=unused", NotAWarning),
            ("-Wl,x", NotAWarning),
            ("-fstack-protector", NotAWarning),
            ("-O2", NotAWarning),
            ("Wunused", NotAWarning),
            ("-Wold-style-cast", CxxOnly),
        ] {
            assert_eq!(pragma_option(text), want, "{text}");
        }
    }

    /// gcc's spelling suggestions: within the cutoff, none for an exact
    /// name, a negative form included.
    #[test]
    fn suggestions_follow_gccs_cutoff() {
        assert_eq!(suggest("-Wunusedjunk").as_deref(), Some("-Wunused"));
        assert_eq!(suggest("-Wunsued").as_deref(), Some("-Wunused"));
        assert_eq!(suggest("-Wno-unused"), None);
        assert_eq!(suggest("-Wbogus"), None);
        assert_eq!(edit_distance("ab", "ba"), BASE_COST);
        assert_eq!(edit_distance("A", "a"), 1);
    }

    /// Every name the warning-emitting code asks about is one the table
    /// says c17 implements.
    #[test]
    fn implemented_groups_are_in_the_table() {
        for name in [
            "attributes",
            "builtin-macro-redefined",
            "c17-dialect",
            "cpp",
            "discarded-qualifiers",
            "endif-labels",
            "incompatible-pointer-types",
            "int-conversion",
            "pointer-sign",
            "pragmas",
            "c17-unsupported-option",
            "implicit-function-declaration",
            "implicit-int",
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
    fn sizes_and_groups() {
        assert_eq!(size_value("100"), Some(100));
        assert_eq!(size_value("2KiB"), Some(2048));
        assert_eq!(size_value("2kB"), Some(2000));
        assert_eq!(size_value("99999999999999999999EiB"), Some(u64::MAX));
        for bad in ["", "KB", "1 KB", "1k", "-1"] {
            assert_eq!(size_value(bad), None, "{bad:?}");
        }
        assert_eq!(werror_group("frame-larger-than=100"), "frame-larger-than=");
        assert_eq!(werror_group("frame-larger-than="), "frame-larger-than=");
        assert_eq!(werror_group("format=2"), "format=");
        assert_eq!(werror_group("shadow=local"), "shadow=local");
        assert_eq!(werror_group("overflow"), "overflow");
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
