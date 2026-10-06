//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `-Werror`, `-Werror=<name>`, `-Wno-error` and `-Wno-error=<name>`, checked
// against gcc 13: which warnings become errors, the `[-Werror...]` tag each
// carries, and the closing "warnings being treated as errors" line.
//

use crate::test_compile::{compile, compile_accepted, compile_rejected_with};

/// A warning c17 has no option name for: gcc's `-Wincompatible-pointer-types`.
const INCOMPATIBLE: &str = "char c; int *g(void){return &c;}\n";
const INCOMPATIBLE_MSG: &str =
    "returning 'char *' from a function with return type 'int *' incompatible pointer type";

/// One `-Woverflow` warning and one `-Wattributes` warning. The overflow is
/// found after parsing, which an error stops, so where the attribute's
/// warning is an error the overflow is tested alone.
const OVERFLOW_AND_ATTRIBUTE: &str = "int x = 1e10;\nint y __attribute__((bogus_attr));\n";
const OVERFLOW: &str = "int x = 1e10;\n";
const ATTRIBUTE: &str = "int y __attribute__((bogus_attr));\n";
const OVERFLOW_MSG: &str = "overflow in conversion from 'double' to 'int' changes value";
const ATTRIBUTE_MSG: &str = "'bogus_attr' attribute directive ignored";

/// A default pedwarn, and one given only under `-Wpedantic`.
const PEDWARNS: &str = "struct s { int a };\nint f(...);\n";
const SEMICOLON_MSG: &str = "no semicolon at end of struct or union";
const NAMED_ARG_MSG: &str = "ISO C requires a named argument before '...'";

const ALL: &str = "c17: all warnings being treated as errors";
const SOME: &str = "c17: some warnings being treated as errors";

#[track_caller]
fn has(stderr: &str, line: &str) {
    assert!(stderr.contains(line), "expected {line:?} in:\n{stderr}");
}

#[track_caller]
fn lacks(stderr: &str, text: &str) {
    assert!(!stderr.contains(text), "unexpected {text:?} in:\n{stderr}");
}

/// An unnamed warning is an error tagged `[-Werror]` under `-Werror`, and
/// the unit ends with gcc's "all warnings" line.
#[test]
fn werror_makes_a_warning_an_error() {
    let plain = compile_accepted("werror_plain", INCOMPATIBLE, &[]);
    has(&plain, &format!("warning: {INCOMPATIBLE_MSG}\n"));
    lacks(&plain, "-Werror");

    let strict = compile_rejected_with("werror_all", INCOMPATIBLE, &["-Werror"]);
    has(&strict, &format!("error: {INCOMPATIBLE_MSG} [-Werror]\n"));
    has(&strict, &format!("{ALL}\n"));
    lacks(&strict, "warning:");
}

/// `-w` gives nothing, so `-Werror` has nothing to promote.
#[test]
fn werror_with_w_is_silent() {
    for flags in [&["-Werror", "-w"][..], &["-w", "-Werror"]] {
        let c = compile("werror_w", INCOMPATIBLE, flags);
        assert!(c.success && c.stderr.is_empty(), "{flags:?}:\n{}", c.stderr);
    }
}

/// A warning in a system header is not shown, and not promoted either.
#[test]
fn werror_does_not_reach_a_system_header() {
    let src = "# 1 \"sys.h\" 1 3\n\
               char c; int *g(void){return &c;}\n\
               # 3 \"main.c\" 2\n\
               int z;\n";
    let c = compile("werror_system_header", src, &["-Werror"]);
    assert!(c.success && c.stderr.is_empty(), "{}", c.stderr);
}

/// `-Werror=<name>` promotes that group alone, with `[-Werror=<name>]`, and
/// the closing line says "some".
#[test]
fn werror_named_promotes_only_its_group() {
    let c = compile_rejected_with(
        "werror_overflow",
        OVERFLOW_AND_ATTRIBUTE,
        &["-Werror=overflow"],
    );
    has(&c, &format!("error: {OVERFLOW_MSG} [-Werror=overflow]\n"));
    has(&c, &format!("warning: {ATTRIBUTE_MSG}\n"));
    has(&c, &format!("{SOME}\n"));
    lacks(&c, ALL);

    let only_attrs = compile_rejected_with("werror_attributes", ATTRIBUTE, &["-Werror=attributes"]);
    has(
        &only_attrs,
        &format!("error: {ATTRIBUTE_MSG} [-Werror=attributes]\n"),
    );
    let overflow = compile_accepted("werror_attributes_only", OVERFLOW, &["-Werror=attributes"]);
    has(&overflow, &format!("warning: {OVERFLOW_MSG}\n"));
}

/// Under `-Werror` a named warning carries its own name, and
/// `-Wno-error=<name>` leaves that group a warning.
#[test]
fn werror_with_a_group_excluded() {
    for (src, line) in [
        (
            OVERFLOW,
            format!("error: {OVERFLOW_MSG} [-Werror=overflow]\n"),
        ),
        (
            ATTRIBUTE,
            format!("error: {ATTRIBUTE_MSG} [-Werror=attributes]\n"),
        ),
    ] {
        let all = compile_rejected_with("werror_named_all", src, &["-Werror"]);
        has(&all, &line);
        has(&all, &format!("{ALL}\n"));
    }

    let c = compile_rejected_with(
        "werror_no_error_attributes",
        OVERFLOW_AND_ATTRIBUTE,
        &["-Werror", "-Wno-error=attributes"],
    );
    has(&c, &format!("error: {OVERFLOW_MSG} [-Werror=overflow]\n"));
    has(&c, &format!("warning: {ATTRIBUTE_MSG}\n"));
    has(&c, &format!("{ALL}\n"));

    // Excluding the only group that warns leaves nothing to fail on.
    let ok = compile_accepted(
        "werror_excluded",
        ATTRIBUTE,
        &["-Werror", "-Wno-error=attributes"],
    );
    has(&ok, &format!("warning: {ATTRIBUTE_MSG}\n"));
    lacks(&ok, "treated as errors");
}

/// `-Werror=<name>` also turns the group on, as gcc's does; a later
/// `-Wno-<name>` turns it off again.
#[test]
fn werror_named_enables_its_group() {
    let c = compile_rejected_with(
        "werror_reenables",
        OVERFLOW_AND_ATTRIBUTE,
        &["-Wno-overflow", "-Werror=overflow"],
    );
    has(&c, &format!("error: {OVERFLOW_MSG} [-Werror=overflow]\n"));

    let off = compile_accepted(
        "werror_then_off",
        OVERFLOW_AND_ATTRIBUTE,
        &["-Werror=overflow", "-Wno-overflow"],
    );
    lacks(&off, OVERFLOW_MSG);
    has(&off, &format!("warning: {ATTRIBUTE_MSG}\n"));

    // `-Werror` with the group off: only the other one is left to promote.
    let off = compile_accepted("werror_group_off", OVERFLOW, &["-Werror", "-Wno-overflow"]);
    assert!(off.is_empty(), "{off}");
}

/// The options fold in command-line order: the last of `-Werror` and
/// `-Wno-error` wins, and neither undoes a named verdict.
#[test]
fn werror_options_fold_in_order() {
    let c = compile_rejected_with("werror_last", INCOMPATIBLE, &["-Wno-error", "-Werror"]);
    has(&c, &format!("error: {INCOMPATIBLE_MSG} [-Werror]\n"));

    let ok = compile_accepted("werror_undone", INCOMPATIBLE, &["-Werror", "-Wno-error"]);
    has(&ok, &format!("warning: {INCOMPATIBLE_MSG}\n"));
    lacks(&ok, "-Werror");

    let named = compile_rejected_with(
        "werror_named_survives",
        OVERFLOW_AND_ATTRIBUTE,
        &["-Werror=overflow", "-Wno-error"],
    );
    has(
        &named,
        &format!("error: {OVERFLOW_MSG} [-Werror=overflow]\n"),
    );
    has(&named, &format!("{SOME}\n"));

    let excluded_first = compile_accepted(
        "werror_excluded_first",
        OVERFLOW,
        &["-Wno-error=overflow", "-Werror"],
    );
    has(&excluded_first, &format!("warning: {OVERFLOW_MSG}\n"));
    let c = compile_rejected_with(
        "werror_excluded_other",
        ATTRIBUTE,
        &["-Wno-error=overflow", "-Werror"],
    );
    has(
        &c,
        &format!("error: {ATTRIBUTE_MSG} [-Werror=attributes]\n"),
    );
}

/// gcc promotes pedwarns as it does any warning: a default one as
/// `[-Werror]`, a `-Wpedantic` one as `[-Werror=pedantic]`.
#[test]
fn werror_promotes_pedwarns() {
    let c = compile_rejected_with("werror_pedantic", PEDWARNS, &["-pedantic", "-Werror"]);
    has(&c, &format!("error: {SEMICOLON_MSG} [-Werror]\n"));
    has(&c, &format!("error: {NAMED_ARG_MSG} [-Werror=pedantic]\n"));
    has(&c, &format!("{ALL}\n"));

    // Without `-pedantic`, only the default pedwarn is given to promote.
    let c = compile_rejected_with("werror_default_pedwarn", PEDWARNS, &["-Werror"]);
    has(&c, &format!("error: {SEMICOLON_MSG} [-Werror]\n"));
    lacks(&c, NAMED_ARG_MSG);

    // `-Werror=pedantic` turns `-Wpedantic` on and promotes it alone.
    let c = compile_rejected_with("werror_eq_pedantic", PEDWARNS, &["-Werror=pedantic"]);
    has(&c, &format!("warning: {SEMICOLON_MSG}\n"));
    has(&c, &format!("error: {NAMED_ARG_MSG} [-Werror=pedantic]\n"));
    has(&c, &format!("{SOME}\n"));

    let c = compile_rejected_with(
        "werror_no_error_pedantic",
        PEDWARNS,
        &["-pedantic", "-Werror", "-Wno-error=pedantic"],
    );
    has(&c, &format!("error: {SEMICOLON_MSG} [-Werror]\n"));
    has(&c, &format!("warning: {NAMED_ARG_MSG}\n"));
}

/// `-pedantic-errors` is unchanged by `-Werror` and `-Wno-error`: plain
/// errors, no tag, no closing line.
#[test]
fn pedantic_errors_unchanged_by_werror() {
    for flags in [
        &["-pedantic-errors"][..],
        &["-pedantic-errors", "-Werror"],
        &["-pedantic-errors", "-Wno-error"],
    ] {
        let c = compile("pedantic_errors_werror", PEDWARNS, flags);
        assert!(!c.success, "{flags:?}:\n{}", c.stderr);
        has(&c.stderr, &format!("error: {SEMICOLON_MSG}\n"));
        has(&c.stderr, &format!("error: {NAMED_ARG_MSG}\n"));
        lacks(&c.stderr, "-Werror");
        lacks(&c.stderr, "treated as errors");
    }
}
