//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `#pragma GCC diagnostic`: c17 does not act on it, but what gcc 13 warns
// about in one -- an option it does not know, one that controls no warning,
// a C++ one, a missing or unknown kind or option -- is a mistake in the
// program, reported in gcc's words under `-Wpragmas`. Every line here is
// gcc's, position included.
//

use crate::test_compile::compile;

/// Every diagnostic of `src`, without the file name.
fn diagnostics(name: &str, src: &str, flags: &[&str]) -> Vec<String> {
    let prefix = format!("{name}.c:");
    compile(name, src, flags)
        .stderr
        .lines()
        .filter_map(|l| l.strip_prefix(&prefix))
        .map(str::to_string)
        .collect()
}

#[test]
fn diagnostic_pragma_options_as_gcc_reads_them() {
    let src = r#"#pragma GCC diagnostic ignored "-Wbogus"
#pragma GCC diagnostic warning "-Wunused"
#pragma GCC diagnostic error "-Wno-unused"
#pragma GCC diagnostic ignored "-fstack-protector"
#pragma GCC diagnostic ignored "-Wl,foo"
#pragma GCC diagnostic ignored "-Wformat=9"
#pragma GCC diagnostic ignored "-Wclass-memaccess"
#pragma GCC diagnostic ignored "-Werror"
#pragma GCC diagnostic ignored "-Wunused" "junk"
#pragma GCC diagnostic ignored -Wunused
#pragma GCC diagnostic ignored
#pragma GCC diagnostic bogus "-Wunused"
#pragma GCC diagnostic
#pragma GCC diagnostic push
#pragma GCC diagnostic pop
#pragma GCC diagnostic ignored "-Wno-error=unused"
#pragma GCC diagnostic ignored "-fbogus"
#pragma GCC diagnostic ignored "foo"
#pragma GCC diagnostic ignored_attributes "vendor::attr,other::"
#pragma GCC diagnostic ignored_attributes
_Pragma("GCC diagnostic ignored \"-Wbogus2\"")
int x;
"#;
    let w = "[-Wpragmas]";
    assert_eq!(
        diagnostics("diag_pragma", src, &[]),
        [
            format!("1:32: warning: unknown option after '#pragma GCC diagnostic' kind {w}"),
            format!("3:30: warning: unknown option after '#pragma GCC diagnostic' kind {w}"),
            format!("4:32: warning: '-fstack-protector' is not an option that controls warnings {w}"),
            format!("5:32: warning: '-Wl,foo' is not an option that controls warnings {w}"),
            format!("7:32: warning: option '-Wclass-memaccess' is valid for C++/ObjC++ but not for C {w}"),
            format!("8:32: warning: '-Werror' is not an option that controls warnings {w}"),
            format!("9:32: warning: unknown option after '#pragma GCC diagnostic' kind {w}"),
            "9:32: note: did you mean '-Wunused'?".to_string(),
            format!("10:32: warning: missing option after '#pragma GCC diagnostic' kind {w}"),
            format!("11:31: warning: missing option after '#pragma GCC diagnostic' kind {w}"),
            format!("12:24: warning: expected 'error', 'warning', 'ignored', 'push', 'pop', 'ignored_attributes' after '#pragma GCC diagnostic' {w}"),
            format!("13:23: warning: missing 'error', 'warning', 'ignored', 'push', 'pop', or 'ignored_attributes' after '#pragma GCC diagnostic' {w}"),
            format!("16:32: warning: unknown option after '#pragma GCC diagnostic' kind {w}"),
            "16:32: note: did you mean '-Wno-error='?".to_string(),
            format!("17:32: warning: unknown option after '#pragma GCC diagnostic' kind {w}"),
            format!("18:32: warning: 'foo' is not an option that controls warnings {w}"),
            format!("20:42: warning: missing attribute name after '#pragma GCC diagnostic ignored_attributes' {w}"),
            format!("21:1: warning: unknown option after '#pragma GCC diagnostic' kind {w}"),
        ]
    );
    // `-Wno-pragmas` silences them.
    assert!(diagnostics("diag_pragma_quiet", src, &["-Wno-pragmas"]).is_empty());
}

/// An `ignored_attributes` list whose item is not `vendor::` or
/// `vendor::name` is gcc's error.
#[test]
fn diagnostic_pragma_ignored_attributes_format() {
    for bad in ["foo", "::attr", "ok::,bad"] {
        let src = format!("#pragma GCC diagnostic ignored_attributes \"{bad}\"\nint x;\n");
        assert_eq!(
            diagnostics("diag_ignored_attrs", &src, &[]),
            [
                "1:9: error: wrong argument to ignored attributes",
                "1:9: note: valid format is 'ns::attr' or 'ns::'",
            ],
            "{bad}"
        );
    }
}
