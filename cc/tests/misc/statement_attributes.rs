//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Attributes written as statements
//

use crate::common::{
    compile_and_run, compile_and_run_optimized, compile_expect_error, compile_expect_no_diagnostic,
    compile_expect_ok, compile_expect_warning, compile_expect_warning_with,
};

/// `__attribute__((fallthrough));` is a null statement carrying an attribute,
/// the GNU spelling of C23's `[[fallthrough]]`, and the way code that builds
/// with `-Wimplicit-fallthrough` marks a deliberate fall into the next case.
/// `__has_attribute(fallthrough)` answers 1, so code that probes for it uses
/// it -- and c17 rejected the statement as "declaration declares nothing".
#[test]
fn misc_fallthrough_statement_attribute() {
    let code = r#"
int f(int x) {
    int r = 0;
    switch (x) {
    case 1: r += 1; __attribute__((fallthrough));
    case 2: r += 2; __attribute__((__fallthrough__));
    case 3: r += 4; break;
    default: r = 100;
    }
    return r;
}
int main(void) {
#if !__has_attribute(fallthrough)
    return 99;
#endif
    return !(f(1) == 7 && f(2) == 6 && f(3) == 4 && f(9) == 100);
}
"#;
    assert_eq!(compile_and_run("fallthrough_stmt", code, &[]), 0);
    assert_eq!(compile_and_run_optimized("fallthrough_stmt_opt", code), 0);
}

/// Where gcc is silent: directly before a `case` or `default`, before an
/// ordinary label, and before a brace a `case` may be reached through.
#[test]
fn misc_fallthrough_statement_before_a_label_is_silent() {
    let code = r#"
int f(int x) {
    switch (x) {
    case 1: x++; __attribute__((fallthrough));
    case 2: x++; __attribute__((fallthrough));
    lab: case 3: x++; __attribute__((fallthrough));
    { case 4: x++; }
    case 5: x++; __attribute__((fallthrough));
    default: x++;
    }
    return x;
}
"#;
    compile_expect_no_diagnostic("fallthrough_stmt_quiet", code, "fallthrough");
}

/// A fallthrough statement with a statement after it falls into no label,
/// which gcc warns about in these words.
#[test]
fn misc_fallthrough_statement_not_before_a_label_warns() {
    let code = r#"
int f(int x) {
    switch (x) {
    case 1: __attribute__((fallthrough)); x++;
    case 2: x++;
    }
    return x;
}
"#;
    compile_expect_warning(
        "fallthrough_stmt_stray",
        code,
        "attribute 'fallthrough' not preceding a case label or default label",
    );
}

/// Outside every `switch` there is nothing to fall into, and gcc rejects it.
#[test]
fn misc_fallthrough_statement_outside_a_switch_is_rejected() {
    compile_expect_error(
        "fallthrough_stmt_no_switch",
        "int f(int x) { if (x) __attribute__((fallthrough)); return x; }\n",
        "invalid use of attribute 'fallthrough'",
    );
}

/// An attribute on a statement that is not null is not the fallthrough
/// statement: gcc reads it as a declaration and rejects it, and so does c17.
#[test]
fn misc_fallthrough_attribute_on_a_statement_is_rejected() {
    compile_expect_error(
        "fallthrough_on_stmt",
        "int f(int x) { switch (x) { case 1: x++; __attribute__((fallthrough)) x++;\n\
         case 2: x++; } return x; }\n",
        "",
    );
}

/// Beside `fallthrough`, another recognised attribute applies to nothing:
/// warned about in gcc's words, under `-Wattributes`.
#[test]
fn misc_fallthrough_statement_ignores_other_attributes() {
    let code = r#"
int f(int x) {
    switch (x) {
    case 1: x++; __attribute__((fallthrough, unused));
    case 2: x++;
    }
    return x;
}
"#;
    compile_expect_warning(
        "fallthrough_stmt_unused",
        code,
        "'unused' attribute ignored",
    );
    let stderr = compile_expect_warning_with(
        "fallthrough_stmt_unused_quiet",
        code,
        &["-Wno-attributes".to_string()],
    );
    assert!(!stderr.contains("attribute ignored"), "{stderr}");
}

/// Any other attribute list alone before `;` is an empty declaration -- in a
/// block and at file scope -- which gcc accepts with a warning.
#[test]
fn misc_attribute_declaration_is_empty() {
    compile_expect_warning(
        "attr_decl_block",
        "int f(int x) { __attribute__((unused)); return x; }\n",
        "empty declaration",
    );
    compile_expect_warning(
        "attr_decl_file",
        "__attribute__((unused));\nint f(int x) { return x; }\n",
        "empty declaration",
    );
}

/// A declaration that begins with an attribute is still a declaration, in a
/// block and at file scope.
#[test]
fn misc_attribute_led_declarations_still_compile() {
    let code = r#"
static int ran;
__attribute__((constructor)) static void init(void) { ran = 1; }
int main(void) {
    __attribute__((unused)) int x = 3;
    __attribute__((aligned(16))) int y = 4;
    if ((unsigned long)&y % 16 != 0)
        return 2;
    return !(ran == 1 && y == 4);
}
"#;
    compile_expect_ok("attr_led_decls", code);
    assert_eq!(compile_and_run("attr_led_decls_run", code, &[]), 0);
}
