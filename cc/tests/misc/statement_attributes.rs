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

use crate::common::{compile_and_run, compile_and_run_optimized};

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
    // The matrix-level run is a section of misc_statement_attributes_mega.
    assert_eq!(compile_and_run_optimized("fallthrough_stmt_opt", code), 0);
}

/// Statement and declaration attributes at the matrix levels, as one program;
/// each section keeps its original test name and doc comment.
///
/// Consolidates the matrix-level runs of misc_fallthrough_statement_attribute and
/// misc_attribute_led_declarations_still_compile (whose compile-only half is a
/// unit test in cc/test_asm/misc_statement_attributes.rs).
#[test]
fn misc_statement_attributes_mega() {
    let code = r#"
/*
 * Exit codes: each section's own failure codes, offset by its base.
 *     1- 99  misc_fallthrough_statement_attribute
 *   101-102  misc_attribute_led_declarations_still_compile
 */

/* ---- misc_fallthrough_statement_attribute (exit codes 1-99) ----
 *
 *  `__attribute__((fallthrough));` is a null statement carrying an attribute,
 *  the GNU spelling of C23's `[[fallthrough]]`, and the way code that builds
 *  with `-Wimplicit-fallthrough` marks a deliberate fall into the next case.
 *  `__has_attribute(fallthrough)` answers 1, so code that probes for it uses
 *  it -- and c17 rejected the statement as "declaration declares nothing".
 */
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
static int t_misc_fallthrough_statement_attribute(void) {
#if !__has_attribute(fallthrough)
    return 99;
#endif
    return !(f(1) == 7 && f(2) == 6 && f(3) == 4 && f(9) == 100);
}


/* ---- misc_attribute_led_declarations_still_compile (exit codes 101-102) ----
 *
 *  A declaration that begins with an attribute is still a declaration, in a
 *  block and at file scope.
 */
static int al_ran;
__attribute__((constructor)) static void al_init(void) { al_ran = 1; }
static int t_misc_attribute_led_declarations_still_compile(void) {
    __attribute__((unused)) int x = 3;
    __attribute__((aligned(16))) int y = 4;
    if ((unsigned long)&y % 16 != 0)
        return 2;
    return !(al_ran == 1 && y == 4);
}

int main(void)
{
    int r;
    if ((r = t_misc_fallthrough_statement_attribute()) != 0)
        return 0 + r;
    if ((r = t_misc_attribute_led_declarations_still_compile()) != 0)
        return 100 + r;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("misc_statement_attributes_mega", code, &[]),
        0
    );
}
