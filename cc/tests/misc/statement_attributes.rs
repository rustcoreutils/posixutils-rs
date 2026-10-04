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

use crate::common::{compile_and_run, compile_and_run_optimized, compile_expect_ok};

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
