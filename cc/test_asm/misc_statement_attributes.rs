//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Compile-only case of tests/misc/statement_attributes.rs, in process.
//

use crate::test_compile::compile_expect_ok;

/// A declaration that begins with an attribute is still a declaration, in a
/// block and at file scope. (The run half is a section of
/// misc_statement_attributes_mega in tests/misc/statement_attributes.rs.)
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
}
