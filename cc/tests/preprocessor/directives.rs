//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Preprocessor Directives Tests
//
// Tests for #line and other preprocessor directives.
//

use crate::common::compile_and_run;

#[test]
fn preprocessor_line_directive() {
    let code = r#"
#include <string.h>

int main(void) {
    // ========== #line sets __LINE__ (returns 1-9) ==========
    // #line N means the NEXT line is line N
    {
#line 100
        if (__LINE__ != 100) return 1;  // this is line 100
        if (__LINE__ != 101) return 2;  // this is line 101
    }

    // ========== #line sets __FILE__ (returns 10-19) ==========
    {
#line 200 "fake.c"
        if (strcmp(__FILE__, "fake.c") != 0) return 10;  // line 200
        if (__LINE__ != 201) return 11;  // line 201
        if (__LINE__ != 202) return 12;  // line 202
    }

    // ========== #line in false #if branch has no effect (returns 20-29) ==========
    {
#line 300
#if 0
#line 999 "wrong.c"
#endif
        // __LINE__ should NOT be 999
        if (__LINE__ == 999) return 20;
        if (strcmp(__FILE__, "wrong.c") == 0) return 21;
    }

    // ========== #line with only line number keeps previous file (returns 30-39) ==========
    {
#line 50 "first.c"
#line 400
        if (__LINE__ != 400) return 30;  // line 400
        if (strcmp(__FILE__, "first.c") != 0) return 31;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("line_directive", code, &[]), 0);
}

// ============================================================================
// Test: #line with macro-expanded tokens
// ============================================================================

#[test]
fn preprocessor_line_directive_macro_expansion() {
    let code = r#"
int main(void) {
    // #line should macro-expand its tokens before parsing
#define LINENUM 100
#line LINENUM
    if (__LINE__ != 100) return 1;

    // Macro-expanded filename
#define FNAME "expanded.c"
#line 200 FNAME
    if (__LINE__ != 200) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("line_directive_macro_expand", code, &[]), 0);
}
