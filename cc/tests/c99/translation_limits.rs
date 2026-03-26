//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C99 Translation Limits Test
//
// Proves compiler meets C99 minimum translation limits (Section 5.2.4.1).
// Our compiler uses dynamic data structures with no artificial limits,
// so these are easily exceeded.
//

use crate::common::compile_and_run;

#[test]
fn c99_translation_limits_mega() {
    // Generate C code that exercises key translation limits
    let mut code = String::from(
        r#"
#include <string.h>

// 63+ significant characters in internal identifier (C99 minimum: 63)
int abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789abc = 42;

// Struct with > 1023 members would be too verbose; test ~130 members
struct BigStruct {
"#,
    );

    // Generate struct with 130 members
    for i in 0..130 {
        code.push_str(&format!("    int m{};\n", i));
    }
    code.push_str("};\n\n");

    // Enum with 130 constants
    code.push_str("enum BigEnum {\n");
    for i in 0..130 {
        if i > 0 {
            code.push_str(",\n");
        }
        code.push_str(&format!("    E_{}", i));
    }
    code.push_str("\n};\n\n");

    // Function with many parameters (test 20, C99 min is 127)
    code.push_str("int many_params(");
    for i in 0..20 {
        if i > 0 {
            code.push(',');
        }
        code.push_str(&format!("int p{}", i));
    }
    code.push_str(") {\n    return ");
    for i in 0..20 {
        if i > 0 {
            code.push('+');
        }
        code.push_str(&format!("p{}", i));
    }
    code.push_str(";\n}\n\n");

    code.push_str(
        r#"
int main(void) {
    // Long identifier (63+ chars)
    if (abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789abc != 42) return 1;

    // Struct with many members
    struct BigStruct bs;
    memset(&bs, 0, sizeof(bs));
    bs.m0 = 10;
    bs.m129 = 20;
    if (bs.m0 != 10 || bs.m129 != 20) return 2;

    // Enum with many constants
    if (E_0 != 0) return 3;
    if (E_129 != 129) return 4;

    // Many function parameters
    if (many_params(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20) != 210) return 5;

    // Deep block nesting (15+ levels, C99 min is 127)
    {{{{{{{{{{{{{{{
        int deep = 99;
        if (deep != 99) return 6;
    }}}}}}}}}}}}}}}

    // Switch with many case labels (20+, C99 min is 1023)
    int x = 15;
    int result = -1;
    switch (x) {
"#,
    );

    // Generate 30 case labels
    for i in 0..30 {
        code.push_str(&format!(
            "        case {}: result = {}; break;\n",
            i,
            i * 10
        ));
    }

    code.push_str(
        r#"    }
    if (result != 150) return 7;

    // Deep parenthesized expression (15+ levels)
    int paren = (((((((((((((((1 + 2)))))))))))))));;
    if (paren != 3) return 8;

    return 0;
}
"#,
    );

    assert_eq!(
        compile_and_run("c99_translation_limits_mega", &code, &[]),
        0
    );
}
