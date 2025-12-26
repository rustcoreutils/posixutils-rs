//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for GNU statement expressions: ({ stmt; stmt; expr; })
//

use crate::common::compile_and_run;

#[test]
fn stmt_expr_basic() {
    let code = r#"
int main(void) {
    // Basic statement expression
    int x = ({ int a = 5; a + 3; });
    if (x != 8) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("stmt_expr_basic", code), 0);
}

#[test]
fn stmt_expr_with_declarations() {
    let code = r#"
int main(void) {
    // Multiple declarations and statements
    int result = ({
        int a = 1;
        int b = 2;
        int c = 3;
        a + b + c;
    });
    if (result != 6) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("stmt_expr_with_declarations", code), 0);
}

#[test]
fn stmt_expr_nested() {
    let code = r#"
int main(void) {
    // Nested statement expressions
    int x = ({
        int a = ({ int inner = 10; inner * 2; });
        a + 5;
    });
    if (x != 25) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("stmt_expr_nested", code), 0);
}

#[test]
fn stmt_expr_as_function_arg() {
    let code = r#"
int add(int a, int b) {
    return a + b;
}

int main(void) {
    // Statement expression as function argument
    int result = add(({ int x = 3; x; }), ({ int y = 4; y; }));
    if (result != 7) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("stmt_expr_as_function_arg", code), 0);
}

#[test]
fn stmt_expr_with_control_flow() {
    let code = r#"
int main(void) {
    // Statement expression with control flow
    int x = ({
        int sum = 0;
        int i;
        for (i = 1; i <= 10; i++) {
            sum = sum + i;
        }
        sum;
    });
    // 1 + 2 + ... + 10 = 55
    if (x != 55) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("stmt_expr_with_control_flow", code), 0);
}
