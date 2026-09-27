//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The <string.h> and <stdio.h> functions c17 knows by prototype
//
// A call to one stays a call, recognised by what the program called; the
// optimizer may then compute its result from constant arguments.
//

use crate::common::compile_and_run;

/// `__builtin_puts`, `__builtin_putchar` and `__builtin_printf` need no
/// declaration of the library function: gcc knows each one's prototype, and
/// so does c17, which declares it from the table rather than guessing -- so
/// the `int` argument of `putchar` is converted, and the result comes back
/// as the `int` it is.
#[test]
fn builtin_stdio_calls_need_no_header() {
    let code = r#"
int main(void) {
    if (__builtin_putchar('A') != 'A') return 1;
    if (__builtin_putchar(0x142) != 0x42) return 2;
    if (__builtin_puts("") < 0) return 3;
    if (__builtin_printf("%d%s\n", 7, "x") != 3) return 4;
    if (__builtin_strlen("abcd") != 4) return 5;
    return 0;
}
"#;
    assert_eq!(compile_and_run("builtin_stdio_no_header", code, &[]), 0);
}
