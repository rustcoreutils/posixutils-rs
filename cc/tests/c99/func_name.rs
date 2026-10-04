//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `__func__` (C17 6.4.2.2): implicitly `static const char __func__[] =
// "function-name";`. c17 typed it `char *`, so `sizeof __func__` was the size
// of a pointer, `_Generic` picked `char *`, and writing through it was
// accepted; with an asm label it held the label rather than the name.
//

use crate::common::compile_and_run;

#[test]
fn func_name_is_a_const_char_array() {
    let src = r#"
#include <string.h>
int f(void) __asm__("c17_func_name_label");
int f(void) { return sizeof(__func__); }
const char *h(void) { return __func__; }
int myfn(void) {
    _Static_assert(sizeof(__func__) == 5, "__func__");
    _Static_assert(sizeof(__FUNCTION__) == 5, "__FUNCTION__");
    _Static_assert(sizeof(__PRETTY_FUNCTION__) == 5, "__PRETTY_FUNCTION__");
    _Static_assert(_Generic(__func__, const char *: 1, default: 0), "decays to const char *");
    _Static_assert(_Generic(&__func__, const char (*)[5]: 1, default: 0), "address of the array");
    static const char *sp = __func__;
    const char (*pa)[5] = &__func__;
    if (sp != __func__ || strcmp(*pa, "myfn") != 0) return 1;
    return 0;
}
int main(void) {
    if (f() != 2) return 10;                  /* "f", not the asm label */
    if (strcmp(h(), "h") != 0) return 11;
    if (myfn() != 0) return 12;
    if (strcmp(__func__, "main") != 0) return 13;
    return 0;
}
"#;
    assert_eq!(compile_and_run("func_name_array", src, &[]), 0);
}
