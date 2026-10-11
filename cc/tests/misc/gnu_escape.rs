//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The GNU escape `\e` (and `\E`): ESC, 27, in every kind of literal.
//

use crate::common::compile_and_run_everywhere;

/// Linux lib/string_helpers.c writes `*p = '\e';`; terminal code writes
/// `"\e["`. Each must be ESC, never the letter.
#[test]
fn gnu_escape_e_is_esc() {
    let src = r#"
#include <string.h>
#include <wchar.h>
#if '\e' != 27 || '\E' != 27
#error "\e in #if"
#endif
int main(void) {
    if ('\e' != 27 || '\E' != 27) return 1;
    if (L'\e' != 27 || u'\E' != 27 || U'\e' != 27) return 2;
    const char *s = "\e[0m\E";
    if (strlen(s) != 5 || s[0] != 27 || s[1] != '[' || s[4] != 27) return 3;
    const wchar_t *w = L"a\eb";
    if (wcslen(w) != 3 || w[1] != 27) return 4;
    if (u8"\e"[0] != 27 || u"\e"[0] != 27 || U"\E"[0] != 27) return 5;
    return 0;
}
"#;
    compile_and_run_everywhere("gnu_escape_e", src);
}
