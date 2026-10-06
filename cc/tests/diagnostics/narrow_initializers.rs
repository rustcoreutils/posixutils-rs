//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// A static initializer is converted to its object's type before it reaches
// the assembler (C17 6.7.9p11, 6.3.1.3p3), with gcc's `-Woverflow` when the
// conversion changes it. `char z = 300;` was emitted as `.byte 300`, which
// the assembler truncated with a warning of its own.
//

use crate::common::{compile_and_run_everywhere, compile_object_run};

const NARROW: &str = r#"
char z = 300;
signed char sc = -129;
unsigned char uc = 300;
short s = 70000;
unsigned short us = 70000;
_Bool b = 2;
struct S { char c; short h; int bf : 3; unsigned ubf : 2; } st = { 300, 70000, 9, 5 };
unsigned char arr[3] = { 255, 256, 1000 };
int i = 4294967297LL;
enum E { A } e = 4294967297LL;
static const char kc = 300;
int f(void) { static char lz = 300; static short ls[2] = { 70000, 1 }; return lz + ls[0]; }
int main(void) {
    if (z != 44 || sc != 127 || uc != 44) return 1;
    if (s != 4464 || us != 4464 || b != 1) return 2;
    if (st.c != 44 || st.h != 4464 || st.bf != 1 || st.ubf != 1) return 3;
    if (arr[0] != 255 || arr[1] != 0 || arr[2] != 232) return 4;
    if (i != 1 || e != 1 || kc != 44) return 5;
    if (f() != 44 + 4464) return 6;
    return 0;
}
"#;

/// The values are gcc's on both targets, at every level, and the assembler
/// never sees one it has to truncate.
#[test]
fn narrow_static_initializers_are_converted() {
    compile_and_run_everywhere("narrow_static_init", NARROW);
    let run = compile_object_run("narrow_static_obj", NARROW, &[]);
    assert!(run.success, "{}", run.stderr);
    assert!(!run.stderr.contains("truncated"), "{}", run.stderr);
    assert!(
        run.stderr.contains(
            ":2:10: warning: overflow in conversion from 'int' to 'char' changes value from '300' to '44'"
        ) || run.stderr.contains(
            ":2:10: warning: unsigned conversion from 'int' to 'char' changes value from '300' to '44'"
        ),
        "{}",
        run.stderr
    );
    let quiet = compile_object_run("narrow_static_quiet", NARROW, &["-Wno-overflow"]);
    assert!(quiet.success && quiet.stderr.is_empty(), "{}", quiet.stderr);
}
