//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for __builtin_offsetof builtin
//

use crate::common::compile_and_run;

#[test]
fn offsetof_basic() {
    let code = r#"
struct S {
    int a;
    int b;
    char c;
};

int main(void) {
    // offsetof(S, a) should be 0
    if (__builtin_offsetof(struct S, a) != 0) return 1;

    // offsetof(S, b) should be sizeof(int) = 4
    if (__builtin_offsetof(struct S, b) != sizeof(int)) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("offsetof_basic", code), 0);
}

#[test]
fn offsetof_nested() {
    let code = r#"
struct Inner { int x; int y; };
struct Outer { int a; struct Inner inner; };

int main(void) {
    // offsetof nested member
    unsigned long off = __builtin_offsetof(struct Outer, inner.y);
    // a=4 bytes, inner.x=4 bytes, so inner.y at offset 8
    if (off != sizeof(int) + sizeof(int)) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("offsetof_nested", code), 0);
}

#[test]
fn offsetof_array() {
    let code = r#"
struct S { int arr[10]; };

int main(void) {
    // offsetof with array index
    unsigned long off = __builtin_offsetof(struct S, arr[5]);
    if (off != 5 * sizeof(int)) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("offsetof_array", code), 0);
}

#[test]
fn offsetof_macro_style() {
    let code = r#"
struct S { int x; int y; };

int main(void) {
    // Using 'offsetof' spelling (macro-compatible)
    unsigned long off = offsetof(struct S, y);
    if (off != sizeof(int)) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("offsetof_macro_style", code), 0);
}
