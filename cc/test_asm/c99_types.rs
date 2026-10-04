//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Compile-only cases from `tests/c99/types.rs`, run in process.
//

use crate::test_compile::{compile, compile_expect_no_diagnostic};

/// 6.7.7p3's other half: such a typedef "shall have block scope". At file
/// scope there is nothing to evaluate the extent, and c17 must say so.
#[test]
fn c17_vm_typedef_is_rejected_at_file_scope() {
    let code = r#"
int n = 4;
typedef int T[n];
int main(void) { return 0; }
"#;
    let c = compile("c17_vm_typedef_file_scope", code, &[]);
    assert!(
        !c.success,
        "a file-scope variably modified typedef must be rejected\nstderr:\n{}",
        c.stderr
    );
}

/// A null pointer constant assigns to a function pointer.
///
/// 6.5.16.1p1 lets a null pointer constant assign to any pointer, and
/// 6.3.2.3p3 makes `(void *)0` one -- which is exactly how glibc spells
/// `NULL`. The check for it was read only in the "target is a pointer, value
/// is not" branch, which a null constant that has already decayed to `void *`
/// never reaches: it was judged by the pointer-to-pointer rules instead, and
/// those diagnose a `void *` meeting a function pointer. So every `fp = NULL`
/// warned, and any `-Werror` build using function pointers broke.
///
/// The run-time half of this case is a section of `c99_types_mega` in
/// `tests/c99/types.rs`.
#[test]
fn c99_null_constant_assigns_to_a_function_pointer() {
    let code = r#"
#include <stddef.h>

typedef void (*handler)(int);
static handler h1 = NULL;
static handler h2 = (void *)0;
static handler h3 = 0;

static void hit(int x) { (void)x; }

int main(void)
{
    handler h = NULL;
    if (h != NULL) return 1;
    h = (void *)0;
    if (h) return 2;
    h = hit;
    if (!h) return 3;
    h = 0;
    if (h) return 4;
    if (h1 || h2 || h3) return 5;

    /* A void* still round-trips through an object pointer. */
    int i = 7;
    void *v = &i;
    int *p = v;
    if (*p != 7) return 6;
    return 0;
}
"#;
    // The warning is the defect, so the compile must be clean, not merely
    // successful.
    compile_expect_no_diagnostic("null_fnptr", code, "forbids");
}
