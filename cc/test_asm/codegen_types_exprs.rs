//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Compile-only cases of tests/codegen/types_exprs.rs, in process: static
// initializers that are not constant expressions, and the width of a function
// address converted to an integer.
//

use crate::test_compile::compile_expect_error;

/// An address does not fit an integer object narrower than a pointer, so it
/// is no initializer for one -- gcc's "not computable at load time". The
/// relocation was emitted as eight bytes over the one- or four-byte object.
#[test]
fn codegen_address_into_a_narrow_integer_is_not_a_static_initializer() {
    let what = "cannot initialize an object with static storage duration";
    for (name, src) in [
        (
            "narrow_addr_char",
            "static int arr[4];\nchar k = (long)arr;\n",
        ),
        (
            "narrow_addr_int",
            "static int arr[4];\nint j = (long)arr + 1;\n",
        ),
        ("narrow_addr_fn", "long h(void);\nshort s = (long)&h;\n"),
    ] {
        compile_expect_error(name, src, what);
    }
}

/// The x86-64 shape of the defect above, where a non-PIE link would put the
/// function below 4 GiB and hide it: the address loaded from the GOT is the
/// value, with no 32-bit move between.
#[test]
fn codegen_function_cast_to_integer_keeps_the_whole_address() {
    use crate::test_asm::asm_probe::{asm_for_with, body_of, X86_64_LINUX};
    let src = "long h(void);\nlong a(void) { return (long)h; }\n";
    let asm = asm_for_with("fn_cast_width", X86_64_LINUX, src, &["-O0"]);
    let body = body_of(&asm, "a");
    assert!(!body.contains("movl"), "{body}");
}

/// The name of an object that does not decay is its *value*, which is not a
/// constant expression (C17 6.6p9). Deciding by the type being initialized
/// rather than the name's own type made `int *q = p;` initialize `q` with
/// the address of `p`.
#[test]
fn codegen_pointer_object_value_is_not_a_static_initializer() {
    compile_expect_error(
        "ptr_value_static_init",
        "int x;\nint *p = &x;\nint *q = p;\n",
        "is not a constant expression",
    );
}
