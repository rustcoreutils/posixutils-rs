//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `return` converts its value "as if by assignment" (C17 6.8.6.4p3), so the
// simple-assignment constraints of 6.5.16.1 decide what it may return, and a
// null pointer constant (6.3.2.3p3) is the same thing in every context that
// asks: an integer constant expression with the value 0, or one cast to
// `void *`. The severities and wording follow gcc's.
//

use crate::test_compile::{
    compile, compile_expect_error, compile_expect_no_diagnostic, compile_expect_warning,
};

const INT_TO_POINTER: &str = "makes pointer from integer without a cast";

const INCOMPATIBLE_POINTER: &str = "incompatible pointer type";

/// Every null pointer constant, and every pointer of the right type, returns
/// from a pointer function in silence -- including an array or a function,
/// which convert to a pointer first (6.3.2.1p3-4).
#[test]
fn return_null_pointer_constants_are_accepted() {
    let cases = [
        ("ret_void_zero", "int *f(void){ return (void*)0; }\n"),
        ("ret_typed_zero", "int *f(void){ return (int*)0; }\n"),
        ("ret_long_zero", "int *f(void){ return 0L; }\n"),
        ("ret_nul_char", "int *f(void){ return '\\0'; }\n"),
        ("ret_folded_zero", "int *f(void){ return (1-1); }\n"),
        ("ret_cast_long_zero", "int *f(void){ return (long)0; }\n"),
        ("ret_void_folded", "int *f(void){ return (void*)(1-1); }\n"),
        (
            "ret_void_long_zero",
            "int *f(void){ return (void*)(long)0; }\n",
        ),
        ("ret_float_cast_zero", "int *f(void){ return (int)0.0; }\n"),
        ("ret_array", "int a[3]; int *f(void){ return a; }\n"),
        (
            "ret_function",
            "int g(void); int (*f(void))(void){ return g; }\n",
        ),
        (
            "ret_fnptr_null",
            "int (*f(void))(void){ return (void*)0; }\n",
        ),
        (
            "ret_same_struct",
            "struct A { int x; }; struct A f(struct A a){ return a; }\n",
        ),
        ("ret_ptr_to_bool", "_Bool f(int *p){ return p; }\n"),
    ];
    for (name, src) in cases {
        compile_expect_no_diagnostic(name, src, "warning");
    }
}

/// A cast to any pointer type other than `void *` makes a pointer value, not
/// a null pointer constant, so `(char *)0` is an `int *` mismatch wherever it
/// is converted. The shared null-constant test stripped every cast, which hid
/// the mismatch from assignment, initialization and argument passing.
#[test]
fn return_typed_null_pointer_is_a_pointer_mismatch() {
    let cases = [
        ("npc_return", "int *f(void){ return (char*)0; }\n"),
        ("npc_assign", "int *p; void f(void){ p = (char*)0; }\n"),
        ("npc_init", "int *p = (char*)0;\n"),
        ("npc_arg", "void g(int *); void f(void){ g((char*)0); }\n"),
    ];
    for (name, src) in cases {
        compile_expect_warning(name, src, INCOMPATIBLE_POINTER);
    }
    // `const void *` is not `void *` either: its qualifier is discarded.
    compile_expect_warning(
        "npc_const_void",
        "int *f(void){ return (const void*)0; }\n",
        "discards a qualifier",
    );
}

/// An integer constant expression may not convert a pointer (C17 6.6p6), so
/// a zero that passed through one is an integer, not a null pointer constant.
#[test]
fn return_zero_through_a_pointer_is_not_a_null_constant() {
    let cases = [
        (
            "nzp_return",
            "int *f(void){ return (int)(long)(char*)0; }\n",
        ),
        (
            "nzp_return_short",
            "int *f(void){ return (int)(char*)0; }\n",
        ),
        (
            "nzp_assign",
            "int *p; void f(void){ p = (int)(long)(char*)0; }\n",
        ),
        ("nzp_init", "int *p = (int)(long)(char*)0;\n"),
    ];
    for (name, src) in cases {
        compile_expect_warning(name, src, INT_TO_POINTER);
    }
    // An unevaluated pointer operand is no obstacle: `sizeof p` is an
    // integer constant all the same.
    compile_expect_no_diagnostic(
        "nzp_sizeof",
        "int *p; int *f(void){ return sizeof p - sizeof p; }\n",
        "warning",
    );
}

/// The pointer/integer conversions are warnings, in gcc's words.
#[test]
fn return_pointer_integer_conversions_are_warned() {
    compile_expect_warning(
        "ret_int_to_ptr",
        "int *f(void){ return 5; }\n",
        "returning 'int' from a function with return type 'int *' makes pointer from integer without a cast",
    );
    compile_expect_warning(
        "ret_ptr_to_int",
        "int f(int *p){ return p; }\n",
        "returning 'int *' from a function with return type 'int' makes integer from pointer without a cast",
    );
    compile_expect_warning(
        "ret_fn_to_void_ptr",
        "int *f(void); void *g(void){ return f; }\n",
        "ISO C forbids return between function pointer and 'void *'",
    );
}

/// Incompatible types are an error, worded as gcc words it for `return`.
#[test]
fn return_incompatible_types_are_rejected() {
    compile_expect_error(
        "ret_wrong_struct",
        "struct A { int x; }; struct B { int x; };\n\
         struct A f(struct B b){ return b; }\n",
        "incompatible types when returning type 'struct B' but 'struct A' was expected",
    );
    compile_expect_error(
        "ret_int_to_struct",
        "struct A { int x; }; struct A f(void){ return 1; }\n",
        "incompatible types when returning type 'int' but 'struct A' was expected",
    );
    compile_expect_error(
        "ret_double_to_ptr",
        "int *f(void){ return 1.0; }\n",
        "incompatible types when returning type 'double' but 'int *' was expected",
    );
}

/// A `void` expression has no value to return, which gcc says plainly rather
/// than as a type mismatch -- the same words assignment uses.
#[test]
fn return_void_value_from_non_void_is_rejected() {
    compile_expect_error(
        "ret_void_value",
        "void h(void); int f(void){ return h(); }\n",
        "void value not ignored as it ought to be",
    );
}

/// A `return` with no value is reported where the `return` is, not where the
/// last expression before it happened to be.
#[test]
fn return_without_value_is_reported_at_the_return() {
    let run = compile(
        "ret_no_value_pos",
        "int g(int x){\n  x++;\n  if (x)\n    return;\n  return 1;\n}\n",
        &[],
    );
    assert!(!run.success, "stderr:\n{}", run.stderr);
    assert!(
        run.stderr
            .contains(":4:5: error: 'return' with no value in a function returning non-void"),
        "stderr:\n{}",
        run.stderr
    );
}
