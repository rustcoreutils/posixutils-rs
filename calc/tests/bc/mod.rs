//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, TestPlan};

fn test_bc(program: &str, expected_output: &str) {
    run_test(TestPlan {
        cmd: String::from("bc"),
        args: vec![],
        stdin_data: program.to_string(),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

fn test_bc_with_math_library(program: &str, expected_output: &str) {
    run_test(TestPlan {
        cmd: String::from("bc"),
        args: vec!["-l".to_string()],
        stdin_data: program.to_string(),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

macro_rules! test_bc {
    ($test_name:ident) => {
        test_bc(
            include_str!(concat!("./", stringify!($test_name), ".bc")),
            include_str!(concat!("./", stringify!($test_name), ".out")),
        )
    };
}

macro_rules! test_bc_l {
    ($test_name:ident) => {
        test_bc_with_math_library(
            include_str!(concat!("./", stringify!($test_name), ".bc")),
            include_str!(concat!("./", stringify!($test_name), ".out")),
        )
    };
}

#[test]
fn test_bc_add() {
    test_bc!(add)
}

// Diagnostics go to stderr, not stdout (audit #B2). A runtime error in the
// REPL is recovered from, so the session still exits 0 after quit.
#[test]
fn test_bc_error_to_stderr() {
    run_test(TestPlan {
        cmd: String::from("bc"),
        args: vec![],
        stdin_data: String::from("1/0\nquit\n"),
        expected_out: String::new(),
        expected_err: String::from("runtime error (line 1): division by zero\n"),
        expected_exit_code: 0,
    });
}

// A file operand that cannot be read: diagnostic to stderr, terminate with a
// non-zero exit status (audit #B11).
#[test]
fn test_bc_missing_file() {
    run_test(TestPlan {
        cmd: String::from("bc"),
        args: vec!["/nonexistent-bc-file.bc".to_string()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::from("bc: cannot read file: /nonexistent-bc-file.bc\n"),
        expected_exit_code: 1,
    });
}

// POSIX limit maxima are enforced so pathological inputs cannot drive
// unbounded allocation (audit #B3 scale, #B4 obase, #B5 array index).
/// Assert that the program fails with a diagnostic containing `needle`.
fn bc_runtime_error_contains(program: &str, needle: &str) {
    let output = plib::testing::run_test_base("bc", &[], format!("{}\nquit\n", program).as_bytes());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains(needle),
        "expected stderr to contain {:?}, got {:?}",
        needle,
        stderr
    );
}

fn bc_runtime_error(program: &str, expected_err: &str) {
    run_test(TestPlan {
        cmd: String::from("bc"),
        args: vec![],
        stdin_data: format!("{program}\nquit\n"),
        expected_out: String::new(),
        expected_err: format!("{expected_err}\n"),
        expected_exit_code: 0,
    });
}

#[test]
fn test_bc_scale_too_large() {
    bc_runtime_error(
        "scale=2147483648",
        "runtime error (line 1): scale is too large",
    );
}

#[test]
fn test_bc_obase_too_large() {
    bc_runtime_error(
        "obase=2147483648",
        "runtime error (line 1): obase is too large",
    );
}

#[test]
fn test_bc_array_index_out_of_bounds() {
    // POSIX: an array holds up to {BC_DIM_MAX} elements and is indexed from 0
    // to {BC_DIM_MAX}-1, so the last valid subscript is one below the limit.
    test_bc("a[16777215]=1\nquit\n", "");
    bc_runtime_error(
        "a[16777216]=1",
        "runtime error (line 1): array index out of bounds",
    );
    bc_runtime_error(
        "a[-1]=1",
        "runtime error (line 1): array index cannot be negative",
    );
}

/// POSIX: "references to any of these names from other functions that are
/// called from this function also refer to the new value". A callee saw the
/// global instead of the caller's parameter or auto.
#[test]
fn test_bc_dynamic_scoping() {
    test_bc(
        "define g(){\nreturn(a)\n}\ndefine f(a){\nreturn(g())\n}\na=1\nf(9)\nquit\n",
        "9\n",
    );
    test_bc(
        "define g(){\nreturn(x)\n}\ndefine f(){\nauto x\nx=5\nreturn(g())\n}\nx=1\nf()\nquit\n",
        "5\n",
    );
    // The caller's value must come back afterwards.
    test_bc("define f(a){\nreturn(a)\n}\na=1\nf(9)\na\nquit\n", "9\n1\n");
}

/// An array argument was always read from the global of that name, so passing
/// a local array on to another function passed the wrong array.
#[test]
fn test_bc_array_argument_uses_the_active_binding() {
    test_bc(
        "define g(x[]){\nreturn(x[0])\n}\ndefine f(x[]){\nreturn(g(x[]))\n}\nx[0]=99\nq[0]=7\nf(q[])\nquit\n",
        "7\n",
    );
    test_bc(
        "define g(x[]){\nreturn(x[0])\n}\ndefine f(){\nauto y[]\ny[0]=42\nreturn(g(y[]))\n}\nf()\nquit\n",
        "42\n",
    );
}

/// A mismatch used to bind the missing parameter to the global of the same
/// name, and silently drop extra arguments without evaluating them.
#[test]
fn test_bc_argument_count_mismatch_is_an_error() {
    bc_runtime_error(
        "define f(a,b){\nreturn(b)\n}\nb=5\nf(1)",
        "runtime error (line 1): wrong number of arguments",
    );
    bc_runtime_error(
        "define f(a){\nreturn(a)\n}\nf(1,2)",
        "runtime error (line 1): wrong number of arguments",
    );
}

/// POSIX makes scale, ibase and obase named expressions, so they increment.
#[test]
fn test_bc_register_increment() {
    // Postfix yields the old value, prefix the new one.
    test_bc("scale=1\nscale++\nscale\nquit\n", "1\n2\n");
    test_bc("scale=1\n++scale\nscale\nquit\n", "2\n2\n");
    test_bc("ibase=9\nibase--\nibase\nquit\n", "9\n8\n");
    // Stepping obase changes the base the result is then printed in: the old
    // value 16 and the new value 15 are both rendered in base 15. Verified
    // against GNU bc.
    test_bc("obase=16\nobase--\nobase\nquit\n", "11\n10\n");
    // The bounds still apply. The REPL executes one line at a time, so the
    // failing line is line 1 of its own program.
    bc_runtime_error(
        "ibase=16\nibase++",
        "runtime error (line 1): ibase must be between 2 and 16",
    );
    bc_runtime_error(
        "obase=2\nobase--",
        "runtime error (line 1): obase must be greater than 1",
    );
}

/// A negative value is out of range in the other direction; reporting it as
/// "too large" was simply wrong.
#[test]
fn test_bc_negative_register_diagnostics() {
    bc_runtime_error(
        "scale=-1",
        "runtime error (line 1): scale cannot be negative",
    );
    bc_runtime_error(
        "obase=-5",
        "runtime error (line 1): obase must be greater than 1",
    );
}

/// Recursion must report a limit rather than abort on a guard page.
#[test]
fn test_bc_runaway_recursion_is_diagnosed() {
    bc_runtime_error_contains(
        "define f(x){\nreturn(f(x))\n}\nf(1)",
        "evaluation nested too deeply",
    );
}

/// A sparse write must not allocate every element below it.
#[test]
fn test_bc_sparse_array() {
    test_bc("a[16777215]=7\na[16777215]\na[5]\nquit\n", "7\n0\n");
}

// x^0 is 1 with scale 0, regardless of the scale register (audit #B8).
#[test]
fn test_bc_pow_zero_scale() {
    test_bc("scale=5\n2.5^0\nquit\n", "1\n");
}

// Regression: `quit` inside a `for` body within a function definition must not
// panic. Per bc semantics quit takes effect when the definition is read, so
// the statements after the definition are never executed (matches GNU bc).
#[test]
fn test_bc_quit_in_for_in_function() {
    test_bc(
        "1\ndefine f(x){\nfor(i=0;i<5;i++){\nif(i==3)quit\n}\n}\n2\nf(0)\n3\n",
        "1\n",
    );
}

#[test]
fn test_bc_arrays_are_passed_to_function_by_value() {
    test_bc!(arrays_are_passed_to_function_by_value)
}

#[test]
fn test_bc_assignment_of_a_single_value_to_base_register_is_hexadecimal() {
    test_bc!(assignment_of_a_single_value_to_base_register_is_hexadecimal)
}

#[test]
fn test_bc_assign_to_array_item() {
    test_bc!(assign_to_array_item)
}

#[test]
fn test_bc_assign_to_function_local_does_not_change_global() {
    test_bc!(assign_to_function_local_does_not_change_global)
}

#[test]
fn test_bc_assign_to_variable() {
    test_bc!(assign_to_variable)
}

#[test]
fn test_bc_break_out_of_loop() {
    test_bc!(break_out_of_loop)
}

#[test]
fn test_bc_comments() {
    test_bc!(comments)
}

#[test]
fn test_bc_compound_assignment() {
    test_bc!(compound_assignment)
}

#[test]
fn test_bc_define_empty_function() {
    test_bc!(define_empty_function)
}

#[test]
fn test_bc_define_function_with_locals() {
    test_bc!(define_function_with_locals)
}

#[test]
fn test_bc_define_function_with_parameters() {
    test_bc!(define_function_with_parameters)
}

#[test]
fn test_bc_div() {
    test_bc!(div)
}

#[test]
fn test_bc_empty_return_returns_zero() {
    test_bc!(empty_return_returns_zero)
}

#[test]
fn test_bc_for_loop() {
    test_bc!(for_loop)
}

#[test]
fn test_bc_function_returns_correct_value() {
    test_bc!(function_returns_correct_value)
}

#[test]
fn test_bc_function_with_no_return_returns_zero() {
    test_bc!(function_with_no_return_returns_zero)
}

#[test]
fn test_bc_if() {
    test_bc!(if)
}

#[test]
fn test_bc_length() {
    test_bc!(length)
}

#[test]
fn test_bc_mod() {
    test_bc!(mod)
}

#[test]
fn test_bc_mul() {
    test_bc!(mul)
}

#[test]
fn test_bc_multiline_numbers() {
    test_bc!(multiline_numbers)
}

#[test]
fn test_bc_operator_precedence() {
    test_bc!(operator_precedence)
}

#[test]
fn test_bc_output_base_1097() {
    test_bc!(output_base_1097)
}

#[test]
fn test_bc_output_base_14() {
    test_bc!(output_base_14)
}

#[test]
fn test_bc_output_base_67() {
    test_bc!(output_base_67)
}

#[test]
fn test_bc_output_base_6() {
    test_bc!(output_base_6)
}

#[test]
fn test_bc_postfix_decrement() {
    test_bc!(postfix_decrement)
}

#[test]
fn test_bc_postfix_increment() {
    test_bc!(postfix_increment)
}

#[test]
fn test_bc_pow() {
    test_bc!(pow)
}

#[test]
fn test_bc_prefix_decrement() {
    test_bc!(prefix_decrement)
}

#[test]
fn test_bc_prefix_increment() {
    test_bc!(prefix_increment)
}

#[test]
fn test_bc_quit() {
    test_bc!(quit)
}

#[test]
fn test_bc_quit_in_unexecuted_code() {
    test_bc!(quit_in_unexecuted_code)
}

#[test]
fn test_bc_read_base_10() {
    test_bc!(read_base_10)
}

#[test]
fn test_bc_read_base_15() {
    test_bc!(read_base_15)
}

#[test]
fn test_bc_read_base_2() {
    test_bc!(read_base_2)
}

#[test]
fn test_bc_scale() {
    test_bc!(scale)
}

#[test]
fn test_bc_sqrt() {
    test_bc!(sqrt)
}

#[test]
fn test_bc_strings() {
    test_bc!(strings)
}

#[test]
fn test_bc_sub() {
    test_bc!(sub)
}

#[test]
fn test_bc_unary_minus() {
    test_bc!(unary_minus)
}

#[test]
fn test_bc_uninitialized_variables_are_zero() {
    test_bc!(uninitialized_variables_are_zero)
}

#[test]
fn test_bc_while_loop() {
    test_bc!(while_loop)
}

#[test]
fn test_bc_compile_math_library() {
    test_bc_with_math_library("quit\n", "");
}

#[test]
fn test_bc_ln_to_scale_17() {
    test_bc_l!(ln_to_scale_17)
}

#[test]
fn test_bc_atan_to_scale_17() {
    test_bc_l!(atan_to_scale_17)
}

#[test]
fn test_bc_sin_to_scale_18() {
    test_bc_l!(sin_to_scale_18)
}

#[test]
fn test_bc_cos_to_scale_18() {
    test_bc_l!(cos_to_scale_18)
}
