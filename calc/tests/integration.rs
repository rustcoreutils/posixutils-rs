//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::{run_test, TestPlan};

fn expr_test(args: &[&str], expected_output: &str) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("expr"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

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
            include_str!(concat!("bc/", stringify!($test_name), ".bc")),
            include_str!(concat!("bc/", stringify!($test_name), ".out")),
        )
    };
}

macro_rules! test_bc_l {
    ($test_name:ident) => {
        test_bc_with_math_library(
            include_str!(concat!("bc/", stringify!($test_name), ".bc")),
            include_str!(concat!("bc/", stringify!($test_name), ".out")),
        )
    };
}

#[test]
fn test_expr_logops() {
    expr_test(&["4", "|", "5", "+", "1"], "5\n");
    expr_test(&["0", "|", "5", "+", "1"], "6\n");
    expr_test(&["4", "&", "5", "+", "1"], "5\n");
    expr_test(&["4", "&", "0", "+", "1"], "1\n");
    expr_test(&["0", "%", "5", "+", "1"], "1\n");
}

#[test]
fn test_expr_intops() {
    expr_test(&["4", "+", "4", "+", "1"], "9\n");
    expr_test(&["4", "-", "4", "+", "1"], "1\n");
    expr_test(&["4", "*", "4", "+", "1"], "17\n");
    expr_test(&["4", "/", "4", "+", "1"], "2\n");
    expr_test(&["4", "%", "4", "+", "1"], "1\n");
}

#[test]
fn test_expr_cmpint() {
    expr_test(&["4", "<", "5", "+", "1"], "2\n");
    expr_test(&["4", ">", "5", "+", "1"], "1\n");
    expr_test(&["4", "<=", "5", "+", "1"], "2\n");
    expr_test(&["4", ">=", "5", "+", "1"], "1\n");
    expr_test(&["4", "=", "5", "+", "1"], "1\n");
    expr_test(&["4", "!=", "5", "+", "1"], "2\n");
}

#[test]
fn test_expr_cmpstr() {
    expr_test(&["aaa", "<", "bbb", "+", "1"], "2\n");
    expr_test(&["aaa", ">", "bbb", "+", "1"], "1\n");
    expr_test(&["aaa", "<=", "bbb", "+", "1"], "2\n");
    expr_test(&["aaa", ">=", "bbb", "+", "1"], "1\n");
    expr_test(&["aaa", "=", "bbb", "+", "1"], "1\n");
    expr_test(&["aaa", "!=", "bbb", "+", "1"], "2\n");
}

#[test]
fn test_bc_add() {
    test_bc!(add)
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
