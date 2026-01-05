use plib::testing::{run_test, run_test_with_checker, TestPlan};

fn test_awk(args: Vec<String>, expected_output: &str) {
    run_test(TestPlan {
        cmd: String::from("awk"),
        args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

macro_rules! test_awk {
    ($test_name:ident $(,$data_file:expr_2021)*) => {
        test_awk(vec![
            "-f".to_string(),
            concat!("tests/awk/", stringify!($test_name), ".awk").to_string(),
            $($data_file.to_string(),)*
        ], include_str!(concat!("awk/", stringify!($test_name), ".out")))
    };
}

#[test]
fn test_awk_empty_program() {
    test_awk!(empty_program);
}

#[test]
fn test_awk_print() {
    test_awk!(print);
}

#[test]
fn test_awk_printf() {
    test_awk!(printf);
}

#[test]
fn test_awk_hello_world() {
    test_awk!(hello_world)
}

#[test]
fn test_awk_missing_pattern_matches_all_records() {
    test_awk!(
        missing_pattern_matches_all_records,
        "tests/awk/test_data.txt"
    );
}

#[test]
fn test_awk_missing_action_prints_the_record() {
    test_awk!(missing_action_prints_the_record, "tests/awk/test_data.txt");
}

#[test]
fn test_awk_actions_execute_in_the_right_order() {
    test_awk!(
        actions_execute_in_the_right_order,
        "tests/awk/test_data.txt"
    );
}

#[test]
fn test_awk_variable_assignment() {
    test_awk!(variable_assignment);
}

#[test]
fn test_awk_arithmetic_operator_precedence() {
    test_awk!(arithmetic_operator_precedence);
}

#[test]
fn test_awk_logical_operator_precedence() {
    test_awk!(logical_operator_precedence);
}

#[test]
fn test_awk_comparison_operator_precedence() {
    test_awk!(comparison_operator_precedence);
}

#[test]
fn test_awk_conditional_expression() {
    test_awk!(conditional_expression);
}

#[test]
fn test_awk_array_assignment() {
    test_awk!(array_assignment);
}

#[test]
fn test_awk_ere_match() {
    test_awk!(ere_match);
}

#[test]
fn test_awk_in_operator() {
    test_awk!(in_operator)
}

#[test]
fn test_awk_multidimensional_index() {
    test_awk!(multidimensional_index);
}

#[test]
fn test_awk_multidimensional_in_operator() {
    test_awk!(multidimensional_in_operator)
}

#[test]
fn test_awk_unary_lvalue_operators() {
    test_awk!(unary_lvalue_operators);
}

#[test]
fn test_awk_compound_assignment() {
    test_awk!(compound_assignment);
}

#[test]
fn test_awk_comparison_operators() {
    test_awk!(comparison_operators);
}

#[test]
fn test_awk_uninitialized_variables() {
    test_awk!(uninitialized_variables);
}

#[test]
fn test_awk_access_field_variables() {
    test_awk!(access_field_variables, "tests/awk/test_data.txt");
}

#[test]
fn test_awk_assigning_to_a_non_existent_field_var_creates_it() {
    test_awk!(
        assigning_to_a_non_existent_field_var_creates_it,
        "tests/awk/test_data.txt"
    );
}

#[test]
fn test_awk_print_program_arguments() {
    test_awk!(print_program_arguments, "one", "two", "three");
}

#[test]
fn test_awk_set_arguments_in_begin() {
    test_awk!(set_arguments_in_begin, "tests/awk/test_data.txt");
}

#[test]
fn test_awk_clear_input_file_in_begin() {
    test_awk!(clear_input_file_in_begin, "tests/awk/test_data.txt");
}

#[test]
fn test_awk_setting_argc_to_one_ignores_all_arguments() {
    test_awk!(
        setting_argc_to_one_ignores_all_arguments,
        "one",
        "two",
        "three"
    );
}

#[test]
fn test_awk_change_default_number_to_string_conversion() {
    test_awk!(change_default_number_to_string_conversion);
}

#[test]
fn test_awk_filename() {
    test_awk!(
        filename,
        "tests/awk/test_data.txt",
        "tests/awk/test_data2.txt"
    );
}

#[test]
fn test_awk_file_record_number() {
    test_awk!(
        file_record_number,
        "tests/awk/test_data.txt",
        "tests/awk/test_data2.txt"
    );
}

#[test]
fn test_awk_record_number() {
    test_awk!(
        record_number,
        "tests/awk/test_data.txt",
        "tests/awk/test_data2.txt"
    );
}

#[test]
fn test_awk_output_float_format() {
    test_awk!(output_float_format);
}

#[test]
fn test_awk_output_field_separator() {
    test_awk!(output_field_separator);
}

#[test]
fn test_awk_output_record_separator() {
    test_awk!(output_record_separator);
}

#[test]
fn test_change_record_separator() {
    test_awk!(change_record_separator, "tests/awk/test_data.txt");
}

#[test]
fn test_awk_subscript_separator() {
    test_awk!(subscript_separator);
}

#[test]
fn test_awk_default_field_separator_rules() {
    test_awk!(default_field_separator_rules, "tests/awk/test_data3.txt");
}

#[test]
fn test_awk_character_field_separator() {
    test_awk!(character_field_separator, "tests/awk/test_data.csv");
}

#[test]
fn test_awk_ere_field_separator() {
    test_awk!(ere_field_separator, "tests/awk/test_data4.txt");
}

#[test]
fn test_awk_program_with_only_end_actions_reads_input_files() {
    test_awk!(
        program_with_only_end_actions_reads_input_files,
        "tests/awk/test_data.txt",
        "tests/awk/test_data2.txt"
    );
}

#[test]
fn test_awk_pattern_range() {
    test_awk!(pattern_range, "tests/awk/test_data.txt");
}

#[test]
fn test_awk_if_stmt() {
    test_awk!(if_stmt);
}

#[test]
fn test_awk_while_stmt() {
    test_awk!(while_stmt);
}

#[test]
fn test_awk_do_while_stmt() {
    test_awk!(do_while_stmt);
}

#[test]
fn test_awk_for_stmt() {
    test_awk!(for_stmt);
}

#[test]
fn test_awk_break_stmt() {
    test_awk!(break_stmt);
}

#[test]
fn test_awk_continue_stmt() {
    test_awk!(continue_stmt);
}

#[test]
fn test_awk_for_each() {
    test_awk!(for_each);
}

#[test]
fn test_awk_delete() {
    test_awk!(delete);
}

#[test]
fn test_awk_next() {
    test_awk!(next, "tests/awk/test_data.txt");
}

#[test]
fn test_awk_nextfile() {
    test_awk!(
        nextfile,
        "tests/awk/test_data.txt",
        "tests/awk/test_data2.txt"
    );
}

#[test]
fn test_awk_exit() {
    test_awk!(exit, "tests/awk/test_data.txt");
}

#[test]
fn test_awk_output_redirection() {
    let mut correct_stdout = true;
    let mut correct_stderr = true;
    let mut correct_exit_code = true;

    let previous_append_file_contents = include_str!("awk/output_redirection_append.txt");

    run_test_with_checker(
        TestPlan {
            cmd: String::from("awk"),
            args: vec![
                "-f".to_string(),
                "tests/awk/output_redirection.awk".to_string(),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_, output| {
            correct_stdout = output.stdout.is_empty();
            correct_stderr = output.stderr.is_empty();
            correct_exit_code = output.status.code() == Some(0);
        },
    );

    let correct_truncate_output = include_str!("awk/output_redirection_truncate.correct.txt");
    let correct_append_output = include_str!("awk/output_redirection_append.correct.txt");

    let truncate_output_result =
        std::fs::read_to_string("tests/awk/output_redirection_truncate.txt");
    let append_output_result = std::fs::read_to_string("tests/awk/output_redirection_append.txt");

    std::fs::write(
        "tests/awk/output_redirection_truncate.txt",
        correct_truncate_output,
    )
    .expect("failed to write to file");
    std::fs::write(
        "tests/awk/output_redirection_append.txt",
        previous_append_file_contents,
    )
    .expect("failed to write to file");

    if !correct_stdout || !correct_stderr || !correct_exit_code {
        panic!("awk output redirection test failed");
    }

    if let (Ok(truncate_output), Ok(append_output)) = (truncate_output_result, append_output_result)
    {
        assert_eq!(truncate_output, correct_truncate_output);
        assert_eq!(append_output, correct_append_output);
    } else {
        panic!("failed to read output files");
    }
}

#[test]
fn test_awk_builtin_arithmetic_functions() {
    test_awk!(builtin_arithmetic_functions);
}

#[test]
fn builtin_string_functions() {
    test_awk!(builtin_string_functions, "tests/awk/test_data.txt");
}

#[test]
fn test_awk_delete_array_elements_in_for_each() {
    test_awk!(delete_array_elements_in_for_each);
}

#[test]
fn test_awk_call_function_no_args() {
    test_awk!(call_function_no_args);
}

#[test]
fn test_awk_scalar_arguments_are_passed_by_copy() {
    test_awk!(scalar_arguments_are_passed_by_copy);
}

#[test]
fn test_awk_array_arguments_are_passed_by_reference() {
    test_awk!(array_arguments_are_passed_by_reference);
}

#[test]
fn test_awk_call_function_with_less_arguments() {
    test_awk!(call_function_with_less_arguments);
}

#[test]
fn test_awk_recursive_function() {
    test_awk!(recursive_function);
}

#[test]
fn test_awk_mutually_recursive_functions() {
    test_awk!(mutually_recursive_functions);
}

#[test]
fn test_awk_empty_print_prints_the_whole_record() {
    test_awk!(
        empty_print_prints_the_whole_record,
        "tests/awk/test_data.txt"
    );
}

#[test]
fn test_awk_ere_pattern() {
    test_awk!(ere_pattern, "tests/awk/test_data.txt");
}

#[test]
fn test_awk_ere_outside_match_matches_record() {
    test_awk!(ere_outside_match_matches_record, "tests/awk/test_data.txt");
}

#[test]
fn test_awk_simple_getline() {
    test_awk!(simple_getline, "tests/awk/test_data.txt");
}

#[test]
fn test_awk_getline_into_var() {
    test_awk!(getline_into_var, "tests/awk/test_data.txt");
}

#[test]
fn test_awk_getline_from_file() {
    test_awk!(getline_from_file, "tests/awk/test_data.txt");
}

#[test]
fn test_awk_read_records_from_stdin() {
    run_test(TestPlan {
        cmd: String::from("awk"),
        args: vec![
            "-f".to_string(),
            "tests/awk/read_records_from_stdin.awk".to_string(),
            "-".to_string(),
        ],
        stdin_data: String::from(include_str!("awk/test_data.txt")),
        expected_out: String::from(include_str!("awk/read_records_from_stdin.out")),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn test_awk_cli_variable_assignment() {
    run_test(TestPlan {
        cmd: String::from("awk"),
        args: vec![
            "-f".to_string(),
            "tests/awk/cli_variable_assignment.awk".to_string(),
            "-v".to_string(),
            "variable=value1".to_string(),
            "-v".to_string(),
            "_variable=val\\nue2".to_string(),
            "-v".to_string(),
            "v2ar4_iable=\"value3\"".to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::from(include_str!("awk/cli_variable_assignment.out")),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn test_awk_variable_assignment_arguments() {
    test_awk!(variable_assignment_arguments, "tests/awk/test_data.txt");
}

#[test]
fn test_awk_correct_comparisons() {
    test_awk!(correct_comparisons, "tests/awk/test_data.txt");
}

#[test]
fn test_awk_execute_program_from_args() {
    run_test(TestPlan {
        cmd: String::from("awk"),
        args: vec!["BEGIN { print \"Hello, World!\" }".to_string()],
        stdin_data: String::new(),
        expected_out: String::from("Hello, World!\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    })
}

#[test]
fn test_awk_use_cli_provided_separator() {
    run_test(TestPlan {
        cmd: String::from("awk"),
        args: vec![
            "-F".to_string(),
            ",".to_string(),
            "-f".to_string(),
            "tests/awk/use_cli_provided_separator.awk".to_string(),
            "tests/awk/test_data.csv".to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::from(include_str!("awk/use_cli_provided_separator.out")),
        expected_err: String::from(""),
        expected_exit_code: 0,
    })
}

#[test]
fn test_awk_no_file_arguments_reads_from_stdin() {
    run_test(TestPlan {
        cmd: String::from("awk"),
        args: vec![
            "-f".to_string(),
            "tests/awk/no_file_arguments_reads_from_stdin.awk".to_string(),
        ],
        stdin_data: include_str!("awk/test_data.txt").to_string(),
        expected_out: include_str!("awk/no_file_arguments_reads_from_stdin.out").to_string(),
        expected_err: String::from(""),
        expected_exit_code: 0,
    })
}

#[test]
fn test_awk_multifile_program() {
    run_test(TestPlan {
        cmd: String::from("awk"),
        args: vec![
            "-f".to_string(),
            "tests/awk/multifile_program1.awk".to_string(),
            "-f".to_string(),
            "tests/awk/multifile_program2.awk".to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::from(include_str!("awk/multifile_program.out")),
        expected_err: String::from(""),
        expected_exit_code: 0,
    })
}

#[test]
fn test_awk_modifying_nf_recomputes_the_record() {
    test_awk!(
        modifying_nf_recomputes_the_record,
        "tests/awk/test_data.txt"
    );
}
