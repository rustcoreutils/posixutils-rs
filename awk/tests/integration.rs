use plib::{run_test, run_test_with_checker, TestPlan};

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
    ($test_name:ident $(,$data_file:expr)*) => {
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
    test_awk!(next);
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
        correct_append_output,
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
