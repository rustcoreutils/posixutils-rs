//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

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
fn test_awk_close_returns_status() {
    test_awk!(close_returns_status);
}

#[test]
fn test_awk_dash_f_escape_processing() {
    // POSIX: `-F sepstring` == `-v FS=sepstring`, so `\t` is a tab (a single
    // character), not the two-character string backslash-t.
    test_awk(
        vec![
            "-F".to_string(),
            "\\t".to_string(),
            "{ print length(FS), $2 }".to_string(),
            "tests/awk/tab_separated.txt".to_string(),
        ],
        "1 b\n",
    );
}

/// Run the program file `tests/awk/<name>.awk` in a UTF-8 locale and compare
/// its output with `<name>.out`. Characters are bytes in the C locale the
/// harness defaults to, so a test of multibyte characters names its locale.
fn test_awk_utf8(name: &str, expected_output: &str) {
    let Some(locale) = plib::testing::utf8_locale() else {
        return;
    };
    plib::testing::run_test_with_env(
        TestPlan {
            cmd: String::from("awk"),
            args: vec!["-f".to_string(), format!("tests/awk/{name}.awk")],
            stdin_data: String::new(),
            expected_out: String::from(expected_output),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("LC_ALL", locale.as_str())],
    );
}

#[test]
fn test_awk_multibyte_char_counts() {
    test_awk_utf8(
        "multibyte_char_counts",
        include_str!("awk/multibyte_char_counts.out"),
    );
}

#[test]
fn test_awk_printf_star_width() {
    test_awk!(printf_star_width);
}

#[test]
fn test_awk_substr_edges() {
    test_awk!(substr_edges);
}

#[test]
fn test_awk_case_mapping() {
    test_awk!(case_mapping);
}

#[test]
fn test_awk_getline_pipe_advances_nr() {
    test_awk!(getline_pipe_nr);
}

#[test]
fn test_awk_record_with_many_fields() {
    // A record with more than the old 1024-field cap must keep every field.
    test_awk!(many_fields, "tests/awk/many_fields.txt");
}

#[test]
fn test_awk_high_field_assignment() {
    test_awk!(high_field_assignment);
}

#[test]
fn test_awk_uninitialized_field_comparison() {
    // POSIX: nonexistent fields (85506) and empty fields from $0/FS (85511) have
    // the uninitialized value, so they compare numerically equal to 0 while
    // still being string-equal to "". Populated fields are unaffected.
    test_awk(
        vec![
            "-F".to_string(),
            ":".to_string(),
            "{ print ($5==0); print ($2==0); print ($2==\"\"); print ($1==\"a\") }".to_string(),
            "tests/awk/empty_fields.txt".to_string(),
        ],
        "1\n1\n1\n1\n",
    );
}

#[test]
fn test_awk_program_file_from_stdin() {
    // POSIX: a `-f` progfile of `-` denotes the standard input.
    run_test(TestPlan {
        cmd: String::from("awk"),
        args: vec!["-f".to_string(), "-".to_string()],
        stdin_data: String::from("BEGIN { print \"okprog\" }\n"),
        expected_out: String::from("okprog\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
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

// Bug fix regression tests

#[test]
fn test_awk_bugfix_atan2() {
    test_awk!(bugfix_atan2);
}

#[test]
fn test_awk_bugfix_printf_c() {
    test_awk!(bugfix_printf_c);
}

#[test]
fn test_awk_bugfix_string_as_regex() {
    test_awk!(bugfix_string_as_regex);
}

#[test]
fn test_awk_bugfix_escaped_backslash() {
    test_awk!(bugfix_escaped_backslash);
}

#[test]
fn test_awk_bugfix_line_continuation() {
    test_awk!(bugfix_line_continuation);
}

#[test]
fn test_awk_bugfix_field_numeric() {
    run_test(TestPlan {
        cmd: String::from("awk"),
        args: vec![
            "-f".to_string(),
            "tests/awk/bugfix_field_numeric.awk".to_string(),
        ],
        stdin_data: String::from("a 5\nb 30\nc 10\n"),
        expected_out: String::from(include_str!("awk/bugfix_field_numeric.out")),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn test_awk_bugfix_fs_empty() {
    run_test(TestPlan {
        cmd: String::from("awk"),
        args: vec![
            "-f".to_string(),
            "tests/awk/bugfix_fs_empty.awk".to_string(),
        ],
        stdin_data: String::from("abc\nhi\n"),
        expected_out: String::from(include_str!("awk/bugfix_fs_empty.out")),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn test_awk_bugfix_field_max_index() {
    // Generate a record with exactly 1024 fields (MAX_FIELDS) to verify
    // that accessing $1024 works after the off-by-one fix (0..MAX_FIELDS -> 0..=MAX_FIELDS)
    let fields: Vec<String> = (1..=1024).map(|i| i.to_string()).collect();
    let input = fields.join(" ") + "\n";
    run_test(TestPlan {
        cmd: String::from("awk"),
        args: vec!["{ print $1024 }".to_string()],
        stdin_data: input,
        expected_out: String::from("1024\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn test_awk_bugfix_trailing_newline_program() {
    run_test(TestPlan {
        cmd: String::from("awk"),
        args: vec!["BEGIN { print \"ok\" }\n".to_string()],
        stdin_data: String::new(),
        expected_out: String::from("ok\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn test_awk_bugfix_multichar_rs() {
    run_test(TestPlan {
        cmd: String::from("awk"),
        args: vec![
            "-v".to_string(),
            "RS=::".to_string(),
            "{ print NR, $0 }".to_string(),
        ],
        stdin_data: String::from("one::two::three"),
        expected_out: String::from("1 one\n2 two\n3 three\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

// POSIX: when RS is null (paragraph mode), newline is always a field separator
// regardless of FS value. Test with single-char FS.
#[test]
fn test_awk_paragraph_mode_newline_is_field_separator_char_fs() {
    run_test(TestPlan {
        cmd: String::from("awk"),
        args: vec!["BEGIN{RS=\"\"; FS=\":\"} {for(i=1;i<=NF;i++) print i, $i}".to_string()],
        stdin_data: String::from("a:b\nc:d\n\ne:f\n"),
        expected_out: String::from("1 a\n2 b\n3 c\n4 d\n1 e\n2 f\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

// POSIX: paragraph mode with ERE FS - newline is also a field separator.
#[test]
fn test_awk_paragraph_mode_newline_is_field_separator_ere_fs() {
    run_test(TestPlan {
        cmd: String::from("awk"),
        args: vec!["BEGIN{RS=\"\"; FS=\":+\"} {for(i=1;i<=NF;i++) print i, $i}".to_string()],
        stdin_data: String::from("a::b\nc::d\n\ne::f\n"),
        expected_out: String::from("1 a\n2 b\n3 c\n4 d\n1 e\n2 f\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn test_awk_bugfix_nextfile_nr() {
    test_awk!(
        bugfix_nextfile_nr,
        "tests/awk/test_data.txt",
        "tests/awk/test_data2.txt"
    );
}

// fflush() with no args should flush all and return 0
#[test]
fn test_awk_fflush_no_args() {
    run_test(TestPlan {
        cmd: String::from("awk"),
        args: vec!["BEGIN { print \"hello\"; ret = fflush(); print ret }".to_string()],
        stdin_data: String::new(),
        expected_out: String::from("hello\n0\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

// fflush("") should flush all and return 0
#[test]
fn test_awk_fflush_empty_string() {
    run_test(TestPlan {
        cmd: String::from("awk"),
        args: vec!["BEGIN { print \"hello\"; ret = fflush(\"\"); print ret }".to_string()],
        stdin_data: String::new(),
        expected_out: String::from("hello\n0\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

// POSIX: paragraph mode with default FS - newline is whitespace, so it
// naturally acts as a field separator already.
#[test]
fn test_awk_paragraph_mode_default_fs() {
    run_test(TestPlan {
        cmd: String::from("awk"),
        args: vec!["BEGIN{RS=\"\"} {for(i=1;i<=NF;i++) print i, $i}".to_string()],
        stdin_data: String::from("a b\nc d\n\ne f\n"),
        expected_out: String::from("1 a\n2 b\n3 c\n4 d\n1 e\n2 f\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

// Regression: numeric strings from input should compare numerically (POSIX)
#[test]
fn test_awk_bugfix_numstr_field_cmp() {
    run_test(TestPlan {
        cmd: String::from("awk"),
        args: vec![
            "-f".to_string(),
            "tests/awk/bugfix_numstr_field_cmp.awk".to_string(),
        ],
        stdin_data: String::from("03 3\n"),
        expected_out: String::from(include_str!("awk/bugfix_numstr_field_cmp.out")),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

// Regression: gsub with zero-width match must not panic on multi-byte UTF-8
#[test]
fn test_awk_bugfix_gsub_multibyte() {
    test_awk_utf8(
        "bugfix_gsub_multibyte",
        include_str!("awk/bugfix_gsub_multibyte.out"),
    );
}

// Regression: default SUBSEP must be \034 (0x1c), not space
#[test]
fn test_awk_bugfix_subsep_default() {
    test_awk!(bugfix_subsep_default);
}

// Regression: & in gsub replacement inserts matched text; \& inserts literal &
#[test]
fn test_awk_bugfix_gsub_ampersand() {
    test_awk!(bugfix_gsub_ampersand);
}

// Regression: system() must return the command exit status
#[test]
fn test_awk_bugfix_system_return() {
    run_test(TestPlan {
        cmd: String::from("awk"),
        args: vec!["BEGIN { ret = system(\"true\"); print ret }".to_string()],
        stdin_data: String::new(),
        expected_out: String::from("0\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

// Regression: NF = 0 must produce an empty record without panic
#[test]
fn test_awk_bugfix_nf_zero() {
    run_test(TestPlan {
        cmd: String::from("awk"),
        args: vec!["-f".to_string(), "tests/awk/bugfix_nf_zero.awk".to_string()],
        stdin_data: String::from("a b c\n"),
        expected_out: String::from(include_str!("awk/bugfix_nf_zero.out")),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

// Regression: > redirect must truncate existing files
#[test]
fn test_awk_bugfix_redirect_truncate() {
    let dir = plib::tmp::tempdir().expect("failed to create temp dir");
    let outfile = dir.path().join("out.txt");
    // Write a long initial content
    std::fs::write(&outfile, "this is long initial content\n").unwrap();
    let program = format!(
        "BEGIN {{ print \"short\" > \"{}\" }}",
        outfile.to_str().unwrap()
    );
    run_test(TestPlan {
        cmd: String::from("awk"),
        args: vec![program],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
    let contents = std::fs::read_to_string(&outfile).unwrap();
    assert_eq!(
        contents, "short\n",
        "file should be truncated on > redirect"
    );
}

// XBD 12.2, Guideline 7: an option-argument may begin with '-'. Each option
// below used to have the word after it refused as an unknown option.
#[test]
fn awk_option_argument_may_begin_with_hyphen() {
    for opt in ["-F", "-f", "-v"] {
        plib::testing::assert_hyphen_option_argument("awk", &[opt, "-zq", "--help"]);
    }
}

#[test]
fn awk_field_separator_begins_with_hyphen() {
    run_test(TestPlan {
        cmd: String::from("awk"),
        args: vec![
            String::from("-F"),
            String::from("-:"),
            String::from("{ print $2 }"),
        ],
        stdin_data: String::from("a-:b\n"),
        expected_out: String::from("b\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

// A '#' inside a regular expression literal, or inside a string, is part of
// that token and does not start a comment. autoconf's config.status uses
// `/^[\t ]*#[\t ]*(define|undef)[\t ]+/` and `sub(/#.*/, "")`.
#[test]
fn awk_hash_inside_regex_literal_is_not_a_comment() {
    let cases = [
        ("/#/", "x\na#b\n", "a#b\n"),
        ("/^#AT_START_/", "#AT_START_1\nAT_START_\n", "#AT_START_1\n"),
        (
            "/^[\\t ]*#[\\t ]*(define|undef)[\\t ]+/ { print $2 }",
            "# define FOO 1\n#undef BAR\nint x;\n",
            "define\nBAR\n",
        ),
        ("{ sub(/#.*/, \"\"); print }", "keep # drop\n", "keep \n"),
        ("/[#]/ { print \"br\" }", "a#\nb\n", "br\n"),
        ("{ print \"a#b\" } # trailing comment", "x\n", "a#b\n"),
        ("/a b/", "ab\na b\n", "a b\n"),
    ];
    for (program, input, output) in cases {
        run_test(TestPlan {
            cmd: String::from("awk"),
            args: vec![String::from(program)],
            stdin_data: String::from(input),
            expected_out: String::from(output),
            expected_err: String::new(),
            expected_exit_code: 0,
        });
    }
}

/// Run awk with `env` added to its environment and return its standard output
/// as raw bytes, asserting that it succeeded without diagnostics.
fn awk_bytes_with_env(args: &[&str], stdin: &[u8], env: &[(&str, &str)]) -> Vec<u8> {
    let args: Vec<String> = args.iter().map(|s| s.to_string()).collect();
    let output = plib::testing::run_test_base_with_env("awk", &args, stdin, env);
    assert_eq!(String::from_utf8_lossy(&output.stderr), "", "{args:?}");
    assert_eq!(output.status.code(), Some(0), "{args:?}");
    output.stdout
}

// In a single-byte locale a character is a byte: `%c` with a numeric
// argument writes the one byte whose value is the argument (modulo 256, as
// gawk and mawk do), never a UTF-8 encoding of it, and input bytes reach the
// output unchanged.
#[test]
fn awk_printf_c_writes_a_byte_in_the_c_locale() {
    let c = [("LC_ALL", "C")];
    let cases: [(&str, &[u8], &[u8]); 6] = [
        ("BEGIN { printf(\"%c\", 200) }", b"", b"\xc8"),
        (
            "BEGIN { s = sprintf(\"%c%c\", 200, 256 + 65); printf \"%s|%d\", s, length(s) }",
            b"",
            b"\xc8A|2",
        ),
        // A string argument gives its first character, which is a byte here.
        (
            "{ printf(\"%c|%c\", $0, \"\\303\\251\") }",
            b"\xe9x\n",
            b"\xe9|\xc3",
        ),
        // Bytes in, the same bytes out; length counts bytes.
        (
            "{ print length($1); print }",
            b"caf\xc3\xa9 \xff\n",
            b"5\ncaf\xc3\xa9 \xff\n",
        ),
        // A regular expression sees bytes: `.` matches one byte.
        ("{ sub(/./, \"x\"); print }", b"\xc3\xa9\n", b"x\xa9\n"),
        (
            "{ print index($0, \"\\251\"), substr($0, 2) }",
            b"\xc3\xa9\n",
            b"2 \xa9\n",
        ),
    ];
    for (program, input, expected) in cases {
        assert_eq!(
            awk_bytes_with_env(&[program], input, &c),
            expected,
            "{program}"
        );
    }
}

// In a UTF-8 locale `%c` writes the character with that code point.
#[test]
fn awk_printf_c_writes_utf8_in_a_utf8_locale() {
    let Some(locale) = plib::testing::utf8_locale() else {
        return;
    };
    let env = [("LC_ALL", locale.as_str())];
    let out = awk_bytes_with_env(
        &["BEGIN { printf(\"%c|%c\", 200, \"éx\"); s = \"é\"; print \"\", length(s) }"],
        b"",
        &env,
    );
    assert_eq!(out, "È|é 1\n".as_bytes());
}

// gsub replaces non-overlapping matches, and an empty match right where the
// previous match ended is not another one: gawk, mawk and the one true awk
// all turn "abc" into "XaXcX" for gsub(/b*/, "X").
#[test]
fn awk_gsub_skips_an_empty_match_after_a_match() {
    let cases = [
        ("{ gsub(/b*/, \"X\"); print }", "abc\n", "XaXcX\n"),
        ("{ n = gsub(/b*/, \"X\"); print n }", "abbc\n", "3\n"),
        ("{ gsub(/x*/, \"-\"); print }", "abc\n", "-a-b-c-\n"),
        ("{ gsub(/a*/, \"X\"); print }", "aab\n", "XbX\n"),
    ];
    for (program, input, output) in cases {
        run_test(TestPlan {
            cmd: String::from("awk"),
            args: vec![String::from(program)],
            stdin_data: String::from(input),
            expected_out: String::from(output),
            expected_err: String::new(),
            expected_exit_code: 0,
        });
    }
}

/// Run awk on `program` with a 20 second limit, so that a hang fails the test
/// instead of the whole run; returns stdout, stderr and the exit status.
fn awk_with_deadline(program: &str) -> (String, String, Option<i32>) {
    awk_with_deadline_input(program, "")
}

/// `awk_with_deadline`, with `input` on standard input.
fn awk_with_deadline_input(program: &str, input: &str) -> (String, String, Option<i32>) {
    use std::io::Write;
    use std::time::{Duration, Instant};
    let mut child = std::process::Command::new(plib::testing::get_binary_path("awk"))
        .arg(program)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .unwrap();
    // the input is small, so writing it all before reading cannot deadlock;
    // awk may exit without reading it
    let mut stdin = child.stdin.take().unwrap();
    if let Err(error) = stdin.write_all(input.as_bytes()) {
        assert_eq!(error.kind(), std::io::ErrorKind::BrokenPipe);
    }
    drop(stdin);
    let start = Instant::now();
    while child.try_wait().unwrap().is_none() {
        if start.elapsed() > Duration::from_secs(20) {
            child.kill().unwrap();
            child.wait().unwrap();
            panic!("awk never finished with {program:?}");
        }
        std::thread::sleep(Duration::from_millis(50));
    }
    let out = child.wait_with_output().unwrap();
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.code(),
    )
}

// After a syntax error awk looks for more errors by reparsing from each later
// `}`, BEGIN, END or `function`.  When the reparse from a keyword failed too,
// the next search found that same keyword again at offset 0 and awk spun for
// ever; texinfo's texindex.awk hung bash's documentation build this way.
#[test]
fn awk_syntax_error_before_a_failing_function_terminates() {
    let programs = [
        "BEGIN { @ }\nfunction f() { @ }\n",
        "BEGIN { @ }\nBEGIN { @ }\n",
        "BEGIN { @ }\nEND { @ }\nEND { @ }\n",
        "{ @ } function",
    ];
    for program in programs {
        let (stdout, stderr, status) = awk_with_deadline(program);
        assert_eq!(stdout, "", "{program:?}");
        assert!(!stderr.is_empty(), "{program:?}");
        assert_ne!(status, Some(0), "{program:?}");
    }
}

// A keyword is a whole word, and only BEGIN and END are reserved, in
// capitals.  `begin`, `end` and `foreach` are ordinary names, and so is a
// name that starts with a keyword: `nextchar` is not `next` followed by
// `char`, `exitcode = 4` does not exit, and `elsewhere` after an `if` is not
// its `else`.  texindex.awk has `function join(array, start, end, sep)` and
// `nextchar = kchars[3]`.
#[test]
fn awk_keywords_are_whole_words() {
    let cases = [
        ("BEGIN { nextchar = 1; print nextchar }", "1\n"),
        (
            "BEGIN { breakx = 2; continued = 3; print breakx, continued }",
            "2 3\n",
        ),
        ("BEGIN { exitcode = 4; print exitcode }", "4\n"),
        (
            "BEGIN { returned = 5; doit = 6; print returned, doit }",
            "5 6\n",
        ),
        (
            "BEGIN { printer = 7; printfx = 8; print printer, printfx }",
            "7 8\n",
        ),
        (
            "BEGIN { deleted = 9; getlines = 10; print deleted, getlines }",
            "9 10\n",
        ),
        (
            "BEGIN { if (0) x = 1\nelsewhere = 11; print elsewhere }",
            "11\n",
        ),
        ("BEGIN { if (0) x = 1; else print 12 }", "12\n"),
        (
            "function j(a, start, end) { return start end } BEGIN { print j(0, 1, 2) }",
            "12\n",
        ),
        ("BEGIN { begin = 3; end = 4; print begin + end }", "7\n"),
        ("BEGIN { foreach = \"f\"; print foreach }", "f\n"),
        ("BEGIN { endx = 1; Begin = 2; print endx, Begin }", "1 2\n"),
    ];
    for (program, output) in cases {
        run_test(TestPlan {
            cmd: String::from("awk"),
            args: vec![String::from(program)],
            stdin_data: String::new(),
            expected_out: String::from(output),
            expected_err: String::new(),
            expected_exit_code: 0,
        });
    }
}

// A newline may follow `&&`, `||` and the comma of a parameter list, with a
// comment before it (POSIX awk, Lexical Conventions); texindex.awk relies on
// all three.
#[test]
fn awk_newline_after_and_or_and_parameter_comma() {
    let cases = [
        ("BEGIN { if (1 &&\n 2) print \"and\" }", "and\n"),
        ("BEGIN { if (0 ||  # why\n\n 2) print \"or\" }", "or\n"),
        ("BEGIN { x = 1 &&\n 0; print x }", "0\n"),
        (
            "function q(a,\t# parameters\n\tb) { return a b }\nBEGIN { print q(1, 2) }",
            "12\n",
        ),
        (
            "function q(a,\n\n b,\n c) { return c }\nBEGIN { print q(1, 2, 3) }",
            "3\n",
        ),
        ("/x/ &&\n/y/ { print }", "xy\n"),
    ];
    for (program, output) in cases {
        run_test(TestPlan {
            cmd: String::from("awk"),
            args: vec![String::from(program)],
            stdin_data: String::from("xy\nx\n"),
            expected_out: String::from(output),
            expected_err: String::new(),
            expected_exit_code: 0,
        });
    }
}

// Each of the three clauses of `for (init; cond; update)` may be empty, and
// so may the body; an empty condition is true.  The compiler took the
// children in order and panicked when one was missing (texindex.awk has
// `for (; i <= j; i++)`).
#[test]
fn awk_for_clauses_may_be_empty() {
    let cases = [
        (
            "BEGIN { i = 1; for (; i <= 3; i++) printf i; print \"\" }",
            "123\n",
        ),
        (
            "BEGIN { for (i = 1; ; i++) if (i > 2) break; print i }",
            "3\n",
        ),
        ("BEGIN { for (i = 1; i < 3;) i++; print i }", "3\n"),
        (
            "BEGIN { for (;;) { n++; if (n == 4) break }; print n }",
            "4\n",
        ),
        ("BEGIN { for (i = 0; i < 5; i++); print i }", "5\n"),
        ("BEGIN { for (i = 0; i < 6; i++) {}\nprint i }", "6\n"),
        (
            "BEGIN { for (i = 0; i < 4; i++) { if (i % 2) continue; s = s i }; print s }",
            "02\n",
        ),
        (
            "BEGIN { i = 0; for (;; i++) if (i == 7) break; print i }",
            "7\n",
        ),
    ];
    for (program, output) in cases {
        run_test(TestPlan {
            cmd: String::from("awk"),
            args: vec![String::from(program)],
            stdin_data: String::new(),
            expected_out: String::from(output),
            expected_err: String::new(),
            expected_exit_code: 0,
        });
    }
}

// An unset variable passed to a function becomes an array in the caller when
// the function uses its parameter as one (filling it with split, assigning
// an element, passing it on), as in gawk, mawk and the one true awk; a
// scalar assigned to the parameter stays local.  texindex.awk's char_split
// fills its caller's local with split(string, array, "").
#[test]
fn awk_unset_argument_becomes_the_callers_array() {
    let cases = [
        (
            "function f(arr) { split(\"x y\", arr) } BEGIN { f(b); print length(b), b[2] }",
            "2 y\n",
        ),
        (
            "function f(arr) { arr[1] = \"set\" } BEGIN { f(b); print b[1] }",
            "set\n",
        ),
        (
            "function f(arr) { return split(\"p q\", arr) }
             function g(  loc, n) { n = f(loc); return n \"-\" loc[2] }
             BEGIN { print g() }",
            "2-q\n",
        ),
        (
            "function h(a) { split(\"m n\", a) } function f(arr) { h(arr) }
             BEGIN { f(b); print b[2] }",
            "n\n",
        ),
        (
            "function g(b) { b[\"k\"] = \"v\" } function f(a) { g(a); return a[\"k\"] }
             BEGIN { print f(x), x[\"k\"] }",
            "v v\n",
        ),
        (
            "function f(a) { a = 5; return a } BEGIN { print f(x); print \"[\" x \"]\" }",
            "5\n[]\n",
        ),
        (
            "function f(a) { a++; a++; return a } function g(  l) { f(l); return \"[\" l \"]\" }
             BEGIN { print g() }",
            "[]\n",
        ),
        (
            "function f(a) { print \"r\" a } BEGIN { f(x); x = 3; print x }",
            "r\n3\n",
        ),
    ];
    for (program, output) in cases {
        run_test(TestPlan {
            cmd: String::from("awk"),
            args: vec![String::from(program)],
            stdin_data: String::new(),
            expected_out: String::from(output),
            expected_err: String::new(),
            expected_exit_code: 0,
        });
    }
}

// An original program written in the style of texinfo's texindex.awk, which
// hung, then failed to parse, then rejected every entry.  The expected
// output is gawk's (mawk agrees).
#[test]
fn test_awk_sort_index_entries() {
    test_awk!(sort_index_entries, "tests/awk/sort_index_entries.txt");
}

// An empty match is one match, wherever the search finds it: gsub(/$/, "X")
// appended "XX" to the record, because the empty match at the end was found
// again from its own position.
#[test]
fn awk_empty_match_at_end_is_counted_once() {
    let cases = [
        ("{ n = gsub(/$/, \"X\"); print n, $0 }", "1 abcX\n"),
        ("{ n = gsub(/b*$/, \"X\"); print n, $0 }", "1 abcX\n"),
        ("{ n = gsub(/c*$/, \"X\"); print n, $0 }", "1 abX\n"),
        ("{ n = gsub(/x*/, \"-\"); print n, $0 }", "4 -a-b-c-\n"),
    ];
    for (program, output) in cases {
        run_test(TestPlan {
            cmd: String::from("awk"),
            args: vec![String::from(program)],
            stdin_data: String::from("abc\n"),
            expected_out: String::from(output),
            expected_err: String::new(),
            expected_exit_code: 0,
        });
    }
}

// An empty match of a field separator does not separate fields, in split()
// and in FS alike, as in gawk and mawk: split("abc", a, /x*/) is one field,
// and /b*/ splits "abc" only at the "b".
#[test]
fn awk_empty_separator_match_does_not_split() {
    let cases = [
        ("{ n = split($0, a, /x*/); print n, a[1] }", "1 abc\n"),
        ("{ n = split($0, a, /b*/); print n, a[1], a[2] }", "2 a c\n"),
        ("{ n = split($0, a, /$/); print n, a[1] }", "1 abc\n"),
        ("BEGIN { FS = \"b*\" } { print NF, $1, $2 }", "2 a c\n"),
        ("BEGIN { FS = \"x*\" } { print NF, $1 }", "1 abc\n"),
    ];
    for (program, output) in cases {
        run_test(TestPlan {
            cmd: String::from("awk"),
            args: vec![String::from(program)],
            stdin_data: String::from("abc\n"),
            expected_out: String::from(output),
            expected_err: String::new(),
            expected_exit_code: 0,
        });
    }
}

// Each syntax error is reported once, at its own line and column: errors
// found after the first were placed relative to where the search for them
// resumed, and found again from every later checkpoint.
#[test]
fn awk_later_syntax_errors_report_their_own_position() {
    let tmp = plib::tmp::TempDir::new().unwrap();
    let program = tmp.path().join("two.awk");
    std::fs::write(&program, "BEGIN { @ }\n\nfunction f() {\n  x = 1 @\n}\n").unwrap();
    let args = vec![String::from("-f"), program.to_string_lossy().into_owned()];
    let output = plib::testing::run_test_base("awk", &args, b"");
    let stderr = String::from_utf8_lossy(&output.stderr);
    let locations: Vec<&str> = stderr
        .lines()
        .filter_map(|line| line.trim_start().strip_prefix("--> "))
        .collect();
    let file = program.to_string_lossy();
    assert_eq!(
        locations,
        [format!("{file}:1:9"), format!("{file}:4:9")],
        "{stderr}"
    );
    assert_ne!(output.status.code(), Some(0));
}

// A string converts to a number as C's strtod reads it, skipping leading
// white space: " 12" + 1 is 13, and a field " 12 " is 12.
#[test]
fn awk_string_to_number_skips_leading_white_space() {
    let cases = [
        (
            "BEGIN { print \" 12\" + 1, \"\\t5\" * 2, \" +3\" + 0, \" -.5e1x\" + 0 }",
            "",
            "13 10 3 -5\n",
        ),
        (
            "BEGIN { print \"\\n7\" + 0, \"\\v8\" + 0, \"\\f9\" + 0, \"\\r4\" + 0 }",
            "",
            "7 8 9 4\n",
        ),
        ("{ print $1 + 0, $1 * 2 }", " 12 |x\n", "12 24\n"),
    ];
    for (program, input, output) in cases {
        run_test(TestPlan {
            cmd: String::from("awk"),
            args: vec![String::from("-F|"), String::from(program)],
            stdin_data: String::from(input),
            expected_out: String::from(output),
            expected_err: String::new(),
            expected_exit_code: 0,
        });
    }
}

// A field is a numeric string only if all of it, apart from leading and
// trailing blanks, looks like a number: "12abc", "12.5f" and "12 x" compare
// as strings ("12abc" > 9 is false), " 12 " and "+12" as numbers.
#[test]
fn awk_numeric_string_is_the_whole_field() {
    let cases = [
        ("12", "1"),
        ("12abc", "0"),
        ("12.5f", "0"),
        ("12 x", "0"),
        ("12e", "0"),
        ("+-12", "0"),
        ("--12", "0"),
        ("0x1A", "0"),
        (".", "0"),
        (" 12 ", "1"),
        ("\t12\t", "1"),
        ("+12", "1"),
        ("12e1", "1"),
        ("1e+1x", "0"),
        ("12.", "1"),
        (".12e2", "1"),
    ];
    let input: String = cases
        .iter()
        .map(|(field, _)| format!("{field}|\n"))
        .collect();
    let output: String = cases.iter().map(|(_, gt)| format!("{gt}\n")).collect();
    run_test(TestPlan {
        cmd: String::from("awk"),
        args: vec![String::from("-F|"), String::from("{ print ($1 > 9) }")],
        stdin_data: input,
        expected_out: output,
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

// Parsing must not take time exponential in how deeply expressions nest.
// Each parenthesised level was parsed about eight times over, by the
// alternatives that failed (`| getline`, `? :`, `(i, j) in`) before the one
// that matched, so five levels took a second and eight never finished.
#[test]
fn awk_deeply_nested_expressions_parse_quickly() {
    let depth = 40;
    let nested =
        |open: &str, close: &str| format!("{}1{}", open.repeat(depth), close.repeat(depth));
    let cases = [
        (format!("BEGIN {{ print {} }}", nested("(", ")")), "1\n"),
        (
            format!("BEGIN {{ x = {}; print x }}", nested("(", ")")),
            "1\n",
        ),
        (
            format!("BEGIN {{ print {} + 1 }}", nested("-(", ")")),
            "2\n",
        ),
        (
            format!("BEGIN {{ print {} }}", nested("length(", ")")),
            "1\n",
        ),
        (
            format!("BEGIN {{ print {} }}", nested("(1 ? ", " : 0)")),
            "1\n",
        ),
        (
            format!(
                "BEGIN {{ a[1, 2]; if (({}, 2) in a) print \"in\" }}",
                nested("(", ")")
            ),
            "in\n",
        ),
        (
            format!(
                "BEGIN {{ while ((\"echo x\" | getline v) > 0) print {} v }}",
                nested("(", ")")
            ),
            "1x\n",
        ),
    ];
    for (program, output) in cases {
        let (stdout, stderr, status) = awk_with_deadline(&program);
        assert_eq!(
            (stdout.as_str(), stderr.as_str()),
            (output, ""),
            "{program}"
        );
        assert_eq!(status, Some(0), "{program}");
    }
}

// Nested subscripts and field references parse in linear time too: each
// level was parsed once as a possible assignment target and again as an
// operand, so 20 levels of a[a[...]] never finished.  The check that skips
// the first parse must not miss a real assignment.
#[test]
fn awk_nested_lvalues_parse_quickly() {
    let depth = 40;
    let nested = |open: &str, inner: &str, close: &str| {
        format!("{}{inner}{}", open.repeat(depth), close.repeat(depth))
    };
    let cases = [
        (
            format!("BEGIN {{ a[1] = 1; print {} }}", nested("a[", "1", "]")),
            "1\n",
        ),
        (
            format!(
                "BEGIN {{ a[1] = 1; {} = 7; print a[1] }}",
                nested("a[", "1", "]")
            ),
            "7\n",
        ),
        (format!("{{ print {} }}", nested("$(", "1", ")")), "1\n"),
        (
            format!("{{ {} = \"z\"; print }}", nested("$(", "1", ")")),
            "z\n",
        ),
        // assignments the shape check cannot read must still be assignments
        (
            String::from("BEGIN { a[\"]\"] = 5; a[\"[\"] += 1; print a[\"]\"], a[\"[\"] }"),
            "5 1\n",
        ),
        (
            String::from("BEGIN { i = 4; a[i/2] = 3; a[i / 2] ^= 2; print a[2] }"),
            "9\n",
        ),
        (
            String::from("{ i = 0; $++i = \"z\"; $(i + 0) = $i \"y\"; print }"),
            "zy\n",
        ),
        (
            String::from("BEGIN { a[1, 2] = 3; a [1] = 4; print a[1, 2], a[1] }"),
            "3 4\n",
        ),
        (
            String::from("BEGIN { a[1] = 1; print (a[1] == 1), a[1] = 2, a[1] }"),
            "1 2 2\n",
        ),
        (String::from("{ x = $1; $1 = \"b\"; print x, $0 }"), "1 b\n"),
    ];
    for (program, output) in cases {
        let (stdout, stderr, status) = awk_with_deadline_input(&program, "1\n");
        assert_eq!(
            (stdout.as_str(), stderr.as_str()),
            (output, ""),
            "{program}"
        );
        assert_eq!(status, Some(0), "{program}");
    }
}

// `s = s t` appends to s in place, so building a string piece by piece is
// linear: it copied the whole of s two or three times per append, and two
// million appends never finished.  The result is the same as concatenating:
// a number on the left is converted with CONVFMT, a numeric string from
// input stops being numeric, and anything that may change s while the
// right-hand side is evaluated keeps the ordinary evaluation order.
#[test]
fn awk_append_to_a_variable_is_linear() {
    let cases = [
        (
            "BEGIN { for (i = 0; i < 2000000; i++) s = s \"x\"; print length(s) }",
            "2000000\n",
        ),
        (
            "function f(  l, i) { for (i = 0; i < 2000000; i++) l = l \"ab\"; return length(l) }
             BEGIN { print f() }",
            "4000000\n",
        ),
        (
            "BEGIN { s = 0.1; CONVFMT = \"%.2f\"; s = s \"|\" 1 + 1 \"|\" 2; print s }",
            "0.10|2|2\n",
        ),
        ("{ s = $1; s = s \"\"; print (s < 9) }", "1\n"),
        ("BEGIN { s = \"a\"; s = s s s; print s }", "aaa\n"),
        ("BEGIN { s = \"a\"; s = s (s = \"b\"); print s }", "ab\n"),
        (
            "BEGIN { s = \"a\"; s = s sub(/a/, \"c\", s) s; print s }",
            "a1c\n",
        ),
        (
            "function g() { s = \"z\"; return \"y\" } BEGIN { s = \"a\"; s = s g(); print s }",
            "ay\n",
        ),
        ("BEGIN { s = \"a\"; s = s 1 < 2; print s }", "0\n"),
        ("BEGIN { s = \"a\"; s = s \"b\" ~ /ab/; print s }", "1\n"),
        ("BEGIN { s = 3; s = s - 1; print s }", "2\n"),
        (
            "BEGIN { x[1] = \"a\"; s = \"q\"; s = s x[1] substr(\"bcd\", 2) toupper(s); print s }",
            "qacdQ\n",
        ),
        ("{ $0 = $0 \"y\"; print $1, NF }", "10y 1\n"),
    ];
    for (program, output) in cases {
        let (stdout, stderr, status) = awk_with_deadline_input(program, "10\n");
        assert_eq!(
            (stdout.as_str(), stderr.as_str()),
            (output, ""),
            "{program}"
        );
        assert_eq!(status, Some(0), "{program}");
    }
}

// An empty match of a regular-expression RS does not end a record, as in
// mawk: RS = "()" or "x*" made every read return an empty record without
// consuming input, so awk printed empty records for ever (gawk's test
// rsnullre).  "b*" still separates at each run of b's.
#[test]
fn awk_empty_rs_match_does_not_end_a_record() {
    let cases = [
        (
            "BEGIN { RS = \"()\" } { printf \"<%s>\", $0 }",
            "foo\n",
            "<foo\n>",
        ),
        (
            "BEGIN { RS = \"x*\" } { printf \"<%s>\", $0 }",
            "foo\n",
            "<foo\n>",
        ),
        (
            "BEGIN { RS = \"b*\" } { printf \"<%s>\", $0 }",
            "abba\ncd",
            "<a><a\ncd>",
        ),
        (
            "BEGIN { RS = \"b*\" } { printf \"<%s>\", $0 }",
            "xabbay",
            "<xa><ay>",
        ),
        (
            "BEGIN { RS = \"b*|;\" } { printf \"<%s>\", $0 }",
            "a;b;c",
            "<a><><><c>",
        ),
    ];
    for (program, input, output) in cases {
        let (stdout, stderr, status) = awk_with_deadline_input(program, input);
        assert_eq!(
            (stdout.as_str(), stderr.as_str()),
            (output, ""),
            "{program}"
        );
        assert_eq!(status, Some(0), "{program}");
    }
}

// A runtime error after a function call reports the caller's location.
// Returning from a call kept the callee's table of source locations, so
// the report indexed past its end and awk panicked instead.
#[test]
fn awk_runtime_error_after_a_call_reports_the_caller() {
    let program = "function f(a, b, c, d, e) { return 1 }\nBEGIN { f(); printf(\"%z\") }";
    let (stdout, stderr, status) = awk_with_deadline(program);
    assert_eq!(stdout, "");
    assert_eq!(
        stderr,
        "awk: runtime error: not enough arguments for format string\ncall trace:\n=> <start> at :2:14\n\n"
    );
    assert_eq!(status, Some(1));
}

// `$` binds tighter than `++` and `--`, so `$NF++` increments the last
// field and `$i++` leaves i alone; awk incremented the field's index
// instead.  In `$$i++` the increment goes to the inner field, as in gawk
// and mawk, so `$$a++++` is `$($a++)++` (gawk's test parse1), and so is
// one after a unary operator: `$+i++` is `$(+(i++))` (gawk's test prec).
#[test]
fn awk_field_reference_binds_tighter_than_increment() {
    let cases = [
        ("{ $NF++; print }", "1 2\n", "1 3\n"),
        ("{ $2--; print }", "1 2\n", "1 1\n"),
        ("{ i = 1; print $i++; print i, $0 }", "5 6\n", "5\n1 6 6\n"),
        ("{ i = 1; print $$i++; print }", "2 5 6\n", "5\n3 5 6\n"),
        (
            "BEGIN { a = 3 } { print $$a++++; print }",
            "3 4 5 6 7 8 9\n",
            "7\n3 4 6 6 8 8 9\n",
        ),
        ("{ i = 1; print $++i, $i^2, $i-1 }", "5 6\n", "6 36 5\n"),
        ("{ i = 1; $!i++; $+i++; print i, $0 }", "5 6\n", "3 5 6\n"),
    ];
    for (program, input, output) in cases {
        let (stdout, stderr, status) = awk_with_deadline_input(program, input);
        assert_eq!(
            (stdout.as_str(), stderr.as_str()),
            (output, ""),
            "{program}"
        );
        assert_eq!(status, Some(0), "{program}");
    }
}

// An operand can have more than one unary operator: `!!x` was a syntax
// error.
#[test]
fn awk_operand_takes_several_unary_operators() {
    let (stdout, stderr, status) =
        awk_with_deadline("BEGIN { x = 2; print !!x, - -x, !-x, -!x, !!!\"\", 1 - -1 }");
    assert_eq!((stdout.as_str(), stderr.as_str()), ("1 2 0 0 1 2\n", ""));
    assert_eq!(status, Some(0));
}

// getline is an operand: `getline > 0` compares its result and `getline x
// y` concatenates y to it, where both were syntax errors (gawk's tests
// getline, getline2, getline3 and inputred).  As in gawk, `getline < f`
// takes no concatenation into the file name, and `cmd | getline` binds
// tighter than a comparison but looser than a concatenation.
#[test]
fn awk_getline_is_an_operand() {
    let cases = [
        ("NR == 1 { while (getline > 0) n++; print n }", "a\nb\nc\n", "2\n"),
        ("NR == 1 { a = (getline x y); print a, x }", "l1\nl2\n", "1 l2\n"),
        ("NR == 1 { print (getline x - 2), x }", "a\nb\n", "-1 b\n"),
        (
            "BEGIN { x = getline line < \"/nonexistent\" \".txt\"; print x; print getline line < \"/nonexistent\" }",
            "",
            "-1.txt\n-1\n",
        ),
        ("BEGIN { print (getline line < \"/dev/null\" > -1) }", "", "1\n"),
        ("BEGIN { y = 7; print (\"echo 4\" | getline x y), x }", "", "17 4\n"),
        ("BEGIN { \"echo a\" \"b\" | getline; print }", "", "ab\n"),
        (
            "BEGIN { while (\"echo z\" | getline line > 0) n++; print n, line }",
            "",
            "1 z\n",
        ),
        ("BEGIN { print (\"echo 5\" | getline x + 1), x }", "", "2 5\n"),
    ];
    for (program, input, output) in cases {
        let (stdout, stderr, status) = awk_with_deadline_input(program, input);
        assert_eq!(
            (stdout.as_str(), stderr.as_str()),
            (output, ""),
            "{program}"
        );
        assert_eq!(status, Some(0), "{program}");
    }
}

// A print list can hold `==`, `!=`, `<` and `<=` without parentheses, as
// in gawk, mawk and busybox awk; `>` still redirects (gawk's test
// gsubtst3).
#[test]
fn awk_print_list_takes_comparisons() {
    let program = "BEGIN { x = 1; print x == 1, x != 1, x < 2, x <= 0, x == 1 ? \"y\" : \"n\"; printf \"%s %s\\n\", x == 1, x != 1 ? \"a\" : \"b\"; print x == 1 > \"/dev/stdout\" }";
    let (stdout, stderr, status) = awk_with_deadline(program);
    assert_eq!(
        (stdout.as_str(), stderr.as_str()),
        ("1 0 1 0 y\n1 b\n1\n", "")
    );
    assert_eq!(status, Some(0));
}

// `exit` in BEGIN skips the input but runs the END actions, `exit` with no
// expression keeps the status of an earlier `exit expr` (gawk's test
// exitval3), and `exit` in a function called from a pattern ends the
// program instead of panicking.
#[test]
fn awk_exit_skips_input_and_keeps_its_status() {
    let cases = [
        (
            "BEGIN { exit 3 } { print \"main\" } END { print \"end\" }",
            "end\n",
            3,
        ),
        ("BEGIN { exit 42 } END { exit }", "", 42),
        ("{ exit 4 } END { exit }", "", 4),
        (
            "function f() { exit 3 } f() { print \"matched\" } END { print \"end\" }",
            "end\n",
            3,
        ),
        (
            "BEGIN { exit 1 } END { print \"e1\"; exit 6; print \"no\" } END { print \"e2\" }",
            "e1\n",
            6,
        ),
    ];
    for (program, output, exit_status) in cases {
        let (stdout, stderr, status) = awk_with_deadline_input(program, "x\n");
        assert_eq!(
            (stdout.as_str(), stderr.as_str()),
            (output, ""),
            "{program}"
        );
        assert_eq!(status, Some(exit_status), "{program}");
    }
}

// Plain getline reads the main input: in BEGIN it opens the first file
// (standard input here), and at the end of a file it goes on to the next
// operand, performing assignments on the way; in END there is no more
// input (gawk's tests getline2 and gsubtst3).  FILENAME is empty until a
// file is opened.
#[test]
fn awk_getline_reads_the_main_input() {
    let cases = [
        (
            "BEGIN { while ((getline l) > 0) n++; print n, NR }",
            "3 3\n",
        ),
        (
            "BEGIN { getline; print \"begin\", $0, NR } { print \"main\", $0, NR }",
            "begin a 1\nmain b 2\nmain c 3\n",
        ),
        (
            "BEGIN { printf \"[%s]\", FILENAME; getline; print FILENAME }",
            "[]-\n",
        ),
    ];
    for (program, output) in cases {
        let (stdout, stderr, status) = awk_with_deadline_input(program, "a\nb\nc\n");
        assert_eq!(
            (stdout.as_str(), stderr.as_str()),
            (output, ""),
            "{program}"
        );
        assert_eq!(status, Some(0), "{program}");
    }
    test_awk(
        vec![
            "{ n = 0; while ((getline l) > 0) n++; print FILENAME, FNR, NR, n, x } END { print NR, (getline l) }".to_string(),
            "tests/awk/test_data2.txt".to_string(),
            "x=5".to_string(),
            "tests/awk/test_data3.txt".to_string(),
        ],
        "tests/awk/test_data3.txt 6 11 10 5\n11 0\n",
    );
}

// Deleting the element at the end of an array's storage, the last one
// added or a slot a `for (k in a)` loop emptied, panicked.
#[test]
fn awk_delete_the_last_stored_element() {
    let cases = [
        (
            "BEGIN { a[1]; a[2]; delete a[2]; print length(a); for (k in a) print k }",
            "1\n1\n",
        ),
        (
            "BEGIN { a[1]; a[2]; a[3]; for (k in a) if (k == 3) delete a[k]; delete a[1]; print length(a); for (k in a) print k }",
            "1\n2\n",
        ),
    ];
    for (program, output) in cases {
        let (stdout, stderr, status) = awk_with_deadline(program);
        assert_eq!(
            (stdout.as_str(), stderr.as_str()),
            (output, ""),
            "{program}"
        );
        assert_eq!(status, Some(0), "{program}");
    }
}

// `break` and `continue` work in a `for (k in a)` loop, and leaving one by
// `break`, `return` or `next` releases the array, which could then never
// be added to again.  Reading a missing element inside such a loop creates
// it instead of failing (gawk's test delarpm2); the loop does not visit
// it, as in gawk and mawk.
#[test]
fn awk_for_in_loop_can_be_left_and_its_array_added_to() {
    let cases = [
        (
            "BEGIN { a[1]; a[2]; a[3]; for (k in a) { n++; break }; for (k in a) { if (k == 2) continue; m++ }; a[4]; print n, m, length(a) }",
            "1 2 4\n",
        ),
        (
            "function f(arr, k) { for (k in arr) return k } BEGIN { a[1]; a[2]; f(a); a[3] = 1; print length(a) }",
            "3\n",
        ),
        (
            "{ for (k in seen) next } { seen[$0] } END { seen[\"z\"]; print length(seen) }",
            "2\n",
        ),
        (
            "BEGIN { a[1]; a[2]; for (k in a) if (a[k \"x\"] == \"\") n++; print n, length(a) }",
            "2 4\n",
        ),
        (
            "BEGIN { a[1]; a[2]; for (k in a) { delete a; a[\"q\"] }; print length(a) }",
            "1\n",
        ),
    ];
    for (program, output) in cases {
        let (stdout, stderr, status) = awk_with_deadline_input(program, "a\nb\nc\n");
        assert_eq!(
            (stdout.as_str(), stderr.as_str()),
            (output, ""),
            "{program}"
        );
        assert_eq!(status, Some(0), "{program}");
    }
}

// `break` and `continue` work in a do-while loop, where they were
// rejected as being outside a loop.
#[test]
fn awk_do_while_takes_break_and_continue() {
    let program = "BEGIN { do { n++; break } while (1); do { m++; if (m < 3) continue; break } while (1); i = 0; while (i < 2) { i++; do { break } while (1); k++ }; print n, m, k }";
    let (stdout, stderr, status) = awk_with_deadline(program);
    assert_eq!((stdout.as_str(), stderr.as_str()), ("1 3 2\n", ""));
    assert_eq!(status, Some(0));
}

// close() of a pipe returns the command's exit status, or 256 plus the
// signal that killed it, as gawk and mawk do (gawk's tests close_status
// and status-close); it returned 0 for any command.
#[test]
fn awk_close_of_a_pipe_returns_the_exit_status() {
    let program = "BEGIN { print \"x\" | \"cat >/dev/null; exit 9\"; print close(\"cat >/dev/null; exit 9\"); \"echo hi; exit 3\" | getline; print close(\"echo hi; exit 3\"); \"kill -9 $$\" | getline; print close(\"kill -9 $$\"); print close(\"echo hi; exit 3\") }";
    let (stdout, stderr, status) = awk_with_deadline(program);
    assert_eq!((stdout.as_str(), stderr.as_str()), ("9\n3\n265\n-1\n", ""));
    assert_eq!(status, Some(0));
}

// The `#` flag follows printf(3): `%#x` of 0 has no 0x prefix, and `%#o`
// makes the first digit a zero even with a precision that leaves room for
// none (gawk's test printf1).
#[test]
fn awk_printf_alternative_form_of_zero() {
    let program =
        "BEGIN { printf \"%#.0o|%#x|%#X|%#o|%#.2o|%#x|%#5x|\\n\", 0, 0, 0, 0, 8, 255, 0 }";
    let (stdout, stderr, status) = awk_with_deadline(program);
    assert_eq!(
        (stdout.as_str(), stderr.as_str()),
        ("0|0|0|0|010|0xff|    0|\n", "")
    );
    assert_eq!(status, Some(0));
}

// printf `%s` counts its width and precision in characters, so a byte
// above 127 in the C locale is one column (gawk's test rebt8b1), and a
// precision that falls inside a UTF-8 character panicked.
#[test]
fn awk_printf_string_width_counts_characters() {
    let c = [("LC_ALL", "C")];
    let out = awk_bytes_with_env(
        &["BEGIN { printf \"%-5s|%.1s|%3s|\\n\", \"a\\351b\", \"\\351x\", \"\\351\" }"],
        b"",
        &c,
    );
    assert_eq!(out, b"a\xe9b  |\xe9|  \xe9|\n");
    let Some(locale) = plib::testing::utf8_locale() else {
        return;
    };
    let env = [("LC_ALL", locale.as_str())];
    let out = awk_bytes_with_env(
        &["BEGIN { printf \"%-5s|%.1s|%3s|\\n\", \"éa\", \"éab\", \"é\" }"],
        b"",
        &env,
    );
    assert_eq!(out, "éa   |é|  é|\n".as_bytes());
}

// The escape sequences of awk EREs, which regcomp does not know, are
// translated: `/\t/` never matched a tab, nor `/\141/` an "a", in an ERE
// token or a string used as one.  Inside a bracket expression a backslash
// escapes too, as in gawk and mawk (gawk's test regrange), and `\8` is a
// plain 8, not a back-reference (gawk's test back89).
#[test]
fn awk_ere_escape_sequences() {
    let cases = [
        (
            "/a\\tb/ { print \"tab\" } $0 ~ \"a\\\\tb\" { print \"dyntab\" } /[\\t]/ { print \"brtab\" } /\\141/ { print \"oct\" }",
            "a\tb\n",
            "tab\ndyntab\nbrtab\noct\n",
        ),
        ("/a\\8b/ { print \"a8b\" }", "a8b\n", "a8b\n"),
        ("{ gsub(/\\//, \"|\"); print }", "a/b\n", "a|b\n"),
        ("/a\\.b/ { print \"wrong\" } { print \"ok\" }", "axb\n", "ok\n"),
        (
            "{ print (\"\\\\\" ~ /[\\\\]/), (\"]\" ~ /[\\]]/), (\"a\" ~ /[\\]a]/), (\"-\" ~ /[a\\-z]/), (\"b\" ~ /[a\\-z]/), (\"^\" ~ /[\\^x]/) }",
            "x\n",
            "1 1 1 1 0 1\n",
        ),
    ];
    for (program, input, output) in cases {
        let (stdout, stderr, status) = awk_with_deadline_input(program, input);
        assert_eq!(
            (stdout.as_str(), stderr.as_str()),
            (output, ""),
            "{program}"
        );
        assert_eq!(status, Some(0), "{program}");
    }
    // a range is the C locale's byte order only there
    let out = awk_bytes_with_env(
        &["{ print ($0 ~ /[\\[-\\]]/) }"],
        b"\\\n",
        &[("LC_ALL", "C")],
    );
    assert_eq!(out, b"1\n");
}

// A `/` inside a bracket expression does not end an ERE token, as in gawk
// and mawk (gawk's test regexpbrack).
#[test]
fn awk_slash_in_bracket_expression_of_ere_token() {
    let program = "/^[]+()0-9.,$%/'\"-]*$/ { print \"num\", $0 } /[/]/ { print \"m\" } /[[:alpha:]/]x/ { print \"no\" }";
    let (stdout, stderr, status) = awk_with_deadline_input(program, "a/b\n12/3\n");
    assert_eq!((stdout.as_str(), stderr.as_str()), ("m\nnum 12/3\nm\n", ""));
    assert_eq!(status, Some(0));
}

// sub and gsub leave their target alone when nothing matched: a field was
// assigned anyway, which rebuilt $0 with OFS (gawk's test gsubtst7), and a
// number became a string.
#[test]
fn awk_sub_without_a_match_assigns_nothing() {
    let cases = [
        (
            "{ gsub(\"foo\", \"bar\", $1); print; sub(/x/, \"y\", $2); print; gsub(/q/, \"z\"); print }",
            " aaa  b\n",
            " aaa  b\n aaa  b\n aaa  b\n",
        ),
        ("BEGIN { x = 10; gsub(/q/, \"\", x); print (x < 9) }", "", "0\n"),
    ];
    for (program, input, output) in cases {
        let (stdout, stderr, status) = awk_with_deadline_input(program, input);
        assert_eq!(
            (stdout.as_str(), stderr.as_str()),
            (output, ""),
            "{program}"
        );
        assert_eq!(status, Some(0), "{program}");
    }
}

// Using a scalar as an array is a runtime error, where indexing it, `in`,
// delete and split panicked (gawk's tests prmarscl and scalar).
#[test]
fn awk_scalar_used_as_an_array_is_an_error() {
    let programs = [
        "function f(a) { print a[1] } BEGIN { j = 4; f(j) }",
        "BEGIN { x = 1; x[1] = 2 }",
        "BEGIN { x = 1; print x[1] }",
        "BEGIN { x = 1; print (1 in x) }",
        "BEGIN { x = 1; delete x[1] }",
        "BEGIN { x = 1; delete x }",
        "BEGIN { x = 1; split(\"a b\", x) }",
        "BEGIN { sub(/x/, \"\", a); a[1] }",
    ];
    for program in programs {
        let (stdout, stderr, status) = awk_with_deadline(program);
        assert_eq!(stdout, "", "{program}");
        assert!(
            stderr.starts_with("awk: runtime error: scalar used in array context\n"),
            "{program}: {stderr}"
        );
        assert_eq!(status, Some(1), "{program}");
    }
}

// A number used as a regular expression is its string value, as POSIX says
// for any expression on the right of `~`: `"a" ~ ("a" ~ "a")` failed with
// "expected extended regular expression".
#[test]
fn awk_number_used_as_a_regular_expression() {
    let program = "BEGIN { print (\"a\" ~ (\"a\" ~ \"a\")), (\"1\" ~ 1), (\"x10\" ~ 5*2), match(\"a.5\", 0.5), s = \"123\", sub(2, \"z\", s), s }";
    let (stdout, stderr, status) = awk_with_deadline(program);
    assert_eq!(
        (stdout.as_str(), stderr.as_str()),
        ("0 1 1 0 123 1 1z3\n", "")
    );
    assert_eq!(status, Some(0));
}

// A write error on output that does not end in a <newline> is reported: that
// output sat in stdout's line buffer until exit, where the error was lost.
#[test]
fn test_awk_reports_write_error_on_final_partial_line() {
    plib::testing::assert_write_error_on_full_device("awk", &["{ printf $0 }"], b"x", 1);
    plib::testing::assert_write_error_on_full_device("awk", &["BEGIN { printf \"x\" }"], b"", 1);
    plib::testing::assert_write_error_on_full_device(
        "awk",
        &["BEGIN { printf \"x\"; exit 3 }"],
        b"",
        3,
    );
}

// XBD 12.1: `-F=` is the field separator "=", not `-F` with an empty one
// (an empty FS split every character into its own field).
#[test]
fn awk_attached_field_separator_may_begin_with_equals() {
    for (args, input, out) in [
        (&["-F=", "{print $2}"][..], "a=b\n", "b\n"),
        (&["-F==", "{print $2}"][..], "a==b\n", "b\n"),
    ] {
        run_test(TestPlan {
            cmd: String::from("awk"),
            args: args.iter().map(|s| s.to_string()).collect(),
            stdin_data: String::from(input),
            expected_out: String::from(out),
            expected_err: String::new(),
            expected_exit_code: 0,
        });
    }
    plib::testing::assert_equals_option_argument("awk", "-F", &["{print $2}"], b"a=b\n");
}

#[cfg(target_os = "linux")]
#[test]
fn awk_write_error_on_full_device() {
    // A failed write to standard output is "awk: write error: REASON",
    // with the system's text for the error and exit status 1.
    plib::testing::assert_write_error_message_on_full_device("awk", &["{ print }"], b"x\n", 1);
}

#[cfg(target_os = "linux")]
#[test]
fn awk_begin_print_write_error_on_full_device() {
    // A failed write to standard output is "awk: write error: REASON",
    // with the system's text for the error and exit status 1.
    plib::testing::assert_write_error_message_on_full_device(
        "awk",
        &["BEGIN { print \"x\" }"],
        b"",
        1,
    );
}

/// A file operand or output file that cannot be opened is reported with the
/// system's text for the error, without Rust's "(os error N)" suffix.
#[test]
fn test_awk_open_errors_name_the_system_error() {
    let missing = "/nonexistent/awk-file";
    let open_error = plib::diag::io_error_text(&std::fs::File::open(missing).unwrap_err());
    for args in [
        vec!["1".to_string(), missing.to_string()],
        vec![format!("BEGIN {{ print \"x\" > \"{missing}\" }}")],
    ] {
        let out = plib::testing::run_test_base("awk", &args, b"");
        let stderr = String::from_utf8_lossy(&out.stderr);
        assert_ne!(out.status.code(), Some(0), "{args:?}");
        assert!(stderr.contains(&open_error), "{args:?}: {stderr:?}");
        assert!(!stderr.contains("(os error"), "{args:?}: {stderr:?}");
    }
}

/// Runs each `(program, input, expected stdout)` case, expecting no
/// diagnostics and exit status 0.
fn assert_awk_cases(cases: &[(&str, &str, &str)]) {
    for &(program, input, output) in cases {
        let (stdout, stderr, status) = awk_with_deadline_input(program, input);
        assert_eq!(
            (stdout.as_str(), stderr.as_str()),
            (output, ""),
            "{program}"
        );
        assert_eq!(status, Some(0), "{program}");
    }
}

// An array element being assigned is found again by its key after the
// right-hand side runs: a function there that deleted from the array, or
// cleared or split into it, made the assignment land on another element or
// panic.
#[test]
fn awk_array_element_assigned_after_rhs_changes_the_array() {
    assert_awk_cases(&[
        (
            "function f() { delete a[\"x\"]; return 5 } BEGIN { a[\"x\"] = 1; a[\"y\"] = 2; a[\"x\"] = f(); print a[\"x\"], a[\"y\"], length(a) }",
            "",
            "5 2 2\n",
        ),
        (
            "function f() { delete a; return 5 } BEGIN { a[\"x\"] = 1; a[\"x\"] = f(); print a[\"x\"], length(a) }",
            "",
            "5 1\n",
        ),
        (
            "function f() { delete a[\"x\"]; return 5 } BEGIN { a[\"x\"] = 1; a[\"x\"] += f(); print a[\"x\"], length(a) }",
            "",
            "5 1\n",
        ),
        (
            "function f() { split(\"p q r\", a); return 5 } BEGIN { a[\"x\"] = 1; a[\"y\"] = 2; a[\"x\"] = f(); print a[1], a[2], a[3], a[\"x\"], length(a) }",
            "",
            "p q r 5 4\n",
        ),
        (
            "function f() { delete a; return \"v\" } BEGIN { a[1] = \"old\"; sub(/o/, f(), a[1]); print length(a), \"[\" a[1] \"]\" }",
            "",
            "1 []\n",
        ),
    ]);
}

// With RS = "", a last paragraph of one line with no newline after it is
// still a record; it was dropped.
#[test]
fn awk_paragraph_mode_keeps_a_last_line_without_newline() {
    assert_awk_cases(&[
        (
            "BEGIN { RS = \"\" } { print NR \":\" $0 }",
            "a\n\nb",
            "1:a\n2:b\n",
        ),
        (
            "BEGIN { RS = \"\" } { print NR \":\" $0 }",
            "abc",
            "1:abc\n",
        ),
        (
            "BEGIN { RS = \"\" } { print NR \":\" $0 }",
            "a\n\n\nb\nc",
            "1:a\n2:b\nc\n",
        ),
    ]);
}

// An integral number converts to the string of its exact value, as by
// "%d", also beyond the range of a 64-bit integer: it saturated to
// 9223372036854775807, so that large subscripts named one element.
#[test]
fn awk_large_integers_convert_exactly() {
    assert_awk_cases(&[
        (
            "BEGIN { x = 1e30; print x; print x \"\"; print 2^63, -2^63, -2^64, 2^53 + 1 }",
            "",
            "1000000000000000019884624838656\n1000000000000000019884624838656\n9223372036854775808 -9223372036854775808 -18446744073709551616 9007199254740992\n",
        ),
        (
            "BEGIN { a[1e19]; a[2e19]; print length(a); for (k in a) n += (k == 1e19 || k == 2e19); print n }",
            "",
            "2\n2\n",
        ),
    ]);
}

// %g, and so OFMT and CONVFMT, chooses between %e and %f style from the
// exponent of the rounded value, as C does, and keeps the zeros of the
// integer part: 120000.25 printed as "12" and 100000 as "1".  Width and
// padding apply to the whole number in every float conversion.
#[test]
fn awk_float_conversions_follow_c() {
    assert_awk_cases(&[
        (
            "BEGIN { print 100000.5, 120000.25, 999999.5, 300000.1 \"\", 1234567.5, 0.000123456, 123456.7 }",
            "",
            "100000 120000 1e+06 300000 1.23457e+06 0.000123456 123457\n",
        ),
        (
            "BEGIN { printf \"%g|%g|%g|%.3g|%g|%G|%#g|%#.3g|%.0g|%g|%g\\n\", 100000, 999999.5, 0.00001, 99950, 9.99999999e-5, 1e-300, 1e30, 100, 123, 0.5, 0 }",
            "",
            "100000|1e+06|1e-05|1e+05|0.0001|1E-300|1.00000e+30|100.|1e+02|0.5|0\n",
        ),
        (
            "BEGIN { printf \"%8g|%-8g|%08g|%+g|% g|%8g|%-9g|%08.3g|\\n\", -1e30, -1e30, -1e30, 1e30, 1e-30, -1.5, 120000, -0.0001 }",
            "",
            "  -1e+30|-1e+30  |-001e+30|+1e+30| 1e-30|    -1.5|120000   |-00.0001|\n",
        ),
        (
            "BEGIN { printf \"%e|%10.3e|%-12e|%012.2e|%#.0e|%.0e|%E\\n\", 9.9999999e9, -1234.5, 1.5, -1.5, 5e10, 5e10, 1e-300 }",
            "",
            "1.000000e+10|-1.234e+03|1.500000e+00|-0001.50e+00|5.e+10|5e+10|1.000000E-300\n",
        ),
        (
            "BEGIN { printf \"%-10.2f|%010.2f|%#.0f|%.0f|%+.1f|%8.3f|\\n\", -1.5, -1.5, 3, 2.5, 0.05, 3.14159 }",
            "",
            "-1.50     |-000001.50|3.|2|+0.1|   3.142|\n",
        ),
        (
            "BEGIN { printf \"%5g|%-6f|%05e|%5G|\\n\", -log(0), log(0), -log(0), -log(0) }",
            "",
            "  inf|-inf  |  inf|  INF|\n",
        ),
    ]);
}

// printf's integer conversions take any number: %d writes every digit of a
// value beyond 64 bits (it saturated), %u, %o and %x write a negative
// value as its 64-bit two's complement (it was a fatal error), and one
// beyond what they can show is written as by %g, as gawk does.
#[test]
fn awk_printf_integer_conversions_of_any_number() {
    assert_awk_cases(&[
        (
            "BEGIN { printf \"%x|%X|%u|%o\\n\", -1, -255, -1, -1 }",
            "",
            "ffffffffffffffff|FFFFFFFFFFFFFF01|18446744073709551615|1777777777777777777777\n",
        ),
        (
            "BEGIN { printf \"%d|%i|%5d|%-34d|\\n\", 1e30, -2^64, 2^63, 1e30 }",
            "",
            "1000000000000000019884624838656|-18446744073709551616|9223372036854775808|1000000000000000019884624838656   |\n",
        ),
        (
            "BEGIN { printf \"%x|%u|%8o|%u\\n\", 1e30, 2^64, -1e30, 2^64 - 2^12 }",
            "",
            "1e+30|1.84467e+19|  -1e+30|18446744073709547520\n",
        ),
        (
            "BEGIN { printf \"%d|%x\\n\", -log(0), log(0) }",
            "",
            "inf|-inf\n",
        ),
    ]);
}

// With the default FS, fields are separated by blanks and newlines only: a
// carriage return or form feed is part of a field, as POSIX says and the
// other awks do.  CR ending a line of a CRLF file was taken as a
// separator, changing NF and dropping the CR from a rebuilt record.
#[test]
fn awk_default_fs_splits_on_blanks_and_newlines_only() {
    assert_awk_cases(&[
        (
            "{ print NF; $1 = $1; print length($0), ($NF == \"\\r\") }",
            "a b \r\n",
            "3\n5 1\n",
        ),
        ("{ print NF }", "a\x0cb\n a\tb \n", "1\n2\n"),
        (
            "BEGIN { print split(\"a\\rb\", x), split(\" a \\n b\\t\", y), y[2] }",
            "",
            "1 2 b\n",
        ),
    ]);
}

// A regular-expression RS is matched only where more input cannot extend
// the match: RS = "\n+" ended a record at the first of two newlines read,
// making an empty record of the second.
#[test]
fn awk_regex_rs_match_is_not_cut_at_a_read_boundary() {
    assert_awk_cases(&[
        (
            "BEGIN { RS = \"\\n+\" } { print NR \":\" $0 }",
            "a\n\nb\n",
            "1:a\n2:b\n",
        ),
        (
            "BEGIN { RS = \"\\n\\n+\" } { print NR \":\" $0 }",
            "a\nb\n\n\n\nc\n",
            "1:a\nb\n2:c\n\n",
        ),
        (
            "BEGIN { RS = \"x+\" } { print NR \":\" $0 }",
            "axxxb",
            "1:a\n2:b\n",
        ),
    ]);
}

// Changing RS from a regular expression to one character, or to "", keeps
// the input the regular expression's reader had read ahead; it was lost.
#[test]
fn awk_rs_change_keeps_input_read_ahead() {
    assert_awk_cases(&[
        (
            "BEGIN { RS = \";;\" } NR == 1 { RS = \"\\n\" } { print NR \":\" $0 }",
            "a;;b\nc\nd\n",
            "1:a\n2:b\n3:c\n4:d\n",
        ),
        (
            "BEGIN { RS = \";;\" } NR == 1 { RS = \"\" } { print NR \":\" $0 }",
            "a;;b\nc\n\nd\n",
            "1:a\n2:b\nc\n3:d\n",
        ),
    ]);
}

/// Starts awk on `program`, writes `input` to it and, with its standard
/// input still open, waits up to 10 seconds for a first line of output:
/// the record awk returns before the end of its input.
fn awk_first_line_before_eof(program: &str, input: &str) -> Option<String> {
    use std::io::{BufRead, Write};
    use std::time::Duration;
    let mut child = std::process::Command::new(plib::testing::get_binary_path("awk"))
        .arg(program)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .spawn()
        .unwrap();
    let mut stdin = child.stdin.take().unwrap();
    stdin.write_all(input.as_bytes()).unwrap();
    stdin.flush().unwrap();
    let stdout = child.stdout.take().unwrap();
    let (sender, receiver) = std::sync::mpsc::channel();
    std::thread::spawn(move || {
        let mut line = String::new();
        let _ = std::io::BufReader::new(stdout).read_line(&mut line);
        let _ = sender.send(line);
    });
    let line = receiver.recv_timeout(Duration::from_secs(10)).ok();
    drop(stdin);
    let _ = child.kill();
    child.wait().unwrap();
    line
}

// A record is returned as soon as its separator is read, without waiting
// for more input, when more input could not make the separator longer.
#[test]
fn awk_record_is_returned_before_more_input() {
    for (program, input) in [
        ("{ print \"got \" $0; fflush() }", "abc\n"),
        (
            "BEGIN { RS = \"\\n|;\" } { print \"got \" $0; fflush() }",
            "abc\n",
        ),
        // a separator with no newline in it, read as soon as it comes
        (
            "BEGIN { RS = \";;\" } { print \"got \" $0; fflush() }",
            "abc;;",
        ),
        (
            "BEGIN { RS = \"\" } { print \"got \" $0; fflush() }",
            "abc\n\n",
        ),
        // a quoted repetition character repeats nothing
        (
            "BEGIN { RS = \"[+]\\n\" } { print \"got \" $0; fflush() }",
            "abc+\n",
        ),
        (
            "BEGIN { RS = \"\\\\*\\n\" } { print \"got \" $0; fflush() }",
            "abc*\n",
        ),
    ] {
        assert_eq!(
            awk_first_line_before_eof(program, input).as_deref(),
            Some("got abc\n"),
            "{program}"
        );
    }
}

// With RS = "", newline separates fields whatever FS is, also when $0 is
// split again after it is assigned or read by plain getline.
#[test]
fn awk_paragraph_mode_resplit_uses_newline_too() {
    assert_awk_cases(&[
        (
            "BEGIN { RS = \"\"; FS = \":\" } { $0 = \"d:e\\nf\"; print NF, $3 }",
            "x\n",
            "3 f\n",
        ),
        (
            "BEGIN { RS = \"\"; FS = \":\" } NR == 1 { getline; print NF, $3 }",
            "a:b\nc\n\nd:e\nf\n",
            "3 f\n",
        ),
    ]);
}

// A record may have more than 65535 fields, the limit a 16-bit field
// index set: such a record ended the run with "too many fields".
#[test]
fn awk_record_with_more_than_65535_fields() {
    let line = format!("{}\n", vec!["x"; 70_001].join(","));
    assert_awk_cases(&[
        (
            "BEGIN { FS = \",\" } { print NF, $70001, length($0) }",
            &line,
            "70001 x 140001\n",
        ),
        (
            "BEGIN { $100000 = \"y\"; print NF, $100000 }",
            "",
            "100000 y\n",
        ),
    ]);
}

// A negative field number is an error, as in gawk and mawk; it was taken
// as $0, so $(-1) = "x" replaced the record.
#[test]
fn awk_negative_field_is_an_error() {
    for program in [
        "{ print $(-1) }",
        "{ $(-1) = \"x\"; print }",
        "{ $(NF - 3)++ }",
    ] {
        let (stdout, stderr, status) = awk_with_deadline_input(program, "a b\n");
        assert_eq!(stdout, "", "{program}");
        assert!(
            stderr.contains("attempt to access field -"),
            "{program}: {stderr}"
        );
        assert_ne!(status, Some(0), "{program}");
    }
    assert_awk_cases(&[("{ print $(-0.5), $1.9 }", "a b\n", "a b a\n")]);
}

// NR and FNR count past 2^32 records; they were 32-bit and wrapped to 0.
#[test]
fn awk_record_counters_go_past_32_bits() {
    assert_awk_cases(&[
        (
            "NR == 1 { NR = 4294967295; FNR = 4294967295 } { print NR, FNR }",
            "a\nb\n",
            "4294967295 4294967295\n4294967296 4294967296\n",
        ),
        (
            "NR == 1 { NR = 5e9 } END { print NR }",
            "a\nb\n",
            "5000000001\n",
        ),
        // a count at the end of the range stays there
        (
            "BEGIN { NR = 1e19 } END { print (NR > 9e18) }",
            "a\nb\n",
            "1\n",
        ),
    ]);
}

/// Runs awk with `args` and `input`, returning stdout, stderr and the
/// exit status.
fn awk_args(args: &[&str], input: &str) -> (String, String, Option<i32>) {
    let args: Vec<String> = args.iter().map(|s| s.to_string()).collect();
    let out = plib::testing::run_test_base("awk", &args, input.as_bytes());
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.code(),
    )
}

// A backslash before a character with no escape meaning stays, with the
// character, as in the one true awk and mawk, in a string literal, -v, -F
// and an assignment operand: so -F '\|' is a literal bar and "\." used as
// a regular expression a literal period.  (It was a fatal error, then
// dropped with a warning as gawk does, which changed what such a regular
// expression matches.)  A backslash at the end of an assigned value stays.
#[test]
fn awk_undefined_escape_keeps_its_backslash() {
    for (args, input, out) in [
        (&["-F", "\\|", "{ print $2 }"][..], "a|b\n", "b\n"),
        (&["-v", "x=a\\.b", "BEGIN { print x }"][..], "", "a\\.b\n"),
        (&["{ print x }", "x=C:\\dir", "-"][..], "l\n", "C:\\dir\n"),
        (&["BEGIN { print \"a\\.b\" }"][..], "", "a\\.b\n"),
        (
            &["-v", "re=\\.", "{ print ($0 ~ re) }"][..],
            "ab\na.b\n",
            "0\n1\n",
        ),
        (&["{ print ($0 ~ \"\\.\") }"][..], "ab\n", "0\n"),
        (&["-v", "x=a\\", "BEGIN { print x }"][..], "", "a\\\n"),
        (&["BEGIN { print \"a\\\nb\" }"][..], "", "ab\n"),
    ] {
        let (stdout, stderr, status) = awk_args(args, input);
        assert_eq!((stdout.as_str(), stderr.as_str()), (out, ""), "{args:?}");
        assert_eq!(status, Some(0), "{args:?}");
    }
}

/// Whether the system has the locale `name`, as `locale -a` lists it.
fn locale_available(name: &str) -> bool {
    let wanted = name.to_lowercase().replace("utf-8", "utf8");
    std::process::Command::new("locale")
        .arg("-a")
        .output()
        .map(|out| {
            String::from_utf8_lossy(&out.stdout)
                .lines()
                .any(|l| l.to_lowercase().replace("utf-8", "utf8") == wanted)
        })
        .unwrap_or(false)
}

// String comparisons other than == and != follow the collation order of
// the locale (POSIX); == and != compare the strings themselves.
#[test]
fn awk_string_comparison_uses_collation() {
    let program = "BEGIN { print (\"a\" < \"B\"), (\"B\" > \"a\"), (\"a\" <= \"a\"), (\"a\" == \"A\"), (\"b\" != \"b\") }";
    let run = |locale: &str| {
        let out = std::process::Command::new(plib::testing::get_binary_path("awk"))
            .arg(program)
            .env("LC_ALL", locale)
            .output()
            .unwrap();
        String::from_utf8_lossy(&out.stdout).into_owned()
    };
    assert_eq!(run("C"), "0 0 1 0 0\n");
    if locale_available("en_US.UTF-8") {
        assert_eq!(run("en_US.UTF-8"), "1 1 1 0 0\n");
    }
}

// An assignment that would create billions of fields at once is an error,
// not an allocation that aborts the process or exhausts memory.
#[test]
fn awk_huge_field_creation_is_an_error() {
    for program in [
        "BEGIN { NF = 2000000000 }",
        "BEGIN { $2147483647 = 1 }",
        "BEGIN { $1e300 = 1 }",
        // nor many steps that each add a few million
        "BEGIN { for (i = 1; i <= 128; i++) NF = i * 2^19 }",
    ] {
        let (_, stderr, status) = awk_with_deadline(program);
        assert_eq!(status, Some(1), "{program}: {stderr}");
        assert!(stderr.contains("field"), "{program}: {stderr}");
        assert!(stderr.len() < 200, "{program}: {stderr}");
    }
}

// A record whose separator could still grow is returned once more input
// shows that it did not, also after a long record: that delay made the
// next check wait until the read-ahead doubled.
#[test]
fn awk_growable_rs_record_is_returned_when_more_input_comes() {
    let input = format!("{}\nb\n", "a".repeat(1000));
    assert_eq!(
        awk_first_line_before_eof(
            "BEGIN { RS = \"\\n+\" } { print \"got \" length($0); fflush() }",
            &input
        )
        .as_deref(),
        Some("got 1000\n")
    );
}

// A NUL byte in the input is an ordinary character: it ended the run with
// "string contains a NUL character".  Regular expressions match on either
// side of it, with ^ and $ anchored at the ends of the whole string.
#[test]
fn awk_nul_bytes_are_ordinary_characters() {
    assert_awk_cases(&[
        ("{ print NR, length($0) }", "a\0b\nc\n", "1 3\n2 1\n"),
        (
            "{ print ($0 ~ /b/), ($0 ~ /^b/), ($0 ~ /a$/), ($0 ~ /^a/), ($0 ~ /b$/), ($0 ~ \"a.b\") }",
            "a\0b\n",
            "1 0 0 1 1 0\n",
        ),
        ("{ gsub(/b/, \"X\"); print }", "a\0b\0b\n", "a\0X\0X\n"),
        (
            "{ n = split($0, x, /,/); print n, length(x[2]), match(x[2], /r/), RSTART }",
            "p,q\0r,s\n",
            "3 3 3 3\n",
        ),
        ("BEGIN { FS = \",+\" } { print NF, $2 }", "a\0b,,c\n", "2 c\n"),
        ("BEGIN { RS = \";;\" } { print length($0) }", "a\0b;;c", "3\n1\n"),
        ("BEGIN { RS = \";+\" } { print length($0) }", "a\0b;;c", "3\n1\n"),
        ("BEGIN { s = \"a\\0b\"; print length(s) }", "", "3\n"),
        // od's layout differs between systems; its bytes in hex do not
        (
            "BEGIN { od = \"od -An -tx1 | tr -d ' \\\\n'\"; printf \"a%cb\", 0 | od; close(od); print \"\" }",
            "",
            "610062\n",
        ),
    ]);
}

// Diagnostics begin with "awk: ", and a file that cannot be opened is
// named in the message.
#[test]
fn awk_diagnostics_name_the_utility_and_the_file() {
    let missing = "/nonexistent/awk-file";
    let reason = plib::diag::io_error_text(&std::fs::File::open(missing).unwrap_err());
    let (_, stderr, status) = awk_args(&["1", missing], "");
    assert_eq!(
        stderr,
        format!("awk: cannot open file '{missing}': {reason}\n")
    );
    assert_eq!(status, Some(1));
    let program = format!("BEGIN {{ print \"x\" > \"{missing}\" }}");
    let (_, stderr, status) = awk_args(&[&program], "");
    assert!(
        stderr.starts_with(&format!(
            "awk: runtime error: cannot open file '{missing}' for writing: {reason}\n"
        )),
        "{stderr}"
    );
    assert_eq!(status, Some(1));
    let (_, stderr, _) = awk_args(&["BEGIN { $(-1) }"], "");
    assert!(
        stderr.starts_with("awk: runtime error: attempt to access field -1\n"),
        "{stderr}"
    );
    let (_, stderr, status) = awk_args(&["-f", missing], "");
    assert_eq!(
        stderr,
        format!("awk: cannot open file '{missing}': {reason}\n")
    );
    assert_eq!(status, Some(1));
    let (_, stderr, status) = awk_args(&[], "");
    assert_eq!(stderr, "awk: missing program argument\n");
    assert_eq!(status, Some(1));
}

// system() returns 256 plus the signal number for a command killed by a
// signal, as close() of a pipe does and as gawk and mawk do; it returned
// 128 plus the number.
#[test]
fn awk_system_status_of_a_signalled_command() {
    assert_awk_cases(&[(
        "BEGIN { print system(\"kill -9 $$\"); print \"\" | \"kill -9 $$\"; print close(\"kill -9 $$\") }",
        "",
        "265\n265\n",
    )]);
}

// Dynamic regular expressions are compiled once and reused; many different
// ones, more than the cache holds, still each match as they should.
#[test]
fn awk_dynamic_regexes_stay_correct_past_the_cache() {
    assert_awk_cases(&[(
        "{ for (i = 0; i < 200; i++) n += ($0 ~ (\"^\" (i % 100) \"$\")); for (i = 0; i < 3; i++) m += ($0 ~ \"x\") } END { print n, m }",
        "5\n50\n500\nx\n",
        "4 3\n",
    )]);
}

// A function may recurse tens of thousands of calls deep; the operand
// stack held 2048 values, so about a thousand calls overflowed it.
#[test]
fn awk_deep_recursion() {
    assert_awk_cases(&[(
        "function f(k) { if (k == 0) return 0; return 1 + f(k - 1) } BEGIN { print f(20000) }",
        "",
        "20000\n",
    )]);
}

// A search after an earlier match sees the character before it, so a word
// boundary is found only where there is one: gsub(/\</, "X") put an X
// before every letter but the first of each word.  (Apple's regcomp has no
// \< or \>.)
#[cfg(not(target_vendor = "apple"))]
#[test]
fn awk_regex_search_after_a_match_sees_the_character_before() {
    assert_awk_cases(&[(
        "BEGIN { s = \"ab cd\"; t = s; gsub(/\\</, \"X\", s); gsub(/\\>/, \"Y\", t); print s, t }",
        "",
        "Xab Xcd abY cdY\n",
    ), (
        "{ gsub(/\\</, \"X\"); print }",
        "ab cd\0ef gh\n",
        "Xab Xcd\0Xef Xgh\n",
    )]);
}

// regexec cannot be given a NUL byte, so a dynamic regular expression
// holding one is an error that says so.
#[test]
fn awk_regex_with_nul_is_an_error_saying_so() {
    let (_, stderr, status) = awk_with_deadline("BEGIN { print (\"a\" ~ \"\\0\") }");
    assert!(
        stderr
            .starts_with("awk: runtime error: a regular expression cannot hold a NUL character\n"),
        "{stderr}"
    );
    assert_eq!(status, Some(1));
}

// Reclaiming the slot a for-in loop emptied costs time for that slot, not
// for the whole array: deleting one element per loop left by break,
// 40000 times over, took half a minute.
#[test]
fn awk_reclaiming_slots_after_each_loop_is_quick() {
    assert_awk_cases(&[(
        "BEGIN { for (i = 0; i < 40000; i++) a[i]; for (j = 0; j < 40000; j++) for (k in a) { delete a[k]; break }; print length(a) }",
        "",
        "0\n",
    )]);
}

// The call trace of a runtime error in a deep recursion shows the
// innermost calls and the outermost, not every one of thousands.
#[test]
fn awk_call_trace_of_a_deep_recursion_is_short() {
    let program = "function f(k) { return k == 0 ? $(-1) : f(k - 1) } BEGIN { f(5000) }";
    let (_, stderr, status) = awk_with_deadline(program);
    assert_eq!(status, Some(1));
    let lines: Vec<&str> = stderr.lines().filter(|l| !l.is_empty()).collect();
    assert!(lines.len() < 20, "{} lines", lines.len());
    assert_eq!(lines[0], "awk: runtime error: attempt to access field -1");
    assert!(lines.contains(&"=> ... 4991 more calls"), "{stderr}");
    assert_eq!(lines.last().copied(), Some("=> <start> at :1:60"));
}

// A printf field width or precision beyond what C allows (INT_MAX) is a
// runtime error; it was taken as given, and the allocation for it
// aborted the process, or a literal one overflowed silently.
#[test]
fn awk_huge_printf_width_is_an_error() {
    for program in [
        "BEGIN { printf \"%*d\", 1e15, 1 }",
        "BEGIN { printf \"%.*f\", 1e15, 1 }",
        "BEGIN { printf \"%99999999999999999999d\", 1 }",
        "BEGIN { x = sprintf(\"%.3000000000s\", \"a\") }",
    ] {
        let (_, stderr, status) = awk_with_deadline(program);
        assert_eq!(status, Some(1), "{program}: {stderr}");
        assert!(stderr.contains("too large"), "{program}: {stderr}");
    }
}

// fflush of an open output file or pipe returns 0; it required the name to
// be open as both, so it returned -1 for every one.
#[test]
fn awk_fflush_of_an_open_file_or_pipe() {
    let tmp = plib::tmp::TempDir::new().unwrap();
    let file = tmp.path().join("out.txt");
    let program = format!(
        "BEGIN {{ print \"x\" > \"{0}\"; a = fflush(\"{0}\"); print \"y\" | \"cat\"; b = fflush(\"cat\"); close(\"cat\"); print a, b, fflush(\"nope\") }}",
        file.display()
    );
    assert_awk_cases(&[(&program, "", "y\n0 0 -1\n")]);
}

// Output reaches its destination in the order awk writes it, also around
// the commands awk runs: standard output is flushed before a command
// starts or is waited for, before reading from a command, and at the end
// of the run, whatever ends it.  /dev/stdout shares awk's standard
// output and /dev/stderr is its standard error.
#[test]
fn awk_output_order_around_commands_and_special_files() {
    assert_awk_cases(&[
        ("BEGIN { printf \"a\"; system(\"echo b\") }", "", "ab\n"),
        (
            "BEGIN { printf \"a\"; print \"b\" | \"cat\"; close(\"cat\"); print \"c\" }",
            "",
            "ab\nc\n",
        ),
        (
            "BEGIN { print \"x\"; print \"b\" | \"sort\"; close(\"sort\"); print \"y\" }",
            "",
            "x\nb\ny\n",
        ),
        (
            "BEGIN { printf \"a\"; \"echo b\" | getline v; print v }",
            "",
            "ab\n",
        ),
        (
            "BEGIN { print 1; print 2 > \"/dev/stdout\"; printf \"3\" >> \"/dev/stdout\"; print 4; print close(\"/dev/stdout\") }",
            "",
            "1\n2\n34\n0\n",
        ),
        ("BEGIN { printf \"x\"; exit }", "", "x"),
        ("{ printf \"%s\", $0 } END { printf \"!\" }", "a\nb\n", "ab!"),
    ]);
    let (stdout, stderr, status) = awk_args(
        &["BEGIN { print \"e\" > \"/dev/stderr\"; print \"o\" }"],
        "",
    );
    assert_eq!(
        (stdout.as_str(), stderr.as_str(), status),
        ("o\n", "e\n", Some(0))
    );
    let (stdout, stderr, status) = awk_args(&["BEGIN { printf \"before\"; $(-1) }"], "");
    assert_eq!(stdout, "before");
    assert!(stderr.starts_with("awk: runtime error"), "{stderr}");
    assert_eq!(status, Some(1));
}

// A failed write to an output file, which may show only when its buffer
// is flushed at the end, is an error naming the file.
#[cfg(target_os = "linux")]
#[test]
fn awk_write_error_on_an_output_file() {
    let (_, stderr, status) = awk_args(&["BEGIN { print \"x\" > \"/dev/full\" }"], "");
    assert!(stderr.contains("/dev/full"), "{stderr}");
    assert_eq!(status, Some(1));
    let (_, _, status) = awk_args(
        &["BEGIN { print \"x\" > \"/dev/full\"; print close(\"/dev/full\") }"],
        "",
    );
    assert_eq!(status, Some(0));
}

// At the end of the run awk's own output is written before the commands
// it printed to are waited for, so it comes before what they print then.
#[test]
fn awk_output_at_exit_comes_before_the_commands_output() {
    assert_awk_cases(&[("BEGIN { print \"x\" | \"cat\"; print \"y\" }", "", "y\nx\n")]);
}

/// Runs awk with `args`, its standard output going to `stdout`, and
/// returns its standard error and exit status.
#[cfg(target_os = "linux")]
fn awk_to(args: &[&str], stdout: std::fs::File) -> (String, Option<i32>) {
    let out = std::process::Command::new(plib::testing::get_binary_path("awk"))
        .args(args)
        .stdin(std::process::Stdio::null())
        .stdout(stdout)
        .output()
        .unwrap();
    (
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.code(),
    )
}

// A failure at the end of the run does not lose the output awk had
// collected, and a runtime error is reported as itself even when writing
// out that output fails too.
#[cfg(target_os = "linux")]
#[test]
fn awk_output_survives_a_failure_at_the_end() {
    let tmp = plib::tmp::TempDir::new().unwrap();
    let path = tmp.path().join("out");
    let (stderr, status) = awk_to(
        &["BEGIN { print \"hello\"; print \"x\" > \"/dev/full\" }"],
        std::fs::File::create(&path).unwrap(),
    );
    assert_eq!(std::fs::read_to_string(&path).unwrap(), "hello\n");
    assert!(stderr.contains("/dev/full"), "{stderr}");
    assert_eq!(status, Some(1));
    let (stderr, status) = awk_to(
        &["BEGIN { print \"a\"; $(-1) }"],
        std::fs::OpenOptions::new()
            .write(true)
            .open("/dev/full")
            .unwrap(),
    );
    assert!(
        stderr.starts_with("awk: runtime error: attempt to access field -1"),
        "{stderr}"
    );
    assert_eq!(status, Some(1));
}

// getline from a file awk is writing reads what awk has written to it:
// the file's output buffer is flushed when getline opens it.
#[test]
fn awk_getline_reads_what_awk_wrote_to_the_file() {
    let tmp = plib::tmp::TempDir::new().unwrap();
    let file = tmp.path().join("t");
    let program = format!(
        "BEGIN {{ print \"data\" > \"{0}\"; while ((getline l < \"{0}\") > 0) n++; print n + 0, l }}",
        file.display()
    );
    assert_awk_cases(&[(&program, "", "1 data\n")]);
}

// Output is written out before awk waits for more input, so a filter in a
// pipeline fed a line at a time passes each line on when it comes, without
// fflush, though its standard output is buffered.
#[test]
fn awk_output_is_written_before_waiting_for_input() {
    assert_eq!(
        awk_first_line_before_eof("{ print \"got \" $0 }", "abc\n").as_deref(),
        Some("got abc\n")
    );
    assert_eq!(
        awk_first_line_before_eof(
            "BEGIN { while ((getline l) > 0) print \"got \" l }",
            "abc\n"
        )
        .as_deref(),
        Some("got abc\n")
    );
}

// The precision of %s is the most characters to write, so a large one
// needs no room: it was reserved, and failed under a memory limit.
#[test]
fn awk_large_string_precision_needs_no_room() {
    let awk = plib::testing::get_binary_path("awk");
    let out = std::process::Command::new("sh")
        .arg("-c")
        .arg(
            "ulimit -v 500000; \"$0\" 'BEGIN { printf \"%.2000000000s|%.1999999999c\\n\", \"abc\", \"d\" }'",
        )
        .arg(awk)
        .output()
        .unwrap();
    assert_eq!(String::from_utf8_lossy(&out.stdout), "abc|d\n");
    assert_eq!(out.status.code(), Some(0));
}

// A record read with more fields than an assignment may create on its own
// may still be extended past them by a few.
#[test]
fn awk_huge_input_record_can_grow() {
    let line = format!("{}\n", vec!["x"; 1_500_000].join(","));
    assert_awk_cases(&[(
        "BEGIN { FS = \",\" } { $(NF + 1) = \"y\"; print NF, $NF }",
        &line,
        "1500001 y\n",
    )]);
}

// The files of several -f options form one program, their concatenation,
// as POSIX says: a rule may begin in one and end in the next.  Syntax and
// runtime errors are still reported with the file and line in it.
#[test]
fn awk_program_files_are_concatenated() {
    let tmp = plib::tmp::TempDir::new().unwrap();
    let write = |name: &str, text: &str| {
        let path = tmp.path().join(name);
        std::fs::write(&path, text).unwrap();
        path.to_string_lossy().into_owned()
    };
    let a = write("a.awk", "BEGIN {");
    let b = write("b.awk", "print \"ok\" }\n");
    let (stdout, stderr, status) = awk_args(&["-f", &a, "-f", &b], "");
    assert_eq!(
        (stdout.as_str(), stderr.as_str(), status),
        ("ok\n", "", Some(0))
    );

    let good = write("good.awk", "BEGIN { x = 1 }\n");
    let bad = write("bad.awk", "\n\nBEGIN { @ }\n");
    let (_, stderr, status) = awk_args(&["-f", &good, "-f", &bad], "");
    assert!(stderr.contains(&format!("--> {bad}:3:9")), "{stderr}");
    assert_ne!(status, Some(0));

    let main = write("main.awk", "BEGIN { f() }\n");
    let lib = write("lib.awk", "\nfunction f() { return $(-1) }\n");
    let (_, stderr, status) = awk_args(&["-f", &main, "-f", &lib], "");
    assert!(stderr.contains(&format!("=> f at {lib}:2:")), "{stderr}");
    assert!(
        stderr.contains(&format!("=> <start> at {main}:1:")),
        "{stderr}"
    );
    assert_eq!(status, Some(1));
}

// At the end of the run the output files are written before the pipes are
// closed, as the commands may read them.
#[test]
fn awk_files_are_written_before_pipes_close_at_exit() {
    let tmp = plib::tmp::TempDir::new().unwrap();
    let file = tmp.path().join("f");
    let program = format!(
        "BEGIN {{ print \"cat {0}\" | \"sh\"; print \"hello\" > \"{0}\" }}",
        file.display()
    );
    assert_awk_cases(&[(&program, "", "hello\n")]);
}

// Standard input named twice is read on from where the first left off:
// each "-" had its own read-ahead, so nextfile threw away the rest.
#[test]
fn awk_stdin_operands_share_their_input() {
    let (stdout, stderr, status) =
        awk_args(&["FNR == 1 { print; nextfile }", "-", "-"], "a\nb\nc\n");
    assert_eq!(
        (stdout.as_str(), stderr.as_str(), status),
        ("a\nb\n", "", Some(0))
    );
}

// A failure to write out standard output before reading more input is not
// a failure of the input: the input is still read, and the write error is
// reported at the end.
#[cfg(target_os = "linux")]
#[test]
fn awk_stdout_failure_before_a_read_is_not_an_input_error() {
    use std::io::Write;
    let mut child = std::process::Command::new(plib::testing::get_binary_path("awk"))
        .arg("BEGIN { print \"x\" } { n++ } END { print \"n=\" n > \"/dev/stderr\" }")
        .stdin(std::process::Stdio::piped())
        .stdout(
            std::fs::OpenOptions::new()
                .write(true)
                .open("/dev/full")
                .unwrap(),
        )
        .stderr(std::process::Stdio::piped())
        .spawn()
        .unwrap();
    child.stdin.take().unwrap().write_all(b"a\nb\nc\n").unwrap();
    let out = child.wait_with_output().unwrap();
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(stderr.starts_with("n=3\n"), "{stderr}");
    assert!(stderr.contains("write error"), "{stderr}");
    assert_eq!(out.status.code(), Some(1));
}

// A regular-expression separator in a full read is found at once: only
// short reads were searched, so a record whose separator came in a full
// 64 KiB read waited for more input.
#[test]
fn awk_regex_rs_in_a_full_read_ends_the_record_at_once() {
    let input = format!("a;{}", "x".repeat(65534));
    assert_eq!(
        awk_first_line_before_eof(
            "BEGIN { RS = \";|#\" } { print \"got \" $0; fflush() }",
            &input
        )
        .as_deref(),
        Some("got a\n")
    );
}
