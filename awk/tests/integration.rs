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
    use std::time::{Duration, Instant};
    let mut child = std::process::Command::new(plib::testing::get_binary_path("awk"))
        .arg(program)
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .unwrap();
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
