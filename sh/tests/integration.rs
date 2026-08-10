//
// Copyright (c) 2025-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

mod pty;

use plib::testing::{run_test, run_test_with_checker, TestPlan};
use std::path::Path;
use std::process::Output;
use std::sync::atomic::{AtomicBool, Ordering};

static SETTING_TEST_VARS: AtomicBool = AtomicBool::new(false);
static TEST_VARS_ARE_SET: AtomicBool = AtomicBool::new(false);

fn set_env_vars() {
    if SETTING_TEST_VARS
        .compare_exchange(false, true, Ordering::SeqCst, Ordering::SeqCst)
        .is_ok()
    {
        let current_dir = std::env::current_dir().unwrap();
        let read_dir = if current_dir.ends_with("sh") {
            current_dir.join("tests/read_dir")
        } else {
            current_dir.join("sh/tests/read_dir")
        };
        std::env::set_var("TEST_READ_DIR", read_dir);

        let write_dir = Path::new(concat!(env!("CARGO_TARGET_TMPDIR"), "/sh_test_write_dir"));
        if !write_dir.exists() {
            std::fs::create_dir(write_dir).expect("failed to create write_dir");
        }
        std::env::set_var("TEST_WRITE_DIR", write_dir);
        TEST_VARS_ARE_SET.store(true, Ordering::SeqCst);
    }
    while !TEST_VARS_ARE_SET.load(Ordering::SeqCst) {
        std::hint::spin_loop()
    }
}

fn run_script_with_checker<F: Fn(&Output)>(script: &str, checker: F) {
    set_env_vars();
    run_test_with_checker(
        TestPlan {
            cmd: "sh".to_string(),
            args: vec!["-s".to_string()],
            stdin_data: script.to_string(),
            expected_out: "".to_string(),
            expected_err: "".to_string(),
            expected_exit_code: 0,
        },
        |_, output| checker(output),
    )
}

pub fn run_successfully_and<F: Fn(&str)>(program: &str, checker: F) {
    set_env_vars();
    run_script_with_checker(program, |output| {
        assert!(output.status.success());
        let stdout = String::from_utf8_lossy(&output.stdout);
        checker(stdout.as_ref());
    })
}

pub fn test_cli(args: Vec<&str>, stdin: &str, expected_output: &str) {
    set_env_vars();
    run_test(TestPlan {
        cmd: "sh".to_string(),
        args: args.iter().map(|s| s.to_string()).collect(),
        stdin_data: stdin.to_string(),
        expected_out: expected_output.to_string(),
        expected_err: String::default(),
        expected_exit_code: 0,
    });
}

pub fn test_script(script: &str, expected_output: &str) {
    set_env_vars();
    run_test(TestPlan {
        cmd: "sh".to_string(),
        args: vec![],
        stdin_data: script.to_string(),
        expected_out: expected_output.to_string(),
        expected_err: String::default(),
        expected_exit_code: 0,
    });
}

fn test_script_expect_stderr_and_stdout(script: &str, expected_output: &str) {
    set_env_vars();
    run_test_with_checker(
        TestPlan {
            cmd: "sh".to_string(),
            args: vec![],
            stdin_data: script.to_string(),
            expected_out: expected_output.to_string(),
            expected_err: String::default(),
            expected_exit_code: 0,
        },
        |_, output| {
            assert!(!output.stderr.is_empty());
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert_eq!(stdout, expected_output)
        },
    );
}

fn test_script_expect_error_status_stderr_and_stdout(script: &str, stdout: Option<&str>) {
    set_env_vars();
    run_test_with_checker(
        TestPlan {
            cmd: "sh".to_string(),
            args: vec!["-s".to_string()],
            stdin_data: script.to_string(),
            expected_out: "".to_string(),
            expected_err: "".to_string(),
            expected_exit_code: 0,
        },
        |_, output| {
            if let Some(stdout) = stdout {
                assert_eq!(String::from_utf8_lossy(&output.stdout), stdout);
            }
            assert!(!output.status.success());
            assert!(!output.stderr.is_empty());
        },
    )
}

fn test_script_expect_error_status_and_stdout(script: &str, stdout: Option<&str>) {
    set_env_vars();
    run_test_with_checker(
        TestPlan {
            cmd: "sh".to_string(),
            args: vec!["-s".to_string()],
            stdin_data: script.to_string(),
            expected_out: "".to_string(),
            expected_err: "".to_string(),
            expected_exit_code: 0,
        },
        |_, output| {
            if let Some(stdout) = stdout {
                assert_eq!(String::from_utf8_lossy(&output.stdout), stdout);
            }
            assert!(!output.status.success());
        },
    )
}

fn expect_exit_code(script: &str, exit_code: i32) {
    expect_cli_exit_code(vec!["-s".to_string()], script, exit_code)
}

fn expect_cli_exit_code(args: Vec<String>, stdin: &str, exit_code: i32) {
    set_env_vars();
    run_test_with_checker(
        TestPlan {
            cmd: "sh".to_string(),
            args,
            stdin_data: stdin.to_string(),
            expected_out: "".to_string(),
            expected_err: "".to_string(),
            expected_exit_code: 0,
        },
        |_, output| {
            assert_eq!(output.status.code().unwrap(), exit_code);
        },
    )
}

pub fn is_pid(s: &str) -> bool {
    s.trim_end_matches('\n').chars().all(|c| c.is_ascii_digit())
}

mod cli {
    use super::*;

    #[test]
    fn read_command_string_no_options_no_args() {
        test_cli(vec!["-c", "echo test"], "", "test\n");
    }

    #[test]
    fn read_command_string_no_options_with_args() {
        test_cli(
            vec!["-c", "echo $0 $1 $2", "sh", "arg1", "arg2"],
            "",
            "sh arg1 arg2\n",
        );
    }

    #[test]
    fn read_command_from_stdin_no_options_no_args() {
        test_cli(vec!["-s"], "echo test", "test\n");
        test_cli(vec![], "echo test", "test\n");
    }

    #[test]
    fn read_command_from_stdin_no_options_with_args() {
        test_cli(vec!["-s", "arg1", "arg2"], "echo $1 $2", "arg1 arg2\n");
    }

    #[test]
    fn print_default_human_readable_options_for_non_interactive_shell() {
        let output = r#"allexport off
notify    off
noclobber off
errexit   off
noglob    off
hashall   on
monitor   off
noexec    off
nounset   off
verbose   off
xtrace    off
ignoreeof off
nolog     off
vi        off
pipefail  off
"#;
        test_cli(vec!["-c", "set -o"], "", output);
        test_cli(vec!["-s"], "set -o", output);
    }

    #[test]
    fn print_default_shell_readable_options_for_non_interactive_shell() {
        let output = r#"set +o allexport
set +o notify
set +o noclobber
set +o errexit
set +o noglob
set -h
set +o monitor
set +o noexec
set +o nounset
set +o verbose
set +o xtrace
set +o ignoreeof
set +o nolog
set +o vi
set +o pipefail
"#;
        test_cli(vec!["-c", "set +o"], "", output);
        test_cli(vec!["-s"], "set +o", output);
    }

    #[test]
    fn set_and_print_options_cli() {
        let output_human_readable = r#"allexport on
notify    off
noclobber off
errexit   on
noglob    on
hashall   on
monitor   off
noexec    off
nounset   on
verbose   off
xtrace    off
ignoreeof off
nolog     off
vi        off
pipefail  off
"#;
        test_cli(
            vec!["-c", "-aef", "+vx", "-o", "nounset", "set -o"],
            "",
            output_human_readable,
        );
        let output_shell_readable = r#"set -o allexport
set +o notify
set +o noclobber
set -o errexit
set -o noglob
set -h
set +o monitor
set +o noexec
set -o nounset
set +o verbose
set +o xtrace
set +o ignoreeof
set +o nolog
set +o vi
set +o pipefail
"#;
        test_cli(
            vec!["-c", "-aef", "+vx", "-o", "nounset", "set +o"],
            "",
            output_shell_readable,
        );
    }

    #[test]
    fn read_from_file() {
        test_cli(
            vec!["tests/sh/hello_world.sh"],
            "",
            include_str!("sh/hello_world.out"),
        );
    }
}

mod quoting {
    use super::*;

    #[test]
    fn escape_with_backslash() {
        test_script(
            include_str!("sh/quoting/escape_with_backslash.sh"),
            include_str!("sh/quoting/escape_with_backslash.out"),
        )
    }

    #[test]
    fn remove_backslash_newline() {
        test_script(
            include_str!("sh/quoting/remove_backslash_newline.sh"),
            include_str!("sh/quoting/remove_backslash_newline.out"),
        )
    }

    #[test]
    fn escape_with_single_quotes() {
        test_script(
            include_str!("sh/quoting/escape_with_single_quotes.sh"),
            include_str!("sh/quoting/escape_with_single_quotes.out"),
        )
    }

    #[test]
    fn escape_with_double_quotes() {
        test_script(
            include_str!("sh/quoting/escape_with_double_quotes.sh"),
            include_str!("sh/quoting/escape_with_double_quotes.out"),
        )
    }

    #[test]
    fn backslash_inside_double_quotes() {
        test_script(
            include_str!("sh/quoting/backslash_inside_double_quotes.sh"),
            include_str!("sh/quoting/backslash_inside_double_quotes.out"),
        )
    }

    #[test]
    fn parameter_expansion_inside_double_quotes() {
        test_script(
            include_str!("sh/quoting/parameter_expansion_inside_double_quotes.sh"),
            include_str!("sh/quoting/parameter_expansion_inside_double_quotes.out"),
        )
    }

    #[test]
    fn command_substitution_inside_double_quotes() {
        test_script(
            include_str!("sh/quoting/command_substitution_inside_double_quotes.sh"),
            include_str!("sh/quoting/command_substitution_inside_double_quotes.out"),
        )
    }

    #[test]
    fn arithmetic_expansion_inside_double_quotes() {
        test_script(
            include_str!("sh/quoting/arithmetic_expansion_inside_double_quotes.sh"),
            include_str!("sh/quoting/arithmetic_expansion_inside_double_quotes.out"),
        )
    }

    #[test]
    fn backtick_command_substitution_inside_double_quotes() {
        test_script(
            include_str!("sh/quoting/backtick_command_substitution_inside_double_quotes.sh"),
            include_str!("sh/quoting/backtick_command_substitution_inside_double_quotes.out"),
        )
    }
}

mod token_recognition {
    use crate::test_script;

    #[test]
    fn alias_command_to_command() {
        test_script(
            include_str!("sh/token_recognition/alias_command_to_command.sh"),
            include_str!("sh/token_recognition/alias_command_to_command.out"),
        )
    }

    #[test]
    fn alias_command_to_command_with_args() {
        test_script(
            include_str!("sh/token_recognition/alias_command_to_command_with_args.sh"),
            include_str!("sh/token_recognition/alias_command_to_command_with_args.out"),
        )
    }

    #[test]
    fn alias_command_to_conjunction() {
        test_script(
            include_str!("sh/token_recognition/alias_command_to_conjunction.sh"),
            include_str!("sh/token_recognition/alias_command_to_conjunction.out"),
        )
    }

    #[test]
    fn arg_alias() {
        test_script(
            include_str!("sh/token_recognition/arg_alias.sh"),
            include_str!("sh/token_recognition/arg_alias.out"),
        )
    }

    #[test]
    fn discard_comments() {
        test_script(
            include_str!("sh/token_recognition/discard_comments.sh"),
            include_str!("sh/token_recognition/discard_comments.out"),
        )
    }

    #[test]
    fn recursive_alias_with_cycle_command_to_command() {
        test_script(
            include_str!("sh/token_recognition/recursive_alias_with_cycle_command_to_command.sh"),
            include_str!("sh/token_recognition/recursive_alias_with_cycle_command_to_command.out"),
        )
    }

    #[test]
    fn recursive_alias_with_cycle_command_to_conjunction() {
        test_script(
            include_str!(
                "sh/token_recognition/recursive_alias_with_cycle_command_to_conjunction.sh"
            ),
            include_str!(
                "sh/token_recognition/recursive_alias_with_cycle_command_to_conjunction.out"
            ),
        )
    }
}

mod special_parameters {
    use super::*;

    #[test]
    fn expand_at() {
        test_cli(vec!["-c", "echo $@", "sh", "1", "2", "3"], "", "1 2 3\n");
        test_cli(
            vec!["-c", "for x in $@; do echo $x; done", "sh", "1 2", "3", "4"],
            "",
            "1\n2\n3\n4\n",
        );
        test_cli(
            vec![
                "-c",
                "for x in a$@b; do echo $x; done",
                "sh",
                "1 2",
                "3",
                "4",
            ],
            "",
            "a1\n2\n3\n4b\n",
        );
        test_cli(
            vec![
                "-c",
                "for x in \"$@\"; do echo $x; done",
                "sh",
                "1 2",
                "3",
                "4",
            ],
            "",
            "1 2\n3\n4\n",
        );
        test_cli(
            vec![
                "-c",
                "for x in a\"$@\"b; do echo $x; done",
                "sh",
                "a1 2",
                "3",
                "4b",
            ],
            "",
            "aa1 2\n3\n4bb\n",
        );
        test_cli(
            vec![
                "-c",
                "for x in ${unset-\"$@\"}; do echo $x; done",
                "sh",
                "1 2",
                "3",
                "4",
            ],
            "",
            "1 2\n3\n4\n",
        );
    }

    #[test]
    fn expand_asterisk() {
        test_cli(vec!["-c", "echo $*", "sh", "1", "2", "3"], "", "1 2 3\n");
        test_cli(
            vec!["-c", "echo \"$*\"", "sh", "1", "2", "3"],
            "",
            "1 2 3\n",
        );
        test_cli(
            vec!["-c", "for x in $*; do echo $x; done", "sh", "1 2", "3", "4"],
            "",
            "1\n2\n3\n4\n",
        );
        test_cli(
            vec!["-c", "IFS=,; echo \"$*\"", "sh", "1", "2", "3"],
            "",
            "1,2,3\n",
        );
        test_cli(
            vec!["-c", "IFS=; echo \"$*\"", "sh", "1", "2", "3"],
            "",
            "123\n",
        );
        test_cli(
            vec!["-c", "unset IFS; echo \"$*\"", "sh", "1", "2", "3"],
            "",
            "1 2 3\n",
        );
    }

    #[test]
    fn expand_arg_count() {
        test_cli(vec!["-c", "echo $#"], "", "0\n");
        test_cli(vec!["-c", "echo $#", "sh"], "", "0\n");
        test_cli(vec!["-c", "echo $#", "sh", "1", "2", "3"], "", "3\n");
    }

    #[test]
    fn expand_question_mark() {
        test_cli(vec!["-c", "echo $?", "sh"], "", "0\n");
        test_cli(vec!["-c", "true; echo $?", "sh"], "", "0\n");
        test_cli(vec!["-c", "false; echo $?", "sh"], "", "1\n");
    }

    #[test]
    fn expand_minus() {
        test_cli(vec!["-c", "echo $-", "sh"], "", "h\n");
        test_cli(vec!["-c", "-aeh", "echo $-", "sh"], "", "aeh\n");
    }

    #[test]
    fn expand_shell_pid() {
        run_successfully_and("echo $$", |output| {
            assert!(!output.is_empty());
            assert!(is_pid(output));
        })
    }

    #[test]
    fn expand_bang() {
        test_cli(vec!["-c", "echo $!"], "", "\n");
        run_successfully_and("true & echo $!", |output| {
            assert!(!output.is_empty());
            assert!(is_pid(output));
        })
    }

    #[test]
    fn expand_zero() {
        test_cli(vec!["-c", "echo $0", "sh", "1", "2", "3"], "", "sh\n");
        test_cli(
            vec!["tests/sh/script_name.sh"],
            "",
            include_str!("sh/script_name.out"),
        );
    }
}

mod special_variables {
    use super::*;

    #[test]
    fn expand_home() {
        run_successfully_and("echo $HOME", |output| {
            assert!(!output.is_empty());
            assert!(output.starts_with('/'));
        })
    }

    #[test]
    fn expand_default_ifs() {
        test_cli(vec!["-c", "echo \"$IFS\""], "", " \t\n\n");
    }

    #[test]
    fn expand_lineno() {
        test_script(
            include_str!("sh/special_variables/expand_lineno.sh"),
            include_str!("sh/special_variables/expand_lineno.out"),
        )
    }

    #[test]
    fn expand_lineno_inside_function() {
        test_script(
            include_str!("sh/special_variables/expand_lineno_inside_function.sh"),
            include_str!("sh/special_variables/expand_lineno_inside_function.out"),
        )
    }

    #[test]
    fn expand_lineno_inside_subshell() {
        test_script(
            include_str!("sh/special_variables/expand_lineno_inside_subshell.sh"),
            include_str!("sh/special_variables/expand_lineno_inside_subshell.out"),
        )
    }

    #[test]
    fn expand_lineno_with_alias_substitution() {
        test_script(
            include_str!("sh/special_variables/expand_lineno_with_alias_substitution.sh"),
            include_str!("sh/special_variables/expand_lineno_with_alias_substitution.out"),
        );
    }

    #[test]
    fn expand_ppid() {
        run_successfully_and("echo $PPID", |output| {
            assert!(!output.is_empty());
            assert!(is_pid(output));
        });
        run_successfully_and("echo $PPID; echo $(echo $PPID)", |output| {
            let mut lines = output.lines();
            let ppid1 = lines.next().unwrap();
            let ppid2 = lines.next().unwrap();
            assert!(is_pid(ppid1));
            assert_eq!(ppid1, ppid2);
        })
    }

    #[test]
    fn expand_default_ps1() {
        test_cli(vec!["-c", "echo \"$PS1\""], "", "\\$ \n");
    }

    #[test]
    fn expand_default_ps2() {
        test_cli(vec!["-c", "echo \"$PS2\""], "", "> \n");
    }

    #[test]
    fn expand_default_ps4() {
        test_cli(vec!["-c", "echo \"$PS4\""], "", "+ \n");
    }

    #[test]
    fn expand_pwd() {
        run_successfully_and("echo $PWD", |output| {
            assert!(!output.is_empty());
            assert!(output.starts_with('/'));
        })
    }
}

mod word_expansion {
    use super::*;

    #[test]
    fn parameter_expansion_indicate_error_if_null_or_unset() {
        test_script("x=val; echo ${x?error}", "val\n");
        test_script("x=val; echo ${x:?error}", "val\n");
        test_script_expect_stderr_and_stdout("echo ${x?error}", "");
        test_script_expect_stderr_and_stdout("echo ${x:?error}", "");
        test_script("x=; echo ${x?error}", "\n");
        test_script_expect_stderr_and_stdout("x=; echo ${x:?error}", "");
    }

    #[test]
    fn string_operations_on_unset_parameters_fail_with_no_unset() {
        test_script_expect_stderr_and_stdout("set -u; echo ${#UNSET}", "");
        test_script_expect_stderr_and_stdout("set -u; echo ${UNSET%test}", "");
    }

    #[test]
    fn arithmetic_expansion_basic_operations() {
        test_script(
            include_str!("sh/word_expansion/arithmetic_expansion_basic_operations.sh"),
            include_str!("sh/word_expansion/arithmetic_expansion_basic_operations.out"),
        );
    }
    #[test]
    fn arithmetic_expansion_constants() {
        test_script(
            include_str!("sh/word_expansion/arithmetic_expansion_constants.sh"),
            include_str!("sh/word_expansion/arithmetic_expansion_constants.out"),
        );
    }
    #[test]
    fn arithmetic_expansion_expand_words() {
        test_script(
            include_str!("sh/word_expansion/arithmetic_expansion_expand_words.sh"),
            include_str!("sh/word_expansion/arithmetic_expansion_expand_words.out"),
        );
    }
    #[test]
    fn arithmetic_expansion_precedence() {
        test_script(
            include_str!("sh/word_expansion/arithmetic_expansion_precedence.sh"),
            include_str!("sh/word_expansion/arithmetic_expansion_precedence.out"),
        );
    }
    #[test]
    fn command_substitution() {
        test_script(
            include_str!("sh/word_expansion/command_substitution.sh"),
            include_str!("sh/word_expansion/command_substitution.out"),
        );
    }
    #[test]
    fn command_substitution_inside_double_quotes_does_not_perform_field_splitting_or_pathname_expansion(
    ) {
        test_script(include_str!("sh/word_expansion/command_substitution_inside_double_quotes_does_not_perform_field_splitting_or_pathname_expansion.sh"), include_str!("sh/word_expansion/command_substitution_inside_double_quotes_does_not_perform_field_splitting_or_pathname_expansion.out"));
    }
    #[test]
    fn nested_command_substitution() {
        test_script(
            include_str!("sh/word_expansion/nested_command_substitution.sh"),
            include_str!("sh/word_expansion/nested_command_substitution.out"),
        );
    }
    #[test]
    fn only_the_results_of_expansion_and_substitution_are_split() {
        test_script(
            include_str!(
                "sh/word_expansion/only_the_results_of_expansion_and_substitution_are_split.sh"
            ),
            include_str!(
                "sh/word_expansion/only_the_results_of_expansion_and_substitution_are_split.out"
            ),
        );
    }
    #[test]
    fn parameter_expansion_assign_default_values() {
        test_script(
            include_str!("sh/word_expansion/parameter_expansion_assign_default_values.sh"),
            include_str!("sh/word_expansion/parameter_expansion_assign_default_values.out"),
        );
    }
    #[test]
    fn parameter_expansion_inside_double_quotes_does_not_perform_pathname_expansion_and_field_splitting(
    ) {
        test_script(include_str!("sh/word_expansion/parameter_expansion_inside_double_quotes_does_not_perform_pathname_expansion_and_field_splitting.sh"), include_str!("sh/word_expansion/parameter_expansion_inside_double_quotes_does_not_perform_pathname_expansion_and_field_splitting.out"));
    }
    #[test]
    fn parameter_expansion_string_operations() {
        test_script(
            include_str!("sh/word_expansion/parameter_expansion_string_operations.sh"),
            include_str!("sh/word_expansion/parameter_expansion_string_operations.out"),
        );
    }
    #[test]
    fn parameter_expansion_use_alternative() {
        test_script(
            include_str!("sh/word_expansion/parameter_expansion_use_alternative.sh"),
            include_str!("sh/word_expansion/parameter_expansion_use_alternative.out"),
        );
    }
    #[test]
    fn parameter_expansion_use_default_values() {
        test_script(
            include_str!("sh/word_expansion/parameter_expansion_use_default_values.sh"),
            include_str!("sh/word_expansion/parameter_expansion_use_default_values.out"),
        );
    }

    #[test]
    fn pathname_expansion() {
        test_script(
            include_str!("sh/word_expansion/pathname_expansion.sh"),
            include_str!("sh/word_expansion/pathname_expansion.out"),
        );
    }
    #[test]
    fn split_fields() {
        test_script(
            include_str!("sh/word_expansion/split_fields.sh"),
            include_str!("sh/word_expansion/split_fields.out"),
        );
    }
    #[test]
    fn split_fields_default_ifs() {
        test_script(
            include_str!("sh/word_expansion/split_fields_default_ifs.sh"),
            include_str!("sh/word_expansion/split_fields_default_ifs.out"),
        );
    }
    #[test]
    fn split_fields_null_ifs() {
        test_script(
            include_str!("sh/word_expansion/split_fields_null_ifs.sh"),
            include_str!("sh/word_expansion/split_fields_null_ifs.out"),
        );
    }
    #[test]
    fn split_fields_unset_ifs() {
        test_script(
            include_str!("sh/word_expansion/split_fields_unset_ifs.sh"),
            include_str!("sh/word_expansion/split_fields_unset_ifs.out"),
        );
    }
    #[test]
    fn tilde_expansion() {
        test_script(
            include_str!("sh/word_expansion/tilde_expansion.sh"),
            include_str!("sh/word_expansion/tilde_expansion.out"),
        );
    }
    #[test]
    fn variable_expansion() {
        test_script(
            include_str!("sh/word_expansion/variable_expansion.sh"),
            include_str!("sh/word_expansion/variable_expansion.out"),
        );
    }

    #[test]
    fn field_splitting_can_create_empty_fields() {
        test_script(
            include_str!("sh/word_expansion/field_splitting_can_create_empty_fields.sh"),
            include_str!("sh/word_expansion/field_splitting_can_create_empty_fields.out"),
        );
    }
}

mod redirection {
    use super::*;

    #[test]
    fn standard_output_redirection_fails_with_noclobber_set() {
        run_script_with_checker(
            r#"
            set -o noclobber
            cd $TEST_WRITE_DIR
            echo test1 > standard_output_redirection_fails_with_noclobber_set.txt
            echo test2 > standard_output_redirection_fails_with_noclobber_set.txt
            cat standard_output_redirection_fails_with_noclobber_set.txt
            echo test3 >| standard_output_redirection_fails_with_noclobber_set.txt
            cat standard_output_redirection_fails_with_noclobber_set.txt
            rm standard_output_redirection_fails_with_noclobber_set.txt
            "#,
            |output| {
                assert!(output.status.success());
                assert!(!output.stderr.is_empty());
                assert_eq!(String::from_utf8_lossy(&output.stdout), "test1\ntest3\n");
            },
        );
    }

    #[test]
    fn duplicate_stderr_to_stdout() {
        run_script_with_checker("echo test 1>&2", |output| {
            assert!(output.status.success());
            assert_eq!(String::from_utf8_lossy(&output.stdout), "");
            assert_eq!(String::from_utf8_lossy(&output.stderr), "test\n");
        });
    }

    #[test]
    fn append_redirected_output() {
        test_script(
            include_str!("sh/redirection/append_redirected_output.sh"),
            include_str!("sh/redirection/append_redirected_output.out"),
        );
    }

    #[test]
    fn contents_of_here_document_are_expanded() {
        test_script(
            include_str!("sh/redirection/contents_of_here_document_are_expanded.sh"),
            include_str!("sh/redirection/contents_of_here_document_are_expanded.out"),
        );
    }

    #[test]
    fn contents_of_here_document_are_not_expanded_if_delimiter_is_quoted() {
        test_script(include_str!("sh/redirection/contents_of_here_document_are_not_expanded_if_delimiter_is_quoted.sh"), include_str!("sh/redirection/contents_of_here_document_are_not_expanded_if_delimiter_is_quoted.out"));
    }

    #[test]
    fn duplicate_input_file_descriptor() {
        test_script(
            include_str!("sh/redirection/duplicate_input_file_descriptor.sh"),
            include_str!("sh/redirection/duplicate_input_file_descriptor.out"),
        );
    }

    #[test]
    fn duplicate_output_file_descriptor() {
        test_script(
            include_str!("sh/redirection/duplicate_output_file_descriptor.sh"),
            include_str!("sh/redirection/duplicate_output_file_descriptor.out"),
        );
    }

    #[test]
    fn here_document() {
        test_script(
            include_str!("sh/redirection/here_document.sh"),
            include_str!("sh/redirection/here_document.out"),
        );
    }

    #[test]
    fn input_redirection() {
        test_script(
            include_str!("sh/redirection/input_redirection.sh"),
            include_str!("sh/redirection/input_redirection.out"),
        );
    }

    #[test]
    fn open_file_descriptor_for_read_and_write() {
        test_script(
            include_str!("sh/redirection/open_file_descriptor_for_read_and_write.sh"),
            include_str!("sh/redirection/open_file_descriptor_for_read_and_write.out"),
        );
    }

    #[test]
    fn output_redirection() {
        test_script(
            include_str!("sh/redirection/output_redirection.sh"),
            include_str!("sh/redirection/output_redirection.out"),
        );
    }

    #[test]
    fn redirect_output_of_special_builtin_commands() {
        test_script(
            include_str!("sh/redirection/redirect_output_of_special_builtin_commands.sh"),
            include_str!("sh/redirection/redirect_output_of_special_builtin_commands.out"),
        )
    }
}

mod errors {
    use super::*;

    #[test]
    fn exit_on_syntax_error() {
        test_script_expect_error_status_stderr_and_stdout("echo &&; echo wrong", None);
    }

    #[test]
    fn exit_on_special_builtin_error() {
        test_script_expect_error_status_stderr_and_stdout("set -o abc; echo wrong", None);
    }

    #[test]
    fn builtin_error_does_not_exit() {
        test_script_expect_stderr_and_stdout(
            "cd nonexistent; echo $?; echo correct",
            "1\ncorrect\n",
        );
    }

    #[test]
    fn expansion_error_shall_exit() {
        test_script_expect_error_status_stderr_and_stdout("${x!y}", None);
    }

    #[test]
    fn exit_code_127_if_command_is_not_found() {
        expect_exit_code("./nonexistent", 127);
    }
}

mod commands {
    use super::*;

    #[test]
    fn assigning_to_readonly_var_is_error() {
        test_script_expect_error_status_stderr_and_stdout("readonly x=1; x=2", None);
    }

    #[test]
    fn for_loop_without_iterable_iterates_over_shell_args() {
        test_cli(
            vec!["-c", "for x; do echo $x; done", "sh", "1", "2", "3"],
            "",
            "1\n2\n3\n",
        );
    }

    #[test]
    fn and_or_list() {
        test_script(
            include_str!("sh/commands/and_or_list.sh"),
            include_str!("sh/commands/and_or_list.out"),
        );
    }
    #[test]
    fn assignments_before_command_are_exported_to_command_env() {
        test_script(
            include_str!("sh/commands/assignments_before_command_are_exported_to_command_env.sh"),
            include_str!("sh/commands/assignments_before_command_are_exported_to_command_env.out"),
        );
    }
    #[test]
    fn assignments_before_function_call_are_local_to_function() {
        test_script(
            include_str!("sh/commands/assignments_before_function_call_are_local_to_function.sh"),
            include_str!("sh/commands/assignments_before_function_call_are_local_to_function.out"),
        );
    }
    #[test]
    fn assignments_before_special_builtin_affect_the_current_env() {
        test_script(
            include_str!(
                "sh/commands/assignments_before_special_builtin_affect_the_current_env.sh"
            ),
            include_str!(
                "sh/commands/assignments_before_special_builtin_affect_the_current_env.out"
            ),
        );
    }
    #[test]
    fn assignments_with_no_command_affect_the_current_env() {
        test_script(
            include_str!("sh/commands/assignments_with_no_command_affect_the_current_env.sh"),
            include_str!("sh/commands/assignments_with_no_command_affect_the_current_env.out"),
        );
    }
    #[test]
    fn braced_grouping() {
        test_script(
            include_str!("sh/commands/braced_grouping.sh"),
            include_str!("sh/commands/braced_grouping.out"),
        );
    }
    #[test]
    fn case_construct() {
        test_script(
            include_str!("sh/commands/case_construct.sh"),
            include_str!("sh/commands/case_construct.out"),
        );
    }
    #[test]
    fn command_precedence() {
        test_script(
            include_str!("sh/commands/command_precedence.sh"),
            include_str!("sh/commands/command_precedence.out"),
        );
    }
    #[test]
    fn exit_status_of_for_loop_is_exit_status_of_last_command() {
        test_script(
            include_str!("sh/commands/exit_status_of_for_loop_is_exit_status_of_last_command.sh"),
            include_str!("sh/commands/exit_status_of_for_loop_is_exit_status_of_last_command.out"),
        );
    }
    #[test]
    fn exit_status_of_if_construct_is_the_exit_status_of_last_executed_command() {
        test_script(include_str!("sh/commands/exit_status_of_if_construct_is_the_exit_status_of_last_executed_command.sh"), include_str!("sh/commands/exit_status_of_if_construct_is_the_exit_status_of_last_executed_command.out"));
    }
    #[test]
    fn for_loop() {
        test_script(
            include_str!("sh/commands/for_loop.sh"),
            include_str!("sh/commands/for_loop.out"),
        );
    }
    #[test]
    fn functions() {
        test_script(
            include_str!("sh/commands/functions.sh"),
            include_str!("sh/commands/functions.out"),
        );
    }
    #[test]
    fn if_construct() {
        test_script(
            include_str!("sh/commands/if_construct.sh"),
            include_str!("sh/commands/if_construct.out"),
        );
    }
    #[test]
    fn invert_pipeline_exit_status() {
        test_script(
            include_str!("sh/commands/invert_pipeline_exit_status.sh"),
            include_str!("sh/commands/invert_pipeline_exit_status.out"),
        );
    }
    #[test]
    fn pass_arguments_to_command() {
        test_script(
            include_str!("sh/commands/pass_arguments_to_command.sh"),
            include_str!("sh/commands/pass_arguments_to_command.out"),
        );
    }
    #[test]
    fn pipeline() {
        test_script(
            include_str!("sh/commands/pipeline.sh"),
            include_str!("sh/commands/pipeline.out"),
        );
    }
    #[test]
    fn pipeline_exit_status_is_determined_by_the_last_command() {
        test_script(
            include_str!("sh/commands/pipeline_exit_status_is_determined_by_the_last_command.sh"),
            include_str!("sh/commands/pipeline_exit_status_is_determined_by_the_last_command.out"),
        );
    }
    #[test]
    fn sequential_list() {
        test_script(
            include_str!("sh/commands/sequential_list.sh"),
            include_str!("sh/commands/sequential_list.out"),
        );
    }
    #[test]
    fn subshell_grouping_command() {
        test_script(
            include_str!("sh/commands/subshell_grouping_command.sh"),
            include_str!("sh/commands/subshell_grouping_command.out"),
        );
    }
    #[test]
    fn until_loop() {
        test_script(
            include_str!("sh/commands/until_loop.sh"),
            include_str!("sh/commands/until_loop.out"),
        );
    }
    #[test]
    fn values_in_variable_assignment_are_expanded() {
        test_script(
            include_str!("sh/commands/values_in_variable_assignment_are_expanded.sh"),
            include_str!("sh/commands/values_in_variable_assignment_are_expanded.out"),
        );
    }
    #[test]
    fn while_loop() {
        test_script(
            include_str!("sh/commands/while_loop.sh"),
            include_str!("sh/commands/while_loop.out"),
        );
    }

    #[test]
    fn if_command_could_not_be_executed_run_as_a_shell_script() {
        test_script(
            include_str!("sh/commands/if_command_could_not_be_executed_run_as_a_shell_script.sh"),
            include_str!("sh/commands/if_command_could_not_be_executed_run_as_a_shell_script.out"),
        )
    }
}

mod builtin {
    use super::*;

    #[test]
    fn readonly() {
        test_script(
            include_str!("sh/builtin/readonly.sh"),
            include_str!("sh/builtin/readonly.out"),
        );
    }

    #[test]
    fn assign_to_readonly_var_through_readonly_is_err() {
        test_script_expect_error_status_stderr_and_stdout("readonly x=1; readonly x=1", None);
    }

    #[test]
    fn break_builtin() {
        test_script(
            include_str!("sh/builtin/break.sh"),
            include_str!("sh/builtin/break.out"),
        )
    }

    #[test]
    fn continue_builtin() {
        test_script(
            include_str!("sh/builtin/continue.sh"),
            include_str!("sh/builtin/continue.out"),
        );
    }

    #[test]
    fn null_utility() {
        test_script(
            include_str!("sh/builtin/null_utility.sh"),
            include_str!("sh/builtin/null_utility.out"),
        );
    }

    #[test]
    fn unset_variable() {
        test_script(
            include_str!("sh/builtin/unset_variable.sh"),
            include_str!("sh/builtin/unset_variable.out"),
        );
    }

    #[test]
    fn unset_function() {
        test_script_expect_stderr_and_stdout(
            include_str!("sh/builtin/unset_function.sh"),
            include_str!("sh/builtin/unset_function.out"),
        );
    }

    #[test]
    fn unset_on_readonly_variable_is_error() {
        test_script_expect_error_status_stderr_and_stdout("readonly var=value; unset var", None);
    }

    #[test]
    fn dot() {
        test_script(
            include_str!("sh/builtin/dot.sh"),
            include_str!("sh/builtin/dot.out"),
        )
    }

    #[test]
    fn eval() {
        test_script(
            include_str!("sh/builtin/eval.sh"),
            include_str!("sh/builtin/eval.out"),
        )
    }

    #[test]
    fn exec() {
        test_script(
            include_str!("sh/builtin/exec.sh"),
            include_str!("sh/builtin/exec.out"),
        )
    }

    #[test]
    fn exit() {
        test_script(
            include_str!("sh/builtin/exit.sh"),
            include_str!("sh/builtin/exit.out"),
        )
    }

    #[test]
    fn export() {
        test_script(
            include_str!("sh/builtin/export.sh"),
            include_str!("sh/builtin/export.out"),
        )
    }

    #[test]
    fn return_builtin() {
        test_script(
            include_str!("sh/builtin/return.sh"),
            include_str!("sh/builtin/return.out"),
        )
    }

    #[test]
    fn set_allexport() {
        test_script(
            include_str!("sh/builtin/set_allexport.sh"),
            include_str!("sh/builtin/set_allexport.out"),
        )
    }

    #[test]
    fn set_errexit() {
        test_script_expect_error_status_and_stdout(
            include_str!("sh/builtin/set_errexit.sh"),
            Some(include_str!("sh/builtin/set_errexit.out")),
        );
    }

    #[test]
    fn set_noglob() {
        test_script(
            include_str!("sh/builtin/set_noglob.sh"),
            include_str!("sh/builtin/set_noglob.out"),
        )
    }

    #[test]
    fn set_noexec() {
        test_script(
            include_str!("sh/builtin/set_noexec.sh"),
            include_str!("sh/builtin/set_noexec.out"),
        )
    }

    #[test]
    fn shift() {
        test_script(
            include_str!("sh/builtin/shift.sh"),
            include_str!("sh/builtin/shift.out"),
        )
    }

    #[test]
    fn trap() {
        test_script(
            include_str!("sh/builtin/trap.sh"),
            include_str!("sh/builtin/trap.out"),
        )
    }

    #[test]
    fn command() {
        test_script(
            include_str!("sh/builtin/command.sh"),
            include_str!("sh/builtin/command.out"),
        )
    }

    #[test]
    fn type_builtin() {
        test_script(
            include_str!("sh/builtin/type.sh"),
            include_str!("sh/builtin/type.out"),
        )
    }

    #[test]
    fn unalias() {
        test_script(
            include_str!("sh/builtin/unalias.sh"),
            include_str!("sh/builtin/unalias.out"),
        )
    }

    #[test]
    fn getopts() {
        test_script(
            include_str!("sh/builtin/getopts.sh"),
            include_str!("sh/builtin/getopts.out"),
        );
    }

    #[test]
    fn getopts_with_invalid_option_prints_err() {
        test_script_expect_stderr_and_stdout(
            "while getopts a opt -d; do echo $opt $OPTARG; done; echo $OPTIND",
            "?\n2\n",
        );
    }

    #[test]
    fn getopts_option_with_missing_argument_prints_error() {
        test_script_expect_stderr_and_stdout(
            "while getopts a: opt -a; do echo $opt $OPTARG; done; echo $OPTIND",
            "?\n2\n",
        );
    }

    #[test]
    fn kill() {
        test_script(
            include_str!("sh/builtin/kill.sh"),
            include_str!("sh/builtin/kill.out"),
        )
    }

    #[test]
    fn umask() {
        test_script(
            include_str!("sh/builtin/umask.sh"),
            include_str!("sh/builtin/umask.out"),
        )
    }

    #[test]
    fn read() {
        test_script(
            include_str!("sh/builtin/read.sh"),
            include_str!("sh/builtin/read.out"),
        );
    }
}

/// Regression tests for the findings in `sh/audit.md`. Each test cites its
/// audit issue number. Tests are grouped by remediation phase.
mod audit_regressions {
    use super::*;
    use std::os::unix::fs::PermissionsExt;

    /// Asserts the script fails (non-zero exit) WITHOUT panicking (a Rust
    /// panic surfaces as exit code 101 and a "panicked" message on stderr).
    fn expect_clean_failure(script: &str) {
        set_env_vars();
        run_test_with_checker(
            TestPlan {
                cmd: "sh".to_string(),
                args: vec!["-s".to_string()],
                stdin_data: script.to_string(),
                expected_out: String::new(),
                expected_err: String::new(),
                expected_exit_code: 0,
            },
            |_, output| {
                let stderr = String::from_utf8_lossy(&output.stderr);
                assert!(!stderr.contains("panicked"), "shell panicked: {stderr}");
                assert_ne!(output.status.code(), Some(101), "shell panicked (exit 101)");
                assert!(!output.status.success(), "expected non-zero exit status");
            },
        );
    }

    // ----- Phase 1: eliminate panics -----

    #[test]
    fn bracket_close_first_is_literal_no_panic() {
        // #2: `]` as the first bracket member is a literal, not a crash.
        test_script("case \"]\" in []]) echo M;; *) echo NO;; esac\n", "M\n");
        test_script("case x in [!]]) echo M;; *) echo NO;; esac\n", "M\n");
    }

    #[test]
    fn arithmetic_div_by_zero_is_error_not_panic() {
        // #3
        expect_clean_failure("echo $((1/0))\n");
        expect_clean_failure("x=1; echo $((x/=0))\n");
    }

    #[test]
    fn arithmetic_mod_by_zero_is_error_not_panic() {
        // #4
        expect_clean_failure("echo $((5%0))\n");
    }

    #[test]
    fn arithmetic_overflow_and_shift_wrap_not_panic() {
        // #3/#4 sibling: overflow and out-of-range shifts wrap, never panic.
        test_script(
            "echo $((9223372036854775807+1))\n",
            "-9223372036854775808\n",
        );
        test_script("echo $((1<<64))\n", "1\n");
    }

    #[test]
    fn read_with_options_before_vars_no_panic() {
        // #5: `read -r x y` must split into vars.len() fields, not panic.
        test_script(
            "printf 'a b c\\n' | { read -r x y; echo \"[$x][$y]\"; }\n",
            "[a][b c]\n",
        );
        test_script(
            "printf 'a b c d e\\n' | { read x y; echo \"[$x][$y]\"; }\n",
            "[a][b c d e]\n",
        );
    }

    #[test]
    fn missing_command_file_exits_127_not_panic() {
        // #6
        set_env_vars();
        run_test_with_checker(
            TestPlan {
                cmd: "sh".to_string(),
                args: vec!["/no_such_sh_audit_file_xyz".to_string()],
                stdin_data: String::new(),
                expected_out: String::new(),
                expected_err: String::new(),
                expected_exit_code: 0,
            },
            |_, output| {
                let stderr = String::from_utf8_lossy(&output.stderr);
                assert!(!stderr.contains("panicked"), "shell panicked: {stderr}");
                assert_eq!(output.status.code(), Some(127));
            },
        );
    }

    #[test]
    fn jobs_bare_id_is_error_not_panic() {
        // #29: `jobs 1` (missing '%') must diagnose, not assert-panic.
        expect_clean_failure("jobs 1\n");
    }

    // ----- Phase 2: pattern-matching correctness -----

    #[test]
    fn case_pattern_is_anchored() {
        // #1: a `case` pattern must match the WHOLE word, not a substring.
        test_script("case ab in a) echo M;; *) echo NO;; esac\n", "NO\n");
        test_script("case foobar in oo) echo M;; *) echo NO;; esac\n", "NO\n");
        test_script("case abc in b) echo M;; *) echo NO;; esac\n", "NO\n");
        // full matches still work
        test_script("case abc in a*c) echo M;; *) echo NO;; esac\n", "M\n");
        test_script("case abc in a?c) echo M;; *) echo NO;; esac\n", "M\n");
        test_script("case hello in hello) echo M;; *) echo NO;; esac\n", "M\n");
    }

    // ----- Phase 3: set -u and exit/error semantics -----

    #[test]
    fn nounset_unset_variable_is_fatal_error() {
        // #7/#13: under `set -u`, expanding an unset parameter errors and exits
        // a non-interactive shell (so "AFTER" is never printed).
        test_script_expect_error_status_and_stdout("set -u; echo $undef; echo AFTER\n", Some(""));
        test_script_expect_error_status_and_stdout("set -u; echo ${undef}; echo AFTER\n", Some(""));
        test_script_expect_error_status_and_stdout("set -u; v=${undef}; echo AFTER\n", Some(""));
    }

    #[test]
    fn nounset_set_variable_is_ok() {
        // #7: a set variable (even empty) is fine under `set -u`.
        test_script("set -u; x=1; echo $x\n", "1\n");
        test_script("set -u; set -- a b; echo $#\n", "2\n");
    }

    #[test]
    fn expansion_error_exits_non_interactive() {
        // #13: `${x:?}` on an unset variable exits the script.
        test_script_expect_error_status_and_stdout("echo ${x:?}; echo AFTER\n", Some(""));
    }

    #[test]
    fn arithmetic_variable_is_recursively_evaluated() {
        // #41: a variable used in $(()) is itself evaluated as an expression.
        test_script("x=1+2; echo $((x))\n", "3\n");
        test_script("a=5; x=a; echo $((x))\n", "5\n");
        // an unset name (without set -u) is 0
        test_script("x=undefined_name; echo $((x))\n", "0\n");
    }

    #[test]
    fn command_not_found_exits_127() {
        // #15
        run_successfully_and("command no_such_cmd_xyz_q; echo rc=$?\n", |out| {
            assert_eq!(out, "rc=127\n");
        });
    }

    #[test]
    fn signal_terminated_status_is_128_plus_signal() {
        // #12: a SIGTERM-killed child yields 128+15 = 143 (not 128+discriminant).
        run_successfully_and("sleep 5 & p=$!; kill -TERM $p; wait $p; echo $?\n", |out| {
            assert_eq!(out, "143\n");
        });
    }

    #[test]
    fn exit_within_exit_trap_does_not_recurse() {
        // #37: `exit` inside the EXIT trap terminates immediately.
        expect_exit_code("trap 'exit 3' EXIT; exit 1\n", 3);
        expect_exit_code("trap ':' EXIT; exit 5\n", 5);
    }

    // ----- Phase 4: control flow & word expansion -----

    #[test]
    fn return_with_no_operand_uses_dollar_question() {
        // #10
        run_successfully_and("f() { false; return; }; f; echo $?\n", |out| {
            assert_eq!(out, "1\n");
        });
        run_successfully_and("f() { return 4; }; f; echo $?\n", |out| {
            assert_eq!(out, "4\n");
        });
    }

    #[test]
    fn break_does_not_escape_a_function() {
        // #11: a break inside a function must not break a loop in the caller.
        run_successfully_and(
            "for x in 1 2; do f() { break; }; f; echo in$x; done; echo done\n",
            |out| assert_eq!(out, "in1\nin2\ndone\n"),
        );
        // break still works inside nested loops
        run_successfully_and(
            "for x in 1 2; do for y in a b; do echo $x$y; break 2; done; done\n",
            |out| assert_eq!(out, "1a\n"),
        );
    }

    #[test]
    fn arithmetic_unary_operators_chain() {
        // #26
        test_script("echo $((!!0))\n", "0\n");
        test_script("echo $((!!5))\n", "1\n");
        test_script("echo $((- -1))\n", "1\n");
        test_script("echo $((~~5))\n", "5\n");
    }

    #[test]
    fn arithmetic_comma_operator() {
        // #40
        test_script("echo $((1,2,3))\n", "3\n");
        test_script("echo $((a=1, b=2, a+b))\n", "3\n");
        test_script("echo $((1+(2,3)))\n", "4\n");
    }

    #[test]
    fn assign_default_does_not_reassign_set_variable() {
        // #9: ${x:=word} must not expand/assign word when x is already set.
        run_successfully_and("x=set; v=${x:=new}; echo \"$x:$v\"\n", |out| {
            assert_eq!(out, "set:set\n");
        });
        // but it does assign when unset
        run_successfully_and("unset x; v=${x:=new}; echo \"$x:$v\"\n", |out| {
            assert_eq!(out, "new:new\n");
        });
    }

    #[test]
    fn parameter_expansion_word_may_contain_blanks() {
        // #59
        test_script("echo ${x:-a b c}\n", "a b c\n");
        run_successfully_and("x=; echo ${x:=d e}; echo \"$x\"\n", |out| {
            assert_eq!(out, "d e\nd e\n");
        });
    }

    #[test]
    fn string_length_counts_characters() {
        // #39 (ASCII path; multibyte verified behaviorally under a UTF-8 locale)
        test_script("x=hello; echo ${#x}\n", "5\n");
    }

    #[test]
    fn tilde_with_unset_home_is_literal() {
        // #30
        test_script("unset HOME; echo ~\n", "~\n");
    }

    // ----- Phase 5: POSIX.1-2024 parser/lexer additions -----

    #[test]
    fn literal_dollar_is_not_an_error() {
        // #58
        test_script("echo $\n", "$\n");
        test_script("echo a$\n", "a$\n");
        test_script("echo \"$\"\n", "$\n");
        test_script("echo $]\n", "$]\n");
        // $$ is still the PID special parameter
        run_successfully_and(
            "case \"$$\" in [0-9]*) echo pid;; *) echo no;; esac\n",
            |out| {
                assert_eq!(out, "pid\n");
            },
        );
    }

    #[test]
    fn dollar_single_quote_escapes() {
        // #20
        test_script("printf '%s' $'a\\tb'\n", "a\tb");
        test_script("printf '%s' $'a\\nb'\n", "a\nb");
        test_script("printf '%s' $'\\x41\\x42'\n", "AB");
        test_script("printf '%s' $'\\101'\n", "A");
        test_script("printf '%s' $'a\\'b'\n", "a'b");
        // not special inside double quotes
        test_script("printf '%s' \"$'abc'\"\n", "$'abc'");
    }

    #[test]
    fn case_semi_and_falls_through() {
        // #21
        test_script(
            "case a in a) echo A;& b) echo B;; c) echo C;; esac\n",
            "A\nB\n",
        );
        test_script(
            "case 1 in 1) echo one;& 2) echo two;& 3) echo three;; esac\n",
            "one\ntwo\nthree\n",
        );
        // ;; still stops
        test_script("case a in a) echo A;; b) echo B;; esac\n", "A\n");
    }

    #[test]
    fn function_definition_allows_linebreak_before_body() {
        // #23
        test_script("f()\n{ echo HI; }\nf\n", "HI\n");
    }

    #[test]
    fn double_dash_ends_options() {
        // #22: `--` ends options; `-c` after it is an operand (a file name),
        // not an "invalid option" error.
        set_env_vars();
        run_test_with_checker(
            TestPlan {
                cmd: "sh".to_string(),
                args: vec!["--".to_string(), "-c".to_string(), "echo hi".to_string()],
                stdin_data: String::new(),
                expected_out: String::new(),
                expected_err: String::new(),
                expected_exit_code: 0,
            },
            |_, output| {
                let stderr = String::from_utf8_lossy(&output.stderr);
                assert!(!stderr.contains("invalid option"), "stderr: {stderr}");
                // treated as a (missing) command_file operand -> 127
                assert_eq!(output.status.code(), Some(127));
            },
        );
    }

    // ----- Phase 6: pipefail & built-in output hygiene -----

    #[test]
    fn pipefail_derives_pipeline_status() {
        // #14
        run_successfully_and("set -o pipefail; false | true; echo $?\n", |out| {
            assert_eq!(out, "1\n");
        });
        run_successfully_and("set -o pipefail; true | true; echo $?\n", |out| {
            assert_eq!(out, "0\n");
        });
        // without pipefail, only the last command counts
        run_successfully_and("false | true; echo $?\n", |out| assert_eq!(out, "0\n"));
    }

    #[test]
    fn type_identifies_reserved_words() {
        // #16
        run_successfully_and("type if\n", |out| {
            assert_eq!(out, "if is a shell keyword\n");
        });
        run_successfully_and("type while\n", |out| {
            assert_eq!(out, "while is a shell keyword\n");
        });
    }

    #[test]
    fn getopts_optind_is_a_plain_integer() {
        // #17: OPTIND stays numeric, so `shift $((OPTIND-1))` works.
        run_successfully_and(
            "set -- -a foo; getopts a o; shift $((OPTIND-1)); echo \"$1\"\n",
            |out| assert_eq!(out, "foo\n"),
        );
        run_successfully_and("set -- -a x; getopts a o; echo \"$OPTIND\"\n", |out| {
            assert_eq!(out, "2\n");
        });
    }

    #[test]
    fn export_p_output_is_reinputtable() {
        // #25: a value containing a single quote is still quoted safely.
        run_successfully_and("export \"Q=a'b\"; export -p | grep '^export Q'\n", |out| {
            assert_eq!(out, "export Q='a'\\''b'\n")
        });
    }

    #[test]
    fn unset_repeated_option_is_ok() {
        // #47
        run_successfully_and("x=1; unset -v -v x; echo \"[${x-gone}]\"\n", |out| {
            assert_eq!(out, "[gone]\n");
        });
    }

    // ----- Phase 7: read / cd / umask / hash / redirection -----

    #[test]
    fn read_honors_backslash_newline_continuation() {
        // #19
        run_successfully_and(
            "printf 'a\\\\\\nb\\n' | { read x; echo \"[$x]\"; }\n",
            |out| assert_eq!(out, "[ab]\n"),
        );
    }

    #[test]
    fn umask_accepts_symbolic_input() {
        // #18
        run_successfully_and("umask 022; umask u=rwx,go=rx; umask\n", |out| {
            assert_eq!(out, "0022\n");
        });
        run_successfully_and("umask 0; umask a-w; umask\n", |out| {
            assert_eq!(out, "0222\n");
        });
    }

    #[test]
    fn cd_empty_operand_is_an_error() {
        // #42: `cd ""` fails (non-zero) but, being a regular built-in, does not
        // abort the script.
        run_successfully_and("cd \"\" 2>/dev/null; echo rc=$?\n", |out| {
            assert_eq!(out, "rc=1\n");
        });
    }

    #[test]
    fn large_io_number_is_accepted() {
        // #43: fd numbers above the old 1023 cap are allowed.
        run_successfully_and(": 100>/dev/null; echo ok\n", |out| {
            assert_eq!(out, "ok\n");
        });
    }

    #[test]
    fn noclobber_blocks_overwrite() {
        // #44
        run_successfully_and(
            "set -C; f=\"$TEST_WRITE_DIR/nc_test\"; rm -f \"$f\"; echo a > \"$f\"; \
             echo b > \"$f\" 2>/dev/null; echo rc=$?; echo c >| \"$f\" && echo override; rm -f \"$f\"\n",
            |out| assert_eq!(out, "rc=1\noverride\n"),
        );
    }

    // ----- Phase 8: jobs / signals / env / startup -----

    #[test]
    fn times_output_is_well_formed() {
        // #27: two lines, each "MmS.SSSs MmS.SSSs".
        run_successfully_and("times\n", |out| {
            let lines: Vec<&str> = out.trim_end().split('\n').collect();
            assert_eq!(lines.len(), 2, "times should print two lines: {out:?}");
            for line in lines {
                let fields: Vec<&str> = line.split(' ').collect();
                assert_eq!(fields.len(), 2, "each line has two times: {line:?}");
                for f in fields {
                    assert!(
                        f.contains('m') && f.ends_with('s') && f.contains('.'),
                        "bad time field {f:?}"
                    );
                    // three fractional digits
                    let frac = &f[f.find('.').unwrap() + 1..f.len() - 1];
                    assert_eq!(frac.len(), 3, "expected 3 fractional digits in {f:?}");
                }
            }
        });
    }

    #[test]
    fn kill_accepts_signal_name_variants() {
        // #49: case-independent names, with or without the SIG prefix.
        run_successfully_and("sleep 5 & kill -s term $!; wait $!; echo $?\n", |out| {
            assert_eq!(out, "143\n");
        });
        run_successfully_and("sleep 5 & kill -s Kill $!; wait $!; echo $?\n", |out| {
            assert_eq!(out, "137\n");
        });
        run_successfully_and("kill -l 9\n", |out| assert_eq!(out, "KILL\n"));
    }

    #[test]
    fn lineno_is_preserved_across_function_calls() {
        // #53
        run_successfully_and("f() { :; }\nf\necho $LINENO\n", |out| {
            assert_eq!(out, "3\n");
        });
    }

    #[test]
    fn pipeline_waits_for_all_commands() {
        // A pipeline must wait for every command, not just the last one. The
        // head writes a marker only after a delay; if the pipeline did not wait,
        // the following `cat` would not see it.
        run_successfully_and(
            "f=\"$TEST_WRITE_DIR/pipe_wait\"; rm -f \"$f\"; \
             { sleep 0.3; echo waited > \"$f\"; } | true; cat \"$f\"; rm -f \"$f\"\n",
            |out| assert_eq!(out, "waited\n"),
        );
    }

    #[test]
    fn glob_matches_symlinks() {
        // #24
        set_env_vars();
        let dir = Path::new(concat!(env!("CARGO_TARGET_TMPDIR"), "/sh_test_write_dir"))
            .join("glob_symlink_test");
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("real"), "x").unwrap();
        std::os::unix::fs::symlink("real", dir.join("lnk")).unwrap();
        run_successfully_and(
            &format!("cd '{}'; for f in *; do echo \"$f\"; done\n", dir.display()),
            |out| assert_eq!(out, "lnk\nreal\n"),
        );
    }

    #[test]
    fn bracket_literal_members_match() {
        // #8: literal members inside `[...]` (incl. '.', '*', '^', ']') match
        // themselves rather than being mis-escaped or mis-parsed.
        test_script("case . in [.]) echo M;; *) echo NO;; esac\n", "M\n");
        test_script("case x in [.]) echo M;; *) echo NO;; esac\n", "NO\n");
        test_script("case '*' in [.*^]) echo M;; *) echo NO;; esac\n", "M\n");
        test_script("case '^' in [.*^]) echo M;; *) echo NO;; esac\n", "M\n");
        test_script("case '^' in [a^]) echo M;; *) echo NO;; esac\n", "M\n");
        test_script("case ']' in []]) echo M;; *) echo NO;; esac\n", "M\n");
        test_script("case d in [!abc]) echo M;; *) echo NO;; esac\n", "M\n");
        test_script("case m in [a-z]) echo M;; *) echo NO;; esac\n", "M\n");
        test_script("case 5 in [[:digit:]]) echo M;; *) echo NO;; esac\n", "M\n");
    }

    // ----- Phase 1 (round 2): remaining process-aborting panics -----

    #[test]
    fn here_document_allows_trailing_tokens_on_the_same_line() {
        // The here-document body starts on the *next* line; whatever follows
        // the delimiter on the current line is still part of the command.
        test_script("cat <<EOF | tr a-z A-Z\nhi\nEOF\n", "HI\n");
        test_script("cat <<A <<B\nfirst\nA\nsecond\nB\n", "second\n");
        run_successfully_and(
            "f=\"$TEST_WRITE_DIR/heredoc_redirect\"; rm -f \"$f\"; \
             cat <<EOF > \"$f\"\nhi\nEOF\ncat \"$f\"; rm -f \"$f\"\n",
            |out| assert_eq!(out, "hi\n"),
        );
    }

    #[test]
    fn here_document_delimiter_may_follow_blanks() {
        // POSIX token recognition: '<<' is an operator, so blanks may separate
        // it from the delimiter word.
        test_script("cat << EOF\nhi\nEOF\n", "hi\n");
        test_script("cat <<\tEOF\nhi\nEOF\n", "hi\n");
    }

    #[test]
    fn here_document_dash_strips_tabs_from_terminator() {
        // `<<-` strips leading tabs from the body *and* from the terminator.
        test_script("cat <<-EOF\n\thi\n\tEOF\n", "hi\n");
    }

    #[test]
    fn here_document_delimiter_split_by_line_continuation() {
        // A '\'-newline inside the delimiter word is removed like anywhere else.
        test_script("cat <<EO\\\nF\nhi\nEOF\n", "hi\n");
    }

    #[test]
    fn cd_physical_accepts_a_relative_operand() {
        // `cd -P <relative>` must resolve to an absolute directory; leaving
        // PWD relative later trips an internal absolute-path invariant.
        set_env_vars();
        let dir = Path::new(concat!(env!("CARGO_TARGET_TMPDIR"), "/sh_test_write_dir"))
            .join("cd_physical_test");
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(dir.join("sub")).unwrap();
        run_successfully_and(
            &format!(
                "cd '{}' && cd -P sub && pwd && for f in *; do echo \"[$f]\"; done\n",
                dir.display()
            ),
            |out| {
                assert_eq!(out, format!("{}/sub\n[*]\n", dir.display()));
            },
        );
    }

    #[test]
    fn interactive_with_non_terminal_stdin_does_not_panic() {
        // `sh -i` is allowed even when stdin is not a terminal.
        set_env_vars();
        run_test_with_checker(
            TestPlan {
                cmd: "sh".to_string(),
                args: vec!["-i".to_string()],
                stdin_data: "echo hi\nexit 0\n".to_string(),
                expected_out: String::new(),
                expected_err: String::new(),
                expected_exit_code: 0,
            },
            |_, output| {
                let stderr = String::from_utf8_lossy(&output.stderr);
                assert!(!stderr.contains("panicked"), "shell panicked: {stderr}");
                assert!(String::from_utf8_lossy(&output.stdout).contains("hi"));
            },
        );
    }

    #[test]
    fn case_pattern_may_expand_to_several_fields() {
        // `$@` still expands to one field per parameter; a pattern is a single
        // word, so the fields are joined with a space (as dash and bash do).
        test_script(
            "set -- a b; case 'a b' in $@) echo M;; *) echo NO;; esac\n",
            "M\n",
        );
        test_script(
            "set -- a b; case a in $@) echo M;; *) echo NO;; esac\n",
            "NO\n",
        );
        // A case pattern is not field-split, so IFS does not apply to it.
        test_script(
            "IFS=,; v='a,b'; case 'a,b' in $v) echo M;; *) echo NO;; esac\n",
            "M\n",
        );
    }

    #[test]
    fn pipeline_writer_is_signalled_when_the_reader_exits() {
        // The shell must not keep the last pipe's read end open while waiting
        // for the writers, or `yes | head` never terminates.
        run_successfully_and("yes | head -n 3\n", |out| assert_eq!(out, "y\ny\ny\n"));
        run_successfully_and("yes | head -n 5 | wc -l\n", |out| {
            assert_eq!(out.trim(), "5")
        });
    }

    #[test]
    fn command_substitution_exceeding_the_pipe_buffer() {
        // The child blocks writing once the pipe fills, so the shell has to
        // drain the pipe before waiting for it.
        run_successfully_and(
            "x=$(yes 0123456789012345678901234567890123456789 | head -n 20000); echo ${#x}\n",
            // 20000 lines of 40 characters plus a newline each, less the
            // trailing newline that command substitution strips.
            |out| assert_eq!(out, "819999\n"),
        );
    }

    #[test]
    fn command_substitution_drops_nul_bytes() {
        // A NUL cannot be carried in a shell word; dropping it must not abort
        // the pattern-matching code paths that use C strings.
        test_script("x=$(printf 'a\\0b'); echo \"[$x]\"\n", "[ab]\n");
        test_script(
            "x=$(printf 'a\\0b'); case \"$x\" in ab) echo M;; *) echo NO;; esac\n",
            "M\n",
        );
        test_script("x=$(printf 'a\\0b'); echo \"[${x#a}]\"\n", "[b]\n");
    }

    #[test]
    fn builtin_output_to_an_unwritable_descriptor_does_not_panic() {
        // stdout redirected to a read-only file: report an error, do not abort.
        set_env_vars();
        run_script_with_checker("exec 1</dev/null; export -p\necho done >&2\n", |output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(!stderr.contains("panicked"), "shell panicked: {stderr}");
            assert_ne!(output.status.code(), Some(101));
        });
    }

    // ----- Phase 2: line continuation during token recognition -----

    #[test]
    fn line_continuation_inside_a_reserved_word() {
        test_script("i\\\nf true; then echo YES; fi\n", "YES\n");
        test_script("if true; then echo A; f\\\ni\n", "A\n");
        test_script("for x in 1; do\\\n echo D$x; done\n", "D1\n");
        // Quoting still keeps a reserved word an ordinary word.
        test_script("f() { echo fn; }; 'f'\n", "fn\n");
    }

    #[test]
    fn line_continuation_inside_an_operator() {
        test_script("true &\\\n& echo AND\n", "AND\n");
        test_script("false |\\\n| echo OR\n", "OR\n");
        test_script("case a in a) echo C;\\\n; esac\n", "C\n");
        run_successfully_and(
            "f=\"$TEST_WRITE_DIR/lc_append\"; rm -f \"$f\"; \
             echo X >\\\n> \"$f\"; cat \"$f\"; rm -f \"$f\"\n",
            |out| assert_eq!(out, "X\n"),
        );
    }

    #[test]
    fn line_continuation_inside_an_io_number() {
        run_successfully_and(
            "f=\"$TEST_WRITE_DIR/lc_ionum\"; rm -f \"$f\"; \
             echo Y 1\\\n> \"$f\"; cat \"$f\"; rm -f \"$f\"\n",
            |out| assert_eq!(out, "Y\n"),
        );
    }

    #[test]
    fn escaped_backslash_before_a_newline_in_double_quotes() {
        // `\\` is a literal backslash, so the newline after it is literal too
        // rather than a line continuation.
        test_script("echo \"a\\\\\nb\"\n", "a\\\nb\n");
        // A backslash only escapes $ ` \" \\ and <newline> inside double quotes.
        test_script("echo \"\\$(echo hi)\"\n", "$(echo hi)\n");
        test_script("echo \"a\\\nb\"\n", "ab\n");
        test_script("echo \"a\\nb\"\n", "a\\nb\n");
    }

    // ----- Phase 3: declaration-utility assignment expansion (POSIX 2.9.1) --

    #[test]
    fn declaration_utilities_tilde_expand_assignment_operands() {
        set_env_vars();
        run_successfully_and("HOME=/H; export a=~; echo \"$a\"\n", |out| {
            assert_eq!(out, "/H\n")
        });
        run_successfully_and("HOME=/H; export p=~/b:~/c; echo \"$p\"\n", |out| {
            assert_eq!(out, "/H/b:/H/c\n")
        });
        run_successfully_and("HOME=/H; readonly b=~; echo \"$b\"\n", |out| {
            assert_eq!(out, "/H\n")
        });
        run_successfully_and("HOME=/H; command export c=~; echo \"$c\"\n", |out| {
            assert_eq!(out, "/H\n")
        });
        // A plain assignment already worked and must keep working.
        run_successfully_and("HOME=/H; v=~; echo \"$v\"\n", |out| assert_eq!(out, "/H\n"));
    }

    #[test]
    fn declaration_utility_operands_are_not_split_or_globbed() {
        set_env_vars();
        // The value of an assignment-shaped operand is a single field even
        // when it contains IFS characters.
        run_successfully_and("IFS=:; export v=a:b:c; echo \"$v\"\n", |out| {
            assert_eq!(out, "a:b:c\n")
        });
        // Non-assignment operands keep their ordinary expansion.
        run_successfully_and("HOME=/H; export -p >/dev/null; x=~; echo \"$x\"\n", |out| {
            assert_eq!(out, "/H\n")
        });
    }

    // ----- Phase 4: ulimit conformance -----

    #[test]
    fn ulimit_reports_a_single_limit_as_a_bare_value() {
        // POSIX: "%1d\n", or "unlimited\n" for a resource with no numeric
        // limit. The labelled form belongs to -a only.
        set_env_vars();
        for script in [
            "ulimit -f\n",
            "ulimit\n",
            "ulimit -S -f\n",
            "ulimit -H -f\n",
        ] {
            run_successfully_and(script, |out| {
                let value = out.trim_end_matches('\n');
                assert!(
                    value == "unlimited" || value.chars().all(|c| c.is_ascii_digit()),
                    "expected a bare limit value, got {out:?}"
                );
            });
        }
        // -a keeps the labelled, one-line-per-resource form.
        run_successfully_and("ulimit -a\n", |out| {
            assert!(out.contains("file size (-f)"), "got {out:?}");
            assert!(out.lines().count() >= 5, "got {out:?}");
        });
    }

    #[test]
    fn ulimit_with_no_options_reports_the_soft_file_size_limit() {
        set_env_vars();
        run_successfully_and("ulimit -f 100\nulimit\n", |out| assert_eq!(out, "100\n"));
    }

    #[test]
    fn ulimit_soft_limit_may_not_exceed_the_hard_limit() {
        // Raising the hard limit needs privilege, so `-S` must refuse rather
        // than silently widening it.
        expect_clean_failure("ulimit -f 100\nulimit -S -f 200\n");
        run_successfully_and(
            "ulimit -f 100\nulimit -S -f 50\nulimit -S -f\nulimit -H -f\n",
            |out| assert_eq!(out, "50\n100\n"),
        );
    }

    // ----- Phase 5: exit status, $-, $!, exec -----

    // `/proc/self/mem` is the only readily available file that opens but fails
    // to read; there is no portable equivalent, so this one is Linux-only.
    #[cfg(target_os = "linux")]
    #[test]
    fn unrecoverable_read_error_exits_128() {
        // sh.md: 128 when commands cannot be read.
        set_env_vars();
        expect_cli_exit_code(vec!["/proc/self/mem".to_string()], "", 128);
    }

    #[test]
    fn non_script_command_file_exits_126() {
        // A command_file that is not valid text is ENOEXEC-like: 126, not 127.
        // Build it here rather than naming a system path, which differs across
        // platforms (`/bin/true` does not exist on macOS).
        set_env_vars();
        let dir = Path::new(concat!(env!("CARGO_TARGET_TMPDIR"), "/sh_test_write_dir"));
        std::fs::create_dir_all(dir).unwrap();
        let binary = dir.join("not_a_script.bin");
        std::fs::write(&binary, [0x7f, b'E', b'L', b'F', 0xff, 0xfe]).unwrap();
        expect_cli_exit_code(vec![binary.to_string_lossy().into_owned()], "", 126);
    }

    #[test]
    fn exec_reports_127_when_the_command_is_not_found() {
        set_env_vars();
        expect_cli_exit_code(
            vec!["-c".to_string(), "exec no_such_command_xyz".to_string()],
            "",
            127,
        );
        // A file that exists but cannot be executed is 126. Build it here
        // rather than naming a system path, which differs across platforms
        // (`/etc/hostname` does not exist on macOS).
        let dir = Path::new(concat!(env!("CARGO_TARGET_TMPDIR"), "/sh_test_write_dir"));
        std::fs::create_dir_all(dir).unwrap();
        let not_executable = dir.join("not_executable");
        std::fs::write(&not_executable, "data\n").unwrap();
        std::fs::set_permissions(&not_executable, std::fs::Permissions::from_mode(0o644)).unwrap();
        expect_cli_exit_code(
            vec![
                "-c".to_string(),
                format!("exec '{}'", not_executable.display()),
            ],
            "",
            126,
        );
    }

    #[test]
    fn dollar_bang_survives_wait() {
        // `$!` names the most recent background command, and stays set after
        // `wait` has reaped it.
        run_successfully_and(
            "sleep 0.05 & p=$!; wait; test \"$!\" = \"$p\" && echo same\n",
            |out| assert_eq!(out, "same\n"),
        );
    }

    #[test]
    fn wait_removes_the_job_it_reaped() {
        run_successfully_and("sleep 0.05 & p=$!; wait $p; echo \"[$(jobs)]\"\n", |out| {
            assert_eq!(out, "[]\n")
        });
    }

    // ----- Phase 6: built-in output and option handling -----

    #[test]
    fn unalias_a_accepts_operands_and_terminates_its_diagnostic() {
        test_script("alias x=y; unalias -a x; echo rc=$?\n", "rc=0\n");
        set_env_vars();
        run_script_with_checker("unalias nope1 nope2\n", |output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert_eq!(stderr.lines().count(), 2, "got {stderr:?}");
            assert!(stderr.ends_with('\n'), "got {stderr:?}");
        });
    }

    #[test]
    fn fc_option_order_does_not_matter() {
        // `-n` and `-r` qualify the other options; `fc -nl` is the common
        // spelling and must behave like `fc -ln`.
        set_env_vars();
        for script in ["fc -nl\n", "fc -ln\n", "fc -n -l\n"] {
            run_script_with_checker(script, |output| {
                let stderr = String::from_utf8_lossy(&output.stderr);
                assert!(!stderr.contains("can only be"), "got {stderr:?}");
            });
        }
        // `-n` still requires `-l`, and `-r` still conflicts with `-s`.
        expect_clean_failure("fc -n\n");
        expect_clean_failure("fc -s -r\n");
        expect_clean_failure("fc -r -s\n");
    }

    // ----- Phase 7: previously deferred findings -----

    #[test]
    fn async_list_reads_from_dev_null_without_job_control() {
        // POSIX 2.11: with job control disabled, an asynchronous list gets
        // /dev/null for standard input unless it redirects it itself.
        set_env_vars();
        run_test_with_checker(
            TestPlan {
                cmd: "sh".to_string(),
                args: vec!["-s".to_string()],
                stdin_data: "{ read x; echo \"child got [$x] rc=$?\"; } &\nsleep 0.3\n".to_string(),
                expected_out: String::new(),
                expected_err: String::new(),
                expected_exit_code: 0,
            },
            |_, output| {
                assert_eq!(
                    String::from_utf8_lossy(&output.stdout),
                    "child got [] rc=1\n"
                );
            },
        );
    }

    #[test]
    fn async_list_ignores_sigint_without_job_control() {
        run_successfully_and(
            "{ sleep 0.4; echo CHILD-SURVIVED; } &\nsleep 0.05\nkill -INT $!\nwait\n",
            |out| assert_eq!(out, "CHILD-SURVIVED\n"),
        );
    }

    #[test]
    fn bg_on_an_already_running_job_succeeds() {
        // POSIX: a job already running in the background needs no action.
        run_successfully_and("set -m\nsleep 0.2 & bg %1\necho rc=$?\n", |out| {
            assert!(out.ends_with("rc=0\n"), "got {out:?}")
        });
    }

    #[test]
    fn read_reports_an_assignment_error_above_end_of_file() {
        // POSIX read EXIT STATUS: 1 is end-of-file, >1 is an error.
        set_env_vars();
        run_script_with_checker("read x </dev/null; echo rc=$?\n", |output| {
            assert_eq!(String::from_utf8_lossy(&output.stdout), "rc=1\n");
        });
        run_script_with_checker(
            "readonly x=1\nprintf 'a\\n' | { read x; echo rc=$?; }\n",
            |output| {
                assert_eq!(String::from_utf8_lossy(&output.stdout), "rc=2\n");
            },
        );
    }

    #[test]
    fn dot_reports_an_unreadable_file() {
        set_env_vars();
        let dir = Path::new(concat!(env!("CARGO_TARGET_TMPDIR"), "/sh_test_write_dir"));
        std::fs::create_dir_all(dir).unwrap();
        let script = dir.join("dot_unreadable.sh");
        std::fs::write(&script, "echo nope\n").unwrap();
        std::fs::set_permissions(&script, std::fs::Permissions::from_mode(0o000)).unwrap();
        run_script_with_checker(&format!(". '{}'\n", script.display()), |output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(stderr.contains("cannot open file"), "got {stderr:?}");
            assert!(!output.status.success());
        });
        let _ = std::fs::set_permissions(&script, std::fs::Permissions::from_mode(0o644));
        let _ = std::fs::remove_file(&script);
    }

    // ----- Phase 8: coverage for previously untested behaviour -----

    #[test]
    fn continue_does_not_escape_a_function() {
        // The loop-depth counter has to be function-scoped, or `continue` in a
        // function unwinds the caller's loop.
        test_script(
            "for x in 1 2; do f() { continue; }; f; echo in$x; done; echo done\n",
            "in1\nin2\ndone\n",
        );
        test_script(
            "f() { continue; }\nfor x in 1 2; do f; echo in$x; done\necho done\n",
            "in1\nin2\ndone\n",
        );
    }

    #[test]
    fn cd_searches_cdpath_and_reports_the_directory_it_found() {
        set_env_vars();
        let base = Path::new(concat!(env!("CARGO_TARGET_TMPDIR"), "/sh_test_write_dir"))
            .join("cdpath_test");
        let _ = std::fs::remove_dir_all(&base);
        std::fs::create_dir_all(base.join("first/sub")).unwrap();
        std::fs::create_dir_all(base.join("second/sub")).unwrap();
        // The first matching CDPATH entry wins, and the resolved directory is
        // written to standard output.
        run_successfully_and(
            &format!(
                "CDPATH='{first}:{second}'; cd sub; pwd\n",
                first = base.join("first").display(),
                second = base.join("second").display(),
            ),
            |out| {
                let expected = base.join("first/sub");
                assert_eq!(
                    out,
                    format!("{}\n{}\n", expected.display(), expected.display())
                );
            },
        );
    }

    #[test]
    fn cd_resolves_dot_dot_lexically_unless_physical() {
        set_env_vars();
        let base = Path::new(concat!(env!("CARGO_TARGET_TMPDIR"), "/sh_test_write_dir"))
            .join("cd_dotdot_test");
        let _ = std::fs::remove_dir_all(&base);
        std::fs::create_dir_all(base.join("real/inner")).unwrap();
        std::os::unix::fs::symlink(base.join("real/inner"), base.join("link")).unwrap();
        // -L (the default) removes `link/..` lexically, landing back in base.
        run_successfully_and(
            &format!("cd '{}/link'; cd ..; pwd\n", base.display()),
            |out| assert_eq!(out, format!("{}\n", base.display())),
        );
        // -P follows the symbolic link first, so `..` lands in real/.
        run_successfully_and(
            &format!("cd '{}/link'; cd -P ..; pwd\n", base.display()),
            |out| assert_eq!(out, format!("{}\n", base.join("real").display())),
        );
    }

    #[test]
    fn hash_forgets_remembered_locations_when_path_changes() {
        set_env_vars();
        // `hash name` remembers a location, `hash` lists it, and assigning to
        // PATH clears the table.
        run_successfully_and(
            "saved=$PATH\nhash true\nbefore=$(hash)\nPATH=/nonexistent\nafter=$(hash)\n\
             PATH=$saved\necho \"[$before][$after]\"\n",
            |out| {
                assert!(
                    out.contains("true"),
                    "expected a remembered location: {out:?}"
                );
                assert!(out.ends_with("[]\n"), "expected an empty table: {out:?}");
            },
        );
        // The not-found diagnostic goes to stderr, not stdout.
        run_script_with_checker("hash no_such_command_xyz\n", |output| {
            assert!(String::from_utf8_lossy(&output.stdout).is_empty());
            assert!(!String::from_utf8_lossy(&output.stderr).is_empty());
        });
    }

    // ----- Review follow-up: regressions found by the branch review -----

    #[test]
    fn here_document_may_be_followed_by_a_closing_operator() {
        // The remainder of the `<<` line is pushed back as a new source part;
        // an operator starting exactly at that boundary must be consumed once,
        // not lexed twice.
        test_script("(cat <<EOF\nx\nEOF\n)\n", "x\n");
        test_script("case a in\na) cat <<EOF\nx\nEOF\n;;\nesac\n", "x\n");
        test_script("f() (cat <<EOF\nx\nEOF\n)\nf\n", "x\n");
        test_script("{ cat <<EOF\nx\nEOF\n}\n", "x\n");
    }

    #[test]
    fn exit_with_a_signal_status_is_a_normal_exit() {
        // POSIX 2.15: `exit n` for n in 0..255 exits with that status and
        // performs no signal side effects such as a core dump.
        set_env_vars();
        for status in [130, 134, 143] {
            run_script_with_checker(&format!("exit {status}\n"), |output| {
                use std::os::unix::process::ExitStatusExt;
                assert_eq!(output.status.signal(), None, "expected a normal exit");
                assert_eq!(output.status.code(), Some(status));
            });
        }
        // A subshell exiting with such a status must not signal its parent.
        run_script_with_checker("(exit 130)\necho alive\nexit 7\n", |output| {
            assert_eq!(String::from_utf8_lossy(&output.stdout), "alive\n");
            assert_eq!(output.status.code(), Some(7));
        });
    }

    #[test]
    fn read_consumes_successive_here_document_lines() {
        test_script(
            "while read l; do echo \"got:$l\"; done <<EOF\na\nb\nEOF\n",
            "got:a\ngot:b\n",
        );
    }

    #[test]
    fn ulimit_accepts_a_bare_newlimit_operand() {
        // POSIX synopsis is `ulimit [-f] [blocks]`, so -f is implied.
        run_successfully_and("ulimit 2000\nulimit -f\n", |out| assert_eq!(out, "2000\n"));
    }

    #[test]
    fn bg_reports_a_job_that_has_already_terminated() {
        // Only an already-*running* job is a silent success; a finished job is
        // still an error.
        expect_clean_failure("set -m\nsleep 0.05 &\nsleep 0.3\nbg %1\n");
    }

    #[test]
    fn fc_out_of_range_endpoints_do_not_underflow() {
        // `fc -l 0` and out-of-range endpoints must clamp, never underflow.
        // The listing itself depends on $HISTFILE, so only the shell's
        // survival is asserted here.
        set_env_vars();
        for script in [
            "echo one\necho two\nfc -l 0 >/dev/null 2>&1\necho done\n",
            "echo one\nfc -l 100 200 >/dev/null 2>&1\necho done\n",
            "echo one\nfc -l -n 0 500 >/dev/null 2>&1\necho done\n",
        ] {
            run_script_with_checker(script, |output| {
                let stderr = String::from_utf8_lossy(&output.stderr);
                assert!(!stderr.contains("panicked"), "shell panicked: {stderr}");
                assert!(String::from_utf8_lossy(&output.stdout).ends_with("done\n"));
            });
        }
    }

    // ----- Review follow-up (round 2) -----

    #[test]
    fn here_document_inside_a_command_substitution() {
        // `WordLexer::next_line` used to stop *at* the newline instead of
        // consuming it, so scanning the extent of a here-document inside
        // `$(...)` looped on the same empty line forever.
        test_script("x=$(cat <<EOF\nhi\nEOF\n)\necho \"[$x]\"\n", "[hi]\n");
        test_script("x=$(cat <<-EOF\n\thi\n\tEOF\n)\necho \"[$x]\"\n", "[hi]\n");
        test_script(
            "x=$(cat <<EOF | tr a-z A-Z\nhi\nEOF\n)\necho \"[$x]\"\n",
            "[HI]\n",
        );
        test_script(
            "x=$(cat <<'E'\n$notexpanded\nE\n)\necho \"$x\"\n",
            "$notexpanded\n",
        );
    }

    #[test]
    fn here_document_larger_than_the_pipe_buffer() {
        // The body used to be written into a pipe nobody was reading yet, so
        // anything past the 64KiB pipe capacity blocked the shell forever.
        let body = "0123456789abcdefghijklmnopqrstuvwxyz\n".repeat(4000);
        let expected = format!("{}\n", body.len());
        run_successfully_and(&format!("wc -c <<EOF\n{body}EOF\n"), |out| {
            assert_eq!(out.trim_start(), expected)
        });
    }

    #[test]
    fn here_document_descriptor_is_seekable_and_reusable() {
        // A here-document redirection behaves like a file, so successive
        // reads continue where the previous one stopped.
        test_script(
            "exec 3<<EOF\nfirst\nsecond\nEOF\nread a <&3\nread b <&3\necho \"[$a][$b]\"\n",
            "[first][second]\n",
        );
    }

    #[test]
    fn fg_and_bg_refuse_to_run_in_a_subshell() {
        // POSIX only *permits* them to work there. This shell's job table is
        // a pre-fork copy, so a subshell would resume the job while leaving
        // the parent convinced it is still stopped.
        for builtin in ["fg", "bg"] {
            run_script_with_checker(
                &format!("set -m\nsleep 1 &\n( {builtin} %1 )\njobs\n"),
                |output| {
                    let stderr = String::from_utf8_lossy(&output.stderr);
                    assert!(
                        stderr.contains("subshell"),
                        "expected a subshell refusal, got: {stderr}"
                    );
                    assert!(!stderr.contains("no child processes"), "raw errno leaked");
                    // The parent's view of the job must be untouched.
                    assert!(String::from_utf8_lossy(&output.stdout).contains("Running"));
                },
            );
        }
    }

    #[test]
    fn fg_still_works_in_a_non_interactive_shell() {
        // Only the subshell case is refused; POSIX allows `fg` in a
        // non-interactive shell and the job really is this shell's child.
        run_successfully_and("set -m\nsleep 0.05 &\nfg %1 >/dev/null\necho ok\n", |out| {
            assert_eq!(out, "ok\n")
        });
    }

    #[test]
    fn pipelines_do_not_leak_descriptors_to_their_commands() {
        // Only fds 0, 1 and 2 (plus whatever the utility opens itself) may
        // reach a pipeline member.
        if !Path::new("/proc/self/fd").exists() {
            return;
        }
        for script in [
            "true | ls /proc/self/fd\n",
            "ls /proc/self/fd | cat\n",
            "true | true | ls /proc/self/fd\n",
        ] {
            run_successfully_and(script, |out| {
                let fds: Vec<&str> = out.split_whitespace().collect();
                // 0, 1, 2 and the descriptor `ls` opened to read the directory.
                assert_eq!(fds.len(), 4, "leaked descriptors: {fds:?}");
            });
        }
    }

    #[test]
    fn exit_status_of_a_bare_command_substitution() {
        // POSIX 2.9.1: a command consisting only of substitutions takes the
        // exit status of the last one.
        test_script("$(exit 3)\necho \"[$?]\"\n", "[3]\n");
        test_script("$(true)\necho \"[$?]\"\n", "[0]\n");
        test_script("false\n$(exit 3)\necho \"[$?]\"\n", "[3]\n");
        // An assignment has its own status, taken from the substitution too.
        test_script("x=$(exit 5)\necho \"[$?]\"\n", "[5]\n");
    }

    #[test]
    fn a_command_file_that_cannot_be_used_exits_126() {
        // POSIX: 127 only when the command_file could not be found, 126 when
        // it was found but could not be invoked. Statuses above 128 are
        // reserved for death by a signal.
        set_env_vars();
        let dir = Path::new(concat!(env!("CARGO_TARGET_TMPDIR"), "/sh_test_write_dir"))
            .join("command_file_status");
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        expect_cli_exit_code(vec![dir.display().to_string()], "", 126);
        expect_cli_exit_code(vec![format!("{}/no/such/file", dir.display())], "", 127);
    }
}
