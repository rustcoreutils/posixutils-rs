//
// Copyright (c) 2025-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

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
}
