use plib::{run_test, run_test_with_checker, TestPlan};

fn run_successfully_and<F: Fn(&str)>(program: &str, checker: F) {
    run_test_with_checker(
        TestPlan {
            cmd: "sh".to_string(),
            args: vec!["-s".to_string()],
            stdin_data: program.to_string(),
            expected_out: "".to_string(),
            expected_err: "".to_string(),
            expected_exit_code: 0,
        },
        |_, output| {
            checker(&String::from_utf8_lossy(&output.stdout));
        },
    )
}

fn test_cli(args: Vec<&str>, stdin: &str, expected_output: &str) {
    run_test(TestPlan {
        cmd: "sh".to_string(),
        args: args.iter().map(|s| s.to_string()).collect(),
        stdin_data: stdin.to_string(),
        expected_out: expected_output.to_string(),
        expected_err: String::default(),
        expected_exit_code: 0,
    });
}

fn is_pid(s: &str) -> bool {
    s.trim_end_matches('\n').chars().all(|c| c.is_ascii_digit())
}

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
hashall   off
monitor   off
noexec    off
nounset   off
verbose   off
xtrace    off
ignoreeof off
nolog     off
vi        off
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
set +h
set +o monitor
set +o noexec
set +o nounset
set +o verbose
set +o xtrace
set +o ignoreeof
set +o nolog
set +o vi
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
hashall   off
monitor   off
noexec    off
nounset   on
verbose   off
xtrace    off
ignoreeof off
nolog     off
vi        off
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
set +h
set +o monitor
set +o noexec
set -o nounset
set +o verbose
set +o xtrace
set +o ignoreeof
set +o nolog
set +o vi
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

#[test]
fn expand_shell_pid() {
    run_successfully_and("echo $$", |output| {
        assert!(!output.is_empty());
        assert!(is_pid(output));
    })
}
