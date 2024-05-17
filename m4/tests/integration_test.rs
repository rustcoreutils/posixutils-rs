//! NOTE: This file has been auto generated using build.rs, don't edit by hand!
use m4::error::GetExitCode;
use similar_asserts::assert_eq;
use std::collections::HashMap;
use std::fs::read_to_string;
use std::os::unix::process::ExitStatusExt;
use std::process::ExitStatus;
use test_log::test;
use tinyjson::JsonValue;

struct Test {
    stdout: String,
    stderr: String,
    status: i32,
}

fn read_test_json(path: impl AsRef<std::path::Path>) -> Test {
    let value: JsonValue = read_to_string(path).unwrap().parse().unwrap();
    let map: &HashMap<_, _> = value.get().unwrap();
    let stdout: &String = map.get("stdout").unwrap().get().unwrap();
    let stderr: &String = map.get("stderr").unwrap().get().unwrap();
    let status: &f64 = map.get("status").unwrap().get().unwrap();
    let status = status.round() as i32;
    Test {
        stdout: stdout.clone(),
        stderr: stderr.clone(),
        status,
    }
}

fn run_command(input: &str) -> std::process::Output {
    // std::process::Command::new("cargo")
    //     .arg("run")
    //     .arg("--")
    //     .arg(input)
    //     .output()
    //     .unwrap()
    log::info!(
        "Running command with input {input:?}:\n\x1b[34m{}\x1b[0m",
        read_to_string(input).unwrap()
    );
    let mut stdout: Vec<u8> = Vec::new();
    let mut stderr: Vec<u8> = Vec::new();
    let args = m4::Args {
        file: Some(input.into()),
        ..m4::Args::default()
    };
    let result = m4::run(&mut stdout, &mut stderr, args);
    let status = ExitStatus::from_raw(result.get_exit_code() as i32);
    log::info!("Received status: {status}");
    log::info!("Received stdout: {}", String::from_utf8_lossy(&stdout));
    log::info!("Received stderr: {}", String::from_utf8_lossy(&stderr));
    std::process::Output {
        stdout,
        stderr,
        status,
    }
}

#[test]
fn test_define_eval_order_quoted() {
    let output = run_command("fixtures/integration_tests/define_eval_order_quoted.m4");

    let test: Test = read_test_json("fixtures/integration_tests/define_eval_order_quoted.json");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status"
    );
    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout"
    );
    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr"
    );
}

#[test]
fn test_define_eval_order_unquoted() {
    let output = run_command("fixtures/integration_tests/define_eval_order_unquoted.m4");

    let test: Test = read_test_json("fixtures/integration_tests/define_eval_order_unquoted.json");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status"
    );
    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout"
    );
    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr"
    );
}

#[test]
fn test_define_eval_syntax_order_quoted_evaluated() {
    let output =
        run_command("fixtures/integration_tests/define_eval_syntax_order_quoted_evaluated.m4");

    let test: Test =
        read_test_json("fixtures/integration_tests/define_eval_syntax_order_quoted_evaluated.json");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status"
    );
    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout"
    );
    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr"
    );
}

#[test]
fn test_define_eval_syntax_order_quoted_unevaluated() {
    let output =
        run_command("fixtures/integration_tests/define_eval_syntax_order_quoted_unevaluated.m4");

    let test: Test = read_test_json(
        "fixtures/integration_tests/define_eval_syntax_order_quoted_unevaluated.json",
    );
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status"
    );
    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout"
    );
    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr"
    );
}

#[test]
fn test_define_eval_syntax_order_unquoted() {
    let output = run_command("fixtures/integration_tests/define_eval_syntax_order_unquoted.m4");

    let test: Test =
        read_test_json("fixtures/integration_tests/define_eval_syntax_order_unquoted.json");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status"
    );
    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout"
    );
    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr"
    );
}

#[test]
fn test_define_order_defined() {
    let output = run_command("fixtures/integration_tests/define_order_defined.m4");

    let test: Test = read_test_json("fixtures/integration_tests/define_order_defined.json");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status"
    );
    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout"
    );
    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr"
    );
}

#[test]
fn test_define_order_undefined() {
    let output = run_command("fixtures/integration_tests/define_order_undefined.m4");

    let test: Test = read_test_json("fixtures/integration_tests/define_order_undefined.json");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status"
    );
    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout"
    );
    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr"
    );
}

#[test]
fn test_evaluation_order() {
    let output = run_command("fixtures/integration_tests/evaluation_order.m4");

    let test: Test = read_test_json("fixtures/integration_tests/evaluation_order.json");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status"
    );
    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout"
    );
    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr"
    );
}

#[test]
fn test_macro_errprint_evaluation() {
    let output = run_command("fixtures/integration_tests/macro_errprint_evaluation.m4");

    let test: Test = read_test_json("fixtures/integration_tests/macro_errprint_evaluation.json");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status"
    );
    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout"
    );
    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr"
    );
}

#[test]
fn test_macro_errprint_no_evaluation() {
    let output = run_command("fixtures/integration_tests/macro_errprint_no_evaluation.m4");

    let test: Test = read_test_json("fixtures/integration_tests/macro_errprint_no_evaluation.json");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status"
    );
    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout"
    );
    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr"
    );
}

#[test]
fn test_macro_errprint_no_evaluation_quoted() {
    let output = run_command("fixtures/integration_tests/macro_errprint_no_evaluation_quoted.m4");

    let test: Test =
        read_test_json("fixtures/integration_tests/macro_errprint_no_evaluation_quoted.json");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status"
    );
    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout"
    );
    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr"
    );
}
