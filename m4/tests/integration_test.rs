//! NOTE: This file has been auto generated using build.rs, don't edit by hand!
use similar_asserts::assert_eq;
use std::collections::HashMap;
use std::fs::read_to_string;
use std::os::unix::process::ExitStatusExt;
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

#[test]
fn test_changequote_in_quotes_excess_arguments() {
    let output = std::process::Command::new("cargo")
        .arg("run")
        .arg("--")
        .arg("fixtures/integration_tests/changequote_in_quotes_excess_arguments.m4")
        .output()
        .unwrap();

    let test: Test =
        read_test_json("fixtures/integration_tests/changequote_in_quotes_excess_arguments.json");
    assert_eq!(String::from_utf8(output.stdout).unwrap(), test.stdout);
    assert_eq!(String::from_utf8(output.stderr).unwrap(), test.stderr);
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status)
    );
}

#[test]
fn test_define_eval_syntax_order_quoted_evaluated() {
    let output = std::process::Command::new("cargo")
        .arg("run")
        .arg("--")
        .arg("fixtures/integration_tests/define_eval_syntax_order_quoted_evaluated.m4")
        .output()
        .unwrap();

    let test: Test =
        read_test_json("fixtures/integration_tests/define_eval_syntax_order_quoted_evaluated.json");
    assert_eq!(String::from_utf8(output.stdout).unwrap(), test.stdout);
    assert_eq!(String::from_utf8(output.stderr).unwrap(), test.stderr);
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status)
    );
}

#[test]
fn test_define_eval_order_unquoted() {
    let output = std::process::Command::new("cargo")
        .arg("run")
        .arg("--")
        .arg("fixtures/integration_tests/define_eval_order_unquoted.m4")
        .output()
        .unwrap();

    let test: Test = read_test_json("fixtures/integration_tests/define_eval_order_unquoted.json");
    assert_eq!(String::from_utf8(output.stdout).unwrap(), test.stdout);
    assert_eq!(String::from_utf8(output.stderr).unwrap(), test.stderr);
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status)
    );
}

#[test]
fn test_evaluation_order() {
    let output = std::process::Command::new("cargo")
        .arg("run")
        .arg("--")
        .arg("fixtures/integration_tests/evaluation_order.m4")
        .output()
        .unwrap();

    let test: Test = read_test_json("fixtures/integration_tests/evaluation_order.json");
    assert_eq!(String::from_utf8(output.stdout).unwrap(), test.stdout);
    assert_eq!(String::from_utf8(output.stderr).unwrap(), test.stderr);
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status)
    );
}

#[test]
fn test_define_order_undefined() {
    let output = std::process::Command::new("cargo")
        .arg("run")
        .arg("--")
        .arg("fixtures/integration_tests/define_order_undefined.m4")
        .output()
        .unwrap();

    let test: Test = read_test_json("fixtures/integration_tests/define_order_undefined.json");
    assert_eq!(String::from_utf8(output.stdout).unwrap(), test.stdout);
    assert_eq!(String::from_utf8(output.stderr).unwrap(), test.stderr);
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status)
    );
}

#[test]
fn test_define_eval_order_quoted() {
    let output = std::process::Command::new("cargo")
        .arg("run")
        .arg("--")
        .arg("fixtures/integration_tests/define_eval_order_quoted.m4")
        .output()
        .unwrap();

    let test: Test = read_test_json("fixtures/integration_tests/define_eval_order_quoted.json");
    assert_eq!(String::from_utf8(output.stdout).unwrap(), test.stdout);
    assert_eq!(String::from_utf8(output.stderr).unwrap(), test.stderr);
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status)
    );
}

#[test]
fn test_define_eval_syntax_order_unquoted() {
    let output = std::process::Command::new("cargo")
        .arg("run")
        .arg("--")
        .arg("fixtures/integration_tests/define_eval_syntax_order_unquoted.m4")
        .output()
        .unwrap();

    let test: Test =
        read_test_json("fixtures/integration_tests/define_eval_syntax_order_unquoted.json");
    assert_eq!(String::from_utf8(output.stdout).unwrap(), test.stdout);
    assert_eq!(String::from_utf8(output.stderr).unwrap(), test.stderr);
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status)
    );
}

#[test]
fn test_macro_errprint_evaluation() {
    let output = std::process::Command::new("cargo")
        .arg("run")
        .arg("--")
        .arg("fixtures/integration_tests/macro_errprint_evaluation.m4")
        .output()
        .unwrap();

    let test: Test = read_test_json("fixtures/integration_tests/macro_errprint_evaluation.json");
    assert_eq!(String::from_utf8(output.stdout).unwrap(), test.stdout);
    assert_eq!(String::from_utf8(output.stderr).unwrap(), test.stderr);
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status)
    );
}

#[test]
fn test_macro_errprint_no_evaluation() {
    let output = std::process::Command::new("cargo")
        .arg("run")
        .arg("--")
        .arg("fixtures/integration_tests/macro_errprint_no_evaluation.m4")
        .output()
        .unwrap();

    let test: Test = read_test_json("fixtures/integration_tests/macro_errprint_no_evaluation.json");
    assert_eq!(String::from_utf8(output.stdout).unwrap(), test.stdout);
    assert_eq!(String::from_utf8(output.stderr).unwrap(), test.stderr);
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status)
    );
}

#[test]
fn test_define_order_defined() {
    let output = std::process::Command::new("cargo")
        .arg("run")
        .arg("--")
        .arg("fixtures/integration_tests/define_order_defined.m4")
        .output()
        .unwrap();

    let test: Test = read_test_json("fixtures/integration_tests/define_order_defined.json");
    assert_eq!(String::from_utf8(output.stdout).unwrap(), test.stdout);
    assert_eq!(String::from_utf8(output.stderr).unwrap(), test.stderr);
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status)
    );
}

#[test]
fn test_macro_errprint_no_evaluation_quoted() {
    let output = std::process::Command::new("cargo")
        .arg("run")
        .arg("--")
        .arg("fixtures/integration_tests/macro_errprint_no_evaluation_quoted.m4")
        .output()
        .unwrap();

    let test: Test =
        read_test_json("fixtures/integration_tests/macro_errprint_no_evaluation_quoted.json");
    assert_eq!(String::from_utf8(output.stdout).unwrap(), test.stdout);
    assert_eq!(String::from_utf8(output.stderr).unwrap(), test.stderr);
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status)
    );
}

#[test]
fn test_define_eval_syntax_order_quoted_unevaluated() {
    let output = std::process::Command::new("cargo")
        .arg("run")
        .arg("--")
        .arg("fixtures/integration_tests/define_eval_syntax_order_quoted_unevaluated.m4")
        .output()
        .unwrap();

    let test: Test = read_test_json(
        "fixtures/integration_tests/define_eval_syntax_order_quoted_unevaluated.json",
    );
    assert_eq!(String::from_utf8(output.stdout).unwrap(), test.stdout);
    assert_eq!(String::from_utf8(output.stderr).unwrap(), test.stderr);
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status)
    );
}
