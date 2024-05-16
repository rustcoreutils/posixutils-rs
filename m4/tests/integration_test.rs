//! NOTE: This file has been auto generated using build.rs, don't edit by hand!
use similar_asserts::assert_eq;
use std::os::unix::process::ExitStatusExt;

#[derive(serde::Deserialize)]
struct ExpectedOutput {
    stdout: String,
    stderr: String,
    status: i32,
}

#[test]
fn test_define_eval_syntax_order_unquoted() {
    let output = std::process::Command::new("cargo")
        .arg("run")
        .arg("--")
        .arg("fixtures/integration_tests/define_eval_syntax_order_unquoted.m4")
        .output()
        .unwrap();

    let expected_output: ExpectedOutput = serde_json::from_str(
        &std::fs::read_to_string(
            "fixtures/integration_tests/define_eval_syntax_order_unquoted.json",
        )
        .unwrap(),
    )
    .unwrap();
    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        expected_output.stdout
    );
    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        expected_output.stderr
    );
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(expected_output.status)
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

    let expected_output: ExpectedOutput = serde_json::from_str(
        &std::fs::read_to_string(
            "fixtures/integration_tests/define_eval_syntax_order_quoted_evaluated.json",
        )
        .unwrap(),
    )
    .unwrap();
    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        expected_output.stdout
    );
    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        expected_output.stderr
    );
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(expected_output.status)
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

    let expected_output: ExpectedOutput = serde_json::from_str(
        &std::fs::read_to_string("fixtures/integration_tests/macro_errprint_evaluation.json")
            .unwrap(),
    )
    .unwrap();
    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        expected_output.stdout
    );
    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        expected_output.stderr
    );
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(expected_output.status)
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

    let expected_output: ExpectedOutput = serde_json::from_str(
        &std::fs::read_to_string("fixtures/integration_tests/define_eval_order_quoted.json")
            .unwrap(),
    )
    .unwrap();
    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        expected_output.stdout
    );
    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        expected_output.stderr
    );
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(expected_output.status)
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

    let expected_output: ExpectedOutput = serde_json::from_str(
        &std::fs::read_to_string("fixtures/integration_tests/evaluation_order.json").unwrap(),
    )
    .unwrap();
    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        expected_output.stdout
    );
    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        expected_output.stderr
    );
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(expected_output.status)
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

    let expected_output: ExpectedOutput = serde_json::from_str(
        &std::fs::read_to_string("fixtures/integration_tests/define_order_undefined.json").unwrap(),
    )
    .unwrap();
    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        expected_output.stdout
    );
    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        expected_output.stderr
    );
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(expected_output.status)
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

    let expected_output: ExpectedOutput = serde_json::from_str(
        &std::fs::read_to_string(
            "fixtures/integration_tests/macro_errprint_no_evaluation_quoted.json",
        )
        .unwrap(),
    )
    .unwrap();
    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        expected_output.stdout
    );
    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        expected_output.stderr
    );
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(expected_output.status)
    );
}

#[test]
fn test_changequote_in_quotes_excess_arguments() {
    let output = std::process::Command::new("cargo")
        .arg("run")
        .arg("--")
        .arg("fixtures/integration_tests/changequote_in_quotes_excess_arguments.m4")
        .output()
        .unwrap();

    let expected_output: ExpectedOutput = serde_json::from_str(
        &std::fs::read_to_string(
            "fixtures/integration_tests/changequote_in_quotes_excess_arguments.json",
        )
        .unwrap(),
    )
    .unwrap();
    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        expected_output.stdout
    );
    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        expected_output.stderr
    );
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(expected_output.status)
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

    let expected_output: ExpectedOutput = serde_json::from_str(
        &std::fs::read_to_string("fixtures/integration_tests/define_order_defined.json").unwrap(),
    )
    .unwrap();
    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        expected_output.stdout
    );
    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        expected_output.stderr
    );
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(expected_output.status)
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

    let expected_output: ExpectedOutput = serde_json::from_str(
        &std::fs::read_to_string("fixtures/integration_tests/macro_errprint_no_evaluation.json")
            .unwrap(),
    )
    .unwrap();
    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        expected_output.stdout
    );
    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        expected_output.stderr
    );
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(expected_output.status)
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

    let expected_output: ExpectedOutput = serde_json::from_str(
        &std::fs::read_to_string("fixtures/integration_tests/define_eval_order_unquoted.json")
            .unwrap(),
    )
    .unwrap();
    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        expected_output.stdout
    );
    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        expected_output.stderr
    );
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(expected_output.status)
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

    let expected_output: ExpectedOutput = serde_json::from_str(
        &std::fs::read_to_string(
            "fixtures/integration_tests/define_eval_syntax_order_quoted_unevaluated.json",
        )
        .unwrap(),
    )
    .unwrap();
    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        expected_output.stdout
    );
    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        expected_output.stderr
    );
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(expected_output.status)
    );
}
