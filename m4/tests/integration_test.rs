//! NOTE: This file has been auto generated using build.rs, don't edit by hand!
use m4::error::GetExitCode;
use m4_test_manager::TestSnapshot;
use similar_asserts::assert_eq;
use std::fs::read_to_string;
use std::os::unix::process::ExitStatusExt;
use std::process::ExitStatus;
use test_log::test;

fn read_test(path: impl AsRef<std::path::Path>) -> TestSnapshot {
    let mut f = std::fs::File::open(path).unwrap();
    TestSnapshot::deserialize(&mut f)
}

fn run_command(input: &str) -> std::process::Output {
    // std::process::Command::new("cargo")
    //     .arg("run")
    //     .arg("--")
    //     .arg(input)
    //     .output()
    //     .unwrap()

    // The reason why we run the command using this as a library is so we can run with it built in
    // test configuration, with all the associated conditionally compiled test log instrumentation.
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

    let test: TestSnapshot = read_test("fixtures/integration_tests/define_eval_order_quoted.out");
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

    let test: TestSnapshot = read_test("fixtures/integration_tests/define_eval_order_unquoted.out");
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

    let test: TestSnapshot =
        read_test("fixtures/integration_tests/define_eval_syntax_order_quoted_evaluated.out");
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

    let test: TestSnapshot =
        read_test("fixtures/integration_tests/define_eval_syntax_order_quoted_unevaluated.out");
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

    let test: TestSnapshot =
        read_test("fixtures/integration_tests/define_eval_syntax_order_unquoted.out");
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

    let test: TestSnapshot = read_test("fixtures/integration_tests/define_order_defined.out");
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

    let test: TestSnapshot = read_test("fixtures/integration_tests/define_order_undefined.out");
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

    let test: TestSnapshot = read_test("fixtures/integration_tests/evaluation_order.out");
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
fn test_include() {
    let output = run_command("fixtures/integration_tests/include.m4");

    let test: TestSnapshot = read_test("fixtures/integration_tests/include.out");
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

    let test: TestSnapshot = read_test("fixtures/integration_tests/macro_errprint_evaluation.out");
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

    let test: TestSnapshot =
        read_test("fixtures/integration_tests/macro_errprint_no_evaluation.out");
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

    let test: TestSnapshot =
        read_test("fixtures/integration_tests/macro_errprint_no_evaluation_quoted.out");
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
