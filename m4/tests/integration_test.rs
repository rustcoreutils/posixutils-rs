//! NOTE: This file has been auto generated using build.rs, don't edit by hand!
//! You can regenerate the tests (which are based on the fixtures in `fixtures/integration_tests/`)
//! using the following command:
//! `cargo run -p m4-test-manager update-snapshots`
use std::fs::read_to_string;
use std::os::unix::ffi::OsStrExt;
use std::os::unix::process::ExitStatusExt;
use std::path::Path;
use std::process::ExitStatus;

use m4_test_manager::TestSnapshot;
use posixutils_m4::error::GetExitCode;
use similar_asserts::assert_eq;

fn init() {
    let _ = env_logger::builder()
        .is_test(true)
        // No timestamp to make it easier to diff output
        .format_timestamp(None)
        .try_init();
}

fn read_test(path: impl AsRef<std::path::Path>) -> TestSnapshot {
    let mut f = std::fs::File::open(path).unwrap();
    let snapshot = TestSnapshot::deserialize(&mut f);
    log::info!("Expecting stdout:\n\x1b[34m{}\x1b[0m", snapshot.stdout,);
    log::info!("Expecting stderr:\n\x1b[34m{}\x1b[0m", snapshot.stderr,);
    log::info!("Expecting status:\n\x1b[34m{}\x1b[0m", snapshot.status,);
    snapshot
}

fn run_command(input: &Path) -> std::process::Output {
    let input_string = read_to_string(input).unwrap();
    log::info!(
        "Running command with input {input:?}:\n\x1b[34m{}\x1b[0m",
        input_string,
    );
    #[derive(Default, Clone)]
    struct StdoutRef(std::rc::Rc<std::cell::RefCell<Vec<u8>>>);
    impl std::io::Write for StdoutRef {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            self.0.borrow_mut().write(buf)
        }

        fn flush(&mut self) -> std::io::Result<()> {
            self.0.borrow_mut().flush()
        }
    }
    impl StdoutRef {
        fn into_inner(self) -> Vec<u8> {
            std::rc::Rc::into_inner(self.0).unwrap().into_inner()
        }
    }
    let (stdout, stderr, status) = match input
        .extension()
        .expect("Input file should have extension")
        .as_bytes()
    {
        b"m4" => {
            // The reason why we run the command using this as a library is so we can run with it built in
            // test configuration, with all the associated conditionally compiled test log instrumentation.

            let stdout = StdoutRef::default();
            let mut stderr: Vec<u8> = Vec::new();
            let args = posixutils_m4::Args {
                files: vec![input.into()],
                ..posixutils_m4::Args::default()
            };
            let result = posixutils_m4::run(stdout.clone(), &mut stderr, args);
            let status = ExitStatus::from_raw(result.get_exit_code() as i32);
            (stdout.into_inner(), stderr, status)
        }
        b"args" => {
            let args = input_string;
            let _cargo_build_output = std::process::Command::new("cargo")
                .arg("build")
                .output()
                .unwrap();

            log::info!("RUST_LOG is ignored for this test because it interferes with output");
            let output = std::process::Command::new("sh")
                .env("RUST_LOG", "") // Disable rust log output because it interferes with the test.
                .arg("-c")
                .arg(format!("../target/debug/m4 {args}"))
                .output()
                .unwrap();

            (output.stdout, output.stderr, output.status)
        }
        _ => panic!("Unsupported input extension {input:?}"),
    };

    log::info!("Received status: {status}");
    log::info!(
        "Received stdout:\n\x1b[34m{}\x1b[0m",
        String::from_utf8_lossy(&stdout)
    );
    log::info!(
        "Received stderr:\n\x1b[34m{}\x1b[0m",
        String::from_utf8_lossy(&stderr)
    );
    std::process::Output {
        stdout,
        stderr,
        status,
    }
}

#[ignore]
#[test]
fn test_bsd() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/bsd.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/bsd.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[ignore]
#[test]
fn test_bsd_math() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/bsd_math.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/bsd_math.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_changecom() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/changecom.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/changecom.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_changequote() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/changequote.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/changequote.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_decr() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/decr.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/decr.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_define() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/define.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/define.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_define_args() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/define_args.args"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/define_args.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[ignore]
#[test]
fn test_define_eval_order_unquoted() {
    init();
    let output = run_command(&Path::new(
        "fixtures/integration_tests/define_eval_order_unquoted.m4",
    ));

    let test: TestSnapshot = read_test("fixtures/integration_tests/define_eval_order_unquoted.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[ignore]
#[test]
fn test_define_eval_syntax_order_quoted_evaluated() {
    init();
    let output = run_command(&Path::new(
        "fixtures/integration_tests/define_eval_syntax_order_quoted_evaluated.m4",
    ));

    let test: TestSnapshot =
        read_test("fixtures/integration_tests/define_eval_syntax_order_quoted_evaluated.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_define_eval_syntax_order_quoted_unevaluated() {
    init();
    let output = run_command(&Path::new(
        "fixtures/integration_tests/define_eval_syntax_order_quoted_unevaluated.m4",
    ));

    let test: TestSnapshot =
        read_test("fixtures/integration_tests/define_eval_syntax_order_quoted_unevaluated.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[ignore]
#[test]
fn test_define_eval_syntax_order_unquoted() {
    init();
    let output = run_command(&Path::new(
        "fixtures/integration_tests/define_eval_syntax_order_unquoted.m4",
    ));

    let test: TestSnapshot =
        read_test("fixtures/integration_tests/define_eval_syntax_order_unquoted.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_define_hanging_quotes() {
    init();
    let output = run_command(&Path::new(
        "fixtures/integration_tests/define_hanging_quotes.m4",
    ));

    let test: TestSnapshot = read_test("fixtures/integration_tests/define_hanging_quotes.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_define_invalid_macro_name() {
    init();
    let output = run_command(&Path::new(
        "fixtures/integration_tests/define_invalid_macro_name.m4",
    ));

    let test: TestSnapshot = read_test("fixtures/integration_tests/define_invalid_macro_name.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_define_iterative() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/define_iterative.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/define_iterative.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_define_iterative_2() {
    init();
    let output = run_command(&Path::new(
        "fixtures/integration_tests/define_iterative_2.m4",
    ));

    let test: TestSnapshot = read_test("fixtures/integration_tests/define_iterative_2.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_define_nested() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/define_nested.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/define_nested.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[ignore]
#[test]
fn test_define_nested_first_arg() {
    init();
    let output = run_command(&Path::new(
        "fixtures/integration_tests/define_nested_first_arg.m4",
    ));

    let test: TestSnapshot = read_test("fixtures/integration_tests/define_nested_first_arg.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_define_number_parsing() {
    init();
    let output = run_command(&Path::new(
        "fixtures/integration_tests/define_number_parsing.m4",
    ));

    let test: TestSnapshot = read_test("fixtures/integration_tests/define_number_parsing.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    if !test.stderr.is_empty() {
        assert!(!output.stderr.is_empty());
    }
}

#[test]
fn test_define_order_defined() {
    init();
    let output = run_command(&Path::new(
        "fixtures/integration_tests/define_order_defined.m4",
    ));

    let test: TestSnapshot = read_test("fixtures/integration_tests/define_order_defined.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_define_order_undefined() {
    init();
    let output = run_command(&Path::new(
        "fixtures/integration_tests/define_order_undefined.m4",
    ));

    let test: TestSnapshot = read_test("fixtures/integration_tests/define_order_undefined.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_define_parse_brackets() {
    init();
    let output = run_command(&Path::new(
        "fixtures/integration_tests/define_parse_brackets.m4",
    ));

    let test: TestSnapshot = read_test("fixtures/integration_tests/define_parse_brackets.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    if !test.stderr.is_empty() {
        assert!(!output.stderr.is_empty());
    }
}

#[test]
fn test_define_pushpopdef_undefine() {
    init();
    let output = run_command(&Path::new(
        "fixtures/integration_tests/define_pushpopdef_undefine.m4",
    ));

    let test: TestSnapshot = read_test("fixtures/integration_tests/define_pushpopdef_undefine.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_define_quoted_number_stacked() {
    init();
    let output = run_command(&Path::new(
        "fixtures/integration_tests/define_quoted_number_stacked.m4",
    ));

    let test: TestSnapshot =
        read_test("fixtures/integration_tests/define_quoted_number_stacked.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    if !test.stderr.is_empty() {
        assert!(!output.stderr.is_empty());
    }
}

#[test]
fn test_define_stacked() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/define_stacked.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/define_stacked.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_define_undefine_order() {
    init();
    let output = run_command(&Path::new(
        "fixtures/integration_tests/define_undefine_order.m4",
    ));

    let test: TestSnapshot = read_test("fixtures/integration_tests/define_undefine_order.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_define_unquoted_number_arg() {
    init();
    let output = run_command(&Path::new(
        "fixtures/integration_tests/define_unquoted_number_arg.m4",
    ));

    let test: TestSnapshot = read_test("fixtures/integration_tests/define_unquoted_number_arg.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    if !test.stderr.is_empty() {
        assert!(!output.stderr.is_empty());
    }
}

#[test]
fn test_defn() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/defn.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/defn.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_divert() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/divert.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/divert.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_divert_nested() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/divert_nested.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/divert_nested.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_divert_nested_2() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/divert_nested_2.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/divert_nested_2.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_divert_nested_3() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/divert_nested_3.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/divert_nested_3.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_divert_nested_4() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/divert_nested_4.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/divert_nested_4.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_dnl() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/dnl.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/dnl.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_dnl_nested() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/dnl_nested.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/dnl_nested.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_dumpdef() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/dumpdef.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/dumpdef.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_dumpdef_notexist() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/dumpdef_notexist.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/dumpdef_notexist.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    if !test.stderr.is_empty() {
        assert!(!output.stderr.is_empty());
    }
}

#[test]
fn test_eval() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/eval.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/eval.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_evaluation_order() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/evaluation_order.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/evaluation_order.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_file() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/file.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/file.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_forloop_nested() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/forloop_nested.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/forloop_nested.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_forloop_simple() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/forloop_simple.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/forloop_simple.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_ifdef() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/ifdef.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/ifdef.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_ifelse() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/ifelse.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/ifelse.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_include() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/include.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/include.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_include_divert() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/include_divert.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/include_divert.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_incr() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/incr.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/incr.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_index() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/index.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/index.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_index_too_few_args() {
    init();
    let output = run_command(&Path::new(
        "fixtures/integration_tests/index_too_few_args.m4",
    ));

    let test: TestSnapshot = read_test("fixtures/integration_tests/index_too_few_args.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    if !test.stderr.is_empty() {
        assert!(!output.stderr.is_empty());
    }
}

#[test]
fn test_len() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/len.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/len.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_m4exit_error() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/m4exit_error.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/m4exit_error.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_m4exit_no_args() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/m4exit_no_args.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/m4exit_no_args.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_m4exit_success() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/m4exit_success.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/m4exit_success.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_m4wrap() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/m4wrap.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/m4wrap.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_macro_errprint_evaluation() {
    init();
    let output = run_command(&Path::new(
        "fixtures/integration_tests/macro_errprint_evaluation.m4",
    ));

    let test: TestSnapshot = read_test("fixtures/integration_tests/macro_errprint_evaluation.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_macro_errprint_no_evaluation() {
    init();
    let output = run_command(&Path::new(
        "fixtures/integration_tests/macro_errprint_no_evaluation.m4",
    ));

    let test: TestSnapshot =
        read_test("fixtures/integration_tests/macro_errprint_no_evaluation.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_macro_errprint_no_evaluation_quoted() {
    init();
    let output = run_command(&Path::new(
        "fixtures/integration_tests/macro_errprint_no_evaluation_quoted.m4",
    ));

    let test: TestSnapshot =
        read_test("fixtures/integration_tests/macro_errprint_no_evaluation_quoted.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_maketemp() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/maketemp.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/maketemp.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    let r = regex_lite::Regex::new(r"^/tmp/m4-.{6}$").unwrap();
    assert!(
        r.is_match(&String::from_utf8(output.stdout).unwrap()),
        "stdout doesn't match regex: r\"{}\"",
        "^/tmp/m4-.{6}$"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_mkstemp() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/mkstemp.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/mkstemp.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    let r = regex_lite::Regex::new(r"^/tmp/m4-.{6}$").unwrap();
    assert!(
        r.is_match(&String::from_utf8(output.stdout).unwrap()),
        "stdout doesn't match regex: r\"{}\"",
        "^/tmp/m4-.{6}$"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_quoted_nested_eof_in_string() {
    init();
    let output = run_command(&Path::new(
        "fixtures/integration_tests/quoted_nested_eof_in_string.m4",
    ));

    let test: TestSnapshot =
        read_test("fixtures/integration_tests/quoted_nested_eof_in_string.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    if !test.stderr.is_empty() {
        assert!(!output.stderr.is_empty());
    }
}

#[test]
fn test_recurse() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/recurse.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/recurse.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_recursive_defines() {
    init();
    let output = run_command(&Path::new(
        "fixtures/integration_tests/recursive_defines.m4",
    ));

    let test: TestSnapshot = read_test("fixtures/integration_tests/recursive_defines.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_redefine_inbuilt() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/redefine_inbuilt.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/redefine_inbuilt.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_reverse() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/reverse.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/reverse.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_shift() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/shift.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/shift.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_sinclude() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/sinclude.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/sinclude.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_substr() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/substr.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/substr.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[ignore]
#[test]
fn test_synclines_1() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/synclines_1.args"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/synclines_1.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[ignore]
#[test]
fn test_synclines_2() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/synclines_2.args"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/synclines_2.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[ignore]
#[test]
fn test_syscmd_sysval() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/syscmd_sysval.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/syscmd_sysval.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_trace() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/trace.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/trace.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_translit() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/translit.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/translit.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_two_files() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/two_files.args"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/two_files.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_undivert() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/undivert.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/undivert.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_undivert_2() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/undivert_2.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/undivert_2.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_undivert_current() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/undivert_current.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/undivert_current.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}

#[test]
fn test_undivert_nested() {
    init();
    let output = run_command(&Path::new("fixtures/integration_tests/undivert_nested.m4"));

    let test: TestSnapshot = read_test("fixtures/integration_tests/undivert_nested.out");
    assert_eq!(
        output.status,
        std::process::ExitStatus::from_raw(test.status),
        "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        test.stdout,
        "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );

    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        test.stderr,
        "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)"
    );
}
