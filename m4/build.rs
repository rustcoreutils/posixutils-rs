use std::{
    collections::BTreeMap,
    fs::read_dir,
    os::unix::ffi::OsStrExt,
    path::{Path, PathBuf},
};

use m4_test_manager::TestSnapshot;

/// A candididate for an integration test [`Test`], can be converted into one if both
/// [`TestCandidate::input`] and [`TestCandidate::output`] are `Some`. This is created during the
/// process of analyzing the available integration test files in order to pair up the input and
/// output for the same test.
#[derive(Default)]
struct TestCandidate {
    /// The name of the test.
    name: String,
    /// Input `.m4` or `.args` file. Will be `None` if has not yet been found.
    input: Option<PathBuf>,
    /// Output `.out` file. Will be `None` if has not yet been found.
    output: Option<PathBuf>,
    /// See [`TestSnapshot::ignore`].
    ignore: bool,
    /// See [`TestSnapshot::expect_error`].
    expect_error: bool,
    /// See [`TestSnapshot::stdout_regex`].
    stdout_regex: Option<String>,
}

impl TryFrom<TestCandidate> for Test {
    type Error = &'static str;

    fn try_from(value: TestCandidate) -> Result<Self, Self::Error> {
        Ok(Test {
            name: value.name,
            input: value
                .input
                .ok_or("No input provided")?
                .to_str()
                .ok_or("Error converting input path to string")?
                .to_owned(),
            output: value
                .output
                .ok_or("No output file provided, please run m4-test-manager update-snapshots")?
                .to_str()
                .ok_or("Error converting output path to string")?
                .to_owned(),
            ignore: value.ignore,
            expect_error: value.expect_error,
            stdout_regex: value.stdout_regex,
        })
    }
}

struct Test {
    /// The name of the test.
    name: String,
    /// Input `.m4` file.
    input: String,
    /// Output `.out` file
    output: String,
    /// See [`TestSnapshot::ignore`].
    ignore: bool,
    /// See [`TestSnapshot::expect_error`].
    expect_error: bool,
    /// See [`TestSnapshot::stdout_regex`].
    stdout_regex: Option<String>,
}
impl Test {
    fn as_code(&self) -> String {
        let Self {
            name,
            input,
            output,
            ignore,
            expect_error,
            stdout_regex,
        } = self;
        let mut s = String::new();

        if *ignore {
            s.push_str("#[ignore]");
        }

        s.push_str(&format!(
            r##"#[test]
fn test_{name}() {{
    init();
    let output = run_command(&Path::new("{input}"));

    let test: TestSnapshot = read_test("{output}");
    assert_eq!(output.status, std::process::ExitStatus::from_raw(test.status), "status (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)");
    
"##
        ));

        if let Some(stdout_regex) = stdout_regex {
            s.push_str(&format!(
                r##"
    let r = regex_lite::Regex::new(r"{stdout_regex}").unwrap();
    assert!(r.is_match(&String::from_utf8(output.stdout).unwrap()), "stdout doesn't match regex: r\"{{}}\"", "{stdout_regex}");
            "##
            ));
        } else {
            s.push_str(r##"
    assert_eq!(String::from_utf8(output.stdout).unwrap(), test.stdout, "stdout (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)");
            "##);
        }

        if *expect_error {
            s.push_str(
                r##"
    if !test.stderr.is_empty() {
        assert!(!output.stderr.is_empty());
    }"##,
            );
        } else {
            s.push_str(r##"
    assert_eq!(String::from_utf8(output.stderr).unwrap(), test.stderr, "stderr (\x1b[31mcurrent\x1b[0m|\x1b[32mexpected\x1b[0m)");"##);
        }

        s.push_str("}");

        s
    }
}

fn name_from_path(path: &Path) -> Option<String> {
    Some(path.file_name()?.to_str()?.split(".").next()?.to_owned())
}

fn main() {
    println!("cargo::rerun-if-changed=fixtures/");
    let mut test_candidates: BTreeMap<String, TestCandidate> = BTreeMap::new();
    let fixtures_directory = Path::new("fixtures/integration_tests");
    for entry in read_dir(fixtures_directory).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();

        match path.extension().map(|e| e.as_bytes()) {
            Some(b"m4") | Some(b"args") => {
                let name = name_from_path(&path).unwrap();
                let snapshot_file_name = format!("{name}.out");
                let snapshot_file = fixtures_directory.join(snapshot_file_name);
                let (ignore, expect_error, stdout_regex) = if snapshot_file.exists() {
                    let mut f = std::fs::OpenOptions::new()
                        .read(true)
                        .open(&snapshot_file)
                        .unwrap();
                    let snapshot = TestSnapshot::deserialize(&mut f);
                    (
                        snapshot.ignore,
                        snapshot.expect_error,
                        snapshot.stdout_regex,
                    )
                } else {
                    (false, false, None)
                };
                let candidate = test_candidates
                    .entry(name.clone())
                    .or_insert(TestCandidate {
                        name,
                        ignore,
                        expect_error,
                        stdout_regex: stdout_regex.clone(),
                        ..TestCandidate::default()
                    });
                candidate.input = Some(path);
                candidate.ignore = ignore;
                candidate.expect_error = expect_error;
                candidate.stdout_regex = stdout_regex;
            }
            Some(b"out") => {
                let name = name_from_path(&path).unwrap();
                let candidate = test_candidates
                    .entry(name.clone())
                    .or_insert(TestCandidate {
                        name,
                        ..TestCandidate::default()
                    });
                candidate.output = Some(path);
            }
            _ => eprintln!("Ignoring file {path:?}"),
        }
    }
    let mut integration_test: String =
        r#"//! NOTE: This file has been auto generated using build.rs, don't edit by hand!
//! You can regenerate the tests (which are based on the fixtures in `fixtures/integration_tests/`)
//! using the following command:
//! `cargo run -p m4-test-manager update-snapshots`
use similar_asserts::assert_eq;
use std::process::ExitStatus;
use std::os::unix::ffi::OsStrExt;
use std::os::unix::process::ExitStatusExt;
use std::fs::read_to_string;
use std::path::Path;
use m4::error::GetExitCode;
use m4_test_manager::TestSnapshot;

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
    log::info!(
        "Expecting stdout:\n\x1b[34m{}\x1b[0m",
        snapshot.stdout,
    );
    log::info!(
        "Expecting stderr:\n\x1b[34m{}\x1b[0m",
        snapshot.stderr,
    );
    log::info!(
        "Expecting status:\n\x1b[34m{}\x1b[0m",
        snapshot.status,
    );
    snapshot
}

fn run_command(input: &Path) -> std::process::Output {
    let input_string = read_to_string(input).unwrap();
    log::info!(
        "Running command with input {input:?}:\n\x1b[34m{}\x1b[0m",
        input_string,
    );
    let (stdout, stderr, status) = match input.extension().expect("Input file should have extension").as_bytes() {
        b"m4" => {
            // The reason why we run the command using this as a library is so we can run with it built in
            // test configuration, with all the associated conditionally compiled test log instrumentation.
            
            let mut stdout: Vec<u8> = Vec::new();
            let mut stderr: Vec<u8> = Vec::new();
            let args = m4::Args {
                files: vec![input.into()],
                ..m4::Args::default()
            };
            let result = m4::run(&mut stdout, &mut stderr, args);
            let status = ExitStatus::from_raw(result.get_exit_code() as i32);
            (stdout, stderr, status)
        },
        b"args" => {
            let args = input_string;
            let _cargo_build_output = std::process::Command::new("cargo")
                .arg("build")
                .output()
                .unwrap();

            let output = std::process::Command::new("sh")
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
"#
        .to_owned();
    for (name, candidate) in test_candidates {
        let test: Test = candidate
            .try_into()
            .expect(&format!("Error creating test from candidate for {name}"));

        integration_test.push('\n');
        integration_test.push_str(&test.as_code());
        integration_test.push('\n');
    }

    std::fs::write("tests/integration_test.rs", integration_test).unwrap();
    let output = std::process::Command::new("cargo")
        .arg("fmt")
        .arg("--")
        .arg("tests/integration_test.rs")
        .output()
        .unwrap();
    if !output.status.success() {
        panic!(
            "Error executing cargo fmt: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }
}
