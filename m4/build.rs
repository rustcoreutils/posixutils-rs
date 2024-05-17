use std::{
    collections::{BTreeMap, HashMap},
    fs::read_dir,
    os::unix::ffi::OsStrExt,
    path::{Path, PathBuf},
};

#[derive(Default)]
struct TestCandidate {
    name: String,
    input: Option<PathBuf>,
    output_json: Option<PathBuf>,
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
            output_json: value
                .output_json
                .ok_or("No output json file provided")?
                .to_str()
                .ok_or("Error converting output json path to string")?
                .to_owned(),
        })
    }
}

struct Test {
    name: String,
    input: String,
    output_json: String,
}
impl Test {
    fn as_code(&self) -> String {
        let Self {
            name,
            input,
            output_json,
        } = self;
        let mut s = format!(
            r##"#[test]
fn test_{name}() {{
    let output = run_command("{input}");

    let test: Test = read_test_json("{output_json}");
    assert_eq!(output.status, std::process::ExitStatus::from_raw(test.status), "status");
    assert_eq!(String::from_utf8(output.stdout).unwrap(), test.stdout, "stdout");
    assert_eq!(String::from_utf8(output.stderr).unwrap(), test.stderr, "stderr");
"##
        );

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
    for entry in read_dir("fixtures/integration_tests").unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();

        match path.extension().map(|e| e.as_bytes()) {
            Some(b"m4") => {
                let name = name_from_path(&path).unwrap();
                let candidate = test_candidates
                    .entry(name.clone())
                    .or_insert(TestCandidate {
                        name,
                        ..TestCandidate::default()
                    });
                candidate.input = Some(path);
            }
            Some(b"json") => {
                let name = name_from_path(&path).unwrap();
                let candidate = test_candidates
                    .entry(name.clone())
                    .or_insert(TestCandidate {
                        name,
                        ..TestCandidate::default()
                    });
                candidate.output_json = Some(path);
            }
            _ => eprintln!("Ignoring file {path:?}"),
        }
    }
    let mut integration_test: String =
        r#"//! NOTE: This file has been auto generated using build.rs, don't edit by hand!
use similar_asserts::assert_eq;
use std::process::ExitStatus;
use std::os::unix::process::ExitStatusExt;
use tinyjson::JsonValue;
use std::fs::read_to_string;
use std::collections::HashMap;
use test_log::test;
use m4::error::GetExitCode;

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
    let status: &f64= map.get("status").unwrap().get().unwrap();
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
    log::info!("Running command with input {input:?}:\n\x1b[34m{}\x1b[0m", read_to_string(input).unwrap());
    let mut stdout: Vec<u8> = Vec::new();
    let mut stderr: Vec<u8> = Vec::new();
    let args = m4::Args { file: Some(input.into()), ..m4::Args::default()};
    let result = m4::run(&mut stdout, &mut stderr, args);
    let status = ExitStatus::from_raw(result.get_exit_code() as i32);
    log::info!("Received status: {status}");
    log::info!("Received stdout: {}", String::from_utf8_lossy(&stdout));
    log::info!("Received stderr: {}", String::from_utf8_lossy(&stderr));
    std::process::Output {
        stdout, stderr, status
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
