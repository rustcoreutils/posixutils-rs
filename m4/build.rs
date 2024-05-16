use std::{
    collections::HashMap,
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
    let output = std::process::Command::new("cargo")
        .arg("run")
        .arg("--")
        .arg("{input}")
        .output()
        .unwrap();

        let expected_output: ExpectedOutput = serde_json::from_str(&std::fs::read_to_string("{output_json}").unwrap()).unwrap();
        assert_eq!(String::from_utf8(output.stdout).unwrap(), expected_output.stdout);
        assert_eq!(String::from_utf8(output.stderr).unwrap(), expected_output.stderr);
        assert_eq!(output.status, std::process::ExitStatus::from_raw(expected_output.status));
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
    let mut test_candidates: HashMap<String, TestCandidate> = HashMap::new();
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
use std::os::unix::process::ExitStatusExt;

#[derive(serde::Deserialize)]
struct ExpectedOutput {
    stdout: String,
    stderr: String,
    status: i32,
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
