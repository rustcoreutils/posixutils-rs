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
    expected_output: Option<PathBuf>,
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
            expected_output: value
                .expected_output
                .ok_or("No expected_output provided")?
                .to_str()
                .ok_or("Error converting expected_output path to string")?
                .to_owned(),
        })
    }
}

struct Test {
    name: String,
    input: String,
    expected_output: String,
}

impl Test {
    fn as_code(&self) -> String {
        let Self {
            name,
            input,
            expected_output,
        } = self;
        format!(
            r##"#[test]
fn test_{name}() {{
    let output = String::from_utf8(std::process::Command::new("cargo")
        .arg("run")
        .arg("--")
        .arg("{input}")
        .output()
        .unwrap()
        .stdout).unwrap();
    
    let expected_output = std::fs::read_to_string("{expected_output}").unwrap();
    assert_eq!(output, expected_output);
}}"##
        )
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
            Some(b"stdout") => {
                let name = name_from_path(&path).unwrap();
                let candidate = test_candidates
                    .entry(name.clone())
                    .or_insert(TestCandidate {
                        name,
                        ..TestCandidate::default()
                    });
                candidate.expected_output = Some(path);
            }
            _ => eprintln!("Ignoring file {path:?}"),
        }
    }
    let mut integration_test: String =
        r#"//! NOTE: This file has been auto generated using build.rs, don't edit by hand!
use similar_asserts::assert_eq;
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
