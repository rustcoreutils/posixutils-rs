use plib::{run_test, TestPlan};
use std::io::Write;
use tempfile::NamedTempFile;

fn join_test(file1_data: &str, file2_data: &str, expected_output: &str, args: Vec<&str>) {
    let file1 = NamedTempFile::new().expect("Failed to create temp file");
    let file2 = NamedTempFile::new().expect("Failed to create temp file");

    {
        let mut file1_handle = file1.as_file();
        let mut file2_handle = file2.as_file();

        write!(file1_handle, "{}", file1_data).expect("Failed to write to file1");
        write!(file2_handle, "{}", file2_data).expect("Failed to write to file2");
    }

    let args: Vec<String> = args
        .into_iter()
        .map(String::from)
        .chain(vec![
            file1.path().to_str().unwrap().to_string(),
            file2.path().to_str().unwrap().to_string(),
        ])
        .collect();

    run_test(TestPlan {
        cmd: String::from("join"),
        args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn test_join_basic() {
    join_test(
        "1 a\n2 b\n3 c\n",
        "1 x\n2 y\n4 z\n",
        "1 a x\n2 b y\n",
        vec!["-t", " "],
    );
}
