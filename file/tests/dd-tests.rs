use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

use plib::{run_test_u8, TestPlanU8};

fn get_test_file_path(filename: &str) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("tests");
    path.push(filename);
    path
}

#[test]
fn test_ascii_to_ebcdic_conversion() {
    let input_file_path = get_test_file_path("dd.ascii");
    let reference_file_path = get_test_file_path("dd.ebcdic");

    let mut input_file = File::open(input_file_path).expect("Unable to open input test file");
    let mut input_data = Vec::new();
    input_file
        .read_to_end(&mut input_data)
        .expect("Unable to read input test file");

    let mut reference_file =
        File::open(reference_file_path).expect("Unable to open reference test file");
    let mut reference_data = Vec::new();
    reference_file
        .read_to_end(&mut reference_data)
        .expect("Unable to read reference test file");

    run_test_u8(TestPlanU8 {
        cmd: String::from("dd"),
        args: vec![String::from("conv=ebcdic")],
        stdin_data: input_data,
        expected_out: reference_data,
        expected_err: Vec::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn test_ebcdic_to_ascii_conversion() {
    let input_file_path = get_test_file_path("dd.ebcdic");
    let reference_file_path = get_test_file_path("dd.ascii");

    let mut input_file = File::open(input_file_path).expect("Unable to open input test file");
    let mut input_data = Vec::new();
    input_file
        .read_to_end(&mut input_data)
        .expect("Unable to read input test file");

    let mut reference_file =
        File::open(reference_file_path).expect("Unable to open reference test file");
    let mut reference_data = Vec::new();
    reference_file
        .read_to_end(&mut reference_data)
        .expect("Unable to read reference test file");

    run_test_u8(TestPlanU8 {
        cmd: String::from("dd"),
        args: vec![String::from("conv=ascii")],
        stdin_data: input_data,
        expected_out: reference_data,
        expected_err: Vec::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn test_ascii_to_ibm_conversion() {
    let input_file_path = get_test_file_path("dd.ascii");
    let reference_file_path = get_test_file_path("dd.ibm");

    let mut input_file = File::open(input_file_path).expect("Unable to open input test file");
    let mut input_data = Vec::new();
    input_file
        .read_to_end(&mut input_data)
        .expect("Unable to read input test file");

    let mut reference_file =
        File::open(reference_file_path).expect("Unable to open reference test file");
    let mut reference_data = Vec::new();
    reference_file
        .read_to_end(&mut reference_data)
        .expect("Unable to read reference test file");

    run_test_u8(TestPlanU8 {
        cmd: String::from("dd"),
        args: vec![String::from("conv=ibm")],
        stdin_data: input_data,
        expected_out: reference_data,
        expected_err: Vec::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn test_ascii_to_swab_conversion() {
    let input_file_path = get_test_file_path("dd.ascii");
    let reference_file_path = get_test_file_path("dd.swab");

    let mut input_file = File::open(input_file_path).expect("Unable to open input test file");
    let mut input_data = Vec::new();
    input_file
        .read_to_end(&mut input_data)
        .expect("Unable to read input test file");

    let mut reference_file =
        File::open(reference_file_path).expect("Unable to open reference test file");
    let mut reference_data = Vec::new();
    reference_file
        .read_to_end(&mut reference_data)
        .expect("Unable to read reference test file");

    run_test_u8(TestPlanU8 {
        cmd: String::from("dd"),
        args: vec![String::from("conv=swab")],
        stdin_data: input_data,
        expected_out: reference_data,
        expected_err: Vec::new(),
        expected_exit_code: 0,
    });
}
