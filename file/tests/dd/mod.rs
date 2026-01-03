use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

use plib::testing::{run_test_u8, TestPlanU8};

fn get_test_file_path(filename: &str) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("tests");
    path.push("dd");
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
        expected_err: b"0+1 records in\n0+1 records out\n".to_vec(),
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
        expected_err: b"0+1 records in\n0+1 records out\n".to_vec(),
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
        expected_err: b"0+1 records in\n0+1 records out\n".to_vec(),
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
        expected_err: b"0+1 records in\n0+1 records out\n".to_vec(),
        expected_exit_code: 0,
    });
}

#[test]
fn test_ucase_conversion() {
    let input_file_path = get_test_file_path("dd.ascii");
    let reference_file_path = get_test_file_path("dd.ucase");

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
        args: vec![String::from("conv=ucase")],
        stdin_data: input_data,
        expected_out: reference_data,
        expected_err: b"0+1 records in\n0+1 records out\n".to_vec(),
        expected_exit_code: 0,
    });
}

#[test]
fn test_lcase_conversion() {
    let input_file_path = get_test_file_path("dd.ascii");
    let reference_file_path = get_test_file_path("dd.lcase");

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
        args: vec![String::from("conv=lcase")],
        stdin_data: input_data,
        expected_out: reference_data,
        expected_err: b"0+1 records in\n0+1 records out\n".to_vec(),
        expected_exit_code: 0,
    });
}

#[test]
fn test_skip_n() {
    let input_file_path = get_test_file_path("dd.ascii");

    let mut input_file = File::open(input_file_path).expect("Unable to open input test file");
    let mut input_data = Vec::new();
    input_file
        .read_to_end(&mut input_data)
        .expect("Unable to read input test file");

    let expected_output = b"world! {[ $&chars ]}\nAlas, poor Yorick, I knew him well.\n";

    run_test_u8(TestPlanU8 {
        cmd: String::from("dd"),
        args: vec![
            String::from("ibs=1"),
            String::from("skip=7"), // Skip 7 bytes to reach the correct output
        ],
        stdin_data: input_data,
        expected_out: expected_output.to_vec(),
        expected_err: b"57+0 records in\n0+1 records out\n".to_vec(),
        expected_exit_code: 0,
    });
}

#[test]
fn test_basic_block_processing() {
    let input_file_path = get_test_file_path("dd_test_input.txt");
    let mut input_file = File::open(input_file_path).expect("Unable to open input test file");
    let mut input_data = Vec::new();
    input_file
        .read_to_end(&mut input_data)
        .expect("Unable to read input test file");

    // Expected output is the same as input for this simple block size test
    let expected_output_data = input_data.clone();

    run_test_u8(TestPlanU8 {
        cmd: String::from("dd"),
        args: vec![String::from("ibs=32"), String::from("obs=32")],
        stdin_data: input_data,
        expected_out: expected_output_data,
        expected_err: b"26+0 records in\n26+0 records out\n".to_vec(),
        expected_exit_code: 0,
    });
}

#[test]
fn test_different_ibs_obs() {
    let input_file_path = get_test_file_path("dd_test_input.txt");
    let mut input_file = File::open(input_file_path).expect("Unable to open input test file");
    let mut input_data = Vec::new();
    input_file
        .read_to_end(&mut input_data)
        .expect("Unable to read input test file");

    // Expected output is the combination of each pair of 32-byte blocks from input
    let mut expected_output_data = Vec::new();
    for chunk in input_data.chunks(32) {
        expected_output_data.extend_from_slice(chunk);
    }

    run_test_u8(TestPlanU8 {
        cmd: String::from("dd"),
        args: vec![String::from("ibs=32"), String::from("obs=64")],
        stdin_data: input_data,
        expected_out: expected_output_data,
        expected_err: b"26+0 records in\n13+0 records out\n".to_vec(),
        expected_exit_code: 0,
    });
}

#[test]
fn test_conv_sync() {
    let input_file_path = get_test_file_path("dd.ascii");
    let reference_file_path = get_test_file_path("dd.sync");

    let mut input_file = File::open(input_file_path).expect("Unable to open input test file");
    let mut input_data = Vec::new();
    input_file
        .read_to_end(&mut input_data)
        .expect("Unable to read input test file");

    // Reference data to compare the output
    let mut reference_file =
        File::open(reference_file_path).expect("Unable to open reference test file");
    let mut reference_data = Vec::new();
    reference_file
        .read_to_end(&mut reference_data)
        .expect("Unable to read reference test file");

    run_test_u8(TestPlanU8 {
        cmd: String::from("dd"),
        args: vec![
            String::from("ibs=512"),
            String::from("obs=512"),
            String::from("conv=sync"),
        ],
        stdin_data: input_data,
        expected_out: reference_data,
        expected_err: b"0+1 records in\n1+0 records out\n".to_vec(),
        expected_exit_code: 0,
    });
}

#[test]
fn test_conv_block() {
    let input_file_path = get_test_file_path("dd.ascii");
    let reference_file_path = get_test_file_path("dd.block");

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
        args: vec![String::from("conv=block"), String::from("cbs=16")],
        stdin_data: input_data,
        expected_out: reference_data,
        expected_err: b"0+1 records in\n0+1 records out\n2 truncated records\n".to_vec(),
        expected_exit_code: 0,
    });
}

// New tests for POSIX compliance

#[test]
fn test_conv_unblock() {
    // dd.block contains two 16-byte fixed-length records:
    // "Hello, world! {[" and "Alas, poor Yoric"
    // Unblock removes trailing spaces and adds newlines
    let input_file_path = get_test_file_path("dd.block");

    let mut input_file = File::open(input_file_path).expect("Unable to open input test file");
    let mut input_data = Vec::new();
    input_file
        .read_to_end(&mut input_data)
        .expect("Unable to read input test file");

    // Expected: each 16-byte record gets trailing spaces stripped and newline added
    // Record 1: "Hello, world! {[" -> no trailing spaces -> "Hello, world! {[\n"
    // Record 2: "Alas, poor Yoric" -> no trailing spaces -> "Alas, poor Yoric\n"
    let expected_output = b"Hello, world! {[\nAlas, poor Yoric\n";

    run_test_u8(TestPlanU8 {
        cmd: String::from("dd"),
        args: vec![String::from("conv=unblock"), String::from("cbs=16")],
        stdin_data: input_data,
        expected_out: expected_output.to_vec(),
        expected_err: b"0+1 records in\n0+1 records out\n".to_vec(),
        expected_exit_code: 0,
    });
}

#[test]
fn test_count_operand() {
    let input_data = b"AAAABBBBCCCCDDDDEEEE";

    run_test_u8(TestPlanU8 {
        cmd: String::from("dd"),
        args: vec![String::from("ibs=4"), String::from("count=2")],
        stdin_data: input_data.to_vec(),
        expected_out: b"AAAABBBB".to_vec(),
        expected_err: b"2+0 records in\n0+1 records out\n".to_vec(),
        expected_exit_code: 0,
    });
}

#[test]
fn test_x_multiplier() {
    // Test that bs=2x4 gives 8-byte blocks
    let input_data = b"12345678ABCDEFGH";

    run_test_u8(TestPlanU8 {
        cmd: String::from("dd"),
        args: vec![String::from("bs=2x4")],
        stdin_data: input_data.to_vec(),
        expected_out: input_data.to_vec(),
        expected_err: b"2+0 records in\n2+0 records out\n".to_vec(),
        expected_exit_code: 0,
    });
}

#[test]
fn test_x_multiplier_with_suffix() {
    // Test that ibs=2x1k gives 2048-byte input blocks
    // Note: stdin may fragment reads, so stderr counts may vary
    let input_data = vec![b'A'; 2048];

    run_test_u8(TestPlanU8 {
        cmd: String::from("dd"),
        args: vec![String::from("ibs=2x1k"), String::from("obs=2x1k")],
        stdin_data: input_data.clone(),
        expected_out: input_data,
        // stdin fragments the read: 2 partial input blocks aggregated to 1 full output
        expected_err: b"0+2 records in\n1+0 records out\n".to_vec(),
        expected_exit_code: 0,
    });
}

#[test]
fn test_combined_conversions() {
    // Test conv=ucase,swab together
    let input_data = b"abcd";

    run_test_u8(TestPlanU8 {
        cmd: String::from("dd"),
        args: vec![String::from("conv=ucase,swab")],
        stdin_data: input_data.to_vec(),
        expected_out: b"BADC".to_vec(),
        expected_err: b"0+1 records in\n0+1 records out\n".to_vec(),
        expected_exit_code: 0,
    });
}

#[test]
fn test_sync_with_block() {
    // Test that sync pads with spaces when block is also specified
    // Input: "test\n" (5 bytes)
    // After sync (ibs=10, with spaces because block is specified): "test\n     " (10 bytes)
    // After block (cbs=8):
    //   - "test" (line before newline) -> padded to 8: "test    "
    //   - "     " (5 spaces after newline, EOF-terminated) -> padded to 8: "        "
    // Total: 16 bytes
    let input_data = b"test\n";

    run_test_u8(TestPlanU8 {
        cmd: String::from("dd"),
        args: vec![
            String::from("ibs=10"),
            String::from("conv=sync,block"),
            String::from("cbs=8"),
        ],
        stdin_data: input_data.to_vec(),
        expected_out: b"test            ".to_vec(), // 4 chars + 4 spaces + 8 spaces = 16 bytes
        expected_err: b"0+1 records in\n0+1 records out\n".to_vec(),
        expected_exit_code: 0,
    });
}
