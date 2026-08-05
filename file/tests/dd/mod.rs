//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

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

    // DD-1: conversions are applied in the fixed spec order regardless of how
    // they are listed; swab runs before ucase either way.
    run_test_u8(TestPlanU8 {
        cmd: String::from("dd"),
        args: vec![String::from("conv=swab,ucase")],
        stdin_data: b"abcd".to_vec(),
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

// DD-5: iflags=fullblock accumulates a full ibs-sized block per read,
// so count= counts whole blocks even when the input arrives in short reads.
#[test]
fn test_iflags_fullblock() {
    run_test_u8(TestPlanU8 {
        cmd: String::from("dd"),
        args: vec![
            String::from("ibs=4"),
            String::from("count=1"),
            String::from("iflags=fullblock"),
        ],
        stdin_data: b"abcdefgh".to_vec(),
        expected_out: b"abcd".to_vec(),
        expected_err: b"1+0 records in\n0+1 records out\n".to_vec(),
        expected_exit_code: 0,
    });
}

// --- coverage gaps closed in the file-crate audit sweep ---

/// Run dd with `args`, feeding `stdin_data`, and return (stdout, stderr, code).
fn run_dd(args: &[&str], stdin_data: &[u8]) -> (Vec<u8>, String, Option<i32>) {
    use std::io::Write;
    use std::process::{Command, Stdio};

    let dd = plib::testing::get_binary_path("dd");
    let mut child = Command::new(&dd)
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to spawn dd");
    // A rejected operand makes dd exit before reading stdin, so this write can
    // legitimately fail with EPIPE. That is a result, not a harness failure.
    let _ = child.stdin.as_mut().unwrap().write_all(stdin_data);
    let out = child.wait_with_output().unwrap();
    (
        out.stdout,
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.code(),
    )
}

// DD-1: POSIX fixes the order in which conversions apply (sync, then swab,
// then charset/case, then block/unblock). The order they are *named* in the
// conv= operand must not change the result.
#[test]
fn test_conv_order_independence() {
    // swab then ucase, requested both ways round.
    let data = b"abcd";
    let (a, _, ca) = run_dd(&["conv=swab,ucase"], data);
    let (b, _, cb) = run_dd(&["conv=ucase,swab"], data);
    assert_eq!(ca, Some(0));
    assert_eq!(cb, Some(0));
    assert_eq!(a, b, "conv= order must not affect the result");
    // swab pairs first (badc), then upper-cases: BADC.
    assert_eq!(a, b"BADC");
}

// DD-3: lcase/ucase go through the locale's LC_CTYPE mapping. In a UTF-8
// locale the single-byte high range has no case mapping, so those bytes must
// pass through untouched rather than being mangled; ASCII still maps.
#[test]
fn test_case_conversion_in_utf8_locale() {
    let Some(loc) = plib::testing::utf8_locale() else {
        eprintln!("skipping: no UTF-8 locale installed");
        return;
    };

    use std::io::Write;
    use std::process::{Command, Stdio};
    let dd = plib::testing::get_binary_path("dd");
    let input: Vec<u8> = vec![b'a', b'B', 0xC3, 0xA9, b'z'];

    let mut child = Command::new(&dd)
        .arg("conv=ucase")
        .env("LC_ALL", &loc)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to spawn dd");
    child.stdin.as_mut().unwrap().write_all(&input).unwrap();
    let out = child.wait_with_output().unwrap();

    assert_eq!(out.status.code(), Some(0));
    // ASCII upper-cased; the two UTF-8 continuation bytes untouched.
    assert_eq!(out.stdout, vec![b'A', b'B', 0xC3, 0xA9, b'Z']);
}

// DD-4: skip= on a non-seekable input counts *blocks*, not read() calls. A
// pipe delivers short reads, so a naive implementation skips too little.
#[test]
fn test_skip_accumulates_short_reads_on_pipe() {
    // 4 blocks of 512; skip 2 => the second half comes out.
    let mut input = vec![b'A'; 1024];
    input.extend(vec![b'B'; 1024]);
    let (stdout, _, code) = run_dd(&["ibs=512", "obs=512", "skip=2"], &input);
    assert_eq!(code, Some(0));
    assert_eq!(stdout.len(), 1024);
    assert!(
        stdout.iter().all(|&b| b == b'B'),
        "skip=2 with ibs=512 must drop exactly the first 1024 bytes"
    );
}

// DD-6: the `w` (word) multiplier is not in POSIX -- the word size is not
// portable -- so it must be rejected rather than silently treated as 2.
#[test]
fn test_w_multiplier_rejected() {
    let (_, stderr, code) = run_dd(&["ibs=2w"], b"abcd");
    assert_ne!(code, Some(0), "`w` suffix must be rejected");
    assert!(
        stderr.contains("suffix"),
        "expected a suffix diagnostic, got {stderr:?}"
    );
}

// DD-2: on SIGINT dd writes its statistics and then terminates *as if by the
// default action*, so the parent observes a signal death (WIFSIGNALED), not a
// plain non-zero exit.
#[test]
fn test_sigint_reports_stats_and_signal_death() {
    use std::io::Read;
    use std::os::unix::process::ExitStatusExt;
    use std::process::{Command, Stdio};

    let dd = plib::testing::get_binary_path("dd");
    // Read from a fifo-like endless source so dd is still running when signaled.
    let mut child = Command::new(&dd)
        .args(["if=/dev/zero", "of=/dev/null"])
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to spawn dd");

    // Give it a moment to get into the copy loop, then interrupt.
    std::thread::sleep(std::time::Duration::from_millis(300));
    let pid = child.id() as libc::pid_t;
    assert_eq!(unsafe { libc::kill(pid, libc::SIGINT) }, 0);

    let status = child.wait().expect("wait failed");
    let mut stderr = String::new();
    child
        .stderr
        .as_mut()
        .unwrap()
        .read_to_string(&mut stderr)
        .unwrap();

    assert_eq!(
        status.signal(),
        Some(libc::SIGINT),
        "dd must die by SIGINT, not exit normally (status {status:?})"
    );
    assert!(
        stderr.contains("records in"),
        "statistics must still be written before dying, got {stderr:?}"
    );
}
