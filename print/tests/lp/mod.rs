//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test_with_checker_and_env, run_test_with_env, TestPlan};

/// Test that lp fails when no destination is specified
#[test]
fn lp_no_destination_error() {
    // Use empty strings to clear the environment variables in the subprocess
    run_test_with_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![],
            stdin_data: String::from("test data"),
            expected_out: String::from(""),
            expected_err: String::from("lp: no destination specified\n"),
            expected_exit_code: 1,
        },
        &[("LPDEST", ""), ("PRINTER", "")],
    );
}

/// Test that a destination name containing a path separator is rejected
#[test]
fn lp_invalid_name_rejected() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec!["-d".to_string(), "bad/name".to_string()],
            stdin_data: String::from("test data"),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 1,
        },
        &[("LPDEST", ""), ("PRINTER", "")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("invalid destination name"),
                "Expected invalid name error, got: {}",
                stderr
            );
            assert_eq!(output.status.code(), Some(1));
        },
    );
}

/// Test that a bare printer name resolves to a localhost IPP URI
#[test]
fn lp_bare_name_resolves_to_localhost() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec!["-d".to_string(), "myprinter".to_string()],
            stdin_data: String::from("test data"),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 1,
        },
        &[("LPDEST", ""), ("PRINTER", "")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // The bare name is resolved, not rejected; the connection then fails.
            assert!(
                !stderr.contains("invalid destination"),
                "Expected bare name to resolve, got: {}",
                stderr
            );
            assert!(
                stderr.contains("printer error")
                    && stderr.contains("ipp://localhost/printers/myprinter"),
                "Expected printer error against resolved localhost URI, got: {}",
                stderr
            );
            assert_eq!(output.status.code(), Some(1));
        },
    );
}

/// Test that -m option is accepted (stub implementation)
#[test]
fn lp_m_option_accepted() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![
                "-m".to_string(),
                "-d".to_string(),
                "ipp://localhost/ipp/print".to_string(),
            ],
            stdin_data: String::from("test data"),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 1,
        },
        &[("LPDEST", ""), ("PRINTER", "")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // -m should be accepted; any failure should be printer error, not option error
            assert!(
                !stderr.contains("-m option not supported"),
                "Expected -m to be accepted, but got: {}",
                stderr
            );
            assert!(
                stderr.contains("printer error"),
                "Expected printer error (no printer available), got: {}",
                stderr
            );
        },
    );
}

/// Test that -w option is accepted (stub implementation)
#[test]
fn lp_w_option_accepted() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![
                "-w".to_string(),
                "-d".to_string(),
                "ipp://localhost/ipp/print".to_string(),
            ],
            stdin_data: String::from("test data"),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 1,
        },
        &[("LPDEST", ""), ("PRINTER", "")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // -w should be accepted; any failure should be printer error, not option error
            assert!(
                !stderr.contains("-w option not supported"),
                "Expected -w to be accepted, but got: {}",
                stderr
            );
            assert!(
                stderr.contains("printer error"),
                "Expected printer error (no printer available), got: {}",
                stderr
            );
        },
    );
}

/// Test that lp fails when file does not exist
#[test]
fn lp_file_not_found() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![
                "-d".to_string(),
                "ipp://localhost/ipp/print".to_string(),
                "/nonexistent/file/path.txt".to_string(),
            ],
            stdin_data: String::from(""),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 1,
        },
        &[("LPDEST", ""), ("PRINTER", "")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("cannot open") && stderr.contains("/nonexistent/file/path.txt"),
                "Expected cannot open error, got: {}",
                stderr
            );
            assert_eq!(output.status.code(), Some(1));
        },
    );
}

/// Test that a per-file error does not abort the remaining operands
#[test]
fn lp_multifile_continues_on_error() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![
                "-d".to_string(),
                "ipp://localhost/ipp/print".to_string(),
                "/nonexistent/file/path.txt".to_string(),
                "-".to_string(),
            ],
            stdin_data: String::from("stdin test data"),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 1,
        },
        &[("LPDEST", ""), ("PRINTER", "")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // First operand fails to open; the loop must STILL process the
            // second operand ('-' stdin), which then hits the printer error.
            assert!(
                stderr.contains("cannot open") && stderr.contains("/nonexistent/file/path.txt"),
                "Expected cannot-open diagnostic for first operand, got: {}",
                stderr
            );
            assert!(
                stderr.contains("printer error"),
                "Expected second operand to still be attempted (printer error), got: {}",
                stderr
            );
            assert_eq!(output.status.code(), Some(1));
        },
    );
}

/// Test LPDEST environment variable is used when -d is not specified
#[test]
fn lp_lpdest_env_used() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![],
            stdin_data: String::from("test data"),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 1,
        },
        &[("LPDEST", "lpdest-printer"), ("PRINTER", "")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // LPDEST is read and resolved to a localhost URI carrying its name.
            assert!(
                stderr.contains("printer error") && stderr.contains("lpdest-printer"),
                "Expected printer error referencing LPDEST name, got: {}",
                stderr
            );
            assert_eq!(output.status.code(), Some(1));
        },
    );
}

/// Test PRINTER environment variable is used when -d and LPDEST are not set
#[test]
fn lp_printer_env_used() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![],
            stdin_data: String::from("test data"),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 1,
        },
        &[("LPDEST", ""), ("PRINTER", "printer-name")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // PRINTER is read and resolved to a localhost URI carrying its name.
            assert!(
                stderr.contains("printer error") && stderr.contains("printer-name"),
                "Expected printer error referencing PRINTER name, got: {}",
                stderr
            );
            assert_eq!(output.status.code(), Some(1));
        },
    );
}

/// Test that -d takes precedence over LPDEST
#[test]
fn lp_d_overrides_lpdest() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec!["-d".to_string(), "dprinter".to_string()],
            stdin_data: String::from("test data"),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 1,
        },
        &[
            ("LPDEST", "ipp://should-not-be-used/ipp/print"),
            ("PRINTER", ""),
        ],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // -d wins: the resolved URI carries the -d name, not the LPDEST host.
            assert!(
                stderr.contains("dprinter") && !stderr.contains("should-not-be-used"),
                "Expected -d to take precedence over LPDEST, got: {}",
                stderr
            );
            assert_eq!(output.status.code(), Some(1));
        },
    );
}

/// Test that -c option is accepted (copy mode)
#[test]
fn lp_c_option_accepted() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![
                "-c".to_string(),
                "-d".to_string(),
                "ipp://localhost/ipp/print".to_string(),
            ],
            stdin_data: String::from("test data"),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 1,
        },
        &[("LPDEST", ""), ("PRINTER", "")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // -c should be accepted; any failure should be printer error, not option error
            assert!(
                !stderr.contains("error: unexpected argument"),
                "Expected -c to be accepted, but got: {}",
                stderr
            );
            assert!(
                stderr.contains("printer error"),
                "Expected printer error (no printer available), got: {}",
                stderr
            );
        },
    );
}

/// Test that -n copies with valid value is accepted
#[test]
fn lp_n_copies_accepted() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![
                "-n".to_string(),
                "5".to_string(),
                "-d".to_string(),
                "ipp://localhost/ipp/print".to_string(),
            ],
            stdin_data: String::from("test data"),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 1,
        },
        &[("LPDEST", ""), ("PRINTER", "")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // -n 5 should be accepted; any failure should be printer error, not argument error
            assert!(
                !stderr.contains("error: invalid value"),
                "Expected -n 5 to be accepted, but got: {}",
                stderr
            );
            assert!(
                stderr.contains("printer error"),
                "Expected printer error (no printer available), got: {}",
                stderr
            );
        },
    );
}

/// Test that -n 0 is rejected by clap validation
#[test]
fn lp_n_zero_rejected() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![
                "-n".to_string(),
                "0".to_string(),
                "-d".to_string(),
                "ipp://localhost/ipp/print".to_string(),
            ],
            stdin_data: String::from("test data"),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 2,
        },
        &[("LPDEST", ""), ("PRINTER", "")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // -n 0 should be rejected with an argument error, not printer error
            assert!(
                stderr.contains("error:") && stderr.contains("0"),
                "Expected argument validation error for -n 0, got: {}",
                stderr
            );
        },
    );
}

/// Test that -n above i32::MAX is rejected by clap validation
#[test]
fn lp_n_copies_overflow_rejected() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![
                "-n".to_string(),
                "3000000000".to_string(),
                "-d".to_string(),
                "ipp://localhost/ipp/print".to_string(),
            ],
            stdin_data: String::from("test data"),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 2,
        },
        &[("LPDEST", ""), ("PRINTER", "")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // Above i32::MAX must be rejected with a validation error, not sent.
            assert!(
                stderr.contains("error:"),
                "Expected validation error for -n 3000000000, got: {}",
                stderr
            );
            assert_eq!(output.status.code(), Some(2));
        },
    );
}

/// `-n i32::MAX` is the largest accepted value and must survive the u32->i32
/// conversion on the way into the IPP `copies` attribute. Paired with
/// `lp_n_copies_overflow_rejected` above, this pins both sides of the boundary.
#[test]
fn lp_n_copies_at_i32_max_accepted() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![
                "-n".to_string(),
                i32::MAX.to_string(),
                "-d".to_string(),
                "ipp://localhost/ipp/print".to_string(),
            ],
            stdin_data: String::from("test data"),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 1,
        },
        &[("LPDEST", ""), ("PRINTER", "")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // Accepted by clap, so the failure must come from the connection,
            // not from argument validation.
            assert!(
                !stderr.contains("error: invalid value"),
                "Expected -n {} to be accepted, got: {}",
                i32::MAX,
                stderr
            );
            assert_eq!(output.status.code(), Some(1));
        },
    );
}

/// Test that a malformed -o option (no '=') produces a warning
#[test]
fn lp_o_malformed_warned() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![
                "-o".to_string(),
                "noequals".to_string(),
                "-d".to_string(),
                "ipp://localhost/ipp/print".to_string(),
            ],
            stdin_data: String::from("test data"),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 1,
        },
        &[("LPDEST", ""), ("PRINTER", "")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("malformed -o option") && stderr.contains("noequals"),
                "Expected malformed -o warning, got: {}",
                stderr
            );
        },
    );
}

/// Test that -t title option is accepted
#[test]
fn lp_t_title_accepted() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![
                "-t".to_string(),
                "My Print Job".to_string(),
                "-d".to_string(),
                "ipp://localhost/ipp/print".to_string(),
            ],
            stdin_data: String::from("test data"),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 1,
        },
        &[("LPDEST", ""), ("PRINTER", "")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // -t should be accepted; any failure should be printer error
            assert!(
                !stderr.contains("error: unexpected argument"),
                "Expected -t to be accepted, but got: {}",
                stderr
            );
            assert!(
                stderr.contains("printer error"),
                "Expected printer error (no printer available), got: {}",
                stderr
            );
        },
    );
}

/// Test that single -o option is accepted
#[test]
fn lp_o_option_single() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![
                "-o".to_string(),
                "media=letter".to_string(),
                "-d".to_string(),
                "ipp://localhost/ipp/print".to_string(),
            ],
            stdin_data: String::from("test data"),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 1,
        },
        &[("LPDEST", ""), ("PRINTER", "")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // -o should be accepted; any failure should be printer error
            assert!(
                !stderr.contains("error: unexpected argument"),
                "Expected -o to be accepted, but got: {}",
                stderr
            );
            assert!(
                stderr.contains("printer error"),
                "Expected printer error (no printer available), got: {}",
                stderr
            );
        },
    );
}

/// Test that multiple -o options are accepted
#[test]
fn lp_o_option_multiple() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![
                "-o".to_string(),
                "media=letter".to_string(),
                "-o".to_string(),
                "sides=two-sided-long-edge".to_string(),
                "-d".to_string(),
                "ipp://localhost/ipp/print".to_string(),
            ],
            stdin_data: String::from("test data"),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 1,
        },
        &[("LPDEST", ""), ("PRINTER", "")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // Multiple -o should be accepted; any failure should be printer error
            assert!(
                !stderr.contains("error: unexpected argument"),
                "Expected multiple -o to be accepted, but got: {}",
                stderr
            );
            assert!(
                stderr.contains("printer error"),
                "Expected printer error (no printer available), got: {}",
                stderr
            );
        },
    );
}

/// Test that combined options work together
#[test]
fn lp_combined_options() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![
                "-c".to_string(),
                "-m".to_string(),
                "-w".to_string(),
                "-s".to_string(),
                "-n".to_string(),
                "3".to_string(),
                "-t".to_string(),
                "Test Title".to_string(),
                "-o".to_string(),
                "media=a4".to_string(),
                "-d".to_string(),
                "ipp://localhost/ipp/print".to_string(),
            ],
            stdin_data: String::from("test data"),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 1,
        },
        &[("LPDEST", ""), ("PRINTER", "")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // All options should be accepted together; failure should be printer error
            assert!(
                !stderr.contains("error: unexpected argument"),
                "Expected all options to be accepted, but got: {}",
                stderr
            );
            assert!(
                stderr.contains("printer error"),
                "Expected printer error (no printer available), got: {}",
                stderr
            );
        },
    );
}

/// Test that LPDEST takes precedence over PRINTER
#[test]
fn lp_lpdest_over_printer() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![],
            stdin_data: String::from("test data"),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 1,
        },
        &[("LPDEST", "lpdest-value"), ("PRINTER", "printer-value")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // LPDEST wins: the resolved URI carries the LPDEST name, not PRINTER.
            assert!(
                stderr.contains("lpdest-value") && !stderr.contains("printer-value"),
                "Expected LPDEST to take precedence over PRINTER, got: {}",
                stderr
            );
            assert_eq!(output.status.code(), Some(1));
        },
    );
}

/// Test that empty LPDEST falls back to PRINTER
#[test]
fn lp_empty_lpdest_fallback() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![],
            stdin_data: String::from("test data"),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 1,
        },
        &[("LPDEST", ""), ("PRINTER", "printer-fallback")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // LPDEST is empty, so PRINTER is used and resolved to a localhost URI.
            assert!(
                stderr.contains("printer error") && stderr.contains("printer-fallback"),
                "Expected printer error referencing PRINTER fallback name, got: {}",
                stderr
            );
            assert_eq!(output.status.code(), Some(1));
        },
    );
}

/// Test that stdin is handled when '-' is passed as file argument
#[test]
fn lp_stdin_dash_argument() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![
                "-d".to_string(),
                "ipp://localhost/ipp/print".to_string(),
                "-".to_string(),
            ],
            stdin_data: String::from("stdin test data"),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 1,
        },
        &[("LPDEST", ""), ("PRINTER", "")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // '-' should be accepted as stdin; failure should be printer error
            assert!(
                !stderr.contains("cannot open"),
                "Expected '-' to be handled as stdin, but got: {}",
                stderr
            );
            assert!(
                stderr.contains("printer error"),
                "Expected printer error (no printer available), got: {}",
                stderr
            );
        },
    );
}

// ===========================================================================
// Stub IPP responder
//
// Every other test in this file stops at the connection failure, which is why
// no success-path behavior was asserted at all. This is the smallest server
// that lp will accept a job from: it speaks just enough IPP-over-HTTP
// (RFC 8010) to answer Print-Job with a job-id.
// ===========================================================================

use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};

/// Assemble an IPP response body granting the job the given id.
fn ipp_ok_response(request_id: u32, job_id: i32) -> Vec<u8> {
    fn put_attr(buf: &mut Vec<u8>, tag: u8, name: &str, value: &[u8]) {
        buf.push(tag);
        buf.extend_from_slice(&(name.len() as u16).to_be_bytes());
        buf.extend_from_slice(name.as_bytes());
        buf.extend_from_slice(&(value.len() as u16).to_be_bytes());
        buf.extend_from_slice(value);
    }

    let mut b = Vec::new();
    b.extend_from_slice(&[0x01, 0x01]); // version 1.1
    b.extend_from_slice(&0u16.to_be_bytes()); // successful-ok
    b.extend_from_slice(&request_id.to_be_bytes());

    b.push(0x01); // operation-attributes-tag
    put_attr(&mut b, 0x47, "attributes-charset", b"utf-8");
    put_attr(&mut b, 0x48, "attributes-natural-language", b"en-us");

    b.push(0x02); // job-attributes-tag
    put_attr(&mut b, 0x21, "job-id", &job_id.to_be_bytes()); // integer
    put_attr(&mut b, 0x23, "job-state", &9i32.to_be_bytes()); // enum: completed

    b.push(0x03); // end-of-attributes-tag
    b
}

/// Read one HTTP request and reply with `body`.
fn serve_one(mut stream: TcpStream, job_id: i32) {
    let mut buf = Vec::new();
    let mut chunk = [0u8; 4096];

    // Read headers, then exactly Content-Length bytes of body.
    let (header_end, content_length) = loop {
        let n = match stream.read(&mut chunk) {
            Ok(0) | Err(_) => return,
            Ok(n) => n,
        };
        buf.extend_from_slice(&chunk[..n]);
        if let Some(pos) = buf.windows(4).position(|w| w == b"\r\n\r\n") {
            let headers = String::from_utf8_lossy(&buf[..pos]).to_lowercase();
            let len = headers
                .lines()
                .find_map(|l| l.strip_prefix("content-length:"))
                .and_then(|v| v.trim().parse::<usize>().ok())
                .unwrap_or(0);
            break (pos + 4, len);
        }
    };
    while buf.len() < header_end + content_length {
        match stream.read(&mut chunk) {
            Ok(0) | Err(_) => break,
            Ok(n) => buf.extend_from_slice(&chunk[..n]),
        }
    }

    // The request-id is bytes 4..8 of the IPP body; echo it back.
    let body = &buf[header_end..];
    let request_id = if body.len() >= 8 {
        u32::from_be_bytes([body[4], body[5], body[6], body[7]])
    } else {
        1
    };

    let ipp = ipp_ok_response(request_id, job_id);
    let mut resp = format!(
        "HTTP/1.1 200 OK\r\nContent-Type: application/ipp\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
        ipp.len()
    )
    .into_bytes();
    resp.extend_from_slice(&ipp);
    let _ = stream.write_all(&resp);
    let _ = stream.flush();
}

/// Bind an ephemeral port and serve `count` requests on a background thread.
/// Returns the `ipp://` URI to point lp at, or `None` if binding failed.
fn spawn_ipp_stub(job_id: i32, count: usize) -> Option<(String, std::thread::JoinHandle<()>)> {
    let listener = TcpListener::bind("127.0.0.1:0").ok()?;
    let port = listener.local_addr().ok()?.port();

    let handle = std::thread::spawn(move || {
        for _ in 0..count {
            match listener.accept() {
                Ok((stream, _)) => serve_one(stream, job_id),
                Err(_) => break,
            }
        }
    });

    Some((format!("ipp://127.0.0.1:{}/printers/stub", port), handle))
}

/// A job the printer accepts writes "request id is <dest>-<job-id>" to stdout
/// (POSIX 103065, 103150-103152) and exits 0.
#[test]
fn lp_successful_job_prints_request_id() {
    let Some((uri, handle)) = spawn_ipp_stub(4242, 1) else {
        return; // cannot bind a port in this environment
    };

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![String::from("-d"), uri.clone()],
            stdin_data: String::from("job payload"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("LPDEST", ""), ("PRINTER", "")],
        |_, output| {
            assert_eq!(
                output.status.code(),
                Some(0),
                "a job the printer accepted must exit 0; stderr={:?}",
                String::from_utf8_lossy(&output.stderr)
            );
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert_eq!(stdout, format!("request id is {}-4242\n", uri));
        },
    );

    let _ = handle.join();
}

/// `-s` suppresses the request-ID line on success — the case that could not be
/// reached while every test stopped at the connection failure.
#[test]
fn lp_silent_suppresses_the_request_id_on_success() {
    let Some((uri, handle)) = spawn_ipp_stub(77, 1) else {
        return;
    };

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![String::from("-s"), String::from("-d"), uri.clone()],
            stdin_data: String::from("job payload"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("LPDEST", ""), ("PRINTER", "")],
        |_, output| {
            assert_eq!(output.status.code(), Some(0));
            assert!(
                output.stdout.is_empty(),
                "-s must suppress the request ID, got {:?}",
                String::from_utf8_lossy(&output.stdout)
            );
        },
    );

    let _ = handle.join();
}

/// Each accepted operand gets its own request ID line, in operand order.
#[test]
fn lp_multiple_files_each_report_a_request_id() {
    let Some((uri, handle)) = spawn_ipp_stub(5, 2) else {
        return;
    };

    let td = std::env::temp_dir().join(format!("posixutils_lp_multi_{}", std::process::id()));
    std::fs::create_dir_all(&td).unwrap();
    let a = td.join("a.txt");
    let b = td.join("b.txt");
    std::fs::write(&a, b"alpha").unwrap();
    std::fs::write(&b, b"beta").unwrap();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![
                String::from("-d"),
                uri.clone(),
                a.to_str().unwrap().to_string(),
                b.to_str().unwrap().to_string(),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("LPDEST", ""), ("PRINTER", "")],
        |_, output| {
            assert_eq!(output.status.code(), Some(0));
            let stdout = String::from_utf8_lossy(&output.stdout);
            let lines: Vec<&str> = stdout.lines().collect();
            assert_eq!(lines.len(), 2, "one request ID per operand, got {stdout:?}");
            for line in lines {
                assert_eq!(line, format!("request id is {}-5", uri));
            }
        },
    );

    let _ = handle.join();
    let _ = std::fs::remove_dir_all(&td);
}
