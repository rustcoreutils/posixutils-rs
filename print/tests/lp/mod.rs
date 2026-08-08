//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test_with_checker_and_env, run_test_with_env, TestPlan};
use std::fs;
use tempfile::TempDir;

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

/// Decode a chunked transfer-encoding body out of `raw`, refilling from
/// `stream` until the terminating zero-length chunk arrives.
///
/// Returns `None` if the peer hangs up before the body is complete.
fn read_chunked(stream: &mut TcpStream, raw: &mut Vec<u8>, mut pos: usize) -> Option<Vec<u8>> {
    let mut chunk = [0u8; 4096];
    let mut body = Vec::new();

    loop {
        // Refill until the chunk-size line terminator is in hand.
        let line_end = loop {
            if let Some(off) = raw[pos..].windows(2).position(|w| w == b"\r\n") {
                break pos + off;
            }
            match stream.read(&mut chunk) {
                Ok(0) | Err(_) => return None,
                Ok(n) => raw.extend_from_slice(&chunk[..n]),
            }
        };

        // "<hex-size>[;chunk-ext]" — the extension is not used by ureq, but
        // ignoring it costs one split.
        let line = String::from_utf8_lossy(&raw[pos..line_end]);
        let size = usize::from_str_radix(line.split(';').next()?.trim(), 16).ok()?;
        pos = line_end + 2;

        // A zero-length chunk ends the body; trailers are ignored.
        if size == 0 {
            return Some(body);
        }

        // Refill until the whole chunk plus its trailing CRLF has arrived.
        while raw.len() < pos + size + 2 {
            match stream.read(&mut chunk) {
                Ok(0) | Err(_) => return None,
                Ok(n) => raw.extend_from_slice(&chunk[..n]),
            }
        }
        body.extend_from_slice(&raw[pos..pos + size]);
        pos += size + 2;
    }
}

/// Read one HTTP request and reply with `body`.
///
/// The body has to be consumed in full before replying: `lp`'s IPP client
/// streams the print payload, and answering early makes the server close the
/// connection while the client is still writing, which the client reports as
/// "Peer disconnected". That is a race, so it showed up as an intermittent
/// failure rather than a consistent one.
fn serve_one(mut stream: TcpStream, job_id: i32) {
    let mut buf = Vec::new();
    let mut chunk = [0u8; 4096];

    // Read the headers, then the body framed the way they say it is.
    let (header_end, content_length, chunked) = loop {
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
            // ureq (via the ipp crate) streams the payload, so the request
            // arrives chunked with no Content-Length at all.
            let chunked = headers
                .lines()
                .filter_map(|l| l.strip_prefix("transfer-encoding:"))
                .any(|v| v.contains("chunked"));
            break (pos + 4, len, chunked);
        }
    };

    let body = if chunked {
        match read_chunked(&mut stream, &mut buf, header_end) {
            Some(body) => body,
            None => return,
        }
    } else {
        while buf.len() < header_end + content_length {
            match stream.read(&mut chunk) {
                Ok(0) | Err(_) => break,
                Ok(n) => buf.extend_from_slice(&chunk[..n]),
            }
        }
        buf[header_end..].to_vec()
    };

    // The request-id is bytes 4..8 of the IPP body; echo it back.
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
            assert_eq!(
                output.status.code(),
                Some(0),
                "stderr={:?}",
                String::from_utf8_lossy(&output.stderr)
            );
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
            assert_eq!(
                output.status.code(),
                Some(0),
                "stderr={:?}",
                String::from_utf8_lossy(&output.stderr)
            );
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

// ============================================================================
// -m and -w: the notifications themselves, not just that the option parses
//
// `lp_m_option_accepted` and `lp_w_option_accepted` used to assert only that
// the flag was accepted and that the connection then failed, which says nothing
// about whether a completion mail or terminal write ever happens. The IPP stub
// answers `job-state = 9` (completed) on its first poll, so `wait_for_job`
// returns immediately and the notification path runs for real. Each submitted
// job adds one Get-Job-Attributes request on top of the Print-Job, so the stub
// must serve two.
// ============================================================================

/// `-m`: "mail sent to the user after the files have been printed"
/// (lp DESCRIPTION). `LP_SENDMAIL` points at a recorder so the message can be
/// read back; without it the only way to observe this is a real MTA.
#[test]
fn lp_m_mails_the_user_when_the_job_completes() {
    let Some((uri, handle)) = spawn_ipp_stub(4321, 2) else {
        return; // cannot bind a port in this environment
    };
    let dir = TempDir::new().unwrap();
    let recorded = dir.path().join("mail.txt");
    let recorder = dir.path().join("fake-sendmail");
    fs::write(
        &recorder,
        format!("#!/bin/sh\nexec cat > {}\n", recorded.to_string_lossy()),
    )
    .unwrap();
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        fs::set_permissions(&recorder, fs::Permissions::from_mode(0o755)).unwrap();
    }

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![String::from("-m"), String::from("-d"), uri.clone()],
            stdin_data: String::from("job payload"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[
            ("LPDEST", ""),
            ("PRINTER", ""),
            ("LP_SENDMAIL", recorder.to_str().unwrap()),
        ],
        |_, output| {
            assert_eq!(
                output.status.code(),
                Some(0),
                "stderr={:?}",
                String::from_utf8_lossy(&output.stderr)
            );
        },
    );

    let _ = handle.join();

    let message = fs::read_to_string(&recorded)
        .expect("-m must invoke the mail transport after the job completes");
    assert!(
        message.contains("To: "),
        "expected an addressed message, got {message:?}"
    );
    assert!(
        message.contains(&format!("{}-4321", uri)),
        "the mail body must name the request ID of the completed job, got {message:?}"
    );
}

/// `-m` with `LP_SENDMAIL` pointing at nothing still succeeds: the mail is
/// best-effort and must not fail the print job.
#[test]
fn lp_m_failure_to_mail_does_not_fail_the_job() {
    let Some((uri, handle)) = spawn_ipp_stub(99, 2) else {
        return;
    };

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![String::from("-m"), String::from("-d"), uri.clone()],
            stdin_data: String::from("job payload"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[
            ("LPDEST", ""),
            ("PRINTER", ""),
            ("LP_SENDMAIL", "/nonexistent/sendmail"),
        ],
        |_, output| {
            assert_eq!(
                output.status.code(),
                Some(0),
                "an undeliverable notification must not fail the job; stderr={:?}",
                String::from_utf8_lossy(&output.stderr)
            );
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.is_empty(),
                "a best-effort mail failure must stay silent, got {stderr:?}"
            );
        },
    );

    let _ = handle.join();
}

/// `-w`: "write a message on the user's terminal after the files have been
/// printed". The message goes to `/dev/tty`, so `lp` has to run on a
/// pseudo-terminal for it to be observable at all.
#[test]
#[cfg(target_os = "linux")]
fn lp_w_writes_to_the_terminal_when_the_job_completes() {
    let Some((uri, handle)) = spawn_ipp_stub(555, 2) else {
        return;
    };

    let pty = match portable_pty::native_pty_system().openpty(portable_pty::PtySize {
        rows: 24,
        cols: 80,
        pixel_width: 0,
        pixel_height: 0,
    }) {
        Ok(p) => p,
        Err(_) => {
            println!("Skipping: no PTY available");
            let _ = handle.join();
            return;
        }
    };

    let mut cmd = portable_pty::CommandBuilder::new(plib::testing::get_binary_path("lp"));
    cmd.arg("-w");
    cmd.arg("-d");
    cmd.arg(&uri);
    cmd.env("LPDEST", "");
    cmd.env("PRINTER", "");
    cmd.env("TERM", "vt100");
    // With no file operands lp reads the job from standard input, which is the
    // terminal here, so give it one instead.
    let dir = TempDir::new().unwrap();
    let job = dir.path().join("job.txt");
    fs::write(&job, b"job payload").unwrap();
    cmd.arg(&job);

    let mut child = pty.slave.spawn_command(cmd).unwrap();
    drop(pty.slave);

    let mut reader = pty.master.try_clone_reader().unwrap();
    let seen = std::sync::Arc::new(std::sync::Mutex::new(String::new()));
    let sink = std::sync::Arc::clone(&seen);
    std::thread::spawn(move || {
        let mut buf = [0u8; 4096];
        loop {
            match std::io::Read::read(&mut reader, &mut buf) {
                Ok(0) | Err(_) => break,
                Ok(n) => sink
                    .lock()
                    .unwrap()
                    .push_str(&String::from_utf8_lossy(&buf[..n])),
            }
        }
    });

    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(15);
    while std::time::Instant::now() < deadline {
        if child.try_wait().ok().flatten().is_some() {
            break;
        }
        std::thread::sleep(std::time::Duration::from_millis(50));
    }
    let _ = child.kill();
    let _ = child.wait();
    let _ = handle.join();
    std::thread::sleep(std::time::Duration::from_millis(200));

    let text = seen.lock().unwrap().clone();
    assert!(
        text.contains("print job completed"),
        "-w must write a completion message to the terminal, saw {text:?}"
    );
    assert!(
        text.contains(&format!("{}-555", uri)),
        "the terminal message must name the request ID, saw {text:?}"
    );
}
