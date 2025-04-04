//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use chrono::{Datelike, Local, NaiveDate, NaiveDateTime, NaiveTime, Timelike};
use posixutils_cron::job::Database;
use std::io::Write;
use std::ops::Sub;
use std::process::{Command, Output, Stdio};
use std::thread;
use std::time::Duration;

fn run_test_base(cmd: &str, args: &Vec<String>, stdin_data: &[u8]) -> Output {
    let relpath = if cfg!(debug_assertions) {
        format!("target/debug/{}", cmd)
    } else {
        format!("target/release/{}", cmd)
    };
    let test_bin_path = std::env::current_dir()
        .unwrap()
        .parent()
        .unwrap() // Move up to the workspace root from the current package directory
        .join(relpath); // Adjust the path to the binary

    let mut command = Command::new(test_bin_path);
    let mut child = command
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect(format!("failed to spawn command {}", cmd).as_str());

    // Separate the mutable borrow of stdin from the child process
    if let Some(mut stdin) = child.stdin.take() {
        let chunk_size = 1024; // Arbitrary chunk size, adjust if needed
        for chunk in stdin_data.chunks(chunk_size) {
            // Write each chunk
            if let Err(e) = stdin.write_all(chunk) {
                eprintln!("Error writing to stdin: {}", e);
                break;
            }
            // Flush after writing each chunk
            if let Err(e) = stdin.flush() {
                eprintln!("Error flushing stdin: {}", e);
                break;
            }

            // Sleep briefly to avoid CPU spinning
            thread::sleep(Duration::from_millis(10));
        }
        // Explicitly drop stdin to close the pipe
        drop(stdin);
    }

    // Ensure we wait for the process to complete after writing to stdin
    let output = child.wait_with_output().expect("failed to wait for child");
    output
}

#[test]
fn no_args() {
    std::env::set_var("LOGNAME", "root");
    let output = run_test_base("crond", &vec![], b"");
    assert_eq!(output.status.code(), Some(0));
}

#[test]
fn test_leap_year() {
    let database = "* * 29 * * echo Ok".parse::<Database>().unwrap();

    let start_date = NaiveDateTime::new(
        NaiveDate::from_ymd_opt(2028, 1, 30).unwrap(),
        NaiveTime::from_hms_opt(15, 38, 00).unwrap(),
    );

    let expected_date = NaiveDateTime::new(
        NaiveDate::from_ymd_opt(2028, 2, 29).unwrap(),
        NaiveTime::from_hms_opt(00, 00, 0).unwrap(),
    );

    assert_eq!(
        expected_date,
        database
            .nearest_job()
            .unwrap()
            .next_execution(&start_date)
            .unwrap()
    );
}

#[test]
fn test_minute() {
    let database = "10 * * * * echo Ok".parse::<Database>().unwrap();

    let start_date = NaiveDateTime::new(
        NaiveDate::from_ymd_opt(2000, 1, 1).unwrap(),
        NaiveTime::from_hms_opt(15, 38, 00).unwrap(),
    );

    let expected_date = NaiveDateTime::new(
        NaiveDate::from_ymd_opt(2000, 1, 1).unwrap(),
        NaiveTime::from_hms_opt(16, 10, 0).unwrap(),
    );

    assert_eq!(
        expected_date,
        database
            .nearest_job()
            .unwrap()
            .next_execution(&start_date)
            .unwrap()
    );
}

#[test]
fn test_hour() {
    let database = "* 1 * * * echo Ok".parse::<Database>().unwrap();

    let start_date = NaiveDateTime::new(
        NaiveDate::from_ymd_opt(2000, 1, 1).unwrap(),
        NaiveTime::from_hms_opt(15, 38, 00).unwrap(),
    );

    let expected_date = NaiveDateTime::new(
        NaiveDate::from_ymd_opt(2000, 1, 2).unwrap(),
        NaiveTime::from_hms_opt(1, 0, 0).unwrap(),
    );

    assert_eq!(
        expected_date,
        database
            .nearest_job()
            .unwrap()
            .next_execution(&start_date)
            .unwrap()
    );
}

#[test]
fn test_weekday() {
    let database = "* * * * 0 echo Ok".parse::<Database>().unwrap();

    let start_date = NaiveDateTime::new(
        NaiveDate::from_ymd_opt(2000, 2, 1).unwrap(),
        NaiveTime::from_hms_opt(15, 38, 00).unwrap(),
    );

    let expected_date = NaiveDateTime::new(
        NaiveDate::from_ymd_opt(2000, 2, 6).unwrap(),
        NaiveTime::from_hms_opt(00, 00, 0).unwrap(),
    );

    assert_eq!(
        expected_date,
        database
            .nearest_job()
            .unwrap()
            .next_execution(&start_date)
            .unwrap()
    );
}

#[test]
fn test_monthday() {
    let database = "* * 20 * * echo Ok".parse::<Database>().unwrap();

    let start_date = NaiveDateTime::new(
        NaiveDate::from_ymd_opt(2000, 1, 1).unwrap(),
        NaiveTime::from_hms_opt(15, 38, 00).unwrap(),
    );

    let expected_date = NaiveDateTime::new(
        NaiveDate::from_ymd_opt(2000, 1, 20).unwrap(),
        NaiveTime::from_hms_opt(00, 00, 0).unwrap(),
    );

    assert_eq!(
        expected_date,
        database
            .nearest_job()
            .unwrap()
            .next_execution(&start_date)
            .unwrap()
    );
}

#[test]
fn test_month() {
    let database = "* * * 12 * echo Ok".parse::<Database>().unwrap();

    let start_date = NaiveDateTime::new(
        NaiveDate::from_ymd_opt(2000, 1, 1).unwrap(),
        NaiveTime::from_hms_opt(15, 38, 00).unwrap(),
    );

    let expected_date = NaiveDateTime::new(
        NaiveDate::from_ymd_opt(2000, 12, 1).unwrap(),
        NaiveTime::from_hms_opt(00, 00, 0).unwrap(),
    );

    assert_eq!(
        expected_date,
        database
            .nearest_job()
            .unwrap()
            .next_execution(&start_date)
            .unwrap()
    );
}
