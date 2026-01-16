//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

mod pid;

use chrono::{NaiveDate, NaiveDateTime, NaiveTime};
use cron::job::Database;
use std::io::Write;
use std::process::{Command, Output, Stdio};
use std::str::FromStr;
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
        .unwrap_or_else(|_| panic!("failed to spawn command {}", cmd));

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

    child.wait_with_output().expect("failed to wait for child")
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

#[test]
fn test_signal() {
    std::env::set_var("LOGNAME", "root");

    let logname = std::env::var("LOGNAME").unwrap_or("root".to_string());
    #[cfg(target_os = "linux")]
    let file = format!("/var/spool/cron/{logname}");
    #[cfg(target_os = "macos")]
    let file = format!("/var/at/tabs/{logname}");

    let output = run_test_base("crond", &vec![], b"");
    assert_eq!(output.status.code(), Some(0));

    let pids = pid::get_pids("target/debug/crond").unwrap();

    if std::path::PathBuf::from_str(&file).unwrap().exists() {
        assert!(!pids.is_empty());
    }

    for pid in &pids {
        unsafe {
            libc::kill(*pid, libc::SIGHUP);
        }
    }

    let mut old_pids = pids;
    let mut pids = pid::get_pids("target/debug/crond").unwrap();

    pids.sort();
    old_pids.sort();
    assert!(pids == old_pids || !pids.is_empty());

    pid::kill("target/debug/crond").unwrap();
}

// Tests for @-prefix special time specifications

#[test]
fn test_at_hourly() {
    let database = "@hourly echo test".parse::<Database>().unwrap();
    assert_eq!(database.0.len(), 1);
    let job = &database.0[0];
    assert!(!job.is_reboot);

    // @hourly means "0 * * * *" - at minute 0 of every hour
    let start_date = NaiveDateTime::new(
        NaiveDate::from_ymd_opt(2000, 1, 1).unwrap(),
        NaiveTime::from_hms_opt(15, 30, 00).unwrap(),
    );

    let expected_date = NaiveDateTime::new(
        NaiveDate::from_ymd_opt(2000, 1, 1).unwrap(),
        NaiveTime::from_hms_opt(16, 0, 0).unwrap(),
    );

    assert_eq!(expected_date, job.next_execution(&start_date).unwrap());
}

#[test]
fn test_at_daily() {
    let database = "@daily echo test".parse::<Database>().unwrap();
    assert_eq!(database.0.len(), 1);
    let job = &database.0[0];
    assert!(!job.is_reboot);

    // @daily means "0 0 * * *" - at midnight every day
    let start_date = NaiveDateTime::new(
        NaiveDate::from_ymd_opt(2000, 1, 1).unwrap(),
        NaiveTime::from_hms_opt(15, 30, 00).unwrap(),
    );

    let expected_date = NaiveDateTime::new(
        NaiveDate::from_ymd_opt(2000, 1, 2).unwrap(),
        NaiveTime::from_hms_opt(0, 0, 0).unwrap(),
    );

    assert_eq!(expected_date, job.next_execution(&start_date).unwrap());
}

#[test]
fn test_at_weekly() {
    let database = "@weekly echo test".parse::<Database>().unwrap();
    assert_eq!(database.0.len(), 1);
    let job = &database.0[0];
    assert!(!job.is_reboot);

    // @weekly means "0 0 * * 0" - at midnight on Sunday
    // 2000-01-01 is Saturday, so next Sunday is 2000-01-02
    let start_date = NaiveDateTime::new(
        NaiveDate::from_ymd_opt(2000, 1, 1).unwrap(),
        NaiveTime::from_hms_opt(15, 30, 00).unwrap(),
    );

    let expected_date = NaiveDateTime::new(
        NaiveDate::from_ymd_opt(2000, 1, 2).unwrap(),
        NaiveTime::from_hms_opt(0, 0, 0).unwrap(),
    );

    assert_eq!(expected_date, job.next_execution(&start_date).unwrap());
}

#[test]
fn test_at_monthly() {
    let database = "@monthly echo test".parse::<Database>().unwrap();
    assert_eq!(database.0.len(), 1);
    let job = &database.0[0];
    assert!(!job.is_reboot);

    // @monthly means "0 0 1 * *" - at midnight on the 1st of each month
    let start_date = NaiveDateTime::new(
        NaiveDate::from_ymd_opt(2000, 1, 15).unwrap(),
        NaiveTime::from_hms_opt(15, 30, 00).unwrap(),
    );

    let expected_date = NaiveDateTime::new(
        NaiveDate::from_ymd_opt(2000, 2, 1).unwrap(),
        NaiveTime::from_hms_opt(0, 0, 0).unwrap(),
    );

    assert_eq!(expected_date, job.next_execution(&start_date).unwrap());
}

#[test]
fn test_at_yearly() {
    let database = "@yearly echo test".parse::<Database>().unwrap();
    assert_eq!(database.0.len(), 1);
    let job = &database.0[0];
    assert!(!job.is_reboot);

    // @yearly means "0 0 1 1 *" - at midnight on Jan 1
    let start_date = NaiveDateTime::new(
        NaiveDate::from_ymd_opt(2000, 3, 15).unwrap(),
        NaiveTime::from_hms_opt(15, 30, 00).unwrap(),
    );

    let expected_date = NaiveDateTime::new(
        NaiveDate::from_ymd_opt(2001, 1, 1).unwrap(),
        NaiveTime::from_hms_opt(0, 0, 0).unwrap(),
    );

    assert_eq!(expected_date, job.next_execution(&start_date).unwrap());
}

#[test]
fn test_at_reboot() {
    let database = "@reboot echo test".parse::<Database>().unwrap();
    assert_eq!(database.0.len(), 1);
    let job = &database.0[0];
    assert!(job.is_reboot);
    assert_eq!(job.command, "echo test");

    // @reboot jobs should not have a next execution time
    let start_date = NaiveDateTime::new(
        NaiveDate::from_ymd_opt(2000, 1, 1).unwrap(),
        NaiveTime::from_hms_opt(15, 30, 00).unwrap(),
    );
    assert!(job.next_execution(&start_date).is_none());
}

#[test]
fn test_reboot_jobs_filtered() {
    let database = "@reboot echo reboot\n* * * * * echo regular"
        .parse::<Database>()
        .unwrap();
    assert_eq!(database.0.len(), 2);

    // reboot_jobs() should return only @reboot jobs
    let reboot_jobs = database.reboot_jobs();
    assert_eq!(reboot_jobs.len(), 1);
    assert!(reboot_jobs[0].is_reboot);
    assert_eq!(reboot_jobs[0].command, "echo reboot");

    // nearest_job() should only return non-reboot jobs
    let nearest = database.nearest_job();
    assert!(nearest.is_some());
    assert!(!nearest.unwrap().is_reboot);
}

// Tests for step parsing

#[test]
fn test_step_every_15_minutes() {
    let database = "*/15 * * * * echo test".parse::<Database>().unwrap();
    assert_eq!(database.0.len(), 1);

    // */15 should trigger at minutes 0, 15, 30, 45
    let start_date = NaiveDateTime::new(
        NaiveDate::from_ymd_opt(2000, 1, 1).unwrap(),
        NaiveTime::from_hms_opt(12, 16, 00).unwrap(),
    );

    let expected_date = NaiveDateTime::new(
        NaiveDate::from_ymd_opt(2000, 1, 1).unwrap(),
        NaiveTime::from_hms_opt(12, 30, 0).unwrap(),
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
fn test_range_with_step() {
    let database = "0-30/10 * * * * echo test".parse::<Database>().unwrap();
    assert_eq!(database.0.len(), 1);

    // 0-30/10 should trigger at minutes 0, 10, 20, 30
    let start_date = NaiveDateTime::new(
        NaiveDate::from_ymd_opt(2000, 1, 1).unwrap(),
        NaiveTime::from_hms_opt(12, 11, 00).unwrap(),
    );

    let expected_date = NaiveDateTime::new(
        NaiveDate::from_ymd_opt(2000, 1, 1).unwrap(),
        NaiveTime::from_hms_opt(12, 20, 0).unwrap(),
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
fn test_list_of_values() {
    let database = "0,15,30,45 * * * * echo test".parse::<Database>().unwrap();
    assert_eq!(database.0.len(), 1);

    // 0,15,30,45 should trigger at those specific minutes
    let start_date = NaiveDateTime::new(
        NaiveDate::from_ymd_opt(2000, 1, 1).unwrap(),
        NaiveTime::from_hms_opt(12, 16, 00).unwrap(),
    );

    let expected_date = NaiveDateTime::new(
        NaiveDate::from_ymd_opt(2000, 1, 1).unwrap(),
        NaiveTime::from_hms_opt(12, 30, 0).unwrap(),
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

// Test system crontab parsing (6-field format)

#[test]
fn test_system_crontab_format() {
    let content = "0 * * * * root echo hello";
    let database = Database::parse_system_crontab(content);
    assert_eq!(database.0.len(), 1);
    let job = &database.0[0];
    assert_eq!(job.command, "echo hello");
    // On test systems, root user should exist
    assert!(job.owner_uid.is_some() || job.owner_name.is_none());
}

#[test]
fn test_system_crontab_skips_env_vars() {
    let content = "SHELL=/bin/bash\nPATH=/usr/bin\n0 * * * * root echo hello";
    let database = Database::parse_system_crontab(content);
    // Should only have 1 job, not parse env vars as jobs
    assert_eq!(database.0.len(), 1);
    assert_eq!(database.0[0].command, "echo hello");
}

#[test]
fn test_system_crontab_at_prefix() {
    let content = "@hourly root echo hourly";
    let database = Database::parse_system_crontab(content);
    assert_eq!(database.0.len(), 1);
    let job = &database.0[0];
    assert!(!job.is_reboot);
    assert_eq!(job.command, "echo hourly");
}
