//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

mod pid;

use chrono::{NaiveDate, NaiveDateTime, NaiveTime};
use cron::job::Database;
use plib::testing::{get_binary_path, run_test_base};
use std::process::Output;
use std::str::FromStr;

fn run_crond_test(cmd: &str, args: &Vec<String>, stdin_data: &[u8]) -> Output {
    run_test_base(cmd, args, stdin_data)
}

#[test]
fn no_args() {
    std::env::set_var("LOGNAME", "root");
    let output = run_crond_test("crond", &vec![], b"");
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

    let output = run_crond_test("crond", &vec![], b"");
    assert_eq!(output.status.code(), Some(0));

    // Get the binary path string for process matching
    let bin_path = get_binary_path("crond");
    let bin_path_str = bin_path.to_string_lossy();

    let pids = pid::get_pids(&bin_path_str).unwrap();

    if std::path::PathBuf::from_str(&file).unwrap().exists() {
        assert!(!pids.is_empty());
    }

    for pid in &pids {
        unsafe {
            libc::kill(*pid, libc::SIGHUP);
        }
    }

    let mut old_pids = pids;
    let mut pids = pid::get_pids(&bin_path_str).unwrap();

    pids.sort();
    old_pids.sort();
    assert!(pids == old_pids || !pids.is_empty());

    pid::kill(&bin_path_str).unwrap();
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

// Secure-execution helpers (audit #D4/#D5).

#[test]
fn command_field_without_percent() {
    let p = cron::job::parse_command_field("echo hello");
    assert_eq!(p.exec_line, "echo hello");
    assert!(p.stdin.is_none());
}

#[test]
fn command_field_percent_is_stdin_with_newlines() {
    // POSIX crontab example: only the first line runs; the rest is stdin, and
    // each `%` becomes a newline.
    let p = cron::job::parse_command_field("mailx john%Happy Birthday!%Time for lunch.");
    assert_eq!(p.exec_line, "mailx john");
    assert_eq!(p.stdin.as_deref(), Some("Happy Birthday!\nTime for lunch."));
}

#[test]
fn command_field_backslash_escapes_percent() {
    let p = cron::job::parse_command_field(r"echo 100\% done");
    assert_eq!(p.exec_line, "echo 100% done");
    assert!(p.stdin.is_none());
}

#[test]
fn job_env_defaults_and_safe_overrides() {
    let overrides = vec![
        ("PATH".to_string(), "/custom/bin".to_string()),
        ("LOGNAME".to_string(), "evil".to_string()),
        ("MAILTO".to_string(), "someone".to_string()),
    ];
    let env = cron::job::build_job_env("alice", "/home/alice", &overrides);
    let get = |k: &str| env.iter().find(|(ek, _)| ek == k).map(|(_, v)| v.clone());
    assert_eq!(get("HOME").as_deref(), Some("/home/alice"));
    assert_eq!(get("USER").as_deref(), Some("alice"));
    assert_eq!(get("SHELL").as_deref(), Some("/bin/sh"));
    // A crontab PATH override is honored...
    assert_eq!(get("PATH").as_deref(), Some("/custom/bin"));
    // ...but LOGNAME stays authoritative and MAILTO is not exported.
    assert_eq!(get("LOGNAME").as_deref(), Some("alice"));
    assert!(get("MAILTO").is_none());
}

#[test]
fn matches_minute_exact_field() {
    let db = "30 4 * * * echo".parse::<Database>().unwrap();
    let job = &db.0[0];
    let yes = NaiveDateTime::new(
        NaiveDate::from_ymd_opt(2024, 1, 1).unwrap(),
        NaiveTime::from_hms_opt(4, 30, 0).unwrap(),
    );
    let no = NaiveDateTime::new(
        NaiveDate::from_ymd_opt(2024, 1, 1).unwrap(),
        NaiveTime::from_hms_opt(4, 31, 0).unwrap(),
    );
    assert!(job.matches_minute(&yes));
    assert!(!job.matches_minute(&no));
}

#[test]
fn matches_minute_dom_dow_union() {
    // "0 0 1,15 * 1" fires on the 1st, the 15th, OR any Monday (POSIX union).
    let db = "0 0 1,15 * 1 echo".parse::<Database>().unwrap();
    let job = &db.0[0];
    let midnight = |y, m, d| {
        NaiveDateTime::new(
            NaiveDate::from_ymd_opt(y, m, d).unwrap(),
            NaiveTime::from_hms_opt(0, 0, 0).unwrap(),
        )
    };
    assert!(job.matches_minute(&midnight(2000, 1, 3))); // Monday
    assert!(job.matches_minute(&midnight(2000, 1, 1))); // 1st (a Saturday)
    assert!(!job.matches_minute(&midnight(2000, 1, 4))); // Tue, not 1/15
}

#[test]
fn user_crontab_collects_env_assignments() {
    let db = "MAILTO=ops\nPATH=/sbin\n* * * * * echo hi"
        .parse::<Database>()
        .unwrap();
    assert_eq!(db.0.len(), 1);
    assert!(db.0[0].env.iter().any(|(k, v)| k == "MAILTO" && v == "ops"));
    assert!(db.0[0].env.iter().any(|(k, v)| k == "PATH" && v == "/sbin"));
}
