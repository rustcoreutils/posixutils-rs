//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! `-j maxjobs` (POSIX 105356-105386): up to maxjobs targets are updated at
//! once for the whole run, not only for the first few prerequisites of a
//! target. automake's `check-TESTS` has one target with hundreds of `.log`
//! prerequisites, and libtool libraries one with hundreds of objects.

use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

use plib::testing::get_binary_path;

use crate::common::Dir;

/// A makefile whose `all` has `count` prerequisites, each taking `secs`
/// seconds. Each job records how many jobs are running when it starts, in
/// `max.<target>`.
fn wide_makefile(count: usize, secs: &str) -> String {
    let names: Vec<String> = (0..count).map(|i| format!("t{i}")).collect();
    let mut text = format!("all: {}\n", names.join(" "));
    for name in &names {
        text.push_str(&format!(
            "{name}:\n\t@touch running/$@; ls running | wc -l >max.$@; sleep {secs}; rm running/$@; touch $@\n"
        ));
    }
    text
}

/// Run make in `dir` with `args`, killed after `limit`: a scheduler that
/// stalls fails the test instead of hanging it. Returns the exit status and
/// how long it took.
fn make_within(dir: &Dir, args: &[&str], limit: Duration) -> (Option<i32>, Duration) {
    let start = Instant::now();
    let mut child = Command::new(get_binary_path("make"))
        .args(args)
        .current_dir(dir.path(""))
        .env_remove("MAKEFLAGS")
        .env_remove("MAKELEVEL")
        .stdout(Stdio::null())
        .spawn()
        .expect("run make");
    loop {
        if let Some(status) = child.try_wait().expect("wait for make") {
            return (status.code(), start.elapsed());
        }
        if start.elapsed() > limit {
            let _ = child.kill();
            let _ = child.wait();
            panic!("make {args:?} still running after {limit:?}");
        }
        std::thread::sleep(Duration::from_millis(20));
    }
}

/// The largest number of jobs any job saw running, and how many ran.
fn concurrency(dir: &Dir, count: usize) -> (usize, usize) {
    let mut max = 0;
    let mut made = 0;
    for i in 0..count {
        if dir.path(&format!("t{i}")).exists() {
            made += 1;
        }
        if let Ok(text) = std::fs::read_to_string(dir.path(&format!("max.t{i}"))) {
            max = max.max(text.trim().parse::<usize>().unwrap_or(0));
        }
    }
    (max, made)
}

// 32 half-second jobs at -j8 take 2 s. The scheduler used to start 7
// workers for the first 7 prerequisites and make the other 25 one after
// another in its own thread, never reusing a worker's token: 13 s.
#[test]
fn every_prerequisite_shares_the_job_slots() {
    let dir = Dir::new("parallel_wide");
    dir.write("Makefile", &wide_makefile(32, "0.5"));
    std::fs::create_dir(dir.path("running")).unwrap();
    let (code, took) = make_within(&dir, &["-j8"], Duration::from_secs(60));
    assert_eq!(code, Some(0));
    let (max, made) = concurrency(&dir, 32);
    assert_eq!(made, 32, "every target is made");
    assert!(max <= 8, "{max} jobs ran at once under -j8");
    assert!(
        took < Duration::from_secs(8),
        "32 half-second jobs took {took:?} under -j8"
    );
}

// A failure under -j: with -k every other prerequisite is still made; without
// it make fails too, and the target that needed it is not made. GNU make
// agrees.
#[test]
fn a_failed_job_under_j() {
    let dir = Dir::new("parallel_failure");
    let names: Vec<String> = (0..40).map(|i| format!("t{i}")).collect();
    let mut text = format!("all: {}\n\t@touch $@\n", names.join(" "));
    for name in &names {
        let command = if name == "t3" { "false" } else { "touch $@" };
        text.push_str(&format!("{name}:\n\t@{command}\n"));
    }
    dir.write("Makefile", &text);
    let run = dir.make(&["-k", "-j4"]);
    assert_eq!(run.code, Some(2), "{run:?}");
    let made = names.iter().filter(|n| dir.path(n).exists()).count();
    assert_eq!(made, 39, "{run:?}");
    assert!(!dir.path("all").exists());
    for name in &names {
        let _ = std::fs::remove_file(dir.path(name));
    }
    let run = dir.make(&["-j4"]);
    assert_eq!(run.code, Some(2), "{run:?}");
    assert!(!dir.path("all").exists());
}
