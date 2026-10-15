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
    wide_makefile_of(0..count, secs)
}

/// The same with the prerequisites `t<i>` for each `i` in `range`.
fn wide_makefile_of(range: std::ops::Range<usize>, secs: &str) -> String {
    let names: Vec<String> = range.map(|i| format!("t{i}")).collect();
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

// POSIX 105378-105384: `-j` is passed to a sub-make in MAKEFLAGS, and the
// sub-make uses the same token pool. Two sub-makes of 24 half-second jobs
// each at -j8 take 3 s, never running more than 8 jobs between them. The
// sub-makes used to run with no -j at all: 12 s.
#[test]
fn sub_makes_share_the_job_slots() {
    let dir = Dir::new("parallel_sub_makes");
    dir.write(
        "Makefile",
        "all: s1 s2\ns1:\n\t@$(MAKE) -f sub1.mk\ns2:\n\t@$(MAKE) -f sub2.mk\n",
    );
    dir.write("sub1.mk", &wide_makefile_of(0..24, "0.5"));
    dir.write("sub2.mk", &wide_makefile_of(24..48, "0.5"));
    std::fs::create_dir(dir.path("running")).unwrap();
    let (code, took) = make_within(&dir, &["-j8"], Duration::from_secs(60));
    assert_eq!(code, Some(0));
    let (max, made) = concurrency(&dir, 48);
    assert_eq!(made, 48, "every target is made");
    assert!(max <= 8, "{max} jobs ran at once under -j8");
    assert!(
        took < Duration::from_secs(8),
        "48 half-second jobs in two sub-makes took {took:?} under -j8"
    );
}

// What a sub-make finds in MAKEFLAGS: the job count and the pool, in GNU
// make's spelling, and nothing of either without -j.
#[test]
fn makeflags_carries_the_job_count_to_sub_makes() {
    let dir = Dir::new("parallel_makeflags");
    dir.write("Makefile", "all:\n\t@$(MAKE) -f sub.mk\n");
    dir.write("sub.mk", "all:\n\t@echo \"[$$MAKEFLAGS]\"\n");
    let run = dir.make(&["-s", "-j3"]);
    assert_eq!(run.code, Some(0), "{run:?}");
    assert!(
        run.stdout.starts_with("[s -j3 --jobserver-auth=") && run.stdout.ends_with("]\n"),
        "{run:?}"
    );
    dir.make(&["-s"]).ok("[s]\n");
    dir.make(&["-s", "-j1"]).ok("[s]\n");
}

// A sub-make given its own -j makes a pool of its own, as GNU make does.
#[test]
fn a_sub_make_with_its_own_j() {
    let dir = Dir::new("parallel_own_j");
    dir.write("Makefile", "all:\n\t@$(MAKE) -j2 -f sub.mk\n");
    dir.write("sub.mk", &wide_makefile(6, "0"));
    std::fs::create_dir(dir.path("running")).unwrap();
    let (code, _) = make_within(&dir, &["-j4"], Duration::from_secs(60));
    assert_eq!(code, Some(0));
    let (max, made) = concurrency(&dir, 6);
    assert_eq!(made, 6);
    assert!(max <= 2, "{max} jobs ran at once under the sub-make's -j2");
}

// A make a command line starts without `$(MAKE)` or `+` is not given the
// pool, though MAKEFLAGS names it: it says so and runs one job at a time,
// as GNU make does.
#[test]
fn a_make_not_run_as_a_sub_make_runs_serially() {
    let dir = Dir::new("parallel_not_sub_make");
    dir.write("Makefile", "all:\n\t@\"$$MAKE_BINARY\" -f sub.mk\n");
    dir.write("sub.mk", &wide_makefile(4, "0"));
    std::fs::create_dir(dir.path("running")).unwrap();
    let binary = get_binary_path("make");
    let run = dir.make_env(&["-j4"], &[("MAKE_BINARY", binary.to_str().unwrap())]);
    assert_eq!(run.code, Some(0), "{run:?}");
    assert_eq!(
        run.stderr,
        "make: warning: jobserver unavailable: using -j1.  Add '+' to parent make rule.\n"
    );
    let (max, made) = concurrency(&dir, 4);
    assert_eq!((max, made), (1, 4));
}

// The targets named on the command line are made in parallel too, as GNU
// make makes them: automake's check-am runs `$(MAKE) $(check_PROGRAMS)`,
// hundreds of test programs as operands. 24 half-second goals at -j8 take
// 1.5 s; made one after another they took 12 s.
#[test]
fn command_line_targets_share_the_job_slots() {
    let dir = Dir::new("parallel_goals");
    dir.write("Makefile", &wide_makefile(24, "0.5"));
    std::fs::create_dir(dir.path("running")).unwrap();
    let goals: Vec<String> = (0..24).map(|i| format!("t{i}")).collect();
    let mut args = vec!["-j8"];
    args.extend(goals.iter().map(String::as_str));
    let (code, took) = make_within(&dir, &args, Duration::from_secs(60));
    assert_eq!(code, Some(0));
    let (max, made) = concurrency(&dir, 24);
    assert_eq!(made, 24, "every target is made");
    assert!(max <= 8, "{max} jobs ran at once under -j8");
    assert!(
        took < Duration::from_secs(6),
        "24 half-second goals took {took:?} under -j8"
    );
}

// Made in parallel, each goal still gets its own message when it needed
// nothing done, as from GNU make.
#[test]
fn messages_for_goals_made_in_parallel() {
    let dir = Dir::new("parallel_goal_messages");
    dir.write("Makefile", "a:\n\ttouch a\nb:\n\ttouch b\nc:\n");
    dir.write("b", "");
    dir.make(&["-j3", "a", "b", "c"])
        .ok("touch a\nmake: 'b' is up to date.\nmake: Nothing to be done for 'c'.\n");
    dir.make(&["-j3", "a", "b", "c"]).ok(
        "make: 'a' is up to date.\nmake: 'b' is up to date.\nmake: Nothing to be done for 'c'.\n",
    );
}

// A goal another goal's walk remade first is not "up to date": its command
// lines count to it, as they would have made one after another, and as GNU
// make counts them under -j. `make -j4 a b` with `b: a` said
// `make: 'a' is up to date.` after `built a`, or not, by which thread
// reached `a` first. Each order is made several times to catch either
// winner.
#[test]
fn a_goal_another_goal_remade_is_not_up_to_date() {
    let dir = Dir::new("parallel_goal_remade_by_another");
    dir.write("Makefile", "a b:\n\t@echo built $@\nb: a\nall: a\n");
    for _ in 0..5 {
        dir.make(&["-j4", "a", "b"]).ok("built a\nbuilt b\n");
        dir.make(&["-j4", "b", "a"])
            .ok("built a\nbuilt b\nmake: 'a' is up to date.\n");
        dir.make(&["-j4", "a", "all"])
            .ok("built a\nmake: Nothing to be done for 'all'.\n");
        dir.make(&["-j4", "all", "a"])
            .ok("built a\nmake: 'a' is up to date.\n");
    }
}
