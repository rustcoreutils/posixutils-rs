//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! procps' dashless BSD options `a`, `u` and `x`, as in `ps aux`.  Headers
//! and column layout are procps-ng 4's.

use plib::testing::get_binary_path;
use std::process::{Command, Output};

const U_HEADER: &str = "USER         PID %CPU %MEM    VSZ   RSS TTY      STAT START   TIME COMMAND";
const DEFAULT_HEADER: &str = "    PID TTY      STAT   TIME COMMAND";

fn ps(args: &[&str]) -> Output {
    Command::new(get_binary_path("ps"))
        .args(args)
        .output()
        .expect("run ps")
}

/// Standard output of a successful run, as lines.
fn ok_lines(args: &[&str]) -> Vec<String> {
    let out = ps(args);
    assert_eq!(out.status.code(), Some(0), "ps {args:?}: {out:?}");
    assert!(out.stderr.is_empty(), "ps {args:?}: {out:?}");
    String::from_utf8_lossy(&out.stdout)
        .lines()
        .map(str::to_string)
        .collect()
}

/// The USER column for the user running the tests: procps shows a name of
/// more than 8 bytes as its first 7 and a `+`.
fn my_user_column() -> String {
    let name = plib::user::get_by_uid(unsafe { libc::geteuid() })
        .expect("the test user has a passwd entry")
        .name
        .to_string_lossy()
        .into_owned();
    if name.len() > 8 {
        format!("{}+", &name[..7])
    } else {
        name
    }
}

/// The row whose PID column (field `pid_field`) is `pid`.
fn row_for(lines: &[String], pid_field: usize, pid: u32) -> Option<&String> {
    let pid = pid.to_string();
    lines[1..]
        .iter()
        .find(|l| l.split_whitespace().nth(pid_field) == Some(pid.as_str()))
}

/// Another user's process is listed: pid 1 (init) on Linux.  macOS may not
/// let an unprivileged process read launchd's details, so it is not checked
/// there.
fn lists_init(lines: &[String], pid_field: usize) -> bool {
    !cfg!(target_os = "linux") || row_for(lines, pid_field, 1).is_some()
}

/// A procps percentage: an integer above 99.9, otherwise one decimal.
fn is_percent(s: &str) -> bool {
    match s.split_once('.') {
        Some((i, d)) => !i.is_empty() && i.bytes().all(|b| b.is_ascii_digit()) && d.len() == 1,
        None => !s.is_empty() && s.bytes().all(|b| b.is_ascii_digit()),
    }
}

/// A BSD TIME value: minutes, `:`, two-digit seconds.
fn is_bsd_time(s: &str) -> bool {
    match s.split_once(':') {
        Some((m, sec)) => {
            !m.is_empty()
                && m.bytes().all(|b| b.is_ascii_digit())
                && sec.len() == 2
                && sec.bytes().all(|b| b.is_ascii_digit())
        }
        None => false,
    }
}

/// Every row of a `u` listing has the eleven columns in procps' layout.
fn check_u_rows(lines: &[String]) {
    assert_eq!(lines[0], U_HEADER);
    for line in &lines[1..] {
        let f: Vec<&str> = line.split_whitespace().collect();
        assert!(f.len() >= 11, "short row: {line:?}");
        assert!(f[1].parse::<u32>().is_ok(), "PID: {line:?}");
        assert!(is_percent(f[2]) && is_percent(f[3]), "%CPU/%MEM: {line:?}");
        assert!(f[4].parse::<u64>().is_ok(), "VSZ: {line:?}");
        assert!(f[5].parse::<u64>().is_ok(), "RSS: {line:?}");
        assert!(
            f[7].chars().next().unwrap().is_ascii_uppercase(),
            "STAT: {line:?}"
        );
        assert!(is_bsd_time(f[9]), "TIME: {line:?}");
    }
}

#[test]
fn aux_lists_every_process_in_user_format() {
    let lines = ok_lines(&["aux"]);
    check_u_rows(&lines);
    assert!(lists_init(&lines, 1), "no pid 1");

    // This test's own row: USER is left-justified in 8 columns and PID
    // right-justified in the 7 after the separating blank.
    let me = std::process::id();
    let row = row_for(&lines, 1, me).expect("own row");
    let user = my_user_column();
    assert_eq!(row.split_whitespace().next(), Some(user.as_str()));
    if user.len() <= 8 {
        assert_eq!(&row[..9], format!("{user:<8} "));
        assert_eq!(&row[9..16], format!("{me:>7}"));
    }
}

#[test]
fn letters_combine_in_any_order_and_word() {
    for args in [&["xua"][..], &["uax"], &["a", "u", "x"], &["ux", "a"]] {
        let lines = ok_lines(args);
        check_u_rows(&lines);
        assert!(lists_init(&lines, 1), "{args:?}: no pid 1");
    }
}

#[test]
fn ax_uses_the_bsd_default_format() {
    let lines = ok_lines(&["ax"]);
    assert_eq!(lines[0], DEFAULT_HEADER);
    let me = std::process::id();
    let row = row_for(&lines, 0, me).expect("own row");
    assert_eq!(&row[..7], format!("{me:>7}"));
    let f: Vec<&str> = row.split_whitespace().collect();
    assert!(is_bsd_time(f[3]), "TIME: {row:?}");
    assert!(lists_init(&lines, 0), "no pid 1");
}

// `x` alone: the invoker's own processes, with a terminal or without.
#[test]
fn ux_selects_own_processes() {
    let lines = ok_lines(&["ux"]);
    check_u_rows(&lines);
    let user = my_user_column();
    for line in &lines[1..] {
        assert_eq!(
            line.split_whitespace().next(),
            Some(user.as_str()),
            "{line:?}"
        );
    }
    assert!(
        row_for(&lines, 1, std::process::id()).is_some(),
        "no own row"
    );
}

// Without `x`, only processes with a terminal; without `a`, only the
// invoker's own.
#[test]
fn a_and_u_need_a_terminal() {
    let user = my_user_column();
    for (args, own_only) in [(&["a"][..], false), (&["au"], false), (&["u"], true)] {
        let lines = ok_lines(args);
        let tty_field = if args[0].contains('u') { 6 } else { 1 };
        for line in &lines[1..] {
            let f: Vec<&str> = line.split_whitespace().collect();
            assert_ne!(f[tty_field], "?", "{args:?}: {line:?}");
            if own_only {
                assert_eq!(f[0], user, "{args:?}: {line:?}");
            }
        }
    }
}

// A BSD word cannot be mixed with dash options; a word with any other
// letter is not a BSD word and is refused as an operand, as before.
#[test]
fn bsd_words_do_not_mix() {
    let out = ps(&["aux", "-f"]);
    assert_eq!(out.status.code(), Some(1), "{out:?}");
    assert_eq!(
        String::from_utf8_lossy(&out.stderr),
        "ps: BSD-style options cannot be combined with other arguments\n"
    );
    assert!(out.stdout.is_empty());

    // Not a BSD word, and not a process ID operand either.
    let out = ps(&["auxf"]);
    assert_eq!(out.status.code(), Some(1), "{out:?}");
    assert_eq!(
        String::from_utf8_lossy(&out.stderr),
        "ps: invalid number: auxf\n"
    );
    assert!(out.stdout.is_empty());
}
