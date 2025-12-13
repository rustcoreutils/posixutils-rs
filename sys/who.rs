//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// TODO:
// - implement -f option (requires updates to utmpx module)
//

use std::os::unix::fs::MetadataExt;
use std::path::PathBuf;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::utmpx::Utmpx;
use plib::{curuser, platform, utmpx};

/// who - display who is on the system
#[derive(Parser)]
#[command(version, about = gettext("who - display who is on the system"))]
struct Args {
    /// Process all utmpx entries
    #[arg(short, long)]
    all: bool,

    /// Print the time and date of the last reboot.
    #[arg(short, long)]
    boot: bool,

    /// List all processes that have expired and not been respawned.
    #[arg(short, long)]
    dead: bool,

    /// Print column headings above the regular output.
    #[arg(short = 'H', long)]
    heading: bool,

    /// List only those lines on which the system is waiting for someone to login.
    #[arg(short, long)]
    login: bool,

    /// Output only information about the current terminal.
    #[arg(short = 'm', long)]
    current_terminal: bool,

    /// List any other process that is currently active and has been previously spawned by init.
    #[arg(short, long)]
    process: bool,

    /// List only the names and the number of users currently logged on.
    #[arg(short = 'q', long)]
    summary: bool,

    /// Print the current run-level of the init process.
    #[arg(short, long)]
    runlevel: bool,

    /// List only the name, line, and time fields (default).
    #[arg(short, long = "short", default_value_t = true, group = "output")]
    short_format: bool,

    /// Indicate the last change to the system clock.
    #[arg(short = 't', long = "time")]
    last_change: bool,

    /// Show the state of each terminal
    #[arg(short = 'T', long, group = "output")]
    terminals: bool,

    /// Normal selection of information
    #[arg(long)]
    userproc: bool,

    /// Write "idle time" for each displayed user
    #[arg(short = 'u', long = "users")]
    idle_time: bool,

    /// Gather information from FILE, instead of default utmp file.
    file: Option<PathBuf>,
}

// convert timestamp into POSIX-specified strftime format (local time)
fn fmt_timestamp(ts: libc::time_t) -> String {
    use chrono::{Local, TimeZone};
    let dt = Local
        .timestamp_opt(ts, 0)
        .single()
        .unwrap_or_else(Local::now);
    dt.format("%b %e %H:%M").to_string()
}

// Get terminal state for -T option: + (write allowed), - (write denied), ? (unknown)
fn get_terminal_state(line: &str) -> char {
    if line == "system boot" {
        return ' ';
    }
    let path = format!("/dev/{}", line);
    match std::fs::metadata(&path) {
        Ok(meta) => {
            let mode = meta.mode();
            // Check group write permission (o+w would be 0o002, g+w is 0o020)
            if mode & 0o020 != 0 {
                '+'
            } else {
                '-'
            }
        }
        Err(_) => '?',
    }
}

// Get idle time for -u option
fn get_idle_time(line: &str) -> String {
    if line == "system boot" {
        return "   .".to_string();
    }
    let path = format!("/dev/{}", line);
    match std::fs::metadata(&path) {
        Ok(meta) => {
            let atime = meta.atime();
            let now = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_secs() as i64;
            let idle_secs = now - atime;
            if idle_secs < 60 {
                "   .".to_string() // Active in last minute
            } else if idle_secs > 24 * 60 * 60 {
                " old".to_string()
            } else {
                let hours = idle_secs / 3600;
                let mins = (idle_secs % 3600) / 60;
                format!("{:02}:{:02}", hours, mins)
            }
        }
        Err(_) => "   ?".to_string(),
    }
}

fn print_fmt_short(args: &Args, entry: &Utmpx, line: &str) {
    if args.idle_time {
        println!(
            "{:<16} {:<12} {} {} {:>5}",
            entry.user,
            line,
            fmt_timestamp(entry.timestamp),
            get_idle_time(line),
            entry.pid
        );
    } else {
        println!(
            "{:<16} {:<12} {}",
            entry.user,
            line,
            fmt_timestamp(entry.timestamp)
        );
    }
}

fn print_fmt_term(args: &Args, entry: &Utmpx, line: &str) {
    let term_state = get_terminal_state(line);
    if args.idle_time {
        println!(
            "{:<16} {} {:<12} {} {} {:>5}",
            entry.user,
            term_state,
            line,
            fmt_timestamp(entry.timestamp),
            get_idle_time(line),
            entry.pid
        );
    } else {
        println!(
            "{:<16} {} {:<12} {}",
            entry.user,
            term_state,
            line,
            fmt_timestamp(entry.timestamp)
        );
    }
}

fn current_terminal() -> Option<String> {
    curuser::tty().map(|s| {
        if let Some(st) = s.strip_prefix("/dev/") {
            st.to_owned()
        } else {
            s
        }
    })
}

fn print_entry(args: &Args, entry: &Utmpx) {
    // Skip if current_terminal option is set and this entry is not for the current terminal
    if args.current_terminal {
        match current_terminal() {
            Some(current_tty) => {
                if entry.line != current_tty {
                    return;
                }
            }
            None => {
                // No tty available, skip all entries for -m
                return;
            }
        }
    }

    let mut selected = false;
    if (args.boot && entry.typ == platform::BOOT_TIME)
        || (args.userproc && entry.typ == platform::USER_PROCESS)
        || (args.dead && entry.typ == platform::DEAD_PROCESS)
        || (args.login && entry.typ == platform::LOGIN_PROCESS)
        || (args.runlevel && entry.typ == platform::RUN_LVL)
        || (args.process && entry.typ == platform::INIT_PROCESS)
    {
        selected = true;
    }

    if !selected {
        return;
    }

    let line = match entry.typ {
        platform::BOOT_TIME => "system boot",
        _ => entry.line.as_str(),
    };

    if args.terminals {
        print_fmt_term(args, entry, line);
    } else {
        print_fmt_short(args, entry, line);
    }
}

fn show_utmpx_entries(args: &Args) {
    if args.heading {
        println!(
            "{:<16} {:<12} {}",
            gettext("NAME"),
            gettext("LINE"),
            gettext("TIME")
        );
    }

    let entries = utmpx::load();
    for entry in &entries {
        print_entry(args, entry);
    }
}

fn show_utmpx_summary() {
    let entries = utmpx::load();
    let users: Vec<&str> = entries
        .iter()
        .filter(|e| !e.user.is_empty() && e.typ == platform::USER_PROCESS)
        .map(|e| e.user.as_str())
        .collect();

    // Print users horizontally, space-separated (POSIX format)
    println!("{}", users.join(" "));
    println!("# users={}", users.len());
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args: Vec<String> = std::env::args().skip(1).collect();
    let am_i = args.len() == 2 && args[0] == "am" && (args[1] == "i" || args[1] == "I");

    // parse command line arguments; if "who am i", use special args
    let mut args = {
        if am_i {
            Args::parse_from(["who", "-m"])
        } else {
            Args::parse()
        }
    };
    if args.all {
        args.userproc = true;
        args.boot = true;
        args.dead = true;
        args.login = true;
        args.process = true;
        args.runlevel = true;
        args.last_change = true;
        args.terminals = true;
        args.idle_time = true;
    } else if !args.boot && !args.dead && !args.login && !args.process && !args.runlevel {
        args.userproc = true;
    }

    let mut exit_code = 0;

    if args.file.is_some() {
        eprintln!("{}", gettext("who: -f option not yet implemented"));
        exit_code = 1;
    } else if args.summary {
        show_utmpx_summary();
    } else {
        show_utmpx_entries(&args);
    }

    std::process::exit(exit_code)
}
