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
// - implement -T, -u options
//

extern crate chrono;
extern crate clap;
extern crate plib;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::PROJECT_NAME;
use std::path::PathBuf;

/// who - display who is on the system
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
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

// convert timestamp into POSIX-specified strftime format
fn fmt_timestamp(ts: libc::time_t) -> String {
    let dt = chrono::DateTime::from_timestamp(ts, 0).unwrap();
    dt.format("%b %e %H:%M").to_string()
}

fn print_fmt_short(entry: &plib::utmpx::Utmpx, line: &str) {
    println!(
        "{:<16} {:<12} {}",
        entry.user,
        line,
        fmt_timestamp(entry.timestamp)
    );
}

fn print_fmt_term(entry: &plib::utmpx::Utmpx, line: &str) {
    let term_state = '?';
    println!(
        "{:<16} {} {:<12} {}",
        entry.user,
        term_state,
        line,
        fmt_timestamp(entry.timestamp)
    );
}

fn current_terminal() -> String {
    let s = plib::curuser::tty();
    if s.starts_with("/dev/") {
        s[5..].to_string()
    } else {
        s
    }
}

fn print_entry(args: &Args, entry: &plib::utmpx::Utmpx) {
    // Skip if current_terminal option is set and this entry is not for the current terminal
    if args.current_terminal {
        let current_tty = current_terminal();
        if entry.line != current_tty {
            return;
        }
    }

    let mut selected = false;
    {
        // TODO: Remove "libc_aliases" when https://github.com/rust-lang/libc/issues/3190 is resolved
        use plib::libc_aliases as libc;
        if (args.boot && entry.typ == libc::BOOT_TIME)
            || (args.userproc && entry.typ == libc::USER_PROCESS)
            || (args.dead && entry.typ == libc::DEAD_PROCESS)
            || (args.login && entry.typ == libc::LOGIN_PROCESS)
            || (args.runlevel && entry.typ == libc::RUN_LVL)
            || (args.process && entry.typ == libc::INIT_PROCESS)
        {
            selected = true;
        }
    }

    if !selected {
        return;
    }

    let line = {
        // TODO: Remove "libc_aliases" when https://github.com/rust-lang/libc/issues/3190 is resolved
        use plib::libc_aliases as libc;
        match entry.typ {
            libc::BOOT_TIME => "system boot",
            _ => entry.line.as_str(),
        }
    };

    if args.short_format {
        print_fmt_short(entry, line);
    } else {
        print_fmt_term(entry, line);
    }
}

fn show_utmpx_entries(args: &Args) {
    if args.heading {
        println!(
            "{:<16} {:<12} {}",
            gettext("USER"),
            gettext("LINE"),
            gettext("TIME")
        );
    }

    let entries = plib::utmpx::load();
    for entry in &entries {
        print_entry(&args, entry);
    }
}

fn show_utmpx_summary() {
    let mut count = 0;
    let entries = plib::utmpx::load();
    for entry in &entries {
        if entry.user.len() > 0 {
            println!("{}", entry.user);
            count += 1;
        }
    }

    println!("# users = {}", count);
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // manual CLI parse for special "who am i" case
    let args: Vec<String> = std::env::args().skip(1).collect();
    let am_i = args.len() == 2 && args[0] == "am" && (args[1] == "i" || args[1] == "I");

    // parse command line arguments; if "who am i", use special args
    let mut args = {
        if am_i {
            Args::parse_from(&["who", "-m"])
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

    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

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
