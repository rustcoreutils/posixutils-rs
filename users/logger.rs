//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::gettext;
use std::io::{self, BufRead};
use std::process::ExitCode;
use std::str::FromStr;
use syslog::{Facility, Formatter3164};

/// logger - log messages
#[derive(Parser)]
#[command(version, about = gettext("logger - log messages"))]
struct Args {
    #[arg(short = 'i', help = gettext("Log the process ID of the logger process with each message"))]
    log_pid: bool,

    #[arg(short = 'f', value_name = "file", help = gettext("Read the log message bodies from file instead of standard input"))]
    file: Option<String>,

    #[arg(short = 'p', value_name = "priority", help = gettext("Log with the given facility.level priority (default user.notice)"))]
    priority: Option<String>,

    #[arg(short = 't', value_name = "tag", help = gettext("Use tag as the message tag (default: the login name)"))]
    tag: Option<String>,

    #[arg(help = gettext("Message text; operands are concatenated with single spaces"))]
    strings: Vec<String>,
}

/// The syslog severity levels POSIX permits in a `facility.level` priority.
#[derive(Clone, Copy)]
enum Level {
    Emerg,
    Alert,
    Crit,
    Err,
    Warning,
    Notice,
    Info,
    Debug,
}

fn parse_level(s: &str) -> Option<Level> {
    Some(match s.to_ascii_lowercase().as_str() {
        "emerg" | "panic" => Level::Emerg,
        "alert" => Level::Alert,
        "crit" => Level::Crit,
        "err" | "error" => Level::Err,
        "warning" | "warn" => Level::Warning,
        "notice" => Level::Notice,
        "info" => Level::Info,
        "debug" => Level::Debug,
        _ => return None,
    })
}

/// Parse a `-p` argument. POSIX specifies a `facility.level` pair; a bare level
/// (no `.`) is accepted as a convenience and defaults the facility to `user`.
fn parse_priority(p: &str) -> Result<(Facility, Level), String> {
    let (facility, level_str) = match p.split_once('.') {
        Some((fac, lvl)) => {
            let facility = Facility::from_str(fac)
                .map_err(|_| format!("{}: {}", gettext("unknown facility"), fac))?;
            (facility, lvl)
        }
        None => (Facility::LOG_USER, p),
    };
    let level = parse_level(level_str)
        .ok_or_else(|| format!("{}: {}", gettext("unknown priority level"), level_str))?;
    Ok((facility, level))
}

/// Collect the message bodies to log. Operands (if any) form a single message;
/// otherwise each non-empty line of `-f file` (or standard input) is one
/// message.
fn collect_messages(args: &Args) -> io::Result<Vec<String>> {
    if !args.strings.is_empty() {
        return Ok(vec![args.strings.join(" ")]);
    }

    let reader: Box<dyn BufRead> = match args.file.as_deref() {
        Some(f) if f != "-" => Box::new(io::BufReader::new(std::fs::File::open(f)?)),
        _ => Box::new(io::BufReader::new(io::stdin())),
    };

    let mut messages = Vec::new();
    for line in reader.lines() {
        let line = line?;
        // Each non-empty line is a separate message (empty lines are skipped).
        if !line.is_empty() {
            messages.push(line);
        }
    }
    Ok(messages)
}

fn main() -> ExitCode {
    plib::diag::init_locale("logger");

    let args = Args::parse();

    // Default priority is user.notice (POSIX 102902).
    let (facility, level) = match &args.priority {
        Some(p) => match parse_priority(p) {
            Ok(v) => v,
            Err(e) => {
                plib::diag::error(&e);
                return ExitCode::from(1);
            }
        },
        None => (Facility::LOG_USER, Level::Notice),
    };

    let messages = match collect_messages(&args) {
        Ok(m) => m,
        Err(e) => {
            plib::diag::error(&format!("{}", e));
            return ExitCode::from(1);
        }
    };

    let formatter = Formatter3164 {
        facility,
        hostname: None,
        // Default tag is the invoking user's login name; -t overrides it.
        process: args.tag.clone().unwrap_or_else(plib::curuser::login_name),
        // -i logs the logger PID with each message.
        pid: if args.log_pid { std::process::id() } else { 0 },
    };

    let mut writer = match syslog::unix(formatter) {
        Ok(w) => w,
        Err(e) => {
            plib::diag::error(&format!(
                "{}: {:?}",
                gettext("unable to connect to syslog"),
                e
            ));
            return ExitCode::from(1);
        }
    };

    for msg in &messages {
        let res = match level {
            Level::Emerg => writer.emerg(msg),
            Level::Alert => writer.alert(msg),
            Level::Crit => writer.crit(msg),
            Level::Err => writer.err(msg),
            Level::Warning => writer.warning(msg),
            Level::Notice => writer.notice(msg),
            Level::Info => writer.info(msg),
            Level::Debug => writer.debug(msg),
        };
        if let Err(e) = res {
            plib::diag::error(&format!("{}: {:?}", gettext("could not write message"), e));
            return ExitCode::from(1);
        }
    }

    ExitCode::SUCCESS
}
