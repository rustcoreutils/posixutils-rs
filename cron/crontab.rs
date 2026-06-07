//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use cron::{CRON_ALLOW, CRON_DENY, CRON_SPOOL_DIR};
use gettextrs::gettext;
use std::env;
use std::fs;
use std::io::{self, ErrorKind, Read, Write};
use std::path::Path;
use std::process::{exit, Command};

#[derive(Parser)]
#[command(version, about = gettext("crontab - schedule periodic background work"))]
struct CronArgs {
    #[arg(short, long, help = gettext("edit user's crontab"))]
    edit: bool,
    #[arg(short, long, help = gettext("list user's crontab"))]
    list: bool,
    #[arg(short, long, help = gettext("delete user's crontab"))]
    remove: bool,
    #[arg(
        name = "FILE",
        help = gettext("replace crontab with FILE (or standard input if '-' or omitted)")
    )]
    file: Option<String>,
}

/// Check whether a user is allowed to use crontab per the cron.allow/cron.deny
/// rules (audit #C7).
fn is_user_allowed(username: &str) -> bool {
    // If cron.allow exists, only the users listed in it are permitted.
    match fs::read_to_string(CRON_ALLOW) {
        Ok(content) => return content.lines().any(|line| line.trim() == username),
        // An existing-but-unreadable allow file fails closed.
        Err(e) if e.kind() != ErrorKind::NotFound => return false,
        Err(_) => {}
    }

    // Otherwise, if cron.deny exists, everyone not listed is permitted (an empty
    // cron.deny therefore permits all users).
    match fs::read_to_string(CRON_DENY) {
        Ok(content) => return !content.lines().any(|line| line.trim() == username),
        Err(e) if e.kind() != ErrorKind::NotFound => return false,
        Err(_) => {}
    }

    // Neither file exists: only a privileged process may submit (POSIX XSI).
    is_privileged()
}

fn is_privileged() -> bool {
    // SAFETY: getuid() is always safe and never fails.
    unsafe { libc::getuid() == 0 }
}

/// Validate `content` against the daemon's parser, then atomically install it
/// over `target`. On any error the existing crontab is left untouched (#C4/#C6).
fn install_crontab(target: &str, content: &str) -> ! {
    if let Err(line) = cron::job::validate_user_crontab(content) {
        diag_error(&format!(
            "{} {}",
            gettext("errors in crontab file, can't install; problem at line"),
            line
        ));
        exit(1);
    }

    if let Err(e) = plib::io::write_atomic(Path::new(target), content.as_bytes()) {
        diag_error(&format!("{}: {}", gettext("cannot install crontab"), e));
        exit(1);
    }

    exit(0);
}

/// Read the replacement crontab from `file`, or from standard input when the
/// operand is absent or `-` (audit #C2).
fn read_source(file: &Option<String>) -> io::Result<String> {
    match file.as_deref() {
        None | Some("-") => {
            let mut buf = String::new();
            io::stdin().read_to_string(&mut buf)?;
            Ok(buf)
        }
        Some(path) => fs::read_to_string(path),
    }
}

fn do_replace(target: &str, file: &Option<String>) -> ! {
    match read_source(file) {
        Ok(content) => install_crontab(target, &content),
        Err(e) => {
            diag_error(&format!("{}: {}", gettext("cannot read input"), e));
            exit(1);
        }
    }
}

fn do_list(target: &str) -> ! {
    match fs::read_to_string(target) {
        Ok(content) => {
            // The listing is the one thing that goes to standard output.
            print!("{content}");
            io::stdout().flush().ok();
            exit(0);
        }
        Err(e) => {
            diag_error(&format!("{}: {}", gettext("no crontab to list"), e));
            exit(1);
        }
    }
}

fn do_remove(target: &str) -> ! {
    match fs::remove_file(target) {
        Ok(()) => exit(0),
        Err(e) => {
            diag_error(&format!("{}: {}", gettext("cannot remove crontab"), e));
            exit(1);
        }
    }
}

/// Edit a copy of the current crontab and install it only on a clean editor
/// exit. The live crontab is never truncated up front (audit #C1).
fn do_edit(target: &str) -> ! {
    // Pre-flight: confirm we can create the install tempfile in the spool dir.
    // This fails fast — before launching the editor — when the result could not
    // be installed, and guarantees the live entry is never touched on failure.
    let spool_dir = Path::new(target).parent().unwrap_or(Path::new("."));
    if let Err(e) = tempfile::NamedTempFile::new_in(spool_dir) {
        diag_error(&format!(
            "{}: {}",
            gettext("cannot access crontab spool"),
            e
        ));
        exit(1);
    }

    // Seed the edit buffer from the current crontab (empty if none exists).
    let current = fs::read_to_string(target).unwrap_or_default();
    let mut tmp = match tempfile::NamedTempFile::new() {
        Ok(t) => t,
        Err(e) => {
            diag_error(&format!(
                "{}: {}",
                gettext("cannot create temporary file"),
                e
            ));
            exit(1);
        }
    };
    if let Err(e) = tmp.write_all(current.as_bytes()).and_then(|()| tmp.flush()) {
        diag_error(&format!(
            "{}: {}",
            gettext("cannot write temporary file"),
            e
        ));
        exit(1);
    }
    let tmp_path = tmp.path().to_path_buf();

    // Launch the editor with the path as a separate argv element — never
    // interpolated into a shell command line (audit #C8).
    let editor = env::var("EDITOR").unwrap_or_else(|_| "vi".to_string());
    let mut parts = editor.split_whitespace();
    let prog = parts.next().unwrap_or("vi");
    let mut cmd = Command::new(prog);
    cmd.args(parts).arg(&tmp_path);

    match cmd.status() {
        Ok(status) if status.success() => {}
        Ok(_) => {
            diag_error(&gettext("editor exited with an error; crontab unchanged"));
            exit(1);
        }
        Err(e) => {
            diag_error(&format!("{}: {}", gettext("cannot launch editor"), e));
            exit(1);
        }
    }

    match fs::read_to_string(&tmp_path) {
        Ok(edited) => install_crontab(target, &edited),
        Err(e) => {
            diag_error(&format!("{}: {}", gettext("cannot read edited crontab"), e));
            exit(1);
        }
    }
}

/// Emit a diagnostic to standard error with the `crontab:` prefix (audit #C3).
fn diag_error(msg: &str) {
    plib::diag::error(msg);
}

fn main() {
    plib::diag::init_locale("crontab");

    let args = CronArgs::parse();

    // Identity comes from the real uid, never the spoofable $LOGNAME (audit #X2).
    let Some(logname) = cron::spool::User::current().map(|u| u.name) else {
        diag_error(&gettext("could not determine the invoking user"));
        exit(1);
    };

    if !is_user_allowed(&logname) {
        diag_error(&format!(
            "{} ({})",
            gettext("you are not allowed to use crontab"),
            logname
        ));
        exit(1);
    }

    let target = format!("{}/{}", CRON_SPOOL_DIR, logname);

    // -e, -l, -r, and a file operand are mutually exclusive.
    let mode_count = [args.edit, args.list, args.remove]
        .iter()
        .filter(|b| **b)
        .count();
    if mode_count > 1 || (mode_count == 1 && args.file.is_some()) {
        diag_error(&gettext(
            "only one of -e, -l, -r, or a file operand may be given",
        ));
        exit(1);
    }

    if args.list {
        do_list(&target);
    } else if args.remove {
        do_remove(&target);
    } else if args.edit {
        do_edit(&target);
    } else {
        // No mode option: replace from the file operand, or from stdin (#C2).
        do_replace(&target, &args.file);
    }
}
