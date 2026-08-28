//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Shared helpers: the paths mailx derives from the environment, the one way it
//! runs a shell command, and the pager decision.
//!
//! Each of these existed in several copies that had drifted from one another --
//! two ways to run `$SHELL -c`, two pagers with different failure behavior,
//! eleven inline `$HOME` fallbacks, four ways to name the mbox.

use std::env;
use std::io::{self, IsTerminal, Write};
use std::process::{Command, Stdio};

use crate::variables::Variables;

/// The user's home directory, falling back to the current directory.
pub fn home() -> String {
    env::var("HOME").unwrap_or_else(|_| ".".to_string())
}

/// The secondary mailbox messages are moved to when they have been read.
pub fn mbox_path(vars: &Variables) -> String {
    vars.get("MBOX")
        .map(|s| s.to_string())
        .or_else(|| env::var("MBOX").ok())
        .unwrap_or_else(|| format!("{}/mbox", home()))
}

/// The file a partial message is saved to on interrupt or delivery error.
pub fn dead_letter_path(vars: &Variables) -> String {
    vars.get("DEAD")
        .map(|s| s.to_string())
        .unwrap_or_else(|| format!("{}/dead.letter", home()))
}

/// The system mailbox belonging to `user`.
///
/// The spool location is unspecified, so the usual three are tried in turn and
/// the first that exists wins; `/var/mail` is the answer when none does.
pub fn spool_path(user: &str) -> String {
    let candidates = [
        format!("/var/mail/{}", user),
        format!("/var/spool/mail/{}", user),
        format!("/usr/spool/mail/{}", user),
    ];
    candidates
        .iter()
        .find(|p| std::path::Path::new(p).exists())
        .cloned()
        .unwrap_or_else(|| candidates[0].clone())
}

/// Run `cmd` through the command interpreter named by `SHELL`.
///
/// POSIX spells the invocation out: three arguments, `-c`, `--`, and the
/// command string (spec 105043-105044, 104870-104872).
pub fn shell(cmd: &str, vars: &Variables) -> Result<std::process::ExitStatus, String> {
    Command::new(vars.get("SHELL").unwrap_or("/bin/sh"))
        .arg("-c")
        .arg("--")
        .arg(cmd)
        .status()
        .map_err(|e| e.to_string())
}

/// Run `cmd` through `SHELL` and collect its standard output.
pub fn shell_output(cmd: &str, vars: &Variables) -> Result<String, String> {
    let out = Command::new(vars.get("SHELL").unwrap_or("/bin/sh"))
        .arg("-c")
        .arg("--")
        .arg(cmd)
        .output()
        .map_err(|e| e.to_string())?;
    if !out.status.success() {
        return Err(format!("{}: {}", cmd, String::from_utf8_lossy(&out.stderr)));
    }
    Ok(String::from_utf8_lossy(&out.stdout).to_string())
}

/// Expand `!` to the previously run shell command.
///
/// A backslash-escaped `\!` stands for a literal `!` (spec 105044-105046).
pub fn expand_bang(cmd: &str, previous: Option<&str>) -> String {
    let mut result = String::new();
    let mut chars = cmd.chars().peekable();

    while let Some(c) = chars.next() {
        match c {
            '\\' if chars.peek() == Some(&'!') => {
                chars.next();
                result.push('!');
            }
            '!' => result.push_str(previous.unwrap_or("")),
            _ => result.push(c),
        }
    }

    result
}

/// Write `text`, paginating through `PAGER` when `crt` says it is long enough.
///
/// Pagination only happens when standard output is a terminal (spec
/// 104362-104367): a pager attached to a pipe would hang or corrupt the output.
pub fn page_or_print(text: &str, vars: &Variables) {
    let long_enough = vars
        .get_number("crt")
        .is_some_and(|crt| text.lines().count() > crt as usize);

    if !long_enough || !io::stdout().is_terminal() {
        print!("{}", text);
        return;
    }

    let pager = vars.get("PAGER").unwrap_or("more");
    let spawned = Command::new(vars.get("SHELL").unwrap_or("/bin/sh"))
        .arg("-c")
        .arg("--")
        .arg(pager)
        .stdin(Stdio::piped())
        .spawn();

    // A pager that cannot be started must not swallow the message.
    let Ok(mut child) = spawned else {
        print!("{}", text);
        return;
    };

    if let Some(mut stdin) = child.stdin.take() {
        let _ = stdin.write_all(text.as_bytes());
    }
    let _ = child.wait();
}

/// Prompt for a header field value, showing `current` as the initial text.
///
/// Returns `None` when the user accepts what is there. Seven copies of this
/// read-a-line-and-trim block existed across three files, which is why `mail`
/// never honored `askcc`/`askbcc` while Send Mode did.
pub fn prompt_field(label: &str, current: &str) -> Result<Option<String>, String> {
    print!("{}: {}", label, current);
    io::stdout().flush().map_err(|e| e.to_string())?;

    let mut line = String::new();
    io::stdin()
        .read_line(&mut line)
        .map_err(|e| e.to_string())?;

    let line = line.trim();
    Ok((!line.is_empty()).then(|| line.to_string()))
}

/// Split a comma-separated address list.
pub fn addresses(list: &str) -> impl Iterator<Item = &str> {
    list.split(',').map(str::trim).filter(|a| !a.is_empty())
}
