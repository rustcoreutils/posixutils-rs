//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Shared helpers: the user and the paths derived from the environment,
//! running shell commands, the pager, and address lists.

use std::env;
use std::io::{self, IsTerminal, Write};
use std::process::{Command, Stdio};

use crate::error::Result;
use crate::variables::Variables;

/// The user's home directory, falling back to the current directory.
pub fn home() -> String {
    env::var("HOME").unwrap_or_else(|_| ".".to_string())
}

/// The login name of the user running mailx, or an empty string if unknown.
///
/// `USER` may be present but empty, which is not a login name; treat it the
/// same as absent rather than letting "" stand in for the user's identity.
pub fn user_login() -> String {
    user_login_or("")
}

/// The login name, or `unknown` when the environment does not say.
pub fn user_login_or_unknown() -> String {
    user_login_or("unknown")
}

fn user_login_or(fallback: &str) -> String {
    for var in ["USER", "LOGNAME"] {
        if let Ok(v) = env::var(var) {
            if !v.trim().is_empty() {
                return v;
            }
        }
    }
    fallback.to_string()
}

/// The secondary mailbox messages are moved to when they have been read.
pub fn mbox_path(vars: &Variables) -> String {
    vars.get_nonempty("MBOX")
        .map(|s| s.to_string())
        .or_else(|| env::var("MBOX").ok())
        .unwrap_or_else(|| format!("{}/mbox", home()))
}

/// The file a partial message is saved to on interrupt or delivery error.
pub fn dead_letter_path(vars: &Variables) -> String {
    vars.get_nonempty("DEAD")
        .map(|s| s.to_string())
        .unwrap_or_else(|| format!("{}/dead.letter", home()))
}

/// The user's own system mailbox.
///
/// `MAIL` names it outright when set (a widely relied-on extension);
/// otherwise the spool is searched.
pub fn system_mailbox() -> String {
    env::var("MAIL").unwrap_or_else(|_| spool_path(&user_login_or_unknown()))
}

/// Whether `a` and `b` name the same file.
pub fn same_file(a: &str, b: &str) -> bool {
    use std::os::unix::fs::MetadataExt;
    match (std::fs::metadata(a), std::fs::metadata(b)) {
        (Ok(a), Ok(b)) => (a.dev(), a.ino()) == (b.dev(), b.ino()),
        _ => a == b,
    }
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

/// The command interpreter `SHELL` names.
pub fn shell_program(vars: &Variables) -> &str {
    vars.get_nonempty("SHELL").unwrap_or("/bin/sh")
}

/// A command that runs `cmd` through the command interpreter.
///
/// POSIX spells the invocation out: three arguments, `-c`, `--`, and the
/// command string (spec 105043-105044, 104870-104872).
pub fn shell_command(cmd: &str, vars: &Variables) -> Command {
    let mut command = Command::new(shell_program(vars));
    command.arg("-c").arg("--").arg(cmd);
    command
}

/// Run the editor `editor` names on `file`.
///
/// As in BSD and Heirloom mailx, the value is split into words, quotes
/// honored (`emacs -nw` is a program and an argument), and run directly:
/// no shell expands it.
pub fn edit(editor: &str, file: &std::path::Path) -> Result<()> {
    let words = crate::variables::split_args(editor);
    let (program, args) = words.split_first().ok_or("No editor")?;
    Command::new(program)
        .args(args)
        .arg(file)
        .status()
        .map_err(|e| e.to_string())?;
    Ok(())
}

/// Run `cmd` through the command interpreter.
pub fn shell(cmd: &str, vars: &Variables) -> Result<std::process::ExitStatus> {
    shell_command(cmd, vars)
        .status()
        .map_err(|e| e.to_string().into())
}

/// Run `cmd` through `SHELL` and collect its standard output.
pub fn shell_output(cmd: &str, vars: &Variables) -> Result<String> {
    shell_output_bytes(cmd, vars).map(|out| String::from_utf8_lossy(&out).into_owned())
}

/// As [`shell_output`], keeping the output as the bytes written.
pub fn shell_output_bytes(cmd: &str, vars: &Variables) -> Result<Vec<u8>> {
    let out = shell_command(cmd, vars)
        .output()
        .map_err(|e| e.to_string())?;
    if !out.status.success() {
        return Err(format!("{}: {}", cmd, String::from_utf8_lossy(&out.stderr)).into());
    }
    Ok(out.stdout)
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
pub fn page_or_print(text: &[u8], vars: &Variables) {
    let long_enough = vars
        .get_number("crt")
        .is_some_and(|crt| byte_lines(text).count() > crt as usize);

    if !long_enough || !io::stdout().is_terminal() {
        print_bytes(text);
        return;
    }

    let pager = vars.get_nonempty("PAGER").unwrap_or("more");
    let spawned = shell_command(pager, vars).stdin(Stdio::piped()).spawn();

    // A pager that cannot be started must not swallow the message.
    let Ok(mut child) = spawned else {
        print_bytes(text);
        return;
    };

    // The user quitting the pager closes this pipe; the message has been
    // shown, so that is not a reason to kill the mail session.
    let _sigpipe = plib::io::SigPipeIgnored::new();

    if let Some(mut stdin) = child.stdin.take() {
        let _ = stdin.write_all(text);
    }
    let _ = child.wait();
}

/// Prompt for a header field value, showing `current` as the initial text.
///
/// Returns `None` when the user accepts what is there.
pub fn prompt_field(label: &str, current: &str) -> Result<Option<String>> {
    print!("{}: {}", label, current);
    io::stdout().flush().map_err(|e| e.to_string())?;

    let mut line = String::new();
    io::stdin()
        .read_line(&mut line)
        .map_err(|e| e.to_string())?;

    let line = line.trim();
    Ok((!line.is_empty()).then(|| line.to_string()))
}

/// The lines of `bytes` as `str::lines` would give them: split at `\n`,
/// a trailing `\r` dropped, and no empty line after a final newline.
pub fn byte_lines(bytes: &[u8]) -> impl Iterator<Item = &[u8]> {
    let body = bytes.strip_suffix(b"\n").unwrap_or(bytes);
    (!bytes.is_empty())
        .then(|| body.split(|&b| b == b'\n'))
        .into_iter()
        .flatten()
        .map(|l| l.strip_suffix(b"\r").unwrap_or(l))
}

/// Write `bytes` to standard output as they are.
///
/// Message text is shown as stored, 8-bit bytes included, as traditional
/// mailx does; the terminal's character set decides how they look.
pub fn print_bytes(bytes: &[u8]) {
    let mut out = io::stdout().lock();
    let _ = out.write_all(bytes);
    let _ = out.flush();
}

/// Options for opening a file mailx may create, with a new file private
/// to the user (mode 0600).
///
/// Mailboxes, saved messages, the record and dead.letter hold mail. Creating
/// them 0666 less the umask -- 0644 under the usual one -- let every local
/// user read them. An existing file keeps its permissions.
pub fn private_open() -> std::fs::OpenOptions {
    use std::os::unix::fs::OpenOptionsExt;
    let mut options = std::fs::OpenOptions::new();
    options.mode(0o600);
    options
}

/// Split a comma-separated address list.
///
/// Only a comma outside quotes, comments, and angle brackets separates two
/// addresses: `"Smith, J" <j@example.com>` is one.
pub fn addresses(list: &str) -> impl Iterator<Item = &str> {
    let mut parts = Vec::new();
    let (mut start, mut in_quote, mut depth, mut in_angle) = (0, false, 0usize, false);
    let mut escaped = false;
    for (i, c) in list.char_indices() {
        if escaped {
            escaped = false;
            continue;
        }
        match c {
            '\\' if in_quote || depth > 0 => escaped = true,
            '"' if depth == 0 && !in_angle => in_quote = !in_quote,
            _ if in_quote => {}
            '(' => depth += 1,
            ')' if depth > 0 => depth -= 1,
            _ if depth > 0 => {}
            '<' => in_angle = true,
            '>' => in_angle = false,
            ',' if !in_angle => {
                parts.push(&list[start..i]);
                start = i + 1;
            }
            _ => {}
        }
    }
    parts.push(&list[start..]);
    parts.into_iter().map(str::trim).filter(|a| !a.is_empty())
}

/// Split recipients given as command arguments.
///
/// Commas separate addresses as in a header, and a quoted display name may
/// hold a comma or a blank. Within a comma-separated part an angle address
/// ends its mailbox, and plain addresses are separated by blanks:
/// `"Smith, J" <j@x> k@x, l@x m@x` is four recipients.
pub fn address_words(list: &str) -> Vec<&str> {
    let mut words = Vec::new();
    for part in addresses(list) {
        let mut rest = part;
        while let Some(end) = angle_address_end(rest) {
            words.push(rest[..end].trim());
            rest = rest[end..].trim_start();
        }
        if rest.contains(['"', '(']) {
            words.push(rest);
        } else {
            words.extend(rest.split_whitespace());
        }
    }
    words
}

/// The index just past the `>` that closes the first angle address in `s`,
/// outside quotes and comments; a `>` with no `<` before it is text.
fn angle_address_end(s: &str) -> Option<usize> {
    let (mut in_quote, mut depth, mut escaped, mut in_angle) = (false, 0usize, false, false);
    for (i, c) in s.char_indices() {
        if escaped {
            escaped = false;
            continue;
        }
        match c {
            '\\' if in_quote || depth > 0 => escaped = true,
            '"' if depth == 0 => in_quote = !in_quote,
            _ if in_quote => {}
            '(' => depth += 1,
            ')' if depth > 0 => depth -= 1,
            '<' => in_angle = true,
            '>' if in_angle => return Some(i + 1),
            _ => {}
        }
    }
    None
}

/// The bare addresses of an address list, comma-separated:
/// `A <a@x>, b@x (B)` is `a@x, b@x`.
pub fn bare_addresses(list: &str) -> String {
    addresses(list)
        .map(crate::message::extract_address)
        .filter(|a| !a.is_empty())
        .collect::<Vec<_>>()
        .join(", ")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn address_words_splits_mailboxes() {
        assert_eq!(
            address_words(r#""Smith, J" <j@x> k@x, l@x m@x"#),
            [r#""Smith, J" <j@x>"#, "k@x", "l@x", "m@x"]
        );
        assert_eq!(address_words("a@x (Alice A) "), ["a@x (Alice A)"]);
        assert_eq!(
            address_words("Bob >_< Smith <bob@x>"),
            ["Bob >_< Smith <bob@x>"]
        );
    }
}
