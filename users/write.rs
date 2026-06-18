//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use chrono::Local;
use clap::Parser;
use gettextrs::gettext;
use plib::{curuser, platform, utmpx};
use std::fs::{File, OpenOptions};
use std::io::{self, BufRead, Write};
use std::os::unix::fs::{FileTypeExt, MetadataExt, PermissionsExt};
use std::os::unix::io::AsRawFd;
use std::process::exit;
use std::sync::atomic::{AtomicI32, Ordering};

/// Raw fd of the recipient terminal, published for the signal handler. An
/// atomic (not a `Mutex<String>`) so the handler stays async-signal-safe: it
/// performs only a raw `write(2)` + `_exit`, with no lock, allocation, file
/// open, or buffered I/O. `-1` means "no recipient open yet".
static RECIPIENT_FD: AtomicI32 = AtomicI32::new(-1);

const ALERT_BYTE: u8 = 0x07;

/// write - write to another user
#[derive(Parser)]
#[command(version, about = gettext("write - write to another user"))]
struct Args {
    /// Login name of the person to whom the message shall be written.
    username: String,

    /// The terminal to which the message shall be written.
    terminal: Option<String>,
}

/// Select the recipient terminal in an implementation-defined manner. If the
/// user is logged in more than once, write an informational message to stdout.
fn select_terminal(user_name: &str) -> String {
    let user_entries: Vec<_> = utmpx::load()
        .into_iter()
        .filter(|entry| entry.user == user_name && entry.typ == platform::USER_PROCESS)
        .collect();

    let chosen = match user_entries.first() {
        Some(e) => e,
        None => {
            plib::diag::error(&format!(
                "{}: {}",
                gettext("user is not logged on"),
                user_name
            ));
            exit(1);
        }
    };

    // Spec: if logged in more than once, note the chosen terminal on stdout.
    if user_entries.len() > 1 {
        println!(
            "{} {} {}",
            user_name,
            gettext("is logged on more than one terminal; writing to"),
            chosen.line
        );
    }

    format!("/dev/{}", chosen.line)
}

/// Resolve and validate the recipient terminal path: it must resolve under
/// `/dev` and be a character device (defeats `/dev/../../etc/passwd`-style
/// operands).
fn validate_terminal(path: &str) -> Result<String, String> {
    let canonical = std::fs::canonicalize(path)
        .map_err(|e| format!("{} {}: {}", gettext("cannot access terminal"), path, e))?;

    if !canonical.starts_with("/dev/") {
        return Err(format!("{}: {}", gettext("not a terminal device"), path));
    }
    let meta = std::fs::metadata(&canonical)
        .map_err(|e| format!("{} {}: {}", gettext("cannot access terminal"), path, e))?;
    if !meta.file_type().is_char_device() {
        return Err(format!("{}: {}", gettext("not a terminal device"), path));
    }

    Ok(canonical.to_string_lossy().into_owned())
}

/// Whether the current user may write to `terminal`'s device, honoring the
/// recipient's `mesg` permission bits. The superuser may always write.
fn check_write_permission(terminal: &str) -> bool {
    // Superuser is not constrained by the recipient's mesg setting.
    if unsafe { libc::geteuid() } == 0 {
        return true;
    }

    match std::fs::metadata(terminal) {
        Ok(metadata) => {
            let mode = metadata.permissions().mode();
            let uid = unsafe { libc::getuid() };
            let gid = unsafe { libc::getgid() };

            (metadata.uid() == uid && mode & 0o200 != 0)
                || (metadata.gid() == gid && mode & 0o020 != 0)
                || (mode & 0o002 != 0)
        }
        Err(err) => {
            plib::diag::error(&format!(
                "{} {}: {}",
                gettext("cannot access terminal"),
                terminal,
                err
            ));
            false
        }
    }
}

fn get_current_date() -> String {
    Local::now().format("%Y-%m-%d %H:%M:%S").to_string()
}

/// Render one canonical input line for the recipient's terminal, per the
/// EXTENDED DESCRIPTION character rules:
///   - the alert (BEL) is forwarded verbatim;
///   - tab and other print/space characters pass through;
///   - other control characters become `^X` (and DEL becomes `^?`);
///   - high bytes (UTF-8 multibyte sequences) pass through for the recipient's
///     terminal to interpret.
///
/// INTR/EOF/erase/kill never reach here: in canonical input mode the terminal
/// driver consumes them, so they are not present in the line data.
fn render_line(bytes: &[u8]) -> Vec<u8> {
    let mut out = Vec::with_capacity(bytes.len() + 1);
    for &b in bytes {
        match b {
            ALERT_BYTE => out.push(ALERT_BYTE),
            b'\t' => out.push(b'\t'),
            0x00..=0x1f => {
                out.push(b'^');
                out.push(b + 64);
            }
            0x7f => out.extend_from_slice(b"^?"),
            _ => out.push(b),
        }
    }
    out.push(b'\n');
    out
}

/// Best-effort double-alert of the *sender's* own terminal (stdout) once the
/// connection is established (spec 123042-123044).
fn alert_sender_terminal() {
    let stdout = io::stdout();
    let mut handle = stdout.lock();
    let _ = handle.write_all(&[ALERT_BYTE, ALERT_BYTE]);
    let _ = handle.flush();
}

/// Signal handler for SIGINT/SIGHUP/SIGPIPE: write "EOT" to the recipient and
/// exit 0 (POSIX mandates a zero status for the interrupt case).
///
/// Async-signal-safe: only a raw `write(2)` to the already-open recipient fd
/// and `_exit(2)` — no locks, allocation, buffered I/O, or file opens, so it
/// cannot deadlock against the main thread or corrupt libc state.
extern "C" fn handle_signal(_sig: libc::c_int) {
    let fd = RECIPIENT_FD.load(Ordering::SeqCst);
    if fd >= 0 {
        let msg = b"EOT\n";
        unsafe {
            libc::write(fd, msg.as_ptr() as *const libc::c_void, msg.len());
        }
    }
    unsafe { libc::_exit(0) };
}

fn register_signal_handlers() {
    for sig in [libc::SIGINT, libc::SIGHUP, libc::SIGPIPE] {
        unsafe {
            if libc::signal(sig, handle_signal as *const () as libc::sighandler_t) == libc::SIG_ERR
            {
                plib::diag::error(&gettext("failed to register signal handler"));
            }
        }
    }
}

/// Write bytes to the recipient terminal; on failure, diagnose and exit.
fn deliver(file: &mut File, bytes: &[u8]) {
    if file.write_all(bytes).and_then(|_| file.flush()).is_err() {
        plib::diag::error(&gettext("write to terminal failed"));
        exit(1);
    }
}

fn main() {
    plib::diag::init_locale("write");

    let args = Args::parse();
    let user_name = args.username;

    // Resolve the recipient terminal.
    let requested = match args.terminal {
        Some(t) if t.starts_with("/dev/") => t,
        Some(t) => format!("/dev/{}", t),
        None => select_terminal(&user_name),
    };
    let terminal = match validate_terminal(&requested) {
        Ok(t) => t,
        Err(msg) => {
            plib::diag::error(&msg);
            exit(1);
        }
    };

    if !check_write_permission(&terminal) {
        plib::diag::error(&gettext("permission denied"));
        exit(1);
    }

    let mut recipient = match OpenOptions::new().write(true).open(&terminal) {
        Ok(f) => f,
        Err(e) => {
            plib::diag::error(&format!(
                "{} {}: {}",
                gettext("cannot open terminal"),
                terminal,
                e
            ));
            exit(1);
        }
    };

    // Publish the recipient fd for the signal handler, then install handlers.
    RECIPIENT_FD.store(recipient.as_raw_fd(), Ordering::SeqCst);
    register_signal_handlers();

    // Banner: "Message from sender-login-id (sending-terminal) [date]..."
    let banner = format!(
        "Message from {} ({}) [{}]...\n",
        curuser::login_name(),
        curuser::tty().unwrap_or_else(|| "???".to_string()),
        get_current_date()
    );
    deliver(&mut recipient, banner.as_bytes());

    // Connection established: alert the sender's terminal twice.
    alert_sender_terminal();

    // Copy canonical input lines to the recipient's terminal.
    let stdin = io::stdin();
    let mut reader = stdin.lock();
    let mut buf = Vec::new();
    loop {
        buf.clear();
        match reader.read_until(b'\n', &mut buf) {
            Ok(0) => break, // EOF (e.g. the EOF special char at line start)
            Ok(_) => {
                if buf.last() == Some(&b'\n') {
                    buf.pop();
                }
                let rendered = render_line(&buf);
                deliver(&mut recipient, &rendered);
            }
            Err(e) => {
                plib::diag::error(&format!("{}: {}", gettext("read error"), e));
                exit(1);
            }
        }
    }

    // Input ended: signal end-of-transmission and exit successfully.
    deliver(&mut recipient, b"EOT\n");
}
