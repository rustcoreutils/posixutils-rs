//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Input mode: reading a message body from the user, with command escapes and interrupts.

use std::io;

use crate::commands::Interp;
use crate::error::{Error, Result};
use crate::escapes::{handle_escape, Escape};
use crate::mailbox::Mailbox;
use crate::send::ComposedMessage;
use crate::variables::Variables;

/// Handle a `SIGINT` received while composing a message in input mode.
///
/// With `ignore` set, the interrupt prints `@` and discards the current line.
/// Otherwise the first interrupt warns; a second consecutive interrupt aborts
/// the message, writing the partial letter to the dead-letter file when `save`
/// is set.  Returns `true` when the message should be aborted.
fn interrupt_message(
    composed: &ComposedMessage,
    vars: &Variables,
    interrupt_count: &mut u32,
) -> bool {
    if vars.is_set("ignore") {
        println!("@");
        return false;
    }
    *interrupt_count += 1;
    if *interrupt_count >= 2 {
        if vars.is_set("save") && !composed.body.is_empty() {
            crate::send::save_dead_letter(composed, vars);
        }
        return true;
    }
    println!("(Interrupt -- one more to kill letter)");
    false
}

/// Read a message body from standard input, handling command escapes.
///
/// Shared by every entry into input mode: `mail`, `reply`, `followup`, and Send
/// Mode.
pub(crate) fn compose_body(
    composed: &mut ComposedMessage,
    mb: Option<&Mailbox>,
    vars: &mut Variables,
    interp: &mut Interp,
) -> Result<()> {
    let escape_char = vars.escape_char();
    let mut interrupt_count = 0;

    loop {
        let mut line = Vec::new();
        match crate::signals::read_bytes_interruptible(&mut line) {
            Ok(0) => {
                // EOF - if ignoreeof is set, require "." (or ~.) to end.
                if vars.is_set("ignoreeof") {
                    println!("Use \".\" to terminate letter.");
                    continue;
                }
                break;
            }
            Ok(_) => {}
            Err(e) if e.kind() == io::ErrorKind::Interrupted => {
                crate::signals::take_sigint();
                if interrupt_message(composed, vars, &mut interrupt_count) {
                    return Err(Error::Interrupted);
                }
                continue;
            }
            Err(e) => return Err(e.to_string().into()),
        }

        // A SIGINT may have arrived between reads.
        if crate::signals::take_sigint() && interrupt_message(composed, vars, &mut interrupt_count)
        {
            return Err(Error::Interrupted);
        }

        interrupt_count = 0;

        if let Some(escape) = escape_text(&line, escape_char) {
            if perform_escape(&escape, composed, vars, interp, mb)? {
                break;
            }
            continue;
        }

        // Check for single period (if dot is set, or ignoreeof forces it)
        if (vars.is_set("dot") || vars.is_set("ignoreeof")) && line.trim_ascii() == b"." {
            break;
        }

        composed.body.extend_from_slice(&line);
    }

    Ok(())
}

/// The command escape `line` holds: the text after a leading escape
/// character, which is compared in its UTF-8 form since the line is bytes.
/// `None` when escapes are disabled (`escape` is null).
fn escape_text(line: &[u8], escape_char: Option<char>) -> Option<String> {
    let mut buf = [0u8; 4];
    let ec = escape_char?.encode_utf8(&mut buf).as_bytes();
    (line.starts_with(ec) && line.len() > ec.len())
        .then(|| String::from_utf8_lossy(&line[ec.len()..]).into_owned())
}

/// Perform a command escape, returning whether it ended the message.
///
/// An error in an escape is diagnosed without abandoning the message (spec
/// 105114-105119). `~q` saves the partial message and `~x` does not (spec
/// 105081-105083, 105097); both end composition with [`Error::Aborted`].
fn perform_escape(
    escape: &str,
    composed: &mut ComposedMessage,
    vars: &mut Variables,
    interp: &mut Interp,
    mb: Option<&Mailbox>,
) -> Result<bool> {
    match handle_escape(escape, composed, vars, interp, mb) {
        Ok(Escape::Continue) => Ok(false),
        Ok(Escape::Finish) => Ok(true),
        Ok(Escape::Abort { save_dead_letter }) => {
            if save_dead_letter && vars.is_set("save") && !composed.body.is_empty() {
                crate::send::save_dead_letter(composed, vars);
            }
            Err(Error::Aborted)
        }
        Err(e) => {
            eprintln!("{}", e);
            Ok(false)
        }
    }
}
