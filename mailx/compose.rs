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
use crate::escapes::handle_escape;
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
/// Mode. There were two copies of this loop, and they had diverged -- the one
/// used by `mail`/`reply`/`followup` ignored the escape result's request to
/// save the partial message, so `~q` from those commands never wrote
/// `dead.letter` although `~q` in Send Mode did.
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

        // Check for escape character (disabled when `escape` is null). The
        // line is bytes, so the escape char is compared in its UTF-8 form; the
        // escape command itself is text.
        let escape = escape_char.and_then(|ec| {
            let mut buf = [0u8; 4];
            let ec = ec.encode_utf8(&mut buf).as_bytes();
            (line.starts_with(ec) && line.len() > ec.len())
                .then(|| String::from_utf8_lossy(&line[ec.len()..]).into_owned())
        });
        if let Some(escape) = escape {
            // A tilde-escape error is diagnosed but does not abort the message
            // (spec 105114-105119).
            let result = match handle_escape(&escape, composed, vars, interp, mb) {
                Ok(r) => r,
                Err(e) => {
                    eprintln!("{}", e);
                    continue;
                }
            };
            if result.done {
                if result.abort {
                    // `~q` saves the partial message, `~x` does not
                    // (spec 105081-105083, 105097).
                    if result.save_dead_letter && vars.is_set("save") && !composed.body.is_empty() {
                        crate::send::save_dead_letter(composed, vars);
                    }
                    return Err(Error::Aborted);
                }
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
