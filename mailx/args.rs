//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Command line argument parsing for mailx

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum Mode {
    Send,
    CheckMail,
    HeadersOnly,
    #[default]
    Receive,
}

#[derive(Debug, Clone, Default)]
pub struct Args {
    pub mode: Mode,
    pub subject: Option<String>,
    pub addresses: Vec<String>,
    pub file: Option<String>,
    pub read_mbox: bool,
    pub record_to_recipient: bool,
    pub no_header_summary: bool,
    pub ignore_interrupts: bool,
    pub no_init: bool,
    pub user: Option<String>,
    /// -E: discard messages with an empty body (do not send)
    pub discard_empty: bool,
}

impl Args {
    pub fn parse(args: Vec<String>) -> Result<Self, String> {
        let mut result = Args::default();
        let mut check_mail = false;
        let mut headers_only = false;
        let mut end_of_opts = false;

        let mut i = 0;
        while i < args.len() {
            let arg = &args[i];

            if arg == "--" && !end_of_opts {
                // Explicit end-of-options terminator (XBD 12.2).
                end_of_opts = true;
            } else if !end_of_opts && arg.starts_with('-') && arg.len() > 1 {
                for (pos, c) in arg[1..].char_indices() {
                    let rest = &arg[1 + pos + c.len_utf8()..];
                    match c {
                        'e' => check_mail = true,
                        'E' => result.discard_empty = true,
                        // `file` is an operand (XBD 12.2), not an
                        // option-argument: it is taken from the operands below.
                        'f' => result.read_mbox = true,
                        'F' => result.record_to_recipient = true,
                        'H' => headers_only = true,
                        'i' => result.ignore_interrupts = true,
                        'n' => result.no_init = true,
                        'N' => result.no_header_summary = true,
                        's' => {
                            result.subject = Some(option_argument(rest, &args, &mut i, c)?);
                            break;
                        }
                        'u' => {
                            result.user = Some(option_argument(rest, &args, &mut i, c)?);
                            break;
                        }
                        _ => return Err(format!("illegal option -- {}", c)),
                    }
                }
            } else if result.read_mbox && result.file.is_none() {
                // With -f, the first operand is the mailbox file to read
                // (Receive Mode), not a recipient address.
                result.file = Some(arg.clone());
            } else {
                result.addresses.push(arg.clone());
            }
            i += 1;
        }

        result.mode = if check_mail {
            Mode::CheckMail
        } else if headers_only {
            Mode::HeadersOnly
        } else if !result.read_mbox && !result.addresses.is_empty() {
            Mode::Send
        } else {
            // -f always selects Receive Mode, reading the named (or default) box.
            Mode::Receive
        };
        Ok(result)
    }
}

/// The argument of option `opt`: the rest of its cluster (`-sSubject`), or
/// else the next argument (`-s Subject`), which `i` is advanced past.
fn option_argument(
    rest: &str,
    args: &[String],
    i: &mut usize,
    opt: char,
) -> Result<String, String> {
    if !rest.is_empty() {
        return Ok(rest.to_string());
    }
    *i += 1;
    args.get(*i)
        .cloned()
        .ok_or_else(|| format!("option requires an argument -- {}", opt))
}
