//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Command line argument parsing for mailx

#[derive(Debug, Clone)]
pub enum Mode {
    Send,
    CheckMail,
    HeadersOnly,
    Receive,
}

#[derive(Debug, Clone)]
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
        let mut result = Args {
            mode: Mode::Receive,
            subject: None,
            addresses: Vec::new(),
            file: None,
            read_mbox: false,
            record_to_recipient: false,
            no_header_summary: false,
            ignore_interrupts: false,
            no_init: false,
            user: None,
            discard_empty: false,
        };

        let mut i = 0;
        let mut check_mail = false;
        let mut headers_only = false;
        let mut end_of_opts = false;

        while i < args.len() {
            let arg = &args[i];

            if arg == "--" && !end_of_opts {
                // Explicit end-of-options terminator (XBD 12.2).
                end_of_opts = true;
                i += 1;
                continue;
            }

            if !end_of_opts && arg.starts_with('-') && arg.len() > 1 {
                let mut chars = arg[1..].chars().peekable();

                while let Some(c) = chars.next() {
                    match c {
                        'e' => {
                            check_mail = true;
                        }
                        'E' => {
                            result.discard_empty = true;
                        }
                        'f' => {
                            // `file` is an operand (XBD 12.2), not an
                            // option-argument: consumed as a trailing operand
                            // in Receive Mode below, not glued to -f here.
                            result.read_mbox = true;
                        }
                        'F' => {
                            result.record_to_recipient = true;
                        }
                        'H' => {
                            headers_only = true;
                        }
                        'i' => {
                            result.ignore_interrupts = true;
                        }
                        'n' => {
                            result.no_init = true;
                        }
                        'N' => {
                            result.no_header_summary = true;
                        }
                        's' => {
                            // Subject - get the value
                            let rest: String = chars.collect();
                            if !rest.is_empty() {
                                result.subject = Some(rest);
                            } else if i + 1 < args.len() {
                                i += 1;
                                result.subject = Some(args[i].clone());
                            } else {
                                return Err("option requires an argument -- s".to_string());
                            }
                            break;
                        }
                        'u' => {
                            // User mailbox
                            let rest: String = chars.collect();
                            if !rest.is_empty() {
                                result.user = Some(rest);
                            } else if i + 1 < args.len() {
                                i += 1;
                                result.user = Some(args[i].clone());
                            } else {
                                return Err("option requires an argument -- u".to_string());
                            }
                            break;
                        }
                        _ => {
                            return Err(format!("illegal option -- {}", c));
                        }
                    }
                }
            } else if result.read_mbox && result.file.is_none() {
                // With -f, the first non-option operand is the mailbox file to
                // read (Receive Mode), not a recipient address.
                result.file = Some(arg.clone());
            } else {
                // Non-option argument - must be an address (Send Mode).
                result.addresses.push(arg.clone());
            }

            i += 1;
        }

        // Determine mode
        if check_mail {
            result.mode = Mode::CheckMail;
        } else if headers_only {
            result.mode = Mode::HeadersOnly;
        } else if result.read_mbox {
            // -f always selects Receive Mode, reading the named (or default) box.
            result.mode = Mode::Receive;
        } else if !result.addresses.is_empty() {
            result.mode = Mode::Send;
        } else {
            result.mode = Mode::Receive;
        }

        // If -u was specified, read that user's system mailbox. Search the
        // usual spool locations (as get_system_mailbox does), defaulting to
        // /var/mail. Read access is governed by the file's permissions, which
        // provide the "appropriate privileges" check (spec 104287-104289).
        if let Some(ref user) = result.user {
            let candidates = [
                format!("/var/mail/{}", user),
                format!("/var/spool/mail/{}", user),
                format!("/usr/spool/mail/{}", user),
            ];
            let path = candidates
                .iter()
                .find(|p| std::path::Path::new(p).exists())
                .cloned()
                .unwrap_or_else(|| candidates[0].clone());
            result.file = Some(path);
        }

        Ok(result)
    }
}
