//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Message list parsing for mailx
//! Handles the various message specification formats defined by POSIX

use crate::mailbox::Mailbox;
use crate::message::{extract_login, Disposition, ReadState};
use crate::variables::Variables;

/// Parse a message list specification and return matching message numbers
///
/// When the `allnet` variable is set, address matching compares only the login
/// part before the `@` (spec 104575-104577).
pub fn parse_msglist(
    spec: &str,
    mb: &Mailbox,
    for_undelete: bool,
    vars: &Variables,
) -> Result<Vec<usize>, String> {
    parse_msglist_with_opts(spec, mb, for_undelete, vars.get_bool("allnet"))
}

fn parse_msglist_with_opts(
    spec: &str,
    mb: &Mailbox,
    for_undelete: bool,
    allnet: bool,
) -> Result<Vec<usize>, String> {
    let spec = spec.trim();

    if spec.is_empty() {
        // Default to current message
        if mb.current > 0 {
            return Ok(vec![mb.current]);
        } else {
            return Err("No applicable messages".to_string());
        }
    }

    let mut result = Vec::new();

    // Split by whitespace to get individual specs
    for token in spec.split_whitespace() {
        let msgs = parse_single_spec(token, mb, for_undelete, allnet)?;
        result.extend(msgs);
    }

    // Remove duplicates and sort
    result.sort();
    result.dedup();

    // Filter out deleted messages unless this is for undelete
    if !for_undelete {
        result.retain(|&n| {
            mb.get(n)
                .map(|m| m.disposition != Disposition::Deleted)
                .unwrap_or(false)
        });
    }

    if result.is_empty() {
        Err("No applicable messages".to_string())
    } else {
        Ok(result)
    }
}

fn parse_single_spec(
    spec: &str,
    mb: &Mailbox,
    for_undelete: bool,
    allnet: bool,
) -> Result<Vec<usize>, String> {
    // Check for range (n-m). A range is a pair of message numbers
    // (spec 104533), so both halves must name one before the `-` is read as a
    // separator. Treating any embedded hyphen as a range turned an ordinary
    // address like `alpha-zeta@example.net` into the span between whatever its
    // two halves happened to match.
    if let Some(dash_pos) = spec.find('-') {
        let (start_str, end_str) = (&spec[..dash_pos], &spec[dash_pos + 1..]);
        if let (Some(start), Some(end)) =
            (range_endpoint(start_str, mb), range_endpoint(end_str, mb))
        {
            if start > end {
                return Err("Invalid range".to_string());
            }
            // Clamp to the mailbox rather than reject: `undelete 1-$` and
            // `undelete 1-9999` both mean "the rest of them".
            let last = mb.message_count();
            if start > last {
                return Err(format!("Invalid message number: {}", start));
            }
            return Ok((start..=end.min(last)).collect());
        }
    }

    // Check for special characters
    match spec {
        "." => {
            // Current message
            if mb.current > 0 {
                Ok(vec![mb.current])
            } else {
                Err("No current message".to_string())
            }
        }
        "^" => {
            // First undeleted message (or first deleted for undelete)
            if for_undelete {
                mb.messages
                    .iter()
                    .position(|m| m.disposition == Disposition::Deleted)
                    .map(|i| vec![i + 1])
                    .ok_or_else(|| "No deleted messages".to_string())
            } else {
                mb.messages
                    .iter()
                    .position(|m| m.disposition != Disposition::Deleted)
                    .map(|i| vec![i + 1])
                    .ok_or_else(|| "No messages".to_string())
            }
        }
        "$" => {
            // Last message
            if mb.message_count() > 0 {
                Ok(vec![mb.message_count()])
            } else {
                Err("No messages".to_string())
            }
        }
        "*" => {
            // All messages
            if for_undelete {
                Ok((1..=mb.message_count()).collect())
            } else {
                Ok((1..=mb.message_count())
                    .filter(|&n| {
                        mb.get(n)
                            .map(|m| m.disposition != Disposition::Deleted)
                            .unwrap_or(false)
                    })
                    .collect())
            }
        }
        "+" => {
            // Next undeleted message
            if for_undelete {
                mb.next_deleted(mb.current)
                    .map(|n| vec![n])
                    .ok_or_else(|| "No more deleted messages".to_string())
            } else {
                mb.next_undeleted(mb.current)
                    .map(|n| vec![n])
                    .ok_or_else(|| "No more messages".to_string())
            }
        }
        "-" => {
            // Previous undeleted message
            if for_undelete {
                mb.prev_deleted(mb.current)
                    .map(|n| vec![n])
                    .ok_or_else(|| "No previous deleted messages".to_string())
            } else {
                mb.prev_undeleted(mb.current)
                    .map(|n| vec![n])
                    .ok_or_else(|| "No previous messages".to_string())
            }
        }
        _ => {
            // Could be a number, /string, :c, or address
            if let Some(suffix) = spec.strip_prefix('/') {
                // Search subject
                let search = suffix.to_lowercase();
                let matches: Vec<usize> = mb
                    .messages
                    .iter()
                    .enumerate()
                    .filter(|(_, m)| {
                        (for_undelete || m.disposition != Disposition::Deleted)
                            && m.subject().to_lowercase().contains(&search)
                    })
                    .map(|(i, _)| i + 1)
                    .collect();

                if matches.is_empty() {
                    Err(format!("No messages matching /{}", suffix))
                } else {
                    Ok(matches)
                }
            } else if spec.starts_with(':') {
                // Message type
                let type_char = spec.chars().nth(1).ok_or("Invalid message type")?;
                let matches: Vec<usize> = mb
                    .messages
                    .iter()
                    .enumerate()
                    .filter(|(_, m)| match type_char {
                        'd' => m.disposition == Disposition::Deleted,
                        'n' => m.read == ReadState::New,
                        // `old` is now a state the model names outright, rather
                        // than "not new, not read, and not deleted".
                        'o' => m.is_old(),
                        'r' => m.read == ReadState::Read,
                        'u' => m.read == ReadState::Unread,
                        _ => false,
                    })
                    .map(|(i, _)| i + 1)
                    .collect();

                if matches.is_empty() {
                    Err(format!("No messages of type :{}", type_char))
                } else {
                    Ok(matches)
                }
            } else if let Ok(num) = spec.parse::<usize>() {
                // Message number
                if num > 0 && num <= mb.message_count() {
                    Ok(vec![num])
                } else {
                    Err(format!("Invalid message number: {}", num))
                }
            } else {
                // Address match
                // If allnet is true, compare only login parts
                let search = if allnet {
                    extract_login(spec).to_lowercase()
                } else {
                    spec.to_lowercase()
                };
                let matches: Vec<usize> = mb
                    .messages
                    .iter()
                    .enumerate()
                    .filter(|(_, m)| {
                        if !(for_undelete || m.disposition != Disposition::Deleted) {
                            return false;
                        }
                        if allnet {
                            // Compare login parts only
                            extract_login(m.from()).to_lowercase().contains(&search)
                        } else {
                            m.from().to_lowercase().contains(&search)
                        }
                    })
                    .map(|(i, _)| i + 1)
                    .collect();

                if matches.is_empty() {
                    Err(format!("No messages from {}", spec))
                } else {
                    Ok(matches)
                }
            }
        }
    }
}

/// Resolve one end of an `n-m` range, or `None` if `s` does not name a message
/// number.
///
/// A plain number, or the `^`, `$`, and `.` forms that stand for one
/// (spec 104525-104533). Anything else -- an address, a subject search -- means
/// the `-` was part of the token, not a separator.
fn range_endpoint(s: &str, mb: &Mailbox) -> Option<usize> {
    match s {
        "^" => Some(1),
        "$" => Some(mb.message_count()),
        "." => Some(mb.current),
        _ if !s.is_empty() && s.chars().all(|c| c.is_ascii_digit()) => s.parse().ok(),
        _ => None,
    }
}

/// The messages an optional msglist argument names, defaulting to the current
/// one.
///
/// Nearly every command begins this way; the three-line form was written out
/// sixteen times.
pub fn msglist_or_current(
    args: &str,
    mb: &Mailbox,
    vars: &Variables,
) -> Result<Vec<usize>, String> {
    if args.is_empty() {
        Ok(vec![mb.current])
    } else {
        parse_msglist(args, mb, false, vars)
    }
}

/// Parse a message list and return just the first message
pub fn parse_message(spec: &str, mb: &Mailbox, vars: &Variables) -> Result<usize, String> {
    let list = parse_msglist(spec, mb, false, vars)?;
    list.first()
        .copied()
        .ok_or_else(|| "No message".to_string())
}
