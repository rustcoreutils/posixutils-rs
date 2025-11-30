//! Message list parsing for mailx
//! Handles the various message specification formats defined by POSIX

use crate::mailbox::Mailbox;
use crate::message::{extract_login, MessageState};

/// Parse a message list specification and return matching message numbers
/// If allnet is true, address matching compares only the login part (before @)
pub fn parse_msglist(spec: &str, mb: &Mailbox, for_undelete: bool) -> Result<Vec<usize>, String> {
    parse_msglist_with_opts(spec, mb, for_undelete, false)
}

/// Parse a message list with allnet option
#[allow(dead_code)]
pub fn parse_msglist_allnet(
    spec: &str,
    mb: &Mailbox,
    for_undelete: bool,
    allnet: bool,
) -> Result<Vec<usize>, String> {
    parse_msglist_with_opts(spec, mb, for_undelete, allnet)
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
                .map(|m| m.state != MessageState::Deleted)
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
    // Check for range (n-m)
    if let Some(dash_pos) = spec.find('-') {
        if dash_pos > 0 && dash_pos < spec.len() - 1 {
            let start_str = &spec[..dash_pos];
            let end_str = &spec[dash_pos + 1..];

            let start = parse_single_number(start_str, mb, for_undelete, allnet)?;
            let end = parse_single_number(end_str, mb, for_undelete, allnet)?;

            if start > end {
                return Err("Invalid range".to_string());
            }

            return Ok((start..=end).collect());
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
                    .position(|m| m.state == MessageState::Deleted)
                    .map(|i| vec![i + 1])
                    .ok_or_else(|| "No deleted messages".to_string())
            } else {
                mb.messages
                    .iter()
                    .position(|m| m.state != MessageState::Deleted)
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
                            .map(|m| m.state != MessageState::Deleted)
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
            if spec.starts_with('/') {
                // Search subject
                let search = &spec[1..].to_lowercase();
                let matches: Vec<usize> = mb
                    .messages
                    .iter()
                    .enumerate()
                    .filter(|(_, m)| {
                        (for_undelete || m.state != MessageState::Deleted)
                            && m.subject().to_lowercase().contains(search)
                    })
                    .map(|(i, _)| i + 1)
                    .collect();

                if matches.is_empty() {
                    Err(format!("No messages matching /{}", &spec[1..]))
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
                        'd' => m.state == MessageState::Deleted,
                        'n' => m.state == MessageState::New,
                        'o' => {
                            m.state != MessageState::New
                                && m.state != MessageState::Read
                                && m.state != MessageState::Deleted
                        }
                        'r' => m.state == MessageState::Read,
                        'u' => m.state == MessageState::Unread,
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
                        if !(for_undelete || m.state != MessageState::Deleted) {
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

fn parse_single_number(
    spec: &str,
    mb: &Mailbox,
    for_undelete: bool,
    allnet: bool,
) -> Result<usize, String> {
    let msgs = parse_single_spec(spec, mb, for_undelete, allnet)?;
    msgs.first()
        .copied()
        .ok_or_else(|| "Invalid message specification".to_string())
}

/// Parse a message list and return just the first message
pub fn parse_message(spec: &str, mb: &Mailbox) -> Result<usize, String> {
    let list = parse_msglist(spec, mb, false)?;
    list.first()
        .copied()
        .ok_or_else(|| "No message".to_string())
}
