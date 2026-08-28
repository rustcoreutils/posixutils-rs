//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Message representation for mailx

use std::collections::HashMap;

/// Message state as defined by POSIX
/// How much of a message the user has seen.
///
/// This is the axis the `Status:` header records and the `:n`/`:o`/`:r`/`:u`
/// selectors ask about. It is deliberately separate from [`Disposition`]:
/// folding both into one enum meant deleting, preserving, or saving a message
/// erased the knowledge of whether it had been read.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReadState {
    /// Never seen in any session; no `Status:` header.
    New,
    /// Seen in an earlier session but not read; `Status: O`.
    Unread,
    /// Read; `Status: RO`.
    Read,
}

/// What should become of a message when the mailbox is closed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Disposition {
    /// Nothing requested; the mailbox's own rules apply.
    Keep,
    /// Marked for deletion by `delete`.
    Deleted,
    /// Held in place by `hold` or `preserve`.
    Preserved,
    /// Written out by `save`, `Save`, or `write`.
    Saved,
}

impl ReadState {
    /// The `Status:` value recording this state, or `None` for a new message.
    pub fn status_value(&self) -> Option<&'static str> {
        match self {
            ReadState::New => None,
            ReadState::Unread => Some("O"),
            ReadState::Read => Some("RO"),
        }
    }
}

impl Message {
    /// The state character shown in a header summary.
    ///
    /// A requested disposition is what the user most needs to see, so it wins
    /// over the read state rather than replacing it in the model.
    pub fn status_char(&self) -> char {
        match self.disposition {
            Disposition::Deleted => 'D',
            Disposition::Saved => '*',
            Disposition::Preserved => 'P',
            Disposition::Keep => match self.read {
                ReadState::New => 'N',
                ReadState::Unread => 'U',
                ReadState::Read => 'R',
            },
        }
    }

    /// Whether the message is `old`: seen before, and not new or read
    /// (spec 104540).
    pub fn is_old(&self) -> bool {
        self.read == ReadState::Unread
    }
}

/// A single email message
#[derive(Debug, Clone)]
pub struct Message {
    /// The full "From " line (envelope)
    pub from_line: String,
    /// Parsed headers (lowercase key -> original value)
    pub headers: HashMap<String, String>,
    /// Original header lines in order
    pub header_lines: Vec<String>,
    /// Message body
    pub body: String,
    /// How much of the message the user has seen.
    pub read: ReadState,
    /// What should become of it when the mailbox closes.
    pub disposition: Disposition,
    /// Whether this message has been displayed
    pub displayed: bool,
    /// Set by the `mbox`/`touch` commands: force this message into the secondary
    /// mbox at quit even when the `hold` variable is set (spec 104853-104855).
    pub force_mbox: bool,
}

impl Message {
    pub fn new() -> Self {
        Message {
            from_line: String::new(),
            headers: HashMap::new(),
            header_lines: Vec::new(),
            body: String::new(),
            read: ReadState::New,
            disposition: Disposition::Keep,
            displayed: false,
            force_mbox: false,
        }
    }

    /// Get a header value (case-insensitive)
    pub fn get_header(&self, name: &str) -> Option<&str> {
        self.headers.get(&name.to_lowercase()).map(|s| s.as_str())
    }

    /// Get the From: header address
    pub fn from(&self) -> &str {
        self.get_header("from").unwrap_or("(unknown)")
    }

    /// Get the Subject: header
    pub fn subject(&self) -> &str {
        self.get_header("subject").unwrap_or("(no subject)")
    }

    /// Get the To: header
    pub fn to(&self) -> &str {
        self.get_header("to").unwrap_or("")
    }

    /// Get the Date: header
    pub fn date(&self) -> &str {
        self.get_header("date").unwrap_or("")
    }

    /// Get the total size in bytes
    pub fn size(&self) -> usize {
        let header_size: usize = self.header_lines.iter().map(|l| l.len() + 1).sum();
        self.from_line.len() + 1 + header_size + 1 + self.body.len()
    }

    /// Get the number of lines
    pub fn line_count(&self) -> usize {
        1 + self.header_lines.len() + 1 + self.body.lines().count()
    }

    /// Format the message for display with header filtering
    /// Render the message for display.
    ///
    /// `show_all_headers` is the capitalized `Print`/`Type` form, which
    /// overrides `discard`/`ignore`/`retain` (spec 104756). The three callers
    /// each repeated the same choice of header lists; the choice belongs here.
    pub fn format_display(
        &self,
        show_all_headers: bool,
        vars: &crate::variables::Variables,
    ) -> String {
        let (ignored, retained): (&[String], &[String]) = if show_all_headers {
            (&[], &[])
        } else {
            (&vars.ignored_headers, &vars.retained_headers)
        };
        let mut output = String::new();

        // Add headers
        for line in &self.header_lines {
            let field_name = line.split(':').next().unwrap_or("").trim().to_lowercase();

            let include = if show_all_headers {
                true
            } else if !retained.is_empty() {
                retained.iter().any(|r| r.to_lowercase() == field_name)
            } else {
                !ignored.iter().any(|i| i.to_lowercase() == field_name)
            };

            if include {
                output.push_str(line);
                output.push('\n');
            }
        }

        output.push('\n');
        output.push_str(&self.body);

        output
    }

    /// Format the full message including From line
    pub fn format_full(&self) -> String {
        let mut output = self.from_line.clone();
        output.push('\n');

        for line in &self.header_lines {
            output.push_str(line);
            output.push('\n');
        }

        output.push('\n');
        output.push_str(&self.body);

        // Ensure trailing newline
        if !output.ends_with('\n') {
            output.push('\n');
        }

        output
    }

    /// Parse a date string and return a short date format for headers
    pub fn short_date(&self) -> String {
        let date = self.date();
        // Try to extract month day from common date formats
        // e.g., "Mon, 25 Nov 2024 10:30:00 +0000"
        let parts: Vec<&str> = date.split_whitespace().collect();
        if parts.len() >= 3 {
            // Try to find month and day
            for i in 0..parts.len().saturating_sub(1) {
                let month = match parts[i].trim_end_matches(',') {
                    "Jan" | "January" => Some("Jan"),
                    "Feb" | "February" => Some("Feb"),
                    "Mar" | "March" => Some("Mar"),
                    "Apr" | "April" => Some("Apr"),
                    "May" => Some("May"),
                    "Jun" | "June" => Some("Jun"),
                    "Jul" | "July" => Some("Jul"),
                    "Aug" | "August" => Some("Aug"),
                    "Sep" | "September" => Some("Sep"),
                    "Oct" | "October" => Some("Oct"),
                    "Nov" | "November" => Some("Nov"),
                    "Dec" | "December" => Some("Dec"),
                    _ => None,
                };
                if let Some(m) = month {
                    if let Ok(day) = parts[i + 1].trim_end_matches(',').parse::<u32>() {
                        return format!("{} {:2}", m, day);
                    }
                    if i > 0 {
                        if let Ok(day) = parts[i - 1].trim_end_matches(',').parse::<u32>() {
                            return format!("{} {:2}", m, day);
                        }
                    }
                }
            }
        }
        // Fallback - just return first 6 chars
        date.chars().take(6).collect()
    }

    /// Extract the sender's name or address for header display
    pub fn short_from(&self) -> String {
        let from = self.from();
        // Try to extract name from "Name <email>" format
        if let Some(start) = from.find('<') {
            let name = from[..start].trim().trim_matches('"');
            if !name.is_empty() {
                return truncate_display(name, 18);
            }
        }
        // Try to extract name from "(Name)" format. The closing paren is
        // searched for *after* the opening one; searching from the start of the
        // string inverts the range whenever `)` precedes `(`, which panicked
        // while printing the start-up header summary.
        if let Some(start) = from.find('(') {
            if let Some(end) = from[start + 1..].find(')') {
                let name = &from[start + 1..start + 1 + end];
                if !name.is_empty() {
                    return truncate_display(name, 18);
                }
            }
        }
        // Just use the address
        let addr = from.trim_start_matches('<').trim_end_matches('>');
        truncate_display(addr, 18)
    }
}

/// Truncate `s` to at most `max_len` characters for header-summary display,
/// appending an ellipsis when shortened.
///
/// Counts and slices by characters (not bytes) so a multibyte `From:`/`Subject:`
/// value cannot land a slice on a non-char boundary and panic (audit #3).
pub(crate) fn truncate_display(s: &str, max_len: usize) -> String {
    if s.chars().count() <= max_len {
        s.to_string()
    } else {
        let keep = max_len.saturating_sub(3);
        let truncated: String = s.chars().take(keep).collect();
        format!("{}...", truncated)
    }
}

/// Extract email address from a string like "Name <email>" or just "email"
///
/// The closing bracket is searched for *after* the opening one. Searching from
/// the start of the string inverted the range for any display name containing a
/// `>` ahead of the `<`, e.g. `Bob >_< Smith <bob@example.com>`.
pub fn extract_address(s: &str) -> &str {
    if let Some(start) = s.find('<') {
        if let Some(end) = s[start + 1..].find('>') {
            return &s[start + 1..start + 1 + end];
        }
    }
    s.trim()
}

/// Extract login name from an email address
pub fn extract_login(addr: &str) -> &str {
    let addr = extract_address(addr);
    if let Some(at) = addr.find('@') {
        &addr[..at]
    } else {
        addr
    }
}

/// The login name of a message's author, usable as a filename component.
///
/// `Save`, `Copy`, `followup`, and `Followup` name a file after the author
/// (spec 104958-104960), and the value comes straight out of a header the
/// sender controls. It is checked against a conservative allow-list rather
/// than a list of things to reject: a login is a login, and anything else --
/// a path separator, a shell metacharacter, a leading `-` that would read as
/// an option -- means the header is not naming one. Rejecting beats
/// sanitizing, since a silently rewritten name is also a file the user did
/// not ask for.
pub fn author_filename(from: &str) -> Result<&str, String> {
    let login = extract_login(from).trim();
    let acceptable = |c: char| c.is_ascii_alphanumeric() || matches!(c, '.' | '-' | '_' | '+');
    if login.is_empty()
        || login.starts_with('-')
        || login.starts_with('.')
        || !login.chars().all(acceptable)
    {
        return Err(format!("{}: invalid author name", from));
    }
    Ok(login)
}
