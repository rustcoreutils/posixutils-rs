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
    /// The full "From " line (envelope), as stored.
    ///
    /// The envelope line, the header lines and the body are kept as the bytes
    /// in the mailbox file: mail need not be UTF-8, and a message must be
    /// written back, saved and copied exactly as it arrived.
    pub from_line: Vec<u8>,
    /// Parsed headers (lowercase key -> original value)
    pub headers: HashMap<String, String>,
    /// Original header lines in order, each with its folded continuations
    pub header_lines: Vec<Vec<u8>>,
    /// Message body
    pub body: Vec<u8>,
    /// Lines the message occupies in the mailbox file, from its `From ` line
    /// up to the next message's.
    pub raw_lines: usize,
    /// Bytes the message occupies in the mailbox file, measured the same way.
    pub raw_bytes: usize,
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
            from_line: Vec::new(),
            headers: HashMap::new(),
            header_lines: Vec::new(),
            body: Vec::new(),
            raw_lines: 0,
            raw_bytes: 0,
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

    /// The message's size in bytes, as stored in the mailbox file.
    ///
    /// Reconstructing it from the parsed parts undercounted: the separating
    /// blank line and any `Status:` header are not kept in them.
    pub fn size(&self) -> usize {
        self.raw_bytes
    }

    /// The number of lines the message occupies in the mailbox file.
    ///
    /// Counting parsed header entries undercounted, since each folded header
    /// is one entry spanning several lines.
    pub fn line_count(&self) -> usize {
        self.raw_lines
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
    ) -> Vec<u8> {
        let (ignored, retained): (&[String], &[String]) = if show_all_headers {
            (&[], &[])
        } else {
            (&vars.ignored_headers, &vars.retained_headers)
        };
        let mut output = Vec::new();

        // Add headers
        for line in &self.header_lines {
            let name = line.split(|&b| b == b':').next().unwrap_or_default();
            let field_name = String::from_utf8_lossy(name).trim().to_lowercase();

            let include = if show_all_headers {
                true
            } else if !retained.is_empty() {
                retained.iter().any(|r| r.to_lowercase() == field_name)
            } else {
                !ignored.iter().any(|i| i.to_lowercase() == field_name)
            };

            if include {
                output.extend_from_slice(line);
                output.push(b'\n');
            }
        }

        output.push(b'\n');
        output.extend_from_slice(&self.body);

        output
    }

    /// Format the full message including From line
    pub fn format_full(&self) -> Vec<u8> {
        let mut output = self.from_line.clone();
        output.push(b'\n');

        for line in &self.header_lines {
            output.extend_from_slice(line);
            output.push(b'\n');
        }

        output.push(b'\n');
        output.extend_from_slice(&self.body);

        // Ensure trailing newline
        if !output.ends_with(b"\n") {
            output.push(b'\n');
        }

        output
    }

    /// The `From ` line as text, for parsing the sender and date out of it.
    pub fn envelope(&self) -> std::borrow::Cow<'_, str> {
        String::from_utf8_lossy(&self.from_line)
    }

    /// The date for the header summary, as `Www Mmm dd hh:mm`.
    ///
    /// Traditional mailx takes it from the `From ` line, which records when
    /// the message was delivered here. A missing or malformed one falls back
    /// to the `Date:` header.
    pub fn short_date(&self) -> String {
        let line = self.envelope();
        let envelope = line
            .strip_prefix("From ")
            .map(|rest| rest.trim_start())
            .and_then(|rest| rest.split_once(char::is_whitespace))
            .and_then(|(_sender, date)| envelope_date(date));
        envelope
            .or_else(|| header_date(self.date()))
            .unwrap_or_default()
    }

    /// The sender for the header summary: the `From:` address, or failing
    /// that the `From ` line's, as traditional mailx shows it.
    pub fn short_from(&self) -> String {
        match self.get_header("from").map(extract_address) {
            Some(addr) if !addr.is_empty() => addr,
            _ => self
                .envelope()
                .strip_prefix("From ")
                .and_then(|rest| rest.split_whitespace().next())
                .unwrap_or("")
                .to_string(),
        }
    }
}

const WEEKDAYS: [&str; 7] = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
const WEEKDAY_NAMES: [&str; 7] = [
    "Sunday",
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday",
];
const MONTHS: [&str; 12] = [
    "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
];
const MONTH_NAMES: [&str; 12] = [
    "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December",
];

/// A day of the month, `1` to `31`.
fn day_of_month(s: &str) -> Option<u32> {
    s.parse().ok().filter(|d| (1..=31).contains(d))
}

/// The `hh:mm` of an `h:mm[:ss]` or `hh:mm[:ss]` time, if it is one.
fn hours_minutes(s: &str) -> Option<String> {
    let mut fields = s.split(':');
    let (h, m) = (fields.next()?, fields.next()?);
    let in_range = |f: &str, max: u32| {
        (1..=2).contains(&f.len())
            && f.bytes().all(|b| b.is_ascii_digit())
            && f.parse::<u32>().is_ok_and(|n| n <= max)
    };
    let seconds_ok = fields.next().is_none_or(|sec| in_range(sec, 60));
    (in_range(h, 23) && m.len() == 2 && in_range(m, 59) && seconds_ok && fields.next().is_none())
        .then(|| format!("{:0>2}:{}", h, m))
}

/// Format the summary date from its parts.
fn summary_date(weekday: Option<&str>, month: &str, day: u32, time: Option<String>) -> String {
    let mut out = String::new();
    if let Some(w) = weekday {
        out.push_str(w);
        out.push(' ');
    }
    out.push_str(&format!("{} {:>2}", month, day));
    if let Some(t) = time {
        out.push(' ');
        out.push_str(&t);
    }
    out
}

/// The summary date of a `From ` line's `Www Mmm dd hh:mm:ss yyyy`.
fn envelope_date(s: &str) -> Option<String> {
    let mut parts = s.split_whitespace();
    let weekday = parts.next().filter(|w| WEEKDAYS.contains(w))?;
    let month = parts.next().filter(|m| MONTHS.contains(m))?;
    let day = day_of_month(parts.next()?)?;
    let time = hours_minutes(parts.next()?)?;
    Some(summary_date(Some(weekday), month, day, Some(time)))
}

/// The abbreviation `s` stands for, when it is one of `abbrevs` or the
/// matching full name in `names` (`Nov`, `November`), ignoring case.
///
/// Only whole words count: matching on the first three letters took any
/// word that began like one, so `marked` read as March.
fn abbreviation(s: &str, abbrevs: &[&'static str], names: &[&str]) -> Option<&'static str> {
    abbrevs
        .iter()
        .zip(names)
        .find(|(a, n)| s.eq_ignore_ascii_case(a) || s.eq_ignore_ascii_case(n))
        .map(|(a, _)| *a)
}

/// The summary date of a `Date:` value.
///
/// RFC 5322 writes `[Www,] dd Mmm yyyy hh:mm`, but mail in the wild also
/// carries `Www Mmm dd hh:mm:ss yyyy`, omits the time, or the space after the
/// comma, so the parts are found rather than expected in place. The day is
/// the number next to the month that can be one, preferring the one before
/// it: taking the number after it read `29 Jan 2007` as `Jan 2007`.
fn header_date(s: &str) -> Option<String> {
    let parts: Vec<&str> = s
        .split(|c: char| c.is_whitespace() || c == ',')
        .filter(|p| !p.is_empty())
        .collect();
    let weekday = parts
        .first()
        .and_then(|w| abbreviation(w, &WEEKDAYS, &WEEKDAY_NAMES));
    let (at, month) = parts
        .iter()
        .enumerate()
        .find_map(|(i, p)| abbreviation(p, &MONTHS, &MONTH_NAMES).map(|m| (i, m)))?;
    let before = at.checked_sub(1).and_then(|i| day_of_month(parts[i]));
    let day = before.or_else(|| parts.get(at + 1).and_then(|p| day_of_month(p)))?;
    let time = parts.iter().find_map(|p| hours_minutes(p));
    Some(summary_date(weekday, month, day, time))
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

/// The bare address of one mailbox written in a header: `Name <a@b>`,
/// `"Smith, J" <a@b>` and `a@b (Name)` are all `a@b`.
///
/// Follows the RFC 5322 syntax far enough to tell the address from its
/// decoration: a quoted string or a `(comment)` may contain `<`, `>`, or a
/// comma without meaning anything, and the angle address is the last `<...>`.
/// An unclosed `<`, as in `Name <a@b`, still introduces the address. With no
/// angle address, the text outside comments is the address.
pub fn extract_address(s: &str) -> String {
    let mut plain = String::new();
    let mut angle: Option<String> = None;
    let mut in_quote = false;
    let mut depth = 0usize;
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if let Some(a) = angle.as_mut() {
            match c {
                // Anything after the angle address is not part of it.
                '>' => return a.trim().to_string(),
                // A later `<` starts the angle address afresh.
                '<' => a.clear(),
                _ => a.push(c),
            }
            continue;
        }
        match c {
            '\\' if in_quote || depth > 0 => {
                let escaped = chars.next();
                if in_quote {
                    plain.push(c);
                    plain.extend(escaped);
                }
            }
            '"' if depth == 0 => {
                in_quote = !in_quote;
                plain.push(c);
            }
            _ if in_quote => plain.push(c),
            '(' => depth += 1,
            ')' if depth > 0 => depth -= 1,
            _ if depth > 0 => {}
            '<' => angle = Some(String::new()),
            _ => plain.push(c),
        }
    }
    if let Some(a) = angle {
        return a.trim().to_string();
    }
    plain.split_whitespace().collect::<Vec<_>>().join(" ")
}

/// Extract login name from an email address
pub fn extract_login(addr: &str) -> String {
    let mut addr = extract_address(addr);
    if let Some(at) = addr.find('@') {
        addr.truncate(at);
    }
    addr
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
pub fn author_filename(from: &str) -> Result<String, String> {
    let login = extract_login(from);
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
