//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Message representation for mailx

use crate::error::Result;

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

    /// Whether the message has yet to be read: `new` or `unread`.
    pub fn is_unread(&self) -> bool {
        matches!(self, ReadState::New | ReadState::Unread)
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

    /// Whether `delete` has marked the message.
    pub fn is_deleted(&self) -> bool {
        self.disposition == Disposition::Deleted
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
    /// Header fields in order, as (lowercase name, unfolded value).
    ///
    /// A field may appear more than once; lookups take the first.
    pub headers: Vec<(String, String)>,
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
    /// The read state recorded in the mailbox file, by its `Status:` header.
    pub loaded_read: ReadState,
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
            headers: Vec::new(),
            header_lines: Vec::new(),
            body: Vec::new(),
            raw_lines: 0,
            raw_bytes: 0,
            read: ReadState::New,
            loaded_read: ReadState::New,
            disposition: Disposition::Keep,
            displayed: false,
            force_mbox: false,
        }
    }

    /// The value of the first `name` field, compared without regard to case.
    pub fn get_header(&self, name: &str) -> Option<&str> {
        self.headers
            .iter()
            .find(|(field, _)| field.eq_ignore_ascii_case(name))
            .map(|(_, value)| value.as_str())
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

    /// Render the message for display.
    ///
    /// `show_all_headers` is the capitalized `Print`/`Type` form, which
    /// overrides `discard`/`ignore`/`retain` (spec 104756).
    pub fn format_display(
        &self,
        show_all_headers: bool,
        vars: &crate::variables::Variables,
    ) -> Vec<u8> {
        let mut output = Vec::new();

        for line in &self.header_lines {
            let field = line.split(|&b| b == b':').next().unwrap_or_default();
            if show_all_headers || vars.header_filter.shows(field.trim_ascii()) {
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
}

/// The bare address of one mailbox written in a header: `Name <a@b>`,
/// `"Smith, J" <a@b>` and `a@b (Name)` are all `a@b`.
///
/// Follows the RFC 5322 syntax far enough to tell the address from its
/// decoration: a quoted string or a `(comment)` may contain `<`, `>`, or a
/// comma without meaning anything. The angle address ends at the first `>`
/// outside them and begins at the last `<` before it, so a stray `<` in a
/// display name (`Bob >_< Smith <bob@x>`) is passed over; anything after that
/// `>` is ignored. An unclosed `<`, as in `Name <a@b`, still introduces the
/// address. With no angle address, the text outside comments is the address.
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
pub fn author_filename(from: &str) -> Result<String> {
    let login = extract_login(from);
    let acceptable = |c: char| c.is_ascii_alphanumeric() || matches!(c, '.' | '-' | '_' | '+');
    if login.is_empty()
        || login.starts_with('-')
        || login.starts_with('.')
        || !login.chars().all(acceptable)
    {
        return Err(format!("{}: invalid author name", from).into());
    }
    Ok(login)
}
