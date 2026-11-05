//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The mbox format: parsing a mailbox file into messages, and writing them back.

use std::io::{self, Write};

use crate::message::{Message, ReadState};

/// Where the parser is within the current message.
#[derive(Debug, Clone, Copy)]
enum Section {
    /// The header block. `folding` says whether a continuation line belongs
    /// to the field before it; it does not after a dropped `Status:`.
    Headers {
        folding: bool,
    },
    Body,
}

/// Split a mailbox file into its messages.
pub(crate) fn parse(bytes: &[u8]) -> Vec<Message> {
    let mut messages = Vec::new();
    let mut current: Option<(Message, Section)> = None;
    // A `From ` line begins a message only at the start of the file or after
    // an empty line (spec 104427-104428); anywhere else it is message text.
    let mut after_blank = true;

    for raw in bytes.split_inclusive(|&b| b == b'\n') {
        // `stored` is the line as kept, a CR of a CRLF ending included, so
        // the message is written back as it was stored; `line` is its text.
        let stored = raw.strip_suffix(b"\n").unwrap_or(raw);
        let line = stored.strip_suffix(b"\r").unwrap_or(stored);
        let in_headers = matches!(current, Some((_, Section::Headers { .. })));
        let starts_message = after_blank && !in_headers && line.starts_with(b"From ");
        after_blank = line.is_empty();

        if starts_message {
            messages.extend(current.take().map(|(msg, _)| finish(msg)));
            current = Some((start(stored, raw), Section::Headers { folding: false }));
        } else if let Some((msg, section)) = current.as_mut() {
            // Every physical line up to the next `From ` line is part of the
            // message as stored, framing included; the header summary and
            // `size` report these counts, as traditional mailx does.
            msg.raw_lines += 1;
            msg.raw_bytes += raw.len();
            *section = match *section {
                Section::Headers { folding } => header_line(msg, folding, stored),
                Section::Body => {
                    push_line(&mut msg.body, unquote_from_line(stored));
                    Section::Body
                }
            };
        }
    }
    messages.extend(current.map(|(msg, _)| finish(msg)));
    messages
}

/// A message begun by the envelope `line`, `raw` with its terminator.
fn start(line: &[u8], raw: &[u8]) -> Message {
    let mut msg = Message::new();
    msg.from_line = line.to_vec();
    msg.raw_lines = 1;
    msg.raw_bytes = raw.len();
    msg
}

/// Add one line of the header block, as stored, returning where the next
/// line falls.
fn header_line(msg: &mut Message, folding: bool, line: &[u8]) -> Section {
    if line.strip_suffix(b"\r").unwrap_or(line).is_empty() {
        return Section::Body;
    }
    if line.starts_with(b" ") || line.starts_with(b"\t") {
        if folding {
            append_continuation(msg, line);
        }
        return Section::Headers { folding };
    }
    let Some(colon) = line.iter().position(|&b| b == b':') else {
        // Not a header field: the body began without its empty line.
        push_line(&mut msg.body, line);
        return Section::Body;
    };

    // The parsed value is text, for matching and display; the line itself is
    // kept as stored.
    let name = String::from_utf8_lossy(&line[..colon]).to_lowercase();
    let value = String::from_utf8_lossy(&line[colon + 1..])
        .trim()
        .to_string();
    if name == "status" {
        if let Some(state) = status_read_state(&value) {
            msg.read = state;
            msg.loaded_read = state;
        }
        // Dropped, with any continuation lines: the writer regenerates
        // `Status:` from the read state, and keeping this copy would write a
        // stale one beside it.
        return Section::Headers { folding: false };
    }
    msg.headers.push((name, value));
    msg.header_lines.push(line.to_vec());
    Section::Headers { folding: true }
}

/// Fold a continuation line into the last header field.
fn append_continuation(msg: &mut Message, line: &[u8]) {
    if let Some((_, value)) = msg.headers.last_mut() {
        value.push(' ');
        value.push_str(String::from_utf8_lossy(line).trim());
    }
    if let Some(last) = msg.header_lines.last_mut() {
        last.push(b'\n');
        last.extend_from_slice(line);
    }
}

/// The read state a `Status:` value records: `R` is read, and `O` without
/// it is seen but unread. Anything else says nothing, leaving the message
/// `new` (spec 104491-104496).
fn status_read_state(value: &str) -> Option<ReadState> {
    if value.contains('R') {
        Some(ReadState::Read)
    } else if value.contains('O') {
        Some(ReadState::Unread)
    } else {
        None
    }
}

/// The lines of `body` split at LF, each with any CR it was stored with.
fn stored_lines(body: &[u8]) -> impl Iterator<Item = &[u8]> {
    let text = body.strip_suffix(b"\n").unwrap_or(body);
    (!body.is_empty())
        .then(|| text.split(|&b| b == b'\n'))
        .into_iter()
        .flatten()
}

fn push_line(body: &mut Vec<u8>, line: &[u8]) {
    body.extend_from_slice(line);
    body.push(b'\n');
}

/// Drop the empty line that separates a message from the next: it belongs
/// to the mbox framing, which the writer adds back, not to the message.
fn finish(mut msg: Message) -> Message {
    if msg.body == b"\n" || msg.body == b"\r\n" {
        msg.body.clear();
    } else if msg.body.ends_with(b"\n\r\n") {
        msg.body.truncate(msg.body.len() - 2);
    } else if msg.body.ends_with(b"\n\n") {
        msg.body.pop();
    }
    msg
}

/// Serialize messages in mbox format (spec 104420-104436).
///
/// Each message is an envelope `From ` line, its header fields with a
/// regenerated `Status:`, an empty line, the `From `-quoted body, and one
/// trailing empty line -- so the next `From ` line is preceded by an empty
/// line, as the format requires.
pub(crate) fn write_messages<W: Write>(
    w: &mut W,
    messages: &[&Message],
    include_from_line: bool,
) -> io::Result<()> {
    for msg in messages {
        // Stored lines keep their own CR; the lines written here take the
        // message's line ending, CRLF when its `From ` line had one.
        let eol: &[u8] = if msg.from_line.ends_with(b"\r") {
            b"\r\n"
        } else {
            b"\n"
        };
        if include_from_line {
            write_line(w, &msg.from_line)?;
        }
        for header in &msg.header_lines {
            write_line(w, header)?;
        }
        // `Status:` is written from the message's read state rather than echoed
        // from the file, so a message read this session comes back as read.
        // The loader keeps it out of `header_lines` for exactly this reason.
        if let Some(status) = msg.read.status_value() {
            write!(w, "Status: {}", status)?;
            w.write_all(eol)?;
        }
        w.write_all(eol)?;
        for line in stored_lines(&msg.body) {
            if needs_from_quoting(line) {
                write!(w, ">")?;
            }
            write_line(w, line)?;
        }
        // The separator before the next message.
        w.write_all(eol)?;
    }

    Ok(())
}

/// Write `line` followed by a newline.
fn write_line<W: Write>(w: &mut W, line: &[u8]) -> io::Result<()> {
    w.write_all(line)?;
    w.write_all(b"\n")
}

/// Remove one level of mbox `From ` quoting from a body line.
///
/// mboxrd: `>From ` came from `From `, `>>From ` from `>From `, and so on.
fn unquote_from_line(line: &[u8]) -> &[u8] {
    let rest = trim_quote_marks(line);
    if rest.starts_with(b"From ") && rest.len() < line.len() {
        &line[1..]
    } else {
        line
    }
}

/// `line` without its leading run of `>`.
fn trim_quote_marks(line: &[u8]) -> &[u8] {
    let n = line.iter().take_while(|&&b| b == b'>').count();
    &line[n..]
}

/// Whether a body line needs mbox `From ` quoting on the way out.
pub(crate) fn needs_from_quoting(line: &[u8]) -> bool {
    trim_quote_marks(line).starts_with(b"From ")
}

/// A writer that counts the bytes passing through it.
///
/// What `save` writes is not the message as loaded -- `Status:` is
/// regenerated and the separator always added -- so its size is counted on
/// the way out rather than predicted.
pub(crate) struct CountingWriter<W: Write> {
    pub(crate) inner: W,
    pub(crate) count: u64,
}

impl<W: Write> Write for CountingWriter<W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let n = self.inner.write(buf)?;
        self.count += n as u64;
        Ok(n)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.inner.flush()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn bodies(mbox: &str) -> Vec<String> {
        parse(mbox.as_bytes())
            .iter()
            .map(|m| String::from_utf8_lossy(&m.body).into_owned())
            .collect()
    }

    #[test]
    fn from_line_splits_only_after_an_empty_line() {
        let mbox = "From a Mon Jan  1 00:00:00 2024\nSubject: one\n\nbody\nFrom here on\n\n\
                    From b Mon Jan  1 00:00:00 2024\nSubject: two\n\nsecond\n";
        assert_eq!(bodies(mbox), ["body\nFrom here on\n", "second\n"]);
    }

    #[test]
    fn quoted_from_lines_lose_one_mark() {
        let mbox = "From a Mon Jan  1 00:00:00 2024\n\n>From x\n>>From y\n>Fromage\n";
        assert_eq!(bodies(mbox), ["From x\n>From y\n>Fromage\n"]);
    }

    #[test]
    fn status_is_dropped_with_its_continuation() {
        let mbox = "From a Mon Jan  1 00:00:00 2024\nSubject: s\nStatus: RO\n x\nTo: t\n\nb\n";
        let msg = &parse(mbox.as_bytes())[0];
        assert_eq!(msg.read, ReadState::Read);
        assert_eq!(msg.loaded_read, ReadState::Read);
        assert_eq!(
            msg.header_lines,
            [b"Subject: s".to_vec(), b"To: t".to_vec()]
        );
        assert_eq!(msg.get_header("subject"), Some("s"));
    }

    #[test]
    fn folded_field_keeps_its_lines_and_unfolds_its_value() {
        let mbox = "From a Mon Jan  1 00:00:00 2024\nSubject: one\n\ttwo\n\nb\n";
        let msg = &parse(mbox.as_bytes())[0];
        assert_eq!(msg.header_lines, [b"Subject: one\n\ttwo".to_vec()]);
        assert_eq!(msg.get_header("subject"), Some("one two"));
    }

    #[test]
    fn a_line_that_is_not_a_field_starts_the_body() {
        let mbox = "From a Mon Jan  1 00:00:00 2024\nSubject: s\nno colon here\nmore\n";
        assert_eq!(bodies(mbox), ["no colon here\nmore\n"]);
    }

    #[test]
    fn crlf_lines_round_trip() {
        let mbox = "From a Mon Jan  1 00:00:00 2024\r\nSubject: s\r\n x\r\nStatus: O\r\n\r\n\
                    >From b\r\nbody\r\n\r\n";
        let messages = parse(mbox.as_bytes());
        assert_eq!(messages[0].get_header("subject"), Some("s x"));
        let mut out = Vec::new();
        write_messages(&mut out, &[&messages[0]], true).unwrap();
        assert_eq!(String::from_utf8_lossy(&out), mbox);
    }

    #[test]
    fn counts_cover_every_stored_line() {
        let mbox = "From a Mon Jan  1 00:00:00 2024\nStatus: O\n\nbody\n\n";
        let msg = &parse(mbox.as_bytes())[0];
        assert_eq!((msg.raw_lines, msg.raw_bytes), (5, mbox.len()));
        assert_eq!(msg.body, b"body\n");
    }
}
