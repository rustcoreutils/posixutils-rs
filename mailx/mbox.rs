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
use crate::store::{Span, Store};

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

/// The message being parsed.
struct Parsing {
    msg: Message,
    section: Section,
    /// The length of the last body line stored, and whether it is empty:
    /// an empty last line is the framing before the next message.
    last_body_line: Option<(u64, bool)>,
}

/// Split a mailbox file into its messages, copying each into `store`.
///
/// The store keeps a message's `From ` line and header lines as stored,
/// without `Status:`, followed by its body as stored, `From `-quoting and
/// all; the empty line ending the header block is framing, not kept.
///
/// Returns the messages and the number of bytes read.
pub(crate) fn parse(
    mut input: impl io::BufRead,
    store: &mut Store,
) -> io::Result<(Vec<Message>, u64)> {
    let mut messages = Vec::new();
    let mut current: Option<Parsing> = None;
    // A `From ` line begins a message only at the start of the file or after
    // an empty line (spec 104427-104428); anywhere else it is message text.
    let mut after_blank = true;
    let mut consumed = 0;
    let mut buf = Vec::new();

    loop {
        buf.clear();
        let n = input.read_until(b'\n', &mut buf)?;
        if n == 0 {
            break;
        }
        consumed += n as u64;
        let raw = buf.as_slice();
        // `stored` is the line as kept, a CR of a CRLF ending included, so
        // the message is written back as it was stored; `line` is its text.
        let stored = raw.strip_suffix(b"\n").unwrap_or(raw);
        let line = stored.strip_suffix(b"\r").unwrap_or(stored);
        let in_headers = matches!(
            current,
            Some(Parsing {
                section: Section::Headers { .. },
                ..
            })
        );
        let starts_message = after_blank && !in_headers && line.starts_with(b"From ");
        after_blank = line.is_empty();

        if starts_message {
            messages.extend(current.take().map(finish));
            let mut msg = start(stored, raw);
            msg.head = store.append(raw)?;
            msg.body_span = msg.head.after(0);
            current = Some(Parsing {
                msg,
                section: Section::Headers { folding: false },
                last_body_line: None,
            });
        } else if let Some(p) = current.as_mut() {
            // Every physical line up to the next `From ` line is part of the
            // message as stored, framing included; the header summary and
            // `size` report these counts, as traditional mailx does.
            p.msg.raw_lines += 1;
            p.msg.raw_bytes += raw.len();
            let was_headers = matches!(p.section, Section::Headers { .. });
            p.section = match p.section {
                Section::Headers { folding } => header_line(&mut p.msg, folding, stored),
                Section::Body => Section::Body,
            };
            store_line(store, p, raw, line, was_headers)?;
        }
    }
    messages.extend(current.map(finish));
    store.flush()?;
    Ok((messages, consumed))
}

/// Copy one line of the current message into the store, where it belongs.
fn store_line(
    store: &mut Store,
    p: &mut Parsing,
    raw: &[u8],
    line: &[u8],
    was_headers: bool,
) -> io::Result<()> {
    match p.section {
        // A kept header line or continuation extends the head; a dropped
        // `Status:` and the empty line ending the block are not stored.
        Section::Headers { folding } => {
            if folding {
                p.msg.head.len += store.append(raw)?.len;
                p.msg.body_span = p.msg.head.after(0);
            }
        }
        Section::Body if was_headers && line.is_empty() => {}
        Section::Body => {
            p.msg.body_span.len += store.append(raw)?.len;
            p.last_body_line = Some((raw.len() as u64, line.is_empty()));
        }
    }
    Ok(())
}

/// A message begun by the envelope `line`, `raw` with its terminator.
fn start(line: &[u8], raw: &[u8]) -> Message {
    let mut msg = Message::new();
    msg.crlf = line.ends_with(b"\r");
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
        return Section::Headers { folding };
    }
    let Some(colon) = line.iter().position(|&b| b == b':') else {
        // Not a header field: the body began without its empty line.
        return Section::Body;
    };

    if line[..colon].eq_ignore_ascii_case(b"status") {
        let value = String::from_utf8_lossy(&line[colon + 1..]);
        if let Some(state) = status_read_state(&value) {
            msg.read = state;
            msg.loaded_read = state;
        }
        // Dropped, with any continuation lines: the writer regenerates
        // `Status:` from the read state, and keeping this copy would write a
        // stale one beside it.
        return Section::Headers { folding: false };
    }
    Section::Headers { folding: true }
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

/// Drop the empty line that separates a message from the next: it belongs
/// to the mbox framing, which the writer adds back, not to the message.
fn finish(p: Parsing) -> Message {
    let mut msg = p.msg;
    if let Some((len, true)) = p.last_body_line {
        msg.body_span.len -= len;
    }
    msg
}

/// Serialize messages from `store` in mbox format (spec 104420-104436).
///
/// Each message is its `From ` line and header fields as stored, a
/// regenerated `Status:`, an empty line, its body as stored -- `From `-quoted
/// already -- and one trailing empty line, so the next `From ` line is
/// preceded by an empty line, as the format requires.
pub(crate) fn write_messages<W: Write>(
    w: &mut W,
    store: &Store,
    messages: &[&Message],
) -> io::Result<()> {
    for msg in messages {
        // Stored lines keep their own CR; the lines written here take the
        // message's line ending, CRLF when its `From ` line had one.
        let eol: &[u8] = if msg.crlf { b"\r\n" } else { b"\n" };
        copy_lines(store, msg.head, w)?;
        // `Status:` is written from the message's read state rather than echoed
        // from the file, so a message read this session comes back as read.
        // The loader keeps it out of the store for exactly this reason.
        if let Some(status) = msg.read.status_value() {
            write!(w, "Status: {}", status)?;
            w.write_all(eol)?;
        }
        w.write_all(eol)?;
        copy_body(store, msg.body_span, w)?;
        // The separator before the next message.
        w.write_all(eol)?;
    }
    Ok(())
}

/// Copy the lines of `span`, ending the last with a newline if it has none.
fn copy_lines<W: Write>(store: &Store, span: Span, w: &mut W) -> io::Result<()> {
    store.copy_to(span, w)?;
    if span.len > 0
        && store.read(Span {
            offset: span.offset + span.len - 1,
            len: 1,
        })? != b"\n"
    {
        w.write_all(b"\n")?;
    }
    Ok(())
}

/// Copy a stored body, `From `-quoting any line stored unquoted.
///
/// Stored bodies are quoted already, except that a `From ` line not after an
/// empty line may be stored as it is; it is quoted on the way out so that no
/// mbox reader takes it for a message boundary.
fn copy_body<W: Write>(store: &Store, span: Span, w: &mut W) -> io::Result<()> {
    let mut reader = store.reader(span);
    let mut line = Vec::new();
    loop {
        line.clear();
        if io::BufRead::read_until(&mut reader, b'\n', &mut line)? == 0 {
            return Ok(());
        }
        if line.starts_with(b"From ") {
            w.write_all(b">")?;
        }
        w.write_all(&line)?;
        if !line.ends_with(b"\n") {
            w.write_all(b"\n")?;
        }
    }
}

/// Serialize a message held in memory -- one composed here, not read from
/// a mailbox -- in mbox format, `From `-quoting its body.
pub(crate) fn write_composed<W: Write>(
    w: &mut W,
    from_line: &str,
    header_lines: &[String],
    body: &[u8],
) -> io::Result<()> {
    write_line(w, from_line.as_bytes())?;
    for header in header_lines {
        write_line(w, header.as_bytes())?;
    }
    writeln!(w)?;
    for line in stored_lines(body) {
        if needs_from_quoting(line) {
            write!(w, ">")?;
        }
        write_line(w, line)?;
    }
    writeln!(w)
}

/// Write `line` followed by a newline.
fn write_line<W: Write>(w: &mut W, line: &[u8]) -> io::Result<()> {
    w.write_all(line)?;
    w.write_all(b"\n")
}

/// Remove one level of mbox `From ` quoting from a body line.
///
/// mboxrd: `>From ` came from `From `, `>>From ` from `>From `, and so on.
pub(crate) fn unquote_from_line(line: &[u8]) -> &[u8] {
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
    use crate::message::Headers;

    fn parsed(mbox: &str) -> (Vec<Message>, Store) {
        let mut store = Store::default();
        let (messages, len) = parse(mbox.as_bytes(), &mut store).unwrap();
        assert_eq!(len, mbox.len() as u64);
        (messages, store)
    }

    /// Each message's body as read back from the store and unquoted.
    fn bodies(mbox: &str) -> Vec<String> {
        let (messages, store) = parsed(mbox);
        messages
            .iter()
            .map(|m| {
                let stored = store.read(m.body_span).unwrap();
                let lines = stored.split_inclusive(|&b| b == b'\n');
                let unquoted: Vec<&[u8]> = lines.map(unquote_from_line).collect();
                String::from_utf8_lossy(&unquoted.concat()).into_owned()
            })
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
        let (messages, store) = parsed(mbox);
        let msg = &messages[0];
        assert_eq!(msg.read, ReadState::Read);
        assert_eq!(msg.loaded_read, ReadState::Read);
        let headers = Headers::parse(&store.read(msg.head).unwrap());
        assert_eq!(headers.lines, [b"Subject: s".to_vec(), b"To: t".to_vec()]);
        assert_eq!(headers.get_header("subject"), Some("s"));
    }

    #[test]
    fn folded_field_keeps_its_lines_and_unfolds_its_value() {
        let mbox = "From a Mon Jan  1 00:00:00 2024\nSubject: one\n\ttwo\n\nb\n";
        let (messages, store) = parsed(mbox);
        let headers = Headers::parse(&store.read(messages[0].head).unwrap());
        assert_eq!(headers.lines, [b"Subject: one\n\ttwo".to_vec()]);
        assert_eq!(headers.get_header("subject"), Some("one two"));
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
        let (messages, store) = parsed(mbox);
        let headers = Headers::parse(&store.read(messages[0].head).unwrap());
        assert_eq!(headers.get_header("subject"), Some("s x"));
        let mut out = Vec::new();
        write_messages(&mut out, &store, &[&messages[0]]).unwrap();
        assert_eq!(String::from_utf8_lossy(&out), mbox);
    }

    #[test]
    fn counts_cover_every_stored_line() {
        let mbox = "From a Mon Jan  1 00:00:00 2024\nStatus: O\n\nbody\n\n";
        let (messages, store) = parsed(mbox);
        let msg = &messages[0];
        assert_eq!((msg.raw_lines, msg.raw_bytes), (5, mbox.len()));
        assert_eq!(store.read(msg.body_span).unwrap(), b"body\n");
    }
}
