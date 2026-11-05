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

/// Split a mailbox file into its messages.
pub(crate) fn parse(bytes: &[u8]) -> Vec<Message> {
    let mut messages = Vec::new();
    let mut current_msg: Option<Message> = None;
    let mut in_headers = false;
    let mut header_continuation = false;
    // A `From ` line only begins a message at the start of the file or
    // after an empty line (spec 104427-104428). Splitting on every body
    // line that happened to start with `From ` tore messages in half --
    // and the format explicitly permits such a line when it does not
    // follow an empty line.
    let mut at_message_boundary = true;

    for raw in bytes.split_inclusive(|&b| b == b'\n') {
        // The same line `str::lines` would yield; `raw` keeps the
        // terminator so the message's on-disk size can be counted.
        let line = match raw.strip_suffix(b"\n") {
            Some(l) => l.strip_suffix(b"\r").unwrap_or(l),
            None => raw,
        };
        let boundary = at_message_boundary;
        at_message_boundary = line.is_empty();

        // Check for message separator (From line)
        if line.starts_with(b"From ") && !in_headers && boundary {
            // Save previous message if any
            if let Some(msg) = current_msg.take() {
                messages.push(msg);
            }

            // Start new message.  Absent a Status: header it is `new`
            // (spec 104491-104496); a Status: header may downgrade it below.
            let mut msg = Message::new();
            msg.from_line = line.to_vec();
            msg.read = ReadState::New;
            msg.raw_lines = 1;
            msg.raw_bytes = raw.len();
            current_msg = Some(msg);
            in_headers = true;
            header_continuation = false;
            continue;
        }

        if let Some(ref mut msg) = current_msg {
            // Every physical line up to the next `From ` line is part of
            // this message as stored: folded header lines, `Status:`, and
            // the separating blank line included. The header summary and
            // `size` report these counts, as traditional mailx does.
            msg.raw_lines += 1;
            msg.raw_bytes += raw.len();

            if in_headers {
                if line.is_empty() {
                    // End of headers
                    in_headers = false;
                } else if line.starts_with(b" ") || line.starts_with(b"\t") {
                    // Header continuation. Skipped entirely when the header
                    // it continues was not retained.
                    if !header_continuation {
                        continue;
                    }
                    if let Some((_, val)) = msg.headers.last_mut() {
                        val.push(' ');
                        val.push_str(String::from_utf8_lossy(line).trim());
                    }
                    // Append to last header line
                    if let Some(last) = msg.header_lines.last_mut() {
                        last.push(b'\n');
                        last.extend_from_slice(line);
                    }
                } else if let Some(colon_pos) = line.iter().position(|&b| b == b':') {
                    // New header. The parsed value is text, for matching
                    // and display; the line itself is kept as stored.
                    let key = String::from_utf8_lossy(&line[..colon_pos]).to_lowercase();
                    let value = String::from_utf8_lossy(&line[colon_pos + 1..])
                        .trim()
                        .to_string();

                    // A Status: header records prior disposition: `R`
                    // (read) wins; `O` alone (seen but not read) is unread.
                    // Absence of Status: leaves the message `new`.
                    if key == "status" {
                        if value.contains('R') {
                            msg.read = ReadState::Read;
                        } else if value.contains('O') {
                            msg.read = ReadState::Unread;
                        }
                        msg.loaded_read = msg.read;
                        // Not retained in `header_lines`: the writer
                        // regenerates it from `read`, and keeping the loaded
                        // copy would emit a stale one beside the fresh one.
                        // Its continuation lines go with it -- leaving the
                        // previous header current would append them to
                        // whatever came before `Status:`.
                        header_continuation = false;
                        continue;
                    }

                    msg.headers.push((key, value));
                    msg.header_lines.push(line.to_vec());
                    header_continuation = true;
                } else {
                    // Malformed header, treat as body start
                    in_headers = false;
                    msg.body.extend_from_slice(line);
                    msg.body.push(b'\n');
                }
            } else {
                // Body. Undo the mbox `From ` quoting: a run of `>`
                // followed by `From ` loses one `>`. Stripping it only from
                // `>From ` (and never adding one to `>>From `) meant a body
                // line that genuinely read `>From x` was silently turned
                // into `From x`.
                msg.body.extend_from_slice(unquote_from_line(line));
                msg.body.push(b'\n');
            }
        }
    }

    // Don't forget the last message
    if let Some(msg) = current_msg {
        messages.push(msg);
    }

    // The empty line that separates one message from the next belongs to
    // the mbox framing, not to the message. Leaving it in the body meant
    // the writer round-tripped it by accident for messages that had one and
    // omitted it entirely for those that did not -- and an unconditional
    // separator on write would then have doubled it, growing the mailbox by
    // a blank line every session.
    for msg in &mut messages {
        if msg.body == b"\n" {
            // A message with no body at all: the one line present is the
            // framing blank, not content.
            msg.body.clear();
        } else if msg.body.ends_with(b"\n\n") {
            msg.body.pop();
        }
    }

    messages
}

/// Serialize messages in mbox format (spec 104420-104436).
///
/// Each message is an envelope `From ` line, its header fields with a
/// regenerated `Status:`, an empty line, the `From `-quoted body, and one
/// trailing empty line -- so the next `From ` line is preceded by an empty
/// line, as the format requires.
pub(crate) fn write_messages<W: Write>(
    w: &mut W,
    messages: &[Message],
    include_from_line: bool,
) -> io::Result<()> {
    for msg in messages {
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
            writeln!(w, "Status: {}", status)?;
        }
        writeln!(w)?;
        for line in crate::util::byte_lines(&msg.body) {
            if needs_from_quoting(line) {
                write!(w, ">")?;
            }
            write_line(w, line)?;
        }
        // The separator before the next message.
        writeln!(w)?;
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
