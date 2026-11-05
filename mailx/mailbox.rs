//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Mailbox handling for mailx - mbox format parsing and writing

use std::fs;
use std::io::{self, Read, Seek, Write};

use crate::error::{IoContext, Result};
use crate::lock::{commit, lock, open_locked, read_locked};
use crate::message::{truncate_display, Disposition, Message, ReadState};
use crate::variables::Variables;

/// A mailbox containing messages
#[derive(Debug)]
pub struct Mailbox {
    /// Path to the mailbox file
    pub path: String,
    /// Messages in the mailbox
    pub messages: Vec<Message>,
    /// Current message index (1-based, 0 means no current message)
    pub current: usize,
    /// Whether this is the system mailbox
    pub is_system_mailbox: bool,
    /// Whether the mailbox has been modified
    pub modified: bool,
    /// Path of the previously-opened folder, for the `#` substitution.
    pub prev_path: Option<String>,
    /// Size of the mailbox file when it was read, in bytes.
    ///
    /// Mail delivered while mailx is running lands past this offset. The
    /// rewrite at quit copies those bytes through verbatim, so a concurrent
    /// delivery is not destroyed by writing back only the messages we parsed.
    loaded_len: u64,
}

impl Mailbox {
    /// Create a new empty mailbox
    pub fn new(path: String) -> Self {
        Mailbox {
            path,
            messages: Vec::new(),
            current: 0,
            is_system_mailbox: false,
            modified: false,
            prev_path: None,
            loaded_len: 0,
        }
    }

    /// Load a mailbox from a file
    pub fn load(path: &str) -> Result<Self> {
        // Read the file in one go rather than streaming it, so the byte length
        // we record is exactly the extent we parsed. A delivery that lands
        // mid-read would otherwise leave `loaded_len` disagreeing with the
        // messages in hand, and the tail would be written back twice.
        //
        // Mail is not necessarily UTF-8, so the mailbox is parsed as bytes.
        // Reading it as a string refused the whole mailbox when any message
        // carried a raw 8-bit byte, so one Latin-1 message from anyone made
        // all of a user's mail unreadable.
        //
        // The read is done under a shared lock, so a delivery in progress is
        // waited for rather than half read.
        let bytes = read_locked(path).context(path)?;
        let mut mailbox = Mailbox::new(path.to_string());
        mailbox.loaded_len = bytes.len() as u64;

        let mut current_msg: Option<Message> = None;
        let mut in_headers = false;
        let mut header_continuation = false;
        let mut last_header_key = String::new();
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
                    mailbox.messages.push(msg);
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
                        if !last_header_key.is_empty() {
                            if let Some(val) = msg.headers.get_mut(&last_header_key) {
                                val.push(' ');
                                val.push_str(String::from_utf8_lossy(line).trim());
                            }
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
                            // Not retained in `header_lines`: the writer
                            // regenerates it from `read`, and keeping the loaded
                            // copy would emit a stale one beside the fresh one.
                            // Its continuation lines go with it -- leaving the
                            // previous header current would append them to
                            // whatever came before `Status:`.
                            msg.headers.insert(key, value);
                            last_header_key.clear();
                            header_continuation = false;
                            continue;
                        }

                        msg.headers.insert(key.clone(), value);
                        msg.header_lines.push(line.to_vec());
                        last_header_key = key;
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
            mailbox.messages.push(msg);
        }

        // The empty line that separates one message from the next belongs to
        // the mbox framing, not to the message. Leaving it in the body meant
        // the writer round-tripped it by accident for messages that had one and
        // omitted it entirely for those that did not -- and an unconditional
        // separator on write would then have doubled it, growing the mailbox by
        // a blank line every session.
        for msg in &mut mailbox.messages {
            if msg.body == b"\n" {
                // A message with no body at all: the one line present is the
                // framing blank, not content.
                msg.body.clear();
            } else if msg.body.ends_with(b"\n\n") {
                msg.body.pop();
            }
        }

        // Set current to first new/unread message, or first message
        mailbox.current = mailbox
            .messages
            .iter()
            .position(|m| m.read == ReadState::New || m.read == ReadState::Unread)
            .map(|i| i + 1)
            .unwrap_or(if mailbox.messages.is_empty() { 0 } else { 1 });

        Ok(mailbox)
    }

    /// Get the number of messages
    pub fn message_count(&self) -> usize {
        self.messages.len()
    }

    /// Get number of undeleted messages
    pub fn undeleted_count(&self) -> usize {
        self.messages
            .iter()
            .filter(|m| m.disposition != Disposition::Deleted)
            .count()
    }

    /// Set whether this is the system mailbox
    pub fn set_is_system_mailbox(&mut self, is_system: bool) {
        self.is_system_mailbox = is_system;
    }

    /// Get a message by number (1-based)
    pub fn get(&self, num: usize) -> Option<&Message> {
        if num > 0 && num <= self.messages.len() {
            Some(&self.messages[num - 1])
        } else {
            None
        }
    }

    /// Get a mutable message by number (1-based)
    pub fn get_mut(&mut self, num: usize) -> Option<&mut Message> {
        if num > 0 && num <= self.messages.len() {
            Some(&mut self.messages[num - 1])
        } else {
            None
        }
    }

    /// Find first undeleted message after the given number
    pub fn next_undeleted(&self, after: usize) -> Option<usize> {
        for i in after..self.messages.len() {
            if self.messages[i].disposition != Disposition::Deleted {
                return Some(i + 1);
            }
        }
        None
    }

    /// Find first undeleted message before the given number
    pub fn prev_undeleted(&self, before: usize) -> Option<usize> {
        if before <= 1 {
            return None;
        }
        for i in (0..before - 1).rev() {
            if self.messages[i].disposition != Disposition::Deleted {
                return Some(i + 1);
            }
        }
        None
    }

    /// Find first deleted message after current for undelete
    pub fn next_deleted(&self, after: usize) -> Option<usize> {
        for i in after..self.messages.len() {
            if self.messages[i].disposition == Disposition::Deleted {
                return Some(i + 1);
            }
        }
        None
    }

    /// Find first deleted message before current for undelete
    pub fn prev_deleted(&self, before: usize) -> Option<usize> {
        if before <= 1 {
            return None;
        }
        for i in (0..before - 1).rev() {
            if self.messages[i].disposition == Disposition::Deleted {
                return Some(i + 1);
            }
        }
        None
    }

    /// Print header summary for messages
    pub fn print_headers(&self, msg_nums: Option<&[usize]>, vars: &Variables) {
        let screen = vars.screen_lines().get();
        let show_to = vars.get_bool("showto");

        let nums: Vec<usize> = msg_nums
            .map(|n| n.to_vec())
            .unwrap_or_else(|| (1..=self.messages.len()).collect());

        for &num in nums.iter().take(screen) {
            if let Some(msg) = self.get(num) {
                let current_marker = if num == self.current { '>' } else { ' ' };
                let state_char = msg.status_char();

                // Decide whether to show To or From
                let address_field = if show_to && vars.is_me(msg.from()) {
                    format!("To {}", crate::util::bare_addresses(msg.to()))
                } else {
                    msg.short_from()
                };

                let date = msg.short_date();
                let lines = msg.line_count();
                let size = msg.size();
                // Both fields are text the sender chose, written to the
                // terminal for every message in the mailbox without the user
                // asking to read it. A raw escape sequence there could
                // retitle or clear the window, or redraw the summary to hide
                // or misrepresent messages, so control characters are shown
                // as `?`.
                let address_field = printable(&address_field);
                let subject = truncate_display(&printable(msg.subject()), 25);

                // The traditional layout: sender cut to 20 columns, date
                // `Www Mmm dd hh:mm`, then lines/bytes.
                println!(
                    "{}{}{:>3} {:<20.20}  {:>16.16} {:>3}/{:<5} {}",
                    current_marker, state_char, num, address_field, date, lines, size, subject
                );
            }
        }
    }

    /// Whether any message's read state differs from the `Status:` it loaded.
    fn state_differs_from_disk(&self) -> bool {
        self.messages.iter().any(|m| {
            let on_disk = m.get_header("status");
            match (on_disk, m.read.status_value()) {
                (None, None) => false,
                (Some(a), Some(b)) => a != b,
                _ => true,
            }
        })
    }

    /// Handle quit - save read messages to mbox, delete saved, etc.
    pub fn quit(&mut self, vars: &Variables) -> Result<()> {
        let hold = vars.get_bool("hold");
        let keepsave = vars.get_bool("keepsave");
        let keep = vars.get_bool("keep");

        // Collect messages for mbox
        let mut mbox_messages = Vec::new();
        let mut keep_messages = Vec::new();

        for msg in &self.messages {
            // The mbox/touch commands force a message into the secondary mbox,
            // overriding a set `hold` variable (spec 104853-104855).
            if msg.force_mbox && msg.disposition != Disposition::Deleted {
                mbox_messages.push(msg.clone());
                continue;
            }
            match msg.disposition {
                Disposition::Deleted => {
                    // Discard
                }
                Disposition::Saved => {
                    // Without `keepsave` an explicitly saved message is dropped
                    // from the mailbox it was saved out of (spec 104890-104892).
                    // With it set, a system-mailbox message goes to mbox
                    // (spec 104638-104640) and a secondary folder keeps its own
                    // copy -- previously the folder case ignored `keepsave`
                    // entirely and discarded the message either way.
                    if keepsave {
                        if self.is_system_mailbox {
                            mbox_messages.push(msg.clone());
                        } else {
                            keep_messages.push(msg.clone());
                        }
                    }
                }
                // `hold` and `preserve` keep a message where it is, whatever
                // its read state. That state is no longer overwritten by the
                // request, so `:r` still matches a preserved message that was
                // read, and the `Status:` written back stays accurate.
                Disposition::Preserved => keep_messages.push(msg.clone()),
                Disposition::Keep => {
                    let migrate = self.is_system_mailbox && !hold && msg.read == ReadState::Read;
                    if migrate {
                        mbox_messages.push(msg.clone());
                    } else {
                        keep_messages.push(msg.clone());
                    }
                }
            }
        }

        // Save to mbox if needed
        if !mbox_messages.is_empty() {
            let mbox_path = crate::util::mbox_path(vars);
            let append = vars.get_bool("append");
            migrate_to_mbox(&mbox_path, &mbox_messages, append).context(&mbox_path)?;
        }

        // Rewrite the current mailbox only when its set of messages actually
        // changed. A message that merely left the mailbox is enough; a message
        // deleted is covered too, since it is absent from `keep_messages`.
        //
        // Leaving the file alone otherwise is not just an optimization: it is
        // what keeps mail delivered while mailx was running untouched in the
        // common case, and it avoids rewriting -- and so re-timestamping -- a
        // mailbox nobody changed.
        // Rewrite when the set of messages changed, or when a message's
        // recorded state no longer matches what is on disk. The second case is
        // what lets read state survive a reopen at all: a session that only
        // read messages changes no membership, but it does change every
        // `Status:` header the mailbox should now carry.
        if keep_messages.len() != self.messages.len() || self.state_differs_from_disk() {
            rewrite_mailbox(&self.path, &keep_messages, self.loaded_len, keep)
                .context(&self.path)?;
        }

        Ok(())
    }

    /// Save messages to a file
    /// Append messages to `path`.
    ///
    /// `include_from_line` distinguishes a mailbox (`save`, `copy`) from a
    /// plain text dump (`write`). It was previously named `include_headers`,
    /// which is not what the value controls.
    pub fn save_messages(
        &self,
        msg_nums: &[usize],
        path: &str,
        include_from_line: bool,
    ) -> Result<u64> {
        let messages: Vec<&Message> = msg_nums.iter().filter_map(|&n| self.get(n)).collect();

        let msgs: Vec<Message> = messages.iter().map(|m| (*m).clone()).collect();
        append_messages_to_file(path, &msgs, include_from_line).context(path)
    }
}

/// `s` with each control character replaced by `?`.
fn printable(s: &str) -> String {
    s.chars()
        .map(|c| if c.is_control() { '?' } else { c })
        .collect()
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

/// Serialize messages in mbox format (spec 104420-104436).
///
/// Each message is an envelope `From ` line, its header fields with a
/// regenerated `Status:`, an empty line, the `From `-quoted body, and one
/// trailing empty line -- so the next `From ` line is preceded by an empty
/// line, as the format requires.
fn write_messages<W: Write>(
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

/// Rewrite a mailbox in place, preserving anything delivered since it loaded.
///
/// The file is truncated and rewritten under an exclusive lock rather than
/// replaced via a temporary and `rename`, so the inode survives: a delivery
/// agent holding the mailbox open, or holding its own lock on it, keeps
/// referring to the same file, and the mode, owner, and group are untouched.
fn rewrite_mailbox(
    path: &str,
    messages: &[Message],
    loaded_len: u64,
    keep: bool,
) -> io::Result<()> {
    let (mut file, _dotlock) = open_locked(path)?;

    // Bytes past the extent we parsed are mail that arrived while mailx was
    // running. They are copied through verbatim -- reparsing them would be
    // wrong, since they were never presented to the user as messages.
    let mut tail = Vec::new();
    if file.metadata()?.len() > loaded_len {
        file.seek(io::SeekFrom::Start(loaded_len))?;
        file.read_to_end(&mut tail)?;
    }

    if messages.is_empty() && tail.is_empty() && !keep {
        // An emptied mailbox is removed unless `keep` is set, in which case it
        // is truncated to zero length instead (spec 104636-104637).
        drop(file);
        return fs::remove_file(path);
    }

    file.rewind()?;
    file.set_len(0)?;
    {
        let mut w = io::BufWriter::new(&mut file);
        write_messages(&mut w, messages, true)?;
        w.write_all(&tail)?;
        w.flush()?;
    }
    commit(file)
}

/// Move read messages into the secondary mbox.
///
/// `noappend`, the default, places them at the *beginning* of mbox; `append`
/// puts them at the end (spec 104579-104580). Neither truncates: the previously
/// saved mail is retained in both cases.
fn migrate_to_mbox(path: &str, messages: &[Message], append: bool) -> io::Result<()> {
    let (mut file, _dotlock) = open_locked(path)?;

    let mut existing = Vec::new();
    if !append {
        file.read_to_end(&mut existing)?;
        file.rewind()?;
        file.set_len(0)?;
    } else {
        file.seek(io::SeekFrom::End(0))?;
    }

    {
        let mut w = io::BufWriter::new(&mut file);
        write_messages(&mut w, messages, true)?;
        w.write_all(&existing)?;
        w.flush()?;
    }
    commit(file)
}

/// Append messages to a file named by `save`, `copy`, or the mbox default.
///
/// The target is whatever the user named. It may be another mailbox someone is
/// delivering to, so the append takes the same exclusive lock as a rewrite --
/// but it may equally be `/dev/null`, a fifo, or a file open only for writing,
/// none of which can be opened for reading, seeked, or synced. Those are not
/// errors in the save; the durability the lock and the sync buy applies to the
/// regular files where it means something.
///
/// Returns the number of bytes appended.
fn append_messages_to_file(
    path: &str,
    messages: &[Message],
    include_from_line: bool,
) -> io::Result<u64> {
    let mut file = crate::util::private_open()
        .create(true)
        .append(true)
        .open(path)?;

    // Best effort: locking fails on some filesystems and object types.
    let _ = lock(&file, true);

    let written = {
        let mut w = CountingWriter {
            inner: io::BufWriter::new(&mut file),
            count: 0,
        };
        write_messages(&mut w, messages, include_from_line)?;
        w.flush()?;
        w.count
    };
    file.flush()?;
    if file.metadata().is_ok_and(|m| m.is_file()) {
        file.sync_all()?;
    }
    Ok(written)
}

/// A writer that counts the bytes passing through it.
///
/// What `save` writes is not the message as loaded -- `Status:` is
/// regenerated and the separator always added -- so its size is counted on
/// the way out rather than predicted.
struct CountingWriter<W: Write> {
    inner: W,
    count: u64,
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
