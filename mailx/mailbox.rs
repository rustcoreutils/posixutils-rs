//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Mailbox handling for mailx - mbox format parsing and writing

use std::fs::{self, File, OpenOptions};
use std::io::{self, Read, Seek, Write};
use std::os::unix::io::AsRawFd;

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
    pub fn load(path: &str) -> io::Result<Self> {
        // Read the file in one go rather than streaming it, so the byte length
        // we record is exactly the extent we parsed. A delivery that lands
        // mid-read would otherwise leave `loaded_len` disagreeing with the
        // messages in hand, and the tail would be written back twice.
        //
        // Mail is not necessarily UTF-8. Reading it as a string refused the
        // whole mailbox when any message carried a raw 8-bit byte, so one
        // Latin-1 message from anyone made all of a user's mail unreadable.
        let bytes = fs::read(path)?;
        let content = decode_mbox(&bytes);
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

        for raw in content.split_inclusive('\n') {
            // The same line `str::lines` would yield; `raw` keeps the
            // terminator so the message's on-disk size can be counted.
            let line = match raw.strip_suffix('\n') {
                Some(l) => l.strip_suffix('\r').unwrap_or(l),
                None => raw,
            };
            let boundary = at_message_boundary;
            at_message_boundary = line.is_empty();

            // Check for message separator (From line)
            if line.starts_with("From ") && !in_headers && boundary {
                // Save previous message if any
                if let Some(msg) = current_msg.take() {
                    mailbox.messages.push(msg);
                }

                // Start new message.  Absent a Status: header it is `new`
                // (spec 104491-104496); a Status: header may downgrade it below.
                let mut msg = Message::new();
                msg.from_line = line.to_string();
                msg.read = ReadState::New;
                msg.raw_lines = 1;
                msg.raw_bytes = encoded_len(raw);
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
                msg.raw_bytes += encoded_len(raw);

                if in_headers {
                    if line.is_empty() {
                        // End of headers
                        in_headers = false;
                    } else if line.starts_with(' ') || line.starts_with('\t') {
                        // Header continuation. Skipped entirely when the header
                        // it continues was not retained.
                        if !header_continuation {
                            continue;
                        }
                        if !last_header_key.is_empty() {
                            if let Some(val) = msg.headers.get_mut(&last_header_key) {
                                val.push(' ');
                                val.push_str(line.trim());
                            }
                        }
                        // Append to last header line
                        if let Some(last) = msg.header_lines.last_mut() {
                            last.push('\n');
                            last.push_str(line);
                        }
                    } else if let Some(colon_pos) = line.find(':') {
                        // New header
                        let key = line[..colon_pos].to_lowercase();
                        let value = line[colon_pos + 1..].trim().to_string();

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
                        msg.header_lines.push(line.to_string());
                        last_header_key = key;
                        header_continuation = true;
                    } else {
                        // Malformed header, treat as body start
                        in_headers = false;
                        msg.body.push_str(line);
                        msg.body.push('\n');
                    }
                } else {
                    // Body. Undo the mbox `From ` quoting: a run of `>`
                    // followed by `From ` loses one `>`. Stripping it only from
                    // `>From ` (and never adding one to `>>From `) meant a body
                    // line that genuinely read `>From x` was silently turned
                    // into `From x`.
                    msg.body.push_str(unquote_from_line(line));
                    msg.body.push('\n');
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
            if msg.body == "\n" {
                // A message with no body at all: the one line present is the
                // framing blank, not content.
                msg.body.clear();
            } else if msg.body.ends_with("\n\n") {
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
                let subject = truncate_display(msg.subject(), 25);

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
    pub fn quit(&mut self, vars: &Variables) -> Result<(), String> {
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
            migrate_to_mbox(&mbox_path, &mbox_messages, append)
                .map_err(|e| format!("{}: {}", mbox_path, plib::diag::io_error_text(&e)))?;
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
                .map_err(|e| format!("{}: {}", self.path, plib::diag::io_error_text(&e)))?;
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
    ) -> Result<u64, String> {
        let messages: Vec<&Message> = msg_nums.iter().filter_map(|&n| self.get(n)).collect();

        let msgs: Vec<Message> = messages.iter().map(|m| (*m).clone()).collect();
        append_messages_to_file(path, &msgs, include_from_line)
            .map_err(|e| format!("{}: {}", path, plib::diag::io_error_text(&e)))
    }
}

/// The first of the private-use characters that stand for undecodable bytes.
///
/// Byte `b` (always `0x80` or above: ASCII is valid UTF-8) is held as
/// `U+F700 + b`, so the range used is `U+F780..=U+F7FF`.
const RAW_BYTE_BASE: u32 = 0xF700;

/// The byte a stand-in character represents, if it is one.
fn raw_byte(c: char) -> Option<u8> {
    let n = u32::from(c).checked_sub(RAW_BYTE_BASE)?;
    u8::try_from(n).ok().filter(|b| *b >= 0x80)
}

/// Decode mailbox bytes, keeping every byte that is not valid UTF-8.
///
/// Each such byte becomes a private-use stand-in that [`encode_mbox`] turns
/// back into the byte, so a message is written back, saved, or copied exactly
/// as it arrived rather than with its 8-bit text replaced.
fn decode_mbox(bytes: &[u8]) -> String {
    let mut out = String::with_capacity(bytes.len());
    for chunk in bytes.utf8_chunks() {
        out.push_str(chunk.valid());
        for &b in chunk.invalid() {
            out.extend(char::from_u32(RAW_BYTE_BASE + u32::from(b)));
        }
    }
    out
}

/// The bytes `s` was decoded from by [`decode_mbox`].
pub(crate) fn encode_mbox(s: &str) -> std::borrow::Cow<'_, [u8]> {
    if !s.chars().any(|c| raw_byte(c).is_some()) {
        return std::borrow::Cow::Borrowed(s.as_bytes());
    }
    let mut out = Vec::with_capacity(s.len());
    for c in s.chars() {
        match raw_byte(c) {
            Some(b) => out.push(b),
            None => out.extend_from_slice(c.encode_utf8(&mut [0; 4]).as_bytes()),
        }
    }
    std::borrow::Cow::Owned(out)
}

/// The length in bytes of `s` as stored in the mailbox file.
fn encoded_len(s: &str) -> usize {
    encode_mbox(s).len()
}

/// Write `s` as stored in a mailbox file, followed by a newline.
fn write_line<W: Write>(w: &mut W, s: &str) -> io::Result<()> {
    w.write_all(&encode_mbox(s))?;
    w.write_all(b"\n")
}

/// Remove one level of mbox `From ` quoting from a body line.
///
/// mboxrd: `>From ` came from `From `, `>>From ` from `>From `, and so on.
fn unquote_from_line(line: &str) -> &str {
    let rest = line.trim_start_matches('>');
    if rest.starts_with("From ") && rest.len() < line.len() {
        &line[1..]
    } else {
        line
    }
}

/// Whether a body line needs mbox `From ` quoting on the way out.
pub(crate) fn needs_from_quoting(line: &str) -> bool {
    line.trim_start_matches('>').starts_with("From ")
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
        for line in msg.body.lines() {
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

/// Take a blocking exclusive lock on `file`.
///
/// Mailbox files are shared with the local delivery agent, so every write path
/// that is not a plain atomic append has to serialize against it.
fn lock_exclusive(file: &File) -> io::Result<()> {
    // SAFETY: the fd is valid for the lifetime of `file`, flock takes an fd and
    // an integer operation, and the return value is checked.
    if unsafe { libc::flock(file.as_raw_fd(), libc::LOCK_EX) } != 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(())
}

/// Open `path` for a locked read-modify-write, creating it if absent.
fn open_locked(path: &str) -> io::Result<File> {
    let file = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(false)
        .open(path)?;
    lock_exclusive(&file)?;
    Ok(file)
}

/// Flush and commit `file`, reporting any deferred write error.
///
/// Dropping a `File` discards a write error that only surfaced at close, which
/// is exactly the case where a mailbox has been truncated but not rewritten.
fn commit(mut file: File) -> io::Result<()> {
    file.flush()?;
    file.sync_all()
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
    let mut file = open_locked(path)?;

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
    let mut file = open_locked(path)?;

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
    let mut file = OpenOptions::new().create(true).append(true).open(path)?;

    // Best effort: flock fails on some filesystems and object types.
    // SAFETY: the fd is valid for the lifetime of `file`.
    unsafe { libc::flock(file.as_raw_fd(), libc::LOCK_EX) };

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
