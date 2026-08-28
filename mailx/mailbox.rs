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
        let content = fs::read_to_string(path)?;
        let mut mailbox = Mailbox::new(path.to_string());
        mailbox.loaded_len = content.len() as u64;

        let mut current_msg: Option<Message> = None;
        let mut in_headers = false;
        let mut header_continuation = false;
        let mut last_header_key = String::new();

        for line in content.lines() {
            // Check for message separator (From line)
            if line.starts_with("From ") && !in_headers {
                // Save previous message if any
                if let Some(msg) = current_msg.take() {
                    mailbox.messages.push(msg);
                }

                // Start new message.  Absent a Status: header it is `new`
                // (spec 104491-104496); a Status: header may downgrade it below.
                let mut msg = Message::new();
                msg.from_line = line.to_string();
                msg.read = ReadState::New;
                current_msg = Some(msg);
                in_headers = true;
                header_continuation = false;
                continue;
            }

            if let Some(ref mut msg) = current_msg {
                if in_headers {
                    if line.is_empty() {
                        // End of headers
                        in_headers = false;
                    } else if line.starts_with(' ') || line.starts_with('\t') {
                        // Header continuation
                        if header_continuation && !last_header_key.is_empty() {
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
                    // Body
                    // Handle escaped From lines (>From)
                    if line.starts_with(">From ") {
                        msg.body.push_str(&line[1..]);
                    } else {
                        msg.body.push_str(line);
                    }
                    msg.body.push('\n');
                }
            }
        }

        // Don't forget the last message
        if let Some(msg) = current_msg {
            mailbox.messages.push(msg);
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
                    format!("To {}", truncate_display(msg.to(), 18))
                } else {
                    msg.short_from()
                };

                let date = msg.short_date();
                let lines = msg.line_count();
                let size = msg.size();
                let subject = truncate_display(msg.subject(), 25);

                println!(
                    "{}{}{:>4}  {:<18}  {:>6}  {:>5}/{:<5}  {}",
                    current_marker, state_char, num, address_field, date, lines, size, subject
                );
            }
        }
    }

    /// Handle quit - save read messages to mbox, delete saved, etc.
    pub fn quit(&mut self, vars: &Variables) -> Result<(), String> {
        if !self.modified
            && !self.messages.iter().any(|m| {
                m.read == ReadState::Read
                    || m.disposition == Disposition::Deleted
                    || m.disposition == Disposition::Saved
            })
        {
            return Ok(());
        }

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
                Disposition::Preserved => keep_messages.push(msg.clone()),
                Disposition::Keep => {
                    if self.is_system_mailbox && !hold && msg.read == ReadState::Read {
                        mbox_messages.push(msg.clone());
                    } else {
                        keep_messages.push(msg.clone());
                    }
                }
            }
        }

        // Save to mbox if needed
        if !mbox_messages.is_empty() {
            let mbox_path = get_mbox_path(vars);
            let append = vars.get_bool("append");
            migrate_to_mbox(&mbox_path, &mbox_messages, append)
                .map_err(|e| format!("{}: {}", mbox_path, e))?;
        }

        // Rewrite the current mailbox only when its set of messages actually
        // changed. A message deleted or moved out is covered, since it is
        // absent from `keep_messages`.
        //
        // Leaving the file alone otherwise is not just an optimization: it is
        // what keeps mail delivered while mailx was running untouched in the
        // common case, and it avoids re-timestamping a mailbox nobody changed.
        if keep_messages.len() != self.messages.len() {
            rewrite_mailbox(&self.path, &keep_messages, self.loaded_len, keep)
                .map_err(|e| format!("{}: {}", self.path, e))?;
        }

        Ok(())
    }

    /// Save messages to a file
    pub fn save_messages(
        &self,
        msg_nums: &[usize],
        path: &str,
        include_headers: bool,
    ) -> Result<(), String> {
        let messages: Vec<&Message> = msg_nums.iter().filter_map(|&n| self.get(n)).collect();

        let msgs: Vec<Message> = messages.iter().map(|m| (*m).clone()).collect();
        append_messages_to_file(path, &msgs, include_headers)
            .map_err(|e| format!("{}: {}", path, e))
    }
}

/// Serialize messages in mbox format.
fn write_messages<W: Write>(
    w: &mut W,
    messages: &[Message],
    include_from_line: bool,
) -> io::Result<()> {
    for msg in messages {
        if include_from_line {
            writeln!(w, "{}", msg.from_line)?;
        }
        for header in &msg.header_lines {
            writeln!(w, "{}", header)?;
        }
        writeln!(w)?;
        // Handle From escaping in body
        for line in msg.body.lines() {
            if line.starts_with("From ") {
                write!(w, ">")?;
            }
            writeln!(w, "{}", line)?;
        }
        // Ensure blank line between messages
        if !msg.body.ends_with('\n') {
            writeln!(w)?;
        }
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
/// The target may itself be a mailbox someone else is delivering to, so the
/// append is taken under the same exclusive lock as a rewrite.
fn append_messages_to_file(
    path: &str,
    messages: &[Message],
    include_from_line: bool,
) -> io::Result<()> {
    let mut file = open_locked(path)?;
    file.seek(io::SeekFrom::End(0))?;
    {
        let mut w = io::BufWriter::new(&mut file);
        write_messages(&mut w, messages, include_from_line)?;
        w.flush()?;
    }
    commit(file)
}
fn get_mbox_path(vars: &Variables) -> String {
    vars.get("MBOX").map(|s| s.to_string()).unwrap_or_else(|| {
        let home = std::env::var("HOME").unwrap_or_else(|_| ".".to_string());
        format!("{}/mbox", home)
    })
}
