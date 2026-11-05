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
use crate::mbox::{write_messages, CountingWriter};
use crate::message::{Disposition, Message, ReadState};
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

        mailbox.messages = crate::mbox::parse(&bytes);

        // Set current to first new/unread message, or first message
        mailbox.current = mailbox
            .messages
            .iter()
            .position(|m| m.read.is_unread())
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

    /// Whether any message's read state differs from the one it loaded with.
    fn state_differs_from_disk(&self) -> bool {
        self.messages.iter().any(|m| m.read != m.loaded_read)
    }

    /// Handle quit - save read messages to mbox, delete saved, etc.
    pub fn quit(&mut self, vars: &Variables) -> Result<()> {
        let hold = vars.is_set("hold");
        let keepsave = vars.is_set("keepsave");
        let keep = vars.is_set("keep");

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
            let append = vars.is_set("append");
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
