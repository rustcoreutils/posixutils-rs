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

/// Which way `Mailbox::find` searches.
#[derive(Debug, Clone, Copy)]
pub enum Direction {
    Forward,
    Backward,
}

/// A mailbox containing messages
#[derive(Debug)]
pub struct Mailbox {
    /// Path to the mailbox file
    pub path: String,
    /// Messages in the mailbox
    pub messages: Vec<Message>,
    /// The current message's number, counted from 1; `None` while there
    /// are no messages.
    current: Option<usize>,
    /// Whether this is the system mailbox
    pub is_system_mailbox: bool,
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
            current: None,
            is_system_mailbox: false,
            prev_path: None,
            loaded_len: 0,
        }
    }

    /// Load a mailbox from a file
    pub fn load(path: &str) -> Result<Self> {
        // Read in one go, under a shared lock, so the length recorded is
        // exactly the extent parsed and a delivery in progress is waited for
        // rather than half read; otherwise `loaded_len` could disagree with the
        // messages in hand and the tail be written back twice. Mail need not
        // be UTF-8, so it is parsed as bytes.
        let bytes = read_locked(path).context(path)?;
        let mut mailbox = Mailbox::new(path.to_string());
        mailbox.loaded_len = bytes.len() as u64;

        mailbox.messages = crate::mbox::parse(&bytes);

        // The first new message, else the first unread one, else the first.
        let first = |state| mailbox.messages.iter().position(|m| m.read == state);
        let first_any = (!mailbox.messages.is_empty()).then_some(0);
        mailbox.current = first(ReadState::New)
            .or_else(|| first(ReadState::Unread))
            .or(first_any)
            .map(|i| i + 1);

        Ok(mailbox)
    }

    pub fn message_count(&self) -> usize {
        self.messages.len()
    }

    /// The number of messages not marked deleted.
    pub fn undeleted_count(&self) -> usize {
        self.messages
            .iter()
            .filter(|m| m.disposition != Disposition::Deleted)
            .count()
    }

    /// The current message's number, if there is a current message.
    pub fn current(&self) -> Option<usize> {
        self.current
    }

    /// The current message, if there is one.
    pub fn current_message(&self) -> Option<&Message> {
        self.current.and_then(|n| self.get(n))
    }

    /// Make message `num` the current message.
    pub fn set_current(&mut self, num: usize) {
        debug_assert!(self.get(num).is_some(), "no message {}", num);
        self.current = Some(num);
    }

    /// Message `num`, counting from 1.
    pub fn get(&self, num: usize) -> Option<&Message> {
        if num > 0 && num <= self.messages.len() {
            Some(&self.messages[num - 1])
        } else {
            None
        }
    }

    /// Message `num`, counting from 1, to change.
    pub fn get_mut(&mut self, num: usize) -> Option<&mut Message> {
        if num > 0 && num <= self.messages.len() {
            Some(&mut self.messages[num - 1])
        } else {
            None
        }
    }

    /// The first message after message `from` (`Forward`) or before it
    /// (`Backward`) that satisfies `pred`. A `from` of 0 searches forward
    /// from the first message.
    pub fn find(
        &self,
        from: usize,
        direction: Direction,
        pred: impl Fn(&Message) -> bool,
    ) -> Option<usize> {
        let matches = |&n: &usize| self.get(n).is_some_and(&pred);
        match direction {
            Direction::Forward => (from + 1..=self.messages.len()).find(matches),
            Direction::Backward => (1..from.min(self.messages.len() + 1)).rev().find(matches),
        }
    }

    /// Whether any message's read state differs from the one it loaded with.
    fn state_differs_from_disk(&self) -> bool {
        self.messages.iter().any(|m| m.read != m.loaded_read)
    }

    /// Close the mailbox as `quit` does: move read messages to mbox, drop
    /// deleted and saved ones as `hold` and `keepsave` direct, and write the
    /// rest back.
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
                mbox_messages.push(msg);
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
                    // (spec 104638-104640) and a secondary folder keeps it.
                    if keepsave {
                        if self.is_system_mailbox {
                            mbox_messages.push(msg);
                        } else {
                            keep_messages.push(msg);
                        }
                    }
                }
                // `hold` and `preserve` keep a message where it is, with its
                // read state.
                Disposition::Preserved => keep_messages.push(msg),
                Disposition::Keep => {
                    let migrate = self.is_system_mailbox && !hold && msg.read == ReadState::Read;
                    if migrate {
                        mbox_messages.push(msg);
                    } else {
                        keep_messages.push(msg);
                    }
                }
            }
        }

        if !mbox_messages.is_empty() {
            let mbox_path = crate::util::mbox_path(vars);
            let append = vars.is_set("append");
            migrate_to_mbox(&mbox_path, &mbox_messages, append).context(&mbox_path)?;
        }

        // Rewrite only if messages left the mailbox or a read state changed.
        // Leaving it alone otherwise keeps mail delivered meanwhile untouched
        // and does not re-timestamp a mailbox nobody changed.
        if keep_messages.len() != self.messages.len() || self.state_differs_from_disk() {
            rewrite_mailbox(&self.path, &keep_messages, self.loaded_len, keep)
                .context(&self.path)?;
        }

        Ok(())
    }

    /// Append messages to `path`.
    ///
    /// `include_from_line` distinguishes a mailbox (`save`, `copy`) from a
    /// plain text dump (`write`).
    pub fn save_messages(
        &self,
        msg_nums: &[usize],
        path: &str,
        include_from_line: bool,
    ) -> Result<u64> {
        let messages: Vec<&Message> = msg_nums.iter().filter_map(|&n| self.get(n)).collect();
        append_messages_to_file(path, &messages, include_from_line).context(path)
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
    messages: &[&Message],
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
fn migrate_to_mbox(path: &str, messages: &[&Message], append: bool) -> io::Result<()> {
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
pub(crate) fn append_messages_to_file(
    path: &str,
    messages: &[&Message],
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
