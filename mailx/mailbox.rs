//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Mailbox handling for mailx - mbox format parsing and writing

use std::fs::{self, File};
use std::io::{self, Read, Seek, Write};

use crate::error::{IoContext, Result};
use crate::lock::{commit, lock, open_locked, open_read_locked};
use crate::mbox::{unquote_from_line, write_messages, CountingWriter};
use crate::message::{Disposition, Headers, Message, ReadState};
use crate::store::Store;
use crate::variables::Variables;

/// The context for an I/O error on the store.
const STORE: &str = "temporary mailbox copy";

/// What quitting does with a message.
enum Fate {
    /// Move it to the secondary mbox.
    Mbox,
    /// Leave it in the mailbox.
    Keep,
    /// Drop it.
    Discard,
}

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
    /// The private copy the messages are read from.
    pub store: Store,
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
            store: Store::default(),
            prev_path: None,
            loaded_len: 0,
        }
    }

    /// Load a mailbox from a file
    pub fn load(path: &str) -> Result<Self> {
        // Read to the end under the locks, so the length recorded is exactly
        // the extent parsed: otherwise `loaded_len` could disagree with the
        // messages in hand and the tail be written back twice. The text goes
        // to the store as it is read; mail need not be UTF-8, so it is
        // parsed as bytes.
        let (file, _dotlock) = open_read_locked(path).context(path)?;
        let mut mailbox = Mailbox::new(path.to_string());
        let input = io::BufReader::with_capacity(64 * 1024, &file);
        let (messages, len) = crate::mbox::parse(input, &mut mailbox.store).context(path)?;
        mailbox.messages = messages;
        mailbox.loaded_len = len;

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

    /// The `From ` line and header fields of message `num`, read from the
    /// store.
    pub fn headers(&self, num: usize) -> Result<Headers> {
        let msg = self.get(num).ok_or("No message")?;
        Ok(Headers::parse(&self.store.read(msg.head).context(STORE)?))
    }

    /// The body of message `num`, read from the store and unquoted, each
    /// line ending with a newline.
    pub fn body(&self, num: usize) -> Result<Vec<u8>> {
        let msg = self.get(num).ok_or("No message")?;
        let mut body = Vec::with_capacity(msg.body_span.len as usize + 1);
        let mut reader = self.store.reader(msg.body_span);
        let mut line = Vec::new();
        loop {
            line.clear();
            if io::BufRead::read_until(&mut reader, b'\n', &mut line).context(STORE)? == 0 {
                return Ok(body);
            }
            let text = line.strip_suffix(b"\n").unwrap_or(&line);
            body.extend_from_slice(unquote_from_line(text));
            body.push(b'\n');
        }
    }

    /// Message `num` as `print` shows it: its header fields as `discard`,
    /// `ignore` and `retain` allow -- or all of them, for the capitalized
    /// `Print`/`Type` form (spec 104756) -- an empty line, and its body.
    pub fn display(&self, num: usize, show_all_headers: bool, vars: &Variables) -> Result<Vec<u8>> {
        let mut output = Vec::new();
        for line in &self.headers(num)?.lines {
            let field = line.split(|&b| b == b':').next().unwrap_or_default();
            if show_all_headers || vars.header_filter.shows(field.trim_ascii()) {
                output.extend_from_slice(line);
                output.push(b'\n');
            }
        }
        output.push(b'\n');
        output.extend_from_slice(&self.body(num)?);
        Ok(output)
    }

    /// Message `num` whole, `From ` line included, as given to an editor.
    pub fn full_text(&self, num: usize) -> Result<Vec<u8>> {
        let headers = self.headers(num)?;
        let mut output = headers.from_line;
        output.push(b'\n');
        for line in &headers.lines {
            output.extend_from_slice(line);
            output.push(b'\n');
        }
        output.push(b'\n');
        output.extend_from_slice(&self.body(num)?);
        Ok(output)
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

    /// Where quitting puts `msg`, given the `hold` and `keepsave` variables.
    fn fate(&self, msg: &Message, hold: bool, keepsave: bool) -> Fate {
        match msg.disposition {
            Disposition::Deleted => Fate::Discard,
            // `mbox` and `touch` send a message to mbox whatever `hold` says
            // (spec 104853-104855).
            _ if msg.force_mbox => Fate::Mbox,
            // Without `keepsave` a saved message leaves the mailbox it was
            // saved from (spec 104890-104892). With it, a system-mailbox
            // message goes to mbox (spec 104638-104640) and a secondary
            // folder keeps it.
            Disposition::Saved if !keepsave => Fate::Discard,
            Disposition::Saved if self.is_system_mailbox => Fate::Mbox,
            Disposition::Saved => Fate::Keep,
            // `hold` and `preserve` keep a message where it is.
            Disposition::Preserved => Fate::Keep,
            Disposition::Keep if self.is_system_mailbox && !hold && msg.read == ReadState::Read => {
                Fate::Mbox
            }
            Disposition::Keep => Fate::Keep,
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

        let mut mbox_messages = Vec::new();
        let mut keep_messages = Vec::new();
        for msg in &self.messages {
            match self.fate(msg, hold, keepsave) {
                Fate::Mbox => mbox_messages.push(msg),
                Fate::Keep => keep_messages.push(msg),
                Fate::Discard => {}
            }
        }

        if !mbox_messages.is_empty() {
            let mbox_path = crate::util::mbox_path(vars);
            let append = vars.is_set("append");
            migrate_to_mbox(&mbox_path, &self.store, &mbox_messages, append).context(&mbox_path)?;
        }

        // Rewrite only if messages left the mailbox or a read state changed.
        // Leaving it alone otherwise keeps mail delivered meanwhile untouched
        // and does not re-timestamp a mailbox nobody changed.
        if keep_messages.len() != self.messages.len() || self.state_differs_from_disk() {
            rewrite_mailbox(
                &self.path,
                &self.store,
                &keep_messages,
                self.loaded_len,
                keep,
            )
            .context(&self.path)?;
        }

        Ok(())
    }

    /// Append messages `msg_nums` to the mailbox `path`.
    pub fn save_messages(&self, msg_nums: &[usize], path: &str) -> Result<u64> {
        let messages: Vec<&Message> = msg_nums.iter().filter_map(|&n| self.get(n)).collect();
        append_to_file(path, |w| write_messages(w, &self.store, &messages)).context(path)
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
    store: &Store,
    messages: &[&Message],
    loaded_len: u64,
    keep: bool,
) -> io::Result<()> {
    let (mut file, _dotlock) = open_locked(path)?;

    // Bytes past the extent we parsed are mail that arrived while mailx was
    // running. They are copied through verbatim -- reparsing them would be
    // wrong, since they were never presented to the user as messages.
    let tail_len = file.metadata()?.len().saturating_sub(loaded_len);
    let mut tail = plib::tmp::tempfile()?;
    if tail_len > 0 {
        file.seek(io::SeekFrom::Start(loaded_len))?;
        io::copy(&mut (&file).take(tail_len), &mut tail)?;
        tail.rewind()?;
    }

    if messages.is_empty() && tail_len == 0 && !keep {
        // An emptied mailbox is removed unless `keep` is set, in which case it
        // is truncated to zero length instead (spec 104636-104637).
        drop(file);
        return fs::remove_file(path);
    }

    file.rewind()?;
    file.set_len(0)?;
    {
        let mut w = io::BufWriter::new(&mut file);
        write_messages(&mut w, store, messages)?;
        io::copy(&mut tail, &mut w)?;
        w.flush()?;
    }
    commit(file)
}

/// Move read messages into the secondary mbox.
///
/// `noappend`, the default, places them at the *beginning* of mbox; `append`
/// puts them at the end (spec 104579-104580). Neither truncates: the previously
/// saved mail is retained in both cases.
fn migrate_to_mbox(
    path: &str,
    store: &Store,
    messages: &[&Message],
    append: bool,
) -> io::Result<()> {
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
        write_messages(&mut w, store, messages)?;
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
/// `write` writes the messages; the number of bytes appended is returned.
pub(crate) fn append_to_file(
    path: &str,
    write: impl FnOnce(&mut CountingWriter<io::BufWriter<&mut File>>) -> io::Result<()>,
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
        write(&mut w)?;
        w.flush()?;
        w.count
    };
    file.flush()?;
    if file.metadata().is_ok_and(|m| m.is_file()) {
        file.sync_all()?;
    }
    Ok(written)
}
