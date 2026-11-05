//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! An open mailbox: its messages, and the session's work on them.
//!
//! Opening a mailbox reads it once, under its locks, into the session's
//! private store (see `store`); memory holds where each message lies and
//! its state. Quitting writes the result back without risking mail:
//!
//! - read messages go to mbox first, appended, or prepended through a
//!   temporary file renamed over mbox;
//! - then the mailbox is rewritten in place, keeping its inode, through a
//!   synced recovery file (see `recovery`), with any mail delivered during
//!   the session copied through after the kept messages;
//! - signals that could cut this short are held off while it runs, lock
//!   waits included.

use std::fs::{self, File};
use std::io::{self, Read, Seek, Write};

use crate::error::{IoContext, Result};
use crate::lock::{commit, lock, open_locked, open_read_locked};
use crate::mbox::{unquote_from_line, write_messages, CountingWriter};
use crate::message::{Disposition, Headers, Message, ReadState};
use crate::signals::{SignalsHeld, LOCK_SIGNALS, QUIT_SIGNALS};
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
    /// Size of the mailbox file when last looked at for new mail: past
    /// `loaded_len` when bytes there were not a message to take in.
    checked_len: u64,
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
            checked_len: 0,
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
        mailbox.checked_len = len;

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

    /// Whether the mailbox file has grown since it was last looked at for new
    /// mail: a cheap check, without locks, for whether to
    /// [`Mailbox::incorporate`]. A mailbox that shrank holds no new mail, as
    /// BSD mailx's `autoinc` has it.
    pub fn changed_size(&self) -> bool {
        fs::metadata(&self.path).is_ok_and(|m| m.len() > self.checked_len)
    }

    /// Take in mail delivered to the mailbox since it was read, returning
    /// how many messages were added.
    ///
    /// The new bytes are parsed into the store under the same locks as the
    /// first read, and `loaded_len` moves past them, so quit neither loses
    /// nor duplicates the messages among them. Text before the first of
    /// them that is not a message is passed over, as it is when a mailbox is
    /// read and as BSD mailx's setptr() passes it over; quit does not keep it.
    pub fn incorporate(&mut self) -> Result<usize> {
        let (file, _dotlock) = open_read_locked(&self.path).context(&self.path)?;
        let len = file.metadata().context(&self.path)?.len();
        if len < self.loaded_len {
            return Err(format!("{}: mailbox shrank while open", self.path).into());
        }
        self.checked_len = self.checked_len.max(len);
        let mut file = &file;
        file.seek(io::SeekFrom::Start(self.loaded_len))
            .context(&self.path)?;
        let input = io::BufReader::new(file.take(len - self.loaded_len));
        let (messages, consumed) =
            crate::mbox::parse(input, &mut self.store).context(&self.path)?;
        self.loaded_len += consumed;
        let added = messages.len();
        self.messages.extend(messages);
        if self.current.is_none() && !self.messages.is_empty() {
            self.current = Some(1);
        }
        Ok(added)
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
        // As BSD mailx ignores HUP, INT and QUIT for its quit, they are held
        // through all of it, lock waits included: an interrupt does not
        // throw the session's work away. They act once this guard drops,
        // after every lock quit took is released. A termination request
        // ends a wait for another process's lock, but is held from the
        // moment mailx owns one (see `DotLock` and `rewrite_mailbox`).
        let _held = SignalsHeld::new(&QUIT_SIGNALS);
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

        if self.is_system_mailbox
            && fs::metadata(&self.path).is_ok_and(|m| m.len() > self.loaded_len)
        {
            println!("New mail has arrived.");
        }

        if !mbox_messages.is_empty() {
            let mbox_path = crate::util::mbox_path(vars);
            let append = vars.is_set("append");
            migrate_to_mbox(&mbox_path, &self.store, &mbox_messages, append).context(&mbox_path)?;
            println!(
                "Saved {} in mbox",
                crate::util::messages(mbox_messages.len())
            );
        }

        // Rewrite only if messages left the mailbox or a read state changed.
        // Leaving it alone otherwise keeps mail delivered meanwhile untouched
        // and does not re-timestamp a mailbox nobody changed.
        let rewritten =
            if keep_messages.len() != self.messages.len() || self.state_differs_from_disk() {
                Some(
                    rewrite_mailbox(
                        &self.path,
                        &self.store,
                        &keep_messages,
                        self.loaded_len,
                        keep,
                    )
                    .context(&self.path)?,
                )
            } else {
                None
            };

        // What BSD mailx reports: messages held in the system mailbox, or
        // how a folder's rewrite ended.
        if self.is_system_mailbox {
            if !keep_messages.is_empty() {
                println!(
                    "Held {} in {}",
                    crate::util::messages(keep_messages.len()),
                    self.path
                );
            }
        } else if let Some(outcome) = rewritten {
            println!("\"{}\" {}", self.path, outcome);
        }
        Ok(())
    }

    /// Append messages `msg_nums` to the mailbox `path`.
    pub fn save_messages(&self, msg_nums: &[usize], path: &str) -> Result<u64> {
        let messages: Vec<&Message> = msg_nums.iter().filter_map(|&n| self.get(n)).collect();
        append_to_file(path, |w| write_messages(w, &self.store, &messages)).context(path)
    }
}

/// Rewrite a mailbox in place, preserving anything delivered since it
/// loaded, through a recovery file (see `recovery`).
///
/// The file is truncated and rewritten under an exclusive lock rather than
/// replaced via a temporary and `rename`, so the inode survives: a delivery
/// agent holding the mailbox open, or holding its own lock on it, keeps
/// referring to the same file, and the mode, owner, and group are untouched.
///
/// Returns how it ended, in BSD mailx's words: `complete`, or `removed` for
/// an emptied mailbox.
fn rewrite_mailbox(
    path: &str,
    store: &Store,
    messages: &[&Message],
    loaded_len: u64,
    keep: bool,
) -> io::Result<&'static str> {
    let (mut file, _dotlock) = open_locked(path)?;
    // Held only once the locks are taken: waiting for another process's
    // lock can be long, and must stay open to a termination request.
    let _held = SignalsHeld::new(&LOCK_SIGNALS);

    // Bytes past the extent we parsed are mail that arrived while mailx was
    // running. They are copied through verbatim -- reparsing them would be
    // wrong, since they were never presented to the user as messages.
    let tail_len = file.metadata()?.len().saturating_sub(loaded_len);

    if messages.is_empty() && tail_len == 0 && !keep {
        // An emptied mailbox is removed unless `keep` is set, in which case it
        // is truncated to zero length instead (spec 104636-104637).
        remove_locked(path, file)?;
        return Ok("removed");
    }

    crate::recovery::rewrite(path, &mut file, |mut old, w| {
        write_messages(w, store, messages)?;
        old.seek(io::SeekFrom::Start(loaded_len))?;
        io::copy(&mut old.take(tail_len), w)?;
        Ok(())
    })?;
    Ok("complete")
}

/// Unlink the mailbox at `path`, then let go of `locks`, its locked file.
///
/// In that order, as BSD mailx removes the mailbox before closing it: a
/// delivery agent waiting on the locks finds, once let in, that the file it
/// locked is gone, and does not append mail to it just before the unlink.
fn remove_locked<L>(path: &str, locks: L) -> io::Result<()> {
    let removed = fs::remove_file(path);
    drop(locks);
    removed
}

/// Move read messages into the secondary mbox.
///
/// `noappend`, the default, places them at the *beginning* of mbox; `append`
/// puts them at the end (spec 104579-104580). Neither loses the mail already
/// there.
fn migrate_to_mbox(
    path: &str,
    store: &Store,
    messages: &[&Message],
    append: bool,
) -> io::Result<()> {
    let (mut file, _dotlock) = open_locked(path)?;
    // As in `rewrite_mailbox`.
    let _held = SignalsHeld::new(&LOCK_SIGNALS);
    if append {
        file.seek(io::SeekFrom::End(0))?;
        let mut w = io::BufWriter::new(&mut file);
        write_messages(&mut w, store, messages)?;
        w.flush()?;
        drop(w);
        return commit(file);
    }
    // A file with other names is rewritten where it is, as BSD mailx always
    // does: renaming over one name would split it from the others.
    if std::os::unix::fs::MetadataExt::nlink(&file.metadata()?) > 1 {
        return prepend_in_place(path, file, store, messages);
    }
    // Through a symbolic link, the file to replace is the one it names.
    let target = fs::canonicalize(path)?;
    match prepend_by_rename(&target, &file, store, messages) {
        Err(e) if e.kind() == io::ErrorKind::PermissionDenied => {
            prepend_in_place(path, file, store, messages)
        }
        result => result,
    }
}

/// Prepend to mbox by writing the new messages and then its old contents to
/// a temporary file beside it, which then replaces it.
///
/// mbox is never truncated, so whenever mailx stops it holds either all of
/// its old contents or all of its new ones. The replacement takes mbox's
/// mode; mbox belongs to the user, so its owner is unchanged. `path` names
/// the file itself, not a link to it.
fn prepend_by_rename(
    path: &std::path::Path,
    old: &File,
    store: &Store,
    messages: &[&Message],
) -> io::Result<()> {
    let dir = match path.parent() {
        Some(dir) if !dir.as_os_str().is_empty() => dir,
        _ => std::path::Path::new("."),
    };
    let mut tmp = plib::tmp::Builder::new()
        .prefix(".mailx-mbox.")
        .tempfile_in(dir)?;
    tmp.as_file()
        .set_permissions(old.metadata()?.permissions())?;
    {
        let mut w = io::BufWriter::new(tmp.as_file_mut());
        write_messages(&mut w, store, messages)?;
        let mut old = old;
        old.rewind()?;
        io::copy(&mut old, &mut w)?;
        w.flush()?;
    }
    tmp.as_file().sync_all()?;
    tmp.persist(path).map_err(io::Error::from)?;
    // The rename itself is durable once the directory is.
    if let Ok(dir) = File::open(dir) {
        let _ = dir.sync_all();
    }
    Ok(())
}

/// Prepend to mbox in place, where its directory cannot hold a temporary
/// file, through a recovery file.
fn prepend_in_place(
    path: &str,
    mut file: File,
    store: &Store,
    messages: &[&Message],
) -> io::Result<()> {
    crate::recovery::rewrite(path, &mut file, |mut old, w| {
        write_messages(w, store, messages)?;
        old.rewind()?;
        io::copy(&mut old, w)?;
        Ok(())
    })
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::signals::PathWatch;
    use std::os::unix::io::AsRawFd;

    /// A signal that arrives while `quit` waits for a lock is held until
    /// the quit is done, its dotlock removed: acting earlier, a SIGTERM
    /// would end mailx and leave the lock behind for others to wait on.
    #[test]
    fn signals_during_quit_wait_for_its_dotlock_to_go() {
        use std::os::unix::thread::JoinHandleExt;

        let dir = plib::tmp::TempDir::new().unwrap();
        let path = dir.path().join("folder");
        fs::write(
            &path,
            "From a@b Mon Jan  1 00:00:00 2024\nSubject: one\n\nfirst\n\n\
             From c@d Mon Jan  1 00:00:00 2024\nSubject: two\n\nsecond\n\n",
        )
        .unwrap();
        let watch = PathWatch::new(libc::SIGQUIT, &format!("{}.lock", path.display()));

        let mut mb = Mailbox::load(path.to_str().unwrap()).unwrap();
        mb.messages[0].disposition = Disposition::Deleted;
        let holder = File::open(&path).unwrap();
        // SAFETY: the fd is valid for the lifetime of `holder`.
        assert_eq!(unsafe { libc::flock(holder.as_raw_fd(), libc::LOCK_EX) }, 0);

        let quitter = std::thread::spawn(move || mb.quit(&Variables::new()));
        std::thread::sleep(std::time::Duration::from_millis(200));
        // SAFETY: the thread is still running, waiting for the lock.
        unsafe {
            libc::pthread_kill(quitter.as_pthread_t(), libc::SIGQUIT);
        }
        std::thread::sleep(std::time::Duration::from_millis(100));
        // SAFETY: as above. Unlocked, not closed: closing a descriptor for
        // the file would drop the quitting thread's fcntl lock.
        assert_eq!(unsafe { libc::flock(holder.as_raw_fd(), libc::LOCK_UN) }, 0);
        let result = quitter.join().unwrap();
        drop(holder);

        assert!(result.is_ok(), "quit failed: {:?}", result.err());
        assert_eq!(
            watch.seen(),
            Some(false),
            "the signal acted while the dotlock was held"
        );
        assert!(!fs::read_to_string(&path).unwrap().contains("first"));
    }

    /// An emptied mailbox is unlinked before its locks are let go: a
    /// delivery agent waiting on them, once let in, finds the file it locked
    /// already gone, rather than appending to it in the moment before mailx
    /// unlinks it and losing the mail.
    #[test]
    fn an_emptied_mailbox_is_unlinked_under_its_locks() {
        /// Stands for the locked file: records, when let go, whether the
        /// mailbox was still there.
        struct Locks<'a>(&'a std::path::Path, &'a std::cell::Cell<Option<bool>>);
        impl Drop for Locks<'_> {
            fn drop(&mut self) {
                self.1.set(Some(self.0.exists()));
            }
        }

        let dir = plib::tmp::TempDir::new().unwrap();
        let path = dir.path().join("folder");
        fs::write(&path, "").unwrap();
        let there_at_release = std::cell::Cell::new(None);
        remove_locked(path.to_str().unwrap(), Locks(&path, &there_at_release)).unwrap();
        assert_eq!(
            there_at_release.get(),
            Some(false),
            "the mailbox was unlinked after its locks were let go"
        );
    }

    const ONE_MESSAGE: &str = "From a@b Mon Jan  1 00:00:00 2024\n\nonly\n\n";

    /// A mailbox that shrank under mailx is not new mail for `autoinc` to
    /// take the locks for -- and fail on -- at every prompt.
    #[test]
    fn a_shrunken_mailbox_has_no_new_mail() {
        let dir = plib::tmp::TempDir::new().unwrap();
        let path = dir.path().join("folder");
        fs::write(&path, ONE_MESSAGE).unwrap();
        let mb = Mailbox::load(path.to_str().unwrap()).unwrap();
        fs::write(&path, "").unwrap();
        assert!(!mb.changed_size());
    }

    /// Bytes that are not a message, once looked at, are not looked at
    /// again until more arrive.
    #[test]
    fn a_tail_that_is_not_mail_is_checked_once() {
        let dir = plib::tmp::TempDir::new().unwrap();
        let path = dir.path().join("folder");
        fs::write(&path, ONE_MESSAGE).unwrap();
        let mut mb = Mailbox::load(path.to_str().unwrap()).unwrap();
        let mut file = fs::OpenOptions::new().append(true).open(&path).unwrap();
        file.write_all(b"half a message\n").unwrap();
        assert!(mb.changed_size());
        assert_eq!(mb.incorporate().unwrap(), 0);
        assert!(!mb.changed_size(), "the same tail is to be checked again");

        file.write_all(b"\nFrom c@d Mon Jan  1 00:00:00 2024\n\nmore\n\n")
            .unwrap();
        assert!(mb.changed_size(), "more mail went unnoticed");
    }
}
