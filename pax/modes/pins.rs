//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Files this run made, held so that a later name is linked to that very
//! file, and the budget of descriptors held for them.
//!
//! A later name of a file -- a tar link member, a later name of a copied
//! hard-linked file -- is linked to the file made for an earlier one, found
//! at the name it was made at. Anyone who can write that directory can remove
//! it and make another file there, and a filesystem that reuses inode numbers
//! (ext4) can give theirs the very same `(st_dev, st_ino)`. So each file made
//! is held by an `O_PATH` descriptor, which keeps its inode -- and its number
//! -- in use and is linked through directly (`Expected::pin`). Past the budget
//! of descriptors, or without a procfs to link a descriptor through, the file
//! is known by its identity and its `ctime`, which a file someone else makes
//! later does not share -- unless it is made within the same tick of the
//! coarse clock the kernel stamps `ctime` from (a few milliseconds): the
//! residual of that fallback.

use crate::modes::anchored::{file_id, Expected};
use plib::madefs::{fstat, lstat_at};
use std::collections::VecDeque;
use std::ffi::CStr;
use std::io;
use std::os::fd::{AsFd, AsRawFd, BorrowedFd, FromRawFd, OwnedFd};

/// A file this run made, as a later name of it is to be linked to it.
#[derive(Debug)]
pub(crate) struct MadeFile {
    /// Its `(st_dev, st_ino)`.
    id: (u64, u64),
    /// Its `ctime` as this run last left it: changed by everything this run
    /// does to it, a link included, and by nothing anyone else can make it
    /// equal to on a file of their own.
    ctime: (i64, i64),
    /// The file itself, held open with `O_PATH`, while the budget allows.
    pin: Option<OwnedFd>,
}

impl MadeFile {
    /// The file open on `fd` -- for writing, or `O_PATH` -- which this run
    /// has just made and given its attributes: pinned by a descriptor of its
    /// own where a verified procfs can reopen it.
    pub(crate) fn of(fd: BorrowedFd<'_>) -> io::Result<Self> {
        let st = fstat(fd.as_raw_fd())?;
        Ok(MadeFile {
            id: file_id(&st),
            ctime: ctime_of(&st),
            pin: reopen_path(fd),
        })
    }

    /// A node this run has just made and given its attributes, already held
    /// by the `O_PATH` descriptor `pin` (`MadeNode`): pinned by a duplicate
    /// of it.
    #[cfg(target_os = "linux")]
    pub(crate) fn held(pin: BorrowedFd<'_>) -> io::Result<Self> {
        let st = fstat(pin.as_raw_fd())?;
        // Without a duplicate it is known by identity and ctime alone.
        let dup = unsafe { libc::fcntl(pin.as_raw_fd(), libc::F_DUPFD_CLOEXEC, 0) };
        Ok(MadeFile {
            id: file_id(&st),
            ctime: ctime_of(&st),
            pin: (dup >= 0).then(|| unsafe { OwnedFd::from_raw_fd(dup) }),
        })
    }

    /// A file this run has just made, known only by the status `st` -- where
    /// nothing holds it.
    #[cfg(not(target_os = "linux"))]
    pub(crate) fn unpinned(st: &libc::stat) -> Self {
        MadeFile {
            id: file_id(st),
            ctime: ctime_of(st),
            pin: None,
        }
    }

    /// Take the `ctime` the file open on `fd`, this one, shows now: after this
    /// run has written it or set its attributes.
    pub(crate) fn refresh(&mut self, fd: BorrowedFd<'_>) {
        if let Ok(st) = fstat(fd.as_raw_fd()) {
            if file_id(&st) == self.id {
                self.ctime = ctime_of(&st);
            }
        }
    }

    /// Its `(st_dev, st_ino)`.
    pub(crate) fn id(&self) -> (u64, u64) {
        self.id
    }

    /// What `link_replacing_with` is to link: this file, with the `ctime` it
    /// shows now where it is pinned, and as last left otherwise.
    pub(crate) fn expected(&self) -> Expected<'_> {
        let pinned_ctime = self
            .pin
            .as_ref()
            .and_then(|pin| fstat(pin.as_raw_fd()).ok())
            .map(|st| ctime_of(&st));
        Expected {
            id: self.id,
            ctime: Some(pinned_ctime.unwrap_or(self.ctime)),
            pin: self.pin.as_ref().map(|pin| pin.as_fd()),
        }
    }

    /// Whether it holds a pin.
    pub(crate) fn is_pinned(&self) -> bool {
        self.pin.is_some()
    }

    /// Close the pin, keeping the `ctime` it shows last.
    pub(crate) fn unpin(&mut self) {
        if let Some(pin) = self.pin.take() {
            if let Ok(st) = fstat(pin.as_raw_fd()) {
                self.ctime = ctime_of(&st);
            }
        }
    }

    /// The same file, known again for another of its names: a pin of its own
    /// where this one has one and it can be duplicated.
    pub(crate) fn share(&self) -> Self {
        let pin = self.pin.as_ref().and_then(|pin| {
            let fd = unsafe { libc::fcntl(pin.as_raw_fd(), libc::F_DUPFD_CLOEXEC, 0) };
            (fd >= 0).then(|| unsafe { OwnedFd::from_raw_fd(fd) })
        });
        MadeFile {
            id: self.id,
            ctime: self.ctime,
            pin,
        }
    }

    /// A name has just been linked to it, which changed its `ctime`. Pinned,
    /// the pin shows that when it is closed; unpinned, it is read from `name`
    /// in `dirfd` if that holds the file.
    pub(crate) fn linked(&mut self, dirfd: BorrowedFd<'_>, name: &CStr) {
        if self.pin.is_some() {
            return;
        }
        if let Ok(st) = lstat_at(dirfd.as_raw_fd(), name) {
            if file_id(&st) == self.id {
                self.ctime = ctime_of(&st);
            }
        }
    }
}

/// A status's `ctime`, seconds and nanoseconds.
pub(crate) fn ctime_of(st: &libc::stat) -> (i64, i64) {
    // Casts needed: the field types differ between platforms.
    #[allow(clippy::unnecessary_cast)]
    (st.st_ctime as i64, st.st_ctime_nsec as i64)
}

/// An `O_PATH` descriptor for the file open on `fd`, reopened through its
/// `self/fd/N` entry in a verified procfs -- the inode itself, never a name.
#[cfg(target_os = "linux")]
fn reopen_path(fd: BorrowedFd<'_>) -> Option<OwnedFd> {
    let proc_dir = plib::madefs::procfs_dir().ok()?;
    let entry = plib::madefs::proc_fd_name(fd.as_raw_fd());
    let flags = libc::O_PATH | libc::O_CLOEXEC;
    let pin = unsafe { libc::openat(proc_dir.as_raw_fd(), entry.as_ptr(), flags) };
    (pin >= 0).then(|| unsafe { OwnedFd::from_raw_fd(pin) })
}

/// Without `O_PATH` and procfs a file cannot be linked through a descriptor:
/// it is known by its identity and `ctime` alone.
#[cfg(not(target_os = "linux"))]
fn reopen_path(_fd: BorrowedFd<'_>) -> Option<OwnedFd> {
    None
}

/// The holders of pins, keyed by `K`, oldest first, and how many may be held
/// at once: a quarter of the descriptor limit, and at most 256.
///
/// An archive can hold any number of files later names refer to, and a pin
/// kept for each would leave no descriptors for the members after them. Past
/// the budget the oldest holder's pin is closed. What it held then falls back
/// to its bare `(st_dev, st_ino)` -- with its `ctime`, for a file this run
/// made (`MadeFile`) -- checked when it is next used.
pub(crate) struct PinBudget<K> {
    held: VecDeque<K>,
    limit: usize,
}

impl<K: PartialEq> PinBudget<K> {
    pub(crate) fn new() -> Self {
        let mut lim = libc::rlimit {
            rlim_cur: 0,
            rlim_max: 0,
        };
        let soft = if unsafe { libc::getrlimit(libc::RLIMIT_NOFILE, &mut lim) } == 0 {
            lim.rlim_cur
        } else {
            0
        };
        PinBudget {
            held: VecDeque::new(),
            limit: usize::try_from(soft / 4).map_or(256, |quarter| quarter.min(256)),
        }
    }

    /// Account for `key`'s holder, which `pinned` says holds a pin now or not:
    /// note it when it has taken one, forget it once it holds none, and when
    /// that puts the budget over, close the oldest holder's pin (`unpin`).
    pub(crate) fn note(&mut self, key: K, pinned: bool, mut unpin: impl FnMut(&K)) {
        let held_at = self.held.iter().position(|k| *k == key);
        match (pinned, held_at) {
            (false, Some(at)) => {
                self.held.remove(at);
            }
            (true, None) => self.held.push_back(key),
            _ => {}
        }
        while self.held.len() > self.limit {
            let Some(oldest) = self.held.pop_front() else {
                break;
            };
            unpin(&oldest);
        }
    }
}

#[cfg(all(test, target_os = "linux"))]
mod tests {
    use super::*;
    use crate::modes::anchored::{link_replacing_with, DirTree};
    use crate::modes::race_hook::{with_hook, Point};

    /// A file made, `f`, kept alive by a second name so it can still be
    /// linked once `f` is taken from it; another file, `planted`, ready to be
    /// renamed over `f`.
    fn made_and_planted() -> (plib::tmp::TempDir, DirTree) {
        let dir = plib::tmp::TempDir::new().unwrap();
        std::fs::write(dir.path().join("f"), "made\n").unwrap();
        std::fs::hard_link(dir.path().join("f"), dir.path().join("kept")).unwrap();
        std::fs::write(dir.path().join("planted"), "planted\n").unwrap();
        let tree = DirTree::open_path(dir.path()).unwrap();
        (dir, tree)
    }

    /// Rename `planted` over `f` when the link is about to be made.
    fn plant(dir: &std::path::Path) -> impl FnMut(Point, libc::c_int, &CStr) + 'static {
        let path = dir.to_path_buf();
        move |point, _, name| {
            if point == Point::Linking && name == c"f" {
                std::fs::rename(path.join("planted"), path.join("f")).unwrap();
            }
        }
    }

    /// Held by a pin, the file made is linked through the pin: whatever is at
    /// its name by then -- even a file with its very inode number, which the
    /// pin keeps from being reused -- is never looked at.
    #[test]
    fn a_pinned_file_is_linked_through_its_pin() {
        let (dir, tree) = made_and_planted();
        let root = tree.root();
        let f = std::fs::File::open(dir.path().join("f")).unwrap();
        let made = MadeFile::of(f.as_fd()).unwrap();
        assert!(made.is_pinned());
        drop(f);

        let expected = Some(made.expected());
        let linked = with_hook(plant(dir.path()), || {
            link_replacing_with(root.as_raw_fd(), c"f", None, expected, root, c"g", false)
        });
        assert!(linked.is_ok(), "{linked:?}");
        let g = lstat_at(root.as_raw_fd(), c"g").unwrap();
        assert_eq!(file_id(&g), made.id(), "g is not the file made");
    }

    /// Unpinned, a file at its name with the same `(st_dev, st_ino)` -- the
    /// number reused for someone else's file once this one was removed,
    /// simulated by recording the planted file's number -- is told from it by
    /// its `ctime`, and not linked.
    #[test]
    fn an_unpinned_file_is_told_from_a_reused_number_by_its_ctime() {
        let (dir, tree) = made_and_planted();
        let root = tree.root();
        let made_st = lstat_at(root.as_raw_fd(), c"f").unwrap();
        // ctime comes from a coarse clock: let the planted file's come from a
        // later tick, as a file made after this one was removed would.
        let planted = dir.path().join("planted");
        for _ in 0..200 {
            let st = lstat_at(root.as_raw_fd(), c"planted").unwrap();
            if ctime_of(&st) != ctime_of(&made_st) {
                break;
            }
            std::thread::sleep(std::time::Duration::from_millis(5));
            let perms = std::fs::metadata(&planted).unwrap().permissions();
            std::fs::set_permissions(&planted, perms).unwrap();
        }
        let planted_st = lstat_at(root.as_raw_fd(), c"planted").unwrap();
        assert_ne!(ctime_of(&planted_st), ctime_of(&made_st));
        let made = MadeFile {
            id: file_id(&planted_st),
            ctime: ctime_of(&made_st),
            pin: None,
        };

        let expected = Some(made.expected());
        let _ = with_hook(plant(dir.path()), || {
            link_replacing_with(root.as_raw_fd(), c"f", None, expected, root, c"g", false)
        });
        let g = std::fs::read_to_string(dir.path().join("g")).unwrap_or_default();
        assert_ne!(g, "planted\n", "the file at the reused number was linked");
    }

    /// The budget closes the oldest pin once more are held than it allows,
    /// and forgets a holder that has closed its own.
    #[test]
    fn the_budget_closes_the_oldest_pin() {
        let mut budget = PinBudget::<u32> {
            held: VecDeque::new(),
            limit: 2,
        };
        let mut closed = Vec::new();
        for key in 0..3 {
            budget.note(key, true, |k| closed.push(*k));
        }
        assert_eq!(closed, [0]);
        budget.note(1, false, |k| closed.push(*k));
        budget.note(3, true, |k| closed.push(*k));
        assert_eq!(closed, [0]);
        budget.note(4, true, |k| closed.push(*k));
        assert_eq!(closed, [0, 2]);
    }
}
