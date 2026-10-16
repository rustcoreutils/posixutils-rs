//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The token pool of `-j maxjobs`, shared with sub-makes.
//!
//! POSIX 105360-105384: make creates a pool of up to maxjobs - 1 tokens, and
//! a sub-make that the `MAKE` macro or a `+` command line starts, given the
//! same `-j` in `MAKEFLAGS`, "shall use the same token pool as its invoking
//! make". The pool a make shares is a pipe holding one byte per token, its
//! descriptors named in `MAKEFLAGS` as GNU make names them
//! (`--jobserver-auth=R,W`), so that GNU make and this one each take part
//! in the other's pool. A make with no sub-makes to share with keeps its
//! tokens in a counter.

use std::os::fd::RawFd;
use std::sync::{Arc, Mutex, OnceLock};

/// The byte that stands for one token in the pipe, as in GNU make.
const TOKEN: u8 = b'+';

/// The tokens bounding how many targets are updated at once. Taking one
/// never waits: a thread that cannot get one makes its target itself, which
/// keeps the build from deadlocking.
pub enum Pool {
    /// Tokens no other process takes part in.
    Local(Mutex<usize>),
    /// Tokens in a pipe, shared with the sub-makes started for it: `read`
    /// and `write` are the ends they are given, and `reader` the
    /// non-blocking descriptor this make takes tokens through.
    Pipe {
        read: RawFd,
        write: RawFd,
        reader: RawFd,
    },
}

/// The pool `main` set up for the run, if it set one up.
static SHARED: OnceLock<Arc<Pool>> = OnceLock::new();

/// The pool for the run: the one `main` set up, or for a `Make` built
/// without one a counter of `jobs - 1` tokens.
pub fn pool_for(jobs: usize) -> Arc<Pool> {
    match SHARED.get() {
        Some(pool) => Arc::clone(pool),
        None => Arc::new(Pool::Local(Mutex::new(jobs.saturating_sub(1)))),
    }
}

/// The descriptors of the run's pool, which a command line that starts a
/// sub-make leaves open for it.
pub fn shared_descriptors() -> Option<[RawFd; 2]> {
    SHARED.get().and_then(|pool| pool.descriptors())
}

/// Where `-j` came from, for `setup`.
pub enum Jobs<'a> {
    /// No `-j` anywhere.
    None,
    /// `-j` on this make's command line: a pool of its own.
    CommandLine(usize),
    /// `-j` from a parent in `MAKEFLAGS`, with the pool it named, if any.
    Inherited(usize, Option<&'a str>),
}

/// Set up the run's pool and return maxjobs and the `MAKEFLAGS` words that
/// pass both to sub-makes (empty for maxjobs 1).
///
/// A parent's pool that cannot be had -- its descriptors were not left open
/// for this make, which happens when a command line starts it without
/// `$(MAKE)` or `+` -- leaves the make serial, as GNU make does: making a
/// pool of its own instead would run more jobs than the user asked for.
pub fn setup(jobs: Jobs<'_>) -> (usize, String) {
    let (jobs, pool) = match jobs {
        Jobs::None => (1, None),
        Jobs::CommandLine(jobs) => (jobs, None),
        Jobs::Inherited(jobs, Some(auth)) if jobs > 1 => match pipe_descriptors(auth) {
            // A pool of a kind this make does not take part in: GNU make
            // 4.4's `fifo:PATH`.
            None => (1, None),
            Some(fds) => match join(fds) {
                Some(pool) => (jobs, Some(pool)),
                None => {
                    eprintln!(
                        "make: {}",
                        gettextrs::gettext(
                            "warning: jobserver unavailable: using -j1.  Add '+' to parent make rule."
                        )
                    );
                    (1, None)
                }
            },
        },
        Jobs::Inherited(jobs, _) => (jobs, None),
    };
    if jobs <= 1 {
        return (1, String::new());
    }
    let (jobs, pool) = match pool {
        Some(pool) => (jobs, pool),
        None => match create(jobs - 1) {
            // A pipe holds fewer tokens than were asked for: the jobs are as
            // many as it holds, plus the one this make has without a token.
            Some((pool, tokens)) => (tokens + 1, pool),
            // No pipe to be had: the tokens stay in this make.
            None => return (jobs, String::new()),
        },
    };
    let flags = match &pool {
        Pool::Pipe { read, write, .. } => format!("-j{jobs} --jobserver-auth={read},{write}"),
        Pool::Local(_) => String::new(),
    };
    let _ = SHARED.set(Arc::new(pool));
    (jobs, flags)
}

/// A new pipe holding `tokens` tokens, or as many as fit in it, and how many
/// it holds.
fn create(tokens: usize) -> Option<(Pool, usize)> {
    let mut fds = [0; 2];
    // SAFETY: `fds` has room for the two descriptors pipe(2) writes.
    if unsafe { libc::pipe(fds.as_mut_ptr()) } != 0 {
        return None;
    }
    let pool = Pool::Pipe {
        read: fds[0],
        write: fds[1],
        reader: fds[0],
    };
    if !own(fds[0]) || !own(fds[1]) || !set_nonblocking(fds[0]) {
        close(fds);
        return None;
    }
    // A pipe holds a fixed number of bytes, 64 KiB on Linux, so a blocking
    // write of more tokens than that would wait forever for a reader. No one
    // else has the pipe yet: fill it through a non-blocking write end, stop
    // where it is full, keep half of what it held, and leave the write end
    // blocking again.
    let filled = match fill(fds[1], tokens) {
        Some(filled) if filled < tokens => drain_half(fds[0], filled),
        filled => filled,
    };
    // SAFETY: fcntl(2) on a descriptor number, with integer arguments.
    let restored = unsafe {
        let flags = libc::fcntl(fds[1], libc::F_GETFL);
        flags >= 0 && libc::fcntl(fds[1], libc::F_SETFL, flags & !libc::O_NONBLOCK) == 0
    };
    match filled {
        Some(filled) if restored => Some((pool, filled)),
        _ => {
            close(fds);
            None
        }
    }
}

/// Write up to `tokens` tokens to the new pipe's write end `fd` and return
/// how many went in before the pipe was full.
fn fill(fd: RawFd, tokens: usize) -> Option<usize> {
    if !set_nonblocking(fd) {
        return None;
    }
    let chunk = [TOKEN; 4096];
    let mut filled = 0;
    while filled < tokens {
        let want = (tokens - filled).min(chunk.len());
        // SAFETY: writing at most `chunk.len()` bytes from `chunk`.
        let wrote = unsafe { libc::write(fd, chunk.as_ptr().cast(), want) };
        if wrote > 0 {
            filled += wrote as usize;
            continue;
        }
        match std::io::Error::last_os_error().kind() {
            std::io::ErrorKind::Interrupted => continue,
            std::io::ErrorKind::WouldBlock => return Some(filled),
            _ => return None,
        }
    }
    Some(filled)
}

/// Take back half of the tokens of a pipe `fill` found full, through its
/// non-blocking read end `fd`, and return how many are left.
///
/// A full pipe has no room to give a token back: Linux keeps a pipe's bytes
/// in a ring of pages, and the page a token was read from is not free again
/// until all of it has been read, so the first token returned after one was
/// taken would wait forever.
fn drain_half(fd: RawFd, filled: usize) -> Option<usize> {
    let mut chunk = [0u8; 4096];
    let mut left = filled;
    while left > filled / 2 {
        let want = (left - filled / 2).min(chunk.len());
        // SAFETY: reading at most `chunk.len()` bytes into `chunk`.
        let got = unsafe { libc::read(fd, chunk.as_mut_ptr().cast(), want) };
        if got > 0 {
            left -= got as usize;
        } else if got < 0
            && std::io::Error::last_os_error().kind() == std::io::ErrorKind::Interrupted
        {
            continue;
        } else {
            return None;
        }
    }
    Some(left)
}

/// The descriptors of a pool named as `R,W`.
fn pipe_descriptors(auth: &str) -> Option<[RawFd; 2]> {
    let (read, write) = auth.split_once(',')?;
    Some([read.parse().ok()?, write.parse().ok()?])
}

/// The pipe a parent named, if this make has it.
///
/// Both descriptors must be open and be the two ends of one pipe: a
/// descriptor that a parent did not leave open may have been reused for
/// something else by the time this make starts.
fn join([read, write]: [RawFd; 2]) -> Option<Pool> {
    let stat = |fd: RawFd| -> Option<libc::stat> {
        let mut st = std::mem::MaybeUninit::<libc::stat>::uninit();
        // SAFETY: fstat(2) fills `st` when it returns 0.
        (unsafe { libc::fstat(fd, st.as_mut_ptr()) } == 0).then(|| unsafe { st.assume_init() })
    };
    let (r, w) = (stat(read)?, stat(write)?);
    let is_fifo = |st: &libc::stat| st.st_mode & libc::S_IFMT == libc::S_IFIFO;
    if !is_fifo(&r) || !is_fifo(&w) {
        return None;
    }
    // The read end must be open for reading and the write end for writing.
    let mode = |fd: RawFd| {
        // SAFETY: fcntl(2) F_GETFL on a descriptor number.
        unsafe { libc::fcntl(fd, libc::F_GETFL) }
    };
    let (rm, wm) = (mode(read), mode(write));
    if rm < 0
        || wm < 0
        || rm & libc::O_ACCMODE != libc::O_RDONLY
        || wm & libc::O_ACCMODE != libc::O_WRONLY
    {
        return None;
    }
    // On Linux both ends of one pipe report the same inode; on macOS each end
    // has an inode of its own, so the check cannot be made there.
    #[cfg(target_os = "linux")]
    if r.st_ino != w.st_ino || r.st_dev != w.st_dev {
        return None;
    }
    if !own(read) || !own(write) {
        return None;
    }
    let reader = match private_reader(read) {
        Some(reader) => reader,
        None if set_nonblocking(read) => read,
        None => return None,
    };
    Some(Pool::Pipe {
        read,
        write,
        reader,
    })
}

/// Make `fd` close when a command is run, so that only a sub-make is given
/// it. The flag is this process's own, unlike the file status flags.
fn own(fd: RawFd) -> bool {
    // SAFETY: fcntl(2) on a descriptor number, with an integer argument.
    unsafe { libc::fcntl(fd, libc::F_SETFD, libc::FD_CLOEXEC) == 0 }
}

/// Make reading `fd` never wait. O_NONBLOCK is a flag of the open file
/// description, which every process holding a copy of `fd` shares.
fn set_nonblocking(fd: RawFd) -> bool {
    // SAFETY: fcntl(2) on a descriptor number, with integer arguments.
    unsafe {
        let flags = libc::fcntl(fd, libc::F_GETFL);
        flags >= 0 && libc::fcntl(fd, libc::F_SETFL, flags | libc::O_NONBLOCK) == 0
    }
}

/// A non-blocking descriptor for reading a parent's pipe `read` of an open
/// file description of this make's own.
///
/// A parent's descriptors share theirs with the parent and every other
/// sub-make, so setting O_NONBLOCK on them changed the pipe under all of
/// them. Linux opens the pipe again through `/proc`, as a new description.
/// Elsewhere there is no such path (`/dev/fd/N` on macOS duplicates the
/// descriptor), and `read` itself is made non-blocking, as GNU make makes
/// it, which every client of the pool then copes with.
#[cfg(target_os = "linux")]
fn private_reader(read: RawFd) -> Option<RawFd> {
    let path = std::ffi::CString::new(format!("/proc/self/fd/{read}")).ok()?;
    // SAFETY: open(2) on a NUL-terminated path.
    let fd = unsafe {
        libc::open(
            path.as_ptr(),
            libc::O_RDONLY | libc::O_NONBLOCK | libc::O_CLOEXEC,
        )
    };
    (fd >= 0).then_some(fd)
}

#[cfg(not(target_os = "linux"))]
fn private_reader(_read: RawFd) -> Option<RawFd> {
    None
}

fn close(fds: [RawFd; 2]) {
    for fd in fds {
        // SAFETY: closing descriptors this make created and no one else uses.
        unsafe { libc::close(fd) };
    }
}

impl Pool {
    /// Take a token if one is free, without waiting.
    pub fn try_acquire(&self) -> bool {
        match self {
            Pool::Local(available) => {
                let mut available = available.lock().unwrap();
                if *available > 0 {
                    *available -= 1;
                    true
                } else {
                    false
                }
            }
            Pool::Pipe { reader, .. } => loop {
                let mut byte = 0u8;
                // SAFETY: reading one byte into `byte`.
                let got = unsafe { libc::read(*reader, (&raw mut byte).cast(), 1) };
                if got == 1 {
                    return true;
                }
                if got < 0
                    && std::io::Error::last_os_error().kind() == std::io::ErrorKind::Interrupted
                {
                    continue;
                }
                return false;
            },
        }
    }

    /// Give a token back.
    pub fn release(&self) {
        match self {
            Pool::Local(available) => *available.lock().unwrap() += 1,
            Pool::Pipe { write, .. } => loop {
                // SAFETY: writing one byte from a one-byte slice.
                let put = unsafe { libc::write(*write, [TOKEN].as_ptr().cast(), 1) };
                if put == 1 {
                    return;
                }
                let err = std::io::Error::last_os_error();
                if put < 0
                    && matches!(
                        err.kind(),
                        std::io::ErrorKind::Interrupted | std::io::ErrorKind::WouldBlock
                    )
                {
                    // This make leaves the write end blocking, but another
                    // client of the pool may not. A pipe holds far more
                    // than maxjobs bytes; it is full only for an instant,
                    // if a token is being written as fast as it is read.
                    continue;
                }
                return;
            },
        }
    }

    /// The descriptors a sub-make must be given to take part in the pool.
    pub fn descriptors(&self) -> Option<[RawFd; 2]> {
        match self {
            Pool::Pipe { read, write, .. } => Some([*read, *write]),
            Pool::Local(_) => None,
        }
    }
}
