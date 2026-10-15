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
    /// Tokens in a pipe, shared with the sub-makes started for it.
    Pipe { read: RawFd, write: RawFd },
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
    let pool = match pool.or_else(|| create(jobs - 1)) {
        Some(pool) => pool,
        // No pipe to be had: the tokens stay in this make.
        None => return (jobs, String::new()),
    };
    let flags = match &pool {
        Pool::Pipe { read, write } => format!("-j{jobs} --jobserver-auth={read},{write}"),
        Pool::Local(_) => String::new(),
    };
    let _ = SHARED.set(Arc::new(pool));
    (jobs, flags)
}

/// A new pipe holding `tokens` tokens.
fn create(tokens: usize) -> Option<Pool> {
    let mut fds = [0; 2];
    // SAFETY: `fds` has room for the two descriptors pipe(2) writes.
    if unsafe { libc::pipe(fds.as_mut_ptr()) } != 0 {
        return None;
    }
    let pool = Pool::Pipe {
        read: fds[0],
        write: fds[1],
    };
    if !own(fds[0]) || !own(fds[1]) {
        close(fds);
        return None;
    }
    for _ in 0..tokens {
        pool.release();
    }
    Some(pool)
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
    if !is_fifo(&r) || !is_fifo(&w) || r.st_ino != w.st_ino || r.st_dev != w.st_dev {
        return None;
    }
    if !own(read) || !own(write) {
        return None;
    }
    Some(Pool::Pipe { read, write })
}

/// Make `fd` close when a command is run, so that only a sub-make is given
/// it, and make reading it never wait.
fn own(fd: RawFd) -> bool {
    // SAFETY: fcntl(2) on a descriptor number, with integer arguments.
    unsafe {
        let flags = libc::fcntl(fd, libc::F_GETFL);
        flags >= 0
            && libc::fcntl(fd, libc::F_SETFL, flags | libc::O_NONBLOCK) == 0
            && libc::fcntl(fd, libc::F_SETFD, libc::FD_CLOEXEC) == 0
    }
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
            Pool::Pipe { read, .. } => loop {
                let mut byte = 0u8;
                // SAFETY: reading one byte into `byte`.
                let got = unsafe { libc::read(*read, (&raw mut byte).cast(), 1) };
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
                    // A pipe holds far more than maxjobs bytes; it is full
                    // only for an instant, if a token is being written as
                    // fast as it is read.
                    continue;
                }
                return;
            },
        }
    }

    /// The descriptors a sub-make must be given to take part in the pool.
    pub fn descriptors(&self) -> Option<[RawFd; 2]> {
        match self {
            Pool::Pipe { read, write } => Some([*read, *write]),
            Pool::Local(_) => None,
        }
    }
}
