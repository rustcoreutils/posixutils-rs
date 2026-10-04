//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The Unix terminal: the command channel in raw mode (termios), the
//! signals that must restore it or redraw, and the window size.

use std::fs::File;
use std::io;
use std::mem::MaybeUninit;
use std::os::fd::{FromRawFd, RawFd};
use std::sync::atomic::{AtomicBool, AtomicI32, Ordering};

/// The command channel's reading end: the terminal's file.
pub type Reader = File;

/// What the signal handlers asked the pager to do since it last looked.
pub fn take_pending() -> super::Pending {
    super::Pending {
        resumed: SIGCONT_PENDING.swap(false, Ordering::SeqCst),
        resized: SIGWINCH_PENDING.swap(false, Ordering::SeqCst),
    }
}

/// The error for a terminal with no usable command channel.
fn no_command_source() -> io::Error {
    io::Error::other("no command source")
}

/// The terminal's size as (columns, rows), asked of standard output.
pub fn terminal_size() -> io::Result<(u16, u16)> {
    // SAFETY: TIOCGWINSZ fills the winsize it is handed and nothing else.
    let mut size: libc::winsize = unsafe { std::mem::zeroed() };
    if unsafe { libc::ioctl(libc::STDOUT_FILENO, libc::TIOCGWINSZ, &mut size) } != 0 {
        return Err(io::Error::last_os_error());
    }
    Ok((size.ws_col, size.ws_row))
}

// === Signal handlers ============================================
//
// Why this exists:
//   * POSIX `more` ASYNCHRONOUS EVENTS mandates handling SIGCONT (refetch
//     winsize + redraw) and SIGWINCH (same, after Austin Group Defect 1185).
//   * Without lifecycle-signal handlers, a SIGINT / SIGTERM / SIGHUP /
//     SIGQUIT skips Rust's `Drop`, so the cooked-mode termios captured by
//     [`CommandIO`] is never restored — leaving the user's shell unusable.
//
// Constraints:
//   * Handlers run in async-signal context.  Only a tiny subset of libc is
//     safe: `tcsetattr`, `_exit`, `raise`, `sigaction`, `write`, atomics.
//     No mallocs, no Rust `Mutex` (parking-lot or std), no `println!`.
//   * Cleanup handlers (TERM/INT/HUP/QUIT) call `tcsetattr` and `_exit`
//     directly — there is no main-loop tick guaranteed before process death.
//   * Event handlers (CONT/WINCH) just set an atomic flag; the pager loop
//     polls these and performs the user-visible work in normal context.
//
// State synchronization:
//   * `SIG_STATE_INIT` is the gate: a handler that observes `false` must
//     not touch [`SAVED_FD`], [`SAVED_COOKED`], or [`SAVED_RAW`].
//   * State is written **before** `SIG_STATE_INIT` is set to `true`
//     (publish), and `SIG_STATE_INIT` is cleared **before** the state is
//     torn down (unpublish).  Both orderings use `SeqCst`.
//   * The termios snapshots are written exactly once by
//     [`publish_signal_state`] and read by handlers; they are not mutated
//     thereafter, so a non-atomic memcpy is safe under this discipline.

static SIG_STATE_INIT: AtomicBool = AtomicBool::new(false);
static SAVED_FD: AtomicI32 = AtomicI32::new(-1);
static mut SAVED_COOKED: MaybeUninit<libc::termios> = MaybeUninit::uninit();
static mut SAVED_RAW: MaybeUninit<libc::termios> = MaybeUninit::uninit();

/// Set by the SIGCONT handler.  Main loop swaps this to `false` and forces a
/// full redraw plus winsize refetch (POSIX: SIGCONT must refresh the screen
/// regardless of whether the size actually changed).
static SIGCONT_PENDING: AtomicBool = AtomicBool::new(false);
/// Set by the SIGWINCH handler.  Main loop swaps this and re-fetches winsize.
static SIGWINCH_PENDING: AtomicBool = AtomicBool::new(false);

/// Publish the fd + cooked/raw termios snapshots that the handlers read.
/// Call this **once**, before [`install_signal_handlers`].
///
/// # Safety
///
/// Caller must guarantee no handler is currently running on the static state
/// (which is true at startup before `install_signal_handlers`).
unsafe fn publish_signal_state(fd: RawFd, cooked: libc::termios, raw: libc::termios) {
    // Write through raw pointers to avoid forming references to the
    // mutable statics (deny-by-default in Rust 2024; warning under 2021).
    let cooked_ptr = std::ptr::addr_of_mut!(SAVED_COOKED);
    let raw_ptr = std::ptr::addr_of_mut!(SAVED_RAW);
    (*cooked_ptr).write(cooked);
    (*raw_ptr).write(raw);
    SAVED_FD.store(fd, Ordering::SeqCst);
    SIG_STATE_INIT.store(true, Ordering::SeqCst);
}

/// Clear the published signal state.  Call this **after**
/// [`uninstall_signal_handlers`] so no in-flight handler races us.
fn unpublish_signal_state() {
    SIG_STATE_INIT.store(false, Ordering::SeqCst);
    SAVED_FD.store(-1, Ordering::SeqCst);
    // No need to clear SAVED_COOKED / SAVED_RAW — handlers gate on
    // SIG_STATE_INIT before reading them.
}

/// Try to restore the cooked-mode termios.  Async-signal-safe: only calls
/// `tcsetattr` and atomic loads.  No-op if state hasn't been published.
fn try_restore_cooked() {
    if !SIG_STATE_INIT.load(Ordering::SeqCst) {
        return;
    }
    let fd = SAVED_FD.load(Ordering::SeqCst);
    if fd < 0 {
        return;
    }
    // SAFETY: SIG_STATE_INIT was true ⇒ publish_signal_state ran ⇒
    // SAVED_COOKED is initialized and never mutated thereafter.  We hand a
    // raw pointer to libc to avoid forming a reference to mutable static.
    unsafe {
        let ptr = std::ptr::addr_of!(SAVED_COOKED) as *const libc::termios;
        libc::tcsetattr(fd, libc::TCSAFLUSH, ptr);
    }
}

/// Try to (re)apply the raw-mode termios.  Used by the SIGCONT handler to
/// re-engage raw mode after a SIGTSTP/SIGCONT job-control cycle.
fn try_apply_raw() {
    if !SIG_STATE_INIT.load(Ordering::SeqCst) {
        return;
    }
    let fd = SAVED_FD.load(Ordering::SeqCst);
    if fd < 0 {
        return;
    }
    // SAFETY: see [`try_restore_cooked`].
    unsafe {
        let ptr = std::ptr::addr_of!(SAVED_RAW) as *const libc::termios;
        libc::tcsetattr(fd, libc::TCSAFLUSH, ptr);
    }
}

/// Handler for SIGINT, SIGTERM, SIGHUP, SIGQUIT.  Restores cooked-mode
/// termios and then `_exit`s with `128 + signum` (the POSIX shell
/// convention for signal-caused exits).  Drop impls are intentionally
/// skipped — by the time these signals arrive we want to die *now*, before
/// any further harm.
extern "C" fn restore_and_exit(signum: libc::c_int) {
    try_restore_cooked();
    unsafe { libc::_exit(128 + signum) };
}

/// Handler for SIGTSTP (job-control stop, typically Ctrl-Z).  Restores
/// cooked mode so the parent shell finds the terminal usable, then resets
/// SIGTSTP to its default disposition and re-raises it so the kernel
/// actually suspends us.  On resume, [`handle_cont`] re-engages raw mode
/// and re-installs this handler.
extern "C" fn handle_tstp(_signum: libc::c_int) {
    try_restore_cooked();
    unsafe {
        let mut act: libc::sigaction = std::mem::zeroed();
        act.sa_sigaction = libc::SIG_DFL;
        libc::sigemptyset(&mut act.sa_mask);
        libc::sigaction(libc::SIGTSTP, &act, std::ptr::null_mut());
        libc::raise(libc::SIGTSTP);
    }
}

/// Handler for SIGCONT (resume from suspend).  Re-applies raw mode (we
/// were stopped in cooked mode by [`handle_tstp`]) and sets the pending
/// flag so the main loop refreshes the screen.  Also re-installs the
/// SIGTSTP handler that [`handle_tstp`] reset to `SIG_DFL`.
extern "C" fn handle_cont(_signum: libc::c_int) {
    try_apply_raw();
    SIGCONT_PENDING.store(true, Ordering::SeqCst);
    unsafe { install_tstp_handler() };
}

/// Handler for SIGWINCH (terminal-window-size change).  Just sets the
/// pending flag — the main loop reads the new winsize via `tcgetwinsize`
/// and triggers a redraw.
extern "C" fn handle_winch(_signum: libc::c_int) {
    SIGWINCH_PENDING.store(true, Ordering::SeqCst);
}

unsafe fn install_handler_raw(
    signum: libc::c_int,
    handler: extern "C" fn(libc::c_int),
    flags: libc::c_int,
) {
    let mut act: libc::sigaction = std::mem::zeroed();
    act.sa_sigaction = handler as libc::sighandler_t;
    act.sa_flags = flags;
    libc::sigemptyset(&mut act.sa_mask);
    libc::sigaction(signum, &act, std::ptr::null_mut());
}

unsafe fn install_tstp_handler() {
    install_handler_raw(libc::SIGTSTP, handle_tstp, 0);
}

/// Install handlers for every signal we care about.  Call **after**
/// [`publish_signal_state`].
fn install_signal_handlers() {
    unsafe {
        // Lifecycle: restore termios + _exit.  No SA_RESTART — we want
        // these to interrupt syscalls and cause immediate termination.
        install_handler_raw(libc::SIGINT, restore_and_exit, 0);
        install_handler_raw(libc::SIGTERM, restore_and_exit, 0);
        install_handler_raw(libc::SIGHUP, restore_and_exit, 0);
        install_handler_raw(libc::SIGQUIT, restore_and_exit, 0);
        // Job control.
        install_tstp_handler();
        // Async events: SA_RESTART so the input thread's read() keeps going.
        install_handler_raw(libc::SIGCONT, handle_cont, libc::SA_RESTART);
        install_handler_raw(libc::SIGWINCH, handle_winch, libc::SA_RESTART);
    }
}

/// Restore SIG_DFL for every signal we installed.  Call **before**
/// [`unpublish_signal_state`].
fn uninstall_signal_handlers() {
    unsafe {
        for signum in &[
            libc::SIGINT,
            libc::SIGTERM,
            libc::SIGHUP,
            libc::SIGQUIT,
            libc::SIGTSTP,
            libc::SIGCONT,
            libc::SIGWINCH,
        ] {
            let mut act: libc::sigaction = std::mem::zeroed();
            act.sa_sigaction = libc::SIG_DFL;
            libc::sigemptyset(&mut act.sa_mask);
            libc::sigaction(*signum, &act, std::ptr::null_mut());
        }
    }
}

// === End of signal handlers ====================================

/// Terminal channel used for reading user commands and writing the prompt.
///
/// POSIX.1-2024 (`more`, INPUT FILES / STDERR sections) requires that when
/// standard output is a terminal, user commands are read from **standard
/// error**; if standard error is not readable, the implementation may attempt
/// to open the controlling terminal (`/dev/tty`); if neither is available it
/// must terminate with an error.  The same channel is used to write the
/// prompt and cursor-control sequences (`STDERR` section).
///
/// This struct owns a file descriptor opened on the chosen channel and places
/// it in raw mode for the lifetime of the pager.  Reader and writer are
/// independent `File` handles created from duplicated file descriptors so the
/// input thread can take the reader by value while the main thread writes the
/// prompt to the writer.  `Drop` restores the original termios and closes the
/// owned fd; each `File` closes its own duplicate.
pub struct CommandIO {
    /// Owned fd in raw mode; closed (after termios restoration) by Drop.
    fd: RawFd,
    /// Cooked-mode termios captured at construction, restored by Drop.
    original_termios: libc::termios,
}

impl CommandIO {
    /// Open a command-input channel per the POSIX-literal precedence rule:
    /// try stderr first (must be a terminal and readable); fall back to
    /// `/dev/tty` (opened `O_RDWR | O_NOCTTY`); error if neither is usable.
    ///
    /// Returns `(channel, reader, writer)`.  The caller hands `reader` to the
    /// input thread and `writer` to the prompt-rendering code; `channel`
    /// retains the original-termios state for restoration on Drop.  Only
    /// meaningful when standard output is a terminal — callers must check
    /// `std::io::stdout().is_terminal()` first and skip this in filter mode.
    pub fn open() -> io::Result<(Self, File, File)> {
        let fd = pick_command_fd()?;
        // Save cooked-mode termios for restoration in Drop.
        let mut original = unsafe { std::mem::zeroed::<libc::termios>() };
        if unsafe { libc::tcgetattr(fd, &mut original) } != 0 {
            unsafe { libc::close(fd) };
            return Err(no_command_source());
        }
        // Derive raw-mode termios.
        let mut raw = original;
        unsafe { libc::cfmakeraw(&mut raw) };
        // VMIN=1 / VTIME=0 — block until at least one byte is available.
        raw.c_cc[libc::VMIN] = 1;
        raw.c_cc[libc::VTIME] = 0;
        // Publish (fd, cooked, raw) for the signal handlers BEFORE applying
        // raw mode.  This ordering matters: if a signal fires during the
        // tcsetattr below, the handler must already see the cooked snapshot
        // so it can restore on _exit.  We then install handlers so they take
        // over from the default dispositions before any raw-mode I/O begins.
        unsafe { publish_signal_state(fd, original, raw) };
        install_signal_handlers();
        // Now apply raw mode.
        if unsafe { libc::tcsetattr(fd, libc::TCSAFLUSH, &raw) } != 0 {
            uninstall_signal_handlers();
            unpublish_signal_state();
            unsafe { libc::close(fd) };
            return Err(no_command_source());
        }
        // Duplicate fd twice so reader and writer each own a distinct
        // descriptor — they are closed when each File is dropped.  The
        // original fd remains owned by `self` for termios restoration.
        let read_fd = unsafe { libc::dup(fd) };
        let write_fd = unsafe { libc::dup(fd) };
        if read_fd < 0 || write_fd < 0 {
            if read_fd >= 0 {
                unsafe { libc::close(read_fd) };
            }
            if write_fd >= 0 {
                unsafe { libc::close(write_fd) };
            }
            unsafe { libc::tcsetattr(fd, libc::TCSAFLUSH, &original) };
            uninstall_signal_handlers();
            unpublish_signal_state();
            unsafe { libc::close(fd) };
            return Err(no_command_source());
        }
        let reader = unsafe { File::from_raw_fd(read_fd) };
        let writer = unsafe { File::from_raw_fd(write_fd) };
        Ok((
            Self {
                fd,
                original_termios: original,
            },
            reader,
            writer,
        ))
    }
}

impl Drop for CommandIO {
    fn drop(&mut self) {
        // Order matters:
        //   1. Restore termios while handlers are still installed (so an
        //      in-flight signal sees consistent state).
        //   2. Uninstall handlers (revert to SIG_DFL).  Now any subsequent
        //      signal takes the kernel's default action on the cooked tty.
        //   3. Unpublish the static state so a handler that races us
        //      cannot read freed fd values.
        //   4. Close the owned fd.
        unsafe {
            libc::tcsetattr(self.fd, libc::TCSAFLUSH, &self.original_termios);
        }
        uninstall_signal_handlers();
        unpublish_signal_state();
        unsafe {
            libc::close(self.fd);
        }
    }
}

/// True if the given fd refers to a terminal AND is open for reading.
/// POSIX requires both checks before we use a descriptor as a command source.
fn is_readable_terminal(fd: RawFd) -> bool {
    if unsafe { libc::isatty(fd) } != 1 {
        return false;
    }
    let flags = unsafe { libc::fcntl(fd, libc::F_GETFL) };
    if flags < 0 {
        return false;
    }
    let access_mode = flags & libc::O_ACCMODE;
    access_mode == libc::O_RDONLY || access_mode == libc::O_RDWR
}

/// Pick the file descriptor to use for reading user commands.
///
/// Returns an owned fd opened `O_RDWR` (or duped from stderr) that the caller
/// must close.  Used by `CommandIO::open`.
fn pick_command_fd() -> io::Result<RawFd> {
    // 1. stderr: only if it is itself a terminal AND open for reading.
    if is_readable_terminal(libc::STDERR_FILENO) {
        let dup = unsafe { libc::dup(libc::STDERR_FILENO) };
        if dup >= 0 {
            return Ok(dup);
        }
    }
    // 2. /dev/tty fallback (POSIX permits this).
    let path = c"/dev/tty";
    let fd = unsafe { libc::open(path.as_ptr(), libc::O_RDWR | libc::O_NOCTTY) };
    if fd >= 0 {
        return Ok(fd);
    }
    Err(no_command_source())
}
