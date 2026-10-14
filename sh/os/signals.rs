//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::trap::TrapAction;
use crate::os::errno::Errno;
use crate::os::{close, dup_private, pipe, read, OsError, OsResult, Pid};
use std::fmt::{Display, Formatter};
use std::os::fd::{AsRawFd, FromRawFd, IntoRawFd, OwnedFd, RawFd};
use std::str::FromStr;
use std::sync::atomic::{AtomicBool, AtomicI32, Ordering};

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Signal {
    SigHup,
    SigInt,
    SigQuit,
    SigIll,
    SigTrap,
    SigAbrt,
    SigBus,
    SigFpe,
    SigKill,
    SigUsr1,
    SigSegv,
    SigUsr2,
    SigPipe,
    SigAlrm,
    SigTerm,
    SigChld,
    SigCont,
    SigStop,
    SigTstp,
    SigTtin,
    SigTtou,
    SigUrg,
    SigXcpu,
    SigXfsz,
    SigVtalrm,
    SigProf,
    SigSys,

    Count,
}

impl Display for Signal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Signal::SigHup => write!(f, "HUP"),
            Signal::SigInt => write!(f, "INT"),
            Signal::SigQuit => write!(f, "QUIT"),
            Signal::SigIll => write!(f, "ILL"),
            Signal::SigTrap => write!(f, "TRAP"),
            Signal::SigAbrt => write!(f, "ABRT"),
            Signal::SigBus => write!(f, "BUS"),
            Signal::SigFpe => write!(f, "FPE"),
            Signal::SigKill => write!(f, "KILL"),
            Signal::SigUsr1 => write!(f, "USR1"),
            Signal::SigSegv => write!(f, "SEGV"),
            Signal::SigUsr2 => write!(f, "USR2"),
            Signal::SigPipe => write!(f, "PIPE"),
            Signal::SigAlrm => write!(f, "ALRM"),
            Signal::SigTerm => write!(f, "TERM"),
            Signal::SigChld => write!(f, "CHLD"),
            Signal::SigCont => write!(f, "CONT"),
            Signal::SigStop => write!(f, "STOP"),
            Signal::SigTstp => write!(f, "TSTP"),
            Signal::SigTtin => write!(f, "TTIN"),
            Signal::SigTtou => write!(f, "TTOU"),
            Signal::SigUrg => write!(f, "URG"),
            Signal::SigXcpu => write!(f, "XCPU"),
            Signal::SigXfsz => write!(f, "XFSZ"),
            Signal::SigVtalrm => write!(f, "VTALRM"),
            Signal::SigProf => write!(f, "PROF"),
            Signal::SigSys => write!(f, "SYS"),
            Signal::Count => unreachable!("invalid trap condition"),
        }
    }
}

impl FromStr for Signal {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // A numeric signal number (the real libc number).
        if let Ok(n) = s.parse::<i32>() {
            return Signal::try_from(n).map_err(|_| ());
        }
        // Signal names are case-independent and may carry a leading "SIG".
        let upper = s.to_ascii_uppercase();
        let name = upper.strip_prefix("SIG").unwrap_or(&upper);
        match name {
            "HUP" => Ok(Signal::SigHup),
            "INT" => Ok(Signal::SigInt),
            "QUIT" => Ok(Signal::SigQuit),
            "ILL" => Ok(Signal::SigIll),
            "TRAP" => Ok(Signal::SigTrap),
            "ABRT" => Ok(Signal::SigAbrt),
            "BUS" => Ok(Signal::SigBus),
            "FPE" => Ok(Signal::SigFpe),
            "KILL" => Ok(Signal::SigKill),
            "USR1" => Ok(Signal::SigUsr1),
            "SEGV" => Ok(Signal::SigSegv),
            "USR2" => Ok(Signal::SigUsr2),
            "PIPE" => Ok(Signal::SigPipe),
            "ALRM" => Ok(Signal::SigAlrm),
            "TERM" => Ok(Signal::SigTerm),
            "CHLD" => Ok(Signal::SigChld),
            "CONT" => Ok(Signal::SigCont),
            "STOP" => Ok(Signal::SigStop),
            "TSTP" => Ok(Signal::SigTstp),
            "TTIN" => Ok(Signal::SigTtin),
            "TTOU" => Ok(Signal::SigTtou),
            "URG" => Ok(Signal::SigUrg),
            "XCPU" => Ok(Signal::SigXcpu),
            "XFSZ" => Ok(Signal::SigXfsz),
            "VTALRM" => Ok(Signal::SigVtalrm),
            "PROF" => Ok(Signal::SigProf),
            "SYS" => Ok(Signal::SigSys),
            _ => Err(()),
        }
    }
}

impl From<Signal> for i32 {
    fn from(value: Signal) -> i32 {
        match value {
            Signal::SigHup => libc::SIGHUP,
            Signal::SigInt => libc::SIGINT,
            Signal::SigQuit => libc::SIGQUIT,
            Signal::SigIll => libc::SIGILL,
            Signal::SigTrap => libc::SIGTRAP,
            Signal::SigAbrt => libc::SIGABRT,
            Signal::SigBus => libc::SIGBUS,
            Signal::SigFpe => libc::SIGFPE,
            Signal::SigKill => libc::SIGKILL,
            Signal::SigUsr1 => libc::SIGUSR1,
            Signal::SigSegv => libc::SIGSEGV,
            Signal::SigUsr2 => libc::SIGUSR2,
            Signal::SigPipe => libc::SIGPIPE,
            Signal::SigAlrm => libc::SIGALRM,
            Signal::SigTerm => libc::SIGTERM,
            Signal::SigChld => libc::SIGCHLD,
            Signal::SigCont => libc::SIGCONT,
            Signal::SigStop => libc::SIGSTOP,
            Signal::SigTstp => libc::SIGTSTP,
            Signal::SigTtin => libc::SIGTTIN,
            Signal::SigTtou => libc::SIGTTOU,
            Signal::SigUrg => libc::SIGURG,
            Signal::SigXcpu => libc::SIGXCPU,
            Signal::SigXfsz => libc::SIGXFSZ,
            Signal::SigVtalrm => libc::SIGVTALRM,
            Signal::SigProf => libc::SIGPROF,
            Signal::SigSys => libc::SIGSYS,
            Signal::Count => unreachable!("invalid trap condition"),
        }
    }
}

impl TryFrom<i32> for Signal {
    type Error = ();

    fn try_from(value: i32) -> Result<Self, Self::Error> {
        match value {
            libc::SIGHUP => Ok(Signal::SigHup),
            libc::SIGINT => Ok(Signal::SigInt),
            libc::SIGQUIT => Ok(Signal::SigQuit),
            libc::SIGILL => Ok(Signal::SigIll),
            libc::SIGTRAP => Ok(Signal::SigTrap),
            libc::SIGABRT => Ok(Signal::SigAbrt),
            libc::SIGBUS => Ok(Signal::SigBus),
            libc::SIGFPE => Ok(Signal::SigFpe),
            libc::SIGKILL => Ok(Signal::SigKill),
            libc::SIGUSR1 => Ok(Signal::SigUsr1),
            libc::SIGSEGV => Ok(Signal::SigSegv),
            libc::SIGUSR2 => Ok(Signal::SigUsr2),
            libc::SIGPIPE => Ok(Signal::SigPipe),
            libc::SIGALRM => Ok(Signal::SigAlrm),
            libc::SIGTERM => Ok(Signal::SigTerm),
            libc::SIGCHLD => Ok(Signal::SigChld),
            libc::SIGCONT => Ok(Signal::SigCont),
            libc::SIGSTOP => Ok(Signal::SigStop),
            libc::SIGTSTP => Ok(Signal::SigTstp),
            libc::SIGTTIN => Ok(Signal::SigTtin),
            libc::SIGTTOU => Ok(Signal::SigTtou),
            libc::SIGURG => Ok(Signal::SigUrg),
            libc::SIGXCPU => Ok(Signal::SigXcpu),
            libc::SIGXFSZ => Ok(Signal::SigXfsz),
            libc::SIGVTALRM => Ok(Signal::SigVtalrm),
            libc::SIGPROF => Ok(Signal::SigProf),
            libc::SIGSYS => Ok(Signal::SigSys),
            _ => Err(()),
        }
    }
}

pub const SIGNALS: &[Signal] = &[
    Signal::SigHup,
    Signal::SigInt,
    Signal::SigQuit,
    Signal::SigIll,
    Signal::SigTrap,
    Signal::SigAbrt,
    Signal::SigBus,
    Signal::SigFpe,
    Signal::SigKill,
    Signal::SigUsr1,
    Signal::SigSegv,
    Signal::SigUsr2,
    Signal::SigPipe,
    Signal::SigAlrm,
    Signal::SigTerm,
    Signal::SigChld,
    Signal::SigCont,
    Signal::SigStop,
    Signal::SigTstp,
    Signal::SigTtin,
    Signal::SigTtou,
    Signal::SigUrg,
    Signal::SigXcpu,
    Signal::SigXfsz,
    Signal::SigVtalrm,
    Signal::SigProf,
    Signal::SigSys,
];

/// The two ends of the self-pipe that wakes the shell when a trapped signal
/// arrives, or -1 before it exists. Written once at startup and read from the
/// signal handler, hence atomics.
static SIGNAL_WRITE: AtomicI32 = AtomicI32::new(-1);
static SIGNAL_READ: AtomicI32 = AtomicI32::new(-1);

/// One flag per signal number, set by the handler and cleared when the shell
/// takes the signal. The pipe only wakes the shell up: it has a fixed
/// capacity, while these flags cannot overflow, so which signals arrived is
/// never lost however many arrive before the shell looks. Several deliveries
/// of one signal coalesce into one, as POSIX allows (XCU 2.11).
static PENDING: [AtomicBool; 128] = [const { AtomicBool::new(false) }; 128];

/// Set by the handler after its flag and wake-up byte, so the shell can
/// check for signals after every command without a system call. The handler
/// interrupts the shell's only thread and runs to completion, so whenever
/// this is clear no flag is set, and a byte in the pipe is not this
/// process's (see `drain_signal_pipe`).
static ANY_PENDING: AtomicBool = AtomicBool::new(false);

/// The calling thread's `errno`.
fn errno_location() -> *mut libc::c_int {
    #[cfg(any(target_os = "linux", target_os = "android", target_os = "emscripten"))]
    unsafe {
        libc::__errno_location()
    }
    #[cfg(any(
        target_vendor = "apple",
        target_os = "freebsd",
        target_os = "dragonfly"
    ))]
    unsafe {
        libc::__error()
    }
    #[cfg(any(target_os = "openbsd", target_os = "netbsd"))]
    unsafe {
        libc::__errno()
    }
}

/// The handler of every trapped signal. It runs between any two instructions
/// of the shell, so it does only async-signal-safe things: it never blocks
/// (the pipe is non-blocking, and a full pipe already holds a wake-up), never
/// allocates or panics, and it leaves `errno` as it found it, or a signal
/// arriving between a failed call and the read of its `errno` would change
/// the error the shell reports.
extern "C" fn record_signal(signal: libc::c_int) {
    let errno = errno_location();
    let saved_errno = unsafe { *errno };
    if let Some(flag) = usize::try_from(signal).ok().and_then(|n| PENDING.get(n)) {
        flag.store(true, Ordering::SeqCst);
    }
    let fd = SIGNAL_WRITE.load(Ordering::SeqCst);
    if fd >= 0 {
        let byte = 0u8;
        // Nothing to do about a failure: EAGAIN is a full pipe, which wakes
        // the shell anyway, and the flag above records the signal.
        unsafe { libc::write(fd, (&byte as *const u8).cast(), 1) };
    }
    ANY_PENDING.store(true, Ordering::SeqCst);
    unsafe { *errno = saved_errno };
}

/// Prepares one end of the signal pipe. Neither end may block: the handler
/// must not wait for room in a full pipe, and the shell must not wait for a
/// byte when none is pending. And the pipe belongs to the shell alone: the
/// utilities it execs must not inherit either end.
fn configure_signal_pipe_end(fd: RawFd) -> OsResult<()> {
    let flags = unsafe { libc::fcntl(fd, libc::F_GETFL) };
    if flags < 0
        || unsafe { libc::fcntl(fd, libc::F_SETFL, flags | libc::O_NONBLOCK) } < 0
        || unsafe { libc::fcntl(fd, libc::F_SETFD, libc::FD_CLOEXEC) } < 0
    {
        return Err(OsError::from_current_errno("fcntl"));
    }
    Ok(())
}

/// # Safety
/// cannot be called by multiple threads
pub unsafe fn setup_signal_handling() {
    // The ends are moved above 9, out of the way of the descriptors
    // scripts redirect (see `dup_private`).
    let created = pipe().and_then(|(read_pipe, write_pipe)| {
        let read_pipe = unsafe { OwnedFd::from_raw_fd(dup_private(read_pipe.as_raw_fd())?) };
        let write_pipe = unsafe { OwnedFd::from_raw_fd(dup_private(write_pipe.as_raw_fd())?) };
        configure_signal_pipe_end(read_pipe.as_raw_fd())?;
        configure_signal_pipe_end(write_pipe.as_raw_fd())?;
        Ok((read_pipe, write_pipe))
    });
    // Without the pipe (no descriptors to spare, say) the shell still runs,
    // traps included, as dash and bash do: the handler's flags record the
    // signals, and a blocking call the shell is in returns EINTR. All that
    // is lost is the wake-up of a wait for terminal input by a signal that
    // arrives just before the wait begins.
    if let Ok((read_pipe, write_pipe)) = created {
        SIGNAL_WRITE.store(write_pipe.into_raw_fd(), Ordering::SeqCst);
        SIGNAL_READ.store(read_pipe.into_raw_fd(), Ordering::SeqCst);
    }
}

/// Whether `fd` is one end of the signal pipe.
pub fn is_signal_pipe_fd(fd: RawFd) -> bool {
    fd >= 0
        && (SIGNAL_READ.load(Ordering::SeqCst) == fd || SIGNAL_WRITE.load(Ordering::SeqCst) == fd)
}

/// Moves the end of the signal pipe numbered `fd` to another descriptor,
/// so that a redirection can have `fd`. Every signal is blocked meanwhile:
/// a handler that read the old number could otherwise write its byte into
/// whatever the redirection puts there.
pub fn move_signal_pipe_fd(fd: RawFd) -> OsResult<()> {
    // SAFETY: both sets are initialised (sigfillset, sigprocmask) before
    // they are read.
    unsafe {
        let mut all: libc::sigset_t = std::mem::zeroed();
        let mut previous: libc::sigset_t = std::mem::zeroed();
        libc::sigfillset(&mut all);
        libc::sigprocmask(libc::SIG_SETMASK, &all, &mut previous);
        let result = dup_private(fd).map(|copy| {
            for end in [&SIGNAL_READ, &SIGNAL_WRITE] {
                let _ = end.compare_exchange(fd, copy, Ordering::SeqCst, Ordering::SeqCst);
            }
            let _ = close(fd);
        });
        libc::sigprocmask(libc::SIG_SETMASK, &previous, std::ptr::null_mut());
        result
    }
}

/// The read end of the self-pipe the signal handlers write to. Pollable, so a
/// wait for input can be woken by a signal instead of being timed out. -1,
/// which `poll` ignores, when there is no pipe.
pub fn signal_read_fd() -> RawFd {
    SIGNAL_READ.load(Ordering::SeqCst)
}

/// Empties the signal pipe. The bytes carry no information, so a failure to
/// read them loses nothing but a wake-up: it is reported and otherwise
/// ignored. A subshell shares the pipe with its parent, so a byte may also
/// come from the other process, whose flags are its own; anything that
/// polls the pipe must empty it once woken, or the stray byte would keep
/// waking it.
pub fn drain_signal_pipe() {
    let fd = signal_read_fd();
    if fd < 0 {
        return;
    }
    let mut buf = [0u8; 256];
    loop {
        match read(fd, &mut buf) {
            Ok(0) => break,
            Ok(_) => {}
            Err(err) if err.errno == Errno::EINTR => {}
            Err(err) if err.errno == Errno::EAGAIN => break,
            Err(err) => {
                stderr_println!("{err}");
                break;
            }
        }
    }
}

fn get_pending_signal() -> Option<Signal> {
    if !ANY_PENDING.swap(false, Ordering::SeqCst) {
        return None;
    }
    // Empty the pipe before looking at the flags: a signal that arrives in
    // between leaves its flag, its byte and ANY_PENDING, so the byte is
    // emptied next time and nothing is missed.
    drain_signal_pipe();
    let signal = SIGNALS
        .iter()
        .copied()
        .find(|&signal| PENDING[i32::from(signal) as usize].swap(false, Ordering::SeqCst));
    if signal.is_some() {
        // Others may be pending too; the caller asks again until None.
        ANY_PENDING.store(true, Ordering::SeqCst);
    }
    signal
}

unsafe fn handle_signal(signal: Signal, handler: libc::sighandler_t) {
    // sigaction contains different field on different systems, we can't
    // initialize it directly
    let mut action = std::mem::zeroed::<libc::sigaction>();
    // A one-argument handler (or SIG_DFL/SIG_IGN), so not SA_SIGINFO, which
    // would call it with three.
    action.sa_sigaction = handler;
    // never fails
    libc::sigemptyset(&mut action.sa_mask);
    action.sa_flags = 0;

    let result = libc::sigaction(signal.into(), &action, std::ptr::null_mut());
    if result < 0 {
        panic!("failed to set signal handler")
    }
}

pub unsafe fn handle_signal_ignore(signal: Signal) {
    handle_signal(signal, libc::SIG_IGN);
}

pub unsafe fn handle_signal_default(signal: Signal) {
    handle_signal(signal, libc::SIG_DFL);
}

/// Whether `signal` is ignored right now, as when the shell starts.
fn is_ignored(signal: Signal) -> bool {
    let mut current = unsafe { std::mem::zeroed::<libc::sigaction>() };
    let result = unsafe { libc::sigaction(signal.into(), std::ptr::null(), &mut current) };
    result == 0 && current.sa_sigaction == libc::SIG_IGN
}

/// The shell waits for its children, which an ignored SIGCHLD would have the
/// system reap before `waitpid` sees them; so a SIGCHLD ignored on entry is
/// reset (as in dash), not kept like the other signals.
pub fn reset_inherited_sigchld() {
    if is_ignored(Signal::SigChld) {
        unsafe { handle_signal_default(Signal::SigChld) };
    }
}

pub unsafe fn handle_signal_write_to_signal_buffer(signal: Signal) {
    handle_signal(
        signal,
        record_signal as *const extern "C" fn(libc::c_int) as libc::sighandler_t,
    )
}

/// What the shell does about a signal that has arrived.
pub enum PendingSignal {
    /// Runs its trap action (or nothing, for an ignored or default one).
    Action(TrapAction),
    /// Abandons the command being run and returns to the prompt.
    Interrupt,
}

#[derive(Clone)]
pub struct SignalManager {
    actions: [TrapAction; Signal::Count as usize],
    /// XCU 2.11 and `trap`: a signal ignored when the shell started stays
    /// ignored in every command the shell runs, and a non-interactive shell
    /// cannot trap or reset it (silently, as in bash and dash). Its default
    /// action, for this shell and its children, is to stay ignored.
    ignored_on_entry: [bool; Signal::Count as usize],
    /// The shell is interactive and this is not a subshell of it: SIGINT
    /// is always caught, and untrapped it abandons the command being run.
    is_interactive: bool,
    sigint_count: u32,
    /// Trap actions are running. Signals that arrive meanwhile wait until
    /// they finish rather than nest inside them: signals are checked after
    /// every command, so a stream of them would otherwise nest without
    /// bound, and the stack would overflow.
    running_traps: bool,
    /// Job control is on in this shell (not a subshell of it), which then
    /// ignores the terminal's stop signals, SIGTSTP, SIGTTIN and SIGTTOU,
    /// unless they are trapped (XCU 2.11, as in dash). The commands it runs
    /// get their default action back.
    job_control: bool,
}

/// The stop signals a job-control shell ignores for itself.
const JOB_CONTROL_STOP_SIGNALS: [Signal; 3] = [Signal::SigTstp, Signal::SigTtin, Signal::SigTtou];

impl SignalManager {
    /// Records the dispositions the shell inherited, so it must be created
    /// before the shell changes any of them.
    pub fn new(is_interactive: bool) -> Self {
        let mut ignored_on_entry = [false; Signal::Count as usize];
        let mut actions = [const { TrapAction::Default }; Signal::Count as usize];
        for &signal in SIGNALS {
            if signal != Signal::SigKill && signal != Signal::SigStop && is_ignored(signal) {
                ignored_on_entry[signal as usize] = true;
                // `trap` lists it as ignored (XCU `trap`).
                actions[signal as usize] = TrapAction::Ignore;
            }
        }
        Self {
            actions,
            ignored_on_entry,
            is_interactive,
            sigint_count: 0,
            running_traps: false,
            job_control: false,
        }
    }

    /// Marks trap actions as running; false if they already are, when the
    /// caller must leave pending signals to the run in progress.
    pub fn begin_running_traps(&mut self) -> bool {
        !std::mem::replace(&mut self.running_traps, true)
    }

    pub fn end_running_traps(&mut self) {
        self.running_traps = false;
    }

    /// Installs the default action of `signal`: SIG_DFL, or SIG_IGN for a
    /// signal ignored on entry.
    fn install_default(&self, signal: Signal) {
        if self.ignored_on_entry[signal as usize]
            || (self.job_control && JOB_CONTROL_STOP_SIGNALS.contains(&signal))
        {
            unsafe { handle_signal_ignore(signal) };
        } else {
            unsafe { handle_signal_default(signal) };
        }
    }

    pub fn reset(&mut self) {
        // A subshell started from a trap action runs its own traps.
        self.running_traps = false;
        // A subshell of an interactive shell is not interactive: an
        // untrapped SIGINT takes its default action there (as in dash).
        self.is_interactive = false;
        // Nor does it run job control: the terminal's stop signals stop it
        // and its commands like any other process.
        self.job_control = false;
        for signal in SIGNALS {
            let signal = *signal;
            if signal == Signal::SigKill || signal == Signal::SigStop {
                continue;
            }
            let action = &mut self.actions[signal as usize];
            match action {
                // we also reset default actions because ignore could have been
                // set at startup for an interactive shell, but its not registered
                // as a trap action
                TrapAction::Commands(_) | TrapAction::Default => {
                    *action = TrapAction::Default;
                    self.install_default(signal);
                }
                // An interactive shell catches an ignored SIGINT, and a
                // caught signal is reset to its default by `exec`.
                TrapAction::Ignore => unsafe { handle_signal_ignore(signal) },
            }
        }
    }

    /// Re-installs `saved`'s dispositions, after a `reset()` that turned out
    /// not to be followed by replacing the process (a failed `exec`).
    pub fn restore(&mut self, saved: Self) {
        self.running_traps = saved.running_traps;
        self.is_interactive = saved.is_interactive;
        self.job_control = saved.job_control;
        for signal in SIGNALS {
            let signal = *signal;
            if signal == Signal::SigKill || signal == Signal::SigStop {
                continue;
            }
            self.set_action(signal, saved.actions[signal as usize].clone());
        }
    }

    pub fn is_ignored_on_entry(&self, signal: Signal) -> bool {
        self.ignored_on_entry[signal as usize]
    }

    /// Turns job control on or off for the dispositions of the terminal's
    /// stop signals: ignored while it is on, unless trapped.
    pub fn set_job_control(&mut self, on: bool) {
        self.job_control = on;
        for signal in JOB_CONTROL_STOP_SIGNALS {
            match self.actions[signal as usize] {
                TrapAction::Default => self.install_default(signal),
                TrapAction::Ignore => unsafe { handle_signal_ignore(signal) },
                TrapAction::Commands(_) => {}
            }
        }
    }

    pub fn set_action(&mut self, signal: Signal, action: TrapAction) {
        assert!(signal != Signal::SigKill && signal != Signal::SigStop);

        if self.ignored_on_entry[signal as usize] && !self.is_interactive {
            return;
        }
        if self.is_interactive
            && signal == Signal::SigInt
            && (action == TrapAction::Ignore || action == TrapAction::Default)
        {
            // in interactive mode we always want catch sigint
            unsafe { handle_signal_write_to_signal_buffer(Signal::SigInt) };
            self.actions[signal as usize] = action;
            return;
        }
        match action {
            TrapAction::Default => {
                self.install_default(signal);
            }
            TrapAction::Ignore => {
                unsafe { handle_signal_ignore(signal) };
            }
            TrapAction::Commands(_) => {
                unsafe { handle_signal_write_to_signal_buffer(signal) };
            }
        }
        self.actions[signal as usize] = action;
    }

    /// Whether any signal has a trap action (commands) set. Ignored signals
    /// do not count: an ignored disposition survives `exec`.
    pub fn catches_any(&self) -> bool {
        self.actions
            .iter()
            .any(|action| matches!(action, TrapAction::Commands(_)))
    }

    /// Takes a signal that has arrived, with what the shell is to do about
    /// it.
    pub fn take_pending_signal(&mut self) -> Option<PendingSignal> {
        let signal = get_pending_signal()?;
        if signal == Signal::SigInt {
            self.sigint_count += 1;
            if self.sigint_interrupts() {
                return Some(PendingSignal::Interrupt);
            }
        }
        Some(PendingSignal::Action(self.actions[signal as usize].clone()))
    }

    /// Whether SIGINT abandons the command being run: in an interactive
    /// shell (not a subshell of one) that has no trap for it, as in dash.
    fn sigint_interrupts(&self) -> bool {
        self.is_interactive && self.actions[Signal::SigInt as usize] == TrapAction::Default
    }

    /// A signal that has arrived and that ends a wait of the `wait` utility,
    /// left pending for `take_pending_signal` to take: one with a trap
    /// action, or a SIGINT that abandons the command being run.
    pub fn pending_signal_ending_wait(&self) -> Option<Signal> {
        SIGNALS.iter().copied().find(|&signal| {
            (matches!(self.actions[signal as usize], TrapAction::Commands(_))
                || (signal == Signal::SigInt && self.sigint_interrupts()))
                && PENDING[i32::from(signal) as usize].load(Ordering::SeqCst)
        })
    }

    /// Whether a SIGINT that abandons the command being run has arrived.
    pub fn interrupt_pending(&self) -> bool {
        self.sigint_interrupts() && PENDING[libc::SIGINT as usize].load(Ordering::SeqCst)
    }

    /// Whether SIGINT, delivered now, would be the shell's to act on: true
    /// for the interactive shell, which catches it whatever its trap.
    pub fn catches_sigint_interactively(&self) -> bool {
        self.is_interactive
    }

    pub fn iter(&self) -> impl Iterator<Item = (Signal, &TrapAction)> {
        SIGNALS
            .iter()
            .map(move |&signal| (signal, &self.actions[signal as usize]))
    }

    pub fn reset_sigint_count(&mut self) {
        self.sigint_count = 0;
    }

    pub fn get_sigint_count(&self) -> u32 {
        self.sigint_count
    }
}

/// A signal number reported by `wait`. A child may be killed by a signal the
/// shell has no name for (SIGPWR, SIGSTKFLT, the realtime signals, …), so the
/// raw number is kept rather than forced into [`Signal`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TermSignal(pub libc::c_int);

impl TermSignal {
    pub fn exit_status(self) -> libc::c_int {
        128 + self.0
    }

    pub fn is(self, signal: Signal) -> bool {
        self.0 == i32::from(signal)
    }
}

impl Display for TermSignal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match Signal::try_from(self.0) {
            Ok(signal) => write!(f, "{signal}"),
            Err(_) => write!(f, "{}", self.0),
        }
    }
}

pub fn kill(pid: Pid, signal: Option<Signal>) -> OsResult<()> {
    let result = unsafe { libc::kill(pid, signal.map(|s| s.into()).unwrap_or(0)) };
    if result < 0 {
        return Err(OsError::from_current_errno("kill"));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn term_signal_exit_status_is_128_plus_the_signal_number() {
        assert_eq!(TermSignal(libc::SIGTERM).exit_status(), 128 + libc::SIGTERM);
        assert_eq!(TermSignal(libc::SIGINT).exit_status(), 130);
    }

    #[test]
    fn term_signal_names_unknown_signals_by_number() {
        // A child may be killed by a signal this shell has no name for (the
        // realtime signals, SIGPWR, …); that must not abort the shell.
        assert_eq!(TermSignal(libc::SIGTERM).to_string(), "TERM");
        assert_eq!(TermSignal(64).to_string(), "64");
        assert_eq!(TermSignal(64).exit_status(), 192);
    }
}
