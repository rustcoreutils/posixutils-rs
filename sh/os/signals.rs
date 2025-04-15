//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::trap::TrapAction;
use crate::os::errno::{get_current_errno_value, Errno};
use crate::os::{pipe, read, write, OsError, OsResult, Pid};
use std::fmt::{Display, Formatter};
use std::os::fd::{AsRawFd, IntoRawFd, RawFd};
use std::str::FromStr;

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
        match s {
            "HUP" | "hup" | "1" => Ok(Signal::SigHup),
            "INT" | "int" | "2" => Ok(Signal::SigInt),
            "QUIT" | "quit" | "3" => Ok(Signal::SigQuit),
            "ILL" | "ill" => Ok(Signal::SigIll),
            "TRAP" | "trap" => Ok(Signal::SigTrap),
            "ABRT" | "abrt" | "6" => Ok(Signal::SigAbrt),
            "BUS" | "bus" => Ok(Signal::SigBus),
            "FPE" | "fpe" => Ok(Signal::SigFpe),
            "USR1" | "usr1" => Ok(Signal::SigUsr1),
            "SEGV" | "segv" => Ok(Signal::SigSegv),
            "USR2" | "usr2" => Ok(Signal::SigUsr2),
            "PIPE" | "pipe" => Ok(Signal::SigPipe),
            "ALRM" | "alrm" | "14" => Ok(Signal::SigAlrm),
            "TERM" | "term" | "15" => Ok(Signal::SigTerm),
            "CHLD" | "chld" => Ok(Signal::SigChld),
            "CONT" | "cont" => Ok(Signal::SigCont),
            "STOP" | "stop" => Ok(Signal::SigStop),
            "TSTP" | "tstp" => Ok(Signal::SigTstp),
            "TTIN" | "ttin" => Ok(Signal::SigTtin),
            "TTOU" | "ttou" => Ok(Signal::SigTtou),
            "URG" | "urg" => Ok(Signal::SigUrg),
            "XCPU" | "xcpu" => Ok(Signal::SigXcpu),
            "XFSZ" | "xfsz" => Ok(Signal::SigXfsz),
            "VTALRM" | "vtalrm" => Ok(Signal::SigVtalrm),
            "PROF" | "prof" => Ok(Signal::SigProf),
            "SYS" | "sys" => Ok(Signal::SigSys),
            _ => Err(()),
        }
    }
}

impl Into<i32> for Signal {
    fn into(self) -> i32 {
        match self {
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

static mut SIGNAL_WRITE: Option<RawFd> = None;
static mut SIGNAL_READ: Option<RawFd> = None;

extern "C" fn write_signal_to_buffer(signal: libc::c_int) {
    // SIGNAL_WRITE is never modified after the initial
    // setup, and is a valid file descriptor, so this is safe
    let fd = unsafe { SIGNAL_WRITE.unwrap() };
    write(fd, &signal.to_ne_bytes()).expect("failed to write to signal buffer");
}

/// # Safety
/// cannot be called by multiple threads
pub unsafe fn setup_signal_handling() {
    unsafe {
        let (read_pipe, write_pipe) = pipe().expect("could not create signal buffer pipe");
        let result = libc::fcntl(read_pipe.as_raw_fd(), libc::F_SETFL, libc::O_NONBLOCK);
        if result < 0 {
            panic!(
                "failed initialize async buffer for signal handling ({})",
                get_current_errno_value()
            );
        }
        SIGNAL_WRITE = Some(write_pipe.into_raw_fd());
        SIGNAL_READ = Some(read_pipe.into_raw_fd());
    }
}

fn get_pending_signal() -> Option<Signal> {
    // SIGNAL_READ is never modified after the initial
    // setup, so this is safe
    let fd = unsafe { SIGNAL_READ.unwrap() };
    let mut buf = [0u8; size_of::<libc::c_int>()];
    match read(fd, &mut buf) {
        Err(err) => {
            if err.errno == Errno::EAGAIN {
                None
            } else {
                panic!("failed to write to signal pipe ({err})");
            }
        }
        Ok(size) => {
            if size == 0 {
                None
            } else {
                let signal = libc::c_int::from_ne_bytes(buf);
                Some(Signal::try_from(signal).unwrap())
            }
        }
    }
}

unsafe fn handle_signal(signal: Signal, handler: libc::sighandler_t) {
    unsafe {
        // sigaction contains different field on different systems, we can't
        // initialize it directly
        let mut action = std::mem::zeroed::<libc::sigaction>();
        action.sa_sigaction = handler;
        // never fails
        libc::sigemptyset(&mut action.sa_mask);
        action.sa_flags = libc::SA_SIGINFO;

        let result = libc::sigaction(signal.into(), &action, std::ptr::null_mut());
        if result < 0 {
            panic!("failed to set signal handler")
        }
    }
}

pub unsafe fn handle_signal_ignore(signal: Signal) {
    unsafe {
        handle_signal(signal, libc::SIG_IGN);
    }
}

pub unsafe fn handle_signal_default(signal: Signal) {
    unsafe {
        handle_signal(signal, libc::SIG_DFL);
    }
}

pub unsafe fn handle_signal_write_to_signal_buffer(signal: Signal) {
    unsafe {
        handle_signal(
            signal,
            write_signal_to_buffer as *const extern "C" fn(libc::c_int) as libc::sighandler_t,
        )
    }
}

#[derive(Clone)]
pub struct SignalManager {
    actions: [TrapAction; Signal::Count as usize],
    is_interactive: bool,
    sigint_count: u32,
}

impl SignalManager {
    pub fn new(is_interactive: bool) -> Self {
        Self {
            actions: [const { TrapAction::Default }; Signal::Count as usize],
            is_interactive,
            sigint_count: 0,
        }
    }

    pub fn reset(&mut self) {
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
                    unsafe { handle_signal_default(signal) };
                    *action = TrapAction::Default;
                }
                TrapAction::Ignore => {}
            }
        }
    }

    pub fn set_action(&mut self, signal: Signal, action: TrapAction) {
        assert!(signal != Signal::SigKill && signal != Signal::SigStop);

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
                unsafe { handle_signal_default(signal) };
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

    pub fn get_pending_action(&mut self) -> Option<&TrapAction> {
        if let Some(signal) = get_pending_signal() {
            if signal == Signal::SigInt {
                self.sigint_count += 1;
            }
            Some(&self.actions[signal as usize])
        } else {
            None
        }
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

pub fn signal_to_exit_status(signal: Signal) -> libc::c_int {
    128 + signal as libc::c_int
}

pub fn kill(pid: Pid, signal: Option<Signal>) -> OsResult<()> {
    let result = unsafe { libc::kill(pid, signal.map(|s| s.into()).unwrap_or(0)) };
    if result < 0 {
        return Err(OsError::from_current_errno("kill"));
    }
    Ok(())
}
