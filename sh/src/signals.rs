use crate::builtin::trap::TrapAction;
use crate::shell::Shell;
use nix::errno::Errno;
use nix::libc;
use nix::sys::signal::{sigaction, SaFlags, SigAction, SigHandler, SigSet, Signal as NixSignal};
use nix::unistd::{read, write};
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::os::fd::{AsRawFd, BorrowedFd, IntoRawFd, RawFd};
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

impl From<Signal> for NixSignal {
    fn from(value: Signal) -> Self {
        match value {
            Signal::SigHup => NixSignal::SIGHUP,
            Signal::SigInt => NixSignal::SIGINT,
            Signal::SigQuit => NixSignal::SIGQUIT,
            Signal::SigIll => NixSignal::SIGILL,
            Signal::SigTrap => NixSignal::SIGTRAP,
            Signal::SigAbrt => NixSignal::SIGABRT,
            Signal::SigBus => NixSignal::SIGBUS,
            Signal::SigFpe => NixSignal::SIGFPE,
            Signal::SigKill => NixSignal::SIGKILL,
            Signal::SigUsr1 => NixSignal::SIGUSR1,
            Signal::SigSegv => NixSignal::SIGSEGV,
            Signal::SigUsr2 => NixSignal::SIGUSR2,
            Signal::SigPipe => NixSignal::SIGPIPE,
            Signal::SigAlrm => NixSignal::SIGALRM,
            Signal::SigTerm => NixSignal::SIGTERM,
            Signal::SigChld => NixSignal::SIGCHLD,
            Signal::SigCont => NixSignal::SIGCONT,
            Signal::SigStop => NixSignal::SIGSTOP,
            Signal::SigTstp => NixSignal::SIGTSTP,
            Signal::SigTtin => NixSignal::SIGTTIN,
            Signal::SigTtou => NixSignal::SIGTTOU,
            Signal::SigUrg => NixSignal::SIGURG,
            Signal::SigXcpu => NixSignal::SIGXCPU,
            Signal::SigXfsz => NixSignal::SIGXFSZ,
            Signal::SigVtalrm => NixSignal::SIGVTALRM,
            Signal::SigProf => NixSignal::SIGPROF,
            Signal::SigSys => NixSignal::SIGSYS,
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

impl From<NixSignal> for Signal {
    fn from(value: NixSignal) -> Self {
        match value {
            NixSignal::SIGHUP => Signal::SigHup,
            NixSignal::SIGINT => Signal::SigInt,
            NixSignal::SIGQUIT => Signal::SigQuit,
            NixSignal::SIGILL => Signal::SigIll,
            NixSignal::SIGTRAP => Signal::SigTrap,
            NixSignal::SIGABRT => Signal::SigAbrt,
            NixSignal::SIGBUS => Signal::SigBus,
            NixSignal::SIGFPE => Signal::SigFpe,
            NixSignal::SIGKILL => Signal::SigKill,
            NixSignal::SIGUSR1 => Signal::SigUsr1,
            NixSignal::SIGSEGV => Signal::SigSegv,
            NixSignal::SIGUSR2 => Signal::SigUsr2,
            NixSignal::SIGPIPE => Signal::SigPipe,
            NixSignal::SIGALRM => Signal::SigAlrm,
            NixSignal::SIGTERM => Signal::SigTerm,
            NixSignal::SIGCHLD => Signal::SigChld,
            NixSignal::SIGCONT => Signal::SigCont,
            NixSignal::SIGSTOP => Signal::SigStop,
            NixSignal::SIGTSTP => Signal::SigTstp,
            NixSignal::SIGTTIN => Signal::SigTtin,
            NixSignal::SIGTTOU => Signal::SigTtou,
            NixSignal::SIGURG => Signal::SigUrg,
            NixSignal::SIGXCPU => Signal::SigXcpu,
            NixSignal::SIGXFSZ => Signal::SigXfsz,
            NixSignal::SIGVTALRM => Signal::SigVtalrm,
            NixSignal::SIGPROF => Signal::SigProf,
            NixSignal::SIGSYS => Signal::SigSys,
            _ => unreachable!("invalid signal"),
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

extern "C" fn handle_signals(signal: libc::c_int) {
    // SIGNAL_WRITE is never modified after the initial
    // setup, and is a valid file descriptor, so this is safe
    let fd = unsafe { BorrowedFd::borrow_raw(SIGNAL_WRITE.unwrap()) };
    write(fd, &signal.to_ne_bytes()).expect("failed to write to signal buffer");
}

/// # Safety
/// cannot be called by multiple threads
pub unsafe fn setup_signal_handling() {
    let (read_pipe, write_pipe) = nix::unistd::pipe().expect("could not create signal buffer pipe");
    nix::fcntl::fcntl(
        read_pipe.as_raw_fd(),
        nix::fcntl::FcntlArg::F_SETFL(nix::fcntl::OFlag::O_NONBLOCK),
    )
    .expect("signal buffer pipe could not be set as non-blocking");
    SIGNAL_WRITE = Some(write_pipe.into_raw_fd());
    SIGNAL_READ = Some(read_pipe.into_raw_fd());
}

fn get_pending_signal() -> Option<Signal> {
    // SIGNAL_READ is never modified after the initial
    // setup, so this is safe
    let fd = unsafe { SIGNAL_READ.unwrap() };
    let mut buf = [0u8; size_of::<libc::c_int>()];
    match read(fd, &mut buf) {
        Err(err) => {
            if err == Errno::EAGAIN {
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

#[derive(Clone)]
pub struct SignalManager {
    actions: [TrapAction; Signal::Count as usize],
}

impl SignalManager {
    pub fn reset(&mut self) {
        for signal in SIGNALS {
            let signal = *signal;
            if signal == Signal::SigKill || signal == Signal::SigStop {
                continue;
            }
            let action = &mut self.actions[signal as usize];
            match action {
                TrapAction::Commands(_) | TrapAction::Ignore => unsafe {
                    sigaction(
                        signal.into(),
                        &SigAction::new(SigHandler::SigDfl, SaFlags::empty(), SigSet::empty()),
                    )
                    .unwrap();
                    *action = TrapAction::Default;
                },
                TrapAction::Default => {
                    // already default, nothing to do
                }
            }
        }
    }

    pub fn set_action(&mut self, signal: Signal, action: TrapAction) {
        assert!(signal != Signal::SigKill && signal != Signal::SigStop);
        match action {
            TrapAction::Default => {
                unsafe {
                    sigaction(
                        signal.into(),
                        &SigAction::new(SigHandler::SigDfl, SaFlags::empty(), SigSet::empty()),
                    )
                    .unwrap()
                };
            }
            TrapAction::Ignore => {
                unsafe {
                    sigaction(
                        signal.into(),
                        &SigAction::new(SigHandler::SigIgn, SaFlags::empty(), SigSet::empty()),
                    )
                    .unwrap()
                };
            }
            TrapAction::Commands(_) => {
                unsafe {
                    sigaction(
                        signal.into(),
                        &SigAction::new(
                            SigHandler::Handler(handle_signals),
                            SaFlags::empty(),
                            SigSet::empty(),
                        ),
                    )
                    .unwrap()
                };
            }
        }
        self.actions[signal as usize] = action;
    }

    pub fn get_pending_action(&self) -> Option<&TrapAction> {
        if let Some(signal) = get_pending_signal() {
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
}

impl Default for SignalManager {
    fn default() -> Self {
        Self {
            actions: [const { TrapAction::Default }; Signal::Count as usize],
        }
    }
}
