use nix::sys::signal::Signal as NixSignal;
use std::fmt::{Display, Formatter};
use std::str::FromStr;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Signal {
    Exit,
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
            Signal::Exit => write!(f, "EXIT"),
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
            "EXIT" | "exit" | "0" => Ok(Signal::Exit),
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

impl TryFrom<i32> for Signal {
    type Error = ();

    fn try_from(value: i32) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Signal::Exit),
            1 => Ok(Signal::SigHup),
            2 => Ok(Signal::SigInt),
            3 => Ok(Signal::SigQuit),
            6 => Ok(Signal::SigAbrt),
            14 => Ok(Signal::SigAlrm),
            15 => Ok(Signal::SigTerm),
            _ => Err(()),
        }
    }
}

impl Signal {
    pub fn to_nix_signal(self) -> Option<NixSignal> {
        match self {
            Signal::Exit => None,
            Signal::SigHup => Some(NixSignal::SIGHUP),
            Signal::SigInt => Some(NixSignal::SIGINT),
            Signal::SigQuit => Some(NixSignal::SIGQUIT),
            Signal::SigIll => Some(NixSignal::SIGILL),
            Signal::SigTrap => Some(NixSignal::SIGTRAP),
            Signal::SigAbrt => Some(NixSignal::SIGABRT),
            Signal::SigBus => Some(NixSignal::SIGBUS),
            Signal::SigFpe => Some(NixSignal::SIGFPE),
            Signal::SigKill => Some(NixSignal::SIGKILL),
            Signal::SigUsr1 => Some(NixSignal::SIGUSR1),
            Signal::SigSegv => Some(NixSignal::SIGSEGV),
            Signal::SigUsr2 => Some(NixSignal::SIGUSR2),
            Signal::SigPipe => Some(NixSignal::SIGPIPE),
            Signal::SigAlrm => Some(NixSignal::SIGALRM),
            Signal::SigTerm => Some(NixSignal::SIGTERM),
            Signal::SigChld => Some(NixSignal::SIGCHLD),
            Signal::SigCont => Some(NixSignal::SIGCONT),
            Signal::SigStop => Some(NixSignal::SIGSTOP),
            Signal::SigTstp => Some(NixSignal::SIGTSTP),
            Signal::SigTtin => Some(NixSignal::SIGTTIN),
            Signal::SigTtou => Some(NixSignal::SIGTTOU),
            Signal::SigUrg => Some(NixSignal::SIGURG),
            Signal::SigXcpu => Some(NixSignal::SIGXCPU),
            Signal::SigXfsz => Some(NixSignal::SIGXFSZ),
            Signal::SigVtalrm => Some(NixSignal::SIGVTALRM),
            Signal::SigProf => Some(NixSignal::SIGPROF),
            Signal::SigSys => Some(NixSignal::SIGSYS),
            Signal::Count => unreachable!("invalid trap condition"),
        }
    }
}

pub const SIGNALS: &[Signal] = &[
    Signal::Exit,
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
