use crate::builtin::{BuiltinError, BuiltinResult, SpecialBuiltinUtility};
use crate::global_shell_signal_handler;
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;
use nix::sys::signal::{sigaction, SaFlags, SigAction, SigHandler, SigSet, Signal};
use std::fmt::{Display, Formatter};
use std::str::FromStr;

#[derive(Clone)]
pub enum TrapAction {
    Default,
    Ignore,
    Commands(String),
}

#[derive(Clone, Copy)]
pub enum TrapCondition {
    Exit,
    SigHup,
    SigInt,
    SigQuit,
    SigIll,
    SigTrap,
    SigAbrt,
    SigBus,
    SigFpe,
    SigUsr1,
    SigSegv,
    SigUsr2,
    SigPipe,
    SigAlrm,
    SigTerm,
    SigChld,
    SigCont,
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

impl Display for TrapCondition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TrapCondition::Exit => write!(f, "EXIT"),
            TrapCondition::SigHup => write!(f, "HUP"),
            TrapCondition::SigInt => write!(f, "INT"),
            TrapCondition::SigQuit => write!(f, "QUIT"),
            TrapCondition::SigIll => write!(f, "ILL"),
            TrapCondition::SigTrap => write!(f, "TRAP"),
            TrapCondition::SigAbrt => write!(f, "ABRT"),
            TrapCondition::SigBus => write!(f, "BUS"),
            TrapCondition::SigFpe => write!(f, "FPE"),
            TrapCondition::SigUsr1 => write!(f, "USR1"),
            TrapCondition::SigSegv => write!(f, "SEGV"),
            TrapCondition::SigUsr2 => write!(f, "USR2"),
            TrapCondition::SigPipe => write!(f, "PIPE"),
            TrapCondition::SigAlrm => write!(f, "ALRM"),
            TrapCondition::SigTerm => write!(f, "TERM"),
            TrapCondition::SigChld => write!(f, "CHLD"),
            TrapCondition::SigCont => write!(f, "CONT"),
            TrapCondition::SigTstp => write!(f, "TSTP"),
            TrapCondition::SigTtin => write!(f, "TTIN"),
            TrapCondition::SigTtou => write!(f, "TTOU"),
            TrapCondition::SigUrg => write!(f, "URG"),
            TrapCondition::SigXcpu => write!(f, "XCPU"),
            TrapCondition::SigXfsz => write!(f, "XFSZ"),
            TrapCondition::SigVtalrm => write!(f, "VTALRM"),
            TrapCondition::SigProf => write!(f, "PROF"),
            TrapCondition::SigSys => write!(f, "SYS"),
            TrapCondition::Count => unreachable!("invalid trap condition"),
        }
    }
}

impl FromStr for TrapCondition {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "EXIT" | "0" => Ok(TrapCondition::Exit),
            "HUP" | "1" => Ok(TrapCondition::SigHup),
            "INT" | "2" => Ok(TrapCondition::SigInt),
            "QUIT" | "3" => Ok(TrapCondition::SigQuit),
            "ILL" => Ok(TrapCondition::SigIll),
            "TRAP" => Ok(TrapCondition::SigTrap),
            "ABRT" | "6" => Ok(TrapCondition::SigAbrt),
            "BUS" => Ok(TrapCondition::SigBus),
            "FPE" => Ok(TrapCondition::SigFpe),
            "USR1" => Ok(TrapCondition::SigUsr1),
            "SEGV" => Ok(TrapCondition::SigSegv),
            "USR2" => Ok(TrapCondition::SigUsr2),
            "PIPE" => Ok(TrapCondition::SigPipe),
            "ALRM" | "14" => Ok(TrapCondition::SigAlrm),
            "TERM" | "15" => Ok(TrapCondition::SigTerm),
            "CHLD" => Ok(TrapCondition::SigChld),
            "CONT" => Ok(TrapCondition::SigCont),
            "TSTP" => Ok(TrapCondition::SigTstp),
            "TTIN" => Ok(TrapCondition::SigTtin),
            "TTOU" => Ok(TrapCondition::SigTtou),
            "URG" => Ok(TrapCondition::SigUrg),
            "XCPU" => Ok(TrapCondition::SigXcpu),
            "XFSZ" => Ok(TrapCondition::SigXfsz),
            "VTALRM" => Ok(TrapCondition::SigVtalrm),
            "PROF" => Ok(TrapCondition::SigProf),
            "SYS" => Ok(TrapCondition::SigSys),
            _ => Err(()),
        }
    }
}

impl TryFrom<i32> for TrapCondition {
    type Error = ();

    fn try_from(value: i32) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(TrapCondition::Exit),
            1 => Ok(TrapCondition::SigHup),
            2 => Ok(TrapCondition::SigInt),
            3 => Ok(TrapCondition::SigQuit),
            6 => Ok(TrapCondition::SigAbrt),
            14 => Ok(TrapCondition::SigAlrm),
            15 => Ok(TrapCondition::SigTerm),
            _ => Err(()),
        }
    }
}

impl From<TrapCondition> for Signal {
    fn from(value: TrapCondition) -> Self {
        match value {
            TrapCondition::SigHup => Signal::SIGHUP,
            TrapCondition::SigInt => Signal::SIGINT,
            TrapCondition::SigQuit => Signal::SIGQUIT,
            TrapCondition::SigIll => Signal::SIGILL,
            TrapCondition::SigTrap => Signal::SIGTRAP,
            TrapCondition::SigAbrt => Signal::SIGABRT,
            TrapCondition::SigBus => Signal::SIGBUS,
            TrapCondition::SigFpe => Signal::SIGFPE,
            TrapCondition::SigUsr1 => Signal::SIGUSR1,
            TrapCondition::SigSegv => Signal::SIGSEGV,
            TrapCondition::SigUsr2 => Signal::SIGUSR2,
            TrapCondition::SigPipe => Signal::SIGPIPE,
            TrapCondition::SigAlrm => Signal::SIGALRM,
            TrapCondition::SigTerm => Signal::SIGTERM,
            TrapCondition::SigChld => Signal::SIGCHLD,
            TrapCondition::SigCont => Signal::SIGCONT,
            TrapCondition::SigTstp => Signal::SIGTSTP,
            TrapCondition::SigTtin => Signal::SIGTTIN,
            TrapCondition::SigTtou => Signal::SIGTTOU,
            TrapCondition::SigUrg => Signal::SIGURG,
            TrapCondition::SigXcpu => Signal::SIGXCPU,
            TrapCondition::SigXfsz => Signal::SIGXFSZ,
            TrapCondition::SigVtalrm => Signal::SIGVTALRM,
            TrapCondition::SigProf => Signal::SIGPROF,
            TrapCondition::SigSys => Signal::SIGSYS,
            TrapCondition::Exit | TrapCondition::Count => unreachable!("invalid trap condition"),
        }
    }
}

fn set_action(
    shell: &mut Shell,
    condition: TrapCondition,
    action: TrapAction,
    handler: SigHandler,
) {
    match condition {
        TrapCondition::Exit => {
            shell.trap_actions[TrapCondition::Exit as usize] = action;
        }
        other => {
            shell.trap_actions[other as usize] = action;
            unsafe {
                sigaction(
                    other.into(),
                    &SigAction::new(handler, SaFlags::empty(), SigSet::empty()),
                )
                .unwrap();
            }
        }
    }
}

fn default_action(shell: &mut Shell, condition: TrapCondition) {
    set_action(shell, condition, TrapAction::Default, SigHandler::SigDfl)
}

fn ignore_action(shell: &mut Shell, condition: TrapCondition) {
    set_action(shell, condition, TrapAction::Ignore, SigHandler::SigIgn)
}

fn print_commands(shell: &mut Shell, opened_files: &mut OpenedFiles, print_default: bool) {
    const CONDITIONS: &[TrapCondition] = &[
        TrapCondition::Exit,
        TrapCondition::SigHup,
        TrapCondition::SigInt,
        TrapCondition::SigQuit,
        TrapCondition::SigIll,
        TrapCondition::SigTrap,
        TrapCondition::SigAbrt,
        TrapCondition::SigBus,
        TrapCondition::SigFpe,
        TrapCondition::SigUsr1,
        TrapCondition::SigSegv,
        TrapCondition::SigUsr2,
        TrapCondition::SigPipe,
        TrapCondition::SigAlrm,
        TrapCondition::SigTerm,
        TrapCondition::SigChld,
        TrapCondition::SigCont,
        TrapCondition::SigTstp,
        TrapCondition::SigTtin,
        TrapCondition::SigTtou,
        TrapCondition::SigUrg,
        TrapCondition::SigXcpu,
        TrapCondition::SigXfsz,
        TrapCondition::SigVtalrm,
        TrapCondition::SigProf,
        TrapCondition::SigSys,
    ];
    for (condition, action) in shell.trap_actions.iter().enumerate() {
        let condition = CONDITIONS[condition];
        match action {
            TrapAction::Default if print_default => {
                opened_files.write_out(format!("trap -- - {}\n", condition));
            }
            TrapAction::Default => {}
            TrapAction::Ignore => {
                opened_files.write_out(format!("trap -- '' {}\n", condition));
            }
            TrapAction::Commands(cmd) => {
                opened_files.write_out(format!("trap -- '{cmd}' {}\n", condition));
            }
        }
    }
}

enum TrapArg<'a> {
    Reset,
    Ignore,
    Command(&'a str),
}

pub struct Trap;

impl SpecialBuiltinUtility for Trap {
    fn exec(
        &self,
        args: &[String],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        if args.is_empty() {
            print_commands(shell, opened_files, false);
            return Ok(0);
        }

        if args[0] == "-p" {
            print_commands(shell, opened_files, true);
            return Ok(0);
        }

        let first_index = if args[0] == "--" {
            if args.len() == 1 {
                return Ok(0);
            } else {
                1
            }
        } else {
            0
        };

        let action = if &args[first_index] == "-" {
            TrapArg::Reset
        } else if let Ok(n) = args[first_index].parse::<i32>() {
            if let Ok(condition) = TrapCondition::try_from(n) {
                default_action(shell, condition);
                TrapArg::Reset
            } else {
                return Err(format!("trap: '{}' is not a valid signal", n).into());
            }
        } else {
            TrapArg::Command(&args[first_index])
        };

        for condition in &args[first_index + 1..] {
            let condition = TrapCondition::from_str(condition).map_err(|_| {
                BuiltinError::CustomError(format!("trap: '{}' is not a valid signal", condition))
            })?;
            match action {
                TrapArg::Reset => {
                    default_action(shell, condition);
                }
                TrapArg::Ignore => {
                    ignore_action(shell, condition);
                }
                TrapArg::Command(cmd) => {
                    set_action(
                        shell,
                        condition,
                        TrapAction::Commands(cmd.to_string()),
                        SigHandler::Handler(global_shell_signal_handler),
                    );
                }
            }
        }

        Ok(0)
    }
}
