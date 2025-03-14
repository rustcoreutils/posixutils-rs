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
    SigAbrt,
    SigAlrm,
    SigTerm,
    Count,
}

impl Display for TrapCondition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TrapCondition::Exit => write!(f, "EXIT"),
            TrapCondition::SigHup => write!(f, "HUP"),
            TrapCondition::SigInt => write!(f, "INT"),
            TrapCondition::SigQuit => write!(f, "QUIT"),
            TrapCondition::SigAbrt => write!(f, "ABRT"),
            TrapCondition::SigAlrm => write!(f, "ALRM"),
            TrapCondition::SigTerm => write!(f, "TERM"),
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
            "ABRT" | "6" => Ok(TrapCondition::SigAbrt),
            "ALRM" | "14" => Ok(TrapCondition::SigAlrm),
            "TERM" | "15" => Ok(TrapCondition::SigTerm),
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
            TrapCondition::SigAbrt => Signal::SIGABRT,
            TrapCondition::SigAlrm => Signal::SIGALRM,
            TrapCondition::SigTerm => Signal::SIGTERM,
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
    for (condition, action) in shell.trap_actions.iter().enumerate() {
        match action {
            TrapAction::Default if print_default => {
                opened_files.stdout().write_str(format!(
                    "trap -- - {}\n",
                    TrapCondition::try_from(condition as i32).unwrap()
                ));
            }
            TrapAction::Default => {}
            TrapAction::Ignore => {
                opened_files.stdout().write_str(format!(
                    "trap -- '' {}\n",
                    TrapCondition::try_from(condition as i32).unwrap()
                ));
            }
            TrapAction::Commands(cmd) => {
                opened_files.stdout().write_str(format!(
                    "trap -- '{cmd}' {}\n",
                    TrapCondition::try_from(condition as i32).unwrap()
                ));
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
