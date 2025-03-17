use crate::builtin::{BuiltinError, BuiltinResult, SpecialBuiltinUtility};
use crate::global_shell_signal_handler;
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;
use crate::signals::{Signal, SIGNALS};
use nix::sys::signal::{sigaction, SaFlags, SigAction, SigHandler, SigSet};
use std::str::FromStr;

#[derive(Clone)]
pub enum TrapAction {
    Default,
    Ignore,
    Commands(String),
}

fn set_action(shell: &mut Shell, condition: Signal, action: TrapAction, handler: SigHandler) {
    match condition {
        Signal::Exit => {
            shell.trap_actions[Signal::Exit as usize] = action;
        }
        other => {
            shell.trap_actions[other as usize] = action;
            // we know signal is neither Exit, Kill, or Stop, so the unwraps are safe
            unsafe {
                sigaction(
                    other.to_nix_signal().unwrap(),
                    &SigAction::new(handler, SaFlags::empty(), SigSet::empty()),
                )
                .unwrap();
            }
        }
    }
}

fn default_action(shell: &mut Shell, condition: Signal) {
    set_action(shell, condition, TrapAction::Default, SigHandler::SigDfl)
}

fn ignore_action(shell: &mut Shell, condition: Signal) {
    set_action(shell, condition, TrapAction::Ignore, SigHandler::SigIgn)
}

fn print_commands(shell: &mut Shell, opened_files: &mut OpenedFiles, print_default: bool) {
    for (condition, action) in shell.trap_actions.iter().enumerate() {
        let condition = SIGNALS[condition];
        if condition == Signal::SigKill || condition == Signal::SigStop {
            // since we decided that calling trap with KILL or STOP is an error, we list them
            continue;
        }
        match action {
            TrapAction::Default if print_default => {
                opened_files.write_out(format!("trap -- - {}\n", condition));
            }
            TrapAction::Default => {
                // ignore default actions if -p is not specified
            }
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
            if let Ok(condition) = Signal::try_from(n) {
                default_action(shell, condition);
                TrapArg::Reset
            } else {
                return Err(format!("trap: '{}' is not a valid signal", n).into());
            }
        } else {
            TrapArg::Command(&args[first_index])
        };

        for condition in &args[first_index + 1..] {
            let condition = Signal::from_str(condition).map_err(|_| {
                BuiltinError::CustomError(format!("trap: '{}' is not a valid signal", condition))
            })?;
            if condition == Signal::SigKill || condition == Signal::SigStop {
                // the standard says it is unspecified what happens if you try to trap KILL or STOP
                // we just return error
                return Err("trap: cannot trap SIGKILL or SIGSTOP".into());
            }
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
