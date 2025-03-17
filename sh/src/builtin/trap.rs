use crate::builtin::{BuiltinError, BuiltinResult, SpecialBuiltinUtility};
use crate::global_shell_signal_handler;
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;
use crate::signals::{Signal, SIGNALS};
use nix::sys::signal::{sigaction, SaFlags, SigAction, SigHandler, SigSet};
use std::fmt::Display;
use std::str::FromStr;

#[derive(Clone)]
pub enum TrapAction {
    Default,
    Ignore,
    Commands(String),
}

fn set_action(shell: &mut Shell, condition: Signal, action: TrapAction, handler: SigHandler) {
    shell.trap_actions[condition as usize] = action;
    // we know signal is neither Kill nor Stop, so the unwraps are safe
    unsafe {
        sigaction(
            condition.into(),
            &SigAction::new(handler, SaFlags::empty(), SigSet::empty()),
        )
        .unwrap();
    }
}

fn default_action(shell: &mut Shell, condition: Signal) {
    set_action(shell, condition, TrapAction::Default, SigHandler::SigDfl)
}

fn ignore_action(shell: &mut Shell, condition: Signal) {
    set_action(shell, condition, TrapAction::Ignore, SigHandler::SigIgn)
}

fn print_action<C: Display>(
    condition: C,
    action: &TrapAction,
    opened_files: &mut OpenedFiles,
    print_default: bool,
) {
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

fn print_commands(shell: &mut Shell, opened_files: &mut OpenedFiles, print_default: bool) {
    print_action("EXIT", &shell.exit_action, opened_files, print_default);
    for (condition, action) in shell.trap_actions.iter().enumerate() {
        let condition = SIGNALS[condition];
        if condition == Signal::SigKill || condition == Signal::SigStop {
            // since we decided that calling trap with KILL or STOP is an error, we don't list them
            continue;
        }
        print_action(condition, action, opened_files, print_default);
    }
}

fn is_unsigned_int(s: &str) -> bool {
    s.chars().all(|c| c.is_ascii_digit())
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
        } else if is_unsigned_int(&args[first_index]) {
            if &args[first_index] == "0" {
                shell.exit_action = TrapAction::Default;
                TrapArg::Reset
            } else if let Ok(condition) = Signal::from_str(&args[first_index]) {
                default_action(shell, condition);
                TrapArg::Reset
            } else {
                return Err(format!("trap: '{}' is not a valid signal", args[first_index]).into());
            }
        } else {
            TrapArg::Command(&args[first_index])
        };

        for condition in &args[first_index + 1..] {
            if condition.eq_ignore_ascii_case("EXIT") || condition == "0" {
                shell.exit_action = match action {
                    TrapArg::Reset => TrapAction::Default,
                    TrapArg::Ignore => TrapAction::Ignore,
                    TrapArg::Command(cmd) => TrapAction::Commands(cmd.to_string()),
                };
                continue;
            }
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
