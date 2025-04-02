//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::{BuiltinError, BuiltinResult, SpecialBuiltinUtility};
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;
use crate::signals::Signal;
use std::fmt::Display;
use std::str::FromStr;

#[derive(Clone, PartialEq, Eq)]
pub enum TrapAction {
    Default,
    Ignore,
    Commands(String),
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
    for (condition, action) in shell.signal_manager.iter() {
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
                shell
                    .signal_manager
                    .set_action(condition, TrapAction::Default);
                TrapArg::Reset
            } else {
                return Err(format!("trap: '{}' is not a valid signal", args[first_index]).into());
            }
        } else if args[first_index].is_empty() {
            TrapArg::Ignore
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
                    shell
                        .signal_manager
                        .set_action(condition, TrapAction::Default);
                }
                TrapArg::Ignore => {
                    shell
                        .signal_manager
                        .set_action(condition, TrapAction::Ignore);
                }
                TrapArg::Command(cmd) => {
                    shell
                        .signal_manager
                        .set_action(condition, TrapAction::Commands(cmd.to_string()));
                }
            }
        }

        Ok(0)
    }
}
