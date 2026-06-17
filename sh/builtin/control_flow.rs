//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::{skip_option_terminator, BuiltinResult, SpecialBuiltinUtility};
use crate::shell::opened_files::OpenedFiles;
use crate::shell::ControlFlowState;
use crate::shell::Shell;
use gettextrs::gettext;

fn loop_control_flow(
    args: &[String],
    shell: &mut Shell,
    name: &str,
    state: fn(u32) -> ControlFlowState,
) -> BuiltinResult {
    let args = skip_option_terminator(args);

    if shell.loop_depth == 0 {
        // POSIX leaves `break`/`continue` with no enclosing loop unspecified.
        // Like dash/bash, treat it as a no-op that succeeds rather than a fatal
        // error (which, for a special built-in, would abort a non-interactive
        // shell). The function/dot boundary already isolates loop nesting.
        return Ok(0);
    }

    if args.len() > 1 {
        return Err(format!("{name}: too many arguments").into());
    }
    let n = if let Some(n) = args.first() {
        match n.parse::<i32>() {
            Ok(n) => n,
            Err(_) => {
                return Err(format!("{name}: expected numeric argument").into());
            }
        }
    } else {
        1
    };
    if n < 1 {
        return Err(format!("{name}: argument has to be bigger than 0").into());
    }

    shell.control_flow_state = state(shell.loop_depth.min(n as u32));
    Ok(0)
}

pub struct Break;

impl SpecialBuiltinUtility for Break {
    fn exec(&self, args: &[String], shell: &mut Shell, _: &mut OpenedFiles) -> BuiltinResult {
        loop_control_flow(args, shell, "break", ControlFlowState::Break)
    }
}

pub struct Continue;

impl SpecialBuiltinUtility for Continue {
    fn exec(&self, args: &[String], shell: &mut Shell, _: &mut OpenedFiles) -> BuiltinResult {
        loop_control_flow(args, shell, "continue", ControlFlowState::Continue)
    }
}

pub struct Return;

impl SpecialBuiltinUtility for Return {
    fn exec(&self, args: &[String], shell: &mut Shell, _: &mut OpenedFiles) -> BuiltinResult {
        if shell.function_call_depth == 0 && shell.dot_script_depth == 0 {
            return Err(
                gettext("return: 'return' can only be used inside function or dot script").into(),
            );
        }
        if args.len() > 1 {
            return Err(gettext("return: too many arguments").into());
        }
        let n = if let Some(n) = args.first() {
            match n.parse::<i32>() {
                Ok(n) => n,
                Err(_) => {
                    return Err(gettext("return: expected numeric argument").into());
                }
            }
        } else {
            // POSIX: with no operand, return the current value of `$?`.
            shell.last_pipeline_exit_status
        };
        shell.control_flow_state = ControlFlowState::Return;
        Ok(n)
    }
}
