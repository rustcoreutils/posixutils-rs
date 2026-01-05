//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::{BuiltinResult, SpecialBuiltinUtility, skip_option_terminator};
use crate::shell::ControlFlowState;
use crate::shell::Shell;
use crate::shell::opened_files::OpenedFiles;

fn loop_control_flow(
    args: &[String],
    shell: &mut Shell,
    name: &str,
    state: fn(u32) -> ControlFlowState,
) -> BuiltinResult {
    if shell.loop_depth == 0 {
        return Err(format!(
            "{name}: '{name}' can only be used inside 'for', 'while' and 'until' loops"
        )
        .into());
    }

    let args = skip_option_terminator(args);

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
        loop_control_flow(args, shell, "break", ControlFlowState::Continue)
    }
}

pub struct Return;

impl SpecialBuiltinUtility for Return {
    fn exec(&self, args: &[String], shell: &mut Shell, _: &mut OpenedFiles) -> BuiltinResult {
        if shell.function_call_depth == 0 && shell.dot_script_depth == 0 {
            return Err("return: 'return' can only be used inside function or dot script".into());
        }
        if args.len() > 1 {
            return Err("return: too many arguments".into());
        }
        let n = if let Some(n) = args.first() {
            match n.parse::<i32>() {
                Ok(n) => n,
                Err(_) => {
                    return Err("return: expected numeric argument".into());
                }
            }
        } else {
            0
        };
        shell.control_flow_state = ControlFlowState::Return;
        Ok(n)
    }
}
