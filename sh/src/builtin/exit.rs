//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::{skip_option_terminator, BuiltinResult, SpecialBuiltinUtility};
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;

pub struct Exit;

impl SpecialBuiltinUtility for Exit {
    fn exec(&self, args: &[String], shell: &mut Shell, _: &mut OpenedFiles) -> BuiltinResult {
        let args = skip_option_terminator(args);
        if args.len() > 1 {
            return Err("exit: too many arguments".into());
        }

        if let Some(arg) = args.first() {
            if let Ok(n) = arg.parse::<i32>() {
                shell.exit(n);
            } else {
                Err(format!("exit: '{arg}' is not a valid number").into())
            }
        } else {
            shell.exit(shell.last_pipeline_exit_status)
        }
    }
}
