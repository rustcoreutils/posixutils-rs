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
use crate::shell::Shell;
use crate::shstr::ShString;
use gettextrs::gettext;

pub struct Exit;

impl SpecialBuiltinUtility for Exit {
    fn exec(&self, args: &[ShString], shell: &mut Shell, _: &mut OpenedFiles) -> BuiltinResult {
        let args = skip_option_terminator(args);
        if args.len() > 1 {
            return Err(gettext("exit: too many arguments").into());
        }

        if let Some(arg) = args.first() {
            if let Ok(n) = arg.to_str().unwrap_or("").parse::<i32>() {
                shell.exit(n);
            } else {
                Err(format!("exit: '{}' is not a valid number", arg.display()).into())
            }
        } else {
            shell.exit(shell.last_pipeline_exit_status)
        }
    }
}
