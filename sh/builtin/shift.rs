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
use gettextrs::gettext;

pub struct Shift;

impl SpecialBuiltinUtility for Shift {
    fn exec(&self, args: &[String], shell: &mut Shell, _: &mut OpenedFiles) -> BuiltinResult {
        let args = skip_option_terminator(args);
        if args.len() > 1 {
            return Err(gettext("shift: too many arguments").into());
        }

        let n = if let Some(n) = args.first() {
            match n.parse::<usize>() {
                Ok(n) => {
                    if n > shell.positional_parameters.len() {
                        return Err(gettext("shift: count out of range").into());
                    }
                    n
                }
                Err(_) => return Err(gettext("shift: positive numeric argument required").into()),
            }
        } else {
            1
        };

        shell.positional_parameters.rotate_left(n);
        shell
            .positional_parameters
            .truncate(shell.positional_parameters.len() - n);

        Ok(0)
    }
}
