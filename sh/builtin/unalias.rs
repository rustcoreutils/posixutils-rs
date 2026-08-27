//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::{args_as_str, skip_option_terminator, BuiltinResult, BuiltinUtility};
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;
use crate::shstr::ShString;
use gettextrs::gettext;

pub struct Unalias;

impl BuiltinUtility for Unalias {
    fn exec(
        &self,
        args: &[ShString],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        let args = &args_as_str("unalias", args)?;
        if args.first().is_some_and(|arg| *arg == "-a") {
            // `-a` removes every alias; any further operands are redundant but
            // not an error (dash and bash accept them too).
            shell.alias_table.clear();
            return Ok(0);
        }

        let mut status = 0;
        let args = skip_option_terminator(args);
        for alias in args {
            if shell.alias_table.remove(*alias).is_none() {
                opened_files.write_err(format!("unalias: '{alias}' {}\n", gettext("not found")));
                status = 1;
            }
        }
        Ok(status)
    }
}
