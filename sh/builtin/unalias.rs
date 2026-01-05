//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::{BuiltinResult, BuiltinUtility, skip_option_terminator};
use crate::shell::Shell;
use crate::shell::opened_files::OpenedFiles;

pub struct Unalias;

impl BuiltinUtility for Unalias {
    fn exec(
        &self,
        args: &[String],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        if args.first().is_some_and(|arg| arg == "-a") {
            if args.len() > 1 {
                return Err("unalias: too many arguments".into());
            }
            shell.alias_table.clear();
            return Ok(0);
        }

        let mut status = 0;
        let args = skip_option_terminator(args);
        for alias in args {
            if shell.alias_table.remove(alias).is_none() {
                opened_files.write_err(format!("unalias: '{alias}' not found "));
                status = 1;
            }
        }
        Ok(status)
    }
}
