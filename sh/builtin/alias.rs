//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::{BuiltinResult, BuiltinUtility};
use crate::shell::Shell;
use crate::shell::opened_files::OpenedFiles;

pub struct AliasBuiltin;

impl BuiltinUtility for AliasBuiltin {
    fn exec(
        &self,
        args: &[String],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        if args.is_empty() {
            for (alias, command) in &shell.alias_table {
                opened_files.write_out(format!("alias {}='{}'", alias, command));
            }
            return Ok(0);
        }

        for arg in args {
            if let Some(eq_pos) = arg.find('=') {
                let (alias, command) = arg.split_at(eq_pos);
                let command = &command[1..];
                shell
                    .alias_table
                    .insert(alias.to_string(), command.to_string());
            } else if let Some(command) = shell.alias_table.get(arg) {
                opened_files.write_out(format!("alias {}='{}'", arg, command));
            } else {
                return Err(format!("alias: {}: not found", arg).into());
            }
        }
        Ok(0)
    }
}
