//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::{BuiltinResult, BuiltinUtility};
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;

pub struct AliasBuiltin;

impl BuiltinUtility for AliasBuiltin {
    fn exec(
        &self,
        args: &[String],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        // POSIX output format is "%s=%s\n" with the value quoted for re-input
        // (no "alias " prefix).
        if args.is_empty() {
            for (alias, command) in &shell.alias_table {
                opened_files.write_out(format!(
                    "{}={}\n",
                    alias,
                    crate::utils::shell_quote(command)
                ));
            }
            return Ok(0);
        }

        let mut status = 0;
        for arg in args {
            if let Some(eq_pos) = arg.find('=') {
                let (alias, command) = arg.split_at(eq_pos);
                let command = &command[1..];
                shell
                    .alias_table
                    .insert(alias.to_string(), command.to_string());
            } else if let Some(command) = shell.alias_table.get(arg) {
                opened_files.write_out(format!("{}={}\n", arg, crate::utils::shell_quote(command)));
            } else {
                // report the error but keep processing the remaining operands
                opened_files.write_err(format!("alias: {}: not found\n", arg));
                status = 1;
            }
        }
        Ok(status)
    }
}
