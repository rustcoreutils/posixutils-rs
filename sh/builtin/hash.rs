//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::{
    BuiltinResult, BuiltinUtility, get_builtin_utility, get_special_builtin_utility,
    skip_option_terminator,
};
use crate::shell::Shell;
use crate::shell::opened_files::OpenedFiles;

pub struct Hash;

impl BuiltinUtility for Hash {
    fn exec(
        &self,
        args: &[String],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        if args.first().is_some_and(|arg| arg == "-r") {
            if args.len() > 1 {
                return Err("hash: too many arguments".into());
            }
            shell.saved_command_locations.clear();
            return Ok(0);
        }

        let args = skip_option_terminator(args);

        if args.is_empty() {
            for (command_name, path) in &shell.saved_command_locations {
                opened_files.write_out(format!("{}: {}\n", command_name, path.to_string_lossy()));
            }
            Ok(0)
        } else {
            let mut status = 0;
            for arg in args {
                if get_special_builtin_utility(arg.as_str()).is_none()
                    && !shell.functions.contains_key(arg.as_str())
                    && get_builtin_utility(arg.as_str()).is_none()
                    && shell.find_command(arg, "", true).is_none()
                {
                    opened_files.write_out(format!("hash: command {} was not found\n", arg));
                    status = 1;
                }
            }
            Ok(status)
        }
    }
}
