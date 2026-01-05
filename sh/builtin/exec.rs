//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::{BuiltinResult, SpecialBuiltinUtility, skip_option_terminator};
use crate::shell::Shell;
use crate::shell::opened_files::OpenedFiles;

pub struct Exec;

impl SpecialBuiltinUtility for Exec {
    fn exec(
        &self,
        args: &[String],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        let args = skip_option_terminator(args);
        if args.is_empty() {
            shell.opened_files = opened_files.clone();
            return Ok(0);
        }

        let command = shell
            .find_command(&args[0], "", true)
            .ok_or(format!("exec: {}: command not found", args[0]))?;

        shell.exec(command, args, opened_files)
    }
}
