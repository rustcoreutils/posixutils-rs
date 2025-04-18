//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::{skip_option_terminator, BuiltinResult, SpecialBuiltinUtility};
use crate::os::find_command;
use crate::shell::opened_files::OpenedFiles;
use crate::shell::{execute_file_as_script, ScriptExecutionError, Shell};
use std::path::Path;

pub struct Dot;

impl SpecialBuiltinUtility for Dot {
    fn exec(
        &self,
        args: &[String],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        let args = skip_option_terminator(args);

        if args.len() != 1 {
            return Err("dot: incorrect number of arguments".into());
        }

        let path = shell.environment.get_str_value("PATH").unwrap_or_default();
        let file_path = if let Some(file_path) = find_command(&args[0], path) {
            file_path
        } else {
            return Err(format!("dot: {}, no such file or directory\n", &args[0]).into());
        };

        std::mem::swap(&mut shell.opened_files, opened_files);
        shell.dot_script_depth += 1;

        let result = execute_file_as_script(shell, Path::new(&file_path));

        shell.dot_script_depth -= 1;
        std::mem::swap(&mut shell.opened_files, opened_files);

        match result {
            Ok(status) => Ok(status),
            Err(ScriptExecutionError::IoError(io_err)) => {
                Err(format!("dot: io error: {}\n", io_err).into())
            }
            Err(ScriptExecutionError::ParsingError(parser_err)) => Err(format!(
                "dot: parsing error ({}): {}\n",
                parser_err.lineno, parser_err.message
            )
            .into()),
        }
    }
}
