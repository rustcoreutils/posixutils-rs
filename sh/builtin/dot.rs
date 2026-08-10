//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
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
use gettextrs::gettext;
use std::fs::File;
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
            return Err(gettext("dot: incorrect number of arguments").into());
        }

        let path = shell.environment.get_str_value("PATH").unwrap_or_default();
        // `find_command` is shared with command lookup, where executability
        // rather than readability is the gate, so check readability here: the
        // dot utility reads the file, it does not execute it.
        let file_path = match find_command(&args[0], path) {
            Some(file_path) if File::open(&file_path).is_ok() => file_path,
            Some(file_path) => {
                return Err(format!(
                    "dot: {}: {}\n",
                    file_path.to_string_lossy(),
                    gettext("cannot open file")
                )
                .into())
            }
            None => {
                return Err(format!("dot: {}, no such file or directory\n", args[0]).into());
            }
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
