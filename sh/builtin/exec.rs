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

pub struct Exec;

impl SpecialBuiltinUtility for Exec {
    fn exec(
        &self,
        args: &[ShString],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        let args = skip_option_terminator(args);
        if args.is_empty() {
            shell.opened_files = opened_files.clone();
            return Ok(0);
        }

        let Some(command) = shell.find_command(&args[0], "", true) else {
            // POSIX: 127 when the command is not found, 126 when it is found
            // but cannot be invoked.
            opened_files.write_err(format!(
                "{}: {}: {}\n",
                gettext("exec"),
                args[0].display(),
                gettext("command not found")
            ));
            if shell.is_interactive && !shell.is_subshell {
                return Ok(127);
            }
            shell.exit(127);
        };

        // An interactive shell must survive a failed `exec`; any other shell
        // exits with the failure status.
        let (message, status) = shell.try_exec(command, args, opened_files);
        opened_files.write_err(message);
        if shell.is_interactive && !shell.is_subshell {
            return Ok(status);
        }
        shell.exit(status)
    }
}
