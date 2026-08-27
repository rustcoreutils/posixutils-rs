//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::{args_as_str, BuiltinResult, BuiltinUtility};
use crate::option_parser::OptionParser;
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;
use crate::shstr::ShString;
use std::path::{Component, Path};

pub struct Pwd;

/// POSIX `pwd` `-L`, step 2: `$PWD` may stand in for the working directory only
/// if it is an absolute pathname with no `.` or `..` components.
fn pwd_is_usable(value: &str) -> bool {
    let path = Path::new(value);
    path.is_absolute()
        && !path
            .components()
            .any(|c| matches!(c, Component::CurDir | Component::ParentDir))
}

impl BuiltinUtility for Pwd {
    fn exec(
        &self,
        args: &[ShString],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        let args = &args_as_str("pwd", args)?;
        // `cd` is a builtin and updates the shell's own idea of where it is, so
        // `pwd` has to be one too: forking /bin/pwd would report the *process*
        // working directory, which differs from the logical one after `cd`
        // through a symbolic link.
        let mut physical = false;
        let mut option_parser = OptionParser::new(args);
        while let Some(option) = option_parser
            .next_option()
            .map_err(|opt| format!("pwd: invalid option -{opt}"))?
        {
            match option {
                'L' => physical = false,
                'P' => physical = true,
                other => return Err(format!("pwd: invalid option -{other}").into()),
            }
        }
        // POSIX defines no operands for `pwd`, but dash and bash both ignore
        // any that appear; rejecting them would break scripts for nothing.

        let logical = shell.environment.get_str_value("PWD").unwrap_or_default();
        let directory = if !physical && pwd_is_usable(logical) {
            logical.to_string()
        } else {
            // -P, or a $PWD that cannot be trusted: report the resolved path.
            std::fs::canonicalize(&shell.current_directory)
                .map(|p| p.to_string_lossy().into_owned())
                .unwrap_or_else(|_| shell.current_directory.to_string_lossy().into_owned())
        };
        opened_files.write_out(format!("{directory}\n"));
        Ok(0)
    }
}
