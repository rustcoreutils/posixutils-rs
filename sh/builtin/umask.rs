//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::{BuiltinResult, BuiltinUtility};
use crate::option_parser::OptionParser;
use crate::shell::Shell;
use crate::shell::opened_files::OpenedFiles;

fn format_mode(mode: u32) -> String {
    let types = ["", "x", "w", "wx", "r", "rx", "rw", "rwx"];
    let user = types[((mode >> 6) & 7) as usize];
    let group = types[((mode >> 3) & 7) as usize];
    let others = types[(mode & 7) as usize];

    format!("u={},g={},o={}\n", user, group, others)
}

pub struct Umask;

impl BuiltinUtility for Umask {
    fn exec(
        &self,
        args: &[String],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        let mut option_parser = OptionParser::new(args);

        let mut produce_symbolic_output = false;
        while let Some(option) = option_parser
            .next_option()
            .map_err(|opt| format!("umask: invalid option -{opt}"))?
        {
            match option {
                'S' => {
                    produce_symbolic_output = true;
                }
                opt => {
                    return Err(format!("umask: invalid option -{opt}").into());
                }
            }
        }

        if option_parser.next_argument() == args.len() {
            if produce_symbolic_output {
                opened_files.write_out(format_mode(shell.umask));
            } else {
                opened_files.write_out(format!("{:04o}\n", !shell.umask & 0o777));
            }
        } else if option_parser.next_argument() == args.len() - 1 {
            // TODO: support symbolic umask
            let new_umask = &args[option_parser.next_argument()];
            // Reject if umask contains a sign
            if new_umask.starts_with('+') || new_umask.starts_with('-') {
                return Err(format!("umask: invalid mask '{new_umask}'").into());
            }
            let new_umask = u32::from_str_radix(new_umask, 8)
                .map_err(|_| format!("umask: invalid mask '{new_umask}'"))?;
            if new_umask > 0o777 {
                return Err("umask: invalid mask".into());
            }
            shell.umask = !new_umask & 0o777;
        } else {
            return Err("umask: too many arguments".into());
        }

        Ok(0)
    }
}
