//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::{skip_option_terminator, BuiltinResult, SpecialBuiltinUtility};
use crate::parse::command_parser::is_valid_name;
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;

pub struct ReadOnly;

impl SpecialBuiltinUtility for ReadOnly {
    fn exec(
        &self,
        args: &[String],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        if args.first().is_some_and(|arg| arg == "-p") {
            if args.len() > 1 && !(args.len() == 2 && args[1] == "--") {
                return Err("export: too many arguments".into());
            }
            let mut pairs = shell
                .environment
                .global_scope()
                .iter()
                .filter(|(_, val)| val.readonly)
                .collect::<Vec<_>>();
            pairs.sort_by_key(|(k, _)| k.as_str());
            for (var, var_value) in pairs {
                if let Some(val) = &var_value.value {
                    opened_files.write_out(format!("readonly {}='{}'\n", var, val));
                } else {
                    opened_files.write_out(format!("readonly {}\n", var));
                }
            }
            return Ok(0);
        }

        let args = skip_option_terminator(args);
        if args.is_empty() {
            return Err("readonly: too few arguments".to_string().into());
        }

        for arg in args {
            let (name, value) = if let Some(pos) = arg.find('=') {
                let (name, value) = arg.split_at(pos);
                if !is_valid_name(name) {
                    return Err(format!("readonly: '{name}' is not a valid name").into());
                }
                (name.to_string(), Some(value[1..].to_string()))
            } else {
                if !is_valid_name(arg) {
                    return Err(format!("readonly: '{arg}' is not a valid name\n").into());
                }
                (arg.clone(), None)
            };
            if let Some(value) = value {
                shell.assign_global(name, value)?.readonly = true;
            } else {
                shell.environment.promote_local_or_get_global(name).readonly = true;
            }
        }
        Ok(0)
    }
}
