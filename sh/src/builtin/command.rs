//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::{
    get_builtin_utility, get_special_builtin_utility, BuiltinError, BuiltinResult, BuiltinUtility,
};
use crate::option_parser::OptionParser;
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;
use crate::utils::DEFAULT_PATH;

#[derive(PartialEq, Eq)]
enum Action {
    Execute,
    PrintShort,
    PrintLong,
}

struct CommandArgs<'a> {
    use_default_path: bool,
    action: Action,
    command_name: &'a str,
    /// includes command_name
    args: &'a [String],
}

impl CommandArgs<'_> {
    fn parse(args: &[String]) -> Result<CommandArgs, String> {
        let mut use_default_path = false;
        let mut action = Action::Execute;

        let mut option_parser = OptionParser::new(args);

        while let Some(option) = option_parser
            .next_option()
            .map_err(|opt| format!("command: invalid option ({opt})"))?
        {
            match option {
                'p' => {
                    if use_default_path {
                        return Err("command: cannot specify -p multiple times".into());
                    }
                    use_default_path = true;
                }
                'v' | 'V' => {
                    if action != Action::Execute {
                        return Err("command: cannot specify both -v and -V".into());
                    }
                    if option == 'v' {
                        action = Action::PrintShort;
                    } else {
                        action = Action::PrintLong;
                    }
                }
                other => return Err(format!("command: invalid option -{other}")),
            }
        }

        let first_operand = option_parser.next_argument();
        if first_operand >= args.len() {
            return Err("command: missing command name".into());
        }
        if action != Action::Execute && first_operand != args.len() - 1 {
            return Err("command: too many arguments".into());
        }
        Ok(CommandArgs {
            use_default_path,
            action,
            command_name: &args[first_operand],
            args: &args[first_operand..],
        })
    }
}

pub struct Command;

impl BuiltinUtility for Command {
    fn exec(
        &self,
        args: &[String],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        let args = CommandArgs::parse(args)?;

        if args.action != Action::Execute {
            if let Some(alias) = shell.alias_table.get(args.command_name) {
                if args.action == Action::PrintLong {
                    opened_files
                        .write_out(format!("{} is aliased to '{}'\n", args.command_name, alias));
                } else {
                    opened_files.write_out(format!("alias {}='{}'\n", args.command_name, alias));
                }

                return Ok(0);
            }
        }

        let default_path = if args.use_default_path {
            DEFAULT_PATH
        } else {
            ""
        };

        if args.action == Action::Execute {
            if let Some(special_builtin_utility) = get_special_builtin_utility(args.command_name) {
                return special_builtin_utility.exec(&args.args[1..], shell, opened_files);
            } else if let Some(builtin_utility) = get_builtin_utility(args.command_name) {
                return builtin_utility.exec(&args.args[1..], shell, opened_files);
            } else if let Some(command) = shell.find_command(args.command_name, default_path, true)
            {
                return shell
                    .fork_and_exec(command, args.args, opened_files)
                    .map_err(BuiltinError::OsError);
            }
            return Err(format!("command: {} not found", args.command_name).into());
        }

        if get_special_builtin_utility(args.command_name).is_some() {
            if args.action == Action::PrintShort {
                opened_files.write_out(format!("{}\n", args.command_name))
            } else {
                opened_files.write_out(format!(
                    "{} is a special shell builtin\n",
                    args.command_name
                ))
            }
        } else if shell.functions.contains_key(args.command_name) {
            if args.action == Action::PrintShort {
                opened_files.write_out(format!("{}\n", args.command_name))
            } else {
                opened_files.write_out(format!("{} is a function\n", args.command_name))
            }
        } else if get_builtin_utility(args.command_name).is_some() {
            if args.action == Action::PrintShort {
                opened_files.write_out(format!("{}\n", args.command_name))
            } else {
                opened_files.write_out(format!("{} is a shell builtin\n", args.command_name))
            }
        } else if let Some(command) = shell.find_command(args.command_name, default_path, true) {
            if args.action == Action::PrintShort {
                opened_files.write_out(format!("{}\n", command.to_string_lossy()))
            } else {
                opened_files.write_out(format!(
                    "{} is {}\n",
                    args.command_name,
                    command.to_string_lossy()
                ))
            }
        } else {
            return Err(format!("command: {} not found", args.command_name).into());
        }

        Ok(0)
    }
}
