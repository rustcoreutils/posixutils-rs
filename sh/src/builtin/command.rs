use crate::builtin::{
    get_builtin_utility, get_special_builtin_utility, BuiltinError, BuiltinResult, BuiltinUtility,
};
use crate::option_parser::OptionParser;
use crate::shell::environment::Environment;
use crate::shell::opened_files::OpenedFiles;
use crate::shell::{CommandExecutionError, ControlFlowState, Shell};
use crate::utils::{find_command, DEFAULT_PATH};

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

        if let Some(special_builtin_utility) = get_special_builtin_utility(args.command_name) {
            match args.action {
                Action::Execute => {
                    return special_builtin_utility.exec(&args.args[1..], shell, opened_files);
                }
                Action::PrintShort => opened_files.write_out(format!("{}\n", args.command_name)),
                Action::PrintLong => opened_files.write_out(format!(
                    "{} is a special shell builtin\n",
                    args.command_name
                )),
            }
        } else if let Some(function_body) = shell.functions.get(args.command_name).cloned() {
            match args.action {
                Action::Execute => {
                    let mut function_args = args.args[1..].to_vec();

                    std::mem::swap(&mut shell.opened_files, opened_files);
                    std::mem::swap(&mut function_args, &mut shell.positional_parameters);
                    shell.function_call_depth += 1;
                    let result = shell.interpret_compound_command(&function_body, &[], false);
                    if shell.control_flow_state == ControlFlowState::Return {
                        shell.control_flow_state = ControlFlowState::None;
                    }
                    shell.function_call_depth -= 1;
                    std::mem::swap(&mut function_args, &mut shell.positional_parameters);
                    std::mem::swap(&mut shell.opened_files, opened_files);
                    return result.map_err(|_| todo!());
                }
                Action::PrintShort => opened_files.write_out(format!("{}\n", args.command_name)),
                Action::PrintLong => {
                    opened_files.write_out(format!("{} is a function\n", args.command_name))
                }
            }
        } else if let Some(builtin_utility) = get_builtin_utility(args.command_name) {
            match args.action {
                Action::Execute => {
                    return builtin_utility.exec(&args.args[1..], shell, opened_files);
                }
                Action::PrintShort => opened_files.write_out(format!("{}\n", args.command_name)),
                Action::PrintLong => {
                    opened_files.write_out(format!("{} is a shell builtin\n", args.command_name))
                }
            }
        } else {
            let path = if args.use_default_path {
                DEFAULT_PATH
            } else {
                shell.environment.get_str_value("PATH").unwrap_or_default()
            };
            let command = if let Some(command) = find_command(args.command_name, path) {
                command
            } else {
                return Err(format!("command: {} not found", args.command_name).into());
            };

            match args.action {
                Action::Execute => {
                    let mut command_environment = shell.clone();
                    command_environment.opened_files = opened_files.clone();
                    return command_environment
                        .exec(command, args.args)
                        .map_err(BuiltinError::OsError);
                }
                Action::PrintShort => {
                    opened_files.write_out(format!("{}\n", command.to_string_lossy()))
                }
                Action::PrintLong => opened_files.write_out(format!(
                    "{} is {}\n",
                    args.command_name,
                    command.to_string_lossy()
                )),
            }
        }

        Ok(0)
    }
}
