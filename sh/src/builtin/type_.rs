use crate::builtin::{
    get_builtin_utility, get_special_builtin_utility, skip_option_terminator, BuiltinResult,
    BuiltinUtility,
};
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;
use crate::utils::find_command;

pub struct Type_;

impl BuiltinUtility for Type_ {
    fn exec(
        &self,
        args: &[String],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        let args = skip_option_terminator(args);
        let mut status = 0;
        for command_name in args {
            if let Some(alias) = shell.alias_table.get(command_name) {
                opened_files.write_out(format!("{} is aliased to '{}'\n", command_name, alias));
            } else if get_special_builtin_utility(command_name).is_some() {
                opened_files.write_out(format!("{} is a special shell builtin\n", command_name));
            } else if shell.functions.get(command_name.as_str()).is_some() {
                opened_files.write_out(format!("{} is a function\n", command_name))
            } else if get_builtin_utility(command_name).is_some() {
                opened_files.write_out(format!("{} is a shell builtin\n", command_name));
            } else {
                let path = shell.environment.get_str_value("PATH").unwrap_or_default();
                if let Some(command) = find_command(command_name, path) {
                    opened_files.write_out(format!(
                        "{} is {}\n",
                        command_name,
                        command.to_string_lossy()
                    ));
                } else {
                    // here we do what bash does, we don't stop processing commands, but we return 1
                    opened_files.write_err(format!("type: '{}' not found\n", command_name));
                    status = 1;
                };
            }
        }

        Ok(status)
    }
}
