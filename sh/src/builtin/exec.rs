use crate::builtin::{SpecialBuiltinResult, SpecialBuiltinUtility};
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;
use crate::utils::{exec, find_command, ExecError};

pub struct Exec;

impl SpecialBuiltinUtility for Exec {
    fn exec(
        &self,
        args: &[String],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> SpecialBuiltinResult {
        if args.is_empty() {
            shell.opened_files = opened_files.clone();
            return Ok(0);
        }

        let path = shell.environment.get_str_value("PATH").unwrap_or_default();
        let command = if let Some(command) = find_command(&args[0], path) {
            command
        } else {
            return Err(format!("exec: '{}' command not found", args[0]));
        };

        match exec(command, &args, opened_files, &shell.environment) {
            Err(ExecError::OsError(err)) => Err(err.to_string()),
            Err(ExecError::CannotExecute(err)) => {
                Err(format!("exec: could not execute '{}' ({})", args[0], err))
            }
            Ok(_) => unreachable!(),
        }
    }
}
