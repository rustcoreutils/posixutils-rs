use crate::builtin::{skip_option_terminator, BuiltinResult, SpecialBuiltinUtility};
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
    ) -> BuiltinResult {
        let args = skip_option_terminator(args);
        if args.is_empty() {
            shell.opened_files = opened_files.clone();
            return Ok(0);
        }

        let command = shell
            .find_command(&args[0], "", true)
            .ok_or(format!("exec: {}: command not found", args[0]))?;

        shell.exec(command, &args, opened_files)
    }
}
