use crate::builtin::{SpecialBuiltinResult, SpecialBuiltinUtility};
use crate::shell::opened_files::OpenedFiles;
use crate::shell::{ControlFlowState, Shell};
use crate::utils::{find_command, find_in_path};
use std::ffi::OsString;

pub struct Dot;

impl SpecialBuiltinUtility for Dot {
    fn exec(
        &self,
        args: &[String],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> SpecialBuiltinResult {
        if args.len() != 1 {
            return Err("dot: incorrect number of arguments".to_string());
        }

        let path = shell.environment.get_str_value("PATH").unwrap_or_default();
        let file_path = if let Some(file_path) = find_command(&args[0], path) {
            file_path
        } else {
            return Err(format!("dot: {}, no such file or directory\n", &args[0]));
        };

        let source = match std::fs::read_to_string(file_path) {
            Ok(source) => source,
            Err(err) => {
                return Err(format!("dot: error opening file ({})\n", err));
            }
        };

        let lineno = shell.last_lineno;
        shell.last_lineno = 0;
        std::mem::swap(&mut shell.opened_files, opened_files);
        shell.dot_script_depth += 1;
        let execution_result = shell.execute_program(&source);
        shell.dot_script_depth -= 1;
        std::mem::swap(&mut shell.opened_files, opened_files);
        shell.last_lineno = lineno;
        execution_result
            .map_err(|err| format!("dot: parsing error({}): {}\n", err.lineno, err.message))
    }
}
