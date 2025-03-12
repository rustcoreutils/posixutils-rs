use crate::builtin::{SpecialBuiltinResult, SpecialBuiltinUtility};
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;
use crate::utils::find_in_path;
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

        let path = if args[0].contains('/') {
            OsString::from(&args[0])
        } else if let Some(path) = find_in_path(
            &args[0],
            shell.environment.get_str_value("PATH").unwrap_or_default(),
        ) {
            path
        } else {
            return Err(format!("dot: {}, no such file or directory\n", &args[0]));
        };

        let source = match std::fs::read_to_string(path) {
            Ok(source) => source,
            Err(err) => {
                return Err(format!("dot: error opening file ({})\n", err));
            }
        };

        let lineno = shell.last_lineno;
        shell.last_lineno = 0;
        std::mem::swap(&mut shell.opened_files, opened_files);
        let execution_result = shell.execute_program(&source);
        std::mem::swap(&mut shell.opened_files, opened_files);
        shell.last_lineno = lineno;
        execution_result
            .map_err(|err| format!("dot: parsing error({}): {}\n", err.lineno, err.message))
    }
}
