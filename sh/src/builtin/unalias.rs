use crate::builtin::{skip_option_terminator, BuiltinResult, BuiltinUtility};
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;

pub struct Unalias;

impl BuiltinUtility for Unalias {
    fn exec(
        &self,
        args: &[String],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        if args.first().is_some_and(|arg| arg == "-a") {
            if args.len() > 1 {
                return Err("unalias: too many arguments".into());
            }
            shell.alias_table.clear();
            return Ok(0);
        }

        let mut status = 0;
        let args = skip_option_terminator(args);
        for alias in args {
            if shell.alias_table.remove(alias).is_none() {
                opened_files.write_err(format!("unalias: '{alias}' not found "));
                status = 1;
            }
        }
        Ok(status)
    }
}
