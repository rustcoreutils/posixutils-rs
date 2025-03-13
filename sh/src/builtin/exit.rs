use crate::builtin::{BuiltinResult, SpecialBuiltinUtility};
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;

pub struct Exit;

impl SpecialBuiltinUtility for Exit {
    fn exec(&self, args: &[String], shell: &mut Shell, _: &mut OpenedFiles) -> BuiltinResult {
        if args.len() > 1 {
            return Err("exit: too many arguments".into());
        }

        if let Some(arg) = args.get(0) {
            if let Ok(n) = arg.parse::<i32>() {
                std::process::exit(n);
            } else {
                Err(format!("exit: '{arg}' is not a valid number").into())
            }
        } else {
            std::process::exit(shell.last_pipeline_exit_status)
        }
    }
}
