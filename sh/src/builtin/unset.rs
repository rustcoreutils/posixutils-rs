use crate::builtin::{BuiltinUtility, SpecialBuiltinResult, SpecialBuiltinUtility};
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;

pub struct BuiltinUnset;

impl SpecialBuiltinUtility for BuiltinUnset {
    fn exec(
        &self,
        args: &[String],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> SpecialBuiltinResult {
        if args.is_empty() {
            return Ok(0);
        }

        let mut unset_var = true;
        let mut start_index = 1;
        match args[0].as_str() {
            "-f" => {
                unset_var = false;
            }
            "-v" => {}
            "-fv" | "-vf" => {
                return Err(
                    "unset: cannot simultaneously unset a function and a variable".to_string(),
                );
            }
            _ => {
                start_index = 0;
            }
        }

        for name in &args[start_index..] {
            if unset_var {
                if shell.environment.unset(name).is_err() {
                    return Err(format!(
                        "unset: cannot unset readonly variable '{}'\n",
                        name
                    ));
                }
            } else {
                shell.functions.remove(name.as_str());
            }
        }

        Ok(0)
    }
}
