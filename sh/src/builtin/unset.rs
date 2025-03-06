use crate::builtin::{BuiltinUtility, SpecialBuiltinUtility};
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;

pub struct BuiltinUnset;

impl SpecialBuiltinUtility for BuiltinUnset {
    fn exec(&self, args: &[String], shell: &mut Shell, opened_files: OpenedFiles) -> i32 {
        if args.is_empty() {
            return 0;
        }

        let mut unset_var = true;
        let mut start_index = 1;
        match args[0].as_str() {
            "-f" => {
                unset_var = false;
            }
            "-v" => {}
            "-fv" | "-vf" => {
                opened_files
                    .stderr()
                    .write_str("unset: cannot simultaneously unset a function and a variable\n");
                return 1;
            }
            _ => {
                start_index = 0;
            }
        }

        for name in &args[start_index..] {
            if unset_var {
                if shell.environment.unset(name).is_err() {
                    opened_files.stderr().write_str(format!(
                        "unset: cannot unset readonly variable '{}'\n",
                        name
                    ));
                    return 1;
                }
            } else {
                shell.functions.remove(name.as_str());
            }
        }

        0
    }
}
