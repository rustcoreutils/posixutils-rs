use crate::builtin::{
    get_builtin_utility, get_special_builtin_utility, skip_option_terminator, BuiltinResult,
    BuiltinUtility,
};
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;

pub struct Hash;

impl BuiltinUtility for Hash {
    fn exec(
        &self,
        args: &[String],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        if args.first().is_some_and(|arg| arg == "-r") {
            if args.len() > 1 {
                return Err("hash: too many arguments".into());
            }
            shell.saved_command_locations.clear();
            return Ok(0);
        }

        let args = skip_option_terminator(args);

        if args.len() == 0 {
            for (command_name, path) in &shell.saved_command_locations {
                opened_files.write_out(format!("{}: {}\n", command_name, path.to_string_lossy()));
            }
            Ok(0)
        } else {
            let mut status = 0;
            for arg in args {
                if !get_special_builtin_utility(arg.as_str()).is_some()
                    && !shell.functions.get(arg.as_str()).is_some()
                    && !get_builtin_utility(arg.as_str()).is_some()
                {
                    if shell.find_command(arg, "", true).is_none() {
                        opened_files.write_out(format!("hash: command {} was not found\n", arg));
                        status = 1;
                    }
                }
            }
            Ok(status)
        }
    }
}
