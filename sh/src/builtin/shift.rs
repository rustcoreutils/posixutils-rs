use crate::builtin::{skip_option_terminator, BuiltinResult, SpecialBuiltinUtility};
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;

pub struct Shift;

impl SpecialBuiltinUtility for Shift {
    fn exec(
        &self,
        args: &[String],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        let args = skip_option_terminator(args);
        if args.len() > 1 {
            return Err("shift: too many arguments".into());
        }

        let n = if let Some(n) = args.get(0) {
            match n.parse::<usize>() {
                Ok(n) => {
                    if n > shell.positional_parameters.len() {
                        return Err("shift: count out of range".into());
                    }
                    n
                }
                Err(_) => return Err("shift: positive numeric argument required".into()),
            }
        } else {
            1
        };

        shell.positional_parameters.rotate_left(n);
        shell
            .positional_parameters
            .truncate(shell.positional_parameters.len() - n);

        Ok(0)
    }
}
