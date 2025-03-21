use crate::builtin::{
    skip_option_terminator, BuiltinResult, BuiltinUtility, SpecialBuiltinUtility,
};
use crate::option_parser::OptionParser;
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;

pub struct BuiltinUnset;

impl SpecialBuiltinUtility for BuiltinUnset {
    fn exec(&self, args: &[String], shell: &mut Shell, _: &mut OpenedFiles) -> BuiltinResult {
        let mut unset_var = false;
        let mut unset_function = false;
        let mut parser = OptionParser::new(args);
        while let Some(option) = parser
            .next_option()
            .map_err(|arg| format!("unset: invalid option {arg}"))?
        {
            if unset_var || unset_function {
                return Err("unset: cannot set multiple options".into());
            }
            match option {
                'f' => {
                    unset_function = true;
                }
                'v' => {
                    unset_var = true;
                }
                other => {
                    return Err(format!("unset: invalid option -{}", other).into());
                }
            }
        }

        if !unset_var && !unset_function {
            unset_var = true;
        }

        for name in &args[parser.next_argument()..] {
            if unset_var {
                if shell.environment.unset(name).is_err() {
                    return Err(
                        format!("unset: cannot unset readonly variable '{}'\n", name).into(),
                    );
                }
            } else {
                shell.functions.remove(name.as_str());
            }
        }

        Ok(0)
    }
}
