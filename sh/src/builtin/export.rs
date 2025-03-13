use crate::builtin::{BuiltinResult, SpecialBuiltinUtility};
use crate::parse::command_parser::is_valid_name;
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;

pub struct Export;

impl SpecialBuiltinUtility for Export {
    fn exec(
        &self,
        args: &[String],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        if args.is_empty() {
            return Err("export: too few arguments".into());
        }

        if args[0] == "-p" {
            let mut pairs = shell
                .environment
                .variables
                .iter()
                .filter(|(_, val)| val.export)
                .collect::<Vec<_>>();
            pairs.sort_by_key(|(k, _)| k.as_str());
            for (var, var_value) in pairs {
                if let Some(val) = &var_value.value {
                    opened_files
                        .stdout()
                        .write_str(format!("export {}='{}'\n", var, val));
                } else {
                    opened_files.stdout().write_str(format!("export {}\n", var));
                }
            }
            return Ok(0);
        }

        for arg in args {
            let (name, value) = if let Some(pos) = arg.find('=') {
                let (name, value) = arg.split_at(pos);
                if !is_valid_name(name) {
                    return Err(format!("export: '{name}' is not a valid name").into());
                }
                (name.to_string(), Some(value[1..].to_string()))
            } else {
                if !is_valid_name(&arg) {
                    return Err(format!("export: '{arg}' is not a valid name\n").into());
                }
                (arg.clone(), None)
            };
            shell.assign(name, value, true, false)?;
        }
        Ok(0)
    }
}
