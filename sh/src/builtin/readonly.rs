use crate::builtin::{SpecialBuiltinResult, SpecialBuiltinUtility};
use crate::parse::command_parser::is_valid_name;
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;
use std::collections::hash_map::Entry;

pub struct ReadOnly;

impl SpecialBuiltinUtility for ReadOnly {
    fn exec(
        &self,
        args: &[String],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> SpecialBuiltinResult {
        if args.is_empty() {
            return Err("readonly: too few arguments".to_string());
        }

        if args[0] == "-p" {
            let mut pairs = shell
                .environment
                .variables
                .iter()
                .filter(|(_, val)| val.readonly)
                .collect::<Vec<_>>();
            pairs.sort_by_key(|(k, _)| k.as_str());
            for (var, var_value) in pairs {
                if let Some(val) = &var_value.value {
                    opened_files
                        .stdout()
                        .write_str(format!("readonly {}='{}'\n", var, val));
                } else {
                    opened_files
                        .stdout()
                        .write_str(format!("readonly {}\n", var));
                }
            }
            return Ok(0);
        }

        for arg in args {
            let (name, value) = if let Some(pos) = arg.find('=') {
                let (name, value) = arg.split_at(pos);
                if !is_valid_name(name) {
                    return Err(format!("readonly: '{name}' is not a valid name"));
                }
                (name.to_string(), Some(value[1..].to_string()))
            } else {
                if !is_valid_name(&arg) {
                    return Err(format!("readonly: '{arg}' is not a valid name\n"));
                }
                (arg.clone(), None)
            };
            shell
                .environment
                .set(name, value, false, true)
                .map_err(|err| {
                    format!(
                        "readonly: cannot assign to readonly var '{}'",
                        err.var_name()
                    )
                })?;
        }
        Ok(0)
    }
}
