use crate::builtin::SpecialBuiltinUtility;
use crate::parse::command_parser::is_valid_name;
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;
use std::collections::hash_map::Entry;

pub struct ReadOnly;

impl SpecialBuiltinUtility for ReadOnly {
    fn exec(&self, args: &[String], shell: &mut Shell, opened_files: OpenedFiles) -> i32 {
        if args.is_empty() {
            opened_files
                .stderr()
                .write_str("readonly: too few arguments\n");
            return 1;
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
            return 0;
        }

        for arg in args {
            let (name, value) = if let Some(pos) = arg.find('=') {
                let (name, value) = arg.split_at(pos);
                if !is_valid_name(name) {
                    opened_files
                        .stderr()
                        .write_str(format!("readonly: '{}' is not a valid name\n", name));
                    return 1;
                }
                (name.to_string(), Some(value[1..].to_string()))
            } else {
                if !is_valid_name(&arg) {
                    opened_files
                        .stderr()
                        .write_str(format!("readonly: '{}' is not a valid name\n", arg));
                    return 1;
                }
                (arg.clone(), None)
            };
            shell.environment.set_readonly(&name, value);
        }
        0
    }
}
