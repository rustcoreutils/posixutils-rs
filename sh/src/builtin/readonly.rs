use crate::builtin::BuiltinUtility;
use crate::parse::command_parser::is_valid_name;
use crate::shell::{Shell, VariableValue};
use std::collections::hash_map::Entry;

pub struct ReadOnly;

impl BuiltinUtility for ReadOnly {
    fn exec(&self, args: &[String], shell: &mut Shell) -> i32 {
        if args.is_empty() {
            eprintln!("readonly: too few arguments");
            return 1;
        }

        if args[0] == "-p" {
            let mut pairs = shell
                .environment
                .iter()
                .filter(|(_, val)| val.readonly)
                .collect::<Vec<_>>();
            pairs.sort_by_key(|(k, _)| k.as_str());
            for (var, var_value) in pairs {
                if let Some(val) = &var_value.value {
                    println!("readonly {}='{}'", var, val);
                } else {
                    println!("readonly {}", var)
                }
            }
            return 0;
        }

        for arg in args {
            let (name, value) = if let Some(pos) = arg.find('=') {
                let (name, value) = arg.split_at(pos);
                if !is_valid_name(name) {
                    eprintln!("readonly: '{}' is not a valid name", name);
                    return 1;
                }
                (name.to_string(), Some(value[1..].to_string()))
            } else {
                if !is_valid_name(&arg) {
                    eprintln!("readonly: '{}' is not a valid name", arg);
                    return 1;
                }
                (arg.clone(), None)
            };
            match shell.environment.entry(name) {
                Entry::Occupied(mut e) => {
                    e.get_mut().readonly = true;
                    if value.is_some() {
                        e.get_mut().value = value;
                    }
                }
                Entry::Vacant(e) => {
                    e.insert(VariableValue {
                        value,
                        export: false,
                        readonly: true,
                    });
                }
            }
        }
        0
    }
}
