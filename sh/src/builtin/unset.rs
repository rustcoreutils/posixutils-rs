use crate::builtin::BuiltinUtility;
use crate::shell::Shell;

pub struct BuiltinUnset;

impl BuiltinUtility for BuiltinUnset {
    fn exec(&self, args: &[String], shell: &mut Shell) -> i32 {
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
                eprintln!("unset: cannot simultaneously unset a function and a variable");
                return 1;
            }
            _ => { start_index = 0; }
        }

        for name in &args[start_index..] {
            if unset_var {
                if let Some(var) = shell.environment.get_mut(name) {
                    if var.readonly {
                        eprintln!("unset: cannot unset readonly variable '{}'", name);
                        return 1;
                    }
                    var.value = None;
                }
            } else {
                shell.functions.remove(name.as_str());
            }
        }

        0
    }
}