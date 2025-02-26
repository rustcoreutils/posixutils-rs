use crate::builtin::BuiltinUtility;
use crate::shell::Shell;

pub struct AliasBuiltin;

impl BuiltinUtility for AliasBuiltin {
    fn exec(&self, args: &[String], shell: &mut Shell) -> i32 {
        if args.is_empty() {
            for (alias, command) in &shell.alias_table {
                println!("alias {}='{}'", alias, command);
            }
            return 0;
        }

        for arg in args {
            if let Some(eq_pos) = arg.find('=') {
                let (alias, command) = arg.split_at(eq_pos);
                let command = &command[1..];
                shell
                    .alias_table
                    .insert(alias.to_string(), command.to_string());
            } else {
                if let Some(command) = shell.alias_table.get(arg) {
                    println!("alias {}='{}'", arg, command);
                } else {
                    eprintln!("alias: {}: not found", arg);
                    return 1;
                }
            }
        }
        0
    }
}
