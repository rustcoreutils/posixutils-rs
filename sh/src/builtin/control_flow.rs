use crate::builtin::BuiltinUtility;
use crate::shell::ControlFlowState;
use crate::shell::Shell;
pub struct Break;

impl BuiltinUtility for Break {
    fn exec(&self, args: &[String], shell: &mut Shell) -> i32 {
        if shell.loop_depth == 0 {
            eprintln!("break: 'break' can only be used inside 'for', 'while' and 'until' loops")
        }
        if args.len() > 1 {
            eprintln!("break: too many arguments");
            return 1;
        }
        let n = if let Some(n) = args.get(0) {
            match n.parse::<i32>() {
                Ok(n) => n,
                Err(_) => {
                    eprintln!("break: expected numeric argument");
                    return 1;
                }
            }
        } else {
            1
        };
        if n < 1 {
            eprintln!("break: argument to break has to be bigger than 0");
            return 1;
        }

        shell.control_flow_state = ControlFlowState::Break(shell.loop_depth.min(n as u32));
        0
    }
}
