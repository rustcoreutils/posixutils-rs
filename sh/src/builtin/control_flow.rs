use crate::builtin::BuiltinUtility;
use crate::shell::ControlFlowState;
use crate::shell::Shell;

fn loop_control_flow(
    args: &[String],
    shell: &mut Shell,
    name: &str,
    state: fn(u32) -> ControlFlowState,
) -> i32 {
    if shell.loop_depth == 0 {
        eprintln!("{name}: '{name}' can only be used inside 'for', 'while' and 'until' loops")
    }
    if args.len() > 1 {
        eprintln!("{name}: too many arguments");
        return 1;
    }
    let n = if let Some(n) = args.get(0) {
        match n.parse::<i32>() {
            Ok(n) => n,
            Err(_) => {
                eprintln!("{name}: expected numeric argument");
                return 1;
            }
        }
    } else {
        1
    };
    if n < 1 {
        eprintln!("{name}: argument has to be bigger than 0");
        return 1;
    }

    shell.control_flow_state = state(shell.loop_depth.min(n as u32));
    0
}

pub struct Break;

impl BuiltinUtility for Break {
    fn exec(&self, args: &[String], shell: &mut Shell) -> i32 {
        loop_control_flow(args, shell, "break", ControlFlowState::Break)
    }
}

pub struct Continue;

impl BuiltinUtility for Continue {
    fn exec(&self, args: &[String], shell: &mut Shell) -> i32 {
        loop_control_flow(args, shell, "break", ControlFlowState::Continue)
    }
}
