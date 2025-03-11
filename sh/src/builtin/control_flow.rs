use crate::builtin::{BuiltinUtility, SpecialBuiltinResult, SpecialBuiltinUtility};
use crate::shell::opened_files::{OpenedFiles, WriteFile};
use crate::shell::ControlFlowState;
use crate::shell::Shell;

fn loop_control_flow(
    args: &[String],
    shell: &mut Shell,
    name: &str,
    state: fn(u32) -> ControlFlowState,
) -> SpecialBuiltinResult {
    if shell.loop_depth == 0 {
        return Err(format!(
            "{name}: '{name}' can only be used inside 'for', 'while' and 'until' loops"
        ));
    }
    if args.len() > 1 {
        return Err(format!("{name}: too many arguments"));
    }
    let n = if let Some(n) = args.get(0) {
        match n.parse::<i32>() {
            Ok(n) => n,
            Err(_) => {
                return Err(format!("{name}: expected numeric argument\n"));
            }
        }
    } else {
        1
    };
    if n < 1 {
        return Err(format!("{name}: argument has to be bigger than 0\n"));
    }

    shell.control_flow_state = state(shell.loop_depth.min(n as u32));
    Ok(0)
}

pub struct Break;

impl SpecialBuiltinUtility for Break {
    fn exec(&self, args: &[String], shell: &mut Shell, _: &OpenedFiles) -> SpecialBuiltinResult {
        loop_control_flow(args, shell, "break", ControlFlowState::Break)
    }
}

pub struct Continue;

impl SpecialBuiltinUtility for Continue {
    fn exec(&self, args: &[String], shell: &mut Shell, _: &OpenedFiles) -> SpecialBuiltinResult {
        loop_control_flow(args, shell, "break", ControlFlowState::Continue)
    }
}
