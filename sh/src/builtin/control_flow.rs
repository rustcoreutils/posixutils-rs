use crate::builtin::{BuiltinUtility, SpecialBuiltinUtility};
use crate::shell::opened_files::{OpenedFiles, WriteFile};
use crate::shell::ControlFlowState;
use crate::shell::Shell;

fn loop_control_flow(
    args: &[String],
    shell: &mut Shell,
    name: &str,
    state: fn(u32) -> ControlFlowState,
    mut stderr: WriteFile,
) -> i32 {
    if shell.loop_depth == 0 {
        stderr.write_str(format!(
            "{name}: '{name}' can only be used inside 'for', 'while' and 'until' loops\n"
        ));
    }
    if args.len() > 1 {
        stderr.write_str(format!("{name}: too many arguments\n"));
        return 1;
    }
    let n = if let Some(n) = args.get(0) {
        match n.parse::<i32>() {
            Ok(n) => n,
            Err(_) => {
                stderr.write_str(format!("{name}: expected numeric argument\n"));
                return 1;
            }
        }
    } else {
        1
    };
    if n < 1 {
        stderr.write_str(format!("{name}: argument has to be bigger than 0\n"));
        return 1;
    }

    shell.control_flow_state = state(shell.loop_depth.min(n as u32));
    0
}

pub struct Break;

impl SpecialBuiltinUtility for Break {
    fn exec(&self, args: &[String], shell: &mut Shell, opened_files: OpenedFiles) -> i32 {
        loop_control_flow(
            args,
            shell,
            "break",
            ControlFlowState::Break,
            opened_files.stderr(),
        )
    }
}

pub struct Continue;

impl SpecialBuiltinUtility for Continue {
    fn exec(&self, args: &[String], shell: &mut Shell, opened_files: OpenedFiles) -> i32 {
        loop_control_flow(
            args,
            shell,
            "break",
            ControlFlowState::Continue,
            opened_files.stderr(),
        )
    }
}
