use crate::builtin::{parse_pid, skip_option_terminator, BuiltinResult, BuiltinUtility};
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;
use nix::errno::Errno;
use nix::unistd::Pid;

fn wait_for_pid(pid: Pid, shell: &mut Shell) -> i32 {
    match shell.wait_child_process(pid) {
        Ok(exit_status) => exit_status,
        Err(err) if err.errno == Errno::ECHILD => 127,
        _ => unreachable!(),
    }
}

pub struct Wait;

impl BuiltinUtility for Wait {
    fn exec(&self, args: &[String], shell: &mut Shell, _: &mut OpenedFiles) -> BuiltinResult {
        let pids = skip_option_terminator(args);

        let mut status = 0;
        if pids.is_empty() {
            let mut jobs = Vec::default();
            std::mem::swap(&mut shell.background_jobs, &mut jobs);
            for job in jobs {
                wait_for_pid(job.pid, shell);
            }
        } else {
            for pid in pids {
                let pid = parse_pid(pid, shell).map_err(|err| format!("wait: {err}"))?;
                status = wait_for_pid(pid, shell);
            }
        }

        Ok(status)
    }
}
