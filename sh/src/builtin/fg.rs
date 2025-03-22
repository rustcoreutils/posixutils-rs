use crate::builtin::{skip_option_terminator, BuiltinResult, BuiltinUtility};
use crate::jobs::{parse_job_id, Job, JobId, JobState};
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;
use nix::sys::signal::kill;
use nix::unistd::Pid;
use std::io;
use std::os::fd::AsFd;

fn run_foreground_job(
    shell: &mut Shell,
    opened_files: &mut OpenedFiles,
    arg: &str,
    job: &Job,
) -> Result<(), String> {
    if let JobState::Done(_) = job.state {
        return Err(format!("fg: job {arg} already terminated"));
    }
    if job.state == JobState::Stopped {
        kill(job.pid, nix::sys::signal::SIGCONT)
            .map_err(|err| format!("fg: failed to resume {arg} ({err})"))?;
    }
    opened_files.write_out(&format!("{}\n", job.command));
    nix::unistd::tcsetpgrp(io::stdin().as_fd(), job.pid).unwrap();
    shell
        .wait_child_process(job.pid)
        .map_err(|err| err.to_string())?;
    nix::unistd::tcsetpgrp(io::stdin().as_fd(), Pid::from_raw(shell.shell_pid)).unwrap();
    Ok(())
}
pub struct Fg;

impl BuiltinUtility for Fg {
    fn exec(
        &self,
        args: &[String],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        if !shell.set_options.monitor {
            return Err("fg: job control is disabled".into());
        }
        if !shell.is_interactive {
            return Err("fg: shell is not interactive".into());
        }

        let mut status = 0;
        let args = skip_option_terminator(args);
        if args.is_empty() {
            if let Some(job) = shell.background_jobs.remove_job(JobId::CurrentJob) {
                if let Err(err) = run_foreground_job(shell, opened_files, "current", &job) {
                    opened_files.write_err(err);
                    status = 1;
                }
            } else {
                opened_files.write_err("bg: no background jobs".to_string());
                status = 1;
            }
        } else {
            for arg in args {
                match parse_job_id(arg) {
                    Ok(job_id) => {
                        if let Some(job) = shell.background_jobs.remove_job(job_id) {
                            if let Err(err) = run_foreground_job(shell, opened_files, arg, &job) {
                                opened_files.write_err(err);
                                status = 1;
                            }
                        } else {
                            opened_files.write_err(format!("fg: '{arg}' no such job"));
                            status = 1;
                        }
                    }
                    Err(_) => {
                        opened_files.write_err(format!("fg: '{arg}' no such job"));
                        status = 1
                    }
                }
            }
        }

        Ok(status)
    }
}
