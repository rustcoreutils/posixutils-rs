use crate::builtin::{skip_option_terminator, BuiltinError, BuiltinResult, BuiltinUtility};
use crate::jobs::{parse_job_id, Job, JobState};
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;
use nix::sys::signal::kill;

fn run_background_job(
    arg: &str,
    job: &mut Job,
    opened_files: &mut OpenedFiles,
) -> Result<(), String> {
    if job.state != JobState::Stopped {
        return Err(format!(
            "bg: job {arg} is already running in the background"
        ));
    }
    kill(job.pid, nix::sys::signal::SIGCONT)
        .map_err(|err| format!("bg: failed to resume job {arg} ({err})"))?;
    opened_files.write_out(format!("[{}] {}\n", job.number, job.command));
    job.state = JobState::Running;
    Ok(())
}
pub struct Bg;

impl BuiltinUtility for Bg {
    fn exec(
        &self,
        args: &[String],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        if !shell.set_options.monitor {
            return Err("bg: cannot use bg when job control is disabled".into());
        }
        if !shell.is_interactive {
            return Err("bg: cannot use bg in a non-interactive shell".into());
        }
        if shell.is_subshell {
            return Err("bg: cannot use bg in a subshell environment".into());
        }

        let mut status = 0;
        let args = skip_option_terminator(args);
        if args.is_empty() {
            if let Some(job) = shell.background_jobs.current_mut() {
                if let Err(err) = run_background_job("current", job, opened_files) {
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
                        if let Some(job) = shell.background_jobs.get_job_mut(job_id) {
                            if let Err(err) = run_background_job(arg, job, opened_files) {
                                opened_files.write_err(err);
                                status = 1;
                            }
                        } else {
                            opened_files.write_err(format!("bg: '{arg}' no such job"));
                            status = 1;
                        }
                    }
                    Err(_) => {
                        opened_files.write_err(format!("bg: '{arg}' no such job"));
                        status = 1
                    }
                }
            }
        }

        Ok(status)
    }
}
