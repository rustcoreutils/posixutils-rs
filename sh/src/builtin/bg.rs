use crate::builtin::{skip_option_terminator, BuiltinError, BuiltinResult, BuiltinUtility};
use crate::jobs::{parse_job_id, Job};
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;
use nix::sys::signal::kill;

fn run_background_job(arg: &str, job: &Job) -> Result<(), BuiltinError> {
    kill(job.pid, nix::sys::signal::SIGCONT)
        .map_err(|err| format!("bg: failed to resume job {arg} ({err})"))?;
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
            return Err("bg: job control is disabled".into());
        }

        let mut status = 0;
        let args = skip_option_terminator(args);
        if args.is_empty() {
            if let Some(job) = shell.background_jobs.last() {
                run_background_job("current", job)?;
            } else {
                opened_files.write_err("bg: no background jobs".to_string());
                status = 1;
            }
        } else {
            for arg in args {
                match parse_job_id(arg) {
                    Ok(job_id) => {
                        if let Some(job) = shell.get_job(job_id) {
                            run_background_job(arg, job)?;
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
