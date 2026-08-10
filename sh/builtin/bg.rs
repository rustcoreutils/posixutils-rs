//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::{skip_option_terminator, BuiltinResult, BuiltinUtility};
use crate::jobs::{parse_job_id, Job, JobState};
use crate::os::signals::{kill, Signal};
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;
use gettextrs::gettext;

fn run_background_job(
    arg: &str,
    job: &mut Job,
    opened_files: &mut OpenedFiles,
) -> Result<(), String> {
    match job.state {
        JobState::Stopped => {}
        // POSIX: a job that is already running in the background needs no
        // action; this is not an error.
        JobState::Running => return Ok(()),
        // A job that has finished cannot be resumed.
        JobState::Done(_) | JobState::Signaled(_) => {
            return Err(format!("bg: job {arg} has terminated\n"))
        }
    }
    kill(job.pid, Some(Signal::SigCont))
        .map_err(|err| format!("bg: failed to resume job {arg} ({err})\n"))?;
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
            return Err(gettext("bg: cannot use bg when job control is disabled").into());
        }
        // POSIX only *permits* `bg` to work in a subshell environment. The job
        // table here is a pre-fork copy that dies with the subshell, so
        // resuming a job would leave the parent shell reporting it as stopped
        // forever.
        if shell.is_subshell {
            return Err(gettext("bg: cannot use bg in a subshell environment").into());
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
                opened_files.write_err(gettext("bg: no background jobs\n"));
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
                            opened_files
                                .write_err(format!("bg: '{arg}' {}\n", gettext("no such job")));
                            status = 1;
                        }
                    }
                    Err(_) => {
                        opened_files.write_err(format!("bg: '{arg}' {}\n", gettext("no such job")));
                        status = 1
                    }
                }
            }
        }

        Ok(status)
    }
}
