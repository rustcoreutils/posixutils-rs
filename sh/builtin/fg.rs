//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::{args_as_str, skip_option_terminator, BuiltinResult, BuiltinUtility};
use crate::jobs::{parse_job_id, Job, JobId, JobState};
use crate::os::signals::{kill, Signal};
use crate::os::{give_terminal_to, job_control_tty};
use crate::shell::opened_files::OpenedFiles;
use crate::shell::{take_terminal_back, Shell};
use crate::shstr::ShString;
use gettextrs::gettext;

/// Resumes `job` in the foreground and waits for it. A job that stops again
/// goes back into the job table under the same job number, as in dash.
fn run_foreground_job(
    shell: &mut Shell,
    opened_files: &mut OpenedFiles,
    arg: &str,
    mut job: Job,
) -> Result<i32, String> {
    if job.state().is_terminated() {
        return Err(format!("fg: job {arg} already terminated\n"));
    }
    opened_files.write_out(format!("{}\n", job.command));
    // XCU 2.11: the job's process group becomes the terminal's foreground
    // group, before it is resumed, so that it can read the terminal at once.
    let terminal = job.process_group.zip(job_control_tty());
    if let Some((pgid, terminal)) = terminal {
        let _ = give_terminal_to(terminal, pgid);
    }
    // As in dash, the whole group is continued whatever state it was last
    // seen in: a process may have stopped since. A running job may also
    // have finished since, which is no failure: the wait collects it.
    let resumed = kill(job.signal_target(), Some(Signal::SigCont));
    if let (Err(err), JobState::Stopped(_)) = (resumed, job.state()) {
        if terminal.is_some() {
            take_terminal_back();
        }
        let message = format!("fg: failed to resume {arg} ({err})\n");
        shell.background_jobs.reinsert_job(job);
        return Err(message);
    }
    job.mark_continued();
    // The exit status of fg is the exit status of the job it waited for.
    let status = shell.wait_for_job(job).map_err(|err| format!("{err}\n"));
    if terminal.is_some() {
        take_terminal_back();
    }
    status
}
pub struct Fg;

impl BuiltinUtility for Fg {
    fn exec(
        &self,
        args: &[ShString],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        let args = &args_as_str("fg", args)?;
        if !shell.set_options.monitor {
            return Err(gettext("fg: cannot use fg when job control is disabled").into());
        }
        // POSIX only *permits* `fg` to work in a subshell environment. This
        // shell cannot: the job table is a pre-fork copy, so resuming here
        // would leave the parent believing the job is still stopped, and the
        // job is not this process's child, so waiting for it fails with
        // ECHILD.
        if shell.is_subshell {
            return Err(gettext("fg: cannot use fg in a subshell environment").into());
        }

        let mut status = 0;
        let args = skip_option_terminator(args);
        if args.is_empty() {
            if let Some(job) = shell.background_jobs.remove_job(JobId::CurrentJob) {
                match run_foreground_job(shell, opened_files, "current", job) {
                    Ok(s) => status = s,
                    Err(err) => {
                        opened_files.write_err(err);
                        status = 1;
                    }
                }
            } else {
                opened_files.write_err(gettext("fg: no background jobs\n"));
                status = 1;
            }
        } else {
            for arg in args {
                match parse_job_id(arg) {
                    Ok(job_id) => {
                        if let Some(job) = shell.background_jobs.remove_job(job_id) {
                            match run_foreground_job(shell, opened_files, arg, job) {
                                Ok(s) => status = s,
                                Err(err) => {
                                    opened_files.write_err(err);
                                    status = 1;
                                }
                            }
                        } else {
                            opened_files
                                .write_err(format!("fg: '{arg}' {}\n", gettext("no such job")));
                            status = 1;
                        }
                    }
                    Err(_) => {
                        opened_files.write_err(format!("fg: '{arg}' {}\n", gettext("no such job")));
                        status = 1
                    }
                }
            }
        }

        Ok(status)
    }
}
