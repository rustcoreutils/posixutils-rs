//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::{parse_pid, skip_option_terminator, BuiltinResult, BuiltinUtility};
use crate::jobs::JobState;
use crate::os::errno::Errno;
use crate::os::Pid;
use crate::shell::opened_files::OpenedFiles;
use crate::shell::{ChildWait, Shell};
use crate::shstr::ShString;

/// Waits for `pid`, returning its status, or `Err` with the status to return
/// at once when a trapped signal ended the wait (XCU 2.11).
fn wait_for_pid(pid: Pid, shell: &mut Shell) -> Result<i32, i32> {
    // A background job may already have been reaped by the periodic sweep, in
    // which case `waitpid` gives ECHILD and the status lives only in the job
    // table -- or, for a repeated `wait`, only in the memory of jobs already
    // collected. dash and bash both keep reporting it.
    if let Some(status) = shell.background_jobs.take_collected_status(pid) {
        return Ok(status);
    }
    if shell.background_jobs.job_with_process(pid).is_some() {
        return wait_for_job_of(pid, shell);
    }
    match shell.wait_child_process_interruptibly(pid) {
        Ok(ChildWait::Terminated(status)) => {
            shell.background_jobs.collect(pid, status);
            Ok(status)
        }
        Ok(ChildWait::Stopped(status)) => Ok(status),
        Ok(ChildWait::Interrupted(status)) => Err(status),
        // No such child: never a child of this shell, or one whose status is
        // long enough past that it is no longer remembered.
        Err(err) if err.errno == Errno::ECHILD => {
            shell.background_jobs.collect(pid, 127);
            Ok(127)
        }
        // Any other error must not abort the shell; report a non-zero status
        // rather than panicking.
        Err(_) => Ok(127),
    }
}

/// Waits for every process of the job that has process `pid`, as dash does:
/// the status is the job's, and a job that stops ends the wait but stays
/// in the table.
fn wait_for_job_of(pid: Pid, shell: &mut Shell) -> Result<i32, i32> {
    loop {
        let Some(job) = shell.background_jobs.job_with_process(pid) else {
            return Ok(127);
        };
        let state = job.state();
        if state.is_terminated() {
            let status = state.exit_status();
            // Reaped, so it is no longer a job.
            shell.background_jobs.collect(pid, status);
            return Ok(status);
        }
        // A stopped process has already reported its stop, so waiting for
        // it would block until something resumed it.
        if let JobState::Stopped(_) = state {
            return Ok(state.exit_status());
        }
        let Some(next) = job.live_pids().first().copied() else {
            return Ok(state.exit_status());
        };
        // Each status is recorded in the job table as it is collected.
        match shell.wait_child_process_interruptibly(next) {
            Ok(ChildWait::Terminated(_)) => {}
            // A merely stopped job is still alive and stays in the table.
            Ok(ChildWait::Stopped(status)) => return Ok(status),
            Ok(ChildWait::Interrupted(status)) => return Err(status),
            Err(err) if err.errno == Errno::ECHILD => {
                shell.background_jobs.record_lost(next, 127);
            }
            Err(_) => return Ok(127),
        }
    }
}

pub struct Wait;

impl BuiltinUtility for Wait {
    fn exec(&self, args: &[ShString], shell: &mut Shell, _: &mut OpenedFiles) -> BuiltinResult {
        let pids = skip_option_terminator(args);

        let mut status = 0;
        if pids.is_empty() {
            // Not a drain: each job goes through the same path as an explicit
            // `wait`, so a job that has already terminated leaves the table
            // with its real status rather than a fabricated one.
            let pids = shell
                .background_jobs
                .iter()
                .filter_map(|job| job.processes.first().map(|process| process.pid))
                .collect::<Vec<_>>();
            for pid in pids {
                if let Err(interrupted) = wait_for_pid(pid, shell) {
                    return Ok(interrupted);
                }
            }
        } else {
            for pid in pids {
                let pid = parse_pid(&pid.display().to_string(), shell)
                    .map_err(|err| format!("wait: {err}"))?;
                status = match wait_for_pid(pid, shell) {
                    Ok(status) => status,
                    Err(interrupted) => return Ok(interrupted),
                };
            }
        }

        Ok(status)
    }
}
