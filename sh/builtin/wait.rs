//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::{parse_pid, skip_option_terminator, BuiltinResult, BuiltinUtility};
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
    match shell.wait_child_process_interruptibly(pid) {
        Ok(ChildWait::Terminated(status)) => {
            // Reaped, so it is no longer a job.
            shell.background_jobs.collect(pid, status);
            Ok(status)
        }
        // A merely stopped child is still alive and stays in the table.
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

pub struct Wait;

impl BuiltinUtility for Wait {
    fn exec(&self, args: &[ShString], shell: &mut Shell, _: &mut OpenedFiles) -> BuiltinResult {
        let pids = skip_option_terminator(args);

        let mut status = 0;
        if pids.is_empty() {
            // Not a drain: each pid goes through the same path as an explicit
            // `wait`, so a job that has already terminated leaves the table
            // with its real status rather than a fabricated one.
            let pids = shell
                .background_jobs
                .iter()
                .map(|job| job.pid)
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
