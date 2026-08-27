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
use crate::shell::Shell;
use crate::shstr::ShString;

/// Waits for `pid`, returning its status and whether it actually terminated
/// (the wait also ends when the child merely stops).
fn wait_for_pid(pid: Pid, shell: &mut Shell) -> (i32, bool) {
    // A background job may already have been reaped by the periodic sweep, in
    // which case `waitpid` gives ECHILD and the status lives only in the job
    // table -- or, for a repeated `wait`, only in the memory of jobs already
    // collected. dash and bash both keep reporting it.
    if let Some(status) = shell.background_jobs.take_collected_status(pid) {
        return (status, true);
    }
    let result = match shell.wait_child_process_result(pid) {
        Ok(result) => result,
        // No such child: never a child of this shell, or one whose status is
        // long enough past that it is no longer remembered.
        Err(err) if err.errno == Errno::ECHILD => (127, true),
        // Any other error (e.g. EINTR from a trapped signal) must not abort the
        // shell; report a non-zero status rather than panicking.
        Err(_) => (127, false),
    };
    if result.1 {
        // Reaped, so it is no longer a job; a merely stopped child is still
        // alive and must stay in the table.
        shell.background_jobs.collect(pid, result.0);
    }
    result
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
                wait_for_pid(pid, shell);
            }
        } else {
            for pid in pids {
                let pid = parse_pid(&pid.display().to_string(), shell)
                    .map_err(|err| format!("wait: {err}"))?;
                (status, _) = wait_for_pid(pid, shell);
            }
        }

        Ok(status)
    }
}
