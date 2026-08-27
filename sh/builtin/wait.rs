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
    match shell.wait_child_process_result(pid) {
        Ok(result) => result,
        // No such child (already reaped or never existed).
        Err(err) if err.errno == Errno::ECHILD => (127, true),
        // Any other error (e.g. EINTR from a trapped signal) must not abort the
        // shell; report a non-zero status rather than panicking.
        Err(_) => (127, false),
    }
}

pub struct Wait;

impl BuiltinUtility for Wait {
    fn exec(&self, args: &[ShString], shell: &mut Shell, _: &mut OpenedFiles) -> BuiltinResult {
        let pids = skip_option_terminator(args);

        let mut status = 0;
        if pids.is_empty() {
            for job in shell.background_jobs.drain() {
                wait_for_pid(job.pid, shell);
            }
        } else {
            for pid in pids {
                let pid = parse_pid(&pid.display().to_string(), shell)
                    .map_err(|err| format!("wait: {err}"))?;
                let terminated;
                (status, terminated) = wait_for_pid(pid, shell);
                if terminated {
                    // reaped, so it is no longer a known job; a merely stopped
                    // child is still alive and must stay in the table
                    shell.background_jobs.remove_job_by_pid(pid);
                }
            }
        }

        Ok(status)
    }
}
