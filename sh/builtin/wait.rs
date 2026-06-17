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

fn wait_for_pid(pid: Pid, shell: &mut Shell) -> i32 {
    match shell.wait_child_process(pid) {
        Ok(exit_status) => exit_status,
        // No such child (already reaped or never existed).
        Err(err) if err.errno == Errno::ECHILD => 127,
        // Any other error (e.g. EINTR from a trapped signal) must not abort the
        // shell; report a non-zero status rather than panicking.
        Err(_) => 127,
    }
}

pub struct Wait;

impl BuiltinUtility for Wait {
    fn exec(&self, args: &[String], shell: &mut Shell, _: &mut OpenedFiles) -> BuiltinResult {
        let pids = skip_option_terminator(args);

        let mut status = 0;
        if pids.is_empty() {
            for job in shell.background_jobs.drain() {
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
