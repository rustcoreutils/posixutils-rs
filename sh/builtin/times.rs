//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::{skip_option_terminator, BuiltinResult, SpecialBuiltinUtility};
use crate::os::errno::get_current_errno_value;
use crate::os::LibcResult;
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;

fn seconds_to_minutes(seconds: f32) -> i32 {
    seconds as i32 / 60
}

fn time_in_seconds(time: libc::timeval) -> f32 {
    time.tv_sec as f32 + time.tv_usec as f32 / 1000.0
}

fn getusage(who: libc::c_int) -> LibcResult<libc::rusage> {
    let mut usage = unsafe { std::mem::zeroed::<libc::rusage>() };
    let result = unsafe { libc::getrusage(who, &mut usage) };
    if result < 0 {
        return Err(get_current_errno_value());
    }
    Ok(usage)
}

pub struct Times;

impl SpecialBuiltinUtility for Times {
    fn exec(
        &self,
        args: &[String],
        _: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        let args = skip_option_terminator(args);
        if !args.is_empty() {
            return Err("times: too many arguments".into());
        }

        let shell_times = getusage(libc::RUSAGE_SELF)
            .map_err(|err| format!("times: failed to read user times ({err})"))?;
        let children_times = getusage(libc::RUSAGE_CHILDREN)
            .map_err(|err| format!("times: failed to read children times ({err})"))?;

        let shell_user_s = time_in_seconds(shell_times.ru_utime);
        let shell_system_s = time_in_seconds(shell_times.ru_stime);
        let children_user_s = time_in_seconds(children_times.ru_utime);
        let children_system_s = time_in_seconds(children_times.ru_stime);

        opened_files.write_out(format!(
            "{}m{}s {}m{}s\n{}m{}s {}m{}s\n",
            seconds_to_minutes(shell_user_s),
            shell_user_s,
            seconds_to_minutes(shell_system_s),
            shell_system_s,
            seconds_to_minutes(children_user_s),
            children_user_s,
            seconds_to_minutes(children_system_s),
            children_system_s
        ));
        Ok(0)
    }
}
