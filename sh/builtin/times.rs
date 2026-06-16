//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
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

/// Formats a CPU time as POSIX `%dm%fs` — integer minutes and the remaining
/// seconds with three fractional digits, e.g. `1m5.250s`.
fn format_time(time: libc::timeval) -> String {
    let total = time.tv_sec as f64 + time.tv_usec as f64 / 1_000_000.0;
    let minutes = (total / 60.0) as i64;
    let seconds = total - (minutes as f64) * 60.0;
    format!("{minutes}m{seconds:.3}s")
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

        opened_files.write_out(format!(
            "{} {}\n{} {}\n",
            format_time(shell_times.ru_utime),
            format_time(shell_times.ru_stime),
            format_time(children_times.ru_utime),
            format_time(children_times.ru_stime),
        ));
        Ok(0)
    }
}
