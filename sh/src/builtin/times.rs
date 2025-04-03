//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use nix::libc::suseconds_t;
use nix::sys::resource::{getrusage, UsageWho};

use crate::builtin::{skip_option_terminator, BuiltinResult, SpecialBuiltinUtility};
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;
fn seconds_to_minutes(seconds: f32) -> i32 {
    seconds as i32 / 60
}

fn microseconds_to_seconds(micro: suseconds_t) -> f32 {
    micro as f32 / 1000.0
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

        let shell_times = getrusage(UsageWho::RUSAGE_SELF)
            .map_err(|err| format!("times: failed to read user times ({err})"))?;
        let children_times = getrusage(UsageWho::RUSAGE_CHILDREN)
            .map_err(|err| format!("times: failed to read children times ({err})"))?;

        let shell_user_s = microseconds_to_seconds(shell_times.user_time().tv_usec());
        let shell_system_s = microseconds_to_seconds(shell_times.system_time().tv_usec());
        let children_user_s = microseconds_to_seconds(children_times.user_time().tv_usec());
        let children_system_s = microseconds_to_seconds(children_times.system_time().tv_usec());

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
