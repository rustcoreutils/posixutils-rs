//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

pub mod job;

// Path constants for cron system files
#[cfg(target_os = "linux")]
pub const CRON_SPOOL_DIR: &str = "/var/spool/cron";
#[cfg(target_os = "macos")]
pub const CRON_SPOOL_DIR: &str = "/var/at/tabs";

pub const SYSTEM_CRONTAB: &str = "/etc/crontab";
pub const PID_FILE: &str = "/var/run/crond.pid";
pub const CRON_ALLOW: &str = "/var/cron/cron.allow";
pub const CRON_DENY: &str = "/var/cron/cron.deny";
