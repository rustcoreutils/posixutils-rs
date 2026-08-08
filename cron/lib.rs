//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

pub mod job;
pub mod spool;
pub mod trust;

// Path constants for cron system files
#[cfg(target_os = "linux")]
pub const CRON_SPOOL_DIR: &str = "/var/spool/cron";
#[cfg(target_os = "macos")]
pub const CRON_SPOOL_DIR: &str = "/var/at/tabs";

pub const SYSTEM_CRONTAB: &str = "/etc/crontab";
pub const PID_FILE: &str = "/var/run/crond.pid";
pub const CRON_ALLOW: &str = "/var/cron/cron.allow";
pub const CRON_DENY: &str = "/var/cron/cron.deny";

/// Resolve an allow/deny file pair, honoring environment overrides.
///
/// The defaults are the implementation-defined locations. The overrides are
/// honored **only when the process carries no elevated privilege** — real and
/// effective uid *and gid* all equal — so an `at` or `crontab` installed
/// set-uid or set-gid cannot be tricked into reading attacker-chosen
/// allow/deny files. The group half is the one that matters in practice:
/// `crontab` is canonically installed set-gid `crontab`, and a uid-only check
/// would wave those overrides straight through.
///
/// The stake is higher here than the usual caution: with neither file present
/// the fallback is "privileged users only", so a redirected pair pointing at a
/// missing allow file and an empty deny file flips that default from root-only
/// to everyone.
pub fn allow_deny_paths(
    allow_var: &str,
    allow_default: &str,
    deny_var: &str,
    deny_default: &str,
) -> (String, String) {
    let overridable = plib::curuser::real_and_effective_ids_match();
    let pick = |var: &str, default: &str| {
        if overridable {
            std::env::var(var).unwrap_or_else(|_| default.to_string())
        } else {
            default.to_string()
        }
    };
    (pick(allow_var, allow_default), pick(deny_var, deny_default))
}
