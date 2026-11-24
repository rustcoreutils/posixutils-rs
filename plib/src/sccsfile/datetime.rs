//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The date and time of a delta, and the login name recorded with it.

use std::fmt;

use super::{Login, SccsError, SccsResult};

/// SCCS date/time format
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct SccsDateTime {
    pub year: u16, // 2-digit year
    pub month: u8,
    pub day: u8,
    pub hour: u8,
    pub minute: u8,
    pub second: u8,
}

/// The login name of the real user, which SCCS records as the author of a
/// delta and the holder of an edit.
///
/// It is the name `getpwuid(getuid())` gives (the real uid), as in
/// historical SCCS and CSSC. Only when that lookup finds nothing does the
/// `LOGNAME` or `USER` environment stand in, and `unknown` when neither is
/// set. The environment is the caller's to set to anything, so what comes
/// from it is checked like any other login: a name with a blank or a newline
/// in it would shift the fields of the delta's line or cut a p-file line in
/// two, and is refused.
pub fn real_login_name() -> SccsResult<Login> {
    let uid = unsafe { libc::getuid() };
    let passwd = crate::user::get_by_uid(uid).map(|user| user.name.into_encoded_bytes());
    let env = std::env::var_os("LOGNAME").or_else(|| std::env::var_os("USER"));
    login_from(
        passwd.as_deref(),
        env.as_ref().map(|name| name.as_encoded_bytes()),
    )
}

/// [`real_login_name`], given what the lookups found.
pub(super) fn login_from(passwd: Option<&[u8]>, env: Option<&[u8]>) -> SccsResult<Login> {
    match (passwd.filter(|name| !name.is_empty()), env) {
        (Some(name), _) | (None, Some(name)) => Login::new(name),
        (None, None) => Login::new(b"unknown"),
    }
}

impl SccsDateTime {
    /// Create current date/time in the local timezone (honoring `TZ`).
    pub fn now() -> Self {
        use std::time::{SystemTime, UNIX_EPOCH};
        let secs = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs() as libc::time_t;

        // SCCS records local wall-clock time; `localtime_r` consults `TZ`
        // (it behaves as if `tzset()` had been called).
        unsafe {
            let mut tm: libc::tm = std::mem::zeroed();
            if !libc::localtime_r(&secs, &mut tm).is_null() {
                return Self {
                    year: ((tm.tm_year + 1900) % 100) as u16,
                    month: (tm.tm_mon + 1) as u8,
                    day: tm.tm_mday as u8,
                    hour: tm.tm_hour as u8,
                    minute: tm.tm_min as u8,
                    second: tm.tm_sec as u8,
                };
            }
        }

        // Fallback: UTC if localtime_r is unavailable.
        Self::from_unix_timestamp(secs as i64)
    }

    /// Create from Unix timestamp
    pub fn from_unix_timestamp(secs: i64) -> Self {
        // Simple implementation - for a full implementation, use chrono
        let days = secs / 86400;
        let time_of_day = secs % 86400;

        let hour = (time_of_day / 3600) as u8;
        let minute = ((time_of_day % 3600) / 60) as u8;
        let second = (time_of_day % 60) as u8;

        // Simplified date calculation (doesn't handle leap years perfectly)
        let mut year = 1970i32;
        let mut remaining_days = days;

        while remaining_days >= 365 {
            let days_in_year = if year % 4 == 0 && (year % 100 != 0 || year % 400 == 0) {
                366
            } else {
                365
            };
            if remaining_days >= days_in_year {
                remaining_days -= days_in_year;
                year += 1;
            } else {
                break;
            }
        }

        let days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
        let mut month = 1u8;
        for (i, &days_in_month) in days_in_months.iter().enumerate() {
            let dim = if i == 1 && year % 4 == 0 && (year % 100 != 0 || year % 400 == 0) {
                29
            } else {
                days_in_month
            };
            if remaining_days >= dim as i64 {
                remaining_days -= dim as i64;
                month += 1;
            } else {
                break;
            }
        }

        let day = remaining_days as u8 + 1;

        Self {
            year: (year % 100) as u16,
            month,
            day,
            hour,
            minute,
            second,
        }
    }

    /// Parse from "YY/MM/DD" and "HH:MM:SS" strings
    pub fn parse(date: &str, time: &str) -> SccsResult<Self> {
        let date_parts: Vec<&str> = date.split('/').collect();
        let time_parts: Vec<&str> = time.split(':').collect();

        if date_parts.len() != 3 || time_parts.len() != 3 {
            return Err(SccsError::InvalidDateTime(format!("{} {}", date, time)));
        }

        Ok(Self {
            year: date_parts[0]
                .parse()
                .map_err(|_| SccsError::InvalidDateTime(date.to_string()))?,
            month: date_parts[1]
                .parse()
                .map_err(|_| SccsError::InvalidDateTime(date.to_string()))?,
            day: date_parts[2]
                .parse()
                .map_err(|_| SccsError::InvalidDateTime(date.to_string()))?,
            hour: time_parts[0]
                .parse()
                .map_err(|_| SccsError::InvalidDateTime(time.to_string()))?,
            minute: time_parts[1]
                .parse()
                .map_err(|_| SccsError::InvalidDateTime(time.to_string()))?,
            second: time_parts[2]
                .parse()
                .map_err(|_| SccsError::InvalidDateTime(time.to_string()))?,
        })
    }

    /// Format as date string "YY/MM/DD"
    pub fn date_string(&self) -> String {
        format!("{:02}/{:02}/{:02}", self.year, self.month, self.day)
    }

    /// Format as time string "HH:MM:SS"
    pub fn time_string(&self) -> String {
        format!("{:02}:{:02}:{:02}", self.hour, self.minute, self.second)
    }
}

impl fmt::Display for SccsDateTime {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.date_string(), self.time_string())
    }
}
