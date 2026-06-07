//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use chrono::{Datelike, Local, NaiveDate, NaiveDateTime, Timelike};
use std::collections::BTreeSet;
use std::ffi::CStr;
use std::iter::Peekable;
use std::process::{Command, Stdio};
use std::str::FromStr;

#[derive(Ord, PartialOrd, Eq, PartialEq, Copy, Clone, Debug)]
enum Value {
    Number(i32),
    Range { min: i32, max: i32, step: i32 },
}

macro_rules! time_unit {
    ($name:ident from $min:literal to $max:literal) => {
        #[derive(Ord, PartialOrd, Eq, PartialEq, Clone, Debug)]
        pub struct $name(Option<Vec<Value>>);

        impl $name {
            const fn range() -> std::ops::RangeInclusive<i32> {
                $min..=$max
            }

            fn to_vec(&self) -> Vec<i32> {
                if let Some(x) = &self.0 {
                    let mut v = x
                        .iter()
                        .map(|x| match x {
                            Value::Number(x) => vec![*x],
                            Value::Range { min, max, step } => {
                                (*min..=*max).step_by(*step as usize).collect()
                            }
                        })
                        .fold(vec![], |mut acc, x| {
                            acc.extend(x);
                            acc
                        });
                    v.sort();
                    v
                } else {
                    Vec::from_iter(Self::range())
                }
            }

            /// True if `value` falls in this field's set (a wildcard matches
            /// everything). Used by the daemon's per-minute wheel (audit #D6).
            fn matches(&self, value: i32) -> bool {
                match &self.0 {
                    None => true,
                    Some(_) => self.to_vec().contains(&value),
                }
            }

            fn parse(s: impl AsRef<str>) -> Result<Self, ()> {
                let s = s.as_ref();
                let mut v = Vec::new();
                let mut src = s.chars().peekable();

                // Handle wildcard: * or */step
                if expect(&mut src, '*') {
                    if expect(&mut src, '/') {
                        let Some(step) = get_number(&mut src) else {
                            return Err(());
                        };
                        let range = Self::range();
                        return Ok(Self(Some(vec![Value::Range {
                            min: *range.start(),
                            max: *range.end(),
                            step,
                        }])));
                    }
                    return Ok(Self(None));
                }

                loop {
                    if let None | Some(' ') = src.peek() {
                        break;
                    }

                    let Some(min) = get_number(&mut src) else {
                        return Err(());
                    };

                    if expect(&mut src, '-') {
                        // Range: min-max or min-max/step
                        let Some(max) = get_number(&mut src) else {
                            return Err(());
                        };
                        let step = if expect(&mut src, '/') {
                            let Some(step) = get_number(&mut src) else {
                                return Err(());
                            };
                            step
                        } else {
                            1
                        };
                        v.push(Value::Range { min, max, step });
                    } else {
                        // Single number
                        v.push(Value::Number(min));
                    }

                    // Handle comma-separated values
                    if !expect(&mut src, ',') {
                        break;
                    }
                }

                Ok(Self(Some(v)))
            }
        }
    };
}

time_unit!(Minute from 0 to 59);
time_unit!(Hour from 0 to 23);
time_unit!(MonthDay from 1 to 31);
time_unit!(Month from 1 to 12);
time_unit!(WeekDay from 0 to 6);

impl MonthDay {
    fn merge(self, other: Self) -> Self {
        let mut a = self.to_vec();
        let b = other.to_vec();
        a.extend(b);
        a.sort();
        a.dedup();
        Self(Some(a.into_iter().map(Value::Number).collect()))
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct CronJob {
    pub minute: Minute,
    pub hour: Hour,
    pub monthday: MonthDay,
    pub month: Month,
    pub weekday: WeekDay,
    pub command: String,
    /// True for @reboot jobs that run once at daemon startup
    pub is_reboot: bool,
    /// Owner info for privilege dropping (None if not set, e.g., in tests)
    pub owner_uid: Option<u32>,
    pub owner_gid: Option<u32>,
    pub owner_name: Option<String>,
    pub owner_home: Option<String>,
    /// `NAME=value` assignments in effect for this job, in crontab order. The
    /// daemon overlays these on the default environment (audit #D4).
    pub env: Vec<(String, String)>,
}

#[derive(Clone)]
pub struct Database(pub Vec<CronJob>);

/// User info retrieved from passwd database
#[derive(Clone, Debug)]
pub struct UserInfo {
    pub uid: u32,
    pub gid: u32,
    pub name: String,
    pub home: String,
}

impl UserInfo {
    /// Look up user info from username using getpwnam
    pub fn from_username(username: &str) -> Option<Self> {
        use std::ffi::CString;

        let c_username = CString::new(username).ok()?;

        // SAFETY: getpwnam() is thread-safe for read-only access.
        // We copy all needed data before returning.
        unsafe {
            let pwd = libc::getpwnam(c_username.as_ptr());
            if pwd.is_null() {
                return None;
            }

            let pw = &*pwd;
            let name = CStr::from_ptr(pw.pw_name).to_string_lossy().into_owned();
            let home = CStr::from_ptr(pw.pw_dir).to_string_lossy().into_owned();

            Some(UserInfo {
                uid: pw.pw_uid,
                gid: pw.pw_gid,
                name,
                home,
            })
        }
    }

    /// Look up user info from a uid using getpwuid. Used to resolve the run-as
    /// identity of an at-spool job from the file's owner (audit #X1).
    pub fn from_uid(uid: u32) -> Option<Self> {
        // SAFETY: getpwuid() is read-only; all fields are copied before return.
        unsafe {
            let pwd = libc::getpwuid(uid);
            if pwd.is_null() {
                return None;
            }
            let pw = &*pwd;
            let name = CStr::from_ptr(pw.pw_name).to_string_lossy().into_owned();
            let home = CStr::from_ptr(pw.pw_dir).to_string_lossy().into_owned();
            Some(UserInfo {
                uid: pw.pw_uid,
                gid: pw.pw_gid,
                name,
                home,
            })
        }
    }
}

impl Database {
    pub fn merge(mut self, other: Database) -> Database {
        self.0.extend(other.0);
        self
    }

    pub fn nearest_job(&self) -> Option<CronJob> {
        let now = Local::now().naive_local();
        self.0
            .iter()
            .filter(|x| x.next_execution(&now).is_some())
            .min_by_key(|x| x.next_execution(&now))
            .cloned()
    }

    /// Get all @reboot jobs
    pub fn reboot_jobs(&self) -> Vec<&CronJob> {
        self.0.iter().filter(|j| j.is_reboot).collect()
    }

    /// Parse a user crontab (5-field format) with owner info
    pub fn parse_user_crontab(content: &str, user: &UserInfo) -> Self {
        let mut db = match content.parse::<Database>() {
            Ok(d) => d,
            Err(()) => Database(vec![]),
        };

        // Set owner info on all jobs
        for job in &mut db.0 {
            job.owner_uid = Some(user.uid);
            job.owner_gid = Some(user.gid);
            job.owner_name = Some(user.name.clone());
            job.owner_home = Some(user.home.clone());
        }

        db
    }

    /// Parse a system crontab (6-field format with username)
    pub fn parse_system_crontab(content: &str) -> Self {
        let mut result = vec![];
        let mut env_vars: Vec<(String, String)> = Vec::new();

        for line in content.lines() {
            let line = line.trim();

            // Skip empty lines and comments
            if line.is_empty() || line.starts_with('#') {
                continue;
            }

            // Collect environment-variable assignments; they apply to all jobs
            // that follow in the file (audit #D4).
            if let Some((key, value)) = parse_env_assignment(line) {
                env_vars.retain(|(k, _)| k != &key);
                env_vars.push((key, value));
                continue;
            }

            let mut fields = line.split_ascii_whitespace();

            let Some(first_field) = fields.next() else {
                continue;
            };

            // Check for @-prefix special time specifications
            let (time_spec, username, command) = if first_field.starts_with('@') {
                let Some(spec) = parse_at_spec(first_field) else {
                    continue; // Unknown @-spec, skip line
                };
                // For system crontab, next field is username
                let Some(user_field) = fields.next() else {
                    continue;
                };
                let cmd: String = fields.collect::<Vec<_>>().join(" ");
                (spec, user_field.to_string(), cmd)
            } else {
                // Standard 6-field format: min hour dom mon dow user cmd
                let minutes_field = first_field;
                let Some(hours_field) = fields.next() else {
                    continue;
                };
                let Some(monthdays_field) = fields.next() else {
                    continue;
                };
                let Some(months_field) = fields.next() else {
                    continue;
                };
                let Some(weekdays_field) = fields.next() else {
                    continue;
                };
                let Some(user_field) = fields.next() else {
                    continue;
                };

                let Ok(minute) = Minute::parse(minutes_field) else {
                    continue;
                };
                let Ok(hour) = Hour::parse(hours_field) else {
                    continue;
                };
                let Ok(monthday) = MonthDay::parse(monthdays_field) else {
                    continue;
                };
                let Ok(month) = Month::parse(months_field) else {
                    continue;
                };
                let Ok(weekday) = WeekDay::parse(weekdays_field) else {
                    continue;
                };

                let spec = TimeSpec {
                    minute,
                    hour,
                    monthday,
                    month,
                    weekday,
                    is_reboot: false,
                };
                let cmd: String = fields.collect::<Vec<_>>().join(" ");
                (spec, user_field.to_string(), cmd)
            };

            if command.is_empty() {
                continue;
            }

            // Look up user info
            let user_info = UserInfo::from_username(&username);

            result.push(CronJob {
                minute: time_spec.minute,
                hour: time_spec.hour,
                monthday: time_spec.monthday,
                month: time_spec.month,
                weekday: time_spec.weekday,
                command,
                is_reboot: time_spec.is_reboot,
                owner_uid: user_info.as_ref().map(|u| u.uid),
                owner_gid: user_info.as_ref().map(|u| u.gid),
                owner_name: user_info.as_ref().map(|u| u.name.clone()),
                owner_home: user_info.map(|u| u.home),
                env: env_vars.clone(),
            });
        }

        Database(result)
    }
}

/// Recognize a crontab `NAME=value` environment assignment, returning the name
/// and its (single-quote/double-quote stripped) value.
fn parse_env_assignment(line: &str) -> Option<(String, String)> {
    let (name, value) = line.split_once('=')?;
    let name = name.trim();
    if name.is_empty()
        || !name
            .chars()
            .enumerate()
            .all(|(i, c)| c == '_' || c.is_ascii_alphabetic() || (i > 0 && c.is_ascii_digit()))
    {
        return None;
    }

    let value = value.trim();
    let value = if value.len() >= 2
        && ((value.starts_with('"') && value.ends_with('"'))
            || (value.starts_with('\'') && value.ends_with('\'')))
    {
        &value[1..value.len() - 1]
    } else {
        value
    };

    Some((name.to_string(), value.to_string()))
}

/// Represents time specification parsed from @-prefix or 5-field format
struct TimeSpec {
    minute: Minute,
    hour: Hour,
    monthday: MonthDay,
    month: Month,
    weekday: WeekDay,
    is_reboot: bool,
}

/// Parse @-prefix special time specifications
fn parse_at_spec(spec: &str) -> Option<TimeSpec> {
    match spec.to_lowercase().as_str() {
        "@reboot" => Some(TimeSpec {
            minute: Minute(None),
            hour: Hour(None),
            monthday: MonthDay(None),
            month: Month(None),
            weekday: WeekDay(None),
            is_reboot: true,
        }),
        "@yearly" | "@annually" => Some(TimeSpec {
            // 0 0 1 1 * - Jan 1, midnight
            minute: Minute(Some(vec![Value::Number(0)])),
            hour: Hour(Some(vec![Value::Number(0)])),
            monthday: MonthDay(Some(vec![Value::Number(1)])),
            month: Month(Some(vec![Value::Number(1)])),
            weekday: WeekDay(None),
            is_reboot: false,
        }),
        "@monthly" => Some(TimeSpec {
            // 0 0 1 * * - 1st of month, midnight
            minute: Minute(Some(vec![Value::Number(0)])),
            hour: Hour(Some(vec![Value::Number(0)])),
            monthday: MonthDay(Some(vec![Value::Number(1)])),
            month: Month(None),
            weekday: WeekDay(None),
            is_reboot: false,
        }),
        "@weekly" => Some(TimeSpec {
            // 0 0 * * 0 - Sunday, midnight
            minute: Minute(Some(vec![Value::Number(0)])),
            hour: Hour(Some(vec![Value::Number(0)])),
            monthday: MonthDay(None),
            month: Month(None),
            weekday: WeekDay(Some(vec![Value::Number(0)])),
            is_reboot: false,
        }),
        "@daily" | "@midnight" => Some(TimeSpec {
            // 0 0 * * * - Every midnight
            minute: Minute(Some(vec![Value::Number(0)])),
            hour: Hour(Some(vec![Value::Number(0)])),
            monthday: MonthDay(None),
            month: Month(None),
            weekday: WeekDay(None),
            is_reboot: false,
        }),
        "@hourly" => Some(TimeSpec {
            // 0 * * * * - Top of every hour
            minute: Minute(Some(vec![Value::Number(0)])),
            hour: Hour(None),
            monthday: MonthDay(None),
            month: Month(None),
            weekday: WeekDay(None),
            is_reboot: false,
        }),
        _ => None,
    }
}

impl FromStr for Database {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut result = vec![];
        let mut env_vars: Vec<(String, String)> = Vec::new();

        for line in s.lines() {
            let line = line.trim();

            // Skip empty lines and comments
            if line.is_empty() || line.starts_with('#') {
                continue;
            }

            // Collect environment-variable assignments; they apply to all jobs
            // that follow in the file (audit #D4).
            if let Some((key, value)) = parse_env_assignment(line) {
                env_vars.retain(|(k, _)| k != &key);
                env_vars.push((key, value));
                continue;
            }

            let mut fields = line.split_ascii_whitespace();

            let Some(first_field) = fields.next() else {
                continue;
            };

            // Check for @-prefix special time specifications
            let (time_spec, command) = if first_field.starts_with('@') {
                let Some(spec) = parse_at_spec(first_field) else {
                    continue; // Unknown @-spec, skip line
                };
                let cmd: String = fields.collect::<Vec<_>>().join(" ");
                (spec, cmd)
            } else {
                // Standard 5-field format
                let minutes_field = first_field;
                let Some(hours_field) = fields.next() else {
                    continue;
                };
                let Some(monthdays_field) = fields.next() else {
                    continue;
                };
                let Some(months_field) = fields.next() else {
                    continue;
                };
                let Some(weekdays_field) = fields.next() else {
                    continue;
                };

                let Ok(minute) = Minute::parse(minutes_field) else {
                    return Err(());
                };
                let Ok(hour) = Hour::parse(hours_field) else {
                    return Err(());
                };
                let Ok(monthday) = MonthDay::parse(monthdays_field) else {
                    return Err(());
                };
                let Ok(month) = Month::parse(months_field) else {
                    return Err(());
                };
                let Ok(weekday) = WeekDay::parse(weekdays_field) else {
                    return Err(());
                };

                let spec = TimeSpec {
                    minute,
                    hour,
                    monthday,
                    month,
                    weekday,
                    is_reboot: false,
                };
                let cmd: String = fields.collect::<Vec<_>>().join(" ");
                (spec, cmd)
            };

            if command.is_empty() {
                continue;
            }

            result.push(CronJob {
                minute: time_spec.minute,
                hour: time_spec.hour,
                monthday: time_spec.monthday,
                month: time_spec.month,
                weekday: time_spec.weekday,
                command,
                is_reboot: time_spec.is_reboot,
                owner_uid: None,
                owner_gid: None,
                owner_name: None,
                owner_home: None,
                env: env_vars.clone(),
            })
        }

        Ok(Self(result))
    }
}

/// Validate a user crontab (5-field format) the way the daemon parses it.
///
/// Returns the 1-based line number of the first entry the daemon would reject
/// (a time field that fails to parse, or an unknown `@`-spec). Lines the daemon
/// silently ignores — blank, comment, or structurally short — are not flagged,
/// so "valid" here means exactly "the daemon will load this crontab". Used by
/// `crontab` to refuse installing a crontab the daemon would choke on (#C4).
pub fn validate_user_crontab(content: &str) -> Result<(), usize> {
    for (idx, raw) in content.lines().enumerate() {
        let line = raw.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }

        let mut fields = line.split_ascii_whitespace();
        let Some(first_field) = fields.next() else {
            continue;
        };

        if first_field.starts_with('@') {
            if parse_at_spec(first_field).is_none() {
                return Err(idx + 1);
            }
            continue;
        }

        // Standard 5-field format. Too few fields ⇒ the daemon skips the line,
        // so we do not flag it; only a malformed time field is a hard error.
        let (Some(hours), Some(mdays), Some(months), Some(wdays)) =
            (fields.next(), fields.next(), fields.next(), fields.next())
        else {
            continue;
        };

        if Minute::parse(first_field).is_err()
            || Hour::parse(hours).is_err()
            || MonthDay::parse(mdays).is_err()
            || Month::parse(months).is_err()
            || WeekDay::parse(wdays).is_err()
        {
            return Err(idx + 1);
        }
    }

    Ok(())
}

impl CronJob {
    /// True if this recurring job is scheduled to fire during the minute `t`
    /// (truncated to minute resolution by the caller). Mirrors the POSIX
    /// month / day-of-month / day-of-week union rule (audit #D6).
    pub fn matches_minute(&self, t: &NaiveDateTime) -> bool {
        if self.is_reboot {
            return false;
        }
        if !self.minute.matches(t.minute() as i32)
            || !self.hour.matches(t.hour() as i32)
            || !self.month.matches(t.month() as i32)
        {
            return false;
        }

        let dom = t.day() as i32;
        let dow = t.weekday().num_days_from_sunday() as i32;
        match (self.monthday.0.is_some(), self.weekday.0.is_some()) {
            (true, true) => self.monthday.matches(dom) || self.weekday.matches(dow),
            (false, true) => self.weekday.matches(dow),
            (true, false) => self.monthday.matches(dom),
            (false, false) => true,
        }
    }

    pub fn next_execution(&self, now: &NaiveDateTime) -> Option<NaiveDateTime> {
        // @reboot jobs don't have scheduled executions
        if self.is_reboot {
            return None;
        }

        let Self {
            minute: minutes,
            hour: hours,
            month: months,
            monthday: monthdays,
            weekday: weekdays,
            command: _,
            is_reboot: _,
            owner_uid: _,
            owner_gid: _,
            owner_name: _,
            owner_home: _,
            env: _,
        } = self;

        let months_vec = months.to_vec();
        let hours_vec = hours.to_vec();
        let minutes_vec = minutes.to_vec();

        let mut candidates = Vec::new();

        // Try current year and next year for year rollover
        for year_offset in 0..=1 {
            let check_year = now.year() + year_offset;

            for month in &months_vec {
                // Convert weekdays to monthdays for this specific month/year
                let date_for_conversion = NaiveDate::from_ymd_opt(check_year, *month as u32, 1)?;
                let monthdays_for_month = match (monthdays.clone().0, weekdays.clone().0) {
                    (Some(_), Some(_)) => monthdays.clone().merge(convert_weekdays_to_monthdays(
                        date_for_conversion,
                        weekdays.clone(),
                    )),
                    (None, Some(_)) => {
                        convert_weekdays_to_monthdays(date_for_conversion, weekdays.clone())
                    }
                    (Some(_), None) => monthdays.clone(),
                    (None, None) => MonthDay(None),
                };

                for monthday in &monthdays_for_month.to_vec() {
                    for hour in &hours_vec {
                        for minute in &minutes_vec {
                            // Try to construct a valid date
                            let Some(date) = NaiveDate::from_ymd_opt(
                                check_year,
                                *month as u32,
                                *monthday as u32,
                            ) else {
                                continue; // Invalid date (e.g., Feb 30)
                            };
                            let Some(time) =
                                chrono::NaiveTime::from_hms_opt(*hour as u32, *minute as u32, 0)
                            else {
                                continue;
                            };
                            let next_exec = NaiveDateTime::new(date, time);

                            if next_exec > *now {
                                candidates.push(next_exec);
                            }
                        }
                    }
                }
            }
        }

        // Return the earliest candidate
        candidates.into_iter().min()
    }

    pub fn run_job(&self) -> std::io::Result<()> {
        // SAFETY: fork() is checked; the parent returns immediately, and the
        // child performs only operations safe in this single-threaded daemon.
        let pid = unsafe { libc::fork() };
        if pid < 0 {
            return Err(std::io::Error::last_os_error());
        }
        if pid != 0 {
            // Parent (daemon) returns immediately; the child is reaped by SIGCHLD.
            return Ok(());
        }

        // Child: start a new session so the job has no controlling terminal and
        // runs in its own process group (audit #D7).
        // SAFETY: setsid() in a fresh child is always safe.
        unsafe {
            libc::setsid();
        }

        // Drop privileges to the job owner, if known.
        if let (Some(uid), Some(gid), Some(name)) =
            (self.owner_uid, self.owner_gid, self.owner_name.as_deref())
        {
            // SAFETY: called in the forked child before exec.
            if let Err(e) = unsafe { drop_privileges(uid, gid, name) } {
                eprintln!("crond: cannot drop privileges for {name}: {e}");
                std::process::exit(1);
            }
        }

        // Working directory: the owner's home, falling back to / (audit #D13).
        let home = self.owner_home.as_deref().unwrap_or("/");
        if std::env::set_current_dir(home).is_err() {
            let _ = std::env::set_current_dir("/");
        }

        // Build the clean default environment overlaid by the crontab's own
        // assignments (audit #D4).
        let owner_name = self.owner_name.as_deref().unwrap_or("");
        let env = build_job_env(owner_name, home, &self.env);
        let shell = env
            .iter()
            .find(|(k, _)| k == "SHELL")
            .map(|(_, v)| v.clone())
            .unwrap_or_else(|| "/bin/sh".to_string());

        // Apply the crontab `%`/standard-input command-field convention (#D5).
        let parsed = parse_command_field(&self.command);

        let mut command = Command::new(&shell);
        command
            .arg("-c")
            .arg(&parsed.exec_line)
            .env_clear()
            .envs(env.iter().cloned())
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());

        let mut child = match command.spawn() {
            Ok(c) => c,
            Err(e) => {
                eprintln!("crond: cannot run job: {e}");
                std::process::exit(1);
            }
        };

        // Feed the standard-input text (everything after the first `%`).
        if let Some(mut stdin) = child.stdin.take() {
            use std::io::Write;
            if let Some(data) = &parsed.stdin {
                let _ = stdin.write_all(data.as_bytes());
            }
            // Dropping `stdin` closes it, signalling EOF.
        }

        let output = match child.wait_with_output() {
            Ok(o) => o,
            Err(_) => std::process::exit(1),
        };

        // Mail the combined output to the user (or MAILTO), unless it is empty or
        // MAILTO is set to the empty string (audit #D8).
        let recipient = match self.env.iter().find(|(k, _)| k == "MAILTO") {
            Some((_, addr)) if addr.is_empty() => None,
            Some((_, addr)) => Some(addr.clone()),
            None => self.owner_name.clone(),
        };
        let mut combined = output.stdout;
        combined.extend_from_slice(&output.stderr);
        if !combined.is_empty() {
            if let Some(rcpt) = recipient {
                let subject = format!("Cron <{owner_name}> {}", parsed.exec_line);
                mail_output(&rcpt, &subject, &combined);
            }
        }

        std::process::exit(output.status.code().unwrap_or(0));
    }
}

/// The first executable line of a crontab command field, plus any standard input
/// text that followed an unescaped `%`.
pub struct ParsedCommand {
    pub exec_line: String,
    pub stdin: Option<String>,
}

/// Apply the crontab command-field conventions (audit #D5): an unescaped `%`
/// ends the command and begins standard-input text; each subsequent unescaped
/// `%` becomes a newline; `\` quotes the following character (including `%`).
pub fn parse_command_field(raw: &str) -> ParsedCommand {
    let mut exec_line = String::new();
    let mut chars = raw.chars();
    let mut stdin = None;

    while let Some(c) = chars.next() {
        match c {
            '\\' => exec_line.push(chars.next().unwrap_or('\\')),
            '%' => {
                let mut data = String::new();
                let mut escaped = false;
                for d in chars.by_ref() {
                    if escaped {
                        data.push(d);
                        escaped = false;
                    } else if d == '\\' {
                        escaped = true;
                    } else if d == '%' {
                        data.push('\n');
                    } else {
                        data.push(d);
                    }
                }
                stdin = Some(data);
                break;
            }
            _ => exec_line.push(c),
        }
    }

    ParsedCommand { exec_line, stdin }
}

/// Build the clean default environment for an executed crontab job, overlaid by
/// the crontab's own `NAME=value` assignments (audit #D4). The authoritative
/// `LOGNAME`/`USER` and the routing-only `MAILTO` cannot be overridden.
pub fn build_job_env(
    name: &str,
    home: &str,
    overrides: &[(String, String)],
) -> Vec<(String, String)> {
    let mut env: Vec<(String, String)> = vec![
        ("HOME".to_string(), home.to_string()),
        ("LOGNAME".to_string(), name.to_string()),
        ("USER".to_string(), name.to_string()),
        ("PATH".to_string(), "/usr/bin:/bin".to_string()),
        ("SHELL".to_string(), "/bin/sh".to_string()),
    ];

    for (key, value) in overrides {
        if key == "MAILTO" || key == "LOGNAME" || key == "USER" {
            continue;
        }
        env.retain(|(k, _)| k != key);
        env.push((key.clone(), value.clone()));
    }

    env
}

/// Drop to `uid`/`gid` and the user's supplementary groups, in the order
/// setgid → initgroups → setuid so privileges cannot be regained.
///
/// # Safety
/// Must be called in a freshly forked child, before exec.
pub unsafe fn drop_privileges(uid: u32, gid: u32, name: &str) -> std::io::Result<()> {
    use std::ffi::CString;

    if libc::setgid(gid) != 0 {
        return Err(std::io::Error::last_os_error());
    }

    if let Ok(c_name) = CString::new(name) {
        // initgroups takes c_int on macOS, gid_t on Linux.
        #[cfg(target_os = "macos")]
        let g = gid as libc::c_int;
        #[cfg(not(target_os = "macos"))]
        let g = gid;
        if libc::initgroups(c_name.as_ptr(), g) != 0 {
            return Err(std::io::Error::last_os_error());
        }
    }

    if libc::setuid(uid) != 0 {
        return Err(std::io::Error::last_os_error());
    }

    Ok(())
}

/// Whether `recipient` is safe to pass to the MTA (no header-injection or
/// whitespace characters).
fn recipient_is_safe(recipient: &str) -> bool {
    !recipient.is_empty()
        && recipient
            .chars()
            .all(|c| !c.is_control() && c != ' ' && !matches!(c, '<' | '>'))
}

/// Pipe `body` to the local MTA addressed to `recipient`, best-effort. Skips
/// silently when no sendmail binary is found or the recipient looks unsafe
/// (audit #D8/#A9).
pub fn mail_output(recipient: &str, subject: &str, body: &[u8]) {
    if !recipient_is_safe(recipient) {
        return;
    }

    let sendmail = [
        "/usr/sbin/sendmail",
        "/usr/lib/sendmail",
        "/usr/bin/sendmail",
    ]
    .into_iter()
    .find(|p| std::path::Path::new(p).exists());
    let Some(sendmail) = sendmail else {
        return;
    };

    let mut child = match Command::new(sendmail)
        .arg("-t")
        .arg("-oi")
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
    {
        Ok(c) => c,
        Err(_) => return,
    };

    if let Some(mut stdin) = child.stdin.take() {
        use std::io::Write;
        let header = format!("To: {recipient}\nSubject: {subject}\n\n");
        let _ = stdin.write_all(header.as_bytes());
        let _ = stdin.write_all(body);
    }
    let _ = child.wait();
}

fn get_number(src: &mut Peekable<impl Iterator<Item = char>>) -> Option<i32> {
    let mut number = String::new();

    while let Some(&c) = src.peek() {
        if c.is_ascii_digit() {
            number.push(c);
            src.next();
        } else {
            break;
        }
    }

    number.parse().ok()
}

fn expect(src: &mut Peekable<impl Iterator<Item = char>>, expected: char) -> bool {
    let Some(&c) = src.peek() else { return false };
    if c == expected {
        src.next();
        true
    } else {
        false
    }
}

fn convert_weekdays_to_monthdays(date: NaiveDate, days: WeekDay) -> MonthDay {
    let days = BTreeSet::from_iter(days.to_vec());
    let mut result = vec![];

    for i in 1..=31 {
        let Some(date) = date.with_day(i) else {
            continue;
        };
        let number = date.weekday().num_days_from_sunday() as i32;
        if days.contains(&number) {
            result.push(Value::Number(date.day() as i32));
        }
    }

    MonthDay(Some(result))
}
