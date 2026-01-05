//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use chrono::{Datelike, Local, NaiveDate, NaiveDateTime};
use std::collections::BTreeSet;
use std::ffi::CStr;
use std::iter::Peekable;
use std::os::unix::process::CommandExt;
use std::process::Command;
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

        for line in content.lines() {
            let line = line.trim();

            // Skip empty lines and comments
            if line.is_empty() || line.starts_with('#') {
                continue;
            }

            // Skip environment variable assignments (NAME=value)
            if line.contains('=') && !line.starts_with('@') && !line.starts_with('*') {
                if let Some(first_char) = line.chars().next() {
                    if first_char.is_alphabetic() {
                        continue;
                    }
                }
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
            });
        }

        Database(result)
    }
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

        for line in s.lines() {
            let line = line.trim();

            // Skip empty lines and comments
            if line.is_empty() || line.starts_with('#') {
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
            })
        }

        Ok(Self(result))
    }
}

impl CronJob {
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
        // SAFETY: fork() is safe to call here because:
        // 1. We immediately check the return value for errors (pid < 0)
        // 2. The child process (pid == 0) immediately exec()s a new process
        // 3. The parent process returns immediately without shared state issues
        // 4. We use Command::exec() which replaces the child process entirely
        unsafe {
            let pid = libc::fork();
            if pid < 0 {
                return Err(std::io::Error::last_os_error());
            }
            if pid == 0 {
                // Child process - drop privileges and execute the command

                // Drop privileges if owner info is available
                if let (Some(uid), Some(gid), Some(name), Some(home)) = (
                    self.owner_uid,
                    self.owner_gid,
                    &self.owner_name,
                    &self.owner_home,
                ) {
                    use std::ffi::CString;

                    // Set GID first (must be done before dropping root)
                    if libc::setgid(gid) != 0 {
                        eprintln!("Failed to setgid({})", gid);
                        std::process::exit(1);
                    }

                    // Set supplementary groups
                    // Note: initgroups takes c_int on macOS, gid_t on Linux
                    if let Ok(c_name) = CString::new(name.as_str()) {
                        #[cfg(target_os = "macos")]
                        let initgroups_gid = gid as libc::c_int;
                        #[cfg(not(target_os = "macos"))]
                        let initgroups_gid = gid;

                        if libc::initgroups(c_name.as_ptr(), initgroups_gid) != 0 {
                            eprintln!("Failed to initgroups for {}", name);
                            std::process::exit(1);
                        }
                    }

                    // Set UID (drops root privileges)
                    if libc::setuid(uid) != 0 {
                        eprintln!("Failed to setuid({})", uid);
                        std::process::exit(1);
                    }

                    // Change to user's home directory
                    if let Ok(c_home) = CString::new(home.as_str()) {
                        // Ignore chdir errors - job may still run from /
                        let _ = libc::chdir(c_home.as_ptr());
                    }
                }

                // Execute the command via sh -c
                let err = Command::new("sh").args(["-c", &self.command]).exec();
                // exec() only returns on error
                eprintln!("Failed to exec job: {}", err);
                std::process::exit(1);
            }
            // Parent returns immediately
            Ok(())
        }
    }
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
