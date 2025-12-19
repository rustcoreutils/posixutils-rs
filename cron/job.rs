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
}

#[derive(Clone)]
pub struct Database(pub Vec<CronJob>);

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

            let Some(minutes_field) = fields.next() else {
                continue;
            };
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

            // Collect all remaining fields as the command
            let command: String = fields.collect::<Vec<_>>().join(" ");
            if command.is_empty() {
                continue;
            }

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

            result.push(CronJob {
                minute,
                hour,
                monthday,
                month,
                weekday,
                command: command.to_string(),
            })
        }

        Ok(Self(result))
    }
}

impl CronJob {
    pub fn next_execution(&self, now: &NaiveDateTime) -> Option<NaiveDateTime> {
        let Self {
            minute: minutes,
            hour: hours,
            month: months,
            monthday: monthdays,
            weekday: weekdays,
            command: _,
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
        unsafe {
            let pid = libc::fork();
            if pid < 0 {
                return Err(std::io::Error::last_os_error());
            }
            if pid == 0 {
                // Child process - execute the command
                // This replaces the child process with sh -c "command"
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
