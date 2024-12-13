//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use chrono::{Datelike, Local, NaiveDate, NaiveDateTime, Timelike};
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::iter::Peekable;
use std::process::Command;
use std::str::FromStr;

trait TimeUnit {}

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

            fn merge(self, other: Self) -> Self {
                let mut a = self.to_vec();
                let b = self.to_vec();
                a.extend(b);
                a.sort();
                a.dedup();
                Self(Some(a.into_iter().map(|x| Value::Number(x)).collect()))
            }

            fn parse(s: impl AsRef<str>) -> Result<Self, ()> {
                let s = s.as_ref();
                let mut v = Vec::new();
                let mut src = s.chars().peekable();

                if expect(&mut src, '*') {
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
                        let Some(max) = get_number(&mut src) else {
                            return Err(());
                        };
                        if expect(&mut src, '/') {
                            let Some(step) = get_number(&mut src) else {
                                return Err(());
                            };
                            v.push(Value::Range { min, max, step });
                        }
                        v.push(Value::Range { min, max, step: 1 });
                    }
                    v.push(Value::Number(min));
                }

                Ok(Self(Some(v)))
            }
        }

        impl TimeUnit for $name {}
    };
}

time_unit!(Minute from 0 to 59);
time_unit!(Hour from 0 to 23);
time_unit!(MonthDay from 1 to 31);
time_unit!(Month from 1 to 12);
time_unit!(WeekDay from 0 to 6);

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct CronJob {
    pub minute: Minute,
    pub hour: Hour,
    pub monthday: MonthDay,
    pub month: Month,
    pub weekday: WeekDay,
    pub command: String,
}

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
            .min_by_key(|x| {
                println!("{x:?}");
                x.next_execution(&now)
            })
            .cloned()
    }
}

impl FromStr for Database {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut result = vec![];

        for line in s.lines() {
            let mut fields = line.split_ascii_whitespace();

            let Some(minutes_field) = fields.next() else {
                return Err(());
            };
            let Some(hours_field) = fields.next() else {
                return Err(());
            };
            let Some(monthdays_field) = fields.next() else {
                return Err(());
            };
            let Some(months_field) = fields.next() else {
                return Err(());
            };
            let Some(weekdays_field) = fields.next() else {
                return Err(());
            };
            let Some(command) = fields.next() else {
                return Err(());
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

        let monthdays = monthdays.clone();
        let weekdays = weekdays.clone();

        let monthdays = match (monthdays.clone().0, weekdays.clone().0) {
            (Some(_), Some(_)) => {
                monthdays.merge(convert_weekdays_to_monthdays(now.date(), weekdays))
            }
            (None, Some(_)) => convert_weekdays_to_monthdays(now.date(), weekdays),
            (Some(_), None) => monthdays,
            (None, None) => MonthDay(None),
        };

        println!("{:?}", monthdays.to_vec());

        for month in &months.to_vec() {
            for monthday in &monthdays.to_vec() {
                for hour in &hours.to_vec() {
                    for minute in &minutes.to_vec() {
                        let mut next_exec = *now;
                        let Some(date) = next_exec.with_minute(*minute as u32) else {
                            continue;
                        };
                        next_exec = date;
                        let Some(date) = next_exec.with_hour(*hour as u32) else {
                            continue;
                        };
                        next_exec = date;
                        let Some(date) = next_exec.with_day(*monthday as u32) else {
                            continue;
                        };
                        next_exec = date;
                        let Some(date) = next_exec.with_month(*month as u32) else {
                            continue;
                        };
                        next_exec = date;

                        if next_exec > *now {
                            return Some(next_exec);
                        }
                    }
                }
            }
        }

        None
    }

    pub fn run_job(&self) -> std::io::Result<std::process::Output> {
        Command::new("sh").args(["-c", &self.command]).output()
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
