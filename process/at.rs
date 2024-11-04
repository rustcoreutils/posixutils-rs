use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use libc::{getlogin, getpwnam, getpwuid, passwd, uid_t};
use plib::PROJECT_NAME;

use std::{
    env,
    ffi::{CStr, CString},
    io::{Read, Seek, Write},
    path::PathBuf,
    process,
};

/*
    TODO:

    1. file with at.allow and at.deny
    first check is allowed, then is denied. If not found in any -> allow
    MAX chars for name in linux is 32

    2.

*/

// TODO: Find proper file for this
const JOB_FILE_NUMBER: &str = "~/todo";

// TODO: Mac variant
const _SPOOL_DIRECTORY: &str = "/var/spool/atjobs/";

/// at - execute commands at a later time
#[derive(Parser, Debug)]
#[command(
    author,
    version,
    about = "at - execute commands at a later time",
    long_about = "The 'at' command schedules commands to be executed later.\n\
                  Usage:\n\
                  at [-m] [-f file] [-q queuename] -t time_arg\n\
                  at [-m] [-f file] [-q queuename] timespec...\n\
                  at -r at_job_id...\n\
                  at -l -q queuename\n\
                  at -l [at_job_id...]"
)]
struct Args {
    /// Change the environment to what would be expected if the user actually logged in again (letter `l`).
    #[arg(short = 'l', long)]
    login: bool,

    /// Specifies the pathname of a file to be used as the source of the at-job, instead of standard input.
    #[arg(short = 'f', long, value_name = "FILE")]
    file: Option<String>,

    /// Send mail to the invoking user after the at-job has run.
    #[arg(short = 'm', long)]
    mail: bool,

    /// Specify in which queue to schedule a job for submission.
    #[arg(short = 'q', long, value_name = "QUEUENAME", default_value = "a")]
    queue: String,

    /// Remove the jobs with the specified at_job_id operands that were previously scheduled by the at utility.
    #[arg(short = 'r', long)]
    remove: bool,

    /// Submit the job to be run at the time specified by the time option-argument.
    #[arg(short = 't', long, value_name = "TIME_ARG")]
    time: Option<String>,

    /// Group ID or group name.
    #[arg(value_name = "GROUP", required = true)]
    group: String,

    /// Job IDs for reporting jobs scheduled for the invoking user.
    #[arg(value_name = "AT_JOB_ID", required = false)]
    at_job_ids: Vec<String>,
}

/// TODO: Validated user email and info?
pub struct User;

// TODO
/// All commands supplied by user via stdin
pub struct Commands;

// TODO
///
pub struct Timespec {
    _time: std::time::SystemTime,
}

/// Structure to represent future job or script to be saved in [`SPOOL_DIRECTORY`]
pub struct Job {
    shell: String,
    user_uid: u16,
    user_gid: u16,
    user_name: String,
    env: std::env::Vars,
    call_place: PathBuf,
    cmd: String,
}

impl Job {
    pub fn into_script(self) -> String {
        let Self {
            shell,
            user_uid,
            user_gid,
            user_name,
            env,
            call_place,
            cmd,
        } = self;

        let env = env
            .into_iter()
            .map(|(key, value)| format!("{}={}; export {}", key, value, key))
            .collect::<Vec<_>>()
            .concat();

        format!(
            "#!{shell}\n
            # atrun uid={user_uid} gid={user_gid}\n
            # mail {user_name} 0\n
            umask 22\n
            {env}\n
            cd {} || {{ echo 'Execution directory inaccessible' >&2 exit 1 }}\n
            {cmd}",
            call_place.to_string_lossy()
        )
    }
}

fn at() -> Result<(), std::io::Error> {
    let _jobno = next_job_id().inspect_err(|_| eprintln!("Cannot generate job number"))?;

    Ok(())
}

fn next_job_id() -> Result<u32, std::io::Error> {
    let mut file_opt = std::fs::OpenOptions::new();
    file_opt.read(true).write(true);

    let mut buf = [0_u8; 4];

    let (next_job_id, mut file) = match file_opt.open(JOB_FILE_NUMBER) {
        Ok(mut file) => {
            file.read_exact(&mut buf)?;
            file.rewind()?;

            (u32::from_be_bytes(buf), file)
        }
        Err(err) => match err.kind() {
            std::io::ErrorKind::NotFound => (0, std::fs::File::create_new(JOB_FILE_NUMBER)?),
            _ => Err(err)?,
        },
    };

    file.write_all(&(1 + next_job_id).to_be_bytes())?;

    Ok(next_job_id)
}

fn get_login_name() -> Option<String> {
    // Try to get the login name using getlogin
    unsafe {
        let login_ptr = getlogin();
        if !login_ptr.is_null() {
            if let Ok(c_str) = CStr::from_ptr(login_ptr).to_str() {
                return Some(c_str.to_string());
            }
        }
    }

    // Fall back to checking the LOGNAME environment variable
    env::var("LOGNAME").ok()
}

fn get_user_info_by_name(name: &str) -> Option<passwd> {
    let c_name = CString::new(name).unwrap();
    let pw_ptr = unsafe { getpwnam(c_name.as_ptr()) };
    if pw_ptr.is_null() {
        None
    } else {
        Some(unsafe { *pw_ptr })
    }
}

fn get_user_info_by_uid(uid: uid_t) -> Option<passwd> {
    let pw_ptr = unsafe { getpwuid(uid) };
    if pw_ptr.is_null() {
        None
    } else {
        Some(unsafe { *pw_ptr })
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::try_parse().unwrap_or_else(|err| {
        eprintln!("{}", err);
        std::process::exit(1);
    });

    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    if args.mail {
        let real_uid = unsafe { libc::getuid() };
        let mut mailname = get_login_name();

        if mailname
            .as_ref()
            .and_then(|name| get_user_info_by_name(name))
            .is_none()
        {
            if let Some(pass_entry) = get_user_info_by_uid(real_uid) {
                mailname = unsafe {
                    // Safely convert pw_name using CString, avoiding memory leaks.
                    let cstr = CString::from_raw(pass_entry.pw_name as *mut i8);
                    cstr.to_str().ok().map(|s| s.to_string())
                };
            }
        }

        match mailname {
            Some(name) => println!("Mailname: {}", name),
            None => println!("Failed to retrieve mailname."),
        }
    }

    let exit_code = match at() {
        Ok(_) => 0,
        Err(err) => {
            eprint!("{}", err);
            1
        }
    };

    process::exit(exit_code)
}

mod time {

    // TODO -t ARG - SHOULD BE SAME AS IN touch utility
    // fn parse_tm_iso(time: &str) -> Result<DateTime<Utc>, Box<dyn std::error::Error>> {
    //     let dt = DateTime::parse_from_rfc3339(time)?;
    //     Ok(dt.into())
    // }

    // fn parse_tm_posix(time: &str) -> Result<DateTime<Utc>, Box<dyn std::error::Error>> {
    //     let mut time = String::from(time);
    //     let mut seconds = String::from("0");

    //     // split into YYYYMMDDhhmm and [.SS] components
    //     let mut tmp_time = String::new();
    //     match time.split_once('.') {
    //         Some((t, secs)) => {
    //             tmp_time = t.to_string();
    //             seconds = secs.to_string();
    //         }
    //         None => {}
    //     }
    //     if !tmp_time.is_empty() {
    //         time = tmp_time;
    //     }

    //     // extract date and time elements, with length implying format
    //     let tmp_year;
    //     let (year_str, month_str, day_str, hour_str, minute_str) = match time.len() {
    //         // format: MMDDhhmm[.SS]
    //         8 => {
    //             tmp_year = Utc::now().year().to_string();
    //             (
    //                 tmp_year.as_str(),
    //                 &time[0..2],
    //                 &time[2..4],
    //                 &time[4..6],
    //                 &time[6..8],
    //             )
    //         }

    //         // format: YYMMDDhhmm[.SS]
    //         10 => {
    //             let mut yearling = time[0..2].parse::<u32>()?;
    //             if yearling <= 68 {
    //                 yearling += 2000;
    //             } else {
    //                 yearling += 1900;
    //             }
    //             tmp_year = yearling.to_string();
    //             (
    //                 tmp_year.as_str(),
    //                 &time[2..4],
    //                 &time[4..6],
    //                 &time[6..8],
    //                 &time[8..10],
    //             )
    //         }

    //         // format: YYYYMMDDhhmm[.SS]
    //         12 => (
    //             &time[0..4],
    //             &time[4..6],
    //             &time[6..8],
    //             &time[8..10],
    //             &time[10..12],
    //         ),
    //         _ => {
    //             return Err("Invalid time format".into());
    //         }
    //     };

    //     // convert strings to integers
    //     let year = year_str.parse::<i32>()?;
    //     let month = month_str.parse::<u32>()?;
    //     let day = day_str.parse::<u32>()?;
    //     let hour = hour_str.parse::<u32>()?;
    //     let minute = minute_str.parse::<u32>()?;
    //     let secs = seconds.parse::<u32>()?;

    //     // convert to DateTime and validate input
    //     let res = Utc.with_ymd_and_hms(year, month, day, hour, minute, secs);
    //     if res == LocalResult::None {
    //         return Err("Invalid time".into());
    //     }

    //     // return parsed date
    //     let dt = res.unwrap();
    //     Ok(dt)
    // }
}

mod timespec {
    use std::str::FromStr;

    use crate::tokens::{
        AmPm, DayNumber, DayOfWeek, Hr24Clock, Hr24ClockHour, Minute, Month, TimezoneName,
        TokenParsingError, WallClock, WallClockHour, YearNumber,
    };

    // TODO: Proper errors for each case and token
    #[derive(Debug, PartialEq)]
    pub struct TimespecParsingError;

    impl std::fmt::Display for TimespecParsingError {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            writeln!(f, "Failed to parse token in str")
        }
    }

    impl From<std::num::ParseIntError> for TimespecParsingError {
        fn from(_value: std::num::ParseIntError) -> Self {
            Self
        }
    }

    impl From<std::char::TryFromCharError> for TimespecParsingError {
        fn from(_value: std::char::TryFromCharError) -> Self {
            Self
        }
    }

    impl From<TokenParsingError> for TimespecParsingError {
        fn from(_value: TokenParsingError) -> Self {
            Self
        }
    }

    #[derive(Debug, PartialEq)]
    pub enum IncPeriod {
        Minute,
        Hour,
        Day,
        Week,
        Month,
        Year,
    }

    impl FromStr for IncPeriod {
        type Err = TimespecParsingError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let result = match s {
                "minute" | "minutes" => Self::Minute,
                "hour" | "hours" => Self::Hour,
                "day" | "days" => Self::Day,
                "week" | "weeks" => Self::Week,
                "month" | "months" => Self::Month,
                "year" | "years" => Self::Year,
                _ => Err(TimespecParsingError)?,
            };

            Ok(result)
        }
    }

    #[derive(Debug, PartialEq)]
    pub enum Increment {
        Next(IncPeriod),
        Plus { number: u16, period: IncPeriod },
    }

    impl FromStr for Increment {
        type Err = TimespecParsingError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let result = match s.starts_with("+") {
                true => {
                    let number: u16 = s
                        .chars()
                        .skip(1)
                        .take_while(|this| this.is_numeric())
                        .collect::<String>()
                        .parse()?;

                    let period = s
                        .chars()
                        .skip(1)
                        .skip_while(|this| this.is_numeric())
                        .collect::<String>()
                        .parse::<IncPeriod>()?;

                    Self::Plus { number, period }
                }
                false => match s.starts_with("next") {
                    true => Self::Next(IncPeriod::from_str(&s.replace("next", ""))?),
                    false => Err(TimespecParsingError)?,
                },
            };

            Ok(result)
        }
    }

    pub enum Date {
        MontDay {
            month_name: Month,
            day_number: DayNumber,
        },
        MontDayYear {
            month_name: Month,
            day_number: DayNumber,
            year_number: YearNumber,
        },
        DayOfWeek(DayOfWeek),
        Today,
        Tomorrow,
    }

    impl FromStr for Date {
        type Err = TimespecParsingError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let result = match s {
                "today" => Self::Today,
                "tomorrow" => Self::Tomorrow,
                _ => match s.contains(",") {
                    true => {
                        let parts = s.split(',').collect::<Vec<_>>();

                        if parts.len() != 2 {
                            Err(TimespecParsingError)?
                        }

                        let (month_name, day_number) = parse_month_and_day(&parts[0])?;
                        let year_number = YearNumber::from_str(&parts[1])?;

                        Self::MontDayYear {
                            month_name,
                            day_number,
                            year_number,
                        }
                    }
                    false => match DayOfWeek::from_str(s) {
                        Ok(day) => Self::DayOfWeek(day),
                        Err(_) => {
                            let (month_name, day_number) = parse_month_and_day(s)?;

                            Self::MontDay {
                                month_name,
                                day_number,
                            }
                        }
                    },
                },
            };

            fn parse_month_and_day(s: &str) -> Result<(Month, DayNumber), TimespecParsingError> {
                let month = s
                    .chars()
                    .take_while(|this| !this.is_numeric())
                    .collect::<String>()
                    .parse::<Month>()?;

                let day = s
                    .chars()
                    .skip_while(|this| !this.is_numeric())
                    .collect::<String>()
                    .parse::<DayNumber>()?;

                Ok((month, day))
            }

            Ok(result)
        }
    }

    #[derive(Debug, PartialEq)]
    pub enum Time {
        Midnight,
        Noon,
        Hr24clockHour(Hr24Clock),
        Hr24clockHourTimezone {
            hour: Hr24Clock,
            timezone: TimezoneName,
        },
        Hr24clockHourMinute {
            hour: Hr24ClockHour,
            minute: Minute,
        },
        Hr24clockHourMinuteTimezone {
            hour: Hr24ClockHour,
            minute: Minute,
            timezone: TimezoneName,
        },
        WallclockHour {
            clock: WallClock,
            am: AmPm,
        },
        WallclockHourTimezone {
            clock: WallClock,
            am: AmPm,
            timezone: TimezoneName,
        },
        WallclockHourMinute {
            clock: WallClockHour,
            minute: Minute,
            am: AmPm,
        },
        WallclockHourMinuteTimezone {
            clock: WallClockHour,
            minute: Minute,
            am: AmPm,
            timezone: TimezoneName,
        },
    }

    impl FromStr for Time {
        type Err = TimespecParsingError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            match s {
                "noon" => return Ok(Self::Noon),
                "midnight" => return Ok(Self::Midnight),
                _ => (),
            };

            if let Ok(hour) = Hr24Clock::from_str(s) {
                return Ok(Self::Hr24clockHour(hour));
            }

            if let Some((possible_hour, other)) = s.split_once(':') {
                if let Ok(clock) = WallClockHour::from_str(possible_hour) {
                    let minute = other
                        .chars()
                        .take_while(|this: &char| this.is_numeric())
                        .collect::<String>();
                    let minutes_len = minute.len();
                    let minute = Minute::from_str(&minute)?;

                    let other = other.chars().skip(minutes_len).collect::<String>();

                    if let Ok(am) = AmPm::from_str(&other) {
                        return Ok(Self::WallclockHourMinute { clock, minute, am });
                    }

                    let am = AmPm::from_str(&other[..2])?;
                    let timezone = TimezoneName::from_str(&other[2..])?;

                    return Ok(Self::WallclockHourMinuteTimezone {
                        clock,
                        minute,
                        am,
                        timezone,
                    });
                }

                if let Ok(hour) = Hr24ClockHour::from_str(possible_hour) {
                    let result = match Minute::from_str(other) {
                        Ok(minute) => Self::Hr24clockHourMinute { hour, minute },
                        Err(_) => {
                            let minute = other
                                .chars()
                                .take_while(|this| this.is_numeric())
                                .collect::<String>()
                                .parse::<Minute>()?;

                            let timezone = other
                                .chars()
                                .skip_while(|this| this.is_numeric())
                                .collect::<String>()
                                .parse::<TimezoneName>()?;

                            Self::Hr24clockHourMinuteTimezone {
                                hour,
                                minute,
                                timezone,
                            }
                        }
                    };

                    return Ok(result);
                }
            }

            let number = s
                .chars()
                .take_while(|this| this.is_numeric())
                .collect::<String>();
            let other = s
                .chars()
                .skip_while(|this| this.is_numeric())
                .collect::<String>();

            if let Ok(clock) = WallClock::from_str(&number) {
                if let Ok(am) = AmPm::from_str(&other[..2]) {
                    let timezone = TimezoneName::from_str(&other[2..]);
                    if let Ok(timezone) = timezone {
                        return Ok(Self::WallclockHourTimezone {
                            clock,
                            am,
                            timezone,
                        });
                    } else {
                        return Ok(Self::WallclockHour { clock, am });
                    }
                }
            }

            if let Ok(hour) = Hr24Clock::from_str(&number) {
                let timezone = TimezoneName::from_str(&other);

                if let Ok(timezone) = timezone {
                    return Ok(Self::Hr24clockHourTimezone { hour, timezone });
                }
            }

            Err(TimespecParsingError)
        }
    }

    #[derive(Debug, PartialEq)]
    pub enum Nowspec {
        Now,
        NowIncrement(Increment),
    }

    impl FromStr for Nowspec {
        type Err = TimespecParsingError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            const NOW: &str = "now";

            let result = match s {
                NOW => Self::Now,
                _ if s.starts_with(NOW) => {
                    let (_, increment) = s.split_once(NOW).ok_or(TimespecParsingError)?;

                    Self::NowIncrement(Increment::from_str(increment)?)
                }
                _ => Err(TimespecParsingError)?,
            };

            Ok(result)
        }
    }

    pub enum Timespec {
        Time(Time),
        TimeDate {
            time: Time,
            date: Date,
        },
        TimeDateIncrement {
            time: Time,
            date: Date,
            inrement: Increment,
        },
        Nowspec(Nowspec),
    }

    impl FromStr for Timespec {
        type Err = TimespecParsingError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            if let Ok(time) = Time::from_str(s) {
                return Ok(Self::Time(time));
            }

            if let Ok(time) = Nowspec::from_str(s) {
                return Ok(Self::Nowspec(time));
            }

            let mut time_index = 0;
            for slice_index in 0..=s.len() {
                // return error before we hit panic with out of bounds
                if slice_index == s.len() {
                    Err(TimespecParsingError)?;
                }

                let time = Time::from_str(&s[..slice_index]);
                if let Ok(_) = time {
                    time_index = slice_index;
                    break;
                }
            }

            let time = Time::from_str(&s[..time_index])?;

            let mut date_index = 0;
            for slice_index in time_index..=s.len() {
                // return error before we hit panic with out of bounds
                if slice_index == s.len() {
                    Err(TimespecParsingError)?;
                }

                let date = Date::from_str(&s[..slice_index]);
                if let Ok(_) = date {
                    date_index = slice_index;
                    break;
                }
            }

            let date = Date::from_str(&s[time_index..date_index])?;

            if date_index != s.len() {
                let inrement = Increment::from_str(&s[date_index..])?;

                return Ok(Self::TimeDateIncrement {
                    time,
                    date,
                    inrement,
                });
            }

            Ok(Self::TimeDate { time, date })
        }
    }

    #[cfg(test)]
    mod test {
        use std::num::NonZero;

        use super::*;

        // increment
        #[test]
        fn increment_period_simple() {
            let actual = Increment::from_str("+1day");

            assert_eq!(
                Ok(Increment::Plus {
                    number: 1,
                    period: IncPeriod::Day
                }),
                actual
            )
        }

        #[test]
        fn increment_period_two_numbers() {
            let actual = Increment::from_str("+12days");

            assert_eq!(
                Ok(Increment::Plus {
                    number: 12,
                    period: IncPeriod::Day
                }),
                actual
            )
        }

        #[test]
        fn increment_period_next() {
            let actual = Increment::from_str("nextday");

            assert_eq!(Ok(Increment::Next(IncPeriod::Day)), actual)
        }

        #[test]
        fn increment_period_no_number_after_sign() {
            let actual = Increment::from_str("+day");

            assert_eq!(Err(TimespecParsingError), actual)
        }

        #[test]
        fn increment_period_empty() {
            let actual = Increment::from_str("");

            assert_eq!(Err(TimespecParsingError), actual)
        }

        #[test]
        fn increment_period_next_no_perid_fails() {
            let actual = Increment::from_str("next");

            assert_eq!(Err(TimespecParsingError), actual)
        }

        // nowspec

        #[test]
        fn nowspec_simple_now() {
            let actual = Nowspec::from_str("now");

            assert_eq!(Ok(Nowspec::Now), actual)
        }

        #[test]
        fn nowspec_incremented_number() {
            let actual = Nowspec::from_str("now+1day");

            assert_eq!(
                Ok(Nowspec::NowIncrement(Increment::Plus {
                    number: 1,
                    period: IncPeriod::Day
                })),
                actual
            );
        }

        #[test]
        fn nowspec_incremented_by_next() {
            let actual = Nowspec::from_str("nownextday");

            assert_eq!(
                Ok(Nowspec::NowIncrement(Increment::Next(IncPeriod::Day))),
                actual
            );
        }

        // time

        #[test]
        fn time_simple_24_hour_single_number() {
            let actual = Time::from_str("1");

            assert_eq!(Ok(Time::Hr24clockHour(Hr24Clock([1, 0]))), actual)
        }

        #[test]
        fn time_24_hour_full() {
            let actual = Time::from_str("1453");

            assert_eq!(Ok(Time::Hr24clockHour(Hr24Clock([14, 53]))), actual)
        }

        #[test]
        fn time_24_hour_noon() {
            let actual = Time::from_str("noon");

            assert_eq!(Ok(Time::Noon), actual)
        }

        #[test]
        fn time_24_hour_midnight() {
            let actual = Time::from_str("midnight");

            assert_eq!(Ok(Time::Midnight), actual)
        }

        #[test]
        fn time_24_hour_plus_timezone() {
            let actual = Time::from_str("1453UTC");

            assert_eq!(
                Ok(Time::Hr24clockHourTimezone {
                    hour: Hr24Clock([14, 53]),
                    timezone: TimezoneName("UTC".to_owned())
                }),
                actual
            )
        }

        #[test]
        fn time_24_hour_plus_timezone_2_digits() {
            let actual = Time::from_str("14UTC");

            assert_eq!(
                Ok(Time::Hr24clockHourTimezone {
                    hour: Hr24Clock([14, 00]),
                    timezone: TimezoneName("UTC".to_owned())
                }),
                actual
            )
        }

        #[test]
        fn time_hr24clock_hour_plus_minute() {
            let actual = Time::from_str("14:53");

            assert_eq!(
                Ok(Time::Hr24clockHourMinute {
                    hour: Hr24ClockHour(14),
                    minute: Minute(53)
                }),
                actual
            )
        }

        #[test]
        fn time_hr24clock_hour_plus_minute_timezone() {
            let actual = Time::from_str("14:53UTC");

            assert_eq!(
                Ok(Time::Hr24clockHourMinuteTimezone {
                    hour: Hr24ClockHour(14),
                    minute: Minute(53),
                    timezone: TimezoneName("UTC".to_owned())
                }),
                actual
            )
        }

        #[test]
        fn time_wallclock_hr_min_am() {
            let actual = Time::from_str("0500am");

            assert_eq!(
                Ok(Time::WallclockHour {
                    clock: WallClock {
                        hour: NonZero::new(5).expect("valid"),
                        minutes: 0
                    },
                    am: AmPm::Am
                }),
                actual
            )
        }

        #[test]
        fn time_wallclock_hr_min_plus_am_timezone() {
            let actual = Time::from_str("0500amUTC");

            assert_eq!(
                Ok(Time::WallclockHourTimezone {
                    clock: WallClock {
                        hour: NonZero::new(5).expect("valid"),
                        minutes: 0
                    },
                    am: AmPm::Am,
                    timezone: TimezoneName("UTC".to_owned())
                }),
                actual
            )
        }

        #[test]
        fn time_wallclock_hour_minute_am() {
            let actual = Time::from_str("05:53am");

            assert_eq!(
                Ok(Time::WallclockHourMinute {
                    clock: WallClockHour(NonZero::new(5).expect("valid")),
                    minute: Minute(53),
                    am: AmPm::Am,
                }),
                actual
            )
        }

        #[test]
        fn time_wallclock_hour_minute_am_timezone() {
            let actual = Time::from_str("05:53amUTC");

            assert_eq!(
                Ok(Time::WallclockHourMinuteTimezone {
                    clock: WallClockHour(NonZero::new(5).expect("valid")),
                    minute: Minute(53),
                    am: AmPm::Am,
                    timezone: TimezoneName("UTC".to_owned())
                }),
                actual
            )
        }
    }
}

mod tokens {
    use std::{num::NonZero, str::FromStr};

    // TODO: Proper errors for each case and token
    #[derive(Debug, PartialEq)]
    pub struct TokenParsingError;

    impl From<std::num::ParseIntError> for TokenParsingError {
        fn from(_value: std::num::ParseIntError) -> Self {
            Self
        }
    }

    impl From<std::char::TryFromCharError> for TokenParsingError {
        fn from(_value: std::char::TryFromCharError) -> Self {
            Self
        }
    }

    impl std::fmt::Display for TokenParsingError {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            writeln!(f, "Failed to parse token in str")
        }
    }

    /// Specific operands in timespec for time
    pub enum TimespecTimeOperands {
        /// Indicates the time 12:00 am (00:00).
        Midnight,
        /// Indicates the time 12:00 pm.
        Noon,
        /// Indicates the current day and time.
        Now,
    }

    impl FromStr for TimespecTimeOperands {
        type Err = TokenParsingError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let result = match s {
                "midnight" => Self::Midnight,
                "noon" => Self::Noon,
                "now" => Self::Now,
                _ => Err(TokenParsingError)?,
            };

            Ok(result)
        }
    }

    /// Specific operands in timespec for date
    pub enum TimespecDateOperands {
        /// Indicates the current day.
        Today,
        /// Indicates the day following the current day.
        Tomorrow,
    }

    impl FromStr for TimespecDateOperands {
        type Err = TokenParsingError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let result = match s {
                "today" => Self::Today,
                "tomorrow" => Self::Tomorrow,
                _ => Err(TokenParsingError)?,
            };

            Ok(result)
        }
    }

    /// An `Hr24clockHour` is a one, two, or four-digit number. A one-digit
    /// or two-digit number constitutes an `Hr24clockHour`. An `Hr24clockHour`
    /// may be any of the single digits `[0,9]`, or may be double digits, ranging
    /// from `[00,23]`. If an `Hr24clockHour` is a four-digit number, the
    /// first two digits shall be a valid `Hr24clockHour`, while the last two
    /// represent the number of minutes, from `[00,59]`.
    #[derive(Debug, PartialEq)]
    pub struct Hr24Clock(pub(crate) [u8; 2]);

    impl Hr24Clock {
        pub fn into_inner(self) -> [u8; 2] {
            self.0
        }
    }

    impl FromStr for Hr24Clock {
        type Err = TokenParsingError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let chars_count = s.chars().count();

            let result = match chars_count {
                1 => {
                    let hour = u8::from_str(s)?;
                    if hour > 9 {
                        Err(TokenParsingError)?
                    }

                    [hour, 0]
                }
                2 => {
                    let hour = u8::from_str(s)?;

                    if hour > 23 {
                        Err(TokenParsingError)?
                    }

                    [hour, 0]
                }
                4 => {
                    // TODO: should be fine?
                    let hour = &s[..2];
                    let minutes = &s[2..];

                    let hour = u8::from_str(hour)?;
                    let minutes = u8::from_str(minutes)?;

                    if hour > 23 || minutes > 59 {
                        Err(TokenParsingError)?
                    }

                    [hour, minutes]
                }
                _ => Err(TokenParsingError)?,
            };

            Ok(Self(result))
        }
    }

    #[derive(Debug, PartialEq)]
    pub struct Hr24ClockHour(pub(crate) u8);

    impl FromStr for Hr24ClockHour {
        type Err = TokenParsingError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let number = u8::from_str(s)?;

            if number > 23 {
                Err(TokenParsingError)?
            }

            Ok(Self(number))
        }
    }

    /// A `WallclockHour` is a one, two-digit, or four-digit number.
    /// A one-digit or two-digit number constitutes a `WallclockHour`.
    /// A `WallclockHour` may be any of the single digits `[1..=9]`, or may
    /// be double digits, ranging from `[01..=12]`. If a `WallclockHour`
    /// is a four-digit number, the first two digits shall be a valid
    /// `WallclockHour`, while the last two represent the number of
    /// minutes, from `[00..=59]`.
    #[derive(Debug, PartialEq)]
    pub struct WallClock {
        pub(crate) hour: NonZero<u8>,
        pub(crate) minutes: u8,
    }

    impl WallClock {
        pub fn into_inner(self) -> (NonZero<u8>, u8) {
            (self.hour, self.minutes)
        }
    }

    impl FromStr for WallClock {
        type Err = TokenParsingError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let chars_count = s.chars().count();

            let result = match chars_count {
                1 => {
                    let hour = NonZero::<u8>::from_str(s)?;
                    if hour.get() > 9 {
                        Err(TokenParsingError)?
                    }

                    Self { hour, minutes: 0 }
                }
                2 => {
                    let hour = NonZero::<u8>::from_str(s)?;

                    if hour.get() > 23 {
                        Err(TokenParsingError)?
                    }

                    Self { hour, minutes: 0 }
                }
                4 => {
                    // TODO: should be fine?
                    let hour = &s[..2];
                    let minutes = &s[2..];

                    let hour = NonZero::<u8>::from_str(hour)?;
                    let minutes = u8::from_str(minutes)?;

                    if hour.get() > 23 || minutes > 59 {
                        Err(TokenParsingError)?
                    }

                    Self { hour, minutes }
                }
                _ => Err(TokenParsingError)?,
            };

            Ok(result)
        }
    }

    #[derive(Debug, PartialEq)]
    pub struct WallClockHour(pub(crate) NonZero<u8>);

    impl FromStr for WallClockHour {
        type Err = TokenParsingError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let number = NonZero::from_str(s)?;

            if number.get() > 12 {
                Err(TokenParsingError)?;
            }

            Ok(Self(number))
        }
    }

    /// A `Minute` is a one or two-digit number whose value
    /// can be `[0..=9]` or` [00..=59]`.
    #[derive(Debug, PartialEq)]
    pub struct Minute(pub(crate) u8);

    impl Minute {
        pub fn into_inner(self) -> u8 {
            self.0
        }
    }

    impl FromStr for Minute {
        type Err = TokenParsingError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let minute = u8::from_str(s)?;

            if minute > 59 {
                Err(TokenParsingError)?
            }

            Ok(Self(minute))
        }
    }

    pub struct DayNumber(NonZero<u8>);

    impl FromStr for DayNumber {
        type Err = TokenParsingError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let number = NonZero::from_str(s).map_err(TokenParsingError::from)?;

            if number.get() > 31 {
                Err(TokenParsingError)?;
            }

            Ok(Self(number))
        }
    }

    /// A `YearNumber` is a four-digit number representing the year A.D., in
    /// which the at_job is to be run.
    pub struct YearNumber(u16);

    impl FromStr for YearNumber {
        type Err = TokenParsingError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let year = u16::from_str(s)?;
            if year < 1000 {
                // it should be 4 number, so yeah...
                Err(TokenParsingError)?
            }

            Ok(Self(year))
        }
    }

    /// The name of an optional timezone suffix to the time field, in an
    /// implementation-defined format.
    #[derive(Debug, PartialEq)]
    pub struct TimezoneName(pub(crate) String);

    impl FromStr for TimezoneName {
        type Err = TokenParsingError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            match s {
                "am" | "pm" | "" => return Err(TokenParsingError),
                _ if s.trim() == "" => return Err(TokenParsingError),
                _ => (),
            }

            // TODO: how to validate this? C impl simply reads env
            Ok(Self(s.to_owned()))
        }
    }

    /// One of the values from the mon or abmon keywords in the LC_TIME
    /// locale category.
    pub struct Month(u8);

    impl FromStr for Month {
        type Err = TokenParsingError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let number = match s {
                "JAN" => 0,
                "FEB" => 1,
                "MAR" => 2,
                "APR" => 3,
                "MAY" => 4,
                "JUN" => 5,
                "JUL" => 6,
                "AUG" => 7,
                "SEP" => 8,
                "OCT" => 9,
                "NOV" => 10,
                "DEC" => 11,
                _ => Err(TokenParsingError)?,
            };

            Ok(Self(number))
        }
    }

    /// One of the values from the day or abday keywords in the LC_TIME
    /// locale category.
    pub struct DayOfWeek(u8);

    impl FromStr for DayOfWeek {
        type Err = TokenParsingError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let number = match s {
                "SUN" => 0,
                "MON" => 1,
                "TUE" => 2,
                "WED" => 3,
                "THU" => 4,
                "FRI" => 5,
                "SAT" => 6,
                _ => Err(TokenParsingError)?,
            };

            Ok(Self(number))
        }
    }

    /// One of the values from the am_pm keyword in the LC_TIME locale
    /// category.
    #[derive(Debug, PartialEq)]
    pub enum AmPm {
        Am,
        Pm,
    }

    impl FromStr for AmPm {
        type Err = TokenParsingError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            Ok(match s.to_lowercase().as_str() {
                "am" => Self::Am,
                "pm" => Self::Pm,
                _ => Err(TokenParsingError)?,
            })
        }
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn hour24_empty_char() {
            let actual = Hr24Clock::from_str("").map(Hr24Clock::into_inner);

            assert_eq!(Err(TokenParsingError), actual)
        }

        #[test]
        fn hour24_single_char_ok() {
            for value in 0..=9 {
                let actual = Hr24Clock::from_str(&value.to_string()).map(Hr24Clock::into_inner);

                assert_eq!(Ok([value, 0,]), actual)
            }
        }

        #[test]
        fn hour24_single_char_not_a_number() {
            let actual = Hr24Clock::from_str("a").map(Hr24Clock::into_inner);

            assert_eq!(Err(TokenParsingError), actual)
        }

        #[test]
        fn hour24_two_chars_ok() {
            for hour in 10..23 {
                let actual = Hr24Clock::from_str(&hour.to_string()).map(Hr24Clock::into_inner);

                assert_eq!(Ok([hour, 0]), actual)
            }
        }

        #[test]
        fn hour24_two_chars_out_of_range() {
            let actual = Hr24Clock::from_str(&format!("24")).map(Hr24Clock::into_inner);

            assert_eq!(Err(TokenParsingError), actual)
        }

        #[test]
        fn hour24_two_chars_not_a_number() {
            let actual = Hr24Clock::from_str(&format!("aa")).map(Hr24Clock::into_inner);

            assert_eq!(Err(TokenParsingError), actual)
        }

        #[test]
        fn hour24_four_chars() {
            for hour in 10..23 {
                for minute in 0..10 {
                    let actual =
                        Hr24Clock::from_str(&format!("{hour}0{minute}")).map(Hr24Clock::into_inner);

                    assert_eq!(Ok([hour, minute]), actual)
                }

                for minute in 10..=59 {
                    let actual =
                        Hr24Clock::from_str(&format!("{hour}{minute}")).map(Hr24Clock::into_inner);

                    assert_eq!(Ok([hour, minute]), actual)
                }
            }
        }

        #[test]
        fn hour24_four_chars_out_of_range() {
            let actual = Hr24Clock::from_str(&format!("2400")).map(Hr24Clock::into_inner);

            assert_eq!(Err(TokenParsingError), actual);

            let actual = Hr24Clock::from_str(&format!("2360")).map(Hr24Clock::into_inner);

            assert_eq!(Err(TokenParsingError), actual)
        }

        #[test]
        fn hour24_four_chars_not_a_number() {
            let actual = Hr24Clock::from_str(&format!("aaaa")).map(Hr24Clock::into_inner);

            assert_eq!(Err(TokenParsingError), actual)
        }

        #[test]
        fn wallclock_hour_empty_char() {
            let actual = WallClock::from_str("").map(WallClock::into_inner);

            assert_eq!(Err(TokenParsingError), actual)
        }

        #[test]
        fn wallclock_hour_single_char_ok() {
            for value in 1..=9 {
                let actual = WallClock::from_str(&value.to_string()).map(WallClock::into_inner);

                assert_eq!(
                    Ok((NonZero::new(value).expect("not a zero"), 0_u8,)),
                    actual
                )
            }
        }

        #[test]
        fn wallclock_hour_single_char_not_a_number() {
            let actual = WallClock::from_str("a").map(WallClock::into_inner);

            assert_eq!(Err(TokenParsingError), actual)
        }

        #[test]
        fn wallclock_hour_two_chars_ok() {
            for hour in 10..23 {
                let actual = WallClock::from_str(&hour.to_string()).map(WallClock::into_inner);

                assert_eq!(Ok((NonZero::new(hour).expect("not a zero"), 0)), actual)
            }
        }

        #[test]
        fn wallclock_hour_two_chars_out_of_range() {
            let actual = WallClock::from_str("24").map(WallClock::into_inner);

            assert_eq!(Err(TokenParsingError), actual)
        }

        #[test]
        fn wallclock_hour_two_chars_not_a_number() {
            let actual = WallClock::from_str("aa").map(WallClock::into_inner);

            assert_eq!(Err(TokenParsingError), actual)
        }

        #[test]
        fn wallclock_hour_four_chars() {
            for hour in 10..23 {
                for minute in 0..10 {
                    let actual =
                        WallClock::from_str(&format!("{hour}0{minute}")).map(WallClock::into_inner);

                    assert_eq!(
                        Ok((NonZero::new(hour).expect("not a zero"), minute)),
                        actual
                    )
                }

                for minute in 10..=59 {
                    let actual =
                        WallClock::from_str(&format!("{hour}{minute}")).map(WallClock::into_inner);

                    assert_eq!(
                        Ok((NonZero::new(hour).expect("not a zero"), minute)),
                        actual
                    )
                }
            }
        }

        #[test]
        fn wallclock_hour_four_chars_out_of_range() {
            let actual = WallClock::from_str(&format!("2400")).map(WallClock::into_inner);

            assert_eq!(Err(TokenParsingError), actual);

            let actual = WallClock::from_str(&format!("2360")).map(WallClock::into_inner);

            assert_eq!(Err(TokenParsingError), actual)
        }

        #[test]
        fn wallclock_hour_four_chars_not_a_number() {
            let actual = WallClock::from_str("aaaa").map(WallClock::into_inner);

            assert_eq!(Err(TokenParsingError), actual)
        }

        #[test]
        fn wallclock_hour_singe_char_zero_fails() {
            let actual = WallClock::from_str("0").map(WallClock::into_inner);

            assert_eq!(Err(TokenParsingError), actual)
        }

        #[test]
        fn wallclock_hour_two_chars_zero_fails() {
            let actual = WallClock::from_str("00").map(WallClock::into_inner);

            assert_eq!(Err(TokenParsingError), actual)
        }

        #[test]
        fn wallclock_hour_four_chars_zero_fails() {
            let actual = WallClock::from_str("0035").map(WallClock::into_inner);

            assert_eq!(Err(TokenParsingError), actual)
        }

        #[test]
        fn minute_empty_char() {
            let actual = Minute::from_str("").map(Minute::into_inner);

            assert_eq!(Err(TokenParsingError), actual)
        }

        #[test]
        fn minute_single_char_ok() {
            for value in 1..=9 {
                let actual = Minute::from_str(&value.to_string()).map(Minute::into_inner);

                assert_eq!(Ok(value), actual)
            }
        }

        #[test]
        fn minute_single_char_not_a_number() {
            let actual = Minute::from_str("a").map(Minute::into_inner);

            assert_eq!(Err(TokenParsingError), actual)
        }

        #[test]
        fn minute_two_chars_ok() {
            for value in 10..=59 {
                let actual = Minute::from_str(&value.to_string()).map(Minute::into_inner);

                assert_eq!(Ok(value), actual)
            }
        }

        #[test]
        fn minute_two_chars_out_of_range() {
            let actual = Minute::from_str("60").map(Minute::into_inner);

            assert_eq!(Err(TokenParsingError), actual)
        }

        #[test]
        fn minute_two_chars_not_a_number() {
            let actual = Minute::from_str("aa").map(Minute::into_inner);

            assert_eq!(Err(TokenParsingError), actual)
        }
    }
}
