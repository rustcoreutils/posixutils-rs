//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use chrono::Datelike;
use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};

/// Month names for calendar display
const MONTH_NAMES: [&str; 12] = [
    "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December",
];

// Julian calendar JDN formula constants
const JDN_JULIAN_YEAR_FACTOR: i64 = 367;
const JDN_JULIAN_MONTH_FACTOR: i64 = 275;
const JDN_JULIAN_EPOCH_OFFSET: i64 = 1729777;

// Gregorian calendar JDN formula constants
const JDN_GREGORIAN_YEAR_BASE: i64 = 4800;
const JDN_GREGORIAN_MONTH_FACTOR: i64 = 153;
const JDN_GREGORIAN_EPOCH_OFFSET: i64 = 32045;

/// Determines if a date is in the Julian calendar era.
/// Julian calendar: January 1, year 1 through September 2, 1752
/// Gregorian calendar: September 14, 1752 through December 31, 9999
fn is_julian(year: u32, month: u32, day: u32) -> bool {
    if year < 1752 {
        return true;
    }
    if year > 1752 {
        return false;
    }
    // year == 1752
    if month < 9 {
        return true;
    }
    if month > 9 {
        return false;
    }
    // September 1752: days 1-2 are Julian, days 14-30 are Gregorian
    day <= 2
}

/// Calculate if a year is a leap year.
/// Julian: divisible by 4
/// Gregorian: divisible by 4, except centuries unless divisible by 400
fn is_leap_year(year: u32, use_julian: bool) -> bool {
    if use_julian {
        year % 4 == 0
    } else {
        year % 4 == 0 && (year % 100 != 0 || year % 400 == 0)
    }
}

/// Calculate the Julian Day Number for a given date.
/// Returns different values for Julian vs Gregorian calendar systems.
fn julian_day_number(year: u32, month: u32, day: u32) -> i64 {
    let y = year as i64;
    let m = month as i64;
    let d = day as i64;

    if is_julian(year, month, day) {
        // Julian calendar formula
        JDN_JULIAN_YEAR_FACTOR * y - (7 * (y + 5001 + (m - 9) / 7)) / 4
            + (JDN_JULIAN_MONTH_FACTOR * m) / 9
            + d
            + JDN_JULIAN_EPOCH_OFFSET
    } else {
        // Gregorian calendar formula
        let a = (14 - m) / 12;
        let y_adj = y + JDN_GREGORIAN_YEAR_BASE - a;
        let m_adj = m + 12 * a - 3;
        d + (JDN_GREGORIAN_MONTH_FACTOR * m_adj + 2) / 5 + 365 * y_adj + y_adj / 4 - y_adj / 100
            + y_adj / 400
            - JDN_GREGORIAN_EPOCH_OFFSET
    }
}

/// Calculate day of week (0 = Sunday, 6 = Saturday) for a given date.
fn day_of_week(year: u32, month: u32, day: u32) -> u32 {
    let jdn = julian_day_number(year, month, day);
    ((jdn + 1) % 7) as u32
}

/// Get the days in a month, accounting for the September 1752 gap.
/// September 1752 has only 19 days: 1, 2, then 14-30 (days 3-13 don't exist).
fn days_in_month(year: u32, month: u32) -> Vec<u32> {
    // Special case: September 1752 has only 19 days (1, 2, 14-30)
    if year == 1752 && month == 9 {
        let mut days = vec![1, 2];
        days.extend(14..=30);
        return days;
    }

    // Determine which calendar system to use for leap year calculation
    let use_julian = is_julian(year, month, 1);
    let leap = is_leap_year(year, use_julian);

    let count = match month {
        4 | 6 | 9 | 11 => 30,
        2 => {
            if leap {
                29
            } else {
                28
            }
        }
        _ => 31,
    };

    (1..=count).collect()
}

#[derive(Parser)]
#[command(version, about = gettext("cal - print a calendar"))]
struct Args {
    #[arg(
        value_parser = clap::value_parser!(u32).range(1..=9999),
        help = gettext(
            "Specify the month to be displayed, represented as a decimal integer from 1 (January) to 12 (December)"
        )
    )]
    month: Option<u32>,

    #[arg(
        value_parser = clap::value_parser!(u32).range(1..=9999),
        help = gettext(
            "Specify the year for which the calendar is displayed, represented as a decimal integer from 1 to 9999"
        )
    )]
    year: Option<u32>,
}

fn print_month(month: u32, year: u32) {
    let month_name = gettext(MONTH_NAMES[(month - 1) as usize]);

    println!("    {} {}", month_name, year);
    println!("{}", gettext("Su Mo Tu We Th Fr Sa"));

    let days = days_in_month(year, month);
    let first_day = days[0];
    let start_weekday = day_of_week(year, month, first_day);

    // Print initial padding
    for _ in 0..start_weekday {
        print!("   ");
    }

    let mut current_weekday = start_weekday;
    for &day in &days {
        print!("{:2} ", day);
        current_weekday += 1;

        // Handle September 1752 gap: after day 2, next is day 14
        // We need to add padding for the missing days 3-13
        if year == 1752 && month == 9 && day == 2 {
            // After day 2 (Thursday), we jump to day 14 (Saturday)
            // Day 2 is at position 4 (Thursday), day 14 should be at position 6 (Saturday)
            // So we need to advance to Saturday position
            if current_weekday % 7 == 0 {
                println!();
            }
            // Add padding to move from Friday (position 5) to Saturday (position 6)
            // Actually, we need to calculate where day 14 falls
            let day14_weekday = day_of_week(1752, 9, 14);
            while current_weekday % 7 != day14_weekday {
                print!("   ");
                current_weekday += 1;
                if current_weekday % 7 == 0 {
                    println!();
                }
            }
            continue;
        }

        if current_weekday % 7 == 0 {
            println!();
        }
    }

    // Final newline if we didn't end on a Sunday
    if current_weekday % 7 != 0 {
        println!();
    }
}

fn print_year(year: u32) {
    for month in 1..=12 {
        print_month(month, year);
        println!();
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let mut args = Args::parse();

    // If no arguments are provided, display the current month
    if args.month.is_none() && args.year.is_none() {
        let now = chrono::Local::now();
        args.month = Some(now.month());
        args.year = Some(now.year() as u32);

    // If only one argument is provided, assume it is the entire year
    } else if args.month.is_some() && args.year.is_none() {
        args.year = args.month;
        args.month = None;
    }

    let year = args
        .year
        .expect("year should be set by argument parsing logic");

    if let Some(month) = args.month {
        if month > 12 {
            return Err(gettext("month must be between 1 and 12").into());
        }
        print_month(month, year);
    } else {
        print_year(year);
    }

    Ok(())
}
