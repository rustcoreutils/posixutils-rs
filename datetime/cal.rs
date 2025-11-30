//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// TODO:
// - Gregorian if >= Sept 1752, otherwise Julian
// - Arg help should indicate "[[month] year]" as the default
//

use chrono::Datelike;
use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};

#[derive(Parser)]
#[command(version, about = gettext("cal - print a calendar"))]
struct Args {
    #[arg(
        value_parser = clap::value_parser!(u32).range(1..),
        help = gettext(
            "Specify the month to be displayed, represented as a decimal integer from 1 (January) to 12 (December)"
        )
    )]
    month: Option<u32>,

    #[arg(
        value_parser = clap::value_parser!(u32).range(1..),
        help = gettext(
            "Specify the year for which the calendar is displayed, represented as a decimal integer from 1 to 9999"
        )
    )]
    year: Option<u32>,
}

fn print_month(month: u32, year: u32) {
    let month_name = match month {
        1 => gettext("January"),
        2 => gettext("February"),
        3 => gettext("March"),
        4 => gettext("April"),
        5 => gettext("May"),
        6 => gettext("June"),
        7 => gettext("July"),
        8 => gettext("August"),
        9 => gettext("September"),
        10 => gettext("October"),
        11 => gettext("November"),
        12 => gettext("December"),
        _ => unreachable!(),
    };

    println!("    {} {}", month_name, year);
    println!("{}", gettext("Su Mo Tu We Th Fr Sa"));

    let first_day = chrono::NaiveDate::from_ymd_opt(year as i32, month, 1).unwrap();
    let start_weekday = first_day.weekday().num_days_from_sunday(); // 0 (Sun) to 6 (Sat)

    let days_in_month = match month {
        4 | 6 | 9 | 11 => 30,
        2 => {
            if year % 4 == 0 && (year % 100 != 0 || year % 400 == 0) {
                29
            } else {
                28
            }
        }
        _ => 31,
    };

    // Print initial padding
    for _ in 0..start_weekday {
        print!("   ");
    }

    for day in 1..=days_in_month {
        print!("{:2} ", day);
        if (start_weekday + day) % 7 == 0 {
            println!();
        }
    }

    println!();
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

    let year = match args.year {
        Some(year) => {
            if year > 9999 {
                return Err(gettext("year must be between 1 and 9999").into());
            }
            year
        }
        None => unreachable!(),
    };

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
