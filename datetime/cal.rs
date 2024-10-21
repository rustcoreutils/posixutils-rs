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
use tr::tr;

#[derive(Parser)]
#[command(version, about = tr!("cal - print a calendar"))]
struct Args {
    #[arg(
        value_parser = clap::value_parser!(u32).range(1..),
        help = tr!(
            "Specify the month to be displayed, represented as a decimal integer from 1 (January) to 12 (December)"
        )
    )]
    month: Option<u32>,

    #[arg(
        value_parser = clap::value_parser!(u32).range(1..),
        help = tr!(
            "Specify the year for which the calendar is displayed, represented as a decimal integer from 1 to 9999"
        )
    )]
    year: Option<u32>,
}

fn print_month(month: u32, year: u32) {
    let month_name = match month {
        1 => tr!("January"),
        2 => tr!("February"),
        3 => tr!("March"),
        4 => tr!("April"),
        5 => tr!("May"),
        6 => tr!("June"),
        7 => tr!("July"),
        8 => tr!("August"),
        9 => tr!("September"),
        10 => tr!("October"),
        11 => tr!("November"),
        12 => tr!("December"),
        _ => unreachable!(),
    };

    println!("{} {}", month_name, year);
    println!("{}", tr!("Su Mo Tu We Th Fr Sa"));

    let mut day = 1;
    let mut weekday = 1;
    let mut days_in_month = 31;
    if month == 4 || month == 6 || month == 9 || month == 11 {
        days_in_month = 30;
    } else if month == 2 {
        if year % 4 == 0 && (year % 100 != 0 || year % 400 == 0) {
            days_in_month = 29;
        } else {
            days_in_month = 28;
        }
    }

    while day <= days_in_month {
        print!("{:2}", day);
        day += 1;
        weekday += 1;
        if weekday > 7 {
            println!();
            weekday = 1;
        } else {
            print!(" ");
        }
    }

    if weekday != 1 {
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
    plib::initialize_i18n!()?;

    // parse command line arguments
    let mut args = Args::parse();

    // If no arguments are provided, display the current month
    if args.month.is_none() && args.year.is_none() {
        let now = chrono::Local::now();
        args.month = Some(now.month() as u32);
        args.year = Some(now.year() as u32);

    // If only one argument is provided, assume it is the entire year
    } else if args.month.is_some() && args.year.is_none() {
        args.year = args.month;
        args.month = None;
    }

    let year = match args.year {
        Some(year) => {
            if year > 9999 {
                return Err(tr!("year must be between 1 and 9999").into());
            }
            year
        }
        None => unreachable!(),
    };

    if let Some(month) = args.month {
        if month > 12 {
            return Err(tr!("month must be between 1 and 12").into());
        }
        print_month(month, year);
    } else {
        print_year(year);
    }

    Ok(())
}
