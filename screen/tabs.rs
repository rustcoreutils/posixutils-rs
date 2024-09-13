//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// TODO:
// - (efficiency): collect all output in a buffer, then write to stdout
// - Research if 100 is a POSIX-compliant limit for MAX_STOPS
//

extern crate clap;
extern crate plib;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::PROJECT_NAME;
use std::io::{self, Error, ErrorKind, Write};
use terminfo::{capability as cap, Database};

// arbitrarily chosen.  todo: search if POSIX-ly correct.
const MAX_STOPS: usize = 100;

/// tabs - set terminal tabs
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Indicate the type of terminal.
    #[arg(short = 'T', long)]
    term: Option<String>,

    /// Specify repetitive tab stops separated by (1) columns
    #[arg(short = '1', long)]
    rep_1: bool,

    /// Specify repetitive tab stops separated by (2) columns
    #[arg(short = '2', long)]
    rep_2: bool,

    /// Specify repetitive tab stops separated by (3) columns
    #[arg(short = '3', long)]
    rep_3: bool,

    /// Specify repetitive tab stops separated by (4) columns
    #[arg(short = '4', long)]
    rep_4: bool,

    /// Specify repetitive tab stops separated by (5) columns
    #[arg(short = '5', long)]
    rep_5: bool,

    /// Specify repetitive tab stops separated by (6) columns
    #[arg(short = '6', long)]
    rep_6: bool,

    /// Specify repetitive tab stops separated by (7) columns
    #[arg(short = '7', long)]
    rep_7: bool,

    /// Specify repetitive tab stops separated by (8) columns
    #[arg(short = '8', long)]
    rep_8: bool,

    /// Specify repetitive tab stops separated by (9) columns
    #[arg(short = '9', long)]
    rep_9: bool,

    /// Assembler, applicable to some mainframes.
    /// 1=[1,10,16,36,72]
    /// 2=[1,10,16,40,72]
    #[arg(short, long, default_missing_value="1", value_parser = clap::value_parser!(u8).range(1..=2))]
    assembler: Option<u8>,

    /// COBOL, normal and compact formats.
    /// 1=[1,8,12,16,20,55]
    /// 2=[1,6,10,14,49]
    /// 3=[1,6,10,14,18,22,26,30,34,38,42,46,50,54,58,62,67]
    #[arg(short, long, default_missing_value="1", value_parser = clap::value_parser!(u8).range(1..=3))]
    cobol: Option<u8>,

    /// FORTAN: [1,7,11,15,19,23]
    #[arg(short, long)]
    fortran: bool,

    /// SNOBOL: [1,10,55]
    #[arg(short, long)]
    snobol: bool,

    /// Assembler, applicable to some mainframes. [1,12,20,44]
    #[arg(short = 'u')]
    assembler_u: bool,

    /// PL/1: [1,5,9,13,17,21,25,29,33,37,41,45,49,53,57,61]
    #[arg(short, long)]
    pl1: bool,

    /// Optional: A single command line argument that consists of one or more tab-stop values (n) separated by a separator character
    tabstops: Option<String>,
}

fn parse_cmd_line(args: &Args) -> Result<Vec<u16>, &'static str> {
    let mut tabstops: Vec<u16> = Vec::new();
    let mut repeating_stop: Option<u16> = None;

    if args.rep_9 {
        repeating_stop = Some(9);
    } else if args.rep_8 {
        repeating_stop = Some(8);
    } else if args.rep_7 {
        repeating_stop = Some(7);
    } else if args.rep_6 {
        repeating_stop = Some(6);
    } else if args.rep_5 {
        repeating_stop = Some(5);
    } else if args.rep_4 {
        repeating_stop = Some(4);
    } else if args.rep_3 {
        repeating_stop = Some(3);
    } else if args.rep_2 {
        repeating_stop = Some(2);
    } else if args.rep_1 {
        repeating_stop = Some(1);
    } else if let Some(variant) = args.assembler {
        match variant {
            1 => tabstops = vec![1, 10, 16, 36, 72],
            2 => tabstops = vec![1, 10, 16, 40, 72],
            _ => unreachable!(),
        }
    } else if let Some(variant) = args.cobol {
        match variant {
            1 => tabstops = vec![1, 8, 12, 16, 20, 55],
            2 => tabstops = vec![1, 6, 10, 14, 49],
            3 => {
                tabstops = vec![
                    1, 6, 10, 14, 18, 22, 26, 30, 34, 38, 42, 46, 50, 54, 58, 62, 67,
                ]
            }
            _ => unreachable!(),
        }
    } else if args.fortran {
        tabstops = vec![1, 7, 11, 15, 19, 23];
    } else if args.pl1 {
        tabstops = vec![1, 5, 9, 13, 17, 21, 25, 29, 33, 37, 41, 45, 49, 53, 57, 61];
    } else if args.snobol {
        tabstops = vec![1, 10, 55];
    } else if args.assembler_u {
        tabstops = vec![1, 12, 20, 44];
    } else if let Some(ref tabstops_str) = args.tabstops {
        for stop in tabstops_str.split(',') {
            tabstops.push(
                stop.parse()
                    .unwrap_or_else(|_| { panic!("{}", gettext("Invalid tabstop value.")) }),
            );
        }
    }

    // handle repetitive tab stops
    if let Some(stop_n) = repeating_stop {
        for _i in 0..MAX_STOPS {
            tabstops.push(stop_n);
        }
    }

    // validate that stops are in strictly ascending order
    for i in 1..tabstops.len() {
        if tabstops[i] <= tabstops[i - 1] {
            return Err("Tabstops must be in strictly ascending order.");
        }
    }

    Ok(tabstops)
}

// set hardware tabs.
fn set_hw_tabs(info: &Database, tabstops: &Vec<u16>) -> io::Result<()> {
    let clear_cap = info.get::<cap::ClearAllTabs>();
    let set_cap = info.get::<cap::SetTab>();

    if clear_cap.is_none() || set_cap.is_none() {
        let msg = gettext("Terminal does not support hardware tabs.");
        return Err(Error::new(ErrorKind::Other, msg));
    }
    let clear_cap = clear_cap.unwrap();
    let set_cap = set_cap.unwrap();

    // clear existing tabs
    if let Err(e) = clear_cap.expand().to(io::stdout()) {
        let msg = format!("{}: {}", gettext("Failed to clear tabs"), e);
        return Err(Error::new(ErrorKind::Other, msg));
    }

    // set new tabs
    let mut col = 0;
    for stop in tabstops {
        let stop = *stop as usize;

        while col < stop {
            io::stdout().write_all(b" ")?;
            col += 1;
        }

        if let Err(e) = set_cap.expand().to(io::stdout()) {
            let msg = format!("{}: {}", gettext("Failed to set tab"), e);
            return Err(Error::new(ErrorKind::Other, msg));
        }
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let info = match args.term {
        None => Database::from_env().unwrap(),
        Some(ref termtype) => Database::from_name(termtype).unwrap(),
    };

    let tabstops = parse_cmd_line(&args)?;
    set_hw_tabs(&info, &tabstops)?;

    Ok(())
}
