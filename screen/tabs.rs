//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::io::{self, Write};
use std::process::ExitCode;

use clap::Parser;
use gettextrs::{LocaleCategory, bind_textdomain_codeset, gettext, setlocale, textdomain};
use terminfo::{Database, capability as cap};

// POSIX doesn't specify a maximum, but terminals have practical limits.
// 160 columns is a reasonable upper bound for most terminals.
const MAX_COLUMN: u16 = 160;

/// Pre-process command line arguments to handle POSIX multi-character options.
/// Converts -a2 -> --a2, -c2 -> --c2, -c3 -> --c3, and -0 -> --rep-0
fn preprocess_args() -> Vec<String> {
    std::env::args()
        .map(|arg| match arg.as_str() {
            "-a2" => "--a2".to_string(),
            "-c2" => "--c2".to_string(),
            "-c3" => "--c3".to_string(),
            "-0" => "--rep-0".to_string(),
            _ => arg,
        })
        .collect()
}

#[derive(Parser)]
#[command(version, about = gettext("tabs - set terminal tabs"))]
struct Args {
    #[arg(short = 'T', long, help = gettext("Indicate the type of terminal"))]
    term: Option<String>,

    // Repetitive tab stops -0 through -9
    #[arg(long = "rep-0", help = gettext("Clear all tab stops"))]
    rep_0: bool,

    #[arg(short = '1', help = gettext("Tab stops every 1 column"))]
    rep_1: bool,

    #[arg(short = '2', help = gettext("Tab stops every 2 columns"))]
    rep_2: bool,

    #[arg(short = '3', help = gettext("Tab stops every 3 columns"))]
    rep_3: bool,

    #[arg(short = '4', help = gettext("Tab stops every 4 columns"))]
    rep_4: bool,

    #[arg(short = '5', help = gettext("Tab stops every 5 columns"))]
    rep_5: bool,

    #[arg(short = '6', help = gettext("Tab stops every 6 columns"))]
    rep_6: bool,

    #[arg(short = '7', help = gettext("Tab stops every 7 columns"))]
    rep_7: bool,

    #[arg(short = '8', help = gettext("Tab stops every 8 columns"))]
    rep_8: bool,

    #[arg(short = '9', help = gettext("Tab stops every 9 columns"))]
    rep_9: bool,

    // XSI language preset options
    #[arg(short = 'a', help = gettext("Assembler: 1,10,16,36,72"))]
    assembler: bool,

    #[arg(long = "a2", help = gettext("Assembler variant: 1,10,16,40,72"))]
    assembler2: bool,

    #[arg(short = 'c', help = gettext("COBOL: 1,8,12,16,20,55"))]
    cobol: bool,

    #[arg(long = "c2", help = gettext("COBOL compact: 1,6,10,14,49"))]
    cobol2: bool,

    #[arg(long = "c3", help = gettext("COBOL compact extended: 1,6,10,14,18,22,26,30,34,38,42,46,50,54,58,62,67"))]
    cobol3: bool,

    #[arg(short = 'f', help = gettext("FORTRAN: 1,7,11,15,19,23"))]
    fortran: bool,

    #[arg(short = 'p', help = gettext("PL/1: 1,5,9,13,17,21,25,29,33,37,41,45,49,53,57,61"))]
    pl1: bool,

    #[arg(short = 's', help = gettext("SNOBOL: 1,10,55"))]
    snobol: bool,

    #[arg(short = 'u', help = gettext("Assembler: 1,12,20,44"))]
    assembler_u: bool,

    #[arg(help = gettext("Tab-stop values (comma or blank separated, +N for increments)"))]
    tabstops: Option<String>,
}

/// Parse tab stop specification from operand string.
/// Supports comma and blank separators, and +N increment notation.
fn parse_tabstops(spec: &str) -> Result<Vec<u16>, String> {
    let mut result = Vec::new();
    let mut prev_value: u16 = 0;

    // Split on comma or whitespace
    for token in spec.split(|c: char| c == ',' || c.is_whitespace()) {
        let token = token.trim();
        if token.is_empty() {
            continue;
        }

        let (is_increment, num_str) = if let Some(stripped) = token.strip_prefix('+') {
            (true, stripped)
        } else {
            (false, token)
        };

        let value: u16 = num_str
            .parse()
            .map_err(|_| format!("{}: '{}'", gettext("invalid tab stop specification"), token))?;

        if value == 0 && !is_increment {
            return Err(gettext("tab stop must be positive").to_string());
        }

        let absolute_value = if is_increment {
            prev_value
                .checked_add(value)
                .ok_or_else(|| gettext("tab stop value overflow").to_string())?
        } else {
            value
        };

        // POSIX: tab stops must be in strictly ascending order
        if !result.is_empty() && absolute_value <= prev_value {
            return Err(gettext("tab stops must be in strictly ascending order").to_string());
        }

        result.push(absolute_value);
        prev_value = absolute_value;
    }

    if result.is_empty() {
        return Err(gettext("no tab stops specified").to_string());
    }

    Ok(result)
}

/// Generate repetitive tab stops at a given interval.
fn generate_repetitive_tabs(interval: u16) -> Vec<u16> {
    let mut result = Vec::new();
    let mut pos = interval;

    while pos <= MAX_COLUMN {
        result.push(pos);
        pos = match pos.checked_add(interval) {
            Some(p) => p,
            None => break,
        };
    }

    result
}

/// Determine tab stops from command line arguments.
fn parse_cmd_line(args: &Args) -> Result<Vec<u16>, String> {
    // Check for repetitive tab stop options (-0 through -9)
    if args.rep_0 {
        // -0: clear tabs, set none
        return Ok(Vec::new());
    }
    if args.rep_1 {
        return Ok(generate_repetitive_tabs(1));
    }
    if args.rep_2 {
        return Ok(generate_repetitive_tabs(2));
    }
    if args.rep_3 {
        return Ok(generate_repetitive_tabs(3));
    }
    if args.rep_4 {
        return Ok(generate_repetitive_tabs(4));
    }
    if args.rep_5 {
        return Ok(generate_repetitive_tabs(5));
    }
    if args.rep_6 {
        return Ok(generate_repetitive_tabs(6));
    }
    if args.rep_7 {
        return Ok(generate_repetitive_tabs(7));
    }
    if args.rep_8 {
        return Ok(generate_repetitive_tabs(8));
    }
    if args.rep_9 {
        return Ok(generate_repetitive_tabs(9));
    }

    // Check for XSI language preset options
    if args.assembler {
        return Ok(vec![1, 10, 16, 36, 72]);
    }
    if args.assembler2 {
        return Ok(vec![1, 10, 16, 40, 72]);
    }
    if args.cobol {
        return Ok(vec![1, 8, 12, 16, 20, 55]);
    }
    if args.cobol2 {
        return Ok(vec![1, 6, 10, 14, 49]);
    }
    if args.cobol3 {
        return Ok(vec![
            1, 6, 10, 14, 18, 22, 26, 30, 34, 38, 42, 46, 50, 54, 58, 62, 67,
        ]);
    }
    if args.fortran {
        return Ok(vec![1, 7, 11, 15, 19, 23]);
    }
    if args.pl1 {
        return Ok(vec![
            1, 5, 9, 13, 17, 21, 25, 29, 33, 37, 41, 45, 49, 53, 57, 61,
        ]);
    }
    if args.snobol {
        return Ok(vec![1, 10, 55]);
    }
    if args.assembler_u {
        return Ok(vec![1, 12, 20, 44]);
    }

    // Check for custom tab stop specification
    if let Some(ref tabstops_str) = args.tabstops {
        return parse_tabstops(tabstops_str);
    }

    // POSIX default: equivalent to -8
    Ok(generate_repetitive_tabs(8))
}

/// Set hardware tabs using terminfo capabilities.
fn set_hw_tabs(info: &Database, tabstops: &[u16]) -> io::Result<()> {
    let clear_cap = info.get::<cap::ClearAllTabs>();
    let set_cap = info.get::<cap::SetTab>();

    if clear_cap.is_none() || set_cap.is_none() {
        let msg = gettext("terminal does not support setting tab stops");
        return Err(io::Error::other(msg));
    }
    let clear_cap = clear_cap.unwrap();
    let set_cap = set_cap.unwrap();

    // Clear existing tabs
    if let Err(e) = clear_cap.expand().to(io::stdout()) {
        let msg = format!("{}: {}", gettext("failed to clear tabs"), e);
        return Err(io::Error::other(msg));
    }

    // Set new tabs (if any - might be empty for -0)
    let mut col: u16 = 0;
    for &stop in tabstops {
        while col < stop {
            io::stdout().write_all(b" ")?;
            col += 1;
        }

        if let Err(e) = set_cap.expand().to(io::stdout()) {
            let msg = format!("{}: {}", gettext("failed to set tab stop"), e);
            return Err(io::Error::other(msg));
        }
    }

    // Carriage return to reset cursor position
    io::stdout().write_all(b"\r")?;
    io::stdout().flush()?;

    Ok(())
}

fn main() -> ExitCode {
    setlocale(LocaleCategory::LcAll, "");
    if let Err(e) = textdomain("posixutils-rs") {
        eprintln!("textdomain: {}", e);
    }
    if let Err(e) = bind_textdomain_codeset("posixutils-rs", "UTF-8") {
        eprintln!("bind_textdomain_codeset: {}", e);
    }

    let preprocessed_args = preprocess_args();
    let args = match Args::try_parse_from(&preprocessed_args) {
        Ok(args) => args,
        Err(e) => {
            // Handle --help and --version specially (they exit with 0)
            if e.kind() == clap::error::ErrorKind::DisplayHelp
                || e.kind() == clap::error::ErrorKind::DisplayVersion
            {
                print!("{}", e);
                return ExitCode::SUCCESS;
            }
            eprintln!("{}", e);
            return ExitCode::from(1);
        }
    };

    // Get terminal database
    let info = match &args.term {
        Some(termtype) => match Database::from_name(termtype) {
            Ok(db) => db,
            Err(e) => {
                eprintln!(
                    "{}: {}: {}",
                    gettext("tabs"),
                    gettext("unknown terminal type"),
                    e
                );
                return ExitCode::from(1);
            }
        },
        None => match Database::from_env() {
            Ok(db) => db,
            Err(e) => {
                eprintln!(
                    "{}: {}: {}",
                    gettext("tabs"),
                    gettext("cannot determine terminal type"),
                    e
                );
                return ExitCode::from(1);
            }
        },
    };

    // Parse tab stops from arguments
    let tabstops = match parse_cmd_line(&args) {
        Ok(stops) => stops,
        Err(e) => {
            eprintln!("{}: {}", gettext("tabs"), e);
            return ExitCode::from(1);
        }
    };

    // Set the hardware tabs
    if let Err(e) = set_hw_tabs(&info, &tabstops) {
        eprintln!("{}: {}", gettext("tabs"), e);
        return ExitCode::from(1);
    }

    ExitCode::SUCCESS
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_tabstops_comma_separated() {
        let result = parse_tabstops("1,10,20,30").unwrap();
        assert_eq!(result, vec![1, 10, 20, 30]);
    }

    #[test]
    fn test_parse_tabstops_blank_separated() {
        let result = parse_tabstops("1 10 20 30").unwrap();
        assert_eq!(result, vec![1, 10, 20, 30]);
    }

    #[test]
    fn test_parse_tabstops_mixed_separators() {
        let result = parse_tabstops("1,10 20,30").unwrap();
        assert_eq!(result, vec![1, 10, 20, 30]);
    }

    #[test]
    fn test_parse_tabstops_with_increments() {
        let result = parse_tabstops("1 10 +10 +10").unwrap();
        assert_eq!(result, vec![1, 10, 20, 30]);
    }

    #[test]
    fn test_parse_tabstops_mixed_absolute_and_increment() {
        let result = parse_tabstops("1,10,+5,+5").unwrap();
        assert_eq!(result, vec![1, 10, 15, 20]);
    }

    #[test]
    fn test_parse_tabstops_not_ascending() {
        let result = parse_tabstops("10,5,20");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_tabstops_invalid_number() {
        let result = parse_tabstops("1,abc,10");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_tabstops_zero_value() {
        let result = parse_tabstops("0,10,20");
        assert!(result.is_err());
    }

    #[test]
    fn test_generate_repetitive_tabs() {
        let result = generate_repetitive_tabs(8);
        assert_eq!(result[0], 8);
        assert_eq!(result[1], 16);
        assert_eq!(result[2], 24);
        assert!(result.len() >= 10); // Should have at least 10 stops
    }

    #[test]
    fn test_generate_repetitive_tabs_1() {
        let result = generate_repetitive_tabs(1);
        assert_eq!(result[0], 1);
        assert_eq!(result[1], 2);
        assert!(result.len() >= MAX_COLUMN as usize);
    }
}
