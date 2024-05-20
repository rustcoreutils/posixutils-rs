//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::{Parser, ValueEnum};
use gettextrs::{bind_textdomain_codeset, textdomain};
use plib::PROJECT_NAME;
use regex::Regex;
use std::fs;
use std::io::{self, BufRead, Read};
use std::path::PathBuf;
use std::process::ExitCode;
use std::str::FromStr;

/// nl - line numbering filter
#[derive(Parser, Debug)]
#[command(author, version, about, long_about, disable_help_flag = true)]
struct Args {
    #[arg(long, action = clap::ArgAction::HelpLong)]
    help: Option<bool>,

    /// Specify which logical page body lines shall be numbered.
    ///
    /// - a - Number all lines.
    ///
    /// - t - Number only non-empty lines.
    ///
    /// - n - No line numbering.
    ///
    /// - pREGEX - Number only lines that contain the basic regular expression
    /// specified in *REGEX*.
    #[arg(short = 'b', long, default_value_t = LineNumberingStyle::NonEmpty)]
    body_numbering: LineNumberingStyle,

    /// Specify the delimiter characters that indicate the start of a logical
    /// page section. These can be changed from the default characters "\:" to
    /// two user-specified characters. If only one character is entered, the
    /// second character shall remain the default character ':'.
    #[arg(short = 'd', long, default_value_t = String::from("\\:"))]
    section_delimiter: String,

    /// Specify the same as b type except for footer.
    #[arg(short = 'f', long, default_value_t = LineNumberingStyle::None)]
    footer_numbering: LineNumberingStyle,

    /// Specify the same as b type except for header.
    #[arg(short = 'h', long, default_value_t = LineNumberingStyle::None)]
    header_numbering: LineNumberingStyle,

    /// Specify the increment value used to number logical page lines.
    #[arg(short = 'i', long, default_value_t = 1)]
    line_increment: i64,

    /// Specify the number of blank lines to be considered as one. For example,
    /// -l 2 results in only the second adjacent blank line being numbered (if
    /// the appropriate -h a, -b a, or -f a option is set).
    #[arg(short = 'l', long, default_value_t = 1, value_parser = clap::value_parser!(i64).range(1..))]
    join_blank_lines: i64,

    /// Specify the line numbering format. Recognized values are: ln, left
    /// justified, leading zeros suppressed; rn, right justified, leading zeros
    /// suppressed; rz, right justified, leading zeros kept.
    #[arg(short = 'n', long, default_value_t = NumberFormat::Rn)]
    number_format: NumberFormat,

    /// Specify that numbering should not be restarted at logical page
    /// delimiters.
    #[arg(short = 'p', long, default_value_t = false)]
    no_renumber: bool,

    /// Specify the characters used in separating the line number and the
    /// corresponding text line.
    #[arg(short = 's', long, default_value_t = String::from("\t"))]
    number_separator: String,

    /// Specify the initial value used to number logical page lines.
    #[arg(short = 'v', long, default_value_t = 1)]
    starting_line_number: i64,

    /// Specify the number of characters to be used for the line number.
    #[arg(short = 'w', long, default_value_t = 6, value_parser = clap::value_parser!(i64).range(1..))]
    number_width: i64,

    /// The standard input shall be used if no file operand is specified,
    /// and shall be used if the file operand is '-' and the implementation
    /// treats the '-' as meaning standard input.
    #[arg()]
    file: Option<PathBuf>,
}

#[derive(Debug, Clone)]
enum LineNumberingStyle {
    All,
    NonEmpty,
    None,
    Regex(Regex),
}

impl FromStr for LineNumberingStyle {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "a" => Ok(LineNumberingStyle::All),
            "t" => Ok(LineNumberingStyle::NonEmpty),
            "n" => Ok(LineNumberingStyle::None),
            s => {
                if let Some(re) = s.strip_prefix('p') {
                    if let Ok(regexp) = Regex::new(re) {
                        return Ok(LineNumberingStyle::Regex(regexp));
                    } else {
                        return Err(format!("invalid regular expression: {re}"));
                    }
                } else {
                    Err(format!("invalid variant: {s}"))
                }
            }
        }
    }
}

impl std::fmt::Display for LineNumberingStyle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LineNumberingStyle::All => write!(f, "a"),
            LineNumberingStyle::NonEmpty => write!(f, "t"),
            LineNumberingStyle::None => write!(f, "n"),
            LineNumberingStyle::Regex(re) => {
                write!(f, "p{}", re)
            }
        }
    }
}

#[derive(Debug, Clone, ValueEnum)]
enum NumberFormat {
    Ln,
    Rn,
    Rz,
}

impl std::fmt::Display for NumberFormat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NumberFormat::Ln => write!(f, "ln"),
            NumberFormat::Rn => write!(f, "rn"),
            NumberFormat::Rz => write!(f, "rz"),
        }
    }
}

fn nl_main(args: &Args) -> io::Result<()> {
    let readable: Box<dyn Read> = if let Some(path) = &args.file {
        if path.as_os_str() == "-" {
            Box::new(io::stdin().lock())
        } else {
            Box::new(fs::File::open(path)?)
        }
    } else {
        Box::new(io::stdin().lock())
    };
    let mut reader = io::BufReader::new(readable);

    let delimiter_header: String = (0..3).map(|_| args.section_delimiter.as_str()).collect();
    let delimiter_body: String = (0..2).map(|_| args.section_delimiter.as_str()).collect();
    let delimiter_footer: String = args.section_delimiter.clone();

    let mut line_buffer = String::new();
    let mut line_number = args.starting_line_number;
    let mut line_number_overflowed = false;
    let mut current_numbering_style = &args.body_numbering;
    let mut consecutive_blank_lines = 0;

    // Need to pass `line_number` and `line_number_overflowed` as arguments since
    // they are also being mutated outside the closure.
    let print_line_number =
        |line_number: &mut i64, line_number_overflowed: &mut bool| -> io::Result<()> {
            if *line_number_overflowed {
                return Err(io::Error::other("line number overflowed"));
            }

            match args.number_format {
                NumberFormat::Ln => print!(
                    "{:<width$}{}",
                    line_number,
                    args.number_separator,
                    width = args.number_width as usize
                ),
                NumberFormat::Rn => print!(
                    "{:>width$}{}",
                    line_number,
                    args.number_separator,
                    width = args.number_width as usize
                ),
                NumberFormat::Rz => print!(
                    "{:0>width$}{}",
                    line_number,
                    args.number_separator,
                    width = args.number_width as usize
                ),
            }
            if let Some(sum) = line_number.checked_add(args.line_increment) {
                *line_number = sum;
            } else {
                *line_number_overflowed = true;
            }

            Ok(())
        };

    let print_spacer = || {
        print!(
            "{:width$}",
            "",
            width = args.number_separator.len() + args.number_width as usize
        );
    };

    loop {
        line_buffer.clear();

        if let Ok(read_bytes) = reader.read_line(&mut line_buffer) {
            // EOF
            if read_bytes == 0 {
                break;
            }

            // Removing the newline makes for easier checks but it has to be
            // added back later
            if line_buffer.ends_with('\n') {
                line_buffer.pop();
            }

            // If we are in a header, body or footer
            let mut non_text = false;

            if line_buffer == delimiter_header {
                non_text = true;
                current_numbering_style = &args.header_numbering;
            } else if line_buffer == delimiter_body {
                non_text = true;
                current_numbering_style = &args.body_numbering;
            } else if line_buffer == delimiter_footer {
                non_text = true;
                current_numbering_style = &args.footer_numbering;
            } else {
                match current_numbering_style {
                    LineNumberingStyle::All => {
                        if args.join_blank_lines > 1 {
                            if !line_buffer.is_empty() {
                                print_line_number(&mut line_number, &mut line_number_overflowed)?;
                                consecutive_blank_lines = 0;
                            } else {
                                consecutive_blank_lines += 1;
                                if consecutive_blank_lines == args.join_blank_lines {
                                    print_line_number(
                                        &mut line_number,
                                        &mut line_number_overflowed,
                                    )?;
                                    consecutive_blank_lines = 0;
                                } else {
                                    print_spacer();
                                }
                            }
                        } else {
                            print_line_number(&mut line_number, &mut line_number_overflowed)?;
                        }
                    }
                    LineNumberingStyle::NonEmpty => {
                        if !line_buffer.is_empty() {
                            print_line_number(&mut line_number, &mut line_number_overflowed)?;
                        } else {
                            print_spacer();
                        }
                    }
                    LineNumberingStyle::None => print_spacer(),
                    LineNumberingStyle::Regex(regexp) => {
                        if regexp.is_match(&line_buffer) {
                            print_line_number(&mut line_number, &mut line_number_overflowed)?;
                        } else {
                            print_spacer();
                        }
                    }
                }

                // Reference `nl` unconditionally adds a newline even on files
                // not ending on a newline
                line_buffer.push('\n');
                print!("{}", &line_buffer);
            }

            if non_text {
                if !args.no_renumber {
                    line_number = args.starting_line_number;
                    line_number_overflowed = false;
                }
                println!("");
            }
        } else {
            break;
        }
    }

    Ok(())
}

fn main() -> ExitCode {
    let mut args = Args::parse();

    match args.section_delimiter.len() {
        1 => {
            args.section_delimiter.push(':');
        }
        2 => (),
        _ => {
            // Delimiter should be at most 2 characters.
            return ExitCode::from(1);
        }
    }

    // Initialize translation system
    textdomain(PROJECT_NAME).unwrap();
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8").unwrap();

    match nl_main(&args) {
        Ok(_) => ExitCode::from(0),
        Err(_) => ExitCode::from(1),
    }
}
