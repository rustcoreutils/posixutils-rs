//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::locale::isblank;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Write};
use std::path::PathBuf;

/// uniq - report or filter out repeated lines in a file
#[derive(Parser)]
#[command(version, about = gettext("uniq - report or filter out repeated lines in a file"))]
struct Args {
    #[arg(short = 'c', help = gettext("Count the number of repeated lines"))]
    count: bool,

    #[arg(short = 'd', help = gettext("Print only the repeated lines"))]
    repeated: bool,

    #[arg(short = 'u', help = gettext("Print only unique lines"))]
    unique: bool,

    #[arg(short = 'f', help = gettext("Ignore the first fields fields on each input line"))]
    fields: Option<usize>,

    #[arg(short = 's', help = gettext("Ignore the first chars characters on each input line"))]
    chars: Option<usize>,

    #[arg(help = gettext("Input file (if not specified, use stdin)"))]
    input_file: Option<PathBuf>,

    #[arg(help = gettext("Output file (if not specified, use stdout)"))]
    output_file: Option<PathBuf>,
}

impl Args {
    /// Validates the arguments to ensure no conflicting options are used together.
    ///
    /// # Errors
    ///
    /// Returns an error if conflicting options are found.
    fn validate_args(&self) -> Result<(), String> {
        // Check if conflicting options are used together
        if self.unique && self.repeated {
            return Err("Options '-u' and '-d' cannot be used together".to_string());
        }
        if self.count && self.repeated {
            return Err("Options '-c' and '-d' cannot be used together".to_string());
        }
        if self.count && self.unique {
            return Err("Options '-c' and '-u' cannot be used together".to_string());
        }
        Ok(())
    }
}

/// Processes the input according to the specified arguments and writes the output.
///
/// # Arguments
///
/// * `args` - A reference to the `Args` struct containing the command line arguments.
///
/// # Errors
///
/// Returns an error if there is an issue reading the input or writing the output.
fn uniq(args: &Args) -> Result<(), Box<dyn std::error::Error>> {
    let input: Box<dyn BufRead> = match &args.input_file {
        Some(file) => {
            if file.as_os_str() == "-" {
                Box::new(BufReader::new(io::stdin()))
            } else {
                Box::new(BufReader::new(File::open(file)?))
            }
        }
        None => Box::new(BufReader::new(io::stdin())),
    };

    // A "-" output_file operand means standard output (POSIX), not a file
    // literally named "-".
    let mut output: Box<dyn Write> = match &args.output_file {
        Some(file) if file.as_os_str() != "-" => Box::new(File::create(file)?),
        _ => Box::new(io::stdout()),
    };

    let lines: Vec<String> = input.lines().collect::<Result<_, _>>()?;

    // Track the first line of the current run plus its comparison key; the key
    // is computed once per line (not recomputed for the previous line each
    // iteration).
    let mut current: Option<(String, String)> = None;
    let mut current_count = 0;

    for line in &lines {
        let key = process_line(line, args.fields, args.chars);

        match &current {
            Some((_, last_key)) if *last_key == key => {
                current_count += 1;
            }
            Some((first_line, _)) => {
                output_result(&mut output, first_line, current_count, args)?;
                current = Some((line.clone(), key));
                current_count = 1;
            }
            None => {
                current = Some((line.clone(), key));
                current_count = 1;
            }
        }
    }

    if let Some((first_line, _)) = current {
        output_result(&mut output, &first_line, current_count, args)?;
    }
    Ok(())
}

/// Returns the comparison key for `line`: the remainder after skipping the
/// first `fields` fields and then the first `chars` characters.
///
/// A field is `[[:blank:]]*[^[:blank:]]*` — leading `<blank>`s belong to the
/// field, followed by the run of non-blanks (POSIX). Character skipping uses
/// character counts, not byte offsets, so multibyte input does not panic. When
/// the skips consume the whole line, the key is the empty string (a null
/// string), so two lines that reduce to nothing compare equal.
fn process_line(line: &str, fields: Option<usize>, chars: Option<usize>) -> String {
    let mut start = 0; // byte offset into `line`

    if let Some(f) = fields {
        let mut it = line.char_indices().peekable();
        for _ in 0..f {
            // Skip leading blanks of this field.
            while let Some(&(i, c)) = it.peek() {
                if isblank(c) {
                    it.next();
                    start = i + c.len_utf8();
                } else {
                    break;
                }
            }
            // Skip the non-blank run of this field.
            while let Some(&(i, c)) = it.peek() {
                if isblank(c) {
                    break;
                }
                it.next();
                start = i + c.len_utf8();
            }
            if it.peek().is_none() {
                break;
            }
        }
    }

    let mut rest = &line[start..];

    if let Some(c) = chars {
        rest = match rest.char_indices().nth(c) {
            Some((i, _)) => &rest[i..],
            None => "", // skipped past the end: null comparison string
        };
    }

    rest.to_string()
}

/// Writes the result to the output according to the specified arguments.
///
/// # Arguments
///
/// * `output` - The output writer.
/// * `line` - The line to be written.
/// * `count` - The count of the line occurrences.
/// * `args` - A reference to the `Args` struct containing the command line arguments.
///
/// # Errors
///
/// Returns an error if there is an issue writing to the output.
#[allow(clippy::if_same_then_else)]
fn output_result<W: Write>(
    output: &mut W,
    line: &str,
    count: usize,
    args: &Args,
) -> Result<(), io::Error> {
    if args.count {
        writeln!(output, "{} {}", count, line)?;
    } else if args.repeated && count > 1 {
        writeln!(output, "{}", line)?;
    } else if args.unique && count == 1 {
        writeln!(output, "{}", line)?;
    } else if !args.repeated && !args.unique {
        writeln!(output, "{}", line)?;
    }
    Ok(())
}

/// The main function that initializes the application, parses the arguments, and runs the uniq function.
///
/// # Errors
///
/// Returns an error if there is an issue with the arguments or the uniq function.
fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    args.validate_args()?;

    let mut exit_code = 0;

    if let Err(err) = uniq(&args) {
        exit_code = 1;
        eprintln!("{}", err);
    }

    std::process::exit(exit_code)
}
