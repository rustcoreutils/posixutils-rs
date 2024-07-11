//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

extern crate clap;
extern crate libc;
extern crate plib;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, textdomain};
use libc::{regcomp, regex_t, regexec, regfree, REG_EXTENDED, REG_ICASE, REG_NOMATCH};
use plib::PROJECT_NAME;
use std::{
    ffi::CString,
    fs::File,
    io::{self, BufRead, BufReader},
    path::{Path, PathBuf},
    ptr,
};

/// grep - search a file for a pattern.
#[derive(Parser)]
#[command(author, version, about, long_about)]
struct Args {
    /// Match using extended regular expressions.
    #[arg(short = 'E', long)]
    extended_regexp: bool,

    /// Match using fixed strings.
    #[arg(short = 'F', long)]
    fixed_strings: bool,

    /// Write only a count of selected lines to standard output.
    #[arg(short, long)]
    count: bool,

    /// Specify one or more patterns to be used during the search for input.
    #[arg(short = 'e', long)]
    regexp: Vec<String>,

    /// Read one or more patterns from the file named by the pathname *file*.
    #[arg(short, long)]
    file: Vec<PathBuf>,

    /// Perform pattern matching in searches without regard to case.
    #[arg(short, long)]
    ignore_case: bool,

    /// Write only the names of input_files containing selected lines to standard output.
    #[arg(short = 'l', long)]
    files_with_matches: bool,

    /// Precede each output line by its relative line number in the file, each file starting at line 1.
    #[arg(short = 'n', long)]
    line_number: bool,

    /// Write only the names of input_files containing selected lines to standard output.
    #[arg(short, long)]
    quiet: bool,

    /// Suppress the error messages ordinarily written for nonexistent or unreadable input_files.
    #[arg(short = 's', long)]
    no_messages: bool,

    /// Select lines not matching any of the specified patterns.
    #[arg(short = 'v', long)]
    invert_match: bool,

    /// Consider only input lines that use all characters in the line excluding the terminating
    /// <newline> to match an entire fixed string or regular expression to be matching lines.
    #[arg(short = 'x', long)]
    line_regexp: bool,

    /// Specify one or more patterns to be used during the search for input. This operand shall be
    /// treated as if it were specified as -e regexp.
    #[arg(name = "PATTERNS")]
    single_pattern: Option<String>,

    /// A pathname of a file to be searched for the patterns. If no file operands are specified, the
    /// standard input shall be used.
    #[arg(name = "FILE")]
    input_files: Vec<String>,

    #[arg(skip)]
    any_errors: bool,
}

impl Args {
    /// Validates the arguments to ensure no conflicting options are used together.
    ///
    /// # Errors
    ///
    /// Returns an error if conflicting options are found.
    fn validate_args(&self) -> Result<(), String> {
        if self.extended_regexp && self.fixed_strings {
            return Err("Options '-E' and '-F' cannot be used together".to_string());
        }
        if self.count && self.files_with_matches {
            return Err("Options '-c' and '-l' cannot be used together".to_string());
        }
        if self.count && self.quiet {
            return Err("Options '-c' and '-q' cannot be used together".to_string());
        }
        if self.files_with_matches && self.quiet {
            return Err("Options '-l' and '-q' cannot be used together".to_string());
        }
        if self.regexp.is_empty() && self.file.is_empty() && self.single_pattern.is_none() {
            return Err("Required at least one pattern list or file".to_string());
        }
        Ok(())
    }

    /// Resolves input patterns and input files. Reads patterns from pattern files and merges them with specified as argument. Handles input files if empty.
    fn resolve(&mut self) {
        for path_buf in &self.file {
            match Self::get_file_patterns(path_buf) {
                Ok(patterns) => self.regexp.extend(patterns),
                Err(err) => {
                    self.any_errors = true;
                    if !self.no_messages {
                        eprintln!("{}: {}", path_buf.display(), err);
                    }
                }
            }
        }

        match &self.single_pattern {
            None => {}
            Some(pattern) => {
                if !self.regexp.is_empty() {
                    self.input_files.insert(0, pattern.clone());
                } else {
                    self.regexp = vec![pattern.clone()];
                }
            }
        }

        self.regexp = self
            .regexp
            .iter()
            .flat_map(|pattern| pattern.split('\n').map(String::from))
            .collect();

        if self.input_files.is_empty() {
            self.input_files.push(String::from("-"))
        }
    }

    /// Reads patterns from file.
    ///
    /// # Arguments
    ///
    /// * `path` - object that implements [AsRef](AsRef) for [Path](Path) and describes file that contains patterns.
    ///
    /// # Errors
    ///
    /// Returns an error if there is an issue reading the file.
    fn get_file_patterns<P: AsRef<Path>>(path: P) -> Result<Vec<String>, io::Error> {
        BufReader::new(File::open(&path)?)
            .lines()
            .collect::<Result<Vec<_>, _>>()
    }

    /// Maps [Args](Args) object into [GrepModel](GrepModel).
    ///
    /// # Returns
    ///
    /// Returns [GrepModel](GrepModel) object.
    fn into_grep_model(self) -> Result<GrepModel, String> {
        let output_mode = if self.count {
            OutputMode::Count(0)
        } else if self.files_with_matches {
            OutputMode::FilesWithMatches
        } else if self.quiet {
            OutputMode::Quiet
        } else {
            OutputMode::Default
        };

        let patterns = Patterns::new(
            self.regexp,
            self.extended_regexp,
            self.fixed_strings,
            self.ignore_case,
            self.line_regexp,
        )?;

        Ok(GrepModel {
            any_matches: false,
            any_errors: self.any_errors,
            line_number: self.line_number,
            no_messages: self.no_messages,
            invert_match: self.invert_match,
            multiple_inputs: self.input_files.len() > 1,
            output_mode,
            patterns,
            input_files: self.input_files,
        })
    }
}

/// Newtype over `Vec[libc::regex_t]`. Provides functionality for matching input data.
enum Patterns {
    Fixed(Vec<String>, bool, bool),
    Regex(Vec<regex_t>),
}

impl Patterns {
    /// Creates a new `Patterns` object with regex patterns.
    ///
    /// # Arguments
    ///
    /// * `patterns` - `Vec<String>` containing the patterns.
    /// * `extended_regexp` - `bool` indicating whether to use extended regular expressions.
    /// * `fixed_string` - `bool` indicating whether pattern is fixed string or regex.
    /// * `ignore_case` - `bool` indicating whether to ignore case.
    /// * `line_regexp` - `bool` indicating whether to match the entire input.
    ///
    /// # Errors
    ///
    /// Returns an error if passed invalid regex.
    ///
    /// # Returns
    ///
    /// Returns [Patterns](Patterns).
    fn new(
        patterns: Vec<String>,
        extended_regexp: bool,
        fixed_string: bool,
        ignore_case: bool,
        line_regexp: bool,
    ) -> Result<Self, String> {
        if fixed_string {
            Ok(Self::Fixed(
                patterns
                    .into_iter()
                    .map(|p| if ignore_case { p.to_lowercase() } else { p })
                    .collect(),
                ignore_case,
                line_regexp,
            ))
        } else {
            let mut ps = vec![];

            let mut cflags = 0;
            if extended_regexp {
                cflags |= REG_EXTENDED;
            }
            if ignore_case {
                cflags |= REG_ICASE;
            }
            for p in patterns {
                let pattern = if line_regexp { format!("^{p}$") } else { p };

                let c_pattern = CString::new(pattern).map_err(|err| err.to_string())?;
                let mut regex = unsafe { std::mem::zeroed::<regex_t>() };

                let result = unsafe { regcomp(&mut regex, c_pattern.as_ptr(), cflags) };
                if result != 0 {
                    return Err(format!(
                        "Error compiling regex '{}'",
                        c_pattern.to_string_lossy()
                    ));
                }
                ps.push(regex);
            }
            Ok(Self::Regex(ps))
        }
    }

    /// Checks if input string matches the present patterns.
    ///
    /// # Arguments
    ///
    /// * `input` - object that implements [AsRef](AsRef) for [str](str) and describes line.
    ///
    /// # Returns
    ///
    /// Returns [bool](bool) - `true` if input matches present patterns, else `false`.
    fn matches(&self, input: impl AsRef<str>) -> bool {
        let input = input.as_ref();
        match self {
            Patterns::Fixed(patterns, ignore_case, line_regexp) => {
                let input = if *ignore_case {
                    input.to_lowercase()
                } else {
                    input.to_string()
                };
                patterns.iter().any(|p| {
                    if *line_regexp {
                        input == *p
                    } else {
                        input.contains(p)
                    }
                })
            }
            Patterns::Regex(patterns) => {
                let c_input = CString::new(input).unwrap();
                patterns.iter().any(|p| unsafe {
                    regexec(p, c_input.as_ptr(), 0, ptr::null_mut(), 0) != REG_NOMATCH
                })
            }
        }
    }
}

impl Drop for Patterns {
    fn drop(&mut self) {
        match &self {
            Patterns::Fixed(_, _, _) => {}
            Patterns::Regex(regexes) => {
                for regex in regexes {
                    unsafe { regfree(regex as *const _ as *mut _) }
                }
            }
        }
    }
}

/// Represents possible `grep` output modes.
#[derive(Eq, PartialEq)]
enum OutputMode {
    Count(u64),
    FilesWithMatches,
    Quiet,
    Default,
}

/// Structure that contains all necessary information for `grep` utility processing.
struct GrepModel {
    any_matches: bool,
    any_errors: bool,
    line_number: bool,
    no_messages: bool,
    invert_match: bool,
    multiple_inputs: bool,
    output_mode: OutputMode,
    patterns: Patterns,
    input_files: Vec<String>,
}

impl GrepModel {
    /// Processes input files or STDIN content.
    ///
    /// # Returns
    ///
    /// Returns [i32](i32) that represents *exit status code*.
    fn grep(&mut self) -> i32 {
        for input_name in self.input_files.drain(..).collect::<Vec<_>>() {
            if input_name == "-" {
                let reader = Box::new(BufReader::new(io::stdin()));
                self.process_input("(standard input)", reader);
            } else {
                match File::open(&input_name) {
                    Ok(file) => {
                        let reader = Box::new(BufReader::new(file));
                        self.process_input(&input_name, reader)
                    }
                    Err(err) => {
                        self.any_errors = true;
                        if !self.no_messages {
                            eprintln!("{}: {}", input_name, err);
                        }
                    }
                }
            }
            if self.any_matches && self.output_mode == OutputMode::Quiet {
                return 0;
            }
        }

        if self.any_errors {
            2
        } else if !self.any_matches {
            1
        } else {
            0
        }
    }

    /// Reads lines from buffer and processes them.
    ///
    /// # Arguments
    ///
    /// * `input_name` - [str](str) that represents content source name.
    /// * `reader` - [Box](Box) that contains object that implements [BufRead] and reads lines.
    fn process_input(&mut self, input_name: &str, mut reader: Box<dyn BufRead>) {
        let mut line_number: u64 = 0;
        loop {
            let mut line = String::new();
            line_number += 1;
            match reader.read_line(&mut line) {
                Ok(n_read) => {
                    if n_read == 0 {
                        break;
                    }
                    let trimmed = if line.ends_with('\n') {
                        &line[..line.len() - 1]
                    } else {
                        &line
                    };

                    let init_matches = self.patterns.matches(trimmed);
                    let matches = if self.invert_match {
                        !init_matches
                    } else {
                        init_matches
                    };
                    if matches {
                        self.any_matches = true;
                        match &mut self.output_mode {
                            OutputMode::Count(count) => {
                                *count += 1;
                            }
                            OutputMode::FilesWithMatches => {
                                println!("{input_name}");
                                break;
                            }
                            OutputMode::Quiet => {
                                return;
                            }
                            OutputMode::Default => {
                                let result = format!(
                                    "{}{}{}",
                                    if self.multiple_inputs {
                                        format!("{input_name}:")
                                    } else {
                                        String::new()
                                    },
                                    if self.line_number {
                                        format!("{line_number}:")
                                    } else {
                                        String::new()
                                    },
                                    trimmed
                                );
                                println!("{result}");
                            }
                        }
                    }
                    line.clear();
                }
                Err(err) => {
                    self.any_errors = true;
                    if !self.no_messages {
                        eprintln!(
                            "{}: Error reading line {} ({})",
                            input_name, line_number, err
                        );
                    }
                }
            }
        }
        if let OutputMode::Count(count) = &mut self.output_mode {
            if self.multiple_inputs {
                println!("{input_name}:{count}");
            } else {
                println!("{count}");
            }
            *count = 0;
        }
    }
}

// Exit code:
//     0 - One or more lines were selected.
//     1 - No lines were selected.
//     >1 - An error occurred.
fn main() -> Result<(), Box<dyn std::error::Error>> {
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;
    // Parse command line arguments
    let mut args = Args::parse();

    let exit_code = args
        .validate_args()
        .and_then(|_| {
            args.resolve();
            args.into_grep_model()
        })
        .map(|mut grep_model| grep_model.grep())
        .unwrap_or_else(|err| {
            eprintln!("{}", err);
            2
        });

    std::process::exit(exit_code);
}
