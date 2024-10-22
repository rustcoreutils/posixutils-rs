//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, textdomain};
use libc::{regcomp, regex_t, regexec, regfree, regmatch_t, REG_EXTENDED, REG_ICASE, REG_NOMATCH};
use plib::PROJECT_NAME;
use std::{
    ffi::CString,
    fs::File,
    io::{self, BufRead, BufReader, StdoutLock, Write},
    path::{Path, PathBuf},
    ptr,
};

/// grep - search a file for a pattern.
#[derive(Parser)]
#[command(version, about)]
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

    /// Write only the names of input files containing selected lines to standard output.
    #[arg(short = 'l', long)]
    files_with_matches: bool,

    /// Precede each output line by its relative line number in the file, each file starting at line 1.
    #[arg(short = 'n', long)]
    line_number: bool,

    /// Only print the matching characters in each line.
    #[arg(short = 'o', long = "only-matching")]
    only_matching: bool,

    /// Do not print to standard output. The presence or absence of a match is communicated with the exit status.
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
            return Err("A pattern list or at least one file is required".to_string());
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
        self.regexp.sort_by_key(|r| r.len());
        self.regexp.dedup();

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
            stdout_lock: io::stdout().lock(),
            only_matching: self.only_matching,
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
            for mut pattern in patterns {
                // macOS version of [regcomp](regcomp) from `libc`
                // provides additional check for empty regex. In this case,
                // an error [REG_EMPTY](https://developer.apple.com/library/archive/documentation/System/Conceptual/ManPages_iPhoneOS/man3/regcomp.3.html)
                // will be returned.

                // Therefore, an empty pattern is replaced with ".*".
                #[cfg(target_os = "macos")]
                {
                    pattern = if pattern.is_empty() {
                        String::from(".*")
                    } else {
                        pattern
                    };
                }
                pattern = if line_regexp {
                    format!("^{pattern}$")
                } else {
                    pattern
                };

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
    fn matches(&self, input: impl AsRef<str>, collect_matching_substrings: bool) -> MatchesResult {
        let input = input.as_ref();

        let mut matching_substrings = Vec::<Vec<u8>>::new();

        let mut any_pattern_matched = false;

        match self {
            Patterns::Fixed(patterns, ignore_case, line_regexp) => {
                let input = if *ignore_case {
                    input.to_lowercase()
                } else {
                    input.to_string()
                };

                for pattern in patterns {
                    if *line_regexp {
                        if input != *pattern {
                            continue;
                        }

                        if !collect_matching_substrings {
                            return MatchesResult::fast_path_match();
                        }

                        any_pattern_matched = true;

                        matching_substrings.push(pattern.as_bytes().to_vec());
                    } else {
                        for st in input.matches(pattern) {
                            if !collect_matching_substrings {
                                return MatchesResult::fast_path_match();
                            }

                            any_pattern_matched = true;

                            matching_substrings.push(st.as_bytes().to_vec());
                        }
                    }
                }
            }
            Patterns::Regex(patterns) => {
                const SINGLE_ELEMENT_ARRAY_SIZE: usize = 1_usize;

                let input_slice = input.as_bytes();

                let mut regmatch_t_array = [const {
                    regmatch_t {
                        rm_so: -1,
                        rm_eo: -1,
                    }
                }; SINGLE_ELEMENT_ARRAY_SIZE];

                let (nmatch, pmatch) = if collect_matching_substrings {
                    (SINGLE_ELEMENT_ARRAY_SIZE, regmatch_t_array.as_mut_ptr())
                } else {
                    (0_usize, ptr::null_mut())
                };

                for pattern in patterns {
                    let mut current_string_index = 0_usize;

                    loop {
                        // Clear values from the last iteration
                        if collect_matching_substrings {
                            let [ref mut regmatch_t] = regmatch_t_array;

                            regmatch_t.rm_so = -1;
                            regmatch_t.rm_eo = -1;
                        }

                        let current_string_slice = &input_slice[current_string_index..];

                        let current_string_c_string = CString::new(current_string_slice).unwrap();

                        let current_string_pointer = current_string_c_string.as_ptr();

                        let regexec_return_value =
                            unsafe { regexec(pattern, current_string_pointer, nmatch, pmatch, 0) };

                        if regexec_return_value != 0 {
                            debug_assert!(regexec_return_value == REG_NOMATCH);

                            break;
                        }

                        if !collect_matching_substrings {
                            return MatchesResult::fast_path_match();
                        }

                        any_pattern_matched = true;

                        let [regmatch_t] = regmatch_t_array;

                        let regmatch_t { rm_so, rm_eo } = regmatch_t;

                        debug_assert!(rm_so != -1);
                        debug_assert!(rm_eo != -1);

                        let start = usize::try_from(rm_so).unwrap();
                        let end = usize::try_from(rm_eo).unwrap();

                        // TODO
                        // Is this the right fix?
                        // The edge case is:
                        //
                        // grep -o ''
                        if end == 0_usize {
                            break;
                        }

                        matching_substrings.push(current_string_slice[start..end].to_vec());

                        current_string_index += end;
                    }
                }
            }
        }

        MatchesResult {
            any_pattern_matched,
            matching_substrings,
        }
    }
}

impl Drop for Patterns {
    fn drop(&mut self) {
        match &self {
            Patterns::Fixed(_, _, _) => {}
            Patterns::Regex(regexes) => {
                for regex in regexes {
                    unsafe { regfree(regex as *const regex_t as *mut regex_t) }
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
    stdout_lock: StdoutLock<'static>,
    only_matching: bool,
}

struct MatchesResult {
    any_pattern_matched: bool,
    /// Will always be empty if the -o option is not being used
    matching_substrings: Vec<Vec<u8>>,
}

impl MatchesResult {
    pub fn fast_path_match() -> MatchesResult {
        MatchesResult {
            any_pattern_matched: true,
            matching_substrings: Vec::<Vec<u8>>::new(),
        }
    }
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

    fn print_line_prefix(&mut self, input_name: &str, line_number: u64) {
        if self.multiple_inputs {
            write!(self.stdout_lock, "{input_name}:").unwrap();
        }

        if self.line_number {
            write!(self.stdout_lock, "{line_number}:").unwrap();
        }
    }

    /// Reads lines from buffer and processes them.
    ///
    /// # Arguments
    ///
    /// * `input_name` - [str](str) that represents content source name.
    /// * `reader` - [Box](Box) that contains object that implements [BufRead] and reads lines.
    fn process_input(&mut self, input_name: &str, mut reader: Box<dyn BufRead>) {
        let mut line_number = 0_u64;

        let mut line = String::new();

        loop {
            line_number += 1;

            line.clear();

            // TODO
            // Probably should work on non-UTF-8 input
            match reader.read_line(&mut line) {
                Ok(n_read) => {
                    if n_read == 0 {
                        break;
                    }

                    let mut chars = line.chars();

                    let line_without_newline = match chars.next_back() {
                        Some('\n') => chars.as_str(),
                        _ => line.as_str(),
                    };

                    let matches_result = self.patterns.matches(
                        line_without_newline,
                        self.only_matching && matches!(self.output_mode, OutputMode::Default),
                    );

                    let MatchesResult {
                        any_pattern_matched,
                        matching_substrings,
                    } = matches_result;

                    let matches = if self.invert_match {
                        !any_pattern_matched
                    } else {
                        any_pattern_matched
                    };

                    if matches {
                        self.any_matches = true;

                        match &mut self.output_mode {
                            OutputMode::Count(count) => {
                                *count += 1;
                            }
                            OutputMode::FilesWithMatches => {
                                writeln!(&mut self.stdout_lock, "{input_name}").unwrap();

                                break;
                            }
                            OutputMode::Quiet => {
                                return;
                            }
                            OutputMode::Default => {
                                if self.only_matching {
                                    for matching_substring in matching_substrings {
                                        self.print_line_prefix(input_name, line_number);

                                        self.stdout_lock
                                            .write_all(matching_substring.as_slice())
                                            .unwrap();

                                        self.stdout_lock.write_all(b"\n").unwrap();
                                    }
                                } else {
                                    self.print_line_prefix(input_name, line_number);

                                    writeln!(self.stdout_lock, "{line_without_newline}").unwrap();
                                }
                            }
                        }
                    }
                }
                Err(err) => {
                    self.any_errors = true;

                    if !self.no_messages {
                        eprintln!("{input_name}: Error reading line {line_number} ({err})",);
                    }
                }
            }
        }

        if let OutputMode::Count(count) = &mut self.output_mode {
            if self.multiple_inputs {
                writeln!(&mut self.stdout_lock, "{input_name}:{count}").unwrap();
            } else {
                writeln!(&mut self.stdout_lock, "{count}").unwrap();
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
            eprintln!("{err}");
            2
        });

    std::process::exit(exit_code);
}
