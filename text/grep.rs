//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use libc::{regcomp, regex_t, regexec, regfree, regmatch_t, REG_EXTENDED, REG_ICASE, REG_NOMATCH};
use memchr::memmem;
use std::{
    error::Error,
    ffi::CString,
    fs::File,
    io::{self, BufRead, BufReader, StdoutLock, Write},
    mem::MaybeUninit,
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

    /// Suppress the error messages ordinarily written for nonexistent or unreadable input files.
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
}

impl Args {
    /// Validates the arguments to ensure no conflicting options are used together.
    ///
    /// # Errors
    ///
    /// Returns an error if conflicting options are found.
    fn validate_args(&self) -> Result<(), Box<dyn Error>> {
        if self.extended_regexp && self.fixed_strings {
            return Err(Box::from("options '-E' and '-F' cannot be used together"));
        }

        if self.count && self.files_with_matches {
            return Err(Box::from("options '-c' and '-l' cannot be used together"));
        }

        if self.count && self.quiet {
            return Err(Box::from("options '-c' and '-q' cannot be used together"));
        }

        if self.files_with_matches && self.quiet {
            return Err(Box::from("options '-l' and '-q' cannot be used together"));
        }

        if self.regexp.is_empty() && self.file.is_empty() && self.single_pattern.is_none() {
            return Err(Box::from("a pattern list or at least one file is required"));
        }

        Ok(())
    }

    /// Resolves input patterns and input files. Reads patterns from pattern files and merges them with specified as argument. Handles input files if empty.
    fn resolve(&mut self, grep_model_state: &mut GrepModelState) {
        for path_buf in &self.file {
            match Self::get_file_patterns(path_buf) {
                Ok(patterns) => self.regexp.extend(patterns),
                Err(err) => {
                    grep_model_state.any_errors = true;

                    if !self.no_messages {
                        let path_buf_display = path_buf.display();

                        eprintln!("grep: {path_buf_display}: {err}");
                    }
                }
            }
        }

        match &self.single_pattern {
            None => {}
            Some(pattern) => {
                if !self.regexp.is_empty() {
                    self.input_files.insert(0, pattern.to_owned());
                } else {
                    self.regexp = vec![pattern.to_owned()];
                }
            }
        }

        self.regexp = self
            .regexp
            .iter()
            .flat_map(|pattern| pattern.split('\n').map(ToOwned::to_owned))
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
    fn into_grep_model(self) -> Result<GrepModel, Box<dyn Error>> {
        let patterns = Patterns::new(
            self.regexp,
            self.extended_regexp,
            self.fixed_strings,
            self.ignore_case,
            self.line_regexp,
        )?;

        let multiple_inputs = self.input_files.len() > 1_usize;

        Ok(GrepModel {
            input_files: self.input_files,
            invert_match: self.invert_match,
            line_number: self.line_number,
            multiple_inputs,
            no_messages: self.no_messages,
            only_matching: self.only_matching,
            patterns,
            quiet: self.quiet,
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
    ) -> Result<Self, Box<dyn Error>> {
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
            let mut cflags = 0;

            if extended_regexp {
                cflags |= REG_EXTENDED;
            }

            if ignore_case {
                cflags |= REG_ICASE;
            }

            let mut regex_vec = Vec::<regex_t>::with_capacity(patterns.len());

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

                let pattern_c_string = CString::new(pattern)?;

                let pattern_c_string_pointer = pattern_c_string.as_ptr();

                let mut regex_maybe_uninit = MaybeUninit::<regex_t>::uninit();

                let regex_maybe_uninit_pointer = regex_maybe_uninit.as_mut_ptr();

                let regcomp_result = unsafe {
                    regcomp(regex_maybe_uninit_pointer, pattern_c_string_pointer, cflags)
                };

                if regcomp_result == 0 {
                    // Safety: just checked regcomp return value
                    let regex = unsafe { regex_maybe_uninit.assume_init() };

                    regex_vec.push(regex);
                } else {
                    return Err(Box::from(format!(
                        "error compiling regex '{}'",
                        pattern_c_string.to_string_lossy()
                    )));
                }
            }

            Ok(Self::Regex(regex_vec))
        }
    }

    /// Checks if input string matches the present patterns.
    fn matches(
        &self,
        input: &[u8],
        collect_matching_substrings: bool,
    ) -> Result<MatchesData, Box<dyn Error>> {
        let mut matching_substrings = Vec::<Vec<u8>>::new();

        let mut any_pattern_matched = false;

        match self {
            Patterns::Fixed(patterns, ignore_case, line_regexp) => {
                let input_ascii_lowercase: Vec<u8>;

                let input_after_ignore_case = if *ignore_case {
                    input_ascii_lowercase = input.to_ascii_lowercase();

                    input_ascii_lowercase.as_slice()
                } else {
                    input
                };

                for pattern in patterns {
                    let pattern_as_bytes = pattern.as_bytes();

                    if *line_regexp {
                        if input_after_ignore_case != pattern_as_bytes {
                            continue;
                        }

                        if !collect_matching_substrings {
                            return Ok(MatchesData::fast_path_match());
                        }

                        any_pattern_matched = true;

                        matching_substrings.push(input.to_vec());
                    } else {
                        if collect_matching_substrings {
                            let pattern_as_bytes_len = pattern_as_bytes.len();

                            for index in
                                memmem::find_iter(input_after_ignore_case, pattern_as_bytes)
                            {
                                any_pattern_matched = true;

                                let match_slice = &input[index..(index + pattern_as_bytes_len)];

                                matching_substrings.push(match_slice.to_vec());
                            }
                        } else {
                            if memmem::find(input_after_ignore_case, pattern_as_bytes).is_some() {
                                return Ok(MatchesData::fast_path_match());
                            }
                        }
                    }
                }
            }
            Patterns::Regex(patterns) => {
                const SINGLE_ELEMENT_ARRAY_SIZE: usize = 1_usize;

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
                    let mut current_position = 0_usize;

                    loop {
                        // Clear values from the last iteration
                        if collect_matching_substrings {
                            let [ref mut regmatch_t] = regmatch_t_array;

                            regmatch_t.rm_so = -1;
                            regmatch_t.rm_eo = -1;
                        }

                        let forward_slice = &input[current_position..];

                        // TODO
                        // How should slices containing null bytes be handled?
                        let current_string_c_string = CString::new(forward_slice)?;

                        let current_string_pointer = current_string_c_string.as_ptr();

                        let regexec_return_value =
                            unsafe { regexec(pattern, current_string_pointer, nmatch, pmatch, 0) };

                        if regexec_return_value != 0 {
                            debug_assert!(regexec_return_value == REG_NOMATCH);

                            break;
                        }

                        if !collect_matching_substrings {
                            return Ok(MatchesData::fast_path_match());
                        }

                        any_pattern_matched = true;

                        let [regmatch_t] = regmatch_t_array;

                        let regmatch_t { rm_so, rm_eo } = regmatch_t;

                        debug_assert!(rm_so != -1);
                        debug_assert!(rm_eo != -1);

                        let start = usize::try_from(rm_so)?;
                        let end = usize::try_from(rm_eo)?;

                        // TODO
                        // Is this the right fix?
                        // The edge case is:
                        //
                        // grep -o ''
                        if end == 0_usize {
                            break;
                        }

                        matching_substrings.push(forward_slice[start..end].to_vec());

                        current_position += end;
                    }
                }
            }
        }

        Ok(MatchesData {
            any_pattern_matched,
            matching_substrings,
        })
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

struct GrepModelState {
    any_errors: bool,
    any_matches: bool,
    output_mode: OutputMode,
    stdout_lock: StdoutLock<'static>,
}

/// Structure that contains all necessary information for `grep` utility processing.
struct GrepModel {
    input_files: Vec<String>,
    invert_match: bool,
    line_number: bool,
    multiple_inputs: bool,
    no_messages: bool,
    only_matching: bool,
    patterns: Patterns,
    quiet: bool,
}

struct MatchesData {
    any_pattern_matched: bool,
    /// Will always be empty if the -o option is not being used
    matching_substrings: Vec<Vec<u8>>,
}

impl MatchesData {
    #[inline]
    pub fn fast_path_match() -> MatchesData {
        MatchesData {
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
    fn grep(&self, grep_model_state: &mut GrepModelState) -> Result<(), Box<dyn Error>> {
        for input_name in &self.input_files {
            let input_name_str = input_name.as_str();

            if input_name_str == "-" {
                let reader = Box::new(BufReader::new(io::stdin()));

                if let Err(bo) = self.process_input(grep_model_state, "(standard input)", reader) {
                    grep_model_state.any_errors = true;

                    if !self.no_messages {
                        eprintln!("grep: {input_name_str}: {bo}");
                    }
                }
            } else {
                match File::open(input_name_str) {
                    Ok(file) => {
                        let reader = Box::new(BufReader::new(file));

                        self.process_input(grep_model_state, input_name_str, reader)?;
                    }
                    Err(er) => {
                        grep_model_state.any_errors = true;

                        if !self.no_messages {
                            eprintln!("grep: {input_name_str}: {er}");
                        }
                    }
                }
            }

            if grep_model_state.any_matches && grep_model_state.output_mode == OutputMode::Quiet {
                return Ok(());
            }
        }

        Ok(())
    }

    fn print_line_prefix(
        &self,
        grep_model_state: &mut GrepModelState,
        input_name: &str,
        line_number: u64,
    ) -> Result<(), Box<dyn Error>> {
        if self.multiple_inputs {
            write!(grep_model_state.stdout_lock, "{input_name}:")?;
        }

        if self.line_number {
            write!(grep_model_state.stdout_lock, "{line_number}:")?;
        }

        Ok(())
    }

    /// Reads lines from buffer and processes them.
    ///
    /// # Arguments
    ///
    /// * `input_name` - [str](str) that represents content source name.
    /// * `reader` - [Box](Box) that contains object that implements [BufRead] and reads lines.
    fn process_input(
        &self,
        grep_model_state: &mut GrepModelState,
        input_name: &str,
        mut reader: Box<dyn BufRead>,
    ) -> Result<(), Box<dyn Error>> {
        let mut line_number = 0_u64;

        let mut line = Vec::<u8>::new();

        loop {
            line.clear();

            match reader.read_until(b'\n', &mut line) {
                Ok(n_read) => {
                    if n_read == 0 {
                        break;
                    }

                    line_number += 1;

                    let mut iter = line.iter();

                    let line_without_newline = match iter.next_back() {
                        Some(b'\n') => iter.as_slice(),
                        _ => line.as_slice(),
                    };

                    let matches_result = self.patterns.matches(
                        line_without_newline,
                        self.only_matching
                            && matches!(grep_model_state.output_mode, OutputMode::Default),
                    );

                    let matches_data = match matches_result {
                        Ok(ma) => ma,
                        Err(bo) => {
                            if !self.no_messages {
                                eprintln!("grep: {input_name}: error matching patterns against line {line_number} ({bo})");
                            }

                            return Err(bo);
                        }
                    };

                    let MatchesData {
                        any_pattern_matched,
                        matching_substrings,
                    } = matches_data;

                    let matches = if self.invert_match {
                        !any_pattern_matched
                    } else {
                        any_pattern_matched
                    };

                    if matches {
                        grep_model_state.any_matches = true;

                        match &mut grep_model_state.output_mode {
                            OutputMode::Count(count) => {
                                *count += 1;
                            }
                            OutputMode::FilesWithMatches => {
                                writeln!(&mut grep_model_state.stdout_lock, "{input_name}")?;

                                break;
                            }
                            OutputMode::Quiet => {
                                return Ok(());
                            }
                            OutputMode::Default => {
                                if self.only_matching {
                                    for matching_substring in matching_substrings {
                                        self.print_line_prefix(
                                            grep_model_state,
                                            input_name,
                                            line_number,
                                        )?;

                                        grep_model_state
                                            .stdout_lock
                                            .write_all(matching_substring.as_slice())?;

                                        grep_model_state.stdout_lock.write_all(b"\n")?;
                                    }
                                } else {
                                    self.print_line_prefix(
                                        grep_model_state,
                                        input_name,
                                        line_number,
                                    )?;

                                    grep_model_state
                                        .stdout_lock
                                        .write_all(line_without_newline)?;

                                    grep_model_state.stdout_lock.write_all(b"\n")?;
                                }
                            }
                        }
                    }
                }
                Err(er) => {
                    if !self.no_messages {
                        eprintln!("grep: {input_name}: error reading line {line_number} ({er})");
                    }

                    return Err(Box::new(er));
                }
            }
        }

        if let OutputMode::Count(count) = &mut grep_model_state.output_mode {
            if self.multiple_inputs {
                writeln!(grep_model_state.stdout_lock, "{input_name}:{count}")?;
            } else {
                writeln!(grep_model_state.stdout_lock, "{count}")?;
            }

            *count = 0;
        }

        Ok(())
    }
}

// Exit code:
//     0 - One or more lines were selected.
//     1 - No lines were selected.
//     >1 - An error occurred.
fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain(env!("PROJECT_NAME"))?;
    bind_textdomain_codeset(env!("PROJECT_NAME"), "UTF-8")?;

    let mut args = Args::parse();

    let output_mode = if args.count {
        OutputMode::Count(0)
    } else if args.files_with_matches {
        OutputMode::FilesWithMatches
    } else if args.quiet {
        OutputMode::Quiet
    } else {
        OutputMode::Default
    };

    let mut grep_model_state = GrepModelState {
        any_errors: false,
        any_matches: false,
        output_mode,
        stdout_lock: io::stdout().lock(),
    };

    let result = args
        .validate_args()
        .and_then(|_| {
            args.resolve(&mut grep_model_state);
            args.into_grep_model()
        })
        .and_then(|gr| match gr.grep(&mut grep_model_state) {
            Ok(()) => Ok(gr),
            Err(bo) => Err(bo),
        });

    let exit_code = match result {
        Ok(grep_model) => {
            if grep_model.quiet && grep_model_state.any_matches {
                0
            } else if grep_model_state.any_errors {
                2
            } else if !grep_model_state.any_matches {
                1
            } else {
                0
            }
        }
        Err(error_message) => {
            eprintln!("grep: {error_message}");

            2
        }
    };

    std::process::exit(exit_code);
}
