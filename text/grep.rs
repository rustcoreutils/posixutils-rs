//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::{CommandFactory, Parser};
use gettextrs::gettext;
use plib::optarg::OptionArguments;
use plib::regex::{Regex, RegexFlags};
use std::{
    cmp::Reverse,
    collections::VecDeque,
    ffi::OsString,
    fs::File,
    io::{self, BufRead, BufReader, Write},
    ops::Range,
    path::{Path, PathBuf},
};

/// Fold bytes to lowercase under the current `LC_CTYPE` (libc `tolower`/
/// `towlower`), used for the `-F -i` fixed-string comparison so case-insensitive
/// matching honors the locale rather than Rust's Unicode-only folding.
///
/// The bytes are taken a character of the locale at a time; a byte that is no
/// character (not valid UTF-8 in a UTF-8 locale, or above 0x7F in the C
/// locale) is kept as it is.
fn locale_lower(bytes: &[u8]) -> Vec<u8> {
    fold_case(bytes, None)
}

/// As [`locale_lower`], also recording in `origin`, for each byte of the folded text and one
/// past its end, the offset in `bytes` of the character it came from, since folding can change
/// a character's length: `-o -F -i` finds a match in the folded text and writes the line's own.
fn fold_case(bytes: &[u8], mut origin: Option<&mut Vec<usize>>) -> Vec<u8> {
    let mut folded = Vec::with_capacity(bytes.len());
    let mut offset = 0;
    for ch in plib::locale::mb_char_slices(bytes) {
        match std::str::from_utf8(ch) {
            Ok(s) => {
                for c in s.chars().map(plib::locale::to_lower) {
                    folded.extend_from_slice(c.encode_utf8(&mut [0; 4]).as_bytes());
                }
            }
            Err(_) => folded.extend_from_slice(ch),
        }
        if let Some(origin) = origin.as_deref_mut() {
            origin.resize(folded.len(), offset);
        }
        offset += ch.len();
    }
    if let Some(origin) = origin {
        origin.push(bytes.len());
    }
    folded
}

/// The first occurrence of `needle` in `haystack` at or after `from`, as its start and end.
fn find_bytes(haystack: &[u8], needle: &[u8], from: usize) -> Option<(usize, usize)> {
    if needle.is_empty() {
        return Some((from, from));
    }
    let start = from
        + haystack[from..]
            .windows(needle.len())
            .position(|w| w == needle)?;
    Some((start, start + needle.len()))
}

/// Whether the character in `ch` (one character of the locale, or one byte that is none) is a
/// word constituent for `-w`: a letter or digit of the locale, or `_`, as in GNU grep.
fn is_word_char(ch: &[u8]) -> bool {
    let Ok(s) = std::str::from_utf8(ch) else {
        return false;
    };
    let mut chars = s.chars();
    match (chars.next(), chars.next()) {
        (Some(c), None) => c == '_' || plib::locale::isalnum(c),
        _ => false,
    }
}

/// Whether `line[start..end]` stands as a word for `-w`: no word constituent just before it
/// or just after it.
fn is_word_at(line: &[u8], start: usize, end: usize) -> bool {
    // No character is longer than this; a window that starts inside one still ends with the
    // whole character before `start`.
    const MAX_CHAR_LEN: usize = 16;
    let before = &line[start.saturating_sub(MAX_CHAR_LEN)..start];
    let after = &line[end..line.len().min(end + MAX_CHAR_LEN)];
    let word_before = plib::locale::mb_char_slices(before)
        .last()
        .is_some_and(|ch| is_word_char(ch));
    let word_after = plib::locale::mb_char_slices(after)
        .first()
        .is_some_and(|ch| is_word_char(ch));
    !word_before && !word_after
}

/// `-w` for a regular expression: the first match of `re` in `line`, at or after `from`, that
/// stands as a word, as its start and end. As GNU grep does, a match that does not is tried
/// again shorter from the same start, and then the search goes on from the next character.
fn regex_find_word(re: &Regex, line: &[u8], mut from: usize) -> Option<(usize, usize)> {
    while let Some(m) = re.find_bytes_in_line(&line[from..], from > 0, false) {
        let start = from + m.start;
        let mut end = from + m.end;
        loop {
            if is_word_at(line, start, end) {
                return Some((start, end));
            }
            if end == start {
                break;
            }
            // The longest match from `start` shorter than this one, if any match starts there:
            // the leftmost-longest match in the shortened text.
            let shorter = &line[start..end - 1];
            match re.find_bytes_in_line(shorter, start > 0, end - 1 < line.len()) {
                Some(m) if m.start == 0 && m.end > 0 => end = start + m.end,
                _ => break,
            }
        }
        from = plib::locale::next_char_offset(line, start)?;
    }
    None
}

/// `-w` for a fixed string: the first occurrence of `needle` in `line`, at or after `from`,
/// that stands as a word, as its start and end.
fn fixed_find_word(line: &[u8], needle: &[u8], mut from: usize) -> Option<(usize, usize)> {
    loop {
        let (start, end) = find_bytes(line, needle, from)?;
        if is_word_at(line, start, end) {
            return Some((start, end));
        }
        from = plib::locale::next_char_offset(line, start)?;
    }
}

/// The first match of fixed string `needle` in `line` at or after `from`, as its start and
/// end, where `whole` says what part of the line it has to be.
fn fixed_find(line: &[u8], needle: &[u8], whole: Whole, from: usize) -> Option<(usize, usize)> {
    match whole {
        Whole::Any => find_bytes(line, needle, from),
        Whole::Word => fixed_find_word(line, needle, from),
        Whole::Line => (from == 0 && line == needle).then_some((0, line.len())),
    }
}

/// The first match of `re` in `line` at or after `from`, as its start and end, where `whole`
/// says what part of the line it has to be (under -x, `re` is anchored already).
fn regex_find(re: &Regex, line: &[u8], whole: Whole, from: usize) -> Option<(usize, usize)> {
    match whole {
        Whole::Word => regex_find_word(re, line, from),
        Whole::Any | Whole::Line => re
            .find_bytes_in_line(&line[from..], from > 0, false)
            .map(|m| (from + m.start, from + m.end)),
    }
}

/// grep - search a file for a pattern
#[derive(Parser)]
#[command(
    version,
    about = gettext("grep - search a file for a pattern"),
    disable_help_flag = true
)]
struct Args {
    // -h is --no-filename, so help is reachable as --help only.
    #[arg(long, action = clap::ArgAction::HelpLong, help = gettext("Print help"))]
    help: Option<bool>,

    #[arg(short = 'H', long, overrides_with = "no_filename", help = gettext("Precede each output line by the file name"))]
    with_filename: bool,

    #[arg(short = 'h', long, overrides_with = "with_filename", help = gettext("Never precede output lines by the file name"))]
    no_filename: bool,

    #[arg(long, allow_hyphen_values = true, help = gettext("Name standard input <LABEL> in output"))]
    label: Option<String>,

    #[arg(short = 'E', long, help = gettext("Match using extended regular expressions"))]
    extended_regexp: bool,

    #[arg(short = 'F', long, help = gettext("Match using fixed strings"))]
    fixed_strings: bool,

    #[arg(short, long, help = gettext("Write only a count of selected lines to standard output"))]
    count: bool,

    #[arg(short = 'e', long, allow_hyphen_values = true, help = gettext("Specify one or more patterns to be used during the search for input"))]
    regexp: Vec<String>,

    #[arg(short, long, allow_hyphen_values = true, help = gettext("Read one or more patterns from the file named by the pathname"))]
    file: Vec<PathBuf>,

    #[arg(short, long, help = gettext("Perform pattern matching in searches without regard to case"))]
    ignore_case: bool,

    #[arg(short = 'l', long, help = gettext("Write only the names of files containing selected lines to standard output"))]
    files_with_matches: bool,

    #[arg(short = 'n', long, help = gettext("Precede each output line by its relative line number in the file"))]
    line_number: bool,

    #[arg(short, long, visible_alias = "silent", help = gettext("Quiet mode, only return exit status"))]
    quiet: bool,

    #[arg(short = 's', long, help = gettext("Suppress the error messages for nonexistent or unreadable files"))]
    no_messages: bool,

    #[arg(short = 'v', long, help = gettext("Select lines not matching any of the specified patterns"))]
    invert_match: bool,

    #[arg(short = 'x', long, help = gettext("Match entire lines only"))]
    line_regexp: bool,

    #[arg(short = 'w', long, help = gettext("Match only whole words"))]
    word_regexp: bool,

    #[arg(short = 'o', long, help = gettext("Write only the matched parts of selected lines, each on its own line"))]
    only_matching: bool,

    #[arg(short = 'A', long, value_name = "NUM", help = gettext("Print NUM lines of context after each selected line"))]
    after_context: Option<usize>,

    #[arg(short = 'B', long, value_name = "NUM", help = gettext("Print NUM lines of context before each selected line"))]
    before_context: Option<usize>,

    #[arg(short = 'C', long, value_name = "NUM", help = gettext("Print NUM lines of context around each selected line; -NUM is the same"))]
    context: Option<usize>,

    #[arg(name = "PATTERNS", help = gettext("Pattern to search for"))]
    single_pattern: Option<String>,

    #[arg(name = "FILE", help = gettext("Files to search (stdin if not specified)"))]
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
                        plib::diag::error(&format!(
                            "{}: {}",
                            path_buf.display(),
                            plib::diag::io_error_text(&err)
                        ));
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

        // Split multi-line -e/-f arguments into individual patterns. Every
        // specified pattern is kept and used (POSIX): duplicates are not
        // removed, as dropping a duplicate (e.g. an empty match-all pattern
        // supplied twice) could change the result.
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
            // -x wins over -w.
            if self.line_regexp {
                Whole::Line
            } else if self.word_regexp {
                Whole::Word
            } else {
                Whole::Any
            },
        )?;

        // -A and -B win over -C, whatever their order, as in GNU grep.
        let context = Context {
            before: self.before_context.or(self.context).unwrap_or(0),
            after: self.after_context.or(self.context).unwrap_or(0),
            separate: self.before_context.is_some()
                || self.after_context.is_some()
                || self.context.is_some(),
        };

        Ok(GrepModel {
            any_matches: false,
            any_errors: self.any_errors,
            line_number: self.line_number,
            no_messages: self.no_messages,
            invert_match: self.invert_match,
            only_matching: self.only_matching,
            // -H and -h override each other, so at most one is set.
            with_filename: self.with_filename || (!self.no_filename && self.input_files.len() > 1),
            stdin_name: self
                .label
                .unwrap_or_else(|| String::from("(standard input)")),
            output_mode,
            patterns,
            context,
            printed_any: false,
            input_files: self.input_files,
        })
    }
}

/// What part of a line a pattern has to match.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Whole {
    /// Any part (the default).
    Any,
    /// A part with no word constituent next to it (`-w`).
    Word,
    /// All of it (`-x`).
    Line,
}

/// Holds patterns for matching input data - either fixed strings or compiled regexes.
enum Patterns {
    /// The strings (folded under -i), whether -i is in effect, and what they must match.
    Fixed(Vec<Vec<u8>>, bool, Whole),
    Regex(Vec<Regex>, Whole),
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
    /// * `whole` - what part of a line a pattern has to match.
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
        whole: Whole,
    ) -> Result<Self, String> {
        if fixed_string {
            Ok(Self::Fixed(
                patterns
                    .into_iter()
                    .map(|p| {
                        if ignore_case {
                            locale_lower(p.as_bytes())
                        } else {
                            p.into_bytes()
                        }
                    })
                    .collect(),
                ignore_case,
                whole,
            ))
        } else {
            let mut ps = vec![];

            // Build flags for regex compilation
            let mut flags = if extended_regexp {
                RegexFlags::ere()
            } else {
                RegexFlags::bre()
            };
            if ignore_case {
                flags = flags.ignore_case();
            }

            for pattern in patterns {
                // For -x option, anchor the pattern to match entire line
                let pattern = if whole == Whole::Line {
                    format!("^{pattern}$")
                } else {
                    pattern
                };

                let regex = Regex::new(&pattern, flags).map_err(|e| e.to_string())?;
                ps.push(regex);
            }
            Ok(Self::Regex(ps, whole))
        }
    }

    /// Checks if the line `input` (its bytes, without the <newline>) matches
    /// the present patterns.
    ///
    /// A line need not be valid UTF-8: in the C locale every byte is a
    /// character, and in a UTF-8 locale a byte that is no character still
    /// leaves the rest of the line to match.
    fn matches(&self, input: &[u8]) -> bool {
        match self {
            Patterns::Fixed(patterns, ignore_case, whole) => {
                let folded;
                let input = if *ignore_case {
                    folded = locale_lower(input);
                    &folded
                } else {
                    input
                };
                patterns
                    .iter()
                    .any(|p| fixed_find(input, p, *whole, 0).is_some())
            }
            Patterns::Regex(patterns, Whole::Word) => patterns
                .iter()
                .any(|re| regex_find_word(re, input, 0).is_some()),
            Patterns::Regex(patterns, _) => patterns.iter().any(|re| re.is_match_bytes(input)),
        }
    }

    /// The parts of the line `input` that `-o` writes: each non-empty match, leftmost first,
    /// the search going on after each one.  Of the matches starting at the same place the
    /// longest is taken, and an empty match is skipped by one character, as in GNU grep.
    fn matched_parts(&self, input: &[u8]) -> Vec<Range<usize>> {
        let Patterns::Fixed(_, true, _) = self else {
            return self.matched_parts_in(input);
        };
        // The patterns are folded, so the match is found in the folded line, and the part
        // written is the line's own.
        let mut origin = Vec::new();
        let folded = fold_case(input, Some(&mut origin));
        self.matched_parts_in(&folded)
            .into_iter()
            .map(|part| origin[part.start]..origin[part.end])
            .collect()
    }

    /// As [`Patterns::matched_parts`], for `text` already folded under `-F -i`.
    fn matched_parts_in(&self, text: &[u8]) -> Vec<Range<usize>> {
        let mut parts = Vec::new();
        let mut from = 0;
        while from < text.len() {
            let Some((start, end)) = self.find_from(text, from) else {
                break;
            };
            if end > start {
                parts.push(start..end);
                from = end;
            } else {
                match plib::locale::next_char_offset(text, start) {
                    Some(next) => from = next,
                    None => break,
                }
            }
        }
        parts
    }

    /// The earliest match of any pattern in `text` at or after `from`, the longest of those
    /// starting there, as its start and end.
    fn find_from(&self, text: &[u8], from: usize) -> Option<(usize, usize)> {
        let earliest_longest = |&(start, end): &(usize, usize)| (start, Reverse(end));
        match self {
            Patterns::Fixed(patterns, _, whole) => patterns
                .iter()
                .filter_map(|p| fixed_find(text, p, *whole, from))
                .min_by_key(earliest_longest),
            Patterns::Regex(patterns, whole) => patterns
                .iter()
                .filter_map(|re| regex_find(re, text, *whole, from))
                .min_by_key(earliest_longest),
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
    /// `-o`: write the matched parts of a line rather than the line.
    only_matching: bool,
    /// Whether output lines and counts carry the input's name: by default
    /// when there is more than one input, always under -H, never under -h.
    with_filename: bool,
    /// What standard input is called in output: `--label`, or GNU's name.
    stdin_name: String,
    output_mode: OutputMode,
    patterns: Patterns,
    context: Context,
    /// Whether any line has been written, so a later group of context is separated from it.
    printed_any: bool,
    input_files: Vec<String>,
}

/// GNU context output (`-A`, `-B`, `-C`, `-NUM`).
struct Context {
    /// Lines written before each selected line.
    before: usize,
    /// Lines written after each selected line.
    after: usize,
    /// Whether groups of lines that do not touch are separated by `--`: whenever a context
    /// option was given, even a context of 0.
    separate: bool,
}

/// The context state of one input.
#[derive(Default)]
struct Pending {
    /// The latest unselected lines not written, up to `Context::before` of them, with their
    /// line numbers.
    before: VecDeque<(u64, Vec<u8>)>,
    /// How many of the next unselected lines are still to be written after a selected one.
    after_left: usize,
    /// The number of the last line written from this input.
    last_printed: Option<u64>,
}

impl GrepModel {
    /// Processes input files or STDIN content.
    ///
    /// # Returns
    ///
    /// Returns [i32](i32) that represents *exit status code*.
    fn grep(&mut self) -> i32 {
        for input_name in std::mem::take(&mut self.input_files) {
            if input_name == "-" {
                let reader = Box::new(BufReader::new(io::stdin()));
                let name = self.stdin_name.clone();
                self.process_input(&name, reader);
            } else {
                match File::open(&input_name) {
                    Ok(file) => {
                        let reader = Box::new(BufReader::new(file));
                        self.process_input(&input_name, reader)
                    }
                    Err(err) => {
                        self.any_errors = true;
                        if !self.no_messages {
                            plib::diag::error(&format!(
                                "{}: {}",
                                input_name,
                                plib::diag::io_error_text(&err)
                            ));
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
        let mut line = Vec::new();
        let mut pending = Pending::default();
        loop {
            line.clear();
            line_number += 1;
            match reader.read_until(b'\n', &mut line) {
                Ok(0) => break,
                Ok(_) => {
                    let trimmed = line.strip_suffix(b"\n").unwrap_or(&line);
                    if self.patterns.matches(trimmed) == self.invert_match {
                        if self.output_mode == OutputMode::Default {
                            self.unselected_line(input_name, line_number, trimmed, &mut pending);
                        }
                        continue;
                    }
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
                            let mut before = std::mem::take(&mut pending.before);
                            for (number, text) in before.drain(..) {
                                self.print_line(input_name, number, &text, b'-', &mut pending);
                            }
                            pending.before = before;
                            self.print_line(input_name, line_number, trimmed, b':', &mut pending);
                            pending.after_left = self.context.after;
                        }
                    }
                }
                // A read error (EISDIR for a directory operand, EIO) does not
                // advance the input and would recur on every retry: report it
                // once and stop reading this input.
                Err(err) => {
                    self.any_errors = true;
                    if !self.no_messages {
                        plib::diag::error(&format!(
                            "{}: {}",
                            input_name,
                            plib::diag::io_error_text(&err)
                        ));
                    }
                    break;
                }
            }
        }
        if let OutputMode::Count(count) = &mut self.output_mode {
            if self.with_filename {
                println!("{input_name}:{count}");
            } else {
                println!("{count}");
            }
            *count = 0;
        }
    }
}

impl GrepModel {
    /// An unselected line: written as context after a selected one, or kept in case one
    /// follows.
    fn unselected_line(&mut self, name: &str, number: u64, text: &[u8], pending: &mut Pending) {
        if pending.after_left > 0 {
            pending.after_left -= 1;
            self.print_line(name, number, text, b'-', pending);
        } else if self.context.before > 0 {
            // Once the window is full, the line falling out of it lends its buffer to the new
            // one, so a long run of unselected lines allocates nothing.
            let mut buf = if pending.before.len() == self.context.before {
                pending
                    .before
                    .pop_front()
                    .map(|(_, buf)| buf)
                    .unwrap_or_default()
            } else {
                Vec::new()
            };
            buf.clear();
            buf.extend_from_slice(text);
            pending.before.push_back((number, buf));
        }
    }

    /// Write line `number` of input `name`: `sep` is `:` for a selected line and `-` for a
    /// context line. A line that does not follow the last one written starts a new group.
    fn print_line(&mut self, name: &str, number: u64, text: &[u8], sep: u8, pending: &mut Pending) {
        if self.context.separate && self.printed_any && pending.last_printed != Some(number - 1) {
            write_line(b"", b"--");
        }
        let mut prefix = Vec::new();
        if self.with_filename {
            prefix.extend_from_slice(name.as_bytes());
            prefix.push(sep);
        }
        if self.line_number {
            prefix.extend_from_slice(number.to_string().as_bytes());
            prefix.push(sep);
        }
        if !self.only_matching {
            write_line(&prefix, text);
        } else if (sep == b':') != self.invert_match {
            // Under -o a line that matches has its matches written: a selected line, or under
            // -v a context line, as in GNU grep.  Other lines write nothing, but still join or
            // separate groups.
            for part in self.patterns.matched_parts(text) {
                write_line(&prefix, &text[part]);
            }
        }
        pending.last_printed = Some(number);
        self.printed_any = true;
    }
}

/// Write a selected line, after `prefix`, to standard output as its bytes.
/// grep cannot go on once its output fails, so a write error ends it with
/// status 2.
fn write_line(prefix: &[u8], line: &[u8]) {
    let mut out = io::stdout().lock();
    let written = out
        .write_all(prefix)
        .and_then(|()| out.write_all(line))
        .and_then(|()| out.write_all(b"\n"));
    if let Err(err) = written {
        plib::diag::error(&format!(
            "{}: {}",
            gettext("write error"),
            plib::diag::io_error_text(&err)
        ));
        std::process::exit(2);
    }
}

/// GNU's `-NUM` option, a context of NUM lines, as `-C NUM`, which clap can parse. Digits before
/// the first letter of a cluster that takes an argument are -NUM, the last run of them counting,
/// as in GNU grep: `-n5` is `-n -C 5`. An option-argument, and anything after `--`, is left as
/// it is.
fn expand_numeric_context(argv: Vec<OsString>) -> Vec<OsString> {
    let options = OptionArguments::of(Args::command());
    plib::optarg::rewrite_short_clusters(argv, &options, |flags, rest| {
        let digits = flags
            .split(|c: char| !c.is_ascii_digit())
            .rfind(|run| !run.is_empty())?;
        let mut words = vec![OsString::from("-C"), OsString::from(digits)];
        let letters: String = flags.chars().filter(|c| !c.is_ascii_digit()).collect();
        if !letters.is_empty() || !rest.is_empty() {
            words.push(OsString::from(format!("-{letters}{rest}")));
        }
        Some(words)
    })
}

// Exit code:
//     0 - One or more lines were selected.
//     1 - No lines were selected.
//     >1 - An error occurred.
fn main() -> Result<(), Box<dyn std::error::Error>> {
    plib::diag::init_locale("grep");

    let mut args = Args::parse_from(plib::optarg::keep_leading_equals::<Args>(
        expand_numeric_context(std::env::args_os().collect()),
    ));

    let exit_code = args
        .validate_args()
        .and_then(|_| {
            args.resolve();
            args.into_grep_model()
        })
        .map(|mut grep_model| grep_model.grep())
        .unwrap_or_else(|err| {
            plib::diag::error(&err);
            2
        });

    std::process::exit(exit_code);
}
