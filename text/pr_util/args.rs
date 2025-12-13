//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::{
    DEFAULT_PAGE_LENGTH, DEFAULT_PAGE_WIDTH, DEFAULT_TAB_WIDTH, NUM_LINES_HEADER_FOOTER,
    PAGE_WIDTH_IF_HAS_SEPARATOR, TAB,
};
use clap::Parser;
use gettextrs::gettext;
use regex::Regex;
use std::path::PathBuf;
use std::str::FromStr;

/// pr - print files
#[derive(Parser)]
#[command(version, about = gettext("pr - print files"), disable_help_flag = true)]
pub struct Args {
    /// Begin output at page number FIRST_PAGE of the formatted input. Stop
    /// printing at LAST_PAGE if present.
    #[arg(long, value_parser = parse_pages, value_name = "FIRST_PAGE[:LAST_PAGE]")]
    pages: Option<(usize, Option<usize>)>,

    /// Produce multi-column output that is arranged in COLUMN columns (the
    /// default shall be 1) and is written down each column in the order in
    /// which the text is received from the input file. This option should not
    /// be used with -m. The options -e and -i shall be assumed for multiple
    /// text-column output. Whether or not text columns are produced with
    /// identical vertical lengths is unspecified, but a text column shall never
    /// exceed the length of the page (see the -l option). When used with -t,
    /// use the minimum number of lines to write the output.
    #[arg(long, group = "multi_column", conflicts_with = "merge")]
    columns: Option<usize>,

    /// Modify the effect of the - column option so that the columns are filled
    /// across the page in a round-robin order (for example, when column is 2,
    /// the first input line heads column 1, the second heads column 2, the
    /// third is the second line in column 1, and so on).
    #[arg(short = 'a', long, requires = "columns")]
    across: bool,

    /// Produce output that is double-spaced; append an extra <newline>
    /// following every <newline> found in the input.
    #[arg(short = 'd', long)]
    double_space: bool,

    /// Expand each input <tab> to the next greater column position specified by
    /// the formula n* WIDTH+1, where n is an integer > 0. If WIDTH is zero or
    /// is omitted, it shall default to 8. All <tab> characters in the input
    /// shall be expanded into the appropriate number of <space> characters. If
    /// any non-digit character, CHAR, is specified, it shall be used as the
    /// input <tab>. If the first character of the -e option-argument is a digit,
    /// the entire option-argument shall be assumed to be gap.
    #[arg(short = 'e', long, value_name = "[CHAR][WIDTH]")]
    expand_tabs: Option<ExpandTabsArg>,

    /// Use a <form-feed> for new pages, instead of the default behavior that
    /// uses a sequence of <newline> characters. Pause before beginning the
    /// first page if the standard output is associated with a terminal.
    #[arg(short = 'f')]
    form_feed_with_pause: bool,

    /// Use a <form-feed> for new pages, instead of the default behavior that
    /// uses a sequence of <newline> characters.
    #[arg(short = 'F', long)]
    form_feed: bool,

    /// Use the string HEADER to replace the contents of the file operand in the
    /// page header.
    #[arg(short = 'h', long, value_name = "HEADER")]
    header: Option<String>,

    /// In output, replace <space> characters with <tab> characters wherever one
    /// or more adjacent <space> characters reach column positions WIDTH+1,
    /// 2* WIDTH+1, 3* WIDTH+1, and so on. If WIDTH is zero or is omitted,
    /// default tab  settings at every eighth column position shall be assumed.
    /// If any non-digit character, CHAR, is specified, it shall be used as the
    /// output <tab>. If the first character of the -i option-argument is a
    /// digit, the entire option-argument shall be assumed to be WIDTH.
    #[arg(short = 'i', long, value_name = "[CHAR][WIDTH]")]
    output_tabs: Option<OutputTabsArg>,

    /// Override the 66-line default and reset the page length to lines. If
    /// lines is not greater than the sum of both the header and trailer depths
    /// (in lines), the pr utility shall suppress both the header and trailer,
    /// as if the -t option were in effect.
    #[arg(short = 'l', long, value_name = "PAGE_LENGTH")]
    length: Option<usize>,

    /// Merge files. Standard output shall be formatted so the pr utility writes
    /// one line from each file specified by a file operand, side by side into
    /// text columns of equal fixed widths, in terms of the number of column
    /// positions. Implementations shall support merging of at least nine file
    /// operands.
    #[arg(short = 'm', long, group = "multi_column")]
    merge: bool,

    /// Provide DIGITS-digit line numbering (default for DIGITS shall be 5). The
    /// number shall occupy the first DIGITS column positions of each text
    /// column of default output or each line of -m output. If SEP (any
    /// non-digit character) is given, it shall be appended to the line number
    /// to separate it from whatever follows (default for SEP is a <tab>).
    #[arg(short = 'n', long, value_name = "[SEP][DIGITS]")]
    number_lines: Option<NumberLinesArg>,

    /// Start counting with NUMBER at 1st line of first page printed.
    #[arg(short = 'N', long, default_value_t = 1, value_name = "NUMBER")]
    first_line_number: usize,

    /// Each line of output shall be preceded by MARGIN <space> characters. If
    /// the -o option is not specified, the default MARGIN shall be zero. The
    /// space taken is in addition to the output line width (see the -w option
    /// below).
    #[arg(short = 'o', long, default_value_t = 0, value_name = "MARGIN")]
    indent: usize,

    /// Pause before beginning each page if the standard output is directed to a
    /// terminal (pr shall write an <alert> to standard error and wait for a
    /// <carriage-return> to be read on /dev/tty).
    #[arg(short = 'p', long)]
    pause: bool,

    /// Write no diagnostic reports on failure to open files.
    #[arg(short = 'r', long)]
    no_file_warnings: bool,

    /// Separate text columns by the single character CHAR instead of by the
    /// appropriate number of <space> characters.
    #[arg(
        short = 's',
        long,
        value_parser = parse_separator,
        value_name = "CHAR",
        requires = "multi_column"
    )]
    separator: Option<char>,

    /// Write neither the five-line identifying header nor the five-line trailer
    /// usually supplied for each page. Quit writing after the last line of each
    /// file without spacing to the end of the page.
    #[arg(short = 't', long)]
    omit_header: bool,

    /// Set the width of the line to PAGE_WIDTH column positions for multiple
    /// text-column output only. If the -w option is not specified and the -s
    /// option is not specified, the default width shall be 72. If the -w option
    /// is not specified and the -s option is specified, the default width shall
    /// be 512.
    #[arg(
        short = 'w',
        long,
        value_name = "PAGE_WIDTH",
        requires = "multi_column"
    )]
    width: Option<usize>,

    /// Enable pretty printing of headers.
    #[arg(long)]
    prettify_headers: bool,

    /// Print help
    #[arg(long, action = clap::ArgAction::HelpLong)]
    help: Option<bool>,

    /// A pathname of a file to be written. If no file operands are specified,
    /// or if a file operand is '-', the standard input shall be used.
    #[arg()]
    file: Vec<PathBuf>,
}

impl Args {
    pub fn add_stdin_if_no_files(&mut self) {
        if self.file.is_empty() {
            self.file.push("-".into());
        }
    }

    pub fn file(&self) -> &[PathBuf] {
        &self.file
    }

    pub fn parse_custom() -> Self {
        let page_column_regex = Regex::new(r"^\+\d+(?::\d+)?|-\d+\b").unwrap();

        let mut env_args: Vec<_> = std::env::args()
            .map(|s| {
                // Map the arguments +FIRST_PAGE[:LAST_PAGE] and -COLUMN
                // to something that `clap` can parse
                if page_column_regex.is_match(&s) {
                    if s.starts_with('+') {
                        format!("--pages={}", s.strip_prefix('+').unwrap())
                    } else {
                        format!("--columns={}", s.strip_prefix('-').unwrap())
                    }
                } else {
                    s
                }
            })
            .collect();

        // Need to do arg surgery for -e, -i, -n, -s if they are supplied like a
        // flag because `clap` only allows that for `bool`.
        //
        // Consider the command:
        //
        // `pr -s c`
        //
        // It's ambiguous whether the `c` here is part of `-s` or is a file.
        // This implementation imitates coreutils' pr by always assuming that
        // `c` is not part of `-s`.
        for arg in env_args.iter_mut() {
            match arg.as_str() {
                "-e" | "-i" | "-n" | "-s" | "--expand_tabs" | "--output_tabs"
                | "--number_lines" | "--separator" => {
                    let new_arg = format!("{}=\t", arg);
                    *arg = new_arg;
                }
                _ => (),
            }
        }

        let mut args = Args::parse_from(env_args);
        args.add_stdin_if_no_files();

        args
    }
}

fn parse_pages(s: &str) -> Result<(usize, Option<usize>), String> {
    let page_option_regex = Regex::new(r"^(\d+)(?::(\d+))?\b").unwrap();
    if let Some(caps) = page_option_regex.captures(s) {
        let first_page = caps.get(1).unwrap().as_str().parse().unwrap();
        let last_page = caps.get(2).map(|x| x.as_str().parse().unwrap());
        return Ok((first_page, last_page));
    }

    Err(format!("invalid value for --pages: {s}"))
}

fn parse_separator(s: &str) -> Result<char, String> {
    if s.is_empty() {
        return Ok(TAB);
    } else {
        let mut chars = s.chars();

        // Only a single character is allowed
        if let Some(c) = chars.next() {
            if chars.next().is_none() {
                return Ok(c);
            }
        }
    }
    Err(format!("invalid value for --separator: {s}"))
}

macro_rules! impl_char_and_number {
    ($t:tt, $option:expr, $default_chr:expr, $default_num:expr) => {
        #[derive(Clone)]
        pub struct $t {
            chr: char,
            num: usize,
        }

        impl Default for $t {
            fn default() -> Self {
                Self {
                    chr: $default_chr,
                    num: $default_num,
                }
            }
        }

        impl FromStr for $t {
            type Err = String;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                let mut char_indices = s.char_indices().peekable();

                let first_char = match char_indices.peek() {
                    Some((_, chr)) => chr,
                    None => return Ok(Self::default()),
                };

                // Entire argument to the option is a number
                if first_char.is_ascii_digit() {
                    if let Ok(num) = s.parse() {
                        return Ok(Self { chr: '\t', num });
                    }
                } else {
                    if let Some((_, chr)) = char_indices.next() {
                        match char_indices.next() {
                            None => {
                                return Ok(Self {
                                    chr,
                                    ..Default::default()
                                })
                            }
                            Some((start, _)) => {
                                if let Ok(num) = &s[start..].parse() {
                                    return Ok(Self { chr, num: *num });
                                }
                            }
                        }
                    }
                }

                Err(format!("invalid value for --{}: {s}", $option))
            }
        }
    };
}

impl_char_and_number!(ExpandTabsArg, "expand_tabs", '\t', DEFAULT_TAB_WIDTH);
impl_char_and_number!(OutputTabsArg, "output_tabs", '\t', DEFAULT_TAB_WIDTH);
impl_char_and_number!(NumberLinesArg, "number_lines", '\t', 5);

pub struct Parameters {
    pub page_number_start: usize,
    pub line_number_start: usize,
    pub page_width: usize,
    pub first_page: usize,
    pub last_page: Option<usize>,
    pub num_columns: usize,
    pub body_lines_per_page: usize,
    pub header: Option<String>,
    pub indent: usize,
    pub across: bool,
    pub merge: bool,
    pub omit_header: bool,
    pub no_file_warnings: bool,
    pub form_feed: bool,
    pub pause: bool,
    pub strict_posix: bool,
    pub column_separator: char,
    pub line_separator: String,
    pub expand_tabs: Option<(char, usize)>,
    pub output_tabs: Option<(char, usize)>,
    pub number_lines: Option<(char, usize)>,
}

impl Parameters {
    pub fn new(args: &Args) -> Self {
        let page_number_start = 1;
        let line_number_start = args.first_line_number;

        let page_width = args.width.unwrap_or(if args.separator.is_none() {
            DEFAULT_PAGE_WIDTH
        } else {
            PAGE_WIDTH_IF_HAS_SEPARATOR
        });

        let (first_page, last_page) = args.pages.unwrap_or((1, None));

        let num_columns = if args.merge {
            args.file.len()
        } else {
            args.columns.unwrap_or(1)
        };

        let form_feed = args.form_feed || args.form_feed_with_pause;
        let pause = args.pause || args.form_feed_with_pause;

        let column_separator = args.separator.unwrap_or(' ');

        let line_separator = if args.double_space {
            "\n".repeat(2)
        } else {
            String::from("\n")
        };

        let expand_tabs = args.expand_tabs.as_ref().map(|x| {
            if x.num == 0 {
                (x.chr, DEFAULT_TAB_WIDTH)
            } else {
                (x.chr, x.num)
            }
        });
        let output_tabs = args.output_tabs.as_ref().map(|x| {
            if x.num == 0 {
                (x.chr, DEFAULT_TAB_WIDTH)
            } else {
                (x.chr, x.num)
            }
        });
        let number_lines = args.number_lines.as_ref().map(|x| (x.chr, x.num));

        let page_length = args.length.unwrap_or(DEFAULT_PAGE_LENGTH);

        let mut omit_header = args.omit_header;

        // From the specification:
        // If *lines* is not greater than the sum of both the header and trailer
        // depths (in lines), the pr utility shall suppress both the header and
        // trailer, as if the -t option were in effect.
        if page_length <= NUM_LINES_HEADER_FOOTER {
            omit_header = true;
        }

        let num_lines_header_footer = if omit_header {
            0
        } else {
            NUM_LINES_HEADER_FOOTER
        };

        let mut body_lines_per_page = page_length - num_lines_header_footer;

        if args.double_space {
            body_lines_per_page /= 2;
        }

        Self {
            page_number_start,
            line_number_start,
            page_width,
            first_page,
            last_page,
            num_columns,
            body_lines_per_page,
            header: args.header.clone(),
            indent: args.indent,
            across: args.across,
            merge: args.merge,
            omit_header,
            no_file_warnings: args.no_file_warnings,
            form_feed,
            pause,
            strict_posix: !args.prettify_headers,
            column_separator,
            line_separator,
            expand_tabs,
            output_tabs,
            number_lines,
        }
    }
}
