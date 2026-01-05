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
    #[arg(long, value_parser = parse_pages, value_name = "FIRST_PAGE[:LAST_PAGE]",
          help = gettext("Begin output at page number FIRST_PAGE, stop at LAST_PAGE if present"))]
    pages: Option<(usize, Option<usize>)>,

    #[arg(long, group = "multi_column", conflicts_with = "merge",
          help = gettext("Produce multi-column output arranged in COLUMN columns"))]
    columns: Option<usize>,

    #[arg(short = 'a', long, requires = "columns",
          help = gettext("Fill columns across the page in round-robin order"))]
    across: bool,

    #[arg(short = 'd', long, help = gettext("Produce double-spaced output"))]
    double_space: bool,

    #[arg(short = 'e', long, value_name = "[CHAR][WIDTH]",
          help = gettext("Expand input tabs to column positions"))]
    expand_tabs: Option<ExpandTabsArg>,

    #[arg(short = 'f', help = gettext("Use form-feed for new pages, pause before first page"))]
    form_feed_with_pause: bool,

    #[arg(short = 'F', long, help = gettext("Use form-feed for new pages"))]
    form_feed: bool,

    #[arg(short = 'h', long, value_name = "HEADER",
          help = gettext("Use string HEADER to replace the file name in page header"))]
    header: Option<String>,

    #[arg(short = 'i', long, value_name = "[CHAR][WIDTH]",
          help = gettext("Replace spaces with tabs in output"))]
    output_tabs: Option<OutputTabsArg>,

    #[arg(short = 'l', long, value_name = "PAGE_LENGTH",
          help = gettext("Override the 66-line default page length"))]
    length: Option<usize>,

    #[arg(short = 'm', long, group = "multi_column",
          help = gettext("Merge files, printing one line from each file side by side"))]
    merge: bool,

    #[arg(short = 'n', long, value_name = "[SEP][DIGITS]",
          help = gettext("Provide line numbering with specified width and separator"))]
    number_lines: Option<NumberLinesArg>,

    #[arg(short = 'N', long, default_value_t = 1, value_name = "NUMBER",
          help = gettext("Start line counting with NUMBER at first line of first page"))]
    first_line_number: usize,

    #[arg(short = 'o', long, default_value_t = 0, value_name = "MARGIN",
          help = gettext("Precede each line with MARGIN space characters"))]
    indent: usize,

    #[arg(short = 'p', long, help = gettext("Pause before beginning each page on terminal output"))]
    pause: bool,

    #[arg(short = 'r', long, help = gettext("Write no diagnostic reports on failure to open files"))]
    no_file_warnings: bool,

    #[arg(short = 's', long, value_parser = parse_separator, value_name = "CHAR", requires = "multi_column",
          help = gettext("Separate text columns by single character CHAR"))]
    separator: Option<char>,

    #[arg(short = 't', long, help = gettext("Omit header and trailer, quit after last line"))]
    omit_header: bool,

    #[arg(short = 'w', long, value_name = "PAGE_WIDTH", requires = "multi_column",
          help = gettext("Set line width to PAGE_WIDTH for multi-column output"))]
    width: Option<usize>,

    #[arg(long, help = gettext("Enable pretty printing of headers"))]
    prettify_headers: bool,

    #[arg(long, action = clap::ArgAction::HelpLong)]
    help: Option<bool>,

    #[arg(help = gettext("Files to print (use - for stdin)"))]
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
    ($t:tt, $option:expr_2021, $default_chr:expr_2021, $default_num:expr_2021) => {
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
                                });
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
