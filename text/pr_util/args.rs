//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
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

// Classification of short option letters for cluster preprocessing.
//
// POSIX `pr` short options:
//   no value: -a -d -f -F -m -p -r -t
//   required value: -h -l -N -o -w
//   optional value (default <tab>): -e -i -n -s
fn short_no_value(c: char) -> bool {
    matches!(c, 'a' | 'd' | 'f' | 'F' | 'm' | 'p' | 'r' | 't')
}
fn short_required_value(c: char) -> bool {
    matches!(c, 'h' | 'l' | 'N' | 'o' | 'w')
}
fn short_optional_value(c: char) -> bool {
    matches!(c, 'e' | 'i' | 'n' | 's')
}

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
        let env_args: Vec<String> = std::env::args().collect();
        let mut out: Vec<String> = Vec::with_capacity(env_args.len());
        let mut iter = env_args.into_iter();

        // Preserve program name (argv[0]) untouched.
        if let Some(prog) = iter.next() {
            out.push(prog);
        }
        for arg in iter {
            preprocess_arg(&arg, &mut out);
        }

        let mut args = Args::parse_from(out);
        args.add_stdin_if_no_files();

        args
    }
}

/// Preprocess a single argv element and push 0+ rewritten elements into `out`.
///
/// Handles POSIX-style short option clustering:
/// * `+PAGES`           -> `--pages=PAGES`
/// * `-COLUMN[REST]`    -> `--columns=COLUMN` [+ `-REST` cluster]
/// * `-CLUSTER`         -> peel optional-value (e/i/n/s) at trailing position
///   and append `=<tab>` so clap accepts it.
///
/// Per POSIX, the value of `-e`, `-i`, `-n`, `-s` must be IMMEDIATELY attached
/// (no whitespace). When the letter is the last char of a cluster with no value
/// attached, the default <tab> is used.
fn preprocess_arg(arg: &str, out: &mut Vec<String>) {
    // +FIRST_PAGE[:LAST_PAGE]
    if let Some(rest) = arg.strip_prefix('+') {
        if rest.chars().next().is_some_and(|c| c.is_ascii_digit()) {
            out.push(format!("--pages={}", rest));
            return;
        }
    }

    // Long options that take an optional value: standalone form gets default.
    // (If the user wants an explicit value, they must use `--name=val`.)
    if matches!(
        arg,
        "--expand_tabs" | "--output_tabs" | "--number_lines" | "--separator"
    ) {
        out.push(format!("{}=\t", arg));
        return;
    }

    // Short option cluster: starts with one '-' (not '--'), len >= 2,
    // and not the bare '-' positional (stdin).
    if let Some(after_dash) = arg.strip_prefix('-') {
        let is_short_cluster =
            !after_dash.is_empty() && !arg.starts_with("--") && !after_dash.starts_with('-');
        if is_short_cluster {
            // Peel leading run of digits as -COLUMN
            let digit_end = after_dash
                .find(|c: char| !c.is_ascii_digit())
                .unwrap_or(after_dash.len());
            if digit_end > 0 {
                let digits = &after_dash[..digit_end];
                out.push(format!("--columns={}", digits));
                let rest = &after_dash[digit_end..];
                if !rest.is_empty() {
                    preprocess_short_cluster(rest, out);
                }
                return;
            }
            preprocess_short_cluster(after_dash, out);
            return;
        }
    }

    out.push(arg.to_string());
}

/// Process a short-option cluster (the part after the leading '-').
///
/// Walks the cluster left-to-right:
///   * `no_value` chars are part of the cluster and contribute no value.
///   * The first `required_value` char consumes the remainder of the token as
///     its value (or pulls from the next argv element); we leave the cluster
///     untouched so clap handles it natively.
///   * The first `optional_value` char either takes the remaining chars as its
///     value, or — if it is the final char with nothing after — gets `=\t`
///     appended so clap accepts the option without requiring a separate arg.
fn preprocess_short_cluster(cluster: &str, out: &mut Vec<String>) {
    let chars: Vec<char> = cluster.chars().collect();
    for (i, &c) in chars.iter().enumerate() {
        if short_no_value(c) {
            continue;
        }
        if short_required_value(c) {
            out.push(format!("-{}", cluster));
            return;
        }
        if short_optional_value(c) {
            if i == chars.len() - 1 {
                out.push(format!("-{}=\t", cluster));
            } else {
                out.push(format!("-{}", cluster));
            }
            return;
        }
        // Unknown short letter: leave for clap to error on.
        out.push(format!("-{}", cluster));
        return;
    }
    // All known no-value letters.
    out.push(format!("-{}", cluster));
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
    /// Whether multi-column cells should be padded/truncated to `column_width`.
    ///
    /// Per POSIX.2024 `pr`:
    ///   "-s ... shall not affect the line truncation or column alignment as
    ///    usually affected by the other options, but shall affect them when
    ///    used in conjunction with the -t and -e options."
    ///
    /// So padding is disabled only when `-s` is combined with `-t` or `-e`.
    pub pad_columns: bool,
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

        // POSIX.2024: -s by itself does not affect truncation/alignment, but
        // combined with -t or -e it does (padding is suppressed).
        let pad_columns =
            !(args.separator.is_some() && (args.omit_header || args.expand_tabs.is_some()));

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
            pad_columns,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{preprocess_arg, preprocess_short_cluster};

    fn run(arg: &str) -> Vec<String> {
        let mut out = Vec::new();
        preprocess_arg(arg, &mut out);
        out
    }

    #[test]
    fn pages_long_form() {
        assert_eq!(run("+1"), vec!["--pages=1".to_string()]);
        assert_eq!(run("+1:5"), vec!["--pages=1:5".to_string()]);
    }

    #[test]
    fn column_short_form_only() {
        assert_eq!(run("-9"), vec!["--columns=9".to_string()]);
        assert_eq!(run("-42"), vec!["--columns=42".to_string()]);
    }

    #[test]
    fn column_with_cluster_suffix() {
        // -4ats -> --columns=4 + -ats=<tab>
        assert_eq!(
            run("-4ats"),
            vec!["--columns=4".to_string(), "-ats=\t".to_string()]
        );
    }

    #[test]
    fn cluster_with_optional_arg_at_end_default_tab() {
        // -ats -> -ats=<tab>
        assert_eq!(run("-ats"), vec!["-ats=\t".to_string()]);
        assert_eq!(run("-s"), vec!["-s=\t".to_string()]);
        assert_eq!(run("-e"), vec!["-e=\t".to_string()]);
        assert_eq!(run("-i"), vec!["-i=\t".to_string()]);
        assert_eq!(run("-n"), vec!["-n=\t".to_string()]);
    }

    #[test]
    fn cluster_with_explicit_optional_arg_value() {
        // -ats, leaves the value attached: clap will see -a -t -s value=','
        assert_eq!(run("-ats,"), vec!["-ats,".to_string()]);
        assert_eq!(run("-s,"), vec!["-s,".to_string()]);
        assert_eq!(run("-n3"), vec!["-n3".to_string()]);
    }

    #[test]
    fn cluster_all_no_value() {
        assert_eq!(run("-at"), vec!["-at".to_string()]);
        assert_eq!(run("-amF"), vec!["-amF".to_string()]);
    }

    #[test]
    fn cluster_with_required_value_letter() {
        // -h is required-value; rest of cluster is value or next argv.
        assert_eq!(run("-h"), vec!["-h".to_string()]);
        assert_eq!(run("-hHDR"), vec!["-hHDR".to_string()]);
        assert_eq!(run("-l20"), vec!["-l20".to_string()]);
        // Combined: -a then -N (value comes from next argv).
        assert_eq!(run("-aN"), vec!["-aN".to_string()]);
    }

    #[test]
    fn long_options_optional_default() {
        assert_eq!(run("--separator"), vec!["--separator=\t".to_string()]);
        assert_eq!(run("--expand_tabs"), vec!["--expand_tabs=\t".to_string()]);
        assert_eq!(run("--output_tabs"), vec!["--output_tabs=\t".to_string()]);
        assert_eq!(run("--number_lines"), vec!["--number_lines=\t".to_string()]);
    }

    #[test]
    fn long_options_with_equals_passthrough() {
        // --separator=, must pass through unchanged.
        assert_eq!(run("--separator=,"), vec!["--separator=,".to_string()]);
    }

    #[test]
    fn positional_args_passthrough() {
        assert_eq!(run("file.txt"), vec!["file.txt".to_string()]);
        assert_eq!(run("-"), vec!["-".to_string()]); // stdin sentinel
        assert_eq!(run("--"), vec!["--".to_string()]); // separator
    }

    #[test]
    fn short_cluster_helper_direct() {
        // direct entry into the cluster helper (no leading '-')
        let mut out = Vec::new();
        preprocess_short_cluster("ats", &mut out);
        assert_eq!(out, vec!["-ats=\t".to_string()]);

        let mut out = Vec::new();
        preprocess_short_cluster("ats,", &mut out);
        assert_eq!(out, vec!["-ats,".to_string()]);
    }
}
