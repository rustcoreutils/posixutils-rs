//
// Copyright (c) 2024 Bloq Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

mod pr_util;

use chrono::{DateTime, Local};
use gettextrs::{bind_textdomain_codeset, gettext, textdomain};
use plib::PROJECT_NAME;
use std::fmt::Write as _;
use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use self::pr_util::{line_transform, Args, PageIterator, Parameters};

const FORM_FEED: char = 12 as char;
const TAB: char = '\t';
const BACKSPACE: char = 8 as char;
const ALERT: char = 7 as char;
const CARRIAGE_RETURN: char = '\r';

const DATE_TIME_FORMAT: &str = "%b %d %H:%M %Y";

const DEFAULT_PAGE_WIDTH: usize = 72;
const PAGE_WIDTH_IF_HAS_SEPARATOR: usize = 512;

const DEFAULT_TAB_WIDTH: usize = 8;
const DEFAULT_PAGE_LENGTH: usize = 66;
const NUM_LINES_HEADER_FOOTER: usize = 10; // 5 + 5 each

pub struct Line {
    line: String,
    ends_on_form_feed: bool,
    is_padding: bool,
}

pub struct Page {
    lines: Vec<io::Result<Line>>,
    num_nonpadding_lines: usize,
}

// Helper trait for converting `std::fmt::Result` -> `std::io::Result<()>`
trait IntoIoResult {
    fn into_io_result(self) -> io::Result<()>;
}

impl IntoIoResult for std::fmt::Result {
    fn into_io_result(self) -> io::Result<()> {
        self.map_err(|e| io::Error::other(format!("{e}")))
    }
}

// Used in the -p option
fn pause() -> io::Result<()> {
    // Must print \a to stderr.
    eprint!("{ALERT}");

    let mut reader = io::BufReader::new(io::stdin().lock());

    // Wait for \r
    let mut chr = 0;
    loop {
        reader.read_exact(std::array::from_mut(&mut chr))?;
        if chr as char == CARRIAGE_RETURN {
            break;
        }
    }
    Ok(())
}

fn print_header(
    dt: &str,
    path: &str,
    page_number: usize,
    header_replacement: Option<&str>,
    page_width: usize,
    strict_posix: bool,
) {
    // "Page" seems to be the only thing that's translated.
    //
    // "The LC_MESSAGES locale category affects the spelling of page;"
    //
    // https://www.gnu.org/software/coreutils/manual/html_node/pr-invocation.html
    let page = gettext("Page");

    let header = header_replacement.unwrap_or(path);

    if strict_posix {
        print!("\n\n{} {} {} {}\n\n\n", dt, header, page, page_number);
    } else {
        let page_number_string = format!("{} {}", page, page_number);

        // Guard against underflow
        let width = page_width.saturating_sub(dt.len() + page_number_string.len() + 2);

        // Uses coreutils' fancier formatting instead of the POSIX formatting
        print!("\n\n{} {:^width$} {}\n\n\n", dt, header, page_number_string);
    }
}

fn print_footer(form_feed_as_page_separator: bool) {
    print!("\n\n\n\n");
    if form_feed_as_page_separator {
        print!("{FORM_FEED}");
    } else {
        println!("");
    }
}

// For the -n option
fn write_line_number(
    output_line: &mut String,
    params: &Parameters,
    line_number: usize,
) -> io::Result<()> {
    if let Some((separator, width)) = params.number_lines {
        let line_number_str = line_number.to_string();

        // Truncate upper digits to fit in `width`
        // `1234` -> `234` if `width` == 3
        let s: &str = if line_number_str.len() > width {
            &line_number_str[line_number_str.len() - width..]
        } else {
            &line_number_str
        };

        write!(output_line, "{:>width$}{separator}", s).into_io_result()?;
    }

    Ok(())
}

fn write_line_content(
    output_line: &mut String,
    params: &Parameters,
    line: &Line,
    line_number: usize,
    column_width: usize,
) -> io::Result<()> {
    if line.is_padding {
        write!(output_line, "{:width$}", "", width = column_width).into_io_result()?;
        return Ok(());
    }

    fn is_non_printable(c: char) -> bool {
        // NOTE: This is only for ASCII characters
        (c as u8) < 32
    }

    // Need to transform (line number + line) independent of the whole output
    let mut tmp = String::new();

    // Merge mode only writes the line number at the start of each line, not on
    // every column
    if !params.merge {
        write_line_number(&mut tmp, params, line_number)?;
    }
    write!(&mut tmp, "{}", &line.line).into_io_result()?;

    // -e
    line_transform::expand_tabs(&mut tmp, params.expand_tabs);
    // -i
    line_transform::replace_spaces(&mut tmp, params.output_tabs);

    if params.num_columns == 1 {
        // Single column output is neither padded nor truncated
        write!(output_line, "{}", &tmp).into_io_result()?;
    } else {
        let mut width = column_width;

        // Control characters have 0 width except backspace where it is -1. See:
        //
        // https://github.com/coreutils/coreutils/blob/f56ae60585cff021e92c11c1d4917fe7a7dce38b/src/pr.c#L2310-L2317
        for c in line.line.chars() {
            if c == BACKSPACE {
                width += 2;
            } else if is_non_printable(c) {
                width += 1;
            }
        }

        // Pad or truncate
        write!(output_line, "{:width$.width$}", &tmp).into_io_result()?;
    }

    Ok(())
}

/// Get the current date as a string formatted according to pr's spec.
fn datetime_now() -> String {
    let dt = Local::now().to_utc();
    dt.format(DATE_TIME_FORMAT).to_string()
}

/// Calculate the column width.
///
/// Divide the page width evenly between the columns taking into account that
/// each column except the last has an extra 1 character wide column separator.
fn column_width(num_columns: usize, page_width: usize) -> usize {
    // Derived by solving for w in this inequality:
    //
    // (num_columns - 1)*(w + 1) + w <= page_width

    (page_width - num_columns + 1) / num_columns
}

fn create_stream(path: &Path) -> io::Result<Box<dyn Read>> {
    if path.as_os_str() == "-" {
        Ok(Box::new(io::stdin().lock()))
    } else {
        Ok(Box::new(fs::File::open(path)?))
    }
}

fn pr_serial(path: &PathBuf, params: &Parameters) -> io::Result<()> {
    let dt = if path.as_os_str() == "-" {
        datetime_now()
    } else {
        let metadata = fs::metadata(path)?;
        let last_modified_time = metadata.modified()?;
        let dt: DateTime<Local> = last_modified_time.into();
        dt.format(DATE_TIME_FORMAT).to_string()
    };

    let stream = create_stream(path)?;

    let column_width = column_width(params.num_columns, params.page_width);

    let mut page_number = params.page_number_start;
    let mut line_number = params.line_number_start;

    let page_iterator = PageIterator::new(stream, params.num_columns * params.body_lines_per_page);

    // Multi-column across mode
    if params.across {
        let mut output_line = String::with_capacity(params.page_width);

        for page in page_iterator {
            // +FIRST_PAGE[:LAST_PAGE]
            if page_number < params.first_page {
                page_number += 1;
                continue;
            }
            if let Some(last_page) = params.last_page {
                if page_number > last_page {
                    break;
                }
            }

            // -p
            if params.pause {
                pause()?;
            }

            if !params.omit_header {
                print_header(
                    &dt,
                    &*path.to_string_lossy(),
                    page_number,
                    params.header.as_deref(),
                    params.page_width,
                    params.strict_posix,
                );
            }

            let required_rows = page.num_nonpadding_lines.div_ceil(params.num_columns);

            let mut line_iterator = page.lines.into_iter();
            // Fill the line buffers by row:
            // | 0 | 1 | 2 | 3 |
            // | 4 | 5 | 6 | 7 |
            for j in 0.. {
                if line_iterator.len() == 0 {
                    break;
                }
                // -t requires only the minimum amount of lines to be written
                if params.omit_header && j >= required_rows {
                    break;
                }

                for i in 0..params.num_columns {
                    // Should not return a `None` because `PageIterator` fills
                    // an incomplete page with empty lines
                    let line = line_iterator.next().unwrap();

                    write_line_content(
                        &mut output_line,
                        params,
                        &line?,
                        line_number,
                        column_width,
                    )?;
                    line_number += 1;

                    let last_index = params.num_columns - 1;
                    if i < last_index {
                        output_line.push(params.column_separator);
                    }
                }

                output_line.push_str(&params.line_separator);
                print!("{:width$}{}", "", output_line, width = params.indent);

                // Reusing the line buffer
                output_line.clear();
            }

            if !params.omit_header {
                print_footer(params.form_feed);
            }

            page_number += 1;
        }

        Ok(())

    // Default multi-column mode
    } else {
        // It's not possible to print a whole line immediately in the
        // multi-column, top-to-bottom mode so we create
        // `body_lines_per_page` buffers that will hold the line rows to be
        // printed.
        let mut output_lines: Vec<_> = (0..params.body_lines_per_page)
            .map(|_| String::with_capacity(params.page_width))
            .collect();

        for page in page_iterator {
            // +FIRST_PAGE[:LAST_PAGE]
            if page_number < params.first_page {
                page_number += 1;
                continue;
            }
            if let Some(last_page) = params.last_page {
                if page_number > last_page {
                    break;
                }
            }

            // -p
            if params.pause {
                pause()?;
            }

            if !params.omit_header {
                print_header(
                    &dt,
                    &*path.to_string_lossy(),
                    page_number,
                    params.header.as_deref(),
                    params.page_width,
                    params.strict_posix,
                );
            }

            let required_rows = page.num_nonpadding_lines.div_ceil(params.num_columns);
            assert!(required_rows <= output_lines.len());
            // Fill the line buffers by column:
            // | 0 | 2 | 4 | 6 |
            // | 1 | 3 | 5 | 7 |
            // before printing them
            for (i, line) in page.lines.into_iter().enumerate() {
                let num_output = output_lines.len();

                // Identical vertical lengths is unspecified, but is required to
                // minimize the lines when -t is used. Might as well always do
                // it.
                let output_line_idx = if required_rows < output_lines.len() {
                    if i < page.num_nonpadding_lines {
                        i % required_rows
                    } else {
                        required_rows + (i % (num_output - required_rows))
                    }
                } else {
                    i % num_output
                };

                let output_line = &mut output_lines[output_line_idx];

                write_line_content(output_line, params, &line?, line_number, column_width)?;
                line_number += 1;

                let current_column = i / num_output;

                let last_index = params.num_columns - 1;
                if current_column < last_index {
                    output_line.push(params.column_separator);
                }
            }

            for (i, output_line) in output_lines.iter_mut().enumerate() {
                // Quit writing after the last line of each file without spacing
                // to the end of the page.
                if params.omit_header && i >= required_rows {
                    break;
                }

                output_line.push_str(&params.line_separator);
                print!("{:width$}{}", "", output_line, width = params.indent);

                // Reusing the line buffer
                output_line.clear();
            }

            if !params.omit_header {
                print_footer(params.form_feed);
            }

            page_number += 1;
        }

        Ok(())
    }
}

fn pr_merged(paths: &[PathBuf], params: &Parameters) -> io::Result<()> {
    let dt = datetime_now();

    let mut page_iterators = Vec::with_capacity(paths.len());
    for p in paths {
        let stream = create_stream(p)?;
        let it = PageIterator::new(stream, params.body_lines_per_page);
        page_iterators.push(it);
    }

    let column_width = column_width(params.num_columns, params.page_width);

    let mut page_number = params.page_number_start;
    let mut line_number = params.line_number_start;
    loop {
        // The `next` methods of the `PageIterator`s need to be called at the
        // start of the loop
        let pages: Vec<_> = page_iterators.iter_mut().map(|x| x.next()).collect();

        // No more pages
        if pages.iter().all(|x| x.is_none()) {
            break;
        }

        // +FIRST_PAGE[:LAST_PAGE]
        if page_number < params.first_page {
            page_number += 1;
            continue;
        }
        if let Some(last_page) = params.last_page {
            if page_number > last_page {
                break;
            }
        }

        // -p
        if params.pause {
            pause()?;
        }

        if !params.omit_header {
            print_header(
                &dt,
                "", // file path is always empty in merged mode
                page_number,
                params.header.as_deref(),
                params.page_width,
                params.strict_posix,
            );
        }

        let mut required_rows = 0;
        for page in pages.iter() {
            if let Some(p) = page {
                if p.num_nonpadding_lines > required_rows {
                    required_rows = p.num_nonpadding_lines;
                }
            }
        }

        let mut pages: Vec<_> = pages
            .into_iter()
            .map(|p| p.map(|p| p.lines.into_iter()))
            .collect();

        // for each line
        for j in 0.. {
            let has_remaining_lines = pages
                .iter()
                .filter_map(|it| it.as_ref())
                .any(|it| it.len() != 0);
            if !has_remaining_lines {
                break;
            }

            // Minimum amount of lines for -t
            if params.omit_header && j >= required_rows {
                break;
            }

            let mut output_line = String::with_capacity(params.page_width);

            write_line_number(&mut output_line, params, line_number)?;
            line_number += 1;

            let last_index = pages.len() - 1;

            // for each column
            for (i, it) in pages.iter_mut().enumerate() {
                if let Some(it) = it {
                    if let Some(line) = it.next() {
                        write_line_content(
                            &mut output_line,
                            params,
                            &line?,
                            line_number,
                            column_width,
                        )?;
                    }
                }

                if i < last_index {
                    output_line.push(params.column_separator);
                }
            }

            output_line.push_str(&params.line_separator);
            print!("{:width$}{}", "", output_line, width = params.indent);
        }

        if !params.omit_header {
            print_footer(params.form_feed);
        }

        page_number += 1;
    }

    Ok(())
}

fn main() -> ExitCode {
    // Initialize translation system
    textdomain(PROJECT_NAME).unwrap();
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8").unwrap();

    let args = Args::parse_custom();

    let params = Parameters::new(&args);

    if params.merge {
        match pr_merged(args.file(), &params) {
            Ok(_) => ExitCode::SUCCESS,
            Err(e) => {
                if !params.no_file_warnings {
                    eprintln!("{e}");
                }
                ExitCode::FAILURE
            }
        }
    } else {
        let mut success = true;
        for file in args.file() {
            if let Err(e) = pr_serial(file, &params) {
                if !params.no_file_warnings {
                    eprintln!("{e}");
                }
                success = false;
            }
        }
        if success {
            ExitCode::SUCCESS
        } else {
            ExitCode::FAILURE
        }
    }
}
