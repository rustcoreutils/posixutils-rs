//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

mod ls_util;

use self::ls_util::{ls_from_utf8_lossy, Entry, LongFormatPadding, MultiColumnPadding};
use clap::{CommandFactory, FromArgMatches, Parser};
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::{platform::P_WINSIZE_REQUEST_CODE, PROJECT_NAME};
use std::{
    collections::HashMap,
    ffi::{CString, OsStr},
    io,
    mem::MaybeUninit,
    os::unix::{ffi::OsStrExt, fs::MetadataExt},
    path::{Path, PathBuf},
    process::ExitCode,
    sync::atomic::{AtomicU8, Ordering},
};

/// ls - list directory contents
#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// Write out all directory entries, including those whose names begin with
    /// a <period> ( '.' ).
    #[arg(short = 'a', long, overrides_with_all = ["almost_all", "all"])]
    all: bool,

    /// Write out all directory entries, including those whose names begin with
    /// a <period> ( '.' ) but excluding the entries dot and dot-dot (if they
    /// exist).
    #[arg(short = 'A', long, overrides_with_all = ["almost_all", "all"])]
    almost_all: bool,

    /// Use time of last modification of the file status information (see XBD
    /// <sys/stat.h>) instead of last modification of the file itself for
    /// sorting ( -t) or writing (-l).
    #[arg(
        short = 'c',
        overrides_with_all = [
            "use_last_status_change_time",
            "use_last_access_time"
        ]
    )]
    use_last_status_change_time: bool,

    /// Write multi-text-column output with entries sorted down the columns,
    /// according to the collating sequence. The number of text columns and the
    /// column separator characters are unspecified, but should be adapted to
    /// the nature of the output device. This option disables long format
    /// output.
    #[arg(
        short = 'C',
        overrides_with_all = [
            "multi_column",
            "stream_output_format",
            "muti_column_across",
            "one_entry_per_line",
        ]
    )]
    multi_column: bool,

    /// Do not follow symbolic links named as operands unless the -H or -L
    /// options are specified. Do not treat directories differently than other
    /// types of files. The use of -d with -R or -f produces unspecified
    /// results.
    #[arg(short = 'd', long)]
    directory: bool,

    /// List the entries in directory operands in the order they appear in the
    /// directory. The behavior for non-directory operands is unspecified.
    /// This option shall turn on -a. When -f is specified, any occurrences of
    /// the -r, -S, and -t options shall be ignored and any occurrences of the
    /// -A, -g, -l, -n, -o, and -s options may be ignored. The use of -f with
    /// -R or -d produces unspecified results.
    #[arg(
        short = 'f',
        overrides_with_all = [
            "sort_by_file_size",
            "sort_by_directory_order",
            "sort_by_last_modified_time",
        ]
    )]
    sort_by_directory_order: bool,

    /// Do not follow symbolic links named as operands unless the -H or -L
    /// options are specified. Write a <slash> ( '/' ) immediately after each
    /// pathname that is a directory, an <asterisk> ( '*' ) after each that is
    /// executable, a <vertical-line> ( '|' ) after each that is a FIFO, and an
    /// at-sign ( '@' ) after each that is a symbolic link. For other file
    /// types, other symbols may be written.
    #[arg(
        short = 'F',
        long,
        overrides_with_all = ["classify", "write_slash_if_directory"]
    )]
    classify: bool,

    /// Turn on the -l (ell) option, but disable writing the file's owner name
    /// or number. Disable the -C, -m, and -x options.
    #[arg(short = 'g')]
    long_format_without_owner: bool,

    /// Evaluate the file information and file type for symbolic links specified
    /// on the command line to be those of the file referenced by the link, and
    /// not the link itself; however, ls shall write the name of the link itself
    /// and not the file referenced by the link.
    #[arg(
        short = 'H',
        long,
        overrides_with_all = ["dereference_command_line", "dereference"]
    )]
    dereference_command_line: bool,

    /// For each file, write the file's file serial number.
    #[arg(short = 'i', long, overrides_with = "inode")]
    inode: bool,

    /// Set the block size for the -s option and the per-directory block count
    /// written for the -l, -n, -s, -g, and -o options to 1024
    /// bytes.
    #[arg(short = 'k', long)]
    kibibytes: bool,

    /// (The letter ell.) Do not follow symbolic links named as operands unless
    /// the -H or -L options are specified. Write out in long format.
    /// Disable the -C, -m, and -x options.
    #[arg(short = 'l')]
    long_format: bool,

    /// Evaluate the file information and file type for all symbolic links
    /// (whether named on the command line or encountered in a file hierarchy)
    /// to be those of the file referenced by the link, and not the link itself;
    /// however, ls shall write the name of the link itself and not the file
    /// referenced by the link. When -L is used with -l, write the contents of
    /// symbolic links in the long format.
    #[arg(
        short = 'L',
        long,
        overrides_with_all = ["dereference_command_line", "dereference"]
    )]
    dereference: bool,

    /// Stream output format; list pathnames across the page, separated by a
    /// <comma> character followed by a <space> character. Use a <newline>
    /// character as the list terminator and after the separator sequence when
    /// there is not room on a line for the next list entry. This option
    /// disables long format output.
    #[arg(
        short = 'm',
        overrides_with_all = [
            "multi_column",
            "stream_output_format",
            "muti_column_across",
            "one_entry_per_line",
        ]
    )]
    stream_output_format: bool,

    /// Turn on the -l (ell) option, but when writing the file's owner or group,
    /// write the file's numeric UID or GID rather than the user or group name,
    /// respectively. Disable the -C, -m, and -x options.
    #[arg(short = 'n', long = "numeric-uid-gid")]
    long_format_numeric_uid_gid: bool,

    /// Turn on the -l (ell) option, but disable writing the file's group name
    /// or number. Disable the -C, -m, and -x options.
    #[arg(short = 'o')]
    long_format_without_group: bool,

    /// Write a <slash> ( '/' ) after each filename if that file is a directory.
    #[arg(
        short = 'p',
        overrides_with_all = ["classify", "write_slash_if_directory"]
    )]
    write_slash_if_directory: bool,

    /// Force each instance of non-printable filename characters and <tab>
    /// characters to be written as the <question-mark> ( '?' ) character.
    /// Implementations may provide this option by default if the output is to a
    /// terminal device.
    #[arg(short = 'q', long)]
    hide_control_chars: bool,

    /// Reverse the order of the sort to get reverse collating sequence oldest
    /// first, or smallest file size first depending on the other options given.
    #[arg(short = 'r', long = "reverse")]
    reverse_sorting: bool,

    /// Recursively list subdirectories encountered. When a symbolic link to a
    /// directory is encountered, the directory shall not be recursively listed
    /// unless the -L option is specified. The use of -R with -d or -f produces
    /// unspecified results.
    #[arg(short = 'R', long)]
    recursive: bool,

    /// Indicate the total number of file system blocks consumed by each file
    /// displayed. If the -k option is also specified, the block size shall be
    /// 1024 bytes; otherwise, the block size is implementation-defined.
    #[arg(short = 's', long = "size")]
    display_size: bool,

    /// Sort with the primary key being file size (in decreasing order) and the
    /// secondary key being filename in the collating sequence (in increasing
    /// order).
    #[arg(
        short = 'S',
        overrides_with_all = [
            "sort_by_file_size",
            "sort_by_directory_order",
            "sort_by_last_modified_time",
        ]
    )]
    sort_by_file_size: bool,

    /// Sort with the primary key being time modified (most recently modified
    /// first) and the secondary key being filename in the collating sequence.
    /// For a symbolic link, the time used as the sort key is that of the
    /// symbolic link itself, unless ls is evaluating its file information to be
    /// that of the file referenced by the link (see the -H and -L options).
    #[arg(
        short = 't',
        overrides_with_all = [
            "sort_by_file_size",
            "sort_by_directory_order",
            "sort_by_last_modified_time",
        ]
    )]
    sort_by_last_modified_time: bool,

    /// Use time of last access instead of last modification of the file for
    /// sorting (-t) or writing (-l).
    #[arg(
        short = 'u',
        overrides_with_all = [
            "use_last_status_change_time",
            "use_last_access_time"
        ]
    )]
    use_last_access_time: bool,

    /// The same as -C, except that the multi-text-column output is produced
    /// with entries sorted across, rather than down, the columns. This option
    /// disables long format output.
    #[arg(
        short = 'x',
        overrides_with_all = [
            "multi_column",
            "stream_output_format",
            "muti_column_across",
            "one_entry_per_line",
        ]
    )]
    muti_column_across: bool,

    /// (The numeric digit one.) Force output to be one entry per line. This
    /// option does not disable long format output. (Long format output is
    /// enabled by -g, -l (ell), -n, and -o; and disabled by -C, -m, and -x.)
    #[arg(
        short = '1',
        overrides_with_all = [
            "multi_column",
            "stream_output_format",
            "muti_column_across",
            "one_entry_per_line",
        ]
    )]
    one_entry_per_line: bool,

    /// A pathname of a file to be written. If the file specified is not found,
    /// a diagnostic message shall be output on standard error.
    #[arg()]
    file: Vec<PathBuf>,
}

const DATE_TIME_FORMAT_RECENT: &str = "%b %d %H:%M";
const DATE_TIME_FORMAT_OLD_OR_FUTURE: &str = "%b %d  %Y"; // Two spaces between %d and %Y
const BLOCK_SIZE: u64 = 512;
const BLOCK_SIZE_KIBIBYTES: u64 = 1024;
const COLUMN_SPACING: usize = 2; // How many spaces in the column separator
const SKIP_OPTIMAL_COLUMN_WIDTH_THRESHOLD: usize = 1000;

struct LongFormatOptions {
    numeric_uid_gid: bool,
    without_owner: bool,
    without_group: bool,
}

enum OutputFormat {
    MultiColumn,
    MultiColumnAcross,
    StreamOutputFormat,
    OneEntryPerLine,
    LongFormat(LongFormatOptions),
}

enum SortBy {
    Lexicographical,
    FileSize,
    DirectoryOrder,
    Time,
}

enum ClassifyFiles {
    Complete,
    DirectoryOrNotDirectory,
    None,
}

enum DereferenceSymbolicLink {
    CommandLine,
    All,
    None,
}

enum FileTimeOption {
    LastModificationTime,
    LastStatusChangeTime,
    LastAcessTime,
}

enum FileInclusion {
    Default,
    IncludeHidden, // Include hidden files (those starting with .)
    All,           // Include everything
}

struct Config {
    output_format: OutputFormat,
    sort_by: SortBy,
    classify_files: ClassifyFiles,
    dereference_symbolic_link: DereferenceSymbolicLink,
    file_time_option: FileTimeOption,
    file_inclusion: FileInclusion,
    inode: bool,
    kibibytes: bool,
    hide_control_chars: bool,
    reverse_sorting: bool,
    display_size: bool,
    recursive: bool,
    terminal_width: usize,
}

impl Config {
    fn new() -> (Self, Vec<PathBuf>) {
        let m = Args::command().get_matches();

        // Enables long format (-g, -n, -l, -o)
        const LONG_FORMAT_WITHOUT_OWNER: &str = "long_format_without_owner";
        const LONG_FORMAT_NUMERIC_UID_GID: &str = "long_format_numeric_uid_gid";
        const LONG_FORMAT: &str = "long_format";
        const LONG_FORMAT_WITHOUT_GROUP: &str = "long_format_without_group";

        // Disables long format (-C, -m, -x)
        const MULTI_COLUMN: &str = "multi_column";
        const STREAM_OUTPUT_FORMAT: &str = "stream_output_format";
        const MUTI_COLUMN_ACROSS: &str = "muti_column_across";

        // Does not enable/disable long format directly but can enable it by
        // overriding -C, -m, or -x
        const ONE_ENTRY_PER_LINE: &str = "one_entry_per_line";

        let mut long_format_args: Vec<_> = [
            LONG_FORMAT_WITHOUT_OWNER,
            LONG_FORMAT_NUMERIC_UID_GID,
            LONG_FORMAT,
            LONG_FORMAT_WITHOUT_GROUP,
            MULTI_COLUMN,
            STREAM_OUTPUT_FORMAT,
            MUTI_COLUMN_ACROSS,
            ONE_ENTRY_PER_LINE,
        ]
        .into_iter()
        .filter_map(|s| {
            if m.get_flag(s) {
                m.index_of(s).map(|i| (i, s))
            } else {
                None
            }
        })
        .collect();

        // Sort in order of appearance in the arguments
        long_format_args.sort_by_key(|p| p.0);

        let mut long_format_state = Vec::new();
        let mut long_format_options = LongFormatOptions {
            numeric_uid_gid: false,
            without_owner: false,
            without_group: false,
        };

        // Manually resolve the format by iterating over the args
        for (_, s) in long_format_args.iter().copied() {
            match s {
                LONG_FORMAT_WITHOUT_OWNER => {
                    long_format_state.push(true);
                    long_format_options.without_owner = true;
                }
                LONG_FORMAT_NUMERIC_UID_GID => {
                    long_format_state.push(true);
                    long_format_options.numeric_uid_gid = true;
                }
                LONG_FORMAT => {
                    long_format_state.push(true);
                }
                LONG_FORMAT_WITHOUT_GROUP => {
                    long_format_state.push(true);
                    long_format_options.without_group = true;
                }
                MULTI_COLUMN | STREAM_OUTPUT_FORMAT | MUTI_COLUMN_ACROSS => {
                    long_format_state.push(false);
                }
                ONE_ENTRY_PER_LINE => loop {
                    // Remove any `false` that was added by -C, -m or -x until
                    // a `true` is reached or the vec is empty
                    match long_format_state.last().copied() {
                        Some(enabled) => {
                            if enabled {
                                break;
                            } else {
                                long_format_state.pop();
                            }
                        }
                        None => break,
                    }
                },
                _ => unreachable!(),
            }
        }

        let long_format_enabled = long_format_state.last().copied().unwrap_or(false);

        let mut args = match Args::from_arg_matches(&m) {
            Ok(args) => args,
            Err(e) => e.exit(),
        };

        let output_format = match (
            args.multi_column,
            args.stream_output_format,
            args.muti_column_across,
            args.one_entry_per_line,
        ) {
            (false, false, false, false) => {
                if long_format_enabled {
                    OutputFormat::LongFormat(long_format_options)
                } else {
                    // According to the specification:
                    //
                    // The default format shall be to list one entry per line to
                    // standard output; ...If the output is to a terminal, the
                    // format is implementation-defined.
                    //
                    // coreutils uses -C by default.
                    OutputFormat::OneEntryPerLine
                }
            }
            (true, false, false, false) => OutputFormat::MultiColumn,
            (false, true, false, false) => OutputFormat::StreamOutputFormat,
            (false, false, true, false) => OutputFormat::MultiColumnAcross,
            (false, false, false, true) => OutputFormat::OneEntryPerLine,
            _ => unreachable!(), // -C, -m, -x, and -1 are mutually exclusive
        };

        let sort_by = match (
            args.sort_by_file_size,
            args.sort_by_directory_order,
            args.sort_by_last_modified_time,
        ) {
            (false, false, false) => SortBy::Lexicographical,
            (true, false, false) => SortBy::FileSize,
            (false, true, false) => SortBy::DirectoryOrder,
            (false, false, true) => SortBy::Time,
            _ => unreachable!(), // -S, -f and -t are mutually exclusive
        };

        let classify_files = match (args.classify, args.write_slash_if_directory) {
            (false, false) => ClassifyFiles::None,
            (true, false) => ClassifyFiles::Complete,
            (false, true) => ClassifyFiles::DirectoryOrNotDirectory,
            _ => unreachable!(), // -F and -p are mutually exclusive
        };

        let dereference_symbolic_link = match (args.dereference_command_line, args.dereference) {
            (false, false) => DereferenceSymbolicLink::None,
            (true, false) => DereferenceSymbolicLink::CommandLine,
            (false, true) => DereferenceSymbolicLink::All,
            _ => unreachable!(), // -H and -L are mutually exclusive
        };

        let file_time_option = match (args.use_last_status_change_time, args.use_last_access_time) {
            (false, false) => FileTimeOption::LastModificationTime,
            (true, false) => FileTimeOption::LastStatusChangeTime,
            (false, true) => FileTimeOption::LastAcessTime,
            _ => unreachable!(), // -c and -u are mutually exclusive
        };

        let mut file_inclusion = match (args.almost_all, args.all) {
            (false, false) => FileInclusion::Default,
            (true, false) => FileInclusion::IncludeHidden,
            (false, true) => FileInclusion::All,
            _ => unreachable!(), // -A and -a are mutually exclusive
        };

        // When enabling -f,
        //  -r, -S, and -t options shall be ignored
        //  -A, -g, -l, -n, -o, and -s options may be ignored
        if args.sort_by_directory_order {
            // -r is ignored
            args.reverse_sorting = false;

            // -A is also ignored
            match file_inclusion {
                FileInclusion::Default => file_inclusion = FileInclusion::All,
                _ => (),
            }
        }

        let mut file = args.file;
        if file.is_empty() {
            file.push(PathBuf::from("."));
        }

        let config = Self {
            output_format,
            sort_by,
            classify_files,
            dereference_symbolic_link,
            file_time_option,
            file_inclusion,

            inode: args.inode,
            kibibytes: args.kibibytes,
            hide_control_chars: args.hide_control_chars,
            reverse_sorting: args.reverse_sorting,
            display_size: args.display_size,
            recursive: args.recursive,

            terminal_width: get_terminal_width(),
        };

        (config, file)
    }
}

fn get_terminal_width() -> usize {
    // COLUMNS is usually automatically set and it even changes when the
    // terminal window is resized.
    if let Ok(s) = std::env::var("COLUMNS") {
        if let Ok(num_columns) = s.parse() {
            return num_columns;
        }
    }

    // Fallback to manually querying via `ioctl`.
    unsafe {
        let mut winsize: MaybeUninit<libc::winsize> = MaybeUninit::zeroed();
        let ret = libc::ioctl(
            libc::STDOUT_FILENO,
            P_WINSIZE_REQUEST_CODE,
            winsize.as_mut_ptr(),
        );

        // We're only interested in stdout here unlike `term_size::dimensions`
        // so we won't query further if the first `ioctl` call fails.
        if ret == 0 {
            let winsize = winsize.assume_init();
            return winsize.ws_col as usize;
        }
    }

    // Historical default terminal width is 80
    80
}

/// Calculate how many columns will fit in `terminal_width` given
/// `column_width`.
fn calc_num_columns(column_width: usize, terminal_width: usize) -> usize {
    // Derived by solving for column_width in this inequality:
    //
    // (num_columns - 1)*(column_width + COLUMN_SPACING) + column_width <= terminal_width
    (terminal_width + COLUMN_SPACING) / (column_width + COLUMN_SPACING)
}

/// Calculate the padding and width for each column that will give the minimum
/// amount of rows.
fn calc_optimal_padding(
    entries: &mut [Entry],
    terminal_width: usize,
    across: bool,
) -> Vec<MultiColumnPadding> {
    let mut min_len = usize::MAX;
    let mut max_len = usize::MIN;

    let mut padding = MultiColumnPadding::default();
    for entry in entries.iter() {
        let p = entry.get_multi_column_padding();
        padding.update_maximum(p);

        min_len = usize::min(min_len, p.total_width);
        max_len = usize::max(max_len, p.total_width);
    }

    // Single column because at least one entry is too big to fit in
    // multi-column
    if max_len > terminal_width || entries.is_empty() {
        padding.file_name_width = 0; // Don't pad when it's just a single column
        return vec![padding];
    }

    for entry in entries.iter_mut() {
        entry.recalculate_widths(&padding);
    }

    let mut num_columns_upperbound = calc_num_columns(min_len, terminal_width);
    let num_columns_lowerbound = calc_num_columns(max_len, terminal_width);

    // If too many
    if entries.len() >= SKIP_OPTIMAL_COLUMN_WIDTH_THRESHOLD {
        // `num_columns_lowerbound` is guaranteed to succeed
        num_columns_upperbound = num_columns_lowerbound;
    }

    // This is O(n^2) hence the `entries.len()` check above to avoid being
    // excessively slow.
    for num_cols in (num_columns_lowerbound..=num_columns_upperbound).rev() {
        let mut widths = Vec::with_capacity(num_cols);
        if across {
            for i in 0..usize::min(num_cols, entries.len()) {
                // Get the maximum width for each column
                if let Some(max_width) = entries[i..]
                    .iter()
                    .map(|e| e.get_multi_column_padding().total_width)
                    .step_by(num_cols)
                    .max()
                {
                    widths.push(max_width);
                }
            }
        } else {
            let num_rows = entries.len().div_ceil(num_cols);

            for col in entries.chunks(num_rows) {
                // Max width for each column.
                // `unwrap` here shouldn't panic since `chunks` should always be
                // returning a non-empty slice.
                let max_width = col
                    .iter()
                    .map(|e| e.get_multi_column_padding().total_width)
                    .max()
                    .unwrap();

                widths.push(max_width);
            }
        }

        assert!(!widths.is_empty());

        let mut total_width = widths.iter().sum::<usize>();

        // Every column except the last has `COLUMN_SPACING` spaces.
        total_width += COLUMN_SPACING * (widths.len() - 1);

        if total_width <= terminal_width {
            let mut result = Vec::with_capacity(num_cols);
            if across {
                for i in 0..usize::min(num_cols, entries.len()) {
                    let mut padding = MultiColumnPadding::default();
                    for entry in entries[i..].iter().step_by(num_cols) {
                        padding.update_maximum(entry.get_multi_column_padding());
                    }
                    result.push(padding);
                }
            } else {
                let num_rows = entries.len().div_ceil(num_cols);

                for col in entries.chunks(num_rows) {
                    let mut padding = MultiColumnPadding::default();
                    for entry in col {
                        padding.update_maximum(entry.get_multi_column_padding());
                    }
                    result.push(padding);
                }
            }
            return result;
        }
    }

    unreachable!()
}

fn display_entries(entries: &mut [Entry], config: &Config, dir_path: Option<&str>) {
    match &config.sort_by {
        SortBy::DirectoryOrder => (), // Already sorted by directory order
        other_sorting => {
            entries.sort_by(|a, b| {
                let sort_fn = match other_sorting {
                    SortBy::Lexicographical => Entry::sorting_cmp_lexicographic,
                    SortBy::FileSize => Entry::sorting_cmp_size,
                    SortBy::Time => Entry::sorting_cmp_time,
                    SortBy::DirectoryOrder => unreachable!(), // Already handled
                };
                if config.reverse_sorting {
                    sort_fn(a, b).reverse()
                } else {
                    sort_fn(a, b)
                }
            });
        }
    }

    let mut display_total_size = config.display_size;
    if let OutputFormat::LongFormat(_) = &config.output_format {
        display_total_size = true;
    }

    // `dir_path.is_some()` to only display the total on directories.
    if display_total_size && dir_path.is_some() {
        let mut total_block_size = 0;
        for entry in entries.iter() {
            total_block_size += BLOCK_SIZE * entry.blocks();
        }

        // The specification seems contradictory here. On the -s flag
        // it says it is implementation-defined. But on the STDOUT
        // section, it mandates it to be 512 when -k is not specified
        // and 1024 when it is.
        // coreutils seems to always have it as 1024 with or without -k.
        if config.kibibytes {
            total_block_size /= BLOCK_SIZE_KIBIBYTES;
        } else {
            total_block_size /= BLOCK_SIZE;
        }
        println!("{} {}", gettext("total"), total_block_size);
    }

    match &config.output_format {
        OutputFormat::LongFormat(_) => {
            let mut padding = LongFormatPadding::default();

            // Calculate required padding
            for entry in entries.iter() {
                padding.update_maximum(&entry.get_long_format_padding());
            }

            // Readjust file size / device ID column
            padding.update_file_info_padding();

            for entry in entries.iter() {
                entry.println_long_format(&padding);
            }
        }
        OutputFormat::MultiColumn => {
            let paddings = calc_optimal_padding(entries, config.terminal_width, false);
            let last_col_idx = paddings.len() - 1;

            let num_columns = paddings.len();
            let num_rows = entries.len().div_ceil(num_columns);

            // Choose a row.
            // | 0 |   |   |   |
            // | * |   |   |   |
            // | 2 |   |   |   |
            for row_idx in 0..num_rows {
                // Create an iterator of columns.
                // |   | 3 |   |   |
                // |   | 4 |   |   |
                // |   | 5 |   |   |
                for (col_idx, (col, padding)) in
                    entries.chunks(num_rows).zip(paddings.iter()).enumerate()
                {
                    // For each column, select one row.
                    // |   | 3 |   |   |
                    // |   | * |   |   |
                    // |   | 5 |   |   |
                    if let Some(entry) = col.get(row_idx) {
                        if col_idx == last_col_idx {
                            entry.print_multi_column(padding);
                        } else {
                            entry.print_multi_column(padding);
                            print!("{:COLUMN_SPACING$}", "");
                        }
                    }
                }
                println!();
            }
        }
        OutputFormat::MultiColumnAcross => {
            let paddings = calc_optimal_padding(entries, config.terminal_width, true);
            let last_col_idx = paddings.len() - 1;

            let num_columns = paddings.len();

            // The `zip` of `entries` and `paddings` in this for loop
            // iterates like the following:
            //
            // `entries`
            // | 0 | 1 | 2 | 3 |
            // | 4 | 5 | 6 |   |
            // |   |   |   |   |
            //
            // `paddings`
            // | 0 | 1 | 2 | 3 |
            // | 0 | 1 | 2 |   |
            // |   |   |   |   |
            for (entry, (col_idx, padding)) in
                entries.iter().zip(paddings.iter().enumerate().cycle())
            {
                if col_idx == last_col_idx {
                    entry.print_multi_column(padding);
                    println!();
                } else {
                    entry.print_multi_column(padding);
                    print!("{:COLUMN_SPACING$}", "");
                }
            }

            // If the last entry does not end up on the bottom right of
            // the grid
            if entries.len() % num_columns != 0 {
                println!();
            }
        }
        OutputFormat::StreamOutputFormat => {
            let stream_outputs: Vec<_> = entries
                .iter()
                .map(|entry| entry.build_stream_mode_string())
                .collect();
            let char_counts: Vec<_> = stream_outputs.iter().map(|s| s.chars().count()).collect();
            let mut start = 0;

            'outer: loop {
                let mut column_width = 0;
                for (slice_idx, count) in char_counts[start..].iter().enumerate() {
                    let i = start + slice_idx;

                    // +2 for the ", " separator
                    let next_width = column_width + count + 2;

                    if next_width < config.terminal_width {
                        column_width = next_width;
                    } else {
                        let width_without_space = column_width + count + 1;

                        // `start..=i` fits in `terminal_width`
                        if width_without_space < config.terminal_width {
                            assert_ne!(start, i);

                            for output in &stream_outputs[start..i] {
                                print!("{}, ", output);
                            }
                            println!("{},", &stream_outputs[i]);

                            start = i + 1;
                        } else {
                            // Long file name that exceeds
                            // `terminal_width` by itself
                            if start == i {
                                println!("{}", &stream_outputs[i]);
                                start = i + 1;

                            // `start..i` fits in `terminal_width`
                            } else {
                                for output in &stream_outputs[start..(i - 1)] {
                                    print!("{}, ", output);
                                }
                                println!("{},", &stream_outputs[i - 1]);

                                start = i;
                            }
                        }

                        continue 'outer;
                    }
                }
                for output in &stream_outputs[start..(stream_outputs.len() - 1)] {
                    print!("{}, ", output);
                }
                // No comma on the very last file name
                println!("{}", &stream_outputs[stream_outputs.len() - 1]);

                break;
            }
        }
        OutputFormat::OneEntryPerLine => {
            // Set the `terminal_width` to one to force to single column
            let paddings = calc_optimal_padding(entries, 1, false);

            let padding = paddings.first().unwrap();

            for entry in entries.iter() {
                entry.print_multi_column(padding);
                println!();
            }
        }
    }
}

fn ls(paths: Vec<PathBuf>, config: &Config) -> io::Result<u8> {
    let mut exit_code = 0;

    let mut directories = Vec::new();
    let mut files = Vec::new();

    // Categorize into directories/files
    for path in paths {
        if path.is_dir() {
            directories.push(path);
        } else {
            files.push(path);
        }
    }

    let num_directory_args = directories.len();
    let num_file_args = files.len();
    let num_args = num_file_args + num_directory_args;

    // Files get processed first
    let mut file_entries = Vec::new();
    for path in files {
        let path_cstr = CString::new(path.as_os_str().as_bytes()).unwrap();
        let metadata = match ftw::Metadata::new(libc::AT_FDCWD, &path_cstr, false) {
            Ok(m) => m,
            Err(e) => {
                eprintln!("ls: {e}");
                exit_code = exit_code.max(1);
                continue;
            }
        };

        let is_commandline_arg = true;
        let dereference_symlink = match config.dereference_symbolic_link {
            DereferenceSymbolicLink::CommandLine => is_commandline_arg,
            DereferenceSymbolicLink::All => true,
            DereferenceSymbolicLink::None => false,
        };

        // If -H or -L are enabled, the metadata to be reported is from the file
        // that the symbolic link points to.
        let metadata = if metadata.is_symlink() && dereference_symlink {
            match ftw::Metadata::new(libc::AT_FDCWD, &path_cstr, true) {
                Ok(m) => m,
                Err(e) => {
                    eprintln!("ls: {e}");
                    exit_code = exit_code.max(1);
                    continue;
                }
            }
        } else {
            metadata
        };

        // Target of the symlink
        let target_path = {
            let mut target_path = None;
            if metadata.is_symlink() && !dereference_symlink {
                if let OutputFormat::LongFormat(_) = &config.output_format {
                    let mut buf = vec![0u8; libc::PATH_MAX as usize];

                    let path_cstr = CString::new(path.as_os_str().as_bytes()).unwrap();
                    let ret = unsafe {
                        libc::readlinkat(
                            libc::AT_FDCWD,
                            path_cstr.as_ptr(),
                            buf.as_mut_ptr().cast(),
                            buf.len(),
                        )
                    };
                    if ret < 0 {
                        return Err(io::Error::last_os_error());
                    }
                    let num_bytes = ret as usize;
                    buf.shrink_to(num_bytes);

                    let target_path_cstr = CString::from_vec_with_nul(buf).unwrap();

                    target_path = Some(ls_from_utf8_lossy(target_path_cstr.to_bytes()));
                }
            }

            target_path
        };

        let entry = match Entry::new(
            target_path,
            path.as_os_str().to_os_string(),
            &metadata,
            config,
        ) {
            Ok(x) => x,
            Err(e) => {
                eprintln!("ls: {e}");
                exit_code = exit_code.max(1);
                continue;
            }
        };
        file_entries.push(entry);
    }
    if !file_entries.is_empty() {
        display_entries(&mut file_entries, config, None);
    }

    let mut is_first_dir_arg = true;
    for path in directories.into_iter() {
        exit_code = exit_code.max(process_single_dir(
            path,
            config,
            num_args,
            num_file_args,
            &mut is_first_dir_arg,
        )?);
    }
    Ok(exit_code)
}

fn process_single_dir(
    path: PathBuf,
    config: &Config,
    num_args: usize,
    num_file_args: usize,
    is_first_dir_arg: &mut bool,
) -> io::Result<u8> {
    // Shared between closures so need to be a type that can be internally
    // mutated
    let exit_code = AtomicU8::new(0);

    // Stores visited paths to prevent infinite loops due to symbolic links
    // Map of canonical path -> path
    let mut visited: HashMap<(u64, u64), PathBuf> = HashMap::new();

    let mut entries: Vec<Entry> = Vec::new();
    let mut errors: Vec<io::Error> = Vec::new();

    let mut current_dir: Option<PathBuf> = None;

    // Always true. According to the reference:
    // "For each operand that names a file of a type other than directory
    // or symbolic link to a directory, ls shall write..."
    let follow_symlinks_on_args = true;

    let follow_symlinks = match config.dereference_symbolic_link {
        DereferenceSymbolicLink::CommandLine => false,
        DereferenceSymbolicLink::All => true,
        DereferenceSymbolicLink::None => false,
    };

    fn print_header(
        config: &Config,
        num_args: usize,
        num_file_args: usize,
        is_first_dir_arg: &mut bool,
        dir: &Path,
    ) {
        let dir_path = ls_from_utf8_lossy(dir.as_os_str().as_bytes());

        // If more than one directory, or a combination of non-directory
        // files and directories are written, either as a result of
        // specifying multiple operands, or the -R option
        let display_directory_header = num_args > 1 || config.recursive;

        if display_directory_header {
            if *is_first_dir_arg && num_file_args == 0 {
                // Trimming the newline on the first directory isn't
                // strictly required by the specification
                println!("{}:", dir_path);
                *is_first_dir_arg = false;
            } else {
                println!("\n{}:", dir_path);
            }
        }
    }

    fn print_contents(
        config: &Config,
        dir: &Path,
        entries: &mut Vec<Entry>,
        errors: &mut Vec<io::Error>,
        exit_code: &AtomicU8,
    ) {
        let dir_path = ls_from_utf8_lossy(dir.as_os_str().as_bytes());

        for e in errors.drain(..) {
            eprintln!("ls: {e}");
            exit_code.fetch_max(1, Ordering::SeqCst);
        }

        if !entries.is_empty() {
            display_entries(entries, config, Some(&dir_path));

            // Already displayed so clear the entries
            entries.clear();
        }
    }

    let mut terminate = false;

    let _ = ftw::traverse_directory(
        path,
        |dir_entry| {
            if terminate {
                return Ok(false);
            }

            let metadata = dir_entry.metadata().unwrap();
            let is_dot_or_double_dot = dir_entry.is_dot_or_double_dot();

            // Get the metadata of the file, equivalent to `std::fs::symlink_metadata`
            let marker = {
                let metadata =
                    match ftw::Metadata::new(dir_entry.dir_fd(), dir_entry.file_name(), false) {
                        Ok(md) => md,
                        Err(e) => {
                            let path_str = ls_from_utf8_lossy(
                                dir_entry.path().as_inner().as_os_str().as_bytes(),
                            );
                            let err_str = gettext!("cannot access '{}': {}", path_str, e);
                            errors.push(io::Error::other(err_str));
                            return Ok(false);
                        }
                    };
                (metadata.dev(), metadata.ino())
            };

            if current_dir.is_none() {
                // Init `dir`. `dir_entry.path()` should still be `path` here.
                current_dir = Some(dir_entry.path().as_inner().to_path_buf());
                print_header(
                    config,
                    num_args,
                    num_file_args,
                    is_first_dir_arg,
                    current_dir.as_ref().unwrap(),
                );

                visited.insert(marker, current_dir.as_ref().unwrap().clone());
                return Ok(true);
            }

            // Helper closure to easily catch the `io::Error` for printing
            let process_dir_entry = |entries: &mut Vec<Entry>| -> io::Result<bool> {
                let path = dir_entry.path();
                let path_str = ls_from_utf8_lossy(path.as_os_str().as_bytes());

                {
                    let include_dot_and_double_dot = match config.file_inclusion {
                        FileInclusion::All => true,
                        _ => false,
                    };

                    if is_dot_or_double_dot && !include_dot_and_double_dot {
                        // Skip
                        return Ok(true);
                    }
                }

                let file_name_raw = OsStr::from_bytes(dir_entry.file_name().to_bytes()).to_owned();

                // Symlink target
                let target_path = {
                    let is_commandline_arg = false;
                    let dereference_symlink = match config.dereference_symbolic_link {
                        DereferenceSymbolicLink::CommandLine => is_commandline_arg,
                        DereferenceSymbolicLink::All => true,
                        DereferenceSymbolicLink::None => false,
                    };

                    let mut target_path = None;
                    if metadata.is_symlink() && !dereference_symlink {
                        if let OutputFormat::LongFormat(_) = &config.output_format {
                            target_path = Some(ls_from_utf8_lossy(
                                dir_entry.read_link().unwrap().to_bytes(),
                            ));
                        }
                    }

                    target_path
                };

                let entry = Entry::new(target_path, file_name_raw, &metadata, config)
                    .map_err(|e| io::Error::other(format!("'{path_str}': {e}")))?;

                let mut include_entry = false;

                // Check if file starts with `.`
                let is_hidden_file = {
                    let byte = dir_entry.file_name().to_bytes_with_nul()[0];
                    byte == b'.'
                };

                if is_hidden_file {
                    match &config.file_inclusion {
                        FileInclusion::IncludeHidden | FileInclusion::All => {
                            include_entry = true;
                        }
                        FileInclusion::Default => (), // Do nothing
                    }
                } else {
                    include_entry = true;
                }

                if include_entry {
                    entries.push(entry);

                    if config.recursive {
                        if metadata.is_dir() {
                            return Ok(true);
                        }
                    }
                }

                Ok(false)
            };

            let canonical_dir_path = dir_entry.path();
            let current_dir_ref = current_dir.as_mut().unwrap();

            // Get the parent of the path like in `std::path::Path::parent` but mapping
            // `parent/.` and `parent/..` to `parent`
            let dir_parent = {
                let mut comps = canonical_dir_path.as_inner().components();

                let is_dot = dir_entry.file_name().to_bytes_with_nul() == &[b'.', 0];

                if !is_dot {
                    comps.next_back();
                }

                comps.as_path()
            };

            if let Some(file_name) = visited.get(&marker) {
                // Exclude . and .. from loop detection logic
                if !is_dot_or_double_dot {
                    // Process and print previous entries before showing the infinite loop error
                    if let Err(e) = process_dir_entry(&mut entries) {
                        errors.push(e);
                    }
                    print_contents(
                        config,
                        &current_dir_ref,
                        &mut entries,
                        &mut errors,
                        &exit_code,
                    );

                    eprintln!(
                        "ls: {}: {}",
                        ls_from_utf8_lossy(file_name.as_os_str().as_bytes()),
                        gettext("not listing already-listed directory")
                    );

                    // This is the only error that has exit code 2 for now.
                    exit_code.fetch_max(2, Ordering::SeqCst);
                    terminate = true;
                    return Ok(false);
                }
            } else {
                visited.insert(marker, current_dir_ref.clone());
            }

            // If moving to a new subdirectory
            if dir_parent != current_dir_ref.as_path() {
                print_contents(
                    config,
                    &current_dir_ref,
                    &mut entries,
                    &mut errors,
                    &exit_code,
                );

                current_dir = Some(dir_parent.to_path_buf());
                print_header(
                    config,
                    num_args,
                    num_file_args,
                    is_first_dir_arg,
                    current_dir.as_ref().unwrap(),
                );
            }

            match process_dir_entry(&mut entries) {
                Ok(b) => Ok(b),
                Err(e) => {
                    errors.push(e);
                    Ok(false)
                }
            }
        },
        |_| Ok(()),
        |entry, error| {
            let path_str = ls_from_utf8_lossy(entry.path().as_inner().as_os_str().as_bytes());
            eprintln!(
                "ls: {}",
                gettext!("cannot access '{}': {}", path_str, error.inner())
            );
            exit_code.fetch_max(1, Ordering::SeqCst);
        },
        ftw::TraverseDirectoryOpts {
            follow_symlinks_on_args,
            follow_symlinks,
            include_dot_and_double_dot: true,
            list_contents_first: true,
        },
    );

    // If there are remaining unprinted entries
    if !entries.is_empty() {
        if let Some(dir) = &current_dir {
            print_contents(config, dir, &mut entries, &mut errors, &exit_code);
        }
    }

    Ok(exit_code.load(Ordering::SeqCst))
}

fn main() -> ExitCode {
    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME).unwrap();
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8").unwrap();

    let (config, paths) = Config::new();

    match ls(paths, &config) {
        Ok(exit_code) => {
            if exit_code == 0 {
                ExitCode::SUCCESS
            } else {
                ExitCode::from(exit_code)
            }
        }
        Err(e) => {
            eprintln!("ls: {e}");
            ExitCode::FAILURE
        }
    }
}
