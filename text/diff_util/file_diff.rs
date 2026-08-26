//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use super::{
    common::{FormatOptions, OutputFormat},
    constants::COULD_NOT_UNWRAP_FILENAME,
    diff_exit_status::DiffExitStatus,
    file_data::{FileData, LineReader},
    functions::{
        check_existance, is_binary, system_time_to_context_format, system_time_to_unified_format,
        write_line,
    },
    hunks::{Hunk, Hunks},
};

use crate::diff_util::constants::NO_NEW_LINE_AT_END_OF_FILE;

use std::{
    cmp::Reverse,
    collections::HashMap,
    fs::File,
    io::{self, BufWriter, Read, Write},
    path::PathBuf,
    time::SystemTime,
};

/// One operand, read into memory.
///
/// Standard input is an operand like any other here, which is why it needs no
/// spill file: the text path always held both files whole anyway. Carrying the
/// display name separately from the path is what lets stdin appear as `-` in a
/// `-u`/`-c` header, as POSIX requires, instead of whatever path it was read
/// from.
pub struct Source {
    name: String,
    content: Vec<u8>,
    modified: SystemTime,
}

impl Source {
    /// Read a file operand, taking its bytes and its modification time from a
    /// single `open`. Each operand used to be opened three times -- to sniff
    /// for binary content, for the contents, and for the timestamp -- which
    /// could observe three different files.
    pub fn from_path(path: PathBuf) -> io::Result<Self> {
        let mut file = File::open(&path)?;
        let metadata = file.metadata()?;
        let mut content = Vec::with_capacity(metadata.len() as usize + 1);
        file.read_to_end(&mut content)?;
        let modified = metadata.modified()?;
        let name = path
            .to_str()
            .unwrap_or(COULD_NOT_UNWRAP_FILENAME)
            .to_string();
        Ok(Self {
            name,
            content,
            modified,
        })
    }

    /// Standard input, buffered in memory.
    pub fn from_stdin() -> io::Result<Self> {
        let mut content = Vec::new();
        io::stdin().read_to_end(&mut content)?;
        Ok(Self {
            name: String::from("-"),
            content,
            modified: SystemTime::now(),
        })
    }
}

/// Format a context-format (`-c`/`-C`) line range starting at 1-indexed
/// `start` and spanning `len` lines. A single line prints as one number, an
/// empty range prints the preceding line number, and longer ranges print
/// `first,last`.
fn context_range(start: usize, len: usize) -> String {
    match len {
        0 => format!("{}", start.saturating_sub(1)),
        1 => format!("{}", start),
        _ => format!("{},{}", start, start + len - 1),
    }
}

/// Format a unified-format (`-u`/`-U`) line range starting at 1-indexed
/// `start` and spanning `len` lines. A single line prints as one number, an
/// empty range prints `before,0`, and longer ranges print `start,len`.
fn unified_range(start: usize, len: usize) -> String {
    match len {
        0 => format!("{},0", start.saturating_sub(1)),
        1 => format!("{}", start),
        _ => format!("{},{}", start, len),
    }
}

/// Entry for histogram-based LCS algorithm.
/// Stores occurrence counts and last positions for lines in both files.
#[derive(Clone, Copy, Default)]
struct HistEntry {
    cnt1: i32,
    pos1: i32,
    cnt2: i32,
    pos2: i32,
}

pub struct FileDiff<'a> {
    file1: &'a mut FileData<'a>,
    file2: &'a mut FileData<'a>,
    hunks: Hunks,
    format_options: &'a FormatOptions,
    are_different: bool,
}

impl<'a> FileDiff<'a> {
    fn new(
        file1: &'a mut FileData<'a>,
        file2: &'a mut FileData<'a>,
        format_options: &'a FormatOptions,
    ) -> Self {
        Self {
            file1,
            file2,
            hunks: Default::default(),
            format_options,
            are_different: false,
        }
    }

    pub fn file_diff(
        path1: PathBuf,
        path2: PathBuf,
        format_options: &FormatOptions,
        show_if_different: Option<String>,
    ) -> io::Result<DiffExitStatus> {
        Self::diff_sources(
            Source::from_path(path1)?,
            Source::from_path(path2)?,
            format_options,
            show_if_different,
        )
    }

    pub fn diff_sources(
        src1: Source,
        src2: Source,
        format_options: &FormatOptions,
        show_if_different: Option<String>,
    ) -> io::Result<DiffExitStatus> {
        let (content1, content2) = (src1.content, src2.content);
        let (modified1, modified2) = (src1.modified, src2.modified);

        if is_binary(&content1) || is_binary(&content2) {
            Self::binary_file_diff(&src1.name, &src2.name, &content1, &content2)
        } else {
            let linereader1 = LineReader::new(&content1);
            let ends_with_newline1 = linereader1.ends_with_newline();
            let lines1: Vec<&[u8]> = linereader1.collect();

            let linereader2 = LineReader::new(&content2);
            let ends_with_newline2 = linereader2.ends_with_newline();
            let lines2: Vec<&[u8]> = linereader2.collect();

            // Pass whitespace normalization flag to FileData
            // When -b is set, hashes are computed using normalized whitespace for comparison
            // but original lines are stored for output
            let normalize_ws = format_options.ignore_trailing_white_spaces;
            let mut file1 = FileData::new(
                src1.name,
                lines1,
                modified1,
                ends_with_newline1,
                normalize_ws,
            );
            let mut file2 = FileData::new(
                src2.name,
                lines2,
                modified2,
                ends_with_newline2,
                normalize_ws,
            );

            let mut diff = FileDiff::new(&mut file1, &mut file2, format_options);

            // histogram diff
            let num_lines1 = diff.file1.line_count();
            let num_lines2 = diff.file2.line_count();
            let mut lcs_indices: Vec<i32> = vec![-1; num_lines1];
            FileDiff::histogram_lcs(
                diff.file1,
                diff.file2,
                0,
                num_lines1,
                0,
                num_lines2,
                &mut lcs_indices,
            );

            diff.hunks
                .create_hunks_from_lcs(&lcs_indices, num_lines1, num_lines2);

            if diff.hunks.hunk_count() > 0 {
                diff.are_different = true;
            }

            if diff.are_different {
                if let Some(show_if_different) = show_if_different {
                    println!("{}", show_if_different);
                }
            }

            diff.print()
        }
    }

    pub fn file_dir_diff(
        path1: PathBuf,
        path2: PathBuf,
        format_options: &FormatOptions,
    ) -> io::Result<DiffExitStatus> {
        let path1_file_type = path1.metadata()?.file_type();

        if path1_file_type.is_file() {
            let path1_file = path1.clone();
            let path1_file = path1_file.file_name().expect(COULD_NOT_UNWRAP_FILENAME);
            let path2 = path2.join(path1_file);

            if !check_existance(&path2)? {
                return Ok(DiffExitStatus::Trouble);
            }

            FileDiff::file_diff(path1, path2, format_options, None)
        } else {
            let path2_file = path2.clone();
            let path2_file = path2_file.file_name().expect(COULD_NOT_UNWRAP_FILENAME);
            let path1 = path1.join(path2_file);

            if !check_existance(&path1)? {
                return Ok(DiffExitStatus::Trouble);
            }

            FileDiff::file_diff(path1, path2, format_options, None)
        }
    }

    fn binary_file_diff(
        name1: &str,
        name2: &str,
        content1: &[u8],
        content2: &[u8],
    ) -> io::Result<DiffExitStatus> {
        if content1 == content2 {
            return Ok(DiffExitStatus::NotDifferent);
        }

        println!("Binary files {} and {} differ", name1, name2);
        Ok(DiffExitStatus::Different)
    }

    fn print(&mut self) -> io::Result<DiffExitStatus> {
        self.order_hunks_by_output_format();

        // One writer for every format: line data is bytes and has to reach
        // stdout through write_all, and buffering it turns a write per line
        // into a write per block.
        let stdout = io::stdout();
        let mut out = BufWriter::new(stdout.lock());

        if let OutputFormat::Context(context) = self.format_options.output_format {
            // Identical files produce no output (not even the file headers).
            if self.are_different {
                self.print_context(&mut out, context)?;
            }
        } else if let OutputFormat::Unified(unified) = self.format_options.output_format {
            if self.are_different {
                self.print_unified(&mut out, unified)?;
            }
        } else {
            let hunks_count = self.hunks.hunks().len();

            for hunk_index in 0..hunks_count {
                let is_last = hunk_index == hunks_count - 1;
                let format = &self.format_options.output_format;
                let hunk = &mut self.hunks.hunks_mut()[hunk_index];
                match format {
                    OutputFormat::Default => {
                        hunk.print_default(&mut out, self.file1, self.file2)?
                    }
                    OutputFormat::EditScript => {
                        hunk.print_edit_script(&mut out, self.file1, self.file2, is_last)?
                    }
                    OutputFormat::ForwardEditScript => {
                        hunk.print_forward_edit_script(&mut out, self.file1, self.file2, is_last)?
                    }
                    OutputFormat::Context(_) | OutputFormat::Unified(_) => {
                        eprintln!("context and unified output are handled above");
                        return Ok(DiffExitStatus::Trouble);
                    }
                }
            }
        }

        out.flush()?;

        if self.are_different {
            // An edit script cannot faithfully represent a file whose final
            // line lacks a trailing newline (ed always terminates lines), so
            // GNU diff reports "trouble" (exit 2) in that case.
            let edit_script = matches!(
                self.format_options.output_format,
                OutputFormat::EditScript | OutputFormat::ForwardEditScript
            );
            if edit_script && (!self.file1.ends_with_newline() || !self.file2.ends_with_newline()) {
                return Ok(DiffExitStatus::Trouble);
            }
            Ok(DiffExitStatus::Different)
        } else {
            Ok(DiffExitStatus::NotDifferent)
        }
    }

    /// Longest common subsequence (LCS) algorithm by recursively building a histogram.
    /// Indices in lcs_indices will be the line number in file1 while values will
    /// be the corresponding line number in file2. If the value is -1, then the
    /// line is not part of the LCS.
    ///
    /// Optimizations:
    /// - Uses pre-computed line hashes for O(1) initial comparison
    /// - Uses hash-keyed HashMap with stack-allocated HistEntry structs
    /// - Pre-allocates HashMap capacity to reduce rehashing
    pub fn histogram_lcs(
        file1: &FileData,
        file2: &FileData,
        mut x0: usize,
        mut x1: usize,
        mut y0: usize,
        mut y1: usize,
        lcs_indices: &mut Vec<i32>,
    ) {
        // Collect common elements at the beginning using hash comparison first.
        // The hashes are computed over normalized lines under -b, so the
        // confirmation has to use the same notion of equality or this scan
        // rejects every pair -b considers equal.
        while (x0 < x1)
            && (y0 < y1)
            && file1.line_hash(x0) == file2.line_hash(y0)
            && file1.lines_equal(x0, file2, y0)
        {
            lcs_indices[x0] = y0 as i32;
            x0 += 1;
            y0 += 1;
        }

        if (x0 == x1) || (y0 == y1) {
            // we can return early
            return;
        }

        // Collect common elements at the end using hash comparison first
        while (x0 < x1)
            && (y0 < y1)
            && file1.line_hash(x1 - 1) == file2.line_hash(y1 - 1)
            && file1.lines_equal(x1 - 1, file2, y1 - 1)
        {
            lcs_indices[x1 - 1] = (y1 - 1) as i32;
            x1 -= 1;
            y1 -= 1;
        }

        if (x0 == x1) || (y0 == y1) {
            return;
        }

        // Build histogram using hash-keyed map with pre-allocated capacity
        let capacity = (x1 - x0) + (y1 - y0);
        let mut hist: HashMap<u64, HistEntry> = HashMap::with_capacity(capacity);

        for i in x0..x1 {
            let h = file1.line_hash(i);
            hist.entry(h)
                .and_modify(|e| {
                    e.cnt1 += 1;
                    e.pos1 = i as i32;
                })
                .or_insert(HistEntry {
                    cnt1: 1,
                    pos1: i as i32,
                    cnt2: 0,
                    pos2: -1,
                });
        }

        for i in y0..y1 {
            let h = file2.line_hash(i);
            hist.entry(h)
                .and_modify(|e| {
                    e.cnt2 += 1;
                    e.pos2 = i as i32;
                })
                .or_insert(HistEntry {
                    cnt1: 0,
                    pos1: -1,
                    cnt2: 1,
                    pos2: i as i32,
                });
        }

        // Find lowest-occurrence item that appears in both files.
        //
        // Ties are the common case rather than the exception — every line that
        // is unique to each file scores 2 — and `HashMap` iterates in an order
        // seeded per process, so breaking ties by iteration order made the
        // whole diff differ from run to run. Tie-break on the first file's
        // position instead, which is stable. (The line hashes themselves were
        // never the problem: `DefaultHasher` is zero-seeded.)
        let pivot = hist
            .iter()
            .filter(|(_, v)| v.cnt1 > 0 && v.cnt2 > 0)
            .min_by_key(|(_, v)| (v.cnt1 + v.cnt2, v.pos1));

        if let Some((hash, entry)) = pivot {
            let x1_new = entry.pos1 as usize;
            let y1_new = entry.pos2 as usize;

            // Verify that lines actually match (handles hash collisions).
            // Uses the same equality as the histogram's hashes, so under -b a
            // pivot is not rejected merely for differing in whitespace.
            if file1.lines_equal(x1_new, file2, y1_new) {
                lcs_indices[x1_new] = y1_new as i32;
                FileDiff::histogram_lcs(file1, file2, x0, x1_new, y0, y1_new, lcs_indices);
                FileDiff::histogram_lcs(file1, file2, x1_new + 1, x1, y1_new + 1, y1, lcs_indices);
            } else {
                // Hash collision: need to find actual matching lines
                // Fall back to scanning for a true match with this hash
                let target_hash = *hash;
                for i in x0..x1 {
                    if file1.line_hash(i) == target_hash {
                        for j in y0..y1 {
                            if file2.line_hash(j) == target_hash && file1.lines_equal(i, file2, j) {
                                lcs_indices[i] = j as i32;
                                FileDiff::histogram_lcs(file1, file2, x0, i, y0, j, lcs_indices);
                                FileDiff::histogram_lcs(
                                    file1,
                                    file2,
                                    i + 1,
                                    x1,
                                    j + 1,
                                    y1,
                                    lcs_indices,
                                );
                                return;
                            }
                        }
                    }
                }
                // No actual match found despite hash match - rare case, skip pivot
            }
        }
    }

    fn order_hunks_by_output_format(&mut self) {
        match self.format_options.output_format {
            OutputFormat::Default => self.order_hunks_ascending(),
            OutputFormat::Context(_) => self.order_hunks_ascending(),
            OutputFormat::EditScript => self.order_hunks_descending(),
            OutputFormat::ForwardEditScript => self.order_hunks_ascending(),
            OutputFormat::Unified(_) => self.order_hunks_ascending(),
        }
    }

    fn order_hunks_ascending(&mut self) {
        self.hunks
            .hunks_mut()
            .sort_by_key(|hunk| (hunk.ln1_end(), hunk.ln2_end()));
    }

    fn order_hunks_descending(&mut self) {
        self.hunks
            .hunks_mut()
            .sort_by_key(|hunk| Reverse((hunk.ln1_end(), hunk.ln2_end())));
    }

    /// Write `start..end` of `file`, each line carrying `prefix`, emitting the
    /// no-newline marker directly after the file's final line when that line
    /// has no trailing newline.
    fn write_lines(
        out: &mut impl Write,
        file: &FileData,
        start: usize,
        end: usize,
        prefix: &[u8],
    ) -> io::Result<()> {
        let total = file.line_count();
        for i in start..end {
            write_line(out, prefix, file.line(i))?;
            if i + 1 == total && !file.ends_with_newline() {
                writeln!(out, "{}", NO_NEW_LINE_AT_END_OF_FILE)?;
            }
        }
        Ok(())
    }

    /// Write one pane of a context-format section: the section's whole line
    /// range for that file, with each hunk's own lines marked `!` when the
    /// hunk changes both files and `-` / `+` when it touches only one.
    fn write_context_pane(
        out: &mut impl Write,
        file: &FileData,
        group: &[Hunk],
        start: usize,
        end: usize,
        is_file1: bool,
    ) -> io::Result<()> {
        let mut pos = start;
        for hunk in group {
            let (h_start, h_end) = if is_file1 {
                (hunk.ln1_start(), hunk.ln1_end())
            } else {
                (hunk.ln2_start(), hunk.ln2_end())
            };
            let prefix = if hunk.ln1_end() > hunk.ln1_start() && hunk.ln2_end() > hunk.ln2_start() {
                b"! "
            } else if is_file1 {
                b"- "
            } else {
                b"+ "
            };
            Self::write_lines(out, file, pos, h_start, b"  ")?;
            Self::write_lines(out, file, h_start, h_end, prefix)?;
            pos = h_end;
        }
        Self::write_lines(out, file, pos, end, b"  ")
    }

    fn print_context(&mut self, out: &mut impl Write, context: usize) -> io::Result<()> {
        writeln!(
            out,
            "*** {}",
            Self::get_context_header(self.file1, self.format_options.label1())
        )?;
        writeln!(
            out,
            "--- {}",
            Self::get_context_header(self.file2, self.format_options.label2())
        )?;

        let num1 = self.file1.line_count();
        let num2 = self.file2.line_count();
        let hunks = self.hunks.hunks();

        for section in self.hunks.sections(context, num1, num2) {
            let group = &hunks[section.first..=section.last];

            writeln!(out, "***************")?;
            // Both range headers are always written; a pane's body is written
            // only when that file has lines of its own in this section, which
            // is what GNU diff does.
            writeln!(
                out,
                "*** {} ****",
                context_range(section.start1 + 1, section.end1 - section.start1)
            )?;
            if group.iter().any(|h| h.ln1_end() > h.ln1_start()) {
                Self::write_context_pane(
                    out,
                    self.file1,
                    group,
                    section.start1,
                    section.end1,
                    true,
                )?;
            }
            writeln!(
                out,
                "--- {} ----",
                context_range(section.start2 + 1, section.end2 - section.start2)
            )?;
            if group.iter().any(|h| h.ln2_end() > h.ln2_start()) {
                Self::write_context_pane(
                    out,
                    self.file2,
                    group,
                    section.start2,
                    section.end2,
                    false,
                )?;
            }
        }

        Ok(())
    }

    fn print_unified(&mut self, out: &mut impl Write, context: usize) -> io::Result<()> {
        writeln!(
            out,
            "--- {}",
            Self::get_unified_header(self.file1, self.format_options.label1())
        )?;
        writeln!(
            out,
            "+++ {}",
            Self::get_unified_header(self.file2, self.format_options.label2())
        )?;

        let num1 = self.file1.line_count();
        let num2 = self.file2.line_count();
        let hunks = self.hunks.hunks();

        for section in self.hunks.sections(context, num1, num2) {
            writeln!(
                out,
                "@@ -{} +{} @@",
                unified_range(section.start1 + 1, section.end1 - section.start1),
                unified_range(section.start2 + 1, section.end2 - section.start2)
            )?;
            // Unchanged lines are written from the first file; they are common
            // to both, so either would do.
            let mut pos1 = section.start1;
            for hunk in &hunks[section.first..=section.last] {
                Self::write_lines(out, self.file1, pos1, hunk.ln1_start(), b" ")?;
                Self::write_lines(out, self.file1, hunk.ln1_start(), hunk.ln1_end(), b"-")?;
                Self::write_lines(out, self.file2, hunk.ln2_start(), hunk.ln2_end(), b"+")?;
                pos1 = hunk.ln1_end();
            }
            Self::write_lines(out, self.file1, pos1, section.end1, b" ")?;
        }

        Ok(())
    }

    pub fn get_context_header(file: &FileData, label: &Option<String>) -> String {
        if let Some(label) = label {
            label.to_string()
        } else {
            format!(
                "{}\t{}",
                file.name(),
                system_time_to_context_format(file.modified())
            )
        }
    }

    pub fn get_unified_header(file: &FileData, label: &Option<String>) -> String {
        if let Some(label) = label {
            label.to_string()
        } else {
            format!(
                "{}\t{}",
                file.name(),
                system_time_to_unified_format(file.modified())
            )
        }
    }
}
