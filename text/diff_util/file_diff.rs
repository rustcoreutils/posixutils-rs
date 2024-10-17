use super::{
    common::{FormatOptions, OutputFormat},
    constants::COULD_NOT_UNWRAP_FILENAME,
    diff_exit_status::DiffExitStatus,
    file_data::{FileData, LineReader},
    functions::{check_existance, is_binary, system_time_to_rfc2822},
    hunks::Hunks,
};

use crate::diff_util::constants::NO_NEW_LINE_AT_END_OF_FILE;

use std::{
    cmp::Reverse,
    collections::HashMap,
    fmt::Write,
    fs::{read_to_string, File},
    io::{self, BufReader, Read},
    os::unix::fs::MetadataExt,
    path::PathBuf,
};

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
        if is_binary(&path1)? || is_binary(&path2)? {
            Self::binary_file_diff(&path1, &path2)
        } else {
            let content1 = read_to_string(&path1)?.into_bytes();
            let linereader1 = LineReader::new(&content1);
            let ends_with_newline1 = linereader1.ends_with_newline();
            let mut lines1 = Vec::new();
            for line in linereader1 {
                if !format_options.ignore_trailing_white_spaces {
                    lines1.push(line);
                } else {
                    lines1.push(line.trim_end());
                }
            }

            let content2 = read_to_string(&path2)?.into_bytes();
            let linereader2 = LineReader::new(&content2);
            let ends_with_newline2 = linereader2.ends_with_newline();
            let mut lines2 = Vec::new();
            for line in linereader2 {
                if !format_options.ignore_trailing_white_spaces {
                    lines2.push(line);
                } else {
                    lines2.push(line.trim_end());
                }
            }
            let mut file1 = FileData::get_file(path1, lines1, ends_with_newline1)?;
            let mut file2 = FileData::get_file(path2, lines2, ends_with_newline2)?;

            let mut diff = FileDiff::new(&mut file1, &mut file2, format_options);

            // histogram diff
            let mut lcs_indices: Vec<i32> = vec![-1; diff.file1.lines().len()];
            let num_lines1 = diff.file1.lines().len();
            let num_lines2 = diff.file2.lines().len();
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

    fn binary_file_diff(file1_path: &PathBuf, file2_path: &PathBuf) -> io::Result<DiffExitStatus> {
        let differ_report = format!(
            "Binary files {} and {} differ",
            file1_path.to_str().unwrap_or(COULD_NOT_UNWRAP_FILENAME),
            file2_path.to_str().unwrap_or(COULD_NOT_UNWRAP_FILENAME)
        );

        let file1 = File::open(file1_path)?;
        let file2 = File::open(file2_path)?;

        if file1.metadata()?.size() != file2.metadata()?.size() {
            println!("{}", differ_report);
            return Ok(DiffExitStatus::Different);
        }

        let file1 = BufReader::new(file1);
        let file2 = BufReader::new(file2);

        for bytes_pair in file1.bytes().zip(file2.bytes()) {
            let (b1, b2) = (bytes_pair.0?, bytes_pair.1?);
            if b1 != b2 {
                println!("{}", differ_report);
                return Ok(DiffExitStatus::Different);
            }
        }

        Ok(DiffExitStatus::NotDifferent)
    }

    fn print(&mut self) -> io::Result<DiffExitStatus> {
        self.order_hunks_by_output_format();

        if let OutputFormat::Context(context) = self.format_options.output_format {
            let _ = self.print_context(context);
        } else if let OutputFormat::Unified(unified) = self.format_options.output_format {
            let _ = self.print_unified(unified);
        } else {
            let hunks_count = self.hunks.hunks().len();

            for hunk_index in 0..hunks_count {
                let hunk = self.hunks.hunk_at_mut(hunk_index);
                match self.format_options.output_format {
                    OutputFormat::Debug => hunk.print_debug(self.file1, self.file2),
                    OutputFormat::Default => {
                        hunk.print_default(self.file1, self.file2, hunk_index == hunks_count - 1)
                    }
                    OutputFormat::EditScript => hunk.print_edit_script(
                        self.file1,
                        self.file2,
                        hunk_index == hunks_count - 1,
                    ),
                    OutputFormat::Context(_) => {
                        eprintln!("OutputFormat::Context should be handled in other place");
                        return Ok(DiffExitStatus::Trouble);
                    }
                    OutputFormat::ForwardEditScript => hunk.print_edit_script(
                        self.file1,
                        self.file2,
                        hunk_index == hunks_count - 1,
                    ),
                    OutputFormat::Unified(_) => {
                        eprintln!("OutputFormat::Unified should be handled in other place");
                        return Ok(DiffExitStatus::Trouble);
                    }
                }
            }
        }

        if self.are_different {
            Ok(DiffExitStatus::Different)
        } else {
            Ok(DiffExitStatus::NotDifferent)
        }
    }

    /// Longest common subsequence (LCS) algorithm by recursively building a histogram.
    /// Indices in lcs_indices will be the line number in file1 while values will
    /// be the corresponding line number in file2. If the value is -1, then the
    /// line is not part of the LCS.
    pub fn histogram_lcs(
        file1: &FileData,
        file2: &FileData,
        mut x0: usize,
        mut x1: usize,
        mut y0: usize,
        mut y1: usize,
        lcs_indices: &mut Vec<i32>,
    ) {
        // collect common elements at the beginning
        while (x0 < x1) && (y0 < y1) && (file1.line(x0) == file2.line(y0)) {
            lcs_indices[x0] = y0 as i32;
            x0 += 1;
            y0 += 1;
        }

        if (x0 == x1) || (y0 == y1) {
            // we can return early
            return;
        }

        // collect common elements at the end
        while (x0 < x1) && (y0 < y1) && (file1.line(x1 - 1) == file2.line(y1 - 1)) {
            lcs_indices[x1 - 1] = (y1 - 1) as i32;
            x1 -= 1;
            y1 -= 1;
        }

        // build histogram
        let mut hist: HashMap<&str, Vec<i32>> = HashMap::new();
        for i in x0..x1 {
            if let Some(rec) = hist.get_mut(file1.line(i)) {
                rec[0] += 1_i32;
                rec[1] = i as i32;
            } else {
                hist.insert(file1.line(i), vec![1, i as i32, 0, -1]);
            }
        }
        for i in y0..y1 {
            if let Some(rec) = hist.get_mut(file2.line(i)) {
                rec[2] += 1_i32;
                rec[3] = i as i32;
            } else {
                hist.insert(file2.line(i), vec![0, -1, 1, i as i32]);
            }
        }

        // find lowest-occurrence item that appears in both files
        let key = hist
            .iter()
            .filter(|(_k, v)| (v[0] > 0) && (v[2] > 0))
            .min_by(|a, b| {
                let c = a.1[0] + a.1[2];
                let d = b.1[0] + b.1[2];
                c.cmp(&d)
            })
            .map(|(k, _v)| *k);

        match key {
            None => {}
            Some(k) => {
                let rec = hist.get(k).unwrap();
                let x1_new = rec[1] as usize;
                let y1_new = rec[3] as usize;
                lcs_indices[x1_new] = y1_new as i32;
                FileDiff::histogram_lcs(file1, file2, x0, x1_new, y0, y1_new, lcs_indices);
                FileDiff::histogram_lcs(file1, file2, x1_new + 1, x1, y1_new + 1, y1, lcs_indices);
            }
        }
    }

    fn order_hunks_by_output_format(&mut self) {
        match self.format_options.output_format {
            OutputFormat::Debug => self.order_hunks_ascending(),
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

    fn print_context(&mut self, context: usize) -> Result<(), std::fmt::Error> {
        println!(
            "*** {}",
            Self::get_header(self.file1, self.format_options.label1())
        );
        println!(
            "--- {}",
            Self::get_header(self.file2, self.format_options.label2())
        );

        let mut diff_disp = ContextDiffDisplay::default();

        for hunk in self.hunks.hunks() {
            // move cursor to the start of context for first hunk
            if diff_disp.curr_pos1 == 0 {
                if hunk.ln1_start() > context {
                    // this is guaranteed to be >= 0 due to the above conditions
                    diff_disp
                        .update_curr_pos(hunk.ln1_start() - context, hunk.ln2_start() - context);
                }
                diff_disp.set_context_start();
            }

            // do we have enough context between hunks?
            if (diff_disp.curr_pos1 != 0) && (hunk.ln1_start() - diff_disp.curr_pos1 > context * 2)
            {
                // add context after the previous hunk
                diff_disp.write_line(
                    self.file1,
                    diff_disp.curr_pos1,
                    diff_disp.curr_pos1 + context,
                    "  ",
                    true,
                )?;
                diff_disp.write_line(
                    self.file2,
                    diff_disp.curr_pos2,
                    diff_disp.curr_pos2 + context,
                    "  ",
                    false,
                )?;
                // update current position and print the whole section
                diff_disp.update_curr_pos(hunk.ln1_start() - context, hunk.ln2_start() - context);
                // check if we have something to display other than just context
                let print_lines1 = diff_disp.hunk_lines[0]
                    .split('\n')
                    .any(|x| x.starts_with('-') || x.starts_with('!'));
                diff_disp.print_section(true, print_lines1);
                let print_lines2 = diff_disp.hunk_lines[1]
                    .split('\n')
                    .any(|x| x.starts_with('-') || x.starts_with('!'));
                diff_disp.print_section(false, print_lines2);
            }

            // add context before current hunk
            diff_disp.write_line(
                self.file1,
                diff_disp.curr_pos1,
                hunk.ln1_start(),
                "  ",
                true,
            )?;
            // add delete hunk
            let h1_prefix = if hunk.ln2_end() - hunk.ln2_start() > 0 {
                "! "
            } else {
                "- "
            };
            diff_disp.write_line(
                self.file1,
                hunk.ln1_start(),
                hunk.ln1_end(),
                h1_prefix,
                true,
            )?;

            // context before insertion
            diff_disp.write_line(
                self.file2,
                diff_disp.curr_pos2,
                hunk.ln2_start(),
                "  ",
                false,
            )?;
            // add insert hunk
            let h2_prefix = if hunk.ln1_end() - hunk.ln1_start() > 0 {
                "! "
            } else {
                "+ "
            };
            diff_disp.write_line(
                self.file2,
                hunk.ln2_start(),
                hunk.ln2_end(),
                h2_prefix,
                false,
            )?;
        }

        // print final hunk
        if !diff_disp.hunk_lines.is_empty() {
            // display the remaining context if possible
            if diff_disp.curr_pos1 < self.file1.lines().len() {
                let end1 = self.file1.lines().len().min(diff_disp.curr_pos1 + context);
                let end2 = self.file2.lines().len().min(diff_disp.curr_pos2 + context);
                diff_disp.write_line(self.file1, diff_disp.curr_pos1, end1, " ", true)?;
                diff_disp.write_line(self.file2, diff_disp.curr_pos2, end2, " ", false)?;
            }
            let print_lines1 = diff_disp.hunk_lines[0]
                .split('\n')
                .any(|x| x.starts_with('-') || x.starts_with('!'));
            diff_disp.print_section(true, print_lines1);
            let print_lines2 = diff_disp.hunk_lines[1]
                .split('\n')
                .any(|x| x.starts_with('+') || x.starts_with('!'));
            diff_disp.print_section(false, print_lines2);
        }

        if !self.file1.ends_with_newline() {
            println!("{}", NO_NEW_LINE_AT_END_OF_FILE);
        }
        if !self.file2.ends_with_newline() {
            println!("{}", NO_NEW_LINE_AT_END_OF_FILE);
        }
        Ok(())
    }

    fn print_unified(&mut self, unified: usize) -> Result<(), std::fmt::Error> {
        println!(
            "--- {}",
            Self::get_header(self.file1, self.format_options.label1())
        );
        println!(
            "+++ {}",
            Self::get_header(self.file2, self.format_options.label2())
        );

        let mut diff_disp = UnifiedDiffDisplay::default();

        for hunk in self.hunks.hunks() {
            // move cursor to the start of context for first hunk
            if diff_disp.curr_pos1 == 0 {
                if hunk.ln1_start() > unified {
                    // this is guaranteed to be >= 0 due to the above conditions
                    diff_disp
                        .update_curr_pos(hunk.ln1_start() - unified, hunk.ln2_start() - unified);
                }
                diff_disp.set_context_start();
            }

            // do we have enough context between hunks?
            if (diff_disp.curr_pos1 != 0) && (hunk.ln1_start() - diff_disp.curr_pos1 > unified * 2)
            {
                // add context after the previous hunk
                diff_disp.write_line(
                    self.file1,
                    diff_disp.curr_pos1,
                    diff_disp.curr_pos1 + unified,
                    " ",
                )?;
                // update current position and print the whole section
                diff_disp.update_curr_pos(hunk.ln1_start() - unified, hunk.ln2_start() - unified);
                diff_disp.print_section();
            }

            // add context before current hunk
            diff_disp.write_line(self.file1, diff_disp.curr_pos1, hunk.ln1_start(), " ")?;
            // add delete hunk
            diff_disp.write_line(self.file1, hunk.ln1_start(), hunk.ln1_end(), "-")?;
            // add insert hunk
            diff_disp.write_line(self.file2, hunk.ln2_start(), hunk.ln2_end(), "+")?;
        }

        // print final hunk
        if !diff_disp.hunk_lines.is_empty() {
            // display the remaining context if possible
            if diff_disp.curr_pos1 < self.file1.lines().len() {
                let end = self.file1.lines().len().min(diff_disp.curr_pos1 + unified);
                diff_disp.write_line(self.file1, diff_disp.curr_pos1, end, " ")?;
            }
            diff_disp.print_section();
        }

        if !self.file1.ends_with_newline() {
            println!("{}", NO_NEW_LINE_AT_END_OF_FILE);
        }
        if !self.file2.ends_with_newline() {
            println!("{}", NO_NEW_LINE_AT_END_OF_FILE);
        }
        Ok(())
    }

    pub fn get_header(file: &FileData, label: &Option<String>) -> String {
        if let Some(label) = label {
            label.to_string()
        } else {
            format!(
                "{}\t{}",
                file.path(),
                system_time_to_rfc2822(file.modified())
            )
        }
    }
}

#[derive(Default)]
pub struct UnifiedDiffDisplay {
    curr_pos1: usize,
    curr_pos2: usize,
    //`context_start` and `hunk_len` to get the values for the hunk header
    //keep track of the lines where context start
    context_start1: usize,
    context_start2: usize,
    //keep track of the length of the current hunk
    hunk1_len: usize,
    hunk2_len: usize,
    hunk_lines: String,
}

impl UnifiedDiffDisplay {
    pub fn write_line(
        &mut self,
        file: &FileData,
        start: usize,
        end: usize,
        prefix: &str,
    ) -> Result<(), std::fmt::Error> {
        let mut offset = 0;
        for i in start..end {
            writeln!(self.hunk_lines, "{prefix}{}", file.line(i))?;
            offset += 1;
        }
        if prefix == " " {
            self.curr_pos1 += offset;
            self.hunk1_len += offset;
            self.hunk2_len += offset;
        } else if prefix == "-" {
            self.curr_pos1 += offset;
            self.hunk1_len += offset;
        } else if prefix == "+" {
            self.hunk2_len += offset;
        }
        Ok(())
    }

    pub fn set_context_start(&mut self) {
        self.context_start1 = self.curr_pos1 + 1;
        self.context_start2 = self.curr_pos2 + 1;
    }
    pub fn update_curr_pos(&mut self, curr1: usize, curr2: usize) {
        self.curr_pos1 = curr1;
        self.curr_pos2 = curr2;
    }

    pub fn print_section(&mut self) {
        println!(
            "@@ -{},{} +{},{} @@",
            self.context_start1, self.hunk1_len, self.context_start2, self.hunk2_len
        );
        self.print_hunk();
        self.context_start1 = self.curr_pos1 + 1;
        self.context_start2 = self.curr_pos2 + 1;
        self.hunk1_len = 0;
        self.hunk2_len = 0;
    }

    pub fn print_hunk(&mut self) {
        if self.hunk_lines.ends_with('\n') {
            self.hunk_lines.pop();
        }
        println!("{}", self.hunk_lines);
        self.hunk_lines.clear();
    }
}

#[derive(Default)]
pub struct ContextDiffDisplay {
    curr_pos1: usize,
    curr_pos2: usize,
    //`context_start` and `hunk_len` to get the values for the hunk header
    //keep track of the lines where context start
    context_start1: usize,
    context_start2: usize,
    //keep track of the length of the current hunk
    hunk1_len: usize,
    hunk2_len: usize,
    hunk_lines: [String; 2],
}

impl ContextDiffDisplay {
    pub fn set_context_start(&mut self) {
        self.context_start1 = self.curr_pos1 + 1;
        self.context_start2 = self.curr_pos2 + 1;
    }
    pub fn update_curr_pos(&mut self, curr1: usize, curr2: usize) {
        self.curr_pos1 = curr1;
        self.curr_pos2 = curr2;
    }

    pub fn write_line(
        &mut self,
        file: &FileData,
        start: usize,
        end: usize,
        prefix: &str,
        is_file1: bool,
    ) -> Result<(), std::fmt::Error> {
        let mut offset = 0;
        let file_index = if is_file1 { 0 } else { 1 };

        for i in start..end {
            writeln!(self.hunk_lines[file_index], "{prefix}{}", file.line(i))?;
            offset += 1;
        }
        if prefix.starts_with(" ") {
            if is_file1 {
                self.curr_pos1 += offset;
                self.hunk1_len += offset;
            } else {
                self.curr_pos2 += offset;
                self.hunk2_len += offset;
            }
        } else if prefix.starts_with("-") {
            self.curr_pos1 += offset;
            self.hunk1_len += offset;
        } else if prefix.starts_with("+") {
            self.curr_pos2 += offset;
            self.hunk2_len += offset;
        } else if prefix.starts_with("!") {
            if is_file1 {
                self.curr_pos1 += offset;
                self.hunk1_len += offset;
            } else {
                self.curr_pos2 += offset;
                self.hunk2_len += offset;
            }
        }
        Ok(())
    }

    pub fn print_section(&mut self, is_file1: bool, print_lines: bool) {
        if is_file1 {
            println!(
                "***************\n*** {},{} ****",
                self.context_start1,
                self.context_start1 + self.hunk1_len - 1
            );
            if print_lines {
                self.print_hunk(true);
            }
            self.context_start1 = self.curr_pos1 + 1;
            self.hunk1_len = 0;
        } else {
            println!(
                "--- {},{} ----",
                self.context_start2,
                self.context_start2 + self.hunk2_len - 1
            );
            if print_lines {
                self.print_hunk(false);
            }
            self.context_start2 = self.curr_pos2 + 1;
            self.hunk2_len = 0;
        }
    }

    pub fn print_hunk(&mut self, is_file1: bool) {
        let file_index = if is_file1 { 0 } else { 1 };
        let lines = &mut self.hunk_lines[file_index];
        if lines.ends_with('\n') {
            lines.pop();
        }
        println!("{}", lines);
        self.hunk_lines[file_index].clear();
    }
}
