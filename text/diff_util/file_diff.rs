use super::{
    common::{FormatOptions, OutputFormat},
    constants::COULD_NOT_UNWRAP_FILENAME,
    diff_exit_status::DiffExitStatus,
    file_data::{FileData, LineReader},
    functions::{check_existance, is_binary, system_time_to_rfc2822},
    hunks::Hunks,
};

use crate::diff_util::{
    change::{Change, ChangeContext},
    constants::NO_NEW_LINE_AT_END_OF_FILE,
};

use std::{
    collections::HashMap,
    fs::{File, read_to_string},
    io::{self, BufReader, Read},
    os::unix::fs::MetadataExt,
    path::PathBuf,
};

#[derive(Debug)]
pub struct FileDiff<'a> {
    file1: &'a mut FileData<'a>,
    file2: &'a mut FileData<'a>,
    hunks: Hunks,
    format_options: &'a FormatOptions,
    are_different: bool,
}

impl<'a> FileDiff<'a> {
    pub fn are_different(&self) -> bool {
        self.are_different
    }

    fn new(
        file1: &'a mut FileData<'a>,
        file2: &'a mut FileData<'a>,
        format_options: &'a FormatOptions,
    ) -> Self {
        if format_options.label1.is_none() && format_options.label2.is_some() {
            panic!("label1 can not be NONE when label2 is available");
        }

        Self {
            file1,
            file2,
            hunks: Hunks::new(),
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
            return Self::binary_file_diff(&path1, &path2);
        } else {
            let content1 = read_to_string(&path1)?.into_bytes();
            let linereader1 = LineReader::new(&content1);
            let ends_with_newline1 = linereader1.ends_with_newline();
            let mut lines1 = Vec::new();
            for line in linereader1 {
                lines1.push(line);
            }

            let content2 = read_to_string(&path2)?.into_bytes();
            let linereader2 = LineReader::new(&content2);
            let ends_with_newline2 = linereader2.ends_with_newline();
            let mut lines2 = Vec::new();
            for line in linereader2 {
                lines2.push(line);
            }
            let mut file1 = FileData::get_file(path1, lines1, ends_with_newline1)?;
            let mut file2 = FileData::get_file(path2, lines2, ends_with_newline2)?;

            let mut diff = FileDiff::new(&mut file1, &mut file2, format_options);

            // histogram diff
            let mut lcs_indices: Vec<i32> = vec![-1; diff.file1.lines().len()];
            let num_lines1 = diff.file1.lines().len();
            let num_lines2 = diff.file2.lines().len();
            FileDiff::histogram_lcs(
                &diff.file1,
                &diff.file2,
                0,
                num_lines1,
                0,
                num_lines2,
                &mut lcs_indices,
            );

            diff.hunks
                .create_hunks_from_lcs(&lcs_indices, num_lines1, num_lines2);

            if diff.are_different() {
                if let Some(show_if_different) = show_if_different {
                    println!("{}", show_if_different);
                }
            }

            return diff.print();
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
            let path1_file = path1_file.file_name().expect(&COULD_NOT_UNWRAP_FILENAME);
            let path2 = path2.join(path1_file);

            if !check_existance(&path2)? {
                return Ok(DiffExitStatus::Trouble);
            }

            return FileDiff::file_diff(path1, path2, format_options, None);
        } else {
            let path2_file = path2.clone();
            let path2_file = path2_file.file_name().expect(&COULD_NOT_UNWRAP_FILENAME);
            let path1 = path1.join(path2_file);

            if !check_existance(&path1)? {
                return Ok(DiffExitStatus::Trouble);
            }

            return FileDiff::file_diff(path1, path2, format_options, None);
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
            self.print_context(context);
        } else if let OutputFormat::Unified(unified) = self.format_options.output_format {
            self.print_unified(unified);
        } else {
            let hunks_count = self.hunks.hunks().len();

            for hunk_index in 0..hunks_count {
                let hunk = self.hunks.hunk_at_mut(hunk_index);
                match self.format_options.output_format {
                    OutputFormat::Debug => hunk.print_debug(&self.file1, &self.file2),
                    OutputFormat::Default => {
                        hunk.print_default(&self.file1, &self.file2, hunk_index == hunks_count - 1)
                    }
                    OutputFormat::EditScript => hunk.print_edit_script(
                        &self.file1,
                        &self.file2,
                        hunk_index == hunks_count - 1,
                    ),
                    OutputFormat::Context(_) => {
                        eprintln!("OutputFormat::Context should be handled in other place");
                        return Ok(DiffExitStatus::Trouble);
                    }
                    OutputFormat::ForwardEditScript => hunk.print_forward_edit_script(
                        &self.file1,
                        &self.file2,
                        hunk_index == hunks_count - 1,
                    ),
                    OutputFormat::Unified(_) => {
                        eprintln!("OutputFormat::Unified should be handled in other place");
                        return Ok(DiffExitStatus::Trouble);
                    }
                }
            }
        }

        if self.are_different() {
            return Ok(DiffExitStatus::Different);
        } else {
            return Ok(DiffExitStatus::NotDifferent);
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
            None => {
                return;
            }
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

    fn get_context_ranges(&self, context: usize) -> Vec<ChangeContext> {
        let f1_lines = self.file1.lines().len();
        let f2_lines = self.file2.lines().len();

        let mut change_ranges = self
            .hunks
            .hunks()
            .iter()
            .filter(|hunk| !Change::is_none(&hunk.kind()) && !Change::is_unchanged(&hunk.kind()))
            .map(|hunk| {
                (
                    hunk.kind().clone(),
                    hunk.ln1_start() as i64,
                    hunk.ln1_end() as i64,
                    hunk.ln2_start() as i64,
                    hunk.ln2_end() as i64,
                )
            })
            .collect::<Vec<(Change, i64, i64, i64, i64)>>();

        change_ranges.sort_by_key(|change| (change.1, change.3));

        let mut context_ranges: Vec<ChangeContext> = Vec::new();

        let f1_max = if self.file1.ends_with_newline() && f1_lines - 1 >= 1 {
            f1_lines - 1
        } else {
            f1_lines
        };

        let f2_max = if self.file2.ends_with_newline() && f2_lines - 1 >= 1 {
            f2_lines - 1
        } else {
            f2_lines
        };

        for cr in change_ranges {
            let ln1s = i64::clamp(cr.1 - context as i64, 1, f1_max as i64);
            let ln1e = i64::clamp(cr.2 + context as i64, 1, f1_max as i64);
            let ln2s = i64::clamp(cr.3 - context as i64, 1, f2_max as i64);
            let ln2e = i64::clamp(cr.4 + context as i64, 1, f2_max as i64);

            if context_ranges.len() > 0 {
                // Overlap check
                if let Some(change_ctx) = context_ranges.last_mut() {
                    if change_ctx.ln1_end >= ln1s as usize || change_ctx.ln2_end >= ln2s as usize {
                        change_ctx.ln1_end = ln1e as usize;
                        change_ctx.ln2_end = ln2e as usize;
                        continue;
                    }
                }
            }

            context_ranges.push(ChangeContext {
                change: cr.0,
                ln1_start: ln1s as usize,
                ln1_end: ln1e as usize,
                hk1_start: cr.1 as usize,
                hk1_end: cr.2 as usize,
                ln2_start: ln2s as usize,
                ln2_end: ln2e as usize,
                hk2_start: cr.3 as usize,
                hk2_end: cr.4 as usize,
            });
        }

        context_ranges
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
        self.order_hunks_ascending();
        self.hunks.hunks_mut().reverse();
    }

    fn print_context(&mut self, context: usize) {
        println!(
            "*** {}",
            Self::get_header(self.file1, &self.format_options.label1)
        );
        println!(
            "--- {}",
            Self::get_header(self.file2, &self.format_options.label2)
        );

        let change_ranges = self.get_context_ranges(context);

        for cr_index in 0..change_ranges.len() {
            let cr = &change_ranges[cr_index];

            println!("***************");
            println!("*** {} ***", format!("{},{}", cr.ln1_start, cr.ln1_end));
            // if self.file1.expected_changed_in_range(
            //     cr.0 - 1,
            //     cr.1 - 1,
            //     &vec![Change::is_delete, Change::is_substitute],
            // ) {
            //     for i in cr.0..=cr.1 {
            //         println!(
            //             "{}",
            //             // self.file1.get_context_identifier(i - 1),
            //             self.file1.line(i - 1)
            //         );
            //     }
            // }

            if cr_index == change_ranges.len() - 1 {
                if self.file1.ends_with_newline() == false {
                    println!("{}", NO_NEW_LINE_AT_END_OF_FILE);
                }
            }

            println!("--- {} ---", format!("{},{}", cr.ln2_start, cr.ln2_end));

            // if self.file2.expected_changed_in_range(
            //     cr.2 - 1,
            //     cr.3 - 1,
            //     &vec![Change::is_insert, Change::is_substitute],
            // ) {
            //     for i in cr.2..=cr.3 {
            //         println!(
            //             "{}",
            //             // self.file2.get_context_identifier(i - 1),
            //             self.file2.line(i - 1)
            //         );
            //     }
            // }

            if cr_index == change_ranges.len() - 1 {
                if self.file2.ends_with_newline() == false {
                    println!("{}", NO_NEW_LINE_AT_END_OF_FILE);
                }
            }
        }
    }

    fn print_line(&self, start: usize, end: usize, prefix: &str) -> usize {
        let mut j = start;
        for i in start..end {
            if prefix == "+" {
                println!("{prefix}{}", self.file2.line(i));
            } else {
                println!("{prefix}{}", self.file1.line(i));
            }
            j += 1;
        }
        j
    }

    fn print_unified(&mut self, unified: usize) {
        println!(
            "--- {}",
            Self::get_header(self.file1, &self.format_options.label1)
        );
        println!(
            "+++ {}",
            Self::get_header(self.file2, &self.format_options.label2)
        );

        let mut curr_pos1 = 0;

        for hunk in self.hunks.hunks() {
            // print context before first hunk
            if (curr_pos1 == 0) && (hunk.ln1_start() > unified) {
                // this is guaranteed to be >= 0 due to the above conditions
                curr_pos1 = hunk.ln1_start() - unified;
                // FIXME: the numbers printed below are wrong
                println!(
                    "@@ -{},{} +{},{} @@",
                    hunk.ln1_start() + 1,
                    hunk.ln1_end() - hunk.ln1_start(),
                    hunk.ln2_start() + 1,
                    hunk.ln2_end() - hunk.ln2_start()
                );
            }

            // do we have enough context between hunks?
            if (curr_pos1 != 0) && (hunk.ln1_start() - curr_pos1 > unified * 2) {
                // print the context after the last hunk
                _ = self.print_line(curr_pos1, curr_pos1 + unified, " ");
                // print a new section start
                // FIXME: the numbers printed below are wrong
                println!(
                    "@@ -{},{} +{},{} @@",
                    hunk.ln1_start() + 1,
                    hunk.ln1_end() - hunk.ln1_start(),
                    hunk.ln2_start() + 1,
                    hunk.ln2_end() - hunk.ln2_start()
                );
                curr_pos1 = hunk.ln1_start() - unified
            }

            // print context before current hunk
            _ = self.print_line(curr_pos1, hunk.ln1_start(), " ");
            // print delete hunk
            curr_pos1 = self.print_line(hunk.ln1_start(), hunk.ln1_end(), "-");
            // print insert hunk
            _ = self.print_line(hunk.ln2_start(), hunk.ln2_end(), "+");
        }

        if !self.file1.ends_with_newline() {
            println!("{}", NO_NEW_LINE_AT_END_OF_FILE);
        }
        if !self.file2.ends_with_newline() {
            println!("{}", NO_NEW_LINE_AT_END_OF_FILE);
        }
    }

    pub fn get_header(file: &FileData, label: &Option<String>) -> String {
        if let Some(label) = label {
            return format!("{}", label);
        } else {
            return format!(
                "{}\t{}",
                file.path(),
                system_time_to_rfc2822(file.modified())
            );
        }
    }
}
