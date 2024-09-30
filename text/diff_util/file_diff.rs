use super::{
    common::{FormatOptions, OutputFormat},
    constants::COULD_NOT_UNWRAP_FILENAME,
    diff_exit_status::DiffExitStatus,
    file_data::FileData,
    functions::{check_existance, is_binary, system_time_to_rfc2822},
    hunks::Hunks,
};

use crate::diff_util::{
    change::Change,
    constants::NO_NEW_LINE_AT_END_OF_FILE,
    functions::{calculate_hash, increase_by_one_if},
};

use std::{
    collections::HashMap,
    fs::File,
    io::{self, BufReader, Read},
    os::unix::fs::MetadataExt,
    path::PathBuf,
};

#[derive(Debug)]
pub struct FileDiff<'a> {
    file1: &'a mut FileData,
    file2: &'a mut FileData,
    hunks: Hunks,
    format_options: &'a FormatOptions,
    are_different: bool,
}

impl<'a> FileDiff<'a> {
    pub fn are_different(&self) -> bool {
        self.are_different
    }

    fn new(
        file1: &'a mut FileData,
        file2: &'a mut FileData,
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
            let mut file1 = FileData::get_file(path1)?;
            let mut file2 = FileData::get_file(path2)?;

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
            if let Some(rec) = hist.get_mut(file1.line(i).as_str()) {
                rec[0] += 1_i32;
                rec[1] = i as i32;
            } else {
                hist.insert(file1.line(i).as_str(), vec![1, i as i32, 0, -1]);
            }
        }
        for i in y0..y1 {
            if let Some(rec) = hist.get_mut(file2.line(i).as_str()) {
                rec[2] += 1_i32;
                rec[3] = i as i32;
            } else {
                hist.insert(file2.line(i).as_str(), vec![0, -1, 1, i as i32]);
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

    fn get_context_ranges(&self, context: usize) -> Vec<(usize, usize, usize, usize)> {
        let f1_lines = self.file1.lines().len();
        let f2_lines = self.file2.lines().len();

        let mut change_ranges = self
            .hunks
            .hunks()
            .iter()
            .filter(|hunk| !Change::is_none(&hunk.kind()) && !Change::is_unchanged(&hunk.kind()))
            .map(|hunk| {
                (
                    hunk.ln1_start() as i64,
                    hunk.ln1_end() as i64,
                    hunk.ln2_start() as i64,
                    hunk.ln2_end() as i64,
                )
            })
            .collect::<Vec<(i64, i64, i64, i64)>>();

        change_ranges.sort_by_key(|change| (change.1, change.3));

        let mut context_ranges = vec![(usize::MIN, usize::MIN, usize::MIN, usize::MIN); 0];

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
            let ln1s = i64::clamp(cr.0 - context as i64, 1, f1_max as i64);
            let ln1e = i64::clamp(cr.1 + context as i64, 1, f1_max as i64);
            let ln2s = i64::clamp(cr.2 - context as i64, 1, f2_max as i64);
            let ln2e = i64::clamp(cr.3 + context as i64, 1, f2_max as i64);

            if context_ranges.len() > 0 {
                // Overlap check
                if let Some((_, ln1_end, _, ln2_end)) = context_ranges.last_mut() {
                    if *ln1_end >= ln1s as usize || *ln2_end >= ln2s as usize {
                        *ln1_end = ln1e as usize;
                        *ln2_end = ln2e as usize;
                        continue;
                    }
                }
            }

            context_ranges.push((ln1s as usize, ln1e as usize, ln2s as usize, ln2e as usize));
        }

        return context_ranges;
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
            let cr = change_ranges[cr_index];

            println!("***************");
            println!("*** {} ***", format!("{},{}", cr.0, cr.1));
            if self.file1.expected_changed_in_range(
                cr.0 - 1,
                cr.1 - 1,
                &vec![Change::is_delete, Change::is_substitute],
            ) {
                for i in cr.0..=cr.1 {
                    println!(
                        "{} {}",
                        self.file1.get_context_identifier(i - 1),
                        self.file1.line(i - 1)
                    );
                }
            }

            if cr_index == change_ranges.len() - 1 {
                if self.file1.ends_with_newline() == false {
                    println!("{}", NO_NEW_LINE_AT_END_OF_FILE);
                }
            }

            println!("--- {} ---", format!("{},{}", cr.2, cr.3));

            if self.file2.expected_changed_in_range(
                cr.2 - 1,
                cr.3 - 1,
                &vec![Change::is_insert, Change::is_substitute],
            ) {
                for i in cr.2..=cr.3 {
                    println!(
                        "{} {}",
                        self.file2.get_context_identifier(i - 1),
                        self.file2.line(i - 1)
                    );
                }
            }

            if cr_index == change_ranges.len() - 1 {
                if self.file2.ends_with_newline() == false {
                    println!("{}", NO_NEW_LINE_AT_END_OF_FILE);
                }
            }
        }
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

        let context_ranges = self.get_context_ranges(unified);

        for cr_index in 0..context_ranges.len() {
            let cr = context_ranges[cr_index];
            let mut changes = HashMap::<u64, Change>::new();

            for f1_line in cr.0..=cr.1 {
                let change = self.file1.change(f1_line - 1);
                changes.insert(calculate_hash(change), change.clone());
            }

            for f2_line in cr.2..=cr.3 {
                let change = self.file2.change(f2_line - 1);
                let hash = calculate_hash(change);
                if changes.contains_key(&hash) == false {
                    changes.insert(calculate_hash(change), change.clone());
                }
            }

            if changes.values().any(|change| {
                Change::is_delete(change)
                    || Change::is_insert(change)
                    || Change::is_substitute(change)
            }) {
                let mut values = changes.values().cloned().collect::<Vec<Change>>();
                values.sort_by_key(|change| change.get_lns());

                let f1_range = if values.iter().all(|change| change.is_insert()) {
                    format!("{},{}", cr.0, 0)
                } else {
                    let start = cr.0;
                    let count = cr.1 - cr.0 + 1;
                    if count == 1 {
                        format!("{}", start)
                    } else {
                        format!("{},{}", start, count)
                    }
                };

                let f2_range = if values.iter().all(|change| change.is_delete()) {
                    format!("{},{}", cr.2, 0)
                } else {
                    let start = cr.2;
                    let count = cr.3 - cr.2 + 1;
                    if count == 1 {
                        format!("{}", start)
                    } else {
                        format!("{},{}", start, count)
                    }
                };

                let unchanged_count = values.iter().filter(|change| change.is_unchanged()).count();
                let insert_count = values.iter().filter(|change| change.is_insert()).count();
                let delete_count = values.iter().filter(|change| change.is_delete()).count();
                let substitute_count = values
                    .iter()
                    .filter(|change| change.is_substitute())
                    .count();

                let mut printed_unchanged = 0usize;
                let mut printed_insert = 0usize;
                let mut printed_delete = 0usize;
                let mut printed_substitute = 0usize;

                let mut f1_no_eof_printable = true;
                let mut f2_no_eof_printable = true;

                println!("@@ -{} +{} @@", f1_range, f2_range);

                for change in values {
                    increase_by_one_if(change.is_unchanged(), &mut printed_unchanged);
                    increase_by_one_if(change.is_insert(), &mut printed_insert);
                    increase_by_one_if(change.is_delete(), &mut printed_delete);
                    increase_by_one_if(change.is_substitute(), &mut printed_substitute);

                    let printables = match change {
                        Change::None => vec![String::new(); 0],
                        Change::Unchanged(data) => {
                            vec![format!(" {}", self.file1.line(data.ln1() - 1))]
                        }
                        Change::Insert(data) => {
                            vec![format!("+{}", self.file2.line(data.ln2() - 1))]
                        }
                        Change::Delete(data) => {
                            vec![format!("-{}", self.file1.line(data.ln1() - 1))]
                        }
                        Change::Substitute(data) => {
                            vec![
                                format!("-{}", self.file1.line(data.ln1() - 1)),
                                format!("+{}", self.file2.line(data.ln2() - 1)),
                            ]
                        }
                    };

                    if change.is_none() {
                        continue;
                    }

                    println!("{}", printables[0]);

                    if f1_no_eof_printable
                        && cr_index == context_ranges.len() - 1
                        && printed_unchanged == unchanged_count
                        && printed_delete == delete_count
                        && printed_substitute == substitute_count
                        && self.file1.ends_with_newline() == false
                    {
                        println!("{}", NO_NEW_LINE_AT_END_OF_FILE);
                        f1_no_eof_printable = false;
                    }

                    if change.is_substitute() {
                        println!("{}", printables[1]);
                    }

                    if f2_no_eof_printable
                        && unified != 0
                        && cr_index == context_ranges.len() - 1
                        && insert_count == printed_insert
                        && printed_substitute == substitute_count
                        && self.file2.ends_with_newline() == false
                    {
                        println!("{}", NO_NEW_LINE_AT_END_OF_FILE);
                        f2_no_eof_printable = false;
                    }
                }
            }
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
