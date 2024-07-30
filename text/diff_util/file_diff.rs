use super::{
    change::ChangeData,
    common::{FormatOptions, OutputFormat},
    constants::COULD_NOT_UNWRAP_FILENAME,
    diff_exit_status::DiffExitStatus,
    file_data::FileData,
    functions::{check_existance, is_binary, system_time_to_rfc2822, vec_min},
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
            format_options: format_options,
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

            diff.needleman_wunsch_diff_lines();

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

    fn compare_lines(&self, l1: &str, l2: &str) -> bool {
        if self.format_options.ignore_trailing_white_spaces {
            l1.trim_end() == l2.trim_end()
        } else {
            l1 == l2
        }
    }

    fn needleman_wunsch_diff_lines(&mut self) {
        let n = self.file1.lines().len();
        let m = self.file2.lines().len();
        let mut distances = vec![vec![0; m + 1]; n + 1];
        let mut file1_considered_lines = vec![0; 0];
        let mut file2_considered_lines = vec![0; 0];

        for i in 0..=n {
            distances[i][0] = i;
        }

        for j in 0..=m {
            distances[0][j] = j;
        }

        for i in 1..=n {
            for j in 1..=m {
                let cost = if self.compare_lines(&self.file1.line(i - 1), &self.file2.line(j - 1)) {
                    if !file1_considered_lines.contains(&i) && !file2_considered_lines.contains(&j)
                    {
                        file1_considered_lines.push(i);
                        file2_considered_lines.push(j);

                        self.add_change(Change::Unchanged(ChangeData::new(i, j)));
                    }
                    0
                } else {
                    1
                };

                let inserted = distances[i - 1][j] + 1;
                let deleted = distances[i][j - 1] + 1;
                let substituted = distances[i - 1][j - 1] + cost;

                distances[i][j] = vec_min(&[inserted, deleted, substituted]);
            }
        }

        let (mut i, mut j) = (n, m);

        while i > 0 || j > 0 {
            if j > 0 && distances[i][j] == distances[i][j - 1] + 1 {
                self.add_change(Change::Insert(ChangeData::new(i, j)));

                j -= 1
            } else if i > 0 && distances[i][j] == distances[i - 1][j] + 1 {
                self.add_change(Change::Delete(ChangeData::new(i, j)));

                i -= 1
            } else {
                if !self.compare_lines(&self.file1.line(i - 1), &self.file2.line(j - 1)) {
                    self.add_change(Change::Substitute(ChangeData::new(i, j)));
                }

                i -= 1;
                j -= 1
            }
        }
    }

    fn add_change(&mut self, change: Change) {
        if let Change::None = change {
        } else {
            if !self.are_different {
                self.are_different = match &change {
                    Change::None => false,
                    Change::Unchanged(_) => false,
                    Change::Insert(_) => true,
                    Change::Delete(_) => true,
                    Change::Substitute(_) => true,
                }
            }

            let (l1, l2) = change.get_lns();

            if l1 != 0 {
                self.file1.set_change(change, l1 - 1);
            }

            if l2 != 0 {
                self.file2.set_change(change, l2 - 1);
            }
        }

        self.hunks.add_change(change);
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
            println!("*** {} ****", format!("{},{}", cr.0, cr.1));
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

            println!("--- {} ----", format!("{},{}", cr.2, cr.3));

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
