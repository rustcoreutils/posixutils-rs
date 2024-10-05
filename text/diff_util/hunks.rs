use crate::diff_util::constants::NO_NEW_LINE_AT_END_OF_FILE;

use super::{change::{Change, ChangeData}, file_data::FileData};

#[derive(Clone, Debug)]
pub struct Hunk {
    pub kind: Change,
    pub changes: Vec<Change>,
    pub ln2_start: usize,
    pub ln1_start: usize,
    pub ln2_end: usize,
    pub ln1_end: usize,
}

impl Default for Hunk {
    fn default() -> Self {
        Self {
            kind: Default::default(),
            changes: Default::default(),
            ln2_start: usize::MAX,
            ln1_start: usize::MAX,
            ln2_end: usize::MIN,
            ln1_end: usize::MIN,
        }
    }
}

impl Hunk {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn f1_range(&self) -> String {
        if self.ln1_start == self.ln1_end {
            format!("{}", self.ln1_start)
        } else {
            format!("{},{}", self.ln1_start, self.ln1_end)
        }
    }

    pub fn f2_range(&self) -> String {
        if self.ln2_start == self.ln2_end {
            format!("{}", self.ln2_start)
        } else {
            format!("{},{}", self.ln2_start, self.ln2_end)
        }
    }

    pub fn kind(&self) -> &Change {
        &self.kind
    }

    pub fn ln1_start(&self) -> usize {
        self.ln1_start
    }

    pub fn ln2_start(&self) -> usize {
        self.ln2_start
    }

    pub fn ln1_end(&self) -> usize {
        self.ln1_end
    }

    pub fn ln2_end(&self) -> usize {
        self.ln2_end
    }

    pub fn print_default(&mut self, file1: &FileData, file2: &FileData, is_last: bool) {
        match self.kind {
            Change::None => {}
            Change::Unchanged(_) => {}
            Change::Insert(_) => {
                println!("{}a{}", self.ln1_start, self.f2_range());

                for i in self.ln2_start..self.ln2_end {
                    println!("> {}", file2.line(i));
                }
            }
            Change::Delete(_) => {
                println!("{}d{}", self.f1_range(), self.ln2_end);

                for i in self.ln1_start..self.ln1_end {
                    println!("< {}", file1.line(i));
                }

                if is_last && file1.ends_with_newline() == false {
                    println!("{}", NO_NEW_LINE_AT_END_OF_FILE);
                }
            }
            Change::Substitute(_) => {

                println!("{}c{}", self.f1_range(), self.f2_range());

                for i in self.ln1_start..self.ln1_end {
                    println!("< {}", file1.line(i));
                }

                if is_last && !file1.ends_with_newline() {
                    println!("{}", NO_NEW_LINE_AT_END_OF_FILE);
                }

                println!("---");
                for i in self.ln2_start..self.ln2_end {
                    println!("> {}", file2.line(i));
                }

                if is_last && !file2.ends_with_newline() {
                    println!("{}", NO_NEW_LINE_AT_END_OF_FILE);
                }
            }
        }
    }

    pub fn print_debug(&mut self, file1: &FileData, file2: &FileData) {
        println!(
            "{}-{} ({}) <> {}-{} ({})",
            self.ln1_start,
            self.ln1_end,
            self.f1_range(),
            self.ln2_start,
            self.ln2_end,
            self.f2_range()
        );

        match &self.kind {
            Change::None => {}
            Change::Unchanged(_) => {
                self.changes.sort_by_key(|change| change.get_ln1());

                for change in &self.changes {
                    println!("{:?} \"{}\"", change, file1.line(change.get_ln1() - 1));
                }
            }
            Change::Insert(_) => {
                self.changes.sort_by_key(|change| change.get_ln2());

                for change in &self.changes {
                    println!("{:?} \"{}\"", change, file2.line(change.get_ln2() - 1));
                }
            }
            Change::Delete(_) => {
                self.changes.sort_by_key(|change| change.get_ln1());

                for change in &self.changes {
                    println!("{:?} \"{}\"", change, file1.line(change.get_ln1() - 1));
                }
            }
            Change::Substitute(_) => {
                self.changes.sort_by_key(|change| change.get_ln2());

                for change in &self.changes {
                    println!(
                        "{:?} \"{}\" => \"{}\"",
                        change,
                        file1.line(change.get_ln1() - 1),
                        file2.line(change.get_ln2() - 1)
                    );
                }
            }
        }
    }

    pub fn print_edit_script(&mut self, file1: &FileData, file2: &FileData, is_last: bool) {
        match &self.kind {
            Change::None => {}
            Change::Unchanged(_) => {}
            Change::Insert(_) => {
                self.changes.sort_by_key(|change| change.get_ln2());

                println!("{}a", self.ln1_end);
                for change in &self.changes {
                    println!("{}", file2.line(change.get_ln2() - 1));
                }

                println!(".")
            }
            Change::Delete(_) => {
                println!("{}d", self.f1_range());
            }
            Change::Substitute(_) => {
                self.changes.sort_by_key(|change| change.get_ln2());
                println!("{}c", self.f1_range());

                for change in &self.changes {
                    println!("{}", file2.line(change.get_ln2() - 1));
                }

                println!(".")
            }
        }

        if is_last && file1.ends_with_newline() == false {
            println!(
                "diff: {}:{}\n",
                file1.name(),
                &NO_NEW_LINE_AT_END_OF_FILE[1..]
            );
        }

        if is_last && file2.ends_with_newline() == false {
            println!(
                "diff: {}:{}\n",
                file2.name(),
                &NO_NEW_LINE_AT_END_OF_FILE[1..]
            );
        }
    }

    pub fn print_forward_edit_script(&mut self, file1: &FileData, file2: &FileData, is_last: bool) {
        match &self.kind {
            Change::None => {}
            Change::Unchanged(_) => {}
            Change::Insert(_) => {
                self.changes.sort_by_key(|change| change.get_ln2());

                println!("a{}", self.ln1_end);
                for change in &self.changes {
                    println!("{}", file2.line(change.get_ln2() - 1));
                }

                println!(".")
            }
            Change::Delete(_) => {
                println!("d{}", self.f1_range().replace(",", " "));
            }
            Change::Substitute(_) => {
                self.changes.sort_by_key(|change| change.get_ln2());
                println!("c{}", self.f1_range().replace(",", " "));

                for change in &self.changes {
                    println!("{}", file2.line(change.get_ln2() - 1));
                }

                println!(".")
            }
        }

        if is_last && file1.ends_with_newline() == false {
            println!(
                "diff: {}:{}\n",
                file1.name(),
                &NO_NEW_LINE_AT_END_OF_FILE[1..]
            );
        }

        if is_last && file2.ends_with_newline() == false {
            println!(
                "diff: {}:{}\n",
                file2.name(),
                &NO_NEW_LINE_AT_END_OF_FILE[1..]
            );
        }
    }
}

#[derive(Debug)]
pub struct Hunks {
    hunks: Vec<Hunk>,
}

impl Hunks {
    pub fn new() -> Self {
        Self {
            hunks: vec![Hunk::new(); 0],
        }
    }

    pub fn hunks(&self) -> &Vec<Hunk> {
        &self.hunks
    }

    pub fn hunks_mut(&mut self) -> &mut Vec<Hunk> {
        &mut self.hunks
    }

    pub fn hunk_at_mut(&mut self, index: usize) -> &mut Hunk {
        &mut self.hunks[index]
    }

    pub fn create_hunks_from_lcs(&mut self, lcs_indices: &Vec<i32>, num_lines1: usize, num_lines2: usize) {
        let mut curr_line1 = 0;
        let mut curr_line2 = 0;
        let mut prev_i = 0;
        let mut prev_l = 0;
        let mut block_start_1 = 0;
        let mut block_start_2 = 0;
        let max_index_2 = lcs_indices.iter().max().unwrap();
        lcs_indices.iter().enumerate().filter(|(_i, l)| **l != -1).for_each(|(i, l)| {
            // check if we reach the end of the block
            if (i - prev_i > 1) || (*l as usize - prev_l > 1) || (l == max_index_2) {
                let hunk_start_1 = curr_line1;
                let hunk_start_2 = curr_line2;

                // handle delete lines from lines1
                let mut has_deleted_lines = false;
                while curr_line1 < block_start_1 {
                    curr_line1 += 1;
                }
                if curr_line1 != hunk_start_1 {
                    has_deleted_lines = true;
                }

                // handle added lines from lines2
                let mut has_added_lines = false;
                while curr_line2 < block_start_2 {
                    curr_line2 += 1;
                }
                if curr_line2 != hunk_start_2 {
                    has_added_lines = true;
                }

                // Add the hunk
                if has_deleted_lines && has_added_lines {
                    self.hunks.push(Hunk {
                        kind: Change::Substitute(ChangeData::new(hunk_start_1, hunk_start_2)),
                        changes: vec![Change::default()],
                        ln2_start: hunk_start_2,
                        ln1_start: hunk_start_1,
                        ln2_end: curr_line2,
                        ln1_end: curr_line1,
                    });
                } else if has_deleted_lines {
                    let mut changes: Vec<Change> = Vec::new();
                    for i in hunk_start_1+1..curr_line1 {
                        changes.push(Change::Delete(ChangeData::new(i, i+1)));
                    }
                    self.hunks.push(Hunk {
                        kind: Change::Delete(ChangeData::new(hunk_start_1, hunk_start_2)),
                        changes: vec![Change::default()],
                        ln2_start: hunk_start_2,
                        ln1_start: hunk_start_1,
                        ln2_end: curr_line2,
                        ln1_end: curr_line1,
                    });
                } else if has_added_lines {
                    self.hunks.push(Hunk {
                        kind: Change::Insert(ChangeData::new(hunk_start_1, hunk_start_2)),
                        changes: vec![Change::default()],
                        ln2_start: hunk_start_2,
                        ln1_start: hunk_start_1,
                        ln2_end: curr_line2,
                        ln1_end: curr_line1,
                    });
                }

                // position current line directly after previous block end
                curr_line1 = prev_i + 1;
                curr_line2 = prev_l + 1;

                // new block start
                block_start_1 = i;
                block_start_2 = *l as usize;
            }
            prev_i = i;
            prev_l = *l as usize;

        });

        // Get last hunk (if any) after the last lcs block
        let mut has_deleted_lines = false;
        curr_line1 = prev_i + 1;
        let hunk_start_1 = curr_line1;
        while curr_line1 < num_lines1 {
            curr_line1 += 1;
        }
        if curr_line1 != hunk_start_1 {
            has_deleted_lines = true;
        }

        let mut has_added_lines = false;
        curr_line2 = prev_l + 1;
        let hunk_start_2 = curr_line2;
        while curr_line2 < num_lines2 {
            curr_line2 += 1;
        }
        if curr_line2 != hunk_start_2 {
            has_added_lines = true;
        }

        if has_deleted_lines && has_added_lines {
            self.hunks.push(Hunk {
                kind: Change::Substitute(ChangeData::new(hunk_start_1, hunk_start_2)),
                changes: vec![Change::default()],
                ln2_start: hunk_start_2,
                ln1_start: hunk_start_1,
                ln2_end: curr_line2,
                ln1_end: curr_line1,
            });
        } else if has_deleted_lines {
            self.hunks.push(Hunk {
                kind: Change::Delete(ChangeData::new(hunk_start_1, hunk_start_2)),
                changes: vec![Change::default()],
                ln2_start: hunk_start_2,
                ln1_start: hunk_start_1,
                ln2_end: curr_line2,
                ln1_end: curr_line1,
            });
        } else if has_added_lines {
            self.hunks.push(Hunk {
                kind: Change::Insert(ChangeData::new(hunk_start_1, hunk_start_2)),
                changes: vec![Change::default()],
                ln2_start: hunk_start_2,
                ln1_start: hunk_start_1,
                ln2_end: curr_line2,
                ln1_end: curr_line1,
            });
        }
    }
}
