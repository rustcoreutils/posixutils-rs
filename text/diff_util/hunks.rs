use crate::diff_util::constants::NO_NEW_LINE_AT_END_OF_FILE;

use super::{change::Change, file_data::FileData};

#[derive(Clone)]
pub struct Hunk {
    kind: Change,
    changes: Vec<Change>,
    ln2_start: usize,
    ln1_start: usize,
    ln2_end: usize,
    ln1_end: usize,
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
    pub fn from(change: Change) -> Self {
        let (ln1, ln2) = change.get_lns();

        Self {
            kind: change.clone(),
            changes: vec![change; 1],
            ln1_start: ln1,
            ln1_end: ln1,
            ln2_start: ln2,
            ln2_end: ln2,
        }
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

    pub fn add(&mut self, change: Change) {
        assert!(
            self.change_sequence_acceptable(&change),
            "Only change of following lines in one file and of the same kind accepted."
        );

        let (ln1, ln2) = change.get_lns();

        if ln1 < self.ln1_start {
            self.ln1_start = ln1
        }

        if ln1 > self.ln1_end {
            self.ln1_end = ln1
        }

        if ln2 < self.ln2_start {
            self.ln2_start = ln2
        }

        if ln2 > self.ln2_end {
            self.ln2_end = ln2
        }

        self.changes.push(change);
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

    pub fn change_sequence_acceptable(&self, change: &Change) -> bool {
        let sequence_is_allowed = if let Some(_) = self.changes.last() {
            change.get_ln1().abs_diff(self.ln1_start) == 1
                || change.get_ln1().abs_diff(self.ln1_end) == 1
                || change.get_ln2().abs_diff(self.ln2_start) == 1
                || change.get_ln2().abs_diff(self.ln2_end) == 1
        } else {
            true
        };

        sequence_is_allowed && self.kind == *change
    }

    pub fn print_default(&mut self, file1: &FileData, file2: &FileData, is_last: bool) {
        match self.kind {
            Change::None => {}
            Change::Unchanged(_) => {}
            Change::Insert(_) => {
                self.changes.sort_by_key(|change| change.get_ln2());

                println!("{}a{}", self.ln1_start, self.f2_range());
                for change in &self.changes {
                    println!("> {}", file2.line(change.get_ln2() - 1));
                }
            }
            Change::Delete(_) => {
                self.changes.sort_by_key(|change| change.get_ln1());

                println!("{}d{}", self.f1_range(), self.ln2_end);
                for change in &self.changes {
                    println!("< {}", file1.line(change.get_ln1() - 1));
                }

                if is_last && file1.ends_with_newline() == false {
                    println!("{}", NO_NEW_LINE_AT_END_OF_FILE);
                }
            }
            Change::Substitute(_) => {
                self.changes.sort_by_key(|change| change.get_ln2());

                println!("{}c{}", self.f1_range(), self.f2_range());

                let mut replaced_lines = vec![""; 0];

                for change in &self.changes {
                    let (new, old) = (
                        file2.line(change.get_ln2() - 1),
                        file1.line(change.get_ln1() - 1),
                    );
                    replaced_lines.push(new);
                    println!("< {}", old);
                }

                if is_last && file1.ends_with_newline() == false {
                    println!("{}", NO_NEW_LINE_AT_END_OF_FILE);
                }

                println!("---");

                for new in replaced_lines {
                    println!("> {}", new);
                }

                if is_last && file2.ends_with_newline() == false {
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

#[derive(Default)]
pub struct Hunks {
    hunks: Vec<Hunk>,
}

impl Hunks {
    pub fn add_change(&mut self, change: Change) {
        if let Some(last_hunk) = self.hunks.last_mut() {
            let last_change_kind = last_hunk.kind();

            if *last_change_kind == change {
                if last_hunk.change_sequence_acceptable(&change) {
                    last_hunk.add(change);
                } else {
                    self.hunks.push(Hunk::from(change))
                }
            } else {
                self.hunks.push(Hunk::from(change));
            }
        } else {
            self.hunks.push(Hunk::from(change));
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
}
