use crate::diff_util::constants::NO_NEW_LINE_AT_END_OF_FILE;

use super::file_data::FileData;

#[derive(Clone, Default)]
pub enum Change {
    #[default]
    None,
    Insert,
    Delete,
    Substitute,
}

#[derive(Clone)]
pub struct Hunk {
    pub kind: Change,
    pub ln2_start: usize,
    pub ln1_start: usize,
    pub ln2_end: usize,
    pub ln1_end: usize,
}

impl Default for Hunk {
    fn default() -> Self {
        Self {
            kind: Default::default(),
            ln2_start: usize::MAX,
            ln1_start: usize::MAX,
            ln2_end: usize::MIN,
            ln1_end: usize::MIN,
        }
    }
}

impl Hunk {
    pub fn f1_range(&self, is_ed: bool) -> String {
        if self.ln1_start == self.ln1_end {
            format!("{}", self.ln1_start)
        } else if is_ed && (self.ln1_start == self.ln1_end - 1) {
            format!("{}", self.ln1_end)
        } else {
            format!("{},{}", self.ln1_start + 1, self.ln1_end)
        }
    }

    pub fn f2_range(&self, is_ed: bool) -> String {
        if self.ln2_start == self.ln2_end {
            format!("{}", self.ln2_start)
        } else if is_ed && (self.ln2_start == self.ln2_end - 1) {
            format!("{}", self.ln2_end)
        } else {
            format!("{},{}", self.ln2_start + 1, self.ln2_end)
        }
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
            Change::Insert => {
                println!("{}a{}", self.ln1_start, self.f2_range(true));

                for i in self.ln2_start..self.ln2_end {
                    println!("> {}", file2.line(i));
                }
            }
            Change::Delete => {
                println!("{}d{}", self.f1_range(true), self.ln2_end);

                for i in self.ln1_start..self.ln1_end {
                    println!("< {}", file1.line(i));
                }

                if is_last && !file1.ends_with_newline() {
                    println!("{}", NO_NEW_LINE_AT_END_OF_FILE);
                }
            }
            Change::Substitute => {
                println!("{}c{}", self.f1_range(true), self.f2_range(true));

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
            self.f1_range(false),
            self.ln2_start,
            self.ln2_end,
            self.f2_range(false)
        );

        match &self.kind {
            Change::None => {}
            Change::Insert => {
                for i in self.ln2_start..self.ln2_end {
                    println!("+ \"{}\"", file2.line(i));
                }
            }
            Change::Delete => {
                for i in self.ln1_start..self.ln1_end {
                    println!("- \"{}\"", file1.line(i));
                }
            }
            Change::Substitute => {
                for i in self.ln1_start..self.ln1_end {
                    println!("- \"{}\"", file1.line(i));
                }
                for i in self.ln2_start..self.ln2_end {
                    println!("+ \"{}\"", file2.line(i));
                }
            }
        }
    }

    pub fn print_edit_script(&mut self, file1: &FileData, file2: &FileData, is_last: bool) {
        match &self.kind {
            Change::None => {}
            Change::Insert => {
                println!("{}a", self.ln1_end);
                for i in self.ln2_start..self.ln2_end {
                    println!("{}", file2.line(i));
                }

                println!(".")
            }
            Change::Delete => {
                println!("{}d", self.f1_range(true));
            }
            Change::Substitute => {
                println!("{}c", self.f1_range(true));
                for i in self.ln2_start..self.ln2_end {
                    println!("{}", file2.line(i));
                }

                println!(".")
            }
        }

        if is_last && !file1.ends_with_newline() {
            println!(
                "diff: {}:{}\n",
                file1.name(),
                &NO_NEW_LINE_AT_END_OF_FILE[1..]
            );
        }

        if is_last && !file2.ends_with_newline() {
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
    pub fn hunks(&self) -> &Vec<Hunk> {
        &self.hunks
    }

    pub fn hunks_mut(&mut self) -> &mut Vec<Hunk> {
        &mut self.hunks
    }

    pub fn hunk_at_mut(&mut self, index: usize) -> &mut Hunk {
        &mut self.hunks[index]
    }

    pub fn hunk_count(&self) -> usize {
        self.hunks.len()
    }

    pub fn create_hunks_from_lcs(
        &mut self,
        lcs_indices: &[i32],
        num_lines1: usize,
        num_lines2: usize,
    ) {
        let mut hunk_start1 = 0;
        let mut hunk_end1: usize;
        let mut hunk_start2 = 0;
        let mut hunk_end2: usize;
        let mut prev_val = -2_i32;
        for (i, lcs_index) in lcs_indices.iter().enumerate() {
            if (lcs_index == &-1) && (prev_val != -1) {
                // We reach a new deletion/substitution block
                hunk_start1 = i;
                hunk_start2 = if prev_val == -2 {
                    0
                } else {
                    (prev_val + 1) as usize
                };
            } else if (i != 0)
                && (prev_val != -1)
                && (lcs_index != &-1)
                && (lcs_index != &(prev_val + 1))
            {
                // there was an insertion (but no deletion)
                // no -1 values but a bump in the values, eg [136, 145]
                hunk_start1 = i;
                hunk_start2 = (prev_val + 1) as usize;
                hunk_end1 = i;
                hunk_end2 = *lcs_index as usize;
                self.add_hunk(hunk_start1, hunk_end1, hunk_start2, hunk_end2);
            }
            if (lcs_index != &-1) && (prev_val == -1) {
                // we reach the end of deletion/substitution block
                hunk_end1 = i;
                hunk_end2 = *lcs_index as usize;
                self.add_hunk(hunk_start1, hunk_end1, hunk_start2, hunk_end2);
            }
            prev_val = *lcs_index;
        }

        // final hunk: we might have only -1 at the end
        if lcs_indices[lcs_indices.len() - 1] == -1 {
            hunk_end1 = num_lines1;
            hunk_end2 = num_lines2;
            self.add_hunk(hunk_start1, hunk_end1, hunk_start2, hunk_end2);
        } else if lcs_indices[lcs_indices.len() - 1] < ((num_lines2 - 1) as i32) {
            // there might be some insertions after the last lcs block
            hunk_start1 = num_lines1 - 1;
            hunk_end1 = num_lines1 - 1;
            hunk_start2 = (lcs_indices[lcs_indices.len() - 1] + 1) as usize;
            hunk_end2 = num_lines2;
            self.add_hunk(hunk_start1, hunk_end1, hunk_start2, hunk_end2);
        }
    }

    pub fn add_hunk(
        &mut self,
        hunk_start1: usize,
        hunk_end1: usize,
        hunk_start2: usize,
        hunk_end2: usize,
    ) {
        let kind: Change;
        if hunk_start1 == hunk_end1 {
            kind = Change::Insert;
        } else if hunk_start2 == hunk_end2 {
            kind = Change::Delete;
        } else {
            kind = Change::Substitute;
        }
        self.hunks.push(Hunk {
            kind,
            ln2_start: hunk_start2,
            ln1_start: hunk_start1,
            ln2_end: hunk_end2,
            ln1_end: hunk_end1,
        });
    }
}
