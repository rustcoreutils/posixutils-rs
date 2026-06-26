//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

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
    /// Format the file1 line range. A single-line range collapses to one
    /// number; a multi-line range joins the two bounds with `sep` (`,` for
    /// default/ed scripts, ` ` for the `-f` forward edit script).
    pub fn f1_range(&self, sep: &str) -> String {
        if self.ln1_start == self.ln1_end {
            format!("{}", self.ln1_start)
        } else if self.ln1_start == self.ln1_end - 1 {
            format!("{}", self.ln1_end)
        } else {
            format!("{}{}{}", self.ln1_start + 1, sep, self.ln1_end)
        }
    }

    /// Format the file2 line range. See [`Hunk::f1_range`].
    pub fn f2_range(&self, sep: &str) -> String {
        if self.ln2_start == self.ln2_end {
            format!("{}", self.ln2_start)
        } else if self.ln2_start == self.ln2_end - 1 {
            format!("{}", self.ln2_end)
        } else {
            format!("{}{}{}", self.ln2_start + 1, sep, self.ln2_end)
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

    /// True when this hunk's file1 span ends at file1's final line and that
    /// line has no trailing newline (so the marker belongs to this hunk).
    fn file1_lacks_final_newline(&self, file1: &FileData) -> bool {
        self.ln1_end == file1.lines().len() && !file1.ends_with_newline()
    }

    /// True when this hunk's file2 span ends at file2's final line and that
    /// line has no trailing newline (so the marker belongs to this hunk).
    fn file2_lacks_final_newline(&self, file2: &FileData) -> bool {
        self.ln2_end == file2.lines().len() && !file2.ends_with_newline()
    }

    pub fn print_default(&mut self, file1: &FileData, file2: &FileData) {
        match self.kind {
            Change::None => {}
            Change::Insert => {
                println!("{}a{}", self.ln1_start, self.f2_range(","));

                for i in self.ln2_start..self.ln2_end {
                    println!("> {}", file2.line(i));
                }

                if self.file2_lacks_final_newline(file2) {
                    println!("{}", NO_NEW_LINE_AT_END_OF_FILE);
                }
            }
            Change::Delete => {
                println!("{}d{}", self.f1_range(","), self.ln2_end);

                for i in self.ln1_start..self.ln1_end {
                    println!("< {}", file1.line(i));
                }

                if self.file1_lacks_final_newline(file1) {
                    println!("{}", NO_NEW_LINE_AT_END_OF_FILE);
                }
            }
            Change::Substitute => {
                println!("{}c{}", self.f1_range(","), self.f2_range(","));

                for i in self.ln1_start..self.ln1_end {
                    println!("< {}", file1.line(i));
                }

                if self.file1_lacks_final_newline(file1) {
                    println!("{}", NO_NEW_LINE_AT_END_OF_FILE);
                }

                println!("---");
                for i in self.ln2_start..self.ln2_end {
                    println!("> {}", file2.line(i));
                }

                if self.file2_lacks_final_newline(file2) {
                    println!("{}", NO_NEW_LINE_AT_END_OF_FILE);
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
                println!("{}d", self.f1_range(","));
            }
            Change::Substitute => {
                println!("{}c", self.f1_range(","));
                for i in self.ln2_start..self.ln2_end {
                    println!("{}", file2.line(i));
                }

                println!(".")
            }
        }

        // POSIX does not specify a no-newline marker for edit-script output, so
        // it must never appear on stdout (it would corrupt the ed script). GNU
        // emits a diagnostic to stderr instead; mirror that.
        if is_last && !file1.ends_with_newline() {
            eprintln!(
                "diff: {}:{}\n",
                file1.path(),
                &NO_NEW_LINE_AT_END_OF_FILE[1..]
            );
        }

        if is_last && !file2.ends_with_newline() {
            eprintln!(
                "diff: {}:{}\n",
                file2.path(),
                &NO_NEW_LINE_AT_END_OF_FILE[1..]
            );
        }
    }

    /// Print forward edit script format (-f flag)
    /// POSIX: command letter comes BEFORE line number (e.g., "c2" not "2c")
    pub fn print_forward_edit_script(&mut self, file1: &FileData, file2: &FileData, is_last: bool) {
        match &self.kind {
            Change::None => {}
            Change::Insert => {
                println!("a{}", self.ln1_end);
                for i in self.ln2_start..self.ln2_end {
                    println!("{}", file2.line(i));
                }
                println!(".")
            }
            Change::Delete => {
                println!("d{}", self.f1_range(" "));
            }
            Change::Substitute => {
                println!("c{}", self.f1_range(" "));
                for i in self.ln2_start..self.ln2_end {
                    println!("{}", file2.line(i));
                }
                println!(".")
            }
        }

        // See print_edit_script: no-newline marker goes to stderr, never stdout.
        if is_last && !file1.ends_with_newline() {
            eprintln!(
                "diff: {}:{}\n",
                file1.path(),
                &NO_NEW_LINE_AT_END_OF_FILE[1..]
            );
        }

        if is_last && !file2.ends_with_newline() {
            eprintln!(
                "diff: {}:{}\n",
                file2.path(),
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
            // For Insert, ln1_start represents the 1-indexed line after which to insert
            // For trailing insertion, this should be num_lines1 (after the last line)
            hunk_start1 = num_lines1;
            hunk_end1 = num_lines1;
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
