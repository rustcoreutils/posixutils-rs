//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// TODO:
// - Just a start with the core algorithm; -C and -U both need context output
// - Implement -r (recurse)
// - Research and implement -f alternate output format properly
//

extern crate clap;
extern crate diff;
extern crate plib;

use chrono::{DateTime, Local};
use clap::Parser;
use core::panic;
use gettextrs::{bind_textdomain_codeset, textdomain};
use plib::PROJECT_NAME;
use std::collections::HashMap;
use std::ffi::OsString;
use std::fs::{self, DirEntry, File};
use std::hash::Hash;
use std::hash::{DefaultHasher, Hasher};
use std::io::{self, BufReader, Read};
use std::os::unix::fs::MetadataExt;
use std::path::Path;
use std::time::SystemTime;

const NO_NEW_LINE_AT_END_OF_FILE: &'static str = "\\ No newline at end of file";

/// diff - compare two files
#[derive(Parser, Debug, Clone)]
#[command(author, version, about, long_about)]
struct Args {
    /// Cause EOL whitespace to be treated as blanks
    #[arg(short = 'b', long = "ignore-space-change")]
    ignore_eol_space: bool,

    /// Output 3 lines of copied context
    #[arg(short)]
    context3: bool,

    /// Output <N> lines of copied context
    #[arg(short='C', value_parser = clap::value_parser!(u32).range(1..))]
    context: Option<u32>,

    /// Produce output in a form suitable as input for the ed utility
    #[arg(short, long)]
    ed: bool,

    /// Produce output in an alternative form, similar in format to -e
    #[arg(short)]
    fed: bool,

    /// Apply diff recursively to files and directories of the same name
    #[arg(short, long)]
    recurse: bool,

    /// Output 3 lines of unified context
    #[arg(short)]
    unified3: bool,

    /// Output <N> lines of unified context
    #[arg(short='U', value_parser = clap::value_parser!(u32).range(1..))]
    unified: Option<u32>,

    /// First comparison file (or directory, if -r is specified)
    file1: String,

    #[arg(long, value_parser= clap::value_parser!(String))]
    label1: Option<String>,

    #[arg(long, value_parser= clap::value_parser!(String))]
    label2: Option<String>,

    /// Second comparison file (or directory, if -r is specified)
    file2: String,
}

struct FileData<'a> {
    name: &'a str,
    lines: Vec<String>,
    changes: Vec<Change>,
    modified: SystemTime,
    ends_with_newline: bool,
}

impl<'a> FileData<'a> {
    fn get_file(path: &'a str) -> io::Result<Self> {
        let file = File::open(path)?;
        let modified = file.metadata()?.modified()?;
        let mut buf_reader = BufReader::new(file);
        let mut content = String::new();
        buf_reader.read_to_string(&mut content)?;

        let mut lines = content
            .lines()
            .map(|line| line.to_string())
            .collect::<Vec<String>>();

        let ends_with_newline = content.ends_with("\n");

        if ends_with_newline {
            lines.push(String::from(""));
        }

        let changes = vec![Change::None; lines.len()];

        let result = Self {
            name: &path,
            lines,
            changes,
            modified,
            ends_with_newline,
        };

        Ok(result)
    }

    fn get_context_identifier(&self, change_index: usize) -> &str {
        match self.changes[change_index] {
            Change::None => " ",
            Change::Unchanged(_) => " ",
            Change::Insert(_) => "+",
            Change::Delete(_) => "-",
            Change::Substitute(_) => "!",
        }
    }

    fn lines(&self) -> &Vec<String> {
        &self.lines
    }

    fn line(&self, index: usize) -> &String {
        &self.lines[index]
    }

    fn modified(&self) -> SystemTime {
        self.modified
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn set_change(&mut self, change: Change, index: usize) {
        self.changes[index] = change;
    }

    fn expected_changed_in_range(
        &self,
        start: usize,
        end: usize,
        expected_changes: &Vec<fn(&Change) -> bool>,
    ) -> bool {
        for i in start..=end {
            for expected_change in expected_changes {
                if expected_change(&self.changes[i]) {
                    return true;
                }
            }
        }

        return false;
    }
}

#[derive(Clone, Copy, Debug, Default, Hash)]
struct ChangeData {
    ln1: usize, // line number in file1
    ln2: usize, // line number in file2
}

#[derive(Clone, Copy, Debug, Default, Hash)]
enum Change {
    #[default]
    None,
    Unchanged(ChangeData),
    Insert(ChangeData),
    Delete(ChangeData),
    Substitute(ChangeData),
}

impl Change {
    fn is_none(&self) -> bool {
        self == Change::None
    }

    fn is_unchanged(&self) -> bool {
        self == Change::Unchanged(Default::default())
    }

    fn is_insert(&self) -> bool {
        self == Change::Insert(Default::default())
    }

    fn is_delete(&self) -> bool {
        self == Change::Delete(Default::default())
    }

    fn is_substitute(&self) -> bool {
        self == Change::Substitute(Default::default())
    }

    /// returns (file1_line_number,file2_line_number)
    fn get_lns(&self) -> (usize, usize) {
        match self {
            Change::None => panic!("Change::None is not allowed in hunk."),
            Change::Unchanged(data) => (data.ln1, data.ln2),
            Change::Insert(data) => (data.ln1, data.ln2),
            Change::Delete(data) => (data.ln1, data.ln2),
            Change::Substitute(data) => (data.ln1, data.ln2),
        }
    }

    fn get_ln1(&self) -> usize {
        match self {
            Change::None => panic!("Change::None is not allowed in hunk."),
            Change::Unchanged(data) => data.ln1,
            Change::Insert(data) => data.ln1,
            Change::Delete(data) => data.ln1,
            Change::Substitute(data) => data.ln1,
        }
    }

    fn get_ln2(&self) -> usize {
        match self {
            Change::None => panic!("Change::None is not allowed in hunk."),
            Change::Unchanged(data) => data.ln2,
            Change::Insert(data) => data.ln2,
            Change::Delete(data) => data.ln2,
            Change::Substitute(data) => data.ln2,
        }
    }
}

impl PartialEq for Change {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::None, Self::None) => true,
            (Self::Unchanged(_), Self::Unchanged(_)) => true,
            (Self::Insert(_), Self::Insert(_)) => true,
            (Self::Delete(_), Self::Delete(_)) => true,
            (Self::Substitute(_), Self::Substitute(_)) => true,
            _ => false,
        }
    }
}

impl PartialEq<Change> for &Change {
    fn eq(&self, other: &Change) -> bool {
        match (self, other) {
            (Change::None, Change::None) => true,
            (Change::Unchanged(_), Change::Unchanged(_)) => true,
            (Change::Insert(_), Change::Insert(_)) => true,
            (Change::Delete(_), Change::Delete(_)) => true,
            (Change::Substitute(_), Change::Substitute(_)) => true,
            _ => false,
        }
    }
}

enum OutputFormat {
    #[allow(dead_code)]
    Debug,
    Default,
    Context(usize),
    EditScript,
    ForwardEditScript,
    Unified(usize),
}

impl From<&Args> for OutputFormat {
    fn from(args: &Args) -> Self {
        let mut args = args.clone();

        if args.context3 {
            args.context = Some(3);
        }

        if args.unified3 {
            args.unified = Some(3);
        }

        if args.ed {
            OutputFormat::EditScript
        } else if args.fed {
            OutputFormat::ForwardEditScript
        } else if let Some(n) = args.context {
            let n = if n == 0 { 1 } else { n };
            OutputFormat::Context(n as usize)
        } else if let Some(n) = args.unified {
            OutputFormat::Unified(n as usize)
        } else {
            OutputFormat::Default
        }
    }
}

#[derive(Clone)]
struct Hunk {
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
    fn new() -> Self {
        Self::default()
    }

    fn from(change: Change) -> Self {
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

    fn f1_range(&self) -> String {
        if self.ln1_start == self.ln1_end {
            format!("{}", self.ln1_start)
        } else {
            format!("{},{}", self.ln1_start, self.ln1_end)
        }
    }

    fn f2_range(&self) -> String {
        if self.ln2_start == self.ln2_end {
            format!("{}", self.ln2_start)
        } else {
            format!("{},{}", self.ln2_start, self.ln2_end)
        }
    }

    fn kind(&self) -> &Change {
        &self.kind
    }

    fn add(&mut self, change: Change) {
        assert!(
            self.change_sequence_acceptable(&change) && self.kind == change,
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

    fn ln1_start(&self) -> usize {
        self.ln1_start
    }

    fn ln2_start(&self) -> usize {
        self.ln2_start
    }

    fn ln1_end(&self) -> usize {
        self.ln1_end
    }

    fn ln2_end(&self) -> usize {
        self.ln2_end
    }

    fn change_sequence_acceptable(&self, change: &Change) -> bool {
        if let Some(_) = self.changes.last() {
            change.get_ln1().abs_diff(self.ln1_start) == 1
                || change.get_ln1().abs_diff(self.ln1_end) == 1
                || change.get_ln2().abs_diff(self.ln2_start) == 1
                || change.get_ln2().abs_diff(self.ln2_end) == 1
        } else {
            true
        }
    }

    fn print_normal(&mut self, file1: &FileData, file2: &FileData) {
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
                    replaced_lines.push(&new);
                    println!("< {}", old);
                }

                println!("---");

                for new in replaced_lines {
                    println!("> {}", new);
                }
            }
        }
    }

    fn print_debug(&mut self, file1: &FileData, file2: &FileData) {
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

    fn print_edit_script(&mut self, file2: &FileData) {
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
    }

    fn print_forward_edit_script(&mut self, file2: &FileData) {
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
    }
}

struct Hunks {
    hunks: Vec<Hunk>,
}

impl Hunks {
    fn new() -> Self {
        Self {
            hunks: vec![Hunk::new(); 0],
        }
    }

    fn add_change(&mut self, change: Change) {
        if let Some(last_hunk) = self.hunks.last_mut() {
            let last_change_kind = last_hunk.kind();

            if last_change_kind == change {
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
}

fn min(a: usize, b: usize, c: usize) -> usize {
    usize::min(usize::min(a, b), c)
}

struct Diff<'a> {
    file1: &'a mut FileData<'a>,
    file2: &'a mut FileData<'a>,
    hunks: Hunks,
    ignore_trailing_white_spaces: bool,
    output_format: OutputFormat,
    label1: &'a Option<String>,
    label2: &'a Option<String>,
}

impl<'a> Diff<'a> {
    fn new(
        file1: &'a mut FileData<'a>,
        file2: &'a mut FileData<'a>,
        ignore_trailing_white_spaces: bool,
        output_format: OutputFormat,
        label1: &'a Option<String>,
        label2: &'a Option<String>,
    ) -> Self {
        if label1.is_none() && label2.is_some() {
            panic!("label1 can not be NONE when label2 is SOME");
        }

        Self {
            file1,
            file2,
            hunks: Hunks::new(),
            ignore_trailing_white_spaces,
            output_format,
            label1,
            label2,
        }
    }

    fn print(&mut self) {
        self.order_hunks_by_output_format();

        if let OutputFormat::Context(context) = self.output_format {
            self.print_context(context);
        } else if let OutputFormat::Unified(unified) = self.output_format {
            self.print_unified(unified);
        } else {
            for hunk in &mut self.hunks.hunks {
                match self.output_format {
                    OutputFormat::Debug => hunk.print_debug(&self.file1, &self.file2),
                    OutputFormat::Default => hunk.print_normal(&self.file1, &self.file2),
                    OutputFormat::EditScript => hunk.print_edit_script(&self.file2),
                    OutputFormat::Context(_) => {
                        panic!("OutputFormat::Context should be handled in other place")
                    }
                    OutputFormat::ForwardEditScript => hunk.print_forward_edit_script(&self.file2),
                    OutputFormat::Unified(_) => todo!(),
                }
            }
        }
    }

    fn compare_lines(&self, l1: &str, l2: &str) -> bool {
        if self.ignore_trailing_white_spaces {
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

                        self.add_change(Change::Unchanged(ChangeData { ln1: i, ln2: j }));
                    }
                    0
                } else {
                    1
                };

                let inserted = distances[i - 1][j] + 1;
                let deleted = distances[i][j - 1] + 1;
                let substituted = distances[i - 1][j - 1] + cost;

                distances[i][j] = min(inserted, deleted, substituted);
            }
        }

        let (mut i, mut j) = (n, m);

        while i > 0 || j > 0 {
            if j > 0 && distances[i][j] == distances[i][j - 1] + 1 {
                self.add_change(Change::Insert(ChangeData { ln1: i, ln2: j }));

                j -= 1
            } else if i > 0 && distances[i][j] == distances[i - 1][j] + 1 {
                self.add_change(Change::Delete(ChangeData { ln1: i, ln2: j }));

                i -= 1
            } else {
                if !self.compare_lines(&self.file1.line(i - 1), &self.file2.line(j - 1)) {
                    self.add_change(Change::Substitute(ChangeData { ln1: i, ln2: j }));
                }

                i -= 1;
                j -= 1
            }
        }
    }

    fn add_change(&mut self, change: Change) {
        if let Change::None = change {
        } else {
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
            .hunks
            .iter()
            .filter(|hunk| !Change::is_none(&hunk.kind) && !Change::is_unchanged(&hunk.kind))
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

        let f1_max = if self.file1.ends_with_newline && f1_lines - 1 >= 1 {
            f1_lines - 1
        } else {
            f1_lines
        };

        let f2_max = if self.file2.ends_with_newline && f2_lines - 1 >= 1 {
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
        match self.output_format {
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
            .hunks
            .sort_by_key(|hunk| (hunk.ln1_end(), hunk.ln2_end()));
    }

    fn order_hunks_descending(&mut self) {
        self.order_hunks_ascending();
        self.hunks.hunks.reverse();
    }

    fn print_context(&mut self, context: usize) {
        println!("*** {}", Self::get_header(self.file1, self.label1));
        println!("--- {}", Self::get_header(self.file2, self.label2));

        let change_ranges = self.get_context_ranges(context);

        for cr_index in 0..change_ranges.len() {
            let cr = change_ranges[cr_index];

            let stars = if cr_index == 0 {
                "***************"
            } else {
                "**************"
            };

            println!("{}", stars);
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
                if self.file1.ends_with_newline == false {
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
                if self.file2.ends_with_newline == false {
                    println!("{}", NO_NEW_LINE_AT_END_OF_FILE);
                }
            }
        }
    }

    fn print_unified(&mut self, unified: usize) {
        println!("*** {}", Self::get_header(self.file1, self.label1));
        println!("--- {}", Self::get_header(self.file2, self.label2));

        let context_ranges = self.get_context_ranges(unified);

        for cr in &context_ranges {
            let mut changes = HashMap::<u64, Change>::new();

            for f1_line in cr.0..=cr.1 {
                let change = &self.file1.changes[f1_line - 1];
                changes.insert(calculate_hash(change), change.clone());
            }

            for f2_line in cr.2..=cr.3 {
                let change = &self.file2.changes[f2_line - 1];
                let hash = calculate_hash(change);
                if changes.contains_key(&hash) == false {
                    changes.insert(hash, change.clone());
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

                println!("@@ -{} +{} @@", f1_range, f2_range);

                for change in values {
                    match change {
                        Change::None => {}
                        Change::Unchanged(data) => println!(" {}", self.file1.line(data.ln1 - 1)),
                        Change::Insert(data) => println!("+{}", self.file2.line(data.ln2 - 1)),
                        Change::Delete(data) => println!("-{}", self.file1.line(data.ln1 - 1)),
                        Change::Substitute(data) => {
                            println!("+{}", self.file2.line(data.ln2 - 1));
                            println!("-{}", self.file1.line(data.ln1 - 1));
                        }
                    }
                }
            }
        }
    }

    fn get_header(file: &FileData<'a>, label: &'a Option<String>) -> String {
        if let Some(label) = label {
            return format!("{}", label);
        } else {
            return format!(
                "{} {}",
                file.name(),
                system_time_to_rfc2822(file.modified())
            );
        }
    }
}

fn system_time_to_rfc2822(system_time: SystemTime) -> String {
    Into::<DateTime<Local>>::into(system_time).to_rfc2822()
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let mut exit_code = 0;

    let output_format: OutputFormat = (&args).into();

    let path1_is_file = is_file(&args.file1);
    let path2_is_file = is_file(&args.file2);

    if let (Ok(path1_is_file), Ok(path2_is_file)) = (&path1_is_file, &path2_is_file) {
        if *path1_is_file && *path2_is_file {
            if is_binary(&args.file1)? || is_binary(&args.file2)? {
                diff_binary_file(&args.file1, &args.file2)?;
                return Ok(());
            }

            let mut file1 = FileData::get_file(&args.file1)?;
            let mut file2 = FileData::get_file(&args.file2)?;

            let mut diff = Diff::new(
                &mut file1,
                &mut file2,
                args.ignore_eol_space,
                output_format,
                &args.label1,
                &args.label2,
            );
            diff.needleman_wunsch_diff_lines();
            diff.print();
        } else if !path1_is_file && !path2_is_file {
            let _ = DirData::load(&args.file1)?;
            let _ = DirData::load(&args.file2)?;
        }
    }

    if let Err(e) = path1_is_file {
        exit_code = 1;
        eprintln!("diff: {}", e);
    }

    if let Err(e) = path2_is_file {
        exit_code = 1;
        eprintln!("diff: {}", e);
    }

    std::process::exit(exit_code)
}

fn is_file(path: &str) -> io::Result<bool> {
    let file = File::open(path)?;
    Ok(file.metadata()?.is_file())
}

fn calculate_hash<H: core::hash::Hash>(obj: &H) -> u64 {
    let mut hasher = DefaultHasher::new();
    obj.hash(&mut hasher);
    hasher.finish()
}

fn is_binary(file_path: &str) -> io::Result<bool> {
    let mut file = File::open(file_path)?;
    let mut buffer = [0; 16];

    file.read_exact(&mut buffer)?;

    if buffer.iter().any(|&byte| byte == 0) {
        return Ok(true);
    }

    Ok(false)
}

fn diff_binary_file(filename1: &str, filename2: &str) -> io::Result<()> {
    let file1 = File::open(filename1)?;
    let file2 = File::open(filename2)?;

    if file1.metadata()?.size() != file2.metadata()?.size() {
        println!("{} and {} differ", filename1, filename2);
    }

    let file1 = BufReader::new(file1);
    let file2 = BufReader::new(file2);

    for bytes_pair in file1.bytes().zip(file2.bytes()) {
        let (b1, b2) = (bytes_pair.0?, bytes_pair.1?);
        if b1 != b2 {
            println!("{} and {} differ", filename1, filename2);
        }
    }

    Ok(())
}

struct DirData {
    path: String,
    files: HashMap<OsString, DirEntry>,
    sub_dirs: Vec<DirData>,
}

impl DirData {
    fn load(path: &str) -> io::Result<DirData> {
        println!("debug path: {}", path);

        let mut dir_data = DirData {
            path: path.to_string(),
            files: Default::default(),
            sub_dirs: Default::default(),
        };
        let entries = fs::read_dir(path)?;

        for entry in entries {
            let entry = entry?;

            if entry.file_type()?.is_dir() {
                let path = Path::new(path)
                    .join(entry.file_name().clone())
                    .to_path_buf();

                if let Some(path) = path.to_str() {
                    dir_data.sub_dirs.push(Self::load(path)?);
                }
            }

            dir_data.files.insert(entry.file_name(), entry);
        }

        Ok(dir_data)
    }
}
