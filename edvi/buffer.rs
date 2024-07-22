//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fs;
use std::io::{self, Write};

#[derive(Debug)]
pub struct Buffer {
    pub pathname: String,

    pub cur_line: usize,

    lines: Vec<String>,
}

impl Buffer {
    pub fn new() -> Buffer {
        Buffer {
            pathname: String::new(),
            cur_line: 0,
            lines: Vec::new(),
        }
    }

    pub fn last_line(&self) -> usize {
        self.lines.len()
    }

    pub fn set_cur_line(&mut self, line: usize) {
        assert!(line <= self.last_line());
        self.cur_line = line;
    }

    pub fn append(&mut self, other: &mut Vec<String>) {
        self.lines.append(other);

        if self.cur_line == 0 && self.lines.len() > 0 {
            self.cur_line = 1;
        }
    }

    pub fn len(&self) -> usize {
        let mut len = 0;
        for line in &self.lines {
            len += line.len();
        }

        len
    }

    pub fn insert(&mut self, line_no: usize, insert_before: bool, data: &mut Vec<String>) {
        if line_no == 0 {
            assert!(insert_before == false);
            self.lines.splice(0..0, data.clone());
        } else if insert_before {
            self.lines.splice(line_no - 1..line_no - 1, data.clone());
        } else {
            self.lines.splice(line_no..line_no, data.clone());
        }
    }

    pub fn write_lines<W: Write>(
        &self,
        start_line: usize,
        end_line: usize,
        writer: &mut W,
    ) -> io::Result<()> {
        for line in &self.lines[start_line - 1..end_line] {
            writer.write_all(line.as_bytes())?;
        }
        Ok(())
    }

    pub fn write_to_file(
        &self,
        start_line: usize,
        end_line: usize,
        pathname: &str,
    ) -> io::Result<()> {
        let mut file = fs::File::create(pathname)?;
        self.write_lines(start_line, end_line, &mut file)
    }
}
