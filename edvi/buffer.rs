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
            assert!(insert_before == false, "Cannot insert before line 0");
            self.lines.splice(0..0, data.clone());
            if self.lines.len() > 0 {
                self.cur_line = 1;
            }
        } else if insert_before {
            assert!(line_no <= self.lines.len(), "Line number out of bounds");
            self.lines.splice(line_no - 1..line_no - 1, data.clone());
        } else {
            assert!(line_no <= self.lines.len(), "Line number out of bounds");
            self.lines.splice(line_no..line_no, data.clone());
        }

        if self.cur_line == 0 && self.lines.len() > 0 {
            self.cur_line = 1;
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

    /// Deletes lines from start to end (inclusive) from the buffer.
    pub fn delete(&mut self, start: usize, end: usize) {
        assert!(
            start > 0 && start <= end && end <= self.lines.len(),
            "Invalid line range"
        );
        self.lines.drain(start - 1..end);
        if self.cur_line > self.lines.len() {
            self.cur_line = self.lines.len();
        }
    }

    // Helper function to generate test data
    #[cfg(test)]
    fn generate_test_data(data: &[&str]) -> Buffer {
        let lines: Vec<String> = data.iter().map(|&s| s.to_string()).collect();
        Buffer {
            pathname: String::new(),
            cur_line: if lines.is_empty() { 0 } else { 1 },
            lines,
        }
    }

    // Helper function to compare Vec<String> with &[&str]
    #[cfg(test)]
    fn compare_lines(lines: &Vec<String>, expected: &[&str]) {
        let expected_strings: Vec<String> = expected.iter().map(|&s| s.to_string()).collect();
        assert_eq!(lines, &expected_strings);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_insert_at_start() {
        let mut buffer = Buffer::new();
        let mut data = vec!["line 1".to_string(), "line 2".to_string()];
        buffer.insert(0, false, &mut data);

        assert_eq!(
            buffer.lines,
            vec!["line 1".to_string(), "line 2".to_string()]
        );
        assert_eq!(buffer.cur_line, 1);
    }

    #[test]
    fn test_insert_before_line() {
        let mut buffer = Buffer::new();
        buffer.lines = vec!["line 3".to_string(), "line 4".to_string()];
        let mut data = vec!["line 1".to_string(), "line 2".to_string()];
        buffer.insert(1, true, &mut data);

        assert_eq!(
            buffer.lines,
            vec![
                "line 1".to_string(),
                "line 2".to_string(),
                "line 3".to_string(),
                "line 4".to_string()
            ]
        );
    }

    #[test]
    fn test_insert_after_line() {
        let mut buffer = Buffer::new();
        buffer.lines = vec!["line 1".to_string(), "line 3".to_string()];
        let mut data = vec!["line 2".to_string()];
        buffer.insert(1, false, &mut data);

        assert_eq!(
            buffer.lines,
            vec![
                "line 1".to_string(),
                "line 2".to_string(),
                "line 3".to_string()
            ]
        );
    }

    #[test]
    fn test_insert_at_end() {
        let mut buffer = Buffer::new();
        buffer.lines = vec!["line 1".to_string(), "line 2".to_string()];
        let mut data = vec!["line 3".to_string(), "line 4".to_string()];
        buffer.insert(2, false, &mut data);

        assert_eq!(
            buffer.lines,
            vec![
                "line 1".to_string(),
                "line 2".to_string(),
                "line 3".to_string(),
                "line 4".to_string()
            ]
        );
    }

    #[test]
    #[should_panic]
    fn test_insert_invalid_line() {
        let mut buffer = Buffer::new();
        let mut data = vec!["line 1".to_string()];
        buffer.insert(1, true, &mut data);
    }

    const TEST_DATA: &[&str] = &["Line 1", "Line 2", "Line 3", "Line 4", "Line 5"];

    #[test]
    fn test_delete_single_line() {
        let mut buffer = Buffer::generate_test_data(TEST_DATA);
        buffer.delete(2, 2);
        Buffer::compare_lines(&buffer.lines, &["Line 1", "Line 3", "Line 4", "Line 5"]);
    }

    #[test]
    fn test_delete_multiple_lines() {
        let mut buffer = Buffer::generate_test_data(TEST_DATA);
        buffer.delete(2, 3);
        Buffer::compare_lines(&buffer.lines, &["Line 1", "Line 4", "Line 5"]);
    }

    #[test]
    fn test_delete_all_lines() {
        let mut buffer = Buffer::generate_test_data(TEST_DATA);
        buffer.delete(1, 5);
        Buffer::compare_lines(&buffer.lines, &[]);
    }

    #[test]
    fn test_delete_invalid_range() {
        let mut buffer = Buffer::generate_test_data(TEST_DATA);
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| buffer.delete(0, 2)));
        assert!(result.is_err());
    }
}
