//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
use std::{
    collections::{hash_map::Entry, HashMap},
    ffi::CString,
    fs::File,
    io::{BufReader, Bytes, Read, Write},
    rc::Rc,
};

use super::string::AwkString;
use crate::regex::Regex;

pub enum RecordSeparator {
    Char(u8),
    Null,
    Ere(Regex),
}

impl TryFrom<AwkString> for RecordSeparator {
    type Error = String;

    fn try_from(value: AwkString) -> Result<Self, Self::Error> {
        if value.is_empty() {
            Ok(RecordSeparator::Null)
        } else if value.len() == 1 {
            Ok(RecordSeparator::Char(value.as_bytes()[0]))
        } else {
            let ere = Regex::new(value.try_into()?)?;
            Ok(RecordSeparator::Ere(ere))
        }
    }
}

type ReadResult = Result<u8, String>;

macro_rules! read_iter_next {
    ($iter:expr, $ret:expr) => {
        match $iter.next() {
            Some(byte_result) => byte_result?,
            None => return $ret,
        }
    };
    ($iter:expr) => {
        read_iter_next!($iter, Ok(None))
    };
}

/// Try to find a regex match in the byte buffer. Returns the record before
/// the match and the remainder after it, or None if no match found.
fn ere_try_match(buf: &[u8], re: &Regex) -> Result<Option<(String, Vec<u8>)>, String> {
    if buf.is_empty() {
        return Ok(None);
    }
    let input = std::str::from_utf8(buf).map_err(|e| e.to_string())?;
    if let Some(m) = re.find_first(input) {
        let record = input[..m.start].to_string();
        let remainder = buf[m.end..].to_vec();
        Ok(Some((record, remainder)))
    } else {
        Ok(None)
    }
}

pub trait RecordReader: Iterator<Item = ReadResult> {
    fn is_done(&self) -> bool;

    fn ere_byte_buffer(&mut self) -> &mut Vec<u8>;

    fn read_next_record(&mut self, separator: &RecordSeparator) -> Result<Option<String>, String> {
        if self.is_done() {
            return Ok(None);
        }
        match separator {
            RecordSeparator::Char(sep) => {
                let mut buf = Vec::new();
                let mut next = read_iter_next!(self);
                while next != *sep {
                    buf.push(next);
                    next = read_iter_next!(
                        self,
                        Ok(Some(String::from_utf8(buf).map_err(|e| e.to_string())?))
                    );
                }
                Ok(Some(String::from_utf8(buf).map_err(|e| e.to_string())?))
            }
            RecordSeparator::Ere(re) => {
                // Incremental matching: read bytes into a buffer and check
                // for RS matches periodically to support streaming/interactive
                // input without reading everything into memory at once.
                let mut byte_buf = std::mem::take(self.ere_byte_buffer());

                // Check existing buffer first (remainder from previous call)
                if let Some((record, remainder)) = ere_try_match(&byte_buf, re)? {
                    *self.ere_byte_buffer() = remainder;
                    return Ok(Some(record));
                }

                // After a failed match at length L, skip newline-triggered checks
                // until the buffer grows enough. This avoids O(n*m) worst-case
                // when every newline triggers a futile re-scan of the whole buffer.
                // Threshold starts at 0 so the first newline always triggers a check
                // (important for interactive/streaming input).
                let mut next_newline_check_len: usize = 0;
                let mut bytes_since_check = 0usize;
                loop {
                    match self.next() {
                        Some(byte_result) => {
                            let byte = byte_result?;
                            byte_buf.push(byte);
                            bytes_since_check += 1;
                            let check = if byte == b'\n' {
                                byte_buf.len() >= next_newline_check_len
                            } else {
                                bytes_since_check >= 8192
                            };
                            if check {
                                bytes_since_check = 0;
                                if let Some((record, remainder)) = ere_try_match(&byte_buf, re)? {
                                    *self.ere_byte_buffer() = remainder;
                                    return Ok(Some(record));
                                }
                                let len = byte_buf.len();
                                next_newline_check_len = len + len.min(8192);
                            }
                        }
                        None => {
                            // EOF: check for final match, then return remainder
                            if byte_buf.is_empty() {
                                return Ok(None);
                            }
                            if let Some((record, remainder)) = ere_try_match(&byte_buf, re)? {
                                if !remainder.is_empty() {
                                    *self.ere_byte_buffer() = remainder;
                                }
                                return Ok(Some(record));
                            }
                            let input = String::from_utf8(byte_buf).map_err(|e| e.to_string())?;
                            return Ok(Some(input));
                        }
                    }
                }
            }
            RecordSeparator::Null => {
                // Skip leading blank lines
                let mut line_buf = Vec::new();
                loop {
                    let next = read_iter_next!(self);
                    if next == b'\n' {
                        if line_buf.is_empty() {
                            // blank line, keep skipping
                            continue;
                        }
                        // non-blank line found, we have the first line
                        break;
                    }
                    line_buf.push(next);
                }

                // line_buf has the first line (without newline)
                let mut record_buf = line_buf;

                // Accumulate subsequent lines until a blank line or EOF
                loop {
                    let mut line_buf = Vec::new();
                    loop {
                        match self.next() {
                            Some(byte_result) => {
                                let byte = byte_result?;
                                if byte == b'\n' {
                                    break;
                                }
                                line_buf.push(byte);
                            }
                            None => {
                                // EOF: if this line has content, add it
                                if !line_buf.is_empty() {
                                    record_buf.push(b'\n');
                                    record_buf.extend_from_slice(&line_buf);
                                }
                                return Ok(Some(
                                    String::from_utf8(record_buf).map_err(|e| e.to_string())?,
                                ));
                            }
                        }
                    }
                    if line_buf.is_empty() {
                        // blank line: end of record
                        break;
                    }
                    record_buf.push(b'\n');
                    record_buf.extend_from_slice(&line_buf);
                }

                Ok(Some(
                    String::from_utf8(record_buf).map_err(|e| e.to_string())?,
                ))
            }
        }
    }
}

pub struct FileStream {
    bytes: Bytes<BufReader<File>>,
    is_done: bool,
    ere_byte_buffer: Vec<u8>,
}

impl FileStream {
    pub fn open(path: &str) -> Result<Self, String> {
        let file = File::open(path).map_err(|e| e.to_string())?;
        let reader = BufReader::new(file);
        Ok(Self {
            bytes: reader.bytes(),
            is_done: false,
            ere_byte_buffer: Vec::new(),
        })
    }
}

impl Iterator for FileStream {
    type Item = ReadResult;

    fn next(&mut self) -> Option<Self::Item> {
        match self.bytes.next() {
            Some(Ok(byte)) => Some(Ok(byte)),
            Some(Err(e)) => Some(Err(e.to_string())),
            None => {
                self.is_done = true;
                None
            }
        }
    }
}

impl RecordReader for FileStream {
    fn is_done(&self) -> bool {
        self.is_done && self.ere_byte_buffer.is_empty()
    }

    fn ere_byte_buffer(&mut self) -> &mut Vec<u8> {
        &mut self.ere_byte_buffer
    }
}

#[cfg(test)]
pub struct StringRecordReader {
    string: String,
    index: usize,
    ere_byte_buffer: Vec<u8>,
}

#[cfg(test)]
impl<S: Into<String>> From<S> for StringRecordReader {
    fn from(value: S) -> Self {
        Self {
            string: value.into(),
            index: 0,
            ere_byte_buffer: Vec::new(),
        }
    }
}

#[cfg(test)]
impl Iterator for StringRecordReader {
    type Item = ReadResult;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index == self.string.len() {
            None
        } else {
            let result = self.string.as_bytes()[self.index];
            self.index += 1;
            Some(Ok(result))
        }
    }
}

#[cfg(test)]
impl RecordReader for StringRecordReader {
    fn is_done(&self) -> bool {
        self.index == self.string.len() && self.ere_byte_buffer.is_empty()
    }

    fn ere_byte_buffer(&mut self) -> &mut Vec<u8> {
        &mut self.ere_byte_buffer
    }
}

/// A no-op record reader that immediately signals EOF.
/// The `ere_byte_buffer` field exists solely to satisfy the `RecordReader` trait;
/// `Vec::new()` (via `Default`) does not heap-allocate.
#[derive(Default)]
pub struct EmptyRecordReader {
    ere_byte_buffer: Vec<u8>,
}

impl Iterator for EmptyRecordReader {
    type Item = ReadResult;

    fn next(&mut self) -> Option<Self::Item> {
        None
    }
}

impl RecordReader for EmptyRecordReader {
    fn is_done(&self) -> bool {
        true
    }

    fn ere_byte_buffer(&mut self) -> &mut Vec<u8> {
        &mut self.ere_byte_buffer
    }
}

#[derive(Default)]
pub struct WriteFiles {
    files: HashMap<String, File>,
}

impl WriteFiles {
    pub fn write(&mut self, filename: &str, contents: &str, append: bool) -> Result<(), String> {
        match self.files.entry(filename.to_string()) {
            Entry::Occupied(mut e) => {
                e.get_mut()
                    .write_all(contents.as_bytes())
                    .map_err(|e| e.to_string())?;
            }
            Entry::Vacant(e) => {
                let mut file = File::options()
                    .write(true)
                    .create(true)
                    .append(append)
                    .open(filename)
                    .map_err(|e| e.to_string())?;
                file.write_all(contents.as_bytes())
                    .map_err(|e| e.to_string())?;
                e.insert(file);
            }
        }
        Ok(())
    }

    pub fn flush_file(&mut self, filename: &str) -> bool {
        if let Some(file) = self.files.get_mut(filename) {
            file.flush().is_ok()
        } else {
            false
        }
    }

    pub fn flush_all(&mut self) -> bool {
        let mut success = true;
        for file in self.files.values_mut() {
            success = success && file.flush().is_ok();
        }
        success
    }

    pub fn close_file(&mut self, filename: &str) {
        self.files.remove(filename);
    }
}

#[derive(Default)]
pub struct ReadFiles {
    files: HashMap<Rc<str>, FileStream>,
}

impl ReadFiles {
    pub fn read_next_record(
        &mut self,
        filename: AwkString,
        separator: &RecordSeparator,
    ) -> Result<Option<String>, String> {
        let filename = Rc::<str>::from(filename);
        match self.files.entry(filename.clone()) {
            Entry::Occupied(mut e) => e.get_mut().read_next_record(separator),
            Entry::Vacant(e) => {
                let mut file = FileStream::open(&filename)?;
                let result = file.read_next_record(separator);
                e.insert(file);
                result
            }
        }
    }

    pub fn close_file(&mut self, filename: &str) {
        self.files.remove(filename);
    }
}

#[derive(Default)]
pub struct WritePipes {
    pipes: HashMap<Rc<str>, *mut libc::FILE>,
}

impl WritePipes {
    pub fn write(&mut self, command: AwkString, contents: AwkString) -> Result<(), String> {
        let key = Rc::from(command.clone());
        let file = match self.pipes.entry(key) {
            Entry::Occupied(e) => *e.get(),
            Entry::Vacant(e) => {
                let command: CString = command.try_into()?;
                let file = unsafe {
                    let file = libc::popen(command.as_ptr(), c"w".as_ptr());
                    if file.is_null() {
                        return Err("failed to open pipe".to_string());
                    }
                    file
                };
                e.insert(file);
                file
            }
        };
        let contents: CString = contents.try_into()?;
        let result = unsafe { libc::fputs(contents.as_ptr(), file) };
        if result == libc::EOF {
            return Err("failed to write to file".to_string());
        }
        Ok(())
    }

    pub fn flush_file(&mut self, filename: &str) -> bool {
        if let Some(file) = self.pipes.get(filename) {
            unsafe { libc::fflush(*file) == 0 }
        } else {
            false
        }
    }

    pub fn flush_all(&mut self) -> bool {
        let mut success = true;
        for file in self.pipes.values() {
            success = success && unsafe { libc::fflush(*file) == 0 };
        }
        success
    }

    pub fn close_pipe(&mut self, filename: &str) {
        if let Some(file) = self.pipes.remove(filename) {
            unsafe {
                libc::pclose(file);
            }
        }
    }
}

impl Drop for WritePipes {
    fn drop(&mut self) {
        for (_, file) in self.pipes.drain() {
            unsafe {
                if libc::pclose(file) == -1 {
                    eprintln!("awk: warning: failed to close write pipe");
                };
            }
        }
    }
}

pub struct PipeRecordReader {
    pipe: *mut libc::FILE,
    is_done: bool,
    ere_byte_buffer: Vec<u8>,
}

impl PipeRecordReader {
    pub fn open(command: &str) -> Result<Self, String> {
        let command = CString::new(command).map_err(|e| e.to_string())?;
        let file = unsafe {
            let file = libc::popen(command.as_ptr(), c"r".as_ptr());
            if file.is_null() {
                return Err("failed to open pipe".to_string());
            }
            file
        };
        Ok(Self {
            pipe: file,
            is_done: false,
            ere_byte_buffer: Vec::new(),
        })
    }
}

impl Iterator for PipeRecordReader {
    type Item = ReadResult;

    fn next(&mut self) -> Option<Self::Item> {
        let result = unsafe { libc::fgetc(self.pipe) };
        if result == libc::EOF {
            self.is_done = true;
            None
        } else {
            Some(Ok(result as u8))
        }
    }
}

impl RecordReader for PipeRecordReader {
    fn is_done(&self) -> bool {
        self.is_done && self.ere_byte_buffer.is_empty()
    }

    fn ere_byte_buffer(&mut self) -> &mut Vec<u8> {
        &mut self.ere_byte_buffer
    }
}

impl Drop for PipeRecordReader {
    fn drop(&mut self) {
        unsafe {
            if libc::pclose(self.pipe) == -1 {
                eprintln!("awk: warning: failed to close read pipe");
            };
        }
    }
}

#[derive(Default)]
pub struct ReadPipes {
    pipes: HashMap<Rc<str>, PipeRecordReader>,
}

impl ReadPipes {
    pub fn read_next_record(
        &mut self,
        command: AwkString,
        separator: &RecordSeparator,
    ) -> Result<Option<String>, String> {
        let command = Rc::<str>::from(command);
        match self.pipes.entry(command.clone()) {
            Entry::Occupied(mut e) => e.get_mut().read_next_record(separator),
            Entry::Vacant(e) => {
                let mut reader = PipeRecordReader::open(&command)?;
                let result = reader.read_next_record(separator);
                e.insert(reader);
                result
            }
        }
    }

    pub fn close_pipe(&mut self, command: &str) {
        self.pipes.remove(command);
    }
}

#[derive(Default)]
pub struct StdinRecordReader {
    is_done: bool,
    ere_byte_buffer: Vec<u8>,
}

impl Iterator for StdinRecordReader {
    type Item = ReadResult;

    fn next(&mut self) -> Option<Self::Item> {
        let next = std::io::stdin().lock().bytes().next();
        match next {
            Some(Ok(byte)) => Some(Ok(byte)),
            Some(Err(e)) => Some(Err(e.to_string())),
            None => {
                self.is_done = true;
                None
            }
        }
    }
}

impl RecordReader for StdinRecordReader {
    fn is_done(&self) -> bool {
        self.is_done && self.ere_byte_buffer.is_empty()
    }

    fn ere_byte_buffer(&mut self) -> &mut Vec<u8> {
        &mut self.ere_byte_buffer
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn split_records(file_contents: &str, separator: RecordSeparator) -> Vec<String> {
        let mut reader = StringRecordReader::from(file_contents);
        let mut result = Vec::new();
        while let Some(record) = reader.read_next_record(&separator).unwrap() {
            result.push(record);
        }
        result
    }

    #[test]
    fn split_empty_file() {
        assert!(split_records("", RecordSeparator::Null).is_empty());
    }

    #[test]
    fn split_records_with_paragraph_separator() {
        let records = split_records("record1\nrecord2\n\nrecord3\n", RecordSeparator::Null);
        assert_eq!(records, vec!["record1\nrecord2", "record3"]);
    }

    #[test]
    fn split_records_with_separator_chars() {
        let records = split_records("record1,record2,record3", RecordSeparator::Char(b','));
        assert_eq!(records, vec!["record1", "record2", "record3"]);
    }
}
