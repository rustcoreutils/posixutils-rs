//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
use std::{
    collections::{hash_map::Entry, HashMap},
    ffi::{CString, OsStr},
    fs::File,
    io::{BufWriter, Read, Write},
    os::unix::ffi::OsStrExt,
    path::PathBuf,
    rc::Rc,
};

use super::output;
use super::string::AwkString;
use crate::charset;
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

/// The awk string for the bytes of a record.
fn bytes_to_string(buf: Vec<u8>) -> String {
    charset::decode(buf)
}

/// The path awk opens for the file name `name`.
fn path_of(name: &str) -> PathBuf {
    PathBuf::from(OsStr::from_bytes(&charset::encode(name)))
}

/// Whether a match of the ERE `pattern` might be made longer by more
/// input: whether it repeats anything, with `*`, `+`, `?` or an interval,
/// outside a bracket expression and not quoted by a backslash.  (An
/// alternative such as `a|ab` could too, but waiting for more input after
/// every separator would delay each record of an interactive input.)
fn match_may_grow(pattern: &str) -> bool {
    let bytes = pattern.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        match bytes[i] {
            b'\\' => i += 2,
            b'[' => i = bracket_expression_end(bytes, i + 1),
            b'*' | b'+' | b'?' | b'{' => return true,
            _ => i += 1,
        }
    }
    false
}

/// The index just past the `]` that ends the bracket expression whose
/// contents start at `start`, or the length of `bytes` if none does.  A
/// `]` first (after an optional `^`) is part of it, as are the contents of
/// `[:class:]`, `[.symbol.]` and `[=class=]`, and a character after a
/// backslash, which awk takes as an escape there too.
fn bracket_expression_end(bytes: &[u8], start: usize) -> usize {
    let mut i = start;
    if bytes.get(i) == Some(&b'^') {
        i += 1;
    }
    if bytes.get(i) == Some(&b']') {
        i += 1;
    }
    while i < bytes.len() {
        match bytes[i] {
            b']' => return i + 1,
            // an awk escape: the character after the backslash is literal
            b'\\' => i += 2,
            b'[' if matches!(bytes.get(i + 1), Some(b':' | b'.' | b'=')) => {
                let delimiter = bytes[i + 1];
                i += 2;
                while i < bytes.len() && !(bytes[i] == delimiter && bytes.get(i + 1) == Some(&b']'))
                {
                    i += 1;
                }
                i += 2;
            }
            _ => i += 1,
        }
    }
    bytes.len()
}

/// How many bytes a reader asks its source for at a time.
const READ_SIZE: usize = 64 * 1024;

/// A reader of records: the bytes of a source, read in large pieces into a
/// buffer, from which records are taken whatever the record separator,
/// which may change between records.  What is read past a record stays in
/// the buffer for the next.
///
/// Each read returns as soon as the source has something, so a record is
/// returned as soon as its separator arrives, and the source is never read
/// again once it has reported its end, as a terminal would then wait for
/// more.
pub struct RecordInput<S> {
    source: S,
    buffer: Vec<u8>,
    /// Where the bytes not yet returned start in `buffer`.
    pos: usize,
    /// The source has reported its end.
    eof: bool,
}

/// What a search for a record separator found, in the bytes after `pos`.
enum SeparatorSearch {
    /// The record ends at the first offset, the separator at the second.
    Found(usize, usize),
    /// A separator reaching the end of the bytes read, which the input to
    /// come could make longer.
    Deferred,
    NotFound,
}

impl<S: Read> RecordInput<S> {
    pub fn new(source: S) -> Self {
        Self {
            source,
            buffer: Vec::new(),
            pos: 0,
            eof: false,
        }
    }

    /// The bytes read and not yet returned.
    fn pending(&self) -> &[u8] {
        &self.buffer[self.pos..]
    }

    /// Reads more of the source into the buffer: how many bytes, 0 at the
    /// end of the input.  The bytes before `pos` may be dropped, so callers
    /// keep offsets relative to it.
    fn fill(&mut self) -> Result<usize, String> {
        if self.eof {
            return Ok(0);
        }
        if self.pos > 0 && (self.pos == self.buffer.len() || self.pos >= READ_SIZE) {
            self.buffer.drain(..self.pos);
            self.pos = 0;
        }
        let old_len = self.buffer.len();
        self.buffer.resize(old_len + READ_SIZE, 0);
        loop {
            match self.source.read(&mut self.buffer[old_len..]) {
                Ok(n) => {
                    self.buffer.truncate(old_len + n);
                    self.eof = n == 0;
                    return Ok(n);
                }
                Err(e) if e.kind() == std::io::ErrorKind::Interrupted => continue,
                Err(e) => {
                    self.buffer.truncate(old_len);
                    return Err(plib::diag::io_error_text(&e));
                }
            }
        }
    }

    /// Returns the `len` bytes after `pos` as a record, and moves `pos`
    /// past them and the `skip` bytes of the separator after them.
    fn take_record(&mut self, len: usize, skip: usize) -> String {
        let record = bytes_to_string(self.pending()[..len].to_vec());
        self.pos += len + skip;
        record
    }

    /// Whether the input has ended and every byte of it has been returned.
    #[cfg(test)]
    pub fn is_done(&self) -> bool {
        self.eof && self.pos == self.buffer.len()
    }

    pub fn read_next_record(
        &mut self,
        separator: &RecordSeparator,
    ) -> Result<Option<String>, String> {
        match separator {
            RecordSeparator::Char(sep) => self.read_to_byte(*sep),
            RecordSeparator::Null => self.read_paragraph(),
            RecordSeparator::Ere(re) => self.read_to_match(re),
        }
    }

    /// A record ended by the byte `sep`, or by the end of the input.
    fn read_to_byte(&mut self, sep: u8) -> Result<Option<String>, String> {
        // the bytes already searched, after `pos`
        let mut searched = 0;
        loop {
            if let Some(i) = self.pending()[searched..].iter().position(|&b| b == sep) {
                return Ok(Some(self.take_record(searched + i, 1)));
            }
            searched = self.pending().len();
            if self.fill()? == 0 {
                return Ok((searched > 0).then(|| self.take_record(searched, 0)));
            }
        }
    }

    /// A record of paragraph mode (RS = ""): lines up to a blank line or
    /// the end of the input, after any blank lines before them.
    fn read_paragraph(&mut self) -> Result<Option<String>, String> {
        loop {
            let blank = self.pending().iter().take_while(|&&b| b == b'\n').count();
            self.pos += blank;
            if !self.pending().is_empty() {
                break;
            }
            if self.fill()? == 0 {
                return Ok(None);
            }
        }
        let mut searched = 0;
        loop {
            let pending = self.pending();
            if let Some(i) = pending[searched..].windows(2).position(|w| w == b"\n\n") {
                return Ok(Some(self.take_record(searched + i, 2)));
            }
            // a newline at the end may begin the blank line
            searched = pending.len().saturating_sub(1);
            if self.fill()? == 0 {
                let len = self.pending().len();
                let newline = self.pending().ends_with(b"\n") as usize;
                return Ok(Some(self.take_record(len - newline, newline)));
            }
        }
    }

    /// A record ended by a match of the regular expression `re`, or by the
    /// end of the input.
    ///
    /// A match is searched for after each read that came back short (the
    /// source had no more for now, as on an interactive input) and after
    /// the bytes read have doubled, so that a long record costs time in
    /// proportion to its length.
    fn read_to_match(&mut self, re: &Regex) -> Result<Option<String>, String> {
        let mut next_search_len = 0;
        loop {
            let at_eof = self.eof;
            if at_eof || self.pending().len() >= next_search_len {
                match self.search_separator(re, at_eof) {
                    SeparatorSearch::Found(len, end) => {
                        return Ok(Some(self.take_record(len, end - len)));
                    }
                    _ if at_eof => {
                        let len = self.pending().len();
                        return Ok((len > 0).then(|| self.take_record(len, 0)));
                    }
                    // try again after the next read
                    SeparatorSearch::Deferred => next_search_len = 0,
                    SeparatorSearch::NotFound => next_search_len = 2 * self.pending().len(),
                }
            }
            if self.fill()? < READ_SIZE {
                // a short read: search what there is
                next_search_len = 0;
            }
        }
    }

    /// Searches the bytes after `pos` for a match of the RS `re`.  Before
    /// the end of the input (`at_eof` false), a match that reaches the end
    /// of them is not taken if the input still to come could extend it
    /// (two newlines read one at a time for RS = "\n+").
    fn search_separator(&self, re: &Regex, at_eof: bool) -> SeparatorSearch {
        let pending = self.pending();
        if pending.is_empty() {
            return SeparatorSearch::NotFound;
        }
        match re.find_nonempty_bytes(pending) {
            Some(m) if at_eof || m.end < pending.len() || !match_may_grow(re.pattern()) => {
                SeparatorSearch::Found(m.start, m.end)
            }
            Some(_) => SeparatorSearch::Deferred,
            None => SeparatorSearch::NotFound,
        }
    }
}

/// The readers of records the main input and getline use.
pub trait RecordReader {
    fn read_next_record(&mut self, separator: &RecordSeparator) -> Result<Option<String>, String>;
}

impl<S: Read> RecordReader for RecordInput<S> {
    fn read_next_record(&mut self, separator: &RecordSeparator) -> Result<Option<String>, String> {
        RecordInput::read_next_record(self, separator)
    }
}

/// A file read for records.
pub type FileStream = RecordInput<File>;

impl FileStream {
    pub fn open(path: &str) -> Result<Self, String> {
        let file = File::open(path_of(path)).map_err(|e| plib::diag::io_error_text(&e))?;
        Ok(Self::new(file))
    }
}

/// Standard input read for records.  `Stdin` takes its lock for each read.
pub type StdinRecordReader = RecordInput<std::io::Stdin>;

impl Default for StdinRecordReader {
    fn default() -> Self {
        Self::new(std::io::stdin())
    }
}

#[derive(Default)]
pub struct WriteFiles {
    files: HashMap<String, BufWriter<File>>,
}

/// An output file name that stands for one of awk's own outputs, which awk
/// writes itself rather than opening the device: `/dev/stdout` shares the
/// buffer of standard output, so the order of the output is kept, and
/// `/dev/stderr` is written at once.
enum StandardOutput {
    Stdout,
    Stderr,
}

impl StandardOutput {
    fn named(filename: &str) -> Option<Self> {
        match filename {
            "/dev/stdout" => Some(Self::Stdout),
            "/dev/stderr" => Some(Self::Stderr),
            _ => None,
        }
    }
}

/// The error for a failed write to the output file `filename`.
fn file_write_error(filename: &str, e: &std::io::Error) -> String {
    gettextrs::gettext!(
        "cannot write to file '{}': {}",
        filename,
        plib::diag::io_error_text(e)
    )
}

impl WriteFiles {
    pub fn write(&mut self, filename: &str, contents: &str, append: bool) -> Result<(), String> {
        match StandardOutput::named(filename) {
            Some(StandardOutput::Stdout) => return output::write_stdout(contents),
            Some(StandardOutput::Stderr) => {
                return std::io::stderr()
                    .write_all(&charset::encode(contents))
                    .map_err(|e| file_write_error(filename, &e))
            }
            None => {}
        }
        let file = match self.files.entry(filename.to_string()) {
            Entry::Occupied(e) => e.into_mut(),
            Entry::Vacant(e) => {
                let file = File::options()
                    .write(true)
                    .create(true)
                    .truncate(!append)
                    .append(append)
                    .open(path_of(filename))
                    .map_err(|e| {
                        gettextrs::gettext!(
                            "cannot open file '{}' for writing: {}",
                            filename,
                            plib::diag::io_error_text(&e)
                        )
                    })?;
                e.insert(BufWriter::new(file))
            }
        };
        file.write_all(&charset::encode(contents))
            .map_err(|e| file_write_error(filename, &e))
    }

    /// Flushes the output file `filename`: whether that succeeded, or
    /// `None` if no such file is open.
    pub fn flush_file(&mut self, filename: &str) -> Option<bool> {
        match StandardOutput::named(filename) {
            Some(StandardOutput::Stdout) => Some(output::flush_stdout().is_ok()),
            Some(StandardOutput::Stderr) => Some(true),
            None => self
                .files
                .get_mut(filename)
                .map(|file| file.flush().is_ok()),
        }
    }

    /// Flushes every output file; whether all succeeded.
    pub fn flush_all(&mut self) -> bool {
        // every one, also after a failure
        let mut success = true;
        for file in self.files.values_mut() {
            success &= file.flush().is_ok();
        }
        success
    }

    /// Close a previously-opened output file. Returns `Some(0)` on a
    /// successful flush+close, `Some(-1)` if flushing failed, or `None` if no
    /// file was open under this name.
    pub fn close_file(&mut self, filename: &str) -> Option<i32> {
        let status = |ok: bool| if ok { 0 } else { -1 };
        match StandardOutput::named(filename) {
            Some(_) => self.flush_file(filename).map(status),
            None => self
                .files
                .remove(filename)
                .map(|mut file| status(file.flush().is_ok())),
        }
    }

    /// Flushes and closes every output file, at the end of the run: an
    /// error for the first that could not be written.
    pub fn close_all(&mut self) -> Result<(), String> {
        let mut result = Ok(());
        for (filename, mut file) in self.files.drain() {
            if let Err(e) = file.flush() {
                if result.is_ok() {
                    result = Err(file_write_error(&filename, &e));
                }
            }
        }
        result
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

    /// Close a previously-opened input file. Returns `Some(0)` if a file was
    /// open under this name, or `None` otherwise.
    pub fn close_file(&mut self, filename: &str) -> Option<i32> {
        self.files.remove(filename).map(|_| 0)
    }
}

/// What close() of a pipe and system() return for the `pclose` or
/// `system` result `status`: the command's exit status, 256 plus the number
/// of the signal that killed it, as in gawk and mawk, or -1 if the call
/// failed.
pub(crate) fn wait_status(status: libc::c_int) -> i32 {
    if status == -1 {
        -1
    } else if libc::WIFEXITED(status) {
        libc::WEXITSTATUS(status)
    } else if libc::WIFSIGNALED(status) {
        256 + libc::WTERMSIG(status)
    } else {
        status
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
        // fwrite, not fputs: the output may hold NUL bytes
        let bytes = charset::encode(&contents);
        let written = unsafe { libc::fwrite(bytes.as_ptr().cast(), 1, bytes.len(), file) };
        if written != bytes.len() {
            return Err("failed to write to file".to_string());
        }
        Ok(())
    }

    /// Whether an output pipe to `command` is open.
    pub fn is_open(&self, command: &str) -> bool {
        self.pipes.contains_key(command)
    }

    /// Flushes the output pipe `filename`: whether that succeeded, or
    /// `None` if no such pipe is open.
    pub fn flush_file(&mut self, filename: &str) -> Option<bool> {
        self.pipes
            .get(filename)
            .map(|file| unsafe { libc::fflush(*file) == 0 })
    }

    /// Flushes every output pipe; whether all succeeded.
    pub fn flush_all(&mut self) -> bool {
        // every one, also after a failure
        let mut success = true;
        for file in self.pipes.values() {
            success &= unsafe { libc::fflush(*file) == 0 };
        }
        success
    }

    /// Close a previously-opened output pipe. Returns the command's
    /// status as `wait_status` gives it, or `None` if no pipe was open under
    /// this name.
    pub fn close_pipe(&mut self, filename: &str) -> Option<i32> {
        self.pipes
            .remove(filename)
            .map(|file| wait_status(unsafe { libc::pclose(file) }))
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

/// The read end of a pipe `popen` opened, read through its descriptor:
/// the `FILE` is used only to `pclose` it, so stdio buffers nothing.
pub struct PipeSource {
    pipe: *mut libc::FILE,
    file: std::mem::ManuallyDrop<File>,
}

impl Read for PipeSource {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        (&*self.file).read(buf)
    }
}

/// The output of a command read for records (`cmd | getline`).
pub struct PipeRecordReader {
    input: RecordInput<PipeSource>,
    /// Set once the pipe has been `pclose`d by an explicit `close()`, so the
    /// `Drop` impl does not close it a second time.
    closed: bool,
}

impl PipeRecordReader {
    pub fn open(command: &str) -> Result<Self, String> {
        use std::os::unix::io::FromRawFd;
        let command = charset::to_cstring(command)?;
        let pipe = unsafe { libc::popen(command.as_ptr(), c"r".as_ptr()) };
        if pipe.is_null() {
            return Err("failed to open pipe".to_string());
        }
        // SAFETY: the descriptor is the pipe's, open until pclose, which
        // closes it; ManuallyDrop keeps the File from closing it too
        let file = std::mem::ManuallyDrop::new(unsafe { File::from_raw_fd(libc::fileno(pipe)) });
        Ok(Self {
            input: RecordInput::new(PipeSource { pipe, file }),
            closed: false,
        })
    }

    pub fn read_next_record(
        &mut self,
        separator: &RecordSeparator,
    ) -> Result<Option<String>, String> {
        self.input.read_next_record(separator)
    }

    /// `pclose` the pipe and return the command's status as `wait_status`
    /// gives it. Marks the reader closed so `Drop` will not close it again.
    fn pclose(&mut self) -> i32 {
        self.closed = true;
        wait_status(unsafe { libc::pclose(self.input.source.pipe) })
    }
}

impl Drop for PipeRecordReader {
    fn drop(&mut self) {
        if self.closed {
            return;
        }
        unsafe {
            if libc::pclose(self.input.source.pipe) == -1 {
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

    /// Whether an input pipe from `command` is open.
    pub fn is_open(&self, command: &str) -> bool {
        self.pipes.contains_key(command)
    }

    /// Close a previously-opened input pipe. Returns the command's status as
    /// `wait_status` gives it, or `None` if no pipe was open under this name.
    pub fn close_pipe(&mut self, command: &str) -> Option<i32> {
        self.pipes.remove(command).map(|mut reader| reader.pclose())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A source of the bytes of a string that gives at most `chunk` bytes
    /// a read.  Like a terminal after an end of file, it must not be read
    /// again once it has reported the end.
    struct ChunkedSource {
        bytes: Vec<u8>,
        pos: usize,
        chunk: usize,
        ended: bool,
    }

    impl Read for ChunkedSource {
        fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
            if self.pos == self.bytes.len() {
                assert!(!self.ended, "read again after the end of the input");
                self.ended = true;
                return Ok(0);
            }
            let n = self.chunk.min(buf.len()).min(self.bytes.len() - self.pos);
            buf[..n].copy_from_slice(&self.bytes[self.pos..self.pos + n]);
            self.pos += n;
            Ok(n)
        }
    }

    /// The records of `file_contents`, which must be the same however the
    /// source divides it into reads.
    fn split_records(file_contents: &str, separator: RecordSeparator) -> Vec<String> {
        let split = |chunk| {
            let mut reader = RecordInput::new(ChunkedSource {
                bytes: file_contents.as_bytes().to_vec(),
                pos: 0,
                chunk,
                ended: false,
            });
            let mut result = Vec::new();
            while let Some(record) = reader.read_next_record(&separator).unwrap() {
                result.push(record);
            }
            assert!(reader.is_done());
            result
        };
        let result = split(READ_SIZE);
        for chunk in [1, 2, 3, 7] {
            assert_eq!(
                split(chunk),
                result,
                "{file_contents:?} read {chunk} bytes at a time"
            );
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
    fn split_paragraphs_without_final_newline() {
        let records = split_records("a\n\nb", RecordSeparator::Null);
        assert_eq!(records, vec!["a", "b"]);
        let records = split_records("abc", RecordSeparator::Null);
        assert_eq!(records, vec!["abc"]);
        let records = split_records("\n\nabc", RecordSeparator::Null);
        assert_eq!(records, vec!["abc"]);
    }

    #[test]
    fn rs_match_may_grow_only_by_repetition() {
        for (pattern, grows) in [
            ("\n+", true),
            ("a{2,}", true),
            ("x?", true),
            ("\n|;", false),
            ("[+]\n", false),
            ("[]*]x", false),
            ("[^]?]x", false),
            ("[[:alpha:]+]", false),
            ("\\*\n", false),
            ("[a]b*", true),
            // awk escapes hold in a bracket expression too
            ("[\\][]x+", true),
            ("[\\]+]", false),
        ] {
            assert_eq!(match_may_grow(pattern), grows, "{pattern}");
        }
    }

    #[test]
    fn regex_rs_does_not_read_again_after_the_end() {
        let ere = || RecordSeparator::Ere(crate::regex::regex_from_str("\n+"));
        assert_eq!(split_records("a\n\nb\n\n", ere()), vec!["a", "b"]);
        assert_eq!(split_records("a\n\nb", ere()), vec!["a", "b"]);
    }

    #[test]
    fn split_records_with_separator_chars() {
        let records = split_records("record1,record2,record3", RecordSeparator::Char(b','));
        assert_eq!(records, vec!["record1", "record2", "record3"]);
    }
}
