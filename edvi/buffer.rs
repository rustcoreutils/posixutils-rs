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

pub const MAX_CHUNK: usize = 1000000;

#[derive(Clone, Debug)]
pub struct Chunk {
    data: String,

    lines: usize,
    first_line: usize,
    last_line: usize,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            data: String::new(),
            lines: 0,
            first_line: 0,
            last_line: 0,
        }
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn is_edge(&self, line_no: usize) -> bool {
        (line_no == self.first_line) || (line_no == self.last_line)
    }

    pub fn line_pos(&self, line_no: usize) -> usize {
        assert!(line_no >= self.first_line && line_no <= self.last_line);

        let target_no = line_no - self.first_line;
        let mut int_no = 0;
        for (i, ch) in self.data.chars().enumerate() {
            if int_no == target_no {
                return i;
            }
            if ch == '\n' {
                int_no += 1;
            }
        }

        self.data.len() // actually a bug; only happens if a line is missing a newline
    }

    pub fn push_line(&mut self, line: &str) {
        self.data.push_str(line);
        self.lines += 1;
    }
}

pub fn as_chunks(vs: &[String]) -> Vec<Chunk> {
    let mut chunks: Vec<Chunk> = Vec::new();

    let mut chunk = Chunk::new();
    for line in vs {
        if chunk.len() + line.len() > MAX_CHUNK {
            chunks.push(chunk);
            chunk = Chunk::new();
        }
        chunk.push_line(line);
    }

    if chunk.len() > 0 {
        chunks.push(chunk);
    }

    chunks
}

#[derive(Debug)]
pub struct Buffer {
    pub pathname: String,

    pub cur_line: usize,
    pub last_line: usize,

    chunks: Vec<Chunk>,
}

impl Buffer {
    pub fn new() -> Buffer {
        Buffer {
            pathname: String::new(),
            cur_line: 0,
            last_line: 0,
            chunks: Vec::new(),
        }
    }

    pub fn set_cur_line(&mut self, line: usize) {
        assert!(line <= self.last_line);
        self.cur_line = line;
    }

    pub fn append(&mut self, mut chunk: Chunk) {
        let cur_tail = self.last_line;
        let new_lines = chunk.lines;

        chunk.first_line = cur_tail + 1;
        chunk.last_line = chunk.first_line + new_lines - 1;

        if self.cur_line == 0 {
            self.cur_line = 1;
        }
        self.last_line += new_lines;

        self.chunks.push(chunk);
    }

    pub fn len(&self) -> usize {
        let mut total = 0;
        for chunk in &self.chunks {
            total += chunk.len();
        }
        total
    }

    fn renumber(&mut self, start_pos: usize, adj: usize, had_insert: bool) {
        let mut pos = 0;
        for chunk in &mut self.chunks {
            if pos >= start_pos {
                if had_insert {
                    chunk.first_line += adj;
                    chunk.last_line += adj;
                } else {
                    chunk.first_line -= adj;
                    chunk.last_line -= adj;
                }
            }
            pos += 1;
        }
    }

    fn chunk_pos_by_line(&self, line_no: usize) -> Option<usize> {
        for (i, chunk) in self.chunks.iter().enumerate() {
            if line_no >= chunk.first_line && line_no <= chunk.last_line {
                return Some(i);
            }
        }
        None
    }

    fn insert_head(&mut self, chunks: &[Chunk]) {
        assert!(chunks.len() > 0);

        let mut chunks = chunks.to_vec();

        // total lines in insertion; assign line numbers.
        let mut total_lines = 0;
        let mut cur_line = 0;
        for chunk in &mut chunks {
            total_lines += chunk.lines;
            chunk.first_line = cur_line + 1;
            chunk.last_line = chunk.first_line + chunk.lines - 1;
            cur_line = chunk.last_line;
        }

        // adjust line numbers of existing chunks
        self.renumber(0, total_lines, true);

        self.last_line += total_lines;

        self.chunks.splice(0..0, chunks);
    }

    fn insert_tail(&mut self, chunks: &[Chunk]) {
        for chunk in chunks {
            self.append(chunk.clone());
        }
    }

    fn split_chunk(&mut self, pos: usize, line_no: usize) {
        let orig_chunk = &self.chunks[pos];
        let mut chunk1 = Chunk::new();
        let mut chunk2 = Chunk::new();

        let split_pos = orig_chunk.line_pos(line_no);

        chunk1.data = orig_chunk.data[0..split_pos].to_string();
        chunk1.lines = line_no - orig_chunk.first_line;
        chunk1.first_line = orig_chunk.first_line;
        chunk1.last_line = line_no - 1;

        chunk2.data = orig_chunk.data[split_pos..].to_string();
        chunk2.lines = orig_chunk.lines - chunk1.lines;
        chunk2.first_line = line_no;
        chunk2.last_line = orig_chunk.last_line;

        self.chunks.splice(pos..pos + 1, vec![chunk1, chunk2]);
    }

    fn insert_middle(&mut self, mut line_no: usize, insert_before: bool, chunks: &[Chunk]) {
        assert!(chunks.len() > 0);

        if !insert_before {
            line_no += 1;
        }

        let mut insert_pos = self.chunk_pos_by_line(line_no).expect("line_no not found");

        if !self.chunks[insert_pos].is_edge(line_no) {
            self.split_chunk(insert_pos, line_no);
            insert_pos += 1;
        }

        let mut chunks = chunks.to_vec();

        let mut total_lines = 0;
        let mut cur_line = line_no;
        for chunk in &mut chunks {
            total_lines += chunk.lines;
            chunk.first_line = cur_line;
            chunk.last_line = chunk.first_line + chunk.lines - 1;
            cur_line = chunk.last_line + 1;
        }

        self.renumber(insert_pos, total_lines, true);

        self.last_line += total_lines;

        self.chunks.splice(insert_pos..insert_pos, chunks);
    }

    pub fn insert(&mut self, line_no: usize, insert_before: bool, chunks: &[Chunk]) {
        if chunks.len() == 0 {
            assert!((insert_before && line_no == 1) || (!insert_before && line_no == 0));
            self.insert_tail(chunks);
        } else if insert_before && (line_no == 1 || line_no == 0) {
            self.insert_head(chunks);
        } else if !insert_before && line_no == self.last_line {
            self.insert_tail(chunks);
        } else {
            self.insert_middle(line_no, insert_before, chunks);
        }
    }

    pub fn write_chunks<W: Write>(
        &self,
        start_line: usize,
        end_line: usize,
        writer: &mut W,
    ) -> io::Result<()> {
        for chunk in &self.chunks {
            if chunk.last_line < start_line || chunk.first_line > end_line {
                continue;
            }
            let chunk_start = if chunk.first_line < start_line {
                chunk.line_pos(start_line)
            } else {
                0
            };
            let chunk_end = if chunk.last_line > end_line {
                chunk.line_pos(end_line + 1)
            } else {
                chunk.len()
            };
            writer.write_all(&chunk.data.as_bytes()[chunk_start..chunk_end])?;
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
        self.write_chunks(start_line, end_line, &mut file)
    }
}
