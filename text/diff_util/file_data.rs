//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::{collections::hash_map::DefaultHasher, hash::Hasher, mem::take, time::SystemTime};

/// The bytes `-b` treats as white space.
///
/// POSIX describes `-b` in terms of "white space", not `<blank>`, and GNU diff
/// tests `isspace()`, so this is the C locale's set minus `<newline>`, which
/// cannot occur inside a line. Keeping `\r` here is what makes `-b` the way to
/// compare a CRLF file with an LF one now that the default comparison reports
/// that difference.
fn is_blank(b: u8) -> bool {
    matches!(b, b' ' | b'\t' | b'\r' | 0x0b | 0x0c)
}

/// The `-b` normal form of a line, yielded byte by byte so nothing is
/// allocated: every run of white space becomes a single space, and a run that
/// reaches the end of the line is dropped instead.
struct Normalized<'a> {
    rest: &'a [u8],
}

impl Iterator for Normalized<'_> {
    type Item = u8;

    fn next(&mut self) -> Option<u8> {
        let &b = self.rest.first()?;
        if !is_blank(b) {
            self.rest = &self.rest[1..];
            return Some(b);
        }
        let run = self.rest.iter().take_while(|&&c| is_blank(c)).count();
        self.rest = &self.rest[run..];
        // Trailing white space is ignored outright, so the normal form ends
        // here rather than with a space.
        (!self.rest.is_empty()).then_some(b' ')
    }
}

fn normalized(line: &[u8]) -> Normalized<'_> {
    Normalized { rest: line }
}

/// A file, split into lines that borrow from the buffer it was read into.
///
/// Lines are bytes, not `str`: the input files "may be of any type" (POSIX
/// INPUT FILES), and a patch generated from a Latin-1 or Shift-JIS file has to
/// carry that file's bytes to be applicable to it.
#[derive(Debug)]
pub struct FileData<'a> {
    /// What to call this file in output. The operand as the user wrote it,
    /// except for standard input, which POSIX names `-`.
    name: String,
    lines: Vec<&'a [u8]>,
    hashes: Vec<u64>, // Pre-computed line hashes for O(1) comparison
    modified: SystemTime,
    ends_with_newline: bool,
    normalize_ws: bool, // Whether whitespace normalization is enabled (-b flag)
}

impl<'a> FileData<'a> {
    pub fn ends_with_newline(&self) -> bool {
        self.ends_with_newline
    }

    /// Build the per-line view of a file already read into memory.
    ///
    /// Takes `modified` rather than looking it up, so the timestamp in a
    /// `-c`/`-u` header comes from the same open file the bytes did.
    pub fn new(
        name: String,
        lines: Vec<&'a [u8]>,
        modified: SystemTime,
        ends_with_newline: bool,
        normalize_ws: bool,
    ) -> Self {
        let line_count = lines.len();
        // Pre-compute hashes for O(1) line comparison. Under -b the hash is
        // taken over the normalized line so that lines differing only in
        // whitespace land in the same bucket; the original bytes are kept for
        // output either way.
        let hashes: Vec<u64> = lines
            .iter()
            .enumerate()
            .map(|(index, line)| {
                let mut hasher = DefaultHasher::new();
                if normalize_ws {
                    for b in normalized(line) {
                        hasher.write_u8(b);
                    }
                } else {
                    hasher.write(line);
                    // Whether the line is newline-terminated is part of what it
                    // is, so it has to be part of the hash: "b\n" and a final
                    // "b" with no newline are different lines. -b ignores the
                    // distinction, as GNU does, because it treats trailing
                    // white space -- including the newline -- as insignificant.
                    let terminated = ends_with_newline || index + 1 != line_count;
                    hasher.write_u8(terminated as u8);
                }
                hasher.finish()
            })
            .collect();

        Self {
            name,
            lines,
            hashes,
            modified,
            ends_with_newline,
            normalize_ws,
        }
    }

    /// Whether line `index` ends with a newline. Only a file's final line can
    /// lack one.
    fn line_terminated(&self, index: usize) -> bool {
        self.ends_with_newline || index + 1 != self.lines.len()
    }

    /// Compare lines, normalizing whitespace when `-b` is in effect.
    pub fn lines_equal(&self, my_index: usize, other: &FileData, other_index: usize) -> bool {
        let (mine, theirs) = (self.lines[my_index], other.lines[other_index]);
        if self.normalize_ws {
            normalized(mine).eq(normalized(theirs))
        } else {
            mine == theirs && self.line_terminated(my_index) == other.line_terminated(other_index)
        }
    }

    pub fn line_count(&self) -> usize {
        self.lines.len()
    }

    pub fn line(&self, index: usize) -> &'a [u8] {
        self.lines[index]
    }

    pub fn line_hash(&self, index: usize) -> u64 {
        self.hashes[index]
    }

    pub fn modified(&self) -> SystemTime {
        self.modified
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}

pub struct LineReader<'a> {
    content: &'a [u8],
    ends_with_newline: bool,
}

impl<'a> LineReader<'a> {
    pub fn new(content: &'a [u8]) -> Self {
        // An empty file has no incomplete final line, so it counts as
        // newline-terminated; treating it otherwise made `diff -e` refuse to
        // build an edit script from an empty file.
        let ends_with_newline = content.is_empty() || content.last() == Some(&b'\n');
        Self {
            content,
            ends_with_newline,
        }
    }
    pub fn ends_with_newline(&self) -> bool {
        self.ends_with_newline
    }
}

impl<'a> Iterator for LineReader<'a> {
    type Item = &'a [u8];

    /// Split on `\n` alone. A `\r` before the newline is part of the line's
    /// data, not part of the terminator: a CRLF file and an LF file are
    /// different files, and a patch generated from CRLF input has to carry the
    /// carriage returns or applying it silently rewrites every line ending.
    fn next(&mut self) -> Option<&'a [u8]> {
        if self.content.is_empty() {
            return None;
        }
        match self.content.iter().position(|&b| b == b'\n') {
            Some(i) => {
                let (line, rest) = self.content.split_at(i + 1);
                self.content = rest;
                Some(&line[..i])
            }
            // A final line with no newline is returned as it stands, trailing
            // `\r` included, which is what the terminated branch above does too.
            None => Some(take(&mut self.content)),
        }
    }
}
