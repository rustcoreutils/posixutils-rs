use std::{
    collections::hash_map::DefaultHasher,
    fs::File,
    hash::{Hash, Hasher},
    io,
    mem::take,
    path::PathBuf,
    str::from_utf8,
    time::SystemTime,
};

use super::constants::COULD_NOT_UNWRAP_FILENAME;

/// Normalize whitespace for -b comparison:
/// - Collapse runs of whitespace to single space
/// - Trim trailing whitespace
fn normalize_whitespace(line: &str) -> String {
    let mut result = String::with_capacity(line.len());
    let mut prev_was_space = false;
    for c in line.chars() {
        if c.is_whitespace() {
            if !prev_was_space {
                result.push(' ');
                prev_was_space = true;
            }
        } else {
            result.push(c);
            prev_was_space = false;
        }
    }
    // Trim trailing whitespace
    result.truncate(result.trim_end().len());
    result
}

#[derive(Debug)]
pub struct FileData<'a> {
    path: PathBuf,
    lines: Vec<&'a str>,
    hashes: Vec<u64>, // Pre-computed line hashes for O(1) comparison
    modified: SystemTime,
    ends_with_newline: bool,
    normalize_ws: bool, // Whether whitespace normalization is enabled (-b flag)
}

impl<'a> FileData<'a> {
    pub fn ends_with_newline(&self) -> bool {
        self.ends_with_newline
    }

    pub fn get_file(
        path: PathBuf,
        lines: Vec<&'a str>,
        ends_with_newline: bool,
        normalize_ws: bool,
    ) -> io::Result<Self> {
        let file = File::open(&path)?;
        let modified = file.metadata()?.modified()?;

        // Pre-compute hashes for O(1) line comparison
        // If normalize_ws is set (-b flag), hash normalized lines for comparison
        // but store original lines for output
        let hashes: Vec<u64> = lines
            .iter()
            .map(|line| {
                let mut hasher = DefaultHasher::new();
                if normalize_ws {
                    normalize_whitespace(line).hash(&mut hasher);
                } else {
                    line.hash(&mut hasher);
                }
                hasher.finish()
            })
            .collect();

        Ok(Self {
            path,
            lines,
            hashes,
            modified,
            ends_with_newline,
            normalize_ws,
        })
    }

    /// Compare lines with normalization if enabled
    pub fn lines_equal(&self, my_index: usize, other: &FileData, other_index: usize) -> bool {
        if self.normalize_ws {
            // When -b is set, compare normalized lines
            normalize_whitespace(self.lines[my_index])
                == normalize_whitespace(other.lines[other_index])
        } else {
            self.lines[my_index] == other.lines[other_index]
        }
    }

    pub fn lines(&self) -> &Vec<&str> {
        &self.lines
    }

    pub fn line(&self, index: usize) -> &str {
        self.lines[index]
    }

    pub fn line_hash(&self, index: usize) -> u64 {
        self.hashes[index]
    }

    pub fn modified(&self) -> SystemTime {
        self.modified
    }

    pub fn name(&self) -> &str {
        if let Some(os_str) = self.path.file_name() {
            if let Some(str_slice) = os_str.to_str() {
                return str_slice;
            }
        }

        COULD_NOT_UNWRAP_FILENAME
    }

    pub fn path(&self) -> &str {
        self.path.to_str().unwrap_or(COULD_NOT_UNWRAP_FILENAME)
    }
}

pub struct LineReader<'a> {
    content: &'a [u8],
    ends_with_newline: bool,
}

impl<'a> LineReader<'a> {
    pub fn new(content: &'a [u8]) -> Self {
        let ends_with_newline = content.last() == Some(&b'\n');
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
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        let mut carriage = false;
        let mut iter = self.content.iter().enumerate();
        let mut line_len = loop {
            match iter.next() {
                Some((i, b'\n')) => break i + 1,
                None => {
                    return (!self.content.is_empty()).then(|| {
                        from_utf8(take(&mut self.content)).expect("Failed to convert to str")
                    });
                }
                Some((_, &it)) => carriage = it == b'\r',
            }
        };
        let (line, rest) = self.content.split_at(line_len);
        if carriage {
            line_len -= 1;
        }
        self.content = rest;
        Some(from_utf8(&line[..line_len - 1]).expect("Failed to convert to str"))
    }
}
