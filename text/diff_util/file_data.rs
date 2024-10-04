use std::{
    fs::File,
    io,
    mem::take,
    path::PathBuf,
    str::from_utf8,
    time::SystemTime,
};

use super::constants::COULD_NOT_UNWRAP_FILENAME;

#[derive(Debug)]
pub struct FileData<'a> {
    path: PathBuf,
    lines: Vec<&'a str>,
    modified: SystemTime,
    ends_with_newline: bool,
}

impl<'a> FileData<'a> {
    pub fn ends_with_newline(&self) -> bool {
        self.ends_with_newline
    }

    pub fn get_file(path: PathBuf, lines: Vec<&'a str>, ends_with_newline: bool) -> io::Result<Self> {
        let file = File::open(&path)?;
        let modified = file.metadata()?.modified()?;

        Ok(Self {
            path,
            lines,
            modified,
            ends_with_newline,
        })
    }

    pub fn lines(&self) -> &Vec<&str> {
        &self.lines
    }

    pub fn line(&self, index: usize) -> &str {
        &self.lines[index]
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

        return COULD_NOT_UNWRAP_FILENAME;
    }

    pub fn path(&self) -> &str {
        self.path.to_str().unwrap_or(&COULD_NOT_UNWRAP_FILENAME)
    }
}

pub struct LineReader<'a> {
    content: &'a [u8],
    ends_with_newline: bool,
}

impl<'a> LineReader<'a> {
    pub fn new(content: &'a [u8]) -> Self {
        let ends_with_newline = content.last() == Some(&b'\n');
        Self { content, ends_with_newline }
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
