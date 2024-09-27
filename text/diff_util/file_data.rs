use std::{
    fs::File,
    io::{self, BufReader, Read},
    path::PathBuf,
    time::SystemTime,
};

use super::{change::Change, constants::COULD_NOT_UNWRAP_FILENAME};

pub struct FileData {
    path: PathBuf,
    lines: Vec<String>,
    changes: Vec<Change>,
    modified: SystemTime,
    ends_with_newline: bool,
}

impl FileData {
    pub fn ends_with_newline(&self) -> bool {
        self.ends_with_newline
    }

    pub fn get_file(path: PathBuf) -> io::Result<Self> {
        let file = File::open(path.clone())?;
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
            path,
            lines,
            changes,
            modified,
            ends_with_newline,
        };

        Ok(result)
    }

    pub fn get_context_identifier(&self, change_index: usize) -> &str {
        match self.changes[change_index] {
            Change::None => " ",
            Change::Unchanged(_) => " ",
            Change::Insert(_) => "+",
            Change::Delete(_) => "-",
            Change::Substitute(_) => "!",
        }
    }

    pub fn lines(&self) -> &Vec<String> {
        &self.lines
    }

    pub fn line(&self, index: usize) -> &String {
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

        COULD_NOT_UNWRAP_FILENAME
    }

    pub fn set_change(&mut self, change: Change, index: usize) {
        self.changes[index] = change;
    }

    pub fn expected_changed_in_range(
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

        false
    }

    pub fn change(&self, index: usize) -> &Change {
        &self.changes[index]
    }

    pub fn path(&self) -> &str {
        self.path.to_str().unwrap_or(&COULD_NOT_UNWRAP_FILENAME)
    }
}
