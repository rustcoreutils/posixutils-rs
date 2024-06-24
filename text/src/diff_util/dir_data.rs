use std::{
    collections::HashMap,
    ffi::OsString,
    fs::{self, DirEntry},
    io,
    path::PathBuf,
};

use super::constants::*;

pub struct DirData {
    path: PathBuf,
    files: HashMap<OsString, DirEntry>,
}

impl DirData {
    pub fn load(path: PathBuf) -> io::Result<DirData> {
        let mut dir_data = DirData {
            path: path,
            files: Default::default(),
        };
        let entries = fs::read_dir(&dir_data.path)?;

        for entry in entries {
            let entry = entry?;

            dir_data.files.insert(entry.file_name(), entry);
        }

        Ok(dir_data)
    }

    pub fn files(&self) -> &HashMap<OsString, DirEntry> {
        &self.files
    }

    pub fn path(&self) -> &PathBuf {
        &self.path
    }

    pub fn path_str(&self) -> &str {
        &self.path.to_str().unwrap_or(COULD_NOT_UNWRAP_FILENAME)
    }
}
