//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::{
    collections::HashMap,
    ffi::OsString,
    fs::{self, DirEntry},
    io,
    path::PathBuf,
};

use super::{constants::*, functions::io_error_at};

pub struct DirData {
    path: PathBuf,
    files: HashMap<OsString, DirEntry>,
}

impl DirData {
    pub fn load(path: PathBuf) -> io::Result<Self> {
        let mut files: HashMap<OsString, DirEntry> = Default::default();

        // Errors carry the directory they came from: a caller holding two
        // directories cannot otherwise tell which of them it could not read.
        let entries = fs::read_dir(&path).map_err(|e| io_error_at(&path, e))?;

        for entry in entries {
            let entry = entry.map_err(|e| io_error_at(&path, e))?;
            files.insert(entry.file_name(), entry);
        }

        Ok(Self { path, files })
    }

    pub fn files(&self) -> &HashMap<OsString, DirEntry> {
        &self.files
    }

    pub fn path(&self) -> &PathBuf {
        &self.path
    }

    pub fn path_str(&self) -> &str {
        self.path.to_str().unwrap_or(COULD_NOT_UNWRAP_FILENAME)
    }
}
