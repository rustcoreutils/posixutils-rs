use std::{
    collections::HashMap,
    ffi::OsString,
    fs::{self, DirEntry},
    io,
    path::{Display, Path},
};

pub struct DirData<'a> {
    path: &'a Path,
    files: HashMap<OsString, DirEntry>,
}

impl<'a> DirData<'a> {
    pub fn load(path: &'a Path) -> io::Result<Self> {
        let mut files: HashMap<OsString, DirEntry> = Default::default();

        let entries = fs::read_dir(path)?;

        for entry in entries {
            let entry = entry?;
            files.insert(entry.file_name(), entry);
        }

        Ok(Self { path, files })
    }

    pub fn files(&self) -> &HashMap<OsString, DirEntry> {
        &self.files
    }

    pub fn path(&'a self) -> &'a Path {
        self.path
    }

    pub fn path_str(&self) -> Display {
        self.path.display()
    }
}
