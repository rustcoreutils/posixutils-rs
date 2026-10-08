//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Race-free access to the files a patch names.
//!
//! A patch is untrusted input, and so is the tree it is applied to: a source
//! package unpacked by dpkg-source may hold a symbolic link where the patch
//! expects a directory or a file. Every name taken from the patch is therefore
//! reached one component at a time from the directory being patched, each
//! directory opened relative to the one before it and never through a link,
//! and the directory is then held open while its file is read, replaced or
//! removed. Only a regular file is ever patched; a new version is written to a
//! fresh file beside it and renamed over the name, so a link standing at the
//! name is replaced, never written through.
//!
//! A name the user gave (the file operand, an absolute -B prefix) reaches its
//! directory the ordinary way, links and all, as any utility's operand does;
//! the last component is held to the same rules.

use super::types::BackupName;
use std::ffi::{OsStr, OsString};
use std::fs::{File, Metadata};
use std::io::{self, BufWriter, Read, Write};
use std::path::{Component, Path, PathBuf};

#[cfg(unix)]
#[path = "safe_fs_unix.rs"]
mod sys;

#[cfg(not(unix))]
#[path = "safe_fs_other.rs"]
mod sys;

pub use sys::open_user_output;

/// Why a name was not used.
#[derive(Debug)]
pub enum Refusal {
    /// A directory on the way to the file is a symbolic link.
    InvalidName,
    /// The file is a symbolic link, a FIFO, a device -- anything but a
    /// regular file.
    NotRegular,
    Io(io::Error),
}

impl From<io::Error> for Refusal {
    fn from(e: io::Error) -> Self {
        Refusal::Io(e)
    }
}

impl Refusal {
    /// The refusal as an I/O error naming `name`, for a caller that reports
    /// errors that way.
    pub fn into_io(self, name: &Path) -> io::Error {
        match self {
            Refusal::InvalidName => {
                io::Error::other(format!("Invalid file name {}", name.display()))
            }
            Refusal::NotRegular => {
                io::Error::other(format!("File {} is not a regular file", name.display()))
            }
            Refusal::Io(e) => e,
        }
    }
}

/// Who chose a name: the patch, or the user.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Origin {
    Patch,
    User,
}

/// A file name, split into the part reached the ordinary way and the part
/// walked one component at a time.
#[derive(Debug, Clone)]
pub struct Name {
    /// Directory opened by its path, following links; None is the working
    /// directory.
    base: Option<PathBuf>,
    /// Walked from `base`, never through a link. Its last component is the
    /// file.
    walk: PathBuf,
    /// The whole name, for messages and for deriving other names.
    shown: PathBuf,
    origin: Origin,
}

impl Name {
    /// A name the patch gave, already checked to be relative and free of
    /// `..`: walked in full from the working directory.
    pub fn from_patch(path: PathBuf) -> Self {
        Name {
            base: None,
            walk: path.clone(),
            shown: path,
            origin: Origin::Patch,
        }
    }

    /// A name the user gave: its directory is theirs to choose.
    pub fn from_user(path: PathBuf) -> Self {
        let base = path
            .parent()
            .filter(|p| !p.as_os_str().is_empty())
            .map(Path::to_path_buf);
        let walk = match path.file_name() {
            Some(leaf) => PathBuf::from(leaf),
            // "/" or "..": no file to name. Walking it is refused.
            None => path.clone(),
        };
        Name {
            base,
            walk,
            shown: path,
            origin: Origin::User,
        }
    }

    pub fn path(&self) -> &Path {
        &self.shown
    }

    /// This name with `suffix` appended, as for its reject file.
    pub fn with_suffix(&self, suffix: &str) -> Self {
        Name {
            base: self.base.clone(),
            walk: super::bytes::with_suffix(&self.walk, suffix),
            shown: super::bytes::with_suffix(&self.shown, suffix),
            origin: self.origin,
        }
    }

    /// The name of this file's backup. A -B prefix that is absolute is the
    /// user's own directory; everything after its last slash, like the whole
    /// of a relative backup name, is walked.
    pub fn backup(&self, naming: &BackupName) -> Self {
        let full = naming.for_file(&self.shown);
        if self.origin == Origin::User {
            return Name::from_user(full);
        }
        let Some(dir) = naming.absolute_prefix_dir() else {
            return Name::from_patch(full);
        };
        // A name that does not strip is walked whole, and its root refused.
        let walk = full
            .strip_prefix(&dir)
            .map(Path::to_path_buf)
            .unwrap_or_else(|_| full.clone());
        Name {
            base: Some(dir),
            walk,
            shown: full,
            origin: Origin::Patch,
        }
    }

    /// The walked components: the directories, and the file.
    fn split(&self) -> Result<(Vec<Component<'_>>, &OsStr), Refusal> {
        let mut parts: Vec<Component> = self.walk.components().collect();
        match parts.pop() {
            Some(Component::Normal(leaf)) => Ok((parts, leaf)),
            _ => Err(Refusal::InvalidName),
        }
    }
}

/// What patch read of a file before changing it.
pub struct Original {
    pub bytes: Vec<u8>,
    pub meta: Metadata,
}

/// A held directory and a name in it.
pub struct Place {
    dir: sys::Dir,
    leaf: OsString,
}

/// Open `name`'s base directory, creating it first if asked.
fn open_base(name: &Name, create: bool) -> io::Result<sys::Dir> {
    match &name.base {
        None => sys::open_cwd(),
        Some(base) => {
            if create {
                std::fs::create_dir_all(base)?;
            }
            sys::open_dir(base)
        }
    }
}

/// Classify a failure to open `name` in `dir` as a directory.
fn step_failure(dir: &sys::Dir, name: &OsStr, err: io::Error) -> Refusal {
    match sys::lstat(dir, name) {
        Ok(Some(sys::Kind::Symlink)) => Refusal::InvalidName,
        _ => Refusal::Io(err),
    }
}

/// Descend from `dir` through `parts`, one held directory at a time, never
/// through a link. A missing directory is made when `create` is set, and
/// otherwise ends the walk with None.
fn walk_dirs(
    mut dir: sys::Dir,
    parts: &[Component],
    create: bool,
) -> Result<Option<sys::Dir>, Refusal> {
    for part in parts {
        let name = match part {
            Component::CurDir => continue,
            Component::ParentDir => OsStr::new(".."),
            Component::Normal(name) => name,
            Component::RootDir | Component::Prefix(_) => return Err(Refusal::InvalidName),
        };
        dir = match sys::open_subdir(&dir, name) {
            Ok(next) => next,
            Err(e) if e.kind() == io::ErrorKind::NotFound => {
                if !create {
                    return Ok(None);
                }
                sys::make_subdir(&dir, name).map_err(|e| step_failure(&dir, name, e))?
            }
            Err(e) => return Err(step_failure(&dir, name, e)),
        };
    }
    Ok(Some(dir))
}

impl Place {
    /// Reach the directory `name` lives in, creating missing directories
    /// when `create` is set. None means a directory is missing.
    pub fn locate(name: &Name, create: bool) -> Result<Option<Place>, Refusal> {
        let (parts, leaf) = name.split()?;
        let base = open_base(name, create)?;
        Ok(walk_dirs(base, &parts, create)?.map(|dir| Place {
            dir,
            leaf: leaf.to_os_string(),
        }))
    }

    /// Whether anything at all stands at the name.
    pub fn exists(&self) -> bool {
        matches!(sys::lstat(&self.dir, &self.leaf), Ok(Some(_)))
    }

    /// Read the file, which must be a regular file. None if there is none.
    pub fn read_regular(&self) -> Result<Option<Original>, Refusal> {
        match sys::lstat(&self.dir, &self.leaf)? {
            None => return Ok(None),
            Some(sys::Kind::Regular) => {}
            Some(_) => return Err(Refusal::NotRegular),
        }
        // Between the lstat and the open the name may have been replaced;
        // the open does not follow a link nor wait on a FIFO, and what it
        // opened is checked again.
        let mut file = sys::open_read(&self.dir, &self.leaf).map_err(|e| {
            match sys::lstat(&self.dir, &self.leaf) {
                Ok(Some(kind)) if kind != sys::Kind::Regular => Refusal::NotRegular,
                _ => Refusal::Io(e),
            }
        })?;
        let meta = file.metadata()?;
        if !meta.is_file() {
            return Err(Refusal::NotRegular);
        }
        let mut bytes = Vec::new();
        file.read_to_end(&mut bytes)?;
        Ok(Some(Original { bytes, meta }))
    }

    /// Replace whatever stands at the name with a new regular file holding
    /// what `fill` writes, carrying over the owner and mode of `like`. The
    /// file is made under a temporary name in the same directory and renamed
    /// over the name, so a link at the name is replaced, never followed.
    pub fn replace(
        &self,
        like: Option<&Metadata>,
        fill: impl FnOnce(&mut dyn Write) -> io::Result<()>,
    ) -> io::Result<()> {
        let (file, temp) = sys::create_temp(&self.dir, like.is_some())?;
        let result = fill_and_rename(self, file, &temp, like, fill);
        if result.is_err() {
            let _ = sys::unlink(&self.dir, &temp);
        }
        result
    }

    /// Append to the file, which must already be a regular file.
    pub fn append(&self, bytes: &[u8]) -> Result<(), Refusal> {
        let mut file = sys::open_append(&self.dir, &self.leaf)?;
        if !file.metadata()?.is_file() {
            return Err(Refusal::NotRegular);
        }
        file.write_all(bytes)?;
        Ok(())
    }

    /// Remove the file, provided it is still the one `original` was read
    /// from.
    pub fn remove(&self, original: &Original) -> io::Result<()> {
        if !sys::is_same_file(&self.dir, &self.leaf, &original.meta)? {
            return Err(io::Error::other("file changed while patching"));
        }
        sys::unlink(&self.dir, &self.leaf)
    }
}

/// Write the temporary file, give it its owner and mode, and rename it over
/// the place's name.
fn fill_and_rename(
    place: &Place,
    file: File,
    temp: &OsStr,
    like: Option<&Metadata>,
    fill: impl FnOnce(&mut dyn Write) -> io::Result<()>,
) -> io::Result<()> {
    let mut writer = BufWriter::new(&file);
    fill(&mut writer)?;
    writer.flush()?;
    drop(writer);
    if let Some(meta) = like {
        sys::set_owner_and_mode(&file, meta)?;
    }
    sys::rename(&place.dir, temp, &place.leaf)
}

/// Remove the directories a removed patch-named file leaves empty, innermost
/// first, as GNU patch does; stop at the first that is not empty. Each is
/// reached by the same walk, so a link put in place of one is not followed.
pub fn prune_empty_dirs(name: &Name) {
    if name.origin != Origin::Patch {
        return;
    }
    let Ok((parts, _)) = name.split() else {
        return;
    };
    if !parts.iter().all(|c| matches!(c, Component::Normal(_))) {
        return;
    }
    for depth in (1..=parts.len()).rev() {
        let Component::Normal(dir_name) = parts[depth - 1] else {
            return;
        };
        let Ok(base) = open_base(name, false) else {
            return;
        };
        let Ok(Some(parent)) = walk_dirs(base, &parts[..depth - 1], false) else {
            return;
        };
        if sys::rmdir(&parent, dir_name).is_err() {
            return;
        }
    }
}
