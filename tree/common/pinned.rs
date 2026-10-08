//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Operands pinned by a descriptor for the directory that holds them, and the record of what a
//! cross-filesystem move copied -- what `mv` needs to act on the files it checked and copied,
//! rather than on whatever their pathnames lead to by the time it acts.

use ftw::{self, FileType};
use std::{
    collections::HashMap,
    ffi::{CStr, CString},
    io,
    os::unix::{ffi::OsStrExt, fs::MetadataExt},
    path::{Path, PathBuf},
    rc::{Rc, Weak},
};

/// An operand's last component, looked up in a directory held open since the operand was
/// pinned: renaming or replacing any directory on the way to it afterwards changes nothing about
/// which directory is searched.
pub struct PinnedEntry {
    dir: Rc<ftw::FileDescriptor>,
    /// The last component, with any trailing slashes the operand had: the kernel looks it up in
    /// `dir` exactly as it would have looked up the whole operand.
    name: CString,
    /// What precedes `name` in the operand, for diagnostics only.
    display_parent: PathBuf,
    path: PathBuf,
}

impl PinnedEntry {
    pub fn dir(&self) -> &ftw::FileDescriptor {
        &self.dir
    }

    pub fn dir_fd(&self) -> libc::c_int {
        std::os::fd::AsRawFd::as_raw_fd(&*self.dir)
    }

    pub fn name(&self) -> &CStr {
        &self.name
    }

    /// The operand as the user wrote it.
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// The operand without its last component, as the user wrote it (empty when it has none).
    pub fn display_parent(&self) -> &Path {
        &self.display_parent
    }

    /// `fstatat` of the entry in its pinned directory.
    pub fn metadata(&self, follow_symlinks: bool) -> io::Result<ftw::Metadata> {
        ftw::Metadata::new(self.dir_fd(), &self.name, follow_symlinks)
    }
}

/// The directories operands have been pinned in, shared among the operands that are in the
/// same one: `mv dir/* elsewhere` holds one descriptor, not one per operand. Only directories
/// some `PinnedEntry` still holds are kept.
#[derive(Default)]
pub struct PinnedDirs(HashMap<(u64, u64), Weak<ftw::FileDescriptor>>);

impl PinnedDirs {
    /// Pin `path`: open the directory its pathname names, up to its last component, as the user
    /// wrote it (symbolic links included).
    pub fn pin(&mut self, path: &Path) -> io::Result<PinnedEntry> {
        let (parent, name) = split_last_component(path.as_os_str().as_bytes());
        let invalid = |_| io::Error::from_raw_os_error(libc::EINVAL);
        let dir = open_lookup_dir(&CString::new(parent.unwrap_or(b".")).map_err(invalid)?)?;
        let md = ftw::Metadata::new(std::os::fd::AsRawFd::as_raw_fd(&dir), c".", false)?;
        let identity = (md.dev(), md.ino());
        let dir = match self.0.get(&identity).and_then(Weak::upgrade) {
            Some(held) => held,
            None => {
                let dir = Rc::new(dir);
                self.0.retain(|_, held| held.strong_count() > 0);
                self.0.insert(identity, Rc::downgrade(&dir));
                dir
            }
        };
        Ok(PinnedEntry {
            dir,
            name: CString::new(name).map_err(invalid)?,
            display_parent: PathBuf::from(std::ffi::OsStr::from_bytes(parent.unwrap_or(b""))),
            path: path.to_path_buf(),
        })
    }
}

/// `path` split before its last component: the part before it (`None` when there is none) and
/// the component itself, trailing slashes included. A path of slashes only is the root
/// directory's `.`, and an empty path is an empty name in the current directory -- both looked
/// up as the kernel would look up the whole path.
fn split_last_component(path: &[u8]) -> (Option<&[u8]>, &[u8]) {
    let Some(last) = path.iter().rposition(|&b| b != b'/') else {
        return if path.is_empty() {
            (None, b"")
        } else {
            (Some(b"/"), b".")
        };
    };
    match path[..last].iter().rposition(|&b| b == b'/') {
        Some(slash) => (Some(&path[..=slash]), &path[slash + 1..]),
        None => (None, path),
    }
}

/// Open a directory for looking names up in it, and nothing else: on Linux with `O_PATH`, which
/// needs no read permission (`rename` needs none either); on macOS with `O_SEARCH` where the
/// system has it, else for reading.
fn open_lookup_dir(path: &CStr) -> io::Result<ftw::FileDescriptor> {
    let cwd = ftw::FileDescriptor::cwd();
    let flags = libc::O_DIRECTORY | libc::O_CLOEXEC;
    #[cfg(target_os = "linux")]
    let lookup_only = Some(libc::O_PATH);
    #[cfg(target_vendor = "apple")]
    let lookup_only = Some(libc::O_SEARCH);
    #[cfg(not(any(target_os = "linux", target_vendor = "apple")))]
    let lookup_only: Option<libc::c_int> = None;
    if let Some(lookup_only) = lookup_only {
        if let Ok(fd) = ftw::FileDescriptor::open_at(&cwd, path, flags | lookup_only) {
            return Ok(fd);
        }
    }
    ftw::FileDescriptor::open_at(&cwd, path, flags | libc::O_RDONLY)
}

/// What a source file was when the copy duplicated it.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct CopiedState {
    file_type: FileType,
    /// Size and modification time, for a non-directory: a write after the copy read it changes
    /// them. (A directory's change as entries are removed from it, and its entries are judged
    /// one by one.)
    contents: Option<(u64, i64, i64)>,
}

impl CopiedState {
    fn of(md: &ftw::Metadata) -> Self {
        let file_type = md.file_type();
        let contents =
            (file_type != FileType::Directory).then(|| (md.size(), md.mtime(), md.mtime_nsec()));
        CopiedState {
            file_type,
            contents,
        }
    }
}

/// Every source file a cross-filesystem move duplicated, by identity: after the copy, `mv`
/// removes these and nothing else.
#[derive(Default)]
pub struct CopiedSources(HashMap<(u64, u64), CopiedState>);

impl CopiedSources {
    /// Record a source file, as the walk saw it, that has been duplicated.
    pub fn record(&mut self, md: &ftw::Metadata) {
        self.0.insert((md.dev(), md.ino()), CopiedState::of(md));
    }

    /// Whether `md` is a file the copy duplicated, unchanged since: the same file, of the same
    /// type, and for a non-directory with the same size and modification time.
    pub fn unchanged(&self, md: &ftw::Metadata) -> bool {
        self.0.get(&(md.dev(), md.ino())) == Some(&CopiedState::of(md))
    }
}

#[cfg(test)]
mod tests {
    use super::split_last_component;

    #[test]
    fn splits_before_the_last_component() {
        let split = |p: &str| {
            let (parent, name) = split_last_component(p.as_bytes());
            (
                parent.map(|p| String::from_utf8(p.to_vec()).unwrap()),
                String::from_utf8(name.to_vec()).unwrap(),
            )
        };
        let some = |p: &str| Some(p.to_string());
        assert_eq!(split("f"), (None, "f".into()));
        assert_eq!(split("a/b/f"), (some("a/b/"), "f".into()));
        assert_eq!(split("/f"), (some("/"), "f".into()));
        assert_eq!(split("a//d//"), (some("a//"), "d//".into()));
        assert_eq!(split("a/."), (some("a/"), ".".into()));
        assert_eq!(split("a/.."), (some("a/"), "..".into()));
        assert_eq!(split("//"), (some("/"), ".".into()));
        assert_eq!(split(""), (None, "".into()));
    }
}
