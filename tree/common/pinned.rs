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

/// How an operand is reached.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Anchor {
    /// Through a descriptor for the directory holding it, held since it was pinned.
    Held,
    /// By its whole pathname, from the working directory: the directory holding it could not be
    /// opened (EACCES -- write and search permission without read, where there is no lookup-only
    /// open). Enough for a rename, which is atomic; never for a move across filesystems, whose
    /// copy and removal must not resolve the pathname again.
    Path,
}

/// An operand's last component, looked up in a directory held open since the operand was
/// pinned: renaming or replacing any directory on the way to it afterwards changes nothing about
/// which directory is searched. (Unless its anchor is `Anchor::Path`.)
pub struct PinnedEntry {
    dir: Rc<ftw::FileDescriptor>,
    /// The last component, with any trailing slashes the operand had: the kernel looks it up in
    /// `dir` exactly as it would have looked up the whole operand. Under `Anchor::Path`, the
    /// whole operand.
    name: CString,
    /// What precedes `name`, for diagnostics only.
    display_parent: PathBuf,
    path: PathBuf,
    anchor: Anchor,
}

impl PinnedEntry {
    /// The operand reached by its whole pathname (`Anchor::Path`).
    fn by_path(path: &Path) -> io::Result<Self> {
        Ok(PinnedEntry {
            dir: Rc::new(ftw::FileDescriptor::cwd()),
            name: CString::new(path.as_os_str().as_bytes())
                .map_err(|_| io::Error::from_raw_os_error(libc::EINVAL))?,
            display_parent: PathBuf::new(),
            path: path.to_path_buf(),
            anchor: Anchor::Path,
        })
    }

    pub fn anchor(&self) -> Anchor {
        self.anchor
    }

    pub fn dir(&self) -> &ftw::FileDescriptor {
        &self.dir
    }

    /// The directory, shared.
    pub fn dir_rc(&self) -> Rc<ftw::FileDescriptor> {
        Rc::clone(&self.dir)
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
    /// wrote it (symbolic links included). A directory that may not be opened (EACCES) leaves
    /// the operand reached by pathname (`Anchor::Path`).
    pub fn pin(&mut self, path: &Path) -> io::Result<PinnedEntry> {
        let (parent, name) = split_last_component(path.as_os_str().as_bytes());
        let invalid = |_| io::Error::from_raw_os_error(libc::EINVAL);
        let dir = match open_lookup_dir(&CString::new(parent.unwrap_or(b".")).map_err(invalid)?) {
            Ok(dir) => dir,
            Err(e) if e.raw_os_error() == Some(libc::EACCES) => return PinnedEntry::by_path(path),
            Err(e) => return Err(e),
        };
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
            anchor: Anchor::Held,
        })
    }
}

/// A directory operand, opened once, that entries are pinned in by name: `mv a b dir` resolves
/// `dir` once, so replacing it partway through the move changes nothing about where the later
/// operands go. (Unless it may not be opened: then `None`, and its entries are reached by
/// pathname, `Anchor::Path`.)
pub struct PinnedDir {
    dir: Option<Rc<ftw::FileDescriptor>>,
    path: PathBuf,
}

impl PinnedDir {
    /// Open the directory `path` names, as the user wrote it (symbolic links included).
    pub fn open(path: &Path) -> io::Result<Self> {
        let path_cstr = CString::new(path.as_os_str().as_bytes())
            .map_err(|_| io::Error::from_raw_os_error(libc::EINVAL))?;
        let dir = match open_lookup_dir(&path_cstr) {
            Ok(dir) => Some(Rc::new(dir)),
            Err(e) if e.raw_os_error() == Some(libc::EACCES) => None,
            Err(e) => return Err(e),
        };
        Ok(PinnedDir {
            dir,
            path: path.to_path_buf(),
        })
    }

    /// The entry `name` (a single component) in this directory.
    pub fn entry(&self, name: &std::ffi::OsStr) -> io::Result<PinnedEntry> {
        if name.as_bytes().contains(&b'/') {
            return Err(io::Error::from_raw_os_error(libc::EINVAL));
        }
        let Some(dir) = &self.dir else {
            return PinnedEntry::by_path(&self.path.join(name));
        };
        Ok(PinnedEntry {
            dir: Rc::clone(dir),
            name: CString::new(name.as_bytes())
                .map_err(|_| io::Error::from_raw_os_error(libc::EINVAL))?,
            display_parent: self.path.clone(),
            path: self.path.join(name),
            anchor: Anchor::Held,
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
/// needs no read permission (`rename` needs none either); on macOS with `O_SEARCH`, or for
/// reading where the system does not know `O_SEARCH` (EINVAL, ENOTSUP); elsewhere for reading.
#[cfg(target_os = "linux")]
fn open_lookup_dir(path: &CStr) -> io::Result<ftw::FileDescriptor> {
    let flags = libc::O_PATH | libc::O_DIRECTORY | libc::O_CLOEXEC;
    ftw::FileDescriptor::open_at(&ftw::FileDescriptor::cwd(), path, flags)
}

#[cfg(target_vendor = "apple")]
fn open_lookup_dir(path: &CStr) -> io::Result<ftw::FileDescriptor> {
    let cwd = ftw::FileDescriptor::cwd();
    let flags = libc::O_DIRECTORY | libc::O_CLOEXEC;
    match ftw::FileDescriptor::open_at(&cwd, path, flags | libc::O_SEARCH) {
        Err(e) if matches!(e.raw_os_error(), Some(libc::EINVAL) | Some(libc::ENOTSUP)) => {
            ftw::FileDescriptor::open_at(&cwd, path, flags | libc::O_RDONLY)
        }
        opened => opened,
    }
}

#[cfg(not(any(target_os = "linux", target_vendor = "apple")))]
fn open_lookup_dir(path: &CStr) -> io::Result<ftw::FileDescriptor> {
    let flags = libc::O_RDONLY | libc::O_DIRECTORY | libc::O_CLOEXEC;
    ftw::FileDescriptor::open_at(&ftw::FileDescriptor::cwd(), path, flags)
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
    use super::{split_last_component, Anchor, PinnedDir, PinnedDirs};

    /// A directory that may not be opened leaves the operand reached by its pathname, for a
    /// rename to report on as it always did, instead of failing the pin.
    #[test]
    fn an_unopenable_directory_leaves_the_operand_reached_by_path() {
        use std::os::unix::fs::PermissionsExt;

        if unsafe { libc::geteuid() } == 0 {
            return; // Permissions do not bind the superuser.
        }
        let dir = std::env::temp_dir().join(format!("pinned_by_path_{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(dir.join("closed/sub")).unwrap();
        std::fs::set_permissions(dir.join("closed"), std::fs::Permissions::from_mode(0o000))
            .unwrap();

        let entry = PinnedDirs::default().pin(&dir.join("closed/sub/x"));
        let dir_entry =
            PinnedDir::open(&dir.join("closed/sub")).and_then(|d| d.entry("x".as_ref()));

        std::fs::set_permissions(dir.join("closed"), std::fs::Permissions::from_mode(0o700))
            .unwrap();
        let _ = std::fs::remove_dir_all(&dir);
        for entry in [entry.unwrap(), dir_entry.unwrap()] {
            assert_eq!(entry.anchor(), Anchor::Path);
            assert_eq!(entry.dir_fd(), libc::AT_FDCWD);
            assert_eq!(
                entry.name().to_bytes(),
                entry.path().as_os_str().as_encoded_bytes()
            );
        }
    }

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
