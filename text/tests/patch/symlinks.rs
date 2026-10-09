//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! A patch is untrusted input: every file it names is reached one component
//! at a time from the directory being patched, never through a symbolic link,
//! and only a regular file is changed. dpkg-source applies the patches of a
//! downloaded source package with patch, and the package's own tree may hold
//! links that point anywhere. Expected results are GNU patch 2.7.6's.

use super::{cleanup_test_dir, setup_test_dir};
use plib::testing::get_binary_path;
use std::fs;
use std::io::Read;
use std::os::unix::fs::{symlink, PermissionsExt};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

/// Longest a run may take; a patch that opens a FIFO and waits for a writer
/// would otherwise hang the test suite.
const RUN_LIMIT: Duration = Duration::from_secs(20);

/// A tree to patch, `tree/`, next to a directory the patch must never reach,
/// `outside/`, which holds one file, `outside/target`.
pub(super) struct Fixture {
    pub(super) root: PathBuf,
    pub(super) tree: PathBuf,
    pub(super) outside: PathBuf,
}

impl Fixture {
    pub(super) fn new(name: &str) -> Self {
        let root = setup_test_dir(name);
        let tree = root.join("tree");
        let outside = root.join("outside");
        fs::create_dir_all(&tree).unwrap();
        fs::create_dir_all(&outside).unwrap();
        fs::write(outside.join("target"), "target\n").unwrap();
        Fixture {
            root,
            tree,
            outside,
        }
    }

    /// Run patch with `-d tree`, reading `patch` from a file outside the
    /// tree; return the exit code and stderr. Fails the test if patch is
    /// still running after RUN_LIMIT.
    pub(super) fn run(&self, args: &[&str], patch: &str) -> (i32, String) {
        let patch_file = self.root.join("p.diff");
        fs::write(&patch_file, patch).unwrap();
        let mut child = Command::new(get_binary_path("patch"))
            .arg("-d")
            .arg(&self.tree)
            .args(args)
            .arg("-i")
            .arg(&patch_file)
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::piped())
            .spawn()
            .expect("spawn patch");
        let started = Instant::now();
        let status = loop {
            if let Some(status) = child.try_wait().unwrap() {
                break status;
            }
            if started.elapsed() > RUN_LIMIT {
                let _ = child.kill();
                let _ = child.wait();
                panic!("patch {:?} did not finish", args);
            }
            std::thread::sleep(Duration::from_millis(20));
        };
        let mut err = String::new();
        child
            .stderr
            .take()
            .unwrap()
            .read_to_string(&mut err)
            .unwrap();
        (status.code().unwrap_or(-1), err)
    }

    pub(super) fn in_tree(&self, rel: &str) -> PathBuf {
        self.tree.join(rel)
    }

    pub(super) fn link_to_outside(&self, rel: &str, dest: &str) {
        symlink(dest, self.in_tree(rel)).unwrap();
    }

    /// The names in `outside/`, sorted.
    fn outside_names(&self) -> Vec<String> {
        let mut names: Vec<String> = fs::read_dir(&self.outside)
            .unwrap()
            .map(|e| e.unwrap().file_name().to_string_lossy().into_owned())
            .collect();
        names.sort();
        names
    }

    /// `outside/` still holds exactly the file it started with.
    pub(super) fn assert_outside_untouched(&self) {
        assert_eq!(self.outside_names(), vec![String::from("target")]);
        assert_eq!(read(&self.outside.join("target")), "target\n");
    }
}

impl Drop for Fixture {
    fn drop(&mut self) {
        cleanup_test_dir(&self.root);
    }
}

fn read(path: &Path) -> String {
    fs::read_to_string(path).unwrap_or_else(|e| panic!("{}: {}", path.display(), e))
}

fn is_symlink(path: &Path) -> bool {
    fs::symlink_metadata(path)
        .map(|m| m.file_type().is_symlink())
        .unwrap_or(false)
}

fn is_regular(path: &Path) -> bool {
    fs::symlink_metadata(path)
        .map(|m| m.file_type().is_file())
        .unwrap_or(false)
}

const MODIFY_F: &str = "--- a/f\n+++ b/f\n@@ -1 +1 @@\n-target\n+pwned\n";
const FAIL_F: &str = "--- a/f\n+++ b/f\n@@ -1 +1 @@\n-nomatch\n+pwned\n";

// A directory component that is a link to outside the tree: a patch creating
// d/evil must not create outside/evil. GNU: "Invalid file name d/evil --
// skipping patch", exit 1.
#[test]
fn test_patch_symlinked_dir_new_file() {
    let fx = Fixture::new("sym_dir_new");
    fx.link_to_outside("d", "../outside");
    let (code, err) = fx.run(
        &["-p1", "-t"],
        "--- /dev/null\n+++ b/d/evil\n@@ -0,0 +1 @@\n+evil\n",
    );
    fx.assert_outside_untouched();
    assert!(
        err.contains("Invalid file name d/evil -- skipping patch"),
        "got: {}",
        err
    );
    assert_eq!(code, 1, "stderr: {}", err);
}

// A link to outside in a directory component of a file that "exists" through
// it: GNU does not find the file, and nothing outside changes.
#[test]
fn test_patch_symlinked_dir_existing_file() {
    let fx = Fixture::new("sym_dir_existing");
    fx.link_to_outside("d", "../outside");
    let (code, err) = fx.run(
        &["-p1", "-t"],
        "--- a/d/target\n+++ b/d/target\n@@ -1 +1 @@\n-target\n+pwned\n",
    );
    fx.assert_outside_untouched();
    assert_ne!(code, 0, "stderr: {}", err);
}

// Deleting a file through a linked directory removes nothing outside.
#[test]
fn test_patch_symlinked_dir_delete() {
    let fx = Fixture::new("sym_dir_delete");
    fx.link_to_outside("d", "../outside");
    let (code, err) = fx.run(
        &["-p1", "-t"],
        "--- a/d/target\n+++ /dev/null\n@@ -1 +0,0 @@\n-target\n",
    );
    fx.assert_outside_untouched();
    assert_ne!(code, 0, "stderr: {}", err);
}

// The file itself is a link to outside. GNU: "File f is not a regular file --
// refusing to patch", the hunks go to f.rej, exit 1.
#[test]
fn test_patch_symlinked_leaf() {
    let fx = Fixture::new("sym_leaf");
    fx.link_to_outside("f", "../outside/target");
    let (code, err) = fx.run(&["-p1", "-t"], MODIFY_F);
    fx.assert_outside_untouched();
    assert!(
        err.contains("File f is not a regular file -- refusing to patch"),
        "got: {}",
        err
    );
    assert_eq!(code, 1, "stderr: {}", err);
    assert!(is_symlink(&fx.in_tree("f")), "the link itself stays");
    assert!(read(&fx.in_tree("f.rej")).contains("+ pwned"));
}

// A link inside the tree is refused just the same: patch changes regular
// files, not whatever a link leads to.
#[test]
fn test_patch_symlinked_leaf_inside_tree() {
    let fx = Fixture::new("sym_leaf_inside");
    fs::write(fx.in_tree("g"), "target\n").unwrap();
    symlink("g", fx.in_tree("f")).unwrap();
    let (code, err) = fx.run(&["-p1", "-t"], MODIFY_F);
    assert!(err.contains("not a regular file"), "got: {}", err);
    assert_eq!(code, 1, "stderr: {}", err);
    assert_eq!(read(&fx.in_tree("g")), "target\n");
}

// A patch creating a file where a dangling link stands must not create the
// link's destination.
#[test]
fn test_patch_new_file_over_dangling_symlink() {
    let fx = Fixture::new("sym_leaf_dangling");
    fx.link_to_outside("f", "../outside/created");
    let (code, err) = fx.run(
        &["-p1", "-t"],
        "--- /dev/null\n+++ b/f\n@@ -0,0 +1 @@\n+new\n",
    );
    fx.assert_outside_untouched();
    assert!(err.contains("not a regular file"), "got: {}", err);
    assert_eq!(code, 1, "stderr: {}", err);
}

// A deletion patch whose file is a link removes neither the link nor what it
// points to.
#[test]
fn test_patch_delete_symlinked_leaf() {
    let fx = Fixture::new("sym_leaf_delete");
    fx.link_to_outside("f", "../outside/target");
    let (code, err) = fx.run(
        &["-p1", "-t"],
        "--- a/f\n+++ /dev/null\n@@ -1 +0,0 @@\n-target\n",
    );
    fx.assert_outside_untouched();
    assert!(err.contains("not a regular file"), "got: {}", err);
    assert_eq!(code, 1, "stderr: {}", err);
    assert!(is_symlink(&fx.in_tree("f")));
}

// The file operand is the user's own name, so a linked directory in it is
// followed (GNU does too) -- but the file itself must still be regular.
#[test]
fn test_patch_operand_symlinked_leaf() {
    let fx = Fixture::new("sym_operand_leaf");
    fx.link_to_outside("f", "../outside/target");
    let (code, err) = fx.run(&["-t", "f"], MODIFY_F);
    fx.assert_outside_untouched();
    assert!(err.contains("not a regular file"), "got: {}", err);
    assert_eq!(code, 1, "stderr: {}", err);
}

// A FIFO standing where the file should be: refused at once, never opened for
// a blocking read or write.
#[test]
fn test_patch_fifo_leaf() {
    let fx = Fixture::new("fifo_leaf");
    mkfifo(&fx.in_tree("f"));
    let (code, err) = fx.run(&["-p1", "-t"], MODIFY_F);
    assert!(
        err.contains("File f is not a regular file -- refusing to patch"),
        "got: {}",
        err
    );
    assert_eq!(code, 1, "stderr: {}", err);
}

// The reject file's name is a link to outside: the link is replaced by a
// regular file, nothing is written through it.
#[test]
fn test_patch_symlinked_reject_file() {
    let fx = Fixture::new("sym_rej");
    fs::write(fx.in_tree("f"), "target\n").unwrap();
    fx.link_to_outside("f.rej", "../outside/rej");
    let (code, err) = fx.run(&["-p1", "-t"], FAIL_F);
    assert_eq!(code, 1, "stderr: {}", err);
    fx.assert_outside_untouched();
    assert!(is_regular(&fx.in_tree("f.rej")));
    assert!(read(&fx.in_tree("f.rej")).contains("+ pwned"));
}

// A FIFO where the reject file goes is replaced, not opened.
#[test]
fn test_patch_fifo_reject_file() {
    let fx = Fixture::new("fifo_rej");
    fs::write(fx.in_tree("f"), "target\n").unwrap();
    mkfifo(&fx.in_tree("f.rej"));
    let (code, err) = fx.run(&["-p1", "-t"], FAIL_F);
    assert_eq!(code, 1, "stderr: {}", err);
    assert!(is_regular(&fx.in_tree("f.rej")));
}

// The backup's name is a link to outside, dangling or not: GNU replaces the
// link with the backup.
#[test]
fn test_patch_symlinked_backup() {
    for dest in ["../outside/orig", "../outside/target"] {
        let fx = Fixture::new("sym_backup");
        fs::write(fx.in_tree("f"), "target\n").unwrap();
        fx.link_to_outside("f.orig", dest);
        let (code, err) = fx.run(&["-p1", "-t", "-b"], MODIFY_F);
        assert_eq!(code, 0, "stderr: {}", err);
        fx.assert_outside_untouched();
        assert!(is_regular(&fx.in_tree("f.orig")), "{}", dest);
        assert_eq!(read(&fx.in_tree("f.orig")), "target\n");
        assert_eq!(read(&fx.in_tree("f")), "pwned\n");
    }
}

// A -B prefix whose directory is a link to outside, as a source package could
// ship .pc: GNU refuses to make the backup and leaves the file alone.
#[test]
fn test_patch_symlinked_backup_prefix_dir() {
    let fx = Fixture::new("sym_backup_prefix");
    fs::write(fx.in_tree("f"), "target\n").unwrap();
    fx.link_to_outside(".pc", "../outside");
    let (code, err) = fx.run(&["-p1", "-t", "-b", "-B", ".pc/x/"], MODIFY_F);
    fx.assert_outside_untouched();
    assert_eq!(code, 2, "stderr: {}", err);
    assert_eq!(read(&fx.in_tree("f")), "target\n");
}

// A patched file keeps its permission bits (it is written to a new file
// that replaces the old one, so they must be carried over).
#[test]
fn test_patch_preserves_mode() {
    let fx = Fixture::new("keeps_mode");
    let f = fx.in_tree("f");
    fs::write(&f, "target\n").unwrap();
    fs::set_permissions(&f, fs::Permissions::from_mode(0o751)).unwrap();
    let (code, err) = fx.run(&["-p1", "-t"], MODIFY_F);
    assert_eq!(code, 0, "stderr: {}", err);
    assert_eq!(read(&f), "pwned\n");
    let mode = fs::metadata(&f).unwrap().permissions().mode() & 0o7777;
    assert_eq!(mode, 0o751);
}

// A patched file is replaced, not rewritten in place: a hard link to the old
// file keeps the old text, as with GNU patch.
#[test]
fn test_patch_replaces_file() {
    let fx = Fixture::new("replaces_file");
    let f = fx.in_tree("f");
    fs::write(&f, "target\n").unwrap();
    fs::hard_link(&f, fx.in_tree("other")).unwrap();
    let (code, err) = fx.run(&["-p1", "-t"], MODIFY_F);
    assert_eq!(code, 0, "stderr: {}", err);
    assert_eq!(read(&f), "pwned\n");
    assert_eq!(read(&fx.in_tree("other")), "target\n");
}

/// A group of the caller's other than its effective one, if it has any.
fn other_group() -> Option<libc::gid_t> {
    // SAFETY: a zero-length query only returns the count.
    let n = unsafe { libc::getgroups(0, std::ptr::null_mut()) };
    let mut groups = vec![0 as libc::gid_t; usize::try_from(n).ok()?];
    // SAFETY: `groups` has room for `n` entries.
    let n = unsafe { libc::getgroups(n, groups.as_mut_ptr()) };
    groups.truncate(usize::try_from(n).ok()?);
    // SAFETY: getegid cannot fail.
    let egid = unsafe { libc::getegid() };
    groups.into_iter().find(|&g| g != egid)
}

// A patched file keeps its group, and with it its set-group-ID bit, when the
// caller belongs to that group, as GNU patch does for a non-root caller.
#[test]
fn test_patch_preserves_group() {
    use std::os::unix::fs::MetadataExt;
    let Some(gid) = other_group() else {
        eprintln!("the caller belongs to only one group; nothing to carry over");
        return;
    };
    let fx = Fixture::new("keeps_group");
    let f = fx.in_tree("f");
    fs::write(&f, "target\n").unwrap();
    std::os::unix::fs::chown(&f, None, Some(gid)).unwrap();
    fs::set_permissions(&f, fs::Permissions::from_mode(0o2751)).unwrap();
    let (code, err) = fx.run(&["-p1", "-t"], MODIFY_F);
    assert_eq!(code, 0, "stderr: {}", err);
    assert_eq!(read(&f), "pwned\n");
    let meta = fs::metadata(&f).unwrap();
    assert_eq!(meta.gid(), gid);
    assert_eq!(meta.mode() & 0o7777, 0o2751);
}

// -o names the user's own output file; a link there is followed, as GNU does.
#[test]
fn test_patch_output_file_follows_link() {
    let fx = Fixture::new("output_link");
    fs::write(fx.in_tree("f"), "target\n").unwrap();
    fx.link_to_outside("out", "../outside/out");
    let (code, err) = fx.run(&["-p1", "-t", "-o", "out"], MODIFY_F);
    assert_eq!(code, 0, "stderr: {}", err);
    assert_eq!(read(&fx.outside.join("out")), "pwned\n");
    assert_eq!(read(&fx.in_tree("f")), "target\n");
}

fn mkfifo(path: &Path) {
    use std::os::unix::ffi::OsStrExt;
    let name = std::ffi::CString::new(path.as_os_str().as_bytes()).unwrap();
    // SAFETY: `name` is a valid NUL-terminated C string.
    let rc = unsafe { libc::mkfifo(name.as_ptr(), 0o644) };
    assert_eq!(rc, 0, "mkfifo {}", path.display());
}
