//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::tmp::TempDir;
use std::{
    collections::HashSet,
    ffi::CString,
    fs, io,
    os::{fd::AsRawFd, unix},
    path::{Path, PathBuf},
};

const DIR_HIERARCHY_DEPTH: usize = 300;

/// Helper struct that cleans up deep directory hierarchies on drop.
/// This is needed because `fs::remove_dir_all()` can fail on very deep hierarchies
/// and we need cleanup to happen even if the test panics.
struct DeepDirCleanup {
    test_dir: TempDir,
    dir_name: CString,
    depth: usize,
}

impl DeepDirCleanup {
    fn new(prefix: &str, dir_name_str: &str, depth: usize) -> io::Result<Self> {
        let test_dir = plib::tmp::Builder::new()
            .prefix(prefix)
            .tempdir_in(env!("CARGO_TARGET_TMPDIR"))?;
        let dir_name = CString::new(dir_name_str.as_bytes()).unwrap();
        Ok(Self {
            test_dir,
            dir_name,
            depth,
        })
    }

    fn path(&self) -> &Path {
        self.test_dir.path()
    }
}

impl Drop for DeepDirCleanup {
    fn drop(&mut self) {
        // Try standard removal first (works for shallow hierarchies)
        if fs::remove_dir_all(self.test_dir.path()).is_ok() {
            // Prevent TempDir from trying to remove again
            if let Ok(new_tmp) = plib::tmp::tempdir() {
                let _ = std::mem::replace(&mut self.test_dir, new_tmp);
            }
            return;
        }

        // Fall back to manual unlinkat() approach for deep hierarchies.
        // The structure is: test_dir/x/x/x/.../x (depth-1 nested 'x' directories inside test_dir)
        //
        // To remove, we navigate to the second-to-last directory (depth-2),
        // then remove from the deepest working back up.
        let test_dir_name =
            CString::new(self.test_dir.path().to_str().unwrap().as_bytes()).unwrap();

        let mut fd = ftw::FileDescriptor::cwd();

        // Navigate down to depth-2 (the parent of the deepest directory).
        for i in 0..(self.depth.saturating_sub(2)) {
            let filename = if i == 0 {
                &test_dir_name
            } else {
                &self.dir_name
            };
            if let Ok(new_fd) = ftw::FileDescriptor::open_at(&fd, filename, libc::O_RDONLY) {
                fd = new_fd;
            } else {
                // Can't descend further - might already be cleaned up
                return;
            }
        }

        // Now fd is at depth-2. Remove directories from deepest to shallowest.
        for remaining in (0..self.depth).rev() {
            let (filename, raw_fd) = if remaining == 0 {
                // Unlinking test_dir itself from cwd
                (&test_dir_name, libc::AT_FDCWD)
            } else {
                // Unlinking an 'x' directory from fd
                (&self.dir_name, fd.as_raw_fd())
            };

            unsafe {
                libc::unlinkat(raw_fd, filename.as_ptr(), libc::AT_REMOVEDIR);
            }

            // Move fd up one level (except when we just unlinked test_dir)
            if remaining > 1 {
                if let Ok(parent_fd) = ftw::FileDescriptor::open_at(&fd, c"..", libc::O_RDONLY) {
                    fd = parent_fd;
                } else {
                    break;
                }
            }
        }

        // Prevent TempDir from trying to remove (we already did, or at least tried)
        if let Ok(new_tmp) = plib::tmp::tempdir() {
            let _ = std::mem::replace(&mut self.test_dir, new_tmp);
        }
    }
}

#[test]
fn test_ftw_simple() {
    let tmp_dir = plib::tmp::Builder::new()
        .prefix("test_ftw_simple")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .unwrap();
    let test_dir = tmp_dir.path().to_str().unwrap();

    let a_1 = format!("{test_dir}/a/1");
    let a_2 = format!("{test_dir}/a/2");
    let b_1 = format!("{test_dir}/b/1");
    let b_2 = format!("{test_dir}/b/2");

    for dir in [&a_1, &a_2, &b_1, &b_2] {
        fs::create_dir_all(dir).unwrap();
    }

    let mut expected_filenames = [
        test_dir.to_string(),
        format!("{test_dir}/b"),
        format!("{test_dir}/b/1"),
        format!("{test_dir}/b/2"),
        format!("{test_dir}/a"),
        format!("{test_dir}/a/1"),
        format!("{test_dir}/a/2"),
    ];

    let mut filenames = Vec::new();

    ftw::traverse_directory(
        test_dir,
        |entry| {
            let s = format!("{}", entry.path());
            filenames.push(s);
            Ok(true)
        },
        |_, _| Ok(()),
        |_, e| panic!("{}", e.inner()),
        ftw::TraverseDirectoryOpts::default(),
    );

    expected_filenames.sort();
    filenames.sort();
    assert_eq!(expected_filenames.as_slice(), filenames.as_slice());

    // Test listing files like in ls
    filenames.clear();
    ftw::traverse_directory(
        test_dir,
        |entry| {
            let s = format!("{}", entry.path());
            filenames.push(s);
            Ok(true)
        },
        |_, _| Ok(()),
        |_, e| panic!("{}", e.inner()),
        ftw::TraverseDirectoryOpts {
            list_contents_first: true,
            ..Default::default()
        },
    );
    filenames.sort();
    assert_eq!(expected_filenames.as_slice(), filenames.as_slice());
    // Cleanup happens automatically when tmp_dir is dropped
}

// Test if symlinks are properly followed.
#[test]
fn test_ftw_symlinks() {
    let tmp_dir = plib::tmp::Builder::new()
        .prefix("test_ftw_symlinks")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .unwrap();
    let test_dir = tmp_dir.path().to_str().unwrap();

    let mut prev = None;
    for i in 0..3 {
        let dir_name = format!("{test_dir}/{i}");
        fs::create_dir(&dir_name).unwrap();

        if let Some(prev_name) = &prev {
            unix::fs::symlink(&dir_name, format!("{prev_name}/symlink")).unwrap();
        }

        prev = Some(dir_name);
    }

    // test_dir
    // 0 -> 1 -> 2
    // 1 -> 2
    // 2
    // Filenames below should be the same as what `ls` should produce
    let mut expected_filenames = [
        test_dir.to_string(),
        format!("{test_dir}/0"),
        format!("{test_dir}/0/symlink"),
        format!("{test_dir}/0/symlink/symlink"),
        format!("{test_dir}/1"),
        format!("{test_dir}/1/symlink"),
        format!("{test_dir}/2"),
    ];

    let mut filenames = Vec::new();

    ftw::traverse_directory(
        test_dir,
        |entry| {
            let s = format!("{}", entry.path());
            filenames.push(s);
            Ok(true)
        },
        |_, _| Ok(()),
        |_, e| panic!("{}", e.inner()),
        ftw::TraverseDirectoryOpts {
            follow_symlinks: true,
            ..Default::default()
        },
    );

    expected_filenames.sort();
    filenames.sort();
    assert_eq!(expected_filenames.as_slice(), filenames.as_slice());
    // Cleanup happens automatically when tmp_dir is dropped
}

// Must be able to navigate arbitrarily deep hierarchies.
#[test]
fn test_ftw_deep() {
    let dir_name_str = ["x"; 200].join("");
    let cleanup = DeepDirCleanup::new("test_ftw_deep", &dir_name_str, DIR_HIERARCHY_DEPTH).unwrap();
    let test_dir = cleanup.path().to_str().unwrap();

    let dir_name = CString::new(dir_name_str.as_bytes()).unwrap();

    let mut fd = ftw::FileDescriptor::open_at(
        &ftw::FileDescriptor::cwd(),
        &CString::new(test_dir.as_bytes()).unwrap(),
        libc::O_RDONLY,
    )
    .unwrap();

    // Create nested directories inside the temp dir (starting at depth 1)
    for _ in 1..DIR_HIERARCHY_DEPTH {
        let ret = unsafe { libc::mkdirat(fd.as_raw_fd(), dir_name.as_ptr(), 0o755) };
        if ret != 0 {
            panic!("{}", io::Error::last_os_error());
        }

        fd = ftw::FileDescriptor::open_at(&fd, &dir_name, libc::O_RDONLY).unwrap();
    }

    let mut count = 0;

    ftw::traverse_directory(
        test_dir,
        |_| {
            count += 1;
            Ok(true)
        },
        |_, _| Ok(()),
        |_, e| panic!("{}", e.inner()),
        ftw::TraverseDirectoryOpts::default(),
    );

    assert_eq!(count, DIR_HIERARCHY_DEPTH);
    // Cleanup happens automatically when cleanup is dropped
}

// Same as `test_ftw_deep` but using symlinks.
#[test]
fn test_ftw_deep_symlinks() {
    let tmp_dir = plib::tmp::Builder::new()
        .prefix("test_ftw_deep_symlinks")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .unwrap();
    let test_dir = tmp_dir.path().to_str().unwrap();

    let mut prev = None;
    for i in 0..DIR_HIERARCHY_DEPTH {
        let dir_name_abs = format!("{test_dir}/{i}");
        fs::create_dir(&dir_name_abs).unwrap();

        if let Some(prev_name) = &prev {
            let dir_name_rel = format!("../{i}");

            // Must be able to handle both relative and absolute symlinks
            let dir_name = if i % 2 == 0 {
                &dir_name_abs
            } else {
                &dir_name_rel
            };

            unix::fs::symlink(dir_name, format!("{prev_name}/symlink")).unwrap();
        }

        prev = Some(dir_name_abs);
    }

    let mut count = 0;

    ftw::traverse_directory(
        format!("{test_dir}/0"), // Follow the symlink chain starting at test_dir/0
        |_| {
            count += 1;
            Ok(true)
        },
        |_, _| Ok(()),
        |_, e| panic!("{}", e.inner()),
        ftw::TraverseDirectoryOpts {
            follow_symlinks: true,
            ..Default::default()
        },
    );

    assert_eq!(count, DIR_HIERARCHY_DEPTH);
    // Cleanup happens automatically when tmp_dir is dropped
}

// Tests the resilience against making the search go to a different directory by modifying the path.
#[test]
fn test_ftw_path_prefix_modification() {
    let tmp_dir = plib::tmp::Builder::new()
        .prefix("test_ftw_path_prefix_modification")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .unwrap();
    let test_dir = tmp_dir.path().to_str().unwrap();

    let correct_dir = format!("{test_dir}/correct_dir");
    let wrong_dir = format!("{test_dir}/wrong_dir");
    let a_b = format!("{test_dir}/a/b");
    let a_b_c = format!("{test_dir}/a/b/c");

    fs::create_dir_all(format!("{correct_dir}/c/d/e/f/g/h/i/j")).unwrap();
    fs::create_dir_all(format!("{wrong_dir}/x/x/x/x/x/x/x/x")).unwrap();

    // a/b -> correct_dir
    fs::create_dir(format!("{test_dir}/a")).unwrap();
    unix::fs::symlink(&correct_dir, &a_b).unwrap();

    let mut filenames = Vec::new();

    // Modifying a/b should not make the directory traversal at a/b/c move to a/b/x
    ftw::traverse_directory(
        &a_b_c,
        |entry| {
            let path = entry.path();
            let filename = path.file_name().unwrap();
            filenames.push(filename.to_str().unwrap().to_owned());

            // Symbolic link to a different directory
            // a/new -> wrong_dir
            let new_symlink = format!("{test_dir}/new");
            unix::fs::symlink(&wrong_dir, &new_symlink).unwrap();

            // Overwrite a/b with a/new
            fs::rename(&new_symlink, &a_b).unwrap();

            // a/b now points to wrong_dir
            assert_eq!(fs::read_link(&a_b).unwrap(), Path::new(&wrong_dir));

            Ok(true)
        },
        |_, _| Ok(()),
        |entry, e| {
            // a/b/c is indeed removed so ignore this error
            if e.kind() == ftw::ErrorKind::Open {
                let path = format!("{}", entry.path().display());
                if path == a_b_c {
                    return;
                }
            }

            panic!("{}", e.inner());
        },
        ftw::TraverseDirectoryOpts {
            follow_symlinks_on_args: true,
            follow_symlinks: true,
            ..Default::default()
        },
    );

    // Once a/b is reached, traverse_directory should not go down the wrong directory even if a/b is
    // changed
    assert!(!filenames.contains(&String::from("x")));

    filenames.clear();
    ftw::traverse_directory(
        &a_b,
        |entry| {
            let path = entry.path();
            let filename = path.file_name().unwrap();
            filenames.push(filename.to_str().unwrap().to_owned());
            Ok(true)
        },
        |_, _| Ok(()),
        |_, e| panic!("{}", e.inner()),
        ftw::TraverseDirectoryOpts {
            follow_symlinks_on_args: true,
            follow_symlinks: true,
            ..Default::default()
        },
    );

    // Rerunning the directory traversal should now follow the "wrong" directory.
    assert_eq!(filenames.iter().filter(|f| f.as_str() == "x").count(), 8);
    // Cleanup happens automatically when tmp_dir is dropped
}

// Tests if `traverse_directory` can open filenames longer than `libc::PATH_MAX`
#[test]
fn test_ftw_long_filename() {
    let dummy_dirs = "abcde";
    let dir_name_str = ["x"; 200].join("");

    let cleanup =
        DeepDirCleanup::new("test_ftw_long_filename", &dir_name_str, DIR_HIERARCHY_DEPTH).unwrap();
    let test_dir = cleanup.path().to_str().unwrap();

    let dir_name = CString::new(dir_name_str.as_bytes()).unwrap();

    let mut fd = ftw::FileDescriptor::open_at(
        &ftw::FileDescriptor::cwd(),
        &CString::new(test_dir.as_bytes()).unwrap(),
        libc::O_RDONLY,
    )
    .unwrap();

    // Create nested directories inside the temp dir (starting at depth 1)
    for i in 1..DIR_HIERARCHY_DEPTH {
        let ret = unsafe { libc::mkdirat(fd.as_raw_fd(), dir_name.as_ptr(), 0o755) };
        if ret != 0 {
            panic!("{}", io::Error::last_os_error());
        }

        fd = ftw::FileDescriptor::open_at(&fd, &dir_name, libc::O_RDONLY).unwrap();

        // If at the last index, add dummy directories
        if i == DIR_HIERARCHY_DEPTH - 1 {
            for c in dummy_dirs.chars() {
                let filename = CString::new(c.to_string().as_bytes()).unwrap();
                let ret = unsafe { libc::mkdirat(fd.as_raw_fd(), filename.as_ptr(), 0o755) };
                if ret != 0 {
                    panic!("{}", io::Error::last_os_error());
                }
            }
        }
    }

    let mut nested_dir = PathBuf::from(test_dir);
    for _ in 1..DIR_HIERARCHY_DEPTH {
        nested_dir.push(dir_name.to_string_lossy().to_string());
    }

    let mut dirs = HashSet::new();

    ftw::traverse_directory(
        nested_dir,
        |entry| {
            dirs.insert(entry.file_name().to_string_lossy().to_string());
            Ok(true)
        },
        |_, _| Ok(()),
        |_, e| panic!("{:?}", e.kind()),
        ftw::TraverseDirectoryOpts::default(),
    );

    for c in dummy_dirs.chars() {
        let filename = c.to_string();
        assert!(dirs.contains(&filename));
    }
    // Cleanup happens automatically when cleanup is dropped
}

/// Forces the descriptor-conserving (`DeferredDir`) strategy from the first level, without
/// touching the process-wide `RLIMIT_NOFILE`.
fn conserving_fds_opts() -> ftw::TraverseDirectoryOpts {
    ftw::TraverseDirectoryOpts {
        caller_fds_per_level: 4096,
        ..Default::default()
    }
}

/// A conserving walk reopens each directory and filters the entries it already yielded. Keying
/// that filter on the inode instead of the name dropped the second of two hard links to one file,
/// and every mount point past the first (all roots are inode 2), silently losing files.
#[test]
fn conserving_walk_yields_every_hard_link() {
    let tmp_dir = plib::tmp::Builder::new()
        .prefix("conserving_walk_yields_every_hard_link")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .unwrap();
    let root = tmp_dir.path();

    // A subdirectory, so the walk has somewhere to descend and must reopen `sub` afterwards.
    let sub = root.join("sub");
    fs::create_dir(&sub).unwrap();
    fs::write(sub.join("plain"), b"x").unwrap();
    fs::write(sub.join("link_a"), b"y").unwrap();
    fs::hard_link(sub.join("link_a"), sub.join("link_b")).unwrap();
    fs::create_dir(sub.join("deeper")).unwrap();
    fs::write(sub.join("deeper/inner"), b"z").unwrap();

    let mut seen: Vec<String> = Vec::new();
    ftw::traverse_directory(
        root,
        |entry| {
            // Skip the operand itself, which is reported relative to the current directory.
            if entry.dir_fd() != libc::AT_FDCWD {
                seen.push(entry.file_name().to_string_lossy().to_string());
            }
            Ok(true)
        },
        |_, _| Ok(()),
        |entry, e| panic!("unexpected error on {}: {:?}", entry.path(), e.kind()),
        conserving_fds_opts(),
    );

    seen.sort();
    assert_eq!(
        seen,
        ["deeper", "inner", "link_a", "link_b", "plain", "sub"],
        "a conserving walk must visit every name exactly once"
    );
}

/// A failing `readdir` in the directory named on the command line is reported against that
/// directory and ends the walk. The reporter used to index `path_stack` two components back,
/// which underflows at depth 1, and the loop used to `continue` -- but `readdir` keeps returning
/// the same error, so it never terminated.
///
/// Whether the sabotage below produces a `readdir` failure at all is platform-dependent, so the
/// assertion about it is made only where it can be provoked; the walk must terminate everywhere.
#[test]
fn readdir_error_on_root_reports_once_and_terminates() {
    let tmp_dir = plib::tmp::Builder::new()
        .prefix("readdir_error_on_root")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .unwrap();
    let root = tmp_dir.path();

    // Several entries so at least one more `readdir` follows the one that succeeded.
    for name in ["a", "b", "c", "d"] {
        fs::write(root.join(name), b"x").unwrap();
    }

    // Replace the directory's descriptor with one referring to a non-directory. `dup2` rather
    // than `close` keeps the descriptor number allocated: closing it would let another thread in
    // this test binary reuse the number.
    //
    // On Linux the next `getdents` on that descriptor fails with ENOTDIR, which is the error path
    // under test. On macOS the entries the C library has already buffered simply drain and the
    // stream ends -- reading the empty scratch file returns end-of-file rather than an error -- so
    // no `readdir` failure is produced there.
    let scratch = fs::File::create(tmp_dir.path().join("scratch")).unwrap();
    let mut sabotaged = false;
    let mut errors: Vec<(String, ftw::ErrorKind)> = Vec::new();

    ftw::traverse_directory(
        root,
        |entry| {
            if !sabotaged && entry.dir_fd() != libc::AT_FDCWD {
                sabotaged = true;
                assert_ne!(
                    unsafe { libc::dup2(scratch.as_raw_fd(), entry.dir_fd()) },
                    -1
                );
            }
            Ok(true)
        },
        |_, _| Ok(()),
        |entry, e| errors.push((entry.path().to_string(), e.kind())),
        ftw::TraverseDirectoryOpts::default(),
    );

    assert!(sabotaged, "the walk never entered the root directory");

    // Reaching here at all is the guard against the non-terminating loop, on every platform.

    // Entries already buffered by the C library are still handed back, and their `fstatat` now
    // fails against the replaced descriptor; those reports are collateral of the sabotage. What
    // matters is the `readdir` failure itself: reported once, and naming the root rather than
    // panicking on a path stack too short to index.
    let readdir_errors: Vec<&String> = errors
        .iter()
        .filter(|(_, kind)| *kind == ftw::ErrorKind::ReadDir)
        .map(|(path, _)| path)
        .collect();

    assert!(
        readdir_errors.len() <= 1,
        "the failing directory was reported more than once: {errors:?}"
    );
    for path in &readdir_errors {
        assert_eq!(
            **path,
            root.to_string_lossy(),
            "the report must name the directory that could not be read"
        );
    }

    #[cfg(target_os = "linux")]
    assert_eq!(
        readdir_errors.len(),
        1,
        "expected a readdir report, got {errors:?}"
    );
}

/// Counts `file_handler` returning `Ok(true)` for a directory against `postprocess_dir` calls.
/// A caller that establishes per-directory state on `Ok(true)` unwinds it in `postprocess_dir`,
/// so the two must match however the walk turns out.
fn assert_enter_exit_balanced(
    root: &Path,
    opts: ftw::TraverseDirectoryOpts,
) -> Vec<ftw::ErrorKind> {
    use std::cell::{Cell, RefCell};

    let depth = Cell::new(0i64);
    let entered = Cell::new(0usize);
    let exited = Cell::new(0usize);
    let errors = RefCell::new(Vec::new());

    ftw::traverse_directory(
        root,
        |entry| {
            if entry.metadata().map(|md| md.is_dir()).unwrap_or(false) {
                depth.set(depth.get() + 1);
                entered.set(entered.get() + 1);
            }
            Ok(true)
        },
        |_, _| {
            depth.set(depth.get() - 1);
            exited.set(exited.get() + 1);
            assert!(
                depth.get() >= 0,
                "postprocess_dir called without a matching enter"
            );
            Ok(())
        },
        |_, e| errors.borrow_mut().push(e.kind()),
        opts,
    );

    assert_eq!(
        entered.get(),
        exited.get(),
        "{} directories entered but {} exited",
        entered.get(),
        exited.get()
    );
    assert_eq!(
        depth.get(),
        0,
        "traversal ended {} levels deep",
        depth.get()
    );
    errors.into_inner()
}

/// An unreadable subdirectory must not unbalance the callbacks for everything that follows it.
#[test]
fn enter_exit_balanced_when_descent_refused() {
    if unsafe { libc::geteuid() } == 0 {
        eprintln!("Skipping test: root can descend into a mode-0 directory");
        return;
    }

    let tmp_dir = plib::tmp::Builder::new()
        .prefix("enter_exit_balanced_when_descent_refused")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .unwrap();
    let root = tmp_dir.path();

    // Sorted before the siblings, so the imbalance would affect everything after it.
    let locked = root.join("aaa_locked");
    fs::create_dir(&locked).unwrap();
    fs::write(locked.join("child"), b"x").unwrap();
    fs::create_dir(root.join("zzz_other")).unwrap();
    fs::write(root.join("zzz_file"), b"x").unwrap();

    fs::set_permissions(&locked, std::os::unix::fs::PermissionsExt::from_mode(0o000)).unwrap();

    let errors = assert_enter_exit_balanced(root, ftw::TraverseDirectoryOpts::default());

    // Restore before the temp dir is removed.
    fs::set_permissions(&locked, std::os::unix::fs::PermissionsExt::from_mode(0o755)).unwrap();

    assert_eq!(
        errors,
        [ftw::ErrorKind::Open],
        "the refusal must be reported, with the errno the open actually returned"
    );
}

/// The same, for a symbolic link loop, which is refused after the handler has already been told
/// to descend.
#[test]
fn enter_exit_balanced_on_symlink_loop() {
    let tmp_dir = plib::tmp::Builder::new()
        .prefix("enter_exit_balanced_on_symlink_loop")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .unwrap();
    let root = tmp_dir.path();

    let sub = root.join("sub");
    fs::create_dir(&sub).unwrap();
    unix::fs::symlink("..", sub.join("loop")).unwrap();
    fs::write(root.join("after"), b"x").unwrap();

    let errors = assert_enter_exit_balanced(
        root,
        ftw::TraverseDirectoryOpts {
            follow_symlinks: true,
            ..Default::default()
        },
    );

    assert_eq!(errors, [ftw::ErrorKind::Stat], "expected an ELOOP report");
}

/// A directory that is readable but not searchable can still be enumerated: `opendir` needs read
/// permission, not search permission. ftw used to probe the mode bits itself and refuse it.
#[test]
fn readable_but_not_searchable_dir_is_enumerated() {
    if unsafe { libc::geteuid() } == 0 {
        eprintln!("Skipping test: root is not subject to the mode bits under test");
        return;
    }

    let tmp_dir = plib::tmp::Builder::new()
        .prefix("readable_but_not_searchable_dir")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .unwrap();
    let root = tmp_dir.path();

    let sub = root.join("sub");
    fs::create_dir(&sub).unwrap();
    fs::write(sub.join("visible"), b"x").unwrap();
    fs::set_permissions(&sub, std::os::unix::fs::PermissionsExt::from_mode(0o600)).unwrap();

    let mut names = Vec::new();
    let mut errors = Vec::new();
    ftw::traverse_directory(
        root,
        |entry| {
            names.push(entry.file_name().to_string_lossy().to_string());
            Ok(true)
        },
        |_, _| Ok(()),
        |entry, e| errors.push((entry.path().to_string(), e.kind())),
        ftw::TraverseDirectoryOpts::default(),
    );

    fs::set_permissions(&sub, std::os::unix::fs::PermissionsExt::from_mode(0o755)).unwrap();

    assert!(
        names.iter().any(|n| n == "sub"),
        "the directory itself was never handed to the handler: {names:?}"
    );

    // `readdir` needs read permission and succeeds, so the walk reaches the child and fails on
    // *its* `fstatat`, which needs search permission. Previously ftw probed the mode bits before
    // descending and refused the whole directory with a single EACCES, never reading it at all.
    assert_eq!(
        errors
            .iter()
            .filter(|(path, kind)| path.ends_with("/visible") && *kind == ftw::ErrorKind::Stat)
            .count(),
        1,
        "expected the walk to enumerate the directory and fail on the child: {errors:?}"
    );
}
