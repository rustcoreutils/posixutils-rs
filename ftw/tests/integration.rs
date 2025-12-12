use std::{
    collections::HashSet,
    ffi::CString,
    fs, io,
    os::{fd::AsRawFd, unix},
    path::{Path, PathBuf},
};
use tempfile::TempDir;

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
        let test_dir = tempfile::Builder::new()
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
            if let Ok(new_tmp) = tempfile::tempdir() {
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
        if let Ok(new_tmp) = tempfile::tempdir() {
            let _ = std::mem::replace(&mut self.test_dir, new_tmp);
        }
    }
}

#[test]
fn test_ftw_simple() {
    let tmp_dir = tempfile::Builder::new()
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
        format!("{test_dir}"),
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
        |_| Ok(()),
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
        |_| Ok(()),
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
    let tmp_dir = tempfile::Builder::new()
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
        format!("{test_dir}"),
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
        |_| Ok(()),
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
        |_| Ok(()),
        |_, e| panic!("{}", e.inner()),
        ftw::TraverseDirectoryOpts::default(),
    );

    assert_eq!(count, DIR_HIERARCHY_DEPTH);
    // Cleanup happens automatically when cleanup is dropped
}

// Same as `test_ftw_deep` but using symlinks.
#[test]
fn test_ftw_deep_symlinks() {
    let tmp_dir = tempfile::Builder::new()
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
        |_| Ok(()),
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
    let tmp_dir = tempfile::Builder::new()
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
        |_| Ok(()),
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
        |_| Ok(()),
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
        |_| Ok(()),
        |_, e| panic!("{:?}", e.kind()),
        ftw::TraverseDirectoryOpts::default(),
    );

    for c in dummy_dirs.chars() {
        let filename = c.to_string();
        assert!(dirs.contains(&filename));
    }
    // Cleanup happens automatically when cleanup is dropped
}
