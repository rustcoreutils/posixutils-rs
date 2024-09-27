use std::{
    collections::HashSet,
    ffi::CString,
    fs, io,
    os::{fd::AsRawFd, unix},
    path::{Path, PathBuf},
};

const DIR_HIERARCHY_DEPTH: usize = 300;

#[test]
fn test_ftw_simple() {
    let test_dir = &format!("{}/test_ftw_simple", env!("CARGO_TARGET_TMPDIR"));
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

    fs::remove_dir_all(test_dir).unwrap();
}

// Test if symlinks are properly followed.
#[test]
fn test_ftw_symlinks() {
    let test_dir = &format!("{}/test_ftw_symlinks", env!("CARGO_TARGET_TMPDIR"));
    fs::create_dir(test_dir).unwrap();

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

    fs::remove_dir_all(test_dir).unwrap();
}

// Must be able to navigate arbitrarily deep hierarchies.
#[test]
fn test_ftw_deep() {
    let test_dir = &format!("{}/test_ftw_deep", env!("CARGO_TARGET_TMPDIR"));

    let test_dir_name = CString::new(test_dir.as_bytes()).unwrap();
    let dir_name = CString::new(["x"; 200].join("").as_bytes()).unwrap();

    let mut fd = ftw::FileDescriptor::cwd();

    for i in 0..DIR_HIERARCHY_DEPTH {
        let filename = if i == 0 { &test_dir_name } else { &dir_name };
        let ret = unsafe { libc::mkdirat(fd.as_raw_fd(), filename.as_ptr(), 0o755) };
        if ret != 0 {
            panic!("{}", io::Error::last_os_error());
        }

        fd = ftw::FileDescriptor::open_at(&fd, filename, libc::O_RDONLY).unwrap();
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

    fs::remove_dir_all(test_dir).unwrap();
}

// Same as `test_ftw_deep` but using symlinks.
#[test]
fn test_ftw_deep_symlinks() {
    let test_dir = &format!("{}/test_ftw_deep_symlinks", env!("CARGO_TARGET_TMPDIR"));
    fs::create_dir(test_dir).unwrap();

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

    fs::remove_dir_all(test_dir).unwrap();
}

// Tests the resilience against making the search go to a different directory by modifying the path.
#[test]
fn test_ftw_path_prefix_modification() {
    let test_dir = &format!(
        "{}/test_ftw_path_prefix_modification",
        env!("CARGO_TARGET_TMPDIR")
    );
    let correct_dir = format!("{test_dir}/correct_dir");
    let wrong_dir = format!("{test_dir}/wrong_dir");
    let a_b = format!("{test_dir}/a/b");
    let a_b_c = format!("{test_dir}/a/b/c");

    fs::create_dir(test_dir).unwrap();

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

    fs::remove_dir_all(test_dir).unwrap();
}

// Tests if `traverse_directory` can open filenames longer than `libc::PATH_MAX`
#[test]
fn test_ftw_long_filename() {
    let test_dir = &format!("{}/test_ftw_long_filename", env!("CARGO_TARGET_TMPDIR"));
    let dummy_dirs = "abcde";

    let test_dir_name = CString::new(test_dir.as_bytes()).unwrap();
    let dir_name = CString::new(["x"; 200].join("").as_bytes()).unwrap();

    let mut fd = ftw::FileDescriptor::cwd();

    for i in 0..DIR_HIERARCHY_DEPTH {
        let filename = if i == 0 { &test_dir_name } else { &dir_name };
        let ret = unsafe { libc::mkdirat(fd.as_raw_fd(), filename.as_ptr(), 0o755) };
        if ret != 0 {
            panic!("{}", io::Error::last_os_error());
        }

        fd = ftw::FileDescriptor::open_at(&fd, filename, libc::O_RDONLY).unwrap();

        // If at the last index, add dummy directories
        if i == DIR_HIERARCHY_DEPTH - 1 {
            for char in dummy_dirs.chars() {
                let filename = CString::new(char.to_string().as_bytes()).unwrap();
                let ret = unsafe { libc::mkdirat(fd.as_raw_fd(), filename.as_ptr(), 0o755) };
                if ret != 0 {
                    panic!("{}", io::Error::last_os_error());
                }
            }
        }
    }

    let mut nested_dir = PathBuf::from(test_dir);
    nested_dir.push(test_dir_name.to_string_lossy().to_string());
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

    for char in dummy_dirs.chars() {
        let filename = char.to_string();
        assert!(dirs.contains(&filename));
    }

    fs::remove_dir_all(test_dir).unwrap();
}
