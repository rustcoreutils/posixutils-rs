use std::{ffi::CString, fs, io, os::unix};

#[test]
fn test_walkdir_simple() {
    let test_dir = &format!("{}/test_walkdir_simple", env!("CARGO_TARGET_TMPDIR"));
    let a_1 = format!("{test_dir}/a/1");
    let a_2 = format!("{test_dir}/a/2");
    let b_1 = format!("{test_dir}/b/1");
    let b_2 = format!("{test_dir}/b/2");

    for dir in [&a_1, &a_2, &b_1, &b_2] {
        std::fs::create_dir_all(dir).unwrap();
    }

    let mut filenames = [
        format!("{test_dir}"),
        format!("{test_dir}/b"),
        format!("{test_dir}/b/1"),
        format!("{test_dir}/b/2"),
        format!("{test_dir}/a"),
        format!("{test_dir}/a/1"),
        format!("{test_dir}/a/2"),
    ];

    let mut resulting_filenames = Vec::new();

    plib::walkdir::traverse_directory(
        test_dir,
        |entry| {
            let s = format!("{}", entry.path().display());
            resulting_filenames.push(s);
            true
        },
        |_| {},
        |_, _| {},
        false,
        false,
    );

    filenames.sort();
    resulting_filenames.sort();
    assert_eq!(filenames.as_slice(), resulting_filenames.as_slice());

    std::fs::remove_dir_all(test_dir).unwrap();
}

// Test if symlinks are properly followed.
#[test]
fn test_walkdir_symlinks() {
    let test_dir = &format!("{}/test_walkdir_symlinks", env!("CARGO_TARGET_TMPDIR"));
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
    let mut filenames = [
        format!("{test_dir}"),
        format!("{test_dir}/0"),
        format!("{test_dir}/1"),
        format!("{test_dir}/2"),
        format!("{test_dir}/1"),
        format!("{test_dir}/2"),
        format!("{test_dir}/2"),
    ];

    let mut resulting_filenames = Vec::new();

    plib::walkdir::traverse_directory(
        test_dir,
        |entry| {
            let s = format!("{}", entry.path().display());
            resulting_filenames.push(s);
            true
        },
        |_| {},
        |_, _| {},
        false,
        true,
    );

    filenames.sort();
    resulting_filenames.sort();
    assert_eq!(filenames.as_slice(), resulting_filenames.as_slice());

    std::fs::remove_dir_all(test_dir).unwrap();
}

// Must be able to navigate arbitrarily deep hierarchies.
#[test]
fn test_walkdir_deep() {
    let test_dir = &format!("{}/test_walkdir_deep", env!("CARGO_TARGET_TMPDIR"));
    fs::create_dir(test_dir).unwrap();

    struct Fd(libc::c_int);

    impl Drop for Fd {
        fn drop(&mut self) {
            let ret = unsafe { libc::close(self.0) };
            if ret != 0 {
                panic!("{}", io::Error::last_os_error());
            }
        }
    }

    // Name of a directory in the hierarchy
    let dir_name = { CString::new(["x"; 200].join("").as_bytes()).unwrap() };

    const DEPTH: usize = 500;
    unsafe {
        let test_dir_cstr = CString::new(test_dir.as_bytes()).unwrap();
        let ret = libc::open(test_dir_cstr.as_ptr(), libc::O_RDONLY);
        if ret == -1 {
            panic!("{}", io::Error::last_os_error());
        }
        let mut fd = Fd(ret);

        for _ in 0..DEPTH {
            let ret = libc::mkdirat(fd.0, dir_name.as_ptr(), 0o755);
            if ret != 0 {
                panic!("{}", io::Error::last_os_error());
            }

            let ret = libc::openat(fd.0, dir_name.as_ptr(), libc::O_RDONLY);
            if ret == -1 {
                panic!("{}", io::Error::last_os_error());
            }

            fd = Fd(ret);
        }
    }

    let mut count = 0;

    plib::walkdir::traverse_directory(
        test_dir,
        |_| {
            count += 1;
            true
        },
        |_| {},
        |_, _| {},
        false,
        false,
    );

    // Add 1 for `test_dir` itself
    assert_eq!(count, DEPTH + 1);

    std::fs::remove_dir_all(test_dir).unwrap();
}

#[test]
fn test_walkdir_deep_symlinks() {
    let test_dir = &format!("{}/test_walkdir_deep_symlinks", env!("CARGO_TARGET_TMPDIR"));
    fs::create_dir(test_dir).unwrap();

    const DEPTH: usize = 500;

    let mut prev = None;
    for i in 0..DEPTH {
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

    plib::walkdir::traverse_directory(
        format!("{test_dir}/0"), // Follow the symlink chain starting at test_dir/0
        |_| {
            count += 1;
            true
        },
        |_| {},
        |_, _| {},
        false,
        true,
    );

    assert_eq!(count, DEPTH);

    std::fs::remove_dir_all(test_dir).unwrap();
}
