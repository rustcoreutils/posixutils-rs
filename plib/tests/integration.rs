use std::{
    ffi::{CStr, CString},
    fs, io,
    os::unix,
    path::Path,
};

// Test the correctness of using the result of telldir under various conditions.
#[test]
fn test_walkdir_telldir() {
    let test_dir = &format!("{}/test_walkdir_telldir", env!("CARGO_TARGET_TMPDIR"));
    let target_dir = &format!("{test_dir}/target_dir");
    let renamed_dir = &format!("{test_dir}/renamed_dir");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir(target_dir).unwrap();

    struct Dir(*mut libc::DIR);

    impl Drop for Dir {
        fn drop(&mut self) {
            unsafe {
                libc::closedir(self.0);
            }
        }
    }

    fn open_dir(target_dir: &str) -> Dir {
        let target_dir_cstr = CString::new(target_dir.as_bytes()).unwrap();
        let dirp = unsafe { libc::opendir(target_dir_cstr.as_ptr()) };
        if dirp.is_null() {
            panic!("{}", io::Error::last_os_error());
        }
        Dir(dirp)
    }

    fn eval(target_dir: &str, filename_next: &CStr, telldir_val: libc::c_long) {
        let dir = open_dir(target_dir);

        unsafe { libc::seekdir(dir.0, telldir_val) };
        let dirent = unsafe { libc::readdir(dir.0) };
        assert!(!dirent.is_null());

        let cstr = unsafe { CStr::from_ptr((&*dirent).d_name.as_ptr()) };
        assert_eq!(filename_next.to_bytes(), cstr.to_bytes());
    }

    const NUM_FILES: usize = 10;

    for i in 0..NUM_FILES {
        fs::File::create(format!("{target_dir}/{i}")).unwrap();
    }

    let mut telldir_val = 0;

    // The next file returned by `libc::readdir` after calling `libc::seekdir` with `telldir_val`.
    let filename_next = {
        let dir = open_dir(&target_dir);

        let target = CString::new(format!("{}", NUM_FILES - 1).as_bytes()).unwrap();

        loop {
            let dirent = unsafe { libc::readdir(dir.0) };
            if dirent.is_null() {
                // Ignoring the case when errno is 0
                panic!("{}", io::Error::last_os_error());
            }

            let name = unsafe { (&*dirent).d_name };
            let cstr = unsafe { CStr::from_ptr(name.as_ptr()) };

            if cstr.to_bytes() == target.to_bytes() {
                break cstr.to_owned();
            }

            telldir_val = unsafe { libc::telldir(dir.0) };
        }
    };

    // Sanity check, no modification to target_dir
    eval(&target_dir, &filename_next, telldir_val);

    // Add new files to target_dir
    {
        for i in NUM_FILES..(2 * NUM_FILES) {
            fs::File::create(format!("{target_dir}/{i}")).unwrap();
        }

        eval(&target_dir, &filename_next, telldir_val);
    }

    // Remove files from target_dir
    {
        {
            let dir = open_dir(&target_dir);
            loop {
                let dirent = unsafe { libc::readdir(dir.0) };
                if dirent.is_null() {
                    let last_err = io::Error::last_os_error();
                    let errno = last_err.raw_os_error().unwrap();
                    if errno == 0 {
                        break;
                    } else {
                        panic!("{}", last_err);
                    }
                }

                let name = unsafe { (&*dirent).d_name };
                let cstr = unsafe { CStr::from_ptr(name.as_ptr()) };

                let dot = c".";
                let dotdot = c"..";

                // Skip . and ..
                if cstr.to_bytes() == dot.to_bytes() || cstr.to_bytes() == dotdot.to_bytes() {
                    continue;
                }

                if cstr.to_bytes() != filename_next.to_bytes() {
                    fs::remove_file(format!("{target_dir}/{}", cstr.to_string_lossy())).unwrap();
                }
            }
        }

        eval(&target_dir, &filename_next, telldir_val);
    }

    // Rename target_dir
    {
        for i in NUM_FILES..(2 * NUM_FILES) {
            fs::File::create(format!("{target_dir}/{i}")).unwrap();
        }
        fs::rename(target_dir, renamed_dir).unwrap();

        eval(&renamed_dir, &filename_next, telldir_val);
    }

    std::fs::remove_dir_all(test_dir).unwrap();
}

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

    plib::walkdir::traverse_directory(
        test_dir,
        |entry| {
            let s = format!("{}", entry.path());
            filenames.push(s);
            Ok(true)
        },
        |_| Ok(()),
        |_, _| {},
        false,
        false,
    );

    expected_filenames.sort();
    filenames.sort();
    assert_eq!(expected_filenames.as_slice(), filenames.as_slice());

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
    let mut expected_filenames = [
        format!("{test_dir}"),
        format!("{test_dir}/0"),
        format!("{test_dir}/1"),
        format!("{test_dir}/2"),
        format!("{test_dir}/1"),
        format!("{test_dir}/2"),
        format!("{test_dir}/2"),
    ];

    let mut filenames = Vec::new();

    plib::walkdir::traverse_directory(
        test_dir,
        |entry| {
            let s = format!("{}", entry.path());
            filenames.push(s);
            Ok(true)
        },
        |_| Ok(()),
        |_, _| {},
        false,
        true,
    );

    expected_filenames.sort();
    filenames.sort();
    assert_eq!(expected_filenames.as_slice(), filenames.as_slice());

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
            Ok(true)
        },
        |_| Ok(()),
        |_, _| {},
        false,
        false,
    );

    // Add 1 for `test_dir` itself
    assert_eq!(count, DEPTH + 1);

    std::fs::remove_dir_all(test_dir).unwrap();
}

// Same as `test_walkdir_deep` but using symlinks.
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
            Ok(true)
        },
        |_| Ok(()),
        |_, _| {},
        false,
        true,
    );

    assert_eq!(count, DEPTH);

    std::fs::remove_dir_all(test_dir).unwrap();
}

// Tests the resilience against making the search go to a different directory by modifying the path.
#[test]
fn test_walkdir_path_prefix_modification() {
    let test_dir = &format!(
        "{}/test_walkdir_path_prefix_modification",
        env!("CARGO_TARGET_TMPDIR")
    );
    let correct_dir = format!("{test_dir}/correct_dir");
    let wrong_dir = format!("{test_dir}/wrong_dir");
    let a_b = format!("{test_dir}/a/b");

    fs::create_dir(test_dir).unwrap();

    fs::create_dir_all(format!("{correct_dir}/c/d/e/f/g/h/i/j")).unwrap();
    fs::create_dir_all(format!("{wrong_dir}/x/x/x/x/x/x/x/x")).unwrap();

    // a/b -> correct_dir
    fs::create_dir(format!("{test_dir}/a")).unwrap();
    unix::fs::symlink(&correct_dir, format!("{test_dir}/a/b")).unwrap();

    let mut entered_dir_a_b: Option<bool> = Some(false);
    let mut filenames = Vec::new();

    plib::walkdir::traverse_directory(
        &a_b,
        |entry| {
            let path = entry.path();
            let filename = path.file_name().unwrap();
            filenames.push(filename.to_str().unwrap().to_owned());

            if entered_dir_a_b == Some(true) {
                // Symbolic link to a different directory
                // a/new -> wrong_dir
                let new_symlink = format!("{test_dir}/a/new");
                unix::fs::symlink(&wrong_dir, &new_symlink).unwrap();

                // Overwrite a/b with a/new
                fs::rename(&new_symlink, &a_b).unwrap();

                // a/b now points to wrong_dir
                assert_eq!(fs::read_link(&a_b).unwrap(), Path::new(&wrong_dir));

                // Prevents doing the rename again
                entered_dir_a_b = None;
            }

            // Wait until a/b is entered before changing the symbolic link
            if entered_dir_a_b == Some(false) {
                entered_dir_a_b = Some(true);
            }

            Ok(true)
        },
        |_| Ok(()),
        |_, _| {},
        true,
        true,
    );

    // Once a/b is reached, traverse_directory should not go down the wrong directory even if a/b is
    // changed
    assert!(!filenames.contains(&String::from("x")));

    filenames.clear();
    plib::walkdir::traverse_directory(
        &a_b,
        |entry| {
            let path = entry.path();
            let filename = path.file_name().unwrap();
            filenames.push(filename.to_str().unwrap().to_owned());
            Ok(true)
        },
        |_| Ok(()),
        |_, _| {},
        true,
        true,
    );

    // Rerunning the directory traversal should now follow the "wrong" directory.
    assert_eq!(filenames.iter().filter(|f| f.as_str() == "x").count(), 8);

    std::fs::remove_dir_all(test_dir).unwrap();
}
