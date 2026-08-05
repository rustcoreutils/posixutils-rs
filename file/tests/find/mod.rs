//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fs::{remove_file, File};
use std::io::Write;
use std::process::{Command, Stdio};

use plib::testing::{get_binary_path, run_test, run_test_with_checker, TestPlan};

fn run_test_find(
    args: &[&str],
    expected_output: &str,
    expected_error: &str,
    expected_exit_code: i32,
) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("find"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code,
    });
}

/// Run find and compare outputs after sorting lines (for order-independent comparison)
fn run_test_find_sorted(
    args: &[&str],
    expected_lines: &[&str],
    expected_error: &str,
    expected_exit_code: i32,
) {
    run_test_find_sorted_env(
        args,
        &[],
        expected_lines,
        expected_error,
        expected_exit_code,
    );
}

/// Like [`run_test_find_sorted`] but with extra environment variables for the
/// child process. Used to pin `LC_ALL=C` for tests whose `-name` bracket ranges
/// (`[a-z]`) are otherwise `LC_COLLATE`-sensitive: `find` matches via libc
/// `fnmatch(3)`, and a UTF-8 locale on some libc implementations (notably the
/// BSD libc on macOS) collates uppercase letters into the `a`–`z` range. Pinning
/// the C locale gives the deterministic, portable ASCII semantics these tests
/// document.
fn run_test_find_sorted_env(
    args: &[&str],
    env: &[(&str, &str)],
    expected_lines: &[&str],
    expected_error: &str,
    expected_exit_code: i32,
) {
    let find_path = get_binary_path("find");

    let mut command = Command::new(&find_path);
    command.args(args).stdin(Stdio::null());
    for (key, value) in env {
        command.env(key, value);
    }
    let output = command.output().expect("failed to execute find");

    let actual_stdout = String::from_utf8_lossy(&output.stdout);
    let actual_stderr = String::from_utf8_lossy(&output.stderr);
    let actual_exit_code = output.status.code().unwrap_or(-1);

    // Sort actual output lines
    let mut actual_lines: Vec<&str> = actual_stdout.lines().collect();
    actual_lines.sort();

    // Sort expected lines
    let mut expected_sorted: Vec<&str> = expected_lines.to_vec();
    expected_sorted.sort();

    assert_eq!(
        actual_lines, expected_sorted,
        "stdout mismatch (sorted comparison)"
    );
    assert_eq!(actual_stderr, expected_error, "stderr mismatch");
    assert_eq!(actual_exit_code, expected_exit_code, "exit code mismatch");
}

#[test]
fn find_size_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args = [&test_dir, "-size", "+4"];

    // The different result of the command is due to a feature of the linux and macos operating systems,
    // namely the use of different file systems (ext4 on linux and APFS on macos).
    // Therefore, the size of folders differs depending on the operating system.
    #[cfg(not(target_os = "macos"))]
    let expected_output = format!("{}\n{}/file1.txt\n", test_dir, test_dir);

    #[cfg(target_os = "macos")]
    let expected_output = format!("{}/file1.txt\n", test_dir);

    run_test_find(&args, &expected_output, "", 0)
}

#[test]
fn find_name_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args = [&test_dir, "-name", "empty_file.txt"];

    let expected_output = format!("{}/empty_file.txt\n", test_dir);

    run_test_find(&args, &expected_output, "", 0)
}

#[test]
fn find_type_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args: [&str; 3] = [&test_dir, "-type", "f"];

    let file1 = format!("{}/empty_file.txt", test_dir);
    let file2 = format!("{}/file with space.txt", test_dir);
    let file3 = format!("{}/file1.txt", test_dir);
    let file4 = format!("{}/rust_file.rs", test_dir);

    run_test_find_sorted(&args, &[&file1, &file2, &file3, &file4], "", 0)
}

#[test]
fn find_mtime_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args = [&test_dir, "-mtime", "7000"];

    run_test_find(&args, "", "", 0)
}

#[test]
fn find_combination_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args = [&test_dir, "-size", "+4", "-print", "-size", "+2", "-print"];

    // The different result of the command is due to a feature of the linux and macos operating systems,
    // namely the use of different file systems (ext4 on linux and APFS on macos).
    // Therefore, the size of folders differs depending on the operating system.
    #[cfg(not(target_os = "macos"))]
    let expected_output = format!(
        "{}\n{}\n{}/file1.txt\n{}/file1.txt\n",
        test_dir, test_dir, test_dir, test_dir
    );

    #[cfg(target_os = "macos")]
    let expected_output = format!("{}/file1.txt\n{}/file1.txt\n", test_dir, test_dir);

    run_test_find(&args, &expected_output, "", 0)
}

#[test]
fn find_not_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args: [&str; 4] = [&test_dir, "!", "-path", "*.txt"];

    let dir = test_dir.clone();
    let file = format!("{}/rust_file.rs", test_dir);

    run_test_find_sorted(&args, &[&dir, &file], "", 0)
}

#[test]
fn find_or_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args: [&str; 6] = [&test_dir, "-path", "*.rs", "-o", "-path", "*.txt"];

    let file1 = format!("{}/empty_file.txt", test_dir);
    let file2 = format!("{}/file with space.txt", test_dir);
    let file3 = format!("{}/file1.txt", test_dir);
    let file4 = format!("{}/rust_file.rs", test_dir);

    run_test_find_sorted(&args, &[&file1, &file2, &file3, &file4], "", 0)
}

#[test]
fn find_and_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args = [&test_dir, "-path", "*.txt", "-a", "-size", "+2"];

    let expected_output = format!("{}/file1.txt\n", test_dir);

    run_test_find(&args, &expected_output, "", 0)
}

#[test]
fn find_space_argument_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args = [&test_dir, "-name", "file with space.txt"];

    let expected_output = format!("{}/file with space.txt\n", test_dir);

    run_test_find(&args, &expected_output, "", 0)
}

#[test]
fn find_no_user_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args = [&test_dir, "-nouser"];

    run_test_find(&args, "", "", 0)
}

#[test]
fn find_no_group_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args = [&test_dir, "-nogroup"];

    run_test_find(&args, "", "", 0)
}

#[test]
fn find_x_dev_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args: [&str; 2] = [&test_dir, "-xdev"];

    let dir = test_dir.clone();
    let file1 = format!("{}/empty_file.txt", test_dir);
    let file2 = format!("{}/file with space.txt", test_dir);
    let file3 = format!("{}/file1.txt", test_dir);
    let file4 = format!("{}/rust_file.rs", test_dir);

    run_test_find_sorted(&args, &[&dir, &file1, &file2, &file3, &file4], "", 0)
}

#[test]
fn find_perm_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args = [&test_dir, "-perm", "777"];

    run_test_find(&args, "", "", 0)
}

#[test]
fn find_links_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args: [&str; 3] = [&test_dir, "-links", "1"];

    let file1 = format!("{}/empty_file.txt", test_dir);
    let file2 = format!("{}/file with space.txt", test_dir);
    let file3 = format!("{}/file1.txt", test_dir);
    let file4 = format!("{}/rust_file.rs", test_dir);

    run_test_find_sorted(&args, &[&file1, &file2, &file3, &file4], "", 0)
}

#[test]
fn find_group_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args = [&test_dir, "-group", "name"];

    run_test_find(&args, "", "", 0)
}

#[test]
fn find_newer_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/newer", project_root);
    let path_to_test_file = format!("{}/qwe.txt", test_dir);
    let mut file = File::create(&path_to_test_file).unwrap();
    writeln!(file, "File content").unwrap();
    let args = [&test_dir, "-newer", &path_to_test_file];

    run_test_find(&args, "", "", 0);

    remove_file(&path_to_test_file).unwrap();
}

/// Run find and compare null-delimited output (for -print0 testing)
fn run_test_find_print0_sorted(args: &[&str], expected_paths: &[&str], expected_exit_code: i32) {
    let find_path = get_binary_path("find");

    let output = Command::new(&find_path)
        .args(args)
        .stdin(Stdio::null())
        .output()
        .expect("failed to execute find");

    let actual_exit_code = output.status.code().unwrap_or(-1);

    // Split output on null bytes and filter empty entries
    let mut actual_paths: Vec<&str> = output
        .stdout
        .split(|&b| b == 0)
        .filter_map(|bytes| std::str::from_utf8(bytes).ok())
        .filter(|s| !s.is_empty())
        .collect();
    actual_paths.sort();

    let mut expected_sorted: Vec<&str> = expected_paths.to_vec();
    expected_sorted.sort();

    assert_eq!(
        actual_paths, expected_sorted,
        "stdout mismatch (null-delimited sorted comparison)"
    );
    assert!(
        output.stderr.is_empty(),
        "stderr should be empty: {:?}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(actual_exit_code, expected_exit_code, "exit code mismatch");
}

#[test]
fn find_print0_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args: [&str; 4] = [&test_dir, "-type", "f", "-print0"];

    let file1 = format!("{}/empty_file.txt", test_dir);
    let file2 = format!("{}/file with space.txt", test_dir);
    let file3 = format!("{}/file1.txt", test_dir);
    let file4 = format!("{}/rust_file.rs", test_dir);

    run_test_find_print0_sorted(&args, &[&file1, &file2, &file3, &file4], 0)
}

#[test]
fn find_print0_with_name_filter() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args = [&test_dir, "-name", "*.txt", "-print0"];

    let file1 = format!("{}/empty_file.txt", test_dir);
    let file2 = format!("{}/file with space.txt", test_dir);
    let file3 = format!("{}/file1.txt", test_dir);

    run_test_find_print0_sorted(&args, &[&file1, &file2, &file3], 0)
}

// --- fnmatch / -iname (find-A) ---

/// Create a fresh temp dir with the given files; returns its path.
fn make_fnmatch_dir(tag: &str, files: &[&str]) -> std::path::PathBuf {
    let dir = std::env::temp_dir().join(format!("posixutils_find_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    for f in files {
        File::create(dir.join(f)).unwrap();
    }
    dir
}

#[test]
fn find_name_bracket_range() {
    // POSIX bracket expression [a-z] must match a single lowercase letter. The
    // range is LC_COLLATE-sensitive, so pin the C locale for portable ASCII
    // semantics (see run_test_find_sorted_env).
    let dir = make_fnmatch_dir("bracket", &["m", "Q", "9", "abc"]);
    let ds = dir.to_str().unwrap();
    let expect = format!("{ds}/m");
    run_test_find_sorted_env(
        &[ds, "-name", "[a-z]"],
        &[("LC_ALL", "C")],
        &[&expect],
        "",
        0,
    );
    std::fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn find_name_bracket_negation() {
    let dir = make_fnmatch_dir("negation", &["m", "Q", "9"]);
    let ds = dir.to_str().unwrap();
    let e1 = format!("{ds}/Q");
    let e2 = format!("{ds}/9");
    // [!a-z] is the complement of the same LC_COLLATE-sensitive range; pin C.
    run_test_find_sorted_env(
        &[ds, "-name", "[!a-z]"],
        &[("LC_ALL", "C")],
        &[&e1, &e2],
        "",
        0,
    );
    std::fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn find_iname_case_insensitive() {
    let dir = make_fnmatch_dir("iname", &["README.md", "other.txt"]);
    let ds = dir.to_str().unwrap();
    let expect = format!("{ds}/README.md");
    run_test_find_sorted(&[ds, "-iname", "readme.md"], &[&expect], "", 0);
    std::fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn find_name_question_mark() {
    let dir = make_fnmatch_dir("qmark", &["ab", "abc", "a"]);
    let ds = dir.to_str().unwrap();
    let expect = format!("{ds}/ab");
    run_test_find_sorted(&[ds, "-name", "a?"], &[&expect], "", 0);
    std::fs::remove_dir_all(&dir).unwrap();
}

// FIND-5: -ok prompts and only runs the utility on an affirmative answer.
fn run_ok_test(answer: &str, expect_stdout_nonempty: bool) {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let target = format!("{}/tests/find/other/empty_file.txt", project_root);
    run_test_with_checker(
        TestPlan {
            cmd: String::from("find"),
            args: vec![
                target.clone(),
                String::from("-ok"),
                String::from("echo"),
                String::from("{}"),
                String::from(";"),
            ],
            stdin_data: String::from(answer),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert_eq!(stdout.contains(&target), expect_stdout_nonempty);
        },
    );
}

#[test]
fn find_ok_accepted() {
    run_ok_test("y\n", true);
}

#[test]
fn find_ok_declined() {
    run_ok_test("n\n", false);
}

// --- -exec ... {} + aggregation (find-B) ---
//
// POSIX (find, ll. 98284-98298): pathnames "shall be aggregated into sets" and
// the utility "invoked once for each set" -- the sets belong to the *primary*.
// The order of invocations *between* different primaries is explicitly
// unspecified, so these tests compare sorted output and never pin it.

#[test]
fn find_exec_plus_primaries_do_not_collide() {
    // Two independent `-exec ... {} +` primaries must each receive only the
    // pathnames their own branch matched.
    let dir = make_fnmatch_dir("execplus", &["aaa", "bbb"]);
    let ds = dir.to_str().unwrap();
    let a = format!("A {ds}/aaa");
    let b = format!("B {ds}/bbb");
    run_test_find_sorted(
        &[
            ds, "-name", "aaa", "-exec", "echo", "A", "{}", "+", "-o", "-name", "bbb", "-exec",
            "echo", "B", "{}", "+",
        ],
        &[&a, &b],
        "",
        0,
    );
    std::fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn find_exec_plus_identical_utilities_stay_separate() {
    // Two *textually identical* primaries are still two primaries: two sets,
    // two invocations. Keying batches by (utility, args_before) would merge
    // them and fail this test.
    let dir = make_fnmatch_dir("execplus_same", &["aaa", "bbb"]);
    let ds = dir.to_str().unwrap();
    let a = format!("{ds}/aaa");
    let b = format!("{ds}/bbb");
    run_test_find_sorted(
        &[
            ds, "-name", "aaa", "-exec", "echo", "{}", "+", "-o", "-name", "bbb", "-exec", "echo",
            "{}", "+",
        ],
        &[&a, &b],
        "",
        0,
    );
    std::fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn find_exec_plus_single_primary_aggregates() {
    // One primary still gathers every match into a single invocation. Counts
    // arguments rather than printing them, so it is independent of readdir
    // order.
    let dir = make_fnmatch_dir("execplus_one", &["f1", "f2", "f3"]);
    let ds = dir.to_str().unwrap();
    run_test_find(
        &[
            ds, "-type", "f", "-exec", "sh", "-c", "echo $#", "sh", "{}", "+",
        ],
        "3\n",
        "",
        0,
    );
    std::fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn find_exec_plus_empty_batch_not_invoked() {
    // A primary that never matches must not be invoked with an empty set.
    let dir = make_fnmatch_dir("execplus_empty", &["aaa"]);
    let ds = dir.to_str().unwrap();
    run_test_find(
        &[ds, "-name", "no_such_name", "-exec", "echo", "X", "{}", "+"],
        "",
        "",
        0,
    );
    std::fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn find_exec_plus_exit_status() {
    // POSIX (l. 98402): a non-zero exit from a `+`-punctuated -exec makes find
    // exit non-zero.
    let dir = make_fnmatch_dir("execplus_fail", &["aaa"]);
    let ds = dir.to_str().unwrap();
    run_test_find(
        &[ds, "-name", "aaa", "-exec", "false", "{}", "+"],
        "",
        "",
        1,
    );
    std::fs::remove_dir_all(&dir).unwrap();
}

// --- coverage gaps closed in the file-crate audit sweep ---

/// Find a child of `root` that sits on a different device, i.e. a mount point.
/// Returns `None` when the host has none under that root, in which case the
/// caller self-skips (containers and macOS runners vary widely here).
fn find_mount_point_under(root: &str) -> Option<String> {
    use std::os::unix::fs::MetadataExt;
    let root_dev = std::fs::metadata(root).ok()?.dev();
    for entry in std::fs::read_dir(root).ok()?.flatten() {
        let md = match entry.metadata() {
            Ok(m) => m,
            Err(_) => continue,
        };
        if md.is_dir() && md.dev() != root_dev {
            return Some(entry.path().to_string_lossy().into_owned());
        }
    }
    None
}

// FIND-3: -mount excludes the directory that crosses the device boundary,
// whereas -xdev reports it but does not descend into it. The two differ by
// exactly that one entry, which is what this pins.
#[test]
fn find_mount_excludes_crossing_directory() {
    let Some(mp) = find_mount_point_under("/dev") else {
        eprintln!("skipping: no mount point found under /dev on this host");
        return;
    };
    let name = std::path::Path::new(&mp)
        .file_name()
        .unwrap()
        .to_string_lossy()
        .into_owned();

    // -xdev reports the mount point itself.
    run_test_find_sorted(&["/dev", "-xdev", "-name", &name], &[&mp], "", 0);
    // -mount does not.
    run_test_find_sorted(&["/dev", "-mount", "-name", &name], &[], "", 0);
}

// FIND-4: a file list too large for a single argv must be split across several
// invocations, with every pathname passed exactly once. The counts per
// invocation are implementation-defined (they depend on ARG_MAX and the
// inherited environment), so assert only that the work was split and that
// nothing was lost or duplicated.
#[test]
fn find_exec_plus_splits_over_arg_max() {
    let dir = std::env::temp_dir().join("posixutils_find_argmax");
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();

    // ~200-byte names so a few thousand files exceed a typical 2 MiB ARG_MAX.
    let pad = "n".repeat(200);
    const COUNT: usize = 9000;
    for i in 0..COUNT {
        File::create(dir.join(format!("{pad}{i:06}"))).unwrap();
    }

    let ds = dir.to_str().unwrap().to_string();
    let find_path = get_binary_path("find");
    let output = Command::new(&find_path)
        .args([
            &ds, "-type", "f", "-exec", "sh", "-c", "echo $#", "sh", "{}", "+",
        ])
        .stdin(Stdio::null())
        .output()
        .expect("failed to execute find");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let counts: Vec<usize> = stdout
        .lines()
        .map(|l| l.trim().parse::<usize>().expect("invocation count"))
        .collect();

    assert!(
        counts.len() > 1,
        "expected the file list to be split across several invocations, got {counts:?}"
    );
    assert_eq!(
        counts.iter().sum::<usize>(),
        COUNT,
        "every pathname must be passed exactly once across all invocations"
    );
    assert_eq!(output.status.code(), Some(0));

    std::fs::remove_dir_all(&dir).unwrap();
}

// FIND-7: -mtime rounds toward zero on a 24-hour granularity, so a file with a
// timestamp in the future has a negative age and must not match `-mtime +N`
// (nor `-mtime N` for any non-negative N).
#[test]
fn find_mtime_future_dated_file() {
    let dir = std::env::temp_dir().join("posixutils_find_future");
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    let f = dir.join("future");
    File::create(&f).unwrap();

    // Stamp the file a week into the future.
    let week = std::time::Duration::from_secs(7 * 24 * 60 * 60);
    let future = std::time::SystemTime::now() + week;
    let ft = filetime_set(&f, future);
    if !ft {
        eprintln!("skipping: could not set a future mtime on this host");
        std::fs::remove_dir_all(&dir).unwrap();
        return;
    }

    let ds = dir.to_str().unwrap();
    let fs_str = f.to_string_lossy().into_owned();
    // A future file is not "more than 0 days old".
    run_test_find_sorted(&[ds, "-type", "f", "-mtime", "+0"], &[], "", 0);
    // Nor exactly 0 days old.
    run_test_find_sorted(&[ds, "-type", "f", "-mtime", "0"], &[], "", 0);
    // It is, however, newer than a file created now.
    let now_file = dir.join("now");
    File::create(&now_file).unwrap();
    run_test_find_sorted(
        &[ds, "-type", "f", "-newer", now_file.to_str().unwrap()],
        &[&fs_str],
        "",
        0,
    );

    std::fs::remove_dir_all(&dir).unwrap();
}

/// Set both atime and mtime of `path` via `utimensat`. Returns false on failure.
fn filetime_set(path: &std::path::Path, t: std::time::SystemTime) -> bool {
    use std::ffi::CString;
    use std::os::unix::ffi::OsStrExt;
    let secs = match t.duration_since(std::time::UNIX_EPOCH) {
        Ok(d) => d.as_secs() as libc::time_t,
        Err(_) => return false,
    };
    let c = match CString::new(path.as_os_str().as_bytes()) {
        Ok(c) => c,
        Err(_) => return false,
    };
    let ts = [
        libc::timespec {
            tv_sec: secs,
            tv_nsec: 0,
        },
        libc::timespec {
            tv_sec: secs,
            tv_nsec: 0,
        },
    ];
    unsafe { libc::utimensat(libc::AT_FDCWD, c.as_ptr(), ts.as_ptr(), 0) == 0 }
}

/// Answer `-ok` with `answer` under `locale`; returns whether the utility ran.
fn ok_answer_under_locale(locale: &str, answer: &str) -> bool {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let target = format!("{}/tests/find/other/empty_file.txt", project_root);
    let find_path = get_binary_path("find");

    let mut child = Command::new(&find_path)
        .args([&target, "-ok", "echo", "{}", ";"])
        .env("LC_ALL", locale)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to spawn find");
    child
        .stdin
        .as_mut()
        .unwrap()
        .write_all(answer.as_bytes())
        .unwrap();
    let out = child.wait_with_output().unwrap();
    String::from_utf8_lossy(&out.stdout).contains(&target)
}

// FIND-5: the answers -ok accepts come from the locale's YESEXPR, not from a
// hardcoded English `y`. The C-locale half runs everywhere; the non-English
// half self-skips unless such a locale is installed.
#[test]
fn find_ok_honors_locale_yesexpr() {
    // Under C, YESEXPR is ^[yY]: `y` is affirmative and `o` is not.
    assert!(ok_answer_under_locale("C", "y\n"));
    assert!(!ok_answer_under_locale("C", "o\n"));

    let Some(loc) =
        plib::testing::locale_matching(&["fr_FR.UTF-8", "fr_FR.utf8", "de_DE.UTF-8", "de_DE.utf8"])
    else {
        eprintln!("skipping non-English half: no French or German locale installed");
        return;
    };

    // French YESEXPR accepts a leading `o` (oui), German a leading `j` (ja) --
    // neither of which the C locale accepts, so this is the assertion that a
    // hardcoded `^[yY]` would fail.
    let answer = if loc.starts_with("fr") { "o\n" } else { "j\n" };
    assert!(
        ok_answer_under_locale(&loc, answer),
        "locale {loc}: affirmative {answer:?} should have run the utility"
    );
}
