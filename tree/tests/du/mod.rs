//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, run_test_with_checker, TestPlan};
use std::{fs, io::Write, os::unix::fs::MetadataExt, process::Output};

fn du_test(args: &[&str], expected_output: &str, expected_error: &str, expected_exit_code: i32) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("du"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code,
    });
}

fn du_test2(args: &[&str], checker: impl FnMut(&TestPlan, &Output)) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("du"),
            args: str_args,
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        checker,
    );
}

// Partial port of coreutils/tests/du/basic.sh
// Omits the nonstandard --block-size and -S options
#[test]
fn test_du_basic() {
    let test_dir = &format!("{}/test_du_basic", env!("CARGO_TARGET_TMPDIR"));

    // DEBUG
    let _ = fs::remove_dir_all(test_dir);

    let a = &format!("{test_dir}/a");

    let a_b = &format!("{test_dir}/a/b");
    let d = &format!("{test_dir}/d");
    let d_sub = &format!("{test_dir}/d/sub");

    let a_b_f = &format!("{a_b}/F");
    let d_1 = &format!("{d}/1");
    let d_sub_2 = &format!("{d_sub}/2");

    fs::create_dir(test_dir).unwrap();
    for dir in [a_b, d, d_sub] {
        fs::create_dir_all(dir).unwrap();
    }

    {
        // Create a > 257 bytes file
        let mut file1 = fs::File::create(a_b_f).unwrap();
        write!(file1, "{:>257}", "x").unwrap();

        // Create a 4 KiB file
        let mut file2 = fs::File::create(d_1).unwrap();
        write!(file2, "{:>4096}", "x").unwrap();

        fs::copy(d_1, d_sub_2).unwrap();
    }

    // Manually calculate the block sizes for directory a
    let [size_abf, mut size_ab, mut size_a] =
        [a_b_f, a_b, a].map(|filename| fs::metadata(filename).unwrap().blocks());
    size_ab += size_abf;
    size_a += size_ab;

    du_test(
        &["-a", a],
        &format!(
            "{size_abf}\t{a_b_f}\
            \n{size_ab}\t{a_b}\
            \n{size_a}\t{a}\n"
        ),
        "",
        0,
    );

    du_test(&["-s", a], &format!("{size_a}\t{a}\n"), "", 0);

    // Manually calculate the block sizes for directory d
    let [size_dsub2, mut size_dsub, size_d1, mut size_d] =
        [d_sub_2, d_sub, d_1, d].map(|filename| fs::metadata(filename).unwrap().blocks());
    size_dsub += size_dsub2;
    size_d += size_d1 + size_dsub;

    du_test2(&["-a", d], |_, output| {
        // d/1 is not guaranteed to be listed after d/sub
        //
        // Case #1:
        // 8	d/sub/2
        // 8	d/sub
        // 8	d/1
        // 16	d
        //
        // Case #2:
        // 8	d/1
        // 8	d/sub/2
        // 8	d/sub
        // 16	d
        //
        // So instead, we compare the sorted lines to check for correctness

        // Split the output wrt to new lines and then sort them.
        let split_then_sort = |s: String| -> Vec<String> {
            let mut lines: Vec<String> = s.lines().map(|s| s.to_string()).collect();
            lines.sort();
            lines
        };

        let stdout = String::from_utf8_lossy(&output.stdout).to_string();

        let expected = format!(
            "{size_dsub2}\t{d_sub_2}\
            \n{size_dsub}\t{d_sub}\
            \n{size_d1}\t{d_1}\
            \n{size_d}\t{d}\n"
        );

        assert_eq!(split_then_sort(stdout), split_then_sort(expected));
    });

    fs::remove_dir_all(test_dir).unwrap();
}
