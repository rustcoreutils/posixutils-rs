//
// Copyright (c) 2024-2026 Jeff Garzik
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use chrono::{DateTime, Local};
use plib::testing::{run_test, run_test_with_checker, TestPlan};
use regex::Regex;
use std::fs;
use std::io::Read;
const PR_DATE_TIME_FORMAT: &str = "%b %e %H:%M %Y";

fn pr_test(args: &[&str], test_data: &str, expected_output: &str) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("pr"),
        args: str_args,
        stdin_data: String::from(test_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

fn pr_read_test_file(
    output_filename: &str,
    input_filename: &str,
    header: Option<&str>,
    date: Option<String>,
) -> String {
    let re = Regex::new(r"<DATE>|<FILENAME>").unwrap();

    let dt_string = date.unwrap_or_else(|| {
        let metadata = fs::metadata(input_filename).unwrap();
        let last_modified_time = metadata.modified().unwrap();
        let dt: DateTime<Local> = last_modified_time.into();
        dt.format(PR_DATE_TIME_FORMAT).to_string()
    });

    let mut file = fs::File::open(output_filename).unwrap();
    let mut buf = String::new();
    file.read_to_string(&mut buf).unwrap();

    let s = re.replace_all(&buf, |captures: &regex::Captures<'_>| -> String {
        let marker = captures.get(0).unwrap();
        match marker.as_str() {
            "<DATE>" => dt_string.clone(),
            "<FILENAME>" => header.unwrap_or(input_filename).to_string(),
            _ => panic!("Unknown pattern"),
        }
    });

    s.to_string()
}

#[test]
fn pr_single_column() {
    let input = "tests/pr/lorem_ipsum.txt";
    let output = pr_read_test_file(
        "tests/pr/lorem_ipsum_output_single_column.txt",
        input,
        None,
        None,
    );
    pr_test(&[input], "", &output);
}

#[test]
fn pr_multi_column() {
    let input = "tests/pr/lorem_ipsum.txt";
    let output = pr_read_test_file("tests/pr/lorem_ipsum_output_9_cols.txt", input, None, None);
    pr_test(&["-9", input], "", &output);
}

#[test]
fn pr_multi_column_across() {
    let input = "tests/pr/lorem_ipsum.txt";
    let output = pr_read_test_file(
        "tests/pr/lorem_ipsum_output_2_cols_across.txt",
        input,
        None,
        None,
    );
    pr_test(&["-2", "-a", input], "", &output);
}

#[test]
fn pr_multi_column_merge() {
    // This test requires the current timestamp.
    //
    // It's possible to inject the current timestamp to the expected output
    // before calling `pr_test` but that would cause spurious errors when the
    // minute portion changes in between now and when the process is actually
    // ran:
    //
    // Apr 18 14:12 2024
    // Apr 18 14:13 2024

    let input = "tests/pr/lorem_ipsum.txt";
    let args = &["+1:1", "-m", input, input, input];
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    let test_plan = TestPlan {
        cmd: String::from("pr"),
        args: str_args,
        stdin_data: String::from(""),
        expected_out: String::from(""),
        expected_err: String::from(""),
        expected_exit_code: 0,
    };

    run_test_with_checker(test_plan, |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);

        // MMM++++++++++YYYY
        let re = Regex::new(r"\w{3}.+\d{4}").unwrap();
        let captures = re.captures(&stdout).unwrap();
        let date = captures.get(0).unwrap().as_str();

        let expected_out = pr_read_test_file(
            "tests/pr/lorem_ipsum_output_merge.txt",
            input,
            None,
            Some(date.to_string()),
        );

        assert_eq!(stdout, expected_out);
    });
}

#[test]
fn pr_page_skip() {
    let input = "tests/pr/numbers.txt";
    let output = pr_read_test_file(
        "tests/pr/numbers_output_9_cols_page15.txt",
        input,
        None,
        None,
    );
    pr_test(&["-9", "+15", input], "", &output);
}

#[test]
fn pr_header_replacement() {
    let header = "custom";
    let input = "tests/pr/lorem_ipsum.txt";
    let output = pr_read_test_file(
        "tests/pr/lorem_ipsum_output_page_1.txt",
        input,
        Some(header),
        None,
    );
    pr_test(&["+1:1", "-h", header, input], "", &output);
}

#[test]
fn pr_limit_lines() {
    let input = "tests/pr/numbers.txt";
    let output = pr_read_test_file("tests/pr/numbers_output_l20.txt", input, None, None);
    pr_test(&["+1:1", "-l20", input], "", &output);
}

#[test]
fn pr_limit_lines_trim() {
    // Lines <= 10 behave like -t is used
    let input = "tests/pr/numbers.txt";
    let output = pr_read_test_file("tests/pr/numbers_output_l10.txt", input, None, None);
    pr_test(&["+1:1", "-l10", input], "", &output);
}

#[test]
fn pr_omit_header() {
    let input = "tests/pr/numbers.txt";
    let output = pr_read_test_file("tests/pr/numbers_output_omit_header.txt", input, None, None);
    pr_test(&["+1:1", "-l20", "-t", input], "", &output);
}

#[test]
fn pr_offset() {
    let input = "tests/pr/numbers.txt";
    let output = pr_read_test_file("tests/pr/numbers_output_offset.txt", input, None, None);
    pr_test(&["+1:1", "-o7", input], "", &output);
}

#[test]
fn pr_width() {
    let input = "tests/pr/long_line.txt";
    let output = pr_read_test_file("tests/pr/long_line_output_w72.txt", input, None, None);
    pr_test(&["-2", "-t", "-w72", input], "", &output);

    let output = pr_read_test_file("tests/pr/long_line_output_w200.txt", input, None, None);
    pr_test(&["-2", "-t", "-w200", input], "", &output);

    // -s without -w causes the width to be 512
    let output = pr_read_test_file("tests/pr/long_line_output_s.txt", input, None, None);
    pr_test(&["-2", "-t", "-s", input], "", &output);
}

#[test]
fn pr_number_line() {
    let input = "tests/pr/lorem_ipsum.txt";
    let output = pr_read_test_file(
        "tests/pr/lorem_ipsum_output_number_line.txt",
        input,
        None,
        None,
    );
    pr_test(&["-9", "-n3", input], "", &output);
}

// Regression: `-4ats` must parse as `--columns=4 -a -t -s` (default sep <tab>).
// POSIX.2024: `-s` with `-t` suppresses padding/truncation.
#[test]
fn pr_columns_cluster_with_optional_sep() {
    let input = (1..=12)
        .map(|n| n.to_string())
        .collect::<Vec<_>>()
        .join("\n")
        + "\n";
    let expected = "1\t2\t3\t4\n5\t6\t7\t8\n9\t10\t11\t12\n";
    pr_test(&["-4ats"], &input, expected);
}

// Same with --columns long form, ensuring `-ats` cluster still resolves
// `-s` to default <tab> without requiring an attached value.
#[test]
fn pr_columns_long_then_cluster_with_optional_sep() {
    let input = (1..=12)
        .map(|n| n.to_string())
        .collect::<Vec<_>>()
        .join("\n")
        + "\n";
    let expected = "1\t2\t3\t4\n5\t6\t7\t8\n9\t10\t11\t12\n";
    pr_test(&["--columns", "4", "-ats"], &input, expected);
}

// `-s` separated from the cluster (`-at -s`) must also use default <tab>.
#[test]
fn pr_columns_separate_s_default_tab() {
    let input = (1..=12)
        .map(|n| n.to_string())
        .collect::<Vec<_>>()
        .join("\n")
        + "\n";
    let expected = "1\t2\t3\t4\n5\t6\t7\t8\n9\t10\t11\t12\n";
    pr_test(&["--columns", "4", "-at", "-s"], &input, expected);
}

// `-s` with an explicit attached value inside a cluster: `-ats,`.
#[test]
fn pr_columns_cluster_with_explicit_sep() {
    let input = (1..=12)
        .map(|n| n.to_string())
        .collect::<Vec<_>>()
        .join("\n")
        + "\n";
    let expected = "1,2,3,4\n5,6,7,8\n9,10,11,12\n";
    pr_test(&["--columns", "4", "-ats,"], &input, expected);
}

// Partial last row keeps trailing separators (no implicit trimming) so
// column positions are preserved. 10 items in 4 across columns: last row
// is `9,10,<pad>,<pad>` and is emitted as `9\t10\t\t`.
#[test]
fn pr_columns_partial_last_row_keeps_seps() {
    let input = (1..=10)
        .map(|n| n.to_string())
        .collect::<Vec<_>>()
        .join("\n")
        + "\n";
    let expected = "1\t2\t3\t4\n5\t6\t7\t8\n9\t10\t\t\n";
    pr_test(&["-4ats"], &input, expected);
}

// POSIX strict: `-s` alone (without `-t` or `-e`) does NOT affect
// truncation/alignment. Cells are still padded/truncated to column width,
// but the separator character is used between them.
//
// Auto-omit-header (page_length <= 10) is used here to suppress the header
// without passing `-t`, which would otherwise switch on the no-pad path.
#[test]
fn pr_separator_alone_keeps_padding() {
    let input = "1\n2\n3\n4\n5\n6\n7\n8\n";
    // -4 -a: 4 cols across. -l9: tiny page (auto-omit-header). -w33: fixed
    // width so column_width = (33 - 4 + 1) / 4 = 7. No -t/-e, so pad_columns
    // is true: cells right-padded to 7 chars and joined by <tab>.
    let expected = "1      \t2      \t3      \t4      \n5      \t6      \t7      \t8      \n";
    pr_test(&["-4", "-a", "-l9", "-w33", "-s"], input, expected);
}

#[test]
fn pr_expand_and_replace() {
    let input = "tests/pr/spaces_and_tabs.txt";
    let output = pr_read_test_file(
        "tests/pr/spaces_and_tabs_expand_and_replace.txt",
        input,
        None,
        None,
    );
    pr_test(&["-i?3", "-e", "-t", input], "", &output);
}

// Finding #6: POSIX places no restriction tying `-s` to multi-column mode. A
// single-column `pr -s, file` must be accepted (previously rejected by a clap
// `requires = "multi_column"`). The separator has no effect in single column.
#[test]
fn pr_separator_single_column() {
    pr_test(&["-s,", "-t", "-l5"], "a\nb\nc\n", "a\nb\nc\n");
}

// Finding #7: a <form-feed> can end a page before it is full. The page's
// non-padding line count must reflect the real number of lines so that
// `required_rows` (and the multi-column row layout) is computed correctly,
// rather than reporting the full page capacity. Here the form-feed after `c`
// ends page 1 with 3 lines; in 2-column down mode each page must render only
// `ceil(3/2) = 2` rows, not the full per-column page length.
#[test]
fn pr_form_feed_multi_column_page_accounting() {
    pr_test(
        &["-2", "-t", "-s,"],
        "a\nb\nc\u{0c}d\ne\nf\n",
        "a,c,\nb,\nd,f,\ne,\n",
    );
}

// ---------------------------------------------------------------------------
// Option coverage and diagnostics
// ---------------------------------------------------------------------------

fn pr_tmp(tag: &str, content: &str) -> std::path::PathBuf {
    let mut p = std::env::temp_dir();
    p.push(format!("posixutils-pr-{}-{}", std::process::id(), tag));
    std::fs::write(&p, content).expect("write temp file");
    p
}

#[test]
fn pr_double_space_output() {
    // -d writes the output double-spaced.
    let f = pr_tmp("d", "l1\nl2\n");
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_pr"))
        .args(["-d", "-t", f.to_str().unwrap()])
        .output()
        .expect("run pr");
    assert_eq!(out.status.code(), Some(0));
    assert_eq!(String::from_utf8_lossy(&out.stdout), "l1\n\nl2\n\n");
    let _ = std::fs::remove_file(f);
}

#[test]
fn pr_suppress_file_warnings() {
    // -r writes no diagnostic for a file that cannot be opened; the exit
    // status still reflects the failure.
    let with = std::process::Command::new(env!("CARGO_BIN_EXE_pr"))
        .args(["-r", "no-such-file-xyz"])
        .output()
        .expect("run pr");
    assert_ne!(with.status.code(), Some(0));
    assert!(
        with.stderr.is_empty(),
        "-r must suppress the diagnostic, got {:?}",
        String::from_utf8_lossy(&with.stderr)
    );

    let without = std::process::Command::new(env!("CARGO_BIN_EXE_pr"))
        .arg("no-such-file-xyz")
        .output()
        .expect("run pr");
    assert_ne!(without.status.code(), Some(0));
    let stderr = String::from_utf8_lossy(&without.stderr);
    assert!(!stderr.is_empty(), "without -r there must be a diagnostic");
    // The diagnostic identifies the utility, as the rest of the tree does.
    assert!(stderr.starts_with("pr:"), "got {stderr:?}");
}

#[test]
fn pr_merge_of_empty_files() {
    let a = pr_tmp("m1", "");
    let b = pr_tmp("m2", "");
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_pr"))
        .args(["-m", "-t", a.to_str().unwrap(), b.to_str().unwrap()])
        .output()
        .expect("run pr");
    assert_eq!(out.status.code(), Some(0));
    assert_eq!(String::from_utf8_lossy(&out.stdout), "");
    let _ = std::fs::remove_file(a);
    let _ = std::fs::remove_file(b);
}

#[test]
fn pr_form_feed_options_differ() {
    // -f writes an <alert> before the form feed on a pause; -F does not pause
    // and writes neither.
    let f = pr_tmp("ff", "l1\n");
    let with_f = std::process::Command::new(env!("CARGO_BIN_EXE_pr"))
        .args(["-f", "-t", f.to_str().unwrap()])
        .output()
        .expect("run pr -f");
    let with_upper_f = std::process::Command::new(env!("CARGO_BIN_EXE_pr"))
        .args(["-F", "-t", f.to_str().unwrap()])
        .output()
        .expect("run pr -F");
    assert_eq!(with_f.status.code(), Some(0));
    assert_eq!(with_upper_f.status.code(), Some(0));
    // POSIX: the pause "shall write an <alert> to standard error", not to
    // standard output, so the page text itself is identical.
    assert!(
        with_f.stderr.contains(&b'\x07'),
        "-f writes an alert to stderr, got {:?}",
        String::from_utf8_lossy(&with_f.stderr)
    );
    assert!(
        !with_upper_f.stderr.contains(&b'\x07'),
        "-F must not pause, got {:?}",
        String::from_utf8_lossy(&with_upper_f.stderr)
    );
    assert_eq!(
        with_f.stdout, with_upper_f.stdout,
        "the alert is the only difference; the page text is the same"
    );
    let _ = std::fs::remove_file(f);
}

#[test]
fn pr_header_date_follows_lc_time() {
    // The page header carries a date. It must render under any locale without
    // failing; the exact spelling is locale data, not something to pin.
    let Some(loc) = plib::testing::utf8_locale() else {
        return;
    };
    let f = pr_tmp("date", "body\n");
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_pr"))
        .arg(f.to_str().unwrap())
        .env("LC_ALL", &loc)
        .env("LC_TIME", &loc)
        .output()
        .expect("run pr");
    assert_eq!(out.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&out.stdout);
    let header = stdout
        .lines()
        .find(|l| l.contains("Page"))
        .expect("a page header is written");
    assert!(header.contains("Page 1"), "got {header:?}");
    assert!(
        header.contains(f.file_name().unwrap().to_str().unwrap()),
        "the header names the file, got {header:?}"
    );
    let _ = std::fs::remove_file(f);
}
