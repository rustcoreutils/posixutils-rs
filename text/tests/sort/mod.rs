//
// Copyright (c) 2024 Jeff Garzik
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{TestPlan, run_test};

fn sort_test(
    args: &[&str],
    test_data: &str,
    expected_output: &str,
    expected_exit_code: i32,
    expected_err: &str,
) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("sort"),
        args: str_args,
        stdin_data: String::from(test_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_err),
        expected_exit_code,
    });
}

#[test]
fn test_n1() {
    sort_test(&["-n"], ".01\n0\n", "0\n.01\n", 0, "");
}

#[test]
fn test_n2() {
    sort_test(&["-n"], ".02\n.01\n", ".01\n.02\n", 0, "");
}

#[test]
fn test_n3() {
    sort_test(&["-n"], ".02\n.00\n", ".00\n.02\n", 0, "");
}

#[test]
fn test_n4() {
    sort_test(&["-n"], ".02\n.000\n", ".000\n.02\n", 0, "");
}

#[test]
fn test_n5() {
    sort_test(&["-n"], ".021\n.029\n", ".021\n.029\n", 0, "");
}

#[test]
fn test_n6() {
    sort_test(&["-n"], ".02\n.0*\n", ".0*\n.02\n", 0, "");
}

#[test]
fn test_n7() {
    sort_test(&["-n"], ".02\n.*\n", ".*\n.02\n", 0, "");
}

#[test]
fn test_n8a() {
    sort_test(&["-n", "-k1,1"], ".0a\n.0b\n", ".0a\n.0b\n", 0, "");
}

#[test]
fn test_n8b() {
    sort_test(&["-n", "-k1,1"], ".0b\n.0a\n", ".0b\n.0a\n", 0, "");
}

#[test]
fn test_n9a() {
    sort_test(&["-n", "-k1,1"], ".000a\n.000b\n", ".000a\n.000b\n", 0, "");
}

#[test]
fn test_n9b() {
    sort_test(&["-n", "-k1,1"], ".000b\n.000a\n", ".000b\n.000a\n", 0, "");
}

#[test]
fn test_n10a() {
    sort_test(&["-n", "-k1,1"], ".00a\n.000b\n", ".00a\n.000b\n", 0, "");
}

#[test]
fn test_n10b() {
    sort_test(&["-n", "-k1,1"], ".00b\n.000a\n", ".00b\n.000a\n", 0, "");
}

#[test]
fn test_n11a() {
    sort_test(&["-n", "-k1,1"], ".01a\n.010\n", ".01a\n.010\n", 0, "");
}

#[test]
fn test_n11b() {
    sort_test(&["-n", "-k1,1"], ".010\n.01a\n", ".010\n.01a\n", 0, "");
}

#[test]
fn test_02a() {
    sort_test(&["-c"], "A\nB\nC\n", "", 0, "");
}

#[test]
fn test_02b() {
    sort_test(
        &["-c"],
        "A\nC\nB\n",
        "",
        1,
        "The order of the lines is not correct on line 2:`C`\n",
    );
}

#[test]
fn test_02c() {
    sort_test(&["-c", "-k1,1"], "a\na b\n", "", 0, "");
}

#[test]
fn test_02d() {
    sort_test(&["-C"], "A\nB\nC\n", "", 0, "");
}

#[test]
fn test_02e() {
    sort_test(
        &["-C"],
        "A\nC\nB\n",
        "",
        1,
        "The order of the lines is not correct\n",
    );
}

#[test]
fn test_02m() {
    sort_test(&["-cu"], "A\nA\n", "", 1, "Duplicate key was found! `A`\n");
}

#[test]
fn test_02n() {
    sort_test(&["-cu"], "A\nB\n", "", 0, "");
}

#[test]
fn test_02o() {
    sort_test(
        &["-cu"],
        "A\nB\nB\n",
        "",
        1,
        "Duplicate key was found! `B`\n",
    );
}

#[test]
fn test_02p() {
    sort_test(
        &["-cu"],
        "B\nA\nB\n",
        "",
        1,
        "Duplicate key was found! `B`\n",
    );
}

#[test]
fn test_03a() {
    sort_test(&["-k1", "-"], "B\nA\n", "A\nB\n", 0, "");
}

#[test]
fn test_03b() {
    sort_test(&["-k1,1", "-"], "B\nA\n", "A\nB\n", 0, "");
}

#[test]
fn test_03c() {
    sort_test(&["-k1", "-k2", "-"], "A b\nA a\n", "A a\nA b\n", 0, "");
}

#[test]
fn test_03d() {
    // Fail with a diagnostic when -k specifies field == 0.
    sort_test(&["-k0", "-"], "", "", 1, "the key can't be zero.\n");
}

#[test]
fn test_04a() {
    sort_test(&["-nc", "-"], "2\n11\n", "", 0, "");
}

#[test]
fn test_04b() {
    sort_test(&["-n", "-"], "11\n2\n", "2\n11\n", 0, "");
}

#[test]
fn test_04c() {
    sort_test(&["-k1n", "-"], "11\n2\n", "2\n11\n", 0, "");
}

#[test]
fn test_04d() {
    sort_test(&["-k1", "-"], "11\n2\n", "11\n2\n", 0, "");
}

#[test]
fn test_04e() {
    sort_test(
        &["-k2", "-"],
        "ignored B\nz-ig A\n",
        "z-ig A\nignored B\n",
        0,
        "",
    );
}

#[test]
fn test_05a() {
    sort_test(&["-k1,2", "-"], "A B\nA A\n", "A A\nA B\n", 0, "");
}

#[test]
fn test_05b() {
    sort_test(&["-k1,2", "-"], "A B A\nA A Z\n", "A A Z\nA B A\n", 0, "");
}

#[test]
fn test_05c() {
    sort_test(
        &["-k1", "-k2", "-"],
        "A B A\nA A Z\n",
        "A A Z\nA B A\n",
        0,
        "",
    );
}

#[test]
fn test_05d() {
    sort_test(&["-k2,2", "-"], "A B A\nA A Z\n", "A A Z\nA B A\n", 0, "");
}

#[test]
fn test_05e() {
    sort_test(&["-k2,2", "-"], "A B Z\nA A A\n", "A A A\nA B Z\n", 0, "");
}

#[test]
fn test_05f() {
    sort_test(&["-k2,2", "-"], "A B A\nA A Z\n", "A A Z\nA B A\n", 0, "");
}

#[test]
fn test_07a() {
    sort_test(&["-k2,3", "-"], "9 a b\n7 a a\n", "7 a a\n9 a b\n", 0, "");
}

#[test]
fn test_07b() {
    sort_test(&["-k2,3"], "a a b\nz a a\n", "z a a\na a b\n", 0, "");
}

#[test]
fn test_07c() {
    sort_test(&["-k2,3", "-"], "y k b\nz k a\n", "z k a\ny k b\n", 0, "");
}

#[test]
fn test_07e() {
    // ensure a character position of 0 includes whole field
    sort_test(&["-k2,3.0", "-"], "a a b\nz a a\n", "z a a\na a b\n", 0, "");
}

#[test]
fn test_07f() {
    // ensure fields with end position before start are error
    sort_test(
        &["-n", "-k1.3,1.1", "-"],
        "a 2\nb 1\n",
        "",
        1,
        "keys fields with end position before start!\n",
    );
}

#[test]
fn test_08a() {
    // report an error for '.' without following char spec
    sort_test(
        &["-k", "2.,3", "-"],
        "",
        "",
        1,
        "cannot parse integer from empty string\n",
    );
}

#[test]
fn test_08b() {
    // report an error for ',' without following POS2
    sort_test(
        &["-k", "2,", "-"],
        "",
        "",
        1,
        "cannot parse integer from empty string\n",
    );
}

#[test]
fn test_09b() {
    sort_test(&["-n", "-"], "1e2\n2e1\n", "1e2\n2e1\n", 0, "");
}

#[test]
fn test_09c() {
    sort_test(&["-n", "-"], "2e1\n1e2\n", "1e2\n2e1\n", 0, "");
}

#[test]
fn test_10a() {
    sort_test(
        &["-t", ":", "-k2.2,2.2", "-"],
        ":ba\n:ab\n",
        ":ba\n:ab\n",
        0,
        "",
    );
}

#[test]
fn test_10c() {
    sort_test(
        &["-t", ":", "-k2.2,2.2", "-"],
        ":ab\n:ba\n",
        ":ba\n:ab\n",
        0,
        "",
    );
}

#[test]
fn test_10a0() {
    sort_test(&["-k2.3,2.3", "-"], "z ba\nz ab\n", "z ba\nz ab\n", 0, "");
}

#[test]
fn test_10a1() {
    sort_test(&["-k1.2,1.2", "-"], "ba\nab\n", "ba\nab\n", 0, "");
}

#[test]
fn test_10a2() {
    sort_test(
        &["-b", "-k2.2,2.2", "-"],
        "z ba\nz ab\n",
        "z ba\nz ab\n",
        0,
        "",
    );
}

#[test]
fn test_10e() {
    sort_test(&["-k1.2,1.2", "-"], "ab\nba\n", "ba\nab\n", 0, "");
}

#[test]
fn test_11a() {
    // Exercise bug re using -b to skip trailing blanks.
    sort_test(
        &["-t:", "-k1,1b", "-k2,2", "-"],
        "a\t:a\na :b\n",
        "a\t:a\na :b\n",
        0,
        "",
    );
}

#[test]
fn test_11b() {
    sort_test(
        &["-t:", "-k1,1b", "-k2,2", "-"],
        "a :b\na\t:a\n",
        "a\t:a\na :b\n",
        0,
        "",
    );
}

#[test]
fn test_11c() {
    sort_test(
        &["-t:", "-k2,2b", "-k3,3", "-"],
        "z:a\t:a\na :b\n",
        "z:a\t:a\na :b\n",
        0,
        "",
    );
}

#[test]
fn test_11d() {
    sort_test(
        &["-t:", "-k2,2b", "-k3,3", "-"],
        "z:a :b\na\t:a\n",
        "a\t:a\nz:a :b\n",
        0,
        "",
    );
}

#[test]
fn test_14a() {
    sort_test(
        &["-d", "-u", "-"],
        "mal\nmal-\nmala\n",
        "mal\nmala\n",
        0,
        "",
    );
}

#[test]
fn test_14b() {
    sort_test(
        &["-f", "-d", "-u", "-"],
        "mal\nmal-\nmala\n",
        "mal\nmala\n",
        0,
        "",
    );
}

#[test]
fn test_15a() {
    sort_test(&["-i", "-u", "-"], "a\na\t\n", "a\n", 0, "");
}

#[test]
fn test_15b() {
    sort_test(&["-i", "-u", "-"], "a\n\ta\n", "a\n", 0, "");
}

#[test]
fn test_15c() {
    sort_test(&["-i", "-u", "-"], "a\t\na\n", "a\t\n", 0, "");
}

#[test]
fn test_15d() {
    sort_test(&["-i", "-u", "-"], "\ta\na\n", "\ta\n", 0, "");
}

#[test]
fn test_15e() {
    sort_test(&["-i", "-u", "-"], "a\n\t\t\t\t\ta\t\t\t\t\n", "a\n", 0, "");
}

#[test]
fn test_18a() {
    sort_test(&["-k1.1,1.2n", "-"], " 901\n100\n", " 901\n100\n", 0, "");
}

#[test]
fn test_18b() {
    sort_test(
        &["-b", "-k1.1,1.2n", "-"],
        " 901\n100\n",
        " 901\n100\n",
        0,
        "",
    );
}

#[test]
fn test_18c() {
    sort_test(&["-k1.1,1.2nb", "-"], " 901\n100\n", "100\n 901\n", 0, "");
}

#[test]
fn test_18d() {
    sort_test(&["-k1.1b,1.2n", "-"], " 901\n100\n", " 901\n100\n", 0, "");
}

#[test]
fn test_18e() {
    sort_test(
        &["-nb", "-k1.1,1.2", "-"],
        " 901\n100\n",
        "100\n 901\n",
        0,
        "",
    );
}

#[test]
fn test_18f() {
    sort_test(&["-k1,1b", "-"], "a  y\na z\n", "a  y\na z\n", 0, "");
}
#[test]
fn test_19b() {
    sort_test(
        &["-k1,1", "-k2nr", "-"],
        "b 2\nb 1\nb 3\n",
        "b 3\nb 2\nb 1\n",
        0,
        "",
    );
}

#[test]
fn test_20a() {
    sort_test(
        &["-"],
        "_________U__free\n_________U__malloc\n_________U__abort\n\
         _________U__memcpy\n_________U__memset\n_________U_dyld_stub_binding_helper\n\
         _________U__malloc\n_________U___iob\n_________U__abort\n_________U__fprintf\n",
        "_________U___iob\n_________U__abort\n_________U__abort\n\
         _________U__fprintf\n_________U__free\n_________U__malloc\n\
         _________U__malloc\n_________U__memcpy\n_________U__memset\n\
         _________U_dyld_stub_binding_helper\n",
        0,
        "",
    );
}

#[test]
fn test_21a() {
    sort_test(&["-"], "A\na\n_\n", "A\n_\na\n", 0, "");
}

#[test]
fn test_21b() {
    sort_test(&["-f", "-"], "A\na\n_\n", "A\na\n_\n", 0, "");
}

#[test]
fn test_21c() {
    sort_test(&["-f", "-"], "a\nA\n_\n", "A\na\n_\n", 0, "");
}

#[test]
fn test_21d() {
    sort_test(&["-f", "-"], "_\na\nA\n", "A\na\n_\n", 0, "");
}

#[test]
fn test_21e() {
    sort_test(&["-f", "-"], "a\n_\nA\n", "A\na\n_\n", 0, "");
}

#[test]
fn test_21g() {
    sort_test(&["-f", "-u", "-"], "a\n_\n", "a\n_\n", 0, "");
}

#[test]
fn test_22a() {
    sort_test(
        &["-k2,2fd", "-k1,1r", "-"],
        "3 b\n4 B\n",
        "4 B\n3 b\n",
        0,
        "",
    );
}

#[test]
fn test_neg_nls() {
    sort_test(&["-n", "-"], "-1\n-9\n", "-9\n-1\n", 0, "");
}

#[test]
fn test_nul_nls() {
    sort_test(&["-"], "\0b\n\0a\n", "\0a\n\0b\n", 0, "");
}

#[test]
fn test_use_nl() {
    sort_test(&["-"], "\n\t\n", "\n\t\n", 0, "");
}

#[test]
fn test_files_sort_1() {
    sort_test(
        &["tests/assets/empty_line.txt", "tests/assets/in_uniq"],
        "",
        "\n\n\n\nXX\nXX\nXX\nYY\nYY\nYY\na\na\nb\nb\nc\nd\nd\nd\nline 1\nline 3\n",
        0,
        "",
    );
}

#[test]
fn test_files_sort_2() {
    sort_test(
        &["-n", "tests/assets/in_seq", "tests/assets/test_file.txt"],
        "",
        "1\n1sdfghnm\n2\n2sadsgdhjmf\n3\n3zcxbncvm vbm\n4\n4asdbncv\n5\n5adsbfdgfnfm\n6\n6sdfcvncbmcg\n7zsdgdgfndcgmncg\n8asdbsfdndcgmn\n9sfbdxgfndcgmncgmn\n10dvsd\n11\n12\n13\n14\n15\n16\n17\n",
        0,
        "",
    );
}
