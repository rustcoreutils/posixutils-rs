//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, run_test_u8, TestPlan, TestPlanU8};

fn od_test(args: &[&str], test_data: &str, expected_output: &str) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("od"),
        args: str_args,
        stdin_data: String::from(test_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn test_od_0() {
    od_test(
        &["-tc", "-An"],
        "\x07",
        r#"  \a
"#,
    );
}

#[test]
fn test_od_1() {
    od_test(&["-c", "-j1", "-An"], "a", "");
}

#[test]
fn test_od_2() {
    od_test(&["-c", "-j3", "-An"], "abc", "");
}

#[test]
fn test_od_3() {
    od_test(&["-tc", "-j3", "-An"], "abcd", "   d\n");
}

#[test]
fn test_od_4() {
    od_test(
        &[],
        "Hello, World!",
        "\
0000000 062510 066154 026157 053440 071157 062154 000041
0000015
",
    );
}

#[test]
fn test_od_5() {
    od_test(
        &["-b"],
        "Hello, World!",
        "\
0000000 110 145 154 154 157 054 040 127 157 162 154 144 041
0000015
",
    );
}

#[test]
fn test_od_6() {
    od_test(
        &["-c"],
        "Hello, World!",
        "\
0000000   H   e   l   l   o   ,       W   o   r   l   d   !
0000015
",
    );
}

#[test]
fn test_od_7() {
    od_test(
        &["-d"],
        "Hello, World!",
        "\
0000000 25928 27756 11375 22304 29295 25708    33
0000015
",
    );
}

#[test]
fn test_od_8() {
    od_test(
        &["-Ad"],
        "Hello, World!",
        "\
0000000 062510 066154 026157 053440 071157 062154 000041
0000013
",
    );
}

#[test]
fn test_od_9() {
    od_test(
        &["-An"],
        "Hello, World!",
        " 062510 066154 026157 053440 071157 062154 000041\n",
    );
}

#[test]
fn test_od_10() {
    od_test(
        &["-Ao"],
        "Hello, World!",
        "\
0000000 062510 066154 026157 053440 071157 062154 000041
0000015
",
    );
}

#[test]
fn test_od_11() {
    od_test(
        &["-Ax"],
        "Hello, World!",
        "\
000000 062510 066154 026157 053440 071157 062154 000041
00000d
",
    );
}

#[test]
fn test_od_12() {
    od_test(
        &["-j7"],
        "Hello, World!",
        "\
0000007 067527 066162 020544
0000015
",
    );
}

#[test]
fn test_od_13() {
    od_test(
        &["-N5"],
        "Hello, World!",
        "\
0000000 062510 066154 000157
0000005
",
    );
}

#[test]
fn test_od_14() {
    od_test(
        &["-o"],
        "Hello, World!",
        "\
0000000 062510 066154 026157 053440 071157 062154 000041
0000015
",
    );
}

#[test]
fn test_od_15() {
    od_test(
        &["-s"],
        "Hello, World!",
        "\
0000000  25928  27756  11375  22304  29295  25708     33
0000015
",
    );
}

// TODO
// Does not match other implementations
#[test]
fn test_od_16() {
    od_test(
        &["-tf4"],
        "Hello, World!",
        "\
0000000 1.1431391224375825e27 1.7611270127616e14 1.7446709643352771e22 0e0
0000015
",
    );
}

#[test]
fn test_od_17() {
    od_test(
        &["-td2"],
        "Hello, World!",
        "\
0000000  25928  27756  11375  22304  29295  25708     33
0000015
",
    );
}

#[test]
fn test_od_18() {
    od_test(
        &["-v"],
        "Hello, World!",
        "\
0000000 062510 066154 026157 053440 071157 062154 000041
0000015
",
    );
}

#[test]
fn test_od_19() {
    od_test(
        &["-x"],
        "Hello, World!",
        "\
0000000 6548 6c6c 2c6f 5720 726f 646c 0021
0000015
",
    );
}

#[test]
fn test_od_20() {
    od_test(
        &["-Ax", "-td4", "-v"],
        "Hello, World!",
        "\
000000  1819043144  1461726319  1684828783          33
00000d
",
    );
}

#[test]
fn test_od_21() {
    od_test(
        &["-j0x10"],
        "Hello, World!123456",
        "\
0000020 032464 000066
0000023
",
    );
}

#[test]
fn test_od_22() {
    od_test(
        &["-tu4"],
        "Hello, World!1",
        "\
0000000 1819043144 1461726319 1684828783      12577
0000016
",
    );
}

// POSIX does not specify that '-A n' should result in a leading space being printed, or that it should not.
// Given that most other implementations of `od` print one or more leading spaces, for compatibility, one
// leading space will be printed.
//
// BusyBox, GNU Core Utilities, uutils's coreutils: 1 leading space
// toybox: 2 leading spaces
//
// Also, none of these implementations print trailing spaces
#[test]
fn test_od_a_n_t_x_one() {
    od_test(
        &["-A", "n", "-t", "x1"],
        "This is a suite of Rust-native core command line utilities",
        " 54 68 69 73 20 69 73 20 61 20 73 75 69 74 65 20
 6f 66 20 52 75 73 74 2d 6e 61 74 69 76 65 20 63
 6f 72 65 20 63 6f 6d 6d 61 6e 64 20 6c 69 6e 65
 20 75 74 69 6c 69 74 69 65 73
",
    );
}

#[test]
fn test_od_a_x_t_a() {
    od_test(
        &["-A", "x", "-t", "a"],
        "Hello, World!",
        "\
000000   H   e   l   l   o   ,  sp   W   o   r   l   d   !
00000d
",
    );
}

// -c must use C-style escapes (\t, \n, \0), NOT named characters (HT, NL, NUL).
#[test]
fn test_od_c_uses_c_escapes() {
    od_test(&["-c", "-An"], "A\tB\nC", "   A  \\t   B  \\n   C\n");
}

#[test]
fn test_od_c_nul_is_backslash_zero() {
    od_test(&["-c", "-An"], "X\0Y", "   X  \\0   Y\n");
}

// -c is equivalent to -t c.
#[test]
fn test_od_c_equals_t_c() {
    let data = "A\tB\0";
    od_test(&["-c", "-An"], data, "   A  \\t   B  \\0\n");
    od_test(&["-t", "c", "-An"], data, "   A  \\t   B  \\0\n");
}

// OD-2: multiple short type options accumulate (one line per type).
#[test]
fn test_od_multiple_short_types() {
    od_test(&["-An", "-b", "-c"], "AB", " 101 102\n   A   B\n");
}

// OD-4: C/S/I/L type size suffixes.
#[test]
fn test_od_type_size_long() {
    od_test(
        &["-An", "-t", "dL"],
        "\x01\x00\x00\x00\x00\x00\x00\x00",
        "                    1\n",
    );
}

// OD-3: -t a uses only the least-significant seven bits (0x8A & 0x7F == 0x0A).
#[test]
fn test_od_named_char_seven_bit() {
    run_test_u8(TestPlanU8 {
        cmd: String::from("od"),
        args: vec![String::from("-An"), String::from("-t"), String::from("a")],
        stdin_data: vec![0x8a],
        expected_out: b"  nl\n".to_vec(),
        expected_err: Vec::new(),
        expected_exit_code: 0,
    });
}

// OD-5: the obsolescent "[+]offset" operand sets the start offset and is not
// opened as a file.
#[test]
fn test_od_plus_offset_operand() {
    od_test(
        &["-b", "+2"],
        "ABCDEFGH",
        "0000002 103 104 105 106 107 110\n0000010\n",
    );
}

// POSIX (od, l. 109177): for -t c a <backslash> byte "shall be written as a
// single <backslash>" -- it is explicitly exempt from the escape table. It is
// still right-aligned in the same 4-column field as every other conversion, so
// emitting a 3-character field shifts the rest of the line left by one column.
#[test]
fn test_od_c_backslash_column_alignment() {
    od_test(&["-c", "-An"], "a\\b", "   a   \\   b\n");
    od_test(&["-t", "c", "-An"], "a\\b", "   a   \\   b\n");
}

// The column shift is cumulative across a full 16-byte line.
#[test]
fn test_od_c_backslash_full_line() {
    let data = "\\".repeat(16);
    let expected = format!("{}\n", "   \\".repeat(16));
    od_test(&["-c", "-An"], &data, &expected);
}

// With the address column present, a regression also shows up as a misaligned
// dump rather than only as a short line.
#[test]
fn test_od_c_backslash_with_offsets() {
    od_test(&["-c"], "\\abc", "0000000   \\   a   b   c\n0000004\n");
}

// -t a is unaffected: a <backslash> has no character name, so it takes the
// graphic-character path. This pins the a/c distinction.
#[test]
fn test_od_a_backslash_unaffected() {
    od_test(&["-t", "a", "-An"], "a\\b", "   a   \\   b\n");
}

// OD-6: skipping past the end of input is a diagnostic error (exit > 0).
#[test]
fn test_od_skip_past_eof() {
    run_test_u8(TestPlanU8 {
        cmd: String::from("od"),
        args: vec![String::from("-j"), String::from("100")],
        stdin_data: b"ab".to_vec(),
        expected_out: Vec::new(),
        // Prefixed and newline-terminated: this expectation used to encode the
        // defect, asserting a bare message with no `od:` and no line ending.
        expected_err: b"od: cannot skip past end of input\n".to_vec(),
        expected_exit_code: 1,
    });
}

// ---------------------------------------------------------------------------
// -t f size suffixes (POSIX 109071-109072)
// ---------------------------------------------------------------------------

/// Run od on raw bytes and return (stdout, stderr, exit code).
fn od_raw(args: &[&str], stdin: &[u8]) -> (String, String, Option<i32>) {
    use std::io::Write;
    let mut child = std::process::Command::new(env!("CARGO_BIN_EXE_od"))
        .args(args)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .expect("spawn od");
    child.stdin.as_mut().unwrap().write_all(stdin).unwrap();
    let out = child.wait_with_output().expect("wait od");
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.code(),
    )
}

// POSIX 109071-2: "The type specification character f can be followed by an
// optional F, D, or L indicating that the conversion should be applied to an
// item of type float, double, or long double".
//
// `parse_type_bytes` had one size table, the C/S/I/L one that belongs to
// d/o/u/x, so `D` matched nothing, fell through a failing `s.parse()`, and
// silently took the 4-byte default: eight bytes of an IEEE double printed as
// two floats rather than one double, with no diagnostic. `fF` was right only
// by accident, and `fL` gave 8 where POSIX and GNU say long double.
#[test]
fn od_float_size_suffix_selects_the_right_width() {
    // IEEE-754 1.0, little-endian.
    let f64_one = 1.0f64.to_le_bytes();
    let f32_one = 1.0f32.to_le_bytes();

    // `fD` must read all eight bytes as one double, exactly as `f8` does.
    let (by_letter, _, code) = od_raw(&["-An", "-t", "fD"], &f64_one);
    assert_eq!(code, Some(0));
    let (by_number, _, _) = od_raw(&["-An", "-t", "f8"], &f64_one);
    assert_eq!(
        by_letter.split_whitespace().count(),
        1,
        "fD must yield one item, got {by_letter:?}"
    );
    assert_eq!(by_letter, by_number, "fD must agree with f8");

    // `fF` is float, and must agree with `f4`.
    let (by_letter, _, _) = od_raw(&["-An", "-t", "fF"], &f32_one);
    let (by_number, _, _) = od_raw(&["-An", "-t", "f4"], &f32_one);
    assert_eq!(by_letter, by_number, "fF must agree with f4");
    assert_eq!(by_letter.split_whitespace().count(), 1);
}

#[test]
fn od_long_double_is_refused_rather_than_silently_narrowed() {
    // `L` on `f` means long double, which this od does not format. It used to
    // be read as 8 bytes -- a double wearing the wrong name. Refusing says so.
    let sixteen = [0u8; 16];
    let (stdout, stderr, code) = od_raw(&["-An", "-t", "fL"], &sixteen);
    assert_ne!(code, Some(0), "fL must not silently produce doubles");
    assert!(stdout.is_empty(), "no output on refusal: {stdout:?}");
    assert!(
        stderr.contains("f16") || stderr.contains("long double"),
        "the diagnostic must name what was refused: {stderr:?}"
    );
}

#[test]
fn od_integer_size_suffixes_are_unchanged() {
    // The C/S/I/L table belongs to d/o/u/x (POSIX 109073-5) and must not have
    // moved when `f` got its own.
    let bytes = [1u8, 0, 0, 0, 0, 0, 0, 0];
    for (letter, number) in [("dC", "d1"), ("dS", "d2"), ("dI", "d4"), ("dL", "d8")] {
        let (by_letter, _, _) = od_raw(&["-An", "-t", letter], &bytes);
        let (by_number, _, _) = od_raw(&["-An", "-t", number], &bytes);
        assert_eq!(by_letter, by_number, "{letter} must agree with {number}");
    }
}

// A diagnostic must name the utility and the file, and end its line.
//
// `main` did `eprint!("{}", err)` on a bare `io::Error` propagated by `?` from
// `File::open`: no `od:` prefix, no filename, and no trailing newline, so the
// message ran into whatever printed next.
#[test]
fn od_names_itself_and_the_file_and_ends_the_line() {
    let (stdout, stderr, code) = od_raw(&["/nonexistent_od_probe"], b"");
    assert_eq!(code, Some(1));
    assert!(stdout.is_empty(), "no output on failure: {stdout:?}");
    assert!(
        stderr.starts_with("od: "),
        "must name the utility: {stderr:?}"
    );
    assert!(
        stderr.contains("/nonexistent_od_probe"),
        "must name the file: {stderr:?}"
    );
    assert!(
        stderr.ends_with('\n'),
        "must end the line, or it runs into the next output: {stderr:?}"
    );
    assert_eq!(stderr.lines().count(), 1, "one line: {stderr:?}");
}
