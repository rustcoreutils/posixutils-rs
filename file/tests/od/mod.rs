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

// The trailing '!' is a one-byte tail. Extended with three null bytes it is
// the denormal 0x00000021, not zero -- the `0e0` this used to assert was the
// `_ => 0.0` arm of a ladder that had no case for a tail shorter than four
// bytes, which is what the "does not match other implementations" comment
// here was recording. The notation is `%g` over the shortest round-trip
// decimal of an f32, in aligned 16-column fields.
#[test]
fn test_od_16() {
    od_test(
        &["-tf4"],
        "Hello, World!",
        "\
0000000   1.1431391e+27    1.761127e+14    1.744671e+22         4.6e-44
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
    // A broken pipe here is a result, not a harness failure: od rejects a
    // malformed `-t` before it opens the input at all, so it has already
    // exited. Swallow only that, and let the exit code and stderr carry the
    // verdict.
    match child.stdin.as_mut().unwrap().write_all(stdin) {
        Ok(()) => {}
        Err(e) if e.kind() == std::io::ErrorKind::BrokenPipe => {}
        Err(e) => panic!("writing od's stdin: {e}"),
    }
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
        stderr.contains("fL") || stderr.contains("16") || stderr.contains("long double"),
        "the diagnostic must name what was refused: {stderr:?}"
    );
}

// A malformed `-t` size must be diagnosed, never guessed at and never a panic.
//
// `parse_type_bytes` ended in `s.parse().unwrap_or(default_bytes)`, so every
// unparseable size fell back to the default without a word: `-t x9z` and
// `-t xZ` both behaved as `-t x2`. Silent acceptance is the defect, not the
// missing size. And the `matches!(num_bytes, 1|2|4|8)` gate ran *after*
// `local_buf.chunks(num_bytes)`, so `-t x0` reached `chunks(0)` and panicked
// with "chunk size must be non-zero".
#[test]
fn od_diagnoses_a_malformed_type_size_rather_than_guessing() {
    for spec in [
        "x0", "f0", "u0", "x9z", "xZ", "fZ", "x99", "x3", "f3", "fL8",
    ] {
        let (stdout, stderr, code) = od_raw(&["-An", "-t", spec], b"ABCD");
        assert_ne!(code, Some(0), "-t {spec} must be refused, not guessed at");
        assert!(
            !stderr.contains("panicked at"),
            "-t {spec} panicked instead of diagnosing: {stderr:?}"
        );
        assert!(
            stdout.is_empty(),
            "-t {spec}: no output on refusal: {stdout:?}"
        );
        assert!(
            stderr.starts_with("od: ") && stderr.ends_with('\n'),
            "-t {spec}: malformed diagnostic: {stderr:?}"
        );
    }
}

// POSIX 109075-109076: "Multiple types can be concatenated within the same
// type_string". The spec's own example at 109237 is `od -A o -t o2x2x -N 18`,
// three types in one string.
//
// The parser took `chars.next()` as the type character and the whole rest as
// the size, so `o2x2x` resolved to one type whose size failed to parse and
// silently became the default, and `x1c` became `x2`.
#[test]
fn od_concatenated_types_in_one_type_string() {
    // Concatenation is defined as equivalent to separate `-t` options, which is
    // the whole of the claim 109075-6 makes. Asserted as an equivalence rather
    // than against fixed text, so it pins the parsing without also pinning the
    // column widths -- those are a separate question, and unspecified besides
    // (109189-92 asks only for "one or more <blank> characters").
    let (concat, _, code) = od_raw(&["-An", "-t", "x1c"], b"AB");
    assert_eq!(code, Some(0));
    let (separate, _, _) = od_raw(&["-An", "-t", "x1", "-t", "c"], b"AB");
    assert_eq!(concat, separate, "`-t x1c` must equal `-t x1 -t c`");

    // Two types means two lines, and `x1`/`c` are both byte-oriented, so the
    // values themselves say nothing about byte order.
    let lines: Vec<&str> = concat.lines().collect();
    assert_eq!(lines.len(), 2, "two types, two lines: {concat:?}");
    assert_eq!(
        lines[0].split_whitespace().collect::<Vec<_>>(),
        ["41", "42"]
    );
    assert_eq!(lines[1].split_whitespace().collect::<Vec<_>>(), ["A", "B"]);

    // The spec's own three-type example. Asserted structurally -- three output
    // lines per 16-byte block -- so it holds on either byte order.
    let (stdout, _, code) = od_raw(&["-A", "o", "-t", "o2x2x"], b"4.3 BSD UNIX #34ab");
    assert_eq!(code, Some(0));
    let lines: Vec<&str> = stdout.lines().collect();
    assert_eq!(
        lines.len(),
        7,
        "three types x two blocks plus the final offset: {stdout:?}"
    );
    assert!(lines[0].starts_with("0000000"), "{stdout:?}");
    assert!(lines[3].starts_with("0000020"), "{stdout:?}");
    assert_eq!(lines[6], "0000022", "{stdout:?}");
}

// POSIX 109141-109143: for `d`, `o`, `u` and `x` the default size is "the size
// of the underlying implementation's basic integer type" -- `int`, 4 bytes.
// 109152-109154: for `f` it is the basic double precision type -- 8 bytes.
//
// The defaults were 2 and 4, so a bare `-t x` printed eight 2-byte fields
// where POSIX and every other od print four 4-byte ones.
//
// Asserted by field count rather than by value, which needs no byte order.
#[test]
fn od_default_type_sizes_are_the_basic_c_types() {
    let sixteen = b"ABCDEFGHIJKLMNOP";
    for (type_char, want) in [("d", 4), ("u", 4), ("x", 4), ("o", 4), ("f", 2)] {
        let (bare, _, code) = od_raw(&["-An", "-t", type_char], sixteen);
        assert_eq!(code, Some(0), "-t {type_char}");
        assert_eq!(
            bare.split_whitespace().count(),
            want,
            "-t {type_char} must convert 16 bytes into {want} items: {bare:?}"
        );

        // And the bare form must be exactly its explicit spelling.
        let sized = format!("{type_char}{}", 16 / want);
        let (explicit, _, _) = od_raw(&["-An", "-t", &sized], sixteen);
        assert_eq!(bare, explicit, "-t {type_char} must equal -t {sized}");
    }
}

// POSIX 109193-109195: "If, as a result of the specification of the -N option
// or end-of-file being reached on the last input file, input data only
// partially satisfies an output type, the input shall be extended sufficiently
// with null bytes to write the last byte of the input."
//
// Every formatter zero-extended a short final chunk at the wrong end for the
// lengths that needed a wider array -- 3, 5, 6 and 7. `arr[3..]` then
// `arr.reverse()` puts the padding in the *low* bytes, so a 5-byte tail under
// `-t x8` came out as the correct value shifted left by 24 bits.
//
// Asserted as the equivalence the clause itself states: a short final chunk
// must render exactly as those same bytes followed by explicit NULs. That
// compares the short-chunk path against the full-chunk path, needs no
// reference implementation, and holds on either byte order.
#[test]
fn od_short_final_chunk_is_extended_with_null_bytes() {
    for (spec, width) in [
        ("x8", 8),
        ("u8", 8),
        ("d8", 8),
        ("o8", 8),
        ("x4", 4),
        ("u4", 4),
        ("d4", 4),
        ("o4", 4),
        ("x2", 2),
        ("u2", 2),
        ("d2", 2),
        ("o2", 2),
        ("f8", 8),
        ("f4", 4),
    ] {
        for tail in 1..width {
            // High bytes throughout, so sign extension and high-order padding
            // are both exercised rather than reading as small positives.
            let data: Vec<u8> = (0..width + tail)
                .map(|i| 0x80u8 | ((i as u8).wrapping_mul(29) & 0x7f))
                .collect();
            let mut padded = data.clone();
            padded.resize(2 * width, 0);

            let (short, _, code) = od_raw(&["-An", "-t", spec], &data);
            assert_eq!(code, Some(0), "-t {spec} with a {tail}-byte tail");
            let (full, _, _) = od_raw(&["-An", "-t", spec], &padded);
            assert_eq!(
                short.split_whitespace().collect::<Vec<_>>(),
                full.split_whitespace().collect::<Vec<_>>(),
                "-t {spec}: a {tail}-byte tail must render as those bytes plus NULs"
            );
        }
    }
}

// The extension is by null bytes, so the *declared* width decides the sign, not
// the number of bytes actually present. `-t d4` with a one-byte tail of 0xc7
// read it as an i8 and printed -57; extended to four bytes it is a positive
// i32 on a little-endian host, and a large negative one on a big-endian host
// where 0xc7 lands in the most significant byte.
#[test]
fn od_short_chunk_sign_follows_the_declared_width() {
    let data = [0x41u8, 0x42, 0x43, 0x44, 0xc7];
    let (stdout, _, code) = od_raw(&["-An", "-t", "d4"], &data);
    assert_eq!(code, Some(0));

    let want = if cfg!(target_endian = "little") {
        "199"
    } else {
        "-956301312"
    };
    let fields: Vec<&str> = stdout.split_whitespace().collect();
    assert_eq!(
        fields.last().copied(),
        Some(want),
        "0xc7 extended to four bytes: {stdout:?}"
    );
}

// The float conversion must be chosen by the declared type, not by how many
// bytes happen to be left.
//
// `FFormatter` matched on `chunk.len()` and ignored `num_bytes` entirely (the
// parameter was spelled `_num_bytes`), so a four-byte tail of a `-t f8` run
// fell into the `4 =>` arm and was decoded as an f32 -- a different number
// altogether, not a rounding difference. Tails of one to three bytes had no
// arm at all and silently produced zero.
#[test]
fn od_float_conversion_is_chosen_by_the_type_not_the_tail() {
    // Twelve bytes under `-t f8`: one full double, then a four-byte tail.
    let data: Vec<u8> = (0..12u8)
        .map(|i| 0x80 | (i.wrapping_mul(29) & 0x7f))
        .collect();

    let (short, _, code) = od_raw(&["-An", "-t", "f8"], &data);
    assert_eq!(code, Some(0));

    // The tail is a double built from those four bytes plus four NULs, which
    // is what `-t f8` on the explicitly padded input gives.
    let mut padded = data.clone();
    padded.resize(16, 0);
    let (full, _, _) = od_raw(&["-An", "-t", "f8"], &padded);
    assert_eq!(
        short.split_whitespace().collect::<Vec<_>>(),
        full.split_whitespace().collect::<Vec<_>>(),
        "a 4-byte tail of -t f8 is a double, not a float: {short:?}"
    );

    // And it is emphatically not the f32 reading of the same four bytes.
    let (as_float, _, _) = od_raw(&["-An", "-t", "f4"], &data[8..]);
    assert_ne!(
        short.split_whitespace().last(),
        as_float.split_whitespace().last(),
        "-t f8 must not decode its tail as an f32"
    );

    // A one-byte tail is a very small denormal, never a zero.
    let (tiny, _, _) = od_raw(&["-An", "-t", "f4"], &[0x21u8]);
    let field = tiny.split_whitespace().next().unwrap_or("");
    assert!(
        field.parse::<f64>().is_ok_and(|v| v != 0.0),
        "a 1-byte tail must extend to a denormal, not zero: {tiny:?}"
    );
}

// `-t f` prints the shortest decimal that reads back as the same value,
// rendered by C's `%g` rules -- scientific when the decimal exponent is below
// -4 or is not less than the number of significant digits, positional
// otherwise -- with the exponent signed and at least two digits wide. Fields
// are right-aligned so the columns line up.
//
// The old code was `format!(" {value:e}")`: Rust's exponent form, always, with
// no field width. That prints `1e0` for 1.0 and `3.4028235e38` without the
// exponent's sign, matches no other od, and leaves the columns ragged.
//
// Every expectation below is GNU od's output for the same bytes.
#[test]
fn od_float_output_is_shortest_round_trip_in_g_format() {
    // Assembled little-endian and cfg-branched, since these are float *bit
    // patterns* being fed to od, not values od computes.
    fn le32(bits: u32) -> [u8; 4] {
        if cfg!(target_endian = "little") {
            bits.to_le_bytes()
        } else {
            bits.to_be_bytes()
        }
    }
    fn le64(bits: u64) -> [u8; 8] {
        if cfg!(target_endian = "little") {
            bits.to_le_bytes()
        } else {
            bits.to_be_bytes()
        }
    }

    let f32_cases: &[(u32, &str)] = &[
        (0x3f80_0000, "1"),
        (0x3dcc_cccd, "0.1"),
        (0x4049_0fdb, "3.1415927"),
        (0x7f7f_ffff, "3.4028235e+38"),
        (0x0000_0001, "1e-45"),
        (0x4974_2400, "1e+06"),
        (0x4b18_9680, "1e+07"),
        (0x3a83_126f, "0.001"),
        (0x3927_c5ac, "0.00016"),
        (0x7f80_0000, "inf"),
        (0xff80_0000, "-inf"),
        (0x7fc0_0000, "nan"),
        (0x8000_0000, "-0"),
        (0x0000_0000, "0"),
        (0xbf80_0000, "-1"),
    ];
    for &(bits, want) in f32_cases {
        let (stdout, _, code) = od_raw(&["-An", "-t", "f4"], &le32(bits));
        assert_eq!(code, Some(0), "f4 {bits:#010x}");
        assert_eq!(
            stdout.split_whitespace().next(),
            Some(want),
            "f4 {bits:#010x}: {stdout:?}"
        );
    }

    let f64_cases: &[(u64, &str)] = &[
        (0x3ff0_0000_0000_0000, "1"),
        (0x3fb9_9999_9999_999a, "0.1"),
        (0x7fef_ffff_ffff_ffff, "1.7976931348623157e+308"),
        (0x0000_0000_0000_0001, "5e-324"),
        (0x430c_6bf5_2634_0000, "1e+15"),
        (0x7ff0_0000_0000_0000, "inf"),
        (0x7ff8_0000_0000_0000, "nan"),
    ];
    for &(bits, want) in f64_cases {
        let (stdout, _, code) = od_raw(&["-An", "-t", "f8"], &le64(bits));
        assert_eq!(code, Some(0), "f8 {bits:#018x}");
        assert_eq!(
            stdout.split_whitespace().next(),
            Some(want),
            "f8 {bits:#018x}: {stdout:?}"
        );
    }
}

// Float fields are a fixed width so the columns line up, as they already do
// for every integer type. GNU uses 16 columns for a 4-byte float and 25 for an
// 8-byte one, counting the separating blank.
#[test]
fn od_float_fields_are_column_aligned() {
    let four_floats: Vec<u8> = (0..4)
        .flat_map(|i| {
            let bits: u32 = [0x3f80_0000, 0x3dcc_cccd, 0x4049_0fdb, 0x7f7f_ffff][i];
            if cfg!(target_endian = "little") {
                bits.to_le_bytes()
            } else {
                bits.to_be_bytes()
            }
        })
        .collect();

    let (stdout, _, code) = od_raw(&["-An", "-t", "f4"], &four_floats);
    assert_eq!(code, Some(0));
    let line = stdout.lines().next().unwrap_or("");
    assert_eq!(line.len(), 4 * 16, "four 16-column fields: {line:?}");
    assert_eq!(
        line,
        "               1             0.1       3.1415927   3.4028235e+38"
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
