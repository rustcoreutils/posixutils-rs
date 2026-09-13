//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, run_test_u8, TestPlan, TestPlanU8};

/// Pick the expectation for the host's byte order.
///
/// od interprets a multi-byte field in the order the system stores one
/// (POSIX 109149-109151), so every expectation below that covers a type wider
/// than a byte has two forms. Branching them keeps the test meaningful on a
/// big-endian host rather than merely skipped there; the big-endian forms were
/// generated from a model of the field layout that was first validated against
/// this binary on little-endian.
fn by_endian<'a>(little: &'a str, big: &'a str) -> &'a str {
    if cfg!(target_endian = "little") {
        little
    } else {
        big
    }
}

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
        by_endian(
            "0000000 062510 066154 026157 053440 071157 062154 000041\n0000015\n",
            "0000000 044145 066154 067454 020127 067562 066144 020400\n0000015\n",
        ),
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
        by_endian(
            "0000000 25928 27756 11375 22304 29295 25708    33\n0000015\n",
            "0000000 18533 27756 28460  8279 28530 27748  8448\n0000015\n",
        ),
    );
}

#[test]
fn test_od_8() {
    od_test(
        &["-Ad"],
        "Hello, World!",
        by_endian(
            "0000000 062510 066154 026157 053440 071157 062154 000041\n0000013\n",
            "0000000 044145 066154 067454 020127 067562 066144 020400\n0000013\n",
        ),
    );
}

#[test]
fn test_od_9() {
    od_test(
        &["-An"],
        "Hello, World!",
        by_endian(
            " 062510 066154 026157 053440 071157 062154 000041\n",
            " 044145 066154 067454 020127 067562 066144 020400\n",
        ),
    );
}

#[test]
fn test_od_10() {
    od_test(
        &["-Ao"],
        "Hello, World!",
        by_endian(
            "0000000 062510 066154 026157 053440 071157 062154 000041\n0000015\n",
            "0000000 044145 066154 067454 020127 067562 066144 020400\n0000015\n",
        ),
    );
}

#[test]
fn test_od_11() {
    od_test(
        &["-Ax"],
        "Hello, World!",
        by_endian(
            "000000 062510 066154 026157 053440 071157 062154 000041\n00000d\n",
            "000000 044145 066154 067454 020127 067562 066144 020400\n00000d\n",
        ),
    );
}

#[test]
fn test_od_12() {
    od_test(
        &["-j7"],
        "Hello, World!",
        by_endian(
            "0000007 067527 066162 020544\n0000015\n",
            "0000007 053557 071154 062041\n0000015\n",
        ),
    );
}

#[test]
fn test_od_13() {
    od_test(
        &["-N5"],
        "Hello, World!",
        by_endian(
            "0000000 062510 066154 000157\n0000005\n",
            "0000000 044145 066154 067400\n0000005\n",
        ),
    );
}

#[test]
fn test_od_14() {
    od_test(
        &["-o"],
        "Hello, World!",
        by_endian(
            "0000000 062510 066154 026157 053440 071157 062154 000041\n0000015\n",
            "0000000 044145 066154 067454 020127 067562 066144 020400\n0000015\n",
        ),
    );
}

#[test]
fn test_od_15() {
    od_test(
        &["-s"],
        "Hello, World!",
        by_endian(
            "0000000  25928  27756  11375  22304  29295  25708     33\n0000015\n",
            "0000000  18533  27756  28460   8279  28530  27748   8448\n0000015\n",
        ),
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
        by_endian(
            "0000000   1.1431391e+27    1.761127e+14    1.744671e+22         4.6e-44\n0000015\n",
            "0000000       234929.69    5.327052e+28    7.502641e+28   4.3368087e-19\n0000015\n",
        ),
    );
}

#[test]
fn test_od_17() {
    od_test(
        &["-td2"],
        "Hello, World!",
        by_endian(
            "0000000  25928  27756  11375  22304  29295  25708     33\n0000015\n",
            "0000000  18533  27756  28460   8279  28530  27748   8448\n0000015\n",
        ),
    );
}

#[test]
fn test_od_18() {
    od_test(
        &["-v"],
        "Hello, World!",
        by_endian(
            "0000000 062510 066154 026157 053440 071157 062154 000041\n0000015\n",
            "0000000 044145 066154 067454 020127 067562 066144 020400\n0000015\n",
        ),
    );
}

#[test]
fn test_od_19() {
    od_test(
        &["-x"],
        "Hello, World!",
        by_endian(
            "0000000 6548 6c6c 2c6f 5720 726f 646c 0021\n0000015\n",
            "0000000 4865 6c6c 6f2c 2057 6f72 6c64 2100\n0000015\n",
        ),
    );
}

#[test]
fn test_od_20() {
    od_test(
        &["-Ax", "-td4", "-v"],
        "Hello, World!",
        by_endian(
            "000000  1819043144  1461726319  1684828783          33\n00000d\n",
            "000000  1214606444  1865162839  1869769828   553648128\n00000d\n",
        ),
    );
}

#[test]
fn test_od_21() {
    od_test(
        &["-j0x10"],
        "Hello, World!123456",
        by_endian(
            "0000020 032464 000066\n0000023\n",
            "0000020 032065 033000\n0000023\n",
        ),
    );
}

#[test]
fn test_od_22() {
    od_test(
        &["-tu4"],
        "Hello, World!1",
        by_endian(
            "0000000 1819043144 1461726319 1684828783      12577\n0000016\n",
            "0000000 1214606444 1865162839 1869769828  556859392\n0000016\n",
        ),
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
        by_endian("                    1\n", "    72057594037927936\n"),
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
    let f64_one = 1.0f64.to_ne_bytes();
    let f32_one = 1.0f32.to_ne_bytes();

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

// POSIX 109155-109157: the `f` conversion "shall support values of the
// optional number of bytes to be converted corresponding to the number of
// bytes in the C-language types float, double, and long double", spelled `F`,
// `D` and `L`. `-t fL` was refused outright.
//
// 109138-109140 ties the sizes to the system's c17: "If the c17 compiler is
// present on the system, these specifiers shall correspond to the sizes used
// by default in that compiler." So `long double` is whatever it is for this
// target -- x87 80-bit on x86-64, IEEE binary128 on aarch64, and plain
// `double` on Apple's aarch64 -- matching the table in `cc/arch/mod.rs`.
#[test]
fn od_long_double_is_converted() {
    // The two formats are laid out differently, so they get different
    // constructors rather than one helper that is right for only one of them.

    // x87 80-bit occupies the first ten bytes of its slot *by memory
    // position*: an explicit 64-bit significand, then sign and exponent. Only
    // x86 has this format and x86 is little-endian, so the positions are the
    // little-endian ones.
    #[cfg(all(target_arch = "x86_64", not(target_os = "macos")))]
    fn slot(significand: u64, sign_exp: u16) -> Vec<u8> {
        let mut bytes = Vec::with_capacity(16);
        bytes.extend(significand.to_le_bytes());
        bytes.extend(sign_exp.to_le_bytes());
        bytes.extend([0u8; 6]);
        bytes
    }

    // binary128 is a single 128-bit field, so it is built as one integer and
    // laid out in the host's order. `hi` is its top 16 bits -- sign and
    // exponent -- which is bytes 14-15 on a little-endian host and bytes 0-1
    // on a big-endian one; `to_ne_bytes` puts them wherever they belong
    // without the test having to say which.
    #[cfg(not(any(
        all(target_arch = "x86_64", not(target_os = "macos")),
        all(target_arch = "aarch64", target_os = "macos")
    )))]
    fn slot(fraction_low: u64, hi: u16) -> Vec<u8> {
        let bits = ((hi as u128) << 112) | fraction_low as u128;
        bits.to_ne_bytes().to_vec()
    }

    #[cfg(all(target_arch = "x86_64", not(target_os = "macos")))]
    let cases: Vec<(Vec<u8>, &str)> = {
        // x87 80-bit: an *explicit* 64-bit significand -- the leading one is
        // stored, unlike every IEEE format -- then sign and a 15-bit exponent
        // biased by 16383.
        vec![
            (slot(0x8000_0000_0000_0000, 0x3fff), "1"),
            (slot(0x8000_0000_0000_0000, 0x4000), "2"),
            (slot(0x8000_0000_0000_0000, 0xbfff), "-1"),
            (slot(0, 0), "0"),
            (slot(0xcccc_cccc_cccc_cccd, 0x3ffb), "0.1"),
            (slot(0x8000_0000_0000_0000, 0x7fff), "inf"),
            (slot(0x8000_0000_0000_0000, 0xffff), "-inf"),
            (slot(0xc000_0000_0000_0000, 0x7fff), "nan"),
        ]
    };

    #[cfg(not(any(
        all(target_arch = "x86_64", not(target_os = "macos")),
        all(target_arch = "aarch64", target_os = "macos")
    )))]
    let cases: Vec<(Vec<u8>, &str)> = {
        // IEEE binary128: sign, a 15-bit exponent biased by 16383, and a
        // 112-bit fraction with an implicit leading one. The high 16 bits are
        // sign and exponent, so they land in `hi` once the low 112 are zero.
        vec![
            (slot(0, 0x3fff), "1"),
            (slot(0, 0x4000), "2"),
            (slot(0, 0xbfff), "-1"),
            (slot(0, 0), "0"),
            (slot(0, 0x7fff), "inf"),
            (slot(0, 0xffff), "-inf"),
        ]
    };

    // Apple's aarch64 makes `long double` a `double`, so `L` is eight bytes
    // there and the 16-byte slot above does not apply.
    #[cfg(all(target_arch = "aarch64", target_os = "macos"))]
    let cases: Vec<(Vec<u8>, &str)> = vec![(1.0f64.to_ne_bytes().to_vec(), "1")];

    for (bytes, want) in cases {
        let (stdout, stderr, code) = od_raw(&["-An", "-t", "fL"], &bytes);
        assert_eq!(code, Some(0), "-t fL must be accepted: {stderr:?}");
        assert_eq!(
            stdout.split_whitespace().next(),
            Some(want),
            "-t fL on {bytes:02x?}: {stdout:?}"
        );
    }

    // A long double outside a double's range prints as zero, because the value
    // is converted through a double to be printed -- Rust has neither an f80
    // nor an f128. This is a deliberate, documented deviation (NONPOSIX.md,
    // "od"), pinned here so that implementing arbitrary-precision conversion
    // later trips this test and the documentation gets updated with it.
    #[cfg(all(target_arch = "x86_64", not(target_os = "macos")))]
    {
        // The smallest x87 subnormal, 2^-16445, which another od shows as
        // roughly 3.6e-4951.
        let (stdout, _, code) = od_raw(&["-An", "-t", "fL"], &slot(1, 0));
        assert_eq!(code, Some(0));
        assert_eq!(
            stdout.split_whitespace().next(),
            Some("0"),
            "a long double below a double's range prints as zero: {stdout:?}"
        );
    }

    // `fL` and the explicit byte count name the same type.
    let width = if cfg!(all(target_arch = "aarch64", target_os = "macos")) {
        "f8"
    } else {
        "f16"
    };
    let bytes = vec![0u8; 16];
    let (by_letter, _, _) = od_raw(&["-An", "-t", "fL"], &bytes);
    let (by_number, _, _) = od_raw(&["-An", "-t", width], &bytes);
    assert_eq!(by_letter, by_number, "fL must agree with {width}");
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
        // The positional/exponential threshold is the *type's* decimal
        // precision -- FLT_DIG, 6 -- not the number of digits this particular
        // value happens to need. 999999 is positional and 1e6 is not.
        (0x4974_23f0, "999999"),
        (0x4974_2400, "1e+06"),
        (0x4b18_9680, "1e+07"),
        (0x42c8_0000, "100"),
        (0x4874_2400, "250000"),
        (0x3a83_126f, "0.001"),
        (0x3927_c5ac, "0.00016"),
        (0x7f80_0000, "inf"),
        (0xff80_0000, "-inf"),
        (0x7fc0_0000, "nan"),
        (0xffc0_0000, "-nan"),
        // An exact tie in the last digit: 4088288.25 reads back the same at
        // either spelling, and od resolves it to even, as C's %g does. Rust's
        // own shortest representation resolves away from zero and would give
        // -4088288.3 here.
        (0xca79_8781, "-4088288.2"),
        (0x4a72_2829, "3967498.2"),
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
        // DBL_DIG is 15, so 1e14 is positional and 1e15 is not.
        (0x42d6_bcc4_1e90_0000, "100000000000000"),
        (0x430c_6bf5_2634_0000, "1e+15"),
        (0x4202_a05f_2000_0000, "10000000000"),
        (0x41d2_6580_b480_0000, "1234567890"),
        (0x7ff0_0000_0000_0000, "inf"),
        (0x7ff8_0000_0000_0000, "nan"),
        (0xfff8_0000_0000_0000, "-nan"),
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

// The octal field is as wide as the widest value the type can hold, which is
// ceil(bits/3) digits -- 3, 6, 11 and 22 for one, two, four and eight bytes.
//
// The width was `num_bytes * 3`, which is right only for one and two bytes.
// A 4-byte octal was printed 12 digits wide when u32::MAX is 37777777777, 11
// digits, so a leading zero was always present and never meaningful; an
// 8-byte one was 24 wide against a 22-digit maximum.
//
// All-ones and all-zero inputs, so this says nothing about byte order.
#[test]
fn od_octal_field_is_as_wide_as_the_type() {
    // The maximum of each width, which must fill the field exactly -- no
    // leading zero, and nothing truncated.
    for (spec, bytes, max) in [
        ("o1", 1, "377"),
        ("o2", 2, "177777"),
        ("o4", 4, "37777777777"),
        ("o8", 8, "1777777777777777777777"),
    ] {
        let ones = vec![0xffu8; bytes];
        let (stdout, _, code) = od_raw(&["-An", "-t", spec], &ones);
        assert_eq!(code, Some(0), "-t {spec}");
        assert_eq!(
            stdout.trim_end_matches('\n'),
            format!(" {max}"),
            "-t {spec}: the maximum must fill the field exactly"
        );

        // Zero is the same width, zero-padded.
        let zeros = vec![0u8; bytes];
        let (stdout, _, _) = od_raw(&["-An", "-t", spec], &zeros);
        assert_eq!(
            stdout.trim_end_matches('\n'),
            format!(" {}", "0".repeat(max.len())),
            "-t {spec}: zero is padded to the same width"
        );
    }
}

// `-N count` reads *count bytes*, not "up to absolute offset count". The
// truncation compared the count against `offset`, which starts at the `-j`
// skip, so every byte skipped was also charged against the count:
// `-j 2 -N 9` read seven bytes instead of nine. And when the skip exceeded
// the count, `count - offset` underflowed a u64 and the result was used to
// index the buffer, so `-j 5 -N 4` panicked outright.
#[test]
fn od_count_is_a_length_not_an_end_offset() {
    let data = b"ABCDEFGHIJKLM"; // 13 bytes

    // Nine bytes from offset two, and the trailing offset is 2 + 9 = 11.
    let (stdout, _, code) = od_raw(&["-j", "2", "-N", "9", "-t", "x1"], data);
    assert_eq!(code, Some(0));
    assert_eq!(
        stdout, "0000002 43 44 45 46 47 48 49 4a 4b\n0000013\n",
        "-j 2 -N 9 must read nine bytes"
    );

    // The skip may exceed the count without underflowing.
    let (stdout, stderr, code) = od_raw(&["-j", "5", "-N", "4", "-t", "x1"], data);
    assert!(
        !stderr.contains("panicked at"),
        "-j 5 -N 4 panicked: {stderr:?}"
    );
    assert_eq!(code, Some(0));
    assert_eq!(stdout, "0000005 46 47 48 49\n0000011\n");

    // A zero count reads nothing and prints only the trailing offset -- no
    // field line, and no duplicate-line asterisk for a block that never was.
    let (stdout, _, code) = od_raw(&["-N", "0", "-t", "x1"], data);
    assert_eq!(code, Some(0));
    assert_eq!(stdout, "0000000\n", "-N 0 prints only the offset");

    let (stdout, stderr, code) = od_raw(&["-j", "2", "-N", "0", "-t", "x1"], data);
    assert!(
        !stderr.contains("panicked at"),
        "-j 2 -N 0 panicked: {stderr:?}"
    );
    assert_eq!(code, Some(0));
    assert_eq!(stdout, "0000002\n");

    // Without a skip, nothing changes.
    let (stdout, _, _) = od_raw(&["-N", "9", "-t", "x1"], data);
    assert_eq!(stdout, "0000000 41 42 43 44 45 46 47 48 49\n0000011\n");
}

// Skipping exactly the whole input is not an error, and still prints the
// trailing offset. The file path returned early when the skip consumed every
// operand, leaving `all_files` empty, so it printed nothing at all -- while
// the stdin path printed the offset, so the two disagreed with each other.
#[test]
fn od_skip_of_exactly_the_whole_input_prints_the_offset() {
    use std::io::Write;

    let path = std::env::temp_dir().join(format!("od_skip_all_{}", std::process::id()));
    std::fs::File::create(&path)
        .unwrap()
        .write_all(b"12345")
        .unwrap();

    let (from_file, stderr, code) = od_raw(&["-j", "5", "-t", "x1", path.to_str().unwrap()], b"");
    std::fs::remove_file(&path).ok();
    assert_eq!(
        code,
        Some(0),
        "skipping the whole file is not an error: {stderr:?}"
    );
    assert_eq!(from_file, "0000005\n");

    // And it agrees with the same skip taken on stdin.
    let (from_stdin, _, _) = od_raw(&["-j", "5", "-t", "x1"], b"12345");
    assert_eq!(from_file, from_stdin, "the file and stdin paths must agree");
}

// `-j` must skip bytes that are actually read, not bytes `stat` claims are
// there. A FIFO, a `/proc` file and a character device all report a size of
// zero, so deciding from the size treated each as already exhausted and
// reported "cannot skip past end of input" over a file with content in it.
#[test]
fn od_skip_works_on_a_file_whose_size_is_not_known() {
    // /proc/version reports st_size 0 and reads several dozen bytes.
    let probe = std::path::Path::new("/proc/version");
    if !probe.exists() {
        return; // Not Linux; the FIFO case below still covers the shape.
    }

    let (stdout, stderr, code) = od_raw(&["-j", "4", "-N", "8", "-t", "x1", "/proc/version"], b"");
    assert_eq!(
        code,
        Some(0),
        "a zero-stat file is not an empty one: {stderr:?}"
    );
    let fields: Vec<&str> = stdout
        .lines()
        .next()
        .unwrap_or("")
        .split_whitespace()
        .collect();
    assert_eq!(
        fields.len(),
        9,
        "the offset and eight bytes from offset four: {stdout:?}"
    );
    assert_eq!(fields[0], "0000004");

    // The same bytes as an ordinary read of the file, skipped by hand.
    let whole = std::fs::read("/proc/version").unwrap();
    let expected: Vec<String> = whole[4..12].iter().map(|b| format!("{b:02x}")).collect();
    assert_eq!(fields[1..], expected[..], "{stdout:?}");
}

// POSIX 109196-109199: "Unless -A n is specified, the *first* output line
// produced for each input block shall be preceded by the input offset". One
// offset per block, not one per type -- the spec's own three-type example at
// 109240-109245 shows the continuation lines blank where the offset would be.
//
// The offset was printed on every type's line.
#[test]
fn od_offset_marks_the_first_line_of_a_block_only() {
    let (stdout, _, code) = od_raw(&["-A", "o", "-t", "x1", "-t", "c"], b"AB");
    assert_eq!(code, Some(0));
    let lines: Vec<&str> = stdout.lines().collect();
    assert_eq!(lines.len(), 3, "two types plus the trailing offset");
    assert!(lines[0].starts_with("0000000"), "{stdout:?}");
    assert!(
        lines[1].starts_with("       "),
        "the second type's line is blank where the offset would be: {lines:?}"
    );
    assert_eq!(
        lines[1].len(),
        lines[0].len(),
        "and is indented to the same column: {lines:?}"
    );
    assert_eq!(lines[2], "0000002");

    // -A n prints no offset at all, on any line.
    let (stdout, _, _) = od_raw(&["-A", "n", "-t", "x1", "-t", "c"], b"AB");
    for line in stdout.lines() {
        assert!(line.starts_with(' '), "-A n: no offset column: {stdout:?}");
    }
}

// Fields of different types line up in columns. Every type shares one
// per-byte column width -- the largest any of them needs -- so a type
// converting more bytes per field gets a proportionally wider field, and the
// lines all come out the same length.
//
// Each type was padded to its own natural width instead, so `-t x1c` put a
// 3-column hex field above a 4-column character one and nothing lined up.
//
// The expectations are GNU od's output for the same bytes.
#[test]
fn od_field_widths_are_shared_across_types() {
    // x1 needs 3 columns, c needs 4, so both get 4.
    let (stdout, _, code) = od_raw(&["-An", "-t", "x1c"], b"AB");
    assert_eq!(code, Some(0));
    assert_eq!(stdout, "  41  42\n   A   B\n");

    // o1 needs 4 and d8 needs 21, over eight bytes -- so the shared per-byte
    // width is o1's 4, and d8's field is 8 * 4 = 32.
    let (stdout, _, _) = od_raw(&["-An", "-t", "o1", "-t", "d8"], b"ABCDEFGHIJKLMNOP");
    let lines: Vec<&str> = stdout.lines().collect();
    assert_eq!(lines[0].len(), 64, "16 fields of 4: {lines:?}");
    assert_eq!(lines[1].len(), 64, "2 fields of 32: {lines:?}");
    assert_eq!(
        lines[1],
        by_endian(
            "             5208208757389214273             5786930140093827657",
            "             4702394921427289928             5281116304131903312",
        )
    );

    // A per-byte width need not be a whole number: o2 needs 7 columns for two
    // bytes, so x1's one-byte fields alternate 4 and 3 to keep the byte
    // positions aligned.
    let (stdout, _, _) = od_raw(&["-An", "-t", "x1", "-t", "o2"], b"ABCDEFGHIJKLMNOP");
    let lines: Vec<&str> = stdout.lines().collect();
    assert_eq!(lines[0].len(), 56, "{lines:?}");
    assert_eq!(lines[1].len(), 56, "{lines:?}");
    assert_eq!(
        lines[0],
        "  41 42  43 44  45 46  47 48  49 4a  4b 4c  4d 4e  4f 50"
    );

    // A single type is unaffected: its own natural width is the shared one.
    let (one, _, _) = od_raw(&["-An", "-t", "x1"], b"AB");
    assert_eq!(one, " 41 42\n");

    // Where a field boundary falls between columns it is rounded *up*, so the
    // slack lands at the start of each group rather than being spread through
    // it. f8 needs 25 columns for eight bytes, so x1's eight 3-column fields
    // leave one column over, and it goes before the first of them.
    let (stdout, _, _) = od_raw(&["-An", "-t", "x1", "-t", "f8"], b"ABCDEFGHIJKLMNOP");
    let lines: Vec<&str> = stdout.lines().collect();
    assert_eq!(
        lines[0], "  41 42 43 44 45 46 47 48  49 4a 4b 4c 4d 4e 4f 50",
        "the spare column belongs to the head of the group: {stdout:?}"
    );
    assert_eq!(lines[0].len(), 50, "two 25-column groups");
}

// POSIX 109079-109082: "any number of groups of output lines, which would be
// identical to the immediately preceding group of output lines (except for the
// byte offsets), shall be replaced with a line containing only an <asterisk>".
//
// The unit is the *group* -- every type's line for one input block -- and the
// suppression repeats. Two defects: `previous_asterisk` was set on the first
// duplicate run and never cleared, so a second run after an intervening
// different block printed in full; and the comparison was per line with state
// shared across types, so a multi-type dump never suppressed anything at all.
#[test]
fn od_duplicate_blocks_collapse_to_an_asterisk() {
    // Three identical blocks, one different, three identical again.
    let mut data = vec![b'A'; 48];
    data.extend([b'B'; 16]);
    data.extend([b'A'; 48]);

    let (stdout, _, code) = od_raw(&["-t", "x1"], &data);
    assert_eq!(code, Some(0));
    assert_eq!(
        stdout,
        "0000000 41 41 41 41 41 41 41 41 41 41 41 41 41 41 41 41\n\
         *\n\
         0000060 42 42 42 42 42 42 42 42 42 42 42 42 42 42 42 42\n\
         0000100 41 41 41 41 41 41 41 41 41 41 41 41 41 41 41 41\n\
         *\n\
         0000160\n",
        "both runs must collapse, not just the first"
    );

    // With two types the asterisk stands for the whole group: one per run,
    // not one per line.
    let (stdout, _, _) = od_raw(&["-t", "x1", "-t", "c"], &data);
    assert_eq!(
        stdout.lines().filter(|l| *l == "*").count(),
        2,
        "one asterisk per suppressed run: {stdout:?}"
    );
    let head: Vec<&str> = stdout.lines().take(3).collect();
    assert!(head[0].starts_with("0000000"), "{stdout:?}");
    assert!(head[1].trim_start().starts_with('A'), "{stdout:?}");
    assert_eq!(
        head[2], "*",
        "the group's lines print before it: {stdout:?}"
    );

    // -v writes every block.
    let (stdout, _, _) = od_raw(&["-v", "-t", "x1"], &data);
    assert!(!stdout.contains('*'), "-v suppresses nothing: {stdout:?}");
    assert_eq!(stdout.lines().count(), 8, "seven blocks plus the offset");
}

// Suppression compares the input block, not the text it renders to. Two blocks
// that print alike are not necessarily the same bytes, and od must not hide
// bytes it was asked to dump.
//
// Comparing the rendered lines lost data two ways. A short final block renders
// identically to the full block before it once the null extension pads it out,
// so the last line of the file simply vanished. And a conversion that is not
// one-to-one -- `-t a` masks to seven bits -- collapsed blocks that differ.
#[test]
fn od_suppression_compares_the_input_not_the_output() {
    // 93 zero bytes: five full 16-byte blocks and a 13-byte tail, which under
    // `-t u4` renders as the same four zeroes as the blocks before it.
    let zeros = vec![0u8; 93];
    let (stdout, _, code) = od_raw(&["-t", "u4"], &zeros);
    assert_eq!(code, Some(0));
    let lines: Vec<&str> = stdout.lines().collect();
    assert_eq!(
        lines.len(),
        4,
        "the short final block must print, not be swallowed: {stdout:?}"
    );
    assert!(lines[1] == "*", "{stdout:?}");
    assert!(
        lines[2].starts_with("0000120"),
        "the 13-byte tail is its own line: {stdout:?}"
    );
    assert_eq!(lines[3], "0000135");

    // `-t a` keeps only the low seven bits, so 0x41 and 0xc1 both print as
    // `A` -- but they are different bytes and all three blocks must show.
    let mut masked = vec![0x41u8; 16];
    masked.extend([0xc1u8; 16]);
    masked.extend([0x41u8; 16]);
    let (stdout, _, _) = od_raw(&["-t", "a"], &masked);
    assert!(
        !stdout.contains('*'),
        "blocks differing only above the mask are still different: {stdout:?}"
    );
    assert_eq!(stdout.lines().count(), 4, "three blocks plus the offset");
}

// A block is filled from the input, however many reads that takes.
//
// The loop called `read` exactly twice, which is not enough to cross more than
// one boundary of a chained reader: with three operands the first block came
// back short. That was a layout wart on its own, but combined with the null
// extension it became a wrong-value bug -- a short block is padded with NULs,
// so `-t x4` reported a byte that appears nowhere in the input.
#[test]
fn od_fills_a_block_across_several_files() {
    use std::io::Write;

    let dir = std::env::temp_dir().join(format!("od_multifile_{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let names: Vec<std::path::PathBuf> =
        [("a1", &b"AAAAA"[..]), ("a2", b"BB"), ("a3", &[b'C'; 16])]
            .iter()
            .map(|(name, body)| {
                let path = dir.join(name);
                std::fs::File::create(&path)
                    .unwrap()
                    .write_all(body)
                    .unwrap();
                path
            })
            .collect();

    let args: Vec<&str> = std::iter::once("-t")
        .chain(std::iter::once("x4"))
        .chain(names.iter().map(|p| p.to_str().unwrap()))
        .collect();
    let (stdout, _, code) = od_raw(&args, b"");
    std::fs::remove_dir_all(&dir).ok();

    assert_eq!(code, Some(0));
    // 23 bytes: one full 16-byte block, then a 7-byte tail. No NUL may appear
    // anywhere but in that tail's padding.
    let lines: Vec<&str> = stdout.lines().collect();
    assert_eq!(lines.len(), 3, "16 + 7 bytes is two blocks: {stdout:?}");
    // The offset is the first token; the four 4-byte fields follow it.
    let first: Vec<&str> = lines[0].split_whitespace().skip(1).collect();
    assert_eq!(
        first.len(),
        4,
        "the first block must be filled to 16 bytes: {stdout:?}"
    );
    assert!(
        first.iter().all(|f| !f.contains("00")),
        "no NUL may be invented mid-stream: {stdout:?}"
    );
    // Only the 7-byte tail is padded, and the nulls follow the byte order:
    // high-order on a little-endian host, low-order on a big-endian one.
    assert_eq!(
        lines[1].split_whitespace().collect::<Vec<_>>().last(),
        Some(&by_endian("00434343", "43434300"))
    );
    assert_eq!(lines[2], "0000027");
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
