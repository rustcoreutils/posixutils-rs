//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{get_binary_path, run_test, TestPlan};
use std::fs::File;
use std::path::Path;
use std::process::Command;

fn printf_test(args: &[&str], expected_out: &str) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("printf"),
        args: str_args,
        expected_out: String::from(expected_out),
        expected_err: String::new(),
        stdin_data: String::new(),
        expected_exit_code: 0,
    });
}

// Basic string output tests
#[test]
fn test_basic_string_output() {
    printf_test(&["Hello, %s!", "World"], "Hello, World!");
}

#[test]
fn test_string_no_args() {
    printf_test(&["Hello World"], "Hello World");
}

#[test]
fn test_string_multiple_args() {
    printf_test(&["%s %s %s", "one", "two", "three"], "one two three");
}

// Integer output tests
#[test]
fn test_integer_output() {
    printf_test(&["The answer is %d.", "42"], "The answer is 42.");
}

#[test]
fn test_integer_negative() {
    printf_test(&["%d", "-42"], "-42");
}

#[test]
fn test_integer_positive_sign() {
    printf_test(&["%d", "+42"], "42");
}

#[test]
fn test_integer_i_specifier() {
    printf_test(&["%i", "123"], "123");
}

// Octal output tests
#[test]
fn test_octal_output() {
    printf_test(&["%o", "8"], "10");
}

#[test]
fn test_octal_output_64() {
    printf_test(&["%o", "64"], "100");
}

// Hex output tests
#[test]
fn test_hex_output() {
    printf_test(&["%x", "255"], "ff");
}

#[test]
fn test_hex_uppercase() {
    printf_test(&["%X", "255"], "FF");
}

// Unsigned integer tests
#[test]
fn test_unsigned_output() {
    printf_test(&["%u", "42"], "42");
}

// Character output tests
#[test]
fn test_char_output() {
    printf_test(&["%c", "A"], "A");
}

#[test]
fn test_char_from_string() {
    printf_test(&["%c", "Hello"], "H");
}

// %b specifier tests (backslash escapes in argument)
#[test]
fn test_b_specifier_basic() {
    printf_test(&["%b", "hello"], "hello");
}

#[test]
fn test_b_specifier_newline() {
    printf_test(&["%b", "hello\\nworld"], "hello\nworld");
}

#[test]
fn test_b_specifier_tab() {
    printf_test(&["%b", "hello\\tworld"], "hello\tworld");
}

#[test]
fn test_b_specifier_backslash() {
    printf_test(&["%b", "hello\\\\world"], "hello\\world");
}

#[test]
fn test_b_specifier_octal() {
    printf_test(&["%b", "\\0101"], "A"); // \0101 = 65 = 'A'
}

#[test]
fn test_b_specifier_alert() {
    printf_test(&["%b", "\\a"], "\x07");
}

#[test]
fn test_b_specifier_backspace() {
    printf_test(&["%b", "\\b"], "\x08");
}

#[test]
fn test_b_specifier_formfeed() {
    printf_test(&["%b", "\\f"], "\x0c");
}

#[test]
fn test_b_specifier_carriage_return() {
    printf_test(&["%b", "\\r"], "\r");
}

#[test]
fn test_b_specifier_vertical_tab() {
    printf_test(&["%b", "\\v"], "\x0b");
}

// %% specifier test
#[test]
fn test_percent_literal() {
    printf_test(&["100%%"], "100%");
}

#[test]
fn test_percent_literal_with_args() {
    printf_test(&["%d%% complete", "50"], "50% complete");
}

// Character constant tests ('A -> 65)
#[test]
fn test_char_constant_single_quote() {
    printf_test(&["%d", "'A"], "65");
}

#[test]
fn test_char_constant_double_quote() {
    printf_test(&["%d", "\"A"], "65");
}

#[test]
fn test_char_constant_plus() {
    printf_test(&["%d", "'+"], "43");
}

#[test]
fn test_char_constant_minus() {
    printf_test(&["%d", "'-"], "45");
}

// Hex/Octal integer argument parsing
#[test]
fn test_hex_arg_parsing() {
    printf_test(&["%d", "0x10"], "16");
}

#[test]
fn test_hex_arg_parsing_uppercase() {
    printf_test(&["%d", "0X1F"], "31");
}

#[test]
fn test_octal_arg_parsing() {
    printf_test(&["%d", "010"], "8");
}

#[test]
fn test_octal_arg_parsing_larger() {
    printf_test(&["%d", "0100"], "64");
}

// Format reuse tests
#[test]
fn test_format_reuse() {
    printf_test(&["%d\n", "1", "2", "3"], "1\n2\n3\n");
}

#[test]
fn test_format_reuse_string() {
    printf_test(&["%s ", "a", "b", "c"], "a b c ");
}

// Width and precision tests
#[test]
fn test_width_right_justify() {
    printf_test(&["%5s", "abc"], "  abc");
}

#[test]
fn test_width_left_justify() {
    printf_test(&["%-5s", "abc"], "abc  ");
}

#[test]
fn test_width_integer() {
    printf_test(&["%5d", "42"], "   42");
}

#[test]
fn test_precision_string() {
    printf_test(&["%.3s", "hello"], "hel");
}

#[test]
fn test_precision_string_longer() {
    printf_test(&["%.10s", "hello"], "hello");
}

#[test]
fn test_width_and_precision() {
    printf_test(&["%10.3s", "hello"], "       hel");
}

// Escape sequences in format string
#[test]
fn test_format_escape_newline() {
    printf_test(&["hello\\nworld"], "hello\nworld");
}

#[test]
fn test_format_escape_tab() {
    printf_test(&["hello\\tworld"], "hello\tworld");
}

#[test]
fn test_format_escape_backslash() {
    printf_test(&["hello\\\\world"], "hello\\world");
}

#[test]
fn test_format_escape_octal() {
    printf_test(&["\\101"], "A"); // \101 = 65 = 'A'
}

#[test]
fn test_format_escape_alert() {
    printf_test(&["\\a"], "\x07");
}

#[test]
fn test_format_escape_backspace() {
    printf_test(&["\\b"], "\x08");
}

#[test]
fn test_format_escape_formfeed() {
    printf_test(&["\\f"], "\x0c");
}

#[test]
fn test_format_escape_carriage_return() {
    printf_test(&["\\r"], "\r");
}

#[test]
fn test_format_escape_vertical_tab() {
    printf_test(&["\\v"], "\x0b");
}

// Zero padding tests
#[test]
fn test_zero_padding_integer() {
    printf_test(&["%05d", "42"], "00042");
}

#[test]
fn test_zero_padding_hex() {
    printf_test(&["%04x", "15"], "000f");
}

// Missing argument defaults
#[test]
fn test_missing_string_arg() {
    printf_test(&["%s%s", "hello"], "hello");
}

#[test]
fn test_missing_int_arg() {
    printf_test(&["%d%d", "42"], "420");
}

// POSIX example from spec
#[test]
fn test_posix_example() {
    printf_test(
        &["%5d%4d\n", "1", "21", "321", "4321", "54321"],
        "    1  21\n  3214321\n54321   0\n",
    );
}

// Empty format
#[test]
fn test_empty_format() {
    printf_test(&[""], "");
}

// \c in format stops output
#[test]
fn test_format_c_escape() {
    printf_test(&["hello\\cworld"], "hello");
}

// \c in %b stops all output
#[test]
fn test_b_specifier_c_escape() {
    printf_test(&["%b%b", "hello\\c", "world"], "hello");
}

// + flag tests (always show sign)
#[test]
fn test_plus_flag_positive() {
    printf_test(&["%+d", "42"], "+42");
}

#[test]
fn test_plus_flag_negative() {
    printf_test(&["%+d", "-42"], "-42");
}

#[test]
fn test_plus_flag_zero() {
    printf_test(&["%+d", "0"], "+0");
}

// space flag tests (space if no sign)
#[test]
fn test_space_flag_positive() {
    printf_test(&["% d", "42"], " 42");
}

#[test]
fn test_space_flag_negative() {
    printf_test(&["% d", "-42"], "-42");
}

#[test]
fn test_plus_overrides_space() {
    printf_test(&["%+ d", "42"], "+42");
}

// # flag tests (alternate form)
#[test]
fn test_alt_form_hex() {
    printf_test(&["%#x", "255"], "0xff");
}

#[test]
fn test_alt_form_hex_upper() {
    printf_test(&["%#X", "255"], "0XFF");
}

#[test]
fn test_alt_form_hex_zero() {
    // # flag doesn't add prefix for zero
    printf_test(&["%#x", "0"], "0");
}

#[test]
fn test_alt_form_octal() {
    printf_test(&["%#o", "8"], "010");
}

#[test]
fn test_alt_form_octal_zero() {
    // # flag doesn't add prefix for zero
    printf_test(&["%#o", "0"], "0");
}

// Integer precision tests
#[test]
fn test_int_precision() {
    printf_test(&["%.5d", "42"], "00042");
}

#[test]
fn test_int_precision_larger() {
    printf_test(&["%.3d", "12345"], "12345");
}

#[test]
fn test_int_precision_negative() {
    printf_test(&["%.5d", "-42"], "-00042");
}

#[test]
fn test_int_width_and_precision() {
    printf_test(&["%8.5d", "42"], "   00042");
}

// Zero padding with sign
#[test]
fn test_zero_pad_negative() {
    printf_test(&["%05d", "-42"], "-0042");
}

#[test]
fn test_zero_pad_with_plus() {
    printf_test(&["%+05d", "42"], "+0042");
}

// Combined flags
#[test]
fn test_left_justify_with_plus() {
    printf_test(&["%-+5d", "42"], "+42  ");
}

#[test]
fn test_alt_form_with_width() {
    printf_test(&["%#8x", "255"], "    0xff");
}

#[test]
fn test_alt_form_with_zero_pad() {
    printf_test(&["%#08x", "255"], "0x0000ff");
}

// Floating point tests - %f
#[test]
fn test_float_f_basic() {
    printf_test(&["%f", "3.14159"], "3.141590");
}

#[test]
fn test_float_f_precision() {
    printf_test(&["%.2f", "3.14159"], "3.14");
}

#[test]
fn test_float_f_precision_zero() {
    printf_test(&["%.0f", "3.7"], "4");
}

#[test]
fn test_float_f_negative() {
    printf_test(&["%f", "-3.14"], "-3.140000");
}

#[test]
fn test_float_f_width() {
    printf_test(&["%10.2f", "3.14"], "      3.14");
}

#[test]
fn test_float_f_zero_pad() {
    printf_test(&["%010.2f", "3.14"], "0000003.14");
}

#[test]
fn test_float_f_plus_flag() {
    printf_test(&["%+f", "3.14"], "+3.140000");
}

#[test]
fn test_float_f_space_flag() {
    printf_test(&["% f", "3.14"], " 3.140000");
}

// Floating point tests - %e (scientific notation)
#[test]
fn test_float_e_basic() {
    printf_test(&["%e", "12345.6789"], "1.234568e+04");
}

#[test]
fn test_float_e_precision() {
    printf_test(&["%.2e", "12345.6789"], "1.23e+04");
}

#[test]
fn test_float_e_negative() {
    printf_test(&["%e", "-0.00123"], "-1.230000e-03");
}

// Floating point tests - %E (uppercase scientific)
#[test]
fn test_float_big_e_basic() {
    printf_test(&["%E", "12345.6789"], "1.234568E+04");
}

// Floating point tests - %g (general format)
#[test]
fn test_float_g_small() {
    printf_test(&["%g", "0.0001234"], "0.0001234");
}

#[test]
fn test_float_g_large() {
    printf_test(&["%g", "1234567"], "1.23457e+06");
}

// Integer and float argument with missing arg defaults
#[test]
fn test_float_missing_arg() {
    printf_test(&["%f", ""], "0.000000");
}

// Character constant for float
#[test]
fn test_float_char_constant() {
    printf_test(&["%f", "'A"], "65.000000");
}

// Regression tests for from_str_radix security issue
// These should fail because double signs are invalid
#[test]
#[should_panic]
fn test_reject_double_plus_hex() {
    // This should fail: "+0x+55" should be rejected, not parsed as 0x55
    printf_test(&["%d", "+0x+55"], "85");
}

#[test]
#[should_panic]
fn test_reject_double_minus_hex() {
    // This should fail: "-0x-55" should be rejected
    printf_test(&["%d", "-0x-55"], "-85");
}

#[test]
#[should_panic]
fn test_reject_double_plus_octal() {
    // This should fail: "+0+55" should be rejected
    printf_test(&["%d", "+0+55"], "45");
}

// ---------------------------------------------------------------------------
// Audit regressions
// ---------------------------------------------------------------------------

/// Run `printf` with the given operands and assert on the raw output bytes.
fn printf_bytes_test(args: &[&str], expected_out: &[u8]) {
    let output = Command::new(get_binary_path("printf"))
        .args(args)
        .output()
        .expect("failed to run printf");

    assert_eq!(output.stdout, expected_out, "stdout mismatch for {args:?}");
    assert_eq!(output.status.code(), Some(0), "exit status for {args:?}");
}

/// Run `printf` with the given operands and assert on stdout and exit status.
fn printf_status_test(args: &[&str], expected_out: &str, expected_code: i32) {
    let output = Command::new(get_binary_path("printf"))
        .args(args)
        .output()
        .expect("failed to run printf");

    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        expected_out,
        "stdout mismatch for {args:?}"
    );
    assert_eq!(
        output.status.code(),
        Some(expected_code),
        "exit status mismatch for {args:?} (stderr: {})",
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn test_audit_p2_unsigned_conversions_are_unsigned() {
    // #P2: a negative operand under o/u/x/X is reinterpreted as its
    // two's-complement bit pattern, not printed as its absolute value.
    printf_test(&["%u", "-1"], "18446744073709551615");
    printf_test(&["%x", "-1"], "ffffffffffffffff");
    printf_test(&["%X", "-1"], "FFFFFFFFFFFFFFFF");
    printf_test(&["%o", "-1"], "1777777777777777777777");
    printf_test(&["%u", "-2"], "18446744073709551614");
    // The + and <space> flags apply only to signed conversions.
    printf_test(&["%+u", "1"], "1");
    printf_test(&["% u", "1"], "1");
    printf_test(&["%+d", "1"], "+1");
}

#[test]
fn test_audit_p3_unsigned_full_range() {
    // #P3: INT64_MIN was rejected as an invalid number, and magnitudes above
    // i64::MAX could not be represented at all.
    printf_status_test(&["%u", "-9223372036854775808"], "9223372036854775808", 0);
    printf_status_test(&["%u", "9223372036854775808"], "9223372036854775808", 0);
    printf_status_test(&["%u", "18446744073709551615"], "18446744073709551615", 0);
    printf_status_test(&["%d", "-9223372036854775808"], "-9223372036854775808", 0);
    printf_status_test(&["%d", "9223372036854775807"], "9223372036854775807", 0);
    printf_status_test(&["%x", "0xffffffffffffffff"], "ffffffffffffffff", 0);
}

#[test]
fn test_audit_p6_overflow_saturates() {
    // #P6: POSIX 112020-112024 requires the accumulated value to be written;
    // the EXAMPLES table (112117-112120) shows strtol()-style saturation
    // together with a diagnostic and a non-zero exit status.
    printf_status_test(&["%d", "9999999999999999999999"], "9223372036854775807", 1);
    printf_status_test(
        &["%d", "-9999999999999999999999"],
        "-9223372036854775808",
        1,
    );
    printf_status_test(
        &["%u", "99999999999999999999999999"],
        "18446744073709551615",
        1,
    );
    // Not-completely-converted still reports and still prints the prefix.
    printf_status_test(&["%d", "5a"], "5", 1);
    printf_status_test(&["%d", "ABC"], "0", 1);
}

#[test]
fn test_audit_p1_numbered_conversions() {
    // #P1 / POSIX 111973-111981 (Austin Group Defect 1592): conversions can
    // name the nth argument operand with "%n$".
    printf_test(&["%2$s %1$s\n", "one", "two"], "two one\n");
    printf_test(&["%1$s%1$s", "x"], "xx");
    // 112064-112066: gaps are allowed -- "%3$s %1$d\n" evaluates the first and
    // third operands but not the second.
    printf_test(&["%3$s %1$d\n", "7", "skipped", "third"], "third 7\n");
    // %% may be mixed with the numbered form.
    printf_test(&["%1$s%%\n", "x"], "x%\n");
    // Flags, width and precision still apply after the number.
    printf_test(&["%1$5d|", "42"], "   42|");
    printf_test(&["%1$-5d|", "42"], "42   |");
    printf_test(&["%1$.2s|", "abcdef"], "ab|");
}

#[test]
fn test_audit_p1_numbered_conversions_format_reuse() {
    // 111994-111996: on reuse, n refers to the nth operand following the
    // highest-numbered operand consumed by the previous use of the format.
    printf_test(&["%1$s\n", "a", "b", "c"], "a\nb\nc\n");
    printf_test(&["[%1$s-%2$s]", "a", "b", "c", "d"], "[a-b][c-d]");
    printf_test(&["%2$s%1$s", "a", "b", "c", "d"], "badc");
}

#[test]
fn test_audit_p1_numbered_missing_operand() {
    // 111998-112000: a missing operand for a numbered conversion should be
    // diagnosed and should not exit zero.
    printf_status_test(&["%1$s %2$s\n", "only"], "only \n", 1);
    printf_status_test(&["%3$s", "a"], "", 1);
}

#[test]
fn test_audit_unnumbered_conversions_unchanged() {
    // A digit run not followed by '$' is still flags + width, not an
    // argument number.
    printf_test(&["%05d", "42"], "00042");
    printf_test(&["%5d", "42"], "   42");
    printf_test(&["%05.2f", "1.5"], "01.50");
    printf_test(
        &["%5d%4d\n", "1", "21", "321", "4321", "54321"],
        "    1  21\n  3214321\n54321   0\n",
    );
}

#[test]
fn test_audit_p4_empty_precision_is_zero() {
    // #P4 / XBD 3562-3563: "The precision shall take the form of a <period>
    // ('.') followed by a decimal digit string; a null digit string is
    // treated as zero."
    printf_test(&["%.f", "10"], "10");
    printf_test(&["%.f", "2.7"], "3");
    printf_test(&["%.s", "abc"], "");
    printf_test(&["%.b", "abc"], "");
    printf_test(&["%.e", "1234"], "1e+03");
    printf_test(&["%.d", "5"], "5");
    printf_test(&["%.5d", "5"], "00005");
}

#[test]
fn test_audit_p7_zero_value_precision_zero() {
    // #P7 / XBD 3615-3616: "The result of converting a zero value with a
    // precision of 0 shall be no characters."
    printf_test(&["%.0d", "0"], "");
    printf_test(&["%.0i", "0"], "");
    printf_test(&["%.0o", "0"], "");
    printf_test(&["%.0u", "0"], "");
    printf_test(&["%.0x", "0"], "");
    printf_test(&["%.0X", "0"], "");
    // Non-zero values are unaffected, and the field width still applies.
    printf_test(&["%.0d", "5"], "5");
    printf_test(&["%3.0d|", "0"], "   |");
    // '#' still forces a leading zero for the o conversion (XBD 3574-3575).
    printf_test(&["%#.0o", "0"], "0");
}

#[test]
fn test_audit_p8_zero_flag_ignored_for_nonfinite() {
    // #P8 / XBD 3580-3582: the '0' flag pads with leading zeros "except when
    // converting an infinity or NaN".
    printf_test(&["%08f", "inf"], "     inf");
    printf_test(&["%08f", "nan"], "     nan");
    printf_test(&["%08.2f", "inf"], "     inf");
    printf_test(&["%08e", "inf"], "     inf");
    printf_test(&["%08f", "-inf"], "    -inf");
    // Finite values still zero-pad.
    printf_test(&["%08.2f", "1.5"], "00001.50");
}

#[test]
fn test_audit_p9_nonfinite_styles() {
    // #P9 / XBD 3627-3632: f produces "inf"/"nan", F produces "INF"/"NAN".
    printf_test(&["%f", "nan"], "nan");
    printf_test(&["%F", "nan"], "NAN");
    printf_test(&["%f", "inf"], "inf");
    printf_test(&["%F", "inf"], "INF");
    printf_test(&["%f", "-inf"], "-inf");
    printf_test(&["%e", "nan"], "nan");
    printf_test(&["%E", "nan"], "NAN");
    printf_test(&["%g", "inf"], "inf");
    printf_test(&["%G", "inf"], "INF");
    printf_test(&["%a", "nan"], "nan");
    printf_test(&["%A", "inf"], "INF");
}

#[test]
fn test_audit_p10_alt_form_octal() {
    // #P10 / XBD 3574-3575: '#' "shall increase the precision to force the
    // first digit of the result to be a zero" -- it is not a prefix, so it
    // must not add a zero to a result that already has one.
    printf_test(&["%#o", "8"], "010");
    printf_test(&["%#.1o", "8"], "010");
    printf_test(&["%#.3o", "8"], "010");
    printf_test(&["%#.5o", "8"], "00010");
    printf_test(&["%#o", "0"], "0");
    // x/X keep their 0x/0X prefix.
    printf_test(&["%#x", "255"], "0xff");
    printf_test(&["%#X", "255"], "0XFF");
    printf_test(&["%#x", "0"], "0");
}

#[test]
fn test_audit_p14_hex_float_precision() {
    // #P14 / XBD 3594-3598: with the precision missing, it "shall be
    // sufficient for an exact representation of the value" -- not the
    // default of 6 used by the other floating-point conversions.
    printf_test(&["%a", "1"], "0x1p+0");
    printf_test(&["%a", "0.5"], "0x1p-1");
    printf_test(&["%a", "3.5"], "0x1.cp+1");
    printf_test(&["%a", "255"], "0x1.fep+7");
    printf_test(&["%a", "0"], "0x0p+0");
    // XBD 3600-3602: A uses "ABCDEF" for the digits and X/P instead of x/p.
    printf_test(&["%A", "3.5"], "0X1.CP+1");
    // An explicit precision rounds rather than truncating: 3.5 is 0x1.c,
    // so one fewer digit rounds up and carries into the leading digit.
    printf_test(&["%.0a", "3.5"], "0x2p+1");
    printf_test(&["%.2a", "3.5"], "0x1.c0p+1");
    printf_test(&["%.13a", "3.5"], "0x1.c000000000000p+1");
}

#[test]
fn test_audit_floating_point_conversions() {
    // The floating-point engine had no test coverage at all; these pin the
    // behavior verified against GNU coreutils printf.
    printf_test(&["%f", "1.5"], "1.500000");
    printf_test(&["%f", "0.1"], "0.100000");
    printf_test(&["%.2f", "3.14159"], "3.14");
    printf_test(&["%.0f", "2.5"], "2"); // round-half-even
    printf_test(&["%.0f", "3.5"], "4");
    printf_test(&["%e", "1234.5678"], "1.234568e+03");
    printf_test(&["%E", "1234.5678"], "1.234568E+03");
    printf_test(&["%e", "1e100"], "1.000000e+100"); // exponent grows past 2 digits
    printf_test(&["%e", "1e5"], "1.000000e+05"); // exponent is at least 2 digits
    printf_test(&["%g", "100"], "100");
    printf_test(&["%g", "0.0001"], "0.0001");
    printf_test(&["%g", "0.00001"], "1e-05");
    printf_test(&["%g", "1000000"], "1e+06");
    printf_test(&["%g", "123456789"], "1.23457e+08");
    printf_test(&["%#g", "100"], "100.000"); // # keeps trailing zeros
    printf_test(&["%.0g", "100"], "1e+02");
    printf_test(&["%+f", "1.5"], "+1.500000");
    printf_test(&["% f", "1.5"], " 1.500000");
    printf_test(&["%-10.2f|", "1.5"], "1.50      |");
    printf_test(&["%10.2f|", "1.5"], "      1.50|");
    printf_test(&["%f", "-0.5"], "-0.500000");
    // Floating-point operands accept the same forms as the integer ones.
    printf_test(&["%.1f", "'A"], "65.0");
    printf_status_test(&["%f", "abc"], "0.000000", 1);
}

#[test]
fn test_audit_p5_write_error_exit_status() {
    // #P5: a failed write must be diagnosed and must not exit 0, even when
    // the output does not end in a <newline> (the buffered case).
    if !Path::new("/dev/full").exists() {
        return;
    }

    for args in [&["hello\\n"][..], &["hello"][..], &["%s", "hello"][..]] {
        let devfull = File::options()
            .write(true)
            .open("/dev/full")
            .expect("failed to open /dev/full");

        let output = Command::new(get_binary_path("printf"))
            .args(args)
            .stdout(devfull)
            .output()
            .expect("failed to run printf");

        assert_eq!(
            output.status.code(),
            Some(1),
            "printf {args:?} > /dev/full should exit 1"
        );
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.starts_with("printf: "),
            "diagnostic should be prefixed with the utility name, got {stderr:?}"
        );
    }
}

#[test]
fn test_audit_p11_octal_escape_range() {
    // #P11 / POSIX 111944-111946: \ddd names "a byte with the numeric value
    // specified by the octal number".  Three digits can exceed a byte; the
    // low 8 bits are kept rather than an arbitrary value being produced.
    printf_bytes_test(&["\\400"], &[0x00]);
    printf_bytes_test(&["\\777"], &[0xff]);
    printf_bytes_test(&["\\101"], b"A");
    printf_bytes_test(&["\\0"], &[0x00]);
    printf_bytes_test(&["\\1"], &[0x01]);
    printf_bytes_test(&["\\18"], &[0x01, b'8']);
}

#[test]
fn test_audit_integer_operand_forms_unchanged() {
    // Guard the operand grammar the parser rewrite touched (112013-112018).
    printf_test(&["%d", "10"], "10");
    printf_test(&["%d", "010"], "8");
    printf_test(&["%d", "0x10"], "16");
    printf_test(&["%d", "0X10"], "16");
    printf_test(&["%d", "+3"], "3");
    printf_test(&["%d", "-3"], "-3");
    printf_test(&["%d", "'3"], "51");
    printf_test(&["%d", "\"+3"], "43");
    printf_test(&["%d", "0"], "0");
    printf_test(&["%d", ""], "0");
    printf_status_test(&["%d", "08"], "0", 1);
}
