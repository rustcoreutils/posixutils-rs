//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{TestPlan, run_test};

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
