//
// Copyright (c) 2024 Jeff Garzik
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::{run_test, TestPlan};

fn tr_test(args: &[&str], test_data: &str, expected_output: &str) {
    let str_args = args
        .iter()
        .map(|st| st.to_owned().to_owned())
        .collect::<Vec<String>>();

    run_test(TestPlan {
        cmd: "tr".to_owned(),
        args: str_args,
        stdin_data: test_data.to_owned(),
        expected_out: expected_output.to_owned(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

fn tr_bad_arguments_failure_test(args: &[&str], expected_stderr: &str) {
    let str_args = args
        .iter()
        .map(|st| st.to_owned().to_owned())
        .collect::<Vec<_>>();

    run_test(TestPlan {
        cmd: "tr".to_owned(),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: expected_stderr.to_owned(),
        expected_exit_code: 1,
    });
}

#[test]
fn test_tr_1() {
    tr_test(&["abcd", "[]*]"], "abcd", "]]]]");
}

#[test]
fn tr_2() {
    tr_test(&["abc", "[%*]xyz"], "abc", "xyz");
}

#[test]
fn tr_3() {
    tr_test(&["abcd", "xy"], "abcde", "xyyye");
}

#[test]
fn tr_4() {
    tr_test(&["abcd", "x[y*]"], "abcde", "xyyye");
}

#[test]
fn tr_5() {
    tr_test(&["-s", "a-p", "%[.*]$"], "abcdefghijklmnop", "%.$");
}

#[test]
fn tr_6() {
    tr_test(&["-s", "a-p", "[.*]$"], "abcdefghijklmnop", ".$");
}

#[test]
fn tr_7() {
    tr_test(&["-s", "a-p", "%[.*]"], "abcdefghijklmnop", "%.");
}

#[test]
fn tr_a() {
    tr_test(&["-s", "[a-z]"], "aabbcc", "abc");
}

#[test]
fn tr_b() {
    tr_test(&["-s", "[a-c]"], "aabbcc", "abc");
}

#[test]
fn tr_c() {
    tr_test(&["-s", "[a-b]"], "aabbcc", "abcc");
}

#[test]
fn tr_d() {
    tr_test(&["-s", "[b-c]"], "aabbcc", "aabc");
}

#[test]
fn tr_f() {
    tr_test(&["-d", "[=[=]"], "[[[[[[[[]]]]]]]]", "]]]]]]]]");
}

#[test]
fn tr_g() {
    tr_test(&["-d", "[=]=]"], "[[[[[[[[]]]]]]]]", "[[[[[[[[");
}

#[test]
fn tr_h() {
    tr_test(&["-d", "[:xdigit:]"], "0123456789acbdefABCDEF", "");
}

#[test]
fn tr_i() {
    tr_test(
        &["-d", "[:xdigit:]"],
        "w0x1y2z3456789acbdefABCDEFz",
        "wxyzz",
    );
}

#[test]
fn tr_j() {
    tr_test(&["-d", "[:digit:]"], "0123456789", "");
}

#[test]
fn tr_k() {
    tr_test(&["-d", "[:digit:]"], "a0b1c2d3e4f5g6h7i8j9k", "abcdefghijk");
}

#[test]
fn tr_l() {
    tr_test(&["-d", "[:lower:]"], "abcdefghijklmnopqrstuvwxyz", "");
}

#[test]
fn tr_m() {
    tr_test(&["-d", "[:upper:]"], "ABCDEFGHIJKLMNOPQRSTUVWXYZ", "");
}

#[test]
fn tr_n() {
    tr_test(
        &["-d", "[:lower:][:upper:]"],
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
        "",
    );
}

#[test]
fn tr_o() {
    tr_test(
        &["-d", "[:alpha:]"],
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
        "",
    );
}

#[test]
fn tr_p() {
    tr_test(
        &["-d", "[:alnum:]"],
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789",
        "",
    );
}

#[test]
fn tr_q() {
    tr_test(
        &["-d", "[:alnum:]"],
        ".abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.",
        "..",
    );
}

#[test]
fn tr_r() {
    tr_test(
        &["-ds", "[:alnum:]", "."],
        ".abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.",
        ".",
    );
}

#[test]
fn tr_s() {
    tr_test(
        &["-c", "[:alnum:]", "\n"],
        "The big black fox jumped over the fence.",
        "The\nbig\nblack\nfox\njumped\nover\nthe\nfence\n",
    );
}

#[test]
fn tr_t() {
    tr_test(
        &["-c", "[:alnum:]", "[\n*]"],
        "The big black fox jumped over the fence.",
        "The\nbig\nblack\nfox\njumped\nover\nthe\nfence\n",
    );
}

#[test]
fn tr_u() {
    tr_test(&["-ds", "b", "a"], "aabbaa", "a");
}

#[test]
fn tr_v() {
    tr_test(
        &["-ds", "[:xdigit:]", "Z"],
        "ZZ0123456789acbdefABCDEFZZ",
        "Z",
    );
}

#[test]
fn tr_w() {
    tr_test(
        &["-ds", "\u{350}", "\u{345}"],
        "\u{300}\u{301}\u{377}\u{345}\u{345}\u{350}\u{345}",
        "\u{300}\u{301}\u{377}\u{345}",
    );
}

#[test]
fn tr_x() {
    tr_test(
        &["-s", "abcdefghijklmn", "[:*016]"],
        "abcdefghijklmnop",
        ":op",
    );
}

#[test]
fn tr_y() {
    tr_test(&["-d", "a-z"], "abc $code", " $");
}

#[test]
fn tr_z() {
    tr_test(&["-ds", "a-z", "$."], "a.b.c $$$$code\\", ". $\\");
}

#[test]
fn tr_range_a_a() {
    tr_test(&["a-a", "z"], "abc", "zbc");
}

#[test]
fn tr_upcase() {
    tr_test(&["[:lower:]", "[:upper:]"], "abcxyzABCXYZ", "ABCXYZABCXYZ");
}

#[test]
fn tr_dncase() {
    tr_test(&["[:upper:]", "[:lower:]"], "abcxyzABCXYZ", "abcxyzabcxyz");
}

#[test]
fn tr_rep_2() {
    tr_test(&["a[b*512]c", "1[x*]2"], "abc", "1x2");
}

#[test]
fn tr_rep_3() {
    tr_test(&["a[b*513]c", "1[x*]2"], "abc", "1x2");
}

#[test]
fn tr_o_rep_2() {
    tr_test(&["[b*010]cd", "[a*7]BC[x*]"], "bcd", "BCx");
}

#[test]
fn tr_ross_1a() {
    tr_test(&["-cs", "[:upper:]", "[X*]"], "AMZamz123.-+AMZ", "AMZXAMZ");
}

#[test]
fn tr_ross_1b() {
    tr_test(&["-cs", "[:upper:][:digit:]", "[Z*]"], "", "");
}

#[test]
fn tr_ross_2() {
    // Modified expected output to match other implementations
    // "amzam" -> "amzamz"
    tr_test(
        &["-dcs", "[:lower:]", "n-rs-z"],
        "amzAMZ123.-+amz",
        "amzamz",
    );
}

#[test]
fn tr_ross_3() {
    tr_test(
        &["-ds", "[:xdigit:]", "[:alnum:]"],
        ".ZABCDEFzabcdefg.0123456788899.GG",
        ".Zzg..G",
    );
}

#[test]
fn tr_ross_4() {
    tr_test(&["-dcs", "[:alnum:]", "[:digit:]"], "", "");
}

#[test]
fn tr_ross_5() {
    tr_test(&["-dc", "[:lower:]"], "", "");
}

#[test]
fn tr_ross_6() {
    tr_test(&["-dc", "[:upper:]"], "", "");
}

#[test]
fn tr_repeat_0() {
    tr_test(&["abc", "[b*0]"], "abcd", "bbbd");
}

#[test]
fn tr_repeat_zeros() {
    tr_test(&["abc", "[b*00000000000000000000]"], "abcd", "bbbd");
}

#[test]
fn tr_repeat_compl() {
    tr_test(&["-c", "[a*65536]\n", "[b*]"], "abcd", "abbb");
}

#[test]
fn tr_repeat_xc() {
    tr_test(&["-C", "[a*65536]\n", "[b*]"], "abcd", "abbb");
}

#[test]
fn tr_no_abort_1() {
    tr_test(&["-c", "a", "[b*256]"], "abc", "abb");
}

#[test]
fn tr_d_space_n() {
    let input = "\
 54 68 69 73 20 69 73 20 61 20 73 75 69 74 65 20
 6f 66 20 52 75 73 74 2d 6e 61 74 69 76 65 20 63
 6f 72 65 20 63 6f 6d 6d 61 6e 64 20 6c 69 6e 65
 20 75 74 69 6c 74 69 65 73 20 28 63 70 2c 20 6d
 76 2c 0a 61 77 6b 2c 20 6d 61 6b 65 2c 20 76 69
 2c 20 2e 2e 2e 29 20 75 73 69 6e 67 20 50 4f 53
 49 58 2e 32 30 32 34 20 61 73 20 74 68 65 20 62
 61 73 65 6c 69 6e 65 20 73 70 65 63 69 66 69 63
 61 74 69 6f 6e 2e 0a
";

    let output = "\
546869732069732061207375697465206f6620527573742d6e617469766520636f726520636f6d6d616e64206c696e65207574696c74696573202863702c206d762c0a61776b2c206d616b652c2076692c202e2e2e29207573696e6720504f5349582e323032342061732074686520626173656c696e652073706563696669636174696f6e2e0a\
";

    tr_test(&["-d", r" \n"], input, output);
}

#[test]
fn tr_ignored_backslash() {
    tr_test(&["-d", r"\z"], "xyz", "xy");
}

#[test]
fn tr_escaped_backslash() {
    tr_test(&["-d", r"\\"], r"a\b\c", "abc");
}

#[test]
fn tr_octal_sequence() {
    tr_test(&["2", r"\44"], "123\n", "1$3\n");
}

#[test]
fn tr_octal_sequence_two() {
    tr_test(&["21", r"\44Z"], "123123\n", "Z$3Z$3\n");
}

#[test]
fn tr_reverse_order_simple() {
    tr_test(&["21", "AB"], "12\n", "BA\n");
}

#[test]
fn tr_duplicate_string1_character_precedence() {
    tr_test(&["AA", "CB"], "AAAA", "BBBB");
}

// Prevent regression to:
//
//    Error: Missing symbol after '['
#[test]
fn tr_left_square_bracket_literal() {
    tr_test(&["1", "["], "123", "[23");
}

// Prevent regression to:
//
//    Error: Missing '*' after '[' for symbol ':'
#[test]
fn tr_multiple_transformations() {
    tr_test(&["3[:lower:]", "![:upper:]"], "abc123", "ABC12!");
}

#[test]
fn tr_equiv_not_one_char() {
    tr_bad_arguments_failure_test(
        &["-d", "[=aa=]"],
        "tr: aa: equivalence class operand must be a single character\n",
    );
}

#[test]
fn tr_backwards_range_normal() {
    tr_bad_arguments_failure_test(
        &["-d", "b-a"],
        "tr: range-endpoints of 'b-a' are in reverse collating sequence order\n",
    );
}

#[test]
fn tr_backwards_range_backslash() {
    tr_bad_arguments_failure_test(
        &["-d", r"\t-\b"],
        r"tr: range-endpoints of '\t-\u{8}' are in reverse collating sequence order
",
    );
}

#[test]
fn tr_backwards_range_octal() {
    tr_bad_arguments_failure_test(
        &["-d", r"\045-\044"],
        "tr: range-endpoints of '%-$' are in reverse collating sequence order\n",
    );
}

#[test]
fn tr_backwards_range_mixed() {
    tr_bad_arguments_failure_test(
        &["-d", r"A-\t"],
        r"tr: range-endpoints of 'A-\t' are in reverse collating sequence order
",
    );
}

#[test]
fn tr_mixed_range() {
    tr_test(
        &["-d", r"\044-Z"],
        "$123456789ABCDEFGHIabcdefghi",
        "abcdefghi",
    );
}

#[test]
fn tr_two_ranges() {
    tr_test(&["ab12", r"\044-\045Y-Z"], "21ba", "ZY%$");
}

#[test]
fn tr_bad_octal_range() {
    tr_bad_arguments_failure_test(
        &["-d", r"\046-\048"],
        r"tr: range-endpoints of '&-\u{4}' are in reverse collating sequence order
",
    );
}

#[test]
fn tr_bad_x_n_construct_decimal() {
    tr_bad_arguments_failure_test(
        &["-d", "[a*100000000000000000000]"],
        "tr: invalid repeat count ‘100000000000000000000’ in [c*n] construct\n",
    );
}

#[test]
fn tr_bad_x_n_construct_octal() {
    tr_bad_arguments_failure_test(
        &["-d", "[a*010000000000000000000000]"],
        "tr: invalid repeat count ‘010000000000000000000000’ in [c*n] construct\n",
    );
}

#[test]
fn tr_bad_x_n_construct_non_decimal_non_octal() {
    tr_bad_arguments_failure_test(
        &["-d", "[a*a]"],
        "tr: invalid repeat count ‘a’ in [c*n] construct\n",
    );
}

#[test]
fn tr_trailing_hyphen() {
    tr_test(&["ab", "c-"], "abc123", "c-c123");
}

#[test]
fn tr_backslash_range() {
    tr_test(
        &["1-9", r"\b-\r"],
        r"\ 987654321 -",
        "\\ \x0D\x0D\x0D\x0D\x0C\x0B\x0A\x09\x08 -",
    );
}

#[test]
fn tr_fill_with_last_char() {
    tr_test(&["1-34-8", "A-C!"], "987654321", "9!!!!!CBA");
}

#[test]
fn tr_octal_above_one_byte_value() {
    let args = &["-d", r"\501"];

    let str_args = args
        .iter()
        .map(|st| st.to_owned().to_owned())
        .collect::<Vec<String>>();

    run_test(TestPlan {
        cmd: "tr".to_owned(),
        args: str_args,
        stdin_data: "(1Ł)".to_owned(),
        expected_out: "Ł)".to_owned(),
        expected_err: r"tr: warning: the ambiguous octal escape \501 is being interpreted as the 2-byte sequence \050, 1
".to_owned(),
        expected_exit_code: 0_i32,
    });
}

#[test]
fn tr_short_octal_with_non_octal_digits_after() {
    // Interpret as \004, '8', and the range from '1' through '3'
    tr_test(&["-d", r"\0481-3"], "A 123 \x04 456 789 Z", "A   456 79 Z");
}

#[test]
fn tr_octal_parsing_ambiguous() {
    // "If an ordinary digit (representing itself) is to follow an octal sequence, the octal sequence must use the full three digits to avoid ambiguity."
    // https://pubs.opengroup.org/onlinepubs/9799919799/utilities/tr.html
    // Interpret as \123, not \012 and '3'
    tr_test(
        &["-d", r"\123"],
        "321 \\ \x0A \x53 \x50 \x02 \x01 \\ CBA",
        "321 \\ \x0A  \x50 \x02 \x01 \\ CBA",
    );
}

#[test]
fn tr_octal_parsing_non_ambiguous() {
    // See above
    // Interpret as \012 and 'A'
    tr_test(
        &["-d", r"\12A"],
        "321 \\ \x0A \x53 \x50 \x02 \x01 \\ CBA",
        "321 \\  \x53 \x50 \x02 \x01 \\ CB",
    );
}

#[test]
fn tr_equiv_class_and_other_deletions() {
    tr_test(&["-d", "4[=a=]2"], "1 3 A a 2 4", "1 3 A   ");
}

#[test]
fn tr_string2_equiv_inappropriate() {
    tr_bad_arguments_failure_test(
        &["1", "[=a=]"],
        "tr: [=c=] expressions may not appear in string2 when translating\n",
    );
}

#[test]
fn tr_equivalence_class_low_priority() {
    const INPUT: &str = "aaa bbb ccc 123";
    const OUTPUT: &str = "YYY bbb ccc 123";

    tr_test(&["[=a=]a", "XY"], INPUT, OUTPUT);

    tr_test(&["a[=a=]", "XY"], INPUT, OUTPUT);
}

#[test]
fn tr_arguments_validation_error_message_format() {
    tr_bad_arguments_failure_test(
        &["a"],
        "tr: missing operand after ‘a’. Two strings must be given when translating.\n",
    );
}

// POSIX does not specify how invalid backslash sequences are handled, so there is some flexibility here
// Still, something useful should be done (for instance, tr should not abort in this case)
#[test]
fn tr_ranges_with_invalid_escape_sequences() {
    const INPUT: &str = "abcdef ABCDEF -\\ \x07 -\\ 123456789";

    // "\7-\9" is:
    //     treated as a range from \007 through '9' by bsdutils and GNU Core Utilities
    //     treated as: 1) a range from \007 through '\', and 2) separately the character '9', by BusyBox
    tr_test(&["-d", r"\7-\9"], INPUT, r"abcdefABCDEF\\");

    // Similar to above
    tr_test(&["-d", r"\7-\A"], INPUT, r"abcdefBCDEF\\");
}

// Make sure state is persisted through multiple calls to `transform`
#[test]
fn tr_streaming_state() {
    let a_s = "a".repeat(16_usize * 1_024_usize);

    tr_test(&["-s", "a", "b"], &a_s, "b");
}

#[test]
fn tr_minimal_d_s() {
    tr_test(&["-d", "-s", "", "A"], "1AA", "1A");
}

#[test]
fn tr_missing_equiv() {
    tr_bad_arguments_failure_test(
        &["-d", "[==]"],
        "tr: missing equivalence class character '[==]'\n",
    );
}

#[test]
fn tr_missing_character_class() {
    tr_bad_arguments_failure_test(&["-d", "[::]"], "tr: missing character class name '[::]'\n");
}
