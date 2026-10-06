//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// gcc's `-Woverflow` for an integer constant that an implicit conversion
// changes: C17 6.3.1.3p3 leaves the result implementation-defined, gcc
// reduces it modulo 2^N, and says so. Every expected line is gcc 13's for
// the target named.
//

use crate::test_compile::compile_warnings;

const X86_64: &str = "--target=x86_64-unknown-linux-gnu";
const AARCH64: &str = "--target=aarch64-unknown-linux-gnu";

/// Every static-initializer shape: a scalar at file scope and at block
/// scope, a member, an array element, a bit-field of each signedness and of
/// each width class, an enumerated object. gcc names a bit-field by the
/// narrowest type that holds its width, and a full-width one by its own.
#[test]
fn diagnostics_static_initializer_conversion_overflow_warns() {
    let src = "char z = 300;\n\
               unsigned char uc = 300;\n\
               short s = 70000;\n\
               unsigned short us = 70000;\n\
               struct S { char c; int bf : 3; unsigned ubf : 2; } st = { 300, 9, 5 };\n\
               char arr[3] = { 1, 256, 1000 };\n\
               int i = 4294967297LL;\n\
               unsigned u2 = 4294967296LL;\n\
               signed char m1 = -129;\n\
               int f(void) { static char lz = 300; return lz; }\n\
               struct T { int a : 10; unsigned b : 20; long long e : 40; int w : 32; } t = { 2000, 2000000, 2000000000000LL, 5000000000LL };\n\
               enum E { A, B = 1000 };\n\
               enum E e = 4294967296LL;\n\
               unsigned char x1 = 300u;\n\
               char x2 = 'a' + 200;\n";
    assert_eq!(
        compile_warnings("cvo_static", src, &[X86_64]),
        [
            "1:10: overflow in conversion from 'int' to 'char' changes value from '300' to '44'",
            "2:20: unsigned conversion from 'int' to 'unsigned char' changes value from '300' to '44'",
            "3:11: overflow in conversion from 'int' to 'short int' changes value from '70000' to '4464'",
            "4:21: unsigned conversion from 'int' to 'short unsigned int' changes value from '70000' to '4464'",
            "5:59: overflow in conversion from 'int' to 'char' changes value from '300' to '44'",
            "5:64: overflow in conversion from 'int' to 'signed char:3' changes value from '9' to '1'",
            "5:67: unsigned conversion from 'int' to 'unsigned char:2' changes value from '5' to '1'",
            "6:20: overflow in conversion from 'int' to 'char' changes value from '256' to '0'",
            "6:25: overflow in conversion from 'int' to 'char' changes value from '1000' to '-24'",
            "7:9: overflow in conversion from 'long long int' to 'int' changes value from '4294967297' to '1'",
            "8:15: unsigned conversion from 'long long int' to 'unsigned int' changes value from '4294967296' to '0'",
            "9:18: overflow in conversion from 'int' to 'signed char' changes value from '-129' to '127'",
            "10:32: overflow in conversion from 'int' to 'char' changes value from '300' to '44'",
            "11:79: overflow in conversion from 'int' to 'short int:10' changes value from '2000' to '-48'",
            "11:85: unsigned conversion from 'int' to 'unsigned int:20' changes value from '2000000' to '951424'",
            "11:94: overflow in conversion from 'long long int' to 'long int:40' changes value from '2000000000000' to '-199023255552'",
            "11:111: overflow in conversion from 'long long int' to 'int' changes value from '5000000000' to '705032704'",
            "13:12: unsigned conversion from 'long long int' to 'enum E' changes value from '4294967296' to '0'",
            "14:20: conversion from 'unsigned int' to 'unsigned char' changes value from '300' to '44'",
            "15:11: overflow in conversion from 'int' to 'char' changes value from '297' to '41'",
        ]
    );
}

/// Plain `char` is unsigned on aarch64 Linux, so the conversion to it is an
/// unsigned one there, and a negative value that fits `signed char` is not
/// reported.
#[test]
fn diagnostics_plain_char_conversion_follows_the_target() {
    let src = "char z = 300;\nchar y = -1;\nchar w = -200;\n";
    assert_eq!(
        compile_warnings("cvo_char_a64", src, &[AARCH64]),
        [
            "1:10: unsigned conversion from 'int' to 'char' changes value from '300' to '44'",
            "3:10: unsigned conversion from 'int' to 'char' changes value from '-200' to '56'",
        ]
    );
    assert_eq!(
        compile_warnings("cvo_char_x86", src, &[X86_64]),
        [
            "1:10: overflow in conversion from 'int' to 'char' changes value from '300' to '44'",
            "3:10: overflow in conversion from 'int' to 'char' changes value from '-200' to '56'",
        ]
    );
}

/// The same conversion in code: initialization, assignment -- to a
/// bit-field too -- a prototyped argument and `return`.
#[test]
fn diagnostics_runtime_conversion_overflow_warns() {
    let src = "struct V { int bf : 3; unsigned ub : 2; } v;\n\
               void h(char);\n\
               char g(void) { char lc = 300; lc = 400; unsigned char q = -500; h(300); v.bf = 9; v.ub = 5; return 300 + lc + q; }\n\
               void k(void) { struct V w = { 9, 5 }; (void)w; }\n";
    assert_eq!(
        compile_warnings("cvo_runtime", src, &[X86_64]),
        [
            "3:26: overflow in conversion from 'int' to 'char' changes value from '300' to '44'",
            "3:36: overflow in conversion from 'int' to 'char' changes value from '400' to '-112'",
            "3:59: unsigned conversion from 'int' to 'unsigned char' changes value from '-500' to '12'",
            "3:67: overflow in conversion from 'int' to 'char' changes value from '300' to '44'",
            "3:80: overflow in conversion from 'int' to 'signed char:3' changes value from '9' to '1'",
            "3:90: unsigned conversion from 'int' to 'unsigned char:2' changes value from '5' to '1'",
            "4:31: overflow in conversion from 'int' to 'signed char:3' changes value from '9' to '1'",
            "4:34: unsigned conversion from 'int' to 'unsigned char:2' changes value from '5' to '1'",
        ]
    );
}

/// What gcc leaves alone: a value that fits the type's other signedness, a
/// cast, a value known only at run time, and `_Bool`, whose conversion is a
/// comparison. `-Wno-overflow` silences the rest.
#[test]
fn diagnostics_conversion_that_gcc_accepts_is_silent() {
    let src = "signed char sc = 200;\n\
               char c = 255;\n\
               unsigned char ucn = -1;\n\
               unsigned u = -1;\n\
               unsigned short usn = -32768;\n\
               char cast = (char)300;\n\
               _Bool b = 2;\n\
               struct B { _Bool g : 1; int h : 1; } bb = { 2, 1 };\n\
               int f(int x) { char a = x ? 300 : 2; return a; }\n";
    for target in [X86_64, AARCH64] {
        let got = compile_warnings("cvo_silent", src, &[target]);
        assert!(
            !got.iter().any(|w| w.contains("overflow")),
            "{target}: {got:?}"
        );
    }
    let quiet = compile_warnings("cvo_wno", "char z = 300;\n", &["-Wno-overflow"]);
    assert!(quiet.is_empty(), "{quiet:?}");
}

/// gcc's `-pedantic` adds the case that fits only the other signedness, when
/// the conversion narrows: `signed char sc = 200;`.
#[test]
fn diagnostics_pedantic_conversion_overflow_warns() {
    let got = compile_warnings(
        "cvo_pedantic",
        "signed char sc = 200;\nunsigned u = -1;\nint i = 0x80000000;\n",
        &["-pedantic", X86_64],
    );
    assert_eq!(
        got,
        ["1:18: overflow in conversion from 'int' to 'signed char' changes value from '200' to '-56'"]
    );
}

/// The type names are gcc's in the floating form of the warning too.
#[test]
fn diagnostics_saturated_conversion_names_gccs_types() {
    let got = compile_warnings(
        "cvo_float_names",
        "short s = 1.0e10;\nunsigned long lu = -1e30;\n",
        &[X86_64],
    );
    assert_eq!(
        got,
        [
            "1:11: overflow in conversion from 'double' to 'short int' changes value",
            "2:20: overflow in conversion from 'double' to 'long unsigned int' changes value",
        ]
    );
}
