//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// A static initializer is converted to its object's type before it is
// emitted (C17 6.7.9p11, 6.3.1.3p3: gcc reduces an out-of-range value modulo
// 2^N). `char z = 300;` was emitted as `.byte 300`, which the assembler
// truncates with a warning of its own.
//

use super::asm_probe::{asm_for, AARCH64_LINUX, X86_64_LINUX};

const SRC: &str = "char z = 300;\n\
                   signed char sc = 200;\n\
                   unsigned char uc = 300;\n\
                   short s = 70000;\n\
                   unsigned short us = 70000;\n\
                   _Bool b = 2;\n\
                   struct S { char c; short s; } st = { 300, 70000 };\n\
                   char arr[3] = { 255, 256, 1000 };\n\
                   int i = 4294967297LL;\n\
                   unsigned u2 = 4294967298LL;\n\
                   enum E { A } e = 4294967297LL;\n\
                   int f(void) { static char lz = 300; static short ls[2] = { 70000, 1 }; return lz + ls[0]; }\n";

/// The first data directives after `label:`, up to the next label.
fn data_after<'a>(asm: &'a str, label: &str) -> Vec<&'a str> {
    let mut lines = asm.lines().skip_while(|l| l.trim() != format!("{label}:"));
    lines
        .next()
        .unwrap_or_else(|| panic!("no {label} in:\n{asm}"));
    lines
        .map(str::trim)
        .take_while(|l| !l.ends_with(':'))
        .filter(|l| l.starts_with('.'))
        .collect()
}

/// Every integer data directive holds a value its width can represent, as
/// a signed or an unsigned number.
fn assert_every_value_fits(asm: &str, triple: &str) {
    for line in asm.lines().map(str::trim) {
        let Some((directive, value)) = line.split_once(' ') else {
            continue;
        };
        let bits = match directive {
            ".byte" => 8,
            ".short" | ".hword" | ".value" | ".2byte" => 16,
            ".long" | ".word" | ".4byte" => 32,
            _ => continue,
        };
        let Ok(v) = value.trim().parse::<i128>() else {
            continue;
        };
        assert!(
            v >= -(1i128 << (bits - 1)) && v < (1i128 << bits),
            "{triple}: '{line}' does not fit {bits} bits:\n{asm}"
        );
    }
}

#[test]
fn codegen_static_initializer_is_converted_to_the_object_type() {
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for("narrow_static_init", triple, SRC);
        assert_every_value_fits(&asm, triple);
        let first = |label: &str| {
            data_after(&asm, label)[0]
                .split_whitespace()
                .last()
                .map(str::to_string)
        };
        assert_eq!(first("z").as_deref(), Some("44"), "{triple}");
        assert_eq!(first("uc").as_deref(), Some("44"), "{triple}");
        assert_eq!(first("s").as_deref(), Some("4464"), "{triple}");
        assert_eq!(first("us").as_deref(), Some("4464"), "{triple}");
        assert_eq!(first("b").as_deref(), Some("1"), "{triple}");
        assert_eq!(first("i").as_deref(), Some("1"), "{triple}");
        assert_eq!(first("u2").as_deref(), Some("2"), "{triple}");
        assert_eq!(first("e").as_deref(), Some("1"), "{triple}");
        let arr: Vec<_> = data_after(&asm, "arr")
            .iter()
            .filter_map(|l| l.strip_prefix(".byte "))
            .map(|v| v.trim().parse::<i128>().unwrap() & 0xff)
            .collect();
        assert_eq!(arr, [255, 0, 0xe8], "{triple}");
    }
}
