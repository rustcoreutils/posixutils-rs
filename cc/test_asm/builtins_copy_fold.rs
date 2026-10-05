//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Assembly and compile-only cases of tests/builtins/copy_fold.rs, in process.
//

use super::asm_probe::mentions;
use super::builtins_string_fold::for_each_target;

/// What `<string.h>` and `<stdio.h>` declare, spelled out: the aarch64
/// programs are built without the target's headers.
const PROTOTYPES: &str = "typedef unsigned long size_t;\n\
    char *strcpy(char *, const char *);\n\
    char *stpcpy(char *, const char *);\n\
    char *strncpy(char *, const char *, size_t);\n\
    char *strcat(char *, const char *);\n\
    char *strncat(char *, const char *, size_t);\n\
    int sprintf(char *, const char *, ...);\n\
    int memcmp(const void *, const void *, size_t);\n\
    void *memset(void *, int, size_t);\n";

/// With the source's length known, the function is not called once
/// optimizing; `strcat` still asks `strlen` where the end is.
#[test]
fn builtins_copy_fold_no_call_for_known_strings() {
    let cases: &[(&str, &str)] = &[
        ("strcpy", "char *f(char *d) { return strcpy(d, \"abc\"); }"),
        (
            "strcpy",
            "char *f(char *d, int k) { return strcpy(d, k ? \"abc\" : \"xyz\"); }",
        ),
        ("stpcpy", "char *f(char *d) { return stpcpy(d, \"abc\"); }"),
        (
            "strncpy",
            "char *f(char *d) { return strncpy(d, \"abc\", 2); }",
        ),
        (
            "strncpy",
            "char *f(char *d) { return strncpy(d, \"abc\", 16); }",
        ),
        ("strcat", "char *f(char *d) { return strcat(d, \"abc\"); }"),
        (
            "strncat",
            "char *f(char *d) { return strncat(d, \"abc\", 9); }",
        ),
        (
            "strncat",
            "char *f(char *d, const char *s) { return strncat(d, s, 0); }",
        ),
        ("sprintf", "int f(char *d) { return sprintf(d, \"abc\"); }"),
        (
            "sprintf",
            "int f(char *d) { return sprintf(d, \"%s\", \"abc\"); }",
        ),
    ];
    for (name, body) in cases {
        let src = format!("{PROTOTYPES}{body}\n");
        for_each_target("copy_fold_known", &src, &["-O2"], |asm, what| {
            assert!(!mentions(asm, name), "{what}: {name} was called:\n{asm}");
        });
    }
    let strcat = format!("{PROTOTYPES}char *f(char *d) {{ return strcat(d, \"abc\"); }}\n");
    for_each_target("copy_fold_strcat", &strcat, &["-O2"], |asm, what| {
        assert!(mentions(asm, "strlen"), "{what}: no strlen:\n{asm}");
    });
}

/// An unknown source whose result nobody reads is copied by `strcpy`; one
/// whose result is read, or a length the fold cannot bound, stays a call.
#[test]
fn builtins_copy_fold_unknown_sources() {
    let to_strcpy: &[(&str, &str)] = &[
        ("stpcpy", "void f(char *d, const char *s) { stpcpy(d, s); }"),
        (
            "sprintf",
            "void f(char *d, const char *s) { sprintf(d, \"%s\", s); }",
        ),
    ];
    for (name, body) in to_strcpy {
        let src = format!("{PROTOTYPES}{body}\n");
        for_each_target("copy_fold_strcpy", &src, &["-O2"], |asm, what| {
            assert!(!mentions(asm, name), "{what}: {name} was called:\n{asm}");
            assert!(mentions(asm, "strcpy"), "{what}: no strcpy:\n{asm}");
        });
    }
    let kept: &[(&str, &str)] = &[
        (
            "strcpy",
            "char *f(char *d, const char *s) { return strcpy(d, s); }",
        ),
        (
            "stpcpy",
            "char *f(char *d, const char *s) { return stpcpy(d, s); }",
        ),
        (
            "strncpy",
            "char *f(char *d, size_t n) { return strncpy(d, \"abc\", n); }",
        ),
        (
            "strncat",
            "char *f(char *d) { return strncat(d, \"abc\", 2); }",
        ),
        (
            "sprintf",
            "int f(char *d, const char *s) { return sprintf(d, \"%s\", s); }",
        ),
        (
            "sprintf",
            "int f(char *d) { return sprintf(d, \"%d\", 4); }",
        ),
    ];
    for (name, body) in kept {
        let src = format!("{PROTOTYPES}{body}\n");
        for_each_target("copy_fold_kept", &src, &["-O2"], |asm, what| {
            assert!(mentions(asm, name), "{what}: no call to {name}:\n{asm}");
        });
    }
    let known = format!("{PROTOTYPES}char *f(char *d) {{ return strcpy(d, \"abc\"); }}\n");
    for_each_target("copy_fold_o0", &known, &["-O0"], |asm, what| {
        assert!(mentions(asm, "strcpy"), "{what}: no call to strcpy:\n{asm}");
    });
}

/// The calls a fold makes go to the program's names for `strcpy` and
/// `memcpy`, asm label and all.
#[test]
fn builtins_copy_fold_follows_asm_labels() {
    let long = "x".repeat(200);
    let src = format!(
        "typedef unsigned long size_t;\n\
         char *stpcpy(char *, const char *);\n\
         char *strcpy(char *, const char *) __asm__(\"my_strcpy\");\n\
         void *memcpy(void *, const void *, size_t) __asm__(\"my_memcpy\");\n\
         void f(char *d, const char *s) {{ stpcpy(d, s); }}\n\
         char *g(char *d) {{ return strcpy(d, \"{long}\"); }}\n"
    );
    for_each_target("copy_fold_asm", &src, &["-O2"], |asm, what| {
        assert!(mentions(asm, "my_strcpy"), "{what}: no my_strcpy:\n{asm}");
        assert!(mentions(asm, "my_memcpy"), "{what}: no my_memcpy:\n{asm}");
        assert!(
            !mentions(asm, "strcpy"),
            "{what}: unlabelled strcpy:\n{asm}"
        );
        assert!(
            !mentions(asm, "memcpy"),
            "{what}: unlabelled memcpy:\n{asm}"
        );
    });
}
