//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The functions that write a string, folded to a `memcpy` of a known length
//
// `strcpy(d, "abc")` copies four bytes whatever `d` is, so once optimizing it
// is a `memcpy` of four -- and so are `stpcpy`, `strncpy`, `strcat`,
// `strncat` and `sprintf` of a string whose length is known.
//

use super::string_fold::{for_each_target, mentions};
use crate::common::{compile_and_run, compile_and_run_aarch64};

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

/// Every fold, checked against what the library writes and answers, with
/// sources the optimizer can see and sources it cannot.
fn values_program() -> String {
    let mut src = String::from(PROTOTYPES);
    src.push_str(
        r#"
static const char hello[] = "hello world";
char *unknown = "FGH";
volatile int vk = 1;
static int effects;
static char *bump(char *p) { effects++; return p; }

int main(void) {
    char d[64];
    const char *const s = "hello world";

    /* strcpy: a string, an offset into one, and a choice of equal lengths. */
    memset(d, 'X', sizeof d);
    if (strcpy(d, "abcde") != d || memcmp(d, "abcde\0X", 7)) return 1;
    if (strcpy(d + 16, "vwxyz" + 1) != d + 16 || memcmp(d + 16, "wxyz\0X", 6)) return 2;
    if (strcpy(d, vk ? "foo" : "bar") != d || memcmp(d, "foo\0", 4)) return 3;
    if (strcpy(d, hello) != d || memcmp(d, "hello world\0X", 13)) return 4;
    if (strcpy(d + 1, "") != d + 1 || memcmp(d, "h\0llo", 5)) return 5;

    /* stpcpy answers the terminator; unknown and unread, it is strcpy. */
    memset(d, 'X', sizeof d);
    if (stpcpy(d, "abcde") != d + 5 || memcmp(d, "abcde\0X", 7)) return 10;
    if (stpcpy(stpcpy(d, "ABCD"), "EFG") != d + 7 || memcmp(d, "ABCDEFG\0", 8)) return 11;
    stpcpy(d + 3, unknown);
    if (memcmp(d, "ABCFGH\0", 7)) return 12;
    if (stpcpy(d, unknown) != d + 3) return 13;

    /* strncpy cuts short, copies the terminator, or pads with zeros. */
    memset(d, 'X', sizeof d);
    if (strncpy(d, s, 4) != d || memcmp(d, "hellXXX", 7)) return 20;
    if (strncpy(d, s, 12) != d || memcmp(d, "hello world\0X", 13)) return 21;
    memset(d, 'X', sizeof d);
    if (strncpy(d, "ab", 8) != d || memcmp(d, "ab\0\0\0\0\0\0X", 9)) return 22;
    if (strncpy(bump(d), bump(d), 0) != d || effects != 2 || d[0] != 'a') return 23;
    memset(d, 'X', sizeof d);
    if (strncpy(d, vk ? "xfoo" + 1 : "bar", 4) != d || memcmp(d, "foo\0X", 5)) return 24;

    /* strcat appends at the end strlen finds; nested, and of "". */
    strcpy(d, s);
    if (strcat(d, "") != d || memcmp(d, "hello world\0", 12)) return 30;
    if (strcat(d + 5, " 2222") != d + 5 || memcmp(d, "hello world 2222\0", 17)) return 31;
    strcpy(d, "a");
    strcat(strcat(strcat(d, ": this "), ""), "is");
    if (memcmp(d, "a: this is\0", 11)) return 32;

    /* strncat: nothing for a bound of 0 or an empty source, whatever the
       arguments do; a bound of at least the length is strcat. */
    strcpy(d, s);
    if (strncat(bump(d), s, 0) != d || effects != 3 || memcmp(d, "hello world\0", 12)) return 40;
    if (strncat(d, "", ++effects) != d || effects != 4) return 41;
    if (strncat(d, "foo", 3) != d || memcmp(d, "hello worldfoo\0", 15)) return 42;
    if (strncat(d, "bar", 100) != d || memcmp(d, "hello worldfoobar\0", 18)) return 43;
    if (strncat(d, "xyz", 2) != d || memcmp(d, "hello worldfoobarxy\0", 20)) return 44;

    /* sprintf of no conversion, or of one %s. */
    memset(d, 'A', sizeof d);
    if (sprintf(d, "foo") != 3 || memcmp(d, "foo\0A", 5)) return 50;
    if (sprintf(d, "%s", "bar!") != 4 || memcmp(d, "bar!\0A", 6)) return 51;
    sprintf(d, "%s", unknown);
    if (memcmp(d, "FGH\0\0", 5)) return 52;
    if (sprintf(d, "%s", unknown) != 3) return 53;
    if (sprintf(d, "100%%") != 4 || memcmp(d, "100%\0", 5)) return 54;
    if (sprintf(d, "%d", 42) != 2 || memcmp(d, "42\0", 3)) return 55;
    return 0;
}
"#,
    );
    src
}

#[test]
fn builtins_copy_fold_values() {
    let code = values_program();
    for opt in ["-O0", "-O1", "-O2"] {
        let name = format!("copy_fold{}", opt.replace('-', "_"));
        assert_eq!(
            compile_and_run(&name, &code, &[opt.to_string()]),
            0,
            "host {opt}"
        );
    }
}

#[test]
fn builtins_copy_fold_values_aarch64() {
    let code = values_program();
    for opt in ["-O0", "-O2"] {
        let name = format!("copy_fold_a64{}", opt.replace('-', "_"));
        if let Some(rc) = compile_and_run_aarch64(&name, &code, opt) {
            assert_eq!(rc, 0, "aarch64 {opt}");
        }
    }
}

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
