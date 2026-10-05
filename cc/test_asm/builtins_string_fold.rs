//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Assembly and compile-only cases of tests/builtins/string_fold.rs, in process.
//

use super::asm_probe::{asm_prefix, mentions};
use crate::test_compile::asm_for;

/// What `<string.h>` declares, spelled out: the aarch64 programs are built
/// without the target's headers.
const PROTOTYPES: &str = "typedef unsigned long size_t;\n\
    size_t strlen(const char *);\n\
    size_t strnlen(const char *, size_t);\n\
    int strcmp(const char *, const char *);\n\
    int strncmp(const char *, const char *, size_t);\n\
    int memcmp(const void *, const void *, size_t);\n\
    char *strchr(const char *, int);\n\
    char *strrchr(const char *, int);\n\
    char *index(const char *, int);\n\
    char *rindex(const char *, int);\n\
    void *memchr(const void *, int, size_t);\n\
    char *strstr(const char *, const char *);\n\
    char *strpbrk(const char *, const char *);\n\
    size_t strcspn(const char *, const char *);\n";

/// Compile `src` at `opt` for the host and for aarch64, and hand the
/// assembly to `check`.
pub(super) fn for_each_target(prefix: &str, src: &str, extra: &[&str], check: impl Fn(&str, &str)) {
    for target in [None, Some("aarch64-unknown-linux-gnu")] {
        let mut args = extra.to_vec();
        if target.is_some() {
            args.push("--target=aarch64-unknown-linux-gnu");
        }
        let asm = asm_for(prefix, src, &args);
        check(&asm, target.unwrap_or("host"));
    }
}

/// With every argument known, nothing is called once optimizing.
#[test]
fn builtins_string_fold_no_call_for_known_strings() {
    let cases: &[(&str, &str)] = &[
        ("strlen", "size_t f(void) { return strlen(\"hello\"); }"),
        (
            "strnlen",
            "size_t f(size_t n) { return strnlen(\"abc\", n); }",
        ),
        ("strcmp", "int f(void) { return strcmp(\"abc\", \"abd\"); }"),
        ("strcmp", "int f(const char *p) { return strcmp(p, \"\"); }"),
        (
            "strncmp",
            "int f(const char *p, const char *q) { return strncmp(p, q, 1); }",
        ),
        (
            "memcmp",
            "int f(void) { return memcmp(\"ab\", \"ac\", 2); }",
        ),
        (
            "strchr",
            "char *f(void) { const char *s = \"hello\"; return strchr(s, 'l'); }",
        ),
        (
            "strrchr",
            "char *f(void) { return strrchr(\"hello\", 'l'); }",
        ),
        (
            "memchr",
            "void *f(void) { return memchr(\"hello\", 'l', 5); }",
        ),
        (
            "strstr",
            "char *f(void) { return strstr(\"hello\", \"ll\"); }",
        ),
        (
            "strpbrk",
            "char *f(void) { return strpbrk(\"hello\", \"xl\"); }",
        ),
        (
            "strcspn",
            "size_t f(void) { return strcspn(\"hello\", \"l\"); }",
        ),
    ];
    for (name, body) in cases {
        let src = format!("{PROTOTYPES}{body}\n");
        for_each_target("string_fold_known", &src, &["-O2"], |asm, what| {
            assert!(!mentions(asm, name), "{what}: {name} was called:\n{asm}");
        });
    }
}

/// Where the arguments do not decide the result, the call stays -- and at
/// `-O0` every call stays but a comparison of no bytes.
#[test]
fn builtins_string_fold_keeps_the_call_otherwise() {
    let unknown: &[(&str, &str)] = &[
        ("strlen", "size_t f(const char *p) { return strlen(p); }"),
        (
            "strcmp",
            "int f(const char *p) { return strcmp(p, \"a\"); }",
        ),
        (
            "strncmp",
            "int f(const char *p, const char *q) { return strncmp(p, q, 2); }",
        ),
        (
            "strchr",
            "char *f(const char *p) { return strchr(p, 'a'); }",
        ),
    ];
    for (name, body) in unknown {
        let src = format!("{PROTOTYPES}{body}\n");
        for_each_target("string_fold_unknown", &src, &["-O2"], |asm, what| {
            assert!(mentions(asm, name), "{what}: no call to {name}:\n{asm}");
        });
    }
    let known = format!("{PROTOTYPES}size_t f(void) {{ return strlen(\"hello\"); }}\n");
    for_each_target("string_fold_o0", &known, &["-O0"], |asm, what| {
        assert!(mentions(asm, "strlen"), "{what}: no call to strlen:\n{asm}");
    });
    let zero =
        format!("{PROTOTYPES}int f(const char *p, const char *q) {{ return strncmp(p, q, 0); }}\n");
    for_each_target("string_fold_o0_zero", &zero, &["-O0"], |asm, what| {
        assert!(
            !mentions(asm, "strncmp"),
            "{what}: strncmp was called:\n{asm}"
        );
    });
}

/// Functions reading a local array whose bytes the stores before the call
/// decide, and ones where something else may have written it.
const LOCAL_ARRAYS: &str = r#"
void *memcpy(void *, const void *, size_t);
volatile int vk = 1;

__attribute__((noinline)) static void grow(char *p) { p[1] = 'y'; p[2] = 0; }

size_t by_bytes(void) {
    char str[8];
    char *ptr = str;
    ptr[0] = 'n'; ptr[1] = 't'; ptr[2] = 's'; ptr[3] = '\0';
    return strlen(ptr) + strlen(ptr + 3) + strlen(str + 1);
}
size_t by_word(void) { char s[8]; unsigned v = 0x00636261; memcpy(s, &v, 4); return strlen(s); }
size_t by_word_mid(void) {
    char s[8];
    unsigned v = 0x61626364;
    memcpy(s, &v, 4);
    s[3] = 0;
    return strlen(s + 1);
}
size_t high_byte(void) { char s[4]; s[0] = '\xff'; s[1] = 0; return strnlen(s, 4); }
size_t across_reader(void) {
    char s[4];
    s[0] = 'a'; s[1] = 0;
    size_t n = strlen(s + (vk - 1));
    return n + strlen(s);
}
size_t one_arm(void) { char s[4] = "ab"; if (vk) s[1] = 0; return strlen(s); }
size_t captured(void) { char s[4]; s[0] = 'x'; s[1] = 0; grow(s); return strlen(s); }
size_t vol(void) { volatile char s[4]; s[0] = 'x'; s[1] = 0; return strlen((char *)s); }
size_t unknown_byte(void) { char s[4]; s[0] = 'a'; s[1] = (char)vk; s[2] = 0; return strlen(s); }
size_t in_loop(void) {
    char s[4] = {'a', 'b', 0, 0};
    size_t n = 0;
    for (int i = 0; i < 3; i++) {
        n += strlen(s);
        s[2] = 'c';
    }
    return n;
}
"#;

/// The body of the function `name` in `asm`: from its label to the end of
/// its frame description.
fn function_body(asm: &str, name: &str) -> String {
    let label = format!("{}{name}:", asm_prefix(asm, name));
    asm.lines()
        .skip_while(|l| *l != label)
        .take_while(|l| !l.contains(".cfi_endproc"))
        .collect::<Vec<_>>()
        .join("\n")
}

/// A local array whose bytes are all known at the call is never passed to
/// `strlen`; one that something else may have written still is.
#[test]
fn builtins_string_fold_local_array_calls() {
    let src = format!("{PROTOTYPES}{LOCAL_ARRAYS}");
    for_each_target("string_fold_local_asm", &src, &["-O2"], |asm, what| {
        for f in ["by_bytes", "by_word", "by_word_mid", "high_byte"] {
            let body = function_body(asm, f);
            assert!(body.len() > f.len(), "{what}: no {f}:\n{asm}");
            assert!(
                !mentions(&body, "strlen") && !mentions(&body, "strnlen"),
                "{what}: {f} calls:\n{body}"
            );
        }
        let body = function_body(asm, "across_reader");
        assert_eq!(
            body.lines().filter(|l| mentions(l, "strlen")).count(),
            1,
            "{what}: only the unknown offset is called:\n{body}"
        );
        for f in ["one_arm", "captured", "vol", "unknown_byte", "in_loop"] {
            let body = function_body(asm, f);
            assert!(mentions(&body, "strlen"), "{what}: {f} folded:\n{body}");
        }
    });
}

/// `-fno-builtin-strlen` and `-fno-builtin` make the bare name an ordinary
/// function, whose calls stay; the reserved spelling still folds.
#[test]
fn builtins_string_fold_fno_builtin() {
    let bare = format!("{PROTOTYPES}size_t f(void) {{ return strlen(\"hello\"); }}\n");
    for flag in ["-fno-builtin-strlen", "-fno-builtin"] {
        for_each_target("string_fold_nb", &bare, &["-O2", flag], |asm, what| {
            assert!(mentions(asm, "strlen"), "{what} {flag}: no call:\n{asm}");
        });
    }
    let reserved = "unsigned long f(void) { return __builtin_strlen(\"hello\"); }\n";
    for_each_target(
        "string_fold_nb_res",
        reserved,
        &["-O2", "-fno-builtin"],
        |asm, what| {
            assert!(
                !mentions(asm, "strlen"),
                "{what}: strlen was called:\n{asm}"
            );
        },
    );
}

/// A call a fold makes is to the program's own name for the function: a
/// one-character `strstr` becomes `strchr`, asm label and all, and the
/// renamed `strstr` itself is folded by what it is, not what it is called.
#[test]
fn builtins_string_fold_follows_asm_labels() {
    let src = "char *strstr(const char *, const char *) __asm__(\"my_strstr\");\n\
               char *strchr(const char *, int) __asm__(\"my_strchr\");\n\
               char *f(const char *p) { return strstr(p, \"o\"); }\n\
               char *g(void) { return strstr(\"hello\", \"ll\"); }\n";
    for_each_target("string_fold_asm", src, &["-O2"], |asm, what| {
        assert!(
            !mentions(asm, "my_strstr"),
            "{what}: my_strstr called:\n{asm}"
        );
        assert!(
            mentions(asm, "my_strchr"),
            "{what}: no my_strchr call:\n{asm}"
        );
        assert!(
            !mentions(asm, "strchr"),
            "{what}: the unlabelled name:\n{asm}"
        );
    });
}

/// `sprintf`'s `%s` argument sits in a variadic position, where the
/// prototype says nothing about its type, so the rewrite to `strcpy` has to
/// check it itself.
///
/// `sprintf(d, "%s", 42)` is undefined either way -- `%s` requires a pointer
/// and the library dereferences whatever it is given -- but the compiler
/// must not be the one inventing the read: rewriting it to `strcpy(d, 42)`
/// makes up a memory access from a value that was never an address. clang
/// keeps the `sprintf`, and so must this. Every other pointer a fold here
/// reads through sits in a prototyped position, which `known_callee` has
/// already matched.
#[test]
fn builtins_string_fold_sprintf_needs_a_pointer_for_percent_s() {
    let decl = "int sprintf(char *, const char *, ...);\n";
    // Each case: the `%s` argument, and whether the copy is allowed.
    let cases: &[(&str, bool)] = &[
        ("const char *s", true),
        ("int x", false),
        ("long x", false),
        ("unsigned long x", false),
        ("double x", false),
    ];
    for (param, folds) in cases {
        let arg = param.rsplit([' ', '*']).next().unwrap();
        let src = format!("{decl}void f(char *d, {param}) {{ sprintf(d, \"%s\", {arg}); }}\n");
        for_each_target("sprintf_pct_s", &src, &["-O2"], |asm, what| {
            assert_eq!(
                mentions(asm, "strcpy"),
                *folds,
                "{what}: `sprintf(d, \"%s\", {arg})` with `{param}` -- \
                 strcpy is {} here; only a pointer may be copied from",
                if *folds { "required" } else { "forbidden" }
            );
            // When it does not fold, the program's own call has to remain.
            if !*folds {
                assert!(
                    mentions(asm, "sprintf"),
                    "{what}: `{param}` did not fold, so the sprintf call \
                     must still be there"
                );
            }
        });
    }
}
