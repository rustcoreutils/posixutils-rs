//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Assembly cases of tests/builtins/fortify.rs: which `_chk` calls become the
// plain function, and which keep their check. Every decision was checked
// against gcc 13 -O2 on x86-64 and aarch64; gcc then expands some of the
// plain `memcpy` and `memset` calls inline on x86-64, where c17 calls them.
//

use super::asm_probe::asm_for_with;

const TRIPLES: [&str; 2] = ["x86_64-unknown-linux-gnu", "aarch64-unknown-linux-gnu"];

/// What the cases share: the prototypes, and the objects they write into.
const PRELUDE: &str = "typedef unsigned long size_t;\n\
    typedef __builtin_va_list va_list;\n\
    #define os(p) __builtin_object_size(p, 0)\n\
    char big[20000], small[8];\n\
    extern const char *s;\n\
    extern size_t n;\n\
    extern void use(char *);\n";

/// The functions `name`'s body calls or jumps to, by name, in order.
fn calls_in(asm: &str, name: &str) -> Vec<String> {
    let start = format!("{name}:");
    let end = format!(".size {name},");
    asm.lines()
        .skip_while(|l| l.trim() != start)
        .take_while(|l| !l.trim().starts_with(&end))
        .filter_map(|l| {
            let mut words = l.split_whitespace();
            let op = words.next()?;
            if !["call", "jmp", "bl", "b"].contains(&op) {
                return None;
            }
            let callee = words.next()?.split('@').next()?;
            (!callee.starts_with('.')).then(|| callee.to_string())
        })
        .collect()
}

/// Each case: a function body, and the one function it calls at `-O2`. A
/// constant length that fits is past the 8 KB x86-64 does in place, so its
/// plain call stays one.
const CASES: &[(&str, &str)] = &[
    // A known object and a length known to fit: the plain call.
    ("void *f(void) { return __builtin___memcpy_chk(big, s, 10000, os(big)); }", "memcpy"),
    // ... and the largest of two lengths fits.
    (
        "void *f(int c) { return __builtin___memset_chk(big, 0, c ? 10000 : 12000, os(big)); }",
        "memset",
    ),
    // A known object and a length known only at run time: checked.
    ("void *f(void) { return __builtin___memcpy_chk(big, s, n, os(big)); }", "__memcpy_chk"),
    // A provable overflow: checked, and it fails at run time. A string of
    // known length is checked as a copy of its bytes.
    ("void *f(void) { return __builtin___memcpy_chk(small + 4, s, 8, os(small + 4)); }", "__memcpy_chk"),
    ("char *f(void) { return __builtin___strcpy_chk(small, \"overflowing\", os(small)); }", "__memcpy_chk"),
    ("char *f(void) { return __builtin___stpcpy_chk(small, \"overflowing\", os(small)); }", "__stpcpy_chk"),
    ("void f(void) { __builtin___stpcpy_chk(small, \"overflowing\", os(small)); }", "__memcpy_chk"),
    // An object nothing is known about, `(size_t)-1`: the plain call.
    ("void *f(char *d) { return __builtin___memcpy_chk(d, s, n, os(d)); }", "memcpy"),
    ("void *f(char *d) { return __builtin___memmove_chk(d, s, n, os(d)); }", "memmove"),
    ("void *f(char *d) { return __builtin___memset_chk(d, 1, n, os(d)); }", "memset"),
    ("char *f(char *d) { return __builtin___strcpy_chk(d, s, os(d)); }", "strcpy"),
    ("char *f(char *d) { return __builtin___stpcpy_chk(d, s, os(d)); }", "stpcpy"),
    ("char *f(char *d) { return __builtin___strncpy_chk(d, s, n, os(d)); }", "strncpy"),
    ("char *f(char *d) { return __builtin___stpncpy_chk(d, s, n, os(d)); }", "stpncpy"),
    ("char *f(char *d) { return __builtin___strcat_chk(d, s, os(d)); }", "strcat"),
    ("char *f(char *d) { return __builtin___strncat_chk(d, s, n, os(d)); }", "strncat"),
    ("int f(char *d, int i) { return __builtin___sprintf_chk(d, 0, os(d), \"%d\", i); }", "sprintf"),
    (
        "int f(char *d, int i) { return __builtin___snprintf_chk(d, n, 0, os(d), \"%d\", i); }",
        "snprintf",
    ),
    (
        "int f(char *d, va_list ap) { return __builtin___vsprintf_chk(d, 0, os(d), s, ap); }",
        "vsprintf",
    ),
    (
        "int f(char *d, va_list ap) { return __builtin___vsnprintf_chk(d, n, 0, os(d), s, ap); }",
        "vsnprintf",
    ),
    // The flag keeps the check of a format with a directive, even of an
    // unknown object -- and lets it go for a format with none.
    ("int f(char *d, int i) { return __builtin___sprintf_chk(d, 1, os(d), \"%d\", i); }", "__sprintf_chk"),
    ("int f(char *d) { return __builtin___sprintf_chk(d, 1, os(d), \"%s\", s); }", "sprintf"),
    // A result nobody reads: the form that answers the destination.
    ("void f(void) { __builtin___mempcpy_chk(big, s, n, os(big)); }", "__memcpy_chk"),
    ("void f(void) { __builtin___stpcpy_chk(big, s, os(big)); }", "__strcpy_chk"),
    ("void f(void) { __builtin___stpncpy_chk(big, s, n, os(big)); }", "__strncpy_chk"),
    // `strcat` onto a string the destination is known to hold.
    (
        "void f(void) { char b[8]; b[0] = 'h'; b[1] = 'i'; b[2] = 0; __builtin___strcat_chk(b, \"abc\", os(b)); use(b); }",
        "use",
    ),
    // A destination found only by propagation: a choice of two objects.
    (
        "void *f(int c) { char *p = c ? big : big + 9000; return __builtin___memcpy_chk(p, s, 10000, os(p)); }",
        "memcpy",
    ),
    (
        "void *f(int c) { char *p = c ? big + 19000 : small; return __builtin___memcpy_chk(p, s, 10000, os(p)); }",
        "__memcpy_chk",
    ),
];

/// Each case calls what gcc leaves at `-O2`, on both targets.
#[test]
fn builtins_fortify_decides_each_check() {
    for triple in TRIPLES {
        for (body, want) in CASES {
            let src = format!("{PRELUDE}{body}\n");
            let asm = asm_for_with("fortify", triple, &src, &["-O2"]);
            assert_eq!(calls_in(&asm, "f"), [*want], "{triple}: {body}");
        }
    }
}

/// An inline function's parameter is the caller's object once inlined: the
/// caller's size decides, as in glibc's `memcpy` wrapper.
#[test]
fn builtins_fortify_sees_through_an_inlined_wrapper() {
    let src = format!(
        "{PRELUDE}\
         static inline __attribute__((always_inline)) void *\n\
         my_memcpy(void *d, const void *s, size_t k)\n\
         {{ return __builtin___memcpy_chk(d, s, k, os(d)); }}\n\
         void *fits(void) {{ return my_memcpy(big, s, 10000); }}\n\
         void *overflows(void) {{ return my_memcpy(small, s, 10000); }}\n"
    );
    for triple in TRIPLES {
        let asm = asm_for_with("fortify_inline", triple, &src, &["-O2"]);
        assert_eq!(calls_in(&asm, "fits"), ["memcpy"], "{triple}");
        assert_eq!(calls_in(&asm, "overflows"), ["__memcpy_chk"], "{triple}");
    }
}

/// At `-O0` nothing is folded: an unknown object is `(size_t)-1`, and the
/// check is called with it, as gcc calls it.
#[test]
fn builtins_fortify_keeps_every_check_at_o0() {
    let src =
        format!("{PRELUDE}void *f(char *d) {{ return __builtin___memcpy_chk(d, s, n, os(d)); }}\n");
    for triple in TRIPLES {
        let asm = asm_for_with("fortify_o0", triple, &src, &["-O0"]);
        assert_eq!(calls_in(&asm, "f"), ["__memcpy_chk"], "{triple}");
    }
}

/// The macOS SDK's `<secure/_string.h>`, `<secure/_strings.h>` and
/// `<secure/_stdio.h>` turn the string and formatted-output functions into
/// these builtins whenever fortification is on, through `__darwin_obsz`,
/// whose type argument is an expression. Each reaches its checking function
/// in the C library -- `memccpy`, `strlcpy` and `strlcat` among them, which
/// c17 did not know, so it pinned `_FORTIFY_SOURCE` at 0 on Darwin.
#[test]
fn darwin_secure_header_builtins() {
    let src = "typedef unsigned long size_t;\n\
        typedef __builtin_va_list va_list;\n\
        #define _USE_FORTIFY_LEVEL 2\n\
        #define __darwin_obsz0(object) __builtin_object_size (object, 0)\n\
        #define __darwin_obsz(object) __builtin_object_size (object, _USE_FORTIFY_LEVEL > 1 ? 1 : 0)\n\
        extern char *d;\n\
        size_t f(const char *s, int c, size_t n, va_list ap) {\n\
            __builtin___memccpy_chk(d, s, c, n, __darwin_obsz0(d));\n\
            __builtin___stpncpy_chk(d, s, n, __darwin_obsz(d));\n\
            __builtin___vsprintf_chk(d, 0, __darwin_obsz(d), s, ap);\n\
            return __builtin___strlcpy_chk(d, s, n, __darwin_obsz(d))\n\
                + __builtin___strlcat_chk(d, s, n, __darwin_obsz(d));\n\
        }\n\
        int has = __has_builtin(__builtin___strlcpy_chk) + __has_builtin(__builtin___memccpy_chk)\n\
            + __has_builtin(__builtin___strlcat_chk) + __has_builtin(__builtin___stpncpy_chk)\n\
            + __has_builtin(__builtin___vsprintf_chk);\n";
    for triple in ["aarch64-apple-darwin", "x86_64-apple-darwin"] {
        let asm = asm_for_with("darwin_secure", triple, src, &["-O2"]);
        // (`__vsprintf_chk` of an object of unknown size folds to
        // `vsprintf`, as on Linux.)
        for callee in [
            "___memccpy_chk",
            "___strlcpy_chk",
            "___strlcat_chk",
            "_vsprintf",
        ] {
            assert!(asm.contains(callee), "{triple} {callee}:\n{asm}");
        }
        assert!(asm.contains(".long 5"), "{triple}: __has_builtin:\n{asm}");
    }
}
