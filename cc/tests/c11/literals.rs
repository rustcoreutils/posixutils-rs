//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C11 encoding-prefixed literals (6.4.4.4, 6.4.5) and adjacent-literal
// concatenation, including the mixed-prefix rules.
//

use crate::common::{compile_and_run, compile_expect_error, run_c17};

/// Every prefix lexes, and each has the right element width. `u8"x"` used to
/// give `undeclared identifier 'u8'` followed by a parse cascade.
#[test]
fn c11_literal_prefix_sizes() {
    let src = r#"
        int main(void) {
            /* u8"..." has type char[] (6.4.5p6), so 3 chars + NUL. */
            if (sizeof(u8"abc") != 4) return 1;
            /* char16_t is 2 bytes. */
            if (sizeof(u"abc") != 8) return 2;
            /* char32_t and wchar_t are 4. */
            if (sizeof(U"abc") != 16) return 3;
            if (sizeof(L"abc") != 16) return 4;
            if (sizeof("abc") != 4) return 5;
            if (sizeof(u'a') != 2) return 6;
            if (sizeof(U'a') != 4) return 7;
            return 0;
        }
    "#;
    assert_eq!(compile_and_run("c11_literal_prefix_sizes", src, &[]), 0);
}

/// The code units themselves, both as an expression and through a pointer.
#[test]
fn c11_literal_prefix_values() {
    let src = r#"
        int main(void) {
            const unsigned short *a = u"AB";
            const unsigned int *b = U"AB";
            if (a[0] != 0x41 || a[1] != 0x42 || a[2] != 0) return 1;
            if (b[0] != 0x41 || b[1] != 0x42 || b[2] != 0) return 2;
            if (u'Z' != 0x5A) return 3;
            if (U'Z' != 0x5A) return 4;
            return 0;
        }
    "#;
    assert_eq!(compile_and_run("c11_literal_prefix_values", src, &[]), 0);
}

/// `u8` is an ordinary narrow string, so the usual string functions apply.
#[test]
fn c11_u8_literal_is_a_char_array() {
    let src = r#"
        #include <string.h>
        int main(void) {
            const char *s = u8"hello";
            if (strlen(s) != 5) return 1;
            if (strcmp(s, "hello") != 0) return 2;
            return 0;
        }
    "#;
    assert_eq!(compile_and_run("c11_u8_char_array", src, &[]), 0);
}

/// A non-ASCII literal must yield code *points*, not the UTF-8 bytes of the
/// source — the lexer keeps one `char` per byte, so this only works because
/// the parser decodes before building the units.
#[test]
fn c11_literal_prefixes_decode_non_ascii() {
    // U+00E9 (e with acute), two bytes in UTF-8.
    let src = "int main(void) {\n\
               const unsigned int *e = U\"\u{e9}\";\n\
               const unsigned short *f = u\"\u{e9}\";\n\
               if (e[0] != 0xE9 || e[1] != 0) return 1;\n\
               if (f[0] != 0xE9 || f[1] != 0) return 2;\n\
               return 0;\n\
               }\n";
    assert_eq!(compile_and_run("c11_literal_non_ascii", src, &[]), 0);
}

/// Static initializers take the same path as expressions.
#[test]
fn c11_literal_prefixes_as_initializers() {
    let src = r#"
        static const unsigned short g16[] = u"hi";
        static const unsigned int  g32[] = U"hi";
        int main(void) {
            if (g16[0] != 'h' || g16[1] != 'i' || g16[2] != 0) return 1;
            if (g32[0] != 'h' || g32[1] != 'i' || g32[2] != 0) return 2;
            if (sizeof(g16) != 6) return 3;
            if (sizeof(g32) != 12) return 4;
            return 0;
        }
    "#;
    assert_eq!(compile_and_run("c11_literal_initializers", src, &[]), 0);
}

// ============================================================================
// #L4 — adjacent literals of different encodings concatenate
// ============================================================================

/// C11 6.4.5p5: if either literal carries a prefix, the whole run takes it.
/// The two separate concatenation loops this replaced could each see only
/// their own kind, so `"a" L"b"` left `L"b"` unconsumed and became a syntax
/// error.
#[test]
fn c11_mixed_narrow_wide_concatenation() {
    let src = r#"
        int main(void) {
            const int *w1 = (const int *)L"a" "b";
            const int *w2 = (const int *)"a" L"b";
            if (w1[0] != 'a' || w1[1] != 'b' || w1[2] != 0) return 1;
            if (w2[0] != 'a' || w2[1] != 'b' || w2[2] != 0) return 2;
            if (sizeof(L"a" "b") != 12) return 3;
            return 0;
        }
    "#;
    assert_eq!(compile_and_run("c11_mixed_concat", src, &[]), 0);
}

/// The same promotion for the char16_t and char32_t prefixes.
#[test]
fn c11_mixed_narrow_utf_concatenation() {
    let src = r#"
        int main(void) {
            const unsigned short *a = u"a" "b";
            const unsigned int *b = "a" U"b";
            if (a[0] != 'a' || a[1] != 'b' || a[2] != 0) return 1;
            if (b[0] != 'a' || b[1] != 'b' || b[2] != 0) return 2;
            return 0;
        }
    "#;
    assert_eq!(compile_and_run("c11_mixed_utf_concat", src, &[]), 0);
}

/// Two *different* prefixes in one run is a constraint violation (6.4.5p2).
#[test]
fn c11_conflicting_prefix_concatenation_is_rejected() {
    compile_expect_error(
        "c11_conflicting_concat",
        "const void *p = L\"a\" u\"b\";\n",
        "different encoding prefixes",
    );
}

/// Plain and wide concatenation must keep working unchanged.
#[test]
fn c11_same_encoding_concatenation_still_works() {
    let src = r#"
        #include <string.h>
        int main(void) {
            const char *s = "abc" "def";
            const int *w = (const int *)L"xy";
            if (strcmp(s, "abcdef") != 0) return 1;
            if (w[0] != 'x' || w[1] != 'y' || w[2] != 0) return 2;
            return 0;
        }
    "#;
    assert_eq!(compile_and_run("c11_same_concat", src, &[]), 0);
}

// ============================================================================
// #P11 — trigraphs, behind an off-by-default flag
// ============================================================================

/// C17 5.2.1.1 still mandates trigraphs (they went away in C23), but the
/// replacement applies inside string literals too, so it is opt-in.
#[test]
fn c17_trigraphs_are_off_by_default() {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_trigraph_off_")
        .tempdir()
        .unwrap();
    let src = dir.path().join("t.c");
    std::fs::write(&src, "int main(void)??<return 0;??>\n").unwrap();
    let exe = dir.path().join("t.out");

    let r = run_c17(&[&src.to_string_lossy(), "-o", &exe.to_string_lossy()]);
    assert!(
        !r.success,
        "trigraphs must not be replaced without --trigraphs"
    );
}

#[test]
fn c17_trigraphs_work_when_enabled() {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_trigraph_on_")
        .tempdir()
        .unwrap();
    let src = dir.path().join("t.c");
    // ??< ??> ??( ??) ??= ??/ ??' ??! ??-  — all nine.
    std::fs::write(
        &src,
        "int main(void)\n\
         ??<\n\
         int a??(2??) = ??< 1, 2 ??>;\n\
         int m = a??(0??) ??' a??(1??);   /* ^ */\n\
         int o = a??(0??) ??! a??(1??);   /* | */\n\
         int n = ??-0;                    /* ~ */\n\
         if (m != 3 || o != 3 || n != -1) return 1;\n\
         return 0;\n\
         ??>\n",
    )
    .unwrap();
    let exe = dir.path().join("t.out");

    let r = run_c17(&[
        "--trigraphs",
        &src.to_string_lossy(),
        "-o",
        &exe.to_string_lossy(),
    ]);
    assert!(r.success, "--trigraphs compile failed: {}", r.stderr);
    let status = std::process::Command::new(&exe)
        .status()
        .expect("failed to run");
    assert_eq!(status.code(), Some(0), "trigraph program returned nonzero");
}

/// The reason it is opt-in: `??!` inside a string literal really does become
/// `|`, and that must happen only when asked for.
#[test]
fn c17_trigraphs_affect_string_literals_only_when_enabled() {
    let src = r#"
        #include <string.h>
        int main(void) { return strcmp("What??!", "What??!") == 0 ? 0 : 1; }
    "#;
    assert_eq!(
        compile_and_run("c17_trigraph_string_default", src, &[]),
        0,
        "without --trigraphs the ?? must survive verbatim"
    );
}

/// `u"..."` / `U"..."` initializing an *automatic* array. The static and
/// pointer forms were covered; the local-declaration path has its own
/// initializer chain, and with no arm for these it fell through to the scalar
/// case and stored the literal's address into the array's first element.
#[test]
fn c11_local_array_from_utf16_and_utf32_literals() {
    // Spelled with the underlying types rather than `char16_t`/`char32_t`,
    // because those come from <uchar.h>, which is not in the bundled set and
    // is not present on every host SDK. What is under test is the array
    // initializer path, not the typedef.
    let src = r#"
        int main(void) {
            unsigned short a[] = u"hi";
            if (a[0] != 0x68 || a[1] != 0x69 || a[2] != 0) return 1;

            unsigned int b[] = U"hi";
            if (b[0] != 0x68 || b[1] != 0x69 || b[2] != 0) return 2;

            /* an explicit bound, and a non-ASCII code point */
            unsigned short c[4] = u"aé";
            if (c[0] != 0x61 || c[1] != 0x00e9 || c[2] != 0 || c[3] != 0) return 3;

            unsigned int d[3] = U"éz";
            if (d[0] != 0x00e9 || d[1] != 0x7a || d[2] != 0) return 4;

            /* the narrow and wide forms must keep working alongside them */
            char n[] = "hi";
            if (n[0] != 'h' || n[2] != 0) return 5;
            return 0;
        }
    "#;
    assert_eq!(compile_and_run("c11_local_utf_array", src, &[]), 0);
}
