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

use crate::common::{compile_and_run, compile_and_run_aarch64, run_c17};

/// String and character literals: prefixes, concatenation, trigraphs left alone
/// without --trigraphs, and UTF-16/32 array initializers, as one program; each
/// section keeps its original test name and doc comment, and the exit-code table
/// is at the top. A failure in c17_trigraphs_affect_string_literals_only_when_enabled
/// (codes 81-81) means: without --trigraphs the ?? must survive verbatim.
///
/// Consolidates: c11_literal_prefix_sizes, c11_literal_prefix_values,
/// c11_u8_literal_is_a_char_array, c11_literal_prefixes_decode_non_ascii,
/// c11_literal_prefixes_as_initializers, c11_mixed_narrow_wide_concatenation,
/// c11_mixed_narrow_utf_concatenation, c11_same_encoding_concatenation_still_works,
/// c17_trigraphs_affect_string_literals_only_when_enabled and
/// c11_local_array_from_utf16_and_utf32_literals.
#[test]
fn c11_literals_mega() {
    let code = r#"
/*
 * Exit codes: each section's own failure codes, offset by its base.
 *     1-  7  c11_literal_prefix_sizes
 *    11- 14  c11_literal_prefix_values
 *    21- 22  c11_u8_literal_is_a_char_array
 *    31- 32  c11_literal_prefixes_decode_non_ascii
 *    41- 44  c11_literal_prefixes_as_initializers
 *    51- 53  c11_mixed_narrow_wide_concatenation
 *    61- 62  c11_mixed_narrow_utf_concatenation
 *    71- 72  c11_same_encoding_concatenation_still_works
 *    81- 81  c17_trigraphs_affect_string_literals_only_when_enabled
 *    91- 95  c11_local_array_from_utf16_and_utf32_literals
 */

/* ---- c11_literal_prefix_sizes (exit codes 1-7) ----
 *
 *  Every prefix lexes, and each has the right element width. `u8"x"` used to
 *  give `undeclared identifier 'u8'` followed by a parse cascade.
 */
        static int t_c11_literal_prefix_sizes(void) {
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
    


/* ---- c11_literal_prefix_values (exit codes 11-14) ----
 *
 *  The code units themselves, both as an expression and through a pointer.
 */
        static int t_c11_literal_prefix_values(void) {
            const unsigned short *a = u"AB";
            const unsigned int *b = U"AB";
            if (a[0] != 0x41 || a[1] != 0x42 || a[2] != 0) return 1;
            if (b[0] != 0x41 || b[1] != 0x42 || b[2] != 0) return 2;
            if (u'Z' != 0x5A) return 3;
            if (U'Z' != 0x5A) return 4;
            return 0;
        }
    


/* ---- c11_u8_literal_is_a_char_array (exit codes 21-22) ----
 *
 *  `u8` is an ordinary narrow string, so the usual string functions apply.
 */
        #include <string.h>
        static int t_c11_u8_literal_is_a_char_array(void) {
            const char *s = u8"hello";
            if (strlen(s) != 5) return 1;
            if (strcmp(s, "hello") != 0) return 2;
            return 0;
        }
    


/* ---- c11_literal_prefixes_decode_non_ascii (exit codes 31-32) ----
 */
/* A non-ASCII literal must yield code *points*, not the UTF-8 bytes of the
   source -- the lexer keeps one `char` per byte, so this only works because
   the parser decodes before building the units. U+00E9 (e with acute), two
   bytes in UTF-8. */
static int t_c11_literal_prefixes_decode_non_ascii(void) {
const unsigned int *e = U"é";
const unsigned short *f = u"é";
if (e[0] != 0xE9 || e[1] != 0) return 1;
if (f[0] != 0xE9 || f[1] != 0) return 2;
return 0;
}


/* ---- c11_literal_prefixes_as_initializers (exit codes 41-44) ----
 *
 *  Static initializers take the same path as expressions.
 */
        static const unsigned short g16[] = u"hi";
        static const unsigned int  g32[] = U"hi";
        static int t_c11_literal_prefixes_as_initializers(void) {
            if (g16[0] != 'h' || g16[1] != 'i' || g16[2] != 0) return 1;
            if (g32[0] != 'h' || g32[1] != 'i' || g32[2] != 0) return 2;
            if (sizeof(g16) != 6) return 3;
            if (sizeof(g32) != 12) return 4;
            return 0;
        }
    


/* ---- c11_mixed_narrow_wide_concatenation (exit codes 51-53) ----
 *
 *  C11 6.4.5p5: if either literal carries a prefix, the whole run takes it.
 *  The two separate concatenation loops this replaced could each see only
 *  their own kind, so `"a" L"b"` left `L"b"` unconsumed and became a syntax
 *  error.
 */
        static int t_c11_mixed_narrow_wide_concatenation(void) {
            const int *w1 = (const int *)L"a" "b";
            const int *w2 = (const int *)"a" L"b";
            if (w1[0] != 'a' || w1[1] != 'b' || w1[2] != 0) return 1;
            if (w2[0] != 'a' || w2[1] != 'b' || w2[2] != 0) return 2;
            if (sizeof(L"a" "b") != 12) return 3;
            return 0;
        }
    


/* ---- c11_mixed_narrow_utf_concatenation (exit codes 61-62) ----
 *
 *  The same promotion for the char16_t and char32_t prefixes.
 */
        static int t_c11_mixed_narrow_utf_concatenation(void) {
            const unsigned short *a = u"a" "b";
            const unsigned int *b = "a" U"b";
            if (a[0] != 'a' || a[1] != 'b' || a[2] != 0) return 1;
            if (b[0] != 'a' || b[1] != 'b' || b[2] != 0) return 2;
            return 0;
        }
    


/* ---- c11_same_encoding_concatenation_still_works (exit codes 71-72) ----
 *
 *  Plain and wide concatenation must keep working unchanged.
 */
        #include <string.h>
        static int t_c11_same_encoding_concatenation_still_works(void) {
            const char *s = "abc" "def";
            const int *w = (const int *)L"xy";
            if (strcmp(s, "abcdef") != 0) return 1;
            if (w[0] != 'x' || w[1] != 'y' || w[2] != 0) return 2;
            return 0;
        }
    


/* ---- c17_trigraphs_affect_string_literals_only_when_enabled (exit codes 81-81) ----
 *
 *  The reason it is opt-in: `??!` inside a string literal really does become
 *  `|`, and that must happen only when asked for.
 */
        #include <string.h>
        static int t_c17_trigraphs_affect_string_literals_only_when_enabled(void) { return strcmp("What??!", "What??!") == 0 ? 0 : 1; }
    


/* ---- c11_local_array_from_utf16_and_utf32_literals (exit codes 91-95) ----
 *
 *  `u"..."` / `U"..."` initializing an *automatic* array. The static and
 *  pointer forms were covered; the local-declaration path has its own
 *  initializer chain, and with no arm for these it fell through to the scalar
 *  case and stored the literal's address into the array's first element.
 *
 *  Spelled with the underlying types rather than `char16_t`/`char32_t`,
 *  because those come from <uchar.h>, which is not in the bundled set and
 *  is not present on every host SDK. What is under test is the array
 *  initializer path, not the typedef.
 */
        static int t_c11_local_array_from_utf16_and_utf32_literals(void) {
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
    

int main(void)
{
    int r;
    if ((r = t_c11_literal_prefix_sizes()) != 0)
        return 0 + r;
    if ((r = t_c11_literal_prefix_values()) != 0)
        return 10 + r;
    if ((r = t_c11_u8_literal_is_a_char_array()) != 0)
        return 20 + r;
    if ((r = t_c11_literal_prefixes_decode_non_ascii()) != 0)
        return 30 + r;
    if ((r = t_c11_literal_prefixes_as_initializers()) != 0)
        return 40 + r;
    if ((r = t_c11_mixed_narrow_wide_concatenation()) != 0)
        return 50 + r;
    if ((r = t_c11_mixed_narrow_utf_concatenation()) != 0)
        return 60 + r;
    if ((r = t_c11_same_encoding_concatenation_still_works()) != 0)
        return 70 + r;
    if ((r = t_c17_trigraphs_affect_string_literals_only_when_enabled()) != 0)
        return 80 + r;
    if ((r = t_c11_local_array_from_utf16_and_utf32_literals()) != 0)
        return 90 + r;
    return 0;
}
"#;
    assert_eq!(compile_and_run("c11_literals_mega", code, &[]), 0);
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

/// An octal or hex escape in a prefixed literal is bounded by the element
/// type, not by a byte (C17 6.4.4.4p9): `L'\x1234'` is 0x1234, and
/// `L"\xffffffff"` holds one all-ones `wchar_t`, whose value then follows the
/// target's signedness. Every escape was cut to eight bits, so `L'\x1234'` was
/// 0x34 and `L'\x80000000'` was 0; and a wide literal was carried as text, so
/// a unit that is not a character -- a lone surrogate, or anything above
/// U+10FFFF -- became U+FFFD. Expected values are gcc's on both Linux
/// targets.
const PREFIXED_ESCAPES: &str = r#"
typedef __WCHAR_TYPE__ wchar_t;
typedef __CHAR16_TYPE__ char16_t;
typedef __CHAR32_TYPE__ char32_t;

static const wchar_t ws[] = L"a\x1234\xffffffff\777\U0001F600";
static const char16_t us[] = u"a\x1234\xd800\777\U0001F600";
static const char32_t Us[] = U"a\x1234\xffffffff\777\U0001F600";

int main(void) {
    int wchar_signed = (wchar_t)-1 < 0;
    long long ones = wchar_signed ? -1 : 4294967295LL;

    if (L'\x1234' != 0x1234 || L'\777' != 0777) return 1;
    if (L'\xffffffff' != ones) return 2;
    if (u'\x1234' != 0x1234 || u'\xd800' != 0xd800) return 3;
    if (U'\xffffffff' != 0xffffffffu || U'\777' != 0777) return 4;

    if (sizeof ws / sizeof ws[0] != 6) return 5;
    if (ws[1] != 0x1234 || ws[2] != ones || ws[3] != 0777 || ws[4] != 0x1f600) return 6;
    /* The escaped unit is kept as a unit; the character is encoded. */
    if (sizeof us / sizeof us[0] != 7) return 7;
    if (us[1] != 0x1234 || us[2] != 0xd800 || us[3] != 0777) return 8;
    if (us[4] != 0xd83d || us[5] != 0xde00) return 9;
    if (Us[2] != 0xffffffffu || Us[4] != 0x1f600) return 10;

    /* Through a pointer, and in an automatic array. */
    const wchar_t *p = L"\x80000000";
    if (p[0] != (wchar_signed ? -2147483647 - 1 : 2147483648LL)) return 11;
    wchar_t a[] = L"\x1234\xfffffffe";
    if (a[0] != 0x1234 || a[1] != (wchar_signed ? -2 : 4294967294LL)) return 12;

#if L'\x1234' != 0x1234 || u'\x1234' != 0x1234 || U'\x10000' != 0x10000
    return 13;
#endif
#if L'\xffffffff' < 0
    if (!wchar_signed) return 14;
#else
    if (wchar_signed) return 15;
#endif
    return 0;
}
"#;

#[test]
fn c11_prefixed_escapes_keep_their_width() {
    assert_eq!(
        compile_and_run("prefixed_escapes", PREFIXED_ESCAPES, &[]),
        0
    );
}

#[test]
fn c11_prefixed_escapes_keep_their_width_aarch64() {
    if let Some(rc) = compile_and_run_aarch64("prefixed_escapes_a64", PREFIXED_ESCAPES, "-O0") {
        assert_eq!(rc, 0);
    }
}
