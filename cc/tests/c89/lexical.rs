//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C89/C99 Lexical Elements Mega-Test
//
// Covers: comments, integer constants, floating constants, character constants,
//         string literals, identifiers, escape sequences
//

use crate::common::compile_and_run;

// ============================================================================
// Mega-test: Lexical elements (Section 9 of C99)
// ============================================================================

#[test]
fn c89_lexical_elements_mega() {
    let code = r#"
#include <string.h>
#include <float.h>

int main(void) {
    // ========== COMMENTS (returns 1-9) ==========
    // This is a line comment (C99)
    {
        int x = 1; /* block comment */ int y = 2;
        if (x != 1 || y != 2) return 1;

        /* multi-line
           block
           comment */
        int z = 3;
        if (z != 3) return 2;

        int a = 10 /* comment between tokens */ + 20;
        if (a != 30) return 3;

        // comment at end of line
        int b = 42;
        if (b != 42) return 4;

        /* comment with * inside */
        /* comment with / inside */
        /* comment with ** inside */
        int c = 7;
        if (c != 7) return 5;
    }

    // ========== INTEGER CONSTANTS (returns 10-29) ==========
    {
        // Decimal
        int d = 123;
        if (d != 123) return 10;

        // Octal (leading 0)
        int o = 0377;
        if (o != 255) return 11;

        int o2 = 010;
        if (o2 != 8) return 12;

        // Hexadecimal (0x and 0X)
        int h = 0xFF;
        if (h != 255) return 13;

        int h2 = 0XAB;
        if (h2 != 171) return 14;

        // Suffix: u / U
        unsigned int u1 = 100u;
        unsigned int u2 = 200U;
        if (u1 != 100) return 15;
        if (u2 != 200) return 16;

        // Suffix: l / L
        long l1 = 1000l;
        long l2 = 2000L;
        if (l1 != 1000) return 17;
        if (l2 != 2000) return 18;

        // Suffix: ll / LL (C99)
        long long ll1 = 1000000ll;
        long long ll2 = 2000000LL;
        if (ll1 != 1000000) return 19;
        if (ll2 != 2000000) return 20;

        // Combined suffixes
        unsigned long ul = 5000UL;
        unsigned long ul2 = 5000uL;
        unsigned long ul3 = 5000Lu;
        if (ul != 5000 || ul2 != 5000 || ul3 != 5000) return 21;

        unsigned long long ull = 9000000ULL;
        unsigned long long ull2 = 9000000ull;
        unsigned long long ull3 = 9000000llu;
        unsigned long long ull4 = 9000000LLu;
        if (ull != 9000000 || ull2 != 9000000) return 22;
        if (ull3 != 9000000 || ull4 != 9000000) return 23;

        // Hex with suffixes
        unsigned int hu = 0xFFu;
        long hl = 0xFFL;
        unsigned long long hull = 0xFFULL;
        if (hu != 255 || hl != 255 || hull != 255) return 24;

        // Octal with suffixes
        unsigned int ou = 0377u;
        long ol = 0377L;
        if (ou != 255 || ol != 255) return 25;

        // Zero
        int zero = 0;
        if (zero != 0) return 26;
    }

    // ========== FLOATING CONSTANTS (returns 30-39) ==========
    {
        // Decimal float
        double d1 = 3.14;
        if (d1 < 3.13 || d1 > 3.15) return 30;

        // Exponent notation (e/E)
        double d2 = 1.5e2;
        if (d2 < 149.9 || d2 > 150.1) return 31;

        double d3 = 2.0E3;
        if (d3 < 1999.9 || d3 > 2000.1) return 32;

        double d4 = 5e-1;
        if (d4 < 0.49 || d4 > 0.51) return 33;

        // Float suffix (f/F) — result is float
        float f1 = 3.14f;
        float f2 = 2.71F;
        if (f1 < 3.13f || f1 > 3.15f) return 34;
        if (f2 < 2.70f || f2 > 2.72f) return 35;

        // Long double suffix (l/L)
        long double ld = 3.14159265358979323846L;
        if (ld < 3.14L || ld > 3.15L) return 36;

        // No suffix = double (verify precision)
        double def = 1.23456789012345;
        if (def < 1.234567890123 || def > 1.234567890124) return 37;

        // Hex float (C99): 0x1.0p10 = 1.0 * 2^10 = 1024.0
        double hf1 = 0x1.0p10;
        if (hf1 < 1023.9 || hf1 > 1024.1) return 38;

        // Hex float with binary exponent (p/P)
        double hf2 = 0xA.0P2;  // 10.0 * 2^2 = 40.0
        if (hf2 < 39.9 || hf2 > 40.1) return 39;
    }

    // ========== CHARACTER CONSTANTS (returns 40-59) ==========
    {
        // Single character
        char c1 = 'A';
        if (c1 != 65) return 40;

        // Standard escape sequences
        char esc_a = '\a';   // bell (7)
        char esc_b = '\b';   // backspace (8)
        char esc_f = '\f';   // form feed (12)
        char esc_n = '\n';   // newline (10)
        char esc_r = '\r';   // carriage return (13)
        char esc_t = '\t';   // tab (9)
        char esc_v = '\v';   // vertical tab (11)
        if (esc_a != 7) return 41;
        if (esc_b != 8) return 42;
        if (esc_f != 12) return 43;
        if (esc_n != 10) return 44;
        if (esc_r != 13) return 45;
        if (esc_t != 9) return 46;
        if (esc_v != 11) return 47;

        // Punctuation escapes
        char esc_bs = '\\';  // backslash
        char esc_sq = '\'';  // single quote
        char esc_dq = '\"';  // double quote
        char esc_q  = '\?';  // question mark
        if (esc_bs != 92) return 48;
        if (esc_sq != 39) return 49;
        if (esc_dq != 34) return 50;
        if (esc_q != 63) return 51;

        // Octal escapes
        char oct1 = '\101';  // 'A' = 65
        char oct2 = '\0';    // null
        char oct3 = '\77';   // '?' = 63
        if (oct1 != 65) return 52;
        if (oct2 != 0) return 53;
        if (oct3 != 63) return 54;

        // Hex escapes
        char hex1 = '\x41';  // 'A' = 65
        char hex2 = '\x0A';  // newline = 10
        char hex3 = '\x00';  // null
        if (hex1 != 65) return 55;
        if (hex2 != 10) return 56;
        if (hex3 != 0) return 57;

        // Wide character (L'x')
        int wc = L'A';       // wchar_t is int-compatible
        if (wc != 65) return 58;
    }

    // ========== STRING LITERALS (returns 60-79) ==========
    {
        // Basic string literal
        const char *s1 = "hello";
        if (strlen(s1) != 5) return 60;
        if (s1[0] != 'h') return 61;

        // Null termination
        if (s1[5] != '\0') return 62;

        // Adjacent string literal concatenation
        const char *s2 = "hello" " " "world";
        if (strlen(s2) != 11) return 63;
        if (strcmp(s2, "hello world") != 0) return 64;

        // Escape sequences in strings
        const char *s3 = "a\tb\nc";
        if (s3[0] != 'a') return 65;
        if (s3[1] != '\t') return 66;
        if (s3[2] != 'b') return 67;
        if (s3[3] != '\n') return 68;
        if (s3[4] != 'c') return 69;

        // Hex/octal escapes in strings
        const char *s4 = "\x41\x42\x43";  // "ABC"
        if (strcmp(s4, "ABC") != 0) return 70;

        const char *s5 = "\101\102\103";   // "ABC" in octal
        if (strcmp(s5, "ABC") != 0) return 71;

        // Empty string
        const char *empty = "";
        if (strlen(empty) != 0) return 72;
        if (empty[0] != '\0') return 73;

        // String with all punctuation escapes
        const char *s6 = "\\\"\'?\a\b\f\n\r\t\v";
        if (s6[0] != '\\') return 74;
        if (s6[1] != '"') return 75;
        if (s6[2] != '\'') return 76;
        if (s6[3] != '?') return 77;
    }

    // ========== IDENTIFIERS (returns 80-89) ==========
    {
        // Start with letter
        int abc = 1;
        // Start with underscore
        int _def = 2;
        int __ghi = 3;
        if (abc != 1 || _def != 2 || __ghi != 3) return 80;

        // Case sensitivity
        int Foo = 10;
        int foo = 20;
        int FOO = 30;
        if (Foo != 10 || foo != 20 || FOO != 30) return 81;

        // Letters, digits, underscores
        int a1b2c3 = 42;
        int _under_score_ = 43;
        if (a1b2c3 != 42 || _under_score_ != 43) return 82;

        // Long identifier (63+ chars, C99 minimum significant)
        int abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789 = 99;
        if (abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789 != 99) return 83;
    }

    // ========== TOKENS / PUNCTUATORS (returns 90-99) ==========
    {
        // All punctuators used in expressions
        int arr[3] = {10, 20, 30};
        if (arr[0] != 10) return 90;       // []
        if (arr[2] != 30) return 91;

        struct { int x; int y; } pt = {5, 6};
        if (pt.x != 5) return 92;          // .

        struct { int x; int y; } *ppt = &pt;
        if (ppt->y != 6) return 93;        // ->

        int t = (1 > 0) ? 100 : 200;
        if (t != 100) return 94;           // ?:

        int a = 1, b = 2;                  // ,
        if (a != 1 || b != 2) return 95;

        // sizeof as token
        if (sizeof(int) != 4) return 96;
        if (sizeof(char) != 1) return 97;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("c89_lexical_elements_mega", code, &[]), 0);
}
