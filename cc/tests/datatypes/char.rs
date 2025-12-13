//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for `char`, `signed char`, and `unsigned char` data types
//
// C99 Note: These three types are distinct. Plain `char` has implementation-
// defined signedness:
//   - x86-64: char is signed (range -128 to 127)
//   - ARM/aarch64: char is unsigned (range 0 to 255)
//
// Character literals (e.g., 'A') have type `int` in C, not `char`.
//

use crate::common::compile_and_run;

// ############################################################################
// CHAR TESTS (platform-dependent signedness, signed on x86-64)
// ############################################################################

// ============================================================================
// Char: Basic Operations and Character Literals
// ============================================================================

#[test]
fn char_basic_and_literals() {
    let code = r#"
int main(void) {
    char a, b;

    // Basic assignment
    a = 'A';
    if (a != 65) return 1;

    // Character literal comparison
    a = 'Z';
    if (a != 'Z') return 2;

    // Lowercase letter
    a = 'a';
    if (a != 97) return 3;

    // Digit character
    a = '0';
    if (a != 48) return 4;

    // Space character
    a = ' ';
    if (a != 32) return 5;

    // Arithmetic with chars
    a = 'A';
    b = a + 1;
    if (b != 'B') return 6;

    // Character difference
    a = 'Z';
    b = 'A';
    if (a - b != 25) return 7;

    return 0;
}
"#;
    assert_eq!(compile_and_run("char_basic", code), 0);
}

// ============================================================================
// Char: Escape Sequences
// ============================================================================

#[test]
fn char_escape_sequences() {
    let code = r#"
int main(void) {
    char c;

    // Null character
    c = '\0';
    if (c != 0) return 1;

    // Newline
    c = '\n';
    if (c != 10) return 2;

    // Tab
    c = '\t';
    if (c != 9) return 3;

    // Carriage return
    c = '\r';
    if (c != 13) return 4;

    // Backslash
    c = '\\';
    if (c != 92) return 5;

    // Single quote
    c = '\'';
    if (c != 39) return 6;

    // Double quote
    c = '"';
    if (c != 34) return 7;

    // Bell (alert)
    c = '\a';
    if (c != 7) return 8;

    // Backspace
    c = '\b';
    if (c != 8) return 9;

    // Form feed
    c = '\f';
    if (c != 12) return 10;

    // Vertical tab
    c = '\v';
    if (c != 11) return 11;

    return 0;
}
"#;
    assert_eq!(compile_and_run("char_escape", code), 0);
}

// ============================================================================
// Char: Hex and Octal Escapes
// ============================================================================

#[test]
fn char_hex_octal_escapes() {
    let code = r#"
int main(void) {
    char c;

    // Hex escape for 'A' (0x41 = 65)
    c = '\x41';
    if (c != 'A') return 1;

    // Hex escape for newline (0x0A = 10)
    c = '\x0a';
    if (c != '\n') return 2;

    // Octal escape for 'A' (0101 = 65)
    c = '\101';
    if (c != 'A') return 3;

    // Octal escape for null (0 = 0)
    c = '\0';
    if (c != 0) return 4;

    // Hex escape for space (0x20 = 32)
    c = '\x20';
    if (c != ' ') return 5;

    return 0;
}
"#;
    assert_eq!(compile_and_run("char_hex_oct", code), 0);
}

// ============================================================================
// Char: Arithmetic Operators
// ============================================================================

#[test]
fn char_arithmetic_operators() {
    let code = r#"
int main(void) {
    char a, b, result;

    // Addition
    a = 30; b = 12;
    result = a + b;
    if (result != 42) return 1;

    // Subtraction
    a = 100; b = 58;
    result = a - b;
    if (result != 42) return 2;

    // Multiplication
    a = 6; b = 7;
    result = a * b;
    if (result != 42) return 3;

    // Division
    a = 84; b = 2;
    result = a / b;
    if (result != 42) return 4;

    // Modulo
    a = 47; b = 10;
    result = a % b;
    if (result != 7) return 5;

    // Unary negation (char is signed on x86-64)
    a = 42;
    result = -a;
    if (result != -42) return 6;

    // Unary plus
    a = 42;
    result = +a;
    if (result != 42) return 7;

    return 0;
}
"#;
    assert_eq!(compile_and_run("char_arith", code), 0);
}

// ============================================================================
// Char: Comparison Operators
// ============================================================================

#[test]
fn char_comparison_operators() {
    let code = r#"
int main(void) {
    char a, b;

    // Equal - true
    a = 'X'; b = 'X';
    if ((a == b) != 1) return 1;

    // Equal - false
    a = 'X'; b = 'Y';
    if ((a == b) != 0) return 2;

    // Not equal - true
    a = 'A'; b = 'B';
    if ((a != b) != 1) return 3;

    // Not equal - false
    a = 'Z'; b = 'Z';
    if ((a != b) != 0) return 4;

    // Less than - true
    a = 'A'; b = 'Z';
    if ((a < b) != 1) return 5;

    // Less than - false
    a = 'Z'; b = 'A';
    if ((a < b) != 0) return 6;

    // Less or equal - true (less)
    a = 'A'; b = 'B';
    if ((a <= b) != 1) return 7;

    // Less or equal - true (equal)
    a = 'M'; b = 'M';
    if ((a <= b) != 1) return 8;

    // Greater than - true
    a = 'Z'; b = 'A';
    if ((a > b) != 1) return 9;

    // Greater than - false
    a = 'A'; b = 'Z';
    if ((a > b) != 0) return 10;

    // Greater or equal - true
    a = 'Z'; b = 'A';
    if ((a >= b) != 1) return 11;

    // Greater or equal - true (equal)
    a = 'K'; b = 'K';
    if ((a >= b) != 1) return 12;

    return 0;
}
"#;
    assert_eq!(compile_and_run("char_cmp", code), 0);
}

// ============================================================================
// Char: Bitwise Operators
// ============================================================================

#[test]
fn char_bitwise_operators() {
    let code = r#"
int main(void) {
    char a, b;

    // Bitwise AND
    a = 0x3F; b = 0x0F;
    if ((a & b) != 0x0F) return 1;

    // Bitwise OR
    a = 0x30; b = 0x0F;
    if ((a | b) != 0x3F) return 2;

    // Bitwise XOR
    a = 0x3F; b = 0x30;
    if ((a ^ b) != 0x0F) return 3;

    // Left shift
    a = 1;
    if ((a << 4) != 16) return 4;

    // Right shift
    a = 64;
    if ((a >> 2) != 16) return 5;

    // Case conversion: uppercase to lowercase (set bit 5)
    a = 'A';
    if ((a | 0x20) != 'a') return 6;

    // Case conversion: lowercase to uppercase (clear bit 5)
    a = 'a';
    if ((a & ~0x20) != 'A') return 7;

    return 0;
}
"#;
    assert_eq!(compile_and_run("char_bitwise", code), 0);
}

// ============================================================================
// Char: Assignment Operators
// ============================================================================

#[test]
fn char_assignment_operators() {
    let code = r#"
int main(void) {
    char a;

    // Simple assignment
    a = 42;
    if (a != 42) return 1;

    // Add assign
    a = 40;
    a += 2;
    if (a != 42) return 2;

    // Sub assign
    a = 50;
    a -= 8;
    if (a != 42) return 3;

    // Mul assign
    a = 21;
    a *= 2;
    if (a != 42) return 4;

    // Div assign
    a = 84;
    a /= 2;
    if (a != 42) return 5;

    // Mod assign
    a = 50;
    a %= 8;
    if (a != 2) return 6;

    // And assign
    a = 0x7F;
    a &= 0x0F;
    if (a != 15) return 7;

    // Or assign
    a = 0x30;
    a |= 0x0F;
    if (a != 0x3F) return 8;

    // Xor assign
    a = 0x3F;
    a ^= 0x30;
    if (a != 0x0F) return 9;

    return 0;
}
"#;
    assert_eq!(compile_and_run("char_assign", code), 0);
}

// ============================================================================
// Char: Increment/Decrement Operators
// ============================================================================

#[test]
fn char_increment_decrement_operators() {
    let code = r#"
int main(void) {
    char a, b;

    // Pre-increment
    a = 'A';
    b = ++a;
    if (b != 'B') return 1;

    // Post-increment (returns original)
    a = 'M';
    b = a++;
    if (b != 'M') return 2;
    if (a != 'N') return 3;

    // Pre-decrement
    a = 'Z';
    b = --a;
    if (b != 'Y') return 4;

    // Post-decrement (returns original)
    a = 'D';
    b = a--;
    if (b != 'D') return 5;
    if (a != 'C') return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("char_incdec", code), 0);
}

// ============================================================================
// Char: Signed Behavior (x86-64 specific - char is signed)
// ============================================================================

#[test]
fn char_signed_behavior() {
    let code = r#"
int main(void) {
    char a;

    // Negative value (valid for signed char on x86-64)
    a = -1;
    if (a != -1) return 1;

    // Min value for signed char
    a = -128;
    if (a != -128) return 2;

    // Max value for signed char
    a = 127;
    if (a != 127) return 3;

    // Negative comparison
    a = -10;
    if (a >= 0) return 4;  // Should be false (negative)

    // Signed comparison
    a = -1;
    char b = 1;
    if (a > b) return 5;  // -1 > 1 should be false for signed

    return 0;
}
"#;
    assert_eq!(compile_and_run("char_signed", code), 0);
}

// ############################################################################
// SIGNED CHAR TESTS (explicitly signed, always -128 to 127)
// ############################################################################

// ============================================================================
// Signed Char: Arithmetic Operators
// ============================================================================

#[test]
fn schar_arithmetic_operators() {
    let code = r#"
int main(void) {
    signed char a, b, result;

    // Addition
    a = 30; b = 12;
    result = a + b;
    if (result != 42) return 1;

    // Subtraction
    a = 100; b = 58;
    result = a - b;
    if (result != 42) return 2;

    // Multiplication
    a = 6; b = 7;
    result = a * b;
    if (result != 42) return 3;

    // Division
    a = 84; b = 2;
    result = a / b;
    if (result != 42) return 4;

    // Modulo
    a = 47; b = 10;
    result = a % b;
    if (result != 7) return 5;

    // Negative values
    a = -42;
    if (-a != 42) return 6;

    // Negative arithmetic
    a = -20; b = -22;
    if (a + b != -42) return 7;

    return 0;
}
"#;
    assert_eq!(compile_and_run("schar_arith", code), 0);
}

// ============================================================================
// Signed Char: Comparison Operators
// ============================================================================

#[test]
fn schar_comparison_operators() {
    let code = r#"
int main(void) {
    signed char a, b;

    // Negative less than positive
    a = -10; b = 10;
    if ((a < b) != 1) return 1;

    // Negative comparison
    a = -20; b = -10;
    if ((a < b) != 1) return 2;  // -20 < -10

    // Equal negative
    a = -42; b = -42;
    if ((a == b) != 1) return 3;

    // Range boundaries
    a = -128; b = 127;
    if ((a < b) != 1) return 4;

    // Min value comparison
    a = -128;
    if (a >= 0) return 5;  // Should be false

    return 0;
}
"#;
    assert_eq!(compile_and_run("schar_cmp", code), 0);
}

// ============================================================================
// Signed Char: Assignment Operators
// ============================================================================

#[test]
fn schar_assignment_operators() {
    let code = r#"
int main(void) {
    signed char a;

    // Simple assignment
    a = -42;
    if (a != -42) return 1;

    // Add assign
    a = -50;
    a += 8;
    if (a != -42) return 2;

    // Sub assign
    a = -40;
    a -= 2;
    if (a != -42) return 3;

    // Mul assign with sign
    a = -21;
    a *= 2;
    if (a != -42) return 4;

    // Div assign with sign
    a = -84;
    a /= 2;
    if (a != -42) return 5;

    return 0;
}
"#;
    assert_eq!(compile_and_run("schar_assign", code), 0);
}

// ############################################################################
// UNSIGNED CHAR TESTS (always 0 to 255)
// ############################################################################

// ============================================================================
// Unsigned Char: Arithmetic Operators
// ============================================================================

#[test]
fn uchar_arithmetic_operators() {
    let code = r#"
int main(void) {
    unsigned char a, b, result;

    // Addition
    a = 30; b = 12;
    result = a + b;
    if (result != 42) return 1;

    // Subtraction
    a = 100; b = 58;
    result = a - b;
    if (result != 42) return 2;

    // Multiplication
    a = 6; b = 7;
    result = a * b;
    if (result != 42) return 3;

    // Division
    a = 84; b = 2;
    result = a / b;
    if (result != 42) return 4;

    // Modulo
    a = 47; b = 10;
    result = a % b;
    if (result != 7) return 5;

    // Max value
    a = 255;
    if (a != 255) return 6;

    // Large values
    a = 200; b = 50;
    result = a - b;
    if (result != 150) return 7;

    return 0;
}
"#;
    assert_eq!(compile_and_run("uchar_arith", code), 0);
}

// ============================================================================
// Unsigned Char: Comparison Operators
// ============================================================================

#[test]
fn uchar_comparison_operators() {
    let code = r#"
int main(void) {
    unsigned char a, b;

    // Basic comparison
    a = 200; b = 100;
    if ((a > b) != 1) return 1;

    // Equal
    a = 255; b = 255;
    if ((a == b) != 1) return 2;

    // Max value comparison
    a = 255; b = 0;
    if ((a > b) != 1) return 3;

    // Zero is minimum
    a = 0; b = 1;
    if ((a < b) != 1) return 4;

    // Unsigned comparison (no negative)
    a = 255;
    if (a < 0) return 5;  // Should always be false for unsigned

    return 0;
}
"#;
    assert_eq!(compile_and_run("uchar_cmp", code), 0);
}

// ============================================================================
// Unsigned Char: Bitwise Operators
// ============================================================================

#[test]
fn uchar_bitwise_operators() {
    let code = r#"
int main(void) {
    unsigned char a, b;

    // Bitwise AND
    a = 0xFF; b = 0x0F;
    if ((a & b) != 0x0F) return 1;

    // Bitwise OR
    a = 0xF0; b = 0x0F;
    if ((a | b) != 0xFF) return 2;

    // Bitwise XOR
    a = 0xFF; b = 0xF0;
    if ((a ^ b) != 0x0F) return 3;

    // Bitwise NOT (result masked to 8 bits when stored)
    a = 0;
    b = ~a;
    if (b != 0xFF) return 4;

    // Left shift
    a = 1;
    if ((a << 4) != 16) return 5;

    // Right shift (logical for unsigned)
    a = 128;
    if ((a >> 1) != 64) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("uchar_bitwise", code), 0);
}

// ============================================================================
// Unsigned Char: Assignment Operators
// ============================================================================

#[test]
fn uchar_assignment_operators() {
    let code = r#"
int main(void) {
    unsigned char a;

    // Simple assignment
    a = 200;
    if (a != 200) return 1;

    // Add assign
    a = 40;
    a += 2;
    if (a != 42) return 2;

    // Sub assign
    a = 50;
    a -= 8;
    if (a != 42) return 3;

    // Mul assign
    a = 21;
    a *= 2;
    if (a != 42) return 4;

    // Div assign
    a = 84;
    a /= 2;
    if (a != 42) return 5;

    // And assign
    a = 0xFF;
    a &= 0x0F;
    if (a != 15) return 6;

    // Or assign
    a = 0xF0;
    a |= 0x0F;
    if (a != 255) return 7;

    return 0;
}
"#;
    assert_eq!(compile_and_run("uchar_assign", code), 0);
}

// ============================================================================
// Unsigned Char: Increment/Decrement Operators
// ============================================================================

#[test]
fn uchar_increment_decrement_operators() {
    let code = r#"
int main(void) {
    unsigned char a, b;

    // Pre-increment
    a = 41;
    if (++a != 42) return 1;

    // Post-increment
    a = 42;
    b = a++;
    if (b != 42) return 2;
    if (a != 43) return 3;

    // Pre-decrement
    a = 43;
    if (--a != 42) return 4;

    // Post-decrement
    a = 42;
    b = a--;
    if (b != 42) return 5;
    if (a != 41) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("uchar_incdec", code), 0);
}

// ============================================================================
// Mixed Char Types and Conversions
// ============================================================================

#[test]
fn char_type_conversions() {
    let code = r#"
int main(void) {
    char c;
    signed char sc;
    unsigned char uc;
    int i;

    // Char to int
    c = 'A';
    i = c;
    if (i != 65) return 1;

    // Int to char
    i = 66;
    c = i;
    if (c != 'B') return 2;

    // Signed char to int (sign extension)
    sc = -1;
    i = sc;
    if (i != -1) return 3;

    // Unsigned char to int (zero extension)
    uc = 255;
    i = uc;
    if (i != 255) return 4;

    // Cross-type assignment
    uc = 200;
    sc = uc;  // Value > 127 becomes negative when viewed as signed
    // Just check it compiles and doesn't crash

    return 0;
}
"#;
    assert_eq!(compile_and_run("char_conv", code), 0);
}

// ============================================================================
// Character Classification (using comparison)
// ============================================================================

#[test]
fn char_classification() {
    let code = r#"
int main(void) {
    char c;

    // Is uppercase?
    c = 'G';
    if (!(c >= 'A' && c <= 'Z')) return 1;

    // Is lowercase?
    c = 'g';
    if (!(c >= 'a' && c <= 'z')) return 2;

    // Is digit?
    c = '5';
    if (!(c >= '0' && c <= '9')) return 3;

    // Is alphabetic?
    c = 'M';
    if (!((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'))) return 4;

    // Is alphanumeric?
    c = '7';
    if (!((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9'))) return 5;

    return 0;
}
"#;
    assert_eq!(compile_and_run("char_class", code), 0);
}

// ============================================================================
// Char Types: Structs, Functions, and Initializers
// ============================================================================

#[test]
fn char_structs_functions_initializers() {
    let code = r#"
// Struct with char members
struct char_container {
    char c;
    signed char sc;
    unsigned char uc;
};

// Function taking char parameter and returning char
char next_char(char c) {
    return c + 1;
}

// Function taking signed char parameter
signed char negate_schar(signed char sc) {
    return -sc;
}

// Function taking unsigned char parameter
unsigned char double_uchar(unsigned char uc) {
    return uc * 2;
}

// Function taking struct parameter
int sum_chars(struct char_container c) {
    return c.c + c.sc + c.uc;
}

// Function returning struct
struct char_container make_container(char a, signed char b, unsigned char c) {
    struct char_container container;
    container.c = a;
    container.sc = b;
    container.uc = c;
    return container;
}

int main(void) {
    // Variable initializers
    char c = 'A';
    if (c != 'A') return 1;

    signed char sc = -10;
    if (sc != -10) return 2;

    unsigned char uc = 200;
    if (uc != 200) return 3;

    // Multiple initializers
    char x = 'X', y = 'Y', z = 'Z';
    if (x != 'X') return 4;
    if (y != 'Y') return 5;
    if (z != 'Z') return 6;

    // Struct member access
    struct char_container cc;
    cc.c = 'M';
    cc.sc = -50;
    cc.uc = 255;
    if (cc.c != 'M') return 7;
    if (cc.sc != -50) return 8;
    if (cc.uc != 255) return 9;

    // Function with char param/return
    if (next_char('A') != 'B') return 10;
    if (next_char('Z') != '[') return 11;

    // Function with signed char param/return
    if (negate_schar(10) != -10) return 12;
    if (negate_schar(-42) != 42) return 13;

    // Function with unsigned char param/return
    if (double_uchar(50) != 100) return 14;
    if (double_uchar(100) != 200) return 15;

    // Struct as function parameter
    cc.c = 10;
    cc.sc = 20;
    cc.uc = 30;
    if (sum_chars(cc) != 60) return 16;

    // Struct as function return
    struct char_container cc2;
    cc2 = make_container('Q', -5, 100);
    if (cc2.c != 'Q') return 17;
    if (cc2.sc != -5) return 18;
    if (cc2.uc != 100) return 19;

    // Pointer to char in struct
    struct char_container *p;
    p = &cc;
    if (p->c != 10) return 20;
    p->c = 'P';
    if (cc.c != 'P') return 21;

    return 0;
}
"#;
    assert_eq!(compile_and_run("char_advanced", code), 0);
}
