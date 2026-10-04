//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C99 initializers: brace elision and string-literal initializers
//

use crate::common::compile_and_run;

// ============================================================================
// Mega-test: brace elision and string initializers
// ============================================================================

// Original test documentation, in section order:
//
// ---- c99_initializers_brace_elision ----
// ============================================================================
// BUG 4: Brace elision (C99 6.7.8p17-20)
// ============================================================================

// ---- c99_initializers_brace_elision_global ----
// ---- c99_initializers_string_no_brace_elision ----
// ============================================================================
// String literals must NOT trigger brace elision (C99 6.7.8p14)
// ============================================================================

// ---- c99_initializers_global_local_parity ----
// ============================================================================
// Parity test: global (static) vs local initializers must produce same results
// ============================================================================

// ---- c99_string_literal_initializer_may_be_braced ----
// C17 6.7.9p14: the string literal initializing a character array may be
// enclosed in braces, and it still initializes that array -- `char b[] =
// {"hi"}` is `char[3]` holding `hi`, not an array of one element.
//
// It was read as an ordinary initializer list, so the element count came out
// as 1 and the characters were never copied in: `sizeof b` was 1 and printing
// it gave garbage. `(char[]){"hi"}` was empty for the same reason, and an
// explicitly-sized `char c[6] = {"hi"}` had the right size with the wrong
// contents. Both the size deduction and the store path had the look-through
// one level down, for `char names[3][4] = {"Sun", "Mon"}`, and neither had it
// at the outermost level.
//
// Every expectation here came from gcc on this source.
// ---- c99_string_initializer_exactly_fills_its_array ----
// C17 6.7.9p14: an array of character type may be exactly as long as the
// string literal initializing it, in which case the terminating null is
// dropped rather than written. `char b[2] = "hi"` holds two characters.
//
// The store loop wrote the null unconditionally, one byte past the object.
// Pre-existing for the unbraced spelling; the braced one reached the same
// loop once the two shared a routine.
// ---- c99_universal_character_names_use_the_execution_encoding ----
// A universal character name denotes a *code point*, which the execution
// character set then encodes — UTF-8 here. It was returned as if it were a
// byte, so `"café"` was the five bytes `caf\xe9` (Latin-1) instead of the
// six `caf\xc3\xa9`, and `sizeof` said 5 where gcc says 6.
//
// Fixing that in isolation would have broken the wide encodings, which had
// the opposite halves right: a UCN worked there and a character typed
// directly in the source did not. The parser sees the literal before escapes
// are resolved, so it *can* tell a source byte from one an escape named —
// `L"café"` is four wide characters and `L"caf\xc3\xa9"` is five, and only
// keeping the two apart gets both right.
//
// Every expectation here came from gcc on this source.
// ---- c99_struct_member_string_initializer_keeps_its_bytes ----
// A `char` array *member* initialized from a string literal was written with
// Rust's UTF-8 encoding of the parsed literal while its null terminator was
// placed by counting characters. For any byte at or above 0x80 the two
// disagree: the data runs one byte long and the terminator lands inside it.
//
// `struct { char t[4]; int g; }` initialized with `"\xc2\x80"` wrote
// `c3 82 00 80` where gcc writes `c2 80 00 00`. Automatic storage only — the
// static path goes through a different routine and was already right, which
// is why the existing high-byte test did not catch it.
// ---- c99_brace_elision_decides_the_deduced_array_bound ----
// An incomplete array's bound comes from the initializer, and with brace
// elision one array element consumes as many list elements as it has scalar
// fields (C17 6.7.9p20).
//
// The parser's bound deduction counted one array element per list element,
// so `int a[][2] = {1,2,3,4}` got four rows instead of two. The values
// themselves landed correctly -- the linearizer knows the rule -- so only
// `sizeof` was wrong, and every idiomatic `for (i = 0; i < sizeof a /
// sizeof a[0]; i++)` walked twice as far as the object.
//
/// Brace elision (C99 6.7.8p17-20) and string-literal initializers
/// (C17 6.7.9p14), at file and block scope.
///
/// Consolidates (one C section each, a `t_<name>` function):
/// - `c99_initializers_brace_elision`
/// - `c99_initializers_brace_elision_global`
/// - `c99_initializers_string_no_brace_elision`
/// - `c99_initializers_global_local_parity`
/// - `c99_string_literal_initializer_may_be_braced`
/// - `c99_string_initializer_exactly_fills_its_array`
/// - `c99_universal_character_names_use_the_execution_encoding`
/// - `c99_struct_member_string_initializer_keeps_its_bytes`
/// - `c99_brace_elision_decides_the_deduced_array_bound`
///
/// Exit codes: see the map at the top of the program.
#[test]
fn c99_initializers_strings_mega() {
    let code = r#"
/* Exit-code map: each section returns its original code, offset by
   the base listed in its banner.
       1.. 53  c99_initializers_brace_elision
      54.. 75  c99_initializers_brace_elision_global
      76.. 98  c99_initializers_string_no_brace_elision
      99..169  c99_initializers_global_local_parity
     170..181  c99_string_literal_initializer_may_be_braced
     182..190  c99_string_initializer_exactly_fills_its_array
     191..211  c99_universal_character_names_use_the_execution_encoding
     212..219  c99_struct_member_string_initializer_keeps_its_bytes
     220..240  c99_brace_elision_decides_the_deduced_array_bound
*/

/* ==== c99_initializers_brace_elision: exit codes 1..53 (original code + 0) ==== */
static __attribute__((noinline)) int t_c99_initializers_brace_elision(void) {
    // 2D array without inner braces
    int a[2][2] = {1, 2, 3, 4};
    if (a[0][0] != 1) return 1;
    if (a[0][1] != 2) return 2;
    if (a[1][0] != 3) return 3;
    if (a[1][1] != 4) return 4;

    // 2D array with partial fill
    int b[2][3] = {1, 2, 3, 4, 5, 6};
    if (b[0][0] != 1) return 10;
    if (b[0][2] != 3) return 11;
    if (b[1][0] != 4) return 12;
    if (b[1][2] != 6) return 13;

    // Array of structs without inner braces
    struct { int a; int b; } arr[2] = {1, 2, 3, 4};
    if (arr[0].a != 1) return 20;
    if (arr[0].b != 2) return 21;
    if (arr[1].a != 3) return 22;
    if (arr[1].b != 4) return 23;

    // Struct containing array with brace elision
    struct { int arr[3]; int val; } s = {10, 20, 30, 40};
    if (s.arr[0] != 10) return 30;
    if (s.arr[1] != 20) return 31;
    if (s.arr[2] != 30) return 32;
    if (s.val != 40) return 33;

    // Mixed: braced and elided
    int c[3][2] = {{1, 2}, 3, 4, {5, 6}};
    if (c[0][0] != 1) return 40;
    if (c[0][1] != 2) return 41;
    if (c[1][0] != 3) return 42;
    if (c[1][1] != 4) return 43;
    if (c[2][0] != 5) return 44;
    if (c[2][1] != 6) return 45;

    // Partial brace elision (fewer elements than needed)
    int d[2][2] = {1, 2, 3};
    if (d[0][0] != 1) return 50;
    if (d[0][1] != 2) return 51;
    if (d[1][0] != 3) return 52;
    if (d[1][1] != 0) return 53;

    return 0;
}

/* ==== c99_initializers_brace_elision_global: exit codes 54..75 (original code + 53) ==== */
// Global 2D array with brace elision
int bg_g[2][2] = {1, 2, 3, 4};

// Global array of structs with brace elision
struct bg_Pair { int x; int y; };
struct bg_Pair bg_pairs[3] = {1, 2, 3, 4, 5, 6};

// Nested struct with brace elision
struct bg_Inner { int a; int b; };
struct bg_Outer { struct bg_Inner inner; int c; };
struct bg_Outer bg_outer = {10, 20, 30};

static __attribute__((noinline)) int t_c99_initializers_brace_elision_global(void) {
    if (bg_g[0][0] != 1) return 1;
    if (bg_g[0][1] != 2) return 2;
    if (bg_g[1][0] != 3) return 3;
    if (bg_g[1][1] != 4) return 4;

    if (bg_pairs[0].x != 1) return 10;
    if (bg_pairs[0].y != 2) return 11;
    if (bg_pairs[1].x != 3) return 12;
    if (bg_pairs[1].y != 4) return 13;
    if (bg_pairs[2].x != 5) return 14;
    if (bg_pairs[2].y != 6) return 15;

    if (bg_outer.inner.a != 10) return 20;
    if (bg_outer.inner.b != 20) return 21;
    if (bg_outer.c != 30) return 22;

    return 0;
}

/* ==== c99_initializers_string_no_brace_elision: exit codes 76..98 (original code + 75) ==== */
// String literals initialize char arrays directly, not via brace elision
struct sn_WithCharArray {
    char name[16];
    int val;
};

// Global: string literal for char array member should not consume next elements
struct sn_WithCharArray sn_g1 = {"hello", 42};

// Array of structs with string members
struct sn_WithCharArray sn_table[] = {
    {"alpha", 1},
    {"beta", 2},
};

static __attribute__((noinline)) int t_c99_initializers_string_no_brace_elision(void) {
    if (sn_g1.name[0] != 'h') return 1;
    if (sn_g1.name[4] != 'o') return 2;
    if (sn_g1.val != 42) return 3;

    // Local with brace elision NOT applied to string
    struct sn_WithCharArray local = {"world", 99};
    if (local.name[0] != 'w') return 10;
    if (local.val != 99) return 11;

    // Array of structs
    if (sn_table[0].name[0] != 'a') return 20;
    if (sn_table[0].val != 1) return 21;
    if (sn_table[1].name[0] != 'b') return 22;
    if (sn_table[1].val != 2) return 23;

    return 0;
}

/* ==== c99_initializers_global_local_parity: exit codes 99..169 (original code + 98) ==== */
struct gl_Point { int x; int y; int z; };
struct gl_Nested { struct gl_Point p; int val; };
struct gl_WithArray { int arr[4]; int extra; };

// Global initializers (static path: ast_init_list_to_ir)
struct gl_Point gl_g_desig = {.z = 30, .x = 10, .y = 20};
struct gl_Point gl_g_pos = {1, 2, 3};
struct gl_Nested gl_g_nested = {{100, 200, 300}, 400};
struct gl_Nested gl_g_nested_desig = {.p = {.y = 50, .x = 40}, .val = 60};
struct gl_WithArray gl_g_arr = {{10, 20, 30, 40}, 50};
struct gl_WithArray gl_g_arr_desig = {.arr = {[2] = 300, [0] = 100}, .extra = 99};
int gl_g_2d[2][3] = {1, 2, 3, 4, 5, 6};
int gl_g_2d_desig[2][3] = {[1] = {[2] = 99}};

static __attribute__((noinline)) int t_c99_initializers_global_local_parity(void) {
    // Local initializers (runtime path: linearize_init_list_at_offset)
    struct gl_Point l_desig = {.z = 30, .x = 10, .y = 20};
    struct gl_Point l_pos = {1, 2, 3};
    struct gl_Nested l_nested = {{100, 200, 300}, 400};
    struct gl_Nested l_nested_desig = {.p = {.y = 50, .x = 40}, .val = 60};
    struct gl_WithArray l_arr = {{10, 20, 30, 40}, 50};
    struct gl_WithArray l_arr_desig = {.arr = {[2] = 300, [0] = 100}, .extra = 99};
    int l_2d[2][3] = {1, 2, 3, 4, 5, 6};
    int l_2d_desig[2][3] = {[1] = {[2] = 99}};

    // Designated struct: global vs local
    if (gl_g_desig.x != l_desig.x) return 1;
    if (gl_g_desig.y != l_desig.y) return 2;
    if (gl_g_desig.z != l_desig.z) return 3;

    // Positional struct: global vs local
    if (gl_g_pos.x != l_pos.x) return 10;
    if (gl_g_pos.y != l_pos.y) return 11;
    if (gl_g_pos.z != l_pos.z) return 12;

    // Nested struct: global vs local
    if (gl_g_nested.p.x != l_nested.p.x) return 20;
    if (gl_g_nested.p.y != l_nested.p.y) return 21;
    if (gl_g_nested.p.z != l_nested.p.z) return 22;
    if (gl_g_nested.val != l_nested.val) return 23;

    // Nested designated: global vs local
    if (gl_g_nested_desig.p.x != l_nested_desig.p.x) return 30;
    if (gl_g_nested_desig.p.y != l_nested_desig.p.y) return 31;
    if (gl_g_nested_desig.val != l_nested_desig.val) return 32;

    // Array in struct: global vs local
    if (gl_g_arr.arr[0] != l_arr.arr[0]) return 40;
    if (gl_g_arr.arr[3] != l_arr.arr[3]) return 41;
    if (gl_g_arr.extra != l_arr.extra) return 42;

    // Designated array in struct: global vs local
    if (gl_g_arr_desig.arr[0] != l_arr_desig.arr[0]) return 50;
    if (gl_g_arr_desig.arr[1] != l_arr_desig.arr[1]) return 51;
    if (gl_g_arr_desig.arr[2] != l_arr_desig.arr[2]) return 52;
    if (gl_g_arr_desig.extra != l_arr_desig.extra) return 53;

    // 2D array brace elision: global vs local
    if (gl_g_2d[0][0] != l_2d[0][0]) return 60;
    if (gl_g_2d[1][2] != l_2d[1][2]) return 61;

    // 2D designated array: global vs local
    if (gl_g_2d_desig[0][0] != l_2d_desig[0][0]) return 70;
    if (gl_g_2d_desig[1][2] != l_2d_desig[1][2]) return 71;

    return 0;
}

/* ==== c99_string_literal_initializer_may_be_braced: exit codes 170..181 (original code + 169) ==== */
#include <wchar.h>

char sb_sb[] = {"hi"};
char sb_sc[6] = {"hi"};
wchar_t sb_sw[] = {L"hi"};
const char *sb_sp[] = {"aa", "bbb"};
char sb_nested[3][4] = {"Sun", "Mon", "Tue"};
struct sb_S { char tag[4]; int n; };
struct sb_S sb_ss = { {"ab"}, 7 };

static __attribute__((noinline)) int t_c99_string_literal_initializer_may_be_braced(void) {
    char b[] = {"hi"};
    char c[6] = {"hi"};
    wchar_t w[] = {L"hi"};
    char *cl = (char[]){"hi"};
    struct sb_S as = { {"cd"}, 9 };

    if (sizeof sb_sb != 3 || sb_sb[0] != 'h' || sb_sb[1] != 'i' || sb_sb[2] != 0) return 1;
    if (sizeof sb_sc != 6 || sb_sc[1] != 'i' || sb_sc[2] != 0 || sb_sc[5] != 0) return 2;
    if (sizeof sb_sw / sizeof sb_sw[0] != 3 || sb_sw[1] != L'i' || sb_sw[2] != 0) return 3;

    /* An array of pointers is the same shape one level up, and must not be
       swallowed by the look-through. */
    if (sizeof sb_sp / sizeof sb_sp[0] != 2 || sb_sp[1][2] != 'b') return 4;
    if (sb_nested[2][0] != 'T' || sb_nested[0][3] != 0) return 5;
    if (sb_ss.tag[1] != 'b' || sb_ss.tag[2] != 0 || sb_ss.n != 7) return 6;

    if (sizeof b != 3 || b[0] != 'h' || b[1] != 'i' || b[2] != 0) return 7;
    if (sizeof c != 6 || c[1] != 'i' || c[2] != 0 || c[5] != 0) return 8;
    if (sizeof w / sizeof w[0] != 3 || w[1] != L'i' || w[2] != 0) return 9;
    if (cl[0] != 'h' || cl[1] != 'i' || cl[2] != 0) return 10;
    if (as.tag[1] != 'd' || as.tag[2] != 0 || as.n != 9) return 11;

    if (sizeof((char[]){"abcd"}) != 5) return 12;
    return 0;
}

/* ==== c99_string_initializer_exactly_fills_its_array: exit codes 182..190 (original code + 181) ==== */
#include <string.h>

char ef_g2[2] = "hi";
char ef_g3[3] = "hi";
char ef_gb2[2] = {"hi"};

static __attribute__((noinline)) int t_c99_string_initializer_exactly_fills_its_array(void) {
    /* Neighbours on the stack must survive an exactly-sized copy. */
    char before[4];
    char b2[2] = "hi";
    char bb2[2] = {"hi"};
    char after[4];
    memset(before, 0x5a, sizeof before);
    memset(after, 0x5a, sizeof after);

    if (sizeof b2 != 2 || b2[0] != 'h' || b2[1] != 'i') return 1;
    if (sizeof bb2 != 2 || bb2[0] != 'h' || bb2[1] != 'i') return 2;
    for (int i = 0; i < 4; i++) {
        if (before[i] != 0x5a) return 3;
        if (after[i] != 0x5a) return 4;
    }

    if (sizeof ef_g2 != 2 || ef_g2[0] != 'h' || ef_g2[1] != 'i') return 5;
    if (sizeof ef_gb2 != 2 || ef_gb2[0] != 'h' || ef_gb2[1] != 'i') return 6;

    /* One byte longer, so the terminator is written and the rest zeroed. */
    if (sizeof ef_g3 != 3 || ef_g3[2] != 0) return 7;
    char b6[6] = "hi";
    if (sizeof b6 != 6 || b6[2] != 0 || b6[5] != 0) return 8;

    /* And an inferred bound still makes room for it. */
    char inferred[] = "hi";
    if (sizeof inferred != 3 || inferred[2] != 0) return 9;

    return 0;
}

/* ==== c99_universal_character_names_use_the_execution_encoding: exit codes 191..211 (original code + 190) ==== */
#include <wchar.h>
/* <uchar.h> does not exist on macOS, and the test needs only the two types. */
typedef unsigned short char16_t;
typedef unsigned int char32_t;

static __attribute__((noinline)) int t_c99_universal_character_names_use_the_execution_encoding(void) {
    /* The same character, reaching the compiler two different ways: as source
       text, and through the escape scanner. Those are separate code paths --
       each was once correct for one spelling and wrong for the other -- so
       every assertion below is made twice, once per spelling. */

    /* Narrow: bytes. */
    if (sizeof("café") != 6) return 1;
    if (sizeof("caf\u00e9") != 6) return 2;
    if ((unsigned char)"café"[3] != 0xc3) return 3;
    if ((unsigned char)"café"[4] != 0xa9) return 4;
    if ((unsigned char)"caf\u00e9"[3] != 0xc3) return 5;
    if ((unsigned char)"caf\u00e9"[4] != 0xa9) return 6;
    /* A byte escape stays one byte, and is a third path again. */
    if (sizeof("caf\xc3\xa9") != 6) return 7;

    /* Wide: characters. One element either way. */
    if (wcslen(L"café") != 4 || L"café"[3] != 233) return 8;
    if (wcslen(L"caf\u00e9") != 4 || L"caf\u00e9"[3] != 233) return 9;
    /* But two byte escapes are two elements, not one character. */
    if (wcslen(L"caf\xc3\xa9") != 5) return 10;
    if (L"caf\xc3\xa9"[3] != 0xc3 || L"caf\xc3\xa9"[4] != 0xa9) return 11;

    /* char16_t and char32_t behave the same way. */
    if (u"café"[3] != 233) return 12;
    if (u"caf\u00e9"[3] != 233) return 13;
    if (U"café"[3] != 233) return 14;
    if (U"caf\u00e9"[3] != 233) return 15;

    /* Outside the BMP: char16_t splits into a surrogate pair. */
    if (u"😀"[0] != 0xd83d || u"😀"[1] != 0xde00) return 16;
    if (u"\U0001f600"[0] != 0xd83d || u"\U0001f600"[1] != 0xde00) return 17;
    if (U"😀"[0] != 0x1f600) return 18;
    if (U"\U0001f600"[0] != 0x1f600) return 19;
    if (sizeof("😀") != 5) return 20;
    if (sizeof("\U0001f600") != 5) return 21;

    return 0;
}

/* ==== c99_struct_member_string_initializer_keeps_its_bytes: exit codes 212..219 (original code + 211) ==== */
struct ms_G { char tag[4]; int guard; };
struct ms_G ms_global = { "\xc2\x80", 0x5a5a5a5a };

static __attribute__((noinline)) int t_c99_struct_member_string_initializer_keeps_its_bytes(void) {
    struct ms_G g = { "\xc2\x80", 0x5a5a5a5a };
    if ((unsigned char)g.tag[0] != 0xc2) return 1;
    if ((unsigned char)g.tag[1] != 0x80) return 2;
    if (g.tag[2] != 0 || g.tag[3] != 0) return 3;
    if (g.guard != 0x5a5a5a5a) return 4;

    /* The static path, which was already correct, must stay so. */
    if ((unsigned char)ms_global.tag[0] != 0xc2) return 5;
    if ((unsigned char)ms_global.tag[1] != 0x80) return 6;
    if (ms_global.guard != 0x5a5a5a5a) return 7;

    /* Plain ASCII, unaffected either way. */
    struct ms_G a = { "ab", 1 };
    if (a.tag[0] != 'a' || a.tag[1] != 'b' || a.tag[2] != 0 || a.guard != 1) return 8;

    return 0;
}

/* ==== c99_brace_elision_decides_the_deduced_array_bound: exit codes 220..240 (original code + 219) ==== */
struct db_P { int x, y; };
struct db_Q { int a; struct db_P p; };

/* Elided braces: several scalars per element. */
int   db_e1[][2]  = {1,2,3,4};
int   db_e2[][3]  = {1,2,3,4};          /* partial final row */
int   db_e3[][2]  = {1,2,3,4,5};        /* partial final row */
struct db_P db_e4[]  = {1,2,3,4};
struct db_Q db_e5[]  = {1,2,3,4,5,6};      /* three scalars each */

/* Explicit braces: one element each -- these were always right. */
int   db_b1[][2]  = {{1,2},{3,4}};
struct db_P db_b2[]  = {{1,2},{3,4}};
char  db_b3[][4]  = {"ab","cd"};

/* A designator still places elements where it says. */
int   db_d1[]     = {[3] = 4, [1] = 2};

static __attribute__((noinline)) int t_c99_brace_elision_decides_the_deduced_array_bound(void) {
    if (sizeof db_e1 / sizeof db_e1[0] != 2) return 1;
    if (db_e1[0][0] != 1 || db_e1[0][1] != 2) return 2;
    if (db_e1[1][0] != 3 || db_e1[1][1] != 4) return 3;

    if (sizeof db_e2 / sizeof db_e2[0] != 2) return 4;
    if (db_e2[1][0] != 4 || db_e2[1][1] != 0 || db_e2[1][2] != 0) return 5;

    if (sizeof db_e3 / sizeof db_e3[0] != 3) return 6;
    if (db_e3[2][0] != 5 || db_e3[2][1] != 0) return 7;

    if (sizeof db_e4 / sizeof db_e4[0] != 2) return 8;
    if (db_e4[1].x != 3 || db_e4[1].y != 4) return 9;

    if (sizeof db_e5 / sizeof db_e5[0] != 2) return 10;
    if (db_e5[1].a != 4 || db_e5[1].p.x != 5 || db_e5[1].p.y != 6) return 11;

    if (sizeof db_b1 / sizeof db_b1[0] != 2) return 12;
    if (sizeof db_b2 / sizeof db_b2[0] != 2) return 13;
    if (sizeof db_b3 / sizeof db_b3[0] != 2) return 14;
    if (db_b3[1][0] != 'c') return 15;

    if (sizeof db_d1 / sizeof db_d1[0] != 4) return 16;
    if (db_d1[3] != 4 || db_d1[1] != 2 || db_d1[0] != 0) return 17;

    /* The same rule at block scope. */
    {
        int l1[][2] = {1,2,3,4};
        struct db_P l2[] = {1,2,3,4};
        if (sizeof l1 / sizeof l1[0] != 2) return 18;
        if (sizeof l2 / sizeof l2[0] != 2) return 19;
        if (l1[1][1] != 4 || l2[1].y != 4) return 20;
    }

    /* An array of scalars is unaffected. */
    {
        int s[] = {1,2,3,4,5};
        if (sizeof s / sizeof s[0] != 5) return 21;
    }
    return 0;
}

int main(void)
{
    int r;
    if ((r = t_c99_initializers_brace_elision()) != 0) return 0 + r;
    if ((r = t_c99_initializers_brace_elision_global()) != 0) return 53 + r;
    if ((r = t_c99_initializers_string_no_brace_elision()) != 0) return 75 + r;
    if ((r = t_c99_initializers_global_local_parity()) != 0) return 98 + r;
    if ((r = t_c99_string_literal_initializer_may_be_braced()) != 0) return 169 + r;
    if ((r = t_c99_string_initializer_exactly_fills_its_array()) != 0) return 181 + r;
    if ((r = t_c99_universal_character_names_use_the_execution_encoding()) != 0) return 190 + r;
    if ((r = t_c99_struct_member_string_initializer_keeps_its_bytes()) != 0) return 211 + r;
    if ((r = t_c99_brace_elision_decides_the_deduced_array_bound()) != 0) return 219 + r;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("c99_initializers_strings_mega", code, &[]),
        0
    );
}

// ============================================================================
// Mega-test: over-long strings and union/GNU designators, at -O1 and -O2
// ============================================================================

// Original test documentation, in section order:
//
// ---- c99_over_long_string_initializer_is_truncated ----
// A string literal longer than the object it initializes is truncated, not
// written past the end.
//
// gcc accepts the over-long form with a warning and keeps only what fits.
// c17 emitted the whole literal, so the excess landed on whatever came
// next: with `const char a[2][3] = { "1234", "xyz" };` the stray `4`
// overwrote the second row, and `struct { char n[3]; int tag; } s =
// { "wxyz", 7 };` pushed `tag` one byte along, reading back 1792 instead
// of 7.
//
// The wide variants had the same defect, plus one of their own: the null
// terminator is part of the value only when there is room for it, so
// `wchar_t w[3] = L"abc"` holds three characters and no terminator.
//
// The torture test is `pr86714`, which only reaches the narrow array case.
// ---- c99_union_first_member_may_be_anonymous ----
// A union's first member for a positional initializer may be an anonymous
// aggregate.
//
// C17 6.7.9p17 initializes a union's first member, and 6.7.2.1p13 makes the
// members of an anonymous structure members of the union itself -- so the
// anonymous structure *is* that first member. c17 searched for the first
// member with a *name* and so skipped it:
//
//   union { struct { int a, b; }; long q; } u = {{1,2}};
//
// wrote `{1,2}` into `q` and left `b` zero, and a union whose members are
// all anonymous found none at all and stayed entirely zero -- which is the
// torture test `pr87053`, where two anonymous structures overlay the same
// eight bytes.
// ---- c99_gnu_colon_field_designator ----
// GNU's obsolete field designator, `fieldname: value`.
//
// It predates C99's `.fieldname = value`; gcc still accepts it under
// `-Wdeprecated` and glibc-era sources use it. One token of lookahead
// settles the form: inside an initializer list an identifier followed by `:`
// cannot be anything else, because a conditional starts `x ?` and a label
// cannot appear there.
//
// Four torture tests need it: `20030408-1`, `991228-1`, `compndlit-1` and
// `struct-ini-4`.
//
/// Over-long string initializers, a union's anonymous first member, and
/// GNU `field:` designators -- each original ran at the matrix level and
/// at -O2, and so does this.
///
/// Consolidates (one C section each, a `t_<name>` function):
/// - `c99_over_long_string_initializer_is_truncated`
/// - `c99_union_first_member_may_be_anonymous`
/// - `c99_gnu_colon_field_designator`
///
/// Exit codes: see the map at the top of the program.
#[test]
fn c99_initializers_overlong_and_union_designators_mega() {
    let code = r#"
/* Exit-code map: each section returns its original code, offset by
   the base listed in its banner.
       1.. 21  c99_over_long_string_initializer_is_truncated
      22.. 35  c99_union_first_member_may_be_anonymous
      36.. 46  c99_gnu_colon_field_designator
*/

/* ==== c99_over_long_string_initializer_is_truncated: exit codes 1..21 (original code + 0) ==== */
typedef __WCHAR_TYPE__ ol_wch;
typedef __CHAR16_TYPE__ ol_c16;
typedef __CHAR32_TYPE__ ol_c32;

struct ol_S { char n[3]; int tag; };
struct ol_WS { ol_wch n[3]; int tag; };
struct ol_US { ol_c16 n[2]; int tag; };
struct ol_TS { ol_c32 n[2]; int tag; };

/* Narrow: an over-long row must not reach the next one. */
const char ol_rows[2][3] = { "1234", "xyz" };
const char ol_one[3] = "12345";
const char ol_tight[3] = "abc";          /* exactly fills, no terminator */
const char ol_roomy[2][4] = { "abc", "def" };   /* the ordinary case */
const struct ol_S ol_s = { "wxyz", 7 };

ol_wch ol_w_room[4] = L"abc";
ol_wch ol_w_tight[3] = L"abc";
ol_wch ol_w_over[2] = L"abcd";
const struct ol_WS ol_ws = { L"abcd", 9 };

ol_c16 ol_u16_tight[3] = u"abc";
ol_c16 ol_u16_over[2] = u"abcd";
const struct ol_US ol_us = { u"abcd", 5 };

ol_c32 ol_u32_tight[3] = U"abc";
const struct ol_TS ol_ts = { U"abcd", 6 };

static __attribute__((noinline)) int t_c99_over_long_string_initializer_is_truncated(void) {
    if (ol_rows[0][0] != '1' || ol_rows[0][1] != '2' || ol_rows[0][2] != '3') return 1;
    if (ol_rows[1][0] != 'x' || ol_rows[1][1] != 'y' || ol_rows[1][2] != 'z') return 2;
    if (ol_one[0] != '1' || ol_one[1] != '2' || ol_one[2] != '3') return 3;
    if (ol_tight[0] != 'a' || ol_tight[1] != 'b' || ol_tight[2] != 'c') return 4;
    if (ol_roomy[0][0] != 'a' || ol_roomy[0][3] != '\0') return 5;
    if (ol_roomy[1][0] != 'd' || ol_roomy[1][3] != '\0') return 6;
    if (ol_s.n[0] != 'w' || ol_s.n[1] != 'x' || ol_s.n[2] != 'y') return 7;
    if (ol_s.tag != 7) return 8;

    /* Wide: the terminator only when it fits. */
    if (ol_w_room[0] != L'a' || ol_w_room[2] != L'c' || ol_w_room[3] != 0) return 9;
    if (ol_w_tight[0] != L'a' || ol_w_tight[1] != L'b' || ol_w_tight[2] != L'c') return 10;
    if (ol_w_over[0] != L'a' || ol_w_over[1] != L'b') return 11;
    if (ol_ws.n[0] != L'a' || ol_ws.n[2] != L'c' || ol_ws.tag != 9) return 12;

    if (ol_u16_tight[0] != u'a' || ol_u16_tight[2] != u'c') return 13;
    if (ol_u16_over[0] != u'a' || ol_u16_over[1] != u'b') return 14;
    if (ol_us.n[0] != u'a' || ol_us.n[1] != u'b' || ol_us.tag != 5) return 15;

    if (ol_u32_tight[0] != U'a' || ol_u32_tight[2] != U'c') return 16;
    if (ol_ts.n[0] != U'a' || ol_ts.n[1] != U'b' || ol_ts.tag != 6) return 17;

    /* The same shapes as automatic and static-local objects, which take
       their own emission paths. */
    {
        char a[2][3] = { "1234", "xyz" };
        if (a[0][2] != '3' || a[1][0] != 'x') return 18;
        struct ol_S ls = { "wxyz", 7 };
        if (ls.n[2] != 'y' || ls.tag != 7) return 19;
    }
    {
        static char a[2][3] = { "1234", "xyz" };
        if (a[0][2] != '3' || a[1][0] != 'x') return 20;
        static struct ol_S ls = { "wxyz", 7 };
        if (ls.n[2] != 'y' || ls.tag != 7) return 21;
    }
    return 0;
}

/* ==== c99_union_first_member_may_be_anonymous: exit codes 22..35 (original code + 21) ==== */
/* Two anonymous structures over the same bytes: the first one initializes,
   and the second must read what it wrote. */
const union {
    struct { char x[4]; char y[4]; };
    struct { char z[8]; };
} ua_overlay = {{"1234", "567"}};

/* An anonymous structure ahead of a named member. */
union ua_AB { struct { int a; int b; }; long q; } ua_ab = {{1, 2}};
/* The designated spelling, which already worked, must keep working. */
union ua_AB ua_ab_desig = {.a = 3, .b = 4};
union ua_AB ua_ab_other = {.q = 5};
/* A named first member is unchanged. */
union ua_NM { struct { int a; int b; } s; long q; } ua_nm = {{6, 7}};
/* A plain union, and an anonymous union inside a struct. */
union ua_P { int i; long q; } ua_p = {8};
struct ua_WithAnon { struct { int a; int b; }; int t; } ua_wa = {{9, 10}, 11};
/* An unnamed bit-field must still be skipped, not chosen. */
union ua_BF { unsigned : 3; int v; } ua_bf = {12};

static __attribute__((noinline)) int t_c99_union_first_member_may_be_anonymous(void) {
    if (sizeof(ua_overlay) != 8) return 1;
    if (ua_overlay.x[0] != '1' || ua_overlay.x[3] != '4') return 2;
    if (ua_overlay.y[0] != '5' || ua_overlay.y[2] != '7') return 3;
    if (ua_overlay.z[0] != '1' || ua_overlay.z[6] != '7' || ua_overlay.z[7] != '\0') return 4;
    if (__builtin_strlen(ua_overlay.z) != 7) return 5;

    if (ua_ab.a != 1 || ua_ab.b != 2) return 6;
    if (ua_ab_desig.a != 3 || ua_ab_desig.b != 4) return 7;
    if (ua_ab_other.q != 5) return 8;
    if (ua_nm.s.a != 6 || ua_nm.s.b != 7) return 9;
    if (ua_p.i != 8) return 10;
    if (ua_wa.a != 9 || ua_wa.b != 10 || ua_wa.t != 11) return 11;
    if (ua_bf.v != 12) return 12;

    /* The same shapes as automatic and static-local objects. */
    {
        union ua_AB l = {{13, 14}};
        if (l.a != 13 || l.b != 14) return 13;
        static union ua_AB s = {{15, 16}};
        if (s.a != 15 || s.b != 16) return 14;
    }
    return 0;
}

/* ==== c99_gnu_colon_field_designator: exit codes 36..46 (original code + 35) ==== */
extern int printf(const char *, ...);

struct gc_s { int a[3]; int c[3]; };
struct gc_s gc_g1 = { c: {1, 2, 3} };

__extension__ union gc_U { double d; int i[2]; } gc_u = { d: -0.25 };

struct gc_P { int a, b, c; };
struct gc_P gc_g2 = { b: 5, a: 6, c: 7 };
/* Mixed with the C99 spelling in one list. */
struct gc_P gc_g3 = { .b = 5, a: 6, c: 7 };
/* Nested, at both levels. */
struct gc_Q { int x; struct gc_P p; };
struct gc_Q gc_g4 = { x: 1, p: { a: 2, b: 3, c: 4 } };
/* An array of structs, reached through an index designator. */
struct gc_P gc_g5[2] = { [1] = { a: 8, c: 9 } };

static __attribute__((noinline)) int t_c99_gnu_colon_field_designator(void) {
    /* The designated member is set and the others stay zero. */
    if (gc_g1.c[0] != 1 || gc_g1.c[1] != 2 || gc_g1.c[2] != 3) return 1;
    if (gc_g1.a[0] != 0 || gc_g1.a[1] != 0 || gc_g1.a[2] != 0) return 2;

    if (gc_u.d != -0.25) return 3;

    if (gc_g2.a != 6 || gc_g2.b != 5 || gc_g2.c != 7) return 4;
    if (gc_g3.a != 6 || gc_g3.b != 5 || gc_g3.c != 7) return 5;
    if (gc_g4.x != 1 || gc_g4.p.a != 2 || gc_g4.p.b != 3 || gc_g4.p.c != 4) return 6;
    if (gc_g5[0].a != 0 || gc_g5[1].a != 8 || gc_g5[1].b != 0 || gc_g5[1].c != 9) return 7;

    /* An automatic object and a compound literal take the same path. */
    {
        struct gc_P l = { c: 9, a: 8 };
        if (l.a != 8 || l.b != 0 || l.c != 9) return 8;
        struct gc_P cl = (struct gc_P){ b: 1, a: 2, c: 3 };
        if (cl.a != 2 || cl.b != 1 || cl.c != 3) return 9;
    }
    /* A conditional in an initializer must still parse as one. */
    {
        int k = 1;
        struct gc_P q = { k ? 4 : 5, 6, 7 };
        if (q.a != 4 || q.b != 6 || q.c != 7) return 10;
    }
    /* And the C99 spelling on its own is untouched. */
    {
        struct gc_P r = { .c = 11, .a = 12 };
        if (r.a != 12 || r.b != 0 || r.c != 11) return 11;
    }
    return 0;
}

int main(void)
{
    int r;
    if ((r = t_c99_over_long_string_initializer_is_truncated()) != 0) return 0 + r;
    if ((r = t_c99_union_first_member_may_be_anonymous()) != 0) return 21 + r;
    if ((r = t_c99_gnu_colon_field_designator()) != 0) return 35 + r;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("c99_initializers_overlong_union_mega", code, &[]),
        0
    );
    assert_eq!(
        compile_and_run(
            "c99_initializers_overlong_union_mega_o2",
            code,
            &["-O2".to_string()]
        ),
        0
    );
}
