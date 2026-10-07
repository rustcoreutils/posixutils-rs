//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Symbols and storage: sections, linkage, static and extern objects,
// thread-local storage, labels and debug information.
//
// The cases that only inspect assembly run in process, in
// `cc/test_asm/codegen_symbols.rs`.
//

use crate::common::{
    compile_and_dlopen, compile_and_run, compile_and_run_everywhere, compile_and_run_optimized,
    compile_and_run_two_units, create_c_file,
};
use plib::testing::run_test_base;
use std::process::Command;

// ============================================================================
// Mega-test: Debug info generation
// ============================================================================

#[test]
fn codegen_debug_sections() {
    let c_file = create_c_file(
        "debug_test",
        r#"
int foo(int x) {
    return x + 1;
}

int main() {
    return foo(41);
}
"#,
    );
    let c_path = c_file.path().to_path_buf();
    let obj_path = std::env::temp_dir().join("c17_debug_mega_test.o");

    // Compile with -g -c to produce object file
    let output = run_test_base(
        "c17",
        &[
            "-g".to_string(),
            "-c".to_string(),
            "-o".to_string(),
            obj_path.to_string_lossy().to_string(),
            c_path.to_string_lossy().to_string(),
        ],
        &[],
    );

    assert!(
        output.status.success(),
        "c17 -g -c failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    // Check for debug sections using platform-specific tools
    #[cfg(target_os = "macos")]
    {
        let otool_output = Command::new("otool")
            .args(["-l", obj_path.to_str().unwrap()])
            .output()
            .expect("failed to run otool");

        let otool_stdout = String::from_utf8_lossy(&otool_output.stdout);
        assert!(
            otool_stdout.contains("__debug_line") || otool_stdout.contains("__DWARF"),
            "No debug sections found in object file"
        );
    }

    #[cfg(target_os = "linux")]
    {
        let objdump_output = Command::new("objdump")
            .args(["-h", obj_path.to_str().unwrap()])
            .output()
            .expect("failed to run objdump");

        let objdump_stdout = String::from_utf8_lossy(&objdump_output.stdout);
        assert!(
            objdump_stdout.contains(".debug_line") || objdump_stdout.contains(".debug_info"),
            "No debug sections found in object file"
        );
    }

    let _ = std::fs::remove_file(&obj_path);
}

/// A c17 unit linked after another compiler's keeps its own abbreviation
/// table. Its compile unit header said offset 0, which in a linked binary is
/// the first object's table: dpkg's dselect links g++ units ahead of c17
/// ones, and `dwz` -- and readelf -- decoded the c17 units against g++'s
/// abbreviations ("Could not find DWARF abbreviation 105").
#[cfg(target_os = "linux")]
#[test]
fn codegen_debug_info_survives_a_mixed_link() {
    let host_cc = ["cc", "gcc"].into_iter().find(|cc| {
        Command::new(cc)
            .arg("--version")
            .output()
            .is_ok_and(|o| o.status.success())
    });
    let Some(host_cc) = host_cc else {
        return;
    };
    let dir = plib::tmp::Builder::new()
        .prefix("c17_mixed_dwarf_")
        .tempdir()
        .expect("tempdir");
    let path = |name: &str| dir.path().join(name).to_string_lossy().into_owned();
    std::fs::write(
        path("first.c"),
        "struct pair { int a; long b; };\nint first(struct pair *p) { return p->a + (int)p->b; }\n",
    )
    .unwrap();
    std::fs::write(
        path("second.c"),
        "int first(void *);\nint main(void) { return first(0) * 0; }\nint second(int x) { return x; }\n",
    )
    .unwrap();
    let host = Command::new(host_cc)
        .args(["-g", "-O0", "-c", "-o", &path("first.o"), &path("first.c")])
        .output()
        .expect("host cc");
    assert!(
        host.status.success(),
        "{}",
        String::from_utf8_lossy(&host.stderr)
    );
    let ours = run_test_base(
        "c17",
        &[
            "-g".to_string(),
            "-c".to_string(),
            "-o".to_string(),
            path("second.o"),
            path("second.c"),
        ],
        &[],
    );
    assert!(
        ours.status.success(),
        "{}",
        String::from_utf8_lossy(&ours.stderr)
    );
    let link = Command::new(host_cc)
        .args(["-o", &path("mixed"), &path("first.o"), &path("second.o")])
        .output()
        .expect("link");
    assert!(
        link.status.success(),
        "{}",
        String::from_utf8_lossy(&link.stderr)
    );

    let dump = Command::new("readelf")
        .args(["--debug-dump=info", &path("mixed")])
        .output()
        .expect("readelf");
    let stdout = String::from_utf8_lossy(&dump.stdout);
    let stderr = String::from_utf8_lossy(&dump.stderr);
    assert!(!stderr.contains("Warning"), "{stderr}");
    assert!(
        stdout.contains("DW_AT_name        : second"),
        "the c17 unit's functions must decode:\n{stdout}"
    );
}

// ============================================================================
// Mega-test: runtime checks of symbols and storage
// ============================================================================
//
// Consolidates, one C program with each original `main` as a `t_<name>()`
// section (exit-code ranges are documented in the program):
//   codegen_ssa_phi_goto_dispatch
//   codegen_ptr_arith_narrow_index
//   codegen_char_literal_signedness
//   codegen_static_local_struct
//   codegen_struct_store_no_rax_clobber
//   codegen_address_of_thread_local_is_thread_local
//   codegen_pointer_and_enum_signedness
//   codegen_static_address_difference
//   codegen_const_objects_fold_in_static_initializers
//
// --- codegen_ssa_phi_goto_dispatch ---
// ============================================================================
// Regression test: SSA phi insertion in goto-dispatch pattern
// ============================================================================
// Tests that variables declared at function scope maintain correct values
// across a goto-dispatch loop with a switch statement. This pattern is
// used heavily by CPython's ceval.c bytecode interpreter.
//
// The bug: insert_phi_nodes() in ssa.rs had a filter that skipped phi node
// insertion at IDF blocks not dominated by the variable's declaration block.
// This caused function-scope variables to lose their values at the dispatch
// label when connected by goto back-edges from case handlers.
//
// --- codegen_ptr_arith_narrow_index ---
// Test: pointer arithmetic with char/short index uses correct sign extension
//
// The negative-index sections say `signed char`, not plain `char`. Plain
// `char`'s signedness is implementation-defined (C17 6.2.5p15) and it is
// unsigned on aarch64, where `-2` is 254 and the subscript runs off the end
// of the array -- cross-gcc fails the plain-`char` form there too. What this
// test is about is whether a *signed* narrow index sign-extends, so it now
// says so. A plain-`char` index is still covered by the positive sections,
// where the two spellings agree.
//
// --- codegen_char_literal_signedness ---
// A character constant takes its value from plain `char`, whose signedness is
// the target's.
//
// C17 6.4.4.4p10: an unprefixed character constant has type `int` and the
// value of a `char` object holding the character, converted to `int`. So
// `'\x80'` is -128 where plain `char` is signed (x86-64) and **128** where it
// is not (aarch64), and `__CHAR_UNSIGNED__` says which. Both are asserted
// here rather than one, because the original test hardcoded the x86-64
// answers and would fail on aarch64 for a conforming compiler.
//
// The dispatch half is the shape that matters: CPython's serialization module
// writes `enum { PROTO = '\x80' }` and switches on a `char` read from a
// buffer. Those two must agree, and they did not while the constant's value
// was computed signed and the `char` type had become unsigned.
//
// The prefixed forms are covered too. They take the code point in their own
// type and never consult plain `char` -- `L'\x80'` is 128 on every target --
// which the shared conversion got wrong independently of any of this.
//
// --- codegen_static_local_struct ---
// Regression test: static local struct variables must be in static storage,
// not on the stack. The linearizer checked type modifiers instead of
// declarator.storage_class for the STATIC flag, which missed struct types.
//
// --- codegen_struct_store_no_rax_clobber ---
// Regression test: emit_struct_store used RAX as a data shuttle in the qword
// copy loop (R10=src, R11=dst, RAX=shuttle). If a live pseudo was in RAX,
// it got clobbered. Fixed by using XMM15 (reserved scratch) as shuttle.
//
// --- codegen_address_of_thread_local_is_thread_local ---
// The address of a `_Thread_local` object is a real thread-local address.
//
// `SymAddr` did not consult the TLS symbol set, so `&tls_var` produced an
// ordinary global address -- `movq tv@GOTPCREL(%rip), %rax` where gcc emits
// `%fs:tv@tpoff`. Reads through it happened to survive; a store through it
// segfaulted, at both -O0 and -O2.
//
// Every expectation was checked against gcc on the same source, which returns
// 0 throughout.
//
// --- codegen_pointer_and_enum_signedness ---
// A pointer comparison is unsigned, and an enumeration takes the signedness
// of the type wide enough to hold it.
//
// Both were asked of a predicate that answers about *integer* types: a
// pointer is not one, so `is_unsigned` said false and `a < b` emitted a
// signed `setl` where gcc emits `setb` -- C17 6.5.8 compares addresses, and
// an address is unsigned. And an enumeration never carried a signedness at
// all, so although `enum_underlying_type` correctly picked `unsigned int`
// for `enum E { BIG = 0x80000000u }`, an *object* of that type was loaded
// and compared as signed: `e` read back -2147483648 and `e < 0` was true,
// while the constant `BIG` was right all along.
//
// --- codegen_static_address_difference ---
// The *values* of the address differences #C101 made compilable.
//
// Acceptance alone would be satisfied by folding to any number, and the two
// unit conventions are easy to swap: `ptr - ptr` counts elements (6.5.6p9)
// while a subtraction of two addresses already cast to an integer counts
// bytes. Each row is checked against the answer gcc gives.
//
// --- codegen_const_objects_fold_in_static_initializers ---
// The values a folded `const` object contributes to a static initializer
// (#C102).
//
// Acceptance alone is not the check: before the fix `int w = c;` was accepted
// and happened to be right while `int w = c + 1;` was rejected, so the
// arithmetic is what distinguishes a real fold from an accident.
//
// `codegen_pointer_and_enum_signedness` also keeps its own -O1 run below.
#[test]
fn codegen_symbols_runtime_mega() {
    let code = r#"/*
 * Consolidates the symbols.rs compile_and_run(&[]) programs. Each original
 * main is a t_<test>() function; main returns base + that function's code.
 *
 *   codegen_ssa_phi_goto_dispatch              1..6
 *   codegen_ptr_arith_narrow_index             11..15
 *   codegen_char_literal_signedness            21..34
 *   codegen_static_local_struct                41..43
 *   codegen_struct_store_no_rax_clobber        51..55
 *   codegen_address_of_thread_local_is_thread_local  61..73
 *   codegen_pointer_and_enum_signedness        81..112
 *   codegen_static_address_difference          121..127
 *   codegen_const_objects_fold_in_static_initializers  131..140
 */
#include <stddef.h>

/* ===== codegen_ssa_phi_goto_dispatch =====
 * Regression test for SSA phi insertion bug in goto-dispatch patterns.
 *
 * This mimics CPython's ceval.c pattern: a bytecode dispatch loop where
 * a variable (accumulator) is modified by different opcode handlers and
 * its value must survive across goto back-edges to the dispatch label.
 */

/* Opcodes for our mini interpreter */
#define OP_LOAD_CONST  0
#define OP_ADD         1
#define OP_MUL         2
#define OP_NEGATE      3
#define OP_HALT        4

struct instruction {
    int opcode;
    int operand;
};

/* A mini bytecode interpreter using goto-dispatch */
int interpret(struct instruction *code, int n_insns) {
    int accumulator = 0;   /* function-scope variable: the bug target */
    int ip = 0;            /* instruction pointer */
    int temp;

    goto dispatch;

dispatch:
    if (ip >= n_insns)
        return -1;  /* ran off the end */

    switch (code[ip].opcode) {
    case OP_LOAD_CONST:
        goto do_load_const;
    case OP_ADD:
        goto do_add;
    case OP_MUL:
        goto do_mul;
    case OP_NEGATE:
        goto do_negate;
    case OP_HALT:
        goto do_halt;
    default:
        return -2;  /* unknown opcode */
    }

do_load_const:
    accumulator = code[ip].operand;
    ip++;
    goto dispatch;

do_add:
    accumulator = accumulator + code[ip].operand;
    ip++;
    goto dispatch;

do_mul:
    accumulator = accumulator * code[ip].operand;
    ip++;
    goto dispatch;

do_negate:
    accumulator = -accumulator;
    ip++;
    goto dispatch;

do_halt:
    return accumulator;
}

static int t_ssa_phi_goto_dispatch(void) {
    /* Test 1: LOAD_CONST 5, HALT -> expect 5 */
    {
        struct instruction prog[] = {
            {OP_LOAD_CONST, 5},
            {OP_HALT, 0}
        };
        int result = interpret(prog, 2);
        if (result != 5) return 1;
    }

    /* Test 2: LOAD_CONST 3, ADD 7, HALT -> expect 10 */
    {
        struct instruction prog[] = {
            {OP_LOAD_CONST, 3},
            {OP_ADD, 7},
            {OP_HALT, 0}
        };
        int result = interpret(prog, 3);
        if (result != 10) return 2;
    }

    /* Test 3: LOAD_CONST 4, MUL 5, HALT -> expect 20 */
    {
        struct instruction prog[] = {
            {OP_LOAD_CONST, 4},
            {OP_MUL, 5},
            {OP_HALT, 0}
        };
        int result = interpret(prog, 3);
        if (result != 20) return 3;
    }

    /* Test 4: LOAD_CONST 7, NEGATE, HALT -> expect -7 */
    {
        struct instruction prog[] = {
            {OP_LOAD_CONST, 7},
            {OP_NEGATE, 0},
            {OP_HALT, 0}
        };
        int result = interpret(prog, 3);
        if (result != -7) return 4;
    }

    /* Test 5: Multi-step computation:
     * LOAD_CONST 2, MUL 3, ADD 4, NEGATE, ADD 100, HALT
     * -> ((2 * 3) + 4) = 10, negate = -10, + 100 = 90 */
    {
        struct instruction prog[] = {
            {OP_LOAD_CONST, 2},
            {OP_MUL, 3},
            {OP_ADD, 4},
            {OP_NEGATE, 0},
            {OP_ADD, 100},
            {OP_HALT, 0}
        };
        int result = interpret(prog, 6);
        if (result != 90) return 5;
    }

    /* Test 6: Verify accumulator starts at 0 each call (no state leakage)
     * ADD 42, HALT -> 0 + 42 = 42 */
    {
        struct instruction prog[] = {
            {OP_ADD, 42},
            {OP_HALT, 0}
        };
        int result = interpret(prog, 2);
        if (result != 42) return 6;
    }

    return 0;
}

/* ===== codegen_ptr_arith_narrow_index ===== */
int pa_arr[10] = {0, 10, 20, 30, 40, 50, 60, 70, 80, 90};

static int t_ptr_arith_narrow_index(void) {
    int *p = pa_arr;

    // Section 1: char index (positive: signedness-independent)
    char ci = 3;
    if (*(p + ci) != 30) return 1;

    // Section 2: short index
    short si = 5;
    if (*(p + si) != 50) return 2;

    // Section 3: negative signed char index from middle of array
    int *mid = &pa_arr[5];
    signed char neg = -2;
    if (*(mid + neg) != 30) return 3;

    // Section 4: negative short index
    short sneg = -3;
    if (*(mid + sneg) != 20) return 4;

    // Section 5: an unsigned narrow index must NOT sign-extend
    unsigned char uc = 2;
    if (*(p + uc) != 20) return 5;

    return 0;
}

/* ===== codegen_char_literal_signedness ===== */
enum opcode { NONE = 'N', PROTO = '\x80', STOP = '.' };

int dispatch(const char *s) {
    switch ((enum opcode)s[0]) {
    case NONE: return 1;
    case PROTO: return 2;
    case STOP: return 3;
    default: return -1;
    }
}

static int t_char_literal_signedness(void) {
    char proto = '\x80';
    char none = 'N';
    char stop = '.';

    /* The enumerator and the char read back from memory must agree, whatever
       the target's plain-char signedness is. */
    if (dispatch(&proto) != 2) return 1;  /* PROTO must match */
    if (dispatch(&none) != 1) return 2;
    if (dispatch(&stop) != 3) return 3;

    /* The value itself, per target. */
    int val  = '\x80';
    int val2 = '\xff';
#ifdef __CHAR_UNSIGNED__
    if (val  != 128) return 4;
    if (val2 != 255) return 5;
    if ((int)proto != 128) return 6;
#else
    if (val  != -128) return 4;
    if (val2 != -1) return 5;
    if ((int)proto != -128) return 6;
#endif

    /* An ASCII constant is the same everywhere. */
    if ('N' != 78 || '.' != 46) return 7;

    /* A prefixed constant is the code point, on every target. */
    if ((int)L'\x80' != 128) return 10;
    if ((int)u'\x80' != 128) return 11;
    if ((int)U'\x80' != 128) return 12;
    if ((int)L'\xe9' != 233) return 13;
    if ((int)L'e' != 101) return 14;

    return 0;
}

/* ===== codegen_static_local_struct ===== */
struct Parser {
    int initialized;
    const char *fname;
    struct Parser *next;
};

struct Parser *get_parser(void) {
    static struct Parser p = { .fname = "test" };
    return &p;
}

static int t_static_local_struct(void) {
    struct Parser *p1 = get_parser();
    struct Parser *p2 = get_parser();

    /* Must return same address (static storage) */
    if (p1 != p2) return 1;

    /* Must NOT be near the stack */
    int stack_var = 0;
    ptrdiff_t diff = (char*)p1 - (char*)&stack_var;
    if (diff < 0) diff = -diff;
    if (diff < 1000000) return 2;  /* on stack = bug */

    /* Value must persist across calls */
    p1->initialized = 42;
    if (get_parser()->initialized != 42) return 3;

    return 0;
}

/* ===== codegen_struct_store_no_rax_clobber ===== */
typedef struct { long a; long b; long c; } Triple;

Triple make_triple(long x, long y, long z) {
    Triple t;
    t.a = x; t.b = y; t.c = z;
    return t;
}

int compute(int x) { return x * 11; }

static int t_struct_store_no_rax_clobber(void) {
    /* Get a value in RAX from a call, then do a struct copy */
    int val = compute(5);   /* val = 55, likely in RAX */
    Triple t = make_triple(1, 2, 3);  /* struct store — must not clobber val */
    int check = val + 1;
    if (check != 56) return 1;
    if (t.a != 1 || t.b != 2 || t.c != 3) return 2;

    /* Chain: struct copy while multiple ints are live */
    int a = compute(3);  /* 33 */
    int b = compute(4);  /* 44 */
    Triple t2 = make_triple(10, 20, 30);
    if (a + b != 77) return 3;
    if (t2.a != 10 || t2.b != 20 || t2.c != 30) return 4;

    /* Struct assignment (copy, not return) */
    Triple t3;
    t3 = t2;
    if (t3.a != 10 || t3.b != 20 || t3.c != 30) return 5;

    return 0;
}

/* ===== codegen_address_of_thread_local_is_thread_local ===== */
_Thread_local int tv = 11;
_Thread_local long tl = 22;
_Thread_local int tarr[4] = { 1, 2, 3, 4 };
_Thread_local struct { int a; int b; } ts = { 5, 6 };

static int *launder(int *p) { return p; }

static int t_address_of_thread_local(void)
{
    /* read through a taken address */
    int *p = &tv;
    if (*p != 11) return 1;
    if (*(&tv) != tv) return 2;
    if (*launder(&tv) != 11) return 3;

    /* write through a taken address -- this is what segfaulted */
    *(&tv) = 99;
    if (tv != 99) return 4;
    *p = 77;
    if (tv != 77) return 5;
    *launder(&tv) = 55;
    if (tv != 55) return 6;

    /* a wider type */
    *(&tl) = 4242;
    if (tl != 4242) return 7;

    /* array element addresses and pointer arithmetic */
    if (*(&tarr[3]) != 4) return 8;
    *(&tarr[2]) = 33;
    if (tarr[2] != 33) return 9;
    int *q = tarr + 1;
    *q = 20;
    if (tarr[1] != 20) return 10;
    if ((&tarr[3] - &tarr[0]) != 3) return 11;

    /* struct member through a taken address */
    if ((&ts)->b != 6) return 12;
    (&ts)->a = 50;
    if (ts.a != 50) return 13;

    return 0;
}

/* ===== codegen_pointer_and_enum_signedness ===== */
enum Big  { BIG = 0x80000000u };          /* needs unsigned int */
enum Wide { W = 0x100000000 };            /* needs a 64-bit type */
enum Neg  { N = -1, P = 2147483647 };     /* stays plain int     */

static int t_pointer_and_enum_signedness(void) {
    /* ===== pointer comparison (returns 1-9) ===== */
    char buf[8];
    char *lo = buf, *hi = buf + 4;
    if (!(lo < hi)) return 1;
    if (hi < lo) return 2;
    if (!(hi > lo)) return 3;
    if (!(lo <= lo)) return 4;
    if (!(hi >= lo)) return 5;
    if (lo >= hi) return 6;

    /* ===== an unsigned enumeration (returns 10-19) ===== */
    enum Big e = BIG;
    if (e < 0) return 10;                      /* the bug: this was true */
    if ((long long)e != 2147483648LL) return 11;
    if (sizeof(enum Big) != 4) return 12;
    if (BIG < 0) return 13;                    /* the constant was always right */
    if (e != BIG) return 14;

    /* ===== a 64-bit enumeration (returns 20-29) ===== */
    enum Wide w = W;
    if ((long long)w != 4294967296LL) return 20;
    if (sizeof(enum Wide) != 8) return 21;

    /* ===== one holding negatives stays signed (returns 30-39) ===== */
    enum Neg n = N;
    if (n != -1) return 30;
    if (!(n < 0)) return 31;
    if (sizeof(enum Neg) != 4) return 32;

    return 0;
}

/* ===== codegen_static_address_difference ===== */
int ad_a[6];
struct S { int x; double y; char z; };
struct S ad_s;
struct I { int p; int q; };
struct O { int head; struct I in; };
struct O ad_o;
int ad_q;

long  d_elems     = &ad_a[4] - &ad_a[1];        /* elements: 3       */
long  d_decay     = (ad_a + 5) - ad_a;          /* elements: 5       */
long  d_negative  = &ad_a[0] - &ad_a[3];        /* elements: -3      */
long  d_bytes     = (char *)&ad_s.z - (char *)&ad_s.x;
unsigned long d_zero = (unsigned long)&ad_q - (unsigned long)&ad_q;
long  d_nested    = (char *)&ad_o.in.q - (char *)&ad_o.head;
long  d_selfsame  = &ad_a[2] - &ad_a[2];

static int t_static_address_difference(void) {
    if (d_elems    !=  3) return 1;
    if (d_decay    !=  5) return 2;
    if (d_negative != -3) return 3;
    /* offsetof(struct S, z) - offsetof(struct S, x) */
    if (d_bytes    != (long)__builtin_offsetof(struct S, z)) return 4;
    if (d_zero     !=  0) return 5;
    if (d_nested   != (long)__builtin_offsetof(struct O, in)
                    + (long)__builtin_offsetof(struct I, q)) return 6;
    if (d_selfsame !=  0) return 7;
    return 0;
}

/* ===== codegen_const_objects_fold_in_static_initializers ===== */
const int   cf_c = 5;
static const int cf_s = 9;
const long  cf_l = 3;
const double cf_d = 2.5;
const float  cf_f = 1.5f;
const char   cf_ch = 'A';
int cf_arr[10];

int    w_plain   = cf_c;
int    w_arith   = cf_c * 2 + 1;
int    w_negate  = -cf_c;
int    w_cond    = cf_c ? 11 : 22;
int    w_static  = cf_s;
long   w_shift   = cf_l << 4;
double w_double  = cf_d * 2;
float  w_float   = cf_f + 1.0f;
int    w_char    = cf_ch + 1;
int   *w_address = &cf_arr[cf_c - 3];

static int t_const_objects_fold(void) {
    if (w_plain  != 5)  return 1;
    if (w_arith  != 11) return 2;
    if (w_negate != -5) return 3;
    if (w_cond   != 11) return 4;
    if (w_static != 9)  return 5;
    if (w_shift  != 48) return 6;
    if (w_double != 5.0) return 7;
    if (w_float  != 2.5f) return 8;
    if (w_char   != 'A' + 1) return 9;
    if (w_address != &cf_arr[2]) return 10;
    return 0;
}

int main(void) {
    int r;
    if ((r = t_ssa_phi_goto_dispatch()) != 0) return r;
    if ((r = t_ptr_arith_narrow_index()) != 0) return 10 + r;
    if ((r = t_char_literal_signedness()) != 0) return 20 + r;
    if ((r = t_static_local_struct()) != 0) return 40 + r;
    if ((r = t_struct_store_no_rax_clobber()) != 0) return 50 + r;
    if ((r = t_address_of_thread_local()) != 0) return 60 + r;
    if ((r = t_pointer_and_enum_signedness()) != 0) return 80 + r;
    if ((r = t_static_address_difference()) != 0) return 120 + r;
    if ((r = t_const_objects_fold()) != 0) return 130 + r;
    return 0;
}
"#;
    assert_eq!(compile_and_run("symbols_runtime_mega", code, &[]), 0);
}

/// A shared object with a large thread-local block must be `dlopen`-able.
///
/// Initial Exec resolves a thread-local's offset through the GOT at load time,
/// which works for a library present at startup but requires the block to fit
/// in the loader's *static TLS surplus*. A library loaded later with a block
/// bigger than that surplus is rejected outright:
///
/// ```text
/// dlopen failed: ./lib.so: cannot allocate memory in static TLS block
/// ```
///
/// Only a dynamic model removes the limit. The block here is deliberately
/// oversized -- a small one fits the surplus and passes under either model, so
/// it would not test anything.
#[test]
fn codegen_dlopen_a_library_with_a_large_thread_local_block() {
    let lib = r#"
_Thread_local int big[600000];
int bump(void) { return ++big[0]; }
"#;
    let main = r#"
#include <stdio.h>
#include <dlfcn.h>
int main(void)
{
    void *h = dlopen("./lib.so", RTLD_NOW);
    if (!h) { printf("dlopen failed: %s\n", dlerror()); return 1; }
    int (*bump)(void) = (int (*)(void))dlsym(h, "bump");
    if (!bump) { printf("dlsym failed: %s\n", dlerror()); return 2; }
    if (bump() != 1) return 3;
    if (bump() != 2) return 4;
    return 0;
}
"#;
    assert_eq!(compile_and_dlopen("tls_big", lib, main, &[]), 0);
}

/// A pointer comparison is unsigned, and an enumeration takes the signedness
/// of the type wide enough to hold it.
///
/// Both were asked of a predicate that answers about *integer* types: a
/// pointer is not one, so `is_unsigned` said false and `a < b` emitted a
/// signed `setl` where gcc emits `setb` -- C17 6.5.8 compares addresses, and
/// an address is unsigned. And an enumeration never carried a signedness at
/// all, so although `enum_underlying_type` correctly picked `unsigned int`
/// for `enum E { BIG = 0x80000000u }`, an *object* of that type was loaded
/// and compared as signed: `e` read back -2147483648 and `e < 0` was true,
/// while the constant `BIG` was right all along.
#[test]
fn codegen_pointer_and_enum_signedness() {
    let code = r#"
enum Big  { BIG = 0x80000000u };          /* needs unsigned int */
enum Wide { W = 0x100000000 };            /* needs a 64-bit type */
enum Neg  { N = -1, P = 2147483647 };     /* stays plain int     */

int main(void) {
    /* ===== pointer comparison (returns 1-9) ===== */
    char buf[8];
    char *lo = buf, *hi = buf + 4;
    if (!(lo < hi)) return 1;
    if (hi < lo) return 2;
    if (!(hi > lo)) return 3;
    if (!(lo <= lo)) return 4;
    if (!(hi >= lo)) return 5;
    if (lo >= hi) return 6;

    /* ===== an unsigned enumeration (returns 10-19) ===== */
    enum Big e = BIG;
    if (e < 0) return 10;                      /* the bug: this was true */
    if ((long long)e != 2147483648LL) return 11;
    if (sizeof(enum Big) != 4) return 12;
    if (BIG < 0) return 13;                    /* the constant was always right */
    if (e != BIG) return 14;

    /* ===== a 64-bit enumeration (returns 20-29) ===== */
    enum Wide w = W;
    if ((long long)w != 4294967296LL) return 20;
    if (sizeof(enum Wide) != 8) return 21;

    /* ===== one holding negatives stays signed (returns 30-39) ===== */
    enum Neg n = N;
    if (n != -1) return 30;
    if (!(n < 0)) return 31;
    if (sizeof(enum Neg) != 4) return 32;

    return 0;
}
"#;
    // The default-matrix run of this program is a section of
    // `codegen_symbols_runtime_mega`; this is its -O1 run.
    assert_eq!(
        compile_and_run_optimized("ptr_enum_signedness_opt", code),
        0
    );
}

/// The prune must reach a fixpoint: a reference held only by a function that
/// is itself dead is not a reference.
///
/// `helper` is called only from `caller`, which nothing calls. One round drops
/// `caller` and leaves `helper` alive on a count its own removal invalidated --
/// and `helper` names a symbol that is never defined, so the program fails to
/// link rather than merely carrying dead code.
#[test]
fn codegen_dead_static_chain_is_pruned_to_a_fixpoint() {
    let code = r#"
extern int never_defined;
static int helper(void) { return never_defined; }
static int caller(void) { return helper(); }
int main(void) { return 0; }
"#;
    for opt in ["-O1", "-O2"] {
        assert_eq!(
            compile_and_run("c17_dead_static_chain", code, &[opt.to_string()]),
            0,
            "at {opt}"
        );
    }
}

/// A parameter or local spelled like a static function is not a reference to
/// it, even when its address is taken.
///
/// `&helper` below takes the parameter's address. The prune counted it as an
/// address of the function `helper`, kept that function, and the program
/// failed to link on the symbol `helper` names but nothing defines.
#[test]
fn codegen_object_spelled_like_a_dead_static_does_not_keep_it() {
    let code = r#"
extern int never_defined(void);
static int helper(void) { return never_defined(); }
void sink(int *p) { *p += 1; }
int by_param(int helper) { sink(&helper); return helper; }
int by_local(void) { int helper = 1; __asm__("" : : "m"(helper)); return helper + 1; }
int main(void) { return by_param(41) + by_local() - 44; }
"#;
    for opt in ["-O1", "-O2"] {
        assert_eq!(
            compile_and_run("c17_shadowed_dead_static", code, &[opt.to_string()]),
            0,
            "at {opt}"
        );
    }
}

/// `&*x` is `x`: the pair cancels, and no object is read, so it is a static
/// address wherever `x` is one.
///
/// The `Deref` arm has to go through `static_address_operand` rather than
/// recurse into the ordinary walk: that walk answers a bare identifier with
/// the address *of* the object, because every other caller has already seen
/// an `&`, so `&*p` for a pointer variable would fold to the address of `p`
/// instead of its value.
#[test]
fn codegen_address_of_dereference_is_a_static_address() {
    let code = r#"
extern void abort(void);

int a[4] = {10, 11, 12, 13};
int v = 99;
struct T { int x, y; } t = {1, 2};

int *p1 = &*(a + 2);
int *p2 = &*&v;
int *p3 = &*&t.y;
char *p4 = (char *)&(*(&"ZYX"[2]));
int *p5 = &*(a + 1) + 1;

int main(void)
{
    if (*p1 != 12) abort();
    if (*p2 != 99) abort();
    if (*p3 != 2) abort();
    if (*p4 != 'X') abort();
    if (*p5 != 12) abort();
    return 0;
}
"#;
    for opt in ["-O0", "-O1", "-O2"] {
        assert_eq!(
            compile_and_run("c17_addr_of_deref", code, &[opt.to_string()]),
            0,
            "at {opt}"
        );
    }
}

/// A function named like one of the backends' own labels.
///
/// Block labels are `.L<function>_<n>`, and the backends' internal labels --
/// an int128 shift's branches, a `va_arg` overflow path, a CAS loop, the
/// constant pools -- were `.L<prefix>_<n>` in the same namespace. So a
/// function named `i128`, `va_done` or `cas_loop` defined one of its blocks
/// under the name of a label the backend had also made, and the assembler
/// rejected the output ("symbol `.Li128_0' is already defined"). Internal
/// labels are `.L.<prefix>.<n>` now, which no C identifier can spell. The
/// function body has enough blocks for a low-numbered internal label to meet
/// one, and every construct that makes internal labels on either target; the
/// expected value is gcc's.
const INTERNAL_LABEL_NAMED: &str = r#"
#include <stdarg.h>
#define NI __attribute__((noinline))
struct P { long a, b; };
_Atomic double ad;
static int word = 1;
/* A function named like one of the backend's own labels, with enough blocks
   that a low-numbered internal label meets one of its block labels, and every
   construct that emits such labels on either target. */
NI long fname(long a, int s, int n, ...)
{
    long acc = 0;
    if (a > 0) acc += 0;
    if (a > 1) acc += 1;
    if (a > 2) acc += 2;
    if (a > 3) acc += 3;
    if (a > 4) acc += 4;
    if (a > 5) acc += 0;
    if (a > 6) acc += 1;
    if (a > 7) acc += 2;
    if (a > 8) acc += 3;
    if (a > 9) acc += 4;
    if (a > 10) acc += 0;
    if (a > 11) acc += 1;
    if (a > 12) acc += 2;
    if (a > 13) acc += 3;
    if (a > 14) acc += 4;
    if (a > 15) acc += 0;
    if (a > 16) acc += 1;
    if (a > 17) acc += 2;
    if (a > 18) acc += 3;
    if (a > 19) acc += 4;
    if (a > 20) acc += 0;
    if (a > 21) acc += 1;
    if (a > 22) acc += 2;
    if (a > 23) acc += 3;
    if (a > 24) acc += 4;
    if (a > 25) acc += 0;
    if (a > 26) acc += 1;
    if (a > 27) acc += 2;
    if (a > 28) acc += 3;
    if (a > 29) acc += 4;
    if (a > 30) acc += 0;
    if (a > 31) acc += 1;
    if (a > 32) acc += 2;
    if (a > 33) acc += 3;
    if (a > 34) acc += 4;
    if (a > 35) acc += 0;
    if (a > 36) acc += 1;
    if (a > 37) acc += 2;
    if (a > 38) acc += 3;
    if (a > 39) acc += 4;
    __int128 x = (__int128)a << s;
    __int128 y = x >> (s + 1);
    unsigned __int128 z = (unsigned __int128)x >> s;
    acc += (long)(x >> 64) + (long)(y >> 64) + (long)(z >> 64) + (long)z;
    va_list ap;
    va_start(ap, n);
    for (int i = 0; i < n; i++)
        acc += va_arg(ap, long);
    acc += (long)va_arg(ap, double);
    struct P p = va_arg(ap, struct P);
    acc += p.a + p.b;
    va_end(ap);
    acc += __sync_val_compare_and_swap(&word, 1, 2);
    acc += __atomic_fetch_and(&word, 3, __ATOMIC_SEQ_CST);
    acc += __atomic_exchange_n(&word, 7, __ATOMIC_SEQ_CST);
    ad += 1.5;
    ad -= 0.5;
    acc += (long)ad;
    double d = a > 3 ? 0.0 : 2.5;
    acc += (long)(d * 2);
    long double ld = 0.0L + (long double)a;
    acc += (long)ld;
    volatile char big[4096];
    big[0] = 1;
    acc += big[0];
    return acc;
}

int main(void)
{
    struct P p = {3, 4};
    long r = fname(5, 70, 2, 10L, 20L, 6.0, p);
    __builtin_printf("%ld\n", r);
    return 0;
}
"#;

#[test]
fn codegen_function_named_like_an_internal_label() {
    let prefixes = [
        "i128",
        "zero_frame",
        "cas_loop",
        "cas_fail",
        "swap_loop",
        "fadd_loop",
        "fsub_loop",
        "va_overflow",
        "va_done",
        "va_fp_overflow",
        "va_fp_done",
        "va_agg_overflow",
        "va_agg_done",
        "sel_then",
        "sel_done",
        "cas_done",
        "atomic_bitop",
        "i128shl_ge64",
        "i128shl_done",
        "i128lsr_ge64",
        "i128lsr_done",
        "i128asr_ge64",
        "i128asr_done",
        "dbl_const",
        "quad_const",
        "ld_const",
    ];
    for name in prefixes {
        compile_and_run_everywhere(name, &INTERNAL_LABEL_NAMED.replace("fname", name));
    }
}

// Consolidates, one C program run everywhere with each original `main` as a
// `t_<name>()` section (doc comments and exit-code ranges are in the program):
//   codegen_label_address_inside_switch
//   codegen_label_address_in_every_statement_context
//   codegen_backward_goto_in_switch_releases_vla
//   codegen_stmt_expr_labeled_last_statement
#[test]
fn codegen_label_addresses_mega() {
    let src = r#"/*
 * Consolidates the symbols.rs label-address programs run everywhere. Each
 * original main is a t_<test>() function; main returns base + its code.
 *
 *   codegen_label_address_inside_switch            1..3
 *   codegen_label_address_in_every_statement_context  11..13
 *   codegen_backward_goto_in_switch_releases_vla   21
 *   codegen_stmt_expr_labeled_last_statement       31..35
 */

/* ===== codegen_label_address_inside_switch =====
 * `&&label` finds a label wherever it is in the function, including one that
 * sits between the case labels of a `switch` body; `goto` always did.
 * gcc.c-torture's `pr21356`. */
int a;
void *p;
int trail;

void step(void)
{
    switch (a) {
    a0: case 0: p = &&a1; trail = trail * 10 + 1; break;
    a1: case 1: p = &&a2; trail = trail * 10 + 2; break;
    a2: default: p = &&a0; trail = trail * 10 + 3; break;
    }
}

int walk(int start)
{
    int hops = 0;
    void *next;
    switch (start) {
    case 0:
    x: next = &&y; hops++; goto *next;
    case 1:
    y: hops++; if (hops < 4) { next = &&x; goto *next; }
    }
    return hops;
}

static int t_label_address_inside_switch(void)
{
    step();
    if (p == 0) return 1;
    a = 1; step();
    a = 2; step();
    if (trail != 123) return 2;
    if (walk(0) != 4) return 3;
    return 0;
}

/* ===== codegen_label_address_in_every_statement_context =====
 * `&&label` finds labels in every other statement context too: after
 * `default:`, in an `if`/`else` arm after a case label, in loops, nested
 * blocks and statement expressions, from code and from a static initializer. */
int ec_a;

static int t_label_address_every_context(void)
{
    int n = 0, r = 0;
    void *p = 0;
    switch (ec_a) {
    default: d1: r += 1; p = &&d1;
    case 5: if (ec_a) { t1: r += 10; } else e1: r += 100;
    }
    for (int i = 0; i < 2; i++) { l1: n++; }
    { { b1: n++; } }
    while (n < 4) { w1: n++; }
    int s = ({ int q = 0; se: q = 7; q; });
    void *t[] = { p, &&t1, &&e1, &&l1, &&b1, &&w1, &&se };
    static void *st[] = { &&d1, &&e1 };
    for (int i = 0; i < 7; i++)
        if (!t[i]) return 1;
    if (st[0] != p || st[1] != t[2]) return 2;
    if (r != 101 || s != 7 || n != 4) return 3;
    return 0;
}

/* ===== codegen_backward_goto_in_switch_releases_vla =====
 * A backward `goto` to a label inside a `switch` body releases the VLA
 * declared after the label. A VLA under a case label did not count as the
 * function declaring one, and the switch-body walk never recorded the
 * label's stack depth, so every trip round grew the stack until the program
 * died. */
static int t_backward_goto_in_switch_vla(int argc, char **argv)
{
    int n = 4096, c = 0;
    (void)argv;
    switch (argc) {
    case 1:
    L: {
        volatile char v[n];
        v[0] = 1;
        c++;
        if (c < 100000) goto L;
    }
    }
    return c == 100000 ? 0 : 1;
}

/* ===== codegen_stmt_expr_labeled_last_statement =====
 * A statement expression ending in a labeled expression statement takes that
 * expression's value, as gcc does -- it was typed `void`, so every use drew
 * "void value not ignored as it ought to be" (compile/pr17913). A constant
 * `?:` also has to keep an arm that defines a label, since a computed `goto`
 * can still reach it; dropping the arm dropped the label. */
int f(int k) {
    void *p = &&a;
    int v = k ? 1 : ({ a: 7; });
    if (k)
        return v;
    goto *p;
}
static int g(void) { return ({ x: 5; }); }
static int h(void) { return ({ int t = 3; x: y: t + 4; }); }
static int loop(void) {
    int s = ({ int i = 0, t = 0; again: t += i; if (++i < 4) goto again; lab: t * 2; });
    return s;
}
static int folded(int k) {
    void *p = k ? &&a : &&b;
    int v = 1 ? 3 : ({ a: 9; });
    int w = 5 ?: ({ b: 6; });
    if (k)
        return v + w;
    goto *p;
}
static int t_stmt_expr_label(void) {
    if (f(1) != 1) return 1;
    if (g() != 5) return 2;
    if (h() != 7) return 3;
    if (loop() != 12) return 4;
    if (folded(1) != 8) return 5;
    return 0;
}

int main(int argc, char **argv)
{
    int r;
    if ((r = t_label_address_inside_switch()) != 0) return r;
    if ((r = t_label_address_every_context()) != 0) return 10 + r;
    if ((r = t_backward_goto_in_switch_vla(argc, argv)) != 0) return 20 + r;
    if ((r = t_stmt_expr_label()) != 0) return 30 + r;
    return 0;
}
"#;
    compile_and_run_everywhere("label_addresses_mega", src);
}

pub(super) const FCMP_FOLD_LINK: &str = r#"
/* Every `link_error` here is behind a comparison that is false for every
   operand value, NaN included; the program links only when the optimizer
   proves it. */
extern void link_error(void);

const double dnan = 1.0 / 0.0 - 1.0 / 0.0;
double gx = 1.0;

__attribute__((noinline)) static int nan_side(float x)
{
    if (dnan == dnan) link_error();
    if (dnan != gx) gx = 1.0; else link_error();
    if (dnan < gx || dnan > gx || dnan <= gx || dnan >= gx || dnan == gx)
        link_error();
    if (gx < dnan || gx > dnan || gx <= dnan || gx >= dnan || gx == dnan)
        link_error();
    if (x == __builtin_nanf("") || !(x != __builtin_nanf("")))
        link_error();
    return 0;
}

__attribute__((noinline)) static int inf_side(double x, float y)
{
    if (x > __builtin_inf()) link_error();
    if (__builtin_inf() < x) link_error();
    if (x < -__builtin_inf()) link_error();
    if (-__builtin_inf() > x) link_error();
    if (y > __builtin_inff()) link_error();
    if (x < x || x > x) link_error();
    return 0;
}

__attribute__((noinline)) static int pairs(float x, float y)
{
    if ((x == y) && (x != y)) link_error();
    if ((x < y) && (x > y)) link_error();
    if ((x < y) && (y < x)) link_error();
    if ((x == y) || (x != y)) {} else link_error();
    if (__builtin_isunordered(x, y) || (x >= y) || (x < y)) {} else link_error();
    if (__builtin_isunordered(y, x) || (x <= y) || (y < x)) {} else link_error();
    if (__builtin_isunordered(x, y) || !__builtin_isunordered(x, y)) {} else link_error();
    return 0;
}

int main(void)
{
    static const float fv[] = { 0.0f, 1.0f, -0.0f, __builtin_inff(), __builtin_nanf("") };
    int r = nan_side(1.0f) + inf_side(2.0, fv[3]);
    for (int i = 0; i < 5; i++)
        for (int j = 0; j < 5; j++)
            r += pairs(fv[i], fv[j]);
    return r;
}
"#;

/// A `static _Thread_local` local is thread-local whatever its type. The
/// block-scope path asked the declarator's *type* for `_Thread_local`, and a
/// structure's type is its tag's, which never carries a storage class -- so
/// `static _Thread_local struct S s;` became an ordinary static, one object
/// shared by every thread.
#[test]
fn codegen_static_thread_local_struct_local_is_per_thread() {
    let src = r#"
#include <pthread.h>

struct S { int a; };

static int *mine(void)
{
    static _Thread_local struct S s;
    return &s.a;
}

/* Compare values, not addresses: once a thread exits its copy is freed, and
   on Darwin, where thread-local storage is allocated on first use, main's
   copy can then land at the same address. */
static void *other(void *arg)
{
    (void)arg;
    int *p = mine();
    if (*p != 0)
        return (void *)1;       /* a fresh copy starts at zero */
    *p = 2;
    return (void *)(long)*mine();
}

int main(void)
{
    pthread_t t;
    void *theirs;
    *mine() = 1;
    if (pthread_create(&t, 0, other, 0) != 0)
        return 1;
    if (pthread_join(t, &theirs) != 0)
        return 2;
    if (theirs != (void *)2)
        return 3;               /* the thread did not keep its own write */
    return *mine() == 1 ? 0 : 4;  /* main's copy is untouched */
}
"#;
    // The host only: `<pthread.h>` is a host header.
    for opts in [vec![], vec!["-O2".to_string()]] {
        assert_eq!(
            compile_and_run("static_thread_local_struct_local", src, &opts),
            0,
            "{opts:?}"
        );
    }
}

/// `#pragma weak name` makes `name` a weak symbol, as the System V and
/// gcc compilers do: a reference that no definition satisfies is a null
/// address rather than a link error, and a definition yields to a strong one
/// elsewhere. The pragma may come before or after the declaration.
///
/// binutils' libctf writes `#pragma weak ctf_open` so that its BFD-free
/// shared library can refer to `ctf_open` without defining it; c17 ignored
/// the pragma, so every program linked against libctf-nobfd.so (readelf
/// first) failed with "undefined reference to `ctf_open'".
#[test]
fn codegen_pragma_weak() {
    let unit_a = r#"
#pragma weak absent_later
int absent_before(void);
int absent_later(void);
#pragma weak absent_before
#pragma weak hook
int hook(void) { return 1; }

int main(void)
{
    if (&absent_before != 0) return 1;
    if (&absent_later != 0) return 2;
    if (hook() != 2) return 3;
    return 0;
}
"#;
    let unit_b = "int hook(void) { return 2; }\n";
    for opts in [vec![], vec!["-O2".to_string()]] {
        assert_eq!(
            compile_and_run_two_units("pragma_weak", unit_a, unit_b, &opts),
            0,
            "{opts:?}"
        );
    }
}
