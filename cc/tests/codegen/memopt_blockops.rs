//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// End-to-end tests for block operations -- a `memset`, `memcpy` or `memmove`
// too long for `memexpand` -- in the memory analyses.
//
// A block op dereferences its pointers and keeps none of them, so a local it
// touches has not escaped, and a function whose only writes are block ops on
// its own locals writes nothing its caller can see. The first two tests
// prove those answers are used, by a call to an undefined `link_error` that
// only a forwarded load deletes. Everything after them is a program whose
// answer changes if the analyses believe one thing too many.
//

use crate::codegen::asm_probe::{asm_for_with, assert_body_lacks, AARCH64_LINUX, X86_64_LINUX};
use crate::common::compile_and_run;

fn at_o2(name: &str, code: &str) -> i32 {
    compile_and_run(name, code, &["-O2".to_string()])
}

fn at_o2_no_inline(name: &str, code: &str) -> i32 {
    compile_and_run(name, code, &["-O2".to_string(), "-fno-inline".to_string()])
}

/// Assert that `main` of `src`, compiled at -O2 without inlining for both
/// targets, no longer calls `link_error`.
fn assert_link_error_folded(name: &str, src: &str, why: &str) {
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for_with(name, triple, src, &["-O2", "-fno-inline"]);
        assert_body_lacks(&asm, "main", "link_error", why);
    }
}

/// A `static` function whose only write is a `memset` of its own buffer
/// writes nothing its caller can observe, so a global the caller stored
/// survives the call to it -- exactly as it would if the buffer were filled
/// by stores.
#[test]
fn memopt_blockops_a_memset_of_a_private_buffer_is_no_write() {
    let src = r#"
extern void *memset(void *, int, unsigned long);
extern void *memcpy(void *, const void *, unsigned long);
extern void *memmove(void *, const void *, unsigned long);
extern void abort(void);
extern void link_error(void);
int g;

static int fill(int n) {
    char buf[256];
    memset(buf, n, sizeof buf);
    return buf[n & 255];
}

int main(void) {
    g = 5;
    int r = fill(3);
    if (g != 5) link_error();
    return r - 3;
}
"#;
    assert_link_error_folded(
        "blockops_private_memset",
        src,
        "fill writes only its own buffer, so g is still 5",
    );
}

/// A local whose address went only to a `memcpy` has not escaped, so a call
/// that was never given it cannot change it.
#[test]
fn memopt_blockops_a_memcpy_source_has_not_escaped() {
    let src = r#"
extern void *memset(void *, int, unsigned long);
extern void *memcpy(void *, const void *, unsigned long);
extern void *memmove(void *, const void *, unsigned long);
extern void abort(void);
extern void link_error(void);
extern void opaque(char *);

int main(void) {
    char buf[200];
    char out[200];
    buf[0] = 7;
    memcpy(out, buf, sizeof out);
    opaque(out);
    if (buf[0] != 7) link_error();
    return 0;
}
"#;
    assert_link_error_folded(
        "blockops_memcpy_source",
        src,
        "opaque was given out, never buf",
    );
}

/// The two folds above, run: the programs they compile must still be right.
#[test]
fn memopt_blockops_the_folds_still_compute_the_right_answer() {
    let code = r#"
extern void *memset(void *, int, unsigned long);
extern void *memcpy(void *, const void *, unsigned long);
extern void *memmove(void *, const void *, unsigned long);
extern void abort(void);
int g;
static int fill(int n) {
    char buf[256];
    memset(buf, n, sizeof buf);
    return buf[n & 255];
}
static char seen;
void opaque(char *p) { seen = p[0]; p[0] = 1; }

int main(void) {
    g = 5;
    if (fill(3) != 3) abort();
    if (g != 5) abort();

    char buf[200];
    char out[200];
    buf[0] = 7;
    memcpy(out, buf, sizeof out);
    opaque(out);
    if (buf[0] != 7 || seen != 7 || out[0] != 1) abort();
    return 0;
}
"#;
    assert_eq!(at_o2("blockops_folds_run", code), 0);
    assert_eq!(at_o2_no_inline("blockops_folds_run_ni", code), 0);
}

/// A block op through a pointer that may be into the object writes the
/// object: here it *is* the object, at an offset only known at run time.
#[test]
fn memopt_blockops_a_block_op_through_an_aliasing_pointer_writes() {
    let code = r#"
extern void *memset(void *, int, unsigned long);
extern void *memcpy(void *, const void *, unsigned long);
extern void *memmove(void *, const void *, unsigned long);
extern void abort(void);
int zero(void) { return 0; }

int main(void) {
    char buf[200];
    buf[0] = 1;
    buf[150] = 2;
    char *p = buf + zero();
    memset(p, 9, 160);
    if (buf[0] != 9 || buf[150] != 9) abort();

    char src[200];
    src[0] = 4;
    buf[10] = 3;
    memcpy(p + 10, src, 180);
    if (buf[10] != 4) abort();
    return 0;
}
"#;
    assert_eq!(at_o2("blockops_alias_ptr", code), 0);
    assert_eq!(at_o2_no_inline("blockops_alias_ptr_ni", code), 0);
}

/// A local filled by `memcpy` whose address escapes *afterwards* is a
/// captured local, and a call may write it from then on.
#[test]
fn memopt_blockops_a_memcpy_target_that_escapes_later() {
    let code = r#"
extern void *memset(void *, int, unsigned long);
extern void *memcpy(void *, const void *, unsigned long);
extern void *memmove(void *, const void *, unsigned long);
extern void abort(void);
static int *kept;
void keep(int *p) { kept = p; }
void poke(void) { kept[0] = 42; }

int main(void) {
    int src[64];
    int a[64];
    src[0] = 1;
    memcpy(a, src, sizeof a);
    if (a[0] != 1) abort();
    keep(a);
    a[0] = 2;
    poke();
    if (a[0] != 42) abort();
    return 0;
}
"#;
    assert_eq!(at_o2("blockops_escape_later", code), 0);
    assert_eq!(at_o2_no_inline("blockops_escape_later_ni", code), 0);
}

/// `memcpy` returns its destination, so the address leaves through the
/// result as surely as through a direct argument.
#[test]
fn memopt_blockops_the_returned_destination_can_escape() {
    let code = r#"
extern void *memset(void *, int, unsigned long);
extern void *memcpy(void *, const void *, unsigned long);
extern void *memmove(void *, const void *, unsigned long);
extern void abort(void);
static char *kept;
void keep(char *p) { kept = p; }
void poke(void) { kept[0] = 42; }

int main(void) {
    char src[200];
    char buf[200];
    src[0] = 1;
    keep(memcpy(buf, src, sizeof buf));
    buf[0] = 2;
    poke();
    if (buf[0] != 42) abort();

    char other[200];
    char *q = memset(other, 0, sizeof other);
    other[5] = 3;
    q[5] = 8;
    if (other[5] != 8) abort();
    return 0;
}
"#;
    assert_eq!(at_o2("blockops_returned_dest", code), 0);
    assert_eq!(at_o2_no_inline("blockops_returned_dest_ni", code), 0);
}

/// An overlapping `memmove` within one local reads what the stores before it
/// wrote, and writes what the loads after it read: neither the stores nor
/// the loads may be resolved past it.
#[test]
fn memopt_blockops_an_overlapping_memmove() {
    let code = r#"
extern void *memset(void *, int, unsigned long);
extern void *memcpy(void *, const void *, unsigned long);
extern void *memmove(void *, const void *, unsigned long);
extern void abort(void);
int main(void) {
    char buf[200];
    for (int i = 0; i < 200; i++) buf[i] = (char)i;
    buf[0] = 100;
    memmove(buf + 1, buf, 150);
    if (buf[1] != 100 || buf[2] != 1 || buf[150] != (char)149 || buf[151] != (char)151) abort();
    buf[3] = 50;
    memmove(buf, buf + 3, 150);
    if (buf[0] != 50) abort();
    return 0;
}
"#;
    assert_eq!(at_o2("blockops_memmove_overlap", code), 0);
    assert_eq!(at_o2_no_inline("blockops_memmove_overlap_ni", code), 0);
}

/// A store a block op reads is live, and a store a block op overwrites is
/// not the value read afterwards: dse and forwarding both see the op.
#[test]
fn memopt_blockops_a_block_op_reads_and_writes_a_private_local() {
    let code = r#"
extern void *memset(void *, int, unsigned long);
extern void *memcpy(void *, const void *, unsigned long);
extern void *memmove(void *, const void *, unsigned long);
extern void abort(void);
extern void opaque(void);
void opaque(void) {}

int main(void) {
    char buf[200];
    char out[200];
    buf[0] = 5;
    memcpy(out, buf, sizeof out);   /* reads the 5 */
    buf[0] = 6;
    opaque();
    if (out[0] != 5 || buf[0] != 6) abort();

    buf[1] = 7;
    memset(buf, 0, sizeof buf);     /* overwrites the 7 */
    opaque();
    if (buf[1] != 0) abort();
    return 0;
}
"#;
    assert_eq!(at_o2("blockops_private_rw", code), 0);
    assert_eq!(at_o2_no_inline("blockops_private_rw_ni", code), 0);
}

/// The effect a block op has is only `Const` when every byte it touches is
/// the callee's own: one that fills a global, writes through a pointer it
/// was given, or reads a global into a private buffer still disturbs the
/// caller's view of memory.
#[test]
fn memopt_blockops_a_block_op_on_visible_memory_is_a_write() {
    let code = r#"
extern void *memset(void *, int, unsigned long);
extern void *memcpy(void *, const void *, unsigned long);
extern void *memmove(void *, const void *, unsigned long);
extern void abort(void);
int g;
char table[200];

static void fill_global(int n) { memset(table, n, sizeof table); }
static void fill_through(char *p, int n) { memset(p, n, 200); }
static void copy_into(char *p) { char buf[200]; memset(buf, 4, sizeof buf); memcpy(p, buf, sizeof buf); }
static int read_global(void) { char buf[200]; memcpy(buf, table, sizeof buf); return buf[0]; }

int main(void) {
    table[0] = 1;
    fill_global(2);
    if (table[0] != 2) abort();

    table[0] = 1;
    fill_through(table, 3);
    if (table[0] != 3) abort();

    table[0] = 1;
    copy_into(table);
    if (table[0] != 4) abort();

    table[0] = 9;
    g = 1;
    if (read_global() != 9 || g != 1) abort();
    table[0] = 10;
    if (read_global() != 10) abort();
    return 0;
}
"#;
    assert_eq!(at_o2("blockops_visible_write", code), 0);
    assert_eq!(at_o2_no_inline("blockops_visible_write_ni", code), 0);
}

/// Two globals of this unit are two objects to `memmove` as to everything
/// else, and one global is one object: a move within it stays a move.
#[test]
fn memopt_blockops_a_move_within_one_global_stays_a_move() {
    let code = r#"
extern void *memset(void *, int, unsigned long);
extern void *memcpy(void *, const void *, unsigned long);
extern void *memmove(void *, const void *, unsigned long);
extern void abort(void);
char a[200], b[200];
int zero(void) { return 0; }

int main(void) {
    for (int i = 0; i < 200; i++) { a[i] = (char)i; b[i] = (char)(i + 1); }
    memmove(a, b, 150);
    if (a[0] != 1 || a[149] != (char)150) abort();
    memmove(a + 1 + zero(), a, 150);
    if (a[1] != 1 || a[2] != 2 || a[150] != (char)150) abort();
    return 0;
}
"#;
    assert_eq!(at_o2("blockops_global_move", code), 0);
    assert_eq!(at_o2_no_inline("blockops_global_move_ni", code), 0);
}
