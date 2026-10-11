//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// x86-64's named address spaces at run time. On Linux `%fs` is the thread
// pointer and a process's `%gs` base is zero, so a `__seg_fs` access
// reaches the thread's own block -- glibc's `THREAD_SELF` reads the TCB's
// self pointer at `%fs:0x10` -- and a `__seg_gs` one reaches the same
// address a plain access would.
//

use crate::common::compile_and_run;

#[test]
fn named_address_spaces_reach_the_segment() {
    let src = r#"
#include <stddef.h>
#include <stdint.h>

static __thread int tv = 42;
static __thread long tl[4] = { 1, 2, 3, 4 };
int plain = 7;
struct pair { int a; short b; double d; };
struct pair pr = { 3, 4, 2.5 };

__attribute__((noinline)) static char *tcb(void) {
    /* The thread control block's first word points at itself. */
    return *(char *__seg_fs *)0;
}

__attribute__((noinline)) static int rd_fs(long off) { return *(int __seg_fs *)off; }
__attribute__((noinline)) static void wr_fs(long off, int v) { *(int __seg_fs *)off = v; }
__attribute__((noinline)) static long idx_fs(long __seg_fs *p, int i) { return p[i]; }
__attribute__((noinline)) static int rd_gs(int *p) { return *(int __seg_gs *)(uintptr_t)p; }
__attribute__((noinline)) static void add_gs(int __seg_gs *p, int v) { *p += v; }
__attribute__((noinline)) static short rd_member(struct pair __seg_gs *p) { return p->b; }
__attribute__((noinline)) static double rd_double(struct pair __seg_gs *p) { return p->d; }
__attribute__((noinline)) static void wr_double(double __seg_gs *p, double v) { *p = v; }
__attribute__((noinline)) static unsigned char rd_byte(unsigned char __seg_gs *p, int i) { return p[i]; }

/* Through %fs, whose base is not zero, so an access that misses the
   segment reads the wrong memory: a struct copied in and out past the
   size a block copy becomes a call at, a bit-field, and asm operands. */
struct big { long a[20]; };
struct bits { unsigned lo : 3, mid : 9, hi : 20; };
static __thread struct big tbig;
static __thread struct bits tbits;
static __thread int tcount;

__attribute__((noinline)) static struct big get_big(long off) {
    return *(struct big __seg_fs *)off;
}
__attribute__((noinline)) static void put_big(long off, struct big b) {
    *(struct big __seg_fs *)off = b;
}
__attribute__((noinline)) static unsigned get_mid(long off) {
    return ((struct bits __seg_fs *)off)->mid;
}
__attribute__((noinline)) static void set_mid(long off, unsigned v) {
    ((struct bits __seg_fs *)off)->mid = v;
}
__attribute__((noinline)) static void asm_add(long off, int v) {
    __asm__("addl %1, %0" : "+m"(*(int __seg_fs *)off) : "r"(v));
}
__attribute__((noinline)) static int asm_read(long off) {
    int v;
    __asm__("movl %1, %0" : "=r"(v) : "m"(*(int __seg_fs *)off));
    return v;
}
__attribute__((noinline)) static int volatile_read(long off) {
    return *(volatile int __seg_fs *)off;
}

/* Values held in storage -- complex, vector, a small struct -- read and
   written through the segment, returned and passed on. */
typedef int v4 __attribute__((vector_size(16)));
struct s12 { int a, b, c; };
static __thread _Complex double tcx;
static __thread v4 tvec;
static __thread struct s12 t12;
__attribute__((noinline)) static _Complex double get_cx(long off) {
    return *(_Complex double __seg_fs *)off;
}
__attribute__((noinline)) static void put_cx(long off, _Complex double v) {
    *(_Complex double __seg_fs *)off = v;
}
__attribute__((noinline)) static double cx_twice(long off) {
    _Complex double z = *(_Complex double __seg_fs *)off * 2.0;
    return __real__ z + __imag__ z;
}
__attribute__((noinline)) static v4 get_vec(long off) { return *(v4 __seg_fs *)off; }
__attribute__((noinline)) static void put_vec(long off, v4 v) { *(v4 __seg_fs *)off = v; }
__attribute__((noinline)) static struct s12 get12(long off) {
    return *(struct s12 __seg_fs *)off;
}
__attribute__((noinline)) static int sum12(struct s12 s) { return s.a + s.b + s.c; }
__attribute__((noinline)) static int arg12(long off) {
    return sum12(*(struct s12 __seg_fs *)off);
}

static int segment_values(char *self) {
    long off = (char *)&tcx - self;
    tcx = 1.5 + 2.5 * __builtin_complex(0.0, 1.0);
    _Complex double z = get_cx(off);
    if (__real__ z != 1.5 || __imag__ z != 2.5) return 50;
    if (cx_twice(off) != 8.0) return 51;
    put_cx(off, 3.0);
    if (__real__ tcx != 3.0 || __imag__ tcx != 0.0) return 52;
    off = (char *)&tvec - self;
    tvec = (v4){ 1, 2, 3, 4 };
    v4 v = get_vec(off);
    if (v[0] != 1 || v[3] != 4) return 53;
    put_vec(off, v * 2);
    if (tvec[1] != 4 || tvec[2] != 6) return 54;
    off = (char *)&t12 - self;
    t12 = (struct s12){ 7, 8, 9 };
    struct s12 s = get12(off);
    if (s.a != 7 || s.c != 9 || arg12(off) != 24) return 55;
    return 0;
}

static int segment_aggregates(char *self) {
    for (int i = 0; i < 20; i++) tbig.a[i] = i * 3;
    long boff = (char *)&tbig - self;
    struct big b = get_big(boff);
    for (int i = 0; i < 20; i++) if (b.a[i] != i * 3) return 20 + i;
    for (int i = 0; i < 20; i++) b.a[i] = -i;
    put_big(boff, b);
    for (int i = 0; i < 20; i++) if (tbig.a[i] != -i) return 40;
    tbits.lo = 5; tbits.mid = 300; tbits.hi = 77;
    long foff = (char *)&tbits - self;
    if (get_mid(foff) != 300) return 41;
    set_mid(foff, 123);
    if (tbits.lo != 5 || tbits.mid != 123 || tbits.hi != 77) return 42;
    long coff = (char *)&tcount - self;
    tcount = 10;
    asm_add(coff, 5);
    if (tcount != 15 || asm_read(coff) != 15 || volatile_read(coff) != 15) return 43;
    return segment_values(self);
}

int main(void) {
    char *self = tcb();
    if (self != (char *)__builtin_thread_pointer()) return 1;
    long off = (char *)&tv - self;
    if (rd_fs(off) != 42) return 2;
    wr_fs(off, 43);
    if (tv != 43) return 3;
    long loff = (char *)&tl[0] - self;
    if (idx_fs((long __seg_fs *)loff, 2) != 3) return 4;
    if (rd_gs(&plain) != 7) return 5;
    add_gs((int __seg_gs *)(uintptr_t)&plain, 5);
    if (plain != 12) return 6;
    struct pair __seg_gs *gp = (struct pair __seg_gs *)(uintptr_t)&pr;
    if (rd_member(gp) != 4 || rd_double(gp) != 2.5) return 7;
    wr_double((double __seg_gs *)(uintptr_t)&pr.d, 6.25);
    if (pr.d != 6.25) return 8;
    unsigned char bytes[4] = { 9, 8, 7, 6 };
    if (rd_byte((unsigned char __seg_gs *)(uintptr_t)bytes, 2) != 7) return 9;
    return segment_aggregates(self);
}
"#;
    for opt in ["-O0", "-O1", "-O2"] {
        assert_eq!(
            compile_and_run("named_address_spaces", src, &[opt.to_string()]),
            0,
            "at {opt}"
        );
    }
}

/// The Linux kernel's Kconfig probe, `CC_HAS_NAMED_AS`, and the predefined
/// macros that tell glibc's and the kernel's headers the spaces exist.
#[test]
fn named_address_spaces_kernel_probe_passes() {
    let r = crate::common::run_c17(&["-E", "-dM", "-x", "c", "/dev/null"]);
    assert!(r.success, "{}", r.stderr);
    assert!(r.stdout.contains("#define __SEG_FS 1\n"), "{}", r.stdout);
    assert!(r.stdout.contains("#define __SEG_GS 1\n"), "{}", r.stdout);
    let dir = plib::tmp::Builder::new()
        .prefix("c17_named_as_")
        .tempdir()
        .unwrap();
    let probe = dir.path().join("probe.c");
    std::fs::write(&probe, "int __seg_fs fs; int __seg_gs gs;\n").unwrap();
    let r = crate::common::run_c17(&["-S", "-o", "/dev/null", probe.to_str().unwrap()]);
    assert!(r.success && r.stderr.is_empty(), "{}", r.stderr);
}
