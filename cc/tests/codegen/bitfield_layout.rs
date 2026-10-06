//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Unnamed bit-fields lay out as gcc lays them out. On x86-64 c17 let one
// raise the alignment of its struct or union, so `struct { char a; int :5;
// char b; }` was 4 bytes where gcc's is 3 -- a different ABI.
//

use super::asm_probe::AARCH64_LINUX;
use crate::common::{
    aarch64_cross_available, compile_and_run_everywhere, compile_with_host_cc, create_c_file,
    cross_link_and_run, run_c17,
};

#[test]
fn codegen_unnamed_bitfields_lay_out_as_in_gcc() {
    compile_and_run_everywhere(
        "unnamed_bitfield_layout",
        r#"
/* Unnamed bit-fields and the layout gcc gives them. On x86-64 (System V)
   an unnamed bit-field does not raise the alignment of the struct or
   union holding it; on aarch64 (AAPCS64) it does. Sizes, alignments and
   offsets must be gcc's, or the struct crosses an ABI boundary wrong. */
#include <stddef.h>
struct s1 { char a; int :5; char b; };
struct s2 { char a; int :0; char b; };
struct s3 { char a; long :3; char b; };
struct s4 { int :20; char c; };
struct s5 { char a; int :5; int :7; char b; };
struct s6 { short a; int :31; char b; };
struct s7 { char a; long long :40; char b; };
struct s8 { char a; int b:5; char c; };
union u1 { int :20; char c; };
union u3 { char c; long :33; };
#define EQ(e, v, code) if ((e) != (v)) return code
int main(void)
{
#if defined(__x86_64__)
    EQ(sizeof(struct s1), 3, 1);  EQ(_Alignof(struct s1), 1, 2);
    EQ(sizeof(struct s3), 3, 3);  EQ(_Alignof(struct s3), 1, 4);
    EQ(sizeof(struct s4), 4, 5);  EQ(_Alignof(struct s4), 1, 6);
    EQ(sizeof(struct s5), 4, 7);  EQ(_Alignof(struct s5), 1, 8);
    EQ(sizeof(struct s6), 10, 9); EQ(_Alignof(struct s6), 2, 10);
    EQ(sizeof(struct s7), 7, 11); EQ(_Alignof(struct s7), 1, 12);
    EQ(sizeof(union u1), 3, 13);
    EQ(sizeof(union u3), 5, 14);  EQ(_Alignof(union u3), 1, 15);
#elif defined(__aarch64__) && defined(__linux__)
    EQ(sizeof(struct s1), 4, 1);  EQ(_Alignof(struct s1), 4, 2);
    EQ(sizeof(struct s3), 8, 3);  EQ(_Alignof(struct s3), 8, 4);
    EQ(sizeof(struct s6), 12, 9); EQ(_Alignof(struct s6), 4, 10);
    EQ(sizeof(struct s7), 8, 11); EQ(_Alignof(struct s7), 8, 12);
    EQ(sizeof(union u3), 8, 14);  EQ(_Alignof(union u3), 8, 15);
#endif
    /* Where named members land agrees on both. */
    EQ(sizeof(struct s2) >= 5, 1, 20);
    EQ(sizeof(struct s8), 4, 21); EQ(_Alignof(struct s8), 4, 22);
    EQ(offsetof(struct s1, b), 2, 23);
    EQ(offsetof(struct s3, b), 2, 24);
    EQ(offsetof(struct s5, b), 3, 25);
    EQ(offsetof(struct s6, b), 8, 26);
    EQ(offsetof(struct s7, b), 6, 27);
    return 0;
}
"#,
    );
}

/// An alignment written after an unnamed bit-field's width, or carried by a
/// typedef'd bit-field type, places the field as gcc places it; the first
/// was a parse error, and a typedef's alignment did not place even a named
/// field. Whether it then aligns the aggregate follows the target's ABI.
#[test]
fn codegen_bitfield_alignment_attributes_place_as_in_gcc() {
    compile_and_run_everywhere(
        "bitfield_alignment_attributes",
        r#"
#include <stddef.h>
typedef int ai8 __attribute__((aligned(8)));
struct a1 { char a; int :5 __attribute__((aligned(8))); char b; };
struct a2 { char a; int :0 __attribute__((aligned(8))); char b; };
struct a3 { char a; ai8 :5; char b; };
struct a4 { char a; ai8 x:5; char b; };
struct a5 { char a; int :5 __attribute__((aligned(8))), :3; char b; };
struct a6 { char a; int :30 __attribute__((packed)); char b; };
#pragma pack(1)
struct a7 { char a; int :0 __attribute__((aligned(8))); char b; };
struct a8 { char a; ai8 :5; char b; };
#pragma pack()
union v1 { char c; int :5 __attribute__((aligned(8))); };
#define EQ(e, v, code) if ((e) != (v)) return code
int main(void)
{
#if defined(__x86_64__)
    EQ(sizeof(struct a1), 10, 1); EQ(_Alignof(struct a1), 1, 2);
    EQ(sizeof(struct a2), 9, 3);  EQ(_Alignof(struct a2), 1, 4);
    EQ(sizeof(struct a3), 10, 5); EQ(_Alignof(struct a3), 1, 6);
    EQ(sizeof(struct a5), 10, 7);
    EQ(sizeof(struct a7), 9, 8);  EQ(_Alignof(struct a7), 1, 9);
    EQ(sizeof(union v1), 1, 10);
#elif defined(__aarch64__) && defined(__linux__)
    EQ(sizeof(struct a1), 16, 1); EQ(_Alignof(struct a1), 8, 2);
    EQ(sizeof(struct a2), 16, 3); EQ(_Alignof(struct a2), 8, 4);
    EQ(sizeof(struct a3), 16, 5); EQ(_Alignof(struct a3), 8, 6);
    EQ(sizeof(struct a5), 16, 7);
    EQ(sizeof(struct a7), 16, 8); EQ(_Alignof(struct a7), 8, 9);
    EQ(sizeof(union v1), 8, 10);
#endif
    EQ(sizeof(struct a4), 16, 20); EQ(_Alignof(struct a4), 8, 21);
    EQ(sizeof(struct a6), 6, 22);  EQ(_Alignof(struct a6), 1, 23);
    EQ(sizeof(struct a8), 3, 24);  EQ(_Alignof(struct a8), 1, 25);
    EQ(offsetof(struct a1, b), 9, 26);
    EQ(offsetof(struct a2, b), 8, 27);
    EQ(offsetof(struct a3, b), 9, 28);
    EQ(offsetof(struct a4, b), 9, 29);
    EQ(offsetof(struct a5, b), 9, 30);
    EQ(offsetof(struct a6, b), 5, 31);
    EQ(offsetof(struct a7, b), 8, 32);
    EQ(offsetof(struct a8, b), 2, 33);
    return 0;
}
"#,
    );
}

// Aggregates whose layout an unnamed bit-field decides, across calls between
// c17 and gcc. On x86-64 `struct u_s1` is three bytes of alignment 1, so it is
// passed in one INTEGER eightbyte and spilled to the stack unaligned; `u_s6`
// is ten bytes and two eightbytes; in `u_fb` the bit-field's bits make the
// first eightbyte INTEGER while the second stays SSE. Both units also report
// every size, alignment and offset, copy a struct through a pointer into an
// array (a copy of the wrong size clobbers the next element), and index an
// array (the stride is the size). No headers: c17 targeting aarch64 from an
// x86-64 host has no aarch64 libc headers to read.
const UNNAMED_DECLS: &str = r#"
#define offsetof(T, m) __builtin_offsetof(T, m)
struct u_s1 { char a; int :5; char b; };
struct u_s3 { char a; long :3; char b; };
struct u_s6 { short a; int :31; char b; };
struct u_s7 { char a; long long :40; char b; };
struct u_fb { float f; int :8; float g; };
union u_u1 { int :20; char c; };
union u_u3 { char c; long :33; };
#define L long
void u_layout(unsigned char *out);
struct u_s1 u_f1(struct u_s1 x, L, L, L, L, L, L, L, L, struct u_s1 y);
struct u_s3 u_f3(struct u_s3 x, L, L, L, L, L, L, L, L, struct u_s3 y);
struct u_s6 u_f6(struct u_s6 x, L, L, L, L, L, L, L, L, struct u_s6 y);
struct u_s7 u_f7(struct u_s7 x, L, L, L, L, L, L, L, L, struct u_s7 y);
struct u_fb u_ffb(struct u_fb x, L, L, L, L, L, L, L, L, struct u_fb y);
union u_u1 u_fu1(union u_u1 x, L, L, L, L, L, L, L, L, union u_u1 y);
union u_u3 u_fu3(union u_u3 x, L, L, L, L, L, L, L, L, union u_u3 y);
void u_copy3(struct u_s3 *dst, const struct u_s3 *src);
int u_sum7(const struct u_s7 *arr, int n);
"#;

const UNNAMED_CALLEE: &str = r#"
#define LAY(T) *out++ = sizeof(T); *out++ = _Alignof(T)
void u_layout(unsigned char *out)
{
    LAY(struct u_s1); LAY(struct u_s3); LAY(struct u_s6); LAY(struct u_s7);
    LAY(struct u_fb); LAY(union u_u1); LAY(union u_u3);
    *out++ = offsetof(struct u_s1, b); *out++ = offsetof(struct u_s3, b);
    *out++ = offsetof(struct u_s6, b); *out++ = offsetof(struct u_s7, b);
    *out++ = offsetof(struct u_fb, g);
}
#define SUM (p + q + r + s + t + u + v + w)
#define PARAMS L p, L q, L r, L s, L t, L u, L v, L w
struct u_s1 u_f1(struct u_s1 x, PARAMS, struct u_s1 y)
{ struct u_s1 z = { x.a + y.a, x.b + y.b + SUM }; return z; }
struct u_s3 u_f3(struct u_s3 x, PARAMS, struct u_s3 y)
{ struct u_s3 z = { x.a + y.a, x.b + y.b + SUM }; return z; }
struct u_s6 u_f6(struct u_s6 x, PARAMS, struct u_s6 y)
{ struct u_s6 z = { x.a + y.a, x.b + y.b + SUM }; return z; }
struct u_s7 u_f7(struct u_s7 x, PARAMS, struct u_s7 y)
{ struct u_s7 z = { x.a + y.a, x.b + y.b + SUM }; return z; }
struct u_fb u_ffb(struct u_fb x, PARAMS, struct u_fb y)
{ struct u_fb z = { x.f + y.f, x.g + y.g + SUM }; return z; }
union u_u1 u_fu1(union u_u1 x, PARAMS, union u_u1 y)
{ union u_u1 z = { x.c + y.c + SUM }; return z; }
union u_u3 u_fu3(union u_u3 x, PARAMS, union u_u3 y)
{ union u_u3 z = { x.c + y.c + SUM }; return z; }
void u_copy3(struct u_s3 *dst, const struct u_s3 *src) { *dst = *src; }
int u_sum7(const struct u_s7 *arr, int n)
{ int t = 0; for (int i = 0; i < n; i++) t += arr[i].a * 10 + arr[i].b; return t; }
"#;

const UNNAMED_CALLER: &str = r#"
#define ARGS 1, 2, 3, 4, 5, 6, 7, 8
int main(void)
{
    unsigned char mine[19] = {
        sizeof(struct u_s1), _Alignof(struct u_s1),
        sizeof(struct u_s3), _Alignof(struct u_s3),
        sizeof(struct u_s6), _Alignof(struct u_s6),
        sizeof(struct u_s7), _Alignof(struct u_s7),
        sizeof(struct u_fb), _Alignof(struct u_fb),
        sizeof(union u_u1), _Alignof(union u_u1),
        sizeof(union u_u3), _Alignof(union u_u3),
        offsetof(struct u_s1, b), offsetof(struct u_s3, b),
        offsetof(struct u_s6, b), offsetof(struct u_s7, b),
        offsetof(struct u_fb, g),
    }, theirs[19];
    u_layout(theirs);
    for (int i = 0; i < 19; i++)
        if (mine[i] != theirs[i]) return 100 + i;

    struct u_s1 a1 = { 1, 2 }, b1 = { 3, 4 }, r1 = u_f1(a1, ARGS, b1);
    if (r1.a != 4 || r1.b != 42) return 1;
    struct u_s3 a3 = { 5, 6 }, b3 = { 7, 8 }, r3 = u_f3(a3, ARGS, b3);
    if (r3.a != 12 || r3.b != 50) return 2;
    struct u_s6 a6 = { 300, 9 }, b6 = { 400, 10 }, r6 = u_f6(a6, ARGS, b6);
    if (r6.a != 700 || r6.b != 55) return 3;
    struct u_s7 a7 = { 11, 12 }, b7 = { 13, 14 }, r7 = u_f7(a7, ARGS, b7);
    if (r7.a != 24 || r7.b != 62) return 4;
    struct u_fb af = { 1.5f, 2.5f }, bf = { 3.0f, 4.0f }, rf = u_ffb(af, ARGS, bf);
    if (rf.f != 4.5f || rf.g != 42.5f) return 5;
    union u_u1 au1 = { 1 }, bu1 = { 2 }, ru1 = u_fu1(au1, ARGS, bu1);
    if (ru1.c != 39) return 6;
    union u_u3 au3 = { 3 }, bu3 = { 4 }, ru3 = u_fu3(au3, ARGS, bu3);
    if (ru3.c != 43) return 7;

    struct u_s3 arr3[2] = { { 0, 0 }, { 21, 22 } }, src3 = { 23, 24 };
    u_copy3(&arr3[0], &src3);
    if (arr3[0].a != 23 || arr3[0].b != 24) return 8;
    if (arr3[1].a != 21 || arr3[1].b != 22) return 9;

    struct u_s7 arr7[3];
    __builtin_memset(arr7, 0x7f, sizeof arr7);
    for (int i = 0; i < 3; i++) { arr7[i].a = i + 1; arr7[i].b = i + 4; }
    if (u_sum7(arr7, 3) != 75) return 10;
    return 0;
}
"#;

/// Structs and unions laid out by an unnamed bit-field pass by value, and
/// are copied and indexed, exactly as gcc does them: a c17 callee under a
/// gcc caller and the reverse, on the x86-64 host and on aarch64 Linux under
/// qemu, where the gcc/gcc pair is the reference.
#[test]
fn cross_abi_unnamed_bitfield_aggregates_match_gcc() {
    let callee = format!("{UNNAMED_DECLS}{UNNAMED_CALLEE}");
    let caller = format!("{UNNAMED_DECLS}{UNNAMED_CALLER}");

    if cfg!(all(target_os = "linux", target_arch = "x86_64")) {
        for (name, c17_unit, host_unit) in [
            ("unnamed_bf_c17_callee", &callee, &caller),
            ("unnamed_bf_c17_caller", &caller, &callee),
        ] {
            if let Some(rc) = compile_with_host_cc(name, c17_unit, host_unit) {
                assert_eq!(rc, 0, "{name}");
            }
        }
    }

    if !aarch64_cross_available() {
        eprintln!(
            "SKIP the aarch64 half of cross_abi_unnamed_bitfield_aggregates_match_gcc: \
             no aarch64 cross toolchain"
        );
        return;
    }
    let callee_c = create_c_file("unnamed_bf_callee", &callee);
    let caller_c = create_c_file("unnamed_bf_caller", &caller);
    let callee_src = callee_c.path().to_string_lossy().into_owned();
    let caller_src = caller_c.path().to_string_lossy().into_owned();
    assert_eq!(
        cross_link_and_run("unnamed_bf_ref", &[&caller_src, &callee_src]),
        0,
        "the gcc/gcc reference must pass, or this probe is not testing the ABI"
    );
    for opt in ["-O0", "-O2"] {
        let asm = |src: &str, tag: &str| {
            let out = plib::tmp::Builder::new()
                .prefix(&format!("c17_unnamed_bf_{tag}_"))
                .suffix(".s")
                .tempfile()
                .expect("failed to create temp file");
            let path = out.path().to_string_lossy().into_owned();
            let run = run_c17(&["--target", AARCH64_LINUX, opt, "-S", "-o", &path, src]);
            assert!(run.success, "c17 failed on the {tag}:\n{}", run.stderr);
            (out, path)
        };
        let (_callee_tmp, callee_s) = asm(&callee_src, "callee");
        let (_caller_tmp, caller_s) = asm(&caller_src, "caller");
        assert_eq!(
            cross_link_and_run("unnamed_bf_c17_callee", &[&caller_src, &callee_s]),
            0,
            "gcc caller, c17 callee, {opt}"
        );
        assert_eq!(
            cross_link_and_run("unnamed_bf_c17_caller", &[&caller_s, &callee_src]),
            0,
            "c17 caller, gcc callee, {opt}"
        );
    }
}
