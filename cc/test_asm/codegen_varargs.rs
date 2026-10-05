//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Variadic functions and frames: the assembly half, compiled in process.
// The programs that run are `tests/codegen/varargs.rs`.
//

use super::asm_probe::asm_for_at;

/// The largest frame c17 admits is allocated whole, on both targets.
///
/// The companion to `diagnostics_frame_at_the_ceiling_is_refused_not_wrapped`:
/// refusing the frames that used to wrap must not refuse -- or shrink -- the
/// ones that fit. An over-aligned local, a variadic save area and a scalar
/// beside it are everything the prologue adds on top of the locals. The
/// allocation is read off the assembly rather than run: a two-gigabyte frame
/// is not a test's to ask for, and c17 now zeroes it with a loop, so compiling
/// it costs nothing.
#[test]
fn codegen_largest_admitted_frame_is_allocated_whole() {
    const OBJECT: i64 = 2_147_479_000;
    let src = "extern void sink(void *);\n\
               void f(int n, ...){ _Alignas(64) char a[2147479000]; long x = n; sink(a); sink(&x); }\n";
    // x86-64: one `subq $N, %rsp`.
    let asm = asm_for_at(
        "frame_edge_x86",
        src,
        &["--target", "x86_64-unknown-linux-gnu"],
    );
    let alloc: i64 = asm
        .lines()
        .find_map(|l| {
            let l = l.trim();
            l.strip_prefix("subq $")?
                .strip_suffix(", %rsp")?
                .parse()
                .ok()
        })
        .expect("x86-64 prologue allocation");
    assert!(alloc >= OBJECT + 8, "x86-64 allocated {alloc}:\n{asm}");
    // aarch64: `movz x15, #lo` / `movk x15, #hi, lsl #16` / `sub sp, sp, x15`.
    let asm = asm_for_at(
        "frame_edge_a64",
        src,
        &["--target", "aarch64-unknown-linux-gnu"],
    );
    let lines: Vec<&str> = asm.lines().map(str::trim).collect();
    let sub = lines
        .iter()
        .position(|l| *l == "sub sp, sp, x15")
        .expect("aarch64 prologue allocation");
    let lo: i64 = lines[sub - 2]
        .strip_prefix("movz x15, #")
        .and_then(|v| v.parse().ok())
        .expect("movz");
    let hi: i64 = lines[sub - 1]
        .strip_prefix("movk x15, #")
        .and_then(|v| v.strip_suffix(", lsl #16"))
        .and_then(|v| v.parse().ok())
        .expect("movk");
    let alloc = lo + (hi << 16);
    assert!(alloc >= OBJECT + 8, "aarch64 allocated {alloc}:\n{asm}");
}
