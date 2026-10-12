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

/// The value `movz`/`movk` lines just before `lines[at]` build in x15.
fn x15_value(lines: &[&str], at: usize) -> i64 {
    let mut value = 0i64;
    for l in lines[..at].iter().rev() {
        let (op, rest) = match (l.strip_prefix("movz x15, #"), l.strip_prefix("movk x15, #")) {
            (Some(rest), _) => (true, rest),
            (_, Some(rest)) => (false, rest),
            _ => break,
        };
        let (half, shift) = match rest.split_once(", lsl #") {
            Some((h, s)) => (h, s.parse::<u32>().expect("shift")),
            None => (rest, 0),
        };
        value |= half.parse::<i64>().expect("halfword") << shift;
        if op {
            break;
        }
    }
    value
}

/// A frame past four gigabytes is allocated whole, on both targets.
///
/// The companion to
/// `diagnostics_frame_at_the_old_ceiling_compiles_and_past_ptrdiff_max_is_refused`:
/// an over-aligned local, a variadic save area and a scalar beside it are
/// everything the prologue adds on top of the locals, and the allocation is
/// past 32 bits, so it goes through a register on both targets -- the far
/// scratch on x86-64, x15 on aarch64. The allocation is read off the assembly
/// rather than run: `codegen::far_frames` runs one.
#[test]
fn codegen_frame_past_four_gigabytes_is_allocated_whole() {
    const OBJECT: i64 = 5_000_000_000;
    let src = "extern void sink(void *);\n\
               void f(int n, ...){ _Alignas(64) char a[5000000000]; long x = n; sink(a); sink(&x); }\n";
    // x86-64: `movabsq $N, %r15` / `subq %r15, %rsp`.
    let asm = asm_for_at(
        "frame_far_x86",
        src,
        &["--target", "x86_64-unknown-linux-gnu"],
    );
    let lines: Vec<&str> = asm.lines().map(str::trim).collect();
    let sub = lines
        .iter()
        .position(|l| *l == "subq %r15, %rsp")
        .expect("x86-64 prologue allocation");
    let alloc: i64 = lines[sub - 1]
        .strip_prefix("movabsq $")
        .and_then(|v| v.strip_suffix(", %r15"))
        .and_then(|v| v.parse().ok())
        .expect("movabsq");
    assert!(alloc >= OBJECT + 8, "x86-64 allocated {alloc}:\n{asm}");
    // aarch64: `movz x15, ...` / `movk x15, ...` / `sub sp, sp, x15`.
    let asm = asm_for_at(
        "frame_far_a64",
        src,
        &["--target", "aarch64-unknown-linux-gnu"],
    );
    let lines: Vec<&str> = asm.lines().map(str::trim).collect();
    let sub = lines
        .iter()
        .position(|l| *l == "sub sp, sp, x15")
        .expect("aarch64 prologue allocation");
    let alloc = x15_value(&lines, sub);
    assert!(alloc >= OBJECT + 8, "aarch64 allocated {alloc}:\n{asm}");
}
