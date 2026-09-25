//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// __builtin_return_address / __builtin_frame_address at a level above zero
//

use crate::common::{compile_and_run, compile_and_run_aarch64, compile_expect_error};

/// Walks the frame chain one and two levels up, from two different call sites.
///
/// `mid` is reached from `call_a` and from `call_b`, so its return address
/// differs between the two calls, and `leaf`'s level-1 answer has to follow
/// it. An implementation that ignores the level answers `leaf`'s own return
/// address -- a point inside `mid`, the same from both call sites -- which is
/// what `execute/20010122-1` in the gcc torture suite caught.
///
/// `mid` reads its level-0 return address *after* a call on purpose: aarch64
/// used to answer it from `x30`, which that call has overwritten.
///
/// Every call is followed by another so that none is a tail call, which would
/// take a frame out of the chain. A level above zero needs frame records: gcc
/// answers it only with `-fno-omit-frame-pointer`, and on aarch64 answers 0.
/// c17 gives every function a frame record, so it walks the chain on both
/// targets.
const FRAME_CHAIN: &str = r#"
#define NOINLINE __attribute__((noinline))

void *ra0[2], *ra1[2], *ra2[2], *fp0[2], *fp1[2], *caller_ra[2];
volatile int sink_count;

NOINLINE void sink(void) { sink_count++; }

NOINLINE void leaf(int i)
{
    ra1[i] = __builtin_return_address(1);
    ra2[i] = __builtin_return_address(2);
    fp1[i] = __builtin_frame_address(1);
}

NOINLINE void mid(int i)
{
    fp0[i] = __builtin_frame_address(0);
    sink();
    ra0[i] = __builtin_return_address(0);
    leaf(i);
    sink();
}

NOINLINE void call_a(void)
{
    caller_ra[0] = __builtin_return_address(0);
    mid(0);
    sink();
}

NOINLINE void call_b(void)
{
    sink();
    caller_ra[1] = __builtin_return_address(0);
    mid(1);
    sink();
}

int main(void)
{
    call_a();
    call_b();

    // Two call sites of `mid`, so two return addresses.
    if (ra0[0] == ra0[1]) return 1;
    // One level up from `leaf` is `mid`'s return address...
    if (ra1[0] != ra0[0]) return 2;
    if (ra1[1] != ra0[1]) return 3;
    // ...and two levels up is `call_a`'s / `call_b`'s, both in `main`.
    if (ra2[0] != caller_ra[0]) return 4;
    if (ra2[1] != caller_ra[1]) return 5;
    if (ra2[0] == ra2[1]) return 6;
    // One frame up from `leaf` is `mid`'s frame.
    if (fp1[0] != fp0[0]) return 7;
    if (fp1[1] != fp0[1]) return 8;
    return 0;
}
"#;

#[test]
fn builtins_frame_chain_levels() {
    assert_eq!(compile_and_run("frame_chain", FRAME_CHAIN, &[]), 0);
}

#[test]
fn builtins_frame_chain_levels_o2() {
    let opts = vec!["-O2".to_string()];
    assert_eq!(compile_and_run("frame_chain_o2", FRAME_CHAIN, &opts), 0);
}

#[test]
fn builtins_frame_chain_levels_aarch64() {
    for opt in ["-O0", "-O2"] {
        if let Some(code) = compile_and_run_aarch64("frame_chain_a64", FRAME_CHAIN, opt) {
            assert_eq!(code, 0, "at {opt}");
        }
    }
}

/// gcc requires the level to be an integer constant ("invalid argument to
/// '__builtin_return_address'"): walking a run-time number of frames is not
/// something either builtin does.
#[test]
fn builtins_frame_level_must_be_constant() {
    for builtin in ["__builtin_return_address", "__builtin_frame_address"] {
        let src = format!("void *f(int n) {{ return {builtin}(n); }}\n");
        compile_expect_error("frame_level_nonconst", &src, builtin);
    }
}
