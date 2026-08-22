//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// AArch64 architecture support
//

mod call;
pub mod codegen;
mod expression;
mod features;
mod float;
pub mod lir;
pub mod macros;
pub(crate) mod mapping;
pub mod regalloc;

pub use macros::get_macros;

/// Where a 128-bit integer argument's register pair starts, or `None` if it
/// must go on the stack.
///
/// AAPCS64 §5.4.2 stage C.10 allocates a 128-bit value to a pair of
/// *consecutive, even-numbered* X registers, so an odd NGRN is rounded up
/// first and the skipped register is left unused. Both the caller and the
/// callee have to agree on that, and on stage C.11: an argument that does not
/// fit sets NGRN to 8, so everything after it is on the stack too. Getting
/// either wrong puts the argument *after* the `__int128` somewhere the other
/// side is not looking.
pub(crate) fn int128_pair_start(ngrn: usize, num_arg_regs: usize) -> Option<usize> {
    let start = (ngrn + 1) & !1;
    (start + 2 <= num_arg_regs).then_some(start)
}

pub(super) use crate::float::f64_to_f16_bits;
