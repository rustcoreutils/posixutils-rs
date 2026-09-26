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

mod atomic;
mod call;
pub mod codegen;
mod expression;
mod features;
mod float;
mod frame;
mod inline_asm;
mod legalize;
pub mod lir;
pub mod macros;
pub(crate) mod mapping;
mod memory;
pub mod regalloc;
mod relax;

pub use macros::get_macros;

pub(super) use crate::float::f64_to_f16_bits;
