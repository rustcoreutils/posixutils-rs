//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// x86_64 architecture support
//

mod atomic;
mod call;
pub mod codegen;
mod expression;
mod features;
mod float;
mod frame;
mod inline_asm;
pub mod lir;
pub mod macros;
pub(crate) mod mapping;
mod memory;
pub mod regalloc;
pub(crate) mod x87;

pub use macros::get_macros;
