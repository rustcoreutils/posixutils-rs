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

pub mod codegen;
mod expression;
mod features;
mod float;
pub mod lir;
pub mod macros;
pub mod regalloc;

pub use macros::get_macros;
