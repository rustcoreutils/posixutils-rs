//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Edit buffer module.
//!
//! This module provides the core text storage and manipulation for the editor.

#[allow(clippy::module_inception)]
mod buffer;
mod line;
mod position;

pub use buffer::{Buffer, LineEdit};
pub use line::{
    ceil_char_boundary, char_index_at_byte, display_col, floor_char_boundary, leading_blank_width,
    next_char_boundary, prev_char_boundary, render_indent, Line,
};
pub use position::{BufferMode, Position, Range};
