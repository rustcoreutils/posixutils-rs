//! Edit buffer module.
//!
//! This module provides the core text storage and manipulation for the editor.

#[allow(clippy::module_inception)]
mod buffer;
mod line;
mod position;

pub use buffer::Buffer;
pub use line::{char_index_at_byte, Line};
pub use position::{BufferMode, Position, Range};
