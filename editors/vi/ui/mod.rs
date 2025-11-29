//! User interface module.
//!
//! This module provides terminal handling, screen buffering, and display logic.

mod display;
mod screen;
mod status;
mod terminal;

pub use display::{
    byte_offset_to_display_col, char_width, display_col_to_byte_offset, expand_for_display,
    string_width, truncate_to_width,
};
pub use screen::Screen;
pub use status::StatusLine;
pub use terminal::{Terminal, TerminalSize};
