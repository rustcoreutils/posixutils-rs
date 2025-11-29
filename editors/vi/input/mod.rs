//! Input handling module.
//!
//! This module provides key event types and input reading from the terminal.

mod key;
mod reader;

pub use key::Key;
pub use reader::InputReader;
