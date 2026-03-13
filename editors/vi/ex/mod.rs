//! Ex command mode implementation.
//!
//! This module handles the colon command line interface (:command).
//! It parses and executes ex commands per POSIX specification.

pub mod address;
pub mod command;
pub mod parser;

pub use address::{Address, AddressRange};
pub use command::{ExCommand, ExResult};
pub use parser::parse_ex_command;
