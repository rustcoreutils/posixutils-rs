//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

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
