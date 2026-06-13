//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Input handling module.
//!
//! This module provides key event types and input reading from the terminal.

mod key;
mod reader;

pub use key::Key;
pub use reader::InputReader;
