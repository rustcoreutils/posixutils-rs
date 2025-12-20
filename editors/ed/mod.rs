//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! POSIX ed line editor implementation.
//!
//! This module implements the POSIX ed editor as specified in POSIX.1-2024.

pub mod buffer;
pub mod editor;
pub mod error;
pub mod parser;

pub use editor::Editor;
