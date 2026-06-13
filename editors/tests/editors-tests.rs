//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Integration tests for the editors crate (vi, ex, ed).
//!
//! This is the test harness - it only contains mod statements.
//! Actual tests are in subdirectories.

mod ed;
mod ex;
mod headless;
mod integration;
mod pty;
