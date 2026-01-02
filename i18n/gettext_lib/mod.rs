//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! gettext library for POSIX i18n utilities
//!
//! This module provides functionality for reading and writing GNU .mo files,
//! parsing .po files, evaluating plural expressions, and looking up messages
//! in locale-specific message catalogs.

pub mod catalog;
pub mod lookup;
pub mod mo_file;
pub mod plural;
pub mod po_file;
