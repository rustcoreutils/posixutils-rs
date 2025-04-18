//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

/// Handle mdoc config file
pub mod config;
/// Converts AST to [`String`] and print it to terminal
pub mod formatter;
/// Store [`Macro`] enum
pub mod mdoc_macro;
/// Converts input mdoc file macros to AST
pub mod parser;
