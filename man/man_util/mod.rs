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
/// Minimal renderer for legacy `man(7)` (roff `man` macro) pages
pub mod man7;
/// Store [`Macro`] enum
pub mod mdoc_macro;
/// Hand-written mdoc/man parsers (replacing pest; selected via MAN_PARSER=v2)
pub mod parse;
/// Converts input mdoc file macros to AST
pub mod parser;
/// The roff front-end: request/escape interpreter (registers, conditionals,
/// user macros, `.so`/`.ig`) run before mdoc/man parsing.
pub mod roff;
/// The single terminal backend (fill/wrap/indent/assembly)
pub mod term;
