//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Hand-written mdoc/man parsers that replace the pest grammar. These produce
//! the same `Element`/`MacroNode`/`Macro` AST without the exponential
//! backtracking of the PEG (no per-line nesting cap needed).
//!
//! These are the only parsers: pest, its grammar and the `MAN_PARSER` switch
//! that once selected between them are gone. Coverage is validated by the
//! formatter's snapshot tests and the integration suite.

pub mod mdoc;
