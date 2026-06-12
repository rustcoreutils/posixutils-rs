//
// Copyright (c) 2024 Hemi Labs, Inc.
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
//! The mdoc parser is being grown to parity with the pest parser behind the
//! `MAN_PARSER=v2` switch; until it reaches full coverage, pest remains the
//! default. Parity is validated against pest in the unit tests.

pub mod mdoc;
