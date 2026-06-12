//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Preprocessors for the `tbl` (`.TS`/`.TE`) and `eqn` (`.EQ`/`.EN`) regions of
//! a manual page. The roff front-end captures each region and hands it here; the
//! result is plain text the language renderer lays out verbatim.

pub mod eqn;
pub mod tbl;
