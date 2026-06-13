//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The single terminal backend: one fill/wrap/indent engine plus page-level
//! assembly, driven by every front-end (`mdoc`, `man(7)`, preprocessors).

mod backend;
mod page;
pub(crate) mod style;

pub use backend::Term;
