//
// Copyright (c) 2024 Bloq Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

mod args;
mod line_iterator;
pub mod line_transform;
mod page_iterator;

pub use args::{Args, Parameters};
pub use line_iterator::LineBreakIterator;
pub use page_iterator::PageIterator;
