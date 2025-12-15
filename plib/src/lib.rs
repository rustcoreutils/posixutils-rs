//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

pub mod curuser;
pub mod group;
pub mod io;
pub mod lzw;
pub mod modestr;
pub mod platform;
pub mod priority;
pub mod regex;
pub mod safe_parse;
pub mod sccsfile;
pub mod testing;
pub mod user;
pub mod utmpx;

pub const BUFSZ: usize = 8 * 1024;
