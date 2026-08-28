//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The `-m mrlist` option-argument.
//!
//! `admin` split on whitespace and `delta` split on whitespace *and* commas,
//! so `admin -m "1,2"` recorded one MR named `1,2` while `delta -m "1,2"`
//! recorded two. CSSC 1.4.1 records one in both cases, which settles it: a
//! comma is an ordinary character in an MR number.

/// Split a `-m` option-argument into individual MR numbers.
pub fn parse_str(list: &str) -> Vec<String> {
    list.split_whitespace().map(String::from).collect()
}

/// Split an optional `-m` option-argument, yielding no MRs when absent.
pub fn parse(mrlist: Option<&str>) -> Vec<String> {
    mrlist.map(parse_str).unwrap_or_default()
}
