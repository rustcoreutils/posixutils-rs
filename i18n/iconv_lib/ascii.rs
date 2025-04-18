//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::{iter, process::exit};

// Convert ASCII to UCS-4
pub fn to_ucs4<I: Iterator<Item = u8> + 'static>(
    mut input: I,
    omit_invalid: bool,
    suppress_error: bool,
) -> Box<dyn Iterator<Item = u32>> {
    let mut position = 0;

    let iter = iter::from_fn(move || {
        for code_point in input.by_ref() {
            position += 1;
            if code_point <= 127 {
                return Some(code_point as u32);
            } else if omit_invalid {
                continue;
            } else if !suppress_error {
                eprintln!("Error: Invalid input position {}", position - 1);
                std::process::exit(1);
            } else {
                return None;
            }
        }
        None
    });

    Box::new(iter)
}

pub fn from_ucs4<I: Iterator<Item = u32> + 'static>(
    mut input: I,
    omit_invalid: bool,
    suppress_error: bool,
) -> Box<dyn Iterator<Item = u8>> {
    let mut position = 0;

    let iter = iter::from_fn(move || {
        for code_point in input.by_ref() {
            position += 1;
            if code_point <= 127 {
                return Some(code_point as u8);
            } else if omit_invalid {
                continue;
            } else {
                if !suppress_error {
                    eprintln!("Error: Invalid input position {}", position - 1);
                    exit(1)
                }
                return None;
            }
        }
        None
    });
    Box::new(iter)
}
