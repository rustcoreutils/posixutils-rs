//
// Copyright (c) 2025 fox0
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use gettextrs::*;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // `gettext()` simultaneously marks a string for translation and translates
    // it at runtime.
    println!("Translated: {}", gettext("Hello, world!"));

    Ok(())
}
