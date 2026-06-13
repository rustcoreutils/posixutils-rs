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
    // gettext supports plurals, i.e. you can have different messages depending
    // on the number of items the message mentions. This even works for
    // languages that have more than one plural form, like Russian or Czech.
    println!("Singular: {}", ngettext("One thing", "Multiple things", 1));
    println!("Plural: {}", ngettext("One thing", "Multiple things", 2));

    Ok(())
}
