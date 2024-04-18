//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fs;
use std::io::{self, Read};
use std::path::PathBuf;

pub fn input_stream(pathname: &PathBuf, dashed_stdin: bool) -> io::Result<Box<dyn Read>> {
    // open file, or stdin
    let file: Box<dyn Read>;
    let path_str = pathname.as_os_str();
    if dashed_stdin && path_str == "-" {
        file = Box::new(io::stdin().lock());
    } else if !dashed_stdin && path_str == "" {
        file = Box::new(io::stdin().lock());
    } else {
        file = Box::new(fs::File::open(pathname)?);
    }

    Ok(file)
}
