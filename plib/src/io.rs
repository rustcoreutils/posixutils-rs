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
    let path_str = pathname.as_os_str();
    let file: Box<dyn Read> =
        if (dashed_stdin && path_str == "-") || (!dashed_stdin && path_str.is_empty()) {
            Box::new(io::stdin().lock())
        } else {
            Box::new(fs::File::open(pathname)?)
        };

    Ok(file)
}

pub fn input_stream_opt(pathname: &Option<PathBuf>) -> io::Result<Box<dyn Read>> {
    match pathname {
        Some(path) => input_stream(path, false),
        None => input_stream(&PathBuf::new(), false),
    }
}

pub fn input_reader(
    pathname: &PathBuf,
    dashed_stdin: bool,
) -> io::Result<io::BufReader<Box<dyn Read>>> {
    let file = input_stream(pathname, dashed_stdin)?;
    Ok(io::BufReader::new(file))
}
