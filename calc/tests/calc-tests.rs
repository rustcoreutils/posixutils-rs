//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

mod bc;
mod expr;

/// The system's own text for a failure to open `path`, as a utility reports it.
fn open_error(path: &str) -> String {
    plib::diag::io_error_text(&std::fs::File::open(path).unwrap_err())
}

/// The system's own text for a write to `/dev/full`, as a utility reports it.
fn full_device_error() -> String {
    use std::io::Write;
    let mut full = std::fs::OpenOptions::new()
        .write(true)
        .open("/dev/full")
        .unwrap();
    let e = full.write_all(b"x").unwrap_err();
    plib::diag::io_error_text(&e)
}
