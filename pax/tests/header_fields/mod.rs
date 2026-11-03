//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The header blocks pax writes, field by field: what a reader that ignores
//! extended headers sees.

use crate::common::*;
use plib::tmp::TempDir;
use std::fs;
use std::path::Path;

/// One header block of a tar archive.
struct Header([u8; BLOCK]);

impl Header {
    fn field(&self, offset: usize, len: usize) -> &[u8] {
        let field = &self.0[offset..offset + len];
        let end = field.iter().position(|&b| b == 0).unwrap_or(len);
        &field[..end]
    }
    fn octal(&self, offset: usize, len: usize) -> u64 {
        let text = std::str::from_utf8(self.field(offset, len)).unwrap().trim();
        u64::from_str_radix(text, 8).unwrap()
    }
    fn typeflag(&self) -> u8 {
        self.0[156]
    }
}

/// Every header block of the tar archive `archive`, in order, skipping the
/// data each one frames.
fn headers(archive: &[u8]) -> Vec<Header> {
    let mut out = Vec::new();
    let mut at = 0;
    while at + BLOCK <= archive.len() && archive[at..at + BLOCK].iter().any(|&b| b != 0) {
        let header = Header(archive[at..at + BLOCK].try_into().unwrap());
        let size = match header.typeflag() {
            b'1' | b'2' | b'5' => 0,
            _ => header.octal(124, 12),
        };
        at += BLOCK + size.div_ceil(BLOCK as u64) as usize * BLOCK;
        out.push(header);
    }
    out
}

/// `pax -w` of `names` in `dir` with `args` ahead of them, the archive's bytes.
fn write(dir: &Path, args: &[&str], names: &[&str]) -> Vec<u8> {
    let archive = dir.join("out.tar");
    let mut all = vec!["-w", "-f", archive.to_str().unwrap()];
    all.extend_from_slice(args);
    all.extend_from_slice(names);
    let output = run_pax_in_dir(&all, dir);
    assert_success(&output, &format!("pax {all:?}"));
    let bytes = fs::read(&archive).unwrap();
    fs::remove_file(&archive).unwrap();
    bytes
}

fn setup() -> TempDir {
    let temp = TempDir::new().unwrap();
    fs::write(temp.path().join("file"), "hello\n").unwrap();
    temp
}

/// An extended header block is a ustar header like any other: its numeric
/// fields hold octal numbers. Leaving devmajor and devminor all NUL made strict
/// ustar checkers (star's tartest) reject the archive.
#[test]
fn test_extended_header_blocks_have_octal_device_numbers() {
    let temp = setup();
    let archive = write(
        temp.path(),
        &["-x", "pax", "-o", "comment=global", "-o", "times"],
        &["file"],
    );
    let headers = headers(&archive);
    let flags: Vec<u8> = headers.iter().map(Header::typeflag).collect();
    assert_eq!(flags, b"gx0", "a g header, an x header and the member");
    for header in &headers {
        for (what, offset) in [("devmajor", 329), ("devminor", 337)] {
            assert_eq!(
                header.octal(offset, 8),
                0,
                "{what} of the {} header",
                header.typeflag() as char
            );
        }
    }
}
