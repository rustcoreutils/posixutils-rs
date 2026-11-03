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
    fn name(&self) -> &[u8] {
        self.field(0, 100)
    }
    fn uid(&self) -> u64 {
        self.octal(108, 8)
    }
    fn gid(&self) -> u64 {
        self.octal(116, 8)
    }
    fn mtime(&self) -> u64 {
        self.octal(136, 12)
    }
    fn linkname(&self) -> &[u8] {
        self.field(157, 100)
    }
    fn uname(&self) -> &[u8] {
        self.field(265, 32)
    }
    fn gname(&self) -> &[u8] {
        self.field(297, 32)
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

/// The headers that describe members, not extended-header data.
fn members(archive: &[u8]) -> Vec<Header> {
    headers(archive)
        .into_iter()
        .filter(|h| !matches!(h.typeflag(), b'x' | b'g'))
        .collect()
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

/// `-o keyword:=value` replaces a member's attribute, and the ustar fields
/// say so too: an archive written to hide who owns a file must not leave the
/// owner in the header a reader falls back to.
#[test]
fn test_per_file_overrides_reach_the_ustar_fields() {
    let temp = setup();
    let archive = write(
        temp.path(),
        &[
            "-x",
            "pax",
            "-o",
            "uid:=0,gid:=0,uname:=nobody,gname:=nogroup,mtime:=1234.5",
        ],
        &["file"],
    );
    let all = members(&archive);
    assert_eq!(all.len(), 1);
    let member = &all[0];
    assert_eq!(member.uid(), 0);
    assert_eq!(member.gid(), 0);
    assert_eq!(member.uname(), b"nobody");
    assert_eq!(member.gname(), b"nogroup");
    assert_eq!(member.mtime(), 1234);
}

/// `-o keyword=value` acts on every member through a `g` header, so it is the
/// value a reader takes as well; and a `keyword:=value` for the same keyword
/// wins over it, as it does in read mode.
#[test]
fn test_global_overrides_reach_the_ustar_fields() {
    let temp = setup();
    let archive = write(
        temp.path(),
        &["-x", "pax", "-o", "uid=7,uname=seven,gid=8,gid:=9"],
        &["file"],
    );
    let member = &members(&archive)[0];
    assert_eq!(member.uid(), 7);
    assert_eq!(member.uname(), b"seven");
    assert_eq!(member.gid(), 9, "keyword:= wins over keyword=");
}

/// A global value is not undone by a record computed from the value it
/// replaced: `-o mtime=0` on a file whose time has a fraction of a second
/// once still drew a per-file `mtime` record of the real time, which a reader
/// takes over the global one.
#[test]
fn test_global_mtime_is_not_overridden_by_the_file_time() {
    let temp = setup();
    let file = temp.path().join("file");
    let time = std::time::UNIX_EPOCH + std::time::Duration::new(1_600_000_000, 500_000_000);
    fs::File::options()
        .write(true)
        .open(&file)
        .unwrap()
        .set_modified(time)
        .unwrap();
    let archive = write(temp.path(), &["-x", "pax", "-o", "mtime=0"], &["file"]);
    assert_eq!(members(&archive)[0].mtime(), 0);
    let listed = run_pax_with_stdin_bytes(&["-o", "listopt=%(mtime)T"], &archive);
    assert_success(&listed, "list");
    assert_eq!(stdout_str(&listed), "Jan  1 00:00 1970\n");
}

/// A zero-length value deletes the record, and with it the name field.
#[test]
fn test_empty_override_clears_the_name_field() {
    let temp = setup();
    let archive = write(temp.path(), &["-x", "pax", "-o", "uname:="], &["file"]);
    assert_eq!(members(&archive)[0].uname(), b"");
}

/// `path` and `linkpath` reach the name and linkname fields; linkpath only on
/// a member that is a link.
#[test]
fn test_path_overrides_reach_the_ustar_fields() {
    let temp = setup();
    std::os::unix::fs::symlink("file", temp.path().join("link")).unwrap();
    let archive = write(
        temp.path(),
        &["-x", "pax", "-o", "linkpath:=elsewhere"],
        &["file", "link"],
    );
    let both = members(&archive);
    assert_eq!(both[0].name(), b"file");
    assert_eq!(both[0].linkname(), b"", "a regular file has no link");
    assert_eq!(both[1].name(), b"link");
    assert_eq!(both[1].linkname(), b"elsewhere");

    let archive = write(
        temp.path(),
        &["-x", "pax", "-o", "path:=renamed"],
        &["file"],
    );
    assert_eq!(members(&archive)[0].name(), b"renamed");
}

/// A `-o delete=` pattern keeps the record out of the archive, so the field
/// is left as the file has it: there is no record for a reader to prefer.
#[test]
fn test_deleted_override_leaves_the_ustar_field() {
    let temp = setup();
    let real = members(&write(temp.path(), &["-x", "pax"], &["file"]))[0].uid();
    let archive = write(
        temp.path(),
        &["-x", "pax", "-o", "uid:=0", "-o", "delete=uid"],
        &["file"],
    );
    assert_eq!(members(&archive)[0].uid(), real);
}
