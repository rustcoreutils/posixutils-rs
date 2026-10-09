//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! What debhelper asks of file(1). dh_strip and dh_shlibdeps run
//!
//! ```text
//! file --brief -e apptype -e ascii -e encoding -e cdf -e compress -e tar -- FILE
//! ```
//!
//! and match the one line it prints against `ELF.*shared`,
//! `ELF.*(executable|shared)`, `not stripped`, `ELF` and `statically linked`.
//! The ELF files here are built byte by byte, so the expected text does not
//! depend on the host's compiler or object format.

use std::fs;
use std::path::{Path, PathBuf};

use plib::testing::{run_test_with_checker, TestPlan};

/// The exclusions dh_strip and dh_shlibdeps pass, verbatim.
const DH_ARGS: [&str; 14] = [
    "--brief", "-e", "apptype", "-e", "ascii", "-e", "encoding", "-e", "cdf", "-e", "compress",
    "-e", "tar", "--",
];

const ET_REL: u16 = 1;
const ET_EXEC: u16 = 2;
const ET_DYN: u16 = 3;
const PT_DYNAMIC: u32 = 2;
const PT_INTERP: u32 = 3;
const SHT_PROGBITS: u32 = 1;
const SHT_SYMTAB: u32 = 2;

/// The shape of a crafted ELF file: only the fields file(1) reads are set.
struct Elf {
    class64: bool,
    msb: bool,
    e_type: u16,
    segments: &'static [u32],
    sections: &'static [u32],
    /// Store the section count in section 0's `sh_size` and leave `e_shnum`
    /// zero, the extended numbering an object with 0xff00+ sections uses.
    extended_shnum: bool,
}

impl Elf {
    fn put(&self, buf: &mut Vec<u8>, at: usize, value: u64, width: usize) {
        if buf.len() < at + width {
            buf.resize(at + width, 0);
        }
        // Least significant byte first, placed according to the file's order.
        for (i, b) in value.to_le_bytes().iter().take(width).enumerate() {
            let pos = if self.msb { at + width - 1 - i } else { at + i };
            buf[pos] = *b;
        }
    }

    fn bytes(&self) -> Vec<u8> {
        let (ehsize, phentsize, shentsize) = if self.class64 {
            (64, 56, 64)
        } else {
            (52, 32, 40)
        };
        // A word is 8 bytes in ELFCLASS64, 4 in ELFCLASS32.
        let word = if self.class64 { 8 } else { 4 };
        let mut buf = vec![0u8; ehsize];
        buf[..4].copy_from_slice(b"\x7fELF");
        buf[4] = if self.class64 { 2 } else { 1 };
        buf[5] = if self.msb { 2 } else { 1 };
        buf[6] = 1;
        self.put(&mut buf, 16, self.e_type.into(), 2);

        let phoff = ehsize;
        let shoff = phoff + phentsize * self.segments.len();
        let shnum = self.sections.len() + 1;
        // e_phoff, e_shoff, then e_ehsize..e_shnum after e_flags.
        let (off_phoff, off_shoff, off_sizes) = if self.class64 {
            (32, 40, 52)
        } else {
            (28, 32, 40)
        };
        if !self.segments.is_empty() {
            self.put(&mut buf, off_phoff, phoff as u64, word);
        }
        self.put(&mut buf, off_shoff, shoff as u64, word);
        self.put(&mut buf, off_sizes, ehsize as u64, 2);
        self.put(&mut buf, off_sizes + 2, phentsize as u64, 2);
        self.put(&mut buf, off_sizes + 4, self.segments.len() as u64, 2);
        self.put(&mut buf, off_sizes + 6, shentsize as u64, 2);
        let e_shnum = if self.extended_shnum { 0 } else { shnum };
        self.put(&mut buf, off_sizes + 8, e_shnum as u64, 2);

        for (i, p_type) in self.segments.iter().enumerate() {
            self.put(&mut buf, phoff + i * phentsize, (*p_type).into(), 4);
        }
        // Section 0 is the null section; sh_size is at 32 (64-bit) or 20.
        buf.resize(shoff + shentsize * shnum, 0);
        if self.extended_shnum {
            let off_size = if self.class64 { 32 } else { 20 };
            self.put(&mut buf, shoff + off_size, shnum as u64, word);
        }
        for (i, sh_type) in self.sections.iter().enumerate() {
            self.put(
                &mut buf,
                shoff + (i + 1) * shentsize + 4,
                (*sh_type).into(),
                4,
            );
        }
        buf
    }
}

fn write_fixture(name: &str, contents: &[u8]) -> PathBuf {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("file_debhelper");
    fs::create_dir_all(&dir).unwrap();
    let path = dir.join(name);
    fs::write(&path, contents).unwrap();
    path
}

/// Run file with `args` then `path`; assert stdout and a zero exit. stderr is
/// not compared: a system without a default magic file says so there.
fn file_stdout(args: &[&str], path: &Path, expected: &str) {
    let mut str_args: Vec<String> = args.iter().map(|s| s.to_string()).collect();
    str_args.push(path.to_str().unwrap().to_string());
    run_test_with_checker(
        TestPlan {
            cmd: String::from("file"),
            args: str_args,
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_, output| {
            assert_eq!(String::from_utf8_lossy(&output.stdout), expected);
            assert_eq!(output.status.code(), Some(0));
        },
    );
}

fn elf64(e_type: u16, segments: &'static [u32], sections: &'static [u32]) -> Elf {
    Elf {
        class64: true,
        msb: false,
        e_type,
        segments,
        sections,
        extended_shnum: false,
    }
}

#[test]
fn file_elf_shared_object_not_stripped() {
    let elf = elf64(ET_DYN, &[PT_DYNAMIC], &[SHT_PROGBITS, SHT_SYMTAB]);
    let path = write_fixture("libx.so", &elf.bytes());
    file_stdout(
        &DH_ARGS,
        &path,
        "ELF 64-bit LSB shared object, dynamically linked, not stripped\n",
    );
}

#[test]
fn file_elf_dynamic_executable_stripped() {
    let elf = elf64(ET_EXEC, &[PT_INTERP, PT_DYNAMIC], &[SHT_PROGBITS]);
    let path = write_fixture("dyn-exe", &elf.bytes());
    file_stdout(
        &DH_ARGS,
        &path,
        "ELF 64-bit LSB executable, dynamically linked, stripped\n",
    );
}

#[test]
fn file_elf_static_executable() {
    let elf = elf64(ET_EXEC, &[1], &[SHT_SYMTAB]);
    let path = write_fixture("static-exe", &elf.bytes());
    file_stdout(
        &DH_ARGS,
        &path,
        "ELF 64-bit LSB executable, statically linked, not stripped\n",
    );
}

#[test]
fn file_elf_relocatable_has_no_linking() {
    let elf = elf64(ET_REL, &[], &[SHT_PROGBITS, SHT_SYMTAB]);
    let path = write_fixture("obj.o", &elf.bytes());
    file_stdout(
        &DH_ARGS,
        &path,
        "ELF 64-bit LSB relocatable, not stripped\n",
    );
}

#[test]
fn file_elf_32bit_msb() {
    let elf = Elf {
        class64: false,
        msb: true,
        e_type: ET_DYN,
        segments: &[PT_INTERP, PT_DYNAMIC],
        sections: &[SHT_SYMTAB],
        extended_shnum: false,
    };
    let path = write_fixture("be32", &elf.bytes());
    file_stdout(
        &DH_ARGS,
        &path,
        "ELF 32-bit MSB shared object, dynamically linked, not stripped\n",
    );
}

#[test]
fn file_elf_extended_section_count() {
    let elf = Elf {
        extended_shnum: true,
        ..elf64(ET_DYN, &[PT_DYNAMIC], &[SHT_PROGBITS, SHT_SYMTAB])
    };
    let path = write_fixture("extnum.so", &elf.bytes());
    file_stdout(
        &["-b"],
        &path,
        "ELF 64-bit LSB shared object, dynamically linked, not stripped\n",
    );
}

#[test]
fn file_elf_truncated_header_is_data() {
    let path = write_fixture("trunc", b"\x7fELF\x02\x01\x01\0\0\0\0\0\0\0\0\0\x03\0");
    file_stdout(&["-b"], &path, "data\n");
}

#[test]
fn file_elf_without_brief_names_the_file() {
    let elf = elf64(ET_DYN, &[PT_DYNAMIC], &[SHT_SYMTAB]);
    let path = write_fixture("named.so", &elf.bytes());
    let expected = format!(
        "{}: ELF 64-bit LSB shared object, dynamically linked, not stripped\n",
        path.display()
    );
    file_stdout(&[], &path, &expected);
}

#[test]
fn file_brief_long_and_short() {
    let path = write_fixture("empty", b"");
    file_stdout(&["--brief"], &path, "empty\n");
    file_stdout(&["-b"], &path, "empty\n");
}

#[test]
fn file_exclude_ascii_skips_text_tests() {
    // GNU file's "ascii" test is the one that recognises text; without it a
    // script that no magic entry matches is "data".
    let path = write_fixture("script", b"#!/bin/sh\necho hi\n");
    file_stdout(
        &["-b"],
        &path,
        "POSIX shell script, ASCII text executable\n",
    );
    file_stdout(&DH_ARGS, &path, "data\n");
}

#[test]
fn file_exclude_unknown_test_is_an_error() {
    let path = write_fixture("unknown-e", b"x");
    run_test_with_checker(
        TestPlan {
            cmd: String::from("file"),
            args: vec![
                "-e".to_string(),
                "nosuchtest".to_string(),
                path.to_str().unwrap().to_string(),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_, output| {
            assert!(output.stdout.is_empty());
            assert_ne!(output.status.code(), Some(0));
        },
    );
}
