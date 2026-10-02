//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use base64::prelude::*;
use clap::Parser;
use gettextrs::gettext;
use plib::diag;
use std::fs::{File, Permissions};
use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};

const PERMISSION_MASK: u32 = 0o7;
#[cfg(unix)]
const RW: u32 = 0o666;

/// uuencode - encode a binary file
#[derive(Parser)]
#[command(version, about = gettext("uuencode - encode a binary file"))]
struct Args {
    #[arg(short = 'm', long, help = gettext("Encode to base64 (MIME) standard, rather than UUE format"))]
    base64: bool,

    /// `[file] decode_pathname` — the optional input file is the *first* operand,
    /// the required decode pathname is the last.
    #[arg(help = gettext("[file] decode_pathname"))]
    operands: Vec<String>,
}

enum EncodingType {
    Historical,
    Base64,
}

impl EncodingType {
    fn get_header(&self) -> String {
        match *self {
            EncodingType::Historical => "begin",
            EncodingType::Base64 => "begin-base64",
        }
        .to_string()
    }
}

/// Resolve operands into `(input file, decode_pathname)`.
///
/// Synopsis is `uuencode [-m] [file] decode_pathname`: the optional `file` is the
/// *first* operand, so a single operand is the (required) decode pathname and the
/// input is standard input.
fn resolve_operands(operands: &[String]) -> Result<(Option<PathBuf>, String), String> {
    match operands {
        [] => Err(gettext("missing decode_pathname operand")),
        [decode_pathname] => Ok((None, decode_pathname.clone())),
        [file, decode_pathname] => Ok((Some(PathBuf::from(file)), decode_pathname.clone())),
        _ => Err(gettext("too many operands")),
    }
}

/// A mode's permission bits as the header's three octal digits (owner,
/// group, other); the set-ID and sticky bits are not written.
fn format_mode(mode: u32) -> String {
    let others_perm = mode & PERMISSION_MASK;
    let group_perm = (mode >> 3) & PERMISSION_MASK;
    let user_perm = (mode >> 6) & PERMISSION_MASK;

    format!("{user_perm}{group_perm}{others_perm}")
}

/// The permission bits of `perm`, as POSIX writes them.
///
/// Windows keeps only a read-only attribute, which is the owner-write bit
/// seen from POSIX: such a file reads as `0444`, any other as `0644`.
fn mode_of(perm: &Permissions) -> u32 {
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        perm.mode()
    }
    #[cfg(windows)]
    {
        if perm.readonly() {
            0o444
        } else {
            0o644
        }
    }
}

/// The mode a newly created file gets: `0666` less the umask, read without
/// leaving it changed (umask(2) has no read-only form). Windows has no umask,
/// and a new file there is an ordinary writable one, `0644`.
fn new_file_mode() -> u32 {
    #[cfg(target_os = "macos")]
    {
        let old = unsafe { libc::umask(RW as u16) };
        unsafe { libc::umask(old) };
        RW & (!old as u32)
    }
    #[cfg(all(unix, not(target_os = "macos")))]
    {
        let old = unsafe { libc::umask(RW) };
        unsafe { libc::umask(old) };
        RW & !old
    }
    #[cfg(windows)]
    {
        0o644
    }
}

fn encode_base64_line(line: &[u8]) -> Vec<u8> {
    let mut out = BASE64_STANDARD.encode(line).as_bytes().to_vec();
    out.push(b'\n');
    out
}

fn encode_historical_line(line: &[u8]) -> Vec<u8> {
    let mut out = Vec::new();
    out.push(line.len() as u8 + 0x20);
    // in every line we'll take a chunk of 3 bytes and encode it
    for in_chunk in line.chunks(3) {
        // there could be less than 3 bytes, we need to fill it up with the padding byte
        // those are simply the null ASCII character i.e 0
        let a = in_chunk[0];
        let b = *in_chunk.get(1).unwrap_or(&0);
        let c = *in_chunk.get(2).unwrap_or(&0);
        let mut out_chunk = [
            0x20 + ((a >> 2) & 0x3F),
            0x20 + (((a & 0x03) << 4) | ((b >> 4) & 0x0F)),
            0x20 + (((b & 0x0F) << 2) | ((c >> 6) & 0x03)),
            0x20 + (c & 0x3F),
        ];

        // https://pubs.opengroup.org/onlinepubs/9699919799/utilities/uuencode.html directly mentions
        // the algorithm below(to convert 3 byte to 4 bytes of historical encoding)
        for i in out_chunk.iter_mut() {
            if *i == 32 {
                *i = 96
            }
        }
        out.extend_from_slice(&out_chunk);
    }
    out.push(b'\n');
    out
}

/// encodes the file (it can be standard input too) and outputs on standard output
fn encode_file(base64: bool, file: Option<&Path>, decode_path: &str) -> io::Result<()> {
    let encoding_type = if base64 {
        EncodingType::Base64
    } else {
        EncodingType::Historical
    };
    let header_init = encoding_type.get_header();

    let mut buf: Vec<u8> = Vec::new();
    let mut out: Vec<u8> = Vec::new();

    match file {
        None => {
            let perm = format_mode(new_file_mode());
            let header = format!("{header_init} {perm} {decode_path}\n");
            out.extend_from_slice(header.as_bytes());
            io::stdin().lock().read_to_end(&mut buf)?;
        }
        Some(path) => {
            let mut f = File::open(path)?;
            let perm = format_mode(mode_of(&f.metadata()?.permissions()));
            let header = format!("{header_init} {perm} {decode_path}\n");
            out.extend_from_slice(header.as_bytes());
            f.read_to_end(&mut buf)?;
        }
    }

    match encoding_type {
        EncodingType::Historical => {
            for line in buf.chunks(45) {
                out.extend(encode_historical_line(line));
            }

            out.extend(b"`\nend");
        }
        EncodingType::Base64 => {
            for line in buf.chunks(45) {
                out.extend(encode_base64_line(line));
            }
            out.extend(b"====");
        }
    }

    io::stdout().write_all(out.as_slice())?;
    io::stdout().write_all(b"\n")?;

    Ok(())
}

fn main() {
    diag::init_locale("uuencode");

    let args = Args::parse();

    let (file, decode_path) = match resolve_operands(&args.operands) {
        Ok(v) => v,
        Err(msg) => {
            diag::error(&msg);
            std::process::exit(1);
        }
    };

    if let Err(e) = encode_file(args.base64, file.as_deref(), &decode_path) {
        let name = file
            .as_ref()
            .map(|p| p.display().to_string())
            .unwrap_or_else(|| gettext("standard input"));
        diag::error(&format!("{name}: {e}"));
    }

    std::process::exit(diag::exit_status())
}
