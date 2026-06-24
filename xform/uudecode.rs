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
use std::fs::File;
use std::io::{self, Read, Write};
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};

/// uudecode - decode a binary file
#[derive(Parser)]
#[command(version, about = gettext("uudecode - decode a binary file"))]
struct Args {
    #[arg(short, long, help = gettext("A pathname of a file that shall be used instead of any pathname contained in the input data"))]
    outfile: Option<PathBuf>,

    #[arg(help = gettext("The pathname of a file containing uuencoded data"))]
    file: Option<PathBuf>,
}

enum DecodingType {
    Historical,

    Base64,
}

struct Header {
    dec_type: DecodingType,

    lower_perm_bits: u32,

    out: PathBuf,
}

/// Build an `InvalidData` error for malformed uuencode input.
fn invalid(msg: &str) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, gettext(msg))
}

/// Drop a single trailing carriage return (matches `str::lines()` for CRLF input).
fn strip_cr(line: &[u8]) -> &[u8] {
    match line.split_last() {
        Some((b'\r', init)) => init,
        _ => line,
    }
}

/// The magic cookies `-` and `/dev/stdout` both mean "write to standard output"
/// (POSIX.1-2024, Austin Group Defect 1544).
fn is_stdout_cookie(path: &Path) -> bool {
    let s = path.as_os_str();
    s == "-" || s == "/dev/stdout"
}

/// Whether the caller has write permission on an existing path (access(2), W_OK).
fn is_writable(path: &Path) -> bool {
    use std::os::unix::ffi::OsStrExt;
    match std::ffi::CString::new(path.as_os_str().as_bytes()) {
        Ok(c) => unsafe { libc::access(c.as_ptr(), libc::W_OK) == 0 },
        Err(_) => false,
    }
}

impl Header {
    fn parse(line: &[u8]) -> io::Result<Self> {
        // The header line is always portable ASCII; reject non-text rather than panic.
        let line = std::str::from_utf8(line).map_err(|_| invalid("header is not valid text"))?;

        // "begin <mode> <decode_pathname>" — the pathname may contain spaces.
        let mut fields = line.splitn(3, ' ');
        let tag = fields.next().unwrap_or("");
        let dec_type = match tag {
            "begin" => DecodingType::Historical,
            "begin-base64" => DecodingType::Base64,
            _ => return Err(invalid("invalid uuencode header")),
        };

        let mode_str = fields
            .next()
            .ok_or_else(|| invalid("missing mode in uuencode header"))?;
        // Reject if mode contains a sign.
        if mode_str.starts_with('+') || mode_str.starts_with('-') {
            return Err(invalid("invalid permission value: unexpected sign"));
        }
        let lower_perm_bits =
            u32::from_str_radix(mode_str, 8).map_err(|_| invalid("invalid permission value"))?;

        let out = fields
            .next()
            .ok_or_else(|| invalid("missing pathname in uuencode header"))?;

        Ok(Self {
            dec_type,
            lower_perm_bits,
            out: PathBuf::from(out),
        })
    }
}

fn decode_historical_line(line: &[u8]) -> Vec<u8> {
    let mut out = Vec::new();

    for chunk in line.chunks(4) {
        // Missing trailing bytes in a short final chunk decode to zero (0x20 - 0x20).
        let v = |i: usize| chunk.get(i).copied().unwrap_or(0x20).wrapping_sub(0x20) & 0x3F;
        let (a, b, c, d) = (v(0), v(1), v(2), v(3));

        out.push((a << 2) | (b >> 4));
        out.push((b << 4) | (c >> 2));
        out.push((c << 6) | d);
    }

    out
}

fn decode_base64_line(line: &[u8]) -> io::Result<Vec<u8>> {
    // Per spec (119899-119900), characters not in the Base64 alphabet (line breaks,
    // stray whitespace, CR from CRLF transport, ...) are ignored by decoders.
    let filtered: Vec<u8> = line
        .iter()
        .copied()
        .filter(|&b| b.is_ascii_alphanumeric() || b == b'+' || b == b'/' || b == b'=')
        .collect();
    BASE64_STANDARD
        .decode(&filtered)
        .map_err(|_| invalid("invalid base64 data"))
}

fn decode_file(args: &Args) -> io::Result<()> {
    let mut buf: Vec<u8> = Vec::new();
    let mut out: Vec<u8> = Vec::new();

    let file_p = args
        .file
        .as_ref()
        .unwrap_or(&PathBuf::from("/dev/stdin"))
        .clone();

    if file_p.as_os_str() == "/dev/stdin" {
        io::stdin().lock().read_to_end(&mut buf)?;
    } else {
        let mut file = File::open(&file_p)?;
        file.read_to_end(&mut buf)?;
    }

    // Scan the input for the begin line (it need not be the first line — the
    // encoded stream may be wrapped in a mail message or preceded by other text).
    let mut lines = buf.split(|&b| b == b'\n');
    let header = loop {
        let line = match lines.next() {
            Some(l) => strip_cr(l),
            None => return Err(invalid("no uuencode header found")),
        };
        if line.starts_with(b"begin ") || line.starts_with(b"begin-base64 ") {
            break Header::parse(line)?;
        }
    };

    match header.dec_type {
        DecodingType::Historical => {
            while let Some(raw) = lines.next() {
                let line = strip_cr(raw);
                if line.is_empty() {
                    continue;
                }

                // Historical encoding optionally replaces 0x20 with 0x60 ('`').
                let line: Vec<u8> = line
                    .iter()
                    .map(|&b| if b == b'`' { b' ' } else { b })
                    .collect();

                if line.len() == 1 && line[0] == b' ' {
                    let end_line = lines.next().map(strip_cr).unwrap_or(b"");
                    if end_line == b"end" {
                        break;
                    } else {
                        return Err(invalid("invalid ending"));
                    }
                }

                let len = line[0].wrapping_sub(0x20) as usize;
                let mut dec_out = decode_historical_line(&line[1..]);
                if len < dec_out.len() {
                    dec_out.truncate(len);
                }
                out.extend_from_slice(&dec_out);
            }
        }

        DecodingType::Base64 => {
            for raw in lines {
                let line = strip_cr(raw);
                if line == b"====" {
                    break;
                }
                if line.is_empty() {
                    continue;
                }
                out.extend_from_slice(&decode_base64_line(line)?);
            }
        }
    }

    let out_path = args.outfile.as_ref().unwrap_or(&header.out);

    if is_stdout_cookie(out_path) {
        io::stdout().write_all(&out)?;
    } else {
        // If the target exists and the user lacks write permission, terminate with
        // an error (spec 119716-119718). Otherwise overwrite the file in place
        // (File::create truncates, keeping the inode) — do not unlink it.
        if out_path.exists() && !is_writable(out_path) {
            return Err(io::Error::new(
                io::ErrorKind::PermissionDenied,
                gettext("output file is not writable"),
            ));
        }

        let mut o_file = File::create(out_path)?;
        let mut o_file_perm = o_file.metadata()?.permissions();
        let o_file_perm_mode = o_file_perm.mode();
        let new_o_file_perm_mode =
            ((o_file_perm_mode >> 9) << 9) | (header.lower_perm_bits & 0o7777);
        o_file_perm.set_mode(new_o_file_perm_mode);

        o_file.write_all(&out)?;
        // If the mode bits cannot be set, this is not an error (spec 119719-119720).
        let _ = o_file.set_permissions(o_file_perm);
    }

    Ok(())
}

fn pathname_display(path: &Option<PathBuf>) -> String {
    match path {
        None => gettext("standard input"),
        Some(p) => p.display().to_string(),
    }
}

fn main() {
    diag::init_locale("uudecode");

    let args = Args::parse();

    if let Err(e) = decode_file(&args) {
        diag::error(&format!("{}: {}", pathname_display(&args.file), e));
    }

    std::process::exit(diag::exit_status())
}
