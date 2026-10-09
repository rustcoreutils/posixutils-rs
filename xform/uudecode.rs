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
use std::fs::{File, OpenOptions};
use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};

/// uudecode - decode a binary file
#[derive(Parser)]
#[command(version, about = gettext("uudecode - decode a binary file"))]
struct Args {
    #[arg(short, long, allow_hyphen_values = true, help = gettext("A pathname of a file that shall be used instead of any pathname contained in the input data"))]
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
        // Flushed here: decoded data need not end in a <newline>, and what
        // is left in stdout's line buffer reaches it only at exit, where a
        // write error is lost.
        // A failure is a write error, not one of the input file's.
        let mut stdout = io::stdout().lock();
        if let Err(e) = stdout.write_all(&out).and_then(|()| stdout.flush()) {
            diag::error(&format!(
                "{}: {}",
                gettext("write error"),
                diag::io_error_text(&e)
            ));
        }
    } else {
        write_output(out_path, header.lower_perm_bits, &out)?;
    }

    Ok(())
}

/// Write `data` to `path`, created or overwritten in place (never unlinked),
/// and give a regular file the access permission bits of `mode`.
///
/// Write permission on an existing file is checked by the open itself, so an
/// unwritable file ends uudecode with an error (spec 119716-119718) with no
/// window between a check and the open. Everything after the open goes
/// through the descriptor: the file type is that of the file opened, and only
/// a regular file is truncated and given the mode, so a device the pathname
/// names is written but never changed.
fn write_output(path: &Path, mode: u32, data: &[u8]) -> io::Result<()> {
    let mut file = open_output(path)?;
    let meta = file.metadata()?;
    if meta.is_file() {
        file.set_len(0)?;
        let mut perm = meta.permissions();
        plib::perm::set_mode(&mut perm, mode & ACCESS_PERMISSION_BITS);
        // If the mode bits cannot be set, this is not an error (spec 119719-119720).
        let _ = file.set_permissions(perm);
    }
    file.write_all(data)
}

/// The file access permission bits, the only ones the `begin` line may set:
/// set-user-ID, set-group-ID and sticky bits in the data are ignored.
const ACCESS_PERMISSION_BITS: u32 = 0o777;

/// Open `path` for writing, creating it if absent, without truncating it.
///
/// A new file starts owner-only until its mode is set. `O_NONBLOCK` makes a
/// FIFO with no reader fail with ENXIO instead of waiting forever, and is
/// cleared once open; `O_NOCTTY` keeps a terminal from becoming the
/// controlling one.
#[cfg(unix)]
fn open_output(path: &Path) -> io::Result<File> {
    use std::os::unix::fs::OpenOptionsExt;
    let file = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(false)
        .mode(0o600)
        .custom_flags(libc::O_NOCTTY | libc::O_NONBLOCK)
        .open(path)?;
    clear_nonblock(&file)?;
    Ok(file)
}

/// Open `path` for writing, creating it if absent, without truncating it.
#[cfg(windows)]
fn open_output(path: &Path) -> io::Result<File> {
    OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(false)
        .open(path)
}

/// Make writes to `file` block again.
#[cfg(unix)]
fn clear_nonblock(file: &File) -> io::Result<()> {
    use std::os::fd::AsRawFd;
    let fd = file.as_raw_fd();
    let flags = unsafe { libc::fcntl(fd, libc::F_GETFL) };
    if flags < 0 || unsafe { libc::fcntl(fd, libc::F_SETFL, flags & !libc::O_NONBLOCK) } < 0 {
        return Err(io::Error::last_os_error());
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
        diag::error(&format!(
            "{}: {}",
            pathname_display(&args.file),
            diag::io_error_text(&e)
        ));
    }

    std::process::exit(diag::exit_status())
}
