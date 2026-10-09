//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};

use clap::Parser;
use gettextrs::gettext;
use plib::io::input_stream;
use plib::BUFSZ;

#[derive(Parser)]
#[command(version, about = gettext("cat - concatenate and print files"))]
struct Args {
    #[arg(
        short,
        long,
        default_value_t = true,
        help = gettext("Disable output buffering (a no-op, for POSIX compat)")
    )]
    unbuffered: bool,

    #[arg(short = 'v', long = "show-nonprinting", help = gettext("Show nonprinting characters as ^X and M-X, except tab and newline"))]
    show_nonprinting: bool,

    #[arg(short = 'e', help = gettext("Same as -v, and mark the end of each line with $"))]
    show_ends: bool,

    #[arg(help = gettext("Files to read as input. Use '-' or no-args for stdin"))]
    files: Vec<PathBuf>,
}

/// How bytes are written out.
#[derive(Clone, Copy)]
struct Render {
    /// `-v`: nonprinting bytes as `^X`, `^?` and `M-X`.
    nonprinting: bool,
    /// `-e`: a `$` before each newline.
    ends: bool,
}

impl Render {
    fn is_plain(self) -> bool {
        !self.nonprinting && !self.ends
    }
}

/// Append the `-v` form of `byte` to `out`, as GNU and BSD cat write it: a
/// byte above 127 is `M-` and the form of the byte 128 below it, a control
/// character is `^` and the character 64 above it, DEL is `^?`. Tab and
/// newline, below 128, are written as they are.
fn push_visible(out: &mut Vec<u8>, byte: u8) {
    let low = if byte >= 0x80 {
        out.extend_from_slice(b"M-");
        byte - 0x80
    } else {
        byte
    };
    match low {
        b'\t' | b'\n' if byte < 0x80 => out.push(low),
        0x00..=0x1f => out.extend_from_slice(&[b'^', low + 0x40]),
        0x7f => out.extend_from_slice(b"^?"),
        _ => out.push(low),
    }
}

/// The bytes `data` is written as under `render`, appended to `out`.
fn render_into(out: &mut Vec<u8>, data: &[u8], render: Render) {
    for &byte in data {
        if byte == b'\n' && render.ends {
            out.push(b'$');
        }
        if render.nonprinting {
            push_visible(out, byte);
        } else {
            out.push(byte);
        }
    }
}

/// Copy one input file to standard output. Diagnostics are emitted here so a
/// read/open error is attributed to the input file while a write error is
/// attributed to standard output (not the input filename). Returns true if an
/// error occurred.
fn cat_file(pathname: &Path, render: Render) -> bool {
    let mut file = match input_stream(pathname, true) {
        Ok(f) => f,
        Err(e) => {
            eprintln!(
                "cat: {}: {}",
                pathname.display(),
                plib::diag::io_error_text(&e)
            );
            return true;
        }
    };
    let mut buffer = [0; BUFSZ];
    let stdout = io::stdout();
    let mut handle = stdout.lock();
    let mut rendered = Vec::new();

    loop {
        let n_read = match file.read(&mut buffer[..]) {
            Ok(0) => break,
            Ok(n) => n,
            Err(e) => {
                eprintln!(
                    "cat: {}: {}",
                    pathname.display(),
                    plib::diag::io_error_text(&e)
                );
                return true;
            }
        };

        let data = if render.is_plain() {
            &buffer[0..n_read]
        } else {
            rendered.clear();
            render_into(&mut rendered, &buffer[0..n_read], render);
            &rendered[..]
        };

        // One write per read, flushed at once: stdout is line-buffered, and a
        // chunk with no <newline> left in the buffer would otherwise reach the
        // device only at exit, where a write error is lost. It is also what
        // -u asks for.
        if let Err(e) = handle.write_all(data).and_then(|()| handle.flush()) {
            eprintln!(
                "cat: {}: {}",
                gettext("write error"),
                plib::diag::io_error_text(&e)
            );
            return true;
        }
    }

    false
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    plib::diag::init_locale("cat");

    let mut args = Args::parse();

    // if no file args, read from stdin
    if args.files.is_empty() {
        args.files.push(PathBuf::from("-"));
    }

    let render = Render {
        nonprinting: args.show_nonprinting || args.show_ends,
        ends: args.show_ends,
    };

    let mut exit_code = 0;

    for filename in &args.files {
        if cat_file(filename, render) {
            exit_code = 1;
        }
    }

    std::process::exit(exit_code)
}
