//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fs::{File, OpenOptions};
use std::io::{self, Read, Write};

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::BUFSZ;

#[derive(Parser)]
#[command(version, about = gettext("tee - duplicate standard input"))]
struct Args {
    #[arg(short, long, help = gettext("Append the output to the files"))]
    append: bool,

    #[arg(short, long, help = gettext("Ignore the SIGINT signal"))]
    ignore: bool,

    #[arg(help = gettext("One or more output files"))]
    files: Vec<String>,
}

struct TeeFile {
    filename: String,
    f: File,
    /// Cleared once a write to this file has failed; further writes are skipped
    /// while writes to the other files and to standard output continue.
    ok: bool,
}

#[derive(Default)]
struct TeeInfo {
    outputs: Vec<TeeFile>,
}

/// Open every file operand. A file that cannot be opened produces a diagnostic
/// and is skipped — the remaining files are still opened — and the overall
/// exit status becomes non-zero (returned as `had_error`).
fn open_outputs(args: &Args, info: &mut TeeInfo) -> bool {
    let mut had_error = false;
    for filename in &args.files {
        let f_res = OpenOptions::new()
            .read(false)
            .write(true)
            .create(true)
            .truncate(!args.append)
            .append(args.append)
            .open(filename);

        match f_res {
            Err(e) => {
                eprintln!("tee: {}: {}", filename, e);
                had_error = true;
            }
            Ok(f) => {
                info.outputs.push(TeeFile {
                    filename: filename.to_string(),
                    f,
                    ok: true,
                });
            }
        }
    }

    had_error
}

/// Copy standard input to standard output and to each open file. Per POSIX
/// CONSEQUENCES OF ERRORS, a failed write to one file does not stop writes to
/// the other files or to standard output; only the exit status becomes
/// non-zero. Returns `had_error`.
fn tee_stdin(info: &mut TeeInfo) -> bool {
    let mut buffer = [0; BUFSZ];
    let mut had_error = false;

    loop {
        let n_read = match io::stdin().read(&mut buffer[..]) {
            Ok(n) => n,
            Err(e) => {
                eprintln!("tee: stdin: {}", e);
                return true;
            }
        };
        if n_read == 0 {
            break;
        }

        let bufslice = &buffer[0..n_read];

        // tee copies standard input to standard output, in addition to each
        // named file (POSIX: "The standard output shall be a copy of the
        // standard input.").
        if let Err(e) = io::stdout().write_all(bufslice) {
            eprintln!("tee: stdout: {}", e);
            had_error = true;
        }

        for output in &mut info.outputs {
            if !output.ok {
                continue;
            }
            if let Err(e) = output.f.write_all(bufslice) {
                eprintln!("tee: {}: {}", output.filename, e);
                output.ok = false;
                had_error = true;
            }
        }
    }

    had_error
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    if args.ignore {
        unsafe {
            libc::signal(libc::SIGINT, libc::SIG_IGN);
        }
    }

    let mut state = TeeInfo::default();

    let mut had_error = open_outputs(&args, &mut state);
    had_error |= tee_stdin(&mut state);

    if had_error {
        std::process::exit(1);
    }
    Ok(())
}
