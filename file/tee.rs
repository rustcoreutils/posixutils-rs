//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fs::{File, OpenOptions};
use std::io::{self, Read, Write};

use clap::Parser;
use gettextrs::{LocaleCategory, bind_textdomain_codeset, gettext, setlocale, textdomain};
use plib::BUFSZ;

#[derive(Parser)]
#[command(version, about = gettext("tee - duplicate standard input"))]
struct Args {
    #[arg(short, long, help = gettext("Append the output to the files"))]
    append: bool,

    #[arg(short, long, help = gettext("Ignore the SIGINT signal"))]
    ignore: bool,

    #[arg(short, long, help = gettext("One or more output files"))]
    files: Vec<String>,
}

struct TeeFile {
    filename: String,
    f: File,
}

#[derive(Default)]
struct TeeInfo {
    outputs: Vec<TeeFile>,
}

fn open_outputs(args: &Args, info: &mut TeeInfo) -> io::Result<()> {
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
                eprintln!("{}: {}", filename, e);
                return Err(e);
            }
            Ok(f) => {
                info.outputs.push(TeeFile {
                    filename: filename.to_string(),
                    f,
                });
            }
        }
    }

    Ok(())
}

fn tee_stdin(info: &mut TeeInfo) -> io::Result<()> {
    let mut buffer = [0; BUFSZ];

    loop {
        let n_read_res = io::stdin().read(&mut buffer[..]);
        if let Err(e) = n_read_res {
            eprintln!("stdin: {}", e);
            return Err(e);
        }
        let n_read = n_read_res.unwrap();
        if n_read == 0 {
            break;
        }

        let bufslice = &buffer[0..n_read];

        for output in &mut info.outputs {
            let res = output.f.write_all(bufslice);
            if let Err(e) = res {
                eprintln!("{}: {}", output.filename, e);
                return Err(e);
            }
        }
    }

    Ok(())
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

    open_outputs(&args, &mut state)?;
    tee_stdin(&mut state)?;

    Ok(())
}
