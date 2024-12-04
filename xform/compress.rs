//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::io::input_stream;
use plib::lzw::UnixLZWWriter;
use std::fs::{self, File};
use std::io::{self, Write};
use std::path::{Path, PathBuf};

const NAME_MAX: usize = 255;

/// compress - compress data
#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// Specify the maximum number of bits to use in a code. 9 <= bits <= 14
    #[arg(short = 'b')]
    bits: Option<u32>,

    /// Write to the standard output; the input file is not changed, and no .Z files are created.
    #[arg(short = 'c', long)]
    stdout: bool,

    /// Do not prompt for overwriting files
    #[arg(short = 'f', long)]
    force: bool,

    /// Write messages to standard error concerning the expansion of each file.
    #[arg(short = 'v', long)]
    verbose: bool,

    /// Files to read as input.  Use "-" or no-args for stdin.
    files: Vec<PathBuf>,
}

fn prompt_user(prompt: &str) -> bool {
    eprint!("compress: {} ", prompt);
    let mut response = String::new();
    io::stdin().read_line(&mut response).unwrap();
    response.to_lowercase().starts_with('y')
}

fn compress_file(args: &Args, pathname: &PathBuf) -> io::Result<i32> {
    let mut file = input_stream(pathname, false)?;

    let mut encoder = UnixLZWWriter::new(args.bits);

    let mut inp_buf = Vec::new();
    file.read_to_end(&mut inp_buf)?;
    let inp_buf_size = inp_buf.len();

    let mut out_buf = encoder.write(&inp_buf)?;
    out_buf.extend_from_slice(&encoder.close()?);
    let out_buf_size = out_buf.len();

    // check if the output buffer size is greater than input buffer size
    // this can indicate that the compressed file is greater than the input file
    //
    // in such case using '-f' works, i.e "force", which means we don't care if size
    // isn't reduced, just compress it
    if (out_buf_size < inp_buf_size) || args.force {
        if args.stdout {
            io::stdout().write_all(&out_buf)?;
        } else {
            let fname = format!("{}.Z", pathname.file_name().unwrap().to_str().unwrap());
            // If "on adding .Z" the name exceeds the limit of NAME_MAX, then we output on the stdout
            if fname.len() > NAME_MAX {
                io::stdout().write_all(&out_buf)?;
            } else {
                let path = Path::new(&fname);
                if path.exists() && !args.force {
                    let is_affirm = prompt_user(&gettext!(
                        "Do you want to overwrite {} (y)es or (n)o?",
                        fname
                    ));

                    if !is_affirm {
                        println!("{fname} not overwritten");
                        return Ok(1);
                    }
                }

                let mut new_file = pathname.clone();
                new_file.set_file_name(&fname);
                let mut f = File::create(&new_file)?;

                fs::remove_file(pathname)?;
                f.write_all(&out_buf)?;

                if args.verbose {
                    println!(
                        "{}: -- replaced with {} Compression: {:.1}%",
                        pathname.display(),
                        new_file.display(),
                        100_f32 - (out_buf_size as f32 / inp_buf_size as f32) * 100_f32
                    );
                }
            }
        }
    } else {
        // error status code when file was not compressed as they would have increased in size and (-f
        // was not provided)
        return Ok(2);
    }

    Ok(0)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let mut args = Args::parse();

    if args.files.is_empty() {
        args.files.push(PathBuf::new());
    }

    let mut exit_code = 0;

    for filename in &args.files {
        match compress_file(&args, filename) {
            Ok(v) => {
                exit_code = v;
            }
            Err(e) => {
                exit_code = 1;
                eprintln!("{}: {}", filename.display(), e);
            }
        }
    }

    std::process::exit(exit_code)
}
