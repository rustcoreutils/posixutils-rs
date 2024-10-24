//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use base64::prelude::*;
use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use std::fs::{remove_file, File};
use std::io::{self, Error, Read, Write};
use std::os::unix::fs::PermissionsExt;
use std::path::PathBuf;

macro_rules! reduce {
    ($e : expr) => {
        $e - 0x20
    };
}

/// uudecode - decode a binary file
#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// A pathname of a file that shall be used instead of any pathname contained in the input data.
    #[arg(short, long)]
    outfile: Option<PathBuf>,

    /// The pathname of a file containing uuencoded data.
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

impl Header {
    fn parse(line: &str) -> Self {
        // split with spaces
        let split: Vec<&str> = line.split(' ').collect();
        let dec_type = if split[0] == "begin" {
            DecodingType::Historical
        } else if split[0] == "begin-base64" {
            DecodingType::Base64
        } else {
            panic!("Invalid encoding type");
        };

        let lower_perm_bits = u32::from_str_radix(split[1], 8).expect("Invalid permission value");
        let out = PathBuf::from(split[2]);

        Self {
            dec_type,
            lower_perm_bits,
            out,
        }
    }
}

fn decode_historical_line(line: &str) -> Vec<u8> {
    let mut out = Vec::new();

    for chunk in line.as_bytes().chunks(4) {
        let chunk = chunk.to_vec();

        let out_chunk = [
            reduce!(chunk[0]) << 2 | (reduce!(chunk[1])) >> 4,
            reduce!(chunk[1]) << 4 | reduce!(chunk[2]) >> 2,
            reduce!(chunk[2]) << 6 | reduce!(chunk[3]),
        ];

        out.extend_from_slice(&out_chunk);
    }

    out
}

fn decode_base64_line(line: &str) -> io::Result<Vec<u8>> {
    BASE64_STANDARD
        .decode(line)
        .map_err(|_| Error::from(io::ErrorKind::InvalidInput))
}

fn decode_file(args: &Args) -> io::Result<()> {
    let mut buf: Vec<u8> = Vec::new();
    let mut out: Vec<u8> = Vec::new();

    let file_p = args
        .file
        .as_ref()
        .unwrap_or(&PathBuf::from("/dev/stdin"))
        .clone();

    if file_p == PathBuf::from("/dev/stdin") {
        io::stdin().lock().read_to_end(&mut buf)?;
    } else {
        let mut file = File::open(&file_p)?;
        file.read_to_end(&mut buf)?;
    }

    let buf = String::from_utf8(buf).unwrap();
    let mut lines = buf.lines();
    let header = Header::parse(lines.next().expect("No header line"));

    match header.dec_type {
        DecodingType::Historical => {
            while let Some(line) = lines.next() {
                let line = line.replace("`", " ");
                if line.len() == 1 && line == " " {
                    let end_line = lines.next().expect("No end line");
                    if end_line == "end" || end_line == "end\r" {
                        break;
                    } else {
                        panic!("Invalid ending")
                    }
                }

                let len = (line[..1].as_bytes()[0] - 32) as usize;
                let mut dec_out = decode_historical_line(&line[1..]);
                dec_out.truncate(len);
                out.extend_from_slice(&dec_out);
            }
        }

        DecodingType::Base64 => {
            for line in lines {
                if line == "====" || line == "====\n" {
                    break;
                }
                out.extend_from_slice(&decode_base64_line(line)?);
            }
        }
    }

    let out_path = args.outfile.as_ref().unwrap_or(&header.out);

    if out_path == &PathBuf::from("/dev/stdout") {
        io::stdout().write_all(&out)?;
    } else {
        if out_path.exists() {
            remove_file(out_path)?;
        }

        let mut o_file = File::create(out_path)?;
        let mut o_file_perm = o_file.metadata()?.permissions();
        let o_file_perm_mode = o_file_perm.mode();
        let new_o_file_perm_mode = ((o_file_perm_mode >> 9) << 9) | header.lower_perm_bits;
        o_file_perm.set_mode(new_o_file_perm_mode);

        o_file.write_all(&out)?;
        o_file.set_permissions(o_file_perm)?;
    }

    Ok(())
}

fn pathname_display(path: &Option<PathBuf>) -> String {
    match path {
        None => "stdin".to_string(),
        Some(p) => p.display().to_string(),
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain(env!("PROJECT_NAME"))?;
    bind_textdomain_codeset(env!("PROJECT_NAME"), "UTF-8")?;

    let args = Args::parse();

    let mut exit_code = 0;

    if let Err(e) = decode_file(&args) {
        exit_code = 1;
        eprintln!("{:?}: {}", pathname_display(&args.file), e);
    }

    std::process::exit(exit_code)
}
