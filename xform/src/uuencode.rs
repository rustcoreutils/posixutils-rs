//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

extern crate clap;
extern crate plib;

use base64::prelude::*;
use clap::Parser;
use gettextrs::{bind_textdomain_codeset, textdomain};
use plib::PROJECT_NAME;
use std::fs::{File, Permissions};
use std::io::{self, Read, Write};
use std::os::unix::fs::PermissionsExt;
use std::path::PathBuf;

const PERMISSION_MASK: u32 = 0o7;

/// uuencode - encode a binary file
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Encode to base64 (MIME) standard, rather than UUE format.
    #[arg(short = 'm', long)]
    base64: bool,

    /// File to read as input.
    file: Option<PathBuf>,

    /// Decode pathname
    decode_path: Option<String>,
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

/// <sys/stat.h> mentions the constants ncessary to extract out the permission
/// so we'll use bit masking by shifting 3 bits everytime to ge
fn get_permission_values(perm: Permissions) -> String {
    let perm_mode = perm.mode();

    let others_perm = perm_mode & PERMISSION_MASK;
    let group_perm = (perm_mode >> 3) & PERMISSION_MASK;
    let user_perm = (perm_mode >> 6) & PERMISSION_MASK;

    format!("{user_perm}{group_perm}{others_perm}")
}

fn encode_base64_line(line: &[u8]) -> Vec<u8> {
    let mut out = BASE64_STANDARD.encode(&line).as_bytes().to_vec();
    out.push(b'\n');
    out
}

fn encode_historical_line(line: &[u8]) -> Vec<u8> {
    let mut out = Vec::new();
    out.push(line.len() as u8 + 0x20);
    // in every line we'll take a chunk of 3 bytes and encode it
    for in_chunk in line.chunks(3) {
        // there could be less than 3 bytes, we need to fill it up with the padding byte
        let a = in_chunk[0];
        let b = *in_chunk.get(1).unwrap_or(&0);
        let c = *in_chunk.get(2).unwrap_or(&0);

        // https://pubs.opengroup.org/onlinepubs/9699919799/utilities/uuencode.html directly mentions
        // the algorithm below(to convert 3 byte to 4 bytes of historical encoding)
        let out_chunk = [
            0x20 + (a >> 2) & 0x3F,
            0x20 + ((a & 0x03) << 4) | ((b >> 4) & 0x0F),
            0x20 + ((b & 0x0F) << 2) | ((c >> 6) & 0x03),
            0x20 + (c & 0x3F),
        ];

        out.extend_from_slice(&out_chunk);
    }
    out.push(b'\n');
    out
}

/// encodes and puts out on stdout
fn encode(args: &Args) -> io::Result<()> {
    let decode_path = match &args.decode_path {
        None => String::from("/dev/stdout"),
        Some(path) => String::from(path),
    };

    let encoding_type = if args.base64 {
        EncodingType::Base64
    } else {
        EncodingType::Historical
    };

    let mut buf: Vec<u8> = Vec::new();
    let mut out: Vec<u8> = Vec::new();

    let header_init = encoding_type.get_header();
    let file_p = args
        .file
        .as_ref()
        .unwrap_or(&PathBuf::from("/dev/stdin"))
        .clone();

    if file_p == PathBuf::from("/dev/stdin") {
        let header = format!("{header_init} 644 {decode_path}\n");
        io::stdout().write_all(header.as_bytes())?;
        io::stdin().lock().read_to_end(&mut buf)?;
    } else {
        let mut file = File::open(&file_p)?;
        let perm = get_permission_values(file.metadata()?.permissions());

        let header = format!("{header_init} {perm} {decode_path}\n");
        out.extend_from_slice(header.as_bytes());
        file.read_to_end(&mut buf)?;
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

fn pathname_display(path: &Option<PathBuf>) -> String {
    match path {
        None => String::from("/dev/stdin"),
        Some(p) => p.display().to_string(),
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let mut exit_code = 0;

    if let Err(e) = encode(&args) {
        exit_code = 1;
        eprintln!("{:?}: {}", pathname_display(&args.file), e);
    }

    std::process::exit(exit_code)
}
